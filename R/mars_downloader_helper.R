
# Load Libraries --------------------
#dplyr stuff
library(magrittr)
library(tidyverse)
library(lubridate)
library(padr)
library(zoo)

#GIS Stuff
library(rgeos)
library(rgdal)

#Other Stuff
library(odbc)

# Set Options --------------
options(stringsAsFactors=FALSE)

# marsExportGISData -----------------------------
marsExportGISData <- function(con, smp_ids, gagenames, specialExports, fileformat = c("KML", "Shapefile", "CSV"), coordsys = c("Unprojected", "State Plane")){
  #Step 0: Parameter validity
    #Make sure con is a valid DB connection
    validCon <- dbIsValid(con)
    if(!validCon){
      stop("You have not made a valid connection to the database.")
    }

    if(fileformat == "KML" & coordsys != "Unprojected"){
      stop("You have attempted to export a KML file that uses a projected coordinate system.")
    }

    #Check for legal special exports
    #The first list in specialExports is the TRUE/FALSE values
    #If more than 1 of these are TRUE, abort
    exportflags <- specialExports[[1]] %>% unlist
    if(sum(exportflags) > 1){
      stop("You have attempted to make more than one special export at the same time.")
    }

    #If no special exports, check for invalid SMP IDs and gagenames
    if(sum(exportflags) == 0){
      #Check for invalid SMP IDs
      smp_loc_ids <- dbGetQuery(con, "SELECT smp_id FROM smp_loc") %>% pull(smp_id)
      idsValid <- smp_ids %in% smp_loc_ids
      idsAllValid <- all(idsValid)
      if(!idsAllValid){
        stop(paste("You have supplied invalid SMP IDs:", smp_ids[!idsValid]))
      }

      if(length(gagenames > 0)){
        #Check for invalid gage names
        gage_loc_names <- dbGetQuery(con, "SELECT g.gagename FROM gage_loc gl left join gage g on gl.gage_uid = g.gage_uid") %>% pull(gagename)
        namesValid <- gagenames %in% gage_loc_names
        namesAllValid <- all(namesValid)
        if(!namesAllValid){
          stop(paste("You have supplied invalid rain gage names:", gagenames[!namesValid]))
        }
      }
    }

  #Step 1: Collect smp locations and coordinate locations
    #Grab SMP locations
    smp_loc <- dbGetQuery(con, "SELECT * from smp_loc") %>%
      filter(smp_id %in% smp_ids) %>%
      select(smp_id, lon_wgs84, lat_wgs84)

    #add SMP type, OW suffixes, current sensor serial numbers as metadata
    asset_type <- dbGetQuery(con, "SELECT * from smpid_facilityid_componentid sfc where sfc.component_id IS NULL") %>%
      semi_join(smp_loc, by = "smp_id") %>%
      select(smp_id, asset_type)

    ow_suffixes <- dbGetQuery(con, "SELECT * from ow_testing") %>%
      semi_join(smp_loc, by = "smp_id") %>%
      select(smp_id, ow_suffix) %>%
      group_by(smp_id) %>%
      summarize(ows = paste(ow_suffix, collapse = "; "))

    smp_everything <- left_join(smp_loc, asset_type, by = "smp_id") %>%
      left_join(ow_suffixes, by = "smp_id")

    #Grab gage locations
    if(length(gagenames) > 0){
      gage_loc <- dbGetQuery(con, "SELECT gagename, lon_wgs84, lat_wgs84 from gage_loc gl left join gage g on gl.gage_uid = g.gage_uid") %>%
        filter(gagename %in% gagenames)
    }

  #Step 2: Convert locations to coordinate system
    coordinates(smp_everything) <- c("lon_wgs84", "lat_wgs84")
    proj4string(smp_everything) <- CRS("+init=epsg:4326")

    if(length(gagenames) > 0){
      coordinates(gage_loc) <- c("lon_wgs84", "lat_wgs84")
      proj4string(gage_loc) <- CRS("+init=epsg:4326")
    }

    if(coordsys == "State Plane"){
      smp_everything <- spTransform(smp_everything, CRS("+init=epsg:2272"))
      colnames(smp_everything@coords) <- c("easting", "northing")

      if(length(gagenames) > 0){
        gage_loc <- spTransform(gage_loc, CRS("+init=epsg:2272"))
        colnames(gage_loc) <- c("easting", "northing")
      }
    }

  #Step 3: Export data and print stats to console
    nowstring <- now() %>%
      {gsub(":", "", .)} %>% #Strip colons out of the timestamp
      {gsub(" ", "_", .)} #Replace the space between the date and time with an underscore

    if(fileformat == "CSV"){
      #Must de-spatialize the objects to write to CSV
      smp_csv <- cbind(smp_everything@data, smp_everything@coords)

      if(length(gagenames) > 0){
        gage_csv <- cbind(gage_loc@data, gage_loc@coords)
      }

      #Write SMP data
      print(paste("Writing SMP location data to:", path = paste(nowstring, "smp_location_data.csv", sep = "_")))
      write_csv(smp_csv, path = paste(nowstring, "smp_location_data.csv", sep = "_"))

      #Write gage data
      if(length(gagenames) > 0){
        print(paste("Writing gage location data to:", path = paste(nowstring, "raingage_location_data.csv", sep = "_")))
        write_csv(gage_csv, path = paste(nowstring, "raingage_location_data.csv", sep = "_"))
      }

      return("Export to CSV Completed")
    } else if(fileformat == "Shapefile"){

      #Write SMP data
      print(paste("Writing SMP location data to:", path = paste(nowstring, "smp_location_data.shp", sep = "_")))
      writeOGR(smp_everything,
        dsn = ".", #For ESRI Shapefiles, this is the folder where the file goes
        layer = paste(nowstring, "smp_location_data", sep = "_"), #For ESRI Shapefiles, this is the filename without .shp
        driver = "ESRI Shapefile")

      #Write gage data
      if(length(gagenames) > 0){
        print(paste("Writing gage location data to:", path = paste(nowstring, "raingage_location_data.shp", sep = "_")))
        writeOGR(gage_loc,
          dsn = ".", #For ESRI Shapefiles, this is the folder where the file goes
          layer = paste(nowstring, "raingage_location_data", sep = "_"), #For ESRI Shapefiles, this is the filename without .shp
          driver = "ESRI Shapefile")
      }

      return("Export to ESRI Shapefile Completed")
    } else if(fileformat == "KML"){

      #Write SMP data
      print(paste("Writing SMP location data to:", path = paste(nowstring, "smp_location_data.kml", sep = "_")))
      suppressWarnings(
        writeOGR(smp_everything,
          dsn = paste(nowstring, "smp_location_data.kml", sep = "_"), #For KML, this is the full filename
          layer = paste(nowstring, "smp_location_data", sep = "_"), #For KML, this is the filename without .kml
          driver = "KML",
          dataset_options = c("NameField=smp_id"))
        )

      #Write gage data
      if(length(gagenames) > 0){
        print(paste("Writing gage location data to:", path = paste(nowstring, "raingage_location_data.kml", sep = "_")))
        suppressWarnings(
          writeOGR(gage_loc,
            dsn = paste(nowstring, "raingage_location_data.kml", sep = "_"),
            layer = paste(nowstring, "raingage_location_data", sep = "_"),
            driver = "KML",
            dataset_options = c("NameField=gagename"))
        )
      }

    } else{
      print("You fucked up.")
    }

}

# marsInterpolateBaro -------------------------
#When requesting baro data, if an SMP has a baro with data on-site for a specific timestep, use that baro.
#If not, use the inverse distance weighted interpolation of all baros with data.
#baro_psi is a vector of baro pressures all measured at the same time
#smp_id is a vector of SMP IDs where the measurements took place
#weights is a vector of inverse distance weights to be applied
#target_id is a single SMP ID where the user has requested data

#roxygen2
#' Interpolate barometric pressure with inverse distance weighting
#'
#' Returns an on-site barometric pressure reading, an interpolated barometric pressure reading, \code{NA},
#'  or a combination.
#'
#' @seealso \code{\link{marsFetchBaroData}}
#'
#' @param baro_psi vector, num, barometric pressures measured at the same timestamp
#' @param smp_id vector, chr, SMP IDs where the measurements took place
#' @param weights vector, num, of inverse distances weights for each baro, calculated by \code{\link{marsFetchBaroData}}
#' @param target_id chr, single SMP ID where the user has requested data
#'
#' @return Output will be a single barometric pressure reading.
#'   If there is a baro at the target SMP, the reading will be from that baro.
#'   If not, and there are more than 5 baros with data,
#'   the reading will be an inverse distance-weighted
#'   interpolation of those readings.
#'   If there are fewer than 5 readings, return \code{NA}.
#'
#' @export


marsInterpolateBaro <- function(baro_psi, smp_id, weight, target_id){

  if (target_id %in% smp_id){
    return(baro_psi[which(target_id == smp_id)])
  } else {
    return(ifelse(length(baro_psi) >=5,
                  sum(baro_psi *weight)/sum(weight),
                  NA)
    )
  }
}

# marsFetchBaroData --------------------------------

#' Fetch barometric pressure data for a target SMP, date range, and interval
#'
#' Returns a data frame with datetime, barometric pressure, smp id, and number of neighbors
#'   interpolated from to collect the data.
#'
#' @param con An ODBC connection to the MARS Analysis database returned by odbc::dbConnect
#' @param target_id chr, single SMP ID where the user has requested data
#' @param start_date POSIXct, format: "YYYY-MM-DD", start of data request range
#' @param end_date POSIXct, format: "YYYY-MM-DD", end of data request range
#' @param data_interval chr, \code{"5 mins"} or \code{"15 mins"}, interval at which baro data will be returned.
#'
#' @return Output will be a dataframe with four columns: "dtime_est" (POSIXct, format: "YYYY-MM-DD HH:MM:SS"),
#'    "baro_psi" (num), "smp_id" (chr), "neighbors" (int).
#'     If the target SMP has an on-site baro with data, the "neighbors" column will be NA.
#'     If there are fewer than five baros to interprolate from, based on \code{\link{marsInterpolateBaro}},
#'     all columns other than "dtime_est" will be NA.
#'
#' @export
#'
#' @seealso \code{\link{marsInterpolateBaro}}
#'
#'

marsFetchBaroData <- function(con, target_id, start_date, end_date, data_interval = c("5 mins", "15 mins")){
  if(!dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }

  #browser()
  #Generate the beginning of a report about the baro request
  report_filename <- paste("//pwdoows/oows/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader/Reports/", paste0(paste(today("EST"), smp_id, "baro_report", sep ="_"), ".txt"))
  report_title <- "Composite Baro Data Generation Report"
  write(report_title, file = report_filename, append = FALSE)

  #Record the arguments used to supply the baro data
  arguments <- c(paste("SMP:", smp_id),
                 paste("Start Date:", start_date),
                 paste("End date:", end_date),
                 paste("Data Interval:", data_interval)
  )
  write(arguments, file = report_filename, append = TRUE)



  #Get SMP locations, and the locations of the baro sensors
  smp_loc <- dbGetQuery(con, "SELECT * FROM public.smp_loc")
  locus_loc <- filter(smp_loc, smp_id == target_id)
  baro_smp <- dbGetQuery(con, "SELECT DISTINCT smp_id FROM public.baro_rawfile;") %>% pull(smp_id)

  #Collect baro data
  #Get all baro data for the specified time period
  baro <- dbGetQuery(con, paste0("SELECT * FROM barodata_smp b WHERE b.dtime_est >= '", start_date, "'", " AND b.dtime_est <= '", end_date + days(1), "';"))
  baro$dtime_est %<>% force_tz(tz = "EST")


  #When the user requests data at a 5-minute resolution, we need to stretch our 15-minute data into 5-minute data
  #We can use tidyr::spread and padr::pad to generate the full 5 minute time series,
  #And then use zoo::na.locf (last observation carried forward) to fill the NAs with the most recent value
  if(data_interval == "5 mins"){

    #Spread data to have all baro measurements use the same dtime_est column
    #So we can pad every 15-minute time series at once
    baro <- spread(baro, "smp_id", "baro_psi")

    #Pad installs 5 minute intervals in our 15 minute dtime_est column. All other columns become NA
    #End value is 10 minutes after the final period because that 15 minute data point is good for 10 more minutes
    baro_pad <- pad(baro, start_val = min(baro$dtime_est), end_val = max(baro$dtime_est) + minutes(10), interval = "5 mins")

    #We report on the number of LOCF operations
    write("Number of LOCFs", file = report_filename, append = TRUE)

    #To count the LOCF operations, we count the NAs in the data frame before and after the LOCF
    countNAs <- baro_pad[1,]
    for(i in 2:ncol(baro_pad)){
      countNAs[,i] <- sum(is.na(baro_pad[,i])) #count NAs before they are filled
      baro_pad[,i] <- na.locf(baro_pad[,i], maxgap = 2, na.rm = FALSE) #maxgap = 2 means only fill NAs created by the pad
      countNAs[,i] <- countNAs[,i]- sum(is.na(baro_pad[,i])) #subtract remaining NAs to get number of NAs filled
      write(paste(colnames(countNAs[i]), ": ", countNAs[,i], sep = ""), file = report_filename, append = TRUE) #Add LOCF count to report
    }

    #Return baro data to long data format
    baro <- gather(baro_pad, "smp_id", "baro_psi", -dtime_est) %>%
      filter(!is.na(baro_psi))
  }



  #Calculate the distance between every baro location and the target SMP, then add weight
  baro_weights <- filter(smp_loc, smp_id %in% baro_smp) %>%
    mutate(lon_dist = lon_wgs84 - locus_loc$lon_wgs84,
           lat_dist = lat_wgs84 - locus_loc$lat_wgs84,
           dist_total = sqrt(abs(lon_dist**2 - lat_dist**2))) %>%
    mutate(weight = 1/dist_total) %>% #inverse distance weight with power = 1
    select(smp_id, weight) %>%
    arrange(smp_id)
#
  #
  #
  #


  interpolated_baro <- left_join(baro, baro_weights, by = "smp_id") %>% #join baro and weights
    group_by(dtime_est) %>% #group datetimes, then calculate weighting effect for each datetime
    summarize(baro_psi =  marsInterpolateBaro(baro_psi, smp_id, weight, target_id),
              smp_id = ifelse(target_id %in% smp_id, target_id, "interpolated"),
              neighbors = ifelse(target_id %in% smp_id, NA, n()))

  #Adding "neighbor" counts and instances to report
  neighbors <- data.frame(group_by(interpolated_baro, neighbors) %>% summarize(count = n()))
  write(paste("Neighbors: Count"), file = report_filename, append = TRUE)
  for(i in 1:nrow(neighbors)){
    write(paste(neighbors$neighbors[i], paste(neighbors$count[i]), sep = ":  "), file = report_filename, append = TRUE)
  }

  #Note the file that the output has been saved to
  write(paste0("Baro data has been saved to ", paste(smp_id, start_date, "to", end_date, sep = "_"), ".csv"), file = report_filename, append = TRUE)

  finalseries <- interpolated_baro

  #Give 5 or 15 minute data as appropriate
  if(data_interval == "15 mins"){
    clippedseries <- data.frame(dtime_est = seq.POSIXt(from = start_date, to = end_date + days(1), by = data_interval) )

    finalseries <- filter(finalseries, dtime_est %in% clippedseries$dtime_est)
    return(finalseries)
  } else{
    return(finalseries)
  }
}
