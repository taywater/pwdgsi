#PWD GSI MARS
#Written by Taylor Heffernan, PWD, and Nicholas Manna, AKRF
#Last modified: 06/10/2019
#Function based on steps to identify private SMPs for 1 tracking number, created by Taylor Heffernan


#lookupPrivateSMPs---------------------
#Rogygen
#' Return query results of private SMPs
#'
#' Returns private SMP tracking number, project name, SMP ID, and plan label.
#'
#' @param con Formal class PostgreSQL, a connection to the MARS Analysis database
#' @param tracking_number chr, tracking number for the requested SMP
#'
#' @return Output will be a dateframe with four columns:
#'   
#'     \item{tracking_number}{chr, requested SMP tracking number}
#'     \item{project name}{chr, SMP poject name}
#'     \item{smp_id}{int, SMP ID}
#'     \item{plan_label}{chr, name that the SMP is given on the plan set}
#'   
#'   If a tracking number is not found, that row will include an error message
#'   in the "project name" column, and NAs in "smp_id" and "plan_label".
#'
#' @export
#' 
#' 


lookupPrivateSMPs <- function(con, tracking_numbers){
  #Validate DB connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }

  #Rather than validating each individual tracking number and selecting them one at a time
  #We can grab the entire table and filter by our tracking numbers to find the valid ones
  planreviewtable <- odbc::dbGetQuery(con, "select p.\"TrackingNumber\" as tracking_number, p.\"Projectname\" as project_name, p.\"SMPID\" as smp_id, p.\"Plan Label\" as plan_label from planreview_view_smpsummary_crosstab_asbuiltall p")
  hits <- dplyr::filter(planreviewtable, tracking_number %in% tracking_numbers)

  #If any of the tracking numbers weren't found, we can return an error message
  misses <- tracking_numbers[!(tracking_numbers %in% planreviewtable$tracking_number)]
  
  if(length(misses) > 0) {
    invalid <- data.frame(tracking_number = misses, project_name = "Tracking number not found. Did you type it wrong?", smp_id = NA, plan_label = NA)
    privateSMPs <- dplyr::bind_rows(hits, invalid)
  } else {
    privateSMPs <- hits
  }

  return(privateSMPs)
}

# marsFetchRainGageData ------------------------------------------
#' Return a dataframe with rain gage data
#'
#' Return data from the rain gage nearest a target SMP, for a specified date range.
#'
#' @param con Formal class 'PostgreSQL', a connection to the MARS Analysis database
#' @param target_id chr, an SMP_ID that where the user has requested data
#' @param start_date POSIXct, format: "YYYY-MM-DD", start of data request range
#' @param end_date POSIXct, format: "YYYY-MM-DD", end of data request range
#' @param daylight_savings logi, Adjust for daylight savings time? when doing QAQC
#'   this should be \code{FALSE} because the water level data does not spring forwards.
#'
#' @return Output will be a data frame with four columns, which corresponds to the specified SMP and date range:
#' 
#'   \item{dtime_est OR dtime_edt}{POSIXct datetime with tz = EST or EDT as specified by \code{daylight_savings}}
#'   \item{rainfall_in}{num, rainfall for the 15 minute preceding the corresponding datetime}
#'   \item{gagename}{rain gage ID}
#'   \item{event_id}{event number during this timestep}
#' 
#' 
#' @seealso \code{\link[pwdgsi]{gapFillEventID}}, \code{\link{detectEvents}}
#'
#' @export

marsFetchRainGageData <- function(con, target_id, start_date, end_date, daylightsavings){
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open RODBC channel")
  }


  #Get closest gage
  smp_gage <- odbc::dbGetQuery(con, "SELECT * FROM public.smp_gage") %>% dplyr::filter(smp_id == target_id)

  #Collect gage data
  #First, get all the relevant data from the closest gage
  gage_query <- paste("SELECT dtime_edt, rainfall_in, gage_uid FROM public.rainfall_gage",
                      "WHERE gage_uid = CAST('", smp_gage$gage_uid[1], "' as int)",
                      "AND dtime_edt >= Date('", start_date, "')",
                      "AND dtime_edt <= Date('", end_date + lubridate::days(1), "');")

  gage_temp <- odbc::dbGetQuery(con, gage_query)

  if(nrow(gage_temp) == 0){

    if(lubridate::month(start_date) == lubridate::month(lubridate::today())){
      stop(paste("Rainfall data appears in the MARS database on about a 5 week delay. \nData for", lubridate::month(start_date, label = TRUE, abbr = FALSE), "should be available in the second week of", lubridate::month(lubridate::today() + lubridate::months(1), label = TRUE, abbr = FALSE)))
    }
    stop("There is no data in the database for this date range.")
  }

  gage_temp$rainfall_in %<>% as.numeric
  gage_temp$dtime_edt %<>% lubridate::ymd_hms(tz = "America/New_york")

  #Apparently, attempting to set the time zone on a datetime that falls squarely on the spring forward datetime
  #Such as 2005-04-03 02:00:00
  #Returns NA, because the time is impossible.
  #I hate this so, so much
  #To mitigate this, we will strip NA values from the new object
  gage_temp %<>% dplyr::filter(!is.na(dtime_edt))

  #Our water level data is not corrected for daylight savings time. ie it doesn't spring forwards
  #So we must shift back any datetimes within the DST window
  #Thankfully, the dst() function returns TRUE if a dtime is within that zone
  if(daylightsavings == FALSE){
    dst_index <- lubridate::dst(gage_temp$dtime_edt)
    gage_temp$dtime_edt %<>% lubridate::force_tz("EST") #Assign new TZ without changing dates
    gage_temp$dtime_edt[dst_index] <- gage_temp$dtime_edt[dst_index] - lubridate::hours(1)
  }

  gage_temp %<>% dplyr::mutate(event_id = detectEvents(dtime_est = dtime_edt, rainfall_in = rainfall_in, iet_hr = 6, mindepth_in = 0.10))

  #Punctuate data with zeroes to prevent linear interpolation when plotting
  #If the time between data points A and B is greater than 15 minutes (the normal timestep), we must insert a zero 15 minutes after A
  #If it's greather than 30 minutes, we must insert a zero 15 minutes before B also

  #First, create data frame to contain zero fills with same column names as our rain data
  zeroFills <- gage_temp[0,]

  for(i in 1:(nrow(gage_temp) - 1)){
    k <- difftime(gage_temp$dtime_edt[i+1], gage_temp$dtime_edt[i], units = "min")

    #If gap is > 15 mins, put a zero 15 minutes after the gap starts
    if(k > 15){

      #browser()

      zeroFillIndex <- nrow(zeroFills)+1

      #Boundaries of the interval to be zeroed
      boundary.low <- gage_temp$dtime_edt[i]
      boundary.high <- gage_temp$dtime_edt[i+1]

      #The zero goes 15 minutes (900 seconds) after the first boundary
      #Filled by index because R is weird about partially filled data frame rows
      fill <- boundary.low + lubridate::seconds(900)
      zeroFills[zeroFillIndex, 1] <- fill                   #dtime_edt
      zeroFills[zeroFillIndex, 2] <- 0                      #rainfall_in
      zeroFills[zeroFillIndex, 3] <- smp_gage$gage_uid[1]   #gage_uid

      #print(paste("Gap-filling event ID. Before:", gage_temp$event[i], "After:", gage_temp$event[i+1]))
      zeroFills[zeroFillIndex, 4] <- gapFillEventID(event_low = gage_temp$event[i], event_high = gage_temp$event[i+1]) #event

      #If the boundary is longer than 30 minutes, we need a second zero
      if(k > 30){

        #This zero goes 15 minutes before the upper boundary
        fill <- boundary.high - lubridate::seconds(900)
        zeroFills[zeroFillIndex + 1, 1] <- fill                   #dtime_edt
        zeroFills[zeroFillIndex + 1, 2] <- 0                      #rainfall_in
        zeroFills[zeroFillIndex + 1, 3] <- smp_gage$gage_uid[1]   #gage_uid

        #print(paste("Gap-filling event ID. Before:", gage_temp$event[i], "After:", gage_temp$event[i+1]))
        zeroFills[zeroFillIndex + 1, 4] <- gapFillEventID(event_low = gage_temp$event[i], event_high = gage_temp$event[i+1]) #event

      }

    }
  }

  #Replace UIDs with SMP IDs
  gages <- odbc::dbGetQuery(con, "SELECT * FROM public.gage")
  finalseries <- dplyr::bind_rows(gage_temp, zeroFills) %>%
    dplyr::left_join(gages) %>%
    dplyr::select(dtime_edt, rainfall_in, gagename, event_id) %>%
    dplyr::arrange(dtime_edt)

  #Rename dtime column if we are undoing daylight savings time
  if(daylightsavings == FALSE){
    finalseries <- finalseries %>%
      dplyr::mutate(dtime_est = dtime_edt) %>%
      dplyr::select(-dtime_edt)
    finalseries <- dplyr::select(finalseries, dtime_est, rainfall_in, gagename, event_id)
  }


  return(finalseries)
}

# gapFillEventID -----------------------
#When determining the appropriate event ID for zero-punctuated timestamps in a rainfall series, use this function
#Zeroes that appear within an event (ie less than 6 hours of time has elapsed between measurements) should have the event ID of the event they occur in
#Zeroes that appear at event boundaries should have event NA
#This function returns an integer or NA as follows:
  #One or both of event_low or event_high is NA: return NA
    #This is a boundary adjacent to, or a hole within, an event of less than the minimum depth, and should not be counted
  #event_low != event_high and neither is NA: return NA
    #This is a boundary between two events, and should not be counted
  #event_low = event_high and neither is NA: return event_low
    #This is a hole within an event of greater than the minimum depth, and should be counted
#Check for NA arguments first because if x == NA returns NA, which chokes the conditional statement

#' Return a dataset with event IDs for zero-punctuated timesteps.
#'
#' Each rainfall event must be zero-punctuated. These zeroes are given event IDs based on the IDs of the
#' rainfall that precede and follow them, given by \code{\link{detectEvents}}.
#'
#' @param event_low num, event ID of preceding rainfall.
#' @param event_high num, event ID of following rainfall.
#'
#' @return Output will be a vector containing either \code{NA} or \code{event low}. If one or both events are
#' \code{NA}, return {NA}. If the event IDs are not equal, return \code{NA}, since this is a boundary between
#' events. If the event IDs are equal, return \code{event low}.

gapFillEventID <- function(event_low, event_high){
  if(is.na(event_low) | is.na(event_high)){
    #print("One or both events are NA. Returning NA")
    return(NA) #This is a boundary adjacent to, or a hole within, an event of less than the minimum depth, and should not be counted
  }
  else if(event_low != event_high){
    #print("Events are not equal. Returning NA")
    return(NA) #This is a boundary between two events, and should not be counted
  }
  else if(event_low == event_high){
    #print(paste("Events are equal. Returning", event_low))
    return(event_low) #This is a hole within an event of greater than the minimum depth, and should be counted
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
#' @seealso \itemize{
#'      \code{\link{marsFetchBaroData}},
#'      data: \code{\link{marsSampleBaro}}
#'  }   
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
#' 
#' @examples
#' 
#'  data(marsSampleBaro)
#' 
#'  marsInterpolateBaro(
#'    baro_psi = marsSampleBaro[[1]]$baro_psi, 
#'    smp_id = marsSampleBaro[[1]]$smp_id, 
#'    weight = marsSampleBaro[[1]]$weight, 
#'    target_id = marsSampleBaro[[2]]
#'    )
#' 


marsInterpolateBaro <- function(baro_psi, smp_id, weight, target_id){

 # browser()
  if (target_id %in% smp_id){
    return(baro_psi[which(target_id == smp_id)])
  } else {
    return(ifelse(length(baro_psi) >=4,
                  sum(baro_psi *weight)/sum(weight),
                  NA)
    )
  }
}



# yday_decimal ------------------------------------------------------------

#' Fetch a decimal day from a datetime
#'
#' Return the day of the year, with hours and seconds as the decimal
#'
#' @param dtime_est POSIXct, format: "POSIXct, format: "YYYY-MM-DD HH:MM:SS""
#' 
#' @return Output with be a day with a decimal
#' 
#' @export



yday_decimal <- function(dtime_est){

  lubridate::yday(dtime_est) + lubridate::hour(dtime_est)/24 + lubridate::minute(dtime_est)/(24*60) + lubridate::second(dtime_est)/(24*60*60)
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
#' @return Output will be a dataframe with four columns: 
#'   
#'     \item{dtime_est}{POSIXct, format: "YYYY-MM-DD HH:MM:SS"}
#'     \item{baro_psi}{num, barometric pressure in psi}
#'     \item{smp_id}{chr, SMP ID for each baro}
#'     \item{neighbors}{num, count of baros interpolated from}
#'     
#'     If the target SMP has an on-site baro with data, the "neighbors" column will be NA.
#'     If there are fewer than five baros to interprolate from, based on \code{\link{marsInterpolateBaro}},
#'     all columns other than "dtime_est" will be NA.
#'     
#'     The function will also output and open an html document describe the data request, and including
#'     a raster plot of barometric pressures. 
#' 
#' @export
#'
#' @seealso \code{\link{marsInterpolateBaro}}
#'
#'

marsFetchBaroData <- function(con, target_id, start_date, end_date, data_interval = c("5 mins", "15 mins")){
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }


  #Get SMP locations, and the locations of the baro sensors
  smp_loc <- odbc::dbGetQuery(con, "SELECT * FROM public.smp_loc")
  locus_loc <- dplyr::filter(smp_loc, smp_id == target_id)
  baro_smp <- odbc::dbGetQuery(con, "SELECT DISTINCT smp_id FROM public.baro_rawfile;") %>% dplyr::pull(smp_id)

  #browser()
  #Collect baro data
  #Get all baro data for the specified time period
  baro <- odbc::dbGetQuery(con, paste0("SELECT * FROM barodata_smp b WHERE b.dtime_est >= '", start_date, "'", " AND b.dtime_est <= '", end_date + lubridate::days(1), "' order by dtime_est;"))
  
  baro_latest_dtime <- odbc::dbGetQuery(con, paste0("SELECT max(dtime_est) FROM baro WHERE dtime_est < '", end_date + lubridate::days(1), "'")) %>% dplyr::pull()
  baro_latest_valid <- odbc::dbGetQuery(con, paste0("SELECT max(dtime_est) FROM barodata_neighbors WHERE neighbors >= 4 and dtime_est < '", end_date + lubridate::days(1), "'")) %>% dplyr::pull()
  
  if(length(baro$dtime_est) == 0){
    stop (paste0("No data available in the reqested interval. The latest available baro data is from ", baro_latest_dtime, "."))
  }
  
  #browser()
  #this is a seperate pipe so that it could be stopped before the error
  needs_thickening <- baro$dtime_est %>% lubridate::second() %>% {. > 0} %>% any() == TRUE
  if(needs_thickening == TRUE){
    baro %<>% padr::thicken(interval = "5 mins", rounding = "down") 
  
    baro %<>% dplyr::group_by(dtime_est_5_min, smp_id)
  
    baro %<>% dplyr::summarize(baro_psi = max(baro_psi, na.rm = TRUE)) %>% dplyr::select(dtime_est = dtime_est_5_min, smp_id, baro_psi) %>% dplyr::ungroup()
  }else{
    baro %<>% dplyr::group_by(dtime_est, smp_id)
    
    baro %<>% dplyr::summarize(baro_psi = max(baro_psi, na.rm = TRUE)) %>% dplyr::select(dtime_est, smp_id, baro_psi) %>% dplyr::ungroup()
  }
  
  baro$dtime_est %<>% lubridate::force_tz(tz = "EST")
  
  #initialize countNAs_t in case the loop doesn't run. It is passed as a param to markdown so it needs to exist. 
  countNAs_t <- 0 

  #When the user requests data at a 5-minute resolution, we need to stretch our 15-minute data into 5-minute data
  #We can use tidyr::spread and padr::pad to generate the full 5 minute time series,
  #And then use zoo::na.locf (last observation carried forward) to fill the NAs with the most recent value
  if(data_interval == "5 mins"){
    
    #Spread data to have all baro measurements use the same dtime_est column
    #So we can pad every 15-minute time series at once
    baro <- tidyr::spread(baro, "smp_id", "baro_psi")
    
    #Pad installs 5 minute intervals in our 15 minute dtime_est column. All other columns become NA
    #End value is 10 minutes after the final period because that 15 minute data point is good for 10 more minutes
    baro_pad <- padr::pad(baro, start_val = min(baro$dtime_est), end_val = max(baro$dtime_est) + lubridate::minutes(10), interval = "5 mins")
    
    #To count the LOCF operations, we count the NAs in the data frame before and after the LOCF
    countNAs <- baro_pad[1,]
    for(i in 2:ncol(baro_pad)){
      countNAs[,i] <- sum(is.na(baro_pad[,i])) #count NAs before they are filled
      baro_pad[,i] <- zoo::na.locf(baro_pad[,i], maxgap = 2, na.rm = FALSE) #maxgap = 2 means only fill NAs created by the pad
      countNAs[,i] <- countNAs[,i]- sum(is.na(baro_pad[,i])) #subtract remaining NAs to get number of NAs filled
    }
    
    countNAs %<>% dplyr::select(-dtime_est)
    countNAs_t <- countNAs %>% t() %>% data.frame() %>% tibble::rownames_to_column() %>%  magrittr::set_colnames(c("Location", "No. of LOCFs"))
    
    #Return baro data to long data format
    baro <- tidyr::gather(baro_pad, "smp_id", "baro_psi", -dtime_est) %>%
      dplyr::filter(!is.na(baro_psi))
  }

  #Calculate the distance between every baro location and the target SMP, then add weight
  baro_weights <- dplyr::filter(smp_loc, smp_id %in% baro_smp) %>%
    dplyr::mutate(lon_dist = lon_wgs84 - locus_loc$lon_wgs84,
                  lat_dist = lat_wgs84 - locus_loc$lat_wgs84,
                  dist_total = sqrt(abs(lon_dist**2 - lat_dist**2))) %>%
    dplyr::mutate(weight = 1/dist_total) %>% #inverse distance weight with power = 1
    dplyr::select(smp_id, weight) %>%
    dplyr::arrange(smp_id)
  
  interpolated_baro <- dplyr::left_join(baro, baro_weights, by = "smp_id") %>% #join baro and weights
    dplyr::group_by(dtime_est) %>% #group datetimes, then calculate weighting effect for each datetime
    dplyr::summarize(baro_psi = marsInterpolateBaro(baro_psi, smp_id, weight, target_id),
                     smp_id = ifelse(target_id %in% smp_id, target_id, "Interpolated"),
                     neighbors = ifelse(target_id %in% smp_id, NA, dplyr::n()))
  
  #Initialize Final Series
  finalseries <- interpolated_baro
  
  
  #Give 5 or 15 minute data as appropriate
  if(data_interval == "15 mins"){
    clippedseries <- data.frame(dtime_est = seq.POSIXt(from = start_date, to = end_date + lubridate::days(1), by = data_interval) )
    
    finalseries <- dplyr::filter(finalseries, dtime_est %in% clippedseries$dtime_est)
    baro <- dplyr::filter(baro, dtime_est %in% clippedseries$dtime_est)
  }
  
  
  #Adding "neighbor" counts and instances to report
  neighbors <- dplyr::group_by(finalseries, neighbors) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    magrittr::set_colnames(c("Neighbors", "Count"))
  
  ##finalseries is now ready, but return() must happen after plot and markdown are created
  
  #Baro Raster Plot
  #Get elevations #This has been removed in favor of using distances to sort SMPs on plot. Code is left in case 
  #baro_elev <- odbc::dbGetQuery(con, "SELECT * FROM smp_elev") %>% filter(smp_id %in% baro$smp_id)
  
  #Bind all raw baro data with interpolated data, and add weights
  baro_p <- dplyr::bind_rows(baro, finalseries)
  baro_p <- dplyr::left_join(baro_p, baro_weights, by = "smp_id") 
  
  #Set NA weights to max weight +1 so interpolated data plots at the top of the chart
  baro_p$weight[is.na(baro_p$weight)] <- max(baro_p$weight)+1
  
  #Add year and day for chart
  baro_p %<>% dplyr::mutate("day" = yday_decimal(baro_p$dtime_est),
                     "year" = lubridate::year(baro_p$dtime_est))
  
  #Sort SMP IDs by elevation
  baro_p$smp_id <- factor(baro_p$smp_id, levels = unique(baro_p$smp_id[order(baro_p$weight)]))
  
  baro_p$smp_id %<>% as.factor
  
  #Create baro Raster Chart
  p <- baroRasterPlot(baro_p)
  
  #Create baro Map
  baro_loc <- smp_loc %>% dplyr::filter(smp_id %in% baro$smp_id)
  rownames(baro_loc) <- NULL
  coords <- baro_loc[c("lon_wgs84", "lat_wgs84")]
  baro_sp <- sp::SpatialPointsDataFrame(coords, data = baro_loc, proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  coords <- locus_loc[c("lon_wgs84", "lat_wgs84")]
  smp_sp <- sp::SpatialPointsDataFrame(coords, data = locus_loc, proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  baro_map <- mapview::mapview(baro_sp, layer.name = "Baro") + mapview::mapview(smp_sp, color = "red", col.regions = NA, layer.name = "Target SMP")
  
  downloader_folder <- ("//pwdoows/oows/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader")
  
  
  #render markdown document
  #output file and output dir arguments do not work, so file is placed where markdown document is, and moved later
  rmarkdown::render(system.file("rmd", "baro.rmd", package = "pwdgsi"), #find .rmd location on local cpu
                    params = list(smp_id = smp_id,  #input parameters to be passed into markdown body
                                  start_date = start_date,
                                  end_date = end_date,
                                  data_interval = data_interval,
                                  neighbors = neighbors,
                                  countNAs = countNAs_t,
                                  p = p,
                                  csv_name = paste0(downloader_folder, "/", paste(smp_id, start_date, "to", end_date, sep = "_"), ".csv"),
                                  map = baro_map, 
                                  baro_latest_dtime = baro_latest_dtime, 
                                  baro_latest_valid = baro_latest_valid))
  
  #give a new filename and path
  new_filename <- paste0(downloader_folder, "/Reports/", paste(smp_id, start_date, "to", end_date, sep = "_"), "_baro_report.html")
  
  #move file to Baro Data Downloader/Reports folder
  file.rename(from = paste0(system.file("rmd", "baro.html", package = "pwdgsi")), 
              to = new_filename)
  
  new_filename_drive <- paste0("O:/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader/Reports/", paste(smp_id, start_date, "to", end_date, sep = "_"), "_baro_report.html")
  #open html 
  browseURL(new_filename_drive)
  
  #return Final Series. 
  return(finalseries)
  
}