#Data Fetch and Write Functions

#marsFetchPrivateSMPRecords---------------------
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


marsFetchPrivateSMPRecords <- function(con, tracking_number){
  #Validate DB connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }

  #Rather than validating each individual tracking number and selecting them one at a time
  #We can grab the entire table and filter by our tracking numbers to find the valid ones
  planreviewtable <- odbc::dbGetQuery(con, "select p.\"TrackingNumber\" as tracking_number, p.\"Projectname\" as project_name, p.\"SMPID\" as smp_id, p.\"Plan Label\" as plan_label from external.tbl_planreview_crosstab p")
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

# marsFetchRainfallData ------------------------------------------
#' Return a dataframe with rain gage data
#'
#' Return data from the rain gage nearest a target SMP, for a specified date range.
#'
#' @param con Formal class 'PostgreSQL', a connection to the MARS Analysis database
#' @param target_id chr, an SMP_ID that where the user has requested data
#' @param source chr, either "gage" or "radar" to retrieve rain gage data or radar rainfall data
#' @param start_date string or POSIXCT date, format: "YYYY-MM-DD", start of data request range
#' @param end_date stringor POSIXCT date, format: "YYYY-MM-DD", end of data request range
#' @param daylightsavings logi, Adjust for daylight savings time? when doing QAQC
#'   this should be \code{FALSE} because the water level data does not spring forwards.
#'
#' @return Output will be a data frame with four columns, which corresponds to the specified SMP and date range:
#' 
#'   \item{dtime_est OR dtime_edt}{POSIXct datetime with tz = EST or EDT as specified by \code{daylight_savings}}
#'   \item{rainfall_in}{num, rainfall for the 15 minute preceding the corresponding datetime}
#'   \item{gage_uid OR radar_uid}{Unique identifier for where the data came from}
#'   \item{event_id}{event number during this timestep}
#' 
#' 
#' @seealso \code{\link[pwdgsi]{marsGapFillEventID}}, \code{\link{marsDetectEvents}}
#'
#' @export

marsFetchRainfallData <- function(con, target_id, source = c("gage", "radar"), start_date, end_date, daylightsavings){
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  # browser()
  start_date %<>% as.POSIXct(format = '%Y-%m-%d')
  end_date %<>% as.POSIXct(format = '%Y-%m-%d')
  
  # Was a string supplied to source?
  if( isTRUE(all.equal(source, c("gage","radar"))) ){
    stop("No argument supplied for 'source'. Provide a string of either 'gage' or 'radar'")
  }
  
  #Are we working with gages or radarcells?
  if(source == "gage"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_gage", raintable = "data.viw_gage_rainfall", uidvar = "gage_uid", loctable = "admin.tbl_gage", eventuidvar = "gage_event_uid", stringsAsFactors=FALSE)
  } else if(source == "radar"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_radar", raintable = "data.viw_radar_rainfall", uidvar = "radar_uid", loctable = "admin.tbl_radar", eventuidvar = "radar_event_uid", stringsAsFactors=FALSE)
  } else { #Parameter is somehow invalid
    stop("Argument 'source' is not one of 'gage' or 'radar'")
  }

  #Get closest rainfall source
  smp_query <- paste0("SELECT * FROM ", rainparams$smptable)
  #print(smp_query)
  rainsource <- odbc::dbGetQuery(con, smp_query) %>% dplyr::filter(smp_id == target_id) %>% dplyr::pull(rainparams$uidvar)
  
  #Collect gage data
  #First, get all the relevant data from the closest gage
  rain_query <- paste(paste0("SELECT *, dtime_edt::varchar as dtime FROM ", rainparams$raintable, " "),
                      paste0("WHERE ", rainparams$uidvar, " = CAST('", rainsource, "' as int)"),
                      "AND dtime_edt >= Date('", start_date, "')",
                      "AND dtime_edt <= Date('", end_date + lubridate::days(1), "');")
  
  #print(rain_query)
  rain_temp <- odbc::dbGetQuery(con, rain_query)
  
  if(nrow(rain_temp) == 0){
    
    if(lubridate::month(start_date) == lubridate::month(lubridate::today())){
      stop(paste("Rainfall data appears in the MARS database on about a 5 week delay. \nData for", lubridate::month(start_date, label = TRUE, abbr = FALSE), "should be available in the second week of", lubridate::month(lubridate::today() + lubridate::dmonths(1), label = TRUE, abbr = FALSE)))
    }
    stop("There is no data in the database for this date range.")
  }
  
  rain_temp$rainfall_in %<>% as.numeric
  # rain_temp$dtime_edt %<>% lubridate::ymd_hms(tz = "America/New_York")
  # we need to reformat dates at midnight because ymd_hms does not know how to handle them
  # this is actually a base R issue, but it is still dumb
  # https://github.com/tidyverse/lubridate/issues/1124
  rain_temp %<>% dplyr::mutate(dtime_est = lubridate::ymd_hms(dtime, tz = "EST")) %>% dplyr::select(-dtime_edt, -dtime)
  
  #Apparently, attempting to set the time zone on a datetime that falls squarely on the spring forward datetime
  #Such as 2005-04-03 02:00:00
  #Returns NA, because the time is impossible.
  #I hate this so, so much
  #To mitigate this, we will strip NA values from the new object
  # rain_temp %<>% dplyr::filter(!is.na(dtime_edt)) %>% dplyr::arrange(dtime_edt)
  rain_temp %<>% dplyr::filter(!is.na(dtime_est)) %>% dplyr::arrange(dtime_est)
  
  #Our water level data is not corrected for daylight savings time. ie it doesn't spring forwards
  #So we must shift back any datetimes within the DST window
  #Thankfully, the dst() function returns TRUE if a dtime is within that zone
  # if(daylightsavings == FALSE){
  #   dst_index <- lubridate::dst(rain_temp$dtime_edt)
  #   rain_temp$dtime_edt %<>% lubridate::force_tz("EST") #Assign new TZ without changing dates
  #   rain_temp$dtime_edt[dst_index] <- rain_temp$dtime_edt[dst_index] - lubridate::hours(1)
  # }
  
  #Punctuate data with zeroes to prevent linear interpolation when plotting
  #If the time between data points A and B is greater than 15 minutes (the normal timestep), we must insert a zero 15 minutes after A
  #If it's greather than 30 minutes, we must insert a zero 15 minutes before B also
  
  #First, create data frame to contain zero fills with same column names as our rain data
  zeroFills <- rain_temp[0,]
  #print("Begin zero-filling process")
  for(i in 1:(nrow(rain_temp) - 1)){
    # k <- difftime(rain_temp$dtime_edt[i+1], rain_temp$dtime_edt[i], units = "min")
    k <- difftime(rain_temp$dtime_est[i+1], rain_temp$dtime_est[i], units = "min")    
    
    #If gap is > 15 mins, put a zero 15 minutes after the gap starts
    if(k > 15){
      
      
      zeroFillIndex <- nrow(zeroFills)+1
      
      #Boundaries of the interval to be zeroed
      boundary.low <- rain_temp$dtime_est[i]
      boundary.high <- rain_temp$dtime_est[i+1]
      # boundary.low <- rain_temp$dtime_edt[i]
      # boundary.high <- rain_temp$dtime_edt[i+1]
      
      #The zero goes 15 minutes (900 seconds) after the first boundary
      #Filled by index because R is weird about partially filled data frame rows
      fill <- boundary.low + lubridate::seconds(900)
      #these were causing several functions to crash. Need a way to index so this doesn't happen again.
      zeroFills[zeroFillIndex, 5] <- fill                   #dtime_edt
      zeroFills[zeroFillIndex, 3] <- 0                      #rainfall_in
      zeroFills[zeroFillIndex, 2] <- rainsource   #gage_uid or radarcell_uid
      # browser()
      # print(paste("Gap-filling event ID. Before:", rain_temp$event[i], "After:", rain_temp$event[i+1]))
      zeroFills[zeroFillIndex, 5] <- marsGapFillEventID(event_low = rain_temp[i, 5], event_high = rain_temp[i+1, 5]) #event
      
      #If the boundary is longer than 30 minutes, we need a second zero
      if(k > 30){
        
        #This zero goes 15 minutes before the upper boundary
        fill <- boundary.high - lubridate::seconds(900)
        zeroFills[zeroFillIndex + 1, 2] <- fill                   #dtime_edt
        zeroFills[zeroFillIndex + 1, 4] <- 0                      #rainfall_in
        zeroFills[zeroFillIndex + 1, 3] <- rainsource   #gage_uid or radarcell_uid
        
        #print(paste("Gap-filling event ID. Before:", rain_temp[i, 5], "After:", rain_temp[i+1, 5]))
        zeroFills[zeroFillIndex + 1, 5] <- marsGapFillEventID(event_low = rain_temp[i, 5], event_high = rain_temp[i+1, 5]) #event
        
      }
      
    }
  }

  #Replace UIDs with SMP IDs
  rainlocs <- odbc::dbGetQuery(con, paste0("SELECT * FROM ", rainparams$loctable))
  # finalseries <- dplyr::bind_rows(rain_temp, zeroFills) %>%
  #   dplyr::left_join(rainlocs, by = rainparams$uidvar) %>%
  #   dplyr::select(dtime_edt, rainfall_in, rainparams$uidvar, rainparams$eventuidvar) %>%
  #   dplyr::arrange(dtime_edt)
  finalseries <- dplyr::bind_rows(rain_temp, zeroFills) %>%
    dplyr::left_join(rainlocs, by = rainparams$uidvar) %>%
    dplyr::select(dtime_est, rainfall_in, rainparams$uidvar, rainparams$eventuidvar) %>%
    dplyr::arrange(dtime_est)
  
  #round date to nearest minute
  finalseries$dtime_est %<>% lubridate::round_date("minute")
  
  #Rename dtime column if we are undoing daylight savings time
  # if(daylightsavings == FALSE){
  #   finalseries <- finalseries %>%
  #     dplyr::mutate(dtime_est = dtime_edt) %>%
  #     dplyr::select(-dtime_edt)
  #   finalseries <- dplyr::select(finalseries, dtime_est, rainfall_in, rainparams$uidvar, rainparams$eventuidvar)
  # }
  
  return(finalseries)
}

# marsGapFillEventID -----------------------
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
#' rainfall that precede and follow them, given by \code{\link{marsDetectEvents}}.
#'
#' @param event_low num, event ID of preceding rainfall.
#' @param event_high num, event ID of following rainfall.
#'
#' @return Output will be a vector containing either \code{NA} or \code{event low}. If one or both events are
#' \code{NA}, return {NA}. If the event IDs are not equal, return \code{NA}, since this is a boundary between
#' events. If the event IDs are equal, return \code{event low}.

marsGapFillEventID <- function(event_low, event_high){
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
#' Returns an interpolated barometric pressure reading or \code{NA}
#'
#' @seealso \itemize{
#'      \code{\link{marsFetchBaroData}},
#'      data: \code{\link{marsSampleBaro}}
#'  }   
#'
#' @param baro_psi vector, num, barometric pressures measured at the same timestamp
#' @param smp_id vector, chr, SMP IDs where the measurements took place
#' @param weight vector, num, of inverse distances weights for each baro, calculated by \code{\link{marsFetchBaroData}}
#' @param target_id chr, single SMP ID where the user has requested data
#'
#' @return Output will be a single barometric pressure reading.
#'   If there are 4 or greater baros with data,
#'   the reading will be an inverse distance-weighted
#'   interpolation of those readings.
#'   If there are fewer than 4 readings, return \code{NA}.
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

 if(length(baro_psi) >= 1){
   return(sum(baro_psi*weight)/sum(weight))
 }else{
   return(NA)
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
#' @param start_date string or POSIXct, format: "YYYY-MM-DD", start of data request range
#' @param end_date string or POSIXct, format: "YYYY-MM-DD", end of data request range
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
#'     The function will also output and open an html document describing the data request and including
#'     a raster plot of barometric pressures. (This feature is currently disabled. 2/2/21 - NM)
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


  #Handle date Conversion
  start_date %<>% as.POSIXct(format = '%Y-%m-%d')
  end_date %<>% as.POSIXct(format = '%Y-%m-%d')
  
  #Get SMP locations, and the locations of the baro sensors
  smp_loc <- odbc::dbGetQuery(con, "SELECT * FROM admin.tbl_smp_loc")
  locus_loc <- dplyr::filter(smp_loc, smp_id == target_id)
  baro_smp <- odbc::dbGetQuery(con, "SELECT DISTINCT smp_id FROM admin.tbl_baro_rawfile;") %>% dplyr::pull(smp_id)

  #Collect baro data
  #Get all baro data for the specified time period
  baro <- odbc::dbGetQuery(con, paste0("SELECT * FROM data.viw_barodata_smp b WHERE b.dtime_est >= '", start_date, "'", " AND b.dtime_est <= '", end_date + lubridate::days(1), "' order by dtime_est;"))
  
  baro_latest_dtime <- odbc::dbGetQuery(con, paste0("SELECT max(dtime_est) FROM data.tbl_baro WHERE dtime_est < '", end_date + lubridate::days(1), "'")) %>% dplyr::pull()
  baro_latest_valid <- odbc::dbGetQuery(con, paste0("SELECT max(dtime_est) FROM data.viw_barodata_neighbors WHERE neighbors >= 4 and dtime_est < '", end_date + lubridate::days(1), "'")) %>% dplyr::pull()
  
  if(length(baro$dtime_est) == 0){
    stop (paste0("No data available in the reqested interval. The latest available baro data is from ", baro_latest_dtime, "."))
  }
  
  #this is a seperate pipe so that it could be stopped before the error
  needs_thickening <- baro$dtime_est %>% lubridate::second() %>% {. > 0} %>% any() == TRUE
  if(needs_thickening == TRUE){
    baro %<>% padr::thicken(interval = "5 mins", rounding = "down") %>% 
      dplyr::group_by(dtime_est_5_min, smp_id) %>% 
      dplyr::summarize(baro_psi = max(baro_psi, na.rm = TRUE)) %>% 
      dplyr::select(dtime_est = dtime_est_5_min, smp_id, baro_psi) %>% 
      dplyr::ungroup()
  }else{
    baro %<>% dplyr::group_by(dtime_est, smp_id) %>% 
      dplyr::summarize(baro_psi = max(baro_psi, na.rm = TRUE)) %>% 
      dplyr::select(dtime_est, smp_id, baro_psi) %>% 
      dplyr::ungroup()
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
  
  #Cap weight at 1000
  baro_weights$weight <- replace(baro_weights$weight, baro_weights$weight > 1000, 1000)
  
  interpolated_baro <- dplyr::left_join(baro, baro_weights, by = "smp_id") %>% #join baro and weights
    dplyr::group_by(dtime_est) %>% #group datetimes, then calculate weighting effect for each datetime
    dplyr::summarize(baro_psi = marsInterpolateBaro(baro_psi, smp_id, weight, target_id),
                     smp_id =  "Interpolated",
                     neighbors = dplyr::n()) %>% 
    zoo::na.trim(sides = "right") #trim trailing NAs
  
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
  
  #currently disabled (2/2/21)
  
  # #Bind all raw baro data with interpolated data, and add weights
  # baro_p <- dplyr::bind_rows(baro, finalseries)
  # baro_p <- dplyr::left_join(baro_p, baro_weights, by = "smp_id") 
  # 
  # #Set NA weights to max weight +1 so interpolated data plots at the top of the chart
  # baro_p$weight[is.na(baro_p$weight)] <- max(baro_p$weight)+1
  # 
  # #Sort SMP IDs by elevation
  # baro_p$smp_id <- factor(baro_p$smp_id, levels = unique(baro_p$smp_id[order(baro_p$weight)]))
  # 
  # #Add year and day for chart
  # baro_p %<>% dplyr::mutate("day" = yday_decimal(baro_p$dtime_est),
  #                    "year" = lubridate::year(baro_p$dtime_est))
  # 
  # 
  # baro_p$smp_id %<>% as.factor
  # 
  # #Create baro Raster Chart
  # p <- marsBaroRasterPlot(baro_p)
  
  
  #Create baro Map
  # baro_loc <- smp_loc %>% dplyr::filter(smp_id %in% baro$smp_id)
  # rownames(baro_loc) <- NULL
  # coords <- baro_loc[c("lon_wgs84", "lat_wgs84")]
  # baro_sp <- sp::SpatialPointsDataFrame(coords, data = baro_loc, proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  # coords <- locus_loc[c("lon_wgs84", "lat_wgs84")]
  # smp_sp <- sp::SpatialPointsDataFrame(coords, data = locus_loc, proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  # baro_map <- mapview::mapview(baro_sp, layer.name = "Baro") + mapview::mapview(smp_sp, color = "red", col.regions = NA, layer.name = "Target SMP")
  # 
  downloader_folder <- "O:/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader"
  # downloader_folder_csv <- "\\\\\\\\pwdoows\\\\oows\\\\Watershed Sciences\\\\GSI Monitoring\\07 Databases and Tracking Spreadsheets\\13 MARS Analysis Database\\\\Scripts\\\\Downloader\\\\Baro Data Downloader\\\\"
  # 
  #render markdown document
  #output file and output dir arguments do not work, so file is placed where markdown document is, and moved later
  
  #markdown is currently disabled
  
  # rmarkdown::render(system.file("rmd", "baro.rmd", package = "pwdgsi"), #find .rmd location on local cpu
  #                   params = list(smp_id = target_id,  #input parameters to be passed into markdown body
  #                                 start_date = start_date,
  #                                 end_date = end_date,
  #                                 data_interval = data_interval,
  #                                 neighbors = neighbors,
  #                                 countNAs = countNAs_t,
  #                                 p = p,
  #                                 map = baro_map,
  #                                 baro_latest_dtime = baro_latest_dtime,
  #                                 baro_latest_valid = baro_latest_valid))

  # #give a new filename and path
  # new_filename <- paste0(downloader_folder, "/Reports/", paste(target_id, start_date, "to", end_date, sep = "_"), "_baro_report.html")
  # 
  # #move file to Baro Data Downloader/Reports folder
  # file.rename(from = paste0(system.file("rmd", "baro.html", package = "pwdgsi")),
  #             to = new_filename)
  # 
  # #open html
  # browseURL(new_filename)
  
  #return Final Series. 
  return(finalseries)
  
}

# marsFetchSMPSnapshot --------------------------------

#' Fetch data snapshot for an SMP
#'
#' Returns a data frame with requested snapshote date, SMP ID, OW suffix, and SMP snapshot
#'   
#' @param con An ODBC connection to the MARS Analysis database returned by odbc::dbConnect
#' @param smp_id vector of chr, SMP ID, where the user has requested data
#' @param ow_suffix vector of chr, OW Suffix, where the user has requested data
#' @param request_date single date or vector the length of SMP ID, either "YYYY-MM-DD" or "today", of the request data
#'
#' @return Output will be a dataframe with the following columns: 
#'   
#'     \item{smp_id}{chr, requested SMP ID}
#'     \item{ow_suffix}{chr, request OW suffix}
#'     \item{ow_uid}{num, ow_uid derived from smp_id and ow_suffix} 
#'     \item{request_date}{chr, date of requested snapshot, format: "YYYY-MM-DD}
#'     \item{snapshot_uid}{num}
#'     \item{dcia_ft2}{num}
#'     \item{storage_footprint_ft2}{num}
#'     \item{orifice_diam_in}{num}
#'     \item{infil_footprint_ft2}{num}
#'     \item{assumption_orificeheight_ft}{num}
#'     \item{storage_depth_ft}{num}
#'     \item{sumpdepth_ft}{num}
#'     \item{lined}{chr, 0 or 1 for unlined or lined, respectively}
#'     \item{surface}{chr, 0 or 1 for subsurface or surface, respectively}
#'     
#'     If a requested smp_id and ow_suffix combination is not valid, the function will return NAs for that snapshot.
#'     Function queries the SQL function "insert_snapshot", which inserts rows into the MARS table "snapshot", and, in turn, "snapshot_metadata".
#'     
#' @export
#'

marsFetchSMPSnapshot <- function(con, smp_id, ow_suffix, request_date){
  
  #1 Argument Validation
  #1.1 Check database connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  #1.2 check argument lengths
  if(length(smp_id) != length(ow_suffix)){
    stop("smp_id and ow_suffix must be of equal length")
  }
  
  if(length(smp_id) != length(request_date) & length(request_date) != 1){
    stop("request_date must be a single date, or equal length to smp_id and ow_suffix")
  }
  
  #1.3 Assign today() to 'today'
  request_date <- stringr::str_replace(request_date, "today", as.character(lubridate::today()))
  
  #1.4 Check if smp_id and ow_suffix combination are valid
  #1.4.1 Create dataframe
  request_df <- data.frame(smp_id, ow_suffix, request_date, stringsAsFactors = FALSE)
  
  #1.4.2 Query fieldwork.tbl_ow and check if each smp id and observation well are there
  # Initialize dataframe
  ow_validity <- data.frame(ow_uid = numeric(), 
                            smp_id =  character(),  
                            ow_suffix = character(), 
                            stringsAsFactors = FALSE)
  
  # Check if smp_id and ow_suffix are in the MARS table "fieldwork.tbl_ow"
  # Return matches
  for(i in 1:length(request_df$smp_id)){
    ow_valid_check <- odbc::dbGetQuery(con, "SELECT * FROM fieldwork.tbl_ow") %>% dplyr::select(-facility_id) %>%  dplyr::filter(smp_id == request_df$smp_id[i] & ow_suffix == request_df$ow_suffix[i])
    ow_validity <- dplyr::bind_rows(ow_validity, ow_valid_check)
  }
  
  # Join dates back to observation wells and ow_uids back to request criteria
  ow_validity %<>% dplyr::left_join(request_df, by = c("smp_id", "ow_suffix")) 
  request_df_validity <- dplyr::left_join(request_df  %>% dplyr::select(-request_date), ow_validity, by = c("smp_id", "ow_suffix"))
  
  #2 Query
  #2.1 initialize dataframe
  result <- data.frame("snapshot_uid" = numeric(),
                       "ow_uid" = numeric(),
                       "dcia_ft2" = numeric(),
                       "storage_footprint_ft2" = numeric(), 
                       "orifice_diam_in" = numeric(),
                       "infil_footprint_ft2" = numeric(),
                       "assumption_orificeheight_ft" = numeric(),
                       "storage_depth_ft" = numeric(),
                       "sumpdepth_ft" = numeric(),
                       #"lined" = logical(), 
                       #"surface" = logical(), 
                       stringsAsFactors = FALSE)
  
  
  #2.2 Run get_arbitrary_snapshot in a loop and bind results
  for(i in 1:length(ow_validity$smp_id)){
    snapshot_query <- paste0("SELECT * FROM metrics.fun_get_arbitrary_snapshot('", ow_validity$smp_id[i], "','", ow_validity$ow_suffix[i], "','", ow_validity$request_date[i], "') ORDER BY snapshot_uid DESC LIMIT 1")
    new_result <- odbc::dbGetQuery(con, snapshot_query)
    result <- dplyr::bind_rows(result, new_result)
  }
  
  #3 Join and return
  #3.1 Join results to request criteria
  snapshot_results <- request_df_validity %>% dplyr::left_join(result, by = "ow_uid") 
  
  #3.2 Return results
  return(snapshot_results)
}

# marsFetchLevelData --------------------------------

#' Fetch water level data for an SMP
#'
#' Returns a data frame with requested SMP water level data
#'   
#' @param con An ODBC connection to the MARS Analysis database returned by odbc::dbConnect
#' @param target_id vector of chr, SMP ID, where the user has requested data
#' @param ow_suffix vector of chr, SMP ID, where the user has requested data
#' @param start_date string, format: "YYYY-MM-DD", start of data request range
#' @param end_date string, format: "YYYY-MM-DD", end of data request range
#' @param sump_correct logical, TRUE if water level should be corrected for to account for sump depth
#'
#' @return Output will be a dataframe with the following columns: 
#' 
#'     \item{ow_leveldata_uid}{int}
#'     \item{dtime_est}{POSIXct datetime}
#'     \item{level_ft}{num, recorded water level in feet}
#'     \item{ow_uid}{num, ow_uid derived from smp_id and ow_suffix} 
#'     
#' @export
#' 
#' @seealso \code{\link{marsFetchRainfallData}}, \code{\link{marsFetchRainEventData}}, \code{\link{marsFetchMonitoringData}}
#' 
marsFetchLevelData <- function(con, target_id, ow_suffix, start_date, end_date, sump_correct){
  
  #1 Argument Validation
  #1.1 Check database connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  
  #1.2 Check if smp_id and ow_suffix are in the MARS table "ow_validity"
  # Return match
  validity_query <- paste0("select * from fieldwork.fun_get_ow_uid('",target_id,"','",ow_suffix,"', NULL)")
  ow_uid <- odbc::dbGetQuery(con, validity_query)
  
  #1.3 Pick which table to query
  if(stringr::str_replace(ow_suffix, ".$", "") %in% c("CW", "GW", "PZ")){
    level_table <- "data.tbl_gw_depthdata_raw"
  }else if(!(stringr::str_replace(ow_suffix, ".$", "") %in% c("CW", "GW", "PZ")) & sump_correct == TRUE){
    level_table <- "data.viw_ow_leveldata_sumpcorrected"
  }else if(!(stringr::str_replace(ow_suffix, ".$", "") %in% c("CW", "GW", "PZ")) & sump_correct == FALSE){
    level_table <- "data.tbl_ow_leveldata_raw"
  }
  start_date %<>% as.POSIXct(format = '%Y-%m-%d')
  end_date %<>% as.POSIXct(format = '%Y-%m-%d')
  
  #1.4 Add buffer to requested dates
  start_date <- lubridate::round_date(start_date) - lubridate::days(1)
  end_date <- lubridate::round_date(end_date) + lubridate::days(1)
  
  #2 Query database for level data
  leveldata_query <- paste0("select * from ", level_table, "
                                WHERE ow_uid = '", ow_uid, "'
                                AND dtime_est BETWEEN '",start_date,"' AND '", end_date, "'")
  
  leveldata <- odbc::dbGetQuery(con, leveldata_query) %>% dplyr::arrange(dtime_est)
  
  leveldata$dtime_est %<>% lubridate::force_tz(tz = "EST") %>% lubridate::round_date("minute")
  
  #3 Return level data
  return(leveldata)
}

# marsFetchRainEventData --------------------------------

#' Fetch rain event data for an SMP
#'
#' Returns a data frame with rain event data from the rain gage closest to the request SMP
#'   
#' @param con An ODBC connection to the MARS Analysis database returned by odbc::dbConnect
#' @param target_id vector of chr, SMP ID, where the user has requested data
#' @param source string, one of "gage" or "radar" to select rain gage events or radar rainfall events
#' @param start_date string, format: "YYYY-MM-DD", start of data request range
#' @param end_date string, format: "YYYY-MM-DD", end of data request range
#'
#' @return Output will be a dataframe with the following columns: 
#' 
#'     \item{rainfall_gage_event_uid}{int}
#'     \item{gage_uid OR radar_uid}{int, unique identifier for rain source, depending on value of source argument}
#'     \item{eventdatastart_edt}{POSIXct datetime}
#'     \item{eventdataend_edt}{POSIXct datetime}
#'     \item{eventduration_hr}{num, duration of event}
#'     \item{eventpeakintensity_inhr}{num, peak intensity of rain event} 
#'     \item{eventavgintensity_inhr}{num, average intensity of rain event} 
#'     \item{eventdepth_in}{num, average intensity of rain event} 
#'     
#' @export
#' 
#' @seealso \code{\link{marsFetchRainfallData}}, \code{\link{marsFetchLevelData}}, \code{\link{marsFetchMonitoringData}}
#' 
marsFetchRainEventData <- function(con, target_id, source = c("gage", "radar"), start_date, end_date){
  #1 Argument validation
  #1.1 Check database connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  #Sanitize start and end date
  start_date %<>% as.POSIXct(format = '%Y-%m-%d')
  end_date %<>% as.POSIXct(format = '%Y-%m-%d')
  
  # Was a string supplied to source?
  if( isTRUE(all.equal(source, c("gage","radar"))) ){
    stop("No argument supplied for 'source'. Provide a string of either 'gage' or 'radar'")
  }
  
  #Are we working with gages or radarcells?
  if(source == "gage"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_gage", eventtable = "data.tbl_gage_event", uidvar = "gage_uid", loctable = "admin.tbl_gage", eventuidvar = "gage_event_uid", stringsAsFactors=FALSE)
  } else if(source == "radar"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_radar", eventtable = "data.tbl_radar_event", uidvar = "radar_uid", loctable = "admin.tbl_radar", eventuidvar = "radar_event_uid", stringsAsFactors=FALSE)
  } else { #Parameter is somehow invalid
    stop("Argument 'source' is not one of 'gage' or 'radar'")
  }
  
  
  #2 Get closest rain source
  rainsource <- odbc::dbGetQuery(con, paste0("SELECT * FROM ", rainparams$smptable)) %>% 
    dplyr::filter(smp_id == target_id) %>%
    dplyr::pull(rainparams$uidvar)
  
  #2.1 Query event data
  event_query <- paste(paste0("SELECT * FROM ", rainparams$eventtable),
                       "WHERE", rainparams$uidvar, "= CAST('", rainsource, "' as int)",
                       "AND eventdatastart_edt >= Date('", start_date, "')",
                       "AND eventdataend_edt <= Date('", end_date + lubridate::days(1), "');")
  
  events <- odbc::dbGetQuery(con, event_query) 
  # making this "EST"
  events %<>% dplyr::mutate(eventdatastart_est = lubridate::force_tz(eventdatastart_edt,"EST"))
  events %<>% dplyr::mutate(eventdataend_est = lubridate::force_tz(eventdataend_edt,"EST"))
  events %<>% dplyr::select(-eventdatastart_edt,
                     -eventdataend_edt)
  
  #3 return event data
  return(events)
}

# marsFetchMonitoringData --------------------------------

#' Fetch monitoring data for an SMP
#'
#' Returns a list with data frames with rain data and rain event data from the rain gage closest to the request SMP, and level data
#'   
#' @param con An ODBC connection to the MARS Analysis database returned by odbc::dbConnect
#' @param target_id vector of chr, SMP ID, where the user has requested data
#' @param ow_suffix vector of chr, OW suffixes corresponding to SMP IDs, where the user has requested data
#' @param source string, one of "gage" or "radar" to select rain gage events or radar rainfall events
#' @param start_date string, format: "YYYY-MM-DD", start of data request range
#' @param end_date string, format: "YYYY-MM-DD", end of data request range
#' @param sump_correct logical, TRUE if water level should be corrected for to account for sump depth
#' @param rain_events logical, TRUE if rain event data should be included in result
#' @param rainfall logical, TRUE if rainfall data should be included in result
#' @param level logical, TRUE if water level should be included in result
#' @param daylight_savings logical, Adjust for daylight savings time? when doing QAQC this should be FALSE because the water level data does not spring forward 
#' @param debug logical, whether to print lookup times and outputs
#'
#' @return Output will be a list consisting of a combination of the following:
#' 
#'     \item{Rain Event Data}{dataframe, output from \code{\link{marsFetchRainEventData}}}
#'     \item{Rain Gage Data}{dataframe, output from \code{\link{marsFetchRainfallData}}}
#'     \item{Level Data}{dataframe, output from \code{\link{marsFetchLevelData}}, plus rainfall_gage_event_uids}
#'     
#' @export
#' 
#' @seealso \code{\link{marsFetchRainfallData}}, \code{\link{marsFetchLevelData}}, \code{\link{marsFetchRainEventData}}
#' 


marsFetchMonitoringData <- function(con, target_id, ow_suffix, source = c("gage", "radar"), start_date, end_date,
                                    sump_correct = TRUE, rain_events = TRUE, rainfall = TRUE, level = TRUE, daylight_savings = FALSE,
                                    debug = FALSE, browser = FALSE){

  if(browser == TRUE){
    browser()
  }
  
  #1 Argument validation
  #1.1 Check database connection
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  # Was a string supplied to source?
  if( isTRUE(all.equal(source, c("gage","radar"))) ){
    stop("No argument supplied for 'source'. Provide a string of either 'gage' or 'radar'")
  }
  
  #Are we working with gages or radarcells?
  if(source == "gage"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_gage", eventtable = "data.tbl_gage_event", uidvar = "gage_uid", loctable = "admin.tbl_gage", eventuidvar = "gage_event_uid", stringsAsFactors=FALSE)
  } else if(source == "radar"){
    rainparams <- data.frame(smptable = "admin.tbl_smp_radar", eventtable = "data.tbl_radar_event", uidvar = "radar_uid", loctable = "admin.tbl_radar", eventuidvar = "radar_event_uid", stringsAsFactors=FALSE)
  } else { #Parameter is somehow invalid
    stop("Argument 'source' is not one of 'gage' or 'radar'")
  }
  
  #2 Initialize list
  results <- list()
  
  #Get closest gage
  
  if(debug){
    ptm <- proc.time()
  }

  smp_rain <- odbc::dbGetQuery(con, paste0("SELECT * FROM ", rainparams$smptable)) %>% dplyr::filter(smp_id %in% target_id)
  ow_validity <- odbc::dbGetQuery(con, "SELECT * FROM fieldwork.tbl_ow")
  ow_uid_gage <- ow_validity %>% dplyr::right_join(smp_rain, by = "smp_id")
  
  if(debug){
  print(paste0(source, "_lookup_time: ", (proc.time()-ptm)[3]))
  }
  
  #Set datetime date types
  start_date %<>% as.POSIXct(format = '%Y-%m-%d')
  end_date %<>% as.POSIXct(format = '%Y-%m-%d')
  
  if(debug){
    ptm <- print(paste("date_time conversion time:", (proc.time()-ptm)[3]))
  }
  
  #3 Add rain events
  if(rain_events == TRUE){
    for(i in 1:length(target_id)){
      results[["Rain Event Data step"]] <- marsFetchRainEventData(con, target_id[i], source, start_date[i], end_date[i])
      start_date[i] <- min(results[["Rain Event Data step"]]$eventdatastart_est)
      end_date[i] <- max(results[["Rain Event Data step"]]$eventdataend_est)
      results[["Rain Event Data"]] <- dplyr::bind_rows(results[["Rain Event Data"]], results[["Rain Event Data step"]])
      results[["Rain Event Data step"]] <- NULL
    }
  }
  
  
  if(debug){
    ptm <- proc.time()
  }
  
  
  #4 Add Rainfall
  if(rainfall == TRUE){
    for(i in 1:length(target_id)){
      results[["Rainfall Data step"]] <- marsFetchRainfallData(con, target_id[i], source, start_date[i], end_date[i], daylight_savings)
      start_date[i] <- min(results[["Rainfall Data step"]]$dtime_est - lubridate::days(1), na.rm = TRUE)
      end_date[i] <- max(results[["Rainfall Data step"]]$dtime_est + lubridate::days(1), na.rm = TRUE)
      results[["Rainfall Data"]] <- dplyr::bind_rows(results[["Rainfall Data"]], results[["Rainfall Data step"]])
      results[["Rainfall Data step"]] <- NULL
    }
  }
  #####
  
  if(debug){
    print(paste("rainfall_lookup_time:", (proc.time()-ptm)[3]))
  }
  
  if(debug){
    ptm <- proc.time()
  }
  
  #5 Add level data
  if(level == TRUE){
    
    # Taylor update: 5/8/23
    #If rain events are being included, we need to switch the rain event dates to EST instead of EDT
    #But because this should only happen once, since the variable is being renamed, do it here instead of in the loop
    # if(rain_events == TRUE){
    #   results[["Rain Event Data"]]$eventdatastart_edt %<>% lubridate::with_tz("EST") #switch to EST and rename
    #   results[["Rain Event Data"]]$eventdataend_edt %<>% lubridate::with_tz("EST")
    #   results[["Rain Event Data"]] %<>% dplyr::rename(eventdatastart_est = eventdatastart_edt, eventdataend_est = eventdataend_edt)
    # }
    #Commented out - timezone is now switched in rainfall fx, handling pull requesst conflict - BC
    
    for(i in 1:length(target_id)){
      results[["Level Data step"]] <- marsFetchLevelData(con, target_id[i], ow_suffix[i], start_date[i], end_date[i], sump_correct) %>% 
        dplyr::left_join(ow_uid_gage, by = "ow_uid") %>%  #join rain gage uid
        dplyr::select(dtime_est, level_ft, ow_uid, rainparams$uidvar) #remove extra columns
      if(rain_events == TRUE){
        level_data_step <- results[["Level Data step"]] #coerce to data frame
        
        results_event_data <- results[["Rain Event Data"]]
        
        level_data_step <- level_data_step[(!is.na(level_data_step$dtime_est)),]
        
        #select relevant columns from the results
        results_event_data %<>% dplyr::select(rainparams$eventuidvar, rainparams$uidvar, eventdatastart_est)
        
        #join by gage uid and by start time, to give a rainfall gage event uid at the start of each event
        level_data_step %<>% dplyr::left_join(results_event_data, 
                                              by = c(rainparams$uidvar, "dtime_est" = "eventdatastart_est"))   
        
        #carry event uids forward from event start to start of next event
        level_data_step[[rainparams$eventuidvar]] %<>% zoo::na.locf(na.rm = FALSE)
        
        #isolate event data needed for assuring that the rainfall gage event uid isn't assigned too far past the event end
        event_data <- results[["Rain Event Data"]]  %>% 
          dplyr::select(rainparams$eventuidvar, eventdataend_est)
        
        #browser()
        
        #join event end times to level data by event uid
        #check that any dtime that has that event uid does not exceed the end time by greater than three days
        #if it does, reassign NA to event uid
        level_data_step %<>% dplyr::left_join(event_data, by = rainparams$eventuidvar) %>% 
          dplyr::mutate(new_event_uid = dplyr::case_when(dtime_est >= (eventdataend_est + lubridate::days(4)) ~ NA_integer_, 
                                                         TRUE ~ level_data_step[[rainparams$eventuidvar]])) %>% 
          dplyr::select(-!!rainparams$eventuidvar, -eventdataend_est) %>% 
          dplyr::rename(!!rainparams$eventuidvar := new_event_uid)
        
        
        results[["Level Data step"]] <- NULL
        results[["Level Data step"]] <- level_data_step
      }
      
      results[["Level Data"]] <- dplyr::bind_rows(results[["Level Data"]], results[["Level Data step"]])
      results[["Level Data step"]] <- NULL
    }
  }
  
  if(debug){
    print(paste("level_lookup_time:", (proc.time()-ptm)[3]))
  }
  
  if(debug){
    ptm <- proc.time()
  }
  #browser()
  #remove incomplete events from level/rainfall/rain event data
  if(rain_events == TRUE & rainfall == TRUE & level == TRUE){
  test_df_id <- dplyr::full_join(results[["Rainfall Data"]], results[["Level Data"]], by = c("dtime_est", rainparams$eventuidvar, rainparams$uidvar)) %>% #join
    dplyr::arrange(dtime_est) %>% 
    dplyr::filter(!is.na(rainparams$eventuidvar) & is.na(level_ft)) %>%  #filter events that are not NA and and water level that is not NA
    dplyr::pull(rainparams$eventuidvar) 
  
  results[["Level Data"]] %<>% dplyr::filter(!(rainparams$eventuidvar %in% test_df_id))
  
  # !!sym syntax comes from here: https://stackoverflow.com/questions/49786597/r-dplyr-filter-with-a-dynamic-variable-name
  # Because our variable name is a string, we need to make it evaluate as an R symbol instead of the string
  
  #filter out rain data for events that do have corresponding water level data
  results[["Rainfall Data"]] %<>% dplyr::filter((!!sym(rainparams$eventuidvar) %in% results[["Level Data"]][, rainparams$eventuidvar]))
  
  #fiter out rain events that no longer have corresponding rainfall data
  results[["Rain Event Data"]] %<>% dplyr::filter((!!sym(rainparams$eventuidvar) %in% results[["Rainfall Data"]][, rainparams$eventuidvar]))
  
  if(debug){
    print(paste("filtering_time:", (proc.time()-ptm)[3]))
  }
  
  }
  
  
  
  #6 Return results
  return(results)
}

# marsWriteInfiltrationData ------------------------------------------
#' Write Infiltration Performance Data to Database 
#' 
#' Receive vectors of infiltration rate and infiltration baseline data calculated with \code{\link{marsInfiltrationRate_inhr}},
#' gather data, and write to MARS Analysis Database performance_infiltration table. Replaces error codes with NAs and moves
#' them to a separate column, \code{error_lookup_uid}.
#' 
#' @param con Formal class PostgreSQL, a connection to the MARS Analysis database
#' @param baseline_ft vector, numeric, point that water level returns to at the end of an event (default ??)
#' @param infiltration_rate_inhr vector, numeric, infiltration rate (in/hr)
#' @param ow_uid vector, numeric observation well UID
#' @param radar_event_uid vector, numeric event UIDs for rain events from radar data
#' @param snapshot_uid vector, numeric
#' @param observed_simulated_lookup_uid vector, numeric, 1 if observed, 2 if simulated
#' 
#' @return \code{TRUE} if the write is succesful, or an error message if unsuccessful
#' 
#' @seealso \code{\link[pwdgsi]{marsWriteOvertoppingData}}, \code{\link{marsWritePercentStorageData}}
#' 
#' @export
#' 
#' @examples 
#' 
#' marsWriteInfiltrationData(con = mars, 
#'   infiltration_rate_inhr = summary_250$infiltration_rate_inhr,
#'   ow_uid = summary_250$ow_uid,
#'   radar_event_uid = summary_250$rainfall_gage_event_uid,
#'   snapshot_uid = summary_250$snapshot_uid,
#'   observed_simulated_lookup_uid = summary_250$observed_simulated_lookup_uid)
#' 
marsWriteInfiltrationData <- function(con, 
                                   infiltration_rate_inhr,
                                   baseline_ft = NA,
                                   ow_uid,
                                   radar_event_uid,
                                   snapshot_uid,
                                   observed_simulated_lookup_uid){

  #check that vectors are the same length
  if(!(length(infiltration_rate_inhr) == length(ow_uid) &
       length(infiltration_rate_inhr) == length(radar_event_uid) &
       length(infiltration_rate_inhr) == length(snapshot_uid))){
    stop("Vectors must be the same length")
  }
    
  #add vectors to dataframe
  summary_df <- data.frame(infiltration_rate_inhr,
                           baseline_ft,
                           ow_uid,
                           radar_event_uid,
                           snapshot_uid) %>% 
    dplyr::filter(observed_simulated_lookup_uid == 1)
  
  #select columns for dataframe
  saturatedperformance_df <- summary_df %>% 
    dplyr::mutate("error_lookup_uid" = ifelse(infiltration_rate_inhr <0,
                                              infiltration_rate_inhr, NA),
                  infiltration_rate_inhr = ifelse(!is.na(error_lookup_uid),
                                                     NA, infiltration_rate_inhr))
    
  #write to table, and return either TRUE (for a succesful write) or the error (upon failure)
  result <- tryCatch(odbc::dbWriteTable(con, DBI::SQL("metrics.tbl_infiltration"), saturatedperformance_df, overwrite = FALSE, append = TRUE), 
                     error = function(error_message){
                       return(error_message$message)
                     }
  )
  
  return(result)
}


# marsWritePercentStorageData ------------------------------------------
#' Write Percent of Storaged Used Data to Database 
#' 
#' Receive vectors of raw and relative percent storage data, calculated with \code{\link{marsPeakStorage_percent}},
#' gather data, and write to MARS Analysis performance_percentstorage table
#' 
#' @param con Formal class PostgreSQL, a connection to the MARS Analysis database
#' @param percentstorageused_peak vector, numeric, peak percent of storage (\%)
#' @param percentstorageused_relative vector, numeric, relative percent storage (\%) 
#' @param ow_uid vector, numeric observation well UID
#' @param radar_event_uid vector, numeric event UIDs for rain events from radar data
#' @param snapshot_uid vector, numeric
#' @param observed_simulated_lookup_uid vector, numeric, 1 if observed, 2 if simulated
#' 
#' @seealso \code{\link{marsWriteOvertoppingData}},  \code{\link{marsWriteDraindownData}}
#' 
#' @return \code{TRUE} if the write is successful, or an error message if unsuccessful
#' 
#' @export
#' 
#' @examples
#' 
#' marsWritePercentStorageData(con = mars, 
#'    percentstorageused_peak = summary_250$percentstorageused_peak,
#'    percentstorageused_relative = summary_250$percentstorageused_relative,
#'    ow_uid = summary_250$ow_uid,
#'    radar_event_uid = summary_250$rainfall_gage_event_uid,
#'    snapshot_uid = summary_250$snapshot_uid,
#'    observed_simulated_lookup_uid = summary_250$observed_simulated_lookup_uid)
#' 


marsWritePercentStorageData <- function(con, 
                                        percentstorageused_peak,
                                        percentstorageused_relative,
                                        ow_uid,
                                        radar_event_uid,
                                        snapshot_uid,
                                        observed_simulated_lookup_uid){
  
  #check that vectors are the same length
  if(!(length(percentstorageused_peak) == length(ow_uid) &
       length(percentstorageused_peak) == length(radar_event_uid) &
       length(percentstorageused_peak) == length(snapshot_uid) &
       length(percentstorageused_peak) == length(observed_simulated_lookup_uid) &
       length(percentstorageused_peak) == length(percentstorageused_relative))){
    stop("Vectors must be the same length")
  }
    
  #add vectors to dataframe
  summary_df <- data.frame(percentstorageused_peak,
                           percentstorageused_relative,
                           ow_uid,
                           radar_event_uid,
                           observed_simulated_lookup_uid,
                           snapshot_uid)
  
  #gather percent storage types in one column
  percentstorage_table <- tidyr::gather(summary_df, key = "relative", value = "percentstorage", percentstorageused_peak, percentstorageused_relative)
  
  #reassign raw and relative percent storage to FALSE and TRUE 
  percentstorage_table[percentstorage_table$relative == "percentstorageused_peak", "relative"] <- FALSE
  percentstorage_table[percentstorage_table$relative == "percentstorageused_relative", "relative"] <- TRUE
  
  #select columns for dataframe
  percentstorage_df <- percentstorage_table %>% 
    dplyr::select(percentstorage,
                  relative,
                  observed_simulated_lookup_uid,
                  ow_uid,
                  radar_event_uid,
                  snapshot_uid)

  
  #write to table, and return either TRUE (for a succesful write) or the error (upon failure)
  result <- tryCatch(odbc::dbWriteTable(con, DBI::SQL("metrics.tbl_percentstorage"), percentstorage_df, overwrite = FALSE, append = TRUE), 
                     error = function(error_message){
                       return(error_message$message)
                     }
  )
  
  return(result)
}


# marsWriteOvertoppingData ------------------------------------------
#' Write Overtopping Data to Database 
#' 
#' Receive vector of overtopping data, calculated with \code{\link{marsOvertoppingCheck_bool}},
#' and write to MARS Analysis Database performance_overtopping table
#' 
#' @param con Formal class PostgreSQL, a connection to the MARS Analysis database
#' @param overtopping vector, logical, TRUE if water level reaches max storage depth
#' @param ow_uid vector, numeric observation well UID
#' @param radar_event_uid vector, numeric event UIDs for rain events from radar data
#' @param snapshot_uid vector, numeric
#' @param observed_simulated_lookup_uid vector, numeric, 1 if observed, 2 if simulated
#' 
#' @return \code{TRUE} if the write is succesful, or an error message if unsuccessful
#' 
#' @seealso \code{\link{marsWritePercentStorageData}},  \code{\link{marsWriteDraindownData}}
#' 
#' @export
#' 
#' @examples
#' 
#' marsWriteOvertoppingData(con = mars, 
#'   overtopping = summary_250$overtop, 
#'   observed_simulated_lookup_uid = summary_250$observed_simulated_lookup_uid, 
#'   ow_uid = summary_250$ow_uid, 
#'   radar_event_uid = summary_250$rainfall_gage_event_uid,
#'   snapshot_uid = summary_250$snapshot_uid)
#' 
#' 
marsWriteOvertoppingData <- function(con, 
                                     overtopping, 
                                     observed_simulated_lookup_uid, 
                                     ow_uid, 
                                     radar_event_uid,
                                     snapshot_uid){
  
  #check that vectors are the same length
  if(!(length(overtopping) == length(ow_uid) &
       length(overtopping) == length(radar_event_uid) &
       length(overtopping) == length(snapshot_uid) &
       length(overtopping) == length(observed_simulated_lookup_uid))){
    stop("Vectors must be the same length")
  }
  
  #add vectors to dataframe
  overtopping_df <- data.frame(overtopping,
                               observed_simulated_lookup_uid,
                               ow_uid,
                               radar_event_uid,
                               snapshot_uid)
  
  #write to table, and return either TRUE (for a succesful write) or the error (upon failure)
  result <- tryCatch(odbc::dbWriteTable(con, DBI::SQL("metrics.tbl_overtopping"), overtopping_df, overwrite = FALSE, append = TRUE), 
                     error = function(error_message){
                       return(error_message$message)
                     }
  )
  
  return(result)
  
}

# marsWriteDraindownData ------------------------------------------
#' Write Draindown Data to Database 
#' 
#' Receive vector of draindown data, calculated with \code{\link{marsDraindown_hr}},
#' and write to MARS Analysis Database performance_draindwown table
#' 
#' @param con Formal class PostgreSQL, a connection to the MARS Analysis database
#' @param draindown_hr vector, numeric, draindown time (hr)
#' @param draindown_assessment_lookup_uid vector, int, assessment of draindown duration from \code{\link{marsDraindownAssessment}} 
#' @param ow_uid vector, numeric observation well UID
#' @param radar_event_uid vector, numeric
#' @param snapshot_uid vector, numeric
#' @param observed_simulated_lookup_uid vector, numeric, 1 if observed, 2 if simulated
#' 
#' @return \code{TRUE} if the write is succesful, or an error message if unsuccessful
#' 
#' @seealso \code{\link{marsWritePercentStorageData}}, \code{\link{marsWriteOvertoppingData}}, 
#' 
#' @export
#' 
#' @examples 
#' 
#' marsWriteDraindownData(con,
#'   draindown_hr = summary_250$draindown_hr,
#'   draindown_assessment_lookup_uid = summary_250$draindown_assessment_lookup_uid,
#'   ow_uid = summary_250$ow_uid,
#'   radar_event_uid = summary_250$rainfall_gage_event_uid, 
#'   snapshot_uid = summary_250$snapshot_uid,
#'   observed_simulated_lookup_uid = summary_250$observed_simulated_lookup_uid)
#' 
marsWriteDraindownData <- function(con,
                                   draindown_hr,
                                   draindown_assessment_lookup_uid,
                                   ow_uid,
                                   radar_event_uid, 
                                   snapshot_uid,
                                   observed_simulated_lookup_uid){
  
  #check that vectors are the same length
  if(!(length(draindown_hr) == length(ow_uid) &
       length(draindown_hr) == length(draindown_assessment_lookup_uid) &
       length(draindown_hr) == length(radar_event_uid) &
       length(draindown_hr) == length(snapshot_uid) &
       length(draindown_hr) == length(observed_simulated_lookup_uid))){
    stop("Vectors must be the same length")
  }  
  
  #add vectors to dataframe
  draindown_df <- data.frame(draindown_hr,
                             observed_simulated_lookup_uid,
                             ow_uid,
                             radar_event_uid,
                             snapshot_uid, 
                             draindown_assessment_lookup_uid)
  
  #select columns for dataframe
  draindown_df <- draindown_df %>% 
    dplyr::mutate("error_lookup_uid" = ifelse(draindown_hr <0,
                                              draindown_hr, NA),
                  draindown_hr = ifelse(!is.na(error_lookup_uid),
                                                  NA, draindown_hr))
  
  #write to table, and return either TRUE (for a succesful write) or the error (upon failure)
  result <- tryCatch(odbc::dbWriteTable(con, DBI::SQL("metrics.tbl_draindown"), draindown_df, overwrite = FALSE, append = TRUE), 
                     error = function(error_message){
                       return(error_message$message)
                     }
  )
  
  return(result)
  
}  

