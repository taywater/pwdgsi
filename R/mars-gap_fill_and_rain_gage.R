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
#'   \item{dtime_est}{POSIXct datetime with tz = EST or EDT as specified by \code{daylight_savings}}
#'   \item{rainfall_in}{num, rainfall for the 15 minute preceding the corresponding datetime}
#'   \item{gagename}{rain gage ID}
#'   \item{event_id}{event number during this timestep}
#' 
#' 
#' @seealso \code{\link[pwdgsi]{gapFillEventID}}, \code{\link{detectEvents}}
#'
#' @export

marsFetchRainGageData <- function(con, target_id, start_date, end_date, daylightsavings){
  if(!dbIsValid(con)){
    stop("Argument 'con' is not an open RODBC channel")
  }


  #Get closest gage
  smp_gage <- dbGetQuery(con, "SELECT * FROM public.smp_gage") %>% filter(smp_id == target_id)

  #Collect gage data
  #First, get all the relevant data from the closest gage
  gage_query <- paste("SELECT dtime_edt, rainfall_in, gage_uid FROM public.rainfall_gage",
                      "WHERE gage_uid = CAST('", smp_gage$gage_uid[1], "' as int)",
                      "AND dtime_edt >= Date('", start_date, "')",
                      "AND dtime_edt <= Date('", end_date + days(1), "');")

  gage_temp <- dbGetQuery(con, gage_query)

  if(nrow(gage_temp) == 0){

    if(month(start_date) == month(today())){
      stop(paste("Rainfall data appears in the MARS database on about a 5 week delay. \nData for", month(start_date, label = TRUE, abbr = FALSE), "should be available in the second week of", month(today() + months(1), label = TRUE, abbr = FALSE)))
    }
    stop("There is no data in the database for this date range.")
  }

  gage_temp$rainfall_in %<>% as.numeric
  gage_temp$dtime_edt %<>% ymd_hms(tz = "America/New_york")

  #Apparently, attempting to set the time zone on a datetime that falls squarely on the spring forward datetime
  #Such as 2005-04-03 02:00:00
  #Returns NA, because the time is impossible.
  #I hate this so, so much
  #To mitigate this, we will strip NA values from the new object
  gage_temp %<>% filter(!is.na(dtime_edt))

  #Our water level data is not corrected for daylight savings time. ie it doesn't spring forwards
  #So we must shift back any datetimes within the DST window
  #Thankfully, the dst() function returns TRUE if a dtime is within that zone
  if(daylightsavings == FALSE){
    dst_index <- dst(gage_temp$dtime_edt)
    gage_temp$dtime_edt %<>% force_tz("EST") #Assign new TZ without changing dates
    gage_temp$dtime_edt[dst_index] <- gage_temp$dtime_edt[dst_index] - hours(1)
  }

  gage_temp %<>% mutate(event_id = detectEvents(dtime_edt = dtime_edt, rainfall_in = rainfall_in, iet_hr = 6, mindepth_in = 0.10))

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
      fill <- boundary.low + seconds(900)
      zeroFills[zeroFillIndex, 1] <- fill                   #dtime_edt
      zeroFills[zeroFillIndex, 2] <- 0                      #rainfall_in
      zeroFills[zeroFillIndex, 3] <- smp_gage$gage_uid[1]   #gage_uid

      #print(paste("Gap-filling event ID. Before:", gage_temp$event[i], "After:", gage_temp$event[i+1]))
      zeroFills[zeroFillIndex, 4] <- gapFillEventID(event_low = gage_temp$event[i], event_high = gage_temp$event[i+1]) #event

      #If the boundary is longer than 30 minutes, we need a second zero
      if(k > 30){

        #This zero goes 15 minutes before the upper boundary
        fill <- boundary.high - seconds(900)
        zeroFills[zeroFillIndex + 1, 1] <- fill                   #dtime_edt
        zeroFills[zeroFillIndex + 1, 2] <- 0                      #rainfall_in
        zeroFills[zeroFillIndex + 1, 3] <- smp_gage$gage_uid[1]   #gage_uid

        #print(paste("Gap-filling event ID. Before:", gage_temp$event[i], "After:", gage_temp$event[i+1]))
        zeroFills[zeroFillIndex + 1, 4] <- gapFillEventID(event_low = gage_temp$event[i], event_high = gage_temp$event[i+1]) #event

      }

    }
  }

  #Replace UIDs with SMP IDs
  gages <- dbGetQuery(con, "SELECT * FROM public.gage")
  finalseries <- bind_rows(gage_temp, zeroFills) %>%
    left_join(gages) %>%
    select(dtime_edt, rainfall_in, gagename, event_id) %>%
    arrange(dtime_edt)

  #Rename dtime column if we are undoing daylight savings time
  if(daylightsavings == FALSE){
    finalseries <- finalseries %>%
      mutate(dtime_est = dtime_edt) %>%
      select(-dtime_edt)
    finalseries <- select(finalseries, dtime_est, rainfall_in, gagename, event_id)
  }


  return(finalseries)
}


