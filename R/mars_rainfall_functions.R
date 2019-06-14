# detectEvents -----------------------------------------
# NOTES: Based on a function written by Taylor Heffernan (see "detectEvents.r" and related email from 4/5/18,
# modified by Katie Swanson 2/4/2019) returns a dataset of event IDs for a rainfall time
# series. Additional edits after review from Taylor Heffernan by Tim Adams and Katie Swanson (3/4/2019)
#
# IN: dtime_est A vector of POSIXct date times, in ascending order
# IN: rainfall_in Rainfall depths during periods corresponding to times in  dtime_est, in inches
# IN: iet_hr Interevent time, in hours
# IN: mindepth_in Minimum depth of a given event, in inches
# OUT: A vector of integers the same length as dtime_est, which represents the event ID for that time step.

# roxygen
#' Identify individual rainfall events
#'
#' Return a dataset of rainfall event IDs for a time period
#'
#' @param dtime_est vector, POSIXct date times
#' @param rainfall_in vector, num, of rainfall depths corresponding to \code{dtime_est}, in inches
#' @param iet_hr num, Interevent time, in hours. The default is 6 hours.
#' @param mindepth_in num, minimum depth of a given event, in inches. The default is 0.10 inches.
#'
#' @return Output will be a vector of integers corresponding to \code{dtime_est} and representing
#'   the event ID for each time step.
#'   
#' @details Function should be used inside \code{\link[dplyr]{mutate}} to add output to the corresponding table.    
#'
#' @export
#' 
#' @examples
#' gage_temp <- mutate(marsSampleRain, 
#'   event_id = detectEvents(dtime_est = marsSampleRain$dtime_est, 
#'   rainfall_in = marsSampleRain$rainfall_in, 
#'   iet_hr = 6, mindepth_in = 0.10))

detectEvents <- function(dtime_est, rainfall_in, iet_hr = 6, mindepth_in = 0.10) {

  # 1. QC checks
  # 1.1 Check for non-zero and negative rainfall values
  if(!all(rainfall_in > 0)) {
    stop("All rainfall data must be greater than 0.")
  }

  # 1.2 Check that datetime is in ascending order
  if(!identical(order(dtime_est), 1:length(dtime_est))) {
    stop("Datetime data is not sorted in ascending order.")
  }

  # 1.3 Check for duplicated data
  if(!all(!duplicated(dtime_est))) {
    stop("Datetime data cannot contain duplicates.")
  }

  # 1.4 Check that datetime is in correct format
  if(!(class(dtime_est)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct.")
  }

  # 1.5 Check to make sure paired data matches
  if(!(length(dtime_est) == length(rainfall_in))) {
    stop("dtime_est and rainfall_in must be the same length")
  }

  # Assumed interval
  interval_sec <- 15 * 60

  # 2. Process rainfall data
  prepseries <- tibble::tibble(dtime = dtime_est,
                            rf_in = rainfall_in) %>%
    dplyr::mutate(lag_time = dplyr::lag(dtime, 1, default = first(dtime) - interval_sec)) %>%
    dplyr::mutate(gap_hr = difftime(dtime, lag_time, unit = "hours"))

  min_interval_hr <- .25

  # 3. Identify events

  # 3.1 Initialize column
  prepseries$start <- 0

  # 3.2 Check whether first measurement in row 1 is included in following event
  prepseries$start[1] <- ifelse(prepseries$gap_hr[2] < iet_hr, 1, 0)

  # 3.3 Identify beginning of new events
  prepseries$start[prepseries$gap_hr >= iet_hr + min_interval_hr] <- 1

  # 3.4 Generate series of new events
  prepseries <- prepseries %>%
    dplyr::mutate(event = cumsum(start))

  # 3.5 Identify events that are less than the min_depth
  prepsums <- prepseries %>%
    dplyr::group_by(event) %>%
    dplyr::summarize(total_in = sum(rf_in)) %>%
    dplyr::filter(total_in >= mindepth_in-0.0000001) %>%
    # note: subtracted from minimum threshold because spot check indicated that
    # some events at the threshold were not being included in the event
    # detection (but not all). Probably a floating point issue.
    dplyr::mutate(event_id = 1:n()) # all events that are greater than the min depth

  # 3.6 Join event summary to rainfall data
  output <- prepseries %>%
    dplyr::left_join(prepsums, by = "event") %>%
    dplyr::select(dtime_est = dtime,
           rainfall_in = rf_in,
           event_id)

  return(output$event_id)
}

# stormDepth_in ------------------------------------
# NOTES: Function to export storm depth from events processed using detectEvents function
#
# IN: rainfall_in A vector of rainfall depths for one storm
# OUT: The total rainfall depth, in inches

#'
#' Return storm attributes
#'
#' Return storm depth, duration, average intensity, and peak intensity of an event processed using \code{\link{detectEvents}}.
#'
#'
#'
#' @name storm
NULL

#' @rdname storm
#'
#' @param rainfall_in vector, num, rainfall depth in inches representing a single rain event
#'
#' @param dtime_est vector, POSIXct date times representing a single rain event
#'
#' @return \describe{
#'        \item{\code{stormDepth_in}}{Output will be total rainfall depth for the event, in inches.}
#' }
#'
#' @seealso \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{arrange}},
#'  \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{summarize}},
#'  \code{\link[dplyr]{select}}
#'  
#' @export
#' 
#' @examples 
#' rain_newevents <- marsSampleRain %>%  #use dplyr pipe to update dataframe
#'  group_by(gage_uid) %>% 
#'   arrange(dtime_est) %>% 
#'   mutate(event_id = detectEvents(dtime_est, rainfall_in)) %>%
#'   group_by(gage_uid, event_id) %>%
#'   summarize(eventdatastart_edt = first(dtime_est),
#'             eventdataend_edt = last(dtime_est),
#'             eventduration_hr = stormDuration_hr(dtime_est),
#'             eventpeakintensity_inhr = stormPeakIntensity_inhr(dtime_est, rainfall_in),
#'             eventavgintensity_inhr = stormAvgIntensity_inhr(dtime_est, rainfall_in),
#'             eventdepth_in = stormDepth_in(rainfall_in)) %>%
 

stormDepth_in <- function(rainfall_in) {

  if(length(rainfall_in) == 0){
    return(NA)
  }

  # 1. QC checks
  if(!all(rainfall_in >= 0)) {
    stop("Negative rainfall data.")
  }

  # 2. Calculate stormDepth
  return(sum(rainfall_in))
}

# stormDuration ------------------
# NOTES: Function to export storm duration from events processed using detectEvents function

#IN: A vector of times at which rainfall was collected in the storm
#OUT: The total rainfall duration, in hours

#' @rdname storm
#'
#' @return \describe{
#'        \item{\code{stormDuration_hr}}{Output will be a double with the duration of the event, in hours.}
#' }
#'
#' @export

stormDuration_hr <- function(dtime_est) {

  if(length(dtime_est) == 0){
    return(NA)
  }

  # 1. QC checks
  if(!identical(order(dtime_est), 1:length(dtime_est))) {
    stop("Datetime data is not sorted in ascending order.")
  }

  if(!all(!duplicated(dtime_est))) {
    stop("Datetime data can not contain duplicates.")
  }

  if(!(class(dtime_est)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct.")
  }

  # 2. Calculate storm duration
  event_start <- dtime_est[1] - 15 * 60 # assumes 15 minute increments

  duration <- difftime(dtime_est[length(dtime_est)], event_start, unit = "hours")
  return(as.double(duration))
}


# stormPeakIntensity -----------------------------
# NOTES: Function to export storm peak intensity from events processed using detectEvents function
#
# IN:dtime_est A vector of times at which rainfall was collected in the storm
# IN:  rainfall_in The depth of water that fell at each time, in inches
# OUT:  The peak intensity, in inches per hour

#' @rdname storm
#'
#' @return \describe{
#'        \item{\code{stormPeakIntensity_inhr}}{Output will be a number representing the event's peak intensity in inches/hour.}
#' }
#'
#' @export

stormPeakIntensity_inhr <- function(dtime_est, rainfall_in) {

  if(length(dtime_est) == 0 | length(rainfall_in) == 0){
    return(NA)
  }

  # 1. QC checks
  if(!all(rainfall_in > 0)) {
    stop("All rainfall data must be greater than 0")
  }

  if(!(identical(order(dtime_est), 1:length(dtime_est)))) {
    stop("Datetime data is not sorted in ascending order")
  }

  if(!all(!duplicated(dtime_est))) {
    stop("Datetime data can not contain duplicates")
  }

  if(!(class(dtime_est)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct")
  }

  if(!(length(dtime_est) == length(rainfall_in))) {
    stop("dtime_est and rainfall_in must be the same length")
  }

  # 3. Calculate peak intensity
  # Assumes that the interval is 15 minutes.
  # 15 minute rainfall intervals means that the peak intensity is 4x the highest measured data point
  maximum <- max(rainfall_in)
  peak <- maximum * 4

  return(peak)
}

# stormAvgIntensity -------------------------------
# NOTES: Function to export storm average intensity from events processed using detectEvents function

# IN: dtime_est A vector of times at which rainfall was collected in the storm
# IN: rainfall_in The depth of water that fell at each time, in inches
# OUT:  The average intensity over the length of the storm, in inches per hour

#' @rdname storm
#'
#' @return \describe{
#'        \item{\code{stormAvgIntensity_inhr}}{Output will be a number representing the event's average intensity in inches/hour.}
#' }
#'
#' @export

stormAvgIntensity_inhr <- function(dtime_est, rainfall_in) {

  if(length(dtime_est) == 0 | length(rainfall_in) == 0){
    return(NA)
  }

  # 1. QC check (all others covered in called functions)
  if(!(length(dtime_est) == length(rainfall_in))) {
    stop("dtime_est and rainfall_in must be the same length")
  }

  # 2. Calculate average intensity
  result <- stormDepth_in(rainfall_in) / stormDuration_hr(dtime_est)
  return(result)
}

