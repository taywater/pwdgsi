#Project: PWD GSI Analysis
#File of stored functions for analysis of green stormwater infrastructure developed
#for use by the Philadelphia Water Department
#Written by Katie Swanson and checked by Laurie Kellndorfer, CDM Smith, February 2019
#Last update: 6/12/2019 - Nick Manna, AKRF


# detectEvents -----------------------------------------
# NOTES: Based on a function written by Taylor Heffernan (see "detectEvents.r" and related email from 4/5/18,
# modified by Katie Swanson 2/4/2019) returns a dataset of event IDs for a rainfall time
# series. Additional edits after review from Taylor Heffernan by Tim Adams and Katie Swanson (3/4/2019)
#
# IN: dtime_edt A vector of POSIXct date times, in ascending order
# IN: rainfall_in Rainfall depths during periods corresponding to times in  dtime_edt, in inches
# IN: iet_hr Interevent time, in hours
# IN: mindepth_in Minimum depth of a given event, in inches
# OUT: A vector of integers the same length as dtime_edt, which represents the event ID for that time step.

# roxygen
#' Identify individual rainfall events
#'
#' Return a dataset of rainfall event IDs for a time period
#'
#' @param dtime_edt vector, POSIXct date times
#' @param rainfall_in vector, num, of rainfall depths corresponding to \code{dtime_edt}, in inches
#' @param iet_hr num, Interevent time, in hours. The default is 6 hours.
#' @param mindepth_in num, minimum depth of a given event, in inches. The default is 0.10 inches.
#'
#' @return Output will be a vector of integers corresponding to \code{dtime_edt} and representing
#'   the event ID for each time step.
#'
#' @export

detectEvents <- function(dtime_edt, rainfall_in, iet_hr = 6, mindepth_in = 0.10) {

  # 1. QC checks
  # 1.1 Check for non-zero and negative rainfall values
  if(!all(rainfall_in > 0)) {
    stop("All rainfall data must be greater than 0.")
  }

  # 1.2 Check that datetime is in ascending order
  if(!identical(order(dtime_edt), 1:length(dtime_edt))) {
    stop("Datetime data is not sorted in ascending order.")
  }

  # 1.3 Check for duplicated data
  if(!all(!duplicated(dtime_edt))) {
    stop("Datetime data cannot contain duplicates.")
  }

  # 1.4 Check that datetime is in correct format
  if(!(class(dtime_edt)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct.")
  }

  # 1.5 Check to make sure paired data matches
  if(!(length(dtime_edt) == length(rainfall_in))) {
    stop("dtime_edt and rainfall_in must be the same length")
  }

  # Assumed interval
  interval_sec <- 15 * 60

  # 2. Process rainfall data
  prepseries <- tibble::tibble(dtime = dtime_edt,
                            rf_in = rainfall_in) %>%
    dplyr::mutate(lag_time = lag(dtime, 1, default = first(dtime) - interval_sec)) %>%
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
    select(dtime_edt = dtime,
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
#' @param dtime_edt vector, POSIXct date times representing a single rain event
#'
#' @return \describe{
#'        \item{\code{\link{stormDepth_in}}}{Output will be total rainfall depth for the event, in inches.}
#' }
#'
#' @export

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
#'        \item{\code{\link{stormDuration_hr}}}{Output will be a double with the duration of the event, in hours.}
#' }
#'
#' @export

stormDuration_hr <- function(dtime_edt) {

  if(length(dtime_edt) == 0){
    return(NA)
  }

  # 1. QC checks
  if(!identical(order(dtime_edt), 1:length(dtime_edt))) {
    stop("Datetime data is not sorted in ascending order.")
  }

  if(!all(!duplicated(dtime_edt))) {
    stop("Datetime data can not contain duplicates.")
  }

  if(!(class(dtime_edt)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct.")
  }

  # 2. Calculate storm duration
  event_start <- dtime_edt[1] - 15 * 60 # assumes 15 minute increments

  duration <- difftime(dtime_edt[length(dtime_edt)], event_start, unit = "hours")
  return(as.double(duration))
}


# stormPeakIntensity -----------------------------
# NOTES: Function to export storm peak intensity from events processed using detectEvents function
#
# IN:dtime_edt A vector of times at which rainfall was collected in the storm
# IN:  rainfall_in The depth of water that fell at each time, in inches
# OUT:  The peak intensity, in inches per hour

#' @rdname storm
#'
#' @return \describe{
#'        \item{\code{\link{stormPeakIntensity_inhr}}}{Output will be a number representing the event's peak intensity in inches/hour.}
#' }
#'
#' @export

stormPeakIntensity_inhr <- function(dtime_edt, rainfall_in) {

  if(length(dtime_edt) == 0 | length(rainfall_in) == 0){
    return(NA)
  }

  # 1. QC checks
  if(!all(rainfall_in > 0)) {
    stop("All rainfall data must be greater than 0")
  }

  if(!(identical(order(dtime_edt), 1:length(dtime_edt)))) {
    stop("Datetime data is not sorted in ascending order")
  }

  if(!all(!duplicated(dtime_edt))) {
    stop("Datetime data can not contain duplicates")
  }

  if(!(class(dtime_edt)[1] == "POSIXct")) {
    stop("Datetime data must be of class POSIXct")
  }

  if(!(length(dtime_edt) == length(rainfall_in))) {
    stop("dtime_edt and rainfall_in must be the same length")
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

# IN: dtime_edt A vector of times at which rainfall was collected in the storm
# IN: rainfall_in The depth of water that fell at each time, in inches
# OUT:  The average intensity over the length of the storm, in inches per hour

#' @rdname storm
#'
#' @return \describe{
#'        \item{\code{\link{stormAvgIntensity_inhr}}}{Output will be a number representing the event's average intensity in inches/hour.}
#' }
#'
#' @export

stormAvgIntensity_inhr <- function(dtime_edt, rainfall_in) {

  if(length(dtime_edt) == 0 | length(rainfall_in) == 0){
    return(NA)
  }

  # 1. QC check (all others covered in called functions)
  if(!(length(dtime_edt) == length(rainfall_in))) {
    stop("dtime_edt and rainfall_in must be the same length")
  }

  # 2. Calculate average intensity
  result <- stormDepth_in(rainfall_in) / stormDuration_hr(dtime_edt)
  return(result)
}

# hyetograph ---------------------------
# NOTES: Based on plots developed by Dwayne Myers and modified by Katie Swanson 2/4/2019
# Plots hyetographs of events processed by detectEvents funtion

# IN: dtime_edt A vector of times at which rainfall was collected in the storm
# IN: rainfall_in The depth of water that fell at each time, in inches
# OUT:  Rainfall hyetograph

#' Plot hyetograph
#'
#' Return hyetograph of events processed by \code{\link{detectEvents}}
#'
#' @param dtime_edit vector, POSIXct datetimes representing a single rain event
#' @param rainfall_in vector, num, rainfall in inches for that rain event
#' @param raingage chr, Label for the hyetograph for what rain gage the data came from
#' @param event chr, label for the hyetograph for what rain gage the data came from
#'
#' @return Output will be a ggplot2 object of the hyetograph. Currently,
#'    the graphical parameters are hard coded and the legend is deleted.
#'    This may change in future versions.
#'
#' @export



hyetograph <- function(dtime_edt, rainfall_in, raingage, event){

  #0. check data
  if(length(dtime_edt) != length(rainfall_in)){
    stop("Datetime and rainfall lengths must be equal")
  }

  if(length(event) > 1){
    stop("Argument 'event' must be of length 1")
  }

  #1.1 Process data
  rain_data <- data.frame(dtimeEST = lubridate::force_tz(dtime_edt, tz = "EST"),
                          rainIN = rainfall_in)

  if(nrow(rain_data) == 0){
    stop("No data loaded")
  }

  #1.3 Assume minimum interval
  min_interval <- minutes(15)

  #1.4  Calculate cumulative rainfall
  rain_data <- rain_data %>% dplyr::mutate(cumulative = cumsum(rainIN))

  #1.5 Generat title block
  startdate <- min(rain_data$dtimeEST) - min_interval
  title_text <- paste0("Hyetograph\nRaingage: ", raingage[1],
                       " | Event: ", event[1],
                       " | Start Date and Time: ",
                       scales::date_format("%Y-%m-%d %H:%M", tz = "EST")(startdate),
                       sep = "")

  #1.6 Shift timestep to beginning of measurement interval
  rain_data$dtimeEST <- rain_data$dtimeEST - min_interval


  #2. Calculate plotting parameters

  #2.1 Calculate plotting limits
  #Calculate minimum and maximum data values
  min_date <- min(rain_data$dtimeEST, na.rm = TRUE)
  max_date <- max(rain_data$dtimeEST, na.rm = TRUE) + hours(6)
  min_rain <- 0
  max_rain <- max(rain_data$rain, na.rm = TRUE)
  #calculate scaling factor for secondary y-axis for cumulative rainfall
  max_cumulative_scaling <- max(1.1*rain_data$cumulative, na.rm = TRUE)/max_rain

  #Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date
  range_rainfall <- max_rain - min_rain


  if(range_rainfall < 0.1){
    max_rain <- 0.1 #set minimum rainfall range to 0.1 inches
    range_rainfall <- max_rain - min_rain #recalculate if necessary
    max_cumulative_scaling<- max(1.1*rain_data$cumulative, na.rm = TRUE)/max_rain #recalculate scaling secondary
  }

  ##Scale fix for events with only one measurement interval
  if(nrow(rain_data)==1){
    max_cumulative_scaling <- max(rain_data$cumulative, na.rm = TRUE)/max_rain #recalculate scaling secondary

  }

  #2.2 Calculate break intervals for y-axis
  #rainfall categories: <0.15, 0.2, 0.5, >0.5
  if(range_rainfall > 0.5){
    rain_major_interval <- 0.2
    rain_minor_interval <- 0.1
  }else{
    if(range_rainfall > 0.2){
      rain_major_interval <- 0.1
      rain_minor_interval <- 0.05
    }else{
      rain_major_interval <- 0.05
      rain_minor_interval <- 0.01
    }}

  #2.3 Calculate break intervals for x-axis
  if(units(event_duration) == "days"){
    #if event duration is greater than 1 day, set x-axis major breaks to 12-hour intervals
    x <- "12 hours"
  }else{
    if(event_duration > 12){
      #if event duration less 1 day and greater than 12 hours, set x-axis major breaks to 6-hour intervals
      x <- "6 hours"
    }else{
      if(event_duration > 8){
        #if event duration less than 12 hours and greater than 8 hours, set x-axis major breaks to 2-hour intervals
        x <- "2 hours"
      }else{
        #for events shorter than 8 hours, set x-axis major breaks to 1-hour intervals
      x <- "hour"
    }}}

  #2.4 Calculations for dashed vertical line at day boundaries
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "EST"), by = "day", length.out = 14), tz = "EST")

  #2.5 Calculate axis breaks based on plotting limits
  #Select major x-axis breaks based on event duration
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = x), tz = "EST")

  #All plots use one-hour interval for minor x-axis breaks
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date + hours(6), by = "hour"), tz = "EST")

  #2.6 Add row for cumulative rainfall
  #Note - this row forces cumulative rainfall to plot throughout full extent shown, otherwise
  #       the cumulative rainfall would end at the last rainfall measurement
  end <- data.frame(dtimeEST = c(max_date-min_interval,max_date),
                    rainIN = c(0,0),
                    cumulative = c(max(rain_data$cumulative),max(rain_data$cumulative)))
  rain_data <- rbind(rain_data, end)


  #3. Plot
  hyetograph <-
    ggplot(data = rain_data,
           aes(x = dtimeEST,
               y = cumulative/max_cumulative_scaling)

    ) +

    geom_area(color = "grey32",
              fill = "slateblue1",
              alpha = 0.2
    ) +

    geom_bar(data = rain_data,
             aes(x = dtimeEST,
                 y = rainIN),
      fill = "cornflowerblue", # set the line color
      stat="identity"
    ) +

    #Day boundaries
    geom_vline(xintercept = day_marker, color = "black", linetype = "dashed", size = 1.2) + #date boundaries

    annotate("rect", xmin = day_marker-0.03*event_duration,
             xmax = day_marker - 0.01*event_duration,
             ymin = 0.7*max_rain,
             ymax = 0.9*max_rain,
             alpha = 0.8,
             fill = "white")+

    annotate("text", x = day_marker-0.02*event_duration,
             y = 0.8*max_rain,
             label = day_marker,
             angle = 90,
             size = 5)+

    theme_bw() + # a basic black and white theme

    scale_x_datetime(
      name = " ", # x axis label
      labels = scales::date_format("%H:%M", "EST"),
      limits = c(min_date - min_interval, max_date), # set x axis limits
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks
      #expand = c(0.03,0) # control where y axis crosses - first number is fraction of plot left as white space
    ) +

    scale_y_continuous(
      #expand = c(0.03,0), # control where x axis crosses - first number is fraction left as white space
      #limits = c(min_rain, max_rain), # set y axis limits
      breaks = seq(min_rain, max_rain, by = rain_major_interval),
      minor_breaks = seq(min_rain, max_rain, by = rain_minor_interval),
      sec.axis = sec_axis(~.*max_cumulative_scaling, name = "Cumulative Rainfall (in)")

    ) +

    labs(
      y = "Rainfall (in)",
      title = title_text
    ) +

    theme(
      text = element_text(size = 16),
      axis.text.x = element_text(size = 14, color = "black"), # set font size and color of x axis text
      axis.text.y = element_text(size = 14, color = "black"), # set font size and color of y axis text
      panel.background =  element_rect(fill = "white", colour = NA), # set white background
      panel.border =      element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  element_line(colour = "grey70", size = 0.2), # set major grid lines
      panel.grid.minor =  element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "none"
    )
  return(hyetograph)
}

