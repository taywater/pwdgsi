# hyetograph ---------------------------
# NOTES: Based on plots developed by Dwayne Myers and modified by Katie Swanson 2/4/2019
# Plots hyetographs of events processed by detectEvents funtion

# IN: dtime_est A vector of times at which rainfall was collected in the storm
# IN: rainfall_in The depth of water that fell at each time, in inches
# OUT:  Rainfall hyetograph

#' Plot hyetograph
#'
#' Return hyetograph of events processed by \code{\link{detectEvents}}
#'
#' @param dtime_est vector, POSIXct datetimes representing a single rain event
#' @param rainfall_in vector, num, rainfall in inches for that rain event
#' @param raingage chr, Label for the hyetograph for what rain gage the data came from
#' @param event chr, label for the hyetograph for what rain gage the data came from
#' @param reverse_y logical, whether the Y axes should be reversed
#'
#' @return Output will be a ggplot2 object of the hyetograph. Currently,
#'    the graphical parameters are hard coded and the legend is deleted.
#'    This may change in future versions.
#'
#' @seealso \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}
#'
#' @export
#' 
#' @examples
#' gage_temp <- mutate(marsSampleRain, 
#'   event_id = detectEvents(dtime_edt = marsSampleRain$dtime_edt, 
#'   rainfall_in = marsSampleRain$rainfall_in, 
#'   iet_hr = 6, mindepth_in = 0.10)) %>% filter(event_id == 2)
#'   
#' hyetograph(dtime_edt = gage_temp$dtime_edt, 
#'   rainfall_in = gage_temp$rainfall_in, raingage = 2, event = 2)   



hyetograph <- function(dtime_est, rainfall_in, raingage, event, reverse_y = FALSE){

  #0. check data
  if(length(dtime_est) != length(rainfall_in)){
    stop("Datetime and rainfall lengths must be equal")
  }

  if(length(event) > 1){
    stop("Argument 'event' must be of length 1")
  }

  #1.1 Process data
  rain_data <- data.frame(dtimeEST = lubridate::force_tz(dtime_est, tz = "EST"),
                          rainIN = rainfall_in)

  if(nrow(rain_data) == 0){
    stop("No data loaded")
  }

  #1.3 Assume minimum interval
  min_interval <- lubridate::minutes(15)

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
  max_date <- max(rain_data$dtimeEST, na.rm = TRUE) + lubridate::hours(6)
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
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date + lubridate::hours(6), by = "hour"), tz = "EST")

  #2.6 Add row for cumulative rainfall
  #Note - this row forces cumulative rainfall to plot throughout full extent shown, otherwise
  #       the cumulative rainfall would end at the last rainfall measurement
  end <- data.frame(dtimeEST = c(max_date-min_interval,max_date),
                    rainIN = c(0,0),
                    cumulative = c(max(rain_data$cumulative),max(rain_data$cumulative)))
  rain_data <- rbind(rain_data, end)


  #Which scale function are we using?
  if(reverse_y == TRUE){
    y_scale_function <- ggplot2::scale_y_reverse
  }else{
    y_scale_function <- ggplot2::scale_y_continuous  
  }
  
  
  #3. Plot
  hyetograph <-
    ggplot2::ggplot(data = rain_data,
           ggplot2::aes(x = dtimeEST,
               y = cumulative/max_cumulative_scaling)

    ) +

    ggplot2::geom_area(color = "grey32",
              fill = "slateblue1",
              alpha = 0.2
    ) +

    ggplot2::geom_bar(data = rain_data,
             ggplot2::aes(x = dtimeEST,
                 y = rainIN),
      fill = "cornflowerblue", # set the line color
      stat="identity"
    ) +

    #Day boundaries
    ggplot2::geom_vline(xintercept = day_marker, color = "black", linetype = "dashed", size = 1.2) + #date boundaries

    ggplot2::annotate("rect", xmin = day_marker-0.03*event_duration,
             xmax = day_marker - 0.01*event_duration,
             ymin = 0.7*max_rain,
             ymax = 0.9*max_rain,
             alpha = 0.8,
             fill = "white")+

    ggplot2::annotate("text", x = day_marker-0.02*event_duration,
             y = 0.8*max_rain,
             label = day_marker,
             angle = 90,
             size = 5)+

    ggplot2::theme_bw() + # a basic black and white theme

    ggplot2::scale_x_datetime(
      name = " ", # x axis label
      labels = scales::date_format("%H:%M", "EST"),
      limits = c(min_date - min_interval, max_date), # set x axis limits
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks
      #expand = c(0.03,0) # control where y axis crosses - first number is fraction of plot left as white space
    ) +

    y_scale_function(
      #expand = c(0.03,0), # control where x axis crosses - first number is fraction left as white space
      #limits = c(min_rain, max_rain), # set y axis limits
      breaks = seq(min_rain, max_rain, by = rain_major_interval),
      minor_breaks = seq(min_rain, max_rain, by = rain_minor_interval),
      sec.axis = ggplot2::sec_axis(~.*max_cumulative_scaling, name = "Cumulative Rainfall (in)")

    ) +

    ggplot2::labs(
      y = "Rainfall (in)",
      title = title_text
    ) +

    ggplot2::theme(
      text = ggplot2::element_text(size = 16),
      axis.text.x = ggplot2::element_text(size = 14, color = "black"), # set font size and color of x axis text
      axis.text.y = ggplot2::element_text(size = 14, color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.2), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "none"
    )
  return(hyetograph)
}
