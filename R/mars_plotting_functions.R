# marsRainfallPlot ------------------------------------------
# NOTES: Based on plots developed by Dwayne Myers and modified by Katie Swanson 2/4/2019
# Plots hyetographs of events processed by marsDetectEvents function

# IN: dtime_est A vector of times at which rainfall was collected in the storm
# IN: rainfall_in The depth of water that fell at each time, in inches
# OUT:  Rainfall hyetograph

#' Plot hyetograph
#'
#' Return hyetograph of events processed by \code{\link{marsDetectEvents}}
#'
#' @param dtime_est vector, POSIXct datetimes representing a single rain event
#' @param rainfall_in vector, num, rainfall in inches for that rain event
#' @param event chr, label for the hyetograph for what rain gage the data came from
#' @param reverse_y logical, whether the Y axes should be reversed
#'
#' @return Output is a ggplot2 object of the hyetograph. 
#'
#' @seealso \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{filter}}
#'
#' @export
#' 
#' @examples
#' gage_temp <- dplyr::mutate(marsSampleRain, 
#'   event_id = marsDetectEvents(dtime_est = marsSampleRain$dtime_est, 
#'   rainfall_in = marsSampleRain$rainfall_in, 
#'   iet_hr = 6, mindepth_in = 0.10)) %>% dplyr::filter(event_id == 2)
#'   
#' marsRainfallPlot(dtime_est = gage_temp$dtime_est, 
#'   rainfall_in = gage_temp$rainfall_in, event = 2)   



marsRainfallPlot <- function(dtime_est, rainfall_in, event, reverse_y = FALSE){
  
  #0. check data
  if(length(dtime_est) != length(rainfall_in)){
    stop("Datetime and rainfall lengths must be equal")
  }
  
  if(length(event) > 1){
    stop("Argument 'event' must be of length 1")
  }
  
  #1.1 Process data
  rain_data <- data.frame(dtimeEST = lubridate::force_tz(dtime_est, tz = "EST"),
                          rainIN = rainfall_in) %>% dplyr::arrange(dtimeEST)
  
  if(nrow(rain_data) == 0){
    stop("No data loaded")
  }
  
  
  #1.3 Assume minimum interval
  min_interval <- lubridate::minutes(15)
  
  #1.4  Calculate cumulative rainfall
  rain_data <- rain_data %>% dplyr::mutate(cumulative = cumsum(rainIN))
  
  #1.5 Generate title block
  startdate <- min(rain_data$dtimeEST) - min_interval
  title_text <- paste0("Hyetograph\n| Event: ", event[1],
                       " | Start Date and Time: ",
                       scales::date_format("%Y-%m-%d %H:%M", tz = "EST")(startdate),
                       sep = "")
  
  #1.6 Shift timestep to beginning of measurement interval
  rain_data$dtimeEST <- rain_data$dtimeEST - min_interval
  
  #2. Calculate plotting parameters
  
  #2.1 Calculate plotting limits
  #Calculate minimum and maximum data values
  min_date <- min(rain_data$dtimeEST, na.rm = TRUE)
  max_date <- max(rain_data$dtimeEST, na.rm = TRUE)
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
  # end <- data.frame(dtimeEST = c(max_date-min_interval,max_date),
  #                   rainIN = c(0,0),
  #                   cumulative = c(max(rain_data$cumulative),max(rain_data$cumulative)))
  # rain_data <- rbind(rain_data, end)
  
  
  #Which scale function are we using?
  if(reverse_y == TRUE){
    y_scale_function <- ggplot2::scale_y_reverse
  }else{
    y_scale_function <- ggplot2::scale_y_continuous  
  }
  
  rain_data <- rain_data %>% dplyr::distinct(dtimeEST, .keep_all = TRUE)
  #3. Plot
  hyetograph <-
    ggplot2::ggplot(data = rain_data,
                    ggplot2::aes(x = dtimeEST,
                                 y = cumulative/max_cumulative_scaling)
                    
    ) +
    
    ggplot2::geom_area(ggplot2::aes(fill = "  Cumulative Rainfall    "), 
                       color = "grey32", 
                       alpha = 0.2)+
    
    ggplot2::geom_bar(data = rain_data, 
                      ggplot2::aes(x = dtimeEST, y = rainIN, fill = "  Rainfall"), 
                      stat = "identity") +
    
    ggplot2::scale_fill_manual(values = c("slateblue1", "cornflowerblue"), 
                               guide = ggplot2::guide_legend(title = NULL, 
                                                             override.aes = list(
                                                               alpha = c(0.2,1))))+
    
    
    #Day boundaries
    ggplot2::geom_vline(xintercept = day_marker, color = "black", linetype = "dashed", size = 1.2) + #date boundaries
    
    # ggplot2::annotate("rect", xmin = day_marker-0.03*event_duration,
    #                   xmax = day_marker - 0.01*event_duration,
    #                   ymin = 0.7*max_rain,
    #                   ymax = 0.9*max_rain,
    #                   alpha = 0.8,
    #                   fill = "white")+
    
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
      axis.text.x = ggplot2::element_text(size = 14, color = "black"), # set font size and color of x axis text
      axis.text.y = ggplot2::element_text(size = 14, color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.2), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 10),
      legend.title=ggplot2::element_blank()
    )
  return(hyetograph)
}
# Pull legend from separate ggplots for combined plot ----------------------
# Function used for created combined legend in gridExtra
# Copied directly from this wiki (accessed May 8, 2019):
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930

#Description of the arguments:

#IN:  myggplot             ggplot object

#OUT: Combined legend

#' Get Legend
#' 
#' Pull legend from separate ggplots for combined plot
#' 
#' @param myggplot ggplot object
#' 
#' @return Returns a combined legend


get_legend<-function(myggplot){
  tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# marsWaterLevelPlot ----------------------------------

#' Observed and Simulated Water Level Plot
#'
#' Create a plot of observed and simulated (optional) water level
#' 
#' @param  event                              Rainfall gage event ID 
#' @param  structure_name                     SMP ID and OW Suffix
#' @param  obs_datetime                       Vector of POSIXct datetimes for observed dataset
#' @param  obs_level_ft                       Vector of water level data (ft), corresponding to \code{obs_datetime}
#' @param  sim_datetime                       Vector of POSIXct datetimes for simulated dataset (optional)
#' @param  sim_level_ft                       Vector of water level data (ft), corresponding to \code{sim_datetime} (optional)
#' @param  storage_depth_ft                   Maximum storage depth of system (ft)
#' @param  orifice_show                       TRUE if user wants to include the orifice height as dashed line on plot (optional)
#' @param  orifice_height_ft                  Orifice height, in feet (optional)
#' @param  metrics_show                       Bool, Default FALSE. TRUE if user wants to include a table of metrics on the plot (optional)
#' @param  obs_RSPU                           Num, Metric: Observed relative percentage of storage used, see \code{marsPeakStorage_percent} (optional)
#' @param  obs_infil_inhr                     Num, Metric: Observed infiltration rate in inches per hour, see \code{marsInfiltrationRate_inhr} (optional)
#' @param  obs_draindown_hr                   Num, Metric: Observed draindown time in hours, see \code{marsDraindown_hr} (optional)
#' @param  obs_overtopping                    Bool, Metric: Observed overtopping boolean, see \code{marsOvertoppingCheck_bool} (optional)
#' @param  sim_RSPU                           Num, Metric: Simulated relative percentage of storage used, see \code{marsPeakStorage_percent} (optional)
#' @param  sim_infil_inhr                     Num, Metric: Simulated infiltration rate in inches per hour, see \code{marsInfiltrationRate_inhr} (optional)
#' @param  sim_draindown_hr                   Num, Metric: Simulated draindown time in hours, see \code{marsDraindown_hr} (optional)
#' @param  sim_overtopping                    Bool, Metric: Simulated overtopping boolean, see \code{marsOvertoppingCheck_bool} (optional)
#' 
#' 
#' @return Output is a ggplot2 object of the water level plot.
#' 
#' @export


marsWaterLevelPlot <- function(event, 
                               structure_name, 
                               storage_depth_ft, 
                               obs_datetime, 
                               obs_level_ft,
                               sim_datetime = NA,
                               sim_level_ft = NA,
                               orifice_show = FALSE,
                               orifice_height_ft = NULL, 
                               snapshot = NA,
                               metrics_show = FALSE,
                               obs_RSPU,
                               sim_RSPU,
                               obs_infil_inhr,
                               sim_infil_inhr,
                               obs_draindown_hr,
                               sim_draindown_hr,
                               obs_overtopping ,
                               sim_overtopping){
  
  #1 Process Data
  
  #1.1 
  #Confirm that storage depth is explicitly defined
  if(!is.numeric(storage_depth_ft) | is.na(storage_depth_ft)){
    stop("storage_depth is not numeric.")
  }
  
  #1.2
  #Set negative water levels to zero
  obs_level_ft[which(obs_level_ft < 0)] <- 0
  
  # if(!is.na(sim_level_ft[1])){
  #   sim_level_ft[which(sim_level_ft < 0)] <- 0
  # }
  
  
  #1.3
  #Check that data is associated with event
  if(length(obs_level_ft) == 0){
    stop(paste0("No data loaded in observed Event", event, "."))
  }
  
  #1.4 QC check for observed data record
  #Using code from marsDetectEvents
  prepseries <- obs_datetime %>%
    data.frame() %>% 
    dplyr::mutate(lag_time = dplyr::lag(obs_datetime, 1)) %>%
    dplyr::mutate(gap_hr = difftime(obs_datetime, lag_time, unit = "hours")) %>%
    dplyr::filter(gap_hr > 6)
  
  if(nrow(prepseries) > 0){
    message(paste0("Warning: Missing values in observed time series."))
    warning_label <- "Warning: Missing values in observed time series."
  }else{
    warning_label <- ""
  }
  
  
  #1.5
  #Check is orifice should be shown
  if(orifice_show == TRUE){
    orifice_plot <- orifice_height_ft
    orifice_lab <- paste0("orifice elevation: ",round(orifice_height_ft, 2))
  }else{
    orifice_plot <- 0 #line will be covered by bottom of structure if option is not selected
  }
  
  #2. Calculate plotting parameters
  
  #2.1 Calculate date plotting limits(x-axis) 
  #Calculate minimum and maximum data values
  
  
  if(!is.na(sim_level_ft[1])){
    min_date <- min(obs_datetime, sim_datetime, na.rm = TRUE)
    max_date <- max(obs_datetime, sim_datetime, na.rm = TRUE)
  }else{
    min_date <- min(obs_datetime, na.rm = TRUE)
    max_date <- max(obs_datetime, na.rm = TRUE) #+ hours(6)
  }
  
  #Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date
  
  #set date marker offset by duration
  if(units(event_duration) == "days"){
    marker_scale <- 0.02
  }else{
    marker_scale <- 0.015
  }
  
  #2.2 Calculations for dashed vertical line at day boundaries
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "EST"), by = "day", length.out = 14), tz = "EST")
  
  #2.4 Calculate axis breaks based on plotting limits
  #Select major x-axis breaks based on event duration (all extend observed record by 12 hours)
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"), tz = "EST") 
  
  #All plots use one-hour interval for minor x-axis breaks
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - lubridate::hours(12), max_date + lubridate::hours(6), by = "hour"), tz = "EST") 
  
  #2.5 Generate title block
  title_text <- paste0("Water Level\nSMP ID: ", structure_name,
                       " | Event: ", event[1],
                       " | Start Date and Time: ", 
                       scales::date_format("%Y-%m-%d %H:%M", tz = "EST")(min_date),
                       sep = "")
  
  obs_df <- data.frame(obs_datetime, obs_level_ft)
  
  if(!is.na(sim_level_ft[1])){
    sim_df <- data.frame(sim_datetime, sim_level_ft)
  }
  
  #3. Generate plot
  #3.1 Water Level (observed)
  level_plot <- 
    ggplot2::ggplot(data = obs_df) +
    
    #Day boundaries
    ggplot2::geom_vline(xintercept = day_marker, color = "black", linetype = "dashed", size = 1.2) + #date boundaries
    
    ggplot2::annotate("text", x = day_marker-marker_scale*event_duration, 
                      y = 0.8*storage_depth_ft, 
                      label = day_marker,
                      angle = 90, 
                      size = ggplot2::rel(5))+ #5
    
    #Warning message for data gaps in observed record
    ggplot2::annotate("text", x = day_marker[1]+1,
                      y = 0.5*storage_depth_ft,
                      label = warning_label, #empty if no warning
                      hjust = 0,
                      color = "red",
                      size = ggplot2::rel(5))+
    
    #Structure top and bottom
    ggplot2::geom_hline(yintercept = 0, color = "black", size = 1.2)+ #bottom
    
    ggplot2::geom_hline(yintercept = storage_depth_ft, color = "orange", size = 1.2)+ #top
    
    ggplot2::geom_label(ggplot2::aes(x = min_date + event_duration/4, 
                                     y = storage_depth_ft*1.04, 
                                     label = "Maximum Storage Depth"),
                        size = ggplot2::rel(5),
                        fill = "white", 
                        label.size = 0) +
    
    #Observed water level
    ggplot2::geom_line(data = obs_df,
                       ggplot2::aes(x = obs_datetime,
                                    y = obs_level_ft,
                                    color = "Observed Water Level  "),
                       size = 2
    ) +
    
    #Formatting
    ggplot2::theme_bw() + # a basic black and white theme
    
    ggplot2::scale_x_datetime(
      name = " ", # x axis label
      labels = scales::date_format("%H:%M", "EST"),
      limits = c(min_date - lubridate::minutes(15), max_date + lubridate::minutes(60)), # set x axis limits
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks
    ) +
    
    ggplot2::scale_y_continuous(
      breaks = seq(0, storage_depth_ft+1, by = if(storage_depth_ft > 2) round(storage_depth_ft/4, 0) else ceiling(storage_depth_ft/4)),
      minor_breaks = seq(-0.5,2*storage_depth_ft, by = 0.1)
    ) +
    
    
    ggplot2::labs(
      y = "Water Level (ft)"
    ) +
    
    ggplot2::theme(
      #text = element_text(size = rel(2)), #size previously set to 16
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"), # set font size and color of y axis text
      panel.background =  ggplot2::element_rect(fill = "white", colour = NA), # set white background
      panel.border =      ggplot2::element_rect(fill = NA, colour="black"), # set black border
      panel.grid.major =  ggplot2::element_line(colour = "grey70", size = 0.2), # set major grid lines
      panel.grid.minor =  ggplot2::element_line(colour = "grey90", size = 0.5), # set minor grid lines
      legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
      legend.text = ggplot2::element_text(size = ggplot2::rel(.9)),
      legend.title=ggplot2::element_blank())
  
  if(!is.na(sim_level_ft[1])){
    level_plot <- level_plot +     
      #Simulated water level
      ggplot2::geom_line(data = sim_df,
                         ggplot2::aes(x = sim_datetime,
                                      y = sim_level_ft,
                                      color = "Simulated Water Level"),
                         size = 2
      )
  }
  
  if(orifice_show == TRUE){
    level_plot <- level_plot + 
      
      ggplot2::geom_hline(yintercept = orifice_plot, color = "grey", linetype = 2, size = 1.2) +
      ggplot2::geom_label(label = orifice_lab,
                          y = orifice_height_ft*1.1,
                          x = obs_datetime[round(0.75*length(obs_datetime))])
    
  }
  
  
  level_plot %<>% metricsTable_show(metrics_show = metrics_show,
                                    obs_RSPU = obs_RSPU,
                                    obs_infil_inhr = obs_infil_inhr,
                                    obs_draindown_hr = obs_draindown_hr,
                                    obs_overtopping = obs_overtopping,
                                    sim_RSPU = sim_RSPU,
                                    sim_infil_inhr = sim_infil_inhr,
                                    sim_draindown_hr = sim_draindown_hr,
                                    sim_overtopping = sim_overtopping)
  
  if(metrics_show == TRUE){
    
    #set missing values to ""
    if( missing(obs_draindown_hr) ){obs_draindown_hr <- ""}
    if( missing(sim_draindown_hr) ){sim_draindown_hr <- ""}
    if( missing(obs_infil_inhr) ){obs_infil_inhr <- ""}
    if( missing(sim_infil_inhr) ){sim_infil_inhr <- ""}
    if( missing(obs_RSPU) ){obs_RSPU <- ""}
    if( missing(sim_RSPU) ){sim_RSPU <- ""}
    if( missing(obs_overtopping) ){obs_overtopping <- ""}
    if( missing(sim_overtopping) ){sim_overtopping <- ""}
    
    if( is.numeric(obs_infil_inhr) & obs_infil_inhr < 0 ){
      obs_infil_inhr <- paste0("ERR: ", obs_infil_inhr)
    }
    if( is.numeric(sim_infil_inhr) & sim_infil_inhr < 0 ){
      sim_infil_inhr <- paste0("ERR: ", sim_infil_inhr)
    }
    
    if(is.numeric(obs_draindown_hr)){ obs_draindown_hr <- round(obs_draindown_hr,2)}
    if(is.numeric(sim_draindown_hr)){ sim_draindown_hr <- round(sim_draindown_hr,2)}
    if(is.numeric(obs_infil_inhr)){ obs_infil_inhr <- round(obs_infil_inhr,2)}
    if(is.numeric(sim_infil_inhr)){ sim_infil_inhr <- round(sim_infil_inhr,2)}
    if(is.numeric(obs_RSPU)){ obs_RSPU <- round(obs_RSPU,2)}
    if(is.numeric(sim_RSPU)){ sim_RSPU <- round(sim_RSPU,2)}
    
    # browser()
    #------ table version
    metric_table <- as.data.frame(matrix(nrow=4))
    colnames(metric_table) <- "Metrics"
    metric_table$Metrics <- c("Drain down (hrs)",
                              "Infiltration rate (in/hr)",
                              "RSPU (%)",
                              "Overtopping (T/F)")

    
    if(obs_infil_inhr < 0){obs_infil_inhr <- paste0("ERR: ",obs_infil_inhr)}
    
    obs_mets <- c(obs_draindown_hr,obs_infil_inhr, obs_RSPU, obs_overtopping)
    sim_mets <- c(sim_draindown_hr,sim_infil_inhr, sim_RSPU, sim_overtopping)
    
    #add columns if obs/sim exists
    if(sum(is.na(obs_mets)) < 4){metric_table$Observed <- obs_mets}
    if(sum(is.na(sim_mets)) < 4){metric_table$Simulated <- sim_mets}
    
    #remove rows if both are empty
    # browser()
    remove <- c()
    if(sum(metric_table[1,] == "") == 2){ remove <- c(remove,1)}
    if(sum(metric_table[2,] == "") == 2){ remove <- c(remove,2)}
    if(sum(metric_table[3,] == "") == 2){ remove <- c(remove,3)}
    if(sum(metric_table[4,] == "") == 2){ remove <- c(remove,4)}
    metric_table <- metric_table[c(1:4)[!(c(1:4) %in% remove)],]
    
    
    level_plot <- level_plot +
      
      ggplot2::annotation_custom (grob = tableGrob(metric_table,
                                                   rows = NULL,
                                                   theme = ggpp::ttheme_gtlight()),
                                  ymin = (storage_depth_ft*0.70),
                                  ymax = (storage_depth_ft*0.95),
                                  xmin = obs_datetime[round(length(obs_datetime)*0.5)],
                                  xmax = obs_datetime[round(length(obs_datetime))])

  }
  
  return(level_plot)
  
}


# marsCombinedPlot --------------------------------------------------------
#' Plot hyetograph and water level plot on the same chart
#'
#' Return hyetograph and observed and simulated (optional) water level plot for the same rain event on the same chart
#'
#' @param event                               chr, rain gage event UID
#' @param structure_name                      chr, SMP ID and OW Suffix
#' @param obs_datetime                        vector, POSIXct datetimes corresponding to \code{obs_level_ft}
#' @param obs_level_ft                        vector, water level data (ft), corresponding to \code{obs_datetime}
#' @param sim_datetime                        vector, POSIXct datetimes corresponding to \code{sim_level_ft} (optional)
#' @param sim_level_ft                        vector, water level data (ft), corresponding to \code{sim_datetime} (optional)
#' @param storage_depth_ft                    num, maximum storage depth of system (ft)
#' @param orifice_show                        TRUE if user wants to include the orifice height as dashed line on plot (optional)
#' @param orifice_height_ft                   Orifice height, in feet (optional)
#' @param rainfall_datetime                   vector, POSIXct datetimes corresponding to \code{rainfall_in}
#' @param rainfall_in                         vector, num, rainfall in inches corresponding to \code{rainfall_datetime}
#' @param metrics_show                        bool, Default FALSE. TRUE if user wants to include a table of metrics on the plot (optional)
#' @param obs_RSPU                            num, Metric: Observed relative percentage of storage used, see \code{marsPeakStorage_percent} (optional)
#' @param obs_infil_inhr                      num, Metric: Observed infiltration rate in inches per hour, see \code{marsInfiltrationRate_inhr} (optional)
#' @param obs_draindown_hr                    num, Metric: Observed draindown time in hours, see \code{marsDraindown_hr} (optional)
#' @param obs_overtopping                     bool, Metric: Observed overtopping boolean, see \code{marsOvertoppingCheck_bool} (optional)
#' @param sim_RSPU                            num, Metric: Simulated relative percentage of storage used, see \code{marsPeakStorage_percent} (optional)
#' @param sim_infil_inhr                      num, Metric: Simulated infiltration rate in inches per hour, see \code{marsInfiltrationRate_inhr} (optional)
#' @param sim_draindown_hr                    num, Metric: Simulated draindown time in hours, see \code{marsDraindown_hr} (optional)
#' @param sim_overtopping                     bool, Metric: Simulated overtopping boolean, see \code{marsOvertoppingCheck_bool} (optional)

#'
#' @return Output will be a gridExtra object of the two plots
#'
#' @seealso \code{\link{marsRainfallPlot}}, \code{\link{marsWaterLevelPlot}}
#'
#' @export

marsCombinedPlot <- function(event, 
                             structure_name, 
                             obs_datetime, 
                             obs_level_ft,
                             sim_datetime = NA,
                             sim_level_ft = NA,
                             storage_depth_ft, 
                             orifice_show = FALSE,
                             orifice_height_ft = NULL,
                             rainfall_datetime,
                             rainfall_in,
                             metrics_show = FALSE,
                             obs_RSPU,
                             sim_RSPU,
                             obs_infil_inhr,
                             sim_infil_inhr,
                             obs_draindown_hr,
                             sim_draindown_hr,
                             obs_overtopping,
                             sim_overtopping
                             ){
  
  # potential to add back in; updated variable names; kept obs_peak_level_ft
  # if(!is.na(obs_peak_level_ft) | !is.na(obs_infil_inhr) | !is.na(obs_percent_storage_relative) | !is.na(obs_draindown_hr)){
  #   metrics_caption <- paste0("Performance Metrics  Obs. Sim. <br />
  #                              Peak Level     (ft)  ", obs_peak_level_ft[1], "  ",  sim_peak_level_ft[1], "<br />
  #                              Sat. Infil  (in/hr)  ", obs_infil_inhr[1], "  ", sim_infil_inhr[1], "<br />
  #                              Rel Storage Use   %  ", obs_RSPU[1], "  ", sim_RSPU[1], "<br />
  #                              Draindown Time (hr)  ", obs_draindown_hr[1], "  ", sim_draindown_hr[1])
  # }else{
  #   metrics_caption <- ""
  # }
  
  #Add a last date so the hyetograph looks better
    rainfall_in <- append(rainfall_in, 0)
  if(!is.na(sim_level_ft[1])){
    rainfall_datetime <- append(rainfall_datetime, max(obs_datetime, sim_datetime)) %>% lubridate::with_tz("EST")
  }else{
    rainfall_datetime <- append(rainfall_datetime, max(obs_datetime)) %>% lubridate::with_tz("EST")
  }
  
  #1 Run functions for individual plots
  level_plot <- pwdgsi::marsWaterLevelPlot(event = event, 
                                           structure_name = structure_name, 
                                           obs_datetime = obs_datetime,
                                           obs_level_ft = obs_level_ft,
                                           sim_datetime = sim_datetime,
                                           sim_level_ft = sim_level_ft,
                                           storage_depth_ft = storage_depth_ft,
                                           orifice_show = orifice_show,
                                           orifice_height_ft = orifice_height_ft)
  
  rainfall_plot <- pwdgsi::marsRainfallPlot(event = event, 
                                            dtime_est = rainfall_datetime, 
                                            rainfall_in = rainfall_in,
                                            reverse_y = TRUE)
  
  #2 Combine Plots
  
  #Save out legends
  level_legend <- pwdgsi:::get_legend(level_plot)
  rainfall_legend <- pwdgsi:::get_legend(rainfall_plot)
  
  #Calculate date plotting limits(x-axis) 
  #Calculate minimum and maximum data values
  if(!is.na(sim_level_ft[1])){
    min_date <- min(obs_datetime, sim_datetime, na.rm = TRUE)
    max_date <- max(obs_datetime, sim_datetime, na.rm = TRUE)
  }else{
    min_date <- min(obs_datetime, na.rm = TRUE)
    max_date <- max(obs_datetime, na.rm = TRUE) #+ hours(6)
  }
  
  #Calculate ranges in values to set axis breaks by category
  event_duration <- max_date - min_date
  
  #set date marker offset by duration
  if(units(event_duration) == "days"){
    marker_scale <- 0.02
  }else{
    marker_scale <- 0.015
  }
  
  #Calculations for dashed vertical line at day boundaries
  day_strip <- lubridate::date(min_date)
  day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "EST"), by = "day", length.out = 14), tz = "EST")
  
  #Calculate axis breaks based on plotting limits
  #Select major x-axis breaks based on event duration (all extend observed record by 12 hours)
  major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"), tz = "EST") 
  
  #All plots use one-hour interval for minor x-axis breaks
  minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - lubridate::hours(12), max_date + lubridate::hours(6), by = "hour"), tz = "EST") 
  
  #Title
  title_text <- paste0("Water Level\nSMP ID: ", structure_name,
                       " | Event: ", event[1],
                       " | Start Date and Time: ", 
                       scales::date_format("%Y-%m-%d %H:%M", tz = "EST")(min_date),
                       sep = "")
  
  #Remove legends and titles and update axes
  level_plot <- level_plot + 
    ggplot2::theme(legend.position = "none", 
                   plot.title = ggplot2::element_blank(), 
                   axis.title = ggplot2::element_text(size = ggplot2::rel(1)), 
                   axis.text = ggplot2::element_text(size = ggplot2::rel(.95)))
  rainfall_plot <- rainfall_plot + 
    ggplot2::theme(legend.position = "none", 
                   plot.title = ggplot2::element_text(size = ggplot2::rel(1.35)),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(), 
                   axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.25)), 
                   axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.25))) +
    ggplot2::scale_x_datetime(
      name = " ", # x axis label
      labels = scales::date_format("%H:%M", "EST"),
      limits = c(min_date - lubridate::minutes(15), max_date + lubridate::minutes(60)),
      breaks = major_date_breaks,
      minor_breaks = minor_date_breaks)  +
    ggplot2::labs(title = title_text)
    # ggplot2::geom_label(ggplot2::aes(x = max_date - (max_date - min_date)*0.1, 
    #                                  y = Inf, 
    #                                  label = metrics_caption),
    #                    # size = ggplot2::rel(5),
    #                     size = 4.7,
    #                     fill = "white", 
    #                     label.size = 0)
    # ggplot2::annotate("richtext", y = Inf, x = max_date - (max_date - min_date)*0.01, vjust=0, hjust = 1, size = 4.7, label = metrics_caption, fill = "white")
    # ggplot2::annotate("text", x = max_date - lubridate::minutes(60), y = max(rainfall_in), vjust=0, hjust = 1, label = metrics_caption)
  

  level_plot %<>% metricsTable_show(metrics_show = metrics_show,
                                    obs_RSPU = obs_RSPU,
                                    obs_infil_inhr = obs_infil_inhr,
                                    obs_draindown_hr = obs_draindown_hr,
                                    obs_overtopping = obs_overtopping,
                                    sim_RSPU = sim_RSPU,
                                    sim_infil_inhr = sim_infil_inhr,
                                    sim_draindown_hr = sim_draindown_hr,
                                    sim_overtopping = sim_overtopping) 

  
  #Calculate max width and set both to that value
  #Grob
  level_grob <- ggplot2::ggplotGrob(level_plot)
  rainfall_grob <- ggplot2::ggplotGrob(rainfall_plot)
  
  #Set max width
  maxWidth = grid::unit.pmax(level_grob$widths[2:9], rainfall_grob$widths[2:9])
  level_grob$widths[2:9] <- maxWidth
  rainfall_grob$widths[2:9] <- maxWidth
  
  #Arrange the plots and export
  combined_plot <- gridExtra::grid.arrange(rainfall_grob, level_grob, #plots
                                           rainfall_legend, level_legend, #legends
                                           ncol = 1,
                                           heights = c(1.1, 2, 0.15, 0.15))
  
  return(combined_plot)
}

# marsBaroRasterPlot --------------------------------------------------------
#' Barometric Pressure Raster Plot
#' 
#' Create a raster plot of barometric pressures from each sensor for each day
#' 
#' @param baro a dataframe with columns: \code{smp_id, baro_psi, day, year}
#' 
#' @return p, a ggplot2 plot
#' 
#' @export
#' 
#' @examples 
#' marsSampleBaro_plot %<>% dplyr::mutate("day" = yday_decimal(marsSampleBaro_plot$dtime_est),
#'                                "year" = lubridate::year(marsSampleBaro_plot$dtime_est))
#' marsBaroRasterPlot(marsSampleBaro_plot)
#'

marsBaroRasterPlot <- function(baro){
  p <- ggplot2::ggplot(baro, ggplot2::aes(x = day, y = smp_id)) +
    ggplot2::facet_grid(. ~ year) +
    ggplot2::geom_tile(ggplot2::aes(fill = baro_psi)) +
    ggplot2::scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdBu")), name = "Pressure (psi)") +
    ggplot2::theme(axis.text=ggplot2::element_text(colour="black", size=15),
          axis.title.x=ggplot2::element_text(colour="black", size=15),
          axis.title.y=ggplot2::element_text(colour="black", size=15),
          legend.text=ggplot2::element_text(size=15),
          legend.title=ggplot2::element_text(size = 15),
          strip.text.x = ggplot2::element_text(size = 15),
          legend.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank()) +
    ggplot2::xlab("Day") + ggplot2::ylab("Baro Sites")
  
  return(p)
  
}


# metricsTable_show ------------------------------------------------------------

#' Add metrics to an existing water level or combined plot
#'
#' Return the gpglot object, with the metrics added to the object as a tableGrob annotation
#'
#' @param in_plot                             ggplot object without annotative metrics table 
#' @param obs_RSPU                            num, Metric: Observed relative percentage of storage used, see \code{marsPeakStorage_percent} (optional)
#' @param obs_infil_inhr                      num, Metric: Observed infiltration rate in inches per hour, see \code{marsInfiltrationRate_inhr} (optional)
#' @param obs_draindown_hr                    num, Metric: Observed draindown time in hours, see \code{marsDraindown_hr} (optional)
#' @param obs_overtopping                     bool, Metric: Observed overtopping boolean, see \code{marsOvertoppingCheck_bool} (optional)
#' @param sim_RSPU                            num, Metric: Simulated relative percentage of storage used, see \code{marsPeakStorage_percent} (optional)
#' @param sim_infil_inhr                      num, Metric: Simulated infiltration rate in inches per hour, see \code{marsInfiltrationRate_inhr} (optional)
#' @param sim_draindown_hr                    num, Metric: Simulated draindown time in hours, see \code{marsDraindown_hr} (optional)
#' @param sim_overtopping                     bool, Metric: Simulated overtopping boolean, see \code{marsOvertoppingCheck_bool} (optional)
#' @param metrics_show                        bool, Default FALSE. TRUE if user wants to include a table of metrics on the plot (optional)
#' 
#' @return Output ggplot object adding metrics when necessary
#' 
#' @examples 
#' level_plot %<>% metricsTable_show(metrics_show = metrics_show)
#'

metricsTable_show <- function(in_plot,
                              metrics_show = FALSE,
                              obs_RSPU = obs_RSPU,
                              obs_infil_inhr = obs_infil_inhr,
                              obs_draindown_hr = obs_draindown_hr,
                              obs_overtopping = obs_overtopping,
                              sim_RSPU = sim_RSPU,
                              sim_infil_inhr = sim_infil_inhr,
                              sim_draindown_hr = sim_draindown_hr,
                              sim_overtopping = sim_overtopping){
  
  if(metrics_show == TRUE){
    
    #set missing values to ""
    if( missing(obs_draindown_hr) ){obs_draindown_hr <- ""}
    if( missing(sim_draindown_hr) ){sim_draindown_hr <- ""}
    if( missing(obs_infil_inhr) ){obs_infil_inhr <- ""}
    if( missing(sim_infil_inhr) ){sim_infil_inhr <- ""}
    if( missing(obs_RSPU) ){obs_RSPU <- ""}
    if( missing(sim_RSPU) ){sim_RSPU <- ""}
    if( missing(obs_overtopping) ){obs_overtopping <- ""}
    if( missing(sim_overtopping) ){sim_overtopping <- ""}
    
    if( is.numeric(obs_infil_inhr) & obs_infil_inhr < 0 ){
      obs_infil_inhr <- paste0("ERR: ", obs_infil_inhr)
    }
    if( is.numeric(sim_infil_inhr) & sim_infil_inhr < 0 ){
      sim_infil_inhr <- paste0("ERR: ", sim_infil_inhr)
    }
    
    if(is.numeric(obs_draindown_hr)){ obs_draindown_hr <- round(obs_draindown_hr,2)}
    if(is.numeric(sim_draindown_hr)){ sim_draindown_hr <- round(sim_draindown_hr,2)}
    if(is.numeric(obs_infil_inhr)){ obs_infil_inhr <- round(obs_infil_inhr,2)}
    if(is.numeric(sim_infil_inhr)){ sim_infil_inhr <- round(sim_infil_inhr,2)}
    if(is.numeric(obs_RSPU)){ obs_RSPU <- round(obs_RSPU,2)}
    if(is.numeric(sim_RSPU)){ sim_RSPU <- round(sim_RSPU,2)}
    
    # browser()
    #------ table version
    metric_table <- as.data.frame(matrix(nrow=4))
    colnames(metric_table) <- "Metrics"
    metric_table$Metrics <- c("Drain down (hrs)",
                              "Infiltration rate (in/hr)",
                              "RSPU (%)",
                              "Overtopping (T/F)")
    
    obs_mets <- c(obs_draindown_hr,obs_infil_inhr, obs_RSPU, obs_overtopping)
    sim_mets <- c(sim_draindown_hr,sim_infil_inhr, sim_RSPU, sim_overtopping)
    
    #add columns if obs/sim exists
    if(sum(is.na(obs_mets)) < 4){metric_table$Observed <- obs_mets}
    if(sum(is.na(sim_mets)) < 4){metric_table$Simulated <- sim_mets}
    
    #remove rows if both are empty
    remove <- c()
    if(sum(metric_table[1,] == "") == 2){ remove <- c(remove,1)}
    if(sum(metric_table[2,] == "") == 2){ remove <- c(remove,2)}
    if(sum(metric_table[3,] == "") == 2){ remove <- c(remove,3)}
    if(sum(metric_table[4,] == "") == 2){ remove <- c(remove,4)}
    
    metric_table <- metric_table[c(1:4)[!(c(1:4) %in% remove)],]
    
    #add table to plot
    level_plot <- level_plot +
      
      ggplot2::annotation_custom (grob = tableGrob(metric_table,
                                                   rows = NULL,
                                                   theme = ggpp::ttheme_gtlight()),
                                  ymin = (storage_depth_ft*0.70),
                                  ymax = (storage_depth_ft*0.95),
                                  xmin = obs_datetime[round(length(obs_datetime)*0.5)],
                                  xmax = obs_datetime[round(length(obs_datetime))])
    return(level_plot)
    
  } else {
    
    return(in_plot)
    
  }
  
}

