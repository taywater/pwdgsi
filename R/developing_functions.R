# Simulated Water Levels Plot NOT CURRENTLY WORKING -------------------------------------------------
#Description of the arguments:

#IN:  event                              Rainfall event ID (grouping variable)
#IN:  df                                 Dataframe passed from nested group in dplyr. The user must specify the column names 
#                                        in the function arguments, though the data is already stored in the dataframe
#IN:  raingage_colname_or_value          Raingage ID, or name of the column containing the Raingage ID information
#IN:  system_colname_or_value            SMP ID, or name of the column containing the system ID
#IN:  datetime_colname                   Name of the column containing the datetime data
#IN:  rainfall_colname                   Name of the column heading containing the rainfall data, in inches
#IN:  storage_depth_colname_or_value_ft  Maximum storage depth, in feet, or name of the column containing the 
#                                        system depth information
#IN:  storage_vol_colname_or_value_ft3   Maximum storage volume (pore space), in cubic feet, or name of the column
#                                        containing the system volume information
#IN:  obswaterlevel_colname              Name of the column containing the observed water level data
#IN:  simwaterlevel_colname              Name of the column containing the observed water level data
#IN:  sim_event_colname                  Name of the column containing the rainfall event ID used for simulated timeseries generation
#IN:  orifice_show                       TRUE if user wants to include the orifice height as dashed line on plot (optional)
#IN:  orifice_height_ft                  Orifice height, in feet, OR column name containing data

#OUT:  Plot of simulated  depth and volume with rainfall

#' Simulated Water Levels Plot
#' 
#' Plot of simulated depth and volume with rainfall
#' 
#' @param event Rainfall Event ID (grouping variable)
#' @param event df Dataframe passed from nested group in dplyr. The user must specify the column names 
#'                                        in the function arguments, though the data is already stored in the dataframe
#' @param raingage_colname_or_value Raingage ID, or name of the column containing the Raingage ID information
#' @param system_colname_or_valuee SMP ID, or name of the column containing the system ID
#' @param datetime_colname Name of the column containing the datetime data
#' @param rainfall_colname Name of the column heading containing the rainfall data, in inches
#' @param storage_depth_colname_or_value_ft Maximum storage depth, in feet, or name of the column containing the 
#'                                        system depth information
#' @param storage_vol_colname_or_value_ft3 Maximum storage volume (pore space), in cubic feet, or name of the column
#'                                       containing the system volume information
#' @param obswaterlevel_colname Name of the column containing the observed water level data
#' @param simwaterlevel_colname Name of the column containing the simulated water level data
#' @param sim_event_colname Name of the column containing the rainfall event ID used for simulated timeseries generation
#' @param orifice_show TRUE if user wants to include the orifice height as dashed line on plot (optional)
#' @param orifice_height_ft Orifice height, in feet, OR column name containing data
#' 
#' @return Plot of simulated depth and volume with rainfall
#' 


simPlotNested <- function(event, #group_by variable, simulated or observed event, for title block
                          df, #nested dataframe
                          #user inputs 
                          raingage_colname_or_value, 
                          system_colname_or_value, 
                          datetime_colname, 
                          rainfall_colname, 
                          storage_depth_colname_or_value_ft, 
                          storage_vol_colname_or_value_ft3,
                          simwaterlevel_colname, 
                          
                          #optional inputs
                          orifice_show = FALSE, #TRUE if user wants to include in plot
                          orifice_height_colname_or_value_ft = NULL 
){
  
  
  #1. Process data
  #1.1 Pull user-defined column names
  #Note: this allows the user to designate any column name in the simulated and observed
  #data processing steps. 
  
  datetime_colname <- rlang::ensym(datetime_colname)
  rainfall_colname <- rlang::ensym(rainfall_colname)
  simwaterlevel_colname <- rlang::ensym(simwaterlevel_colname)
  
  
  #1.2 Rename data
  comb_data <- df %>%
    dplyr::mutate(dtime_est = !!datetime_colname,
                  rainfall_in = !!rainfall_colname,
                  simwaterlevel_ft = !!simwaterlevel_colname,
                  cumulative_rain = 0) #cumulative rainfall
  
  #1.2.1 Force timezone
  comb_data$dtime_est <- lubridate::force_tz(comb_data$dtime_est, tz = "EST")
  
  #1.3 Find user-defined columns for raingage and SMP specifications
  
  #1.3.1 Storage depth
  #Was storage depth explicitly defined?
  if(is.numeric(storage_depth_colname_or_value_ft)){
    max_depth_ft <- storage_depth_colname_or_value_ft[1] #allows user to define value outside of dataframe
    max_vol_ft3 <- depth.to.vol(max)
    
  }else{ #Pull depth if joined to dataframe
    storage_depth_colname_or_value_ft <- rlang::ensym(storage_depth_colname_or_value_ft)
    comb_data <- comb_data %>% 
      dplyr::mutate(stor_depth_ft = !!storage_depth_colname_or_value_ft)
    max_depth_ft <- comb_data$stor_depth_ft[1]
  }
  
  #1.3.2 Storage volume
  #Was storage volume explicitly defined?
  if(is.numeric(storage_vol_colname_or_value_ft3)){
    max_vol_ft3 <- storage_vol_colname_or_value_ft3[1]
  }else{ #Pull structure info, if joined to dataframe
    storage_vol_colname_or_value_ft3 <- rlang::ensym(storage_vol_colname_or_value_ft3)
    comb_data <- comb_data %>% 
      dplyr::mutate(stor_vol_ft3 = !!storage_vol_colname_or_value_ft3)
    max_vol_ft3 <- comb_data$stor_vol_ft3[1]
  }
  
  
  #1.3.3 SMP ID
  #is user-defined column name containing structure ID specified?
  if(!!system_colname_or_value %in% colnames(comb_data)){
    system_colname_or_value <- rlang::ensym(system_colname_or_value)
    comb_data <- comb_data %>%
      dplyr::mutate(structure = !!system_colname_or_value) 
    temp <- comb_data %>%
      filter(is.na(structure) == FALSE)
    structure <- temp$structure[1]
  }else{ #assume structure ID is explicitly defined by user
    structure <- system_colname_or_value[1]
  }
  
  #1.3.4 Raingage ID
  #is user-defined column name containing raingage ID specified?  
  if(!!raingage_colname_or_value %in% colnames(comb_data)){
    raingage_colname_or_value <- rlang::ensym(raingage_colname_or_value)
    comb_data <- comb_data %>%
      dplyr::mutate(raingage = !!raingage_colname_or_value) 
    
    temp <- comb_data %>%
      filter(is.na(raingage) == FALSE)
    raingage <- temp$raingage[1]
    
  }else{ #assume raingage ID is explicitly defined by user
    raingage <- raingage_colname_or_value[1]
  }
  
  
  #1.3.5 Orifice Plotting Option
  #Does the user want to show the orifice height?
  if(orifice_show == TRUE){
    if(!!orifice_height_ft %in% colnames(comb_data)){
      orifice_height_ft <- rlang::ensym(orifice_height_ft)
      comb_data <- comb_data %>%
        dplyr::mutate(orifice_height_ft = !!orifice_height_ft) 
      
      temp <- comb_data%>%
        filter(is.na(orifice_height_ft) == FALSE)
      orifice_plot <- temp$orifice_height_ft[1]
    }else{ #assume orifice height is explicitly defined by user
      orifice_plot <- orifice_height_ft[1]
    } 
  }else{
    orifice_plot <- 0 #line will be covered by bottom of structure if option is not selected
  }
  
  #1.4 Check that data is associated with event - grob objects can't be empty for plotting
  if(nrow(comb_data) == 0){
    stop(paste0("No data loaded in Event ", event, "."))
  }else{
    
    #1.5 Assume minimum interval  
    min_interval <- lubridate::minutes(15)
    
    
    #1.7 Calculate cumulative rainfall
    comb_data <- comb_data %>% 
      dplyr::mutate(rainfall_in = tidyr::replace_na(rainfall_in, 0)) %>% #first replace NA's
      dplyr::mutate(cumulative_rain = cumsum(rainfall_in))
    
    #2. Calculate plotting parameters
    
    #2.1 Calculate rainfall plotting limits (y-axis, rainfall plot)
    min_rain <- 0
    max_rain <- max(comb_data$rainfall_in, na.rm = TRUE)
    
    #calculate scaling factor for secondary y-axis for cumulative rainfall
    max_cumulative_scaling <- max(1.1*comb_data$cumulative_rain, na.rm = TRUE)/max_rain 
    
    range_rainfall <- max_rain - min_rain
    
    if(range_rainfall < 0.1){
      max_rain <- 0.1 #set minimum rainfall range to 0.1 inches 
      range_rainfall <- max_rain - min_rain #recalculate if necessary
      max_cumulative_scaling<- max(1.1*comb_data$cumulative_rain, na.rm = TRUE)/max_rain #recalculate scaling secondary
    }
    
    ##Scale fix for events with only one measurement interval
    if(nrow(comb_data)==1){
      max_cumulative_scaling <- max(comb_data$cumulative_rain, na.rm = TRUE)/max_rain #recalculate scaling secondary
      
    }
    
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
    
    
    
    #2.2 Calculate date plotting limits(x-axis) 
    #Calculate minimum and maximum data values
    min_date <- min(comb_data$dtime_est, na.rm = TRUE)
    max_date <- max(comb_data$dtime_est, na.rm = TRUE) #+ hours(6)
    
    #Calculate scaling factor for secondary y-axis for volume
    max_vol_scaling <- max_vol_ft3/max_depth_ft
    
    #Calculate ranges in values to set axis breaks by category
    event_duration <- max_date - min_date
    #range_WL <- max_WL - min_WL
    
    #Set axis end to time when observed water level drops below 0 during recession
    #First Re-rerun detectEvents to identify end of rainfall event
    rain_event_end <- comb_data %>%
      dplyr::mutate(rainfall_in = tidyr::replace_na(rainfall_in, 0)) %>%
      dplyr::filter(rainfall_in != 0) %>%
      dplyr::arrange(dtime_est) %>% #confirm that dtime is in ascending order
      dplyr::mutate(rain_event = detectEvents(dtime_est, rainfall_in)) %>%
      dplyr::filter(rain_event== 1) %>%
      dplyr::slice(dplyr::n()) #pull last row (corresponds to end of first rainfall event)
    
    #Filter by end of rain event and simulated water level below zero
    sim_recession_end <- comb_data %>%
      dplyr::filter(dtime_est > rain_event_end$dtime_est) %>% #pull recession period
      dplyr::filter(simwaterlevel_ft < 0.0001) %>%
      dplyr::slice(1L)
    
    #Set value to max_date, if earlier than end of processed dataframe (comb_data)
    if(nrow(sim_recession_end) > 0 ){ #check that simulated water level does drop below 0
      max_date <- sim_recession_end$dtime_est + lubridate::hours(6)
    }
    
    #Recalculate ranges in values to set axis breaks by category
    event_duration <- max_date - min_date
    
    #set date marker offset by duration
    if(units(event_duration) == "days"){
      marker_scale <- 0.02
    }else{
      marker_scale <- 0.015
    }
    
    
    #2.3 Calculations for dashed vertical line at day boundaries
    day_strip <- lubridate::date(min_date)
    day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "EST"), by = "day", length.out = 14), tz = "EST")
    
    #2.4 Calculate axis breaks based on plotting limits
    #Select major x-axis breaks based on event duration (all extend observed record by 12 hours)
    major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"), tz = "EST") 
    
    #All plots use one-hour interval for minor x-axis breaks
    minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - hours(12), max_date + hours(6), by = "hour"), tz = "EST")  
    
    
    #2.5 Generate title block
    startdate <- min(comb_data$dtime_est)
    title_text <- paste0("Simulated Water Levels\nSMP ID: ", structure,
                         " | Raingage: ", raingage,
                         " | Event: ", event[1],
                         " | Start Date and Time: ", 
                         scales::date_format("%Y-%m-%d %H:%M", tz = "EST")(startdate),
                         sep = "")
    
    
    #3. Generate individual plots
    #3.1 Water Level (Simulated)
    sim_plot <- 
      ggplot2::ggplot(data = comb_data,
                      aes(x = dtime_est,
                          y = simwaterlevel_ft)) +
      
      #orifice (covered by structure bottom if option is not selected)
      geom_hline(yintercept = orifice_plot, color = "grey", linetype = 2, size = 1.2) +
      
      #Structure top and bottom
      geom_hline(yintercept = 0, color = "grey", size = 1.2)+ #bottom
      geom_hline(yintercept = max_depth_ft, color = "grey", size = 1.2)+ #top
      
      
      
      #simulated water level
      geom_line(aes(color = "Simulated Water Level"),
                linetype = "dotted",
                size = 2
      ) +
      
      #Format colors & legend
      scale_color_manual(values = c("cornflowerblue"),   
                         guide = guide_legend(title = NULL,
                                              override.aes = list(
                                                linetype = c("solid"))) # simulated (dotted format is difficult to read - intentionally set as solid)
      )+
      
      #Day boundaries
      geom_vline(xintercept = day_marker, color = "black", linetype = "dashed", size = 1.2) + #date boundaries
      
      annotate("text", x = day_marker-marker_scale*event_duration, 
               y = 0.8*max_depth_ft, 
               label = day_marker,
               angle = 90, 
               size = rel(5))+ #5
      
      
      #Formatting
      theme_bw() + # a basic black and white theme
      
      scale_x_datetime(
        name = " ", # x axis label
        labels = scales::date_format("%H:%M", "EST"),
        limits = c(min_date - min_interval, max_date), # set x axis limits
        breaks = major_date_breaks,
        minor_breaks = minor_date_breaks
      ) +
      
      scale_y_continuous(
        breaks = seq(0, max_depth_ft, by = 0.5),
        minor_breaks = seq(-0.5,2*max_depth_ft, by = 0.1),
        sec.axis = sec_axis(~.*max_vol_scaling, name = bquote('Volume ( '*ft^3*')'))
      ) +
      
      
      labs(
        y = "Water Level (ft)"
      ) +
      
      theme(
        #text = element_text(size = rel(2)), #size previously set to 16
        axis.title.y = element_text(size = rel(1.2), color = "black"),
        axis.text.x = element_text(size = rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
        axis.text.y = element_text(size = rel(1.2), color = "black"), # set font size and color of y axis text
        panel.background =  element_rect(fill = "white", colour = NA), # set white background
        panel.border =      element_rect(fill = NA, colour="black"), # set black border
        panel.grid.major =  element_line(colour = "grey70", size = 0.2), # set major grid lines
        panel.grid.minor =  element_line(colour = "grey90", size = 0.5), # set minor grid lines
        legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
        legend.title=element_blank())
    
    
    
    #3.2 Rainfall Plot
    #Adapted from code sent by Taylor Heffnernan on 4/17/2019
    
    rainfallplot <- ggplot2::ggplot(data = comb_data, 
                                    aes(x = dtime_est,
                                        y = cumulative_rain/max_cumulative_scaling))+
      
      geom_area(aes(fill = "Cumulative Rainfall"),
                color = "grey32",
                alpha = 0.2
      ) +
      
      geom_bar(data = comb_data,
               aes(x = dtime_est, y = rainfall_in,
                   fill = "Rainfall"),
               stat="identity"
      ) +
      
      labs(x = "", y = "Rainfall (in)",
           title = title_text) +
      
      #Format colors & legend
      scale_fill_manual(values = c( "slateblue1", "cornflowerblue"),   
                        guide = guide_legend(title = NULL,
                                             override.aes = list(
                                               alpha = c(0.2,1))) #cumulative rainfall, rainfall
      )+
      
      
      #Day boundaries
      geom_vline(xintercept = day_marker, color = "black", linetype = "dashed", size = 1.2) + #date boundaries
      
      #same scale as CWL plot
      scale_x_datetime(
        name = " ", # x axis label
        labels = scales::date_format("%H:%M", "EST"),
        limits = c(min_date - min_interval, max_date), # set x axis limits
        breaks = major_date_breaks,
        minor_breaks = minor_date_breaks
      ) +
      
      #scale_y_reverse turns the rainfall plot upside down
      scale_y_reverse(#expand=c(0,0),  #Don't show extra space above or below the y limits 
        breaks = seq(min_rain, max_rain, by = rain_major_interval),
        minor_breaks = seq(min_rain, max_rain, by = rain_minor_interval),
        sec.axis = sec_axis(~.*max_cumulative_scaling, name = "Cumulative Rainfall (in)"),
        labels = function(x) format(x,nsmall = 2,scientific = FALSE)) + #2 subdecimal places
      
      theme_bw() + 
      theme(#text = element_text(size = rel(8)), #size previously set to 14
        #title =  element_text(size = rel(1.4)),
        axis.title.y = element_text(size = rel(1.24)),
        axis.text.y = element_text(size = rel(1.1), color = "black"), #element_text(size = rel(1.2)),
        axis.text.x = element_blank(),                  #Even though element_blank() removes X-axis text,
        plot.margin = unit(c(0.1,0.21,-0.2,0.09), "in"),  #ggplot2 still makes room for it. Clip the margin on the bottom edge
        panel.background =  element_rect(fill = "white", colour = NA), # set white background
        panel.border =      element_rect(fill = NA, colour="black"), # set black border
        panel.grid.major =  element_line(colour = "grey70", size = 0.2), # set major grid lines
        panel.grid.minor =  element_line(colour = "grey90", size = 0.5), # set minor grid lines
        legend.position = "bottom", #format legend (to be compiled with CWL plot in grid.arrange())
        legend.title=element_blank())
    
    
    #4. Combine Plots
    
    #4.1 Format legends
    
    #4.1.1 Save out legends from individual plots
    sim_legend <- get_legend(sim_plot)
    rain_legend <- get_legend(rainfallplot)
    
    #4.1.2 Remove legends from indiviudal plots
    rainfallplot <- rainfallplot + ggplot2::theme(legend.position = "none")
    sim_plot <- sim_plot + ggplot2::theme(legend.position = "none")
    
    #4.1 Calculate max with and set both to that value
    #Export combined plot
    cwlGrob <- ggplot2::ggplotGrob(sim_plot)
    rainGrob <- ggplot2::ggplotGrob(rainfallplot)
    
    #set max width of both plots
    maxWidth = grid::unit.pmax(cwlGrob$widths[2:5], rainGrob$widths[2:5])
    cwlGrob$widths[2:5] <- maxWidth
    rainGrob$widths[2:5] <- maxWidth
    
    
    #4.2 Arrange the plots together and export
    gridExtra::grid.arrange(rainGrob, cwlGrob, #plots
                            rain_legend, sim_legend, #legends
                            ncol = 1,
                            heights = c(1.1, 2, 0.15, 0.15))
    
    
  } #if statement to confirm that data is loaded for plots 
}# Function end


# Observed and Simulated Water Levels Plot TO BE REMOVED----------------------------------
#Description of the arguments:

#IN:  event                              Rainfall event ID (grouping variable)
#IN:  df                                 Dataframe passed from nested group in dplyr. The user must specify the column names 
#                                        in the function arguments, though the data is already stored in the dataframe
#IN:  raingage_colname_or_value          Raingage ID, or name of the column containing the Raingage ID information
#IN:  system_colname_or_value            SMP ID, or name of the column containing the system ID
#IN:  datetime_colname                   Name of the column containing the datetime data
#IN:  rainfall_colname                   Name of the column heading containing the rainfall data, in inches
#IN:  storage_depth_colname_or_value_ft  Maximum storage depth, in feet, or name of the column containing the 
#                                        system depth information
#IN:  storage_vol_colname_or_value_ft3   Maximum storage volume (pore space), in cubic feet, or name of the column
#                                        containing the system volume information
#IN:  obswaterlevel_colname              Name of the column containing the observed water level data
#IN:  simwaterlevel_colname              Name of the column containing the observed water level data
#IN:  sim_event_colname                  Name of the column containing the rainfall event ID used for simulated timeseries generation
#IN:  orifice_show                       TRUE if user wants to include the orifice height as dashed line on plot (optional)
#IN:  orifice_height_ft                  Orifice height, in feet, OR column name containing data

#OUT:  Plot of simulated and observed depth and volume with rainfall

#' Observed and Simulated Water Levels Plot
#'
#' Create a plot of simulated and observed depth and volume with rainfall
#' 
#' @param  event                              Rainfall event ID (grouping variable)
#' @param  df                                 Dataframe passed from nested group in dplyr. The user must specify the column names 
#'                                        in the function arguments, though the data is already stored in the dataframe
#' @param  raingage_colname_or_value          Raingage ID, or name of the column containing the Raingage ID information
#' @param  system_colname_or_value            SMP ID, or name of the column containing the system ID
#' @param  datetime_colname                   Name of the column containing the datetime data
#' @param  rainfall_colname                   Name of the column heading containing the rainfall data, in inches
#' @param  storage_depth_colname_or_value_ft  Maximum storage depth, in feet, or name of the column containing the 
#'                                        system depth information
#' @param  storage_vol_colname_or_value_ft3   Maximum storage volume (pore space), in cubic feet, or name of the column
#'                                        containing the system volume information
#' @param  obswaterlevel_colname              Name of the column containing the observed water level data
#' @param  simwaterlevel_colname              Name of the column containing the observed water level data
#' @param  sim_event_colname                  Name of the column containing the rainfall event ID used for 
#'                                        simulated timeseries generation
#' @param  orifice_show                       TRUE if user wants to include the orifice height as dashed line on plot (optional)
#' @param  orifice_height_ft                  Orifice height, in feet, OR column name containing data
#' 
#' @return Output is a plot of simulated and observed water depth and volume with rainfall




obsPlotNested <- function(event, #group_by variable, simulated or observed event, for title block
                          df, #nested dataframe
                          #user inputs 
                          raingage_colname_or_value, 
                          system_colname_or_value, 
                          datetime_colname, 
                          rainfall_colname, 
                          storage_depth_colname_or_value_ft, 
                          storage_vol_colname_or_value_ft3,
                          obswaterlevel_colname, 
                          simwaterlevel_colname, 
                          sim_event_colname, 
                          #optional inputs
                          orifice_show = FALSE, #TRUE if user wants to include in plot
                          orifice_height_colname_or_value_ft = NULL 
){
  
  
  #1. Process data
  #1.1 Pull user-defined column names
  #Note: this allows the user to designate any column name in the simulated and observed
  #data processing steps. 
  
  datetime_colname <- rlang::ensym(datetime_colname)
  rainfall_colname <- rlang::ensym(rainfall_colname)
  obswaterlevel_colname <- rlang::ensym(obswaterlevel_colname)
  simwaterlevel_colname <- rlang::ensym(simwaterlevel_colname)
  
  
  #1.2 Rename data
  comb_data <- df %>%
    dplyr::mutate(dtime_est = !!datetime_colname,
                  rainfall_in = !!rainfall_colname,
                  simwaterlevel_ft = !!simwaterlevel_colname,
                  obswaterlevel_ft = !!obswaterlevel_colname,
                  cumulative_rain = 0) #cumulative rainfall
  
  #1.2.1 Force timezone
  comb_data$dtime_est <- lubridate::force_tz(comb_data$dtime_est, tz = "EST")
  
  #1.3 Find user-defined columns for raingage and SMP specifications
  
  #1.3.1 Storage depth
  #Was storage depth explicitly defined?
  if(is.numeric(storage_depth_colname_or_value_ft)){
    max_depth_ft <- storage_depth_colname_or_value_ft[1] #allows user to define value outside of dataframe
    max_vol_ft3 <- depth.to.vol(max)
    
  }else{ #Pull depth if joined to dataframe
    storage_depth_colname_or_value_ft <- rlang::ensym(storage_depth_colname_or_value_ft)
    comb_data <- comb_data %>% 
      dplyr::mutate(stor_depth_ft = !!storage_depth_colname_or_value_ft)
    max_depth_ft <- comb_data$stor_depth_ft[1]
  }
  
  #1.3.2 Storage volume
  #Was storage volume explicitly defined?
  if(is.numeric(storage_vol_colname_or_value_ft3)){
    max_vol_ft3 <- storage_vol_colname_or_value_ft3[1]
  }else{ #Pull structure info, if joined to dataframe
    storage_vol_colname_or_value_ft3 <- rlang::ensym(storage_vol_colname_or_value_ft3)
    comb_data <- comb_data %>% 
      dplyr::mutate(stor_vol_ft3 = !!storage_vol_colname_or_value_ft3)
    max_vol_ft3 <- comb_data$stor_vol_ft3[1]
  }
  
  
  #1.3.3 SMP ID
  #is user-defined column name containing structure ID specified?
  if(!!system_colname_or_value %in% colnames(comb_data)){
    system_colname_or_value <- rlang::ensym(system_colname_or_value)
    comb_data <- comb_data %>%
      dplyr::mutate(structure = !!system_colname_or_value) 
    temp <- comb_data %>%
      filter(is.na(structure) == FALSE)
    structure <- temp$structure[1]
  }else{ #assume structure ID is explicitly defined by user
    structure <- system_colname_or_value[1]
  }
  
  #1.3.4 Raingage ID
  #is user-defined column name containing raingage ID specified?  
  if(!!raingage_colname_or_value %in% colnames(comb_data)){
    raingage_colname_or_value <- rlang::ensym(raingage_colname_or_value)
    comb_data <- comb_data %>%
      dplyr::mutate(raingage = !!raingage_colname_or_value) 
    
    temp <- comb_data %>%
      filter(is.na(raingage) == FALSE)
    raingage <- temp$raingage[1]
    
  }else{ #assume raingage ID is explicitly defined by user
    raingage <- raingage_colname_or_value[1]
  }
  
  #1.3.5 Orifice Plotting Option
  #Does the user want to show the orifice height?
  if(orifice_show == TRUE){
    if(!!orifice_height_ft %in% colnames(comb_data)){
      orifice_height_ft <- rlang::ensym(orifice_height_ft)
      comb_data <- comb_data %>%
        dplyr::mutate(orifice_height_ft = !!orifice_height_ft) 
      
      temp <- comb_data%>%
        filter(is.na(orifice_height_ft) == FALSE)
      orifice_plot <- temp$orifice_height_ft[1]
    }else{ #assume orifice height is explicitly defined by user
      orifice_plot <- orifice_height_ft[1]
    } 
  }else{
    orifice_plot <- 0 #line will be covered by bottom of structure if option is not selected
  }
  
  #1.4 Remove overlapping simulated events 
  #(synthetic recession period, if interevent period is less than 72 hours)
  sim_event_colname <- rlang::ensym(sim_event_colname)
  comb_data <- comb_data %>%
    dplyr::filter(event == !!sim_event_colname)
  
  #1.5 Set negative water levels to zero (below sump depth)
  comb_data$obswaterlevel_ft[which(comb_data$obswaterlevel_ft < 0)] <- 0
  
  #1.6 Check that data is associated with event - grob objects can't be empty for plotting
  if(nrow(comb_data) == 0){
    stop(paste0("No data loaded in Event ", event, "."))
  }else{
    
    #1.7 Assume minimum interval  
    min_interval <- lubridate::minutes(15)
    
    
    #1.9 Calculate cumulative rainfall
    comb_data <- comb_data %>% 
      dplyr::mutate(rainfall_in = tidyr::replace_na(rainfall_in, 0)) %>% #first replace NA's
      dplyr::mutate(cumulative_rain = cumsum(rainfall_in))
    
    #1.10 QC check for observed data record
    #Using code from detectEvents
    prepseries <- comb_data %>%
      dplyr::mutate(lag_time = dplyr::lag(dtime_est, 1)) %>%
      dplyr::mutate(gap_hr = difftime(dtime_est, lag_time, unit = "hours")) %>%
      dplyr::filter(gap_hr > 6)
    
    if(nrow(prepseries) > 0){
      message(paste0("Warning: Missing values in observed time series."))
      warning_label <- "Warning: Missing values in observed time series."
    }else{
      warning_label <- ""
    }
    
    
    #2. Calculate plotting parameters
    
    #2.1 Calculate rainfall plotting limits (y-axis, rainfall plot)
    min_rain <- 0
    max_rain <- max(comb_data$rainfall_in, na.rm = TRUE)
    
    #calculate scaling factor for secondary y-axis for cumulative rainfall
    max_cumulative_scaling <- max(1.1*comb_data$cumulative_rain, na.rm = TRUE)/max_rain 
    
    range_rainfall <- max_rain - min_rain
    
    if(range_rainfall < 0.1){
      max_rain <- 0.1 #set minimum rainfall range to 0.1 inches 
      range_rainfall <- max_rain - min_rain #recalculate if necessary
      max_cumulative_scaling<- max(1.1*comb_data$cumulative_rain, na.rm = TRUE)/max_rain #recalculate scaling secondary
    }
    
    ##Scale fix for events with only one measurement interval
    if(nrow(comb_data)==1){
      max_cumulative_scaling <- max(comb_data$cumulative_rain, na.rm = TRUE)/max_rain #recalculate scaling secondary
      
    }
    
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
    
    
    
    #2.2 Calculate date plotting limits(x-axis) 
    #Calculate minimum and maximum data values
    min_date <- min(comb_data$dtime_est, na.rm = TRUE)
    max_date <- max(comb_data$dtime_est, na.rm = TRUE) #+ hours(6)
    
    #Calculate scaling factor for secondary y-axis for volume
    max_vol_scaling <- max_vol_ft3/max_depth_ft
    
    
    #Set axis end to time when observed simulated water level drops below 0 during recession (whichever happens last)
    #First Re-rerun detectEvents to identify end of rainfall event
    rain_event_end <- comb_data %>%
      dplyr::mutate(rainfall_in = tidyr::replace_na(rainfall_in, 0)) %>%
      dplyr::filter(rainfall_in != 0) %>%
      dplyr::arrange(dtime_est) %>% #confirm that dtime is in ascending order
      dplyr::mutate(rain_event = detectEvents(dtime_est, rainfall_in)) %>%
      dplyr::filter(rain_event== 1) %>%
      dplyr::slice(dplyr::n()) #pull last row (corresponds to end of first rainfall event)
    
    #Filter by end of rain event and simulated water level below zero
    sim_recession_end <- comb_data %>%
      dplyr::filter(dtime_est > rain_event_end$dtime_est) %>% #pull recession period
      dplyr::filter(simwaterlevel_ft < 0.0001) %>%
      dplyr::slice(1L)
    
    #Filter by end of rain event and observed water level below zero
    obs_recession_end <- comb_data %>%
      dplyr::filter(dtime_est > rain_event_end$dtime_est) %>%
      dplyr::filter(obswaterlevel_ft < 0.0001) %>%
      dplyr::slice(1L)
    
    #Check if simulated water level drops below 0 later than observed
    if(nrow(sim_recession_end) > 0 ){ 
      if(nrow(obs_recession_end) > 0){
        end_date <- max(sim_recession_end$dtime_est, obs_recession_end$time_est)
        max_date <- end_date + lubridate::hours(6)
      }else{
        #Set value to max_date, if earlier than end of processed dataframe (comb_data)
        max_date <- sim_recession_end$dtime_est + lubridate::hours(6)
      }
    }
    
    #Check if observed water level drops below 0 later than simulated
    if(nrow(obs_recession_end) > 0 ){ 
      if(nrow(sim_recession_end) > 0){
        end_date <- max(sim_recession_end$dtime_est, obs_recession_end$dtime_est)
        max_date <- end_date + lubridate::hours(6)
      }else{
        max_date <- obs_recession_end$dtime_est + lubridate::hours(6)
      }
    }
    
    #Calculate ranges in values to set axis breaks by category
    event_duration <- max_date - min_date
    
    #set date marker offset by duration
    if(units(event_duration) == "days"){
      marker_scale <- 0.02
    }else{
      marker_scale <- 0.015
    }
    
    
    #2.3 Calculations for dashed vertical line at day boundaries
    day_strip <- lubridate::date(min_date)
    day_marker <- lubridate::force_tz(seq.POSIXt(as.POSIXlt(day_strip, tz = "EST"), by = "day", length.out = 14), tz = "EST")
    
    #2.4 Calculate axis breaks based on plotting limits
    #Select major x-axis breaks based on event duration (all extend observed record by 12 hours)
    major_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1], max_date, by = "12 hours"), tz = "EST") 
    
    #All plots use one-hour interval for minor x-axis breaks
    minor_date_breaks <- lubridate::force_tz(seq.POSIXt(day_marker[1] - hours(12), max_date + hours(6), by = "hour"), tz = "EST")  
    
    
    #2.5 Generate title block
    startdate <- min(comb_data$dtime_est)
    title_text <- paste0("Observed and Simulated Water Levels\nSMP ID: ", structure,
                         " | Raingage: ", raingage,
                         " | Event: ", event[1],
                         " | Start Date and Time: ", 
                         scales::date_format("%Y-%m-%d %H:%M", tz = "EST")(startdate),
                         sep = "")
    
    
    #3. Generate individual plots
    #3.1 Water Level (observed and simulated)
    sim_plot <- 
      ggplot2::ggplot(data = comb_data,
                      aes(x = dtime_est,
                          y = simwaterlevel_ft)) +
      
      #orifice (covered by structure bottom if option is not selected)
      geom_hline(yintercept = orifice_plot, color = "grey", linetype = 2, size = 1.2) +
      
      #Structure top and bottom
      geom_hline(yintercept = 0, color = "grey", size = 1.2)+ #bottom
      geom_hline(yintercept = max_depth_ft, color = "grey", size = 1.2)+ #top
      
      
      
      #simulated water level
      geom_line(aes(color = "Simulated Water Level"),
                linetype = "dotted",
                size = 2
      ) +
      
      #Observed water level
      geom_line(data = comb_data,
                aes(x = dtime_est,
                    y = obswaterlevel_ft,
                    color = "Observed Water Level"),
                size = 2
      ) +
      
      #Format colors & legend
      scale_color_manual(values = c( "salmon", "cornflowerblue"),   
                         guide = guide_legend(title = NULL,
                                              override.aes = list(
                                                linetype = c("solid", "solid"))) #observed, simulated (dotted format is difficult to read - intentionally set as solid)
      )+
      
      #Day boundaries
      geom_vline(xintercept = day_marker, color = "black", linetype = "dashed", size = 1.2) + #date boundaries
      
      annotate("text", x = day_marker-marker_scale*event_duration, 
               y = 0.8*max_depth_ft, 
               label = day_marker,
               angle = 90, 
               size = rel(5))+ #5
      
      #Warning message for data gaps in observed record
      annotate("text", x = day_marker[1]+1,
               y = 0.5*max_depth_ft,
               label = warning_label, #empty if no warning
               hjust = 0,
               color = "red",
               size = rel(5))+
      
      
      #Formatting
      theme_bw() + # a basic black and white theme
      
      scale_x_datetime(
        name = " ", # x axis label
        labels = scales::date_format("%H:%M", "EST"),
        limits = c(min_date - min_interval, max_date), # set x axis limits
        breaks = major_date_breaks,
        minor_breaks = minor_date_breaks
      ) +
      
      scale_y_continuous(
        breaks = seq(0, max_depth_ft+1, by = 0.5),
        minor_breaks = seq(-0.5,2*max_depth_ft, by = 0.1),
        sec.axis = sec_axis(~.*max_vol_scaling, name = bquote('Volume ( '*ft^3*')'))
      ) +
      
      
      labs(
        y = "Water Level (ft)"
      ) +
      
      theme(
        #text = element_text(size = rel(2)), #size previously set to 16
        axis.title.y = element_text(size = rel(1.2), color = "black"),
        axis.text.x = element_text(size = rel(1.2), color = "black"), # set font size and color of x axis text #size previously set to 14
        axis.text.y = element_text(size = rel(1.2), color = "black"), # set font size and color of y axis text
        panel.background =  element_rect(fill = "white", colour = NA), # set white background
        panel.border =      element_rect(fill = NA, colour="black"), # set black border
        panel.grid.major =  element_line(colour = "grey70", size = 0.2), # set major grid lines
        panel.grid.minor =  element_line(colour = "grey90", size = 0.5), # set minor grid lines
        legend.position = "bottom", #format legend (to be compiled with rainfall plot in grid.arrange())
        legend.title=element_blank())
    
    
    
    #3.2 Rainfall Plot
    #Adapted from code sent by Taylor Heffnernan on 4/17/2019
    
    rainfallplot <- ggplot2::ggplot(data = comb_data, 
                                    aes(x = dtime_est,
                                        y = cumulative_rain/max_cumulative_scaling))+
      
      geom_area(aes(fill = "Cumulative Rainfall"),
                color = "grey32",
                alpha = 0.2
      ) +
      
      geom_bar(data = comb_data,
               aes(x = dtime_est, y = rainfall_in,
                   fill = "Rainfall"),
               stat="identity"
      ) +
      
      labs(x = "", y = "Rainfall (in)",
           title = title_text) +
      
      #Format colors & legend
      scale_fill_manual(values = c( "slateblue1", "cornflowerblue"),   
                        guide = guide_legend(title = NULL,
                                             override.aes = list(
                                               alpha = c(0.2,1))) #cumulative rainfall, rainfall
      )+
      
      
      #Day boundaries
      geom_vline(xintercept = day_marker, color = "black", linetype = "dashed", size = 1.2) + #date boundaries
      
      #same scale as CWL plot
      scale_x_datetime(
        name = " ", # x axis label
        labels = scales::date_format("%H:%M", "EST"),
        limits = c(min_date - min_interval, max_date), # set x axis limits
        breaks = major_date_breaks,
        minor_breaks = minor_date_breaks
      ) +
      
      #scale_y_reverse turns the rainfall plot upside down
      scale_y_reverse(#expand=c(0,0),  #Don't show extra space above or below the y limits 
        breaks = seq(min_rain, max_rain, by = rain_major_interval),
        minor_breaks = seq(min_rain, max_rain, by = rain_minor_interval),
        sec.axis = sec_axis(~.*max_cumulative_scaling, name = "Cumulative Rainfall (in)"),
        labels = function(x) format(x,nsmall = 2,scientific = FALSE)) + #2 subdecimal places
      
      theme_bw() + 
      theme(#text = element_text(size = rel(8)), #size previously set to 14
        #title =  element_text(size = rel(1.4)),
        axis.title.y = element_text(size = rel(1.24)), #1.25
        axis.text.y = element_text(size = rel(1.1), color = "black"), #element_text(size = rel(1.2)),
        axis.text.x = element_blank(),                  #Even though element_blank() removes X-axis text,
        plot.margin = unit(c(0.1,0.21,-0.2,0.09), "in"),  #ggplot2 still makes room for it. Clip the margin on the bottom edge
        panel.background =  element_rect(fill = "white", colour = NA), # set white background
        panel.border =      element_rect(fill = NA, colour="black"), # set black border
        panel.grid.major =  element_line(colour = "grey70", size = 0.2), # set major grid lines
        panel.grid.minor =  element_line(colour = "grey90", size = 0.5), # set minor grid lines
        legend.position = "bottom", #format legend (to be compiled with CWL plot in grid.arrange())
        legend.title=element_blank())
    
    
    #4. Combine Plots
    
    #4.1 Format legends
    
    #4.1.1 Save out legends from individual plots
    sim_legend <- get_legend(sim_plot)
    rain_legend <- get_legend(rainfallplot)
    
    #4.1.2 Remove legends from indiviudal plots
    rainfallplot <- rainfallplot + ggplot2::theme(legend.position = "none")
    sim_plot <- sim_plot + ggplot2::theme(legend.position = "none")
    
    #4.1 Calculate max with and set both to that value
    #Export combined plot
    cwlGrob <- ggplot2::ggplotGrob(sim_plot)
    rainGrob <- ggplot2::ggplotGrob(rainfallplot)
    
    #set max width of both plots
    maxWidth = grid::unit.pmax(cwlGrob$widths[2:5], rainGrob$widths[2:5])
    cwlGrob$widths[2:5] <- maxWidth
    rainGrob$widths[2:5] <- maxWidth
    
    
    #4.2 Arrange the plots together and export
    gridExtra::grid.arrange(rainGrob, cwlGrob, #plots
                            rain_legend, sim_legend, #legends
                            ncol = 1,
                            heights = c(1.1, 2, 0.15, 0.15))
    
    
  } #if statement to confirm that data is loaded for plots 
}# Function end
