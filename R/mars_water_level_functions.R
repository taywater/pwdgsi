# marsSaturatedPerformance_inhr ------------------------------------------
#' Saturated Performance 
#' 
#' Estimated either infiltration rate or recession rate, as specified, based on observed data
#' 
#' @param  event                Rainfall event ID (grouping variable)
#' @param  dtime_est            A vector of POSIXct date times, in ascending order
#' @param  rainfall_in          Rainfall depths during periods corresponding to times in  dtime_est (in)
#' @param  dcia_ft2             Directly connected impervious area (sf)
#' @param  orifice_height_ft    Orifice height (ft)
#' @param  orifice_diam_in      Orifice diameter (in)
#' @param  storage_depth_ft     Maximum storage depth of system (ft)
#' @param  storage_vol_ft3      Maximum storage volume (pore space) of system, in cubic feet
#' @param  waterlevel_ft        Observed water level data (ft)
#' @param  discharge_coeff      Orifice discharge coefficient
#' @param  type                 "infiltration" or "recession"
#' 
#' @return Output is estimated infiltration rate (in/hr). These outputs are codes for the following messages: 
#'  \describe{
#'        \item{\code{-900}}{Event does not include observation data that approximately equals 5 or 7 in. water depth}
#'        \item{\code{-910}}{Code captures rising limb in event.}
#'        \item{\code{-920}}{Rainfall occurs during recession period between 7 in. and 5in. }
#'  }
#' 
#' @seealso \code{\link{marsUnderdrainOutflow_cf}}
#' 
#' @export
#' 
#' @examples
#' obs_250_fill %>%
#'   filter(is.na(event) == FALSE) %>%
#'   group_by(event) %>%
#'   arrange(dtime_est)%>%
#'   summarize( #Calculate performance metrics
#'     #Observed infiltration rate
#'     Infiltration_Rate_inhr = saturatedPerformance(event, dtime_est,
#'                                                rainfall_in,
#'                                                dcia_ft2,
#'                                                storage_depth_ft = storage_depth_ft,
#'                                                storage_vol_ft3 = storage_vol_ft3,
#'                                                orifice_diam_in,
#'                                                orifice_height_ft,
#'                                                waterlevel_ft = level_ft, 
#'                                                type = "infiltration"))
#' 


marsSaturatedPerformance_inhr <- function(event, #for warning messages
                                          dtime_est,
                                          rainfall_in, #for removing overlapping events
                                          dcia_ft2, #directly connected impervious area
                                          orifice_height_ft = NA, #default to NA if no orifice outlet
                                          orifice_diam_in = NA, #default to NA if no orifice outlet
                                          storage_depth_ft,
                                          storage_vol_ft3,
                                          waterlevel_ft, #observed data
                                          discharge_coeff = 0.62, #Orifice discharge coefficient
                                          type #"infiltration" or "recession"
){
  
  #1. Prepare data
  #1.1 Initialize data frame
  df <- tibble::tibble(dtime_est = lubridate::force_tz(dtime_est, tz = "EST"),
                       rainfall_in, 
                       depth_ft = waterlevel_ft, #observed data 
                       vol_ft3 = 0,
                       #runoff_ft3 = 0,
                       slow_release_ft3 = 0)
  
  #1.2 Calculate volume
  df$vol_ft3 <- pwdgsi:::depth.to.vol(maxdepth_ft = storage_depth_ft[1],
                                      maxvol_cf = storage_vol_ft3[1],
                                      depth_ft = df$depth_ft)

  #1.3 Calculate orifice flow
  # If orifice dimensions are not provided, slow_release_ft = 0 (1.1)
  if(!is.na(orifice_diam_in[1])){ 
    df$slow_release_ft3 <- marsUnderdrainOutflow_cf(dtime_est,
                                            waterlevel_ft,
                                            orifice_height_ft,
                                            orifice_diam_in)
    
  }
  
  #2. Identify times associated with 5" and 7" depths
  # Note: For this approach, the time at which the depth drops below the 5" or 7" threshold is identified
  # by working backwards through the dataset. The data is first filtered to remove subsequent peaks and
  # is then again filtered by the threshold. The last row (slice(n())) represents the timestep immediately
  # before the level drops below the threshold. This value may not represent the value closest to the 
  # threshold, however, this approach ensures that the values are taken from receding limbs.
  
  #2.1 Re-rerun marsDetectEvents to identify overlapping events (if joined with synthetic recession period)
  overlapping <- df %>%
    dplyr::mutate(rainfall_in = tidyr::replace_na(rainfall_in, 0)) %>%
    dplyr::filter(rainfall_in != 0) %>%
    dplyr::arrange(dtime_est) %>% #confirm that dtime is in ascending order
    dplyr::mutate(rain_event = marsDetectEvents(dtime_est, rainfall_in)) %>%
    dplyr::filter(rain_event > 1) %>%
    dplyr::slice(dplyr::n()) #pull last row (corresponds to end of first rainfall event)
  
  
  #2.2 If overlapping events, remove subsquent and determine 5" and 7" times
  if(nrow(overlapping) != 0){
    
    last_depth5 <- df %>%
      dplyr::filter(dtime_est < overlapping$dtime_est) %>% #remove values associated with subsequent events
      dplyr::filter(depth_ft > 5/12) %>% #value immediately prior to water level crossing 5"
      dplyr::slice(dplyr::n()) #latest timestep
    
    last_depth7 <- df %>%
      dplyr::filter(dtime_est < overlapping$dtime_est) %>% #remove values associated with subsequent events
      dplyr::filter(depth_ft > 7/12) %>% #value immediately prior to water level crossing 5"
      dplyr::slice(dplyr::n()) #latest timestep
    
  }else{
    #2.3 If no overlapping events, proceed with calculation
    
    last_depth5 <- df %>%
      dplyr::filter(depth_ft > 5/12) %>% #value immediately prior to water level crossing 5"
      dplyr::slice(dplyr::n()) #latest timestep
    
    last_depth7 <- df %>%
      dplyr::filter(depth_ft > 7/12) %>% #value immediately prior to water level crossing 7"
      dplyr::slice(dplyr::n()) #latest timestep
  }
  
  #2.4 Check that data is appropriate for calculating infiltration rate
  
  #2.4.1 Do observation values exist in the dataset approximately equal to 5 and 7"?
  if(nrow(last_depth5)== 0 | nrow(last_depth7) == 0){
    message(paste("Event",event[1], "does not include observation data that approximately equals 5 or 7 in. of water depth."))
    infiltration_rate_inhr <- -900
    recession_rate_inhr <- -900
  }else{
    
    #2.4.2 Does the 5" measurement occur after the 7"?
    if(last_depth5$dtime_est < last_depth7$dtime_est){
      message(paste0("Code captures rising limb in Event ", event[1], "."))
      infiltration_rate_inhr <- -910
      recession_rate_inhr <- -910
    }else{
      
      #2.4.3 Does rainfall occur during the recession period between 7" and 5"?
      tempseries <- df %>%
        dplyr::filter(dtime_est >= last_depth7$dtime_est & dtime_est <= last_depth5$dtime_est) 
      
      if(sum(tempseries$rainfall_in, na.rm = TRUE) != 0){
        message(paste0("Rainfall occurs during recession period between 7 in. and 5 in. in Event ", event[1], ".")) 
        infiltration_rate_inhr <- -920
        recession_rate_inhr <- -920
      }else{
        
        #3. Calculate infiltration and recession rate
        
        #3.1 Recession rate
        recession_rate_inhr <- round(((last_depth7$depth_ft - last_depth5$depth_ft)*12)/ #inches
                                       as.numeric(difftime(last_depth5$dtime_est, last_depth7$dtime_est, units = "hours")),3)
        
        #3.2 Calculate total orifice flow  
        total_orifice_ft3 <- sum(tempseries$slow_release_ft3, na.rm = TRUE)    
        
        
        #3.3 Calculate total change storage
        change_storage_ft3 <- tempseries$vol_ft3[1] - tempseries$vol_ft3[nrow(tempseries)] - total_orifice_ft3
        
        change_depth_in <- vol.to.depth(maxdepth_ft = storage_depth_ft,
                                     maxvol_cf = storage_vol_ft3,
                                    vol_cf = change_storage_ft3)*12
        
        #3.4 Calculate infiltration
        infiltration_rate_inhr <- round(change_depth_in/ #inches
                                          as.numeric(difftime(last_depth5$dtime_est, last_depth7$dtime_est, units = "hours")),3)
      }}}
  
  
  
  #Function returns infiltration rate in in/hr
  if(type == "infiltration"){
    return(infiltration_rate_inhr)
  }
  
  if(type == "recession"){
    return(recession_rate_inhr)
  }
}


# Observed Orifice Outflow Volume -------------------------------------------
#Description of the arguments:

#IN:  dtime_est            A vector of POSIXct date times, in ascending order
#IN:  waterlevel_ft        Observed water level data, in feet
#IN:  orifice_height_ft    Orifice height, in feet
#IN:  orifice_diam_in      Orifice diameter, in inches
#IN:  discharge_coeff      Orifice discharge coefficient

#OUT: Total observed orifice outflow volume, in cubic feet

#' Observed Orifice Outflow Volume
#' 
#' Total observed orifice outflow volume (cf)
#' 
#' @param  dtime_est            A vector of POSIXct date times, in ascending order
#' @param  waterlevel_ft        Observed water level data (ft)
#' @param  orifice_height_ft    Orifice height (ft)
#' @param  orifice_diam_in      Orifice diameter (in)
#' @param  discharge_coeff      Orifice discharge coefficient
#' 
#' @return Output is total observed orifice outflow volume (cf)
#' 
#' @seealso \code{\link{marsSaturatedPerformance_inhr}}
#' 
#' @export
#' 
#' @examples
#' obs_250_fill <- obs_250_all %>%  
#' arrange(dtime_est)%>%
#'   fill(event) %>% #Fill NA's
#'   mutate( # Pull in system specs from smp_stats table    
#'     storage_depth_ft = smp_stats$storage_depth_ft[7], 
#'     storage_vol_ft3 = smp_stats$storage_vol_ft3[7],
#'     infil_footprint_ft2 = smp_stats$infil_footprint_ft2[7],
#'     dcia_ft2 =  smp_stats$dcia_ft2[7],
#'     orifice_height_ft = smp_stats$orifice_height_ft[7],
#'     orifice_diam_in = smp_stats$orifice_diam_in[7],
#'     
#'     # Calculate orifice flow, if applicable
#'    orifice_vol_cf = marsUnderdrainOutflow_cf(dtime_est,
#'                                         waterlevel_ft = level_ft,
#'                                         orifice_height_ft,
#'                                         orifice_diam_in)
#'   )
#' 
#' 

marsUnderdrainOutflow_cf <- function(dtime_est, 
                                waterlevel_ft, 
                                orifice_height_ft,
                                orifice_diam_in,
                                #DEFAULT VALUES
                                discharge_coeff = 0.62){ #Orifice discharge coefficient
  
  #1. Prepare data
  #1.1 Initialize data frame
  df <- tibble::tibble(dtime_est = lubridate::force_tz(dtime_est, tz = "EST"),
                       depth_ft = waterlevel_ft)#, #observed data
  #elapsed_time_hr = 0, 
  #WL_above_orifice_ft = 0,
  #slow_release_vol_ft3 = 0) 
  
  
  
  #2. Calculate Orifice Outflow
  # Orifice equation:
  # Q_orifice = C * Area * sqrt(2 * gravity * depth) * time
  
  #2.1 Calculate area of orifice (ft2)
  orifice_area_ft2 <- pi*((orifice_diam_in[1]/12)^2)/4 #area of orifice (ft2)
  
  
  df <- df %>%
    dplyr:: mutate(#2.2 calculate elapsed time (hrs) 
      elapsed_time_hr = difftime(dtime_est, dplyr::lag(dtime_est), unit = "hours"), #difftime(lead(dtime_est), dtime_est, unit = "hours"),
      
      #2.3 Calculate height of water above orifice (ft)
      WL_above_orifice_ft = depth_ft - orifice_height_ft[1],
      
      #2.4 Set height of water to 0 if below elevation of orifice
      WL_correction = ifelse(WL_above_orifice_ft < 0,0, WL_above_orifice_ft),
      
      #2.4 Calculate total discharge through orifice
      slow_release_ft3 = discharge_coeff*
        orifice_area_ft2*
        sqrt(2 * 32.2 * WL_correction) * 
        60*60 * #convert cfs to cfhr     
        as.numeric(elapsed_time_hr))
  
  
  return(df$slow_release_ft3)
}

# Depth - Volume Conversions ----------------------
# Functions taken directly from Rmarkdown file "GSI_Data_Analysis_EWRILIDFork_6AprVersion_180612_RDM.Rmd"
# NOTE: The following comment was in script: "QA RECOMMENDED TO CONFIRM WE ARE HANDLING STAGE-STORAGE RELATIONSHIP AS INTENDED"
# Laurie and Katie discussed this with Andrew Baldridge and Dwayne Myers and confirmed that the functions convert correctly - 3/22/2019

#' Return Volume or Depth
#' Input maximum depth and maximum volume of SMP, along with depth or volume, to return the volume or depth. 
#'
#' @name depth.volume
NULL

#' @rdname depth.volume
#' 
#' @param maxdepth_ft depth of SMP when full, (ft)
#' @param maxvol_cf volume of SMP when full (cf)
#' @param vol_cf input or output volume (cf)
#' @param depth_ft input or output depth (cf)
#' 
#' @return Output is either volume (cf) or depth (ft)
#' 
#' @export

vol.to.depth <- function(maxdepth_ft, maxvol_cf, vol_cf){
  return(maxdepth_ft[1] * vol_cf/maxvol_cf[1])
}

#' @rdname depth.volume
#' 

depth.to.vol <- function(maxdepth_ft, maxvol_cf, depth_ft){
  return(maxvol_cf[1] * depth_ft/maxdepth_ft[1])
}

# marsSimulatedLevelSeries_ft -------------------------------------------------
# NOTES: Based on a time series simulation function written by Taylor Heffernan (4/5/2018) and Dwayne Myers (6/13/2018), modified by Katie Swanson (March 2019), then modified by Nick Manna (July 2019)
#        Function generates simulated water level in subsurface stormwater infiltration tank with underdrain (orifice outlet)
#        Updated to include option for orifice flow.

#Description of the arguments:

#IN:  dtime_est               A vector of POSIXct date times, in ascending order
#IN:  rainfall_in             Rainfall depths during periods corresponding to times in  dtime_est, in inches
#IN:  event                   A vector of Event IDs
#IN:  infil_footprint_ft2     Total area of the system that is open to infiltration, in square feet
#IN:  dcia_ft2                Directly connected impervious area, in square feet
#IN:  orifice_height_ft       Orifice height, in feet (NA if no orifice outlet)
#IN:  orifice_diam_in         Orifice diameter, in inches (NA if no orifice outlet)
#IN:  storage_depth_ft        Maximum storage depth, in feet
#IN:  storage_vol_ft3         Maximum storage volume (pore space), in cubic feet
#IN:  infil_rate_inhr         System design infiltration rate, in inches per hour
#IN:  initial_water_level_ft  Initial water Level, in feet (Default = 0)
#IN:  runoff_coeff            Rational method coefficient (Default = 1)
#IN:  discharge_coeff         Orifice discharge coefficient (Defauly = 0.62)


#OUT: Dataframe of the following columns: dtime_est, rainfall_in, event, Simulated_depth_ft, Simulated_vol_ft3, Simulated_orifice_vol_ft3

#' Simulated Water Level
#' 
#' Simulates water level in subsurface stormwater infiltration system with underdrain
#' 
#' @param  dtime_est               A vector of POSIXct date times, in ascending order
#' @param  rainfall_in             Rainfall depths during periods corresponding to times in  dtime_est (in)
#' @param  event                   A vector of Event IDs
#' @param  infil_footprint_ft2     Total area of the system that is open to infiltration (sf)
#' @param  dcia_ft2                Directly connected impervious area (sf)
#' @param  orifice_height_ft       Orifice height (ft) (NA if no orifice outlet)
#' @param  orifice_diam_in         Orifice diameter (in) (NA if no orifice outlet)
#' @param  storage_depth_ft        Maximum storage depth (ft)
#' @param  storage_vol_ft3         Maximum storage volume (pore space) (cf)
#' @param  infil_rate_inhr         System design infiltration rate (in/hr)
#' @param  initial_water_level_ft  Initial water Level (ft) (Default = 0)
#' @param  runoff_coeff            Rational method coefficient (Default = 1)
#' @param  discharge_coeff         Orifice discharge coefficient (Defauly = 0.62)
#' 
#' @return Output is a dataframe with the following columns: dtime_est, rainfall_in, rainfall_gage_event_uid, Simulated_depth_ft, Simulated_vol_ft3, Simulated_orifice_vol_ft3
#' 
#' @seealso \code{\link{simulation.stats}}
#' 
#' @examples 
#' simulated_data <- marsSimulatedLevelSeries_ft(dtime_est = rain_data_filtered$dtime_est, 
#'   rainfall_in = rain_data_filtered$rainfall_in, 
#'   event = rain_data_filtered$event,
#'   infil_footprint_ft2 = smp_stats$infil_footprint_ft2[7], 
#'   dcia_ft2 = smp_stats$dcia_ft2[7],
#'   orifice_height_ft = smp_stats$orifice_height_ft[7],
#'   orifice_diam_in = smp_stats$orifice_diam_in[7],
#'   storage_depth_ft = smp_stats$storage_depth_ft[7],
#'   storage_vol_ft3 = smp_stats$storage_vol_ft3[7],
#'   infil_rate_inhr = smp_stats$infil_rate_inhr[7])
#' 
#' 
#' @export

marsSimulatedLevelSeries_ft <- function(dtime_est,
                                        rainfall_in,
                                        event,
                                        infil_footprint_ft2, #footprint of the SMP that is open to infiltration
                                        dcia_ft2, #directly connected impervious area
                                        orifice_height_ft = NA, #default to NA if no orifice outlet
                                        orifice_diam_in = NA, #default to NA if no orifice outlet
                                        storage_depth_ft,
                                        storage_vol_ft3,
                                        infil_rate_inhr,
                                        #default values
                                        initial_water_level_ft = 0, 
                                        runoff_coeff = 1, #rational method coefficient
                                        discharge_coeff = 0.62 #Orifice discharge coefficient
){ 
  
  #Prepare data
  #Initialize data frames
  collected_data <- data.frame(dtime_est, rainfall_in, event)
  simseries_total <- tibble::tibble(dtime_est = lubridate::force_tz(dtime_est, tz = "EST"),
                                    rainfall_in = 0, 
                                    event = 0,
                                    depth_ft = 0, 
                                    vol_ft3 = 0,
                                    runoff_ft3 = 0,
                                    slow_release_ft3 = 0,
                                    infiltration_ft3 = 0, #POTENTIAL infiltration
                                    end_vol_ft3 = 0) 
  simseries_total <- simseries_total[0,]
  unique_events <- unique(collected_data$event) #vector of unique event IDs
  unique_events <- unique_events[!is.na(unique_events)]
  
  #Filter to create a separate dataframe, and run analysis, for each unique event
  for(j in 1:length(unique_events)){
    by_event <- collected_data %>% 
      dplyr::filter(event == unique_events[j])
    
    dtime_est <- by_event$dtime_est
    rainfall_in <- by_event$rainfall_in
    
    timeseries <- seq.POSIXt(as.POSIXct(min(dtime_est)), #first measurement
                             as.POSIXct(max(dtime_est)), #last measurement
                             by = 0.25*60*60) #15-min interval - function will not accept minutes(15)
    
    #Generate data with only timesteps that don't have rainfall measurements
    new_rows <- as.data.frame(timeseries[!timeseries %in% dtime_est]) %>% dplyr::mutate(rainfall_in = 0, event = unique_events[j])
    colnames(new_rows) <- c("dtime_est", "rainfall_in", "event")
    
    #bind with timesteps that have rainfall measurements
    output <- dplyr::bind_rows(by_event, new_rows) %>% dplyr::arrange(dtime_est)
    
    
    #Create dataframe to be filled                         
    simseries <- tibble::tibble(dtime_est = lubridate::force_tz(output$dtime_est, tz = "EST"),
                                rainfall_in = output$rainfall_in, 
                                event = output$event, 
                                depth_ft = 0, 
                                vol_ft3 = 0,
                                runoff_ft3 = 0,
                                slow_release_ft3 = 0,
                                infiltration_ft3 = 0, #POTENTIAL infiltration
                                end_vol_ft3 = 0) 
    
    
    
    #Calculate runoff (independent of timestep)
    simseries$runoff_ft3 <- runoff_coeff * output$rainfall_in/12 * dcia_ft2[1] #convert in to ft
    
    #Mass balance
    #Calculate starting values
    #Starting Storage
    simseries$depth_ft[1] <- initial_water_level_ft[1]
    simseries$vol_ft3[1] <- depth.to.vol(maxdepth_ft = storage_depth_ft[1],
                                         maxvol_cf = storage_vol_ft3[1],
                                         depth_ft = simseries$depth_ft[1])
    #Orifice Outflow
    if(is.na(orifice_diam_in[1]) == FALSE){
      orifice_area_ft2 <- pi*((orifice_diam_in[1]/12)^2)/4
      WL_above_orifice_ft <- simseries$depth_ft[1] - orifice_height_ft[1] 
      
      # Q_orifice = C * Area * sqrt(2 * gravity * depth) * time
      
      simseries$slow_release_ft3[1] <- discharge_coeff * 
        orifice_area_ft2 *
        sqrt(2 * 32.2 * max(WL_above_orifice_ft, 0)) * #set to 0 if below orifice
        60 * #convert cfs to cfm     
        lubridate::minutes(15) #assuming 15 minutes for first timestep
    }
    
    # Potential Infiltration
    # Total volume for period of time - assume first timestep is 15 minutes
    # Q_infil = Area * infiltration rate * time
    simseries$infiltration_ft3[1] <- infil_footprint_ft2[1] * infil_rate_inhr[1]/12 * 15/60 
    
    # Change in storage
    simseries$end_vol_ft3[1] <- max(0, simseries$vol_ft3[1] + simseries$runoff_ft3[1] -
                                      simseries$slow_release_ft3[1] - simseries$infiltration_ft3[1]) 
    
    max_time <- nrow(simseries)+4*24*60/15 #length of existing simulated data series, plus 4 days * 24 hours * 4 timesteps/hour
    last_rainfall <- nrow(simseries)
    # Mass balance for all other timesteps  
    for(i in (2:(max_time))){
      
      #If timestep exceeds the dataframe length, add a new row at the next timestep
      if(i > nrow(simseries)){
        simseries <- rbind(simseries, data.frame("dtime_est" = lubridate::force_tz(simseries$dtime_est[i-1]+lubridate::minutes(15), tz = "EST"),
                                                 "rainfall_in" = 0,
                                                 "event" = output$event[1],
                                                 "depth_ft" = 0,
                                                 "vol_ft3" = 0 ,
                                                 "runoff_ft3" = 0,
                                                 "slow_release_ft3" = 0,
                                                 "infiltration_ft3" = 0,
                                                 "end_vol_ft3"= 0))
      }
      
      # Calculate time since last measurement
      elapsed_time_hr = (simseries$dtime_est[i-1] %--% simseries$dtime_est[i])/lubridate::hours(1)
      
      # Starting Storage
      simseries$vol_ft3[i] <- simseries$end_vol_ft3[i-1]
      simseries$depth_ft[i] <- vol.to.depth(maxdepth_ft = storage_depth_ft[1],
                                            maxvol_cf = storage_vol_ft3[1],
                                            vol_cf = simseries$vol_ft3[i])
      
      # Orifice Outflow
      if(is.na(orifice_diam_in[1])== FALSE){
        orifice_area_ft2 <- pi*((orifice_diam_in[1]/12)^2)/4
        WL_above_orifice_ft <- simseries$depth_ft[i] - orifice_height_ft[1] 
        
        # Q_orifice = C * Area * sqrt(2 * g * depth) * time
        
        simseries$slow_release_ft3[i] <- discharge_coeff * 
          orifice_area_ft2 *
          sqrt(2 * 32.2 * max(WL_above_orifice_ft, 0)) * #set to 0 if below orifice
          60 * 60 *  #convert cf/s to cf/hr
          elapsed_time_hr #assuming 15 minutes for first timestep
      }
      # Potential Infiltration
      # Total volume for period of time - assume first timestep is 15 minutes
      # Q_infil = Area * infiltration rate * time
      simseries$infiltration_ft3[i] <- infil_footprint_ft2[1] * infil_rate_inhr[1]/12 * elapsed_time_hr 
      
      # Ending volume calculation  
      
      #Calculate volume at end of timestep
      end_vol_cf <- simseries$vol_ft3[i] + simseries$runoff_ft3[i] -
        simseries$slow_release_ft3[i] - simseries$infiltration_ft3[i]
      
      #If volume is less than 0 (eg. infiltration potential exceeds storage), set to 0
      min_volume_check_cf <- max(0, end_vol_cf)
      
      #If volume is more than maximum storage capacity, set to maximum storage capacity
      volume_check_cf <- min(min_volume_check_cf, storage_vol_ft3[1])
      
      #Save out final volume with correction for min and max storage applied
      simseries$end_vol_ft3[i] <- volume_check_cf
      
      if(simseries$vol_ft3[i] == 0 && i > last_rainfall){
        break
      }
      
    }
    #bind new data 
    simseries_total <- dplyr::bind_rows(simseries_total, simseries) 
    
  }
  #Series returns a data frame including water depth #may be updated
  
  simseries_total <- simseries_total %>% dplyr::select("dtime_est", "rainfall_in", "event", "depth_ft", "vol_ft3", "slow_release_ft3")
  
  colnames(simseries_total) <- c("dtime_est", 
                                 "rainfall_in", 
                                 "rainfall_gage_event_uid", 
                                 "Simulated_depth_ft", 
                                 "Simulated_vol_ft3", 
                                 "Simulated_orifice_vol_ft3")
  
  return(simseries_total)
  
}






# Overtopping Check ----------------------------------

#Description of the arguments:

#IN:  waterlevel_ft      Water level, in feet
#IN:  storage_depth_ft   Maximum storage depth, in feet

#OUT: T/F designation based on overtopping evaluation

#' Simulation Stats
#' 
#' Summarize and Evaluate System Performance
#' 
#' @name simulation.stats
NULL

#' @rdname simulation.stats
#' 
#' @param waterlevel_ft Water Level (ft)
#' @param storage_depth_ft Total storage Depth (ft)
#' 
#' @return \describe{
#'      \item{\code{marsOvertoppingCheck_bool}}{Output is true or false based on overtopping evaluation}
#' }
#' 
#' @examples
#' 
#' simulation_summary <- simulated_data %>%
#'   dplyr::group_by(event) %>%
#'   dplyr::summarize(startdate = min(dtime_est), #add start date of event to summary table,
#'                   
#'    #1. Overtopping check
#'    overtopping = marsOvertoppingCheck_bool(Simulated_depth_ft,smp_stats$storage_depth_ft[7]),
#' 
#'    #2. Simulated storage utilization
#'    peakUtilization = marsPeakStorage_percent(Simulated_depth_ft,smp_stats$storage_depth_ft[7]),
#' 
#'    #3. Peak release rate
#'    peakReleaseRate_cfs = marsPeakReleaseRate_cfs(dtime_est, Simulated_orifice_vol_ft3),
#' 
#'    #4. Total orifice outflow volume (rounded for table format)
#'    orifice_volume_ft3 = round(sum(Simulated_orifice_vol_ft3),0),
#' 
#'    #5. Draindown time
#'    draindown_time_hr = marsDraindown_hr(dtime_est, rainfall_in, Simulated_depth_ft))
#'    
#' @export



marsOvertoppingCheck_bool <- function(waterlevel_ft, storage_depth_ft){
  
  #1. Pull max water level
  max_water_level <- max(waterlevel_ft, na.rm = TRUE)
  
  #2. Compare to max structure storage
  check <- ifelse(max_water_level < storage_depth_ft, "FALSE", "TRUE")
  
  return(check)
}

# Peak Storage ------------------------------------
#Description of the arguments:

#IN:  waterlevel_ft      Water level, in feet
#IN:  storage_depth_ft   Maximum storage depth, in feet

#OUT: Peak Storage Utilization Percentage

# Peak Storage Utilization Percentage
# 
# Percentage of peak storage filled

#' @rdname simulation.stats
#' 
#' @return \describe{
#'      \item{\code{marsPeakStorage_percent}}{Output is a percentage of peak storage filled, by depth}
#' }
#' 
#' @export

marsPeakStorage_percent <- function(waterlevel_ft, storage_depth_ft){
  
  #1. Pull starting water level
  starting_level <- ifelse(waterlevel_ft[1] < 0, 0, waterlevel_ft[1])
  
  #2. Pull max water level
  max_water_level <- max(waterlevel_ft, na.rm = TRUE)
  
  #3. Apply correction for starting water levels
  event_max_water_level <- max_water_level - starting_level
  
  max_storage <- storage_depth_ft - starting_level
  
  #3. Calculate Peak Storage Utilization
  peak_util <- (event_max_water_level/max_storage)*100
  
  #4. Apply correction for overtopping
  peak_util <- ifelse(peak_util > 100, 100, peak_util)
  
  #5. Set to zero if negative
  peak_util <- ifelse(peak_util < 0, 0, peak_util)
  
  return(peak_util)
}


# Peak Release Rate -----------------------------------------------
#Description of the arguments:

#IN:  dtime_est             A vector of POSIXct date times, in ascending order
#IN:  orifice_outflow_ft3   Orifice outflow volume, in cubic feet

#OUT: Peak orifice release rate, in cfs

#' @rdname simulation.stats
#' 
#' @param dtime_est A vector of POSIXct date times, in ascending order
#' @param orifice_outflow_ft3 Orifice outflow volume (cf)
#' 
#' @return \describe{
#'      \item{\code{marsPeakReleaseRate_cfs}}{Output is peak orifice release rate (cfs)}
#' }
#' 
#' @export


marsPeakReleaseRate_cfs <- function(dtime_est,
                                    orifice_outflow_ft3){
  
  #1. Prepare data
  #1.1 Initialize data frame
  df <- tibble::tibble(dtime_est = lubridate::force_tz(dtime_est, tz = "EST"),
                       orifice_ft3 = orifice_outflow_ft3) 
  
  #2. Calculate timestep and pull maximum value
  df_max <- df %>%
    dplyr::mutate(elapsed_time_hr = difftime(dplyr::lead(dtime_est), dtime_est, unit = "hours")) %>%
    dplyr::filter(is.na(orifice_ft3) == FALSE) %>%
    dplyr::arrange(orifice_ft3) %>%
    dplyr::slice(dplyr::n()) #pull row containing max orifice volume
  
  #3. Calculate peak rate
  #3.1 Check that outflow data is not NA
  if(nrow(df_max) == 0){
    rate <- NA
  }else{
    
    #3.2 Calculate rate
    rate <- df_max$orifice_ft3/
      as.numeric(df_max$elapsed_time_hr*60*60) #hr converted to seconds 
  }
  
  #4. Round to 3 digits
  rate <- round(rate, 3)
  
  return(rate)
}


# Draindown ---------------------------------------------------------------
#Description of the arguments:

#IN:  dtime_est        A vector of POSIXct date times, in ascending order
#IN:  rainfall_in      Rainfall depths during periods corresponding to times in  dtime_est, in inches
#IN:  waterlevelt_ft   Water level, in feet

# OUT:  Calculated Draindown time, in hours

#' @rdname simulation.stats
#' 
#' @param dtime_est A vector of POSIXct date times, in ascending order
#' @param rainfall_in Rainfall depths during periods corresponding to times in  dtime_est (in)
#' 
#' @return \describe{
#'      \item{\code{marsDraindown_hr}}{Output is Calculated Draindown time (hr)}
#' }
#' 
#' @export

marsDraindown_hr <- function(dtime_est, rainfall_in, waterlevel_ft){
  
  #1. Process data
  #1.1 Initialize dataframe
  dtime_est <- lubridate::force_tz(dtime_est, tz = "EST")
  
  combined_data <- tibble::tibble(
    dtime_est = dtime_est,
    rainfall_in = rainfall_in,
    waterlevel_ft  = waterlevel_ft)
  
  #1.2 Re-run marsDetectEvents to pull rain event endtime
  rain_end <- combined_data %>%
    dplyr::mutate(rainfall_in = tidyr::replace_na(rainfall_in, 0)) %>%
    dplyr::filter(rainfall_in != 0) %>%
    dplyr::arrange(dtime_est) %>% #confirm that dtime is in ascending order
    dplyr::mutate(rain_event = marsDetectEvents(dtime_est, rainfall_in)) %>%
    dplyr::filter(rain_event == 1) %>%
    dplyr::arrange(dtime_est) %>% #confirm that dtime is in ascending order
    dplyr::slice(dplyr::n()) #pull last row (corresponds to end of rainfall event)
  
  #2. Confirm that there was a response in the structure during the event (water level > 0)
  check <- any(waterlevel_ft > 0)  
  
  if(check == FALSE){
    draindown_hrs <- NA
    
  }else{
    
    #3. Filter by storage depth to pull time to empty
    stor_end <- combined_data %>%
      dplyr::filter(dtime_est > rain_end$dtime_est) %>%
      dplyr::filter(waterlevel_ft < 0.001) %>%
      dplyr::arrange(dtime_est) %>%
      dplyr::slice(1L)
    
    
    #4. Check that water level drops below zero after event
    if(nrow(stor_end) > 0){
      #4.1 Calculate draindown time
      draindown_hrs <- difftime(stor_end$dtime_est, rain_end$dtime_est, unit = "hours")
      
      #4.2 Round to whole number
      draindown_hrs <- round(draindown_hrs,0)
      
    }else{
      draindown_hrs <- NA
      
    } # End #4 check
    
  }# End #2 check
  
  
  return(as.double(draindown_hrs))
}




