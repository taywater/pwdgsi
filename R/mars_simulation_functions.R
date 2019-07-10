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
#' @export

depth.to.vol <- function(maxdepth_ft, maxvol_cf, depth_ft){
  return(maxvol_cf[1] * depth_ft/maxdepth_ft[1])
}

# simWaterLevel_ft -------------------------------------------------
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
#' @return Output is a dataframe with the following columns: dtime_est, rainfall_in, event, Simulated_depth_ft, Simulated_vol_ft3, Simulated_orifice_vol_ft3
#' 
#' @export

simWaterLevel_ft <- function(dtime_est,
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
  
  #Filter to create a separate dataframe, and run analysis, for each unique event
  for(j in 1:length(unique_events)){
    by_event <- collected_data %>% 
      filter(event == unique_events[j])
    
    dtime_est <- by_event$dtime_est
    rainfall_in <- by_event$rainfall_in
    
    timeseries <- seq.POSIXt(as.POSIXct(min(dtime_est)), #first measurement
                             as.POSIXct(max(dtime_est)), #last measurement
                             by = 0.25*60*60) #15-min interval - function will not accept minutes(15)
    
    #Generate data with only timesteps that don't have rainfall measurements
    new_rows <- as.data.frame(timeseries[!timeseries %in% dtime_est]) %>% mutate(rainfall_in = 0, event = unique_events[j])
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
        minutes(15) #assuming 15 minutes for first timestep
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
        simseries <- rbind(simseries, data.frame("dtime_est" = lubridate::force_tz(simseries$dtime_est[i-1]+minutes(15), tz = "EST"),
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
      elapsed_time_hr = (simseries$dtime_est[i-1] %--% simseries$dtime_est[i])/hours(1)
      
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
    simseries_total <- bind_rows(simseries_total, simseries) 
    
  }
  #Series returns a data frame including water depth #may be updated
  
  simseries_total <- simseries_total %>% select("dtime_est", "rainfall_in", "event", "depth_ft", "vol_ft3", "slow_release_ft3")
  
  colnames(simseries_total) <- c("dtime_est", 
                                 "rainfall_in", 
                                 "event", 
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
#'      \item{\code{overtoppingCheck_bool}}{Output is true or false based on overtopping evaluation}
#' }
#' 
#' @export



overtoppingCheck_bool <- function(waterlevel_ft, storage_depth_ft){
  
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
#'      \item{\code{peakStorUtil_percent}}{Output is a percentage of peak storage filled, by depth}
#' }
#' 
#' @export

peakStorUtil_percent <- function(waterlevel_ft, storage_depth_ft){
  
  #1. Pull starting water level
  starting_level <- ifelse(waterlevel_ft[1] < 0, 0, waterlevel_ft[1])
  
  #2. Pull max water level
  max_water_level <- max(waterlevel_ft, na.rm = TRUE)
  
  #3. Apply correction for starting water levels
  event_max_water_level <- max_water_level - starting_level
  
  max_storage <- storage_depth_ft - starting_level
  
  #3. Calculate Peak Storage Ultilization
  peak_util <- round((event_max_water_level/max_storage)*100, 1)
  
  #4. Apply correction for overtopping
  peak_util <- ifelse(peak_util > 100, 100, peak_util)
  
  #5. Set to zero if negative
  peak_util <- ifelse(peak_util < 0, 0, peak_util)
  
  #6. Round to whole number
  peak_util <- round(peak_util, 0)
  
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
#'      \item{\code{orificePeakRelease_cfs}}{Output is peak orifice release rate (cfs)}
#' }
#' 
#' @export


orificePeakRelease_cfs <- function(dtime_est,
                                   orifice_outflow_ft3){
  
  #1. Prepare data
  #1.1 Initialize data frame
  df <- tibble::tibble(dtime_est = lubridate::force_tz(dtime_est, tz = "EST"),
                       orifice_ft3 = orifice_outflow_ft3) 
  
  #2. Calculate timestep and pull maximum value
  df_max <- df %>%
    dplyr::mutate(elapsed_time_hr = difftime(lead(dtime_est), dtime_est, unit = "hours")) %>%
    dplyr::filter(is.na(orifice_ft3) == FALSE) %>%
    dplyr::arrange(orifice_ft3) %>%
    dplyr::slice(n()) #pull row containing max orifice volume
  
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
#'      \item{\code{draindown_hr}}{Output is Calculated Draindown time (hr)}
#' }
#' 
#' @export

draindown_hr <- function(dtime_est, rainfall_in, waterlevel_ft){
  
  #1. Process data
  #1.1 Initialize dataframe
  dtime_est <- lubridate::force_tz(dtime_est, tz = "EST")
  
  combined_data <- tibble::tibble(
    dtime_est = dtime_est,
    rainfall_in = rainfall_in,
    waterlevel_ft  = waterlevel_ft)
  
  #1.2 Re-run detectEvents to pull rain event endtime
  rain_end <- combined_data %>%
    dplyr::mutate(rainfall_in = tidyr::replace_na(rainfall_in, 0)) %>%
    dplyr::filter(rainfall_in != 0) %>%
    dplyr::arrange(dtime_est) %>% #confirm that dtime is in ascending order
    dplyr::mutate(rain_event = detectEvents(dtime_est, rainfall_in)) %>%
    dplyr::filter(rain_event == 1) %>%
    dplyr::arrange(dtime_est) %>% #confirm that dtime is in ascending order
    dplyr::slice(n()) #pull last row (corresponds to end of rainfall event)
  
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




