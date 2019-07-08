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
  check <- ifelse(max_water_level < storage_depth_ft[1], "FALSE", "TRUE")
  
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
  
  max_storage <- storage_depth_ft[1] - starting_level
  
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




