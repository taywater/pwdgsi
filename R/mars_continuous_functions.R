# Observed Infiltration Rate ------------------------------------------
#Description of the arguments:

#IN:  event                Rainfall event ID (grouping variable)
#IN:  dtime_est            A vector of POSIXct date times, in ascending order
#IN:  rainfall_in          Rainfall depths during periods corresponding to times in  dtime_est, in inches
#IN:  infil_footprint_ft2  Total area of the system that is open to infiltration, in square feet
#IN:  dcia_ft2             Directly connected impervious area, in square feet
#IN:  orifice_height_ft    Orifice height, in feet
#IN:  orifice_diam_in      Orifice diameter, in inches
#IN:  storage_depth_ft     Maximum storage depth of system, in feet
#IN:  storage_vol_ft3      Maximum storage volume (pore space) of system, in cubic feet
#IN:  waterlevel_ft        Observed water level data, in feet
#IN:  discharge_coeff      Orifice discharge coefficient

#OUT: Estimated infiltration rate, in inches per hour

#' Observed Infiltration Rate
#' 
#' Estimated infiltration rate based on observed data
#' 
#' @param  event                Rainfall event ID (grouping variable)
#' @param  dtime_est            A vector of POSIXct date times, in ascending order
#' @param  rainfall_in          Rainfall depths during periods corresponding to times in  dtime_est (in)
#' @param  infil_footprint_ft2  Total area of the system that is open to infiltration (sf)
#' @param  dcia_ft2             Directly connected impervious area (sf)
#' @param  orifice_height_ft    Orifice height (ft)
#' @param  orifice_diam_in      Orifice diameter (in)
#' @param  storage_depth_ft     Maximum storage depth of system (ft)
#' @param  storage_vol_ft3      Maximum storage volume (pore space) of system, in cubic feet
#' @param  waterlevel_ft        Observed water level data (ft)
#' @param  discharge_coeff      Orifice discharge coefficient
#' 
#' @return Output is estimated infiltration rate (in/hr)
#' 
#' @seealso \code{\link{obsRecessionRate_inhr}}, \code{\link{obsOrificeOutVol_cf}}
#' 
#' @export
#' 
#' @example
#' obs_250_fill %>%
#' filter(is.na(event) == FALSE) %>%
#'   group_by(event) %>%
#'   arrange(dtime_est)%>%
#'   summarize( #Calculate performance metrics
#'     #Observed infiltration rate
#'     Infiltration_Rate_inhr = obsInfilRate_inhr(event, dtime_est,
#'                                                rainfall_in,
#'                                                infil_footprint_ft2,
#'                                                dcia_ft2,
#'                                                storage_depth_ft = storage_depth_ft,
#'                                                storage_vol_ft3 = storage_vol_ft3,
#'                                                orifice_diam_in,
#'                                                orifice_height_ft,
#'                                                waterlevel_ft = level_ft))
#' 

obsInfilRate_inhr <- function(event, #for warning messages
                              dtime_est,
                              rainfall_in, #for removing overlapping events
                              infil_footprint_ft2, #footprint of the system that is open to infiltration
                              dcia_ft2, #directly connected impervious area
                              orifice_height_ft = NA, #default to NA if no orifice outlet
                              orifice_diam_in = NA, #default to NA if no orifice outlet
                              storage_depth_ft,
                              storage_vol_ft3,
                              waterlevel_ft, #observed data
                              #default values
                              discharge_coeff = 0.62 #Orifice discharge coefficient
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
  df$vol_ft3 <- depth.to.vol(maxdepth_ft = storage_depth_ft[1],
                             maxvol_cf = storage_vol_ft3[1],
                             depth_ft = df$depth_ft)
  
  #1.3 Calculate orifice flow
  # If orifice dimensions are not provided, slow_release_ft = 0 (1.1)
  if(is.na(orifice_diam_in[1]) == FALSE ){ 
    orifice_area_ft2 <- pi*((orifice_diam_in[1]/12)^2)/4
    
    df <- df %>%
      dplyr::mutate(#1.4.1 calculate elapsed time (hrs) 
        elapsed_time_hr = difftime(dtime_est, dplyr::lag(dtime_est), unit = "hours"),
        
        #1.4.2 Calculate height of water above orifice (ft)
        WL_above_orifice_ft = depth_ft - orifice_height_ft[1],
        
        #1.4.3 Set height of water to 0 if below elevation of orifice
        WL_correction = ifelse(WL_above_orifice_ft < 0,0, WL_above_orifice_ft),
        
        #1.4.4 Calculate total discharge through orifice
        slow_release_ft3 = discharge_coeff*
          orifice_area_ft2*
          sqrt(2 * 32.2 * WL_correction) * 
          60*60 * #convert cfs to cfhr     
          as.numeric(elapsed_time_hr)) %>%
      dplyr::select(-elapsed_time_hr, -WL_above_orifice_ft, - WL_correction)
  }
  
  #2. Identify times associated with 5" and 7" depths
  # Note: For this approach, the time at which the depth drops below the 5" or 7" threshold is identified
  # by working backwards through the dataset. The data is first filtered to remove subsequent peaks and
  # is then again filtered by the threshold. The last row (slice(n())) represents the timestep immediately
  # before the level drops below the threshold. This value may not represent the value closest to the 
  # threshold, however, this approach ensures that the values are taken from receding limbs.
  
  #2.1 Re-rerun detectEvents to identify overlapping events (if joined with synthetic recession period)
  overlapping <- df %>%
    dplyr::mutate(rainfall_in = tidyr::replace_na(rainfall_in, 0)) %>%
    dplyr::filter(rainfall_in != 0) %>%
    dplyr::arrange(dtime_est) %>% #confirm that dtime is in ascending order
    dplyr::mutate(rain_event = detectEvents(dtime_est, rainfall_in)) %>%
    dplyr::filter(rain_event > 1) %>%
    dplyr::slice(dplyr::n()) #pull last row (corresponds to end of first rainfall event)
  
  
  #2.2 If overlapping events, remove subsquent and determine 5" and 7" times
  if(nrow(overlapping) != 0){
    
    last_depth5 <- df %>%
      dplyr::filter(dtime_est < overlapping$dtime_est)%>% #remove values associated with subsequent events
      dplyr::filter(depth_ft > 5/12) %>% #value immediately prior to water level crossing 5"
      dplyr::slice(dplyr::n()) #latest timestep
    
    last_depth7 <- df %>%
      dplyr::filter(dtime_est < overlapping$dtime_est)%>% #remove values associated with subsequent events
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
    infiltration_rate_inhr <- NA
  }else{
    
    #2.4.2 Does the 5" measurement occur after the 7"?
    if(last_depth5$dtime_est < last_depth7$dtime_est){
      message(paste0("Code captures rising limb in Event ", event[1], "."))
      infiltration_rate_inhr <- NA
    }else{
      
      #2.4.3 Does rainfall occur during the recession period between 7" and 5"?
      tempseries <- df %>%
        dplyr::filter(dtime_est >= last_depth7$dtime_est & dtime_est <= last_depth5$dtime_est) 
      
      if(sum(tempseries$rainfall_in, na.rm = TRUE) != 0){
        message(paste0("Rainfall occurs during recession period between 7 in. and 5 in. in Event ", event[1], ".")) 
        infiltration_rate_inhr <- NA
      }else{
        
        
        
        
        #3. Calculate infiltration rate
        
        #3.1 Isolate observed data between 5" and 7"
        #tempseries calculated in Step 2.4.3 
        
        #3.2 Calculate total orifice flow  
        total_orifice_ft3 <- sum(tempseries$slow_release_ft3, na.rm = TRUE)    
        
        
        #3.3 Calculate total change storage
        change_storage <- tempseries$vol_ft3[1] - tempseries$vol_ft3[nrow(tempseries)] - total_orifice_ft3
        
        
        #3.4 Calculate infiltration
        infiltration_rate_inhr <- round(change_storage/infil_footprint_ft2[1]*12/ #inches
                                          as.numeric(difftime(last_depth5$dtime_est, last_depth7$dtime_est, units = "hours")),3)
      }}}
  
  
  
  #Function returns infiltration rate in in/hr
  return(infiltration_rate_inhr)
  
}

# Observed Recession Rate -----------------------------------------
#Description of the arguments:

#IN:  event                Rainfall event ID (grouping variable)
#IN:  dtime_est            A vector of POSIXct date times, in ascending order
#IN:  rainfall_in          Rainfall depths during periods corresponding to times in  dtime_est, in inches
#IN:  waterlevel_ft        Observed water level data, in feet

#OUT: Estimated recession rate, in inches per hour

#' Observed Recession Rate
#' 
#' Estimated recession rate based on observed data
#' 
#' @param  event                Rainfall event ID (grouping variable)
#' @param  dtime_est            A vector of POSIXct date times, in ascending order
#' @param  rainfall_in          Rainfall depths during periods corresponding to times in  dtime_est (in)
#' @param  waterlevel_ft        Observed water level data (ft)
#' 
#' @return Estimated recession rate (in/hr)
#' 
#' @seealso \code{\link{obsInfilRate_inhr}}, \code{\link{obsOrificeOutVol_cf}}
#' 
#' @export
#' 
#' @example
#' obs_250_fill %>%
#' filter(is.na(event) == FALSE) %>%
#'   group_by(event) %>%
#'   arrange(dtime_est)%>%
#'   summarize( #Calculate performance metrics
#'     
#'     Observed recession rate
#'     Recession_Rate_inhr = obsRecessionRate_inhr(event, dtime_est, rainfall_in, waterlevel_ft = level_ft))
#' 
#' 

obsRecessionRate_inhr <- function(event, #for warning message
                                  dtime_est,
                                  rainfall_in, #for identifying recession limb
                                  waterlevel_ft #observed data
){ 
  
  
  #1. Prepare data
  #1.1 Initialize data frame
  df <- tibble::tibble(dtime_est = lubridate::force_tz(dtime_est, tz = "EST"),
                       rainfall_in, 
                       depth_ft = waterlevel_ft) #observed data 
  
  #2. Identify times associated with 5" and 7" depths
  # Note: For this approach, the time at which the depth drops below the 5" or 7" threshold is identified
  # by working backwards through the dataset. The data is first filtered to remove subsequent peaks and
  # is then again filtered by the threshold. The last row (slice(n())) represents the timestep immediately
  # before the level drops below the threshold. This value may not represent the value closest to the 
  # threshold, however, this approach ensures that the values are taken from receding limbs.
  
  
  #2.1 Re-rerun detectEvents to identify overlapping events (if joined with synthetic recession period)
  overlapping <- df %>%
    dplyr::mutate(rainfall_in = tidyr::replace_na(rainfall_in, 0)) %>%
    dplyr::filter(rainfall_in != 0) %>%
    dplyr::arrange(dtime_est) %>% #confirm that dtime is in ascending order
    dplyr::mutate(rain_event = detectEvents(dtime_est, rainfall_in)) %>%
    dplyr::filter(rain_event > 1) %>%
    dplyr::slice(dplyr::n()) #pull last row (corresponds to end of first rainfall event)
  
  
  #2.2 If overlapping events, remove subsquent and determine 5" and 7" times
  if(nrow(overlapping) != 0){
    
    last_depth5 <- df %>%
      dplyr::filter(dtime_est < overlapping$dtime_est)%>% #remove values associated with subsequent events
      dplyr::filter(depth_ft > 5/12) %>% #value immediately prior to water level crossing 5"
      dplyr::slice(dplyr::n()) #latest timestep
    
    last_depth7 <- df %>%
      dplyr::filter(dtime_est < overlapping$dtime_est)%>% #remove values associated with subsequent events
      dplyr::filter(depth_ft > 7/12) %>% #value immediately prior to water level crossing 5"
      dplyr::slice(dplyr::n()) #latest timestep
    
  }else{
    #2.3 If no overlapping events, proceed with calculation
    
    #2.3.1 Pull 5"
    last_depth5 <- df %>%
      dplyr::filter(depth_ft > 5/12) %>% #value immediately prior to water level crossing 5"
      dplyr::slice(dplyr::n()) #latest timestep
    
    #2.3.2 Pull 7"
    last_depth7 <- df %>%
      dplyr::filter(depth_ft > 7/12) %>% #value immediately prior to water level crossing 5"
      dplyr::slice(dplyr::n()) #latest timestep
    
  }
  
  #2.4 Check that data is appropriate for calculating infiltration rate
  
  #2.4.1 Do observation values exist in the dataset approximately equal to 5 and 7"?
  if(nrow(last_depth5)== 0 | nrow(last_depth7) == 0){
    message(paste("Event",event[1], "does not include observation data that approximately equals 5 or 7 in. of water depth."))
    recession_rate_inhr <- NA
  }else{
    
    #2.4.2 Does the 5" measurement occur after the 7"?
    if(last_depth5$dtime_est < last_depth7$dtime_est){
      message(paste0("Code captures rising limb in Event ", event[1], "."))
      recession_rate_inhr <- NA
    }else{
      
      #2.4.3 Does rainfall occur during the recession period between 7" and 5"?
      tempseries <- df %>%
        dplyr::filter(dtime_est >= last_depth7$dtime_est & dtime_est <= last_depth5$dtime_est) 
      
      if(sum(tempseries$rainfall_in, na.rm = TRUE) != 0){
        message(paste0("Rainfall occurs during recession period between 7 in. and 5 in. in Event ", event[1], ".")) 
        recession_rate_inhr <- NA
      }else{
        
        #3. Calculate recession Rate
        
        recession_rate_inhr <- round(((last_depth7$depth_ft - last_depth5$depth_ft)*12)/ #inches
                                       as.numeric(difftime(last_depth5$dtime_est, last_depth7$dtime_est, units = "hours")),3)
      }}}
  
  #Function returns recession rate in in/hr
  return(recession_rate_inhr)
  
}



# Observed Orifice Outflow Volume -------------------------------------------
#Description of the arguments:

#IN:  dtime_est            A vector of POSIXct date times, in ascending order
#IN:  waterlevel_ft        Observed water level data, in feet
#IN:  rainfall_in          Rainfall depths during periods corresponding to times in  dtime_est, in inches
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
#' @param  rainfall_in          Rainfall depths during periods corresponding to times in  dtime_est (in)
#' @param  orifice_height_ft    Orifice height (ft)
#' @param  orifice_diam_in      Orifice diameter (in)
#' @param  discharge_coeff      Orifice discharge coefficient
#' 
#' @return Output is total observed orifice outflow volume (cf)
#' 
#' @seealso \code{\link{obsInfilRate_inhr}}, \code{\link{obsRecessionRate_inhr}}
#' 
#' @export
#' 
#' @example
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
#'    orifice_vol_cf = obsOrificeOutVol_cf(dtime_est,
#'                                         waterlevel_ft = level_ft,
#'                                         orifice_height_ft,
#'                                         orifice_diam_in)
#'   )
#' 
#' 

obsOrificeOutVol_cf <- function(dtime_est, 
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

