# marsWriteSaturatedData ------------------------------------------
#' Write Saturated Performance Data to Database 
#' 
#' Receive vectors of infiltration rate and recession rate, calculated with \code{\link{marsSaturatedPerformance_inhr}}
#' gather data, and write to MARS Analysis Database performance_saturated table
#' 
#' @param infiltration_rate_inhr vector, numeric, infiltration rate (in/hr)
#' @param recession_rate_inhr vector, numeric, recession rate (in/hr)
#' @param ow_uid vector, numeric observation well UID
#' @param rainfall_gage_event_uid vector, numeric
#' @param snapshot_uid vector, numeric
#' @param observed_simulated_lookup_uid vector, numeric, 1 if observed, 2 if simulated
#' @param con Formal class PostgreSQL, a connection to the MARS Analysis database
#' 
#' @return \code{TRUE} if the write is succesful, or an error message if unsuccessful
#' 
#' @seealso \code{\link[pwdgsi]{marsWriteOvertoppingData}}, \code{\link{marsWritePercentStorageData}}
#' 
#' @export
#' 
marsWriteSaturatedData <- function(infiltration_rate_inhr,
                                   recession_rate_inhr,
                                   ow_uid,
                                   rainfall_gage_event_uid,
                                   snapshot_uid,
                                   observed_simulated_lookup_uid,
                                   con){
  
  #check that vectors are the same length
  if(!(length(infiltration_rate_inhr) == length(ow_uid) &
       length(infiltration_rate_inhr) == length(rainfall_gage_event_uid) &
       length(infiltration_rate_inhr) == length(snapshot_uid) &
       length(infiltration_rate_inhr) == length(recession_rate_inhr))){
    stop("Vectors must be the same length")
  }
  
  #add vectors to dataframe
  summary_df <- data.frame(infiltration_rate_inhr,
                           recession_rate_inhr,
                           ow_uid,
                           rainfall_gage_event_uid,
                           snapshot_uid) %>% 
    dplyr::filter(observed_simulated_lookup_uid == 1)
  
  #gather saturated performance types in one column
  saturated_table <- tidyr::gather(summary_df, key = "performance_saturated_lookup_uid", value = "saturatedperformance_inhr", infiltration_rate_inhr, recession_rate_inhr)
  
  #reassign recession and infiltration to 1 and 2 
  saturated_table[saturated_table$performance_saturated_lookup_uid == "recession_rate_inhr", "performance_saturated_lookup_uid"] <- 1
  saturated_table[saturated_table$performance_saturated_lookup_uid == "infiltration_rate_inhr", "performance_saturated_lookup_uid"] <- 2
  
  #select columns for dataframe
  saturatedperformance_df <- saturated_table %>% 
    dplyr::select(saturatedperformance_inhr,
                  performance_saturated_lookup_uid,
                  ow_uid,
                  rainfall_gage_event_uid,
                  snapshot_uid)
  
  #write to table, and return either TRUE (for a succesful write) or the error (upon failure)
  result <- tryCatch(dbWriteTable(con, "performance_saturated", saturatedperformance_df, overwrite = FALSE, append = TRUE), 
                     error = function(error_message){
                       return(error_message$message)
                     }
  )
  
  return(result)
}


# marsWritePercentStorageData ------------------------------------------
#' Write Percent of Storaged Used Data to Database 
#' 
#' Receive vectors of raw and relative percent storage data, calculated with \code{\link{marsPeakStorage_percent}},
#' gather data, and write to MARS Analysis performance_percentstorage table
#' 
#' @param infiltration_rate_inhr vector, numeric, infiltration rate (in/hr)
#' @param recession_rate_inhr vector, numeric, recession rate (in/hr)
#' @param ow_uid vector, numeric observation well UID
#' @param rainfall_gage_event_uid vector, numeric
#' @param snapshot_uid vector, numeric
#' @param observed_simulated_lookup_uid vector, numeric, 1 if observed, 2 if simulated
#' @param con Formal class PostgreSQL, a connection to the MARS Analysis database
#' 
#' @seealso \code{\link[pwdgsi]{marsWriteSaturatedData}}, \code{\link{marsWriteOvertoppingData}}
#' 
#' @return \code{TRUE} if the write is succesful, or an error message if unsuccessful
#' 
#' @export
#' 
#' 
marsWritePercentStorageData <- function(percentstorageused_peak,
                                        percentstorageused_relative,
                                        ow_uid,
                                        rainfall_gage_event_uid,
                                        snapshot_uid,
                                        observed_simulated_lookup_uid,
                                        con){
  
  #check that vectors are the same length
  if(!(length(percentstorageused_peak) == length(ow_uid) &
       length(percentstorageused_peak) == length(rainfall_gage_event_uid) &
       length(percentstorageused_peak) == length(snapshot_uid) &
       length(percentstorageused_peak) == length(observed_simulated_lookup_uid) &
       length(percentstorageused_peak) == length(percentstorageused_relative))){
    stop("Vectors must be the same length")
  }
  
  #add vectors to dataframe
  summary_df <- data.frame(percentstorageused_peak,
                           percentstorageused_relative,
                           ow_uid,
                           rainfall_gage_event_uid,
                           observed_simulated_lookup_uid,
                           snapshot_uid)
  
  #gather percent storage types in one column
  percentstorage_table <- tidyr::gather(summary_df, key = "relative", value = "percentstorage", percentstorageused_peak, percentstorageused_relative)
  
  #reassign raw and relative percent storage to FALSE and TRUE 
  percentstorage_table[percentstorage_table$relative == "percentstorageused_peak", "relative"] <- FALSE
  percentstorage_table[percentstorage_table$relative == "percentstorageused_relative", "relative"] <- TRUE
  
  #select columns for dataframe
  percentstorage_df <- percentstorage_table %>% 
    dplyr::select(percentstorage,
                  relative,
                  observed_simulated_lookup_uid,
                  ow_uid,
                  rainfall_gage_event_uid,
                  snapshot_uid)
  
  #write to table, and return either TRUE (for a succesful write) or the error (upon failure)
  result <- tryCatch(dbWriteTable(con, "performance_percentstorage", percentstorage_df, overwrite = FALSE, append = TRUE), 
                     error = function(error_message){
                       return(error_message$message)
                     }
  )
  
  return(result)
}


# marsWriteOvertoppingData ------------------------------------------
#' Write Overtopping Data to Database 
#' 
#' Receive vector of overtopping data, calculated with \code{\link{marsOvertoppingCheck_bool}},
#' and write to MARS Analysis Database performance_overtopping table
#' 
#' @param overtopping vector, logical, TRUE if water level reaches max storage depth
#' @param ow_uid vector, numeric observation well UID
#' @param rainfall_gage_event_uid vector, numeric
#' @param snapshot_uid vector, numeric
#' @param observed_simulated_lookup_uid vector, numeric, 1 if observed, 2 if simulated
#' @param con Formal class PostgreSQL, a connection to the MARS Analysis database
#' 
#' @return \code{TRUE} if the write is succesful, or an error message if unsuccessful
#' 
#' @seealso \code{\link[pwdgsi]{marsWriteSaturatedData}}, \code{\link{marsWritePercentStorageData}}
#' 
#' @export
#' 
marsWriteOvertoppingData <- function(overtopping, 
                                     observed_simulated_lookup_uid, 
                                     ow_uid, 
                                     rainfall_gage_event_uid,
                                     snapshot_uid){
  
  #check that vectors are the same length
  if(!(length(overtopping) == length(ow_uid) &
       length(overtopping) == length(rainfall_gage_event_uid) &
       length(overtopping) == length(snapshot_uid) &
       length(overtopping) == length(observed_simulated_lookup_uid))){
    stop("Vectors must be the same length")
  }
  
  #add vectors to dataframe
  overtopping_df <- data.frame(overtopping,
                               observed_simulated_lookup_uid,
                               ow_uid,
                               rainfall_gage_event_uid,
                               snapshot_uid)
  
  #write to table, and return either TRUE (for a succesful write) or the error (upon failure)
  result <- tryCatch(dbWriteTable(con, "performance_overtopping", overtopping_df, overwrite = FALSE, append = TRUE), 
                     error = function(error_message){
                       return(error_message$message)
                     }
  )
  
  return(result)
  
}