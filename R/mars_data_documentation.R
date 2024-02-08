#' Sample Baro Data
#' 
#' A dataset containing barometric pressures and inverse distance weights for SMPs,
#' along with a target SMP ID, for which the user requests data. 
#' 
#' @docType data
#' 
#' @usage marsSampleBaro
#' 
#' @format A list containing a dataframe with 9 rows and 3 variables, and a character. 
#' \describe{
#'   \item{smp_id}{SMP ID}
#'   \item{baro_psi}{barometric pressure at the corresponding SMP}
#'   \item{weight}{weight assigned based on distance from target SMP}
#'   \item{target_ID}{SMP ID for which the user requests data}
#' }
#' 
#' @seealso \code{\link{marsInterpolateBaro}}
#' 
"marsSampleBaro"


#' Sample Rain Data
#' 
#' A dataset containing datetimes, rainfall in inches, and rainfall gage id. 
#' 
#' @docType data
#' 
#' @usage marsSampleRain
#' 
#' @format A dataframe with 105 rows and 3 variables
#' \describe{
#'   \item{dtime_edt}{POSIXct datetime, YYYY-MM-DD HH:MM:SS}
#'   \item{rainfall_in}{rainfall total in the 15 minutes preceding the corresponding datetime}
#'   \item{gage_uid}{rain gage ID}
#' }
#' 
#' @seealso \code{\link{marsDetectEvents}}, \code{\link{marsRainfallPlot}}
#' 
"marsSampleRain"

#' Sample Baro Data for Plotting
#' 
#' A dataset containing datetime, smp_id, barometric pressure, number of neighbors, and weight
#' 
#' @docType data
#' 
#' @usage marsBaroRasterPlot
#' 
#' @format a dataframe with 2890 rows and 5 variables
#' \describe{
#'   \item{dtime_est}{POSIXct datetime, YYYY-MM-DD HH:MM:SS}
#'   \item{smp_id}{factor, SMP ID}
#'   \item{baro_psi}{num, barometric pressure (psi)}
#'   \item{neighbors}{int, number of nearby barometric sensors with recorded data}
#'   \item{weight}{num, weight of baro data based on distance, for interpolation}
#'   }
"marsSampleBaro_plot"

#' Sample SMP Data
#' 
#' A dataset containing water level, rain, and SMP data. \code{obs_250_fill} is joined with SMP stats.  
#' 
#' @docType data
#' 
#' @usage obs_250_all
#' 
#' @format a dataframe with 2890 rows and 5 variables
#' \describe{
#'  \item{dtime_est}{POSIXct datetime, YYYY-MM-DD HH:MM:SS}
#'  \item{level_ft}{num, water level (ft)}
#'  \item{smp_id}{factor, SMP ID}
#'  \item{ow_suffix}{factor, OW Suffix}
#'  \item{rainfall_in}{num, rainfall at each time step (in)}
#'  \item{gagename}{num, rain gage ID}
#'  \item{event}{num, rain event}
#'  
#'  }
"obs_250_all"

#' obs_250_fill
#' 
#' A dataset containing the data of \code{obs_250_all} joined with select SMP stats from \code{smp_stats}.
#' 
#' @docType data
#' 
#' @usage obs_250_fill
#' 
#' @format a dataframe with 67820 rows and 14 variables
#' \describe{
#'  \item{dtime_est}{POSIXct datetime, YYYY-MM-DD HH:MM:SS}
#'  \item{level_ft}{num, water level (ft)}
#'  \item{smp_id}{factor, SMP ID}
#'  \item{ow_suffix}{factor, OW Suffix}
#'  \item{rainfall_in}{num, rainfall at each time step (in)}
#'  \item{gagename}{num, rain gage ID}
#'  \item{event}{num, rain event}
#'  \item{storage_depth_ft}{num, system storage depth (ft)}
#'  \item{storage_vol_ft3}{num, system storage volume (cf)}
#'  \item{infil_footprint_ft2}{num, system infiltration footprint (sf)}
#'  \item{dcia_ft2}{num, directly connected impervious area (sf)}
#'  \item{orifice_height_ft}{num, height of the slow release orifice relative to the bottom of stone storage (ft)}
#'  \item{orifice_diam_in}{num, orifice diameter (in)}
#'  \item{orifice_vol_cf}{num, volume leaving the orifice during the timestep (cf)}
#' }
"obs_250_fill"

#' SMP Snapshot
#' 
#' A dataset containing stats like footprint and storage depth for SMP 250-1-1
#' 
#' @docType data
#' 
#' @usage snapshot_250
#' 
#' @format a dataframe with 1 row and 16 columns
#' 
"snapshot_250"

#' SMP Monitoring Data
#' 
#' A dataset containing water level, rainfall, and rain event data for SMP 250-1-1 from 2018-01-01 to 2018-03-01. 
#' 
#' @docType data
#' 
#' @usage monitoringdata_250
#' 
#' @format a list with 3 dataframes
#' 
"monitoringdata_250"

#' SMP Summary 
#' 
#' A dataset containing summary statistics for SMP 250-1-1 from 2018-01-01 to 2018-03-01.
#' 
#' @docType data
#' 
#' @usage summary_250
#' 
#' @format a dataframe with 27 rows and 11 columns
#' 
"summary_250"

#' smp_stats




#' baro_spdf
#' 
#' A spatial dataset containing the latitude, longitude, smp id, and uid of the MARS barometric sensor network.
#' 
#' @docType data
#' 
#' @usage baro_spdf
#' 
#' @format Spatial Points Dataframe
"baro_spdf" 

#' smp_stats
#' 
#' A dataset containing summary statistics for 8 SMPs.
#' 
#' @docType data
#' 
#' @usage smp_stats
#' 
#' @format a dataframe with 8 rows and 11 columns
#' #' \describe{
#'  \item{smp_id}{string, SMP ID}
#'  \item{ow_suffix}{factor, OW Suffix}
#'  \item{smp_footprint_ft2}{num, SMP surface area footprint (sf)}
#'  \item{infil_footprint_ft2}{num, SMP infiltration surface area footprint (sf)}
#'  \item{dcia_ft2}{num, directly connected impervious area (sf)}
#'  \item{orifice_height_ft}{num, height of the slow release orifice relative to the bottom of stone storage (ft)}
#'  \item{orifice_diam_in}{num, orifice diameter (in)}
#'  \item{storage_depth_ft}{num, system storage depth (ft)}
#'  \item{sump_depth_ft}{num, observation well sump depth (ft)}
#'  \item{storage_vol_ft3}{num, system storage volume (cf)}
#'  \item{infil_rate_inhr}{infiltration rate (in/hr)}
#' }
"smp_stats"

#' simulated_data
#' 
#' An example dataframe of simulated draindown data
#' 
#' @docType data
#' 
#' @usage simulated_data
#' 
#' @format a dataframe with 1309 rows and 6 columns
#'   \describe{
#'   \item{dtime_edt}{POSIXct datetime, YYYY-MM-DD HH:MM:SS}
#'   \item{rainfall_in}{num, rainfall total in the 15 minutes preceding the corresponding datetime (in)}
#'   \item{event}{int, rainfall event uid}
#'   \item{simulated_depth_ft}{simulated water level in observation well (ft)}
#'   \item{simulated_vol_ft3}{simulated volume in the system (cf)}
#'   \item{simulated_orifice_vol_ft3}{simulated volume leaving the orifice (cf)}
#'   }
#' 
"simulated_data"


#' sys1265_data
#' 
#' An example list of observed data for system 1265-7. Used for testing function performance.
#' 
#' @docType list
#' 
#' @usage testing

#' 
"sys1265_data"

#' sys1265_mets
#' 
#' An example list of metrics for system 1265-7. Used for testing function performance.
#' 
#' @docType list
#' 
#' @usage testing

#' 
"sys1265_mets"

#' sys1265_snap
#' 
#' An example snapshot for system 1265-7. Used for testing function performance.
#' 
#' @docType list
#' 
#' @usage testing

#' 
"sys1265_snap"

