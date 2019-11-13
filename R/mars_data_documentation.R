#' Sample Baro Data
#' 
#' A dataset containing barometric pressures and inverse distance weights for SMPs,
#' along with a target SMP ID, for which the user requests data. 
#' 
#' @docType data
#' 
#' @usage \code{marsSampleBaro}
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
#' @usage \code{marsSampleRain}
#' 
#' @format A dataframe with 105 rows and 3 variables
#' \describe{
#'   \item{dtime_edt}{POSIXct datetime, YYYY-MM-DD HH:MM:SS}
#'   \item{rainfall_in}{rainfall total in the 15 minutes preceding the corresponding datetime}
#'   \item{gage_uid}{rain gage ID}
#' }
#' 
#' @seealso \code{\link{detectEvents}}, \code{\link{storm}}, \code{\link{hyetograph}}
#' 
"marsSampleRain"

#' Sample Baro Data for Plotting
#' 
#' A dataset containing datetime, smp_id, barometric pressure, number of neighbors, and weight
#' 
#' @docType data
#' 
#' @usage \code{marsBaroRasterPlot}
#' 
#' @format a dataframe with 2890 rows and 5 variables
#' \describe{
#'   \item{dtime_est}{POSIXct datetime, YYYY-MM-DD HH:MM:SS}
#'   \item{smp_id}{factor, SMP ID}
#'   \item{baro_psi}{num, barometric pressure (psi)}
#'   \item{neighbors}{int, number of nearby barometric sensors with recorded data}
#'   \item{weight}{num, weight of baro data based on distance, for interpolation}}
"marsSampleBaro_plot"

#' Sample SMP Data
#' 
#' A dataset containing water level, rain, and SMP data. \code{obs_250_fill} is joined with SMP stats.  
#' 
#' @docType data
#' 
#' @usage \code{obs_250_all}
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
"obs_250_fill"

#' SMP Snapshot
#' 
#' A dataset containing stats like footprint and storage depth for SMP 250-1-1
#' 
#' @docType data
#' 
#' @usage \code{snapshot_250}
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
#' @usage \code{monitoringdata_250}
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
#' @usage \code{summary_250}
#' 
#' @format a dataframe with 27 rows and 11 columns
#' 
"summary_250"
