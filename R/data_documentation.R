#' Interpolate Baro List
#' 
#' A dataset containing barometric pressures and inverse distance weights for SMPs,
#' along with a target SMP ID, for which the user requests data. 
#' 
#' @docType data
#' 
#' @usage \code{interpolateBaro}
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
"interpolateBaro"


#' Rainfall
#' 
#' A dataset containing datetimes, rainfall in inches, and rainfall gage id. 
#' 
#' @docType data
#' 
#' @usage \code{rainfall}
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
"rainfall"
