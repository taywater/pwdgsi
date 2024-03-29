% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mars_water_level_functions.R
\name{marsSimulatedLevelSeries_ft}
\alias{marsSimulatedLevelSeries_ft}
\title{Simulated Water Level}
\usage{
marsSimulatedLevelSeries_ft(
  dtime_est,
  rainfall_in,
  event,
  infil_footprint_ft2,
  dcia_ft2,
  orifice_height_ft = NA,
  orifice_diam_in = NA,
  storage_depth_ft,
  storage_vol_ft3,
  infil_rate_inhr = 0.06,
  initial_water_level_ft = 0,
  runoff_coeff = 1,
  discharge_coeff = 0.62
)
}
\arguments{
\item{dtime_est}{A vector of POSIXct date times, in ascending order}

\item{rainfall_in}{Rainfall depths during periods corresponding to times in  dtime_est (in)}

\item{event}{A vector of Event IDs}

\item{infil_footprint_ft2}{Total area of the system that is open to infiltration (sf)}

\item{dcia_ft2}{Directly connected impervious area (sf)}

\item{orifice_height_ft}{Orifice height (ft) (NA if no orifice outlet)}

\item{orifice_diam_in}{Orifice diameter (in) (NA if no orifice outlet)}

\item{storage_depth_ft}{Maximum storage depth (ft)}

\item{storage_vol_ft3}{Maximum storage volume (pore space) (cf)}

\item{infil_rate_inhr}{System design infiltration rate (in/hr)}

\item{initial_water_level_ft}{Initial water Level (ft); either a single value or a vector of length equal to and corresponding to length(unique(event)) (Default = 0)}

\item{runoff_coeff}{Rational method coefficient (Default = 1)}

\item{discharge_coeff}{Orifice discharge coefficient (Default = 0.62)}
}
\value{
Output is a dataframe with the following columns: dtime_est, rainfall_in, radar_event_uid, simulated_depth_ft, simulated_vol_ft3, simulated_orifice_vol_ft3
}
\description{
Simulates water level in subsurface stormwater infiltration system with underdrain.
Note: This version of the package targets the PG12 database, and simulating rain gage events is deprecated. The rain event variable name is hard-coded to the radar IDs.
}
\examples{
simulated_data <- marsSimulatedLevelSeries_ft(dtime_est = marsSampleRain$dtime_est, 
  rainfall_in = marsSampleRain$rainfall_in, 
  event = marsSampleRain$event,
  infil_footprint_ft2 = smp_stats$infil_footprint_ft2[7], 
  dcia_ft2 = smp_stats$dcia_ft2[7],
  orifice_height_ft = smp_stats$orifice_height_ft[7],
  orifice_diam_in = smp_stats$orifice_diam_in[7],
  storage_depth_ft = smp_stats$storage_depth_ft[7],
  storage_vol_ft3 = smp_stats$storage_vol_ft3[7],
  infil_rate_inhr = smp_stats$infil_rate_inhr[7])


}
\seealso{
\code{\link{simulation.stats}}
}
