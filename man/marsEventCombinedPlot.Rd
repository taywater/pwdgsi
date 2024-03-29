% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mars_plotting_functions.R
\name{marsEventCombinedPlot}
\alias{marsEventCombinedPlot}
\title{Plot hyetograph and observed water level for a single plot the same chart with minimal inputs}
\usage{
marsEventCombinedPlot(
  con,
  event_date,
  source = "radar",
  event_uid,
  smp_id,
  ow_suffix,
  sump_correct = TRUE,
  orifice_show = FALSE,
  orifice_height_ft = NULL,
  storage_depth_ft,
  metrics_show = FALSE,
  obs_RSPU,
  sim_RSPU,
  obs_infil_inhr,
  sim_infil_inhr,
  obs_draindown_hr,
  sim_draindown_hr,
  obs_overtopping,
  sim_overtopping
)
}
\arguments{
\item{con}{An ODBC connection to the MARS Analysis database returned by odbc::dbConnect}

\item{event_date}{chr or POSIXCT, day during which event occurs}

\item{source}{chr, rainfall source, one of 'gage'/'gauge' or 'radar'. Defaults to 'radar'}

\item{event_uid}{int, rain event uid. Alternate to override event_date and rain_source (optional)}

\item{smp_id}{chr, SMP ID}

\item{ow_suffix}{chr, OW Suffix}

\item{sump_correct}{boolean, passed to fetch monitoring date to trim data}

\item{...}{additional arguments for showing metrics and overriding orifice/storage elevations. See \code{\link{marsCombinedPlot}}}
}
\value{
Output will be a gridExtra object of the two plots
}
\description{
Return hyetograph and observed water level plot for the same rain event on the same chart
}
\seealso{
\code{\link{marsRainfallPlot}}, \code{\link{marsWaterLevelPlot}}, \code{\link{marsCombinedPlot}}
}
