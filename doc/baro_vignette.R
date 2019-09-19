## ----setup, include = FALSE----------------------------------------------
library(pwdgsi)
library(magrittr)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----map, fig.height=4, fig.width=4.5, include = FALSE-------------------
#leaving this here for now in case the map needs to be modified. 
# baro_map <- mapview::mapview(baro_spdf, legend = FALSE)
# mapview::mapshot(baro_map, file = "C:/Users/nicholas.manna/Documents/R/pwdgsi/vignettes/baro_map.png")


## ----table, echo = FALSE-------------------------------------------------
knitr::kable(marsSampleBaro[[1]], caption = "Table 1: Sample Baro Data", digits = 2)

## ----data----------------------------------------------------------------
marsInterpolateBaro(
   baro_psi = marsSampleBaro[[1]]$baro_psi, 
   smp_id = marsSampleBaro[[1]]$smp_id, 
   weight = marsSampleBaro[[1]]$weight, 
   target_id = marsSampleBaro[[2]]
   )

## ----raster_data, echo = FALSE-------------------------------------------
marsSampleBaro_plot %>% {.[sample(nrow(.)),]} %>% head(5) %>% 
  knitr::kable(caption = "Table 2: Sample Baro Plot Data", row.names = FALSE, digits = 2) 
marsSampleBaro_plot %<>% dplyr::mutate("day" = yday_decimal(marsSampleBaro_plot$dtime_est),
                               "year" = lubridate::year(marsSampleBaro_plot$dtime_est))

## ----raster_plot, fig.width=7, fig.height=5, message=FALSE, fig.cap="Figure 2: Raster Plot of Barometric Pressures from Jan. 1 to Jan. 4, 2019"----
baroRasterPlot(marsSampleBaro_plot)

