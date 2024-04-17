library(odbc);library(DBI);library(tidyverse);library(magrittr)
mars_con <- dbConnect(odbc(), "mars14_datav2")

test_that("RPSU calculations", {
  
  # load data
  sys306_data <- pwdgsi::sys306_data
  sys1265_data <- pwdgsi::sys1265_data
  sys306_mets <- pwdgsi::sys306_mets
  sys1265_mets <- pwdgsi::sys1265_mets
  sys306_snap <- pwdgsi::sys306_snap
  sys1265_snap <- pwdgsi::sys1265_snap
  
  # pick an event
  event306 <- sys306_data$`Rain Event Data` %>%
              dplyr::filter(eventdepth_in == max(sys306_data$`Rain Event Data`$eventdepth_in)) %>%
              pull(radar_event_uid)
  
  event1265 <- sys1265_data$`Rain Event Data` %>%
                dplyr::filter(eventdepth_in == max(sys1265_data$`Rain Event Data`$eventdepth_in)) %>%
                pull(radar_event_uid)
  
  

  # calculate
  sys306_RPSU <- pwdgsi::marsPeakStorage_percent((sys306_data$`Level Data` %>% dplyr::filter(radar_event_uid == event306) %>% pull(level_ft)),
                                                 sys306_snap$storage_depth_ft)
  
  
  sys1265_RPSU <- pwdgsi::marsPeakStorage_percent((sys1265_data$`Level Data` %>% dplyr::filter(radar_event_uid == event1265) %>% pull(level_ft)),
                                                  sys1265_snap$storage_depth_ft)


  
  # check
  testthat::expect_equal(sys306_RPSU, as.numeric(sys306_mets$RPSU))
  testthat::expect_equal(sys1265_RPSU, as.numeric(sys1265_mets$RPSU))
  
})

test_that("Infil calculations", {
  
  # load data
  sys306_data <- pwdgsi::sys306_data
  sys1265_data <- pwdgsi::sys1265_data
  sys306_mets <- pwdgsi::sys306_mets
  sys1265_mets <- pwdgsi::sys1265_mets
  sys306_snap <- pwdgsi::sys306_snap
  sys1265_snap <- pwdgsi::sys1265_snap

  # pick an event
  event306 <- sys306_data$`Rain Event Data` %>%
    dplyr::filter(eventdepth_in == max(sys306_data$`Rain Event Data`$eventdepth_in)) %>%
    pull(radar_event_uid)
  
  event1265 <- sys1265_data$`Rain Event Data` %>%
    dplyr::filter(eventdepth_in == max(sys1265_data$`Rain Event Data`$eventdepth_in)) %>%
    pull(radar_event_uid)
  
  
  # match the data sizes
  infil_data306 <- sys306_data$`Level Data` %>% 
                   dplyr::filter(radar_event_uid == event306) %>%
                   dplyr::left_join(sys306_data$`Rainfall Data`, by = 'dtime_est')
  
  infil_data1265 <- sys1265_data$`Level Data` %>%
                    dplyr::filter(radar_event_uid == event1265) %>%
                    dplyr::left_join(sys1265_data$`Rainfall Data`, by = 'dtime_est')

  # calculate
  sys306_infil <- pwdgsi::marsInfiltrationRate_inhr(event = event306,
                                                    dtime_est = infil_data306$dtime_est,
                                                    rainfall_in = infil_data306$rainfall_in,
                                                    waterlevel_ft = infil_data306$level_ft,
                                                    storage_vol_ft3 = sys306_snap$storage_volume_ft3,
                                                    storage_depth_ft = sys306_snap$storage_depth_ft)
  
  sys1265_infil <- pwdgsi::marsInfiltrationRate_inhr(event = event1265,
                                                     dtime_est = infil_data1265$dtime_est,
                                                     rainfall_in = infil_data1265$rainfall_in,
                                                     waterlevel_ft = infil_data1265$level_ft,
                                                     storage_vol_ft3 = sys1265_snap$storage_volume_ft3,
                                                     storage_depth_ft = sys1265_snap$storage_depth_ft)
  
  testthat::expect_equal(sys306_infil, as.numeric(sys306_mets$Infiltration_rate_inhrs))
  testthat::expect_equal(sys1265_infil, as.numeric(sys1265_mets$Infiltration_rate_inhrs))
  
})
