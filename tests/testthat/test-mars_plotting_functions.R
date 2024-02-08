library(odbc);library(DBI);library(tidyverse);library(magrittr)
mars_con <- dbConnect(odbc(), "mars14_datav2")


expectedCombined <- c("gtable", "gTree", "grob", "gDesc")

test_that("marsWaterLevelplot", {
  # # data path
  # folder <- paste0(getwd(),'/tests/testthat/')
  # # source test data for a few systems
  # load("C://users//brian.cruice//pwdgsi//tests//testthat//sys306_data.Rds")
  # load("c:/users/brian.cruice/pwdgsi/tests/testthat/sys1265_data.Rds")
  # load("C://users//brian.cruice//pwdgsi//tests//testthat//sys1265_snap.Rds")
  # load("C://users//brian.cruice//pwdgsi//tests//testthat//sys306_snap.Rds")
  
  # load data
  load(system.file("testdata","sys306_data.Rds",package = "pwdgsi"))
  load(system.file("testdata","sys1265_data.Rds",package = "pwdgsi")) 
  load(system.file("testdata","sys306_snap.Rds",package = "pwdgsi"))
  load(system.file("testdata","sys1265_snap.Rds",package = "pwdgsi"))
  
  #system 2
  sys1265_event <- sys1265_data$`Rain Event Data` %>%
    dplyr::filter(eventdepth_in == max(eventdepth_in))
  
  sys306_event <- sys306_data$`Rain Event Data` %>%
    dplyr::filter(eventdepth_in == max(eventdepth_in))
  
  sys1265_rain <- sys1265_data$`Rainfall Data` %>% dplyr::filter(radar_event_uid == sys1265_event$radar_event_uid)
  sys1265_level <- sys1265_data$`Level Data` %>% dplyr::filter(radar_event_uid == sys1265_event$radar_event_uid)

  sys306_rain <- sys306_data$`Rainfall Data` %>% dplyr::filter(radar_event_uid == sys306_event$radar_event_uid)
  sys306_level <- sys306_data$`Level Data` %>% dplyr::filter(radar_event_uid == sys306_event$radar_event_uid)
  
  synthetic_2 <- sys1265_level$level_ft + 0.6
  synthetic_3 <- sys1265_level$level_ft + 1.8
  synthetic_4 <- sys1265_level$level_ft + 2.2
  
  synthetic_5 <- sys306_level$level_ft + 0.6
  synthetic_6 <- sys306_level$level_ft + 1.2
  synthetic_7 <- sys306_level$level_ft + 1.8
  
  waterlev1 <- marsWaterLevelPlot(event = "testplot", storage_depth_ft = sys1265_snap$storage_depth_ft,
                     obs_datetime = sys1265_level$dtime_est, obs_level_ft = sys1265_level$level_ft,
                     datetime_2 = sys1265_level$dtime_est,
                     level_ft_2 = synthetic_2,
                     datetime_3 = sys1265_level$dtime_est,
                     level_ft_3 = synthetic_3,
                     datetime_4 = sys1265_level$dtime_est,
                     level_ft_4 = synthetic_4,
                     structure_name = "OW1")
  
  waterlev2 <- marsWaterLevelPlot(event = "testplot2", storage_depth_ft = sys306_snap$storage_depth_ft,
                                  obs_datetime = sys306_level$dtime_est, obs_level_ft = sys306_level$level_ft,
                                  datetime_2 = sys306_level$dtime_est,
                                  level_ft_2 = synthetic_5,
                                  datetime_3 = sys306_level$dtime_est,
                                  level_ft_3 = synthetic_6,
                                  datetime_4 = sys306_level$dtime_est,
                                  level_ft_4 = synthetic_7,
                                  structure_name = "OW1")
  
  # correct layer number
  testthat::expect_equal(length(waterlev1$layers), expected = 10)
  testthat::expect_equal(length(waterlev2$layers), expected = 10)
  

  
})


expectedWLMets <- c("gg", "ggplot")

test_that("marsWaterLevelplot with metrics", {
  
  # # source test data for a few systems
  load(system.file("testdata","sys306_data.Rds",package = "pwdgsi"))
  load(system.file("testdata","sys1265_data.Rds",package = "pwdgsi")) 
  sys306_mets <- readRDS(system.file("testdata","sys306_metrics.Rds",package = "pwdgsi"))
  sys1265_mets <- readRDS(system.file("testdata","sys1265_metrics.Rds",package = "pwdgsi"))
  load(system.file("testdata","sys306_snap.Rds",package = "pwdgsi"))
  load(system.file("testdata","sys1265_snap.Rds",package = "pwdgsi"))
  
  
  #system 2
  sys1265_event <- sys1265_data$`Rain Event Data` %>%
    dplyr::filter(eventdepth_in == max(eventdepth_in))
  
  sys306_event <- sys306_data$`Rain Event Data` %>%
    dplyr::filter(eventdepth_in == max(eventdepth_in))
  
  sys1265_rain <- sys1265_data$`Rainfall Data` %>% dplyr::filter(radar_event_uid == sys1265_event$radar_event_uid)
  sys1265_level <- sys1265_data$`Level Data` %>% dplyr::filter(radar_event_uid == sys1265_event$radar_event_uid)
  
  sys306_rain <- sys306_data$`Rainfall Data` %>% dplyr::filter(radar_event_uid == sys306_event$radar_event_uid)
  sys306_level <- sys306_data$`Level Data` %>% dplyr::filter(radar_event_uid == sys306_event$radar_event_uid)
  
  synthetic_2 <- sys1265_level$level_ft + 0.6
  synthetic_3 <- sys1265_level$level_ft + 1.8
  synthetic_4 <- sys1265_level$level_ft + 2.2
  
  synthetic_5 <- sys306_level$level_ft + 0.6
  synthetic_6 <- sys306_level$level_ft + 1.2
  synthetic_7 <- sys306_level$level_ft + 1.8
  
  wl_met1 <- marsWaterLevelPlot(event = "testplot", storage_depth_ft = sys1265_snap$storage_depth_ft,
                                  obs_datetime = sys1265_level$dtime_est, obs_level_ft = sys1265_level$level_ft,
                                  datetime_2 = sys1265_level$dtime_est,
                                  level_ft_2 = synthetic_2,
                                  datetime_3 = sys1265_level$dtime_est,
                                  level_ft_3 = synthetic_3,
                                  datetime_4 = sys1265_level$dtime_est,
                                  level_ft_4 = synthetic_4,
                                  structure_name = "OW1",
                                  metrics_show = TRUE,
                                  obs_RSPU = sys1265_mets$RPSU[1],
                                  obs_infil_inhr = sys1265_mets$Infiltration_rate_inhrs[1],
                                  obs_draindown_hr = sys1265_mets$Draindown_hrs[1],
                                  obs_overtopping = sys1265_mets$Overtopping[1])
  
  wl_met2 <- marsWaterLevelPlot(event = "testplot2", storage_depth_ft = sys306_snap$storage_depth_ft,
                                  obs_datetime = sys306_level$dtime_est, obs_level_ft = sys306_level$level_ft,
                                  datetime_2 = sys306_level$dtime_est,
                                  level_ft_2 = synthetic_5,
                                  datetime_3 = sys306_level$dtime_est,
                                  level_ft_3 = synthetic_6,
                                  datetime_4 = sys306_level$dtime_est,
                                  level_ft_4 = synthetic_7,
                                  structure_name = "OW1",
                                  metrics_show = TRUE,
                                  obs_RSPU = sys306_mets$RPSU[1],
                                  obs_infil_inhr = sys306_mets$Infiltration_rate_inhrs[1],
                                  obs_draindown_hr = sys306_mets$Draindown_hrs[1],
                                  obs_overtopping = sys306_mets$Overtopping[1])

  expect_s3_class(wl_met1, expectedWLMets)
  expect_s3_class(wl_met2, expectedWLMets)
  # correct layer number
  testthat::expect_equal(length(wl_met1$layers), expected = 11)
  testthat::expect_equal(length(wl_met2$layers), expected = 11)
  
})


test_that("marsEventCombinedPlot w/ date supplied", {
  comb1 <- marsEventCombinedPlot(con = mars_con,
                                 event_date = '2023-01-25',
                                 smp_id = '1265-7-1',
                                 ow_suffix = 'OW1')
  
  expect_s3_class(comb1,expectedCombined)
  
  comb2 <- marsEventCombinedPlot(con = mars_con,
                                 event_date = '2023-01-04',
                                 smp_id = '306-3-1',
                                 ow_suffix = 'OW1')
  
  expect_s3_class(comb2,expectedCombined)
  
  })


test_that("marsEventCombinedPlot w/ event supplied", {
  
  # # source pre-calculated metrics
  sys306_mets <- readRDS(system.file("testdata","sys306_metrics.Rds",package = "pwdgsi"))
  sys1265_mets <- readRDS(system.file("testdata","sys1265_metrics.Rds",package = "pwdgsi"))

  
  comb3 <- marsEventCombinedPlot(con = mars_con,
                                 event_uid = 332235,
                                 smp_id = '1265-7-1',
                                 ow_suffix = 'OW1',
                                 metrics_show = TRUE,
                                 obs_RSPU = sys1265_mets$RPSU[1],
                                 obs_infil_inhr = sys1265_mets$Infiltration_rate_inhrs[1],
                                 obs_draindown_hr = sys1265_mets$Draindown_hrs[1],
                                 obs_overtopping = sys1265_mets$Overtopping[1])
  
  expect_s3_class(comb3,expectedCombined)
  
  comb4 <- marsEventCombinedPlot(con = mars_con,
                                 event_uid = 330576,
                                 smp_id = '306-3-1',
                                 ow_suffix = 'OW1',
                                 metrics_show = TRUE,
                                 obs_RSPU = sys306_mets$RPSU[1],
                                 obs_infil_inhr = sys306_mets$Infiltration_rate_inhrs[1],
                                 obs_draindown_hr = sys306_mets$Draindown_hrs[1],
                                 obs_overtopping = sys306_mets$Overtopping[1])
  
  expect_s3_class(comb4,expectedCombined)
  
})



