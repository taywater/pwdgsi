library(odbc);library(DBI);library(tidyverse);library(magrittr)
mars_con <- dbConnect(odbc(), "mars14_datav2")



testthat::test_that("marsCheckSMPSnapshot",{
  test_smpid <- pwdgsi::snapshotTestData$smp_id
  test_ow_suffix <- pwdgsi::snapshotTestData$ow_suffix
  test_request_date <- pwdgsi::snapshotTestData$request_date
  
  
  mcs_outcome <- pwdgsi::marsCheckSMPSnapshot(con = mars_con,
                               smp_id = test_smpid,
                               ow_suffix = test_ow_suffix,
                               request_date = test_request_date)
  
  testthat::expect_equal(mcs_outcome, c(TRUE,TRUE,FALSE))
  
})



test_that("marsFetchSnapshot",{
  test_smpid <- pwdgsi::snapshotTestData$smp_id
  test_ow_suffix <- pwdgsi::snapshotTestData$ow_suffix
  test_request_date <- pwdgsi::snapshotTestData$request_date
  
  mfs_outcome <- pwdgsi::marsFetchSMPSnapshot(con = mars_con,
                               smp_id = test_smpid,
                               ow_suffix = test_ow_suffix,
                               request_date = test_request_date)
  
  cols <-  c("snapshot_uid", "ow_uid", "well_measurement_uid", "smp_id",
           "ow_suffix", "dcia_ft2", "storage_footprint_ft2", "orifice_diam_in",
           "infil_footprint_ft2", "storage_depth_ft", "lined", "surface",
           "storage_volume_ft3", "infil_dsg_rate_inhr", "old_stays_valid",
           "orifice_lookup_uid", "orificedepth_ft", "sumpdepth_lookup_uid",
           "sumpdepth_ft")
  
  testthat::expect_equal(colnames(mfs_outcome), cols)
  
  testthat::expect_equal(nrow(mfs_oucome), 2)
  
  
})