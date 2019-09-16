# #Author: Nick Manna
# #This is a reprex not to be included in stable branch, intended to test and practice baroRasterPlot and rmarkdown::render() before being included in marsFetchBaroData. For devtools and installation to work properly, it need to be commented out.
# 
# ##### Front Matter #####
# library(tidyverse)
# library(lubridate)
# library(magrittr)
# library(padr)
# 
# #Other Stuff
# library(odbc)
# library(assertthat)
# library(pwdgsi)
# 
# #setwd("//pwdoows/oows/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro data Downloader")
# options(stringsAsFactors=FALSE)
# 
# smp_id <- "250-1-1"
# 
# start_date <- lubridate::mdy("01-01-2019", tz = "EST")
# end_date <- lubridate::mdy("03-01-2019", tz = "EST")
# 
# # Select 1 of "5 mins" or "15 mins"
# data_interval <- "5 mins"
# 
# mars <- dbConnect(odbc::odbc(), "mars")
# smplist <- dbGetQuery(mars, "SELECT * FROM smpid_facilityid_componentid")
# smplocations <- dbGetQuery(mars, "SELECT * FROM smp_loc")
# 
# # If this assert_that statement doesn't return TRUE, the datbase doesn't know about your SMP.
# assert_that(smp_id %in% smplist$smp_id, msg = "SMP ID does not exist in MARS database")
# 
# # If this assert_that statement doesn't return TRUE, there isn't a GIS location of your SMP.
# assert_that(smp_id %in% smplocations$smp_id, msg = "SMP ID does not have a lat/long location in MARS")
# 
#   #initialize function variables
#   con = mars
#   target_id = smp_id
#   #start_date = start_date
#   #end_date = end_date
#   #data_interval = data_interval
# 
#   if(!odbc::dbIsValid(con)){
#     stop("Argument 'con' is not an open ODBC channel")
#   }
# 
#   #Get SMP locations, and the locations of the baro sensors
#   smp_loc <- odbc::dbGetQuery(con, "SELECT * FROM public.smp_loc")
#   locus_loc <- dplyr::filter(smp_loc, smp_id == target_id)
#   baro_smp <- odbc::dbGetQuery(con, "SELECT DISTINCT smp_id FROM public.baro_rawfile;") %>% dplyr::pull(smp_id)
# 
#   #Collect baro data
#   #Get all baro data for the specified time period
#   baro <- odbc::dbGetQuery(con, paste0("SELECT * FROM barodata_smp b WHERE b.dtime_est >= '", start_date, "'", " AND b.dtime_est <= '", end_date + lubridate::days(1), "';"))
#   baro$dtime_est %<>% lubridate::force_tz(tz = "EST")
# 
#   #initialize countNAs_t in case the loop doesn't run. It is passed as a param to markdown so it needs to exist.
#   countNAs_t <- 0
#   #When the user requests data at a 5-minute resolution, we need to stretch our 15-minute data into 5-minute data
#   #We can use tidyr::spread and padr::pad to generate the full 5 minute time series,
#   #And then use zoo::na.locf (last observation carried forward) to fill the NAs with the most recent value
#   if(data_interval == "5 mins"){
# 
#     #Spread data to have all baro measurements use the same dtime_est column
#     #So we can pad every 15-minute time series at once
#     baro <- tidyr::spread(baro, "smp_id", "baro_psi")
# 
#     #Pad installs 5 minute intervals in our 15 minute dtime_est column. All other columns become NA
#     #End value is 10 minutes after the final period because that 15 minute data point is good for 10 more minutes
#     baro_pad <- padr::pad(baro, start_val = min(baro$dtime_est), end_val = max(baro$dtime_est) + lubridate::minutes(10), interval = "5 mins")
# 
#     #To count the LOCF operations, we count the NAs in the data frame before and after the LOCF
#     countNAs <- baro_pad[1,]
#     for(i in 2:ncol(baro_pad)){
#       countNAs[,i] <- sum(is.na(baro_pad[,i])) #count NAs before they are filled
#       baro_pad[,i] <- zoo::na.locf(baro_pad[,i], maxgap = 2, na.rm = FALSE) #maxgap = 2 means only fill NAs created by the pad
#       countNAs[,i] <- countNAs[,i]- sum(is.na(baro_pad[,i])) #subtract remaining NAs to get number of NAs filled
#     }
# 
#     countNAs %<>% dplyr::select(-dtime_est)
#     countNAs_t <- countNAs %>% t() %>% data.frame() %>% tibble::rownames_to_column() %>%  magrittr::set_colnames(c("Location", "No. of LOCFs"))
# 
#     #Return baro data to long data format
#     baro <- tidyr::gather(baro_pad, "smp_id", "baro_psi", -dtime_est) %>%
#       dplyr::filter(!is.na(baro_psi))
#   }
# 
#   #Calculate the distance between every baro location and the target SMP, then add weight
#   baro_weights <- dplyr::filter(smp_loc, smp_id %in% baro_smp) %>%
#     dplyr::mutate(lon_dist = lon_wgs84 - locus_loc$lon_wgs84,
#                   lat_dist = lat_wgs84 - locus_loc$lat_wgs84,
#                   dist_total = sqrt(abs(lon_dist**2 - lat_dist**2))) %>%
#     dplyr::mutate(weight = 1/dist_total) %>% #inverse distance weight with power = 1
#     dplyr::select(smp_id, weight) %>%
#     dplyr::arrange(smp_id)
# 
#   interpolated_baro <- dplyr::left_join(baro, baro_weights, by = "smp_id") %>% #join baro and weights
#     dplyr::group_by(dtime_est) %>% #group datetimes, then calculate weighting effect for each datetime
#     dplyr::summarize(baro_psi = marsInterpolateBaro(baro_psi, smp_id, weight, target_id),
#                      smp_id = ifelse(target_id %in% smp_id, target_id, "Interpolated"),
#                      neighbors = ifelse(target_id %in% smp_id, NA, dplyr::n()))
# 
#   #Initialize Final Series
#   finalseries <- interpolated_baro
# 
#   #Give 5 or 15 minute data as appropriate
#   if(data_interval == "15 mins"){
#     clippedseries <- data.frame(dtime_est = seq.POSIXt(from = start_date, to = end_date + lubridate::days(1), by = data_interval) )
# 
#     finalseries <- dplyr::filter(finalseries, dtime_est %in% clippedseries$dtime_est)
#     baro <- dplyr::filter(baro, dtime_est %in% clippedseries$dtime_est)
#   }
# 
#   #return(finalseries)
# 
#   #Adding "neighbor" counts and instances to report
#   neighbors <- dplyr::group_by(finalseries, neighbors) %>%
#     dplyr::summarize(count = dplyr::n()) %>%
#     magrittr::set_colnames(c("Neighbors", "Count"))
# 
#   #Baro Raster Plot
#   #Get elevations
#   #baro_elev <- odbc::dbGetQuery(con, "SELECT * FROM smp_elev") %>% filter(smp_id %in% baro$smp_id)
# 
#   #Bind all raw baro data with interpolated data, and add weights
#   baro_p <- bind_rows(baro, finalseries)
#   baro_p <- left_join(baro_p, baro_weights, by = "smp_id")
# 
#   #Set NA weights to -10 so interpolated data plots at the top of the chart
#   baro_p$weight[is.na(baro_p$weight)] <- -10
# 
#   #Add year and day for chart
#   baro_p %<>% mutate("day" = lubridate::yday(baro_p$dtime_est),
#                      "year" = lubridate::year(baro_p$dtime_est))
# 
#   #Sort SMP IDs by elevation
#   baro_p$smp_id <- factor(baro_p$smp_id, levels = rev(unique(baro_p$smp_id[order(baro_p$weight)])))
# 
#   baro_p$smp_id %<>% as.factor
# 
#   #Create baro Raster Chart
#   p <- baroRasterPlot(baro_p)
# 
#   #Create baro Map
#   baro_loc <- smp_loc %>% filter(smp_id %in% baro$smp_id)
#   rownames(baro_loc) <- NULL
#   coords <- baro_loc[c("lon_wgs84", "lat_wgs84")]
#   baro_sp <- sp::SpatialPointsDataFrame(coords, data = baro_loc, proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#   coords <- locus_loc[c("lon_wgs84", "lat_wgs84")]
#   smp_sp <- sp::SpatialPointsDataFrame(coords, data = locus_loc, proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#   baro_map <- mapview::mapview(baro_sp, layer.name = "Baro") + mapview::mapview(smp_sp, color = "red", col.regions = "red", layer.name = "Target SMP")
# ######
# 
# rmarkdown::render(system.file("rmd", "baro.rmd", package = "pwdgsi"),
#                   params = list(smp_id = smp_id,
#                                 start_date = start_date,
#                                 end_date = end_date,
#                                 data_interval = data_interval,
#                                 neighbors = neighbors,
#                                 countNAs = countNAs_t,
#                                 p = p,
#                                 csv_name = paste0(paste(smp_id, start_date, "to", end_date, sep = "_"), ".csv"),
#                                 map = baro_map))
# 
# new_filename <- paste(lubridate::today(), smp_id, "baro_report.html", sep = "_")
# 
# file.rename(from = paste0(system.file("rmd", "baro.html", package = "pwdgsi")),
#                           to = paste0("O:/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader/Reports/", new_filename))
# 
# browseURL(paste0("O:/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader/Reports/", new_filename))
# 
# 
