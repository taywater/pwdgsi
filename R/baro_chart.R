# marsFetchBaroData --------------------------------

#' Fetch barometric pressure data for a target SMP, date range, and interval
#'
#' Returns a data frame with datetime, barometric pressure, smp id, and number of neighbors
#'   interpolated from to collect the data.
#'
#' @param con An ODBC connection to the MARS Analysis database returned by odbc::dbConnect
#' @param target_id chr, single SMP ID where the user has requested data
#' @param start_date POSIXct, format: "YYYY-MM-DD", start of data request range
#' @param end_date POSIXct, format: "YYYY-MM-DD", end of data request range
#' @param data_interval chr, \code{"5 mins"} or \code{"15 mins"}, interval at which baro data will be returned.
#'
#' @return Output will be a dataframe with four columns: 
#'   
#'     \item{dtime_est}{POSIXct, format: "YYYY-MM-DD HH:MM:SS"}
#'     \item{baro_psi}{num, barometric pressure in psi}
#'     \item{smp_id}{chr, SMP ID for each baro}
#'     \item{neighbors}{num, count of baros interpolated from}
#'     
#'     If the target SMP has an on-site baro with data, the "neighbors" column will be NA.
#'     If there are fewer than five baros to interprolate from, based on \code{\link{marsInterpolateBaro}},
#'     all columns other than "dtime_est" will be NA.
#' 
#' @export
#'
#' @seealso \code{\link{marsInterpolateBaro}}
#'
#'

##### Front Matter #####
library(tidyverse)
library(lubridate)
library(magrittr)
library(padr)

#Other Stuff
library(odbc)
library(assertthat)
source("//pwdoows/oows/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Downloader Helper Functions/mars_downloader_helper.R")

setwd("//pwdoows/oows/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro data downloader")
options(stringsAsFactors=FALSE)

##### Step 1: What SMP are you working with?
# Change the SMP ID to tell the database what SMP you're using.
########################
smp_id <- "231-2-1"
########################

# Change the date boundaries to reflect the time period for which you want data
# Data begins Jan 1 2016
### 30 days hath September, April, June and November
### All the rest have 31 (Except February)
####################################
start_date <- mdy("03-01-2019", tz = "EST")
end_date <- mdy("06-01-2019", tz = "EST")
####################################

# What interval do you want for the final data?
# Select 1 of "5 mins" or "15 mins"
# It won't work if you type "Mins" or "minutes" or something like that.
# So please don't do that.
#################################
data_interval <- "5 mins"
#################################

test <- dbConnect(odbc::odbc(), "mars")
#dbListTables(test) #If that didn't work, your DSN isn't working.
#dbDisconnect(test)

mars <- dbConnect(odbc::odbc(), "mars")
smplist <- dbGetQuery(mars, "SELECT * FROM smpid_facilityid_componentid")
smplocations <- dbGetQuery(mars, "SELECT * FROM smp_loc")

# If this assert_that statement doesn't return TRUE, the datbase doesn't know about your SMP.
assert_that(smp_id %in% smplist$smp_id, msg = "SMP ID does not exist in MARS database")

# If this assert_that statement doesn't return TRUE, there isn't a GIS location of your SMP.
assert_that(smp_id %in% smplocations$smp_id, msg = "SMP ID does not have a lat/long location in MARS")

##### Step 2: Find barometric data
print(paste("Fetching baro data for SMP:", smp_id))

#####
#marsFetchBaroData <- function(con, target_id, start_date, end_date, data_interval = c("5 mins", "15 mins")){
  
  con = mars
  target_id = smp_id
  start_date = start_date
  end_date = end_date
  data_interval = data_interval
  
  
  if(!odbc::dbIsValid(con)){
    stop("Argument 'con' is not an open ODBC channel")
  }
  
  #browser()
  #Generate the beginning of a report about the baro request
  report_filename <- paste("//pwdoows/oows/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader/Reports/", paste0(paste(lubridate::today("EST"), target_id, "baro_report", sep ="_"), ".html"))
  report_title <- "Composite Baro Data Generation Report"
  
  #Get SMP locations, and the locations of the baro sensors
  smp_loc <- odbc::dbGetQuery(con, "SELECT * FROM public.smp_loc")
  locus_loc <- dplyr::filter(smp_loc, smp_id == target_id)
  baro_smp <- odbc::dbGetQuery(con, "SELECT DISTINCT smp_id FROM public.baro_rawfile;") %>% dplyr::pull(smp_id)
  
  #Collect baro data
  #Get all baro data for the specified time period
  baro <- odbc::dbGetQuery(con, paste0("SELECT * FROM barodata_smp b WHERE b.dtime_est >= '", start_date, "'", " AND b.dtime_est <= '", end_date + lubridate::days(1), "';"))
  baro$dtime_est %<>% lubridate::force_tz(tz = "EST")
  
  #When the user requests data at a 5-minute resolution, we need to stretch our 15-minute data into 5-minute data
  #We can use tidyr::spread and padr::pad to generate the full 5 minute time series,
  #And then use zoo::na.locf (last observation carried forward) to fill the NAs with the most recent value
  if(data_interval == "5 mins"){
    
    #Spread data to have all baro measurements use the same dtime_est column
    #So we can pad every 15-minute time series at once
    baro <- tidyr::spread(baro, "smp_id", "baro_psi")
    
    #Pad installs 5 minute intervals in our 15 minute dtime_est column. All other columns become NA
    #End value is 10 minutes after the final period because that 15 minute data point is good for 10 more minutes
    baro_pad <- padr::pad(baro, start_val = min(baro$dtime_est), end_val = max(baro$dtime_est) + lubridate::minutes(10), interval = "5 mins")
    
    #We report on the number of LOCF operations
    write("Number of LOCFs", file = report_filename, append = TRUE)

    #To count the LOCF operations, we count the NAs in the data frame before and after the LOCF
    countNAs <- baro_pad[1,]
    for(i in 2:ncol(baro_pad)){
      countNAs[,i] <- sum(is.na(baro_pad[,i])) #count NAs before they are filled
      baro_pad[,i] <- zoo::na.locf(baro_pad[,i], maxgap = 2, na.rm = FALSE) #maxgap = 2 means only fill NAs created by the pad
      countNAs[,i] <- countNAs[,i]- sum(is.na(baro_pad[,i])) #subtract remaining NAs to get number of NAs filled
      #write(paste(colnames(countNAs[i]), ": ", countNAs[,i], sep = ""), file = report_filename, append = TRUE) #Add LOCF count to report
    }
    
    countNAs %<>% select(-dtime_est)
    countNAs_t <- countNAs %>% t() %>% data.frame() %>% tibble::rownames_to_column() %>%  set_colnames(c("Location", "No. of LOCFs"))

    
    #Return baro data to long data format
    baro <- tidyr::gather(baro_pad, "smp_id", "baro_psi", -dtime_est) %>%
      dplyr::filter(!is.na(baro_psi))
  }
  
  
  
  #Calculate the distance between every baro location and the target SMP, then add weight
  baro_weights <- dplyr::filter(smp_loc, smp_id %in% baro_smp) %>%
    dplyr::mutate(lon_dist = lon_wgs84 - locus_loc$lon_wgs84,
                  lat_dist = lat_wgs84 - locus_loc$lat_wgs84,
                  dist_total = sqrt(abs(lon_dist**2 - lat_dist**2))) %>%
    dplyr::mutate(weight = 1/dist_total) %>% #inverse distance weight with power = 1
    dplyr::select(smp_id, weight) %>%
    dplyr::arrange(smp_id)
  
  interpolated_baro <- dplyr::left_join(baro, baro_weights, by = "smp_id") %>% #join baro and weights
    dplyr::group_by(dtime_est) %>% #group datetimes, then calculate weighting effect for each datetime
    dplyr::summarize(baro_psi =  marsInterpolateBaro(baro_psi, smp_id, weight, target_id),
                     smp_id = ifelse(target_id %in% smp_id, target_id, "Interpolated"),
                     neighbors = ifelse(target_id %in% smp_id, NA, dplyr::n()))
  
  #Initialize Final Series
  finalseries <- interpolated_baro
  
  #Give 5 or 15 minute data as appropriate
  if(data_interval == "15 mins"){
    clippedseries <- data.frame(dtime_est = seq.POSIXt(from = start_date, to = end_date + lubridate::days(1), by = data_interval) )
    
    finalseries <- dplyr::filter(finalseries, dtime_est %in% clippedseries$dtime_est)
    baro <- dplyr::filter(baro, dtime_est %in% clippedseries$dtime_est)
  }
  
  return(finalseries)
  
  #Adding "neighbor" counts and instances to report
  neighbors <- dplyr::group_by(finalseries, neighbors) %>% 
    dplyr::summarize(count = dplyr::n()) %>% 
    set_colnames(c("Neighbors", "Count"))
  
  #Baro Raster Plot
  #Get elevations
  baro_elev <- odbc::dbGetQuery(con, "SELECT * FROM smp_elev") %>% filter(smp_id %in% baro$smp_id)
  
  #Bind all raw baro data with interpolated data, and add elevations
  baro_p <- bind_rows(baro, finalseries)
  baro_p <- left_join(baro_p, baro_elev, by = "smp_id") %>% dplyr::select(-smp_elev_uid)
  
  #Set NA elevations to -10 so interpolated data plots at the bottom of the chart
  baro_p$elev_ft[is.na(baro_p$elev_ft)] <- -10
  
  #Add year and day for chart
  baro_p %<>% mutate("day" = yday(baro_p$dtime_est), 
                     "year" = year(baro_p$dtime_est))
  
  #Sort SMP IDs by elevation
  baro_p$smp_id <- factor(baro_p$smp_id, levels = unique(baro_p$smp_id[order(baro_p$elev_ft)]))
  
  baro_p$smp_id %<>% as.factor
  p <- ggplot(baro_p, aes(x = day, y = smp_id)) +
    facet_grid(. ~ year) +
    geom_raster(aes(fill = baro_psi)) +
    scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "RdBu")), name = "Pressure (psi)") +
    theme(axis.text=element_text(colour="black", size=15),
          axis.title.x=element_text(colour="black", size=15),
          axis.title.y=element_text(colour="black", size=15),
          legend.text=element_text(size=15),
          legend.title=element_text(size = 15),
          strip.text.x = element_text(size = 15),
          legend.background = element_blank(),
          panel.background = element_blank()) +
    xlab("Day") + ylab("Baro Sites")
  
  ggsave("./Reports/recentchart.png", plot = p, width = 8, height = 6, dpi = 300)
  
  

  
  #Create a markdown object
  markobj <- c('---',
               'title: `r report_title`',
               'output: html_document',
               '---',
               '',
               '## `r report_title`  ', #title
               'SMP: `r smp_id`  ', #user inputs
               'Start Date: `r start_date`  ',
               'End Date:  `r end_date`  ', 
               'Data Interval: `r data_interval`  ',
               '`r paste0("Baro data has been saved to ", paste(target_id, start_date, "to", end_date, sep = "_"), ".csv")`',
               '```{r neighbors, echo = FALSE}',
               'neighbors %>% knitr::kable()', 
               'if(data_interval == "5 mins"){',
               'countNAs_t %>% knitr::kable()',
               '}',
               '```',
               '![](//pwdoows/oows/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader/Reports/recentchart.png)')
  
  markdown::markdownToHTML(text = knitr::knit(text = markobj), output = report_filename)
  
  #open markdown document
  #browseURL("//pwdoows/oows/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader/Reports/ 2019-09-04_231-2-1_baro_report.html")
  
}

rmarkdown::render("C:/Users/nicholas.manna/Documents/R/working/baro.rmd", params = list(smp_id = smp_id, start_date = start_date, end_date = end_date, data_interval = data_interval, neighbors = neighbors, countNAs = countNAs_t, p = p))

browseURL("C:/Users/nicholas.manna/Documents/R/working/baro.html")


#![](//pwdoows/oows/Watershed Sciences/GSI Monitoring/07 Databases and Tracking Spreadsheets/13 MARS Analysis Database/Scripts/Downloader/Baro Data Downloader/Reports/recentchart.png)