## code to prepare `ZooSpatial.RData` dataset goes here

suppressPackageStartupMessages({
  library(lubridate)
  library(data.table)
  library(sf)
  library(ozmaps)
  library(tidyverse) ## should go near last to put on tip of search path
})

source("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/IMOS_Plankton_functions.R", local = FALSE)

## ZooSpatial.R

freqMapData <-  function(){
  ZooCountNRS <- planktonr::pr_get_NRSZooData() %>%
    rename(Sample = TripCode, Counts = TaxonCount) %>%
    filter(!is.na(Species) & !grepl("cf.|ssp.|grp", Species) & Genus != '') %>% 
    mutate(Taxon = paste0(word(Genus,1), " ", word(Species,1)), 
           Survey = 'NRS') %>%
    group_by(Sample, Survey, Taxon, SampVol_m3) %>% summarise(Counts = sum(Counts, na.rm = TRUE), .groups = "drop")
  
  ZooCountCPR <- planktonr::pr_get_CPRZooData("Count") %>% 
    rename(Counts = TaxonCount) %>%
    filter(!is.na(Species) & !grepl("cf.|ssp.|grp", Species) & Genus != '') %>% 
    mutate(Taxon = paste0(word(Genus,1), " ", word(Species,1)), 
           Survey = 'CPR') %>%
    group_by(Sample, Survey, Taxon, SampVol_m3) %>% summarise(Counts = sum(Counts, na.rm = TRUE), .groups = "drop")
  
  obs <- rbind(ZooCountCPR, ZooCountNRS) %>% arrange(Taxon)
  
  NRSSamp <- planktonr::pr_get_NRSTrips("Z") %>%
    rename(Sample = TripCode, Date = SampleDateLocal) %>%
    mutate(DOY = yday(Date),
           Start = as.Date(paste0(min(year(Date))-1, "-12-31")),
           days = difftime(as.Date(Date), Start, units = "days") %>% as.numeric(),
           thetadoy = (days %% 365.25)/365.25 * 2 * base::pi, ## leap years...
           Survey = 'NRS')  %>% 
    select(Sample, Survey, Date, DOY, Latitude, Longitude, thetadoy) 
  
  CPRSamp <- planktonr::pr_get_CPRSamps("Z") %>% 
    rename(Date = SampleDateUTC) %>%
    mutate(DOY = yday(Date),
           Start = as.Date(paste0(min(year(Date))-1, "-12-31")),
           days = difftime(as.Date(Date), Start, units = "days") %>% as.numeric(),
           thetadoy = (days %% 365.25)/365.25 * 2 * base::pi, ## leap years...
           Survey = 'CPR')  %>% 
    select(Sample, Survey, Date, DOY, Latitude, Longitude, thetadoy)
  
  SampLocs <- rbind(CPRSamp, NRSSamp) %>%
    mutate(Lat = round(Latitude), #/0.5, 0)*0.5,
           Long = round(Longitude), #/0.5, 0)*0.5,
           Month = month(Date),
           Season = ifelse(Month >2 & Month < 6, "March - May",
                           ifelse(Month >5 & Month < 9, "June - August",
                                  ifelse(Month > 8 & Month < 12, "September - November", "December - February")))) %>% 
    select(Sample, Survey, Lat, Long, Season) 
  
  mapdata <- obs %>%
    dplyr::select(.data$Sample, .data$Taxon, .data$Counts) %>%
    dplyr::left_join(SampLocs, by="Sample") %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$Season, .data$Taxon, .data$Lat, .data$Long) %>%
    dplyr::summarise(freq = n()) %>%
    dplyr::left_join(SampLocs %>%  group_by(Lat, Long, Season) %>% summarise(samples = n()), by=c("Lat", "Long", "Season")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(freqsamp = .data$freq/.data$samples,
                  freqfac = as.factor(ifelse(.data$freqsamp<0.375, "Seen in 25%",
                                             ifelse(.data$freqsamp>0.875, "100 % of Samples",
                                                    ifelse(.data$freqsamp>0.375 & .data$freqsamp<0.625, '50%', '75%')))),
                  Season = factor(.data$Season, levels = c("December - February","March - May","June - August","September - November")),
                  Taxon = as.factor(.data$Taxon)) %>%
    dplyr::select(.data$Season, .data$Lat, .data$Long, .data$Taxon, .data$freqsamp, .data$freqfac) 
  
  absences <-  SampLocs %>%  distinct(Lat, Long, Season) %>% mutate(Taxon = "Taxon", freqsamp = 0, freqfac = as.factor("Absent")) 
  freqMapData <- list(mapdata, absences)
  return(freqMapData)
  
}


fMapData <- freqMapData()

# save data into data file
usethis::use_data(obs, Samples, SampLocs, absences, internal = FALSE)

## files for SDMs (this will only work for me at the moment)
listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
file.copy(from=files, to="inst/app/www")


