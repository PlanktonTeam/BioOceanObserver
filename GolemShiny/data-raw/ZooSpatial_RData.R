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

ZooCountNRS <- getNRSZooCount() %>%
  rename(Sample = TripCode, Counts = TaxonCount) %>%
  filter(Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species) & Genus != '') %>% 
  mutate(Taxon = paste0(word(Genus,1), " ", word(Species,1)), 
         Survey = 'NRS') %>%
  group_by(Sample, Survey, Taxon, SampVol_m3) %>% summarise(Counts = sum(Counts, na.rm = TRUE), .groups = "drop")

ZooCountCPR <- getCPRZooCount() %>% 
  rename(Counts = TaxonCount) %>%
  filter(Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species) & Genus != '') %>% 
  mutate(Taxon = paste0(word(Genus,1), " ", word(Species,1)), 
         Survey = 'CPR') %>%
  group_by(Sample, Survey, Taxon, SampVol_m3) %>% summarise(Counts = sum(Counts, na.rm = TRUE), .groups = "drop")

obs <- rbind(ZooCountCPR, ZooCountNRS) %>% arrange(Taxon)

NRSSamp <- getNRSTrips() %>%
  rename(Sample = TripCode, Date = SampleDateLocal) %>%
  mutate(DOY = yday(Date),
         Start = as.Date(paste0(min(year(Date))-1, "-12-31")),
         days = difftime(as.Date(Date), Start, units = "days") %>% as.numeric(),
         thetadoy = (days %% 365.25)/365.25 * 2 * base::pi, ## leap years...
         Survey = 'NRS')  %>% 
  select(Sample, Survey, Date, DOY, Latitude, Longitude, thetadoy, SampleType) 

CPRSamp <- getCPRSamps() %>% 
  rename(Date = SampleDateUTC) %>%
  mutate(DOY = yday(Date),
         Start = as.Date(paste0(min(year(Date))-1, "-12-31")),
         days = difftime(as.Date(Date), Start, units = "days") %>% as.numeric(),
         thetadoy = (days %% 365.25)/365.25 * 2 * base::pi, ## leap years...
         Survey = 'CPR')  %>% 
  select(Sample, Survey, Date, DOY, Latitude, Longitude, thetadoy, SampleType)

SampLocs <- rbind(CPRSamp %>% filter(grepl("Z", SampleType)), NRSSamp %>% filter(grepl("Z", SampleType))) %>%
  mutate(Lat = round(Latitude), #/0.5, 0)*0.5,
         Long = round(Longitude), #/0.5, 0)*0.5,
         Month = month(Date),
         Season = ifelse(Month >2 & Month < 6, "March - May",
                         ifelse(Month >5 & Month < 9, "June - August",
                                ifelse(Month > 8 & Month < 12, "September - November", "December - February")))) %>% 
  select(Sample, Survey, Lat, Long, Season) %>% untibble()

Samples <- SampLocs %>%  group_by(Lat, Long, Season) %>% summarise(samples = n()) %>% untibble()

aus <- ozmap()

absences <-  Samples[1:3] %>% mutate(Taxon = "Taxon", freqsamp = 0, freqfac = as.factor("Absent")) %>%
  untibble()


usethis::use_data(obs, Samples, SampLocs, absences, aus, internal = TRUE)

# Updating sysdata with objects you've created just now
new_data <- c("obs", "Samples", "SampLocs", "absences", "aus")
sysdata_filenames <- load("R/sysdata.rda")
save(list = c(sysdata_filenames, new_data), file = "R/sysdata.rda")
