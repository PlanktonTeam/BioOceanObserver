## script for all RData 
## Aim to change all source calls to planktonr functions
# library(planktonr)


# load("~/GitHub/IMOS_BioOceanObserver/R/sysdata.rda")

# rm("CPRfgp", "CPRfgz", "CPRinfo", "datCPRp", "datCPRz",
   # "datNRSm", "datNRSp", "datNRSz", "daynightp", "daynightz",
   # "LTnuts", "NRSfgp", "NRSfgz", "NRSinfo", "Nuts", "Pico", "Pigs",
   # "PolCPR", "PolNRS", "stip", "stiz")

# NRS indices data
datNRSz <- planktonr::pr_get_indices("NRS", "Z")
datNRSp <- planktonr::pr_get_indices("NRS", "P")
datNRSm <- planktonr::pr_get_NRSMicro() ## microbial data

# CPR time series data
datCPRz <- planktonr::pr_get_indices("CPR", "Z")
datCPRp <- planktonr::pr_get_indices("CPR", "P")

# FG time series data
NRSfgz <- planktonr::pr_get_fg("NRS", "Z")
NRSfgp <- planktonr::pr_get_fg("NRS", "P")
CPRfgz <- planktonr::pr_get_fg("CPR", "Z")
CPRfgp <- planktonr::pr_get_fg("CPR", "P")

# BGC Environmental variables data
Nuts <- planktonr::pr_get_NRSChemistry()
Pigs <- planktonr::pr_get_NRSPigments()
Pico <- planktonr::pr_get_NRSPico()
LTnuts <- planktonr::pr_get_LTnuts()


# STI data
stiz <- planktonr::pr_get_sti("Z")
stip <- planktonr::pr_get_sti("P")

# Day-Night data (from CPR only)
daynightz <- planktonr::pr_get_daynight("Z")
daynightp <- planktonr::pr_get_daynight("P")

# Policy data
PolNRS <- planktonr::pr_get_pol("NRS")
PolCPR <- planktonr::pr_get_pol("CPR")
NRSinfo <- planktonr::pr_get_polInfo("NRS")
CPRinfo <- planktonr::pr_get_polInfo("CPR")

# Species distribution data
fMapDataz <- planktonr::pr_get_fMap_data("Z")
fMapDatap <- planktonr::pr_get_fMap_data("P")

# Progress Map
PMapData <- dplyr::bind_rows(planktonr::pr_get_NRSTrips(Type = c("P", "Z")) %>%
  dplyr::select(.data$StationCode, .data$Longitude, .data$Latitude) %>%
  dplyr::rename(Region = .data$StationCode) %>%
  dplyr::mutate(Survey = 'NRS'), 
  readr::read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/deprecated/CPR_Samp.csv")  %>%
  dplyr::select(.data$REGION, .data$LONGITUDE, .data$LATITUDE) %>%
  dplyr::rename(Region = .data$REGION, Longitude = .data$LONGITUDE, Latitude = .data$LATITUDE) %>%
  dplyr::mutate(Survey = 'CPR'))

# add data to sysdata.rda
usethis::use_data(Nuts, Pigs, fMapDataz, fMapDatap, Pico, LTnuts, 
                  PolNRS, PolCPR, NRSinfo, CPRinfo, 
                  datCPRz, datCPRp, datNRSz, datNRSp, datNRSm,
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
                  stiz, stip, daynightz, daynightp,
                  overwrite = TRUE, internal = TRUE)

## files for SDMs (this will only work for Claire at the moment)
listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
file.copy(from=files, to="inst/app/www")

