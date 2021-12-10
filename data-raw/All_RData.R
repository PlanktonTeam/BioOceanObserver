## script for all RData 
## Aim to change all source calls to planktonr functions

library(planktonr)

# NRS time series data
datNRSz <- planktonr::pr_get_tsdata("NRS", "Z")
datNRSp <- planktonr::pr_get_tsdata("NRS", "P")

# CPR time series data
datCPRz <- planktonr::pr_get_tsdata("CPR", "Z")
datCPRp <- planktonr::pr_get_tsdata("CPR", "P")

# FG time series data
NRSfgz <- planktonr::pr_get_fg("NRS", "Z")
NRSfgp <- planktonr::pr_get_fg("NRS", "P")
CPRfgz <- planktonr::pr_get_fg("CPR", "Z")
CPRfgp <- planktonr::pr_get_fg("CPR", "P")

# BGC Environmental variables data
Nuts <- planktonr::pr_get_nuts()
Pigs <- planktonr::pr_get_pigs()
Pico <- planktonr::pr_get_pico()

# Species distribution data
fMapDataz <- planktonr::pr_get_fMap_data("Z")
fMapDatap <- planktonr::pr_get_fMap_data("P")

# STI data
stiz <- planktonr::pr_get_sti("Z")
stip <- planktonr::pr_get_sti("P")

# daynight data (from CPR only)
daynightz <- planktonr::pr_get_daynight("Z")
daynightp <- planktonr::pr_get_daynight("P")

## microbial data
library(tidyverse)
datNRSm <- readr::read_csv("data/datNRSm.csv") %>%
  dplyr::mutate(SampleDepth_m = as.numeric(stringr::str_sub(TripCode_depth, -3, -1))) %>%
  dplyr::select(StationName, SampleDepth_m, StationCode, SampleDateLocal, Year, Month, 
         Prochlorococcus_cells_ml:Eukaryote_Chlorophyll_Index) %>%
  dplyr::rename(Prochlorococcus_Cellsml = Prochlorococcus_cells_ml,
                Synecochoccus_Cellsml = Synecochoccus_cells_ml,
                Picoeukaryotes_Cellsml = Picoeukaryotes_cells_ml) %>%
  tidyr::pivot_longer(-c(StationName:Month), values_to = "Values", names_to = "parameters")

# add data to sysdata.rda
usethis::use_data(Nuts, Pigs, fMapDataz, fMapDatap, Pico,
                  datCPRz, datCPRp, datNRSz, datNRSp, datNRSm,
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp,
                  stiz, stip, daynightz, daynightp,
                  overwrite = TRUE, internal = TRUE)

## files for SDMs (this will only work for Claire at the moment)
listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
file.copy(from=files, to="inst/app/www")

