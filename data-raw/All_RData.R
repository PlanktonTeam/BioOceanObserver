## script for all RData 
library(tidyverse)

# NRS indices data
datNRSz <- planktonr::pr_get_Indices("NRS", "Z") 
datNRSp <- planktonr::pr_get_Indices("NRS", "P") 
datNRSm <- planktonr::pr_get_NRSMicro() ## microbial data
datNRSw <- planktonr::pr_get_Indices("NRS", "W") %>%
  tidyr::pivot_wider(values_from = "Values", names_from = "Parameters") %>%
  dplyr::mutate(MLD_m = dplyr::case_when(.data$MLDtemp_m <= .data$MLDsal_m ~ .data$MLDtemp_m,
                                         .data$MLDsal_m < .data$MLDtemp_m ~ .data$MLDsal_m,
                                         TRUE ~ NA_real_)) %>%
  dplyr::select(-c(.data$MLDtemp_m, .data$MLDsal_m)) %>%
  tidyr::pivot_longer(-c(.data$Year_Local:.data$StationCode), names_to = 'Parameters', values_to = 'Values')

# CPR time series data
datCPRz <- planktonr::pr_get_Indices("CPR", "Z", join = "st_nearest_feature") %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west")) %>% 
  droplevels()

datCPRp <- planktonr::pr_get_Indices("CPR", "P", join = "st_nearest_feature") %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west")) %>% 
  droplevels()

datCPRw <- planktonr::pr_get_Indices("CPR", "W", join = "st_nearest_feature")  %>% # just PCI atm
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west")) %>% 
  droplevels()

# FG time series data
NRSfgz <- planktonr::pr_get_FuncGroups("NRS", "Z")
NRSfgp <- planktonr::pr_get_FuncGroups("NRS", "P")

CPRfgz <- planktonr::pr_get_FuncGroups("CPR", "Z", join = "st_nearest_feature") %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west")) %>% 
  droplevels()
CPRfgp <- planktonr::pr_get_FuncGroups("CPR", "P", join = "st_nearest_feature") %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west")) %>% 
  droplevels()

# BGC Environmental variables data
Nuts <- planktonr::pr_get_NRSChemistry() %>% planktonr::pr_remove_outliers(2)
Pigs <- planktonr::pr_get_NRSPigments(Format = "binned") %>% planktonr::pr_remove_outliers(2)
Pico <- planktonr::pr_get_NRSPico() %>% planktonr::pr_remove_outliers(2)
LTnuts <- planktonr::pr_get_LTnuts() %>% planktonr::pr_remove_outliers(2)

# STI data
stiz <- planktonr::pr_get_STI("Z")
stip <- planktonr::pr_get_STI("P")

# Day-Night data (from CPR only)
daynightz <- planktonr::pr_get_DayNight("Z")
daynightp <- planktonr::pr_get_DayNight("P")

# Policy data
PolNRS <- planktonr::pr_get_PolicyData("NRS") %>% planktonr::pr_remove_outliers(2)
PolCPR <- planktonr::pr_get_PolicyData("CPR", join = "st_nearest_feature") %>% planktonr::pr_remove_outliers(2)
PolLTM <- planktonr::pr_get_PolicyData("LTM") %>% planktonr::pr_remove_outliers(2)

NRSinfo <- planktonr::pr_get_PolicyInfo("NRS")
CPRinfo <- planktonr::pr_get_PolicyInfo("CPR")

# Species distribution data
fMapDataz <- planktonr::pr_get_FreqMap("Z")

fMapDatap <- planktonr::pr_get_FreqMap("P")

# Progress Map
PMapData <- planktonr::pr_get_ProgressMap(c("NRS", "CPR"))


PMapDatan <- dplyr::bind_rows(planktonr::pr_get_Indices("NRS", "Z"), planktonr::pr_get_Indices("NRS", "P")) %>% 
  dplyr::filter(.data$Parameters == "ZoopAbundance_m3" | .data$Parameters == "PhytoAbundance_CellsL") %>% 
  tidyr::pivot_wider(names_from = .data$Parameters, values_from = .data$Values) %>% 
  dplyr::rename(Name = .data$StationName) %>% 
  dplyr::select(-.data$StationCode) %>% 
  dplyr::mutate(Survey = "NRS")

PMapDatac <- dplyr::bind_rows(planktonr::pr_get_Indices("CPR", "Z"), planktonr::pr_get_Indices("CPR", "P")) %>% 
  dplyr::filter(.data$Parameters == "ZoopAbundance_m3" | .data$Parameters == "PhytoAbundance_Cellsm3") %>% 
  tidyr::pivot_wider(names_from = .data$Parameters, values_from = .data$Values) %>% 
  dplyr::mutate(PhytoAbundance_Cellsm3 = .data$PhytoAbundance_Cellsm3/1e3, 
                Survey = "CPR") %>% 
  dplyr::rename(PhytoAbundance_CellsL = .data$PhytoAbundance_Cellsm3, 
                Name = .data$BioRegion)

PMapData2 <- dplyr::bind_rows(PMapDatan, PMapDatac) %>% 
  dplyr::select(-c(.data$Year_Local, .data$Month_Local , .data$tz))
  
  # add data to sysdata.rda
usethis::use_data(Nuts, Pigs, fMapDataz, fMapDatap, Pico, LTnuts, 
                    PolNRS, PolCPR, PolLTM, NRSinfo, CPRinfo, 
                    datCPRz, datCPRp, datCPRw,
                    datNRSz, datNRSp, datNRSm, datNRSw,
                    NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
                    stiz, stip, daynightz, daynightp, PMapData2,
                    overwrite = TRUE, internal = TRUE)

## files for SDMs (this will only work for Claire at the moment)
# listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
# files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
# file.copy(from=files, to="inst/app/www")

