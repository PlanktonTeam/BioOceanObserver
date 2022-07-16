## script for all RData 

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
datCPRz <- planktonr::pr_get_Indices("CPR", "Z") %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west")) %>% 
  droplevels()

datCPRp <- planktonr::pr_get_Indices("CPR", "P") %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west")) %>% 
  droplevels()

datCPRw <- planktonr::pr_get_Indices("CPR", "W")  %>% # just PCI atm
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west")) %>% 
  droplevels()

# FG time series data
NRSfgz <- planktonr::pr_get_FuncGroups("NRS", "Z")
NRSfgp <- planktonr::pr_get_FuncGroups("NRS", "P")

CPRfgz <- planktonr::pr_get_FuncGroups("CPR", "Z") %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west")) %>% 
  droplevels()
CPRfgp <- planktonr::pr_get_FuncGroups("CPR", "P") %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west")) %>% 
  droplevels()

# BGC Environmental variables data
Nuts <- planktonr::pr_get_NRSChemistry()
Pigs <- planktonr::pr_get_NRSPigments()
Pico <- planktonr::pr_get_NRSPico()
LTnuts <- planktonr::pr_get_LTnuts()

# STI data
stiz <- planktonr::pr_get_STI("Z")
stip <- planktonr::pr_get_STI("P")

# Day-Night data (from CPR only)
daynightz <- planktonr::pr_get_DayNight("Z")
daynightp <- planktonr::pr_get_DayNight("P")

# Policy data
PolNRS <- planktonr::pr_get_PolicyData("NRS")
PolCPR <- planktonr::pr_get_PolicyData("CPR")
PolLTM <- planktonr::pr_get_PolicyData("LTM")

NRSinfo <- planktonr::pr_get_PolicyInfo("NRS")
CPRinfo <- planktonr::pr_get_PolicyInfo("CPR")

# Species distribution data
fMapDataz <- planktonr::pr_get_FreqMap("Z")

fMapDatap <- planktonr::pr_get_FreqMap("P")

# Progress Map
PMapData <- planktonr::pr_get_ProgressMap(c("NRS", "CPR"))

# add data to sysdata.rda
usethis::use_data(Nuts, Pigs, fMapDataz, fMapDatap, Pico, LTnuts, 
                  PolNRS, PolCPR, PolLTM, NRSinfo, CPRinfo, 
                  datCPRz, datCPRp, datCPRw,
                  datNRSz, datNRSp, datNRSm, datNRSw,
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
                  stiz, stip, daynightz, daynightp,
                  overwrite = TRUE, internal = TRUE)

## files for SDMs (this will only work for Claire at the moment)
# listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
# files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
# file.copy(from=files, to="inst/app/www")

