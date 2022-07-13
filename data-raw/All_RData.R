## script for all RData 
## Aim to change all source calls to planktonr functions
# library(planktonr)


# load("~/GitHub/IMOS_BioOceanObserver/R/sysdata.rda")

# rm("CPRfgp", "CPRfgz", "CPRinfo", "datCPRp", "datCPRz",
   # "datNRSm", "datNRSp", "datNRSz", "daynightp", "daynightz",
   # "LTnuts", "NRSfgp", "NRSfgz", "NRSinfo", "Nuts", "Pico", "Pigs",
   # "PolCPR", "PolNRS", "stip", "stiz")

# NRS Indices data
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
datCPRz <- planktonr::pr_get_Indices("CPR", "Z")
datCPRp <- planktonr::pr_get_Indices("CPR", "P")
datCPRw <- planktonr::pr_get_Indices("CPR", "W")

# FG time series data
NRSfgz <- planktonr::pr_get_FuncGroups("NRS", "Z")
NRSfgp <- planktonr::pr_get_FuncGroups("NRS", "P")
CPRfgz <- planktonr::pr_get_FuncGroups("CPR", "Z")
CPRfgp <- planktonr::pr_get_FuncGroups("CPR", "P")

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
NRSinfo <- planktonr::pr_get_PolicyInfo("NRS")
CPRinfo <- planktonr::pr_get_PolicyInfo("CPR")
PolLTM <- planktonr::pr_get_PolicyData("LTM")

# Species distribution data
fMapDataz <- planktonr::pr_get_FreqMap("Z")
fMapDatap <- planktonr::pr_get_FreqMap("P")

# Progress Map
PmapData <- planktonr::pr_get_ProgressMap(c("NRS", "CPR"))

# add data to sysdata.rda
usethis::use_data(Nuts, Pigs, fMapDataz, fMapDatap, Pico, LTnuts, 
                  PolNRS, PolCPR, PolLTM, NRSinfo, CPRinfo, 
                  datCPRz, datCPRp, datNRSz, datNRSp, datNRSm,
                  datNRSw, datCPRw,
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
                  stiz, stip, daynightz, daynightp,
                  overwrite = TRUE, internal = TRUE)

## files for SDMs (this will only work for Claire at the moment)
listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
file.copy(from=files, to="inst/app/www")

