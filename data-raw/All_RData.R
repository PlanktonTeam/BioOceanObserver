## script for all RData 
## Aim to change all source calls to planktonr functions
# library(planktonr)


# load("~/GitHub/IMOS_BioOceanObserver/R/sysdata.rda")

# rm("CPRfgp", "CPRfgz", "CPRinfo", "datCPRp", "datCPRz",
   # "datNRSm", "datNRSp", "datNRSz", "daynightp", "daynightz",
   # "LTnuts", "NRSfgp", "NRSfgz", "NRSinfo", "Nuts", "Pico", "Pigs",
   # "PolCPR", "PolNRS", "stip", "stiz")

# NRS indices data
datNRSz <- planktonr::pr_get_Indices("NRS", "Z")
datNRSp <- planktonr::pr_get_Indices("NRS", "P")
datNRSm <- planktonr::pr_get_NRSMicro() ## microbial data

# CPR time series data
datCPRz <- planktonr::pr_get_Indices("CPR", "Z")
datCPRp <- planktonr::pr_get_Indices("CPR", "P")

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
                  datCPRz, datCPRp, datNRSz, datNRSp, datNRSm,
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
                  stiz, stip, daynightz, daynightp,
                  overwrite = TRUE, internal = TRUE)

## files for SDMs (this will only work for Claire at the moment)
listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
file.copy(from=files, to="inst/app/www")

