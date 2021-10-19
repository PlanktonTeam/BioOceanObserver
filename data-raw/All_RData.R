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
fMapData <- planktonr::pr_get_fMap_data()

# add data to sysdata.rda
usethis::use_data(Nuts, Pigs, fMapData, Pico,
                  datCPRz, datCPRp, datNRSz, datNRSp, 
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp,
                  overwrite = TRUE, internal = TRUE)



## files for SDMs (this will only work for Claire at the moment)
listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
file.copy(from=files, to="inst/app/www")

