## script for all RData 
## Aim to change all source calls to planktonr functions

# NRS timeseries data
datNRSz <- planktonr::pr_get_tsdata("NRS", "Z")
datNRSp <- planktonr::pr_get_tsdata("NRS", "P")

# CPR timeseries data
datCPRz <- planktonr::pr_get_tsdata("CPR", "Z")
datCPRp <- planktonr::pr_get_tsdata("CPR", "P")

# BGC Environmental variables data
Nuts <- planktonr::pr_get_nuts()
Pigs <- planktonr::pr_get_pigs()

# Species distribution data
source("EnvDataBGC_RData.R")

# add data to sysdata.rda
usethis::use_data(Nuts, Pigs, obs, Samples, SampLocs, absences, 
                  datCPRz, datCPRp, datNRSz, datNRSp, MapOz, 
                  overwrite = TRUE, internal = TRUE)
