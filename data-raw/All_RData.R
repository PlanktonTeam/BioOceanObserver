## script for all RData 
library(planktonr)
library(tidyverse)

# Trip Data
datNRSTrip <- planktonr::pr_get_NRSTrips(Type = c("P", "Z"))
datCPRTrip <- planktonr::pr_get_CPRTrips()
NRSStation <- planktonr::pr_get_NRSStation() %>% 
  dplyr::filter(ProjectName == 'NRS') %>% 
  dplyr::select(-c('IMCRA', 'IMCRA_PB', 'ProjectName')) %>% 
  dplyr::arrange(desc(Latitude))

# NRS indices data
datNRSz <- planktonr::pr_get_Indices("NRS", "Z") 
datNRSp <- planktonr::pr_get_Indices("NRS", "P") 
datNRSm <- planktonr::pr_get_NRSMicro() ## microbial data

datNRSw <- planktonr::pr_get_Indices("NRS", "W") %>%
  tidyr::pivot_wider(values_from = "Values", names_from = "Parameters") %>%
  dplyr::mutate(MLD_m = dplyr::case_when(.data$MLDtemp_m <= .data$MLDsal_m ~ .data$MLDtemp_m,
                                         .data$MLDsal_m < .data$MLDtemp_m ~ .data$MLDsal_m,
                                         TRUE ~ NA_real_)) %>%
  dplyr::select(-c(MLDtemp_m, MLDsal_m)) %>%
  tidyr::pivot_longer(-c("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "tz", "Latitude", "Longitude", "StationName", "StationCode"), 
                      names_to = "Parameters", values_to = "Values")

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
Nuts <- planktonr::pr_get_NRSChemistry() %>% 
  dplyr::filter(Parameters != "SecchiDepth_m") %>% 
  planktonr::pr_remove_outliers(2)
Pigs <- planktonr::pr_get_NRSPigments(Format = "binned") %>% planktonr::pr_remove_outliers(2)
Pico <- planktonr::pr_get_NRSPico() %>% planktonr::pr_remove_outliers(2)
LTnuts <- planktonr::pr_get_LTnuts() %>% planktonr::pr_remove_outliers(2)

# get Sat data
# These can take a long time to run depending on the number of locations
# These do not need to be available to the APP but should be in the DAP collection and planktonr
# #TODO add function to remove the data that already has products matched.
# res_spat <- 10
# SSTsat <- planktonr::pr_get_DataLocs("NRS") %>% planktonr::pr_match_GHRSST(pr = "sea_surface_temperature")
# ALTsat <- planktonr::pr_get_DataLocs("NRS") %>% planktonr::pr_match_Altimetry(pr = "GSLA") 
# CHLsat <- planktonr::pr_get_DataLocs("NRS") %>% planktonr::pr_match_MODIS(pr = "chl_oc3")

# SatData <- SSTsat %>%
#   dplyr::left_join(ALTsat, by = c("Latitude", "Longitude", "Year", "Month", "Day")) %>%
#   dplyr::left_join(CHLsat, by = c("Latitude", "Longitude", "Year", "Month", "Day"))

# readr::write_csv(SatData, "SatDataNRS.csv")

# STI data
stiz <- planktonr::pr_get_STIdata("Z")
stip <- planktonr::pr_get_STIdata("P")

# Day-Night data (from CPR only)
daynightz <- planktonr::pr_get_DayNight("Z")
daynightp <- planktonr::pr_get_DayNight("P")

# Policy data
PolNRS <- planktonr::pr_get_PolicyData("NRS") %>% 
  dplyr::filter(!StationCode %in% c("NIN", "ESP")) %>% 
  planktonr::pr_remove_outliers(2)
PolCPR <- planktonr::pr_get_PolicyData("CPR", join = "st_nearest_feature") %>% planktonr::pr_remove_outliers(2)
PolLTM <- planktonr::pr_get_PolicyData("LTM") %>% planktonr::pr_remove_outliers(2)

NRSinfo <- planktonr::pr_get_PolicyInfo("NRS")
CPRinfo <- planktonr::pr_get_PolicyInfo("CPR")

# Species distribution data
fMapDataz <- planktonr::pr_get_FreqMap("Z")
fMapDatap <- planktonr::pr_get_FreqMap("P")

legdat <- data.frame(
  text = c("Absent", "Seen in 25%", "50%", "75%", "100 % of Samples","Absent", "Seen in 25%", "50%", "75%", "100 % of Samples"),
  colnames = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
  size = c(2,5,5,5,5,1,5,5,5,5),
  yt = c(1.2,1.2,1.2,1.2,1.2),
  xt = c(1,2,3,4,5),
  x = c(1,2,3,4,5,1,2,3,4,5),
  y = c(1.1,1.1,1.1,1.1,1.1,1,1,1,1,1)
)

legendPlot <- ggplot2::ggplot() +
  ggplot2::geom_point(data = legdat, ggplot2::aes(x, y, size = size, colour = colnames)) +
  ggplot2::scale_color_manual(values = c("lightblue1", "skyblue3", "dodgerblue2", "blue1", "navyblue", "#CCFFCC", "#99FF99", "#669933", "#009900", "#006600")) + 
  ggplot2::geom_text(data = legdat, ggplot2::aes(x = xt, y = yt, label = text)) +
  ggplot2::geom_text(ggplot2::aes(x = c(5.5,5.5), y = c(1.1,1), label = c("CPR", "NRS"))) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none")


## Progress Map
PMapData <- planktonr::pr_get_ProgressMapData(c("NRS", "CPR"))
PMapData2 <- planktonr::pr_get_ProgressMapData(c("NRS", "CPR"), interactive = TRUE) #%>% dplyr::filter(Survey == "CPR" & Name == "Southern Ocean Region")

## Using the Long Time Series Moorings Products
## NRS climatologies

#TODO when these are accessible from AODN, make into a real planktonr function and change file path
pr_get_mooringTS <- function(Stations, Depth, Names){
  if(Stations == "PHB"){
    file <- tidync::tidync(file.path("data-raw/LMTS", "PH100_CLIM.nc"))
  } else {
    file <- tidync::tidync(file.path("data-raw/LMTS", paste0("NRS", Stations, "_CLIM.nc")))
  }
  
  out <- file %>%
    tidync::hyper_filter(DEPTH = DEPTH == Depth, 
                         TIME = index < 366) %>% # ignoring leap years
    tidync::hyper_tibble() %>%
    dplyr::rename(DOY = .data$TIME) %>%
    dplyr::mutate(StationCode = Stations, 
                  Names = Names)
  
}

Stations <- planktonr::pr_get_NRSStation() %>%
  dplyr::select('StationCode') %>%
  dplyr::filter(!.data$StationCode %in% c("NIN", "ESP", "PH4"))
Stations <- rep(Stations$StationCode, 3)

Depths <- c(rep(0, 7), 
            32, 24, 20, 8, 10, 20, 28, # mean MLD
            80, 100, 50, 20, 20, 60, 100) # max depth of sampling

Names <- c(rep("Surface", 7),
           rep("MLD", 7),
           rep("Bottom", 7))

MooringTS <- purrr::pmap_dfr(list(Stations, Depths, Names), pr_get_mooringTS) %>%
  planktonr::pr_add_StationName() %>%
  planktonr::pr_reorder()

pr_get_mooringClim <- function(Stations){
  if(Stations == "PHB"){
    file <- tidync::tidync(file.path("data-raw/LMTS", "PH100_CLIM.nc"))
  } else {
    file <- tidync::tidync(file.path("data-raw/LMTS", paste0("NRS", Stations, "_CLIM.nc")))
  }
  
  df <- tidync::hyper_tibble(file) %>%
    dplyr::mutate(StationCode = Stations)
}  

MooringClim <- purrr::map_dfr(Stations, pr_get_mooringClim) %>%
  planktonr::pr_add_StationName() %>%
  planktonr::pr_reorder()


# Get Species Info for each Taxa
SpInfoP <- planktonr::pr_get_SpeciesInfo(Type = "P")
SpInfoZ <- planktonr::pr_get_SpeciesInfo(Type = "Z")

# Get Larval Fish Data 
LFData <- planktonr::pr_get_LFData()


# Get Taxa Accumulation Info
PSpNRSAccum <- planktonr::pr_get_TaxaAccum(Survey = "NRS", Type = "P")
PSpCPRAccum <- planktonr::pr_get_TaxaAccum(Survey = "CPR", Type = "Z")
ZSpNRSAccum <- planktonr::pr_get_TaxaAccum(Survey = "NRS", Type = "Z")
ZSpCPRAccum <- planktonr::pr_get_TaxaAccum(Survey = "CPR", Type = "Z")


# add data to sysdata.rda
usethis::use_data(Nuts, Pigs, Pico, LTnuts,
                  fMapDataz, fMapDatap, legendPlot,
                  MooringTS, MooringClim,
                  PolNRS, PolCPR, PolLTM, NRSinfo, CPRinfo, NRSStation,
                  datCPRz, datCPRp, datCPRw,
                  datNRSz, datNRSp, datNRSm, datNRSw,
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
                  stiz, stip, daynightz, daynightp, PMapData2,
                  SpInfoP, SpInfoZ, LFData, datNRSTrip, datCPRTrip,
                  PSpNRSAccum, PSpCPRAccum, ZSpNRSAccum, ZSpCPRAccum,
                  overwrite = TRUE, internal = TRUE)

save(Nuts, Pigs, Pico, LTnuts, 
     fMapDataz, fMapDatap, legendPlot,
     MooringTS, MooringClim,
     PolNRS, PolCPR, PolLTM, NRSinfo, CPRinfo, NRSStation,
     datCPRz, datCPRp, datCPRw,
     datNRSz, datNRSp, datNRSm, datNRSw,
     NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
     stiz, stip, daynightz, daynightp, PMapData2, 
     SpInfoP, SpInfoZ, datNRSTrip, datCPRTrip,
     PSpNRSAccum, PSpCPRAccum, ZSpNRSAccum, ZSpCPRAccum,
     file = "data/sysdata.rda")

# Write to csv to save onto the DAP
# write_csv(Nuts, "Nuts.csv")
# write_csv(Pigs,  "Pigs.csv")
# write_csv(Pico,  "Pico.csv")
# write_csv(LTnuts,    "LTnuts.csv") 
# write_csv(fMapDataz,  "fMapDataz.csv")
# write_csv(fMapDatap,  "fMapDatap.csv")
# write_csv(legendPlot, "legendPlot.csv")
# write_csv(MooringTS,  "MooringTS.csv")
# write_csv(MooringClim, "MooringClim.csv")
# write_csv(PolNRS, "PolNRS.csv")
# write_csv(PolCPR,  "PolCPR.csv")
# write_csv(PolLTM,  "PolLTM.csv")
# write_csv(NRSinfo,  "NRSinfo.csv")
# write_csv(CPRinfo,  "CPRinfo.csv")
# write_csv(datCPRz,  "datCPRz.csv")
# write_csv(datCPRp,  "datCPRp.csv")
# write_csv(datCPRw, "datCPRw.csv") 
# write_csv(datNRSz,  "datNRSz.csv")
# write_csv(datNRSp,  "datNRSp.csv")
# write_csv(datNRSm,  "datNRSm.csv")
# write_csv(datNRSw, "datNRSw.csv")
# write_csv(NRSfgz,  "NRSfgz.csv")
# write_csv(NRSfgp,  "NRSfgp.csv")
# write_csv(CPRfgz,  "CPRfgz.csv")
# write_csv(CPRfgp,  "CPRfgp.csv")
# write_csv(PMapData, "PMapData.csv")
# write_csv(stiz,  "stiz.csv")
# write_csv(stip,  "stip.csv")
# write_csv(daynightz,  "daynightz.csv")
# write_csv(daynightp,  "daynightp.csv")
# write_csv(PMapData2, "PMapData2.csv")
# write_csv(SatData, "SatData.csv")





