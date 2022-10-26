## script for all RData 
library(tidyverse)
library(planktonr)

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
Nuts <- planktonr::pr_get_NRSChemistry() %>% 
  dplyr::filter(Parameters != "SecchiDepth_m") %>% 
  planktonr::pr_remove_outliers(2)
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
PolNRS <- planktonr::pr_get_PolicyData("NRS") %>% 
  dplyr::filter(!StationCode %in% c('NIN', 'ESP')) %>% 
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
  colnames = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'),
  size = c(2,5,5,5,5,1,5,5,5,5),
  yt = c(1.2,1.2,1.2,1.2,1.2),
  xt = c(1,2,3,4,5),
  x = c(1,2,3,4,5,1,2,3,4,5),
  y = c(1.1,1.1,1.1,1.1,1.1,1,1,1,1,1)
)

legendPlot <- ggplot2::ggplot() +
  ggplot2::geom_point(data = legdat, ggplot2::aes(x, y, size = size, colour = colnames)) +
  ggplot2::scale_color_manual(values = c("lightblue1", "skyblue3", "dodgerblue2", "blue1", "navyblue", "#CCFFCC", "#99FF99", "#669933", "#009900", "#006600")) + 
  ggplot2::geom_text(data = legdat, ggplot2::aes(x= xt, y= yt, label = text)) +
  ggplot2::geom_text(ggplot2::aes(x= c(5.5,5.5), y= c(1.1,1), label = c("NRS", "CPR"))) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = 'none')
legendPlot

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

## Using the Long Time Series Moorings Products
## NRS climatologies

library(tidync)
library(planktonr)

#TODO when these are accessible from AODN, make into a real planktonr function and change file path
pr_get_mooringTS <- function(Stations, Depth, Names){
  if(Stations == 'PHB'){
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
  dplyr::select(.data$StationCode) %>%
  dplyr::filter(!.data$StationCode %in% c('NIN', 'ESP', 'PH4'))
Stations <- rep(Stations$StationCode, 3)

Depths <- c(rep(1, 7), 
            32, 24, 21, 8, 11, 20, 28, # mean MLD
            80, 100, 50, 20, 20, 60, 100) # max depth of sampling

Names <- c(rep('Surface', 7),
           rep('MLD', 7),
           rep('Bottom', 7))

MooringTS <- purrr::pmap_dfr(list(Stations, Depths, Names), pr_get_mooringTS) %>%
  planktonr::pr_add_StationName() %>%
  planktonr::pr_reorder()

pr_get_mooringClim <- function(Stations){
  if(Stations == 'PHB'){
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

# # add data to sysdata.rda
# usethis::use_data(Nuts, Pigs, Pico, LTnuts, 
#                   fMapDataz, fMapDatap, legendPlot,
#                   MooringTS, MooringClim,
#                   PolNRS, PolCPR, PolLTM, NRSinfo, CPRinfo, 
#                   datCPRz, datCPRp, datCPRw,
#                   datNRSz, datNRSp, datNRSm, datNRSw,
#                   NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
#                   stiz, stip, daynightz, daynightp, PMapData2,
#                   overwrite = TRUE, internal = TRUE)

# Save data to ./data/sysdata.rda
if (!file.exists("./data")) dir.create(file.path("./", "data"))
save(Nuts, Pigs, Pico, LTnuts, 
     fMapDataz, fMapDatap, legendPlot,
     MooringTS, MooringClim,
     PolNRS, PolCPR, PolLTM, NRSinfo, CPRinfo,
     datCPRz, datCPRp, datCPRw,
     datNRSz, datNRSp, datNRSm, datNRSw,
     NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
     stiz, stip, daynightz, daynightp, PMapData2,
     file='sysdata.rda')

## files for SDMs (this will only work for Claire at the moment)
# listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
# files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
# file.copy(from=files, to="inst/app/www")







  