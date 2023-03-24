## script for all RData 
library(tidyverse)

## Update planktonr package
# remotes::install_github("PlanktonTeam/planktonr", force=FALSE, upgrade=TRUE)

# Set up colours for the app ----------------------------------------------

col12 <- RColorBrewer::brewer.pal(12, "Paired") %>% 
  stringr::str_replace("#FFFF99", "#000000") # Replace yellow with black


# Trip Data Information ---------------------------------------------------

datNRSTrip <- planktonr::pr_get_NRSTrips(Type = c("P", "Z")) %>% 
  dplyr::select(c("Year_Local", "Month_Local", "StationName"))

datCPRTrip <- planktonr::pr_get_CPRTrips() %>% 
  dplyr::select(c("Latitude", "Year_Local", "Month_Local", "Region", "TripCode"))

NRSStation <- planktonr::pr_get_NRSStation() %>% 
  dplyr::filter(ProjectName == "NRS") %>% 
  dplyr::select(-c("IMCRA", "IMCRA_PB", "ProjectName")) %>% 
  dplyr::arrange(desc(Latitude))


# NRS indices data --------------------------------------------------------

datNRSz <- planktonr::pr_get_Indices("NRS", "Z") 
datNRSp <- planktonr::pr_get_Indices("NRS", "P") 
datNRSm <- planktonr::pr_get_NRSMicro() ## microbial data

datNRSw <- planktonr::pr_get_Indices("NRS", "W") %>% #TODO move the MLD calcs to planktonr
  tidyr::pivot_wider(values_from = "Values", names_from = "Parameters") %>%
  dplyr::mutate(MLD_m = dplyr::case_when(.data$MLDtemp_m <= .data$MLDsal_m ~ .data$MLDtemp_m,
                                         .data$MLDsal_m < .data$MLDtemp_m ~ .data$MLDsal_m,
                                         TRUE ~ NA_real_)) %>%
  dplyr::select(-c(MLDtemp_m, MLDsal_m)) %>%
  tidyr::pivot_longer(-c("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "tz", "Latitude", "Longitude", "StationName", "StationCode"), 
                      names_to = "Parameters", values_to = "Values")


# CPR time series data ----------------------------------------------------

datCPRz <- planktonr::pr_get_Indices("CPR", "Z", near_dist_km = 250) %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None")) %>% 
  select(-c("Sample_ID", "tz")) %>% 
  droplevels()

datCPRp <- planktonr::pr_get_Indices("CPR", "P", near_dist_km = 250) %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None")) %>% 
  select(-c("Sample_ID", "tz")) %>% 
  droplevels()

PCI <- planktonr::pr_get_PCIData()

# FG time series data -----------------------------------------------------

NRSfgz <- planktonr::pr_get_FuncGroups("NRS", "Z")
NRSfgp <- planktonr::pr_get_FuncGroups("NRS", "P")

CPRfgz <- planktonr::pr_get_FuncGroups("CPR", "Z", near_dist_km = 250) %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None")) %>% 
  droplevels()

CPRfgp <- planktonr::pr_get_FuncGroups("CPR", "P", near_dist_km = 250) %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None")) %>% 
  droplevels()


# BGC Environmental variables data ----------------------------------------

Nuts <- planktonr::pr_get_NRSEnvContour('Chemistry') %>% 
  dplyr::filter(Parameters != "SecchiDepth_m") 
Pigs <- planktonr::pr_get_NRSPigments(Format = "binned") %>% 
  planktonr::pr_remove_outliers(2)
Pico <- planktonr::pr_get_NRSEnvContour('Pico')


# STI data ----------------------------------------------------------------

stizAll <- planktonr::pr_get_STIdata("Z") %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(totals = dplyr::n(),
                   .groups = 'drop') %>% 
  dplyr::filter(totals > 19)

stiz <- planktonr::pr_get_STIdata("Z") %>% 
  dplyr::filter(Species %in% stizAll$Species)

stipAll <- planktonr::pr_get_STIdata("P") %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(totals = dplyr::n(),
                   .groups = 'drop') %>% 
  dplyr::filter(totals > 19)

stip <- planktonr::pr_get_STIdata("P") %>% 
  dplyr::filter(Species %in% stipAll$Species)

rm(stizAll, stipAll)


# Day-Night data (from CPR only) ------------------------------------------

daynightzAll <- planktonr::pr_get_DayNight("Z") %>% 
  dplyr::filter(Species_m3 > 0) %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(count = dplyr::n(),
                   .groups = 'drop') %>% 
  dplyr::filter(count > 10)

daynightz <- planktonr::pr_get_DayNight("Z") %>% 
  dplyr::filter(Species %in% daynightzAll$Species)

daynightpAll <- planktonr::pr_get_DayNight("P") %>% 
  dplyr::filter(Species_m3 > 0) %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(count = dplyr::n(),
                   .groups = 'drop') %>% 
  dplyr::filter(count > 10)

daynightp <- planktonr::pr_get_DayNight("P") %>% 
  dplyr::filter(Species %in% daynightpAll$Species)

rm(daynightzAll, daynightpAll)

# Policy data -------------------------------------------------------------

PolNRS <- planktonr::pr_get_EOVs("NRS") %>% 
  dplyr::filter(!StationCode %in% c("NIN", "ESP")) %>% 
  planktonr::pr_remove_outliers(2)
PolCPR <- planktonr::pr_get_EOVs("CPR", near_dist_km = 250) %>% 
  planktonr::pr_remove_outliers(2) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None"))
PolLTM <- planktonr::pr_get_EOVs("LTM") %>% 
  planktonr::pr_remove_outliers(2)

NRSinfo <- planktonr::pr_get_PolicyInfo("NRS")
CPRinfo <- planktonr::pr_get_PolicyInfo("CPR")

# Species distribution data
fMapDataz <- planktonr::pr_get_FreqMap("Z") %>% 
  dplyr::select(-c('samples', 'freq', 'freqsamp'))
fMapDatap <- planktonr::pr_get_FreqMap("P") %>% 
  dplyr::select(-c('samples', 'freq', 'freqsamp'))

# Progress Map ------------------------------------------------------------

PM <- planktonr::pr_get_ProgressMapData(c("CPR", "NRS"), interactive = TRUE, near_dist_km = 250)

PM_NRS <- PM %>% 
  dplyr::filter(Survey == "NRS") %>% 
  dplyr::group_by(.data$Name) %>% 
  dplyr::summarise(Start_Date = as.Date(min(.data$SampleTime_Local)),
                   End_Date = as.Date(max(.data$SampleTime_Local)),
                   Latitude = dplyr::first(Latitude),
                   Longitude = dplyr::first(Longitude),
                   Samples = dplyr::n(),
                   .groups = "drop")

PM_CPR <- PM %>% 
  dplyr::filter(Survey == "CPR") %>% 
  dplyr::mutate(Latitude = round(Latitude, digits = 2),
                Longitude = round(Longitude, digits = 2),
                Month_Local = lubridate::month(SampleTime_Local)) %>%
  dplyr::distinct(Latitude, Longitude, SampleTime_Local, .keep_all = TRUE) %>% # For BOO for the moment, no labels and distinct data
  dplyr::arrange(.data$TripCode, .data$SampleTime_Local) %>% 
  dplyr::select(-c("TripCode", "SampleTime_Local", "Sample_ID"))

PM_CPR <- PM_CPR[seq(2, nrow(PM_CPR), 2),] # Only keep every 3rd row for the moment

PMapData <- list(NRS = PM_NRS, CPR = PM_CPR)

rm(PM, PM_NRS, PM_CPR)

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
    dplyr::rename(DOY = "TIME") %>%
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



# Get Species Info for each Taxa ------------------------------------------

SpInfoP <- planktonr::pr_get_SpeciesInfo(Type = "P")
SpInfoZ <- planktonr::pr_get_SpeciesInfo(Type = "Z")


# Get Larval Fish Data ----------------------------------------------------

temp <- planktonr::pr_get_LFData() %>% 
  dplyr::select("Species" = "Species2", "Project", "Latitude", "Longitude", "SampleTime_Local",
                "Month_Local", "SampleDepth_m", "Count", "Abundance_1000m3", "Temperature_degC", 
                "Salinity_psu", "Volume_m3", "Vessel", "TowType", "GearMesh_um", "Bathymetry_m") %>% 
  dplyr::filter(!.data$Species %in% c("Superclass: Pisces (37000000)"))

LFDataAbs <- temp %>% 
  dplyr::distinct(Latitude, Longitude, SampleTime_Local, .keep_all = TRUE)

LFData <- temp %>% 
  dplyr::filter(.data$Count > 0) %>% 
  dplyr::arrange(.data$Species)

rm(temp)


# Get Taxa Accumulation Info ----------------------------------------------

PSpNRSAccum <- planktonr::pr_get_TaxaAccum(Survey = "NRS", Type = "P")
PSpCPRAccum <- planktonr::pr_get_TaxaAccum(Survey = "CPR", Type = "P")
ZSpNRSAccum <- planktonr::pr_get_TaxaAccum(Survey = "NRS", Type = "Z")
ZSpCPRAccum <- planktonr::pr_get_TaxaAccum(Survey = "CPR", Type = "Z")

# Parameter Definitions
ParamDef <- readr::read_csv(file.path("data-raw", "ParameterDefn.csv"), na = character())

# Save data to sysdata.rda archive ----------------------------------------

system("mkdir -p data/sysdata-archive")
save(Nuts, Pigs, Pico, 
     fMapDataz, fMapDatap, 
     MooringTS, MooringClim,
     PolNRS, PolCPR, PolLTM, 
     NRSinfo, CPRinfo, NRSStation,
     datCPRz, datCPRp, PCI,
     datNRSz, datNRSp, datNRSm, datNRSw,
     NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
     stiz, stip, daynightz, daynightp,
     SpInfoP, SpInfoZ, LFData, LFDataAbs,
     datNRSTrip, datCPRTrip,
     PSpNRSAccum, PSpCPRAccum, ZSpNRSAccum, ZSpCPRAccum,
     ParamDef, col12, file = paste0("data/sysdata-archive/sysdata-",format(Sys.time(), "%Y%M%d"),".rda"))





