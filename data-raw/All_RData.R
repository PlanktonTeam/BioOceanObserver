## script for all RData 
library(tidyverse)

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
datNRSm <- planktonr::pr_get_NRSMicro("NRS") %>%  ## NRS microbial data
  tidyr::drop_na() 
Tricho <- planktonr::pr_get_NRSData(Type = 'Phytoplankton', Variable = "abundance", Subset = "genus") %>% 
  dplyr::select(dplyr::any_of(colnames(datNRSm)), Values = "Trichodesmium")  %>% 
  dplyr::filter(!.data$StationCode %in% c("NWS", "SOTS_RAS", "NA"))%>% 
  dplyr::mutate(Parameters = "Trichodesmium") %>% 
  planktonr::pr_reorder()
datNRSm <- datNRSm %>% dplyr::bind_rows(Tricho)
rm(Tricho)

datCSm  <- planktonr::pr_get_NRSMicro("Coastal") %>% ## coastal microbial data
  droplevels() %>% 
  dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10,0)*10,
                SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'day'))

datGSm <- planktonr::pr_get_NRSMicro("GO-SHIP")

datNRSw <- planktonr::pr_get_Indices("NRS", "W") %>% #TODO move the MLD calcs to planktonr
  tidyr::pivot_wider(values_from = "Values", names_from = "Parameters") %>%
  dplyr::mutate(MLD_m = dplyr::case_when(.data$MLDtemp_m <= .data$MLDsal_m ~ .data$MLDtemp_m,
                                         .data$MLDsal_m < .data$MLDtemp_m ~ .data$MLDsal_m,
                                         TRUE ~ NA_real_)) %>%
  dplyr::select(-c("MLDtemp_m", "MLDsal_m", "Latitude", "Longitude", "tz")) %>%
  tidyr::pivot_longer(-c("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "StationName", "StationCode"), 
                      names_to = "Parameters", values_to = "Values") %>%
  dplyr::filter(Values > 0, 
                !(Values == 5.964 & StationCode == 'YON')) %>%   
  planktonr::pr_remove_outliers(2) 

# CPR time series data ----------------------------------------------------

datCPRz <- planktonr::pr_get_Indices("CPR", "Z", near_dist_km = 250) %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None")) %>% 
  select(-c("Sample_ID", "tz")) %>% 
  planktonr::pr_remove_outliers(2) %>% 
  droplevels()

datCPRp <- planktonr::pr_get_Indices("CPR", "P", near_dist_km = 250) %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None")) %>% 
  select(-c("Sample_ID", "tz")) %>% 
  planktonr::pr_remove_outliers(2) %>% 
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

ctd <- planktonr::pr_get_NRSCTD() %>% 
  dplyr::select(-c("Project", "file_id", "tz", "SampleTime_UTC", "Latitude", "Longitude"), 
                CTD_Salinity = "Salinity_psu", CTD_Temperature_degC = "Temperature_degC") %>% 
  dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m,0)) %>% 
  dplyr::filter(SampleDepth_m %in% datNRSm$SampleDepth_m) %>% 
  tidyr::pivot_longer(-c(dplyr::any_of(colnames(datNRSm))), values_to = "Values", names_to = "Parameters") 

CSChem <- planktonr::pr_get_CSChem() %>% dplyr::filter(Parameters %in% c("Chla_mgm3", "Temperature_degC",
                                                                         "Salinity_psu")) %>% 
  dplyr::mutate(Parameters = ifelse(Parameters == "Salinity_psu", "Salinity", Parameters),
                SampleDepth_m = round(.data$SampleDepth_m/10,0)*10,
                SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'day'))

# Get Sat data ------------------------------------------------------------

# These can take a long time to run depending on the number of locations
# These do not need to be available to the APP but should be in the DAP collection and planktonr
# #TODO add function to remove the data that already has products matched.
# 
# SSTsat <- planktonr::pr_get_DataLocs("NRS") %>% planktonr::pr_match_GHRSST(pr = "sea_surface_temperature", res_spat = 10)
# ALTsat <- planktonr::pr_get_DataLocs("NRS") %>% planktonr::pr_match_Altimetry(pr = "GSLA", res_spat = 10) 
# CHLsat <- planktonr::pr_get_DataLocs("NRS") %>% planktonr::pr_match_MODIS(pr = "chl_oc3", res_spat = 10)

# SatData <- SSTsat %>%
#   dplyr::left_join(ALTsat, by = c("Latitude", "Longitude", "Year", "Month", "Day")) %>%
#   dplyr::left_join(CHLsat, by = c("Latitude", "Longitude", "Year", "Month", "Day"))

# readr::write_csv(SatData, file.path("data-raw","SatDataNRS.csv"))


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
  dplyr::filter(count > 14)

daynightz <- planktonr::pr_get_DayNight("Z") %>% 
  dplyr::filter(Species %in% daynightzAll$Species)

daynightpAll <- planktonr::pr_get_DayNight("P") %>% 
  dplyr::filter(Species_m3 > 0) %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(count = dplyr::n(),
                   .groups = 'drop') %>% 
  dplyr::filter(count > 14)

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

# # To test sysdata access from DAP
# PolNRS <- PolNRS[PolNRS$Year_Local < 2017,]
# PolCPR <- PolCPR[PolCPR$Year_Local < 2017,] 
# PolLTM <- PolLTM[PolLTM$Year_Local < 2017,]

# Add data to sysdata.rda -------------------------------------------------
usethis::use_data(Nuts, Pigs, Pico, ctd, CSChem,
                  fMapDataz, fMapDatap, 
                  MooringTS, MooringClim,
                  PolNRS, PolCPR, PolLTM, 
                  NRSinfo, CPRinfo, NRSStation,
                  datCPRz, datCPRp, PCI,
                  datNRSz, datNRSp,  datNRSw,
                  datNRSm, datCSm, datGSm,
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
                  stiz, stip, daynightz, daynightp,
                  SpInfoP, SpInfoZ, LFData, LFDataAbs,
                  datNRSTrip, datCPRTrip,
                  PSpNRSAccum, PSpCPRAccum, ZSpNRSAccum, ZSpCPRAccum,
                  ParamDef, col12, overwrite = TRUE, internal = TRUE)





