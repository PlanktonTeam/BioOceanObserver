## script for all RData 
library(tidyverse)
library(planktonr)

modified_time <- lubridate::now()

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

datNRSz <- planktonr::pr_get_Indices(Survey = "NRS", Type = "Zooplankton") 

datNRSp <- planktonr::pr_get_Indices(Survey = "NRS", Type = "Phytoplankton") 

trophy <- planktonr::pr_get_PlanktonInfo(Type = 'Phytoplankton') %>% # for information used in estimating Indices as per NRS data
  dplyr::select(TaxonName = 'Taxon Name', Trophy = 'Functional Type', Carbon = 'Cell Carbon (pgN cell-1)', CellBioV = 'Cell BioVolume (um3)',
                FunctionalGroup = 'Functional Group') %>% 
  dplyr::mutate(genus = dplyr::case_when(stringr::word(TaxonName, 1) == 'cf.' ~ stringr::word(TaxonName, 2),
                                         TRUE ~ stringr::word(TaxonName, 1)),
                species = dplyr::case_when(stringr::word(TaxonName, 2) == 'cf.' ~ 'spp.',
                                           stringr::word(TaxonName, 1) == 'cf.' ~ 'spp.',
                                           grepl("\\(|\\/|diatom|dino|\\-|group", TaxonName) ~ 'spp.',
                                           TRUE ~ stringr::word(TaxonName, 2)))

main_vars <- c("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "tz", "Latitude", "Longitude", "StationName", "StationCode", "Method")

# SOTS Indices not available from AODN so calculated here 
#TODO - add a function to planktonr to get this data

SOTSp <- planktonr::pr_get_NRSData(Type = 'phytoplankton', Variable = 'abundance', Subset = 'raw') %>% 
  dplyr::filter(grepl('SOTS', StationCode),
                Method == 'LM') %>% # only use LM at this stage
  tidyr::pivot_longer(-c(Project:Method), names_to = 'TaxonName', values_to = 'abund') %>% 
  dplyr::select(StationName:Month_Local, SampleDepth_m, TaxonName, abund) %>% 
  dplyr::left_join(trophy, by = 'TaxonName') %>% 
  dplyr::filter(abund > 0#,
                #!FunctionalGroup %in% c('Other', 'Silicoflagellate', "Foraminifera", "Ciliate", "Radiozoa")
  ) %>%
  dplyr::mutate(TaxonName = paste0(genus, " ", species),
                tz = "Australia/Hobart",
                StationName = 'Southern Ocean Time Series',
                StationCode = 'SOTS') %>% 
  dplyr::group_by(across(tidyselect::any_of(main_vars)), TaxonName, FunctionalGroup, CellBioV, Carbon) %>% 
  dplyr::summarise(abund = sum(abund, na.rm = TRUE), ## add up all occurrences of spp. within one sample
                   .groups = 'drop') %>% 
  dplyr::group_by(across(tidyselect::any_of(main_vars))) %>% 
  dplyr::summarise(AvgCellVol_um3 = mean(CellBioV*abund/sum(abund), na.rm = TRUE),
                   DiatomDinoflagellateRatio = sum(abund[grepl('iatom', FunctionalGroup)])/sum(abund[grepl('iatom|Dinof', FunctionalGroup)]),
                   NoPhytoSpecies_Sample = length(abund[!grepl('NA|spp', TaxonName)]),
                   NoDiatomSpecies_Sample = length(abund[grepl('iatom', FunctionalGroup) & !grepl('NA|spp', TaxonName)]),
                   NoDinoSpecies_Sample = length(abund[FunctionalGroup == 'Dinoflagellate' & !grepl('NA|spp', TaxonName)]),
                   PhytoAbundance_CellsL = sum(abund, na.rm = TRUE),
                   PhytoBiomassCarbon_pgL = sum(abund * Carbon, na.rm = TRUE),
                   ShannonPhytoDiversity = vegan::diversity(abund[!grepl('NA|spp', TaxonName)], index = 'shannon'), 
                   ShannonDiatomDiversity = vegan::diversity(abund[grepl('iatom', FunctionalGroup) & !grepl('NA|spp', TaxonName)], index = 'shannon'), 
                   ShannonDinoDiversity = vegan::diversity(abund[grepl('Dinof', FunctionalGroup) & !grepl('NA|spp', TaxonName)], index = 'shannon'), 
                   PhytoEvenness = ShannonPhytoDiversity/log10(NoPhytoSpecies_Sample),
                   DiatomEvenness = ShannonDiatomDiversity/log10(NoDiatomSpecies_Sample),
                   DinoflagellateEvenness = ShannonDinoDiversity/log10(NoDinoSpecies_Sample),
                   .groups = 'drop') %>% 
  tidyr::pivot_longer(-tidyselect::any_of(main_vars), values_to = 'Values', names_to = 'Parameters') %>% 
  planktonr::pr_remove_outliers(2) %>% 
  droplevels() %>% 
  planktonr::planktonr_dat("Phytoplankton", 'SOTS')

rm( main_vars, trophy)

datNRSm <- planktonr::pr_get_NRSMicro(Survey = "NRS") %>% 
  tidyr::drop_na() ## NRS microbial data

Tricho <- planktonr::pr_get_NRSData(Type = 'Phytoplankton', Variable = "abundance", Subset = "genus") %>% 
  dplyr::select(dplyr::any_of(colnames(datNRSm)), Values = "Trichodesmium")  %>% 
  dplyr::filter(!.data$StationCode %in% c("NWS", "SOTS_RAS", "NA"))%>% 
  dplyr::mutate(Parameters = "Trichodesmium") %>% 
  planktonr::pr_reorder()
datNRSm <- datNRSm %>% dplyr::bind_rows(Tricho)
rm(Tricho)

datCSm  <- planktonr::pr_get_NRSMicro(Survey = "Coastal") %>% ## coastal microbial data
  droplevels() %>% 
  dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10,0)*10,
                SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = "day")) %>% 
  dplyr::filter(Values != -9999) %>% 
  tidyr::drop_na(Values)

datGSm <- planktonr::pr_get_NRSMicro(Survey = "GO-SHIP")

datNRSw <- planktonr::pr_get_Indices(Survey = "NRS", Type = "Water") %>% #TODO move the MLD calcs to planktonr
  tidyr::pivot_wider(values_from = "Values", names_from = "Parameters") %>%
  dplyr::mutate(MLD_m = dplyr::case_when(.data$MLDtemp_m <= .data$MLDsal_m ~ .data$MLDtemp_m,
                                         .data$MLDsal_m < .data$MLDtemp_m ~ .data$MLDsal_m,
                                         TRUE ~ NA_real_)) %>%
  dplyr::select(-c("MLDtemp_m", "MLDsal_m", "Latitude", "Longitude", "tz")) %>%
  tidyr::pivot_longer(-c("TripCode", "Year_Local", "Month_Local", "SampleTime_Local", "StationName", "StationCode"), 
                      names_to = "Parameters", values_to = "Values") %>%
  dplyr::filter(Values > 0, 
                !(Values == 5.964 & StationCode == "YON")) %>%   
  planktonr::pr_remove_outliers(2) 

# CPR time series data ----------------------------------------------------

datCPRz <- planktonr::pr_get_Indices(Survey = "CPR", Type = "Zooplankton", near_dist_km = 250) %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None")) %>% 
  select(-c("Sample_ID", "tz")) %>% 
  planktonr::pr_remove_outliers(2) %>% 
  droplevels()

datCPRp <- planktonr::pr_get_Indices(Survey = "CPR", Type = "Phytoplankton", near_dist_km = 250) %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None")) %>% 
  select(-c("Sample_ID", "tz")) %>% 
  planktonr::pr_remove_outliers(2) %>% 
  droplevels()

PCI <- planktonr::pr_get_PCIData()

# FG time series data -----------------------------------------------------

NRSfgz <- planktonr::pr_get_FuncGroups(Survey = "NRS", Type = "Zooplankton")
NRSfgp <- planktonr::pr_get_FuncGroups(Survey = "NRS", Type = "Phytoplankton")
SOTSfgp <- planktonr::pr_get_FuncGroups(Survey = "SOTS", Type = "Phytoplankton")

CPRfgz <- planktonr::pr_get_FuncGroups(Survey = "CPR", Type = "Zooplankton", near_dist_km = 250) %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None")) %>% 
  droplevels()

CPRfgp <- planktonr::pr_get_FuncGroups(Survey = "CPR", Type = "Phytoplankton", near_dist_km = 250) %>% 
  tidyr::drop_na(BioRegion) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None")) %>% 
  droplevels()

# BGC Environmental variables data ----------------------------------------

Nuts <- planktonr::pr_get_NRSEnvContour('Chemistry') %>% 
  dplyr::filter(.data$Parameters != "SecchiDepth_m") 
Pigs <- planktonr::pr_get_NRSPigments(Format = "binned") %>% 
  planktonr::pr_remove_outliers(2)
Pico <- planktonr::pr_get_NRSEnvContour('Pico')

ctd <- planktonr::pr_get_NRSCTD() %>% 
  dplyr::select(-c("Project", "file_id", "tz", "SampleTime_UTC", "Latitude", "Longitude"), 
                CTD_Salinity = "Salinity_psu", CTD_Temperature_degC = "Temperature_degC") %>% 
  dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m,0)) %>% 
  dplyr::filter(SampleDepth_m %in% datNRSm$SampleDepth_m) %>% 
  tidyr::pivot_longer(-c(dplyr::any_of(colnames(datNRSm))), values_to = "Values", names_to = "Parameters") 

CSChem <- planktonr::pr_get_CSChem() %>% 
  dplyr::filter(.data$Parameters %in% c("Chla_mgm3", "Temperature_degC", "Salinity_psu")) %>% 
  dplyr::mutate(Parameters = ifelse(Parameters == "Salinity_psu", "Salinity", Parameters),
                SampleDepth_m = round(.data$SampleDepth_m/10,0)*10,
                SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'day')) %>% 
  dplyr::filter(Values != -9999)


# get SOTS physical and chemical data ----------------------------------

# access the SOTS physical data

thredds_url <- "https://thredds.aodn.org.au/thredds/catalog/IMOS/DWM/SOTS/derived_products/gridded/catalog.html"
catalog_html <- RCurl::getURL(thredds_url)
catalog_parsed <- rvest::read_html(catalog_html)
file_nodes <- rvest::html_nodes(catalog_parsed, "a[href]")
file_list <- rvest::html_attr(file_nodes, "href")
file_list <- file_list[grepl("\\.nc$", file_list)] 

# access the SOTS nutrient data

years <- seq(1997, lubridate::year(Sys.Date()), 1)

nutsFiles <- function(years){
  thredds_url_nuts <- paste0("https://thredds.aodn.org.au/thredds/catalog/IMOS/DWM/SOTS/", years, "/catalog.html")
  catalog_html <- RCurl::getURL(thredds_url_nuts)
  catalog_parsed <- rvest::read_html(catalog_html)
  file_nodes <- rvest::html_nodes(catalog_parsed, "a[href]")
  file_list <- rvest::html_attr(file_nodes, "href")
  file_list[grepl("*RAS*", file_list)]  # Assuming you are looking for .nc files
}

file_list_nuts <- (purrr::compact(purrr::map(years, nutsFiles)) %>% 
                     purrr::map_df(tibble::as_tibble))$value


varlist <- function(file_list){
  # Construct the full URL if needed
  full_url <- paste0("https://thredds.aodn.org.au/thredds/dodsC/", sub(".*=", "", file_list))
  # Open the NetCDF file
  nc <- ncdf4::nc_open(full_url)
  dimensions <- data.frame(names(nc$var))
  ncdf4::nc_close(nc)
  dimensions
}

SOTSvar <- purrr::map(file_list, varlist) %>% 
  purrr::list_rbind() %>% dplyr::distinct() 
NUTSvar <- purrr::map(file_list_nuts, varlist) %>% 
  purrr::list_rbind() %>% dplyr::distinct()

# make a dataset in the required format

SOTSdata <- function(file_list){
  # Construct the full URL if needed
  full_url <- paste0("https://thredds.aodn.org.au/thredds/dodsC/", sub(".*=", "", file_list))
  # Open the NetCDF file
  nc <- ncdf4::nc_open(full_url)
  
  dimensions <- names(nc$var)
  
  variables <- c("NOMINAL_DEPTH_SST", "SST", "NOMINAL_DEPTH_TEMP", "TEMP","NOMINAL_DEPTH_CPHL", 'CPHL', 
                 "NOMINAL_DEPTH_DOX2", 'DOX2', "NOMINAL_DEPTH_PSAL", 'PSAL', 'MLD', 'PAR', 'NOMINAL_DEPTH_PAR',
                 'pHt', 'NOMINAL_DEPTH_pHt', 'NTRI_CONC', "NTRI", "PHOS_CONC", "PHOS", "SLCA_CONC", "SLCA", 
                 "ALKA_CONC", "TALK","TCO2", "DEPTH", "NOMINAL_DEPTH")
  
  variablesAvailable <- intersect(dimensions, variables) %>% stringr::str_subset(pattern = 'DEPTH', negate = TRUE)
  
  dates <- as.POSIXct((nc$dim$TIME)$vals*3600*24, origin = '1950-01-01 00:00:00', tz = 'UTC')
  
  vardat <- function(variablesAvailable){
    if('MLD' %in% variablesAvailable){
      vdat <- data.frame(ncdf4::ncvar_get(nc, varid = variablesAvailable), dates) 
      colnames(vdat) <- c('0', 'SampleTime_Local')
      vdat %>% tidyr::pivot_longer(-SampleTime_Local, values_to = 'Values', names_to = 'SampleDepth_m') %>% 
        dplyr::mutate(SampleDepth_m = as.numeric(SampleDepth_m),
                      Parameters = paste0(variablesAvailable))  
    } else if(length(stringr::str_subset(pattern = 'NOMINAL_DEPTH_', dimensions)) > 0) {
      Depthvars <- ncdf4::ncvar_get(nc, varid=paste0("NOMINAL_DEPTH_", variablesAvailable))
      vdat <- data.frame(ncdf4::ncvar_get(nc, varid = variablesAvailable), dates) 
      colnames(vdat) <- c(Depthvars, 'SampleTime_Local')
      vdat %>% tidyr::pivot_longer(-SampleTime_Local, values_to = 'Values', names_to = 'SampleDepth_m') %>% 
        dplyr::mutate(SampleDepth_m = as.numeric(SampleDepth_m),
                      Parameters = paste0(variablesAvailable))  
    } else if(length(stringr::str_subset(pattern = 'NOMINAL_DEPTH', dimensions)) > 0) {
      Depthvars <- ncdf4::ncvar_get(nc, varid=paste0("NOMINAL_DEPTH"))
      vdat <- data.frame(ncdf4::ncvar_get(nc, varid = variablesAvailable), dates) 
      colnames(vdat) <- c(Depthvars, 'SampleTime_Local')
      vdat %>% tidyr::pivot_longer(-SampleTime_Local, values_to = 'Values', names_to = 'SampleDepth_m') %>% 
        dplyr::mutate(SampleDepth_m = as.numeric(SampleDepth_m),
                      Parameters = paste0(variablesAvailable))  
    } else {
      Depthvars <- ncdf4::ncvar_get(nc, varid=paste0("DEPTH"))
      vdat <- data.frame(ncdf4::ncvar_get(nc, varid = variablesAvailable), dates) 
      colnames(vdat) <- c(Depthvars, 'SampleTime_Local')
      vdat %>% tidyr::pivot_longer(-SampleTime_Local, values_to = 'Values', names_to = 'SampleDepth_m') %>% 
        dplyr::mutate(SampleDepth_m = as.numeric(SampleDepth_m),
                      Parameters = paste0(variablesAvailable))  
      
    }
  }
  
  df <- purrr::map(variablesAvailable, vardat) %>% 
    purrr::list_rbind()
  
  ncdf4::nc_close(nc)
  
  df %>% 
    dplyr::mutate(StationName = 'Southern Ocean Time Series',
                  StationCode = 'SOTS',
           Month_Local = lubridate::month(SampleTime_Local),
           SampleTime_Local = lubridate::floor_date(SampleTime_Local, unit = 'day'),
           Parameters = dplyr::case_when(grepl("CPHL", Parameters) ~ "ChlF_mgm3",
                                         grepl("DOX2", Parameters) ~ "DissolvedOxygen_umolkg",
                                         grepl("MLD", Parameters) ~ "MLD_m",
                                         grepl("PSAL", Parameters) ~ "Salinty",
                                         grepl("TEMP", Parameters) ~ "Nitrate_umolL",
                                         grepl("NTRI", Parameters) ~ "MLD_m",
                                         grepl("PHOS", Parameters) ~ "Phosphate_umolL",
                                         grepl("SLCA", Parameters) ~ "Silicate_umolL",
                                         grepl("TALK|ALKA", Parameters) ~ "Alkalinity_umolkg",
                                         grepl("pHt", Parameters) ~ "pH",
                                         grepl("TCO", Parameters) ~ "DIC_umolkg",
                                         .default = Parameters)) %>%
    dplyr::group_by(StationName, StationCode, Month_Local, SampleTime_Local, Parameters, SampleDepth_m) %>%
    dplyr::summarise(Values = mean(Values, na.rm = TRUE),
              .groups = 'drop') %>%
    tidyr::drop_na(Parameters) 
  
}

SOTSwater <- purrr::map(file_list, SOTSdata) %>% 
  purrr::list_rbind() %>% 
  planktonr::planktonr_dat("Water", "SOTS")
NutsSots <- purrr::map(file_list_nuts, SOTSdata) %>% 
  purrr::list_rbind() %>% 
  planktonr::planktonr_dat("Water", "SOTS")

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

daynightzAll <- planktonr::pr_get_DayNight(Type = "Zooplankton") %>% 
  dplyr::filter(Species_m3 > 0) %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(count = dplyr::n(),
                   .groups = 'drop') %>% 
  dplyr::filter(count > 14)

daynightz <- planktonr::pr_get_DayNight(Type = "Zooplankton") %>% 
  dplyr::filter(Species %in% daynightzAll$Species)

daynightpAll <- planktonr::pr_get_DayNight(Type = "Phytoplankton") %>% 
  dplyr::filter(Species_m3 > 0) %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(count = dplyr::n(),
                   .groups = 'drop') %>% 
  dplyr::filter(count > 14)

daynightp <- planktonr::pr_get_DayNight(Type = "Phytoplankton") %>% 
  dplyr::filter(Species %in% daynightpAll$Species)

rm(daynightzAll, daynightpAll)

# Policy data -------------------------------------------------------------

PolNRS <- planktonr::pr_get_EOVs(Survey = "NRS") %>% 
  dplyr::filter(!StationCode %in% c("NIN", "ESP")) %>% 
  planktonr::pr_remove_outliers(2)

PolCPR <- planktonr::pr_get_EOVs(Survey = "CPR", near_dist_km = 250) %>% 
  planktonr::pr_remove_outliers(2) %>% 
  dplyr::filter(!BioRegion %in% c("North", "North-west", "None"))

PolLTM <- planktonr::pr_get_EOVs(Survey = "LTM") %>% 
  planktonr::pr_remove_outliers(2)

NRSinfo <- planktonr::pr_get_PolicyInfo(Survey = "NRS")
CPRinfo <- planktonr::pr_get_PolicyInfo(Survey = "CPR")

# Species distribution data
fMapDataz <- planktonr::pr_get_FreqMap(Type = "Zooplankton") %>% 
  dplyr::select(-c('samples', 'freq', 'freqsamp'))

fMapDatap <- planktonr::pr_get_FreqMap(Type = "Phytoplankton") %>% 
  dplyr::select(-c('samples', 'freq', 'freqsamp'))

# Progress Map ------------------------------------------------------------

PM <- planktonr::pr_get_ProgressMapData(Survey = c("CPR", "NRS"), interactive = TRUE, near_dist_km = 250)

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
    file <- ncdf4::nc_open(file.path("data-raw/LMTS", "PH100_CLIM.nc"))
  } else {
    file <- ncdf4::nc_open(file.path("data-raw/LMTS", paste0("NRS", Stations, "_CLIM.nc")))
  }
  
  all_depth <- ncdf4::ncvar_get(file, "DEPTH")
  i <- which(all_depth == Depth)
  
  # CLIM[DEPTH, TIME]
  tibble::tibble(CLIM = ncdf4::ncvar_get(file, varid = "CLIM", count = c(1, 365), start = c(i, 1)),
                  DOY = ncdf4::ncvar_get(file, varid = "TIME", count = 365, start = 1),
                  StationCode = Stations,
                  Names = Names)
}

Stations <- planktonr::pr_get_NRSStation() %>%
  dplyr::select('StationCode') %>%
  dplyr::filter(!.data$StationCode %in% c("NIN", "ESP", "PH4", "VBM"))
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
  
  tidync::hyper_tibble(file) %>%
    dplyr::mutate(StationCode = Stations,
                  TIME = lubridate::yday(TIME),
                  DEPTH = as.numeric(DEPTH)) %>% 
    dplyr::summarise(CLIM = mean(CLIM, na.rm = TRUE), .by = tidyselect::all_of(c("DEPTH", "TIME", "StationCode"))) # Get rid of the extra day that causes trouble with the join later
}  

MooringClim <- purrr::map_dfr(Stations, pr_get_mooringClim) %>%
  planktonr::pr_add_StationName() %>%
  planktonr::pr_reorder()

# Get Species Info for each Taxa ------------------------------------------

SpInfoP <- planktonr::pr_get_SpeciesInfo(Type = "Phytoplankton")
SpInfoZ <- planktonr::pr_get_SpeciesInfo(Type = "Zooplankton")

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

# Add data to sysdata.rda -------------------------------------------------
usethis::use_data(Nuts, Pigs, Pico, ctd, CSChem,
                  fMapDataz, fMapDatap, 
                  MooringTS, MooringClim,
                  PolNRS, PolCPR, PolLTM, 
                  NRSinfo, CPRinfo, NRSStation,
                  datCPRz, datCPRp, PCI,
                  datNRSz, datNRSp, datNRSw, 
                  datNRSm, datCSm, datGSm,
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
                  SOTSp, SOTSwater, NutsSots, SOTSfgp,
                  stiz, stip, daynightz, daynightp,
                  SpInfoP, SpInfoZ, LFData, LFDataAbs,
                  datNRSTrip, datCPRTrip,
                  PSpNRSAccum, PSpCPRAccum, ZSpNRSAccum, ZSpCPRAccum,
                  ParamDef, col12, modified_time,
                  overwrite = TRUE, internal = TRUE)

