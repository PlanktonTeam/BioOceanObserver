## script for all RData 
## Aim to change all source calls to planktonr functions

# library(planktonr)

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
LTnuts <- planktonr::pr_get_LTnuts()

# Species distribution data
fMapDataz <- planktonr::pr_get_fMap_data("Z")
fMapDatap <- planktonr::pr_get_fMap_data("P")

# STI data
stiz <- planktonr::pr_get_sti("Z")
stip <- planktonr::pr_get_sti("P")

# Day-Night data (from CPR only)
daynightz <- planktonr::pr_get_daynight("Z")
daynightp <- planktonr::pr_get_daynight("P")

library(magrittr)
# Policy data set
pr_get_pol <- function(Survey = 'NRS'){
  if(Survey == 'CPR'){
    file <- 'CPR_Indices.csv'
    } else {
    file <- 'NRS_Indices.csv'
  }
  Polr <- readr::read_csv(paste0(planktonr::pr_get_outputs(), file), na = "NA", show_col_types = FALSE) 
  
  if(Survey == 'CPR') {
    
      Pol <-  Polr %>% 
        dplyr::select(SampleDateUTC, Year, Month, BioRegion, Biomass_mgm3, PhytoBiomassCarbon_pgm3, ShannonCopepodDiversityCPR, 
                      ShannonPhytoDiversitycpr) %>%
        dplyr::filter(!BioRegion %in% c("North", "North-west")) %>%
        tidyr::pivot_longer(-c(SampleDateUTC:BioRegion), values_to = 'Values', names_to = "parameters")

      means <- Polr %>%
        dplyr::select(BioRegion, Biomass_mgm3, PhytoBiomassCarbon_pgm3, ShannonCopepodDiversityCPR, 
                      ShannonPhytoDiversitycpr) %>%
        tidyr::pivot_longer(-c(BioRegion), values_to = 'Values', names_to = "parameters") %>%
        dplyr::group_by(BioRegion, parameters) %>%
        dplyr::summarise(means = mean(Values, na.rm = TRUE), 
                         sd = stats::sd(Values, na.rm = TRUE),
                         .groups = 'drop')
      
      Pol <- Pol %>% dplyr::left_join(means, by = c("BioRegion", "parameters")) %>%
        dplyr::mutate(anomaly = (Values - means)/sd)  
      
    } else {
      
      Pol <-  Polr %>% 
        dplyr::select(SampleDateLocal, Year, Month, StationName, StationCode, Biomass_mgm3, PhytoBiomassCarbon_pgL, CTDTemperature_degC, ShannonCopepodDiversity, 
                      ShannonPhytoDiversity, CTDSalinity_psu, PigmentChla_mgm3) %>%
        tidyr::pivot_longer(-c(SampleDateLocal:StationCode), values_to = 'Values', names_to = "parameters")
      
      means <- Polr %>%
        dplyr::select(StationName, Biomass_mgm3, PhytoBiomassCarbon_pgL, CTDTemperature_degC, ShannonCopepodDiversity, 
                      ShannonPhytoDiversity, Salinity_psu, PigmentChla_mgm3) %>%
        tidyr::pivot_longer(-c(StationName), values_to = 'Values', names_to = "parameters") %>%
        dplyr::group_by(StationName, parameters) %>%
        dplyr::summarise(means = mean(Values, na.rm = TRUE), 
                         sd = stats::sd(Values, na.rm = TRUE),
                         .groups = 'drop')
      
      Pol <- Pol %>% 
        dplyr::left_join(means, by = c("StationName", "parameters")) %>%
        dplyr::mutate(anomaly = (Values - means)/sd)  
      
    }
  }
  
pr_get_polInfo <- function(Survey = 'NRS'){
  
  if(Survey == 'NRS'){
    
    NRSinfo <- planktonr::pr_get_NRSStation() %>%   
      dplyr::mutate(Region = dplyr::case_when(StationCode %in% c("DAR") ~ "Tropical North",
                                              StationCode %in% c("YON") ~ "GBR Lagoon",
                                              StationCode %in% c("NSI", "PHB", "MAI") ~ "South East",
                                              StationCode %in% c("KAI") ~ "South Central",
                                              StationCode %in% c("ROT", "ESP", "NIN") ~ "South West"),
                    Features = dplyr::case_when(StationCode %in% c("DAR") ~ "broad, shallow shelf seas with strong tidal influence and tropical neritic communities.",
                                                StationCode %in% c("YON") ~ "shallow water influenced by the EAC and Hiri currents and is floristically distinct.",
                                                StationCode %in% c("NSI", "PHB", "MAI") ~ "very narrow shelf influenced by the EAC and its eddies with temperate neritic communities", 
                                                StationCode %in% c("KAI") ~ "upwelling systems and the Leeuwin and Flinders currents and covers the GAB and SA Gulf.",
                                                StationCode %in% c("ROT", "ESP", "NIN") ~ "narrow shelf influenced by the Leeuwin Current with tropical oeanic communities"),
                    now = dplyr::case_when(StationCode %in% c("DAR", "YON", "NSI", "PHB", "MAI", "KAI", 'ROT') ~ "and is ongoing",
                                           StationCode %in% c("ESP", "NIN") ~ "and concluded in March 2013")) %>%
      dplyr::select(-c(ProjectName, StationCode, IMCRA)) 
  
    } else {
    
      CPRinfo <- planktonr::pr_get_CPRTrips() %>%
        dplyr::mutate(Latitude = STARTLATITUDE, 
                      Longitude = ENDLONGITUDE) %>%
        planktonr::pr_add_bioregions() %>%
        dplyr::select(BioRegion, STARTSAMPLEDATEUTC, MILES) %>%
        dplyr::mutate(Features = dplyr::case_when(BioRegion %in% c("South-east") ~ "narrow shelf intensifying currents, eddies and upwellings with low nutrient and primary productivity",
                                                  BioRegion %in% c("South-west") ~ "temperate and subtropical habitats influenced by the nutrient deplete Leeuwin Current.",
                                                  BioRegion %in% c("Temperate East") ~ "temperate and subtropical habitats influenced by the East Australian Current and its eddies.",
                                                  BioRegion %in% c("Temperate East") ~ "Western Pacific Warm Pool water mass with low surface nutrient levels"))
  }
 
}


PolNRS <- pr_get_pol("NRS")
PolCPR <- pr_get_pol("CPR")
NRSinfo <- pr_get_polInfo("NRS")
CPRinfo <- pr_get_polInfo("CPR")

## microbial data

datNRSm <- readr::read_csv("data-raw/datNRSm.csv") %>%
  dplyr::mutate(SampleDepth_m = as.numeric(stringr::str_sub(TripCode_depth, -3, -1))) %>%
  dplyr::select(StationName, SampleDepth_m, StationCode, SampleDateLocal, Year, Month, 
         Prochlorococcus_cells_ml:Eukaryote_Chlorophyll_Index) %>%
  dplyr::rename(Prochlorococcus_Cellsml = Prochlorococcus_cells_ml,
                Synecochoccus_Cellsml = Synecochoccus_cells_ml,
                Picoeukaryotes_Cellsml = Picoeukaryotes_cells_ml) %>%
  tidyr::pivot_longer(-c(StationName:Month), values_to = "Values", names_to = "parameters")

# add data to sysdata.rda
usethis::use_data(Nuts, Pigs, fMapDataz, fMapDatap, Pico, LTnuts, 
                  PolNRS, PolCPR, NRSinfo, CPRinfo, 
                  datCPRz, datCPRp, datNRSz, datNRSp, datNRSm,
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp, 
                  stiz, stip, daynightz, daynightp,
                  overwrite = TRUE, internal = TRUE)

## files for SDMs (this will only work for Claire at the moment)
listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
file.copy(from=files, to="inst/app/www")

