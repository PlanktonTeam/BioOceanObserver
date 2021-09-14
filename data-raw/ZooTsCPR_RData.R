## code to prepare `ZooTsCPR_RData` dataset goes here
library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rgdal)
library(cmocean)

# source all the main functions from the IMOS_Toolbox, this is the default place for the package functions for now
source("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/IMOS_Plankton_functions.R", local = FALSE)

datCPRzts <- readr::read_csv(paste0(planktonr::pr_get_outputs(), "CPR_Indices.csv"), na = "", show_col_types = FALSE) %>%
  dplyr::select(.data$Latitude, .data$SampleDateUTC, .data$Year, .data$Month, .data$Day, .data$BioRegion, .data$Biomass_mgm3, .data$ZoopAbundance_m3:.data$CopepodEvenness) %>%
  dplyr::mutate(Biomass_mgm3 = ifelse(.data$Biomass_mgm3 < 0 , 0, .data$Biomass_mgm3),
                SampleDateUTC = lubridate::round_date(.data$SampleDateUTC, "month"),
                YearMon = paste(.data$Year, .data$Month)) %>% # this step can be improved when nesting supports data pronouns
  tidyr::complete(.data$BioRegion, .data$YearMon) %>%
  dplyr::mutate(Year = as.numeric(stringr::str_sub(.data$YearMon, 1, 4)),
                Month = as.numeric(stringr::str_sub(.data$YearMon, -2, -1))) %>%
  tidyr::pivot_longer(.data$Biomass_mgm3:.data$CopepodEvenness, values_to = "Values", names_to = 'parameters') %>%
  dplyr::group_by(.data$SampleDateUTC, .data$Year, .data$Month, .data$BioRegion, .data$parameters) %>%
  dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                   Latitude = mean(.data$Latitude, na.rm = TRUE), # so we can arrange by latitude and get the BioRegions in the correct order
                   .groups = "drop") %>%
  dplyr::filter(!is.na(.data$BioRegion), 
                .data$BioRegion != 'North',
                .data$BioRegion != 'North-west') %>%
  dplyr::arrange(-.data$Latitude) # this is included to keep the mapping in the right order for the ui

latlonCRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

bioregion <- readOGR("C:/Users/dav649/Documents/GitHub/IMOS_Toolbox/General/Shapefiles/marine_regions") %>% 
  st_as_sf(crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>% 
  st_transform(crs = latlonCRS) %>%
  filter(ENVIRON == "Marine")

# save data into data folder
usethis::use_data(datCPRzts, bioregion, internal = FALSE, overwrite = TRUE)

