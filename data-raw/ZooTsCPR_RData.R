## code to prepare `ZooTsCPR_RData` dataset goes here
library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rgdal)

# source all the main functions from the IMOS_Toolbox, this is the default place for the package functions for now
source("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/IMOS_Plankton_functions.R", local = FALSE)

CPRSamps <- getCPRSamps()

datCPRzts <- read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/Output/CPR_Indices.csv") %>%
  select(SampleDateUTC, Year, Month, Day, BioRegion, Biomass_mgm3, ZoopAbundance_m3:CopepodEvenness) %>%
  mutate(Biomass_mgm3 = ifelse(Biomass_mgm3 < 0 , 0, Biomass_mgm3),
         SampleDate = round_date(SampleDateUTC, "month")) %>%
  pivot_longer(Biomass_mgm3:CopepodEvenness, values_to = "values", names_to = 'parameters') %>%
  group_by(SampleDate, Year, Month, BioRegion, parameters) %>%
  summarise(values = mean(values, na.rm = TRUE),
            .groups = "drop") %>%
  filter(!is.na(BioRegion))

bioregion <- readOGR("C:/Users/dav649/Documents/GitHub/IMOS_Toolbox/General/Shapefiles/marine_regions") %>% 
  st_as_sf(crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>% 
  st_transform(crs = latlonCRS) %>%
  filter(ENVIRON == "Marine")

# save data into data folder
usethis::use_data(datCPRzts, bioregion, internal = FALSE, overwrite = TRUE)

"C:\Users\dav649\Documents\GitHub\IMOS_Toolbox\General"