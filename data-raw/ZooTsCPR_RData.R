## code to prepare `ZooTsCPR_RData` dataset goes here
library(tidyverse)
library(planktonr)
library(rgdal)

# source all the main functions from the IMOS_Toolbox, this is the default place for the package functions for now
source("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/IMOS_Plankton_functions.R", local = FALSE)

datCPRz <- planktonr::pr_get_tsdata("CPR", "Z")
datCPRp <- planktonr::pr_get_tsdata("CPR", "P")

bioregion <- readOGR("C:/Users/dav649/Documents/GitHub/IMOS_Toolbox/General/Shapefiles/marine_regions") %>% 
  st_as_sf(crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>% 
  st_transform(crs = latlonCRS) %>%
  filter(ENVIRON == "Marine")

# save data into data folder
#usethis::use_data(datCPRzts, bioregion, internal = FALSE, overwrite = TRUE)

