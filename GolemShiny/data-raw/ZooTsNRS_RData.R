## code to prepare `ZooTsNRS.RData` dataset goes here
library(tidyverse)

# source all the main functions from the IMOS_Toolbox, this is the default place for the package functions for now
source("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/IMOS_Plankton_functions.R", local = FALSE)

datNRSi <- read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/Output/NRS_Indices.csv") %>% 
  mutate(Month = lubridate::month(SampleDateLocal),
         Year = lubridate::year(SampleDateLocal),
         Code = str_sub(TripCode, 1, 3),
         Name = str_c(Station, " (",Code,")"), # Create neat name for plotting
         Code = factor(Code),
         Name = factor(Name)) %>%
  complete(Year, nesting(Station, Code)) %>% # Turns implicit missing values into explicit missing values.
  select(Year, Month, SampleDateLocal, Latitude, Station, Code, Biomass_mgm3:CopepodEvenness) %>%
  pivot_longer(-c(Year:Code), values_to = 'Values', names_to = "parameters") %>%
  arrange(-Latitude)  # Sort in ascending date order

meta_sf <- getNRSTrips() %>% select(Station, StationCode, Longitude, Latitude) %>% unique() %>%
  rename(Code = StationCode) %>%
  filter(Station != 'Port Hacking 4') %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

usethis::use_data(datNRSi, meta_sf, overwrite = TRUE)
