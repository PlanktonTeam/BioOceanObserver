## code to prepare `ZooTsNRS.RData` dataset goes here

# source all the main functions from the IMOS_Toolbox, this is the default place for the package functions for now

datNRSz <- planktonr::pr_get_tsdata("NRS", "Z")
datNRSp <- planktonr::pr_get_tsdata("NRS", "P")

meta_sf <- planktonr::pr_get_NRSTrips("Z") %>% 
  dplyr::select(StationName, StationCode, Longitude, Latitude) %>% unique() %>%
  dplyr::rename(Code = StationCode, Station = StationName) %>%
  dplyr::filter(Station != 'Port Hacking 4') %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

MapOz <- rnaturalearth::ne_countries(scale = "medium", country = "Australia",
                                     returnclass = "sf")

# add the outputs to the use_this::use_data line in 02_dev.R

