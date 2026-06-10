library(tidyverse)
library(sf)
library(worrms)
library(aws.s3)

# reprocessCSV <- FALSE

# Canonical installation name corrections (applied before any summarise) -------
installation_name_fixes <- c(
  "Lady Elliot Island SEQ" = "Lady Elliot Island",
  "Byron Bay array"        = "Byron Bay"
)

# if (isTRUE(reprocessCSV)){
  

# dat <- dat %>% 
#   select(WORMS_species_aphia_id, species_scientific_name) %>% 
#   filter(is.na(WORMS_species_aphia_id)) %>% 
#   pull(species_scientific_name) %>% 
#   unique()

  no_aphia <- c(
    "Protonibea diacanthus",
    "Caranx lugubris",
    "Chelodina expansa",
    "Pseudaphritis urvillii",
    "Chelodina longicollis",
    "Cyprinus carpio",
    "Rhynchobatus palpebratus",
    "Macquaria ambigua",
    "Leptomithrax gaimardii",
    "Eretmochelys imbricata",
    "Elusor macrurus",
    "Maccullochella peelii",
    "Aetomylaeus vespertilio",
    "Platycephalus laevigatus",
    "Istiophorus platypterus",
    "Trygonorrhina dumerilii",
    "Carcharhinus brevipinna",
    "Aptychotrema vincentiana",
    "Carcharhinus coatesi",
    "Aetobatus ocellatus"
  )
  
  
  # Fill missing WORMS AphiaIDs for species listed in no_aphia --------------------
  aphia_lookup <- tibble(
    species_scientific_name = no_aphia,
    aphia_id_fetched = purrr::map_int(
      no_aphia,
      ~ tryCatch(worrms::wm_name2id(.x), error = function(e) NA_integer_)
    )
  )
  
  
  keep_cols <- c("animal_id", "date_UTC", "nb_detections", "species_common_name", "species_scientific_name",
                 "WORMS_species_aphia_id", "animal_sex", "measurement", "transmitter_deployment_locality",
                 "tag_deployment_project_name", "installation_name", "station_name", "receiver_project_name", "receiver_name",
                 "receiver_deployment_id")
  
  # Load species data from AODN public S3 bucket
  # https://data.aodn.org.au/?prefix=IMOS/AATAMS/acoustic_detections_QC_summary/
    
  bucket_objects <- aws.s3::get_bucket(
    bucket = "imos-data",
    prefix = "IMOS/AATAMS/acoustic_detections_QC_summary/",
    region = "ap-southeast-2",
    max    = Inf
  )

  files <- unname(purrr::map_chr(bucket_objects, ~ .x[["Key"]]))
  
  # The file names are retrieved but they have a permanent redirect on them. Perhaps its not a true S3 bucket.
  # The solution is to get the filenames only and append them to the AODN weblink

  files <- paste0("https://data.aodn.org.au/IMOS/AATAMS/acoustic_detections_QC_summary/", basename(files))
  
  dat <-purrr::map(files, read_csv, 
                   col_types = cols(receiver_deployment_datetime_UTC = col_datetime(),
                                    receiver_recovery_datetime_UTC = col_datetime(),
                                    transmitter_deployment_datetime_UTC = col_datetime(),
                                    transmitter_recovery_datetime_UTC = col_datetime()))
  
  
  dat <- dat %>% 
    bind_rows() %>% 
    dplyr::mutate(date_UTC = lubridate::as_date(date_hour_UTC)) %>% 
    dplyr::select(all_of(keep_cols)) %>% 
    mutate(
      species_common_name = stringr::str_to_title(species_common_name),
      installation_name   = dplyr::recode(installation_name, !!!installation_name_fixes)
    ) %>%
    summarise(
      total_detections = sum(nb_detections),
      .by = c("animal_id", "installation_name", "date_UTC",
              # These are to keep the data
              "species_common_name", "species_scientific_name",
              "WORMS_species_aphia_id", "animal_sex", "measurement")
    ) %>% 
    # 0. Keep only the first entry when measurement is repeated (e.g. "L = 820 mm; L = 820 mm")
    mutate(measurement = str_trim(str_extract(measurement, "^[^;]+"))) %>%
    # 1. Extract the three parts: Type, Value, and Unit
    # The regex groups () capture the text we want to keep
    extract(
      measurement,
      into = c("Length_Type", "Length_cm", "Unit"),
      regex = "(.*) = ([0-9]+) (.*)",
      remove = FALSE,
      convert = TRUE # Automatically turns the Value column into a numeric type
    ) %>%
    # 2. Convert everything to cm and clean up the Type casing
    mutate(
      Length_cm = case_when(
        str_to_lower(Unit) == "mm" ~ Length_cm / 10,  # case-insensitive: handles "mm", "Mm", "MM" etc.
        TRUE ~ as.numeric(Length_cm)
      ),
      Length_Type = str_to_title(Length_Type) # Standardizes "TOTAL LENGTH" to "Total Length"
    ) %>%
    # 3. Drop the Unit column since everything is now cm
    select(-Unit, -measurement)
  
  
  
  dat <- dat %>%
    left_join(aphia_lookup, by = "species_scientific_name") %>%
    mutate(
      WORMS_species_aphia_id = dplyr::coalesce(WORMS_species_aphia_id, aphia_id_fetched)
    ) %>%
    select(-aphia_id_fetched)
  
  write_rds(dat, "data-raw/AnimalTracking/Output/AnimalSummary.rds", compress = "xz")
  



dat2 <- dat %>%
  mutate(month_UTC = floor_date(date_UTC, "month")) %>%
  summarise(total_individuals = n(),
            total_detections = sum(total_detections, na.rm = TRUE),
            .by = c("installation_name", "month_UTC", "species_common_name", 
                    "species_scientific_name", "WORMS_species_aphia_id"))

write_rds(dat2, "data-raw/AnimalTracking/Output/SpeciesSummary.rds", compress = "xz")



# Receiver data

APIreceivers <- function(url) {
  tmp <- tempfile(fileext = ".zip")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  files <- unzip(tmp, list = TRUE)
  csv_path <- unzip(tmp, files = files$Name[1], exdir = tempdir())
  data <- read_csv(csv_path)
  return(data)
}


# Load receiver data from the portal
receivers_raw <- APIreceivers(url = "https://animaltracking.aodn.org.au/api/receiver/deployment/deployments.zip") %>%
  mutate(
    active = active == "YES",
    installation_name = dplyr::recode(installation_name, !!!installation_name_fixes)
  ) 


# First fix what is easy
receivers_fix <- receivers_raw %>%
  dplyr::filter_out(installation_name == "Cleveland Bay" & receiver_deployment_longitude > 150) %>%  # Some sites are off Lord Howe Island
  dplyr::filter_out(installation_name == "Port Stephens VPS" & station_name == "Station 4") %>%  # incorrect latitude
  dplyr::filter_out(installation_name == "IMOS-ATF Coffs Harbour line" & receiver_deployment_longitude < -31) %>%  # incorrect latitude
  # Then mutate and create distance columns  
  dplyr::mutate(diff_lat = max(receiver_deployment_latitude, na.rm = TRUE) - min(receiver_deployment_latitude, na.rm = TRUE),
                diff_lon = max(receiver_deployment_longitude, na.rm = TRUE) - min(receiver_deployment_longitude, na.rm = TRUE),
                .by = c("installation_name")) %>% 
# Use if_else to create a new installation column - either standard or `installation (station)`
  dplyr::rename(installation_name_orig = installation_name) %>% 
  dplyr::mutate(installation_name = dplyr::if_else(diff_lat >= 2 | diff_lon >= 2, 
                                                   paste0(installation_name_orig, " (", station_name, ")"), 
                                                   installation_name_orig),
                   .by = c("installation_name_orig"))


receivers <- receivers_fix %>%
  sf::st_as_sf(coords = c("receiver_deployment_longitude", "receiver_deployment_latitude"), crs = 4326, remove = FALSE) %>%
  summarise(
    total_receivers = n(),
    deployment_date = lubridate::as_date(min(receiver_deployment_datetime, na.rm = TRUE)),
    deployment_date = if_else(is.finite(deployment_date), deployment_date, NA),
    recovery_date = lubridate::as_date(max(receiver_recovery_datetime, na.rm = TRUE)),
    recovery_date = if_else(is.finite(recovery_date), recovery_date, NA),
    n_purchasing_organisation = length(unique(purchasing_organisation)),
    n_receiver_project_name = length(unique(receiver_project_name)),
    purchasing_organisation = paste(unique(purchasing_organisation), collapse = "; "),
    receiver_project_name = paste(unique(receiver_project_name), collapse = "; "),
    active = any(active, na.rm = TRUE),
    geometry = st_union(geometry),
    .by = c("installation_name")
  ) %>%
  sf::st_centroid()


write_rds(receivers, "data-raw/AnimalTracking/Output/ReceiverSummary.rds")



# # Diagnostic: find installations within ±0.01 ° of each other ----------------
# PROXIMITY_DEG <- 0.01
# 
# receiver_coords <- receivers %>%
#   mutate(
#     lon = round(sf::st_coordinates(.)[, 1], 5),
#     lat = round(sf::st_coordinates(.)[, 2], 5)
#   ) %>%
#   sf::st_drop_geometry() %>%
#   select(installation_name, lon, lat)
# 
# nearby_pairs <- receiver_coords %>%
#   rename(name_a = installation_name, lon_a = lon, lat_a = lat) %>%
#   cross_join(
#     receiver_coords %>% rename(name_b = installation_name, lon_b = lon, lat_b = lat)
#   ) %>%
#   filter(
#     name_a < name_b,   # keep each pair once, exclude self-pairs
#     abs(lon_a - lon_b) <= PROXIMITY_DEG,
#     abs(lat_a - lat_b) <= PROXIMITY_DEG
#   ) %>%
#   mutate(
#     dist_lon = round(abs(lon_a - lon_b), 5),
#     dist_lat = round(abs(lat_a - lat_b), 5)
#   ) %>%
#   arrange(dist_lon + dist_lat)

# if (nrow(nearby_pairs) == 0) {
#   message("No installations found within ", PROXIMITY_DEG, " degrees of each other.")
# } else {
#   message(nrow(nearby_pairs), " pair(s) of installations within ", PROXIMITY_DEG, " degrees:")
#   print(as.data.frame(nearby_pairs))
# }





# # ==============================================================================
# # DATA PREPARATION
# # ==============================================================================
# 
# receivers      <- readRDS("data-raw/AnimalTracking/Output/ReceiverSummary.rds")   # sf POINT
# animal_summary <- readRDS("data-raw/AnimalTracking/Output/AnimalSummary.rds")     # tbl with Type/Value cols
# 
# # Station-level aggregate counts -----------------------------------------------
# station_summary <- animal_summary %>%
#   group_by(installation_name) %>%
#   summarise(
#     n_species        = n_distinct(species_common_name),
#     n_individuals    = n_distinct(animal_id),
#     total_detections = sum(total_detections),
#     .groups = "drop"
#   )
# 
# # Species breakdown per station (for sidebar cards) ----------------------------
# station_species <- animal_summary %>%
#   group_by(installation_name, species_common_name, species_scientific_name,
#            WORMS_species_aphia_id) %>%
#   summarise(
#     n_individuals    = n_distinct(animal_id),
#     total_detections = sum(total_detections),
#     .groups = "drop"
#   ) %>%
#   arrange(installation_name, species_common_name)
# 
# # Individual-level data: one row per animal at each station --------------------
# # (deduplicated - used for sex ratio and length charts)
# individual_data <- animal_summary %>%
#   distinct(animal_id, installation_name, species_common_name,
#            species_scientific_name, animal_sex, Type, Value)
# 
# # Sorted species list for the selectize filter --------------------------------
# all_species <- sort(unique(animal_summary$species_common_name))
# 
# # Daily time-series per station × species (for sparklines) --------------------
# daily_summary <- animal_summary %>%
#   group_by(installation_name, species_common_name, date_UTC) %>%
#   summarise(
#     n_individuals    = n_distinct(animal_id),
#     total_detections = sum(total_detections),
#     .groups = "drop"
#   )
# 
# # Enrich receiver sf object — keep ONLY stations with detection data -----------
# receivers <- receivers %>%
#   left_join(station_summary, by = "installation_name") %>%
#   filter(!is.na(n_species)) %>%                          # drop stations with no data
#   mutate(
#     has_data         = TRUE,
#     n_species        = as.integer(n_species),
#     n_individuals    = as.integer(n_individuals),
#     total_detections = as.integer(total_detections),
#     lon              = round(sf::st_coordinates(.)[, 1], 5),
#     lat              = round(sf::st_coordinates(.)[, 2], 5),
#     # Opacity: log scale from 0 detections (0.1) up to >= 1M detections (1.0)
#     point_opacity    = pmin(1, pmax(0.1, log1p(total_detections) / log1p(1e6)))
#   )
# 
# # Build popup HTML column (small Mapbox popup on click) ------------------------
# receivers$popup_html <- map_chr(seq_len(nrow(receivers)), function(i) {
#   r <- sf::st_drop_geometry(receivers[i, ])
#   data_line <- paste0(
#     '<span class="at-popup-has-data">&#10003;&nbsp;',
#     r$n_species, " species &bull; ",
#     format(r$n_individuals, big.mark = ","), " individuals",
#     "</span>"
#   )
#   paste0(
#     '<div class="at-popup">',
#     '<div class="at-popup-header">RECEIVER STATION</div>',
#     '<div class="at-popup-body">',
#     '<div class="at-popup-name">', htmlEscape(r$installation_name), "</div>",
#     '<div class="at-popup-coords">', r$lon, ", ", r$lat, "</div>",
#     data_line,
#     "</div></div>"
#   )
# })
