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


# Receivers ----
# Build receivers FIRST so the installation_name lookup is available for dat

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


# Build a lookup: (installation_name_orig, station_name) -> new installation_name
# This is used to propagate the split installation names into the detection data.
installation_name_lookup <- receivers_fix %>%
  dplyr::distinct(installation_name_orig, station_name, installation_name)


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
    # Apply simple canonical name fixes first (same as receivers_raw)
    installation_name   = dplyr::recode(installation_name, !!!installation_name_fixes)
  ) %>%
  # Apply the split-installation lookup so that installations spanning >= 2 degrees
  # get the same "installation_name (station_name)" label used in ReceiverSummary.rds.
  # Rows whose (installation_name, station_name) pair is not in the lookup (i.e. the
  # installation was NOT split) retain their original installation_name unchanged.
  dplyr::left_join(
    installation_name_lookup,
    by = c("installation_name" = "installation_name_orig", "station_name")
  ) %>%
  dplyr::mutate(
    installation_name = dplyr::coalesce(installation_name.y, installation_name)
  ) %>%
  dplyr::select(-installation_name.y) %>%
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


# Species Summary -----

dat_spp <- dat %>%
  mutate(month_UTC = floor_date(date_UTC, "month")) %>%
  summarise(total_individuals = n(),
            total_detections = sum(total_detections, na.rm = TRUE),
            .by = c("installation_name", "month_UTC", "species_common_name", 
                    "species_scientific_name", "WORMS_species_aphia_id"))


# Save all outputs at the end ---------------------------------------------------

write_rds(dat,      "data-raw/AnimalTracking/Output/AnimalSummary.rds",  compress = "xz")
write_rds(dat_spp,  "data-raw/AnimalTracking/Output/SpeciesSummary.rds", compress = "xz")
write_rds(receivers,"data-raw/AnimalTracking/Output/ReceiverSummary.rds")
