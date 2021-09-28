## code to prepare `EnvDataBGC.RData` dataset goes here
suppressPackageStartupMessages({
  library(lubridate)
  library(data.table)
  library(sf)
  library(ozmaps)
  library(tidyverse) ## should go near last to put on tip of search path
})

## EnvDataBGC

Nuts <- readr::read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/RawData/BGC_Chemistry.csv",
                 col_types = list(SAMPLEDATELOCAL = readr::col_datetime())) %>%
  dplyr::select_if(!grepl('FLAG', names(.)) & !grepl('COMMENTS', names(.)) & !grepl('MICROB', names(.))) %>%
  dplyr::filter(PROJECTNAME == 'NRS') %>%
  pr_rename() %>%
  dplyr::mutate(StationCode = stringr::str_sub(TripCode, 1, 3), 
                Month = lubridate::month(SampleDateLocal)) %>%
  dplyr::select(-TripCode) %>%
  tidyr::pivot_longer(Salinity_psu:Oxygen_umolL, values_to = "Values", names_to = 'parameters') %>%
  pr_get_StationName() %>%
  dplyr::mutate(StationName = factor(StationName, levels = c("Darwin", "Yongala", 'Ningaloo', "North Stradbroke Island", 'Rottnest Island', 'Esperance', 'Port Hacking', 
                                                                'Kangaroo Island', 'Maria Island')))

Pigs  <- readr::read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/RawData/BGC_Pigments.csv",
                  col_types = list(PROJECTNAME = readr::col_character(),
                                   TRIP_CODE = readr::col_character(),
                                   SAMPLEDATELOCAL = readr::col_datetime(),
                                   SAMPLEDEPTH_M = readr::col_character(),
                                   .default = readr::col_double())) %>%
  dplyr::select_if(!grepl('FLAG', names(.)) & !grepl('COMMENTS', names(.))) %>%
  pr_rename() %>%
  dplyr::filter(ProjectName == 'NRS', SampleDepth_m != 'WC') %>%
  dplyr::rowwise() %>% 
  dplyr::mutate(SampleDepth_m = as.numeric(SampleDepth_m),
         TotalChla = sum(CPHLIDE_A,DV_CPHL_A,CPHL_A, na.rm = TRUE),
         TotalChl = sum(CPHLIDE_A, DV_CPHL_A, CPHL_A, DV_CPHL_B, CPHL_B, CPHL_C3, CPHL_C2, CPHL_C1, na.rm = TRUE),
         PPC = sum(ALLO, DIADCHR, DIADINO, DIATO, ZEA,  na.rm = TRUE),#+ CARO, #Photoprotective Carotenoids
         PSC = sum(BUT_FUCO, HEX_FUCO, PERID,  na.rm = TRUE),#Photosynthetic Carotenoids
         PSP = sum(PSC, TotalChl,  na.rm = TRUE),#Photosynthetic pigments
         TCaro = sum(PSC, PSP,  na.rm = TRUE),#Total Carotenoids
         TAcc = sum(TCaro, DV_CPHL_B, CPHL_B, CPHL_C3, CPHL_C2, CPHL_C1,  na.rm = TRUE),#Total Accessory pigments
         TPig = sum(TAcc, TotalChla,  na.rm = TRUE),#Total pigments
         TDP = sum(PSC, ALLO, ZEA, DV_CPHL_B, CPHL_B,  na.rm = TRUE),#Total Diagnostic pigments
         StationCode = stringr::str_sub(TripCode, 1, 3), 
         Month = lubridate::month(SampleDateLocal)) %>%
  dplyr::filter(TotalChla != 0) %>% 
  dplyr::select(ProjectName:SampleDepth_m, TotalChla:Month, -TripCode)  %>%
  tidyr::pivot_longer(TotalChla:TDP, values_to = "Values", names_to = 'parameters') %>%
  pr_get_StationName() %>%
  dplyr::mutate(StationName = factor(StationName, levels = c("Darwin", "Yongala", 'Ningaloo', "North Stradbroke Island", 'Rottnest Island', 'Esperance', 'Port Hacking', 
                                                             'Kangaroo Island', 'Maria Island')))

  
#usethis::use_data(NRSBGCEnvData, overwrite = TRUE, internal = FALSE)
pr_get_StationName <- function(df){
  df <- df %>%
    dplyr::mutate(StationName = dplyr::case_when(
      StationCode == "DAR" ~ "Darwin",
      StationCode == "YON" ~ "Yongala",
      StationCode == "NSI" ~ "North Stradbroke Island",
      StationCode == "PHB" ~ "Port Hacking",
      StationCode == "MAI" ~ "Maria Island",
      StationCode == "KAI" ~ "Kangaroo Island",
      StationCode == "ESP" ~ "Esperance",
      StationCode == "ROT" ~ "Rottnest Island",
      StationCode == "NIN" ~ "Ningaloo"))
}
pr_rename <- function(df){
  
  ##TODO - Need to replace these before I implement the flagging code.
  rename_df <- tibble::as_tibble(matrix(c(
    "Ammonium_Flag", "AMMONIUM_FLAG",
    "Ammonium_umolL", "AMMONIUM_UMOLL",
    "BioVolume_um3m3", "BIOVOL_UM3M3",
    "Biomass_mgm3", "BIOMASS_MGM3",
    "Biovolume_um3L", "BIOVOLUME_UM3L",
    # "CTDChlF_mgm", "Chla_mgm",
    # "CTDConductivity_Sm", "Conductivity_Sm",
    # "CTDSalinity", "Salinity_psu",
    # "CTDTemperature", "Temperature_degC",
    # "CTDTurbidity_ntu", "Turbidity_NTU",
    # "CTDChlF_mgm3"
    "Cells_L", "CELL_L",
    "Chla_flag", "CPHL_quality_control",
    "Chla_mgm3", "CPHL",
    # "ChlorophyllSatellite_mgm", "CHLA",
    "Comments", "COMMENTS",
    "Conductivity_Sm", "CNDC",
    "Conductivity_flag", "CNDC_quality_control",
    "Copepod", "COPEPOD", ##
    "Date", "SampleDateLocal", ##
    "Density_kgm3", "WaterDensity_kgm3",
    "DIC_Comments", "CARBON_COMMENTS",
    "DIC_Flag", "CARBON_FLAG",
    "DIC_umolkg", "DIC_UMOLKG",
    "Diet", "DIET",
    "DissolvedOxygen_flag", "DOX2_quality_control",
    "DissolvedOxygen_umolkg", "DOX2",
    # "EndLatitude", "ENDLATITUDE",
    # "EndLongitude", "ENDLONGITUDE",
    # "EndSampleDateUTC", "ENDSAMPLEDATEUTC",
    "FlagComments", "FLAG_COMMENT",
    "FlagQC", "QC_FLAG",
    "FovCount", "FOV_COUNT",
    "GearDepth_m", "GEARDEPTH_M",
    "GearMesh_um", "GEARMESH_UM",
    "Genus", "GENUS", ##
    # "InorganicFraction_mgL", "INORGANIC_FRACTION_MG_PER_L", ##***
    "InorganicFraction_mgL", "INORGANICFRACTION_MGL",
    "Latitude", "LATITUDE",
    "Length_mm", "LENGTH_MM",
    "Longitude", "LONGITUDE",
    "Nitrate_Flag", "NITRATE_FLAG",
    "Nitrate_umolL", "NITRATE_UMOLL",
    "Nitrite_Flag", "NITRITE_FLAG",
    "Nitrite_umolL", "NITRITE_UMOLL",
    "Nutrient_Comments", "NUTRIENT_COMMENTS",
    # "OrganicFraction_mgL", "ORGANIC_FRACTION_MG_PER_L",
    "OrganicFraction_mgL", "ORGANICFRACTION_MGL",
    "Oxygen_Flag", "OXYGEN_FLAG",
    "Oxygen_umolL", "OXYGEN_UMOLL",
    "Oxygen_Comments", "OXYGEN_COMMENTS",
    "ParentName", "PARENT_NAME",
    "Phosphate_umolL", "PHOSPHATE_UMOLL",
    "Phosphate_Flag", "PHOSPHATE_FLAG",
    "PhytoAbund_m3", "PHYTO_ABUNDANCE_M3",
    # "PhytoSampleDepth_m", "PHYTOSAMPLEDEPTH_M",
    "Picoeukaryotes_Cellsml", "PICOEUKARYOTES_CELLSML",
    "Picoeukaryotes_Flag", "PICOEUKARYOTES_FLAG",
    "PigmentsComments", "PIGMENTS_COMMENTS",
    "PigmentsFlag", "PIGMENTS_FLAG",
    "Pressure_dbar", "PRES_REL",
    "Prochlorococcus_Cellsml", "PROCHLOROCOCCUS_CELLSML",
    "Prochlorococcus_Flag", "PROCHLOROCOCCUS_FLAG",
    "ProjectName", "PROJECTNAME",
    # "Replicate", "REPLICATE",
    "SPCode", "SPCODE",
    "Salinity_Comments", "SALINITY_COMMENTS",
    "Salinity_Flag", "SALINITY_FLAG",
    "Salinity_psu", "SALINITY_PSU", ## ****
    "SampVol_L", "SAMPVOL_L",
    "SampVol_m3", "SAMPVOL_M3", ###******
    "Sample", "SAMPLE",
    "SampleDateUTC", "SAMPLEDATEUTC",
    "SampleDateLocal", "SAMPLEDATELOCAL",
    "SampleDepth_m", "SAMPLEDEPTH_M",
    # "SampleDepth_m", "DEPTH",
    "SampleType", "SAMPLETYPE",
    "ScientificName", "SCIENTIFICNAME",
    # "Secchi_m", "SECCHIDEPTH_M",
    "Secchi_m", "SECCHI_M",
    "Silicate_Flag", "SILICATE_FLAG",
    "Silicate_umolL", "SILICATE_UMOLL",
    "SizeRef", "SIZEREFERENCE",
    "Species", "SPECIES", ###
    "StartDate", "STARTDATE",
    # "StartDate", "START_DATE",
    # "StartLatitude", "STARTLATITUDE",
    # "StartLongitude", "STARTLONGITUDE",
    # "StartSampleDateUTC", "STARTSAMPLEDATEUTC",
    "StationName", "STATIONNAME",
    "StationCode", "STATIONCODE",
    # "StationDepth_m", "STATIONDEPTH_M",
    "Synecochoccus_Cellsml", "SYNECOCHOCCUS_CELLSML",
    "Synecochoccus_Flag", "SYNECOCHOCCUS_FLAG",
    "TAlkalinity_Comments", "TALKALINITY_COMMENTS",
    "TAlkalinity_Flag", "ALKALINITY_FLAG",
    "TAlkalinity_umolkg", "TALKALINITY_UMOLKG",
    "TSS_Comments", "TSS_COMMENTS",
    "TSS_Flag", "TSS_FLAG",
    # "TSS_mgL", "TSS_MGL",
    # "TSS_mgL", "TSS_MG_PER_L",
    "TSS_mgL", "TSS_MGL",
    "TaxonCount", "TAXON_COUNT",
    "TaxonGroup", "TAXON_GROUP",
    "TaxonName", "TAXON_NAME",
    "Temperature_degC", "TEMPERATURE_C",
    "Temperature_flag", "TEMP_quality_control",
    "TowType", "TOWTYPE",
    "TripCode", "TRIP_CODE",
    "Turbidity_NTU", "TURB",
    "Turbidity_flag", "TURB_quality_control",
    "Vessel", "VESSEL",
    "Volume_m3", "VOLUME_M3",
    "WaterDensity_flag", "DENS_quality_control",
    "WaterDensity_kgm3", "DENS",
    "WaterDepth_m", "DEPTH_M",
    "ZoopAbund_m3", "ZOOP_ABUNDANCE_M3", ###
    # "ZoopSampleDepth_m", "ZOOPSAMPLEDEPTH_M",
    "i_Sample", "I_SAMPLE_ID"),
    ncol = 2, byrow = TRUE, dimnames = list(NULL, c("New","Old"))))
  
  df <- data.table::setnames(df, old = rename_df$Old, new = rename_df$New, skip_absent = TRUE)
  
  return(df)
}
