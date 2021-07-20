## Data preparation for server scripts
## These functions / routines to be automated by GitHub Actions to keep the source data updated

suppressPackageStartupMessages({
  library(lubridate)
  library(data.table)
  library(sf)
  library(ozmaps)
  library(tidyverse) ## should go near last to put on tip of search path
})

# source all the main functions from the IMOS_Toolbox, this is the default place for the package functions for now
source("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/IMOS_Plankton_functions.R", local = FALSE)

## EnvDataBGC

DataPrepEnvBGC <- memoise(function(){
  NRSBGCEnvData <- read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/Output/NRS_CombinedWaterQuality.csv",
                            col_types =  cols(SampleDepth_m = col_character())) %>% 
    mutate(TotalChla = CPHLIDE_A + DV_CPHL_A + CPHL_A,
           TotalChl = CPHLIDE_A + DV_CPHL_A + CPHL_A + DV_CPHL_B + CPHL_B + CPHL_C3 + CPHL_C2 + CPHL_C1,
           PPC = ALLO + DIADCHR + DIADINO + DIATO + ZEA, #+ CARO, #Photoprotective Carotenoids
           PSC = BUT_FUCO + HEX_FUCO + PERID, #Photosynthetic Carotenoids
           PSP = PSC + TotalChl, #Photosynthetic pigments
           TCaro = PSC + PSP, #Total Carotenoids
           TAcc = TCaro + DV_CPHL_B + CPHL_B + CPHL_C3 + CPHL_C2 + CPHL_C1, #Total Accessory pigments
           TPig = TAcc + TotalChla, #Total pigments
           TDP = PSC + ALLO + ZEA + DV_CPHL_B + CPHL_B) %>% #Total Diagnostic pigments
    select(TripCode:Picoeukaryotes_cellsml, TSS_mgL:TDP) %>%
    pivot_longer(-c(TripCode:SampleDateUTC, IMOSsampleCode)) %>% drop_na()
} )

NRSBGCEnvData <- DataPrepEnvBGC()
save(NRSBGCEnvData, file = "data/envdatabgc.RData")


## ZooSpatial.R

ZooCountNRS <- getNRSZooCount() %>%
  rename(Sample = TripCode, Counts = TaxonCount) %>%
  filter(Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species) & Genus != '') %>% 
  mutate(Taxon = paste0(word(Genus,1), " ", word(Species,1)), 
         Survey = 'NRS') %>%
  group_by(Sample, Survey, Taxon, SampVol_L) %>% summarise(Counts = sum(Counts, na.rm = TRUE), .groups = "drop")

ZooCountCPR <- getCPRZooCount() %>% 
  rename(Counts = TaxonCount) %>%
  filter(Species != "spp." & !is.na(Species) & !grepl("cf.", Species) & !grepl("grp", Species) & Genus != '') %>% 
  mutate(Taxon = paste0(word(Genus,1), " ", word(Species,1)), 
         Survey = 'CPR') %>%
  group_by(Sample, Survey, Taxon, SampVol_L) %>% summarise(Counts = sum(Counts, na.rm = TRUE), .groups = "drop")

obs <- rbind(ZooCountCPR, ZooCountNRS) %>% arrange(Taxon)
fwrite(obs, "data/obs.rds")

NRSSamp <- getNRSTrips() %>%
  rename(Sample = TripCode, Date = SampleDateLocal) %>%
  mutate(DOY = yday(Date),
         Start = as.Date(paste0(min(year(Date))-1, "-12-31")),
         days = difftime(as.Date(Date), Start, units = "days") %>% as.numeric(),
         thetadoy = (days %% 365.25)/365.25 * 2 * base::pi, ## leap years...
         Survey = 'NRS')  %>% 
  select(Sample, Survey, Date, DOY, Latitude, Longitude, thetadoy, SampleType) 

CPRSamp <- getCPRTrips() %>% 
  rename(Date = SampleDateUTC) %>%
  mutate(DOY = yday(Date),
         Start = as.Date(paste0(min(year(Date))-1, "-12-31")),
         days = difftime(as.Date(Date), Start, units = "days") %>% as.numeric(),
         thetadoy = (days %% 365.25)/365.25 * 2 * base::pi, ## leap years...
         Survey = 'CPR')  %>% 
  select(Sample, Survey, Date, DOY, Latitude, Longitude, thetadoy, SampleType)

SampLocs <- rbind(CPRSamp %>% filter(grepl("Z", SampleType)), NRSSamp %>% filter(grepl("Z", SampleType))) %>%
  mutate(Lat = round(Latitude), #/0.5, 0)*0.5,
         Long = round(Longitude), #/0.5, 0)*0.5,
         Month = month(Date),
         Season = ifelse(Month >2 & Month < 6, "March - May",
                         ifelse(Month >5 & Month < 9, "June - August",
                                ifelse(Month > 8 & Month < 12, "September - November", "December - February")))) %>% 
  select(Sample, Survey, Lat, Long, Season) %>% untibble()

Samples <- SampLocs %>%  group_by(Lat, Long, Season) %>% summarise(samples = n()) %>% untibble()

aus <- ozmap()

absences <-  Samples[1:3] %>% mutate(Taxon = "Taxon", freqsamp = 0, freqfac = as.factor("Absent")) %>%
  untibble()

save(obs, Samples, SampLocs, absences, aus, file = "data/zoospatial.RData")

## ZooTSNRS.R

datNRSi <- read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/Output/NRS_Indices.csv") %>% 
  mutate(Month = month(SampleDateLocal),
         Year = year(SampleDateLocal),
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
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

save(datNRSi, meta_sf, file = "data/zootsnrs.RData")
