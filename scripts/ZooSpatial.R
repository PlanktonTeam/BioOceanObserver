## Zooplankton
# Spatial

suppressPackageStartupMessages({
  library(lubridate)
  library(sf)
  library(ozmaps)
  library(tidyverse) ## should go near last to put on tip of search path
})

## Prepare data

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

obs <- rbind(ZooCountCPR, ZooCountNRS)

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

SampLocs <- rbind(CPRSamp %>% filter(grepl("Z", SampleType)), NRSSamp %>% filter(grepl("Z", SampleType)))

PlotDotMaps <- function(SampLocs, obs) {
  SampLocss <- SampLocs %>%
    mutate(Lat = round(Latitude), #/0.5, 0)*0.5,
           Long = round(Longitude), #/0.5, 0)*0.5,
           Month = month(Date),
           Season = ifelse(Month >2 & Month < 6, "March - May",
                           ifelse(Month >5 & Month < 9, "June - August",
                                  ifelse(Month > 8 & Month < 12, "September - November", "December - February")))) %>% 
    select(Sample, Survey, Lat, Long, Season) %>% untibble()
  
  Samples <- SampLocss %>%  group_by(Lat, Long, Season) %>% summarise(samples = n()) %>% untibble()
  
  freq <- obs[c(1,3,4)] %>% left_join(SampLocss, by="Sample") %>%
    drop_na() %>%
    group_by(Season, Taxon, Lat, Long) %>%
    summarise(freq = n()) %>%
    left_join(Samples, by=c("Lat", "Long", "Season")) %>% 
    ungroup() %>%
    mutate(freqsamp = freq/samples, 
           freqfac = as.factor(ifelse(freqsamp<0.375, "Seen in 25%", 
                                      ifelse(freqsamp>0.875, "100 % of Samples", 
                                             ifelse(freqsamp>0.375 & freqsamp<0.625, '50%', '75%')))),
           Season = as.factor(Season),
           Taxon = as.factor(Taxon)) %>%
    select(Season, Lat, Long, Taxon, freqsamp, freqfac) %>%
    untibble()
  
  freq$Season <- factor(freq$Season, levels(freq$Season)[c(1, 3, 2, 4)])
  
  aus <- ozmap()
  
  absences <-  Samples[1:3] %>% mutate(Taxon = "Taxon", freqsamp = 0, freqfac = as.factor("Absent")) %>%
    untibble()
  
  n <- nlevels(freq$Taxon)
  
  cols <- c("lightblue1" ,"skyblue3", "dodgerblue2","blue1", "navyblue")
  colscale <- scale_colour_manual(name = '', values = cols, drop = FALSE)
  
  for (i in 1:n) {
    Title <- levels(freq$Taxon)[i]
    data  <-  freq %>% filter(Taxon == Title) %>% droplevels("freqfac")
    data <- absences %>% mutate(Taxon = Title) %>% rbind(data)
    data$freqfac <- factor(data$freqfac, levels(data$freqfac)[c(1, 5, 3, 4, 2)])
    map <- ggplot() + geom_sf(data = aus) + 
      geom_point(data=data, aes(x=Long, y=Lat, colour=freqfac), size = 2) + facet_wrap( ~ Season, dir = "v") +
      theme_bw(base_size=14) + labs(title = Title) + colscale +
      theme(strip.background = element_blank(), legend.position = "bottom",
            title = element_text(face = "italic"),
            legend.title = element_text(face = "plain"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_rect(fill = 'snow1')) 
    
  }
}

# function for UI module

ZooSpatialUI <- function(id){
  
  nsZooSpatial <- NS(id)
  
  tagList(
    sidebarPanel(
      # Species selector
      selectizeInput(inputId = nsZooSpatial('Species'), label = "Select a species", choices = unique(obs$Taxon))
      ),
    mainPanel(
        
      )
  )
}

# function for server

ZooSpatial <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
      
    }
  )
}
