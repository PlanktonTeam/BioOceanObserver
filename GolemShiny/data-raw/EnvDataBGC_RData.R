## code to prepare `EnvDataBGC.RData` dataset goes here
suppressPackageStartupMessages({
  library(lubridate)
  library(data.table)
  library(sf)
  library(ozmaps)
  library(tidyverse) ## should go near last to put on tip of search path
})

## EnvDataBGC

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


usethis::use_data(NRSBGCEnvData, overwrite = TRUE, internal = FALSE)
