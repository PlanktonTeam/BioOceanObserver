
function(input, output, session) {
  
  library(lubridate)
  library(memoise)
  library(rgeos)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(patchwork)
  library(plotly)
  library(tidyverse)
  
  # source the R scripts with the HeaderTab code
  source("scripts/ZooTsNRS.R")
  source("scripts/EnvDataBGC.R")
  
  ## global options and themes can go here
  theme_set(theme_bw())

  ## only run if selected by tab 
  observe({
    ### NRS time series data  
    if (req(input$navbar) == "Zooplankton")
      ZooTsNRS("one")
  
  ## Env Data - NRS BGC
    if (req(input$navbar) == "Environmental Data")
      EnvDataBGC("one")
  
  })
}

