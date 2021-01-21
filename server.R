
function(input, output, session) {
  
  library(lubridate)
  library(rgeos)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(patchwork)
  library(plotly)
  library(tidyverse)
  
  # source the R scripts with the HeaderTab code
  source("ZooTsNRS.R")
  source("EnvDataBGC.R")
  
  ## global options and themes can go here
  theme_set(theme_bw())
  
  ## run the code only if the tab panel is selected
#  observe({
  ### NRS time series data  
#  if(req(input$navbar) == "Zooplankton")
    ZooTsNRS("one")
  
  ## Env Data - NRS BGC
#  if(req(input$navbar) == "Environmental Data")
    EnvDataBGC("one")
#  })
}

