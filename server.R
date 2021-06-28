
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
  
  # source all the main functions from the IMOS_Toolbox
  source("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/IMOS_Plankton_functions.R")
  # source the R scripts with the HeaderTab code
  source("scripts/ZooTsNRS.R")
  source("scripts/EnvDataBGC.R")
  source("scripts/ZooSpatial.R")
  
  ## global options and themes can go here
  theme_set(theme_bw(base_size = 10) + theme(legend.position = "bottom"))
  options(na.action = "na.omit")

  ## only run if selected by tab 
  observe({
    ### Zooplankton time series data  
    if (req(input$navbar) == "Zooplankton")
      ZooTsNRS("one")
    
    ### Zooplankton Spatial data  
    if (req(input$navbar) == "Zooplankton")
      ZooSpatial("one")
    
    ## Env Data - NRS BGC
    if (req(input$navbar) == "Environmental Data")
      EnvDataBGC("one")
  
  })
}

