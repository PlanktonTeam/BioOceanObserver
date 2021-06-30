
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
  source("scripts/ZooSpatial.R")
  
  ## global options and themes can go here
  theme_set(theme_bw(base_size = 10) + theme(legend.position = "bottom"))
  options(na.action = "na.omit")

  ## only run if selected by tab - this should be home page for each Tab level
  observe({
    ### Zooplankton time series data  
    if (req(input$navbar) == "Zooplankton")
      ZooTsNRS("one") # these ids could be more explanatory if we are going to reuse UIs, although that might result in more complication
    
    ## Env Data - NRS BGC
    if (req(input$navbar) == "Environmental Data")
      EnvDataBGC("one")
  
  })

  ## Run when changing page within tab
  ### Zooplankton Spatial data  
  observeEvent(input$zoo, {
    if(input$zoo == "dist"){
    ZooSpatial("one")
  }
  })
  
}

