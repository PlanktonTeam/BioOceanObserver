#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  library(lubridate)
  library(memoise)
  library(rgeos)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(patchwork)
  library(plotly)
  library(tidyverse)
  ## only run if selected by tab - this should be home page for each Tab level
  observe({
    ### Zooplankton time series data  
    if (req(input$navbar) == "Zooplankton")  
      mod_ZooTsNRS_server("ZooTsNRS_ui_1")
  })
}
