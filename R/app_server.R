#' The application server-side
#' 
#' @param input,output,session Internal Parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom rlang .data
#' @noRd
#' 
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  ## global options and themes can go here
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 12) + ggplot2::theme(legend.position = "bottom")) 
  options(na.action = "na.omit")
  
  ## only run if selected by tab - this should be home page for each Tab level
  ### Snapshot page
  observeEvent(input$navbar, {
    
    if(input$navbar == "Home") {
      mod_home_server("home_1") 
    }
    
    if(input$navbar == "EOVs") {
      mod_PolNRS_server("PolNRS_ui_1")
    }
    
    if(input$navbar == "Microbes") {
      mod_MicroTsNRS_server("MicroTsNRS_ui_1")
    }
    
    if(input$navbar == "Phytoplankton") {
      mod_PhytoTsNRS_server("PhytoTsNRS_ui_1")
    }
    
    if(input$navbar == "Zooplankton") {
      mod_ZooTsNRS_server("ZooTsNRS_ui_1")
    }
    
    if(input$navbar == "Larval Fish") {
      mod_LFishSpatial_server("LFishSpatial_1")
    }
    
    if(input$navbar == "Environmental Data") {
      mod_NutrientsBGC_server("NutrientsBGC_ui_1")
    }
    
    if(input$navbar == "Information") {
      mod_info_server("info_1")
    }
  })
  
  
  ## Run when changing page within tab
  
  
  # EOVs --------------------------------------------------------------------
  
  observeEvent(input$pol, {
    
    ### Policy CPR time series data  
    if(input$pol == "cpr"){
      mod_PolCPR_server("PolCPR_ui_1")
    }
    
    if(input$pol == "LTM"){
      mod_PolLTM_server("PolLTM_ui_1")
    }
    
  })
  
  
  # Microbes ----------------------------------------------------------------
  
  
  
  # Phytoplankton -----------------------------------------------------------
  
  observeEvent(input$phyto, {
    
    ### Phytoplankton CPR time series data  
    if(input$phyto == "ptscpr"){
      mod_PhytoTsCPR_server("PhytoTsCPR_ui_1")
    }
    
    ### Phytoplankton Spatial data  
    if(input$phyto == "distp"){
      mod_PhytoSpatial_server("PhytoSpatial_ui_1")
    }
    
  })
  
  
  # Zooplankton -------------------------------------------------------------
  
  observeEvent(input$zoo, {
    
    ### Zooplankton CPR time series data
    if(input$zoo == "ztscpr"){
      mod_ZooTsCPR_server("ZooTsCPR_ui_1")
    }
    
    ### Zooplankton Spatial data  
    if(input$zoo == "dist"){
      mod_ZooSpatial_server("ZooSpatial_ui_1")
    }
  })
  
  
  
  # Larval Fish -------------------------------------------------------------
  
  # Season Larval Fish
  observeEvent(input$fish, {
    if(input$fish == "fseas"){
      mod_LFishSeason_server("LFishSeason_1")
    }
    if(input$fish == "fdata"){
      mod_LFishData_server("LFishData_1")
    }
  })
  
  
  
  # Environmental -----------------------------------------------------------
  
  observeEvent(input$env, {
    
    ### Pigments 
    if(input$env == "pigs"){
      mod_PigmentsBGC_server("PigmentsBGC_ui_1")
    }
  })
  
  # Picoplankton
  observeEvent(input$env, {
    if(input$env == "pico"){
      mod_PicoBGC_server("PicoBGC_ui_1")
    }
  })
  
  # Nutrient data
  observeEvent(input$env, {
    if(input$env == "water"){
      mod_WaterBGC_server("WaterBGC_ui_1")
    }
  })
  
  # Moorings
  observeEvent(input$env, {
    if(input$env == "moor"){
      mod_MoorBGC_server("MoorBGC_ui_1")
    }
  })
}