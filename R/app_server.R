#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom rlang .data
#' @importFrom stats sd
#' @importFrom utils write.table 
#' @importFrom ggplot2 ggplot aes element_blank element_rect element_text facet_grid facet_wrap 
#' geom_blank geom_col geom_errorbar geom_line geom_point geom_sf geom_smooth geom_text ggsave
#' labs position_dodge 
#' scale_colour_manual scale_fill_manual scale_x_continuous scale_x_datetime scale_y_continuous 
#' theme theme_set theme_bw theme_minimal theme_void unit
#' @importFrom plotly ggplotly layout plotlyOutput renderPlotly style subplot 
#' @importFrom dplyr filter group_by inner_join left_join mutate n select summarise ungroup
#' @importFrom tidyr drop_na
#' @importFrom shinycssloaders withSpinner
#' @noRd
#' 
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  ## global options and themes can go here
  theme_set(theme_bw(base_size = 10) + theme(legend.position = "bottom")) 
  options(na.action = "na.omit")

    ## only run if selected by tab - this should be home page for each Tab level
  observe({
    ### Zooplankton time series data  
    if (req(input$navbar) == "Zooplankton")  
      mod_ZooTsNRS_server("ZooTsNRS_ui_1")
    
    ### Phytoplankton time series data  
    if (req(input$navbar) == "Phytoplankton")  
      mod_PhytoTsNRS_server("PhytoTsNRS_ui_1")
    
    ### Env Data BGC
    if (req(input$navbar) == "Environmental Data")
      mod_NutrientsBGC_server("NutrientsBGC_ui_1")
  })
  
    ## Run when changing page within tab
    ### Zooplankton CPR time series data
    observeEvent(input$zoo, {
      if(input$zoo == "ztscpr"){
        mod_ZooTsCPR_server("ZooTsCPR_ui_1")
      }
    })
    ### Phytoplankton CPR time series data
    observeEvent(input$phyto, {
      if(input$phyto == "ptscpr"){
        mod_PhytoTsCPR_server("PhytoTsCPR_ui_1")
      }
    })
    
    ### Zooplankton Spatial data  
    observeEvent(input$zoo, {
      if(input$zoo == "dist"){
        mod_ZooSpatial_server("ZooSpatial_ui_1")
      }
    })
    
    ### Zooplankton Spatial data  
    observeEvent(input$env, {
        if(input$env == "pigs"){
          mod_PigmentsBGC_server("PigmentsBGC_ui_1")
        }
        
    })
}
