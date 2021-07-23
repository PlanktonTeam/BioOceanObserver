#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom ggplot2 ggplot aes geom_line geom_sf geom_point scale_x_datetime theme geom_col geom_errorbar labs 
#' scale_x_continuous scale_y_continuous theme_void element_blank element_rect position_dodge facet_wrap
#' @importFrom plotly ggplotly layout renderPlotly plotlyOutput subplot style
#' @importFrom dplyr summarise filter select group_by left_join inner_join ungroup mutate
#' @importFrom tidyr drop_na
#' @importFrom shinycssloaders withSpinner
#' @noRd
#' 
app_server <- function( input, output, session ) {
  # Your application server logic 

    ## only run if selected by tab - this should be home page for each Tab level
  observe({
    ### Zooplankton time series data  
    if (req(input$navbar) == "Zooplankton")  
      mod_ZooTsNRS_server("ZooTsNRS_ui_1")
  })
  
    ## Run when changing page within tab
    ### Zooplankton Spatial data  
    observeEvent(input$zoo, {
      if(input$zoo == "dist"){
        mod_ZooSpatial_server("ZooSpatial_ui_1")
      }
    })
}
