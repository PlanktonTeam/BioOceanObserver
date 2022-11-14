#' Snapshot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom stats runif
mod_Snapshot_ui <- function(id){
  nsSnap <- NS(id)
  tagList(
    fluidRow(
      leaflet::leafletOutput(nsSnap("progplot"), height = 800) %>% 
        shinycssloaders::withSpinner())
  )
}

#' Snapshot Server Functions
#'
#' @noRd 
mod_Snapshot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$nsSnap
    output$progplot <- leaflet::renderLeaflet({
      planktonr::pr_plot_ProgressMap(PMapData2, interactive = TRUE)
      
    })
  })
}
