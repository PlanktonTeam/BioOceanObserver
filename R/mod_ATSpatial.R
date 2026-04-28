#' ATSpatial UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ATSpatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ATSpatial Server Functions
#'
#' @noRd 
mod_ATSpatial_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ATSpatial_ui("ATSpatial_1")
    
## To be copied in the server
# mod_ATSpatial_server("ATSpatial_1")
