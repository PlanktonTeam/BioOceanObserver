#' ATCases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ATCases_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ATCases Server Functions
#'
#' @noRd 
mod_ATCases_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ATCases_ui("ATCases_1")
    
## To be copied in the server
# mod_ATCases_server("ATCases_1")
