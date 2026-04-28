#' ATStats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ATStats_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ATStats Server Functions
#'
#' @noRd 
mod_ATStats_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ATStats_ui("ATStats_1")
    
## To be copied in the server
# mod_ATStats_server("ATStats_1")
