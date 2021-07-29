#' ZooTsCPR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZooTsCPR_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ZooTsCPR Server Functions
#'
#' @noRd 
mod_ZooTsCPR_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ZooTsCPR_ui("ZooTsCPR_ui_1")
    
## To be copied in the server
# mod_ZooTsCPR_server("ZooTsCPR_ui_1")
