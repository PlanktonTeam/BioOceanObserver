#' EnvDataBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_EnvDataBGC_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' EnvDataBGC Server Functions
#'
#' @noRd 
mod_EnvDataBGC_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_EnvDataBGC_ui("EnvDataBGC_ui_1")
    
## To be copied in the server
# mod_EnvDataBGC_server("EnvDataBGC_ui_1")
