#' LFishSpatial UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_LFishSpatial_ui <- function(id){
  nsLFishSpatial <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        shiny::HTML("This data originates from......"),
        selectizeInput(inputId = nsLFishSpatial("species"), label = "Select larval fish", choices = unique(LFData$Species2))
        #TODO Add a link to the worms site of the form https://www.marinespecies.org/aphia.php?p=taxdetails&id=110045
        
      ),
      mainPanel(
        leaflet::leafletOutput(nsLFishSpatial("LFMap"), width = "100%") %>% 
          shinycssloaders::withSpinner(color="#0dc5c1"),
        
        )
    )
  )
}
    
#' LFishSpatial Server Functions
#'
#' @noRd 
mod_LFishSpatial_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    LFMapr <- reactive({
      
      req(input$species)
      validate(need(!is.na(input$species), "Error: Please select a species"))
      
      planktonr::pr_plot_LarvalFishDist(LFData, SpeciesName = input$species, interactive = TRUE)
      
    }) %>% bindCache(input$species)
    
    
    output$LFMap <- leaflet::renderLeaflet({
      LFMapr()
    }) %>% bindCache(input$species)
    
 
  })
}
    
## To be copied in the UI
# mod_LFishSpatial_ui("LFishSpatial_1")
    
## To be copied in the server
# mod_LFishSpatial_server("LFishSpatial_1")
