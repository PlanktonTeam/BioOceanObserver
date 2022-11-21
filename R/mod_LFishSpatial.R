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
    shiny::fluidPage(    
      shiny::fluidRow(
        # shiny::column(width = 3, shiny::HTML("<strong>Select Larval Fish:</strong>")),
        shiny::column(width = 6, offset = 6,
                      selectizeInput(inputId = nsLFishSpatial("species"), label = NULL, choices = unique(LFData$Species2), width = "100%"),
        )),
      shiny::fluidRow(
        leaflet::leafletOutput(nsLFishSpatial("LFMap"), width = "100%", height = "700px") %>% 
          shinycssloaders::withSpinner(color="#0dc5c1"),
      ),
    )
  )
}

#' LFishSpatial Server Functions
#'
#' @noRd 
mod_LFishSpatial_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    LFDatar <- reactive({
      
      dat <- LFData %>%
        dplyr::filter(.data$Species2 == input$species & .data$Count > 0) %>% 
        dplyr::distinct(.data$Latitude, .data$Longitude, .keep_all = TRUE)
      
      return(dat)
    }) %>% bindCache(input$species)
    
    # Render basemap
    output$LFMap <- leaflet::renderLeaflet({
      
      leaflet::leaflet(LFData %>% 
                         dplyr::distinct(.data$Latitude, .data$Longitude)) %>%
        leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>% 
        leaflet::setMaxBounds(~110, ~-45, ~160, ~-10) %>%
        leaflet::addCircleMarkers(lng = ~ Longitude,
                                  lat = ~ Latitude,
                                  color = "grey",
                                  opacity = 1,
                                  fillOpacity = 1,
                                  radius = 2)
      
    })
    
    
    # Add points for chosen larval fish
    leaflet::observe({
      
      leaflet::leafletProxy("LFMap", data = LFDatar()) %>%
        leaflet::setMaxBounds(~110, ~-45, ~160, ~-10) %>%
        leaflet::removeMarker(layerId = "Present") %>%
        leaflet::addCircleMarkers(data = LFDatar(), 
                                  lng = ~ Longitude,
                                  lat = ~ Latitude,
                                  color = "blue",
                                  opacity = 1,
                                  fillOpacity = 1,
                                  radius = 5,
                                  layerId = "Present")
    })
    
  })
}

## To be copied in the UI
# mod_LFishSpatial_ui("LFishSpatial_1")

## To be copied in the server
# mod_LFishSpatial_server("LFishSpatial_1")
