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
                      selectizeInput(inputId = nsLFishSpatial("species"), label = NULL, choices = unique(pkg.env$LFData$Species), width = "100%"),
        )),
      shiny::fluidRow(
        leaflet::leafletOutput(nsLFishSpatial("LFMap"), width = "100%", height = "800px") %>% 
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
      
      dat <- pkg.env$LFData %>%
        dplyr::filter(.data$Species == input$species)# %>% 
        # dplyr::distinct(.data$Latitude, .data$Longitude, .keep_all = TRUE)
      
      return(dat)
      
    }) %>% bindCache(input$species)
    
    # Render complete map: grey background dots + blue species dots
    output$LFMap <- leaflet::renderLeaflet({
      sdf <- LFDatar()
      labs_fish <- lapply(seq(nrow(sdf)), function(i) {
        paste("<strong>Date:</strong>", sdf$SampleTime_Local[i], "<br>",
              "<strong>Latitude:</strong>", sdf$Latitude[i], "<br>",
              "<strong>Longitude:</strong>", sdf$Longitude[i], "<br>",
              "<strong>Count:</strong>", sdf$Count[i], "<br>",
              "<strong>Abundance (1000 m\u207B\u00B3):</strong>", round(sdf$Abundance_1000m3[i], digits = 2), "<br>",
              "<strong>Temperature (\u00B0C):</strong>", sdf$Temperature_degC[i], "<br>",
              "<strong>Depth (m):</strong>", sdf$SampleDepth_m[i], "<br>")
      })

      leaflet::leaflet(pkg.env$LFDataAbs %>%
                         dplyr::distinct(.data$Latitude, .data$Longitude)) %>%
        leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>%
        leaflet::addCircleMarkers(lng = ~ Longitude,
                                  lat = ~ Latitude,
                                  color = "grey",
                                  opacity = 1,
                                  fillOpacity = 1,
                                  radius = 2) %>%
        leaflet::addCircleMarkers(data = sdf,
                                  lng = ~ Longitude,
                                  lat = ~ Latitude,
                                  color = "blue",
                                  opacity = 1,
                                  fillOpacity = 1,
                                  radius = 5,
                                  group = "Present",
                                  label = lapply(labs_fish, htmltools::HTML))
    })
    
  })
}

## To be copied in the UI
# mod_LFishSpatial_ui("LFishSpatial_1")

## To be copied in the server
# mod_LFishSpatial_server("LFishSpatial_1")
