#' LFishSeason UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_LFishSeason_ui <- function(id){
  nsLFishSeason <- NS(id)
  
  tagList(
    shiny::fluidPage(    
      shiny::fluidRow(
        shiny::column(width = 6, offset = 6,
                      selectizeInput(inputId = nsLFishSeason("species"), label = NULL, 
                                     choices = unique(LFData$Species2), width = "100%",
                                     options = list(dropdownParent = 'body')),
        )),
      shiny::fluidRow(
        shiny::column(6, shiny::h4("Summer"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapSum"), width = "90%", height = "250px") %>% 
                        shinycssloaders::withSpinner(color="#0dc5c1")), 
        shiny::column(6, shiny::h4("Autumn"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapAut"), width = "90%", height = "250px") %>% 
                        shinycssloaders::withSpinner(color="#0dc5c1"))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::h4("Winter"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapWin"), width = "90%", height = "250px") %>% 
                        shinycssloaders::withSpinner(color="#0dc5c1")), 
        shiny::column(6, shiny::h4("Spring"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapSpr"), width = "90%", height = "250px") %>% 
                        shinycssloaders::withSpinner(color="#0dc5c1"))
      )
      
    )
  )
}

#' LFishSeason Server Functions
#'
#' @noRd 
mod_LFishSeason_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    LFDatar <- reactive({
      dat <- LFData %>%
        dplyr::filter(.data$Species2 == input$species & .data$Count > 0) %>% 
        dplyr::distinct(.data$Latitude, .data$Longitude, .keep_all = TRUE) %>% 
        dplyr::mutate(Season = dplyr::case_when(.data$Month_Local >= 3 &.data$Month_Local <= 5 ~ "Autumn",
                                                .data$Month_Local >= 6 &.data$Month_Local <= 8 ~ "Winter",
                                                .data$Month_Local >= 9 &.data$Month_Local <= 11 ~ "Spring",
                                                TRUE ~ "Summer"))
      return(dat)
    }) %>% bindCache(input$species)
    
    
    LFLeaflet <- function(df){
      leaflet::leaflet(df %>% 
                         dplyr::distinct(.data$Latitude, .data$Longitude)) %>%
        leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>% 
        leaflet::setMaxBounds(~110, ~-45, ~160, ~-10) %>%
        leaflet::addCircleMarkers(lng = ~ Longitude,
                                  lat = ~ Latitude,
                                  color = "grey",
                                  opacity = 1,
                                  fillOpacity = 1,
                                  radius = 0.5, 
                                  group = "Absent") %>% 
        leaflet::addLegend("bottomleft", 
                           colors = c("blue",  "grey"),
                           labels = c("Seasonal Presence", "Seasonal Absence"),
                           title = input$Species2,
                           opacity = 1)
    }
    
    
    LFLeaflet_obs <- function(sdf, name){
      leaflet::leafletProxy(name, data = sdf) %>%
        leaflet::setMaxBounds(~110, ~-45, ~160, ~-10) %>%
        leaflet::clearGroup("Present") %>%
        leaflet::addCircleMarkers(data = sdf, 
                                  lng = ~ Longitude,
                                  lat = ~ Latitude,
                                  color = "blue",
                                  opacity = 1,
                                  fillOpacity = 1,
                                  radius = 2,
                                  group = "Present")
    }
    
    
    # Summer
    output$LFMapSum <- leaflet::renderLeaflet({
      lf <- LFLeaflet(LFDatar())
      return(lf)
    })
    
    # Autumn
    output$LFMapAut <- leaflet::renderLeaflet({
      lf <- LFLeaflet(LFDatar())
      return(lf)
    })
    
    # Winter
    output$LFMapWin <- leaflet::renderLeaflet({
      lf <- LFLeaflet(LFDatar())
      return(lf)
    })
    
    # Spring
    output$LFMapSpr <- leaflet::renderLeaflet({
      lf <- LFLeaflet(LFDatar())
      return(lf)
    })
    
    
    
    # Add points for chosen larval fish by season
    observe({
      LFLeaflet_obs(sdf = LFDatar() %>% filter(Season == "Summer"), name = "LFMapSum")
      LFLeaflet_obs(sdf = LFDatar() %>% filter(Season == "Autumn"), name = "LFMapAut")
      LFLeaflet_obs(sdf = LFDatar() %>% filter(Season == "Winter"), name = "LFMapWin")
      LFLeaflet_obs(sdf = LFDatar() %>% filter(Season == "Spring"), name = "LFMapSpr")
      
    })
    
  })
}

## To be copied in the UI
# mod_LFishSeason_ui("LFishSeason_1")

## To be copied in the server
# mod_LFishSeason_server("LFishSeason_1")