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
                                     choices = unique(pkg.env$LFData$Species), width = "100%",
                                     options = list(dropdownParent = 'body')),
        )),
      shiny::fluidRow(
        shiny::column(width = 6,
                      class = "col-no-spacing",
                      shiny::h4("December - February"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapSum"), width = "99%", height = "300px") %>% 
                        shinycssloaders::withSpinner(color="#0dc5c1")), 
        shiny::column(width = 6,
                      class = "col-no-spacing",
                      shiny::h4("March - May"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapAut"), width = "99%", height = "300px") %>%
                        shinycssloaders::withSpinner(color="#0dc5c1")
                      )
      ),
      shiny::fluidRow(
        shiny::column(width = 6,
                      class = "col-no-spacing",
                      shiny::h4("June - August"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapWin"), width = "99%", height = "300px") %>% 
                        shinycssloaders::withSpinner(color="#0dc5c1")), 
        shiny::column(width = 6,
                      class = "col-no-spacing",
                      shiny::h4("September - November"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapSpr"), width = "99%", height = "300px") %>% 
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
      dat <- pkg.env$LFData %>%
        dplyr::filter(.data$Species == input$species) %>%
        dplyr::distinct(.data$Latitude, .data$Longitude, .keep_all = TRUE) %>% 
        dplyr::mutate(Season = dplyr::case_when(.data$Month_Local >= 3 &.data$Month_Local <= 5 ~ "March - May",
                                                .data$Month_Local >= 6 &.data$Month_Local <= 8 ~ "June - August",
                                                .data$Month_Local >= 9 &.data$Month_Local <= 11 ~ "September - November",
                                                TRUE ~ "December - February"))
      return(dat)
    }) %>% bindCache(input$species)
    
    
    # Helper to build a complete seasonal map (base + species dots + legend)
    makeSeasonMap <- function(dat_all, season_label) {
      sdf <- dat_all %>% dplyr::filter(.data$Season == season_label)
      Species <- unique(dat_all$Species)
      labs <- lapply(seq(nrow(sdf)), function(i) {
        paste("<strong>Date:</strong>", sdf$SampleTime_Local[i], "<br>",
              "<strong>Latitude:</strong>", sdf$Latitude[i], "<br>",
              "<strong>Longitude:</strong>", sdf$Longitude[i], "<br>",
              "<strong>Count:</strong>", sdf$Count[i], "<br>",
              "<strong>Abundance (1000 m\u207B\u00B3):</strong>", round(sdf$Abundance_1000m3[i], digits = 2), "<br>",
              "<strong>Temperature (\u00B0C):</strong>", sdf$Temperature_degC[i], "<br>",
              "<strong>Depth (m):</strong>", sdf$SampleDepth_m[i], "<br>")
      })
      LeafletBase(dat_all) %>%
        leaflet::addCircleMarkers(data = sdf,
                                  lng = ~ Longitude,
                                  lat = ~ Latitude,
                                  color = 'blue',
                                  opacity = 1,
                                  fillOpacity = 1,
                                  radius = 2,
                                  group = "Present",
                                  label = lapply(labs, htmltools::HTML)) %>%
        leaflet::addLegend("bottomleft",
                           colors = c("blue", "#CCCCCC"),
                           labels = c("Seasonal Presence", "Seasonal Absence"),
                           title = Species,
                           opacity = 1)
    }

    # Summer
    output$LFMapSum <- leaflet::renderLeaflet({
      makeSeasonMap(LFDatar(), "December - February")
    }) %>% bindCache(input$species, "sum")

    # Autumn
    output$LFMapAut <- leaflet::renderLeaflet({
      makeSeasonMap(LFDatar(), "March - May")
    }) %>% bindCache(input$species, "aut")

    # Winter
    output$LFMapWin <- leaflet::renderLeaflet({
      makeSeasonMap(LFDatar(), "June - August")
    }) %>% bindCache(input$species, "win")

    # Spring
    output$LFMapSpr <- leaflet::renderLeaflet({
      makeSeasonMap(LFDatar(), "September - November")
    }) %>% bindCache(input$species, "spr")
    
  })
}

## To be copied in the UI
# mod_LFishSeason_ui("LFishSeason_1")

## To be copied in the server
# mod_LFishSeason_server("LFishSeason_1")
