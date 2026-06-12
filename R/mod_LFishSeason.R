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
  ns <- NS(id)
  
  tagList(
    shiny::fluidPage(    
      shiny::fluidRow(
        shiny::column(width = 6, offset = 6,
                      selectizeInput(inputId = ns("species"), label = NULL, 
                                     choices = unique(pkg.env$LFData$Species), width = "100%",
                                     options = list(dropdownParent = 'body')),
        )),
      shiny::fluidRow(
        shiny::column(width = 6,
                      class = "col-no-spacing",
                      shiny::h4("December - February"),
                      mapgl::mapboxglOutput(ns("LFMapSum"), width = "99%", height = "300px") %>% 
                        shinycssloaders::withSpinner(color="#0dc5c1")), 
        shiny::column(width = 6,
                      class = "col-no-spacing",
                      shiny::h4("March - May"),
                      mapgl::mapboxglOutput(ns("LFMapAut"), width = "99%", height = "300px") %>%
                        shinycssloaders::withSpinner(color="#0dc5c1")
                      )
      ),
      shiny::fluidRow(
        shiny::column(width = 6,
                      class = "col-no-spacing",
                      shiny::h4("June - August"),
                      mapgl::mapboxglOutput(ns("LFMapWin"), width = "99%", height = "300px") %>% 
                        shinycssloaders::withSpinner(color="#0dc5c1")), 
        shiny::column(width = 6,
                      class = "col-no-spacing",
                      shiny::h4("September - November"),
                      mapgl::mapboxglOutput(ns("LFMapSpr"), width = "99%", height = "300px") %>% 
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
    
    
    # Helper to build a complete seasonal mapboxgl map (absence + presence layers + legend)
    makeSeasonMap <- function(dat_all, season_label) {
      sdf <- dat_all %>% dplyr::filter(.data$Season == season_label)
      Species <- unique(dat_all$Species)
      
      # Build HTML popup column for presence points
      sdf <- sdf %>%
        dplyr::mutate(
          popup_html = paste0(
            "<strong>Date:</strong> ", .data$SampleTime_Local, "<br>",
            "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
            "<strong>Longitude:</strong> ", .data$Longitude, "<br>",
            "<strong>Count:</strong> ", .data$Count, "<br>",
            "<strong>Abundance (1000 m\u207B\u00B3):</strong> ", round(.data$Abundance_1000m3, digits = 2), "<br>",
            "<strong>Temperature (\u00B0C):</strong> ", .data$Temperature_degC, "<br>",
            "<strong>Depth (m):</strong> ", .data$SampleDepth_m
          )
        )
      
      # Convert to sf objects (WGS84)
      # NOTE: a non-geometry property column (id) is required so that
      # geojsonsf::sf_geojson() produces a FeatureCollection (length 1)
      # rather than individual geometry strings (length n).
      abs_sf <- dat_all %>%
        dplyr::distinct(.data$Latitude, .data$Longitude) %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
      
      pres_sf <- sdf %>%
        sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
      
      mapgl::mapboxgl(
        access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
        style        = mapgl::mapbox_style("light"),
        center       = c(134.0, -27.0),
        zoom         = 3.0,
        projection   = "mercator"
      ) %>%
        mapgl::add_circle_layer(
          id             = "absence",
          source         = abs_sf,
          circle_color   = "#CCCCCC",
          circle_opacity = 1,
          circle_radius  = 2
        ) %>%
        mapgl::add_circle_layer(
          id             = "presence",
          source         = pres_sf,
          circle_color   = "blue",
          circle_opacity = 1,
          circle_radius  = 3,
          popup          = "popup_html"
        ) %>%
        mapgl::add_categorical_legend(
          legend_title = "",
          values       = c("Seasonal Presence", "Seasonal Absence"),
          colors       = c("blue", "#CCCCCC"),
          position     = "bottom-left"
        )
    }

    # Summer
    output$LFMapSum <- mapgl::renderMapboxgl({
      makeSeasonMap(LFDatar(), "December - February")
    }) %>% bindCache(input$species, "sum")

    # Autumn
    output$LFMapAut <- mapgl::renderMapboxgl({
      makeSeasonMap(LFDatar(), "March - May")
    }) %>% bindCache(input$species, "aut")

    # Winter
    output$LFMapWin <- mapgl::renderMapboxgl({
      makeSeasonMap(LFDatar(), "June - August")
    }) %>% bindCache(input$species, "win")

    # Spring
    output$LFMapSpr <- mapgl::renderMapboxgl({
      makeSeasonMap(LFDatar(), "September - November")
    }) %>% bindCache(input$species, "spr")
    
  })
}

## To be copied in the UI
# mod_LFishSeason_ui("LFishSeason_1")

## To be copied in the server
# mod_LFishSeason_server("LFishSeason_1")
