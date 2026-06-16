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
  ns <- NS(id)
  tagList(
    shiny::fluidPage(    
      shiny::fluidRow(
        # shiny::column(width = 3, shiny::HTML("<strong>Select Larval Fish:</strong>")),
        shiny::column(width = 6, offset = 6,
                      selectizeInput(inputId = ns("species"), label = NULL, choices = unique(pkg.env$LFData$Species), width = "100%"),
        )),
      shiny::fluidRow(
        mapgl::mapboxglOutput(ns("LFMap"), width = "100%", height = "800px") %>% 
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
        dplyr::filter(.data$Species == input$species)
      
      return(dat)
      
    }) %>% bindCache(input$species)
    
    # Render complete map: grey background dots + blue species dots
    output$LFMap <- mapgl::renderMapboxgl({
      sdf <- LFDatar()
      
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
      abs_sf <- pkg.env$LFDataAbs %>%
        dplyr::distinct(.data$Latitude, .data$Longitude) %>%
        dplyr::mutate(id = dplyr::row_number()) %>%
        sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
      
      pres_sf <- sdf %>%
        sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
      
      mapgl::mapboxgl(
        access_token = golem::get_golem_options("mapbox_token"),
        style        = mapgl::mapbox_style("light"),
        center       = c(134.0, -27.0),
        zoom         = 3.5,
        projection   = "mercator"
      ) %>%
        mapgl::add_circle_layer(
          id             = "absence",
          source         = abs_sf,
          circle_color   = "grey",
          circle_opacity = 1,
          circle_radius  = 2
        ) %>%
        mapgl::add_circle_layer(
          id             = "presence",
          source         = pres_sf,
          circle_color   = "blue",
          circle_opacity = 1,
          circle_radius  = 5,
          popup          = "popup_html"
        ) %>%
        mapgl::add_categorical_legend(
          legend_title = "",
          values       = c("Presence", "Absence"),
          colors       = c("blue", "grey"),
          position     = "bottom-left"
        )
    }) %>% bindCache(input$species)
    
  })
}

## To be copied in the UI
# mod_LFishSpatial_ui("LFishSpatial_1")

## To be copied in the server
# mod_LFishSpatial_server("LFishSpatial_1")
