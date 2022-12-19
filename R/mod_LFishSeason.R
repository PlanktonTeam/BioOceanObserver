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
                                     choices = unique(LFData$Species), width = "100%",
                                     options = list(dropdownParent = 'body')),
        )),
      shiny::fluidRow(
        shiny::column(width = 6,
                      style = "padding:0px; margin:0px;",
                      shiny::h4("December - February"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapSum"), width = "99%", height = "250px") %>% 
                        shinycssloaders::withSpinner(color="#0dc5c1")), 
        shiny::column(width = 6,
                      style = "padding:0px; margin:0px;",
                      shiny::h4("March - May"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapAut"), width = "99%", height = "250px") %>%
                        shinycssloaders::withSpinner(color="#0dc5c1")
                      )
      ),
      shiny::fluidRow(
        shiny::column(width = 6,
                      style = "padding:0px; margin:0px;",
                      shiny::h4("June = August"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapWin"), width = "99%", height = "250px") %>% 
                        shinycssloaders::withSpinner(color="#0dc5c1")), 
        shiny::column(width = 6,
                      style = "padding:0px; margin:0px;",
                      shiny::h4("September - November"),
                      leaflet::leafletOutput(nsLFishSeason("LFMapSpr"), width = "99%", height = "250px") %>% 
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
        dplyr::filter(.data$Species == input$species) %>%
        dplyr::distinct(.data$Latitude, .data$Longitude, .keep_all = TRUE) %>% 
        dplyr::mutate(Season = dplyr::case_when(.data$Month_Local >= 3 &.data$Month_Local <= 5 ~ "March - May",
                                                .data$Month_Local >= 6 &.data$Month_Local <= 8 ~ "June = August",
                                                .data$Month_Local >= 9 &.data$Month_Local <= 11 ~ "September - November",
                                                TRUE ~ "December - February"))
      return(dat)
    }) %>% bindCache(input$species)
    
    
    # Summer
    output$LFMapSum <- leaflet::renderLeaflet({
      lf <- LeafletBase(LFDatar())
      return(lf)
    })
    
    # Autumn
    output$LFMapAut <- leaflet::renderLeaflet({
      lf <- LeafletBase(LFDatar())
      return(lf)
    })
    
    # Winter
    output$LFMapWin <- leaflet::renderLeaflet({
      lf <- LeafletBase(LFDatar())
      return(lf)
    })
    
    # Spring
    output$LFMapSpr <- leaflet::renderLeaflet({
      lf <- LeafletBase(LFDatar())
      return(lf)
    })
    
    
    
    # Add points for chosen larval fish by season
    observe({
      LeafletObs(sdf = LFDatar() %>% dplyr::filter(.data$Season == "December - February"), name = "LFMapSum", Type = 'PA')
      LeafletObs(sdf = LFDatar() %>% dplyr::filter(.data$Season == "September - November"), name = "LFMapAut", Type = 'PA')
      LeafletObs(sdf = LFDatar() %>% dplyr::filter(.data$Season == "June = August"), name = "LFMapWin", Type = 'PA')
      LeafletObs(sdf = LFDatar() %>% dplyr::filter(.data$Season == "March - May"), name = "LFMapSpr", Type = 'PA')
      
    })
    
  })
}

## To be copied in the UI
# mod_LFishSeason_ui("LFishSeason_1")

## To be copied in the server
# mod_LFishSeason_server("LFishSeason_1")
