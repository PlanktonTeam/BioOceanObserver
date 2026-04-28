#' ATStats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ATStats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      shiny::sidebarPanel(
        shiny::HTML("<h3>Select a filter:</h3>"),
        shiny::fluidRow(class = "row_multicol", 
                        tags$div(align = "left", 
                                 class = "multicol",
                                 shiny::radioButtons(inputId = ns("filter"), 
                                                     label = NULL,
                                                     choices = c("None", "Species", "Location"), 
                                                     selected = "None"))),
        shiny::HTML("<h3>Make your selection:</h3>"),
        shiny::fluidRow(class = "row_multicol", 
                        tags$div(align = "left", 
                                 class = "multicol",
                                 shiny::selectizeInput(inputId = ns("selection"), 
                                                       label = NULL,
                                                       choices = NULL, 
                                                       selected = NULL,
                                                       multiple = TRUE ))),
      ),
      shiny::mainPanel(
        shiny::htmlOutput(ns("PlotExp1")),
        fluidRow(column(12, DT::DTOutput(ns("AnimalDataTable")))),
        fluidRow(column(12, plotOutput(ns("gg1")))) %>% 
          shinycssloaders::withSpinner(color="#0dc5c1"),
        fluidRow(column(12, plotOutput(ns("gg2")))) %>% 
          shinycssloaders::withSpinner(color="#0dc5c1")
      )
    )
  )
}
    
#' ATStats Server Functions
#'
#' @noRd 
mod_ATStats_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    animalData <- reactive({
      
      animals <- readRDS("data-raw/AnimalTracking/AnimalSummary.rds")
      
    })
    
    locationData <- reactive({
      
      locations <- readRDS("data-raw/AnimalTracking/ReceiverSummary.rds")
      
    })
    
    observeEvent(input$filter, {
      
      if(input$filter == "None"){
        shiny::updateSelectizeInput(session, "selection", selected = NULL, choices = NULL)
      } else if (input$filter == "Species"){
        species <- sort(unique(animalData()$species_common_name))
        shiny::updateSelectizeInput(session, "selection", selected = species[1], choices = species)
      } else {
        locations <- sort(unique(animalData()$installation_name))
        shiny::updateSelectizeInput(session, "selection", selected = locations[1], choices = locations)
      }
      
    })
    
    fdata <- reactive({
      if(input$filter == 'None'){
        fdata <- animalData() 
      } else if (input$filter == 'Species'){
        fdata <- animalData() %>% 
          dplyr::filter(species_common_name %in% input$selection)
      } else {
        fdata <- animalData() %>% 
          dplyr::filter(installation_name %in% input$selection) 
      }
      
    }) %>% bindCache(input$filter, input$selection)
    
    output$AnimalDataTable <- DT::renderDT(
      fdata()
     )

    gg_out1 <- reactive({
      
      req(input$filter)
      
      df <- fdata() %>% 
        dplyr::mutate(Year = lubridate::year(date_UTC)) %>% 
        dplyr::group_by(Year) %>% 
        dplyr::summarise(Values = sum(total_detections, na.rm = TRUE))
      
      ggplot2::ggplot(data = df, ggplot2::aes(x = Year, y = Values)) +
        ggplot2::geom_col() +
        ggplot2::theme_bw()
      
    }) %>% bindCache(input$filter, input$selection)
    
    output$gg1 <- renderPlot({
      gg_out1()
    })
    
    gg_out2 <- reactive({
      
      req(input$filter)
      
      df <- fdata() %>% 
        dplyr::mutate(Month = lubridate::month(date_UTC)) %>% 
        dplyr::group_by(Month) %>% 
        dplyr::summarise(Values = sum(total_detections, na.rm = TRUE))
      
      ggplot2::ggplot(data = df, ggplot2::aes(x = Month, y = Values)) +
        ggplot2::geom_col() +
        ggplot2::theme_bw()
      
    }) %>% bindCache(input$filter, input$selection)
    
    output$gg2 <- renderPlot({
      gg_out2()
    })
    
    
    # add text information
    output$PlotExp1 <- renderText({
      if(input$filter == 'None') {
        "A table and bar plots of detections - unfiltered"
      } else if(input$filter == 'Species') {
        "A table and bar plots of detections - filtered by species"
      } else {
        "A table and bar plots of detections - filtered by location"
      }
    })
  })
}
    
