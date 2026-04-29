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
        fluidRow(column(12, DT::DTOutput(ns("SpeciesDataTable")))),
        shiny::htmlOutput(ns("PlotExp2")),
        fluidRow(column(12, DT::DTOutput(ns("locationsDataTable")))),
        shiny::htmlOutput(ns("PlotExp3")),
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
    
    speciesData <- reactive({
      
      species <- readRDS("data-raw/AnimalTracking/SpeciesSummary.rds")
      
    })
    
    locationData <- reactive({
      
      locations <- readRDS("data-raw/AnimalTracking/ReceiverSummary.rds")
      
    })
    
    observeEvent(input$filter, {
      
      if(input$filter == "None"){
        shiny::updateSelectizeInput(session, "selection", selected = NULL, choices = NULL)
      } else if (input$filter == "Species"){
        species <- sort(unique(speciesData()$species_common_name))
        shiny::updateSelectizeInput(session, "selection", selected = species[1], choices = species)
      } else {
        locations <- sort(unique(speciesData()$installation_name))
        shiny::updateSelectizeInput(session, "selection", selected = locations[1], choices = locations)
      }
      
    })
    
    fdata <- reactive({
      if(input$filter == 'None'){
        fdata <- speciesData() 
      } else if (input$filter == 'Species'){
        fdata <- speciesData() %>% 
          dplyr::filter(species_common_name %in% input$selection)
      } else {
        fdata <- speciesData() %>% 
          dplyr::filter(installation_name %in% input$selection) 
      }
      
    }) %>% bindCache(input$filter, input$selection)
    
    ldata <- reactive({
      if(input$filter == 'None'){
        ldata <- locationData() 
      } else if (input$filter == 'Species'){
        ldata <- locationData() %>% 
          dplyr::filter(installation_name %in% fdata()$installation_name)
      } else {
        ldata <- locationData() %>% 
          dplyr::filter(installation_name %in% input$selection) 
      }
      
    }) %>% bindCache(input$filter, input$selection)
    
    output$SpeciesDataTable <- DT::renderDT(
      fdata()
     )

    output$locationsDataTable <- DT::renderDT(
      ldata()
    )
    
    gg_out1 <- reactive({
      
      req(input$filter)
      
      ATbardata <- function(df, time){
        df <- df %>% 
          dplyr::mutate(Year = lubridate::year(month_UTC), 
                        Month = lubridate::month(month_UTC)) %>% 
          dplyr::group_by(!!rlang::sym(time)) %>% 
          dplyr::summarise(Values = sum(total_detections, na.rm = TRUE))
        
      }
      
      ATbarPlot <- function(df, time){
        p <- ggplot2::ggplot(data = df, ggplot2::aes(x = !!rlang::sym(time), y = Values)) +
          ggplot2::geom_col(fill = "#E2ECF3", color = "#3B6E8F") + 
          ggplot2::labs(y = "Number of Detections") +
          ggplot2::scale_y_continuous(expand = c(0,0.05)) +
          planktonr::theme_pr()
          
       if(rlang::as_string(time) %in% c("Month")){
         p <- p + ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12),
                                            labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
       } else {
         p
       }
          return(p)
      }
      
      daty <- ATbardata(fdata(), 'Year')
      ploty <- ATbarPlot(daty, 'Year')
      datm <- ATbardata(fdata(), 'Month')
      plotm <- ATbarPlot(datm, 'Month') +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

      ploty + plotm
      
    }) %>% bindCache(input$filter, input$selection)
    
    output$gg1 <- renderPlot({
      gg_out1()
    })
    

    # add text information
    output$PlotExp1 <- renderText({
      if(input$filter == 'None') {
        HTML("<h3>Table of detections - unfiltered</h3>")
      } else if(input$filter == 'Species') {
        HTML("<h3>Table of detections - filtered by species</h3>")
      } else {
        HTML("<h3>Table of detections - filtered by location</h3>")
      }
    })
    output$PlotExp2 <- renderText({
      if(input$filter == 'None') {
        HTML("<h3>Table of receiver details at installation locations - unfiltered</h3>")
      } else if(input$filter == 'Species') {
        HTML("<h3>Table of receiver details at installation locations - filtered by species</h3>")
      } else {
        HTML("<h3>Table of receiver details at installation locations - filtered by location</h3>")
      }
    })
    output$PlotExp3 <- renderText({
      if(input$filter == 'None') {
        HTML("<h3>Yearly and monthly bar plots of detections - unfiltered</h3>")
      } else if(input$filter == 'Species') {
        HTML("<h3>Yearly and monthly bar plots of detections - filtered by species</h3>")
      } else {
        HTML("<h3>Yearly and monthly plots of detections - filtered by location</h3>")
      }
    })
  })
}
    
