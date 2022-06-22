#' PigmentsBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PigmentsBGC_ui <- function(id){
  nsPigmentsBGC <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        plotOutput(nsPigmentsBGC("plotmap")),
        # station selector
        checkboxGroupInput(inputId = nsPigmentsBGC('station'), label = "Select a station", choices = unique(sort(Pigs$StationName)), selected = 'Port Hacking'),
        # Date selector
        sliderInput(nsPigmentsBGC("date"), "Dates:", min = lubridate::ymd(20090101), max = Sys.Date(), 
                    value = c(lubridate::ymd(20090101), Sys.Date()-1), timeFormat="%Y-%m-%d"),
        # select parameter
        selectizeInput(inputId = nsPigmentsBGC('parameter'), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(Pigs$parameters), style = "simple"), selected = 'TotalChla', multiple = FALSE),
        #selectizeInput(inputId = nsPigmentsBGC('depth'), label = 'Select a depth', choices = NULL, selected = '0'),
        # Select whether to overlay smooth trend line
        selectizeInput(inputId = nsPigmentsBGC("smoother"), label = strong("Overlay trend line"), choices = c("Smoother", "Linear", "None"), selected = "None")
      ),
      mainPanel(
        h6(textOutput(nsPigmentsBGC("PlotExp"), container = span)),
        plotlyOutput(nsPigmentsBGC("plot")) %>% withSpinner(color="#0dc5c1")
      )
    )
  )
}

#' PigmentsBGC Server Functions
#'
#' @noRd 
mod_PigmentsBGC_server <- function(id){
  moduleServer( id, function(input, output, session){
    #     select depths
    
    observe({
      req(input$station)
      req(input$parameter)
      validate(need(!is.na(input$station), "Error: Please select a station."))
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      # updateSelectizeInput(session, "depth", "Select a depth", server = TRUE, 
      #                      choices = NRSBGCPigments[NRSBGCPigments$Station %in% input$station & NRSBGCPigments$name %in% input$parameter,]$SampleDepth_m)
    })
    
    selected <- reactive({
      req(input$date)
      validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      Pigs %>%
        filter(.data$StationName %in% input$station,
               .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
               .data$parameters %in% input$parameter) %>%
        mutate(Station = as.factor(.data$StationName),
               name = as.factor(.data$parameters),
               SampleDepth_m = round(.data$SampleDepth_m, -1)) %>%
        tidyr::drop_na() 
    }) %>% bindCache(input$station, input$parameter, input$date)
    
    shiny::exportTestValues(
      PigsBGC = {ncol(selected())},
      PigsBGCRows = {nrow(selected()) > 0},
      PigsBGCProjectisChr = {class(selected()$Project)},
      PigsBGCMonthisNumeric = {class(selected()$Month_Local)},
      PigsBGCDepthisNumeric = {class(selected()$SampleDepth_m)},
      PigsBGCDateisDate = {class(selected()$SampleTime_Local)},
      PigsBGCStationisFactor = {class(selected()$StationName)},
      PigsBGCCodeisChr = {class(selected()$StationCode)},
      PigsBGCparametersisChr = {class(selected()$parameters)},
      PigsBGCValuesisNumeric = {class(selected()$Values)}
    )
    
  
    # Create timeseries object the plotOutput function is expecting
    output$plot <- renderPlotly({
      
      trend <-  input$smoother
      
      plot <- planktonr::pr_plot_env_var(selected(), trend = trend)
      
    }) %>% bindCache(input$station, input$parameter, input$date, input$smoother)
    
    # add a map in sidebar
    output$plotmap <- renderPlot({ 
      
      planktonr::pr_plot_NRSmap(selected())
      
    }) %>% bindCache(input$station)
    
    # add text information 
    output$PlotExp <- renderText({
      "A plot of selected nutrient parameters from the NRS as timeseries at analysed depths"
    }) 
    
    # create table output
    output$table <- DT::renderDataTable(
      selected() ) 
    
  })
}
