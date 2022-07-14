#' WaterBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_WaterBGC_ui <- function(id){
  nsWaterBGC <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        plotOutput(nsWaterBGC("plotmap")),
        # station selector
        checkboxGroupInput(inputId = nsWaterBGC('station'), label = "Select a station", choices = unique(sort(datNRSw$StationName)), selected = 'Port Hacking'),
        # Date selector
        sliderInput(nsWaterBGC("date"), "Dates:", min = lubridate::ymd(20090101), max = Sys.Date(), 
                    value = c(lubridate::ymd(20090101), Sys.Date()-1), timeFormat="%Y-%m-%d"),
        # select parameter
        selectizeInput(inputId = nsWaterBGC('Parameter'), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datNRSw$Parameters), style = "simple"), selected = 'CTDTemperature_degC', multiple = FALSE)
        # Select whether to overlay smooth trend line
        #selectizeInput(inputId = nsWaterBGC("smoother"), label = strong("Overlay trend line"), choices = c("Smoother", "Linear", "None"), selected = "None")
      ),
      mainPanel(
        h6(textOutput(nsWaterBGC("PlotExp"), container = span)),
        plotOutput(nsWaterBGC("plot"), height = 'auto') %>% withSpinner(color="#0dc5c1")
      )
    )
  )
}

#' WaterBGC Server Functions
#'
#' @noRd 
mod_WaterBGC_server <- function(id){
  moduleServer( id, function(input, output, session){
    #     select depths
    
    observe({
      req(input$station)
      req(input$Parameter)
      validate(need(!is.na(input$station), "Error: Please select a station."))
      validate(need(!is.na(input$Parameter), "Error: Please select a parameter."))
    })
    
    selected <- reactive({
      req(input$date)
      validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      datNRSw %>%
        filter(.data$StationName %in% input$station,
               .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
               .data$Parameters %in% input$Parameter) %>%
        mutate(name = as.factor(.data$Parameters)) %>%
        tidyr::drop_na() 
    }) %>% bindCache(input$station, input$Parameter, input$date)
    
    shiny::exportTestValues(
      WaterBGC = {ncol(selected())},
      WaterBGCRows = {nrow(selected()) > 0},
      WaterBGCProjectisChr = {class(selected()$Project)},
      WaterBGCMonthisNumeric = {class(selected()$Month_Local)},
      WaterBGCDateisDate = {class(selected()$SampleTime_Local)},
      WaterBGCStationisFactor = {class(selected()$StationName)},
      WaterBGCCodeisChr = {class(selected()$StationCode)},
      WaterBGCparametersisChr = {class(selected()$Parameters)},
      WaterBGCValuesisNumeric = {class(selected()$Values)}
    )
    
    # Create timeseries object the plotOutput function is expecting
    ts1 <-  reactive({
      
      p1 <- planktonr::pr_plot_Trends(selected(), Trend = "Raw", Survey = "NRS", method = "lm", trans = 'identity')
      p2 <- planktonr::pr_plot_Trends(selected(), Trend = "Month", Survey = "NRS", method = "loess", trans = 'identity') +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = 'collect')
      
    }) %>% bindCache(input$station, input$Parameter, input$date, input$smoother)
    
    output$plot <- renderPlot({
      ts1()
    }, height = function() {length(unique(selected()$StationName)) * 200})
    
      # add a map in sidebar
    output$plotmap <- renderPlot({ 
      
      planktonr::pr_plot_NRSmap(selected())
      
    }) %>% bindCache(input$station)
    
    # add text information 
    output$PlotExp <- renderText({
      "A plot of selected water parameters from the NRS as timeseries"
    }) 
    
    # create table output
    output$table <- DT::renderDataTable(
      selected() ) 
    
  })
}
