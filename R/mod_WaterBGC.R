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
        style = "padding:1%;",
        tags$head(tags$style(HTML( #TODO move to custom css
          ".multicol{
          height:auto;
          -webkit-column-count: 2;
          -moz-column-count: 2;
          column-count: 2;}"))),
        # shiny::div(
          # style = "padding:0px; margin:0px; max-height: 1000px;", #bottom: 0px; left: 0px; right: 0px; max-width: 1000px;  min-height: 10px
          shiny::plotOutput(nsWaterBGC("plotmap"),
                            width = "100%"),
        # ),
        shiny::HTML("<h5><strong>Select a station:</strong></h5>"),
        shiny::fluidRow(tags$div(align = "left", 
                                 class = "multicol",
                                 shiny::checkboxGroupInput(inputId = nsWaterBGC("station"), 
                                                           label = NULL,
                                                           choices = NRSStation %>% 
                                                             dplyr::filter(.data$StationCode != "PH4") %>% 
                                                             dplyr::pull(.data$StationName), 
                                                           selected = "Port Hacking"))),
        sliderInput(nsWaterBGC("date"), "Dates:", min = lubridate::ymd(20090101), max = Sys.Date(), 
                    value = c(lubridate::ymd(20090101), Sys.Date()-1), timeFormat="%Y-%m-%d"),
        # select parameter
        selectizeInput(inputId = nsWaterBGC('Parameter'), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datNRSw$Parameters), style = "simple"), selected = 'CTDTemperature_degC', multiple = FALSE),
        # Select whether to overlay smooth trend line
        #selectizeInput(inputId = nsWaterBGC("smoother"), label = strong("Overlay trend line"), choices = c("Smoother", "Linear", "None"), selected = "None")
      shiny::br(), # Give a bit of space for the menu to expand
      shiny::br()
        ),
      fEnviroPanel(id = id)
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
        dplyr::filter(.data$StationName %in% input$station,
               .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
               .data$Parameters %in% input$Parameter) %>%
        dplyr::mutate(name = as.factor(.data$Parameters)) %>%
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
    gg_out1 <-  reactive({
      
      p1 <- planktonr::pr_plot_Trends(selected(), Trend = "Raw", Survey = "NRS", method = "lm", trans = 'identity')
      p2 <- planktonr::pr_plot_Trends(selected(), Trend = "Month", Survey = "NRS", method = "loess", trans = 'identity') +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = 'collect')
      
    }) %>% bindCache(input$station, input$Parameter, input$date, input$smoother)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {length(unique(selected()$StationName)) * 200})
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selected(), "water") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "water") # Download figure
    
    # add a map in sidebar
    output$plotmap <- renderPlot({ 
      
      planktonr::pr_plot_NRSmap(selected())
      
    }, bg = "transparent") %>% bindCache(input$station)
    
    # add text information 
    output$PlotExp <- renderText({
      "A plot of selected water parameters from the NRS as timeseries"
    }) 
    
  })
}
