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
  tagList(
    sidebarLayout(
      fEnviroSidebar(id = id, dat = pkg.env$datNRSw),
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
      req(input$parameter)
      shiny::validate(need(!is.na(input$station), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
    })
    
    selectedData <- reactive({
      req(input$date)
      shiny::validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      shiny::validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      pkg.env$datNRSw %>%
        dplyr::filter(.data$StationName %in% input$station,
               .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
               .data$Parameters %in% input$parameter) %>%
        dplyr::mutate(name = as.factor(.data$Parameters)) %>%
        tidyr::drop_na() 
      
    }) %>% bindCache(input$station, input$parameter, input$date)
    
    shiny::exportTestValues(
      WaterBGC = {ncol(selectedData())},
      WaterBGCRows = {nrow(selectedData()) > 0},
      WaterBGCProjectisChr = {class(selectedData()$Project)},
      WaterBGCMonthisNumeric = {class(selectedData()$Month_Local)},
      WaterBGCDateisDate = {class(selectedData()$SampleTime_Local)},
      WaterBGCStationisFactor = {class(selectedData()$StationName)},
      WaterBGCCodeisChr = {class(selectedData()$StationCode)},
      WaterBGCparametersisChr = {class(selectedData()$Parameters)},
      WaterBGCValuesisNumeric = {class(selectedData()$Values)}
    )
    
    # Create timeseries object the plotOutput function is expecting
    gg_out1 <-  reactive({
      p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", method = "lm", trans = 'identity')
      p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", method = "loess", trans = 'identity') +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = 'collect')
      
    }) %>% bindCache(input$station, input$parameter, input$date, input$smoother)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {length(unique(selectedData()$StationName)) * 200})
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "water") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "water") # Download figure
    
    # add a map in sidebar
    output$plotmap <- plotly::renderPlotly({ 
      p1 <- planktonr::pr_plot_NRSmap(unique(selectedData()$StationCode))
      fPlotlyMap(p1, tooltip = "colour")
    })  # No cache - allows responsive resizing
    
    # add text information 
    output$PlotExp <- renderText({
      "A plot of selected water parameters from the NRS as timeseries"
    }) 
    
    # Parameter Definition
    output$ParamDefb <- fParamDefServer(selectedData) # Download csv of data
    
  })
}
