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
      fEnviroSidebar(id = id, dat = datNRSw),
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
      validate(need(!is.na(input$station), "Error: Please select a station."))
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
    })
    
    selected <- reactive({
      req(input$date)
      validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      
      datNRSw %>%
        dplyr::filter(.data$StationName %in% input$station,
               .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
               .data$Parameters %in% input$parameter) %>%
        dplyr::mutate(name = as.factor(.data$Parameters)) %>%
        tidyr::drop_na() 
      
    }) %>% bindCache(input$station, input$parameter, input$date)
    
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
      
    }) %>% bindCache(input$station, input$parameter, input$date, input$smoother)
    
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
