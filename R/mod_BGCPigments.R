#' PigmentsBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PigmentsBGC_ui <- function(id){
  tagList(
    sidebarLayout(
      fEnviroSidebar(id = id, dat = pkg.env$Pigs),
      fEnviroPanel(id = id)
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
      req(input$site)
      req(input$parameter)
      shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      # updateSelectizeInput(session, "depth", "Select a depth", server = TRUE, 
      #                      choices = NRSBGCPigments[NRSBGCPigments$Station %in% input$site & NRSBGCPigments$name %in% input$parameter,]$SampleDepth_m)
    })
    
    selectedData <- reactive({
      req(input$date)
      shiny::validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      shiny::validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      
      pkg.env$Pigs %>%
        dplyr::filter(.data$StationName %in% input$site,
               .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
               .data$Parameters %in% input$parameter) %>%
        dplyr::mutate(name = as.factor(.data$Parameters),
                      SampleDepth_m = round(.data$SampleDepth_m, -1)) %>%
        tidyr::drop_na() 
    }) %>% bindCache(input$site, input$parameter, input$date)
    
    shiny::exportTestValues(
      PigsBGC = {ncol(selectedData())},
      PigsBGCRows = {nrow(selectedData()) > 0},
      PigsBGCProjectisChr = {class(selectedData()$Project)},
      PigsBGCMonthisNumeric = {class(selectedData()$Month_Local)},
      PigsBGCDepthisNumeric = {class(selectedData()$SampleDepth_m)},
      PigsBGCDateisDate = {class(selectedData()$SampleTime_Local)},
      PigsBGCStationisFactor = {class(selectedData()$StationName)},
      PigsBGCCodeisChr = {class(selectedData()$StationCode)},
      PigsBGCParametersisChr = {class(selectedData()$Parameters)},
      PigsBGCValuesisNumeric = {class(selectedData()$Values)}
    )
    
    
    # Create timeseries object the plotOutput function is expecting
    gg_out1 <- reactive({
      trend <-  input$smoother
      planktonr::pr_plot_Enviro(selectedData(), Trend = trend)
    }) %>% bindCache(input$site, input$parameter, input$date, input$smoother)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {length(unique(selectedData()$SampleDepth_m)) * 200}) 
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Pigs") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Pigs") # Download figure
    
    # add a map in sidebar
    output$plotmap <- plotly::renderPlotly({ 
      p1 <- planktonr::pr_plot_NRSmap(unique(selectedData()$StationCode))
      fPlotlyMap(p1, tooltip = "colour")
    })  # No cache - allows responsive resizing
    
    # add text information 
    output$PlotExp <- renderText({
      "A plot of selected nutrient Parameters from the NRS as timeseries at analysed depths"
    }) 
    
    # Parameter Definition
    output$ParamDefb <- fParamDefServer(selectedData) # Download csv of data
    
    
  })
}
