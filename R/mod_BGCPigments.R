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
      req(input$station)
      req(input$parameter)
      shiny::validate(need(!is.na(input$station), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      # updateSelectizeInput(session, "depth", "Select a depth", server = TRUE, 
      #                      choices = NRSBGCPigments[NRSBGCPigments$Station %in% input$station & NRSBGCPigments$name %in% input$parameter,]$SampleDepth_m)
    })
    
    selected <- reactive({
      req(input$date)
      shiny::validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      shiny::validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      
      pkg.env$Pigs %>%
        dplyr::filter(.data$StationName %in% input$station,
               .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
               .data$Parameters %in% input$parameter) %>%
        dplyr::mutate(name = as.factor(.data$Parameters),
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
      PigsBGCParametersisChr = {class(selected()$Parameters)},
      PigsBGCValuesisNumeric = {class(selected()$Values)}
    )
    
    
    # Create timeseries object the plotOutput function is expecting
    gg_out1 <- reactive({
      trend <-  input$smoother
      planktonr::pr_plot_Enviro(selected(), Trend = trend)
    }) %>% bindCache(input$station, input$parameter, input$date, input$smoother)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {length(unique(selected()$SampleDepth_m)) * 200}) 
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selected(), "Pigs") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Pigs") # Download figure
    
    # add a map in sidebar
    output$plotmap <- renderPlot({ 
      
      planktonr::pr_plot_NRSmap(selected())
      
    }, bg = "transparent") %>% bindCache(input$station)
    
    # add text information 
    output$PlotExp <- renderText({
      "A plot of selected nutrient Parameters from the NRS as timeseries at analysed depths"
    }) 
    
    # Parameter Definition
    output$ParamDefb <- fParamDefServer(selected) # Download csv of data
    
    
  })
}
