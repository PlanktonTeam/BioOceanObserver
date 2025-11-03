#' PicoBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PicoBGC_ui <- function(id){
  tagList(
    sidebarLayout(
      fEnviroSidebar(id = id, dat = pkg.env$Pico),
      fEnviroPanel(id = id)
    )
  )
}

#' PicoBGC Server Functions
#'
#' @noRd 
mod_PicoBGC_server <- function(id){
  moduleServer( id, function(input, output, session){
    #     select depths
    
    observe({
      req(input$site)
      req(input$parameter)
      shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      # updateSelectizeInput(session, "depth", "Select a depth", server = TRUE, 
      #                      choices = NRSBGCPico[NRSBGCPico$Station %in% input$site & NRSBGCPico$name %in% input$parameter,]$SampleDepth_m)
    })
    
    selectedData <- reactive({
      
      req(input$date)
      shiny::validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      shiny::validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      pkg.env$Pico %>%
        dplyr::filter(.data$StationName %in% input$site,
                      .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
                      .data$Parameters %in% input$parameter) %>%
        dplyr::mutate(name = as.factor(.data$Parameters)) %>%
        tidyr::drop_na() 
      
    }) %>% bindCache(input$site, input$parameter, input$date)
    
    shiny::exportTestValues(
      PicoBGC = {ncol(selectedData())},
      PicoBGCRows = {nrow(selectedData()) > 0},
      PicoBGCProjectisChr = {class(selectedData()$Project)},
      PicoBGCMonthisNumeric = {class(selectedData()$Month_Local)},
      PicoBGCDepthisNumeric = {class(selectedData()$SampleDepth_m)},
      PicoBGCDateisDate = {class(selectedData()$SampleTime_Local)},
      PicoBGCStationisFactor = {class(selectedData()$StationName)},
      PicoBGCCodeisChr = {class(selectedData()$StationCode)},
      PicoBGCParametersisChr = {class(selectedData()$Parameters)},
      PicoBGCValuesisNumeric = {class(selectedData()$Values)}
    )
    
    # Create timeseries object the plotOutput function is expecting
    gg_out1 <- reactive({
      
      interp <- input$interp
      
      if(interp == 'Interpolate'){
        planktonr::pr_plot_NRSEnvContour(selectedData(), na.fill = TRUE)
      } else {
        planktonr::pr_plot_NRSEnvContour(selectedData(), na.fill = FALSE)
      }
      
    }) %>% bindCache(input$site, input$parameter, input$date, input$interp)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {
      if(length(unique(selectedData()$StationName)) < 2) 
      {300} else 
      {length(unique(selectedData()$StationName)) * 200}})
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Pico") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Pico") # Download figure
    
    # Sidebar Map - Initial render
    output$plotmap <- leaflet::renderLeaflet({ 
      fLeafletMap(character(0), Survey = "NRS", Type = "Zooplankton")
    })
    
    # Update map when station selection changes
    observe({
      fLeafletUpdate("plotmap", session, unique(selectedData()$StationCode), 
                     Survey = "NRS", Type = "Zooplankton")
    })
    
    # add text information 
    output$PlotExp <- renderText({
      "A contour plot of picoplankton from the NRS around Australia, as a time series and a monthly climatology by depth. 
      If raw data is used the dots represent actual samples."
    }) 
    
    # Parameter Definition
    output$ParamDefb <- fParamDefServer(selectedData) # Download csv of data
    
    
  })
}
