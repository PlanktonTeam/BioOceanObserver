#' NutrientsBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_NutrientsBGC_ui <- function(id){
  tagList(
    sidebarLayout(
      fEnviroSidebar(id = id, dat = pkg.env$Nuts),
      fEnviroPanel(id = id)
      )
    )
}

#' NutrientsBGC Server Functions
#'
#' @noRd 
mod_NutrientsBGC_server <- function(id){
  moduleServer( id, function(input, output, session){
    #     select depths
    
    observe({
      req(input$site)
      req(input$parameter)
      shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
    })
    
    selectedData <- reactive({
      req(input$date)
      shiny::validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      shiny::validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      
      pkg.env$Nuts %>%
        dplyr::filter(.data$StationName %in% input$site,
               .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
               .data$Parameters %in% input$parameter) %>%
        dplyr::mutate(name = as.factor(.data$Parameters)) %>%
        tidyr::drop_na() 
    }) %>% bindCache(input$site, input$parameter, input$date)
    
    shiny::exportTestValues(
      NutrientsBGC = {ncol(selectedData())},
      NutrientsBGCRows = {nrow(selectedData()) > 0},
      NutrientsBGCProjectisChr = {class(selectedData()$Project)},
      NutrientsBGCMonthisNumeric = {class(selectedData()$Month_Local)},
      NutrientsBGCDepthisNumeric = {class(selectedData()$SampleDepth_m)},
      NutrientsBGCDateisDate = {class(selectedData()$SampleTime_Local)},
      NutrientsBGCStationisFactor = {class(selectedData()$StationName)},
      NutrientsBGCCodeisChr = {class(selectedData()$StationCode)},
      NutrientsBGCParametersisChr = {class(selectedData()$Parameters)},
      NutrientsBGCValuesisNumeric = {class(selectedData()$Values)}
    )
    
    # Create timeseries object the plotOutput function is expecting
    gg_out1 <- reactive({
      
      if(input$parameter == 'Oxygen_umolL' & !("Maria Island" %in% input$site || "Rottnest Island" %in% input$site)){
        ggplot2::ggplot + ggplot2::geom_blank()
      } else {
        interp <- input$interp
      
      if(interp == 'Interpolate'){
        planktonr::pr_plot_NRSEnvContour(selectedData(), na.fill = TRUE)
      } else {
        planktonr::pr_plot_NRSEnvContour(selectedData(), na.fill = FALSE)
      }
      }
      
    }) %>% bindCache(input$site, input$parameter, input$date, input$interp)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {
      if(length(unique(selectedData()$StationName)) < 2) 
      {300} else 
          {length(unique(selectedData()$StationName)) * 200}})
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Nuts") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Nuts") # Download figure
    
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
      
      if(input$parameter == 'Oxygen_umolL') {
        paste("A contour plot of nutrients from the NRS around Australia, as a time series and a monthly climatology by depth. 
      If raw data is used the dots represent actual samples. <br> <br> <b>NOTE: Oxygen data is only available for Maria Island and Rottnest Island</b>")
      } else {
        "A contour plot of nutrients from the NRS around Australia, as a time series and a monthly climatology by depth. 
      If raw data is used the dots represent actual samples"
      }
      
    }) %>% bindCache(input$parameter)
    
    # Parameter Definition
    
    output$ParamDefb <- if(input$parameter == 'Oxygen_umolL'){
            shiny::renderText({
              paste("<p><strong>", planktonr::pr_relabel('Oxygen_umolL', style = "plotly"), ":</strong> ",
              pkg.env$ParamDef %>% 
                dplyr::filter(.data$Parameter == 'Oxygen_umolL') %>% 
                dplyr::pull("Definition"), ".</p>", sep = "")
              })
    } else {
      fParamDefServer(selectedData) # Download csv of data
    } %>% bindCache(input$parameter)
    
  })
}
