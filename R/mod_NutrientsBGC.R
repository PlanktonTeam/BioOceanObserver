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
      req(input$station)
      req(input$parameter)
      validate(need(!is.na(input$station), "Error: Please select a station."))
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
    })
    
    selected <- reactive({
      req(input$date)
      validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      
      pkg.env$Nuts %>%
        dplyr::filter(.data$StationName %in% input$station,
               .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
               .data$Parameters %in% input$parameter) %>%
        dplyr::mutate(name = as.factor(.data$Parameters)) %>%
        tidyr::drop_na() 
    }) %>% bindCache(input$station, input$parameter, input$date)
    
    shiny::exportTestValues(
      NutrientsBGC = {ncol(selected())},
      NutrientsBGCRows = {nrow(selected()) > 0},
      NutrientsBGCProjectisChr = {class(selected()$Project)},
      NutrientsBGCMonthisNumeric = {class(selected()$Month_Local)},
      NutrientsBGCDepthisNumeric = {class(selected()$SampleDepth_m)},
      NutrientsBGCDateisDate = {class(selected()$SampleTime_Local)},
      NutrientsBGCStationisFactor = {class(selected()$StationName)},
      NutrientsBGCCodeisChr = {class(selected()$StationCode)},
      NutrientsBGCParametersisChr = {class(selected()$Parameters)},
      NutrientsBGCValuesisNumeric = {class(selected()$Values)}
    )
    
    # Create timeseries object the plotOutput function is expecting
    gg_out1 <- reactive({
      
      if(input$parameter == 'Oxygen_umolL' & !("Maria Island" %in% input$station || "Rottnest Island" %in% input$station)){
        ggplot2::ggplot + ggplot2::geom_blank()
      } else {
        interp <- input$interp
      
      if(interp == 'Interpolate'){
        planktonr::pr_plot_NRSEnvContour(selected(), Interpolation = TRUE, Fill_NA = FALSE)
      } else if (interp == 'Interpolate with gap filling'){
        planktonr::pr_plot_NRSEnvContour(selected(), Interpolation = TRUE, Fill_NA = TRUE, maxGap = 3)
      } else {
        planktonr::pr_plot_NRSEnvContour(selected(), Interpolation = FALSE, Fill_NA = FALSE)
      }
      }
      
    }) %>% bindCache(input$station, input$parameter, input$date, input$interp)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {
      if(length(unique(selected()$StationName)) < 2) 
      {300} else 
          {length(unique(selected()$StationName)) * 200}})
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selected(), "Nuts") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Nuts") # Download figure
    
    # add a map in sidebar
    output$plotmap <- renderPlot({ 

        planktonr::pr_plot_NRSmap(selected())
 
    }, bg = "transparent") %>% bindCache(input$station)
    
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
              paste("<h6><strong>", planktonr::pr_relabel('Oxygen_umolL', style = "plotly"), ":</strong> ",
              pkg.env$ParamDef %>% dplyr::filter(Parameter == 'Oxygen_umolL') %>% dplyr::pull("Definition"), ".</h6>", sep = "")
              })
    } else {
      fParamDefServer(selected) # Download csv of data
    } %>% bindCache(input$parameter)
    
  })
}
