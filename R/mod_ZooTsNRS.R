#' ZooTsNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZooTsNRS_ui <- function(id){
  nsZooTsNRS <- NS(id)
  tagList(
    sidebarLayout(
      fPlanktonSidebar(id = id, panel_id = "NRSzts", input = input, dat = datNRSz),
      fPLanktonPanel(id = id, panel_id = "NRSzts")
    )
  )
}

#' ZooTsNRS Server Functions
#'
#' @noRd 
mod_ZooTsNRS_server <- function(id){
  moduleServer(id, function(input, output, session, NRSzts){
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
      req(input$Site)
      req(input$parameter)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      selectedData <- datNRSz %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter, input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    shiny::exportTestValues(
      ZtsNRS = {ncol(selectedData())},
      ZtsNRSRows = {nrow(selectedData()) > 0},
      ZtsNRSYearisNumeric = {class(selectedData()$Year_Local)},
      ZtsNRSMonthisNumeric = {class(selectedData()$Month_Local)},
      ZtsNRSDateisDate = {class(selectedData()$SampleTime_Local)},
      ZtsNRSStationisFactor = {class(selectedData()$StationName)},
      ZtsNRSCodeisChr = {class(selectedData()$StationCode)},
      ZtsNRSParametersisChr = {class(selectedData()$Parameters)},
      ZtsNRSValuesisNumeric = {class(selectedData()$Values)}
    )
    
    # Sidebar Map
    output$plotmap <- renderPlot({ 
      planktonr::pr_plot_NRSmap(selectedData()) 
    }) %>% bindCache(input$Site)
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected zooplankton Parameters from the NRS around Australia, as a time series and a monthly climatology by station."
    }) 
    
    output$PlotExp2 <- renderText({
      "A plot of selected indices from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
    }) 
    
    output$PlotExp3 <- renderText({
      "A plot of zooplankton functional groups from the NRS around Australia, as a time series and a monthly climatology by station"
    }) 
    
    
    # Plot Trends -------------------------------------------------------------
    ts1 <- reactive({
      
      if (is.null(datNRSz$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      
      trans <- dplyr::if_else(input$scaler1, "log10", "identity")
      
      p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", Survey = "NRS", method = "lm", trans = trans)
      p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", Survey = "NRS", method = "loess", trans = trans) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")
      
    }) %>% bindCache(input$parameter, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
    
    output$timeseries1 <- renderPlot({
      ts1()
    }, height = function() {length(unique(selectedData()$StationName)) * 200}) 
    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    output$timeseries2 <- renderPlot({
      
      if (is.null(datNRSz$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      
      trans <- dplyr::if_else(input$scaler1, "log10", "identity")
      
      p1 <- planktonr::pr_plot_TimeSeries(selectedData(), Survey = "NRS", trans = trans) + 
        ggplot2::theme(legend.position = "none",
                       axis.title.y = ggplot2::element_blank())
      
      p2 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Month", trans = trans) + 
        ggplot2::theme(legend.position = "bottom",
                       axis.title.y = ggplot2::element_blank())
      
      p3 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Year", trans = trans) + 
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       legend.position = "bottom")
      
      titleplot <- names(planktonr::pr_relabel(input$parameter, style = "simple"))
      
      p1 / (p2 | p3) + patchwork::plot_layout(guides = "collect") + patchwork::plot_annotation(title = titleplot)
      
    }) %>% bindCache(input$parameter, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
    
    # Functional groups -------------------------------------------------------
    # observeEvent(input$NRSzts, {
    #   if(input$NRSzts == "Functional groups") {
    #     
    selectedDataFG <- reactive({
      req(input$Site)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      selectedDataFG <- NRSfgz %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
    }) %>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    ts3 <- reactive({
      
      scale <- dplyr::if_else(input$scaler3, "Actual", "Percent")
      
      p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
      p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month") + 
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       legend.position = "none")
      p1 + p2 + patchwork::plot_layout(widths = c(3,1))
    }) %>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler3)
    
    output$timeseries3 <- renderPlot({
      ts3()
    }, height = function() {length(unique(selectedDataFG()$StationName)) * 200}) 
    
    #   }
    # })
    
    
    # Download -------------------------------------------------------
    
  })
}


