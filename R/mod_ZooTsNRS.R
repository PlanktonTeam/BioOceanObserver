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
      fPlanktonSidebar(id = id, tabsetPanel_id = "NRSzts", dat = pkg.env$datNRSz),
      fPLanktonPanel(id = id, tabsetPanel_id = "NRSzts")
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
      shiny::validate(need(!is.na(input$Site), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      selectedData <- pkg.env$datNRSz %>% 
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
      planktonr::pr_plot_NRSmap(unique(selectedData()$StationCode))
    }, bg = "transparent") %>% bindCache(input$Site)
    
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
    observeEvent({input$NRSzts == 1}, {
      
      gg_out1 <- reactive({
        
        if (is.null(pkg.env$datNRSz$StationCode)){return(NULL)}
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", method = "lm", trans = trans)
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")
        
      }) %>% bindCache(input$parameter, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {length(unique(selectedData()$StationName)) * 200})
      
      # Download -------------------------------------------------------
  
      output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Trend") # Download csv of data
      
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_out1, "Trend") # Download figure
      
      # Parameter Definition
      output$ParamDef <- fParamDefServer(selectedData)
      
    })
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    observeEvent({input$NRSzts == 2}, {
      
      gg_out2 <- reactive({   
        if (is.null(pkg.env$datNRSz$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
          return(NULL)
        
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        p1 <- planktonr::pr_plot_TimeSeries(selectedData(), trans = trans) + 
          ggplot2::theme(legend.position = "none")
        
        p2 <- planktonr::pr_plot_Climatology(selectedData(), Trend = "Month", trans = trans) + 
          ggplot2::theme(legend.position = "none")
        
        p3 <- planktonr::pr_plot_Climatology(selectedData(), Trend = "Year", trans = trans) + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                         legend.position = "bottom")
        
        p1 / 
          (p2 + p3 + patchwork::plot_layout(ncol = 2, guides = "collect") & ggplot2::theme(legend.position = "bottom"))
        
      }) %>% bindCache(input$parameter, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
      output$timeseries2 <- renderPlot({
        gg_out2()
      })
      
      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData, "Climate") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2, "Climate") # Download figure
      
      # Parameter Definition
      output$ParamDef <- fParamDefServer(selectedData)
      
    })
    
    # Functional groups -------------------------------------------------------
    
    observeEvent({input$NRSzts == 3}, {
      selectedDataFG <- reactive({
        req(input$Site)
        shiny::validate(need(!is.na(input$Site), "Error: Please select a station."))
        selectedDataFG <- pkg.env$NRSfgz %>% 
          dplyr::filter(.data$StationName %in% input$Site,
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
          droplevels()
      }) %>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2])
      
      
      gg_out3 <- reactive({
        
        scale <- dplyr::if_else(input$scaler3, "Percent", "Actual")
        
        p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
        p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month") + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                         legend.position = "none")
        p1 + p2 + patchwork::plot_layout(widths = c(3,1))
      }) %>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler3)
      
      
      output$timeseries3 <- renderPlot({
        gg_out3()
      }, height = function() {
        if(length(unique(selectedDataFG()$StationName)) < 2) 
        {300} else 
        {length(unique(selectedDataFG()$StationName)) * 200}})
      
      # Download -------------------------------------------------------
      output$downloadData3 <- fDownloadButtonServer(input, selectedDataFG, "FuncGroup") # Download csv of data
      output$downloadPlot3 <- fDownloadPlotServer(input, gg_id = gg_out3, "FuncGroup") # Download figure
      
    })
    
  })
}
