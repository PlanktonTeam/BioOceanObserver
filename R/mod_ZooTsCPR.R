#' ZooTsCPR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZooTsCPR_ui <- function(id){
  nsZooTsCPR <- NS(id)
  tagList(
    sidebarLayout(
      fPlanktonSidebar(id = id, tabsetPanel_id = "CPRzts", dat = pkg.env$datCPRz),
      fPLanktonPanel(id = id, tabsetPanel_id = "CPRzts"),
    )
  )
}

#' ZooTsCPR Server Functions
#'
#' @noRd 
mod_ZooTsCPR_server <- function(id){
  moduleServer( id, function(input, output, session, CPRzts){
    
    # Sidebar ----------------------------------------------------------
    
    selectedData <- reactive({
      req(input$region)
      req(input$parameter)
      shiny::validate(need(!is.na(input$region), "Error: Please select a region"))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      ## Need to make these factors load automatically...... if possible
      selectedData <- pkg.env$datCPRz %>% 
        dplyr::filter(.data$BioRegion %in% input$region,
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2])
    
    shiny::exportTestValues(
      ZtsCPR = {ncol(selectedData())},
      ZtsCPRRows = {nrow(selectedData()) > 0},
      ZtsCPRYearisNumeric = {class(selectedData()$Year_Local)},
      ZtsCPRMonthisNumeric = {class(selectedData()$Month_Local)},
      ZtsCPRDateisDate = {class(selectedData()$SampleTime_Local)},
      ZtsCPRRegionisFactor = {class(selectedData()$BioRegion)},
      ZtsCPRParametersisChr = {class(selectedData()$Parameters)},
      ZtsCPRValuesisNumeric = {class(selectedData()$Values)}
    )
    
    output$plotmap <- renderPlot({ 
      planktonr::pr_plot_CPRmap(unique(selectedData()$BioRegion))
    }, bg = "transparent") %>% bindCache(input$region)
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected zooplankton Parameters from the CPR around Australia, as a time series and a monthly climatology across bioregions. "
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected zooplankton Parameters from the CPR around Australia, as a time series, a monthly climatology and an annual mean for each bioregion"
    }) 
    output$PlotExp3 <- renderText({
      "A plot of functional groups from the zooplankton counts from the CPR around Australia, as a time series and a monthly climatology for each bioregion"
    })     
    
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$CPRpts == 1}, {
      
      gg_out1 <- reactive({
        
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", method = "lm", trans = trans)
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", method = "loess", trans = trans) + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        p1 + p2 + patchwork::plot_layout(widths = c(3,1))
        
      }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {length(unique(selectedData()$BioRegion)) * 200})
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Trend") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Trend") # Download figure
      
      # Parameter Definition
      output$ParamDef <- fParamDefServer(selectedData)
      
    })
    
    
    # Climatologies -----------------------------------------------------------
    observeEvent({input$NRSpts == 2}, {
      
      gg_out2 <- reactive({
        
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        if (identical(input$region, "")) return(NULL)
        if (identical(input$parameter, "")) return(NULL)
        
        p1 <- planktonr::pr_plot_TimeSeries(selectedData(), trans = trans) + 
          ggplot2::theme(legend.position = "none")
        
        p2 <- planktonr::pr_plot_Climatology(selectedData(), Trend = "Month", trans = trans) +
          ggplot2::theme(legend.position = "none")
        
        p3 <- planktonr::pr_plot_Climatology(selectedData(), Trend = "Year", trans = trans) + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                         legend.position = "bottom")
        
        p1 / 
          (p2 + p3 + patchwork::plot_layout(ncol = 2, guides = "collect") & ggplot2::theme(legend.position = "bottom"))
        
      }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
      
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
    observeEvent({input$NRSpts == 3}, {
      
      selectedDataFG <- reactive({
        req(input$region)
        shiny::validate(need(!is.na(input$region), "Error: Please select a bioregion"))
        
        selectedDataFG <- pkg.env$CPRfgz %>% 
          dplyr::filter(.data$BioRegion %in% input$region,
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
          droplevels()
      }) %>% bindCache(input$region, input$DatesSlide[1], input$DatesSlide[2])
      
      gg_out3 <- reactive({
        
        if (is.null(pkg.env$CPRfgz$BioRegion)) {return(NULL)}
        scale <- dplyr::if_else(input$scaler3, "Percent", "Actual")
        
        p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
        p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month") + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                         legend.position = "none")
        p1 + p2 + patchwork::plot_layout(widths = c(3,1))
        
      }) %>% bindCache(input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler3)
      
      output$timeseries3 <- renderPlot({
        gg_out3()
      }, height = function() {
        if(length(unique(selectedDataFG()$BioRegion)) < 2) 
        {300} else 
        {length(unique(selectedDataFG()$BioRegion)) * 200}})
      
      # Download -------------------------------------------------------
      output$downloadData3 <- fDownloadButtonServer(input, selectedDataFG, "FuncGroup") # Download csv of data
      output$downloadPlot3 <- fDownloadPlotServer(input, gg_id = gg_out3, "FuncGroup") # Download figure
      
    })
    
  })
}
