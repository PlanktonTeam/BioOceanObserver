#' PhytoTsCPR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PhytoTsCPR_ui <- function(id){
  nsPhytoTsCPR <- NS(id)
  tagList(
    sidebarLayout(
      fPlanktonSidebar(id = id, tabsetPanel_id = "CPRpts", dat = pkg.env$datCPRp),
      fPLanktonPanel(id = id, tabsetPanel_id = "CPRpts")
    )
  )
}

#' PhytoTsCPR Server Functions
#'
#' @noRd 
mod_PhytoTsCPR_server <- function(id){
  moduleServer( id, function(input, output, session, CPRpts){
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
      req(input$region)
      req(input$parameter)
      validate(need(!is.na(input$region), "Error: Please select a region"))
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      selectedData <- pkg.env$datCPRp %>% 
        dplyr::filter(.data$BioRegion %in% input$region,
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2])
    
    shiny::exportTestValues(
      PhytoTsCPR = {ncol(selectedData())},
      PhytoTsCPRRows = {nrow(selectedData()) > 0},
      PhytoTsCPRYearisNumeric = {class(selectedData()$Year_Local)},
      PhytoTsCPRMonthisNumeric = {class(selectedData()$Month_Local)},
      PhytoTsCPRDateisDate = {class(selectedData()$SampleTime_Local)},
      PhytoTsCPRRegionisFactor = {class(selectedData()$BioRegion)},
      PhytoTsCPRParametersisChr = {class(selectedData()$Parameters)},
      PhytoTsCPRValuesisNumeric = {class(selectedData()$Values)}
    )
    
    output$plotmap <- renderPlot({ # renderCachedPlot plot so cached version can be returned if it exists (code only run once per scenario per session)
      planktonr::pr_plot_CPRmap(selectedData())
    }, bg = "transparent") %>% bindCache(input$region)
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected Phytoplantkon Parameters from the CPR around Australia, as a time series and a monthly climatology across bioregions. "
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected Phytoplantkon Parameters from the CPR around Australia, as a time series, a monthly climatology and an annual mean for each bioregion"
    }) 
    output$PlotExp3 <- renderText({
      "A plot of functional groups from the light microscope phytoplankton counts from the CPR around Australia, as a time series and a monthly climatology for each bioregion"
    }) 
    
    
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$CPRpts == 1}, {
      
      gg_out1 <- reactive({
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", Survey = "CPR", method = "lm", trans = trans)
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", Survey = "CPR", method = "loess", trans = trans) + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        p1 + p2 + patchwork::plot_layout(widths = c(3,1))
        
      }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {length(unique(selectedData()$BioRegion)) * 200}) 
      
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData(), "Trend") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Trend") # Download figure
    })
    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    observeEvent({input$CPRpts == 2}, {
      
      gg_out2 <- reactive({
        
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        if (identical(input$region, "")) return(NULL)
        if (identical(input$parameter, "")) return(NULL)
        
        p1 <- planktonr::pr_plot_TimeSeries(selectedData(), Survey = "CPR", trans = trans) + 
          ggplot2::theme(legend.position = "none",
                         axis.title.y = ggplot2::element_blank())
        
        p2 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "CPR", Trend = "Month", trans = trans) + 
          ggplot2::theme(legend.position = "bottom",
                         axis.title.y = ggplot2::element_blank())
        
        p3 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "CPR", Trend = "Year", trans = trans) + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                         legend.position = "bottom")
        
        titleplot <- names(planktonr::pr_relabel(input$parameter, style = "simple"))
        
        p1 / 
          (p2 + p3 + patchwork::plot_layout(ncol = 2, guides = "collect") & ggplot2::theme(legend.position = "bottom")) +
          patchwork::plot_annotation(title = titleplot)
        
        
      }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
      # Functional groups -------------------------------------------------------
      
      output$timeseries2 <- renderPlot({
        gg_out2()
      })
      
      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData(), "Climate") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2(), "Climate") # Download figure
    })
    
    # Functional groups -------------------------------------------------------
    observeEvent({input$CPRpts == 3}, {
      
      selectedDataFG <- reactive({
        req(input$region)
        validate(need(!is.na(input$region), "Error: Please select a bioregion"))
        
        selectedDataFG <- pkg.env$CPRfgp %>% 
          dplyr::filter(.data$BioRegion %in% input$region,
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
          droplevels()
      }) %>% bindCache(input$region, input$DatesSlide[1], input$DatesSlide[2])
      
      shiny::exportTestValues(
        PhytoFGCPR = {ncol(selectedDataFG())},
        PhytoFGCPRRows = {nrow(selectedDataFG()) > 0},
        PhytoFGCPRYearisNumeric = {class(selectedDataFG()$Year_Local)},
        PhytoFGCPRMonthisNumeric = {class(selectedDataFG()$Month_Local)},
        PhytoFGCPRDateisDate = {class(selectedDataFG()$SampleTime_Local)},
        PhytoFGCPRRegionisFactor = {class(selectedDataFG()$BioRegion)},
        PhytoFGCPRParametersisChr = {class(selectedDataFG()$Parameters)},
        PhytoFGCPRValuesisNumeric = {class(selectedDataFG()$Values)}
      )
      
      gg_out3 <- reactive({
        
        if (is.null(pkg.env$CPRfgp$BioRegion)) {  
          return(NULL)
        }
        
        scale <- dplyr::if_else(input$scaler3, "Actual", "Percent")
        
        p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
        p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month")
        
        p1 + p2 + patchwork::plot_layout(widths = c(3,1))
        
      }) %>% bindCache(input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler3)
      
      output$timeseries3 <- renderPlot({
        gg_out3()
      }, height = function() {length(unique(selectedDataFG()$BioRegion)) * 200}) 
      
      # Download -------------------------------------------------------
      output$downloadData3 <- fDownloadButtonServer(input, selectedDataFG(), "FuncGroup") # Download csv of data
      output$downloadPlot3 <- fDownloadPlotServer(input, gg_id = gg_out3(), "FuncGroup") # Download figure
      
    })
  })
}
