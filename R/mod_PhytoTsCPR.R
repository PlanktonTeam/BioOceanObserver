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
      fPlanktonSidebar(id = id, panel_id = "CPRpts", input = input, dat = datCPRp),
      fPLanktonPanel(id = id, panel_id = "CPRpts")
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
      
      selectedData <- datCPRp %>% 
        mutate(BioRegion = factor(.data$BioRegion, levels = c("Coral Sea", "Temperate East", "South-west", "South-east"))) %>%
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
    }) %>% bindCache(input$region)
    
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
    
    ts1 <- reactive({
      
      trans <- dplyr::if_else(input$scaler1, "log10", "identity")
      
      p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", Survey = "CPR", method = "lm", trans = trans)
      p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", Survey = "CPR", method = "loess", trans = trans) + 
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      
      p1 + p2 + patchwork::plot_layout(widths = c(3,1))
      
    }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler)
    
    output$timeseries1 <- renderPlot({
      ts1()
    }, height = function() {length(unique(selectedData()$BioRegion)) * 200}) 
    
    
    # Climatologies -----------------------------------------------------------
    
    output$timeseries2 <- renderPlot({
      
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
      
      p1 / (p2 | p3) + patchwork::plot_layout(guides = "collect") + patchwork::plot_annotation(
        title = titleplot)
      
      
    }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler)
    
    # Functional groups -------------------------------------------------------
    
    selectedDataFG <- reactive({
      req(input$region)
      validate(need(!is.na(input$region), "Error: Please select a bioregion"))
      
      selectedDataFG <- CPRfgp %>% 
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
    
    ts3 <- reactive({
      
      if (is.null(CPRfgp$BioRegion)) {  
        return(NULL)
      }
      
      scale <- dplyr::if_else(input$scaler3, "Actual", "Percent")
      
      p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
      p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month")
      
      p1 + p2 + patchwork::plot_layout(widths = c(3,1))
      
    }) %>% bindCache(input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
    
    output$timeseries3 <- renderPlot({
      ts3()
    }, height = function() {length(unique(selectedDataFG()$BioRegion)) * 200})     
    
    # Download -------------------------------------------------------
  })
}
