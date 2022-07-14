#' PhytoTsNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PhytoTsNRS_ui <- function(id){
  nsPhytoTsNRS <- NS(id)
  tagList(
    sidebarLayout(
      fPlanktonSidebar(id = id, panel_id = "NRSpts", input = input, dat = datNRSp),
      mainPanel(
        tabsetPanel(id = "NRSpts",
                    tabPanel("Trend Analysis", value = 1,
                             h6(textOutput(nsPhytoTsNRS("PlotExp1"), container = span)),
                             plotOutput(nsPhytoTsNRS("timeseries1"), height = "auto") %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Climatologies", value = 2,
                             h6(textOutput(nsPhytoTsNRS("PlotExp2"), container = span)),  
                             plotOutput(nsPhytoTsNRS("timeseries2"), height = 800) %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Functional groups", value = 3,
                             h6(textOutput(nsPhytoTsNRS("PlotExp3"), container = span)),  
                             plotOutput(nsPhytoTsNRS("timeseries3"), height = "auto") %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1")
                    )
        )
      )
    )
  )
}

#' PhytoTsNRS Server Functions
#'
#' @noRd 
mod_PhytoTsNRS_server <- function(id){
  moduleServer(id, function(input, output, session, NRSpts){
    
    # Sidebar ----------------------------------------------------------
    
    # observeEvent({input$NRSpt == 1 | input$NRSpt == 2}, {
    selectedData <- reactive({
      req(input$Site)
      req(input$parameter)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      selectedData <- datNRSp %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter,input$Site, input$DatesSlide[1], input$DatesSlide[2])
    # }) 
    
    output$plotmap <- renderPlot({ 
      planktonr::pr_plot_NRSmap(selectedData())
    }) %>% bindCache(input$Site)
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected phytoplantkon Parameters from the NRS around Australia, as a time series and a monthly climatology by station."
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected indicies from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
    }) 
    output$PlotExp3 <- renderText({
      "A plot of functional groups from the light microscope phytoplankton counts from the NRS around Australia, as a time series and a monthly climatology"
    }) 
    
    
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$NRSpts == 1}, {
      
      gg_out1 <- reactive({
        
        if (is.null(datNRSp$StationCode)) {return(NULL)}
        
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", Survey = "NRS", method = "lm", trans = trans)
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", Survey = "NRS", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")
        
      }) %>% bindCache(input$parameter,input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler)
      
      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {length(unique(selectedData()$StationName)) * 200}) 
      
      
      # Download -------------------------------------------------------
      output$downloadData <- fDownloadDataServer(input, selectedData()) # Download csv of data
      output$downloadPlot <- fDownloadPlotServer(input, gg_id = gg_out1()) # Download figure
    }) 
    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    observeEvent({input$NRSpt == 2}, {
      
      gg_out2 <- reactive({
        
        if (is.null(datNRSp$StationCode)) {return(NULL)}
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        p1 <- planktonr::pr_plot_TimeSeries(selectedData(), Survey = "NRS", trans = trans) + 
          ggplot2::theme(legend.position = "none")
        
        p2 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Month", trans = trans) + 
          ggplot2::theme(legend.position = "none", axis.title.y = ggplot2::element_blank())
        
        p3 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Year", trans = trans) + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "bottom")
        
        titleplot <- names(planktonr::pr_relabel(input$parameter, style = "simple"))
        
        p1 / (p2 | p3) + patchwork::plot_layout(guides = "collect") + 
          patchwork::plot_annotation(title = titleplot)
        
      }) %>% bindCache(input$parameter,input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler)
      
      output$timeseries2 <- renderPlot({
        gg_out2()
      }) 
      
      # Download -------------------------------------------------------
      output$downloadData <- fDownloadDataServer(input, selectedData()) # Download csv of data
      output$downloadPlot <- fDownloadPlotServer(input, gg_id = gg_out2()) # Download figure
    })
    
    # Functional groups -------------------------------------------------------
    observeEvent({input$NRSpt == 3}, {
      
      selectedDataFG <- reactive({
        req(input$Site)
        validate(need(!is.na(input$Site), "Error: Please select a station."))
        
        selectedDataFG <- NRSfgp %>% 
          dplyr::filter(.data$StationName %in% input$Site,
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
          droplevels()
        
      }) %>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2])
      
      gg_out3 <- reactive({
        
        if (is.null(NRSfgp$StationCode)) {return(NULL)}
        scale <- dplyr::if_else(input$scaler3, "Actual", "Percent")
        
        p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
        p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month") + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "none")
        p1 + p2 + patchwork::plot_layout(widths = c(3,1))
        
      }) %>% bindCache(input$Site, input$scaler1, input$DatesSlide[1], input$DatesSlide[2])
      
      output$timeseries3 <- renderPlot({
        gg_out3()
      }, height = function() {length(unique(selectedData()$StationName)) * 200}) 
      
      # Download -------------------------------------------------------
      output$downloadData <- fDownloadDataServer(input, selectedDataFG()) # Download csv of data
      output$downloadPlot <- fDownloadPlotServer(input, gg_id = gg_out3()) # Download figure
      
    })
    
  })
}
