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
      fPlanktonSidebar(id = id, tabsetPanel_id = "NRSpts", dat = pkg.env$datNRSp, dat1 = pkg.env$SOTSp),
      fPLanktonPanel(id = id, tabsetPanel_id = "NRSpts")
    )
  )
}

#' PhytoTsNRS Server Functions
#'
#' @noRd 
mod_PhytoTsNRS_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    # Sidebar ----------------------------------------------------------
    # observeEvent({input$NRSpt == 1 | input$NRSpt == 2}, {
    selectedData <- reactive({ #TODO - This reactive encompasses things from 1/2 AND 3. Can we split them?
      
      req(input$site)
      req(input$parameter)
      shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      selectedData <- pkg.env$datNRSp %>%
        dplyr::bind_rows(pkg.env$SOTSp) %>% #dplyr::filter(.data$SampleDepth_m < 20)) %>% #TODO should be able to remove filter once in planktonr
        dplyr::filter(.data$StationName %in% input$site,
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>% 
        planktonr::pr_reorder()
      
    }) %>% bindCache(input$parameter,input$site, input$DatesSlide[1], input$DatesSlide[2])
    # })
    
    # Sidebar Map - Initial render
    output$plotmap <- leaflet::renderLeaflet({
      fLeafletMap(character(0), Survey = "NRS", Type = "Phytoplankton")
    })
    
    # Update map when station selection changes
    observe({
      fLeafletUpdate("plotmap", session, unique(selectedData()$StationCode), 
                     Survey = "NRS", Type = "Phytoplankton")
    })

    # add text information
    output$PlotExp1 <- renderText({
      if('Southern Ocean Time Series' %in% input$site) {
        paste0("A plot of selected phytoplankton Parameters from the NRS around Australia, as a time series and a monthly climatology by station.<br>
        <b>Note:</b> At SOTS, phytoplankton samples were initially collected around 30m, from 2020 onwards this was changed to 10m")
    } else {
      "A plot of selected phytoplankton Parameters from the NRS around Australia, as a time series and a monthly climatology by station."
    }
      })
    output$PlotExp2 <- renderText({
      "A plot of selected indicies from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
    })
    output$PlotExp3 <- renderText({
      if('Southern Ocean Time Series' %in% input$site) {
        paste0("A plot of functional groups from the light microscope phytoplankton counts from the NRS around Australia, as a time series and a monthly climatology.<br>
        <b>Note:</b> At SOTS, phytoplankton samples were initially collected around 30m, from 2020 onwards this was changed to 10m")
      } else {
        "A plot of functional groups from the light microscope phytoplankton counts from the NRS around Australia, as a time series and a monthly climatology."}
    })

    # Plot Trends -------------------------------------------------------------
    observeEvent({input$NRSpts == 1}, {
      
      gg_out1 <- reactive({
        if (is.null(pkg.env$datNRSp$StationCode)) {return(NULL)}
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", method = "lm", trans = trans)
        
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")
        
      }) %>% bindCache(input$parameter,input$site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {length(unique(selectedData()$StationName)) * 200})
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Trend") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Trend") # Download figure
      
      # Parameter Definition
      output$ParamDef <- fParamDefServer(selectedData)
      
    })
    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    observeEvent({input$NRSpts == 2}, {
      
      gg_out2 <- reactive({
        if (is.null(pkg.env$datNRSp$StationCode)) {return(NULL)}
        
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        p1 <- planktonr::pr_plot_TimeSeries(selectedData(), trans = trans) +
          ggplot2::theme(legend.position = "none")
        
        p2 <- planktonr::pr_plot_Climatology(selectedData(), Trend = "Month", trans = trans) +
          ggplot2::theme(legend.position = "none")
        
        p3 <- planktonr::pr_plot_Climatology(selectedData(), Trend = "Year", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "bottom")
        
        p1 / 
          (p2 + p3 + patchwork::plot_layout(ncol = 2, guides = "collect") & ggplot2::theme(legend.position = "bottom"))
        
      }) %>% bindCache(input$parameter, input$site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
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
        req(input$site)
        shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
        
        selectedDataFG <- pkg.env$NRSfgp %>%
          dplyr::bind_rows(pkg.env$SOTSfgp) %>% #dplyr::filter(.data$SampleDepth_m < 20)) %>% 
          dplyr::filter(.data$StationName %in% input$site,
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
          planktonr::pr_reorder() %>% 
          droplevels()
        
      }) %>% bindCache(input$site, input$DatesSlide[1], input$DatesSlide[2])
      
      gg_out3 <- reactive({
        
        if (is.null(pkg.env$NRSfgp$StationCode)) {return(NULL)}
        
        scale <- dplyr::if_else(input$scaler3, "Proportion", "Actual")
        
        p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)

        p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month") +
          ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "none")
        
        p1 + p2 + patchwork::plot_layout(widths = c(3,1))
        
      }) %>% bindCache(input$site, input$scaler3, input$DatesSlide[1], input$DatesSlide[2])
      
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
