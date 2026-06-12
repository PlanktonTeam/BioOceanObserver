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
  ns <- NS(id)
  tagList(
    sidebarLayout(
      fPlanktonSidebar(id = id, tabsetPanel_id = "NRSpts", dat = pkg.env$datNRSp_all),
      fPLanktonPanel(id = id, tabsetPanel_id = "NRSpts")
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
    selectedData <- reactive({ #TODO - This reactive encompasses things from 1/2 AND 3. Can we split them?
      
      req(input$site)
      req(input$parameter)
      shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      selectedData <- pkg.env$datNRSp_all %>%
        dplyr::filter(.data$StationName %in% input$site,
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>% 
        planktonr:::pr_reorder()
      
    }) %>% bindCache(input$parameter,input$site, input$DatesSlide[1], input$DatesSlide[2])
    # })
    
    # Sidebar Map - Initial render with current selection
    output$plotmap <- mapgl::renderMapboxgl({
      stationCodes <- if (length(input$site) > 0) {
        pkg.env$NRSStation %>%
          dplyr::bind_rows(pkg.env$SOTSinfo %>%
                             dplyr::select(-c(.data$ProjectName, .data$IMCRA, .data$IMCRA_PB))) %>%
          dplyr::filter(.data$StationName %in% input$site) %>%
          dplyr::pull(.data$StationCode)
      } else {
        character(0)
      }
      fMapboxMap(stationCodes, Survey = "NRS", Type = "Phytoplankton")
    })

    outputOptions(output, "plotmap", suspendWhenHidden = FALSE) # prevent shiny from suspending map when tab is hidden

    # Update map when station selection changes
    observe({
      stationCodes <- if (length(input$site) > 0) {
        pkg.env$NRSStation %>%
          dplyr::bind_rows(pkg.env$SOTSinfo %>%
                             dplyr::select(-c(.data$ProjectName, .data$IMCRA, .data$IMCRA_PB))) %>%
          dplyr::filter(.data$StationName %in% input$site) %>%
          dplyr::pull(.data$StationCode)
      } else {
        character(0)
      }
      fMapboxUpdate("plotmap", session, stationCodes,
                    Survey = "NRS", Type = "Phytoplankton")
    }) %>% shiny::bindEvent(input$site, ignoreNULL = FALSE)

    # add text information
    output$PlotExp1 <- renderText({
      if('Southern Ocean Time Series' %in% input$site) {
        paste0("A plot of selected phytoplankton Parameters from the NRS around Australia, as a time series and a monthly climatology by station.<br>
        <b>Note:</b> At SOTS, phytoplankton samples were initially collected around 30m, from 2020 onwards this was changed to 5-10m")
    } else {
      "A plot of selected phytoplankton Parameters from the NRS around Australia, as a time series and a monthly climatology by station."
    }
      })
    output$PlotExp2 <- renderText({
      if('Southern Ocean Time Series' %in% input$site) {
        paste0("A plot of selected indicies from the NRS around Australia, as a time series, a monthly climatology and an annual mean.<br>
        <b>Note:</b> At SOTS, phytoplankton samples were initially collected around 30m, from 2020 onwards this was changed to 5-10m")
      } else {
        "A plot of selected indicies from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
        }
    })
    output$PlotExp3 <- renderText({
      if('Southern Ocean Time Series' %in% input$site) {
        paste0("A plot of functional groups from the light microscope phytoplankton counts from the NRS around Australia, as a time series and a monthly climatology.<br>
        <b>Note:</b> At SOTS, phytoplankton samples were initially collected around 30m, from 2020 onwards this was changed to 5-10m")
      } else {
        "A plot of functional groups from the light microscope phytoplankton counts from the NRS around Australia, as a time series and a monthly climatology."}
    })

    # Plot Trends -------------------------------------------------------------
    gg_out1 <- reactive({
      if (is.null(pkg.env$datNRSp$StationCode)) {return(NULL)}
      trans <- dplyr::if_else(input$scaler1, "log10", "identity")
      
      p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", method = "lm", trans = trans)
      
      p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", method = "loess", trans = trans) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      
      p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")
      
    }) %>% bindCache(input$parameter, input$site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
    
    output$timeseries1 <- renderPlot({
      req(is.null(input$NRSpts) || input$NRSpts == "1")
      gg_out1()
    }, height = function() {length(unique(selectedData()$StationName)) * 200})
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Trend") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Trend") # Download figure
    
    # Parameter Definition
    output$ParamDef <- fParamDefServer(selectedData)
    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
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
    
    # Functional groups -------------------------------------------------------
    selectedDataFG <- reactive({
      req(input$site)
      shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
      
      selectedDataFG <- pkg.env$NRSfgp %>%
        #dplyr::bind_rows(pkg.env$SOTSfgp) %>%
        dplyr::filter(.data$StationName %in% input$site,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        planktonr:::pr_reorder() %>%
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
}
