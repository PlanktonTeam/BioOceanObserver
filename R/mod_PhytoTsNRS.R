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
  moduleServer(id, function(input, output, session, NRSpts){
    
    # fPLanktonPanelServer(id, panel_id, input)
    
    
    # Sidebar ----------------------------------------------------------
    # observeEvent({input$NRSpt == 1 | input$NRSpt == 2}, {
    selectedData <- reactive({ #TODO - This reactive encompasses things from 1/2 AND 3. Can we split them?
      req(input$Site)
      req(input$parameter)
      shiny::validate(need(!is.na(input$Site), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      selectedData <- pkg.env$datNRSp %>%
        dplyr::bind_rows(pkg.env$SOTSp %>% dplyr::filter(.data$SampleDepth_m < 20)) %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>% 
        planktonr::pr_reorder()
      
    }) %>% bindCache(input$parameter,input$Site, input$DatesSlide[1], input$DatesSlide[2])
    # })
    
    output$plotmap <- renderPlot({
      planktonr::pr_plot_NRSmap(unique(selectedData()$StationCode), Type = 'Phytoplankton')
    }, bg = "transparent") %>% bindCache(input$Site)
    
    # add text information
    output$PlotExp1 <- renderText({
      if('Southern Ocean Time Series' %in% input$Site) {
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
      if('Southern Ocean Time Series' %in% input$Site) {
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
        
        if('Southern Ocean Time Series' %in% input$Site & lubridate::year(input$DatesSlide[1]) < 2015){
          sots30 <- pkg.env$SOTSp %>% 
            dplyr::filter(.data$SampleDepth_m > 20,
                          .data$Parameters %in% "PhytoAbundance_CellsL",
                          dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
            dplyr::mutate(StationName = factor(.data$StationName,
                                               levels = c("Darwin", "Yongala", "Ningaloo", "North Stradbroke Island",
                                                          "Rottnest Island", "Esperance", "Port Hacking", "Kangaroo Island",
                                                          "Bonney Coast", "Maria Island", "Southern Ocean Time Series",
                                                          "Southern Ocean Time Series - Remote Access Sampler")))
          
          p1 <- p1 +
            ggplot2::geom_point(data = sots30, ggplot2::aes(x = SampleTime_Local, y = Values), colour = 'black', alpha = 0.2) +
            ggplot2::geom_smooth(data = sots30, ggplot2::aes(x = SampleTime_Local, y = Values), method = "lm", alpha = 0.2,
                                 formula = 'y ~ x') 
        }
        
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")
        
      }) %>% bindCache(input$parameter,input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
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
    observeEvent({input$NRSpts == 3}, {
      
      selectedDataFG <- reactive({
        req(input$Site)
        shiny::validate(need(!is.na(input$Site), "Error: Please select a station."))
        
        selectedDataFG <- pkg.env$NRSfgp %>%
          dplyr::bind_rows(pkg.env$SOTSfgp %>% dplyr::filter(.data$SampleDepth_m < 20)) %>% 
          dplyr::filter(.data$StationName %in% input$Site,
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
          planktonr::pr_reorder() %>% 
          droplevels()
        
      }) %>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2])
      
      gg_out3 <- reactive({
        
        if (is.null(pkg.env$NRSfgp$StationCode)) {return(NULL)}
        scale <- dplyr::if_else(input$scaler3, "Percent", "Actual")
        
        if('Southern Ocean Time Series' %in% input$Site & lubridate::year(input$DatesSlide[1]) < 2015){
          sotsfg30 <- SOTSfgp %>% 
            dplyr::filter(dplyr::between(.data$SampleDepth_m, 20, 34.5),
                          dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
            dplyr::mutate(group = 'group1',
                          StationName = factor(.data$StationName,
                                               levels = c("Darwin", "Yongala", "Ningaloo", "North Stradbroke Island",
                                                          "Rottnest Island", "Esperance", "Port Hacking", "Kangaroo Island",
                                                          "Bonney Coast", "Maria Island", "Southern Ocean Time Series",
                                                          "Southern Ocean Time Series - Remote Access Sampler")))
          selectedDataFG <- selectedDataFG() %>% 
            dplyr::mutate(group = 'group2') %>% 
            dplyr::bind_rows(sotsfg30)

          if(scale == "Percent") {
            selectedDataFG <- selectedDataFGf %>%
              dplyr::summarise(n = sum(.data$Values, na.rm = TRUE),
                               .by = c('StationName', "Parameters")) %>%
              dplyr::mutate(Values = .data$n / sum(.data$n, na.rm = TRUE))
          } else {
            selectedDataFG <- selectedDataFG %>%
              dplyr::mutate(Values = log10(.data$Values))
          }
          
          lims <- as.POSIXct(strptime(c(min(selectedDataFG$SampleTime_Local),max(selectedDataFG$SampleTime_Local)), format = "%Y-%m-%d %H:%M"))
          
          p1 <- ggplot2::ggplot(selectedDataFG, ggplot2::aes(x = SampleTime_Local, y = Values, fill = Parameters, group = interaction(Parameters, group))) +
              ggplot2::geom_area(alpha = 0.9 , linewidth = 0.2, colour = "white") +
              ggplot2::facet_wrap(~StationName, scales = "free", ncol = 1) +
              ggplot2::labs(y = planktonr::pr_relabel("PhytoAbundance_CellsL", style = "ggplot")) +
              ggplot2::scale_fill_brewer(palette = "Set1", drop = FALSE) +
              planktonr::theme_pr() +
              ggplot2::scale_y_continuous(expand = c(0,0)) +
              ggplot2::theme(legend.title = ggplot2::element_blank(),
                             strip.text = ggplot2::element_text(hjust = 0)) +
              ggplot2::scale_x_datetime(date_breaks = "2 years", limits = lims, date_labels = "%Y", expand = ggplot2::expansion(add = c(0.15, 0.15))) +
              ggplot2::xlab("Sample Date")
            
        } else {
          
          p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
        
          }
          
        p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month") +
          ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "none")
        
        p1 + p2 + patchwork::plot_layout(widths = c(3,1))
        
      }) %>% bindCache(input$Site, input$scaler3, input$DatesSlide[1], input$DatesSlide[2])
      
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
