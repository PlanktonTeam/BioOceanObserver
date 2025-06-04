#' MicroTsNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MicroTsNRS_ui <- function(id){
  nsMicroTsNRS <- NS(id)
  tagList(
    sidebarLayout(
      fPlanktonSidebar(id = id, tabsetPanel_id = "NRSmts", dat = pkg.env$datNRSm),
      fPLanktonPanel(id = id,  tabsetPanel_id = "NRSmts"),
      
    )
  )
}


#' MicroTSNRS Server Functions
#'
#' @noRd 
mod_MicroTsNRS_server <- function(id){
  moduleServer(id, function(input, output, session, NRSmts){
    
    # Sidebar ----------------------------------------------------------
    observeEvent(input$all, {
      if(input$all == TRUE){
        params <- planktonr::pr_relabel(unique(pkg.env$datNRSm$Parameters), style = "simple", named = TRUE)
      } else {
        params <- planktonr::pr_relabel(unique((pkg.env$datNRSm %>% 
                                        dplyr::filter(grepl("Temperature_Index_KD|Abund|gene|ASV", .data$Parameters)))$Parameters), style = "simple", named = TRUE)
      }
      shiny::updateSelectInput(session, 'parameterm', choices = params, selected = "Bacterial_Temperature_Index_KD")
    })
    
    selectedData <- reactive({
      
      selectedData <- pkg.env$datNRSm %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% input$parameterm,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels() %>% 
        dplyr::mutate(name = as.factor(.data$Parameters))
      
    }) %>% bindCache(input$parameterm, input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    shiny::exportTestValues(
      MicroTs = {ncol(selectedData())},
      MicroTsRows = {nrow(selectedData()) > 0},
      MicroTsYearisNumeric = {class(selectedData()$Year_Local)},
      MicroTsMonthisNumeric = {class(selectedData()$Month_Local)},
      MicroTsDepthisNumeric = {class(selectedData()$SampleDepth_m)},
      MicroTsDateisDate = {class(selectedData()$SampleTime_Local)},
      MicroTsStationisFactor = {class(selectedData()$StationName)},
      MicroTsCodeisChr = {class(selectedData()$StationCode)},
      MicroTsParametersisChr = {class(selectedData()$Parameters)},
      MicroTsValuesisNumeric = {class(selectedData()$Values)}
    )
    
    # Sidebar Map
    output$plotmap <- renderPlot({ 
      planktonr::pr_plot_NRSmap(unique(selectedData()$StationCode))
    }, bg = "transparent") %>% bindCache(input$Site)
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected microbial indices from the NRS around Australia, as a time series and a monthly climatology by station averaged across all depths."
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected indices from the NRS around Australia, as a time series, a monthly climatology and an annual mean averaged across all depths."
    }) 
    output$PlotExp3 <- renderText({
      "A contour plot of microbial indices from the NRS around Australia, as a time series and a monthly climatology by depth. 
      If raw data is used the dots represent actual samples"
    }) 
    output$PlotExp4 <- renderText({
      "A plot of microbial indices against abundance measure from the NRS around Australia"
    }) 
    
    
    
    # Plot Trends -------------------------------------------------------------
    
    observeEvent({input$NRSmts == 1}, {
      
      gg_out1 <- reactive({
        
        if(length(selectedData()$Parameters)>0){
          
        if(input$scaler1){
          trans <- 'log10'
        } else {
          trans <- 'identity'
        }
        
        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", method = "lm", trans = trans)
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")
        } else {
          ggplot2::ggplot + ggplot2::geom_blank()
        }
        
      }) %>% bindCache(input$parameterm, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {length(unique(selectedData()$StationName)) * 200}) 
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Trend") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Trend") # Download figure
      
      # Parameter Definition
      output$ParamDefm <- fParamDefServer(selectedData) # Download csv of data
      
    })
    
    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    observeEvent({input$NRSmts == 2}, {
      
      gg_out2 <- reactive({
        
        if (is.null(pkg.env$datNRSm$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
          return(NULL)
        
        trans <- 'identity'
        if(input$scaler1){
          trans <- 'log10'
        }
        
        p1 <- planktonr::pr_plot_TimeSeries(selectedData(), trans = trans) + 
          ggplot2::theme(legend.position = "none")
        
        p2 <- planktonr::pr_plot_Climatology(selectedData(), Trend = "Month", trans = trans) + 
          ggplot2::theme(legend.position = "none")
        
        p3 <- planktonr::pr_plot_Climatology(selectedData(), Trend = "Year", trans = trans) + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        #titley <- names(planktonr::pr_relabel(unique(selectedData()$Parameters), style = "simple", named = TRUE))
        
        # p1 / (p2 | p3) + patchwork::plot_layout(guides = "collect")
        p1 / 
          (p2 + p3 + patchwork::plot_layout(ncol = 2, guides = "collect") & ggplot2::theme(legend.position = "bottom")) #+
        # patchwork::plot_annotation(title = titleplot)
        
      }) %>% bindCache(input$parameterm, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
      output$timeseries2 <- renderPlot({
        gg_out2()
      })
      
      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData, "Climate") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2, "Climate") # Download figure
      
      # Parameter Definition
      output$ParamDefm <- fParamDefServer(selectedData)
      
    })
    
    # Plots by depths ---------------------------------------------------------
    
    observeEvent({input$NRSmts == 3}, {
      
      selectedDataDepth <- reactive({
        
        selectedDataDepth <- selectedData() %>% 
          tidyr::drop_na() %>%
          dplyr::mutate(SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'month')) %>%
          planktonr::pr_remove_outliers(2) %>%
          droplevels() %>%
          planktonr::pr_reorder()
        
      }) %>% bindCache(input$parameterm, input$Site, input$DatesSlide[1], input$DatesSlide[2])
      
      gg_out3 <-  reactive({  
        
        interp <-  input$interp
        
        if(interp == 'Interpolate'){
          planktonr::pr_plot_NRSEnvContour(selectedDataDepth(), na.fill = TRUE)
        } else {
          planktonr::pr_plot_NRSEnvContour(selectedDataDepth(), na.fill = FALSE)
        }
        
      }) %>% bindCache(input$parameterm, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$interp)
      
      output$timeseries3 <- renderPlot({
        gg_out3()
      }, height = function() {
        if(length(unique(selectedDataDepth()$StationName)) < 2) 
        {300} else 
        {length(unique(selectedDataDepth()$StationName)) * 200}})
      
      # Download -------------------------------------------------------
      output$downloadData3 <- fDownloadButtonServer(input, selectedData, "Enviro") # Download csv of data
      output$downloadPlot3 <- fDownloadPlotServer(input, gg_id = gg_out3, "Enviro") # Download figure
      
      # Parameter Definition
      output$ParamDefm <- fParamDefServer(selectedData)
      
    })
    

  })
}
