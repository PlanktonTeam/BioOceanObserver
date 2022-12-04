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
      sidebarPanel(
        conditionalPanel(
          condition="input.NRSmts == 1",
          checkboxInput(inputId = nsMicroTsNRS("scaler1"), label = strong("Change the plot scale to log10"), value = FALSE)
        ),
        conditionalPanel(
          condition = "input.NRSmts == 2",
          selectizeInput(inputId = nsMicroTsNRS("interp"), label = strong("Interpolate data?"), choices = c("Interpolate", "Raw data", "Interpolate with gap filling"), selected = "Interpolate")
        ),
        conditionalPanel(
          condition = "input.NRSmts == 1 | input.NRSmts == 2", 
          sliderInput(nsMicroTsNRS("DatesSlide"), "Dates:", min = as.POSIXct('2009-01-01 00:00',
                                                                             format = "%Y-%m-%d %H:%M",
                                                                             tz = "Australia/Hobart"), max = Sys.time(), 
                      value = c(as.POSIXct('2009-01-01 00:00',
                                           format = "%Y-%m-%d %H:%M",
                                           tz = "Australia/Hobart"), Sys.time()-1), timeFormat="%Y-%m-%d"),
          selectInput(inputId = nsMicroTsNRS("ycol"), label = 'Select a parameter', 
                      choices = planktonr::pr_relabel(unique(datNRSm$Parameters), 
                                                      style = "simple"), selected = "Bacterial_Richness"),
        ),
        conditionalPanel(
          condition = "input.NRSmts == 3", 
          selectInput(inputId = nsMicroTsNRS("p1"), label = 'Select an x parameter', choices = planktonr::pr_relabel(unique(datNRSm$Parameters), style = "simple"), selected = "Eukaryote_Chlorophyll_Index"),
          selectInput(inputId = nsMicroTsNRS("p2"), label = 'Select a y parameter', 
                      choices = planktonr::pr_relabel(unique(Pico$Parameters), style = "simple"), selected = "Prochlorococcus_cellsmL")
        ),
        conditionalPanel(
          condition = "input.NRSmts == 1 | input.NRSmts == 2 | input.NRSmts == 3", 
          plotOutput(nsMicroTsNRS("plotmap")),
          checkboxGroupInput(inputId = nsMicroTsNRS("Site"), label = "Select a station", choices = unique(sort(datNRSm$StationName)), selected = c("Maria Island", "Port Hacking", "Yongala"))
        )
      ),
      mainPanel(
        tabsetPanel(id = "NRSmts",
                    tabPanel("Trend Analysis", value=1, 
                             h6(textOutput(nsMicroTsNRS("PlotExp1"), container = span)),
                             plotOutput(nsMicroTsNRS("timeseries1"), height = "auto") %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1"),
                             div(style="display:inline-block; float:right; width:60%",
                                 fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                                 fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
                                 fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action"))
                    ),
                    tabPanel("Climatologies", value=1,
                             h6(textOutput(nsMicroTsNRS("PlotExp2"), container = span)),
                             plotOutput(nsMicroTsNRS("timeseries2")) %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1"),
                             div(style="display:inline-block; float:right; width:60%",
                                 fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
                                 fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"),
                                 fButtons(id, button_id = "downloadCode2", label = "R Code Example", Type = "Action"))
                    ),
                    tabPanel("Trend analysis by depth", value=2,
                             h6(textOutput(nsMicroTsNRS("PlotExp3"), container = span)),  
                             plotOutput(nsMicroTsNRS("timeseries3"), height = 'auto') %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1"),
                             div(style="display:inline-block; float:right; width:60%",
                                 fButtons(id, button_id = "downloadPlot3", label = "Plot", Type = "Download"),
                                 fButtons(id, button_id = "downloadData3", label = "Data", Type = "Download"),
                                 fButtons(id, button_id = "downloadCode3", label = "R Code Example", Type = "Action"))
                    ),
                    tabPanel("Cell counts vs Indices", value=3,
                             h6(textOutput(nsMicroTsNRS("PlotExp4"), container = span)),  
                             plotOutput(nsMicroTsNRS("timeseries4")) %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1"),
                             div(style="display:inline-block; float:right; width:60%",
                                 fButtons(id, button_id = "downloadPlot4", label = "Plot", Type = "Download"),
                                 fButtons(id, button_id = "downloadData4", label = "Data", Type = "Download"),
                                 fButtons(id, button_id = "downloadCode4", label = "R Code Example", Type = "Action"))
                    )
        )
      )
    )
  )
}


#' MicroTSNRS Server Functions
#'
#' @noRd 
mod_MicroTsNRS_server <- function(id){
  moduleServer(id, function(input, output, session, NRSmts){
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
      selectedData <- datNRSm %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% input$ycol,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels() %>% 
        dplyr::mutate(name = as.factor(.data$Parameters))
      
    }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    shiny::exportTestValues(
      MicroTs = {ncol(selectedData())},
      MicroTsRows = {nrow(selectedData()) > 0},
      MicroTsYearisNumeric = {class(selectedData()$Year)},
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
      planktonr::pr_plot_NRSmap(selectedData())
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
        
        if (is.null(datNRSm$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
          return(NULL)
        
        if(input$scaler1){
          trans <- 'log10'
        } else {
          trans <- 'identity'
        }
        
        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", Survey = "NRS", method = "lm", trans = trans)
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", Survey = "NRS", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")
        
      }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {length(unique(selectedData()$StationName)) * 200}) 
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData(), "Trend") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Trend") # Download figure
      
    })
    
    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    observeEvent({input$NRSmts == 1}, {
      
      gg_out2 <- reactive({
        
        if (is.null(datNRSm$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
          return(NULL)
        
        trans <- 'identity'
        if(input$scaler1){
          trans <- 'log10'
        }
        
        p1 <- planktonr::pr_plot_TimeSeries(selectedData(), Survey = "NRS", trans = trans) + 
          ggplot2::theme(legend.position = "none")
        
        p2 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Month", trans = trans) + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        p3 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Year", trans = trans) + 
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        #titley <- names(planktonr::pr_relabel(unique(selectedData()$Parameters), style = "simple"))
        
        p1 / (p2 | p3) + patchwork::plot_layout(guides = "collect")
        
        
      }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
      
      output$timeseries2 <- renderPlot({
        gg_out2()
      })
      
      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData(), "Climate") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2(), "Climate") # Download figure
    })
    
    # Plots by depths ---------------------------------------------------------
    
    observeEvent({input$NRSmts == 2}, {
      
      selectedDataDepth <- reactive({
        selectedDataDepth <- selectedData() %>% 
          tidyr::drop_na() %>%
          dplyr::mutate(SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'month')) %>%
          planktonr::pr_remove_outliers(2) %>%
          droplevels() %>%
          planktonr::pr_reorder()
        
      }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2])
      
      gg_out3 <-  reactive({  
        
        interp <-  input$interp
        
        if(interp == 'Interpolate'){
          planktonr::pr_plot_NRSEnvContour(selectedDataDepth(), Interpolation = TRUE, Fill_NA = FALSE)
        } else if (interp == 'Interpolate with gap filling'){
          planktonr::pr_plot_NRSEnvContour(selectedDataDepth(), Interpolation = TRUE, Fill_NA = TRUE, maxGap = 3)
        } else {
          planktonr::pr_plot_NRSEnvContour(selectedDataDepth(), Interpolation = FALSE, Fill_NA = FALSE)
        }
        
      }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$interp)
      
      output$timeseries3 <- renderPlot({
        gg_out3()
      }, height = function() {length(unique(selectedDataDepth()$StationName)) * 200})
      
      # Download -------------------------------------------------------
      output$downloadData3 <- fDownloadButtonServer(input, selectedData(), "Enviro") # Download csv of data
      output$downloadPlot3 <- fDownloadPlotServer(input, gg_id = gg_out3(), "Enviro") # Download figure
      
    })
    
    # Plots by Parameters ---------------------------------------------------------
    
    observeEvent({input$NRSmts == 3}, {
      
      selectedData1 <- reactive({
        req(input$Site)
        req(input$p1)
        validate(need(!is.na(input$Site), "Error: Please select a station."))
        validate(need(!is.na(input$p1), "Error: Please select a parameter."))
        
        selectedData1 <- datNRSm %>% 
          dplyr::filter(.data$StationName %in% input$Site,
                        .data$Parameters %in% c(input$p1, input$p2)) %>%
          tidyr::pivot_wider(c("StationName", "SampleDepth_m", "SampleTime_Local"), names_from = "Parameters", values_from = "Values", values_fn = mean)
        
      }) %>% bindCache(input$p1, input$p2, input$Site)
      
      gg_out4 <- reactive({
        x <- rlang::sym(colnames(selectedData1()[, 5]))
        y <- rlang::sym(colnames(selectedData1()[, 4]))
        
        titlex <- planktonr::pr_relabel(rlang::as_string(x), style = "ggplot")
        titley <- planktonr::pr_relabel(rlang::as_string(y), style = "ggplot")
        
        ggplot2::ggplot(data = selectedData1()) +
          ggplot2::geom_point(ggplot2::aes(!!x, !!y, colour = .data$StationName)) +
          ggplot2::xlab(titlex) + ggplot2::ylab(titley) 
        
      }) %>% bindCache(input$p1, input$p2, input$Site, input$DatesSlide[1], input$DatesSlide[2])
      
      output$timeseries4 <- renderPlot({
        gg_out4()
      })
      
      # Download -------------------------------------------------------
      output$downloadData4 <- fDownloadButtonServer(input, selectedData1(), "Compare") # Download csv of data
      output$downloadPlot4 <- fDownloadPlotServer(input, gg_id = gg_out4(), "Compare") # Download figure
      
    })
  })
}
