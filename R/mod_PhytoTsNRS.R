#' PhytoTsNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PhytoTsNRS_ui <- function(id){
  nsPhytoTsNRS <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition="input.NRSpts == 1",  
          checkboxInput(inputId = nsPhytoTsNRS("scaler"), label = strong("Change the plot scale to log10"), value = FALSE),
          selectInput(inputId = nsPhytoTsNRS("ycol"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datNRSp$parameters), style = "simple"), 
                      selected = "PhytoBiomassCarbon_pgL")
        ),
        conditionalPanel(
          condition="input.NRSpts == 2",  
          checkboxInput(inputId = nsPhytoTsNRS("scaler1"), label = strong("Change the plot scale to percent"), value = FALSE)
       ),
      absolutePanel(  
        plotlyOutput(nsPhytoTsNRS("plotmap")),
        checkboxGroupInput(inputId = nsPhytoTsNRS("Site"), label = "Select a station", choices = unique(sort(datNRSp$StationName)), selected = c("Maria Island", "Port Hacking", "Yongala")),
        sliderInput(nsPhytoTsNRS("DatesSlide"), "Dates:", min = lubridate::ymd_hms(20090101000000), max = Sys.time(), 
                    value = c(lubridate::ymd_hms(20090101000000),Sys.time()), timeFormat="%Y-%m-%d"),
        downloadButton(nsPhytoTsNRS("downloadData"), "Data"),
        downloadButton(nsPhytoTsNRS("downloadPlot"), "Plot"),
        downloadButton(nsPhytoTsNRS("downloadNote"), "Notebook")
        )
      ),
      mainPanel(
        tabsetPanel(id = "NRSpts",
                    tabPanel("Trend Analysis", value=1,
                             h6(textOutput(nsPhytoTsNRS("PlotExp1"), container = span)),
                             plotly::plotlyOutput(nsPhytoTsNRS("timeseries1")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Climatologies", value=1,
                             h6(textOutput(nsPhytoTsNRS("PlotExp2"), container = span)),  
                             plotly::plotlyOutput(nsPhytoTsNRS("timeseries2")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Functional groups", value=2,
                             h6(textOutput(nsPhytoTsNRS("PlotExp3"), container = span)),  
                             plotly::plotlyOutput(nsPhytoTsNRS("timeseries3")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
    selectedData <- reactive({
      req(input$Site)
      req(input$ycol)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      validate(need(!is.na(input$ycol), "Error: Please select a parameter."))
      
      selectedData <- datNRSp %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$parameters %in% input$ycol,
                      dplyr::between(.data$SampleDateLocal, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$ycol,input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    output$plotmap <- renderPlotly({ 
      pmap <- planktonr::pr_plot_NRSmap(selectedData())
    }) %>% bindCache(input$Site)
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected phytoplantkon parameters from the NRS around Australia, as a time series and a monthly climatology by station."
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected indicies from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
    }) 
    output$PlotExp3 <- renderText({
      "A plot of functional groups from the light microscope phytoplankton counts from the NRS around Australia, as a time series and a monthly climatology"
    }) 
    
    
    # Plot Trends -------------------------------------------------------------
    
    output$timeseries1 <- plotly::renderPlotly({
      
      if (is.null(datNRSp$StationCode)) {  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      }
      
      if(input$scaler){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      np <- length(unique(selectedData()$StationName))
      p1 <- planktonr::pr_plot_trends(selectedData(), trend = "Raw", survey = "NRS", method = "lm", pal = "matter", y_trans = Scale, output = "ggplot")
      p2 <- planktonr::pr_plot_trends(selectedData(), trend = "Month", survey = "NRS", method = "loess", pal = "matter", y_trans = Scale, output = "ggplot")
      p1 <- plotly::ggplotly(p1, height = 200 * np) 
      p2 <- plotly::ggplotly(p2, height = 200 * np)
      p <- plotly::subplot(p1,p2, 
                           titleY = TRUE,
                           widths = c(0.7,0.3))
      
    }) %>% bindCache(selectedData(), input$scaler)

    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    output$timeseries2 <- plotly::renderPlotly({
      
      if (is.null(datNRSp$StationCode)) {  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      }
    # 
      if(input$scaler){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      np <- length(unique(selectedData()$StationName))
      p1 <- planktonr::pr_plot_timeseries(selectedData(), 'NRS', 'matter', Scale) + ggplot2::theme(legend.position = 'none',
                                                                                                   axis.title.y = ggplot2::element_blank())
      
      p2 <- planktonr::pr_plot_climate(selectedData(), 'NRS', Month, 'matter', Scale) + ggplot2::theme(legend.position = 'none',
                                                                                                       axis.title.y = ggplot2::element_blank())
      
      p3 <- planktonr::pr_plot_climate(selectedData(), 'NRS', Year, 'matter', Scale) + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                                                                                      legend.title = ggplot2::element_blank())
      
      titley <- planktonr::pr_relabel(unique(selectedData()$parameters), style = "plotly")
      p1 <- plotly::ggplotly(p1, height = 200 * np) 
      p2 <- plotly::ggplotly(p2, height = 200 * np)
      p3 <- plotly::ggplotly(p3, height = 200 * np)
      p <- plotly::subplot(p1 %>% plotly::layout(showlegend = FALSE),
                           p2 %>% plotly::layout(yaxis = list(title = titley)), 
                           p3 %>% plotly::layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
                                                               title = '',  x = 0.5, y = -0.2)), 
                           nrows = 3,
                           titleY = TRUE)
      
    }) %>% bindCache(selectedData(), input$scaler)
    
    # Functional groups -------------------------------------------------------
    
    selectedDataFG <- reactive({
      req(input$Site)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      
      selectedDataFG <- NRSfgp %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      dplyr::between(.data$SampleDateLocal, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    output$timeseries3 <- plotly::renderPlotly({
      
      if (is.null(NRSfgp$StationCode)) {  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      }
      
      if(input$scaler1){
        scale <- 'Percent'
      } else {
        scale <- 'Actual'
      }
      
      titley <- planktonr::pr_relabel("FunctionalGroup_CellsL", style = "plotly")
      np <- length(unique(selectedDataFG()$StationName))
      p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
      p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, "Month")
      p1 <- plotly::ggplotly(p1, height = 200 * np)
      p2 <- plotly::ggplotly(p2, height = 200 * np)
      s1  <- plotly::subplot((p1 %>% plotly::layout(yaxis = list(title = titley))), 
                             p2 %>% plotly::layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
                                                                 title = '',  x = 0.5, y = -0.2)),
                             titleY = TRUE, 
                             widths = c(0.7, 0.3))
      
    }) %>% bindCache(selectedDataFG(), input$scaler1, input$DatesSlide[1], input$DatesSlide[2])
    
    
    
    # Downloads ---------------------------------------------------------------
    
    
    # Table of selected dataset ----
    output$table <- renderTable({
      # datasetInput()
    })
    
    #Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(input$ycol),"_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".csv")
      },
      content = function(file) {
        vroom::vroom_write(selectedData(), file, delim = ",")
      })
    
    # Download figure
    # output$downloadPlot <- downloadHandler(
    #   filename = function() {paste(input$ycol, '.png', sep='') },
    #   content = function(file) {
    #     ggsave(file, plot = plotInput(), device = "png")
    #   })
  })
}
