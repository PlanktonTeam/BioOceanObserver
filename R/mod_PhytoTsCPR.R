#' PhytoTsCPR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PhytoTsCPR_ui <- function(id){
  nsPhytoTsCPR <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition="input.CPRpts == 1",
          checkboxInput(inputId = nsPhytoTsCPR("scaler"), label = strong("Change the plot scale to log10"), value = FALSE),
          selectInput(inputId = nsPhytoTsCPR("parameter"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datCPRp$parameters), style = "simple"), 
                      selected = "PhytoAbund_Cellsm3")
        ),
        conditionalPanel(
          condition="input.CPRpts == 2",
          checkboxInput(inputId = nsPhytoTsCPR("scaler1"), label = strong("Change the plot scale to percent"), value = FALSE)
        ),
        absolutePanel(
          plotlyOutput(nsPhytoTsCPR("plotmap")),
          h6("Note there is very little data in the North and North-west regions"),
          checkboxGroupInput(inputId = nsPhytoTsCPR("region"), label = "Select a region", choices = unique(sort(datCPRp$BioRegion)), selected = unique(datCPRp$BioRegion)),
          sliderInput(nsPhytoTsCPR("DatesSlide"), "Dates:", min = lubridate::ymd_hms(20090101000000), max = Sys.time(), 
                      value = c(lubridate::ymd_hms(20090101000000), Sys.time()-1), timeFormat="%Y-%m-%d"),
          downloadButton(nsPhytoTsCPR("downloadData"), "Data"),
          downloadButton(nsPhytoTsCPR("downloadPlot"), "Plot"),
          downloadButton(nsPhytoTsCPR("downloadNote"), "Notebook")
        )
     ),
      mainPanel(
        tabsetPanel(id = "CPRpts",
                    tabPanel("Trend Analysis", value = 1,
                             h6(textOutput(nsPhytoTsCPR("PlotExp1"), container = span)),  
                             plotly::plotlyOutput(nsPhytoTsCPR("timeseries1")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Climatologies", value = 1,
                             h6(textOutput(nsPhytoTsCPR("PlotExp2"), container = span)),  
                             plotly::plotlyOutput(nsPhytoTsCPR("timeseries2")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Functional groups", value = 2,
                             h6(textOutput(nsPhytoTsCPR("PlotExp3"), container = span)),  
                             plotly::plotlyOutput(nsPhytoTsCPR("timeseries3")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    )
        )
      )
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
                      .data$parameters %in% input$parameter,
                      dplyr::between(.data$SampleDate_UTC, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2])
    
    output$plotmap <- renderPlotly({ # renderCachedPlot plot so cached version can be returned if it exists (code only run once per scenario per session)
      plotmap <- planktonr::pr_plot_CPRmap(selectedData())
    }) %>% bindCache(selectedData())
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected Phytoplantkon parameters from the CPR around Australia, as a time series and a monthly climatology across bioregions. "
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected Phytoplantkon parameters from the CPR around Australia, as a time series, a monthly climatology and an annual mean for each bioregion"
    }) 
    output$PlotExp3 <- renderText({
      "A plot of functional groups from the light microscope phytoplankton counts from the CPR around Australia, as a time series and a monthly climatology for each bioregion"
    }) 
    
    
    # Plot Trends -------------------------------------------------------------

    output$timeseries1 <- plotly::renderPlotly({
      if(input$scaler){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      np <- length(unique(selectedData()$BioRegion))
      p1 <- planktonr::pr_plot_trends(selectedData(), trend = "Raw", survey = "CPR", method = "lm", pal = "matter", y_trans = Scale, output = "ggplot")
      p2 <- planktonr::pr_plot_trends(selectedData(), trend = "Month", survey = "CPR", method = "loess", pal = "matter", y_trans = Scale, output = "ggplot")
      p1 <- plotly::ggplotly(p1, height = 200 * np)
      p2 <- plotly::ggplotly(p2, height = 200 * np)
      p <- plotly::subplot(p1,p2, 
                           titleY = TRUE,
                           widths = c(0.7,0.3))
      
    }) %>% bindCache(selectedData(), input$scaler)
    
    
    # Climatologies -----------------------------------------------------------
    
    output$timeseries2 <- plotly::renderPlotly({
      if(input$scaler){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      if (identical(input$region, "")) return(NULL)
      if (identical(input$parameters, "")) return(NULL)
      
      np <- length(unique(selectedData()$BioRegion))
      p1 <- planktonr::pr_plot_timeseries(selectedData(), 'CPR', 'matter', Scale) + ggplot2::theme(legend.position = 'none',
                                                                                                   axis.title.y = ggplot2::element_blank())
      
      p2 <- planktonr::pr_plot_climate(selectedData(), 'CPR', Month, 'matter', Scale) + ggplot2::theme(legend.position = 'none',
                                                                                                       axis.title.y = ggplot2::element_blank())
      
      p3 <- planktonr::pr_plot_climate(selectedData(), 'CPR', Year, 'matter', Scale) + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
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
      req(input$region)
      validate(need(!is.na(input$region), "Error: Please select a bioregion"))
      
      selectedDataFG <- CPRfgp %>% 
        dplyr::filter(.data$BioRegion %in% input$region,
                      dplyr::between(.data$SampleDate_UTC, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
    }) %>% bindCache(input$region, input$DatesSlide[1], input$DatesSlide[2])
    
    output$timeseries3 <- plotly::renderPlotly({
      
      if (is.null(CPRfgp$BioRegion)) {  
        return(NULL)
      }
      
      if(input$scaler1){
        scale <- 'Percent'
      } else {
        scale <- 'Actual'
      }
      
      titley <- planktonr::pr_relabel("FunctionalGroup_CellsL", style = "plotly")
      np <- length(unique(selectedDataFG()$BioRegion))
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
    #   filename = function() {paste(input$parameter, '.png', sep='') },
    #   content = function(file) {
    #     ggsave(file, plot = plotInput(), device = "png")
    #   }
    # )
  })
}
