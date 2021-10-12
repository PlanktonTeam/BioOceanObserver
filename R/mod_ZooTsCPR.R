#' ZooTsCPR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZooTsCPR_ui <- function(id){
  nsZooTsCPR <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        plotlyOutput(nsZooTsCPR("plotmap")),
        h6("Note there is very little data in the North and North-west regions"),
        checkboxGroupInput(inputId = nsZooTsCPR("region"), label = "Select a region", choices = unique(sort(datCPRz$BioRegion)), selected = unique(datCPRz$BioRegion)),
        selectInput(inputId = nsZooTsCPR("parameter"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datCPRz$parameters), style = "simple"), selected = "ZoopAbundance_m3"),
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = nsZooTsCPR("scaler"), label = strong("Change the plot scale to log10"), value = FALSE),
        downloadButton(nsZooTsCPR("downloadData"), "Data"),
        downloadButton(nsZooTsCPR("downloadPlot"), "Plot"),
        downloadButton(nsZooTsCPR("downloadNote"), "Notebook")
      ),
      mainPanel(
        tabsetPanel(id = "CPRzts",
                    tabPanel("Trend Analysis",
                             h6(textOutput(nsZooTsCPR("PlotExp1"), container = span)),  
                             plotly::plotlyOutput(nsZooTsCPR("timeseries1"), height = "800px") %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Climatologies",
                             h6(textOutput(nsZooTsCPR("PlotExp2"), container = span)),  
                             plotly::plotlyOutput(nsZooTsCPR("timeseries2"), height = "800px") %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1")
                    )
        )
      )
    )
  )
}

#' ZooTsCPR Server Functions
#'
#' @noRd 
mod_ZooTsCPR_server <- function(id){
  moduleServer( id, function(input, output, session, CPRzts){
    
    
    # Sidebar ----------------------------------------------------------
    
    selectedData <- reactive({
      req(input$region)
      req(input$parameter)
      validate(need(!is.na(input$region), "Error: Please select a region"))
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      selectedData <- datCPRz %>% 
        mutate(BioRegion = factor(.data$BioRegion, levels = c("Coral Sea", "Temperate East", "South-west", "South-east"))) %>%
        dplyr::filter(.data$BioRegion %in% input$region,
                      .data$parameters %in% input$parameter) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter,input$region)
    
    
    output$plotmap <- renderPlotly({ # renderCachedPlot plot so cached version can be returned if it exists (code only run once per scenario per session)
      
      plotmap <- planktonr::pr_plot_CPRmap(selectedData())
      
    }) %>% bindCache(selectedData())
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected zooplankton parameters from the CPR around Australia, as a time series and a monthly climatology across bioregions. "
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected zooplankton parameters from the CPR around Australia, as a time series, a monthly climatology and an annual mean for each bioregion"
    }) 
    
    
    # Plot Trends -------------------------------------------------------------
    output$timeseries1 <- plotly::renderPlotly({
      if(input$scaler){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      p1 <- planktonr::pr_plot_trends(selectedData(), trend = "Raw", survey = "CPR", method = "lm", pal = "matter", y_trans = Scale, output = "plotly")
      p2 <- planktonr::pr_plot_trends(selectedData(), trend = "Month", survey = "CPR", method = "loess", pal = "matter", y_trans = Scale, output = "plotly")
      p <- plotly::subplot(p1,p2, titleY = TRUE, widths = c(0.7,0.3))
    }) %>% bindCache(selectedData(), input$scaler)
    
    
    # Climatologies -----------------------------------------------------------
    output$timeseries2 <- plotly::renderPlotly({
      if(input$scaler1){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      plots <- planktonr::pr_plot_tsclimate(selectedData(), 'CPR', 'matter', Scale) 
      
    }) %>% bindCache(selectedData(), input$scaler)
    
    
    
    # Downloads ---------------------------------------------------------------
    
    ## Table of selected dataset ----
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
    
    ## Download figure
    # output$downloadPlot <- downloadHandler(
    #   filename = function() {paste(input$parameter, '.png', sep='') },
    #   content = function(file) {
    #     ggsave(file, plot = plotInput(), device = "png")
    #   })
  })
}
