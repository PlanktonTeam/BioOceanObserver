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
        plotlyOutput(nsPhytoTsCPR("plotmap")),
        h6("Note there is very little data in the North and North-west regions"),
        checkboxGroupInput(inputId = nsPhytoTsCPR("region"), label = "Select a region", choices = unique(sort(datCPRp$BioRegion)), selected = unique(datCPRp$BioRegion)),
        selectInput(inputId = nsPhytoTsCPR("parameter"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datCPRp$parameters), style = "simple"), selected = "PhytoAbundance_m3"),
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = nsPhytoTsCPR("scaler3"), label = strong("Change the plot scale to log10"), value = FALSE),
        downloadButton(nsPhytoTsCPR("downloadData"), "Data"),
        downloadButton(nsPhytoTsCPR("downloadPlot"), "Plot"),
        downloadButton(nsPhytoTsCPR("downloadNote"), "Notebook")
      ),
      mainPanel(
        tabsetPanel(id = "CPRpts",
                    tabPanel("Abundances",
                             h6(textOutput(nsPhytoTsCPR("PlotExp1"), container = span)),  
                             plotly::plotlyOutput(nsPhytoTsCPR("timeseries1"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Indices",
                             h6(textOutput(nsPhytoTsCPR("PlotExp2"), container = span)),  
                             plotly::plotlyOutput(nsPhytoTsCPR("timeseries2"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
        mutate(BioRegion = factor(BioRegion, levels = c("Coral Sea", "Temperate East", "South-west", "South-east"))) %>%
        dplyr::filter(BioRegion %in% input$region,
                      parameters %in% input$parameter) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter,input$region)
    
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
    
    
    # Plot Trends -------------------------------------------------------------

    output$timeseries1 <- plotly::renderPlotly({
      if(input$scaler3){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      p1 <- planktonr::pr_plot_trends(selectedData(), trend = "Raw", survey = "CPR", method = "lm", pal = "matter", y_trans = Scale, output = "plotly")
      p2 <- planktonr::pr_plot_trends(selectedData(), trend = "Month", survey = "CPR", method = "loess", pal = "matter", y_trans = Scale, output = "plotly")
      p <- plotly::subplot(p1,p2, titleY = TRUE, widths = c(0.7,0.3))
    })
    
    
    # Climatologies -----------------------------------------------------------
    
    output$timeseries2 <- plotly::renderPlotly({
      if(input$scaler3){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      if (identical(input$region, "")) return(NULL)
      if (identical(input$parameters, "")) return(NULL)
      
      plots <- planktonr::pr_plot_tsclimate(selectedData(), 'CPR', 'matter', Scale)
      
    }) %>% bindCache(selectedData(), input$scaler3)
    
   
    
    
    # Downloads ---------------------------------------------------------------
    # Table of selected dataset ----
    output$table <- renderTable({
      datasetInput()
    })
    
    #Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {paste(input$parameter, ".csv", sep = "")},
      content = function(file) {
        write.table(selectedData(), file, row.names = FALSE, col.names = c("SampleDateUTC", "Month", "Region", input$parameter), sep = ",")
      }
    )
    
    # Download figure
    output$downloadPlot <- downloadHandler(
      filename = function() {paste(input$parameter, '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = plotInput(), device = "png")
      }
    )
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
