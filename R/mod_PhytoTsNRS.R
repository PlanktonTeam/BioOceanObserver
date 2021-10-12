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
        plotlyOutput(nsPhytoTsNRS("plotmap")),
        checkboxGroupInput(inputId = nsPhytoTsNRS("Site"), label = "Select a station", choices = unique(sort(datNRSp$StationName)), selected = c("Maria Island", "Port Hacking", "Yongala")),
        selectInput(inputId = nsPhytoTsNRS("ycol"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datNRSp$parameters), style = "simple"), selected = "PhytoBiomassCarbon_pgL"),
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = nsPhytoTsNRS("scaler"), label = strong("Change the plot scale to log10"), value = FALSE),
        downloadButton(nsPhytoTsNRS("downloadData"), "Data"),
        downloadButton(nsPhytoTsNRS("downloadPlot"), "Plot"),
        downloadButton(nsPhytoTsNRS("downloadNote"), "Notebook")
      ),
      mainPanel(
        tabsetPanel(id = "NRSpts",
                    tabPanel("Abundances",
                             h6(textOutput(nsPhytoTsNRS("PlotExp1"), container = span)),
                             plotly::plotlyOutput(nsPhytoTsNRS("timeseries1"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Indices",
                             h6(textOutput(nsPhytoTsNRS("PlotExp2"), container = span)),  
                             plotly::plotlyOutput(nsPhytoTsNRS("timeseries2"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
                      .data$parameters %in% input$ycol) %>%
        droplevels()
    }) %>% bindCache(input$ycol,input$Site)
    
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
      
      p1 <- planktonr::pr_plot_trends(selectedData(), trend = "Raw", survey = "NRS", method = "lm", pal = "matter", y_trans = Scale, output = "plotly")
      p2 <- planktonr::pr_plot_trends(selectedData(), trend = "Month", survey = "NRS", method = "loess", pal = "matter", y_trans = Scale, output = "plotly")
      p <- plotly::subplot(p1,p2, titleY = TRUE, widths = c(0.7,0.3))
      
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
      
      plots <- planktonr::pr_plot_tsclimate(selectedData(), 'NRS', 'matter', Scale)
      
    }) %>% bindCache(selectedData(), input$scaler)
    
    
  
    
    # Table of selected dataset ----
    output$table <- renderTable({
      datasetInput()
    })
    
    #Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {paste(input$ycol, ".csv", sep = "")},
      # colnames(selectedData)[colnames(selectedData)=="ycol"] <- paste(input$ycol),
      content = function(file) {
        write.table(selectedData(), file, row.names = FALSE, col.names = c("SampleDateLocal", "Month", "Code", input$ycol), sep = ",")
      }
    )
    
    # Download figure
    output$downloadPlot <- downloadHandler(
      filename = function() {paste(input$ycol, '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = plotInput(), device = "png")
      }
    )
  })
}
