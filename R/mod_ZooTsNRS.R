#' ZooTsNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZooTsNRS_ui <- function(id){
  nsZooTsNRS <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        plotlyOutput(nsZooTsNRS("plotmap")),
        
        checkboxGroupInput(inputId = nsZooTsNRS("Site"), label = "Select a station", choices = unique(sort(datNRSz$StationName)), selected = "Maria Island"),
        selectInput(inputId = nsZooTsNRS("ycol"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datNRSz$parameters), style = "simple"), selected = "Biomass_mgm3"),
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = nsZooTsNRS("scaler1"), label = strong("Change the plot scale to log10"), value = FALSE),
        downloadButton(nsZooTsNRS("downloadData"), "Data"),
        downloadButton(nsZooTsNRS("downloadPlot"), "Plot"),
        downloadButton(nsZooTsNRS("downloadNote"), "Notebook")
      ),
      mainPanel(
        tabsetPanel(id = "NRSzts",
                    tabPanel("Abundances",
                             h6(textOutput(nsZooTsNRS("PlotExp1"), container = span))
                    ),
                    tabPanel("Indices",
                             h6(textOutput(nsZooTsNRS("PlotExp2"), container = span)),  
                             textOutput(nsZooTsNRS("selected_var")),
                             plotly::plotlyOutput(nsZooTsNRS("timeseries"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    )
        )
      )
    )
  )
}

#' ZooTsNRS Server Functions
#'
#' @noRd 
mod_ZooTsNRS_server <- function(id){
  moduleServer(id, function(input, output, session, NRSzts){
    
    selectedData <- reactive({
      req(input$Site)
      req(input$ycol)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      validate(need(!is.na(input$ycol), "Error: Please select a parameter."))
      
      selectedData <- datNRSz %>% dplyr::filter(.data$StationName %in% input$Site,
                                                .data$parameters %in% input$ycol) %>%
        droplevels()
      
    }) %>% bindCache(input$ycol,input$Site)
    
    # Plot abundance spectra by species
    output$timeseries <- plotly::renderPlotly({
      
      if (is.null(datNRSz$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      
      Scale <- 'identity'
      if(input$scaler1){
        Scale <- 'log10'
      } 
      
      plots <- planktonr::pr_plot_tsclimate(selectedData(), 'NRS', 'matter', Scale)
      
      }) %>% bindCache(selectedData(), input$scaler1)
    
    output$plotmap <- renderPlotly({ 
      
      pmap <- planktonr::pr_plot_NRSmap(selectedData())
      
    }) %>% bindCache(input$ycol, selectedData())
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected zooplantkon parameters from the NRS around Australia, as a time series and a monthly climatology by station."
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected indicies from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
    }) 
    
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
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
