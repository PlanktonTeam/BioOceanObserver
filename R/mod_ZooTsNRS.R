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
        
        checkboxGroupInput(inputId = nsZooTsNRS("Site"), label = "Select a station", choices = unique(sort(datNRSz$StationName)), selected = c("Maria Island", "Port Hacking", "Yongala")),
        selectInput(inputId = nsZooTsNRS("ycol"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datNRSz$parameters), style = "simple"), selected = "Biomass_mgm3"),
        
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = nsZooTsNRS("scaler1"), label = strong("Change the plot scale to log10"), value = FALSE),
        downloadButton(nsZooTsNRS("downloadData"), "Data"),
        downloadButton(nsZooTsNRS("downloadPlot"), "Plot"),
        downloadButton(nsZooTsNRS("downloadNote"), "Notebook")
      ),
      mainPanel(
        tabsetPanel(id = "NRSzts",
                    tabPanel("Trend Analysis",
                             h6(textOutput(nsZooTsNRS("PlotExp1"), container = span)),
                             # textOutput(nsZooTsNRS("selected_var")),
                             plotly::plotlyOutput(nsZooTsNRS("timeseries1"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Climatologies",
                             h6(textOutput(nsZooTsNRS("PlotExp2"), container = span)),  
                             textOutput(nsZooTsNRS("selected_var")),
                             plotly::plotlyOutput(nsZooTsNRS("timeseries2"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
      req(input$Site)
      req(input$ycol)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      validate(need(!is.na(input$ycol), "Error: Please select a parameter."))
      
      selectedData <- datNRSz %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$parameters %in% input$ycol) %>%
        droplevels()
    }) %>% bindCache(input$ycol,input$Site)
    
    # Sidebar Map
    output$plotmap <- renderPlotly({ 
      pmap <- planktonr::pr_plot_NRSmap(selectedData())
    }) %>% bindCache(input$ycol, selectedData())
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected zooplankton parameters from the NRS around Australia, as a time series and a monthly climatology by station."
    }) 
    
    output$PlotExp2 <- renderText({
      "A plot of selected indices from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
    }) 
    
    
    # Plot Trends -------------------------------------------------------------
    output$timeseries1 <- plotly::renderPlotly({
      
      if (is.null(datNRSz$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      
      if(input$scaler1){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      p1 <- planktonr::pr_plot_trends(selectedData(), trend = "Raw", survey = "NRS", method = "lm", pal = "matter", y_trans = Scale, output = "plotly")
      p2 <- planktonr::pr_plot_trends(selectedData(), trend = "Month", survey = "NRS", method = "loess", pal = "matter", y_trans = Scale, output = "plotly")
      p <- plotly::subplot(p1,p2, 
                           titleY = TRUE,
                           widths = c(0.7,0.3))
      
    }) %>% bindCache(selectedData(), input$scaler1)
    
    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    output$timeseries2 <- plotly::renderPlotly({
      
      if (is.null(datNRSz$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      
      Scale <- 'identity'
      if(input$scaler1){
        Scale <- 'log10'
      } 
      
      plots <- planktonr::pr_plot_tsclimate(selectedData(), 'NRS', 'matter', Scale)
      
    }) %>% bindCache(selectedData(), input$scaler1)
    
    
    
    
    # Downloads ---------------------------------------------------------------
    
    # Table of selected dataset ----
    output$table <- renderTable({
      # datasetInput()
    })
    
    # Download -------------------------------------------------------
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
