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
        selectInput(inputId = nsPhytoTsCPR("parameter"), label = 'Select a parameter', choices = unique(datCPRp$parameters), selected = "PhytoAbundance_m3"),
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = nsPhytoTsCPR("scaler3"), label = strong("Change the plot scale to log10"), value = FALSE),
        downloadButton(nsPhytoTsCPR("downloadData"), "Data"),
        downloadButton(nsPhytoTsCPR("downloadPlot"), "Plot"),
        downloadButton(nsPhytoTsCPR("downloadNote"), "Notebook")
      ),
      mainPanel(
        tabsetPanel(id = "CPRpts",
                    tabPanel("Abundances",
                             h6(textOutput(nsPhytoTsCPR("PlotExp3"), container = span)),  
                             plotly::plotlyOutput(nsPhytoTsCPR("timeseries3"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Indices",
                             h6(textOutput(nsPhytoTsCPR("PlotExp4"), container = span)),  
                             plotly::plotlyOutput(nsPhytoTsCPR("timeseries4"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
    
    # For abundances tab  
    selectedAbundData <- reactive({
      req(input$parameter)
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      selectedAbundData <- datCPRp %>%
        mutate(BioRegion = factor(BioRegion, levels = c("Coral Sea", "Temperate East", "South-west", "South-east"))) %>%
        dplyr::filter(parameters %in% input$parameter,
                      BioRegion %in% input$region) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter,input$region)
    
    # Plot abundances by ts and monthly
    
    output$timeseries3 <- plotly::renderPlotly({
      p1 <- ggplot(selectedAbundData(), aes(x = SampleDateUTC, y = log10(Values+1))) + # do this logging as in pr_plot_tsclimate
        geom_smooth(method = 'lm', formula = y ~ x) +
        geom_point() +
        facet_grid(BioRegion~., scales = 'free') +
        scale_x_datetime() +
        labs(y = input$parameter, x = "Time") +
        theme(legend.position = "bottom",
              strip.background = element_blank(),
              strip.text = element_blank())
      p1 <- ggplotly(p1) %>% layout(showlegend = FALSE)
      
      dat_mth <- selectedAbundData() %>% filter(Month != 'NA') %>% # need to drop NA from month, added to dataset by complete(Year, Code)
        group_by(Month, BioRegion) %>%
        summarise(mean = mean(log10(Values+1), na.rm = TRUE),
                  N = length(log10(Values+1)),
                  sd = sd(log10(Values+1), na.rm = TRUE),
                  se = sd / sqrt(N),
                  .groups = "drop")
      
      # Error bars represent standard error of the mean
      p2 <- ggplot(data = dat_mth, aes(x = Month, y = mean)) +
        geom_point() +
        geom_smooth(method = 'loess', formula = y ~ x) +
        facet_grid(BioRegion~., scales = 'free') +
        labs(y = "", x = "Month") +
        scale_x_continuous(breaks= seq(1,12,length.out = 12), labels=c("J", "F", "M", "A", "M", "J","J","A","S","O","N","D")) + 
        theme(legend.position = "none",
              strip.background = element_blank())
      p2 <- ggplotly(p2) %>% layout(showlegend = FALSE) 
      
      subplot(p1, p2, widths = c(0.75,0.25)) 
    })
    
    
    # For indices tab
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
    
    # Plot timeseries by BioRegion
    output$timeseries4 <- plotly::renderPlotly({
      if(input$scaler3){
        Scale <- 'log10'
      } else
      {
        Scale <- 'identity'
      }
      if (identical(input$region, "")) return(NULL)
      if (identical(input$parameters, "")) return(NULL)
      
      plots <- planktonr::pr_plot_tsclimate(selectedData(), 'CPR', 'matter', Scale)
      
    }) %>% bindCache(selectedData(), input$scaler3)
    
    output$plotmap <- renderPlotly({ # renderCachedPlot plot so cached version can be returned if it exists (code only run once per scenario per session)
      
      plotmap <- planktonr::pr_plot_CPRmap(selectedData())
      
    }) %>% bindCache(selectedData())
    
    # add text information 
    output$PlotExp3 <- renderText({
      "A plot of selected Phytoplantkon parameters from the CPR around Australia, as a time series and a monthly climatology across bioregions. "
    }) 
    output$PlotExp4 <- renderText({
      "A plot of selected Phytoplantkon parameters from the CPR around Australia, as a time series, a monthly climatology and an annual mean for each bioregion"
    }) 
    
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
