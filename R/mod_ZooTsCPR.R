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
        plotlyOutput(nsZooTsCPR("plotmap"), height = "200px"),
        h6("Note there is very little data in the North and North-west regions"),
        checkboxGroupInput(inputId = nsZooTsCPR("region"), label = "Select a region", choices = unique(datCPRz$BioRegion), selected = unique(datCPRz$BioRegion)),
        selectInput(inputId = nsZooTsCPR("parameter"), label = 'Select a parameter', choices = unique(datCPRz$parameters), selected = "ZoopAbundance_m3"),
        downloadButton(nsZooTsCPR("downloadData"), "Data"),
        downloadButton(nsZooTsCPR("downloadPlot"), "Plot"),
        downloadButton(nsZooTsCPR("downloadNote"), "Notebook")
      ),
      mainPanel(
        tabsetPanel(id = "CPRts",
          tabPanel("Abundances",
                   h6(textOutput(nsZooTsCPR("PlotExp1"), container = span)),  
                   plotly::plotlyOutput(nsZooTsCPR("timeseries1"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                   ),
          tabPanel("Indices",
                   h6(textOutput(nsZooTsCPR("PlotExp2"), container = span)),  
                   plotly::plotlyOutput(nsZooTsCPR("timeseries2"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
  moduleServer( id, function(input, output, session, CPRts){
    
    # For abundances tab  
    selectedAbundData <- reactive({
      req(input$parameter)
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      
      selectedAbundData <- datCPRz %>%
        mutate(BioRegion = factor(BioRegion, levels = c("Coral Sea", "Temperate East", "South-west", "South-east"))) %>%
        dplyr::filter(parameters %in% input$parameter,
                      BioRegion %in% input$region) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter,input$region)

    # Plot abundances by ts and monthly

    output$timeseries1 <- plotly::renderPlotly({
      p1 <- ggplot(selectedAbundData(), aes(x = SampleDateUTC, y = log10(Values+1))) +
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
      
      selectedData <- datCPRz %>% 
        mutate(BioRegion = factor(BioRegion, levels = c("Coral Sea", "Temperate East", "South-west", "South-east"))) %>%
        dplyr::filter(BioRegion %in% input$region,
                      parameters %in% input$parameter) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter,input$region)

    bioregionSelection <- reactive({
      bioregionSelection <- bioregion %>% dplyr::filter(REGION %in% input$region) %>% 
        mutate(REGION = factor(REGION, levels = c("Coral Sea", "Temperate East", "South-west", "South-east"))) 
    }) %>% bindCache(input$region)
    
    n <- length(unique(bioregionSelection()$REGION))
    
    # Plot timeseries by BioRegion
    output$timeseries2 <- plotly::renderPlotly({
    
      plots <- planktonr::pr_plot_tsclimate('CPR', selectedData(), 'matter')
      
    })

    output$plotmap <- renderPlotly({ # renderCachedPlot plot so cached version can be returned if it exists (code only run once per scenario per session)
     
      aust <- rnaturalearth::ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
      
      gg <- ggplot() +
        geom_sf(data = bioregion, colour = 'black', fill = 'white') + 
        geom_sf(data = bioregionSelection(), colour = 'black', aes(fill = REGION)) +
        geom_sf(data = aust, size = 0.05, fill = "grey80") +
        scale_fill_manual(values = cmocean::cmocean('matter')(n)) +
        labs(x="", y="") +
        theme_void() +
        theme(legend.position = "none",
              plot.background = element_rect(fill = "grey92"),
              panel.background = element_rect(fill = "grey92"),
              axis.line = element_blank(),
              plot.margin = unit(c(0,0,0,0),"cm"))
        }) %>% bindCache(selectedData())
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected zooplantkon parameters from the CPR around Australia, as a time series and a monthly climatology across bioregions. "
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected zooplantkon parameters from the CPR around Australia, as a time series, a monthly climatology and an annual mean for each bioregion"
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
