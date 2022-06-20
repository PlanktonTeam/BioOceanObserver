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
        conditionalPanel(
          condition = "input.CPRzts == 1",
          checkboxInput(inputId = nsZooTsCPR("scaler"), label = strong("Change the plot scale to log10"), value = FALSE),
          selectInput(inputId = nsZooTsCPR("parameter"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datCPRz$parameters), style = "simple"), 
                      selected = "ZoopAbundance_m3")
        ),
        conditionalPanel(
          condition = "input.CPRzts == 2",
          checkboxInput(inputId = nsZooTsCPR("scaler1"), label = strong("Change the plot scale to percent"), value = FALSE)
        ),  
        absolutePanel(
        plotOutput(nsZooTsCPR("plotmap")),
        h6("Note there is very little data in the North and North-west regions"),
        checkboxGroupInput(inputId = nsZooTsCPR("region"), label = "Select a region", choices = unique(sort(datCPRz$BioRegion)), selected = unique(datCPRz$BioRegion)),
        sliderInput(nsZooTsCPR("DatesSlide"), "Dates:", min = lubridate::ymd(20090101), max = Sys.Date(), 
                    value = c(lubridate::ymd(20090101), Sys.Date()-1), timeFormat="%Y-%m-%d"),
        downloadButton(nsZooTsCPR("downloadData"), "Data"),
        downloadButton(nsZooTsCPR("downloadPlot"), "Plot"),
        downloadButton(nsZooTsCPR("downloadNote"), "Notebook")
        )
      ),
      mainPanel(
        tabsetPanel(id = "CPRzts",
                    tabPanel("Trend Analysis", value = 1,
                             h6(textOutput(nsZooTsCPR("PlotExp1"), container = span)),  
                             plotOutput(nsZooTsCPR("timeseries1"), height = 'auto') %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Climatologies", value = 1,
                             h6(textOutput(nsZooTsCPR("PlotExp2"), container = span)),  
                             plotOutput(nsZooTsCPR("timeseries2"), height = 800) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Functional groups", value = 2,
                             h6(textOutput(nsZooTsCPR("PlotExp3"), container = span)),  
                             plotOutput(nsZooTsCPR("timeseries3"), height = 'auto') %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
                      .data$parameters %in% input$parameter,
                      dplyr::between(.data$SampleDate_UTC, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2])
    
    shiny::exportTestValues(
      ZtsCPR = {ncol(selectedData())},
      ZtsCPRRows = {nrow(selectedData()) > 0},
      ZtsCPRYearisNumeric = {class(selectedData()$Year)},
      ZtsCPRMonthisNumeric = {class(selectedData()$Month)},
      ZtsCPRDateisDate = {class(selectedData()$SampleDate_UTC)},
      ZtsCPRRegionisFactor = {class(selectedData()$BioRegion)},
      ZtsCPRparametersisChr = {class(selectedData()$parameters)},
      ZtsCPRValuesisNumeric = {class(selectedData()$Values)}
    )

    output$plotmap <- renderPlot({ # renderCachedPlot plot so cached version can be returned if it exists (code only run once per scenario per session)
      
      planktonr::pr_plot_CPRmap(selectedData()) +
        ggplot2::theme(plot.background = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank())
      
    }) %>% bindCache(input$region)
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected zooplankton parameters from the CPR around Australia, as a time series and a monthly climatology across bioregions. "
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected zooplankton parameters from the CPR around Australia, as a time series, a monthly climatology and an annual mean for each bioregion"
    }) 
    output$PlotExp3 <- renderText({
      "A plot of functional groups from the zooplankton counts from the CPR around Australia, as a time series and a monthly climatology for each bioregion"
    })     
    
    # Plot Trends -------------------------------------------------------------
    ts1 <- reactive({
      if(input$scaler){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      p1 <- planktonr::pr_plot_trends(selectedData(), trend = "Raw", survey = "CPR", method = "lm", pal = "matter", y_trans = Scale, output = "ggplot")
      p2 <- planktonr::pr_plot_trends(selectedData(), trend = "Month", survey = "CPR", method = "loess", pal = "matter", y_trans = Scale, output = "ggplot") + 
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

      p1 + p2 + patchwork::plot_layout(widths = c(3,1))
      
          }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler)
    
    output$timeseries1 <- renderPlot({
      ts1()
    }, height = function() {length(unique(selectedData()$BioRegion)) * 200}) 
    
    # Climatologies -----------------------------------------------------------
    output$timeseries2 <- renderPlot({
      if(input$scaler){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      if (identical(input$region, "")) return(NULL)
      if (identical(input$parameter, "")) return(NULL)
      
      p1 <- planktonr::pr_plot_timeseries(selectedData(), 'CPR', 'matter', Scale) + ggplot2::theme(legend.position = 'none',
                                                                                                   axis.title.y = ggplot2::element_blank())
      
      p2 <- planktonr::pr_plot_climate(selectedData(), 'CPR', Month, 'matter', Scale) + ggplot2::theme(legend.position = 'bottom',
                                                                                                       axis.title.y = ggplot2::element_blank())
      
      p3 <- planktonr::pr_plot_climate(selectedData(), 'CPR', Year, 'matter', Scale) + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                                                                                      legend.position = 'bottom')
      
      titleplot <- names(planktonr::pr_relabel(input$parameter, style = 'simple'))
      
      p1 / (p2 | p3) + patchwork::plot_layout(guides = 'collect') + patchwork::plot_annotation(
        title = titleplot)
      
    }) %>% bindCache(input$parameter,input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler)
    

    # Functional groups -------------------------------------------------------
    
    selectedDataFG <- reactive({
      req(input$region)
      validate(need(!is.na(input$region), "Error: Please select a bioregion"))
      
      selectedDataFG <- CPRfgz %>% 
        dplyr::mutate(SampleDate_UTC = as.Date(SampleDate_UTC)) %>%
        dplyr::filter(.data$BioRegion %in% input$region,
                      dplyr::between(.data$SampleDate_UTC, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
    }) %>% bindCache(input$region, input$DatesSlide[1], input$DatesSlide[2])
    
    ts3 <- reactive({
      
      if (is.null(CPRfgz$BioRegion)) {  
        return(NULL)
      }
      
      if(input$scaler1){
        scale <- 'Percent'
      } else {
        scale <- 'Actual'
      }
      
      p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
      p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, "Month")
      
      p1 + p2 + patchwork::plot_layout(widths = c(3,1))
      
    }) %>% bindCache(input$region, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
    
    output$timeseries3 <- renderPlot({
      ts3()
    }, height = function() {length(unique(selectedDataFG()$BioRegion)) * 200})     

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
