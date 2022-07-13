#' PhytoTsNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
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
          selectInput(inputId = nsPhytoTsNRS("ycol"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datNRSp$Parameters), style = "simple"), 
                      selected = "PhytoBiomassCarbon_pgL")
        ),
        conditionalPanel(
          condition="input.NRSpts == 2",  
          checkboxInput(inputId = nsPhytoTsNRS("scaler1"), label = strong("Change the plot scale to percent"), value = FALSE)
        ),
        absolutePanel(  
          plotOutput(nsPhytoTsNRS("plotmap")),
          checkboxGroupInput(inputId = nsPhytoTsNRS("Site"), label = "Select a station", choices = unique(sort(datNRSp$StationName)), selected = c("Maria Island", "Port Hacking", "Yongala")),
          sliderInput(nsPhytoTsNRS("DatesSlide"), "Dates:", min = as.POSIXct('2009-01-01 00:00',
                                                                             format = "%Y-%m-%d %H:%M",
                                                                             tz = "Australia/Hobart"), max = Sys.time(), 
                      value = c(as.POSIXct('2009-01-01 00:00',
                                                 format = "%Y-%m-%d %H:%M",
                                                 tz = "Australia/Hobart"), Sys.time()-1), timeFormat="%Y-%m-%d"),
          downloadButton(nsPhytoTsNRS("downloadData"), "Data"),
          downloadButton(nsPhytoTsNRS("downloadPlot"), "Plot"),
          downloadButton(nsPhytoTsNRS("downloadNote"), "Notebook")
        )
      ),
      mainPanel(
        tabsetPanel(id = "NRSpts",
                    type = "pills",
                    tabPanel("Trend Analysis", value=1,
                             h6(textOutput(nsPhytoTsNRS("PlotExp1"), container = span)),
                             plotOutput(nsPhytoTsNRS("timeseries1"), height = 'auto') %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Climatologies", value=1,
                             h6(textOutput(nsPhytoTsNRS("PlotExp2"), container = span)),  
                             plotOutput(nsPhytoTsNRS("timeseries2"), height = 800) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Functional groups", value=2,
                             h6(textOutput(nsPhytoTsNRS("PlotExp3"), container = span)),  
                             plotOutput(nsPhytoTsNRS("timeseries3"), height = 'auto') %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
                      .data$Parameters %in% input$ycol,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$ycol,input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    output$plotmap <- renderPlot({ 
      planktonr::pr_plot_NRSmap(selectedData())
    }) %>% bindCache(input$Site)
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected phytoplantkon Parameters from the NRS around Australia, as a time series and a monthly climatology by station."
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected indicies from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
    }) 
    output$PlotExp3 <- renderText({
      "A plot of functional groups from the light microscope phytoplankton counts from the NRS around Australia, as a time series and a monthly climatology"
    }) 
    
    
    # Plot Trends -------------------------------------------------------------
    
    ts1 <- reactive({
      
      if (is.null(datNRSp$StationCode)) {  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      }
      
      if(input$scaler){
        trans <- 'log10'
      } else {
        trans <- 'identity'
      }
      
      p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", Survey = "NRS", method = "lm", trans = trans)
      p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", Survey = "NRS", method = "loess", trans = trans) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      
      p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = 'collect')
      
    }) %>% bindCache(input$ycol,input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler)
    
    output$timeseries1 <- renderPlot({
      ts1()
    }, height = function() {length(unique(selectedData()$StationName)) * 200}) 
      
      
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    output$timeseries2 <- renderPlot({
      
      if (is.null(datNRSp$StationCode)) {  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      }
      # 
      if(input$scaler){
        trans <- 'log10'
      } else {
        trans <- 'identity'
      }
      
      p1 <- planktonr::pr_plot_TimeSeries(selectedData(), Survey = "NRS", trans = trans) + 
        ggplot2::theme(legend.position = 'none')
      
      p2 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Month", trans = trans) + 
        ggplot2::theme(legend.position = 'none',
                       axis.title.y = ggplot2::element_blank())
      
      p3 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Year", trans = trans) + 
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       legend.position = 'bottom')
      
      titleplot <- names(planktonr::pr_relabel(input$ycol, style = 'simple'))
      
      p1 / (p2 | p3) + patchwork::plot_layout(guides = 'collect') + patchwork::plot_annotation(
        title = titleplot)
      
    }) %>% bindCache(input$ycol,input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler)
    
    # Functional groups -------------------------------------------------------
    
    selectedDataFG <- reactive({
      req(input$Site)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      
      selectedDataFG <- NRSfgp %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
     ts3 <- reactive({
       
       if (is.null(NRSfgp$StationCode)) {
        return(NULL)
      }
      
      if(input$scaler1){
        scale <- 'Percent'
      } else {
        scale <- 'Actual'
      }
      
      p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
      p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month") + 
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       legend.position = 'none')
      
      p1 + p2 + patchwork::plot_layout(widths = c(3,1))

    }) %>% bindCache(input$Site, input$scaler1, input$DatesSlide[1], input$DatesSlide[2])
    
     output$timeseries3 <- renderPlot({
       ts3()
     }, height = function() {length(unique(selectedDataFG()$StationName)) * 200}) 
     
    
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
