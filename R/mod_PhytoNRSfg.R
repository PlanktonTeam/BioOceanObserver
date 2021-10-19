#' PhytoNRSFG UI Function
#'
#' @description A shiny Module for plotting functional group time series.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PhytoNRSfg_ui <- function(id){
  nsPhytoNRSfg <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        plotlyOutput(nsPhytoNRSfg("plotmap")),
        checkboxGroupInput(inputId = nsPhytoNRSfg("Site"), label = "Select a station", choices = unique(sort(NRSfgp$StationName)), selected = c("Maria Island", "Port Hacking", "Yongala")),
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = nsPhytoNRSfg("scaler"), label = strong("Change the plot scale: Log10 or Percent"), value = FALSE),
        downloadButton(nsPhytoNRSfg("downloadData"), "Data"),
        downloadButton(nsPhytoNRSfg("downloadPlot"), "Plot"),
        downloadButton(nsPhytoNRSfg("downloadNote"), "Notebook")
      ),
      mainPanel(
        tabsetPanel(id = "NRSpfg",
                    tabPanel("Phytoplankton",
                             h6(textOutput(nsPhytoNRSfg("PlotExp1"), container = span)), 
                             #plotOutput(nsPhytoNRSfg("legend")),
                             plotly::plotlyOutput(nsPhytoNRSfg("timeseries")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Picoplankton",
                             h6(textOutput(nsPhytoNRSfg("PlotExp2"), container = span)), 
                             #plotOutput(nsPhytoNRSfg("legend2")),
                             plotly::plotlyOutput(nsPhytoNRSfg("timeseries2")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    )
        )
      )
    )
  )
}
    
#' PhytoFG Server Functions
#'
#' @noRd 
mod_PhytoNRSfg_server <- function(id){
  moduleServer( id, function(input, output, session, NRSpfg){
    ns <- session$ns
   
     selectedData <- reactive({
      req(input$Site)
      validate(need(!is.na(input$Site), "Error: Please select a station."))

      selectedData <- NRSfgp %>% 
        dplyr::filter(.data$StationName %in% input$Site) %>%
        droplevels()
    }) %>% bindCache(input$Site)
     
     selectedData1 <- reactive({
       req(input$Site)
       validate(need(!is.na(input$Site), "Error: Please select a station."))
       
       selectedData1 <- Pico %>% 
         dplyr::filter(.data$StationName %in% input$Site) %>%
         dplyr::mutate(SampleDepth_m = as.numeric(dplyr::recode(.data$SampleDepth_m, 'WC' = '0'))) %>%
         dplyr::filter(.data$SampleDepth_m < 51) %>%
         dplyr::group_by(.data$SampleDateLocal, .data$StationName, .data$Year, .data$Month, .data$parameters) %>%
         dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE)) %>%
         droplevels()
     }) %>% bindCache(input$Site)
     
    output$plotmap <- renderPlotly({ 
      pmap <- planktonr::pr_plot_NRSmap(selectedData())
    }) %>% bindCache(input$Site)
    
    # add text information 
    output$PlotExp1 <- renderText({
      "A plot of phytoplantkon functional groups from the NRS around Australia, as a time series and a monthly climatology by station."
    }) 
    output$PlotExp2 <- renderText({
      "A plot of picoplankton (mean over 50 m depth) from the NRS around Australia, as a time series and a monthly climatology by station"
    }) 
    
    output$timeseries <- plotly::renderPlotly({
      
      if (is.null(NRSfgp$StationCode)) {  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      }
      
      if(input$scaler){
        scale <- 'Percent'
      } else {
        scale <- 'Actual'
      }

      titley <- planktonr::pr_relabel("FunctionalGroup_CellsL", style = "plotly")
      np <- length(unique(selectedData()$StationName))
      p1 <- planktonr::pr_plot_tsfg(selectedData(), Scale = scale)
      p2 <- planktonr::pr_plot_tsfg(selectedData(), Scale = scale, "Month")
      p1 <- plotly::ggplotly(p1, height = 200 * np)
      p2 <- plotly::ggplotly(p2, height = 200 * np)
      s1  <- plotly::subplot((p1 %>% plotly::layout(yaxis = list(title = titley))), 
                             p2 %>% plotly::layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
                                                                 title = '',  x = 0.5, y = -0.2)),
                             titleY = TRUE, 
                             widths = c(0.7, 0.3))

    }) %>% bindCache(selectedData(), input$scaler)
    
    # output$legend <- renderPlot({
    #   
    #   p1 <- planktonr::pr_plot_tsfg(NRSfgp) + ggplot2::theme_bw(base_size = 30) +
    #     ggplot2::theme(legend.title = ggplot2::element_blank(),
    #                    legend.direction = 'horizontal')
    #   pleg <- ggpubr::get_legend(p1)
    #   plegplot <- ggpubr::as_ggplot(pleg) 
    #   plegplot
    # 
    #   }) %>% bindCache()
    
    output$timeseries2 <- plotly::renderPlotly({
      
      if (is.null(NRSfgp$StationCode)) {  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      }
      
      if(input$scaler){
        scale <- 'Percent'
      } else {
        scale <- 'Actual'
      }
      
      titley <- planktonr::pr_relabel("Picoplankton_Cellsml", style = "plotly")
      np <- length(unique(selectedData1()$StationName))
      p3 <- planktonr::pr_plot_tsfg(selectedData1(), Scale = scale)
      p4 <- planktonr::pr_plot_tsfg(selectedData1(), Scale = scale, "Month")
      
      p3 <- plotly::ggplotly(p3, height = 200 * np) 
      p4 <- plotly::ggplotly(p4, height = 200 * np)
      s2  <- plotly::subplot((p3 %>% plotly::layout(yaxis = list(title = titley))),
                             p4 %>% plotly::layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
                                                                 title = '',  x = 0.5, y = -0.2)),
                             titleY = TRUE,
                             widths = c(0.7, 0.3))

    }) %>% bindCache(selectedData1(), input$scaler)
  
    # output$legend2 <- renderPlot({
    #   
    #   p1 <- planktonr::pr_plot_tsfg(Pico) + ggplot2::theme_bw(base_size = 30) +
    #     ggplot2::theme(legend.title = ggplot2::element_blank(),
    #                    legend.direction = 'horizontal')
    #   pleg <- ggpubr::get_legend(p1)
    #   plegplot <- ggpubr::as_ggplot(pleg)
    #   plegplot
    #   
    # }) %>% bindCache()
    
      
  })
}
    
