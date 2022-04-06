#' MicroTsNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MicroTsNRS_ui <- function(id){
  nsMicroTsNRS <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # conditionalPanel(
        #   condition="input.NRSmts == 1",  
        #   # Select whether to overlay smooth trend line 
        #   checkboxInput(inputId = nsMicroTsNRS("scaler1"), label = strong("Change the plot scale to log10"), value = FALSE)
        # ),
        conditionalPanel(
          condition="input.NRSmts == 2", 
          selectizeInput(inputId = nsMicroTsNRS("smoother"), label = strong("Overlay trend line"), choices = c("Smoother", "Linear", "None"), selected = "None")
        ),
        conditionalPanel(
          condition="input.NRSmts == 2 | input.NRSmts == 1", 
          checkboxInput(inputId = nsMicroTsNRS("scaler1"), label = strong("Change the plot scale to log10"), value = FALSE),
          sliderInput(nsMicroTsNRS("DatesSlide"), "Dates:", min = lubridate::ymd_hms(20090101000000), max = Sys.time(), 
                      value = c(lubridate::ymd_hms(20090101000000),Sys.time()-1), timeFormat="%Y-%m-%d"),
          selectInput(inputId = nsMicroTsNRS("ycol"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datNRSm$parameters), style = "simple"), selected = "Bacterial_Richness"),
        ),
        conditionalPanel(
          condition="input.NRSmts == 3", 
          selectInput(inputId = nsMicroTsNRS("p1"), label = 'Select an x parameter', choices = planktonr::pr_relabel(unique(datNRSm$parameters), style = "simple"), selected = "Eukaryote_Chlorophyll_Index"),
          selectInput(inputId = nsMicroTsNRS("p2"), label = 'Select a y parameter', 
                      choices = planktonr::pr_relabel(c("Prochlorococcus_Cellsml", "Synecochoccus_Cellsml", "Picoeukaryotes_Cellsml"), 
                                                      style = "simple"), selected = "Prochlorococcus_Cellsml")
        ),
        absolutePanel(
          plotlyOutput(nsMicroTsNRS("plotmap")),
          checkboxGroupInput(inputId = nsMicroTsNRS("Site"), label = "Select a station", choices = unique(sort(datNRSm$StationName)), selected = c("Maria Island", "Port Hacking", "Yongala")),
          downloadButton(nsMicroTsNRS("downloadData"), "Data"),
          downloadButton(nsMicroTsNRS("downloadPlot"), "Plot"),
          downloadButton(nsMicroTsNRS("downloadNote"), "Notebook")
        )
      ),
      mainPanel(
        tabsetPanel(id = "NRSmts",
                    tabPanel("Trend Analysis", value=1,
                             h6(textOutput(nsMicroTsNRS("PlotExp1"), container = span)),
                             plotly::plotlyOutput(nsMicroTsNRS("timeseries1")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Climatologies", value=1,
                             h6(textOutput(nsMicroTsNRS("PlotExp2"), container = span)),
                             plotly::plotlyOutput(nsMicroTsNRS("timeseries2")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Trend analysis by depth", value=2,
                             h6(textOutput(nsMicroTsNRS("PlotExp3"), container = span)),  
                             plotly::plotlyOutput(nsMicroTsNRS("timeseries3")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Cell counts vs Indices", value=3,
                             h6(textOutput(nsMicroTsNRS("PlotExp4"), container = span)),  
                             plotly::plotlyOutput(nsMicroTsNRS("timeseries4")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    )
        )
      )
    )
  )
}


#' MicroTSNRS Server Functions
#'
#' @noRd 
mod_MicroTsNRS_server <- function(id){
  moduleServer(id, function(input, output, session, NRSmts){
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
        selectedData <- datNRSm %>% 
          dplyr::filter(.data$StationName %in% input$Site,
                        .data$parameters %in% input$ycol,
                        dplyr::between(.data$SampleDate_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
          mutate(name = as.factor(.data$parameters),
                 SampleDepth_m = dplyr::if_else(stringr::str_detect("WC", SampleDepth_m),
                                                "WC",
                                                as.character(round(as.numeric(.data$SampleDepth_m)/5,0)*5))) %>%
          droplevels()

    }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    # Sidebar Map
    output$plotmap <- renderPlotly({ 
      pmap <- planktonr::pr_plot_NRSmap(selectedData())
    }) %>% bindCache(input$ycol, selectedData())
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected microbial indices from the NRS around Australia, as a time series and a monthly climatology by station averaged across all depths."
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected indices from the NRS around Australia, as a time series, a monthly climatology and an annual mean averaged across all depths."
    }) 
    output$PlotExp3 <- renderText({
      "A plot of microbial indices from the NRS around Australia, as a time series and a monthly climatology by depth"
    }) 
    output$PlotExp4 <- renderText({
      "A plot of microbial indices against abundance measure from the NRS around Australia"
    }) 
    
    
    
    # Plot Trends -------------------------------------------------------------
    output$timeseries1 <- plotly::renderPlotly({

      if (is.null(datNRSm$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)

      if(input$scaler1){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }

      np <- length(unique(selectedData()$StationName))
      p1 <- planktonr::pr_plot_trends(selectedData(), trend = "Raw", survey = "NRS", method = "lm", pal = "matter", y_trans = Scale, output = "ggplot")
      p2 <- planktonr::pr_plot_trends(selectedData(), trend = "Month", survey = "NRS", method = "loess", pal = "matter", y_trans = Scale, output = "ggplot")
      p1 <- plotly::ggplotly(p1, height = 200 * np)
      p2 <- plotly::ggplotly(p2, height = 200 * np)
      p <- plotly::subplot(p1,p2,
                           titleY = TRUE,
                           widths = c(0.7,0.3))

    }) %>% bindCache(selectedData(), input$scaler1)

    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    output$timeseries2 <- plotly::renderPlotly({
      
      if (is.null(datNRSm$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)

      Scale <- 'identity'
      if(input$scaler1){
        Scale <- 'log10'
      }
      
      np <- length(unique(selectedData()$StationName))
      p1 <- planktonr::pr_plot_timeseries(selectedData(), 'NRS', 'matter', Scale) + 
        ggplot2::theme(legend.position = 'none',
                       axis.title.y = ggplot2::element_blank())
      
      p2 <- planktonr::pr_plot_climate(selectedData(), 'NRS', Month, 'matter', Scale) + 
        ggplot2::theme(legend.position = 'none',
                       axis.title.y = ggplot2::element_blank())
      
      p3 <- planktonr::pr_plot_climate(selectedData(), 'NRS', Year, 'matter', Scale) + 
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       legend.title = ggplot2::element_blank())
      
      titley <- planktonr::pr_relabel(unique(selectedData()$parameters), style = "plotly")
      p1 <- plotly::ggplotly(p1, height = 200 * np)
      p2 <- plotly::ggplotly(p2, height = 200 * np)
      p3 <- plotly::ggplotly(p3, height = 200 * np)
      p <- plotly::subplot(p1 %>% plotly::layout(showlegend = FALSE),
                           p2 %>% plotly::layout(yaxis = list(title = titley)),
                           p3 %>% plotly::layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
                                                               title = '',  x = 0.5, y = -0.2)),
                           nrows = 3,
                           titleY = TRUE)


    }) %>% bindCache(selectedData(), input$scaler1)

    # Plots by depths ---------------------------------------------------------
    
    output$timeseries3 <- renderPlotly({
      
      trend <-  input$smoother
      
      if(input$scaler1){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      plot <- planktonr::pr_plot_env_var(selectedData(), trend = trend, Scale = Scale)
      
    }) %>% bindCache(selectedData(), input$smoother, input$scaler1)
    
    # Plots by depths ---------------------------------------------------------
    
    selectedData1 <- reactive({
      req(input$Site)
      req(input$p1)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      validate(need(!is.na(input$p1), "Error: Please select a parameter."))
      
      selectedData1 <- datNRSm %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$parameters %in% c(input$p1, input$p2)) %>%
        tidyr::pivot_wider(c(StationName, SampleDepth_m, SampleDate_Local), names_from = parameters, values_from = Values, values_fn = mean)

      # selectedData1 <- datNRSm %>% 
      #   dplyr::filter(.data$StationName %in% 'Yongala',
      #                 .data$parameters %in% c('Bacterial_Richness', 'Prochlorococcus_Cellsml')) %>%
      #   tidyr::pivot_wider(c(StationName, SampleDepth_m, SampleDate_Local), names_from = parameters, values_from = Values, values_fn = mean)
      # 
      #       
    }) %>% bindCache(input$p1, input$p2, input$Site)
    
    output$timeseries4 <- renderPlotly({
      x <- rlang::sym(colnames(selectedData1()[, 5]))
      y <- rlang::sym(colnames(selectedData1()[, 4]))
      
      titlex <- planktonr::pr_relabel(rlang::as_string(x), style = "plotly")
      titley <- planktonr::pr_relabel(rlang::as_string(y), style = "plotly")

      plot <- ggplot2::ggplot(data = selectedData1()) +
        ggplot2::geom_point(ggplot2::aes(!!x, !!y, colour = .data$StationName)) +
        ggplot2::xlab(titlex) + ggplot2::ylab(titley) 
      plot <- plotly::ggplotly(plot) %>% plotly::layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
                                                                      title = '',  x = 0.5, y = -0.2))
      
    }) %>% bindCache(selectedData1())
    
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
