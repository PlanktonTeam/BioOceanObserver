#' MicroLatGS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MicroLatGS_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      shiny::sidebarPanel(
        leaflet::leafletOutput(ns("plotmap"), height = "400px"),
          shiny::HTML("<h3>Latitude range to plot:</h3>"),
          shiny::sliderInput(ns("LatSlide"), 
                             label = NULL, 
                             min = floor(min(pkg.env$datGSm$Latitude)),
                             max = ceiling(max(pkg.env$datGSm$Latitude)), 
                             value = c(min(pkg.env$datGSm$Latitude),
                                       max(pkg.env$datGSm$Latitude)) 
                             ),
          shiny::HTML("<h3>Depth range to plot:</h3>"),
          shiny::sliderInput(ns("DepthSlide"), 
                             label = NULL, 
                             min = floor(min(pkg.env$datGSm$SampleDepth_m)), 
                             max = floor(max(pkg.env$datGSm$SampleDepth_m)), 
                             value = c(min(pkg.env$datGSm$SampleDepth_m), 100)
          ),
          shiny::HTML("<h3>Select a parameter:</h3>"),
          shiny::selectInput(inputId = ns("parameterm"), 
                             label = NULL, 
                             choices = 'Bacterial_Temperature_Index_KD', 
                             selected = 'Bacterial_Temperature_Index_KD'),
          shiny::htmlOutput(ns("ParamDefm")),
          shiny::checkboxInput(inputId = ns("all"), 
                               label = "Tick for more microbial parameters", 
                               value = FALSE),
      ),
      shiny::mainPanel(h4(textOutput(ns("voyageTitle"), container = span)),
                       shiny::htmlOutput(ns("PlotExp1")),
                       plotOutput(ns("timeseries1")) %>% 
                         shinycssloaders::withSpinner(color="#0dc5c1"),
                       div(class="download-button-container",
                           fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                           fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"))
                           )
    )
  )
}


#' MicroLatGS Server Functions
#'
#' @noRd
mod_MicroLatGS_server <- function(id){
  moduleServer(id, function(input, output, session, GSlat){

    # Sidebar ----------------------------------------------------------
    observeEvent(input$all, {
      if(input$all == TRUE){
        params <- planktonr:::pr_relabel(unique(pkg.env$datGSm$Parameters), style = "simple", named = TRUE) 
      } else {
        params <- planktonr:::pr_relabel(unique((pkg.env$datGSm %>% 
                                                  dplyr::filter(grepl("Temperature_Index_KD|Abund|gene|ASV", .data$Parameters)))$Parameters), style = "simple", named = TRUE)
      }
      shiny::updateSelectInput(session, 'parameterm', choices = params, selected = "Bacterial_Temperature_Index_KD")
    })
    
    selectedData <- reactive({

      selectedData <- pkg.env$datGSm %>%
        dplyr::filter(.data$Parameters %in% input$parameterm,
                      dplyr::between(.data$Latitude, input$LatSlide[1], input$LatSlide[2]),
                      dplyr::between(.data$SampleDepth_m, input$DepthSlide[1], input$DepthSlide[2])) %>%
        droplevels() %>%
        dplyr::mutate(name = as.factor(.data$Parameters)) %>% 
        tidyr::drop_na()

    }) %>% bindCache(input$parameterm, input$LatSlide[1], input$LatSlide[2], input$DepthSlide[1], input$DepthSlide[2])

    shiny::exportTestValues(
      MicroLatGS = {ncol(selectedData())},
      MicroLatGSRows = {nrow(selectedData()) > 0},
      MicroLatGSParametersisChr = {class(selectedData()$Parameters)},
      MicroLatGSValuesisNumeric = {class(selectedData()$Values)}
    )

    # Sidebar Map - Initial render
    output$plotmap <- leaflet::renderLeaflet({
      fLeafletMap(sites = character(0), Survey = "GO-SHIP", Type = "Microbes")
    })
    
    # Update map when station selection changes
    observe({
      fLeafletUpdate("plotmap", session, sites = input$LatSlide, 
                     Survey = "GO-SHIP", Type = "Microbes")
    })
    
    
    # Add text information
    output$voyageTitle <- renderText({
      "GO-SHIP P15S 2016."
    })
    output$PlotExp1 <- renderText({
      if(length(selectedData()$Parameters)>50){
        "A plot of selected microbial indices from voyage data plotted latitudinally as points and a raster."
      } else {
        paste("A plot of selected microbial indices from voyage data plotted latitudinally as points and a raster. 
              <br> <br> <b>NOTE: Not enough data for plot</b>")
        }
    }) %>% bindCache(input$parameterm)


    # Plot Trends -------------------------------------------------------------

    gg_out1 <- reactive({

      if(length(selectedData()$Parameters)>50){
        planktonr::pr_plot_latitude(selectedData(), na.fill = mean)
      } else {
        ggplot2::ggplot + ggplot2::geom_blank()
      }
    }) %>% bindCache(input$parameterm, input$LatSlide[1], input$LatSlide[2], input$DepthSlide[1], input$DepthSlide[2])

    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {800}) 

    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Trend") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Trend") # Download figure

    # Parameter Definition
    output$ParamDefm <- fParamDefServer(selectedData) # Download csv of data

  })
}
