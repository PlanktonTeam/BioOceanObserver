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
  nsMicroLatGS <- NS(id)
  tagList(
    sidebarLayout(
      shiny::sidebarPanel(
          tags$head(tags$style(HTML(
            ".multicol{
          height:auto;
          -webkit-column-count: 2;
          -moz-column-count: 2;
          column-count: 2;}"))),
          shiny::plotOutput(nsMicroLatGS("plotmap"),
                            height = "300px", 
                            width = "100%"),
          shiny::HTML("<h5><strong>Latitude range to plot:</strong></h5>"),
          shiny::sliderInput(nsMicroLatGS("LatSlide"), 
                             label = NULL, 
                             min = floor(min(pkg.env$datGSm$Latitude)),
                             max = floor(max(pkg.env$datGSm$Latitude)), 
                             value = c(min(pkg.env$datGSm$Latitude),
                                       max(pkg.env$datGSm$Latitude)) 
                             ),
          shiny::HTML("<h5><strong>Depth range to plot:</strong></h5>"),
          shiny::sliderInput(nsMicroLatGS("DepthSlide"), 
                             label = NULL, 
                             min = floor(min(pkg.env$datGSm$SampleDepth_m)), 
                             max = floor(max(pkg.env$datGSm$SampleDepth_m)), 
                             value = c(min(pkg.env$datGSm$SampleDepth_m), 100)
          ),
          shiny::HTML("<h5><strong>Select a parameter:</strong></h5>"),
          shiny::selectInput(inputId = nsMicroLatGS("parameterm"), 
                             label = NULL, 
                             choices = 'Bacterial_Temperature_Index_KD', 
                             selected = 'Bacterial_Temperature_Index_KD'),
          shiny::htmlOutput(nsMicroLatGS("ParamDefm")),
          shiny::checkboxInput(inputId = nsMicroLatGS("all"), 
                               label = strong("Tick for more microbial parameters"), 
                               value = FALSE),
          shiny::br()
      ),
      shiny::mainPanel(h4(textOutput(nsMicroLatGS("voyageTitle"), container = span)),
                       h6(textOutput(nsMicroLatGS("PlotExp1"), container = span)),
                       plotOutput(nsMicroLatGS("timeseries1")) %>% 
                         shinycssloaders::withSpinner(color="#0dc5c1"),
                       div(style="display:inline-block; float:right; width:60%",
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
        params <- planktonr::pr_relabel(unique(pkg.env$datGSm$Parameters), style = "simple") 
      } else {
        params <- planktonr::pr_relabel(unique((pkg.env$datGSm %>% 
                                                  dplyr::filter(grepl("Temperature_Index_KD|Abund|gene|ASV", .data$Parameters)))$Parameters), style = "simple")
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

    # Sidebar Map
    output$plotmap <- renderPlot({
      planktonr::pr_plot_Voyagemap(pkg.env$datGSm, selectedData(), Country = c("Australia", "New Zealand")) 
    }, bg = "transparent") %>% bindCache(input$LatSlide[1], input$LatSlide[2])

    # Add text information
    output$voyageTitle <- renderText({
      "GO-SHIP P15S 2016."
    })
    output$PlotExp1 <- renderText({
      "A plot of selected microbial indices from voyage data plotted latitudinally as points and a raster."
    })

    # Plot Trends -------------------------------------------------------------

    observeEvent({input$GSmts == 1}, {

      gg_out1 <- reactive({

        if (is.null(pkg.env$datGSm$Latitude))  
          return(NULL)

        planktonr::pr_plot_latitude(selectedData(), Fill_NA = TRUE, maxGap = 3)

      }) %>% bindCache(input$parameterm, input$LatSlide[1], input$LatSlide[2], input$DepthSlide[1], input$DepthSlide[2])

      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {800}) 

      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData(), "Trend") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Trend") # Download figure

      # Parameter Definition
      output$ParamDefm <- fParamDefServer(selectedData) # Download csv of data

    })
  })
}
