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
          shiny::HTML("<h5><strong>Latitudes:</strong></h5>"),
          shiny::sliderInput(nsMicroLatGS("LatSlide"), 
                             label = NULL, 
                             min = floor(min(datGSm$Latitude)), #TODO pkg.env$
                             max = floor(max(datGSm$Latitude)), #TODO pkg.env$
                             value = c(floor(min(datGSm$Latitude)), #TODO pkg.env$
                                       floor(max(datGSm$Latitude))) #TODO pkg.env$
                             ),
          shiny::HTML("<h5><strong>Max depth to plot:</strong></h5>"),
          shiny::numericInput(nsMicroLatGS("Depth"), label = NULL, value = 100, min = 10, max = max(datGSm$SampleDepth_m), step = NA),#TODO pkg.env$
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
        params <- planktonr::pr_relabel(unique(datGSm$Parameters), style = "simple") #TODO pkg.env$
      } else {
        params <- planktonr::pr_relabel(unique((datGSm %>% #TODO pkg.env$
                                                  dplyr::filter(grepl("Temperature_Index_KD|Abund|gene|ASV", .data$Parameters)))$Parameters), style = "simple")
      }
      shiny::updateSelectInput(session, 'parameterm', choices = params, selected = "Bacterial_Temperature_Index_KD")
    })
    
    selectedData <- reactive({

      selectedData <- datGSm %>% #pkg.env$datCSm %>%
        dplyr::filter(.data$Parameters %in% input$parameterm,
                      dplyr::between(.data$Latitude, input$LatSlide[1], input$LatSlide[2])) %>%
        droplevels() %>%
        dplyr::mutate(name = as.factor(.data$Parameters)) %>% 
        tidyr::drop_na()

    }) %>% bindCache(input$parameterm, input$LatSlide[1], input$LatSlide[2])

    shiny::exportTestValues(
      MicroLatGS = {ncol(selectedData())},
      MicroLatGSRows = {nrow(selectedData()) > 0},
      MicroLatGSParametersisChr = {class(selectedData()$Parameters)},
      MicroLatGSValuesisNumeric = {class(selectedData()$Values)}
    )

    # Sidebar Map
    output$plotmap <- renderPlot({
      planktonr::pr_plot_Voyagemap(datGSm, selectedData(), Country = c("Australia", "New Zealand")) 
    }, bg = "transparent") %>% bindCache(input$LatSlide[1], input$LatSlide[2])

    # Add text information
    output$voyageTitle <- renderText({
      "GO-SHIP P15S 2016."
    })
    output$PlotExp1 <- renderText({
      "A plot of selected microbial indices from GO-SHIP P15 plotted latitudinally as points and a raster."
    })

    # Plot Trends -------------------------------------------------------------

    observeEvent({input$GSmts == 1}, {

      gg_out1 <- reactive({

        if (is.null(datGSm$StationName)) #pkg.env$datCSm$StationName))  ## was reading datNRSi() as function so had to change to this, there should always be a code
          return(NULL)

        planktonr::pr_plot_latitude(selectedData(), maxDepth = input$Depth, Fill_NA = TRUE, maxGap = 3)

      }) %>% bindCache(input$parameterm, input$LatSlide[1], input$LatSlide[2], input$Depth)

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
