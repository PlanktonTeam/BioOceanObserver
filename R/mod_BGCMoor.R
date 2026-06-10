#' MoorBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MoorBGC_ui <- function(id){
  tagList(
    sidebarLayout(
      fEnviroSidebar(id = id),
      fEnviroPanel(id = id)
    )
  )
}

#' MoorBGC Server Functions
#'
#' @noRd 
mod_MoorBGC_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    selectedClim <- reactive({
      req(input$site)
      shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
      
      selectedClim <- pr_get_MoorClimPlotData(pkg.env$MooringClim, input$site, 5) %>% droplevels()
      
    }) %>% bindCache(input$site)
    
    selectedTS <- reactive({
      req(input$site)
      shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
      
      selectedTS <- pr_get_MoorTSPlotData(pkg.env$MooringTS, input$site, 5) %>% droplevels()
      
    }) %>% bindCache(input$site)
    
    # Sidebar Map - Initial render with current selection
    output$plotmap <- mapgl::renderMapboxgl({
      stationCodes <- if (length(input$site) > 0) {
        pkg.env$NRSStation %>%
          dplyr::filter(.data$StationName %in% input$site) %>%
          dplyr::pull(.data$StationCode)
      } else {
        character(0)
      }
      fMapboxMap(stationCodes, Survey = "NRS", Type = "Zooplankton")
    })

    outputOptions(output, "plotmap", suspendWhenHidden = FALSE)

    # Update map when station selection changes
    observe({
      stationCodes <- if (length(input$site) > 0) {
        pkg.env$NRSStation %>%
          dplyr::filter(.data$StationName %in% input$site) %>%
          dplyr::pull(.data$StationCode)
      } else {
        character(0)
      }
      fMapboxUpdate("plotmap", session, stationCodes,
                    Survey = "NRS", Type = "Zooplankton")
    }) %>% shiny::bindEvent(input$site, ignoreNULL = FALSE)
    
    # add climate plot
    gg_out1 <- reactive({ 
      
      p1 <- pr_plot_MoorClim(selectedClim())
      p2 <- pr_plot_MoorTS(selectedTS())
      
      p1 + p2
      
    }) %>% bindCache(input$site)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {
      if(length(unique(selectedTS()$StationName)) < 2) 
      {300} else 
      {length(unique(selectedTS()$StationName)) * 200}})
    
    
    # add text information 
    output$PlotExp <- renderText({
      "Plot showing 5 years of climatology from each stations and the timeseries of temperatures at the surface, the mean mixed layer depth and the bottom"
    }) 
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedTS, "MooringTS") # Download csv of data
    output$downloadData2 <- fDownloadButtonServer(input, selectedClim, "MooringClim") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Mooring") # Download figure
    
  })
}
