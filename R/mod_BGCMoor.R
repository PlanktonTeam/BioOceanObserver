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
      req(input$station)
      shiny::validate(need(!is.na(input$station), "Error: Please select a station."))
      
      selectedClim <- pr_get_MoorClimPlotData(pkg.env$MooringClim, input$station, 5) %>% droplevels()
      
    }) %>% bindCache(input$station)
    
    selectedTS <- reactive({
      req(input$station)
      shiny::validate(need(!is.na(input$station), "Error: Please select a station."))
      
      selectedTS <- pr_get_MoorTSPlotData(pkg.env$MooringTS, input$station, 5) %>% droplevels()
      
    }) %>% bindCache(input$station)
    
    # add a map in sidebar
    output$plotmap <- plotly::renderPlotly({ 
      p1 <- planktonr::pr_plot_NRSmap(unique(selectedClim()$StationCode))
      fPlotlyMap(p1, tooltip = "colour")
    })  # No cache - allows responsive resizing
    
    # add climate plot
    gg_out1 <- reactive({ 
      
      p1 <- pr_plot_MoorClim(selectedClim())
      p2 <- pr_plot_MoorTS(selectedTS())
      
      p1 + p2
      
    }) %>% bindCache(input$station)
    
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
