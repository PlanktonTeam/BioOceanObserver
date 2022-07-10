#' Policy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom stats runif
mod_PolNRS_ui <- function(id){
  nsPolNRS <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        shinydashboard::menuSubItem(text = "Find out more about the NRS stations here", href = "https://github.com/PlanktonTeam/IMOS_BioOceanObserver/wiki/National-Reference-Stations"),
        shinydashboard::menuSubItem(text = "Find out more about EOVs here", href = "https://www.goosocean.org/index.php?option=com_content&view=article&layout=edit&id=283&Itemid=441"),
        plotOutput(nsPolNRS("plotmap")),
        radioButtons(inputId = nsPolNRS("Site"), label = "Select a station", choices = unique(sort(PolNRS$StationName)), selected = "Maria Island"),
        downloadButton(nsPolNRS("downloadData"), "Data"),
        downloadButton(nsPolNRS("downloadPlot"), "Plot"),
        downloadButton(nsPolNRS("downloadNote"), "Notebook")
          ),
      mainPanel(id = "EOV Biomass by NRS", 
                h6(textOutput(nsPolNRS("PlotExp1"), container = span)),
                h6(verbatimTextOutput(nsPolNRS("PlotExp5"))),
                plotOutput(nsPolNRS("timeseries1"), height = 1600) %>% shinycssloaders::withSpinner(color="#0dc5c1"), 
                h6(verbatimTextOutput(nsPolNRS("PlotExp3")))
             )
      )
    )
}

#' Policy Server Functions
#'
#' @noRd 
mod_PolNRS_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
      req(input$Site)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      
      selectedData <- PolNRS %>% 
        dplyr::filter(.data$StationName %in% input$Site)
    }) %>% bindCache(input$Site)
    
    shiny::exportTestValues(
      PolNRS = {ncol(selectedData())},
      PolNRSRows = {nrow(selectedData()) > 0},
      PolNRSYearisNumeric = {class(selectedData()$Year_Local)},
      PolNRSMonthisNumeric = {class(selectedData()$Month_Local)},
      PolNRSMMeansisNumeric = {class(selectedData()$means)},
      PolNRSsdisNumeric = {class(selectedData()$sd)},
      PolNRSAnomalyisNumeric = {class(selectedData()$anomaly)},
      PolNRSDateisDate = {class(selectedData()$SampleTime_Local)},
      PolNRSStationisChr = {class(selectedData()$StationName)},
      PolNRSCodeisChr = {class(selectedData()$StationCode)},
      PolNRSparametersisChr = {class(selectedData()$parameters)},
      PolNRSValuesisNumeric = {class(selectedData()$Values)}
    )
    outputs <- reactive({
      outputs <- planktonr::pr_get_coeffs(selectedData())
    }) %>% bindCache(input$Site)
    
    info <- reactive({
      info <- outputs() %>% dplyr::select(.data$slope, .data$p, .data$parameters) %>% unique()
    }) %>% bindCache(input$Site)
    
    stationData <- reactive({
      stationData <- NRSinfo %>% dplyr::filter(.data$StationName == input$Site) 
    }) %>% bindCache(input$Site)
    
    # Sidebar Map
    output$plotmap <- renderPlot({ 
      planktonr::pr_plot_NRSmap(selectedData()) 
      
      
    }) %>% bindCache(input$Site)
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "Biomass and diversity are the Essential Ocean Variables (EOVs) for plankton. 
      These are the important variables that scientists have identified to monitor our oceans.
      They are chosen based on impact of the measurement and the feasiblity to take consistent measurements.
      They are commonly measured by observing systems and frequently used in policy making and input into reporting such as State of Environment"
    }) 
    output$PlotExp3 <- renderText({
      paste(" Zooplankton biomass at", input$Site, "is", info()[1,1], info()[1,2],  "\n",
            "Phytoplankton carbon biomass at", input$Site, "is", info()[2,1], info()[2,2],  "\n",
            "Copepod diversity at", input$Site, "is", info()[4,1], info()[4,2],  "\n",
            "Phytoplankton diveristy at", input$Site, "is", info()[5,1], info()[5,2],  "\n",
            "Surface temperature at", input$Site, "is", info()[3,1], info()[3,2],  "\n",
            "Surface chlorophyll at", input$Site, "is", info()[7,1], info()[7,2],  "\n",
            "Surface salinity at", input$Site, "is", info()[6,1], info()[6,2])
    }) 
    output$PlotExp5 <- renderText({
      paste("STation Name:", input$Site, "\n",
            input$Site, " National Reference Station is located at ", round(stationData()$Latitude,2), "\u00B0S and ", round(stationData()$Longitude,2), "\u00B0E", ".", "\n",  
            "The water depth at the station is ", round(stationData()$StationDepth_m,0), "m and is currently sampled ", stationData()$SamplingEffort, ".", "\n", 
            "The station has been sampled since ", stationData()$StationStartDate, " ", stationData()$now, ".", "\n", 
            input$Site, " is part of ", stationData()$Node, " and is in the ", stationData()$ManagementRegion, " management bioregion.",  "\n", 
            "The station is characterised by ", stationData()$Features, sep = "")
    })
    
    # Plot Trends -------------------------------------------------------------
    layout1 <- c(
      patchwork::area(1,1,1,1),
      patchwork::area(2,1,2,3),
      patchwork::area(3,1,3,3),
      patchwork::area(4,1,4,1),
      patchwork::area(5,1,5,3),
      patchwork::area(6,1,6,3),
      patchwork::area(7,1,7,1),
      patchwork::area(8,1,8,3),
      patchwork::area(9,1,9,3),
      patchwork::area(10,1,10,3)
    )
    
    output$timeseries1 <- renderPlot({

      p1 <- planktonr::pr_plot_EOV(outputs(), EOV = "Biomass_mgm3", trans = "log10", col = "cornflowerblue", labels = "no") 
      p2 <- planktonr::pr_plot_EOV(outputs(), EOV = "PhytoBiomassCarbon_pgL", trans = "log10", col = "darkolivegreen4") 
      
      p6 <- planktonr::pr_plot_EOV(outputs(), EOV = "ShannonCopepodDiversity", trans = "log10", col = "cornflowerblue", labels = "no") 
      p7 <- planktonr::pr_plot_EOV(outputs(), EOV = "ShannonPhytoDiversity", trans = "log10", col = "darkolivegreen4")
      
      p3 <- planktonr::pr_plot_EOV(outputs(), EOV = "CTDTemperature_degC", trans = "identity", col = "darkviolet", labels = "no")
      p4 <- planktonr::pr_plot_EOV(outputs(), EOV = "PigmentChla_mgm3", trans = "log10", col = "darkgoldenrod", labels = "no") 
      p5 <- planktonr::pr_plot_EOV(outputs(), EOV = "CTDSalinity_PSU", trans = "identity", col = "darkred")
      
      patchwork::wrap_elements(grid::textGrob("Biomass EOVs", gp = grid::gpar(fontsize=20))) + 
        p1 + p2 + 
        grid::textGrob("Diversity EOVs", gp = grid::gpar(fontsize=20)) + 
        p6 + p7 + 
        grid::textGrob("Physcial EOVs", gp = grid::gpar(fontsize=20)) + 
        p3 + p4 + p5 + patchwork::plot_layout(design = layout1) +
        patchwork::plot_annotation(title = input$Site) & 
        ggplot2::theme(title = element_text(size = 20, face = "bold"),
                       plot.title = element_text(hjust = 0.5))
      
      }) %>% bindCache(input$Site)
    
})}
