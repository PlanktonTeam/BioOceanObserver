#' Policy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PolCPR_ui <- function(id){
  nsPolCPR <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        shinydashboard::menuSubItem(text = "Find out more about the BioRegions here", href = "https://soe.environment.gov.au/theme/marine-environment/topic/2016/marine-regions"),
        shinydashboard::menuSubItem(text = "Find out more about EOVs here", href = "https://www.goosocean.org/index.php?option=com_content&view=article&layout=edit&id=283&Itemid=441"),
        plotOutput(nsPolCPR("plotmap")),
        h6("Note there is very little data in the North and North-west regions"),
        radioButtons(inputId = nsPolCPR("Site"), label = "Select a bioregion", choices = unique(sort(PolCPR$BioRegion)), selected = "Temperate East"),
      ),
      mainPanel(id = "EOV Biomass by CPR", 
                h6(textOutput(nsPolCPR("PlotExp1"), container = span)),
                h6(verbatimTextOutput(nsPolCPR("PlotExp5"))),
                plotOutput(nsPolCPR("timeseries1"), height = 1000) %>% shinycssloaders::withSpinner(color="#0dc5c1"),
                h6(verbatimTextOutput(nsPolCPR("PlotExp3")),
                   div(style="display:inline-block; float:right; width:60%",
                       fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                       fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
                       fButtons(id, button_id = "downloadCode1", label = "Code", Type = "Action")))
      )
    )
  )
}

#' Policy Server Functions
#'
#' @noRd 
mod_PolCPR_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
      req(input$Site)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      
      selectedData <- PolCPR %>% 
        dplyr::filter(.data$BioRegion %in% input$Site) 
    }) %>% bindCache(input$Site)
    
    shiny::exportTestValues(
      Polcpr = {ncol(selectedData())},
      PolcprRows = {nrow(selectedData()) > 0},
      PolcprYearisNumeric = {class(selectedData()$Year_Local)},
      PolcprMonthisNumeric = {class(selectedData()$Month_Local)},
      PolcprMeansisNumeric = {class(selectedData()$means)},
      PolcprsdisNumeric = {class(selectedData()$sd)},
      PolcprAnomalyisNumeric = {class(selectedData()$anomaly)},
      PolcprDateisDate = {class(selectedData()$SampleTime_Local)},
      PolcprRegionisChr = {class(selectedData()$BioRegion)},
      PolcprParametersisChr = {class(selectedData()$Parameters)},
      PolcprValuesisNumeric = {class(selectedData()$Values)}
    )
    
    outputs <- reactive({
      outputs <- planktonr::pr_get_Coeffs(selectedData())
    }) %>% bindCache(input$Site)
    
    info <- reactive({
      info <- outputs() %>% 
        dplyr::select(.data$slope, .data$p, .data$Parameters) %>% 
        unique %>%
        dplyr::arrange(.data$Parameters)
    }) %>% bindCache(input$Site)
    
    stationData <- reactive({
      stationData <- CPRinfo %>% 
        dplyr::filter(.data$BioRegion == input$Site) 
    }) %>% bindCache(input$Site)
    
    # Sidebar Map
    output$plotmap <- renderPlot({ 
      planktonr::pr_plot_CPRmap(selectedData())
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
            "Phytoplankton carbon biomass at", input$Site, "is", info()[2,1], info()[2,2], "\n",
            "Copepod diversity at", input$Site, "is", info()[3,1], info()[3,2],  "\n",
            "Phytoplankton diveristy at", input$Site, "is", info()[4,1], info()[4,2])
    }) 
    
    # Plot Trends -------------------------------------------------------------
    layout1 <- c(
      patchwork::area(1,1,1,3),  # Text
      patchwork::area(2,1,2,1),  # Header
      patchwork::area(3,1,3,3),
      patchwork::area(4,1,4,3),
      patchwork::area(5,1,5,1),  # Header
      patchwork::area(6,1,6,3),
      patchwork::area(7,1,7,3)
    )
    
    gg_out1 <- reactive({
      
      p1 <- planktonr::pr_plot_EOV(outputs(), EOV = "BiomassIndex_mgm3", Survey = 'CPR', 
                                   trans = "log10", col = "cornflowerblue", labels = "no")
      p2 <- planktonr::pr_plot_EOV(outputs(), EOV = "PhytoBiomassCarbon_pgm3", Survey = 'CPR', 
                                   trans = "log10", col = "darkolivegreen4") 
      
      p6 <- planktonr::pr_plot_EOV(outputs(), EOV = "ShannonCopepodDiversity", Survey = 'CPR', 
                                   trans = "log10", col = "cornflowerblue", labels = "no") #check these col names with new indices data from AODN
      p7 <- planktonr::pr_plot_EOV(outputs(), EOV = "ShannonPhytoDiversity", Survey = 'CPR', 
                                   trans = "log10", col = "darkolivegreen4")
      
      BioRegionSummary <- strwrap(
        paste("The CPR has been sampling in the ", input$Site," bioregion since ", min(stationData()$SampleStartDate), 
              " and sampling is ongoing.", " Approximately ", format(sum(stationData()$Miles), big.mark=",", scientific=FALSE), 
              " nautical miles has been towed in this region. The ", input$Site, " bioregion is characterised by ", 
              unique(stationData()$Features), sep = ""),
        width = 80, simplify = FALSE)
      BioRegionSummary2 <- sapply(BioRegionSummary, paste, collapse = "\n")
      
      
      patchwork::wrap_elements(
        grid::textGrob(BioRegionSummary2, gp = grid::gpar(fontsize=16))) +
        grid::textGrob("Biomass EOVs", gp = grid::gpar(fontsize=20)) + 
        p1 + p2 + 
        grid::textGrob("Diversity EOVs", gp = grid::gpar(fontsize=20)) + 
        p6 + p7 + 
        patchwork::plot_layout(design = layout1) +
        patchwork::plot_annotation(title = input$Site) & 
        ggplot2::theme(title = element_text(size = 20, face = "bold"),
                       plot.title = element_text(hjust = 0.5))
      
    }) %>% bindCache(input$Site)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    })
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, outputs(), "Policy") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Policy") # Download figure
    
  })}
