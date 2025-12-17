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
        leaflet::leafletOutput(nsPolCPR("plotmap"), height = "400px"),
        shiny::p("Note there is very little data in the North and North-west regions"),
        shiny::HTML("<h3>Select a bioregion:</h3>"),
        shiny::radioButtons(inputId = nsPolCPR("site"), label = NULL, 
                     choices = unique(sort(pkg.env$PolCPR$BioRegion)), selected = "Temperate East"),
      ),
      mainPanel(id = "EOV Biomass by CPR", 
                shiny::br(),
                shiny::h3("Essential Ocean Variables"),
                shiny::HTML("<a href='https://www.goosocean.org/index.php?option=com_content&view=article&layout=edit&id=283&Itemid=441'> 
                        Essential Ocean Variables (EOVs)</a> are the important variables that scientists 
                        have identified to monitor our oceans. They are chosen based on impact of the measurement and the 
                        feasiblity to take consistent measurements. They are commonly measured by observing systems and 
                        frequently used in policy making and input into reporting such as State of Environment."),
                shiny::hr(class = "hr-separator"),
                shiny::br(),
                shiny::htmlOutput(nsPolCPR("StationSummary")),
                shiny::br(),
                plotOutput(nsPolCPR("timeseries1"), height = 1500) %>% 
                  shinycssloaders::withSpinner(color="#0dc5c1"),
                    div(class="download-button-container",
                       fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                       fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
                       fButtons(id, button_id = "downloadCode1", label = "Code", Type = "Action")))
      )
    )
  # )
}

#' Policy Server Functions
#'
#' @noRd 
mod_PolCPR_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
      req(input$site)
      shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
      
      selectedData <- pkg.env$PolCPR %>% 
        dplyr::filter(.data$BioRegion %in% input$site)
      
      }) %>% bindCache(input$site)
    
    selectedPCI <- reactive({
      req(input$site)
      shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
      
      selectedPCI <- pkg.env$PCI %>% 
      dplyr::filter(.data$BioRegion %in% input$site) 
      
    }) %>% bindCache(input$site)
    
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
    

    stationData <- reactive({
      stationData <- pkg.env$CPRinfo %>% 
        dplyr::filter(.data$BioRegion == input$site) 
    }) %>% bindCache(input$site)
    
    # Sidebar Map: use leaflet for CPR polygons
    output$plotmap <- leaflet::renderLeaflet({
      fLeafletMap(character(0), Survey = "CPR", Type = "Policy")
    })

    observe({
      fLeafletUpdate("plotmap", session, unique(selectedData()$BioRegion), Survey = "CPR", Type = "Policy")
    })
    
    
    output$StationSummary <- shiny::renderText({ 
      paste('<h3 class="centered-heading">',input$site,'</h3>The CPR has been sampling 
              in the ', input$site,' bioregion since ', format(min(stationData()$SampleStartDate), "%A %d %B %Y"), 
            ' and sampling is ongoing.', ' Approximately ', format(sum(stationData()$Miles), big.mark=",", scientific=FALSE), 
            ' nautical miles has been towed in this region. The ', input$site, ' bioregion is characterised by ', 
            unique(stationData()$Features), sep = "")
    })
    
    col1 <- fEOVutilities(vector = "col", Survey = "CPR")
    
    # Plot Trends -------------------------------------------------------------
    #t, l, b, r
    layout1 <- c(
      patchwork::area(1,1,1,3),  # Header
      patchwork::area(2,1,3,3),
      patchwork::area(4,1,5,3),
      patchwork::area(6,1,6,3),  # Header
      patchwork::area(7,1,8,3),
      patchwork::area(9,1,10,3),
      patchwork::area(11,1,11,3),  # Header
      patchwork::area(12,1,13,3),
      patchwork::area(14,1,15,3),
      patchwork::area(16,1,16,3), # Header
      patchwork::area(17,1,25,3)
    )
    
    gg_out1 <- reactive({
      
      p1 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "PhytoBiomassCarbon_pgm3", trans = "log10", col = col1["PhytoBiomassCarbon_pgm3"], labels = FALSE) 
      p2 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "BiomassIndex_mgm3", trans = "log10", col = col1["BiomassIndex_mgm3"])
      
      p3 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "ShannonPhytoDiversity", trans = "log10", col = col1["ShannonPhytoDiversity"], labels = FALSE)
      p4 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "ShannonCopepodDiversity", trans = "log10", col = col1["ShannonCopepodDiversity"])
      
      p5 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "SST", trans = "identity", col = col1["SST"], labels = FALSE)
      p6 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "chl_oc3", trans = "identity", col = col1["chl_oc3"])
      
      p7 <- planktonr::pr_plot_PCImap(selectedPCI())
      
      patchwork::wrap_elements(grid::textGrob("Biomass EOVs", gp = grid::gpar(fontsize=20))) + 
        p1 + p2 + 
        grid::textGrob("Diversity EOVs", gp = grid::gpar(fontsize=20)) + 
        p3 + p4 + 
        grid::textGrob("Physical EOVs from satellite data", gp = grid::gpar(fontsize=20)) + 
        p5 + p6 + 
        grid::textGrob("Chlorophyll density from Phytoplankton Colour Index", gp = grid::gpar(fontsize=20)) + 
        p7 +
        patchwork::plot_layout(design = layout1) & 
        ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                       axis.title = ggplot2::element_text(size = 12, face = "plain"),
                       axis.text =  ggplot2::element_text(size = 10, face = "plain"),
                       plot.title = ggplot2::element_text(hjust = 0.5))
      
    }) %>% bindCache(input$site)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    })
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Policy") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Policy", papersize = "A2") # Download figure
    
  })}
