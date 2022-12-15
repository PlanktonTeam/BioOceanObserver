#' Policy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PolNRS_ui <- function(id){
  nsPolNRS <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        plotOutput(nsPolNRS("plotmap")),
        radioButtons(inputId = nsPolNRS("Site"), label = "Select a station", choices = unique(sort(PolNRS$StationName)), selected = "Maria Island")
      ),
      mainPanel(id = "EOV Biomass by NRS", 
                h6(htmlOutput(nsPolNRS("PlotExp1"), container = span)),
                # h6(verbatimTextOutput(nsPolNRS("PlotExp5"))),
                plotOutput(nsPolNRS("timeseries1"), height = 1800) %>% 
                  shinycssloaders::withSpinner(color="#0dc5c1"), 
                # h6(verbatimTextOutput(nsPolNRS("PlotExp3")),
                   div(style="display:inline-block; float:right; width:60%",
                       fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                       fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
                       fButtons(id, button_id = "downloadCode1", label = "Code", Type = "Action")))
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
      PolNRSParametersisChr = {class(selectedData()$Parameters)},
      PolNRSValuesisNumeric = {class(selectedData()$Values)}
    )
    outputs <- reactive({
      outputs <- planktonr::pr_get_Coeffs(selectedData())
    }) %>% bindCache(input$Site)
    
    info <- reactive({
      info <- outputs() %>% dplyr::select(.data$slope, .data$p, .data$Parameters) %>% unique()
    }) %>% bindCache(input$Site)
    
    stationData <- reactive({
      stationData <- NRSinfo %>% 
        dplyr::filter(.data$StationName == input$Site) 
    }) %>% bindCache(input$Site)
    
    # Sidebar Map
    output$plotmap <- renderPlot({ 
      planktonr::pr_plot_NRSmap(selectedData()) 
    }, bg = "transparent") %>% bindCache(input$Site)
    
    # Add text information 
    output$PlotExp1 <- renderUI({
      shiny::HTML("Biomass and diversity are the <a href = 'https://www.goosocean.org/index.php?option=com_content&view=article&layout=edit&id=283&Itemid=441'>Essential Ocean Variables (EOVs)</a> for plankton. 
      These are the important variables that scientists have identified to monitor our oceans.
      They are chosen based on impact of the measurement and the feasiblity to take consistent measurements.
      They are commonly measured by observing systems and frequently used in policy making and input into reporting such as State of Environment.")
    }) 
    # output$PlotExp3 <- renderText({
    #   paste("Zooplankton biomass at", input$Site, "is", info()[1,1], info()[1,2],  "\n",
    #         "Phytoplankton carbon biomass at", input$Site, "is", info()[2,1], info()[2,2],  "\n",
    #         "Copepod diversity at", input$Site, "is", info()[4,1], info()[4,2],  "\n",
    #         "Phytoplankton diveristy at", input$Site, "is", info()[5,1], info()[5,2],  "\n",
    #         "Surface temperature at", input$Site, "is", info()[3,1], info()[3,2],  "\n",
    #         "Surface chlorophyll at", input$Site, "is", info()[7,1], info()[7,2],  "\n",
    #         "Surface salinity at", input$Site, "is", info()[6,1], info()[6,2])
    # }) 
    
    # Plot Trends -------------------------------------------------------------
    layout1 <- c(
      #t, l, b, r
      patchwork::area(1,1,3,3),
      patchwork::area(4,1,4,3), # Biomass Header
      patchwork::area(5,1,6,3),
      patchwork::area(7,1,8,3),
      patchwork::area(9,1,9,3), # Diversity Header
      patchwork::area(10,1,11,3),
      patchwork::area(12,1,13,3),
      patchwork::area(14,1,14,3), # Physical Header 
      patchwork::area(15,1,16,3),
      patchwork::area(17,1,18,3),
      patchwork::area(19,1,20,3)
    )
    
    layout2 <- c(
      #t, l, b, r
      patchwork::area(1,1,3,3),
      patchwork::area(4,1,4,3), # Biomass Header
      patchwork::area(5,1,6,3),
      patchwork::area(7,1,8,3),
      patchwork::area(9,1,9,3), # Diversity Header
      patchwork::area(10,1,11,3),
      patchwork::area(12,1,13,3),
      patchwork::area(14,1,14,3), # Physical Header 
      patchwork::area(15,1,16,3),
      patchwork::area(17,1,18,3),
      patchwork::area(19,1,20,3),
      patchwork::area(21,1,21,3),  # Biochemistry Header 
      patchwork::area(22,1,23,3), # O2
      patchwork::area(24,1,25,3), # NH4
      patchwork::area(26,1,27,3), # NO2
      patchwork::area(28,1,29,3), # NO3
      patchwork::area(30,1,31,3)  # 02
    )
    
    gg_out1 <- reactive({
      
      p1 <- planktonr::pr_plot_EOV(outputs(), EOV = "Biomass_mgm3", trans = "log10", col = col12[2], labels = "no") 
      p2 <- planktonr::pr_plot_EOV(outputs(), EOV = "PhytoBiomassCarbon_pgL", trans = "log10", col = col12[4]) 
      
      p3 <- planktonr::pr_plot_EOV(outputs(), EOV = "ShannonCopepodDiversity", trans = "log10", col = col12[1], labels = "no") 
      p4 <- planktonr::pr_plot_EOV(outputs(), EOV = "ShannonPhytoDiversity", trans = "log10", col = col12[3])
      
      p5 <- planktonr::pr_plot_EOV(outputs(), EOV = "CTDTemperature_degC", trans = "identity", col = col12[5], labels = "no")
      p6 <- planktonr::pr_plot_EOV(outputs(), EOV = "PigmentChla_mgm3", trans = "log10", col = col12[6], labels = "no") 
      p7 <- planktonr::pr_plot_EOV(outputs(), EOV = "CTDSalinity_PSU", trans = "identity", col = col12[7])
      
      p8 <- planktonr::pr_plot_EOV(outputs(), EOV = "Ammonium_umolL", trans = "identity", col = col12[8], labels = "no")
      p9 <- planktonr::pr_plot_EOV(outputs(), EOV = "Nitrate_umolL", trans = "identity", col = col12[9], labels = "no")
      p10 <- planktonr::pr_plot_EOV(outputs(), EOV = "Nitrite_umolL", trans = "identity", col = col12[10], labels = "no")
      p11 <- planktonr::pr_plot_EOV(outputs(), EOV = "Phosphate_umolL", trans = "log10", col = col12[11], labels = "no") 
      p12 <- planktonr::pr_plot_EOV(outputs(), EOV = "Oxygen_umolL", trans = "identity", col = col12[12])
      
      
      StationSummary <- strwrap(
        paste("The ", input$Site, " National Reference Station is located at ", round(stationData()$Latitude,2), 
              "\u00B0S and ", round(stationData()$Longitude,2), "\u00B0E", ". The water depth at the station is ", 
              round(stationData()$StationDepth_m,0), "m and is currently sampled ", stationData()$SamplingEffort, 
              ". The station has been sampled since ", stationData()$StationStartDate, " ", stationData()$now,
              ". ", input$Site, " is part of ", stationData()$Node, " and is in the ", stationData()$ManagementRegion, 
              " management bioregion. The station is characterised by ", stationData()$Features, ".", sep = ""), 
        width = 100, simplify = FALSE)
      StationSummary2 <- sapply(StationSummary, paste, collapse = "\n")
      
      patchwork::wrap_elements(
        grid::textGrob(StationSummary2, gp = grid::gpar(fontsize=16))) +
        grid::textGrob("Biomass EOVs", gp = grid::gpar(fontsize=20)) +
        p1 + p2 + 
        grid::textGrob("Diversity EOVs", gp = grid::gpar(fontsize=20)) + 
        p3 + p4 + 
        grid::textGrob("Physcial EOVs", gp = grid::gpar(fontsize=20)) + 
        p5 + p6 + p7 + patchwork::plot_layout(design = layout1) +
        grid::textGrob("Biochemical EOVs", gp = grid::gpar(fontsize=20)) + 
        p8 + p9 + p10 + p11 + p12 + patchwork::plot_layout(design = layout2) +
        patchwork::plot_annotation(title = input$Site) & 
        ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                       axis.title = ggplot2::element_text(size = 12, face = "plain"),
                       axis.text =  ggplot2::element_text(size = 10, face = "plain"),
                       plot.title = ggplot2::element_text(hjust = 0.5))
      
    }) %>% bindCache(input$Site)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    })
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, outputs(), "Policy") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Policy") # Download figure
    
  })}
