#' Policy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PolLTM_ui <- function(id){
  nsPolLTM <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        plotOutput(nsPolLTM("plotmap")),
        shiny::HTML("<h5><strong>Select a station:</strong></h5>"),
        radioButtons(inputId = nsPolLTM("SiteLTM"), label = NULL, choices = unique(sort(pkg.env$PolLTM$StationName)), selected = "Port Hacking")
      ),
      mainPanel(id = "EOV paramters from Long Term Monitoring", 
                shiny::br(),
                shiny::h3("Essential Ocean Variables"),
                shiny::HTML("<a href='https://www.goosocean.org/index.php?option=com_content&view=article&layout=edit&id=283&Itemid=441'> 
                        Essential Ocean Variables (EOVs)</a> are the important variables that scientists 
                        have identified to monitor our oceans. They are chosen based on impact of the measurement and the 
                        feasiblity to take consistent measurements. They are commonly measured by observing systems and 
                        frequently used in policy making and input into reporting such as State of Environment."),
                shiny::hr(style = "border-top: 2px solid #000000;"),
                shiny::br(),
                shiny::htmlOutput(nsPolLTM("StationSummary")),
                shiny::br(),
                plotOutput(nsPolLTM("timeseries1"), height = 1000) %>% 
                  shinycssloaders::withSpinner(color="#0dc5c1"), 
                div(style="display:inline-block; float:right; width:60%",
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
mod_PolLTM_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Sidebar ----------------------------------------------------------
    selectedDataLTM <- reactive({
      req(input$SiteLTM)
      shiny::validate(need(!is.na(input$SiteLTM), "Error: Please select a station."))
      
      selectedDataLTM <- pkg.env$PolLTM %>% 
        dplyr::filter(.data$StationName %in% input$SiteLTM,
                      !.data$Parameters %in% c("Ammonium_umolL","Nitrite_umolL", "Oxygen_umolL")) 
      
    }) %>% bindCache(input$SiteLTM)
    
    shiny::exportTestValues(
      PolLTM = {ncol(selectedDataLTM())},
      PolLTMRows = {nrow(selectedDataLTM()) > 0},
      PolLTMYearisNumeric = {class(selectedDataLTM()$Year_Local)},
      PolLTMMonthisNumeric = {class(selectedDataLTM()$Month_Local)},
      PolLTMMeansisNumeric = {class(selectedDataLTM()$means)},
      PolLTMsdisNumeric = {class(selectedDataLTM()$sd)},
      PolLTMAnomalyisNumeric = {class(selectedDataLTM()$anomaly)},
      PolLTMDepthisNumeric = {class(selectedDataLTM()$SampleDepth_m)},
      PolLTMDateisDate = {class(selectedDataLTM()$SampleTimee_Local)},
      PolLTMProjectisChr = {class(selectedDataLTM()$Project)},
      PolLTMStationisChr = {class(selectedDataLTM()$StationName)},
      PolLTMCodeisChr = {class(selectedDataLTM()$StationCode)},
      PolLTMParametersisChr = {class(selectedDataLTM()$Parameters)},
      PolLTMValuesisNumeric = {class(selectedDataLTM()$Values)}
    )
    
    outputs <- reactive({
      outputs <- planktonr::pr_get_Coeffs(selectedDataLTM())
    }) %>% bindCache(input$SiteLTM)
    
    info <- reactive({
      info <- outputs() %>% dplyr::select(.data$slope, .data$p, .data$Parameters) %>% unique() %>%
        dplyr::arrange(.data$Parameters)
    }) %>% bindCache(input$SiteLTM)
    
    stationData <- reactive({
      stationData <- pkg.env$NRSinfo %>% dplyr::filter(.data$StationName == input$SiteLTM) 
    }) %>% bindCache(input$SiteLTM)
    
    # Sidebar Map
    output$plotmap <- renderPlot({ 
      planktonr::pr_plot_NRSmap(selectedDataLTM())
    }, bg = "transparent") %>% bindCache(input$SiteLTM)
    
    output$StationSummary <- shiny::renderText({ 
      paste("<h4 style='text-align:center; font-weight: bold;'>",input$SiteLTM,"</h5>The ", input$SiteLTM, 
            " Longterm Monitoring Station is located at ", round(stationData()$Latitude,2), 
            "\u00B0S and ", round(stationData()$Longitude,2), "\u00B0E", ". The water depth at the station is ", 
            round(stationData()$StationDepth_m,0), "m and is currently sampled ", stationData()$SamplingEffort, 
            ". The station has been sampled since ", format(min(selectedDataLTM()$SampleTime_Local), "%A %d %B %Y"), " ", stationData()$now,
            ". ", input$SiteLTM, " is part of ", stationData()$Node, " and is in the ", stationData()$ManagementRegion, 
            " management bioregion. The station is characterised by ", stationData()$Features, ".", sep = "")})
    
    col1 <- fEOVutilities(vector = "col", Survey = "LTM")
    
    # Plot Trends -------------------------------------------------------------
    #t, l, b, r
    layout1 <- c(
      patchwork::area(1,1,1,3), # Header
      patchwork::area(2,1,3,3),
      patchwork::area(4,1,5,3),
      patchwork::area(6,1,7,3),
      patchwork::area(8,1,9,3), # Header
      patchwork::area(10,1,11,3),
      patchwork::area(12,1,13,3)
    )
    
    gg_out1 <- reactive({
      
      p1 <- planktonr::pr_plot_EOVs(outputs(), EOV = "Nitrate_umolL", Survey = "LTM", trans = "identity", col = col1["Nitrate_umolL"], labels = FALSE)
      p2 <- planktonr::pr_plot_EOVs(outputs(), EOV = "Phosphate_umolL", Survey = "LTM", trans = "identity", col = col1["Phosphate_umolL"], labels = FALSE) 
      p4 <- planktonr::pr_plot_EOVs(outputs(), EOV = "Silicate_umolL", Survey = "LTM", trans = "identity", col = col1["Silicate_umolL"], labels = FALSE) 
      p7 <- planktonr::pr_plot_EOVs(outputs(), EOV = "Temperature_degC", Survey = "LTM", trans = "identity", col = col1["Temperature_degC"], labels = FALSE)
      p3 <- planktonr::pr_plot_EOVs(outputs(), EOV = "Salinity", Survey = "LTM", trans = "identity", col = col1["Salinity"])
      
      
      patchwork::wrap_elements(
        grid::textGrob("Physical EOVs", gp = grid::gpar(fontsize=20))) + 
        p7 + p3 + 
        grid::textGrob("Biochemical EOVs", gp = grid::gpar(fontsize=20)) + 
        p1 + p2 + p4 + 
        patchwork::plot_layout(design = layout1) &
        ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                       axis.title = ggplot2::element_text(size = 12, face = "plain"),
                       axis.text = ggplot2::element_text(size = 10, face = "plain"),
                       plot.title = ggplot2::element_text(hjust = 0.5)) 
      
    }) %>% bindCache(input$SiteLTM)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    })
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, outputs(), "Policy") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Policy", papersize = "A2") # Download figure
    
  })}
