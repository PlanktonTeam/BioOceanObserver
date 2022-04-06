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
mod_PolLTM_ui <- function(id){
  nsPolLTM <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        shinydashboard::menuSubItem(text = "Find out more about the NRS stations here", href = "https://github.com/PlanktonTeam/IMOS_BioOceanObserver/wiki/National-Reference-Stations"),
        shinydashboard::menuSubItem(text = "Find out more about EOVs here", href = "https://www.goosocean.org/index.php?option=com_content&view=article&layout=edit&id=283&Itemid=441"),
        plotlyOutput(nsPolLTM("plotmap")),
        radioButtons(inputId = nsPolLTM("SiteLTM"), label = "Select a station", choices = unique(sort(LTnuts$StationName)), selected = "Port Hacking"),
        downloadButton(nsPolLTM("downloadData"), "Data"),
        downloadButton(nsPolLTM("downloadPlot"), "Plot"),
        downloadButton(nsPolLTM("downloadNote"), "Notebook")
          ),
      mainPanel(id = "EOV paramters from Long Term Monitoring", 
                h6(textOutput(nsPolLTM("PlotExp1"), container = span)),
                h6(verbatimTextOutput(nsPolLTM("PlotExp5"))),
                plotOutput(nsPolLTM("timeseriesLTM"), height = 1000) %>% shinycssloaders::withSpinner(color="#0dc5c1"), 
                h6(verbatimTextOutput(nsPolLTM("PlotExp3")))
             )
      )
    )
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
      validate(need(!is.na(input$SiteLTM), "Error: Please select a station."))
      
      selectedDataLTM <- LTnuts %>% 
        dplyr::filter(.data$StationName %in% input$SiteLTM,
                      .data$SampleDepth_m < 15,
                      !.data$parameters %in% c( "SOI", "Ammonium_umolL","Nitrite_umolL","DIC_umolkg", "TAlkalinity_umolkg", "Oxygen_umolL")) %>%
        dplyr::group_by(.data$StationCode, .data$StationName, .data$SampleDate_Local, .data$anomaly, .data$Year, .data$Month, parameters) %>%
        dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                                      .groups = 'drop') %>%
        dplyr::rename(SampleDate = .data$SampleDate_Local) %>% 
        dplyr::mutate(Month = .data$Month * 2 * 3.142 / 12) %>%
        droplevels()
      
    }) %>% bindCache(input$SiteLTM)
    
    outputs <- reactive({
      outputs <- planktonr::pr_get_coeffs(selectedDataLTM())
    }) %>% bindCache(input$SiteLTM)
    
    info <- reactive({
      info <- outputs() %>% dplyr::select(slope, p, parameters) %>% unique() %>%
        dplyr::arrange(parameters)
    }) %>% bindCache(input$SiteLTM)
    
    stationData <- reactive({
       stationData <- NRSinfo %>% dplyr::filter(StationName == input$SiteLTM) 
    }) %>% bindCache(input$SiteLTM)
    
    # Sidebar Map
    output$plotmap <- renderPlotly({ 
      pmap <- planktonr::pr_plot_NRSmap(selectedDataLTM())
    }) %>% bindCache(input$SiteLTM)
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "Biomass and diversity are the Essential Ocean Variables (EOVs) for plankton. 
      These are the important variables that scientists have identified to monitor our oceans.
      They are chosen based on impact of the measurement and the feasiblity to take consistent measurements.
      They are commonly measured by observing systems and frequently used in policy making and input into reporting such as State of Environment"
    }) 
    output$PlotExp3 <- renderText({
        paste(" Nitrate concentration at", input$SiteLTM, "is", info()[1,1], info()[1,2],  "\n",
            "Phosphate concentration at", input$SiteLTM, "is", info()[2,1], info()[2,2],  "\n",
            "Silicate concentration", input$SiteLTM, "is", info()[4,1], info()[4,2],  "\n",
            "Temperature at", input$SiteLTM, "is", info()[5,1], info()[5,2],  "\n",
            "Salinity at", input$SiteLTM, "is", info()[3,1], info()[3,2],  "\n")
    }) 
    output$PlotExp5 <- renderText({
      paste("STation Name:", input$SiteLTM, "\n",
            input$SiteLTM, " National Reference Station is located at ", round(stationData()$Latitude,2), "\u00B0S and ", round(stationData()$Longitude,2), "\u00B0E", ".", "\n",
            "The water depth at the station is ", round(stationData()$STATIONDEPTH_M,0), "m and is currently sampled ", stationData()$SAMPLINGEFFORT, ".", "\n",
            "The station has been sampled since ", stationData()$STATIONSTARTDATE, " ", stationData()$now, ".", "\n",
            input$SiteLTM, " is part of ", stationData()$NODE, " and is in the ", stationData()$MANAGEMENTREGION, " management bioregion.",  "\n",
            "The station is characterised by ", stationData()$Features, sep = "")
    })
    
    # Plot Trends -------------------------------------------------------------
    layout1 <- c(
      patchwork::area(1,1,1,1),
      patchwork::area(2,1,2,3),
      patchwork::area(3,1,3,3),
      patchwork::area(4,1,4,3),
      patchwork::area(5,1,5,3),
      patchwork::area(6,1,6,3)
    )
    
    output$timeseriesLTM <- renderPlot({

          p1 <-planktonr::pr_plot_EOV(outputs(), "Nitrate_umolL", Survey = 'LTM', trans = "identity", pal = "matter", labels = "no")
          p2 <-planktonr::pr_plot_EOV(outputs(), "Phosphate_umolL", Survey = 'LTM', "identity", pal = "algae", labels = "no") 
          p4 <-planktonr::pr_plot_EOV(outputs(), "Silicate_umolL", Survey = 'LTM', "identity", pal = "haline", labels = "no") 
          p7 <-planktonr::pr_plot_EOV(outputs(), "Temperature_degC", Survey = 'LTM', "identity", pal = "solar", labels = "no")
          p3 <-planktonr::pr_plot_EOV(outputs(), "Salinity_psu", Survey = 'LTM', "identity", pal = "dense")
          
          patchwork::wrap_elements(grid::textGrob("Physcial EOVs", gp = grid::gpar(fontsize=20))) + 
            p1 + p2 + p4 + p7 + p3 +
            patchwork::plot_layout(design = layout1) &
            patchwork::plot_annotation(title = input$SiteLTM)  +
            ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                           plot.title = ggplot2::element_text(hjust = 0.5)) 
          
        }) %>% bindCache(input$SiteLTM)
    
})}
