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
mod_PolCPR_ui <- function(id){
  nsPolCPR <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        shinydashboard::menuSubItem(text = "Find out more about the BioRegions here", href = "https://soe.environment.gov.au/theme/marine-environment/topic/2016/marine-regions"),
        shinydashboard::menuSubItem(text = "Find out more about EOVs here", href = "https://www.goosocean.org/index.php?option=com_content&view=article&layout=edit&id=283&Itemid=441"),
        plotlyOutput(nsPolCPR("plotmap")),
        h6("Note there is very little data in the North and North-west regions"),
        radioButtons(inputId = nsPolCPR("Site"), label = "Select a bioregion", choices = unique(sort(PolCPR$BioRegion)), selected = "Temperate East"),
        downloadButton(nsPolCPR("downloadData"), "Data"),
        downloadButton(nsPolCPR("downloadPlot"), "Plot"),
        downloadButton(nsPolCPR("downloadNote"), "Notebook")
      ),
      mainPanel(id = "EOV Biomass by CPR", 
                h6(textOutput(nsPolCPR("PlotExp1"), container = span)),
                h6(verbatimTextOutput(nsPolCPR("PlotExp5"))),
                plotOutput(nsPolCPR("timeseries1"), height = 1000) %>% shinycssloaders::withSpinner(color="#0dc5c1"),
                h6(verbatimTextOutput(nsPolCPR("PlotExp3")))
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
        dplyr::filter(.data$BioRegion %in% input$Site) %>% 
        dplyr::mutate(Month = Month * 2 * 3.142 / 12) %>%
        droplevels()
    }) %>% bindCache(input$Site)
    
    outputs <- reactive({
      outputs <- planktonr::pr_get_coeffs(selectedData())
    }) %>% bindCache(input$Site)
    
    info <- reactive({
      info <- outputs() %>% dplyr::select(slope, p, parameters) %>% unique %>%
        dplyr::arrange(parameters)
    }) %>% bindCache(input$Site)
    
    stationData <- reactive({
      stationData <- CPRinfo %>% dplyr::filter(BioRegion == input$Site) 
    }) %>% bindCache(input$Site)
    
    # Sidebar Map
    output$plotmap <- renderPlotly({ 
      pmap <- planktonr::pr_plot_CPRmap(selectedData())
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
    output$PlotExp5 <- renderText({
      paste("BioRegion:", input$Site, "\n", 
            "The CPR has been sampling in this bioregion since ", min(stationData()$STARTSAMPLEDATEUTC), " and sampling is ongoing.", "\n",
            "A total of ", sum(stationData()$MILES), " nautical miles has been towed.", "\n",
            "The station is characterised by ", unique(stationData()$Features), sep = "")
    })
    
    # Plot Trends -------------------------------------------------------------
    layout1 <- c(
      patchwork::area(1,1,1,1),
      patchwork::area(2,1,2,3),
      patchwork::area(3,1,3,3),
      patchwork::area(4,1,4,1),
      patchwork::area(5,1,5,3),
      patchwork::area(6,1,6,3)
    )
    
    output$timeseries1 <- renderPlot({
      
      p1 <-planktonr::pr_plot_EOV(outputs(), "Biomass_mgm3", Survey = 'CPR', "log10", pal = "matter", labels = "no")
      p2 <-planktonr::pr_plot_EOV(outputs(), "PhytoBiomassCarbon_pgm3", Survey = 'CPR', "log10", pal = "algae") 
      
      p6 <-planktonr::pr_plot_EOV(outputs(), "ShannonCopepodDiversityCPR", Survey = 'CPR', "log10", pal = "matter", labels = "no") #check these col names with new indicies data from AODN
      p7 <-planktonr::pr_plot_EOV(outputs(), "ShannonPhytoDiversitycpr", Survey = 'CPR', "log10", pal = "algae")
      
     patchwork::wrap_elements(grid::textGrob("Biomass EOVs", gp = grid::gpar(fontsize=20))) + 
        p1 + p2 + 
        grid::textGrob("Diversity EOVs", gp = grid::gpar(fontsize=20)) + 
        p6 + p7 + 
        patchwork::plot_layout(design = layout1) +
        patchwork::plot_annotation(title = input$Site) & 
        ggplot2::theme(title = element_text(size = 20, face = "bold"),
                       plot.title = element_text(hjust = 0.5))
      
    }) %>% bindCache(input$Site)
    
  })}
