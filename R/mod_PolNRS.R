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
        shiny::HTML("<h3>Select a station:</h3>"),
        shiny::radioButtons(inputId = nsPolNRS("Site"), label = NULL, choices = unique(sort(pkg.env$PolNRS$StationName)), selected = "Maria Island"),
        shiny::conditionalPanel(
          condition = paste0("input.EOV_NRS == 1"), # Only first tab
          shiny::HTML("<h3>Select a parameter:</h3>"),
          shiny::checkboxGroupInput(inputId = nsPolNRS("Parameters"), label = NULL, 
                                    choices = planktonr::pr_relabel(
                                      c("Biomass_mgm3", "PhytoBiomassCarbon_pgL", "ShannonPhytoDiversity", "ShannonCopepodDiversity", 
                                        "CTDTemperature_degC", "Salinity", "PigmentChla_mgm3", "Ammonium_umolL", "Nitrate_umolL", 
                                        "Silicate_umolL", "Phosphate_umolL", "Oxygen_umolL"), style = "simple", named = TRUE),
                                    selected = c("Biomass_mgm3", "PhytoBiomassCarbon_pgL", "CTDTemperature_degC", "Nitrate_umolL", "Phosphate_umolL")),
          shiny::HTML("<strong>NOTE:</strong> Oxygen only available at Maria Island & Rottnest Island")
        ),
        
      ),
      shiny::mainPanel(
        shiny::br(),
        shiny::h3("Essential Ocean Variables"),
        shiny::HTML("<a href='https://www.goosocean.org/index.php?option=com_content&view=article&layout=edit&id=283&Itemid=441'> 
                        Essential Ocean Variables (EOVs)</a> are the important variables that scientists 
                        have identified to monitor our oceans. They are chosen based on impact of the measurement and the 
                        feasiblity to take consistent measurements. They are commonly measured by observing systems and 
                        frequently used in policy making and input into reporting such as State of Environment."),
        shiny::hr(style = "border-top: 2px solid #000000;"),
        shiny::htmlOutput(nsPolNRS("PlotExp1")),
        shiny::br(),
        shiny::htmlOutput(nsPolNRS("StationSummary")),
        shiny::br(),
        shiny::tabsetPanel(id = "EOV_NRS", type = "pills",
                           shiny::tabPanel("All", value = 1,
                                           
                                           shiny::plotOutput(nsPolNRS("timeseries1"), height = 5 * 200) %>%
                                             shinycssloaders::withSpinner(color="#0dc5c1"),
                                           div(style="display:inline-block; float:right; width:60%",
                                               fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                                               fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
                                               fButtons(id, button_id = "downloadCode1", label = "Code", Type = "Action"))
                           ),
                           shiny::tabPanel("Biological", value = 2,
                                           shiny::plotOutput(nsPolNRS("timeseries2"), height = 5 * 200) %>%
                                             shinycssloaders::withSpinner(color="#0dc5c1"),
                                           div(style="display:inline-block; float:right; width:60%",
                                               fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
                                               fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"),
                                               fButtons(id, button_id = "downloadCode2", label = "Code", Type = "Action"))
                           ),
                           shiny::tabPanel("Chemical", value = 3,
                                           shiny::plotOutput(nsPolNRS("timeseries3"), height = 5 * 200) %>%
                                             shinycssloaders::withSpinner(color="#0dc5c1"),
                                           div(style="display:inline-block; float:right; width:60%",
                                               fButtons(id, button_id = "downloadPlot3", label = "Plot", Type = "Download"),
                                               fButtons(id, button_id = "downloadData3", label = "Data", Type = "Download"),
                                               fButtons(id, button_id = "downloadCode3", label = "Code", Type = "Action"))
                           ),
                           shiny::tabPanel("Physical", value = 4,
                                           shiny::plotOutput(nsPolNRS("timeseries4"), height = 2 * 200) %>%
                                             shinycssloaders::withSpinner(color="#0dc5c1"),
                                           div(style="display:inline-block; float:right; width:60%",
                                               fButtons(id, button_id = "downloadPlot4", label = "Plot", Type = "Download"),
                                               fButtons(id, button_id = "downloadData4", label = "Data", Type = "Download"),
                                               fButtons(id, button_id = "downloadCode4", label = "Code", Type = "Action"))
                           ),
        ),
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
      shiny::validate(need(!is.na(input$Site), "Error: Please select a station."))
      
      selectedData <- pkg.env$PolNRS %>% 
        dplyr::filter(.data$StationName %in% input$Site)
      
    }) %>% bindCache(input$Site, input$Parameters)
    
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
    
    stationData <- reactive({
      stationData <- pkg.env$NRSinfo %>% 
        dplyr::filter(.data$StationName == input$Site) 
    }) %>% bindCache(input$Site)
    
    # Sidebar Map
    output$plotmap <- renderPlot({ 
      planktonr::pr_plot_NRSmap(unique(selectedData()$StationCode))
    }, bg = "transparent") %>% 
      bindCache(input$Site)
    
    output$StationSummary <- shiny::renderText({ 
      paste("<h4 style='text-align:center;'>",input$Site,"</h3>The IMOS ", input$Site, " National Reference Station is located at ", round(stationData()$Latitude,2), 
            "\u00B0S and ", round(stationData()$Longitude,2), "\u00B0E", ". The water depth at the station is ", 
            round(stationData()$StationDepth_m,0), "m and is currently sampled ", stationData()$SamplingEffort, 
            ". The station has been sampled since ", format(stationData()$StationStartDate, "%A %d %B %Y"), " ", stationData()$now,
            ". ", input$Site, " is in the ", stationData()$ManagementRegion, 
            " management bioregion. The station is characterised by ", stationData()$Features, ".", sep = "")
    })
    
    titley <- planktonr::pr_relabel(c("PigmentChla_mgm3"), style = "ggplot")
    
    col1 <- fEOVutilities(vector = "col")
    trans1 <- fEOVutilities(vector = "trans")
    
    
    observeEvent({input$EOV_NRS == 1}, {
      
      gg_out1 <- reactive({
        
        p_list <- list()
        for (idx in 1:length(input$Parameters)){
          p <- planktonr::pr_plot_EOVs(selectedData(), EOV = input$Parameters[idx], trans = trans1[[input$Parameters[idx]]], col = col1[[input$Parameters[idx]]])
          p_list[[idx]] <- p
        }
        
        patchwork::wrap_plots(p_list, ncol = 1, byrow = TRUE)  &
          ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                         axis.title = ggplot2::element_text(size = 12, face = "plain"),
                         axis.text =  ggplot2::element_text(size = 10, face = "plain"),
                         plot.title = ggplot2::element_text(hjust = 0.5))
        
      }) %>% bindCache(input$Site, input$Parameters)
      
      
      output$timeseries1 <- renderPlot({
        gg_out1()
      })
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Policy_Select") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Policy_Select", papersize = "A4") # Download figure  
      
    })
    
    
    observeEvent({input$EOV_NRS == 2}, {
      
      gg_out2 <- reactive({
        p1 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "PigmentChla_mgm3", trans = "log10", col = col1["PigmentChla_mgm3"], labels = FALSE) 
        p2 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "PhytoBiomassCarbon_pgL", trans = "log10", col = col1["PhytoBiomassCarbon_pgL"], labels = FALSE) 
        p3 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "Biomass_mgm3", trans = "log10", col = col1["Biomass_mgm3"], labels = FALSE) 
        p4 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "ShannonPhytoDiversity", trans = "log10", col = col1["ShannonPhytoDiversity"], labels = FALSE)
        p5 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "ShannonCopepodDiversity", trans = "log10", col = col1["ShannonCopepodDiversity"]) 
        
        patchwork::wrap_elements(p1 / p2 / p3 / p4 / p5) &
          ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                         axis.title = ggplot2::element_text(size = 12, face = "plain"),
                         axis.text =  ggplot2::element_text(size = 10, face = "plain"),
                         plot.title = ggplot2::element_text(hjust = 0.5))
        
      }) %>% bindCache(input$Site)
      
      output$timeseries2 <- renderPlot({
        gg_out2()
      })
      
      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData, "Policy_Bio") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2, "Policy_Bio", papersize = "A4") # Download figure  
      
    })
    
    observeEvent({input$EOV_NRS == 3}, {
      
      gg_out3 <- reactive({
        p1 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "Ammonium_umolL", trans = "identity", col = col1["Ammonium_umolL"], labels = FALSE)
        p2 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "Nitrate_umolL", trans = "identity", col = col1["Nitrate_umolL"], labels = FALSE)
        p3 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "Silicate_umolL", trans = "identity", col = col1["Silicate_umolL"], labels = FALSE)
        
        if(input$Site %in% c('Maria Island', 'Rottnest Island')){
          p4 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "Phosphate_umolL", trans = "log10", col = col1["Phosphate_umolL"], labels = FALSE)
          p5 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "Oxygen_umolL", trans = "identity", col = col1["Oxygen_umolL"])
        } else {
          p4 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "Phosphate_umolL", trans = "log10", col = col1["Phosphate_umolL"], labels = TRUE)
          p5 <- ggplot2::ggplot() + ggplot2::geom_blank() + ggplot2::theme_void()
        }
        
        patchwork::wrap_elements(p1 / p2 / p3/ p4 / p5) &
          ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                         axis.title = ggplot2::element_text(size = 12, face = "plain"),
                         axis.text =  ggplot2::element_text(size = 10, face = "plain"),
                         plot.title = ggplot2::element_text(hjust = 0.5))
        
      }) %>% bindCache(input$Site)
      
      output$timeseries3 <- renderPlot({
        gg_out3()
      })
      
      # Download -------------------------------------------------------
      output$downloadData3 <- fDownloadButtonServer(input, selectedData, "Policy_Chem") # Download csv of data
      output$downloadPlot3 <- fDownloadPlotServer(input, gg_id = gg_out3, "Policy_Chem", papersize = "A4") # Download figure  
      
    })
    
    observeEvent({input$EOV_NRS == 4}, {
      gg_out4 <- reactive({
        p1 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "CTDTemperature_degC", trans = "identity", col = col1["CTDTemperature_degC"], labels = FALSE)
        p2 <- planktonr::pr_plot_EOVs(selectedData(), EOV = "Salinity", trans = "identity", col = col1["Salinity"])
        
        patchwork::wrap_elements(p1 / p2) &
          ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                         axis.title = ggplot2::element_text(size = 12, face = "plain"),
                         axis.text =  ggplot2::element_text(size = 10, face = "plain"),
                         plot.title = ggplot2::element_text(hjust = 0.5))
        
      }) %>% bindCache(input$Site)
      
      output$timeseries4 <- renderPlot({
        gg_out4()
      })
      
      
      # Download -------------------------------------------------------
      output$downloadData4 <- fDownloadButtonServer(input, selectedData, "Policy_Phys") # Download csv of data
      output$downloadPlot4 <- fDownloadPlotServer(input, gg_id = gg_out4, "Policy_Phys", papersize = "A4") # Download figure  
    })
    
    
  })}
