#' Policy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PolSOTS_ui <- function(id){
  nsPolSOTS <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        leaflet::leafletOutput(nsPolSOTS("plotmap"), height = "400px"),
        shiny::HTML("<h5><strong>Select a station:</strong></h5>"),
        shiny::radioButtons(inputId = nsPolSOTS("Site"), label = NULL, choices = unique(sort(pkg.env$PolSOTS$StationName)), 
                            selected = "Southern Ocean Time Series"),
        shiny::conditionalPanel(
          condition = paste0("input.EOV_SOTS == 1"), # Only first tab
          shiny::HTML("<h5><strong>Select a parameter:</strong></h5>"),
          shiny::checkboxGroupInput(inputId = nsPolSOTS("Parameters"), label = NULL, 
                                    choices = planktonr::pr_relabel(
                                      c("ChlF_mgm3", "DissolvedOxygen_umolkg", "PhytoBiomassCarbon_pgL",
                                        "ShannonPhytoDiversity", "Nitrate_umolL", 
                                        "Salinity", "Temperature_degC", "Silicate_umolL",
                                        "Phosphate_umolL"), style = "simple", named = TRUE),
                                    selected = c("ChlF_mgm3", "Temperature_degC", "Nitrate_umolL", "Phosphate_umolL")),
          shiny::HTML("<h5><strong>Select a depth:</strong></h5>"),
          shiny::radioButtons(inputId = nsPolSOTS("Depths"), label = NULL, 
                                    choices = c("0 m", "30 m"),
                                    selected = c("0 m"))
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
        shiny::htmlOutput(nsPolSOTS("PlotExp1")),
        shiny::br(),
        shiny::htmlOutput(nsPolSOTS("StationSummary")),
        shiny::br(),
        shiny::tabsetPanel(id = "EOV_SOTS", type = "pills",
                           shiny::tabPanel("All", value = 1,
                                           
                                           shiny::plotOutput(nsPolSOTS("timeseries1"), height = 5 * 200) %>%
                                             shinycssloaders::withSpinner(color="#0dc5c1"),
                                           div(style="display:inline-block; float:right; width:60%",
                                               fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                                               fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
                                               fButtons(id, button_id = "downloadCode1", label = "Code", Type = "Action"))
                           ),
                           shiny::tabPanel("Biological", value = 2,
                                           shiny::plotOutput(nsPolSOTS("timeseries2"), height = 5 * 200) %>%
                                             shinycssloaders::withSpinner(color="#0dc5c1"),
                                           div(style="display:inline-block; float:right; width:60%",
                                               fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
                                               fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"),
                                               fButtons(id, button_id = "downloadCode2", label = "Code", Type = "Action"))
                           ),
                           shiny::tabPanel("Chemical", value = 3,
                                           shiny::plotOutput(nsPolSOTS("timeseries3"), height = 5 * 200) %>%
                                             shinycssloaders::withSpinner(color="#0dc5c1"),
                                           div(style="display:inline-block; float:right; width:60%",
                                               fButtons(id, button_id = "downloadPlot3", label = "Plot", Type = "Download"),
                                               fButtons(id, button_id = "downloadData3", label = "Data", Type = "Download"),
                                               fButtons(id, button_id = "downloadCode3", label = "Code", Type = "Action"))
                           ),
                           shiny::tabPanel("Physical", value = 4,
                                           shiny::plotOutput(nsPolSOTS("timeseries4"), height = 4 * 200) %>%
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
mod_PolSOTS_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Sidebar ----------------------------------------------------------
    selectedData0 <- reactive({
      req(input$Site)
      shiny::validate(need(!is.na(input$Site), "Error: Please select a station."))
      
      selectedData0 <- pkg.env$PolSOTS %>% 
        dplyr::filter(.data$SampleDepth_m == 0)
      
    }) %>% bindCache(input$Site, input$Parameters)
    
    selectedData30 <- reactive({
      req(input$Site)
      shiny::validate(need(!is.na(input$Site), "Error: Please select a station."))
      
      selectedData30 <- pkg.env$PolSOTS %>% 
        dplyr::filter(.data$SampleDepth_m == 30 | is.na(.data$SampleDepth_m)) # phyto needs depths, then change this
      
    }) %>% bindCache(input$Site, input$Parameters)
    
    selectedData200 <- reactive({
      req(input$Site)
      shiny::validate(need(!is.na(input$Site), "Error: Please select a station."))
      
      selectedData200 <- pkg.env$PolSOTS %>% 
        dplyr::filter(.data$SampleDepth_m == 200)
      
    }) %>% bindCache(input$Site, input$Parameters)
    
    selectedData500 <- reactive({
      req(input$Site)
      shiny::validate(need(!is.na(input$Site), "Error: Please select a station."))
      
      selectedData500 <- pkg.env$PolSOTS %>% 
        dplyr::filter(.data$SampleDepth_m == 500)
      
    }) %>% bindCache(input$Site, input$Parameters)    
    
    stationData <- reactive({
      stationData <- pkg.env$SOTSinfo %>% 
        dplyr::filter(.data$StationName == input$Site) 
    }) %>% bindCache(input$Site)
    
    # Sidebar Map - Initial render
    output$plotmap <- leaflet::renderLeaflet({ 
      fLeafletMap(character(0), Survey = "NRS", Type = "Phytoplankton")
    })
    
    # Update map when station selection changes
    observe({
      fLeafletUpdate("plotmap", session, unique(selectedData0()$StationCode), 
                     Survey = "NRS", Type = "Phytoplankton")
    })
    
    
    output$StationSummary <- shiny::renderText({ 
      paste("<h4 style='text-align:center; font-weight: bold;'>",input$Site,"</h5>The IMOS ", input$Site, " National Reference Station is located at ", round(stationData()$Latitude,2), 
            "\u00B0S and ", round(stationData()$Longitude,2), "\u00B0E", ". The water depth at the station is ", 
            round(stationData()$StationDepth_m,0), "m and is currently sampled ", stationData()$SamplingEffort, 
            ". The station has been sampled since ", format(stationData()$StationStartDate, "%A %d %B %Y"), " ", stationData()$now,
            ". ", input$Site, " is in the ", stationData()$ManagementRegion, 
            " management bioregion. The station is characterised by ", stationData()$Features, ".", sep = "")
    })
    
    col1 <- fEOVutilities(vector = "col", Survey = "SOTS")
    trans1 <- fEOVutilities(vector = "trans", Survey = "SOTS")
    
    observeEvent({input$EOV_SOTS == 1}, {
      
      gg_out1 <- reactive({
        
        if(input$Depths == "30 m"){
          dat <- selectedData30()
          lab <- "At 30 m"
        } else {
          dat <- selectedData0()
          lab<- "At 0 m"
        }
        
        p_list <- list()
        for (idx in 1:length(input$Parameters)){
          p <- planktonr::pr_plot_EOVs(dat, EOV = input$Parameters[idx], trans = trans1[[input$Parameters[idx]]], col = col1[[input$Parameters[idx]]])
          p_list[[idx]] <- p
        }
        
        patchwork::wrap_plots(patchwork::wrap_elements(grid::textGrob(lab, gp = grid::gpar(fontsize = 20))) / p_list, ncol = 1, byrow = TRUE)  &
          ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                         axis.title = ggplot2::element_text(size = 12, face = "plain"),
                         axis.text =  ggplot2::element_text(size = 10, face = "plain"),
                         plot.title = ggplot2::element_text(hjust = 0.5))
        
      }) %>% bindCache(input$Site, input$Parameters, input$Depths)
      
      output$timeseries1 <- renderPlot({
        gg_out1()
      })
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData0, "Policy_Select") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Policy_Select", papersize = "A4") # Download figure  
      
    })

    observeEvent({input$EOV_SOTS == 2}, {
      
      gg_out2 <- reactive({
        
        p10 <- planktonr::pr_plot_EOVs(selectedData0(), EOV = "ChlF_mgm3", trans = "identity", col = col1["ChlF_mgm3"], labels = FALSE) 
        p130 <- planktonr::pr_plot_EOVs(selectedData30(), EOV = "ChlF_mgm3", trans = "identity", col = col1["ChlF_mgm3"], labels = FALSE)
        p20 <- planktonr::pr_plot_EOVs(selectedData0(), EOV = "PhytoBiomassCarbon_pgL", trans = "log10", col = col1["PhytoBiomassCarbon_pgL"], labels = FALSE) 
        p230 <- planktonr::pr_plot_EOVs(selectedData30(), EOV = "PhytoBiomassCarbon_pgL", trans = "log10", col = col1["PhytoBiomassCarbon_pgL"], labels = FALSE) 
        p30 <- planktonr::pr_plot_EOVs(selectedData0(), EOV = "ShannonPhytoDiversity", trans = "log10", col = col1["ShannonPhytoDiversity"], labels = FALSE) 
        p330 <- planktonr::pr_plot_EOVs(selectedData30(), EOV = "ShannonPhytoDiversity", trans = "log10", col = col1["ShannonPhytoDiversity"], labels = FALSE) 
        
        patchwork::wrap_elements(patchwork::wrap_elements(grid::textGrob('At 0 m')) / p10 / p20 / p30 /
                                 patchwork::wrap_elements(grid::textGrob('At 30 m')) / p130 / p230 / p330) &
          ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                         axis.title = ggplot2::element_text(size = 12, face = "plain"),
                         axis.text =  ggplot2::element_text(size = 10, face = "plain"),
                         plot.title = ggplot2::element_text(hjust = 0.5)) 
        
      }) %>% bindCache(input$Site)
      
      output$timeseries2 <- renderPlot({
        gg_out2()
      })
      
      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData0, "Policy_Bio") # Download csv of data
      #TODO - this needs to be 0m and 30m data, same goes for all downloads
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2, "Policy_Bio", papersize = "A4") # Download figure  
      
    })
    
    observeEvent({input$EOV_SOTS == 3}, {
      
      gg_out3 <- reactive({
        p20 <- planktonr::pr_plot_EOVs(selectedData0(), EOV = "Nitrate_umolL", trans = "identity", col = col1["Nitrate_umolL"], labels = FALSE)
        p230 <- planktonr::pr_plot_EOVs(selectedData30(), EOV = "Nitrate_umolL", trans = "identity", col = col1["Nitrate_umolL"], labels = FALSE)
        p30 <- planktonr::pr_plot_EOVs(selectedData0(), EOV = "Silicate_umolL", trans = "identity", col = col1["Silicate_umolL"], labels = FALSE)
        p330 <- planktonr::pr_plot_EOVs(selectedData30(), EOV = "Silicate_umolL", trans = "identity", col = col1["Silicate_umolL"], labels = FALSE)
        p40 <- planktonr::pr_plot_EOVs(selectedData0(), EOV = "Phosphate_umolL", trans = "log10", col = col1["Phosphate_umolL"], labels = FALSE)
        p430 <- planktonr::pr_plot_EOVs(selectedData30(), EOV = "Phosphate_umolL", trans = "log10", col = col1["Phosphate_umolL"], labels = FALSE)

        patchwork::wrap_elements(patchwork::wrap_elements(grid::textGrob('At 0 m', gp = grid::gpar(fontsize = 20))) / p20 / p30/ p40 /
                                   patchwork::wrap_elements(grid::textGrob('At 30 m', gp = grid::gpar(fontsize = 20))) /p230 / p330/ p430 ) & #/ p5) &
          ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                         axis.title = ggplot2::element_text(size = 12, face = "plain"),
                         axis.text =  ggplot2::element_text(size = 10, face = "plain"),
                         plot.title = ggplot2::element_text(hjust = 0.5))
        
      }) %>% bindCache(input$Site)
      
      output$timeseries3 <- renderPlot({
        gg_out3()
      })
      
      # Download -------------------------------------------------------
      output$downloadData3 <- fDownloadButtonServer(input, selectedData0, "Policy_Chem") # Download csv of data
      output$downloadPlot3 <- fDownloadPlotServer(input, gg_id = gg_out3, "Policy_Chem", papersize = "A4") # Download figure  
      
    })
    
    observeEvent({input$EOV_SOTS == 4}, {
      gg_out4 <- reactive({
        
        p10 <- planktonr::pr_plot_EOVs(selectedData0(), EOV = "Temperature_degC", trans = "identity", col = col1["Temperature_degC"], labels = FALSE)
        p1200 <- planktonr::pr_plot_EOVs(selectedData200(), EOV = "Temperature_degC", trans = "identity", col = col1["Temperature_degC"], labels = FALSE)
        p1500 <- planktonr::pr_plot_EOVs(selectedData500(), EOV = "Temperature_degC", trans = "identity", col = col1["Temperature_degC"], labels = FALSE)
        p20 <- planktonr::pr_plot_EOVs(selectedData0(), EOV = "Salinity", trans = "identity", col = col1["Salinity"])
        p2200 <- planktonr::pr_plot_EOVs(selectedData200(), EOV = "Salinity", trans = "identity", col = col1["Salinity"])
        
        patchwork::wrap_elements(patchwork::wrap_elements(grid::textGrob('At 0 m', gp = grid::gpar(fontsize = 20))) / p10 / p20 /
                                   patchwork::wrap_elements(grid::textGrob('At 200 m', gp = grid::gpar(fontsize = 20))) /p1200 / p2200 / 
                                   patchwork::wrap_elements(grid::textGrob('At 500 m', gp = grid::gpar(fontsize = 20))) / p1500) &
          ggplot2::theme(title = ggplot2::element_text(size = 20, face = "bold"),
                         axis.title = ggplot2::element_text(size = 12, face = "plain"),
                         axis.text =  ggplot2::element_text(size = 10, face = "plain"),
                         plot.title = ggplot2::element_text(hjust = 0.5))
        
      }) %>% bindCache(input$Site)
      
      output$timeseries4 <- renderPlot({
        gg_out4()
      })
      
      
      # Download -------------------------------------------------------
      output$downloadData4 <- fDownloadButtonServer(input, selectedData0, "Policy_Phys") # Download csv of data
      output$downloadPlot4 <- fDownloadPlotServer(input, gg_id = gg_out4, "Policy_Phys", papersize = "A4") # Download figure  
    })
    
    
  })}
