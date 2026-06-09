#' PhytoTsHAB UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PhytoTsHAB_ui <- function(id){
  nsPhytoTsHAB <- NS(id)
  tagList(
    sidebarLayout(
      fPlanktonSidebar(id = id, tabsetPanel_id = "pHABts", dat = pkg.env$datHABTrip), 
      fPLanktonPanel(id = id, tabsetPanel_id = "pHABts")
    )
  )
}

#' PhytoTsHAB Server Functions
#'
#' @noRd 
mod_PhytoTsHAB_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    observeEvent({input$statepick1}, {
      
      req(input$statepick1)

      # Filter sites based on the selected state
      filtered_sites <- pkg.env$datHABTrip %>%
        dplyr::filter(State %in% input$statepick1) %>%
        dplyr::pull(StationName) %>%
        unique() %>%
        sort()
      # Update the site_input choices
      selectedsites1 <- if(any(input$station1 %in% filtered_sites)){
        input$station1
      } else (
        filtered_sites[1]
      )
      shiny::updateSelectInput(session, "station1",  choices = filtered_sites, selected = selectedsites1)

          }) 

    taxa1 <- reactive({
      
      req(input$tax1)

      taxa <- if(input$tax1 == "genus"){
        taxa <-pkg.env$datHABg 
      } else {
        taxa <-pkg.env$datHABs 
      }
 
    }) %>% bindCache(input$tax1)

    observe({
      req(input$statepick1)

      dat <- taxa1()  %>% 
        dplyr::filter(StationName %in% input$station1) %>% 
        dplyr::summarise(non_zero_count = sum(.data$Values != 0, na.rm = TRUE), .by = c(.data$TaxonName, .data$Parameters)) %>% 
        dplyr::filter(.data$non_zero_count > 50)  
      
      taxa <- unique(sort(dat$TaxonName))
      params <- planktonr:::pr_relabel(unique(sort(dat$Parameters)), style = "simple", named = TRUE)
      
      selectedtaxa1 <- if(input$taxgs1 %in% taxa){
        input$taxgs1
      } else {
        taxa[1]
      }
      
      shiny::updateSelectInput(session, 'taxgs1', choices = taxa, selected = selectedtaxa1)
      shiny::updateSelectInput(session, 'parameter', choices = params, selected = params[1])
      
    }) 
    
    # # Sidebar ----------------------------------------------------------
    #Sidebar Maps - Initial render with current selection
    output$plotmap1 <- leaflet::renderLeaflet({
      if (shiny::isTruthy(input$statepick1) && shiny::isTruthy(input$station1)) {
        select1 <- c(input$station1, input$statepick1)
      } else {
        select1 <- c("Bar Island", "NSW")
      }
      fLeafletMap(select1, Survey = "HAB", Type = "Phytoplankton")
    })
    output$plotmap2 <- leaflet::renderLeaflet({
      fLeafletMap(character(0), Survey = "HAB", Type = "Phytoplankton")
    })
    
    observe({
      if (shiny::isTruthy(input$statepick1) && shiny::isTruthy(input$station1)) {
        select1 <- c(input$station1, input$statepick1)
      } else {
        select1 <- c("Bar Island", "NSW")
      }
      fLeafletUpdate("plotmap1", session, select1, Survey = "HAB", Type = "Phytoplankton")
    }) %>% shiny::bindEvent(input$statepick1, input$station1, ignoreNULL = FALSE)

    # add text information
    output$PlotExp1 <- renderText({
      "A plot of selected phytoplankton Parameters from the Coastal Phytoplankton collection, as a time series and a monthly climatology by station.
      This data comes from a count of selected taxa, it is not a full community count so indices are limited to those appropriate."
      })
    
    output$PlotExp2 <- renderText({
      "A plot of selected phytoplankton Parameters from the Coastal Phytoplankton collection, as a time series and a monthly climatology by station.
      This data comes from a count of selected taxa, it is not a full community count so indices are limited to those appropriate."
    })
    
    param1 <- reactive({
      param <- taxa1() %>% dplyr::filter(.data$Parameters %in% input$parameter)
    }) %>% bindCache(input$parameter)

    # Plot Trends by location -------------------------------------------------------------
    selectedData <- reactive({
      req(input$station1)
      req(input$statepick1)
      req(input$tax1)
      req(input$taxgs1)
      req(input$parameter)
      shiny::validate(need(!is.na(input$station1), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      shiny::validate(need(!is.na(input$tax1), "Error: Please select the taxonomic resolution."))
      shiny::validate(need(!is.na(input$taxgs1), "Error: Please select the taxonomic resolution."))
      shiny::validate(need(!is.na(input$statepick1), "Error: Please select a state."))
      
      df <- taxa1() %>%
        dplyr::filter(.data$TaxonName %in% input$taxgs1,
                      .data$StationName %in% input$station1,
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        dplyr::select(.data$SampleTime_Local, .data$StationName, .data$TaxonName, .data$Parameters, .data$Values)
      
      ## Need to add in zeros
      events <- taxa1() %>%
        dplyr::filter(.data$StationName %in% input$station1,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        dplyr::select(-c(.data$Parameters, .data$Values, .data$TaxonName)) %>%
        dplyr::distinct()
      
      selectedData <- events %>%
        dplyr::left_join(df, by = c("SampleTime_Local", "StationName")) %>%
        dplyr::mutate(Parameters = input$parameter,
                      TaxonName = input$taxgs1,
                      Values = ifelse(is.na(.data$Values), 0, .data$Values),
                      Month_Local = lubridate::month(.data$SampleTime_Local),
                      Year_Local = lubridate::year(.data$SampleTime_Local),
                      StationCode = .data$StationName)
      
    }) %>% bindCache(input$statepick1, input$parameter, input$station1, input$DatesSlide[1], input$DatesSlide[2], input$tax1, input$taxgs1)

    gg_out1 <- reactive({
      dat <- selectedData()
      if(nrow(dat) > 0) {
        
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        if(input$parameter == 'PhytoAbundance_CellsL'){
          taxa <- input$taxgs1
          titley <- bquote(" "*italic(.(taxa))*" (Cells L"^-1*")")
        } else if (input$parameter == 'Biovolume_um3L'){
          titley <- bquote(" "*italic(.(input$taxgs1))*" ("*mu*"m"^-3*")")
        } else if (input$parameter == 'PhytoBiomassCarbon_pgL'){
          titley <- bquote(" "*italic(.(input$taxgs1))*" (pgL"^-1*")")
        } else {
          titley <- bquote(" "*italic(.(input$taxgs1))*" (No species)")
        }
        
        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", method = "lm", trans = trans) +
          ggplot2::labs(y = titley)
        
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")
      } else {
        ggplot2::ggplot() + ggplot2::theme_void()
      }
    }) %>% bindCache(input$statepick1, input$parameter, input$station1, input$DatesSlide[1], input$DatesSlide[2], input$scaler1, input$tax1, input$taxgs1)
    
    output$timeseries1 <- renderPlot({
      req(input$pHABts == 1)
      shiny::validate(
        shiny::need(
          nrow(gg_out1()$data) > 0,
          "Change your selections for a plot to appear."
        )
      )
      gg_out1()
    }, height = function() {if(length(unique(selectedData()$StationName))>0) {
      length(unique(selectedData()$StationName)) * 200
    } else {200}})

    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "TrendLocation") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "TrendLocation") # Download figure

    # Parameter Definition
    output$ParamDef <- fParamDefServer(param1)

    # Plot trends by taxa  -----------------------------------------------------------
    taxa2 <- reactive({
      req(input$tax2)
      
      taxa <- if(input$tax2 == "genus"){
        taxa <- pkg.env$datHABg
      } else {
        taxa <- pkg.env$datHABs
      }
      
    }) %>% bindCache(input$tax2)
    
    observe({
      req(input$pHABts == 2)
      taxa <- unique(sort(taxa2()$TaxonName))
      matched_taxa <- intersect(input$taxgs2, taxa)
      
      selectedtaxa <- if(length(matched_taxa) > 0){
        matched_taxa
      } else {
        taxa[1]
      }
      params <- planktonr:::pr_relabel(unique(sort(taxa2()$Parameters)), style = "simple", named = TRUE)
      
      shiny::updateSelectInput(session, 'taxgs2', choices = taxa, selected = selectedtaxa)
      shiny::updateSelectInput(session, 'parameter', choices = params, selected = params[1])
    })
    
    param2 <- reactive({
      param <- taxa2() %>% dplyr::filter(.data$Parameters %in% input$parameter)
    }) %>% bindCache(input$parameter)
    
    output$ParamDef <- fParamDefServer(param2)

    availableStations2 <- reactive({
      req(input$statepick2)
      req(input$tax2)
      req(input$taxgs2)

      stationsInState <- pkg.env$datHABTrip %>%
        dplyr::filter(State %in% input$statepick2) %>%
        dplyr::pull(StationName) %>%
        unique() %>%
        sort()
      
      if (length(stationsInState) == 0) {
        return(character(0))
      }

      dat <- taxa2() %>%
        dplyr::filter(StationName %in% stationsInState,
                      TaxonName %in% input$taxgs2) %>%
        dplyr::summarise(non_zero_count = sum(.data$Values != 0, na.rm = TRUE), .by = c(.data$TaxonName, .data$StationName)) %>%
        dplyr::filter(.data$non_zero_count > 50)
      
      unique(sort(dat$StationName))
      
    }) %>% bindCache(input$statepick2, input$taxgs2, input$tax2)

    observe({
      req(input$pHABts == 2)
      req(input$statepick2)
      req(input$station2)

      station <- tryCatch({
        availableStations2()
      }, error = function(e) {
        return(NULL) # isTruthy(NULL) is FALSE
      })

      if(isTruthy(station) && input$station2 %in% station){
        select2 <- c(input$station2, input$statepick2)
      } else {
        select2 <- unname(input$statepick2)
      }

      fLeafletUpdate("plotmap2", session, select2, Survey = "HAB", Type = "Phytoplankton")
    }) %>% shiny::bindEvent(input$statepick2, input$station2, ignoreNULL = FALSE)
    
    observeEvent(list(input$tax2, input$taxgs2, input$statepick2, input$DatesSlide[1], input$DatesSlide[2]), {
      req(input$pHABts == 2)
      req(input$statepick2)
      req(input$taxgs2)
      
      station <- availableStations2()
      
      if(length(station) < 1){
        choices <- list("No stations available" = "")
        selectedstation2 <- ""
      } else if(input$station2 %in% station) {
        choices <- station
        selectedstation2 <- input$station2
      } else {
        choices <- station
        selectedstation2 <- station[1]
      }
      shiny::updateSelectInput(session, 'station2', choices = choices, selected = selectedstation2)
    })
    
    selectedData2 <- reactive({
      req(input$statepick2)
      req(input$tax2)
      req(input$taxgs2)
      req(input$parameter)
      station2_val <- input$station2
      validStations <- availableStations2()
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      shiny::validate(need(!is.na(input$tax2), "Error: Please select the taxonomic resolution."))
      shiny::validate(need(!is.na(input$taxgs2), "Error: Please select the taxonomic resolution."))
      shiny::validate(need(!is.na(input$statepick2), "Error: Please select a state."))
      
      # Return empty data frame when no station is available or if station2 is no longer valid
      if (is.null(station2_val) || length(station2_val) == 0 || is.na(station2_val) || identical(station2_val, "") || !(station2_val %in% validStations)) {
        return(data.frame())
      }
      
      df <- taxa2() %>%
        dplyr::filter(.data$TaxonName %in% input$taxgs2,
                      .data$StationName %in% station2_val,
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        dplyr::select(.data$SampleTime_Local, .data$StationName, .data$TaxonName, .data$Parameters, .data$Values)
      
      ## Need to add in zeros
      events <- taxa2() %>%
        dplyr::filter(.data$StationName %in% station2_val,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        dplyr::select(-c(.data$Parameters, .data$Values, .data$TaxonName)) %>%
        dplyr::distinct() %>%
        tidyr::expand_grid(TaxonName = c(input$taxgs2))
      
      selectedData2 <- events %>%
        dplyr::left_join(df, by = c("SampleTime_Local", "StationName", "TaxonName")) %>%
        dplyr::mutate(Parameters = input$parameter,
                      Values = ifelse(is.na(.data$Values), 0, .data$Values)) %>%
        dplyr::mutate(Taxon = .data$StationName,  # change these around so planktonr::pr_plot_trends works without changing the function
                      StationName = .data$TaxonName,
                      StationCode = .data$TaxonName,
                      TaxonName = .data$Taxon,
                      Month_Local = lubridate::month(.data$SampleTime_Local),
                      Year_Local = lubridate::year(.data$SampleTime_Local)) %>%
        planktonr::planktonr_dat(Type = 'Phytoplankton', Survey = 'HAB')
      
      selectedData2

    }) %>% bindCache(input$statepick2, input$parameter, input$station2, input$DatesSlide[1], input$DatesSlide[2], input$tax2, input$taxgs2)

    gg_out2 <- reactive({
      dat <- selectedData2()
      if(nrow(dat) > 0) {
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")
        
        p1 <- planktonr::pr_plot_Trends(selectedData2(), Trend = "Raw", method = "lm", trans = trans) +
          ggplot2::theme(strip.text = ggplot2::element_text(face = "italic", size = 12))
        
        p2 <- planktonr::pr_plot_Trends(selectedData2(), Trend = "Month", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                         strip.text = ggplot2::element_text(face = "italic", size = 12))
        
        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect") +
          patchwork::plot_annotation(title = input$station2,
                                     theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5)))
      } else {
        ggplot2::ggplot() + ggplot2::theme_void()
      }
      
    }) %>% bindCache(input$statepick2, input$parameter, input$station2, input$DatesSlide[1], input$DatesSlide[2], input$scaler2, input$tax2, input$taxgs2)

    output$timeseries2 <- renderPlot({
      req(input$pHABts == 2)
      shiny::validate(
        shiny::need(
          nrow(gg_out2()$data) > 0,
          "Change your selections for a plot to appear."
        )
      )
      gg_out2()
    }, height = function() {if(length(unique(selectedData2()$StationName))>0) {
      length(unique(selectedData2()$StationName)) * 200
    } else {200}})

    # Download -------------------------------------------------------
    output$downloadData2 <- fDownloadButtonServer(input, selectedData2, "TrendTaxa") # Download csv of data
    output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2, "TrendTaxa") # Download figure

    outputOptions(output, "timeseries2", suspendWhenHidden = FALSE)
    outputOptions(output, "plotmap1", suspendWhenHidden = FALSE) # prevent shiny from re-rendering as using this base map twice under phyto tab
    outputOptions(output, "plotmap2", suspendWhenHidden = FALSE)
    
  })
}
