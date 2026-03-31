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
      fPlanktonSidebar(id = id, tabsetPanel_id = "phabts", dat = pkg.env$datHABg, dat1 = pkg.env$datHABTrip), #TODO add pkg.env$ throughout
      fPLanktonPanel(id = id, tabsetPanel_id = "phabts")
    )
  )
}

#' PhytoTsHAB Server Functions
#'
#' @noRd 
mod_PhytoTsHAB_server <- function(id){
  
  moduleServer(id, function(input, output, session, phabts){
    
    # # Sidebar ----------------------------------------------------------
    observe({

      taxon <- input$tax

      if(taxon == "genus"){
        dat <- pkg.env$datHABg %>%
          dplyr::rename(TaxonName = .data$genus) 
      } else {
        dat <- pkg.env$datHABs 
        }
     
      dat <- dat %>% 
        dplyr::filter(.data$StationName %in% input$station,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2]))
      
        taxa <- unique(sort(dat$TaxonName))
        params <- planktonr:::pr_relabel(unique(sort(dat$Parameters)), style = "simple", named = TRUE)

      shiny::updateSelectInput(session, 'taxgs', choices = taxa, selected = taxa[1])
      shiny::updateSelectInput(session, 'parameter', choices = params, selected = params[1])
      
    }) %>%  shiny::bindEvent(input$tax, input$station, input$DatesSlide[1], input$DatesSlide[2])

    observeEvent(input$state, {

      # Filter sites based on the selected state
      filtered_sites <- unique(sort((pkg.env$datHABTrip %>% dplyr::filter(.data$State %in% input$state))$StationName))

      # Update the site_input choices
      shiny::updateSelectInput(session, "station",  choices = filtered_sites, selected = filtered_sites[1])
    })


    # observeEvent({input$NRSpt == 1 | input$NRSpt == 2}, {
    selectedData <- reactive({ #TODO - This reactive encompasses things from 1/2 AND 3. Can we split them?

      req(input$station)
      req(input$state)
      req(input$tax)
      req(input$taxgs)
      req(input$parameter)
      shiny::validate(need(!is.na(input$station), "Error: Please select a station."))
      shiny::validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      shiny::validate(need(!is.na(input$tax), "Error: Please select the taxonomic resolution."))
      shiny::validate(need(!is.na(input$taxgs), "Error: Please select the taxonomic resolution."))
      shiny::validate(need(!is.na(input$state), "Error: Please select a state."))

      if(input$tax == "genus"){
        dat <- pkg.env$datHABg %>%
          dplyr::rename(TaxonName = .data$genus)
      } else {
        dat <- pkg.env$datHABs 
      }
      
      df <- dat %>%
        dplyr::filter(.data$TaxonName %in% input$taxgs,
                      .data$StationName %in% input$station,
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>% 
        dplyr::select(.data$SampleTime_Local, .data$StationName, .data$TaxonName, .data$Parameters, .data$Values) 
      
      ## Need to add in zeros
      events <- dat %>% 
        dplyr::filter(.data$StationName %in% input$station,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>% 
        dplyr::select(-c(.data$Parameters, .data$Values, .data$TaxonName)) %>% 
        dplyr::distinct()
      
      selectedData <- events %>% 
        dplyr::left_join(df, by = c("SampleTime_Local", "StationName")) %>% 
        dplyr::mutate(Parameters = input$parameter,
                      TaxonName = input$taxgs, 
                      Values = ifelse(is.na(Values), 0, Values))

    }) %>% bindCache(input$parameter, input$station, input$DatesSlide[1], input$DatesSlide[2], input$tax, input$taxgs)
    # })

    # observeEvent({input$phabts == 2},{
    # 
    #   # 1. Capture current selections
    #   current_taxa <- input$taxgs
    #   current_station <- input$station
    #   choices_to_use <- if(input$tax == "genus"){
    #     pkg.env$datHABg %>%
    #       dplyr::rename(TaxonName = genus) %>%
    #       dplyr::distinct(.data$TaxonName) %>%
    #       dplyr::pull(.data$TaxonName)
    #   } else {
    #     pkg.env$datHABs %>%
    #       dplyr::distinct(.data$TaxonName) %>%
    #       dplyr::pull(.data$TaxonName)
    #   }
    # 
    #     multipleTax <- TRUE
    #     multipleLoc <- FALSE
    #     # If switching to single-select, take only the first current value
    #     if(length(current_station) > 1) {current_station <- current_station[1]}
    # 
    #   shiny::updateSelectInput(session, 'taxgs', choices = choices_to_use,
    #                            selected = 'Amphora', multiple = TRUE)
    #   shiny::updateSelectInput(session, 'station', choices = unique(sort(pkg.env$datHABTrip$StationName)),
    #                            selected = 'Bar Island', multiple = multipleLoc)
    # })
    # 
    # #Sidebar Map - Initial render
    # output$plotmap <- leaflet::renderLeaflet({
    #   fLeafletMap(character(0), Survey = "HAB", Type = "Phytoplankton")
    # })
    
  #  Update map when station selection changes
    observe({
      #Convert StationName to StationCode, handle empty selection
      StationNames <- if (length(input$station) > 0) {
        unique(pkg.env$datHABTrip %>%
          dplyr::filter(.data$StationName %in% input$station) %>%
          dplyr::pull(.data$StationName))
      } else {
        character(0)
      }
      fLeafletUpdate("plotmap", session, StationNames,
                     Survey = "HAB", Type = "Phytoplankton")
    })

    # add text information
    output$PlotExp1 <- renderText({
      "A plot of selected phytoplankton Parameters from the Coastal Phytoplankton collection, as a time series and a monthly climatology by station.
      This data comes from a count of selected taxa, it is not a full community count so indices are limited to those appropriate."
      })


    # Plot Trends by location -------------------------------------------------------------
    observeEvent({input$phabts == 1}, {

      gg_out1 <- reactive({
        if (is.null(datHABg$StationCode)) {return(NULL)}
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")

        if(input$parameter == 'PhytoAbundance_CellsL'){
          taxa <- input$taxgs
          titley <- bquote(" "*italic(.(taxa))*" (Cells L"^-1*")")
        } else if (input$parameter == 'Biovolume_um3L'){
          titley <- bquote(" "*italic(.(input$taxgs))*" ("*mu*"m"^-3*")")
        } else if (input$parameter == 'PhytoBiomassCarbon_pgL'){
          titley <- bquote(" "*italic(.(input$taxgs))*" (pgL"^-1*")")
        } else {
          titley <- bquote(" "*italic(.(input$taxgs))*" (No species)")
        }
        
        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", method = "lm", trans = trans) +
          ggplot2::labs(y = titley) 

        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())
        
        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect") 

      }) %>% bindCache(input$parameter,input$station, input$DatesSlide[1], input$DatesSlide[2], input$scaler1, input$taxgs)

      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {length(unique(selectedData()$StationName)) * 200})

      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData, "TrendLocation") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "TrendLocation") # Download figure

      # Parameter Definition
      output$ParamDef <- fParamDefServer(selectedData)

    })
    
    # Plot trends by taxa  -----------------------------------------------------------
    
    # Plot abundance spectra by species
    # observeEvent({input$NRSpts == 2}, {
    # 
    #   gg_out2 <- reactive({
    #     if (is.null(datHABg$StationCode)) {return(NULL)}
    #     trans <- dplyr::if_else(input$scaler1, "log10", "identity")
    #     
    #     if(input$parameter == 'PhytoAbundance_CellsL'){
    #       taxa <- input$taxgs
    #       titley <- bquote(" "*italic(.(taxa))*" (Cells L"^-1*")")
    #     } else if (input$parameter == 'Biovolume_um3L'){
    #       titley <- bquote(" "*italic(.(input$taxgs))*" ("*mu*"m"^-3*")")
    #     } else if (input$parameter == 'PhytoBiomassCarbon_pgL'){
    #       titley <- bquote(" "*italic(.(input$taxgs))*" (pgL"^-1*")")
    #     } else {
    #       titley <- bquote(" "*italic(.(input$taxgs))*" (No species)")
    #     }
    #     
    #     p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", method = "lm", trans = trans) +
    #       ggplot2::labs(y = titley) 
    #     
    #     p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", method = "loess", trans = trans) +
    #       ggplot2::theme(axis.title.y = ggplot2::element_blank())
    #     
    #     p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect") 
    # 
    #   }) %>% bindCache(input$parameter, input$site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
    # 
    #   output$timeseries2 <- renderPlot({
    #     gg_out2()
    #   })

      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData, "TrendTaxa") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2, "TrendTaxa") # Download figure

      # Parameter Definition
      output$ParamDef <- fParamDefServer(selectedData)

    })

    # Functional groups -------------------------------------------------------
    # observeEvent({input$NRSpts == 3}, {
    #   
    #   selectedDataFG <- reactive({
    #     req(input$site)
    #     shiny::validate(need(!is.na(input$site), "Error: Please select a station."))
    #     
    #     selectedDataFG <- pkg.env$NRSfgp %>%
    #       #dplyr::bind_rows(pkg.env$SOTSfgp) %>% 
    #       dplyr::filter(.data$StationName %in% input$site,
    #                     dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
    #       planktonr:::pr_reorder() %>% 
    #       droplevels()
    #     
    #   }) %>% bindCache(input$site, input$DatesSlide[1], input$DatesSlide[2])
    #   
    #   gg_out3 <- reactive({
    #     
    #     if (is.null(pkg.env$NRSfgp$StationCode)) {return(NULL)}
    #     
    #     scale <- dplyr::if_else(input$scaler3, "Proportion", "Actual")
    #     
    #     p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
    # 
    #     p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month") +
    #       ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "none")
    #     
    #     p1 + p2 + patchwork::plot_layout(widths = c(3,1))
    #     
    #   }) %>% bindCache(input$site, input$scaler3, input$DatesSlide[1], input$DatesSlide[2])
    #   
    #   output$timeseries3 <- renderPlot({
    #     gg_out3()
    #   }, height = function() {
    #     if(length(unique(selectedDataFG()$StationName)) < 2) 
    #     {300} else 
    #     {length(unique(selectedDataFG()$StationName)) * 200}})
    #     
    #   # Download -------------------------------------------------------
    #   output$downloadData3 <- fDownloadButtonServer(input, selectedDataFG, "FuncGroup") # Download csv of data
    #   output$downloadPlot3 <- fDownloadPlotServer(input, gg_id = gg_out3, "FuncGroup") # Download figure
    #   
    # })
  #})
}
