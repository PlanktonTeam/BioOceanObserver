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
    observeEvent(input$state, {
      
      # Filter sites based on the selected state
      filtered_sites <- unique(sort((pkg.env$datHABTrip %>% dplyr::filter(.data$State %in% input$state))$StationName))
      
      # Update the site_input choices
      shiny::updateSelectInput(session, "station",  choices = filtered_sites, selected = filtered_sites[1])
    })

    taxa <- reactive({
      if(input$tax == "genus"){
        taxa <- pkg.env$datHABg %>%
          dplyr::rename(TaxonName = .data$genus)
      } else {
        taxa <- pkg.env$datHABs 
      }
    }) %>% bindCache(input$tax)
    
    #Sidebar Map - Initial render
    output$plotmap <- leaflet::renderLeaflet({
      fLeafletMap(character(0), Survey = "HAB", Type = "Phytoplankton")
    })
    
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
    
    output$PlotExp2 <- renderText({
      "A plot of selected phytoplankton Parameters from the Coastal Phytoplankton collection, as a time series and a monthly climatology by station.
      This data comes from a count of selected taxa, it is not a full community count so indices are limited to those appropriate."
    })
    
    param <- reactive({
      param <- taxa() %>% dplyr::filter(Parameters %in% input$parameter)
    }) %>% shiny::bindCache(input$parameter)


    # Plot Trends by location -------------------------------------------------------------
    observeEvent({input$phabts == 1}, {

      observe({
        
        dat <- taxa() %>% 
          dplyr::filter(.data$StationName %in% input$station,
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2]))
        
        taxa <- unique(sort(dat$TaxonName))
        params <- planktonr:::pr_relabel(unique(sort(dat$Parameters)), style = "simple", named = TRUE)
        
        shiny::updateSelectInput(session, 'taxgs', choices = taxa, selected = taxa[1])
        shiny::updateSelectInput(session, 'parameter', choices = params, selected = params[1])
        
      }) %>%  shiny::bindEvent(input$tax, input$station, input$DatesSlide[1], input$DatesSlide[2])
      
      selectedData <- reactive({ 
        
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
        
        df <- taxa() %>%
          dplyr::filter(.data$TaxonName %in% input$taxgs[1], # need to specify [1] as two species are allowed in tab 2 for swithcing between tabs
                        .data$StationName %in% input$station,
                        .data$Parameters %in% input$parameter,
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>% 
          dplyr::select(.data$SampleTime_Local, .data$StationName, .data$TaxonName, .data$Parameters, .data$Values) 
        
        ## Need to add in zeros
        events <- taxa() %>% 
          dplyr::filter(.data$StationName %in% input$station,
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>% 
          dplyr::select(-c(.data$Parameters, .data$Values, .data$TaxonName)) %>% 
          dplyr::distinct()
        
        selectedData <- events %>% 
          dplyr::left_join(df, by = c("SampleTime_Local", "StationName")) %>% 
          dplyr::mutate(Parameters = input$parameter,
                        TaxonName = input$taxgs[1], 
                        Values = ifelse(is.na(Values), 0, Values))
        
      }) %>% bindCache(input$parameter, input$station, input$DatesSlide[1], input$DatesSlide[2], input$tax, input$taxgs)

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

      }) %>% bindCache(input$parameter, input$station, input$DatesSlide[1], input$DatesSlide[2], input$scaler1, input$tax, input$taxgs)

      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {length(unique(selectedData()$StationName)) * 200})

      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData, "TrendLocation") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "TrendLocation") # Download figure

      # Parameter Definition
      output$ParamDef <- fParamDefServer(param)

    })
    
    # Plot trends by taxa  -----------------------------------------------------------
    
    observeEvent({input$phabts == 2}, {
      
      observe({
        
        dat <- taxa() %>% 
          dplyr::filter(.data$StationName %in% input$station,
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2]))
        
        taxa <- unique(sort(dat$TaxonName))
        params <- planktonr:::pr_relabel(unique(sort(dat$Parameters)), style = "simple", named = TRUE)
        
        shiny::updateSelectInput(session, 'taxgs', choices = taxa, selected = taxa[1])
        shiny::updateSelectInput(session, 'parameter', choices = params, selected = params[1])
        
      }) %>%  shiny::bindEvent(input$tax, input$station, input$DatesSlide[1], input$DatesSlide[2])
      
      selectedData2 <- reactive({ 
      
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
      
      df <- taxa() %>%
        dplyr::filter(.data$TaxonName %in% input$taxgs, 
                      .data$StationName %in% input$station[1], # need to specify [1] as two species are allowed in tab 2 for swithcing between tabs
                      .data$Parameters %in% input$parameter,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>% 
        dplyr::select(.data$SampleTime_Local, .data$StationName, .data$TaxonName, .data$Parameters, .data$Values) 
      
      ## Need to add in zeros
      events <- taxa() %>% 
        dplyr::filter(.data$StationName %in% input$station[1],
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>% 
        dplyr::select(-c(.data$Parameters, .data$Values, .data$TaxonName)) %>% 
        dplyr::distinct() %>%
        tidyr::expand_grid(TaxonName = c(input$taxgs))
      
      selectedData2 <- events %>% 
        dplyr::left_join(df, by = c("SampleTime_Local", "StationName", "TaxonName")) %>% 
        dplyr::mutate(Parameters = input$parameter,
                      Values = ifelse(is.na(Values), 0, Values)) %>% 
        dplyr::mutate(Taxon = .data$StationName,
                      StationName = .data$TaxonName,
                      StationCode = .data$TaxonName, 
                      TaxonName = .data$Taxon) %>% 
        planktonr::planktonr_dat(Type = 'Phytoplankton', Survey = 'HAB')
      
    }) %>% bindCache(input$parameter, input$station, input$DatesSlide[1], input$DatesSlide[2], input$tax, input$taxgs)

    gg_out2 <- reactive({
        if (is.null(datHABg$StationCode)) {return(NULL)}
        trans <- dplyr::if_else(input$scaler1, "log10", "identity")

        p1 <- planktonr::pr_plot_Trends(selectedData2(), Trend = "Raw", method = "lm", trans = trans) 

        p2 <- planktonr::pr_plot_Trends(selectedData2(), Trend = "Month", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())

        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect") +
          patchwork::plot_annotation(title = input$station,
                                     theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 20, hjust = 0.5))
                                     )

      }) %>% bindCache(input$parameter, input$station, input$DatesSlide[1], input$DatesSlide[2], input$scaler1, input$tax, input$taxgs)

      output$timeseries2 <- renderPlot({
        gg_out2()
      }, height = function() {length(unique(selectedData2()$StationName)) * 200})

      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData2, "TrendTaxa") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2, "TrendTaxa") # Download figure

      # Parameter Definition
      output$ParamDef <- fParamDefServer(param)

     })
  })
}
