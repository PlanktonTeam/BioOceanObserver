#' RelNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_RelNRS_ui <- function(id){
  nsRelNRS <- NS(id)

  tagList(
    sidebarLayout(
      fRelationSidebar(id = id, tabsetPanel_id = "relNRS", dat1 = pkg.env$datNRSz, dat2 = pkg.env$datNRSp, 
                       dat3 = pkg.env$datNRSm, dat4 = pkg.env$ctd, dat5 = pkg.env$Nuts), 
      fRelationPanel(id = id, tabsetPanel_id = "relNRS")
    )
  )
}

#' RelNRS Server Functions
#'
#' @noRd 
mod_RelNRS_server <- function(id){
  moduleServer(id, function(input, output, session, relNRS){
    
    daty <- reactive({
      
      if(input$groupy %in% 'Zooplankton'){
        dat <- pkg.env$datNRSz %>%
          dplyr::mutate(SampleDepth_m = 10)
        } else if (input$groupy %in% 'Phytoplankton'){
        dat <- pkg.env$datNRSp %>%
          dplyr::mutate(SampleDepth_m = 10)
        } else if (input$groupy %in% 'Microbes - NRS' & input$all == TRUE){
        dat <- pkg.env$datNRSm %>% 
          dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10,0)*10) 
        } else if (input$groupy %in% 'Microbes - NRS' & input$all == FALSE){
          dat <- pkg.env$datNRSm %>% 
            dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10,0)*10) %>% 
            dplyr::filter(grepl("Temperature_Index_KD|Abund|gene|ASV", .data$Parameters))
        } else if (input$groupy %in% 'Physical'){
        dat <- pkg.env$ctd  
        } else if (input$groupy %in% 'Chemical'){
          dat <- pkg.env$Nuts  %>% dplyr::group_by(.data$StationName, .data$StationCode, .data$SampleTime_Local, .data$Parameters) %>% 
            dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                             .groups = 'drop') %>%
            dplyr::mutate(SampleDepth_m = 10)
        }
      
      dat <- dat %>% 
        dplyr::mutate(SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'month')) 
      
    }) %>% bindCache(input$groupy, input$all)
    
    observeEvent(daty(), {
      vars <- c("Biomass_mgm3", "PhytoAbundance_CellsL", "Bacterial_Temperature_Index_KD", "CTD_Temperature_degC", "Silicate_umolL")
      sv <- daty() %>% 
        dplyr::filter(.data$Parameters %in% vars) 
      sv <- unique(sv$Parameters)
      choicesy <- planktonr::pr_relabel(unique(daty()$Parameters), style = "simple", named = TRUE)
      shiny::updateSelectizeInput(session, 'py', choices = choicesy, selected = sv)
    })
      
    datx <- reactive({
      if(input$groupx %in% 'Zooplankton'){
        dat1 <- pkg.env$datNRSz %>%
          dplyr::mutate(SampleDepth_m = 10)
        } else if (input$groupx %in% 'Phytoplankton'){
          dat1 <- pkg.env$datNRSp %>%
            dplyr::mutate(SampleDepth_m = 10)
        } else if (input$groupx %in% 'Microbes - NRS' & input$all == TRUE){
          dat1 <- pkg.env$datNRSm %>% 
            dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10,0)*10) 
        } else if (input$groupx %in% 'Microbes - NRS' & input$all == FALSE){
          dat1 <- pkg.env$datNRSm %>% 
            dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10,0)*10) %>% 
            dplyr::filter(grepl("Temperature_Index_KD|Abund|gene|ASV", .data$Parameters))
            } else if (input$groupx %in% 'Physical'){
              dat1 <- pkg.env$ctd  
            } else if (input$groupx %in% 'Chemical'){
              dat1 <- pkg.env$Nuts %>% dplyr::group_by(.data$StationName, .data$StationCode, .data$SampleTime_Local, .data$Parameters) %>% 
                dplyr::summarise(Values = mean(.data$Values, na.rm = TRUE),
                                 .groups = 'drop') %>%
                dplyr::mutate(SampleDepth_m = 10)
            }       
      dat1 <- dat1 %>% 
        dplyr::mutate(SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = 'month')) 
    
      }) %>% bindCache(input$groupx)

    observeEvent(datx(), {
      vars <- c("Biomass_mgm3", "PhytoAbundance_CellsL", "Bacterial_Temperature_Index_KD", "CTD_Temperature_degC", "Silicate_umolL")
      sv <- datx() %>% 
        dplyr::filter(.data$Parameters %in% vars) 
      sv <- unique(sv$Parameters)
      choicesx <- planktonr::pr_relabel(unique(datx()$Parameters), style = "simple", named = TRUE)
      shiny::updateSelectizeInput(session, 'px', choices = choicesx, selected = sv)
    })
    
    selectedData <- reactive({
      y <- rlang::string(input$py)
      x <- rlang::string(input$px)
      vars <- c("StationName", "StationCode", "SampleTime_Local", "SampleDepth_m") # only microbes has depth data
      
      selectedData <- daty() %>%   
        dplyr::bind_rows(datx()) %>%
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% c(x, y)) %>%
        planktonr::pr_remove_outliers(2) %>% 
        tidyr::pivot_wider(id_cols = dplyr::any_of(vars),
                           names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
        tidyr::drop_na() %>% 
        planktonr::pr_reorder()
      
    }) %>% bindCache(input$Site, input$py, input$px)
    
  # Parameter Definition
  output$ParamDefy <-   shiny::renderText({
    paste("<h6><strong>", planktonr::pr_relabel(input$py, style = "plotly"), ":</strong> ",
          pkg.env$ParamDef %>% 
            dplyr::filter(.data$Parameter == input$py) %>% 
            dplyr::pull("Definition"), ".</h6>", sep = "")
    })
  output$ParamDefx <-   shiny::renderText({
    paste("<h6><strong>", planktonr::pr_relabel(input$px, style = "plotly"), ":</strong> ",
          pkg.env$ParamDef %>% 
            dplyr::filter(.data$Parameter == input$px) %>% 
            dplyr::pull("Definition"), ".</h6>", sep = "")
    })  

    # Sidebar Map
    output$plotmap <- renderPlot({
      planktonr::pr_plot_NRSmap(unique(selectedData()$StationCode))
    }, bg = "transparent") %>% bindCache(input$Site)

    # Add text information 
    output$PlotExp1 <- shiny::renderText({
      if(rlang::string(input$py) %in% colnames(selectedData()) & rlang::string(input$px) %in% colnames(selectedData()) & length(unique(selectedData()$SampleDepth_m)) > 0){
        "A scatter plot of selected indices against oceanographic parameters measured from the NRS around Australia"}
      else{
        paste("A scatter plot of selected indices against oceanographic parameters measured from the NRS around Australia 
        <br> <br> <b>NOTE: Not enough data for plot</b>")
      }
    }) %>% bindCache(input$py, input$px, input$Site)
    # Add text information 
    output$PlotExp2 <- shiny::renderText({
      if(rlang::string(input$py) %in% colnames(selectedData())){
        "A box plot of selected indices showing range of each parameter at the NRS around Australia"}
      else{
        paste("A box plot of selected indices showing range of each parameter at the NRS around Australia 
              <br> <br> <b>NOTE: Not enough data for plot</b>")
      }
    }) %>% bindCache(input$py, input$Site)
    
    ## scatter plot
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$relNRS == 1}, {
      
      gg_out1 <- reactive({
      
          trend <- input$smoother
          y <- rlang::string(input$py)
          x <- rlang::string(input$px)

     if(y %in% colnames(selectedData()) & x %in% colnames(selectedData()) & length(unique(selectedData()$SampleDepth_m)) > 0){
          planktonr::pr_plot_scatter(selectedData(), x, y, trend)
        } else{
          ggplot2::ggplot + ggplot2::geom_blank()
          }

    }) %>% bindCache(input$Site, input$py, input$px, input$smoother)

    output$scatter1 <- renderPlot({
      gg_out1()
    }, height = function() {
      if(input$groupy != 'Microbes - NRS')
      {300} else if(length(unique(selectedData()$SampleDepth_m)) < 2) 
        {300}
      else {
        length(unique(selectedData()$SampleDepth_m)) * 200}
      })

    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Scatter") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Scatter") # Download figure

    })
    
    ## scatter plot
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$relNRS == 2}, {
      
      gg_out2 <- reactive({
          y <- rlang::string(input$py)

        if(y %in% colnames(selectedData())){
        planktonr::pr_plot_box(selectedData(), y)
        } else{
          ggplot2::ggplot + ggplot2::geom_blank()
        }
      }) %>% bindCache(input$py, input$Site)
      
      output$box2 <- renderPlot({
        gg_out2()
      })
      
      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData, "Scatter") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2, "Scatter") # Download figure
      
    })
  }
  )
}
