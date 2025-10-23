#' RelCS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_RelCS_ui <- function(id){
  nsRelCS <- NS(id)
  
  tagList(
    sidebarLayout(
      fRelationSidebar(id = id, tabsetPanel_id = "RelCS", dat1 = pkg.env$datCSm, dat2 = datNRSm, dat3 = pkg.env$datNRSp,
                       dat4 = pkg.env$CSChem, dat5 = pkg.env$CSChem), 
      fRelationPanel(id = id, tabsetPanel_id = "RelCS")
    )
  )
}

#' RelCS Server Functions
#'
#' @noRd 
mod_RelCS_server <- function(id){
  moduleServer(id, function(input, output, session, RelCS){
    
    # Sidebar ----------------------------------------------------------

    daty <- reactive({
      if(input$all == TRUE){
        dat <- pkg.env$datCSm 
      } else {
        dat <- pkg.env$datCSm %>% 
          dplyr::filter(grepl("Temperature_Index_KD|Abund|gene|ASV", .data$Parameters))  
        }
    }) %>% bindCache(input$all)
    
    observeEvent(daty(), {
      choicesy <- planktonr::pr_relabel(unique(daty()$Parameters), style = "simple", named = TRUE)
      shiny::updateSelectizeInput(session, 'py', choices = choicesy, selected = 'Bacterial_Temperature_Index_KD')
    })
    
    datx <- reactive({
      dat1 <- pkg.env$CSChem         
    }) %>% bindCache(input$groupx)
    
    observeEvent(datx(), {
      choicesx <- planktonr::pr_relabel(unique(datx()$Parameters), style = "simple", named = TRUE)
      shiny::updateSelectizeInput(session, 'px', choices = choicesx, selected = 'Temperature_degC')
    })
    
    selectedData <- reactive({
      
      y <- rlang::string(input$py)
      x <- rlang::string(input$px)
      vars <- c("StationName", "StationCode", "SampleTime_Local", "SampleDepth_m", "State") # only microbes has depth data
      
      selectedData <- daty() %>%  
        dplyr::bind_rows(datx()) %>% 
        dplyr::filter(.data$State %in% input$site,
                      .data$Parameters %in% c(x, y)) %>%
        planktonr::pr_remove_outliers(2) %>% 
        tidyr::pivot_wider(id_cols = dplyr::any_of(vars),
                           names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
        tidyr::drop_na() 
      
    }) %>% bindCache(input$site, input$py, input$px)
    
    # Parameter Definition
    output$ParamDefy <-   shiny::renderText({
      paste("<p><strong>", planktonr::pr_relabel(input$py, style = "plotly"), ":</strong> ",
            pkg.env$ParamDef %>% 
              dplyr::filter(.data$Parameter == input$py) %>% 
              dplyr::pull("Definition"), ".</p>", sep = "")
    })
    # Parameter Definition
    output$ParamDefx <- shiny::renderText({
      paste("<p><strong>", planktonr::pr_relabel(input$px, style = "plotly"), ":</strong> ",
            pkg.env$ParamDef %>% 
              dplyr::filter(.data$Parameter == input$px) %>%
              dplyr::pull("Definition"), ".</p>", sep = "")
    })
    
    # Sidebar Map
    output$plotmap <- plotly::renderPlotly({
      p1 <- planktonr::pr_plot_NRSmap(unique(selectedData()$StationCode), Survey = "Coastal")
      fPlotlyMap(p1, tooltip = "colour")
    })  # No cache - allows responsive resizing
    
    # Add text information 
    output$PlotExp1 <- shiny::renderText({
      if(rlang::string(input$py) %in% colnames(selectedData())){
        "A scatter plot of selected indices against oceanographic parameters measured from the NRS around Australia"
      } else {  
        paste("A scatter plot of selected indices against oceanographic parameters measured from the NRS around Australia <br> <br> <b>NOTE: Not enough data for plot</b>")
      }
    })  %>% bindCache(input$py)
    
    # Add text information 
    output$PlotExp2 <- shiny::renderText({
      if(rlang::string(input$py) %in% colnames(selectedData())){
        "A box plot of selected indices showing range of each parameter at the NRS around Australia"
      } else {
         paste("A box plot of selected indices showing range of each parameter at the NRS around Australia <br> <br> <b>NOTE: Not enough data for plot</b>")
        }
    })  %>% bindCache(input$py)
    
    ## scatter plot
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$RelCS == 1}, {
      
      gg_out1 <- reactive({
        
         trend <- input$smoother
         y <- rlang::string(input$py)
         x <- rlang::string(input$px)

         if(y %in% colnames(selectedData()) & x %in% colnames(selectedData())){
           planktonr::pr_plot_scatter(selectedData(), x, y, Trend = trend)
         } else {
           ggplot2::ggplot + ggplot2::geom_blank()
         }
         }) %>% bindCache(input$py, input$px, input$site, input$smoother)
      
      output$scatter1 <- renderPlot({
           gg_out1()
      }, height = function() {length(unique(selectedData()$SampleDepth_m)) * 200})
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Scatter") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Scatter") # Download figure
      
    })
    
    ## scatter plot
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$RelCS == 2}, {
      
      gg_out2 <- reactive({
        
        y <- rlang::string(input$py)

        if(y %in% colnames(selectedData())){
          planktonr::pr_plot_box(selectedData(), y)
        } else {
          ggplot2::ggplot + ggplot2::geom_blank()
        }
        
      }) %>% bindCache(input$py, input$site)
      
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
