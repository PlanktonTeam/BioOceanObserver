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
      fRelationSidebar(id = id, tabsetPanel_id = "RelCS", dat1 = datCSm, dat2 = CSChem, dat3 = pkg.env$datNRSp,
                       dat4 = pkg.env$datNRSm, dat5 = ctd), #TODO pkg.env$
      fRelationPanel(id = id, tabsetPanel_id = "RelCS")
    )
  )
}

#' RelCS Server Functions
#'
#' @noRd 
mod_RelCS_server <- function(id){
  moduleServer(id, function(input, output, session, RelCS){
    
    selectedData <- reactive({
      
      y <- rlang::string(input$p1)
      x <- rlang::string(input$p2)
      vars <- c("StationName", "StationCode", "SampleTime_Local", "SampleDepth_m") # only microbes has depth data
      
      selectedData <- datCSm %>%  #TODO pkg.env$
        dplyr::bind_rows(CSChem) %>% #TODO pkg.env$
        dplyr::filter(.data$State %in% input$Site,
                      .data$Parameters %in% c(x, y)) %>%
        tidyr::pivot_wider(id_cols = dplyr::any_of(vars),
                           names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
        tidyr::drop_na()
      
    }) %>% bindCache(input$Site, input$p2, input$p1)
    
    # Parameter Definition
    output$ParamDefm1 <-   shiny::renderText({
      paste("<h6><strong>", planktonr::pr_relabel(input$p1, style = "plotly"), ":</strong> ",
            pkg.env$ParamDef %>% dplyr::filter(Parameter == input$p1) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
    })
    # Parameter Definition
    output$ParamDefm2 <- shiny::renderText({
      paste("<h6><strong>", planktonr::pr_relabel(input$p2, style = "plotly"), ":</strong> ",
            pkg.env$ParamDef %>% dplyr::filter(Parameter == input$p2) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
    })
    
    # Sidebar Map
    output$plotmap <- renderPlot({
      planktonr::pr_plot_NRSmap(selectedData(), Survey = 'Coastal')
    }, bg = "transparent") %>% bindCache(input$Site)
    
    # Add text information 
    output$PlotExp1 <- shiny::renderText({
      "A scatter plot of selected indices against oceanographic parameters measured from the NRS around Australia"
    }) 
    # Add text information 
    output$PlotExp2 <- shiny::renderText({
      "A box plot of selected indices showing range of each parameter at the NRS around Australia"
    }) 
    
    ## scatter plot
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$RelCS == 1}, {
      
      gg_out1 <- reactive({
        
        trend <- input$smoother
        y <- rlang::string(input$p1)
        x <- rlang::string(input$p2)
        
        planktonr::pr_plot_scatter(selectedData(), x, y, trend)
        
      }) %>% bindCache(input$p1, input$p2, input$p3, input$p5, input$Site, input$group, input$smoother)
      
      output$scatter1 <- renderPlot({
        gg_out1()
      }, height = 300)
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData(), "Scatter") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Scatter") # Download figure
      
    })
    
    ## scatter plot
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$RelCS == 2}, {
      
      gg_out2 <- reactive({
        
        y <- rlang::string(input$p1)

        planktonr::pr_plot_box(selectedData(), y)
        
      }) %>% bindCache(input$p1, input$Site)
      
      output$box2 <- renderPlot({
        gg_out2()
      })
      
      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData(), "Scatter") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2(), "Scatter") # Download figure
      
    })
  }
  )
  
}