#' RelCPR UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_RelCPR_ui <- function(id){
  nsRelCPR <- NS(id)
  
  tagList(
    sidebarLayout(
      fRelationSidebar(id = id, tabsetPanel_id = "RelCPR", dat1 = datCPRz, dat2 = pkg.env$PolCPR, dat3 = pkg.env$datCPRp, 
                       dat4 = pkg.env$datNRSm, dat5 = ctd), #TODO pkg.env$
      fRelationPanel(id = id, tabsetPanel_id = "RelCPR")
    )
  )
}

#' RelCPR Server Functions
#'
#' @noRd 
mod_RelCPR_server <- function(id){
  moduleServer(id, function(input, output, session, RelCPR){
    
    selectedData <- reactive({
      
      if(input$group %in% 'Zooplankton'){
        dat <- pkg.env$datCPRz
        y <- rlang::string(input$p1)
        x <- rlang::string(input$p2)
      } else if (input$group %in% 'Phytoplankton'){
        dat <- pkg.env$datCPRp
        y <- rlang::string(input$p3)
        x <- rlang::string(input$p2)
      } 
      
      dat1 <- pkg.env$PolCPR %>% 
        dplyr::filter(.data$Parameters %in% c("SST", "chl_oc3")) %>% 
        dplyr::select(-c("sd", "anomaly", "Survey", "means"))
      
      vars <- c("BioRegion", "SampleTime_Local") 
      
      selectedData <- dat %>%  
        dplyr::select(-c("Latitude", "Longitude")) %>% 
        dplyr::bind_rows(dat1) %>% 
        dplyr::filter(.data$BioRegion %in% input$Site,
                      .data$Parameters %in% c(x, y)) %>%
        tidyr::pivot_wider(id_cols = dplyr::any_of(vars),
                           names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
        tidyr::drop_na()
      
    }) %>% bindCache(input$Site, input$p2, input$p1, input$group)
    
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
      planktonr::pr_plot_CPRmap(selectedData())
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
    observeEvent({input$RelCPR == 1}, {
      
      gg_out1 <- reactive({
        
        trend <- input$smoother

        if(input$group %in% 'Zooplankton'){
          dat <- pkg.env$datCPRz
          y <- rlang::string(input$p1)
          x <- rlang::string(input$p2)
        } else if (input$group %in% 'Phytoplankton'){
          dat <- pkg.env$datCPRp
          y <- rlang::string(input$p3)
          x <- rlang::string(input$p2)
        } 
        
        planktonr::pr_plot_scatter(selectedData(), x, y, trend)

      }) %>% bindCache(input$p1, input$p2, input$Site, input$group, input$smoother)
      
      output$scatter1 <- renderPlot({
        gg_out1()
      }, height = 300)
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData(), "Scatter") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Scatter") # Download figure
      
    })
    
    ## scatter plot
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$RelCPR == 2}, {
      
      gg_out2 <- reactive({
        
        if(input$group %in% 'Zooplankton'){
          y <- rlang::string(input$p1)
        } else if (input$group %in% 'Phytoplankton'){
          y <- rlang::string(input$p3)
        }
        
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
