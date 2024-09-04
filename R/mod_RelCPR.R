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
      fRelationSidebar(id = id, tabsetPanel_id = "RelCPR", dat1 = pkg.env$datCPRz, dat2 = pkg.env$datCPRp, 
                       dat3 = pkg.env$datNRSm, dat4 = pkg.env$PolCPR, dat5 = pkg.env$PolCPR), 
      fRelationPanel(id = id, tabsetPanel_id = "RelCPR")
    )
  )
}

#' RelCPR Server Functions
#'
#' @noRd 
mod_RelCPR_server <- function(id){
  moduleServer(id, function(input, output, session, RelCPR){
    
    daty <- reactive({
      
      if(input$groupy %in% 'Zooplankton'){
        dat <- pkg.env$datCPRz  %>%  
          dplyr::select(-c("Latitude", "Longitude")) 
      } else if (input$groupy %in% 'Phytoplankton'){
        dat <- pkg.env$datCPRp  %>%  
          dplyr::select(-c("Latitude", "Longitude")) 
      } else if (input$groupy %in% 'Physical'){
        dat <- pkg.env$PolCPR %>% 
          dplyr::filter(.data$Parameters %in% c("SST", "chl_oc3")) %>% 
          dplyr::select(-c("sd", "anomaly", "Survey", "means"))
      }
    }) %>% bindCache(input$groupy)
    
    observeEvent(daty(), {
      vars <- c("BiomassIndex_mgm3", "PhytoAbundance_Cellsm3", "SST")
      sv <- daty() %>% 
        dplyr::filter(.data$Parameters %in% vars) 
      sv <- unique(sv$Parameters)
      choicesy <- planktonr::pr_relabel(unique(daty()$Parameters), style = "simple", named = TRUE)
      shiny::updateSelectizeInput(session, 'py', choices = choicesy)
    })
    
    datx <- reactive({
      if(input$groupx %in% 'Zooplankton'){
        dat1 <- pkg.env$datCPRz  %>%  
          dplyr::select(-c("Latitude", "Longitude")) 
      } else if (input$groupx %in% 'Phytoplankton'){
        dat1 <- pkg.env$datCPRp  %>%  
          dplyr::select(-c("Latitude", "Longitude"))
      } else if (input$groupx %in% 'Physical'){
        dat1 <- pkg.env$PolCPR %>% 
          dplyr::filter(.data$Parameters %in% c("SST", "chl_oc3")) %>% 
          dplyr::select(-c("sd", "anomaly", "Survey", "means"))
      }       
    }) %>% bindCache(input$groupx)
    
    observeEvent(datx(), {
      vars <- c("BiomassIndex_mgm3", "PhytoAbundance_Cellsm3", "SST")
      sv <- datx() %>% 
        dplyr::filter(.data$Parameters %in% vars) 
      sv <- unique(sv$Parameters)
      choicesx <- planktonr::pr_relabel(unique(datx()$Parameters), style = "simple", named = TRUE)
      shiny::updateSelectizeInput(session, 'px', choices = choicesx)
    })
    
    selectedData <- reactive({
      
      y <- rlang::string(input$py)
      x <- rlang::string(input$px)
      vars <- c("BioRegion", "SampleTime_Local") 
      
      selectedData <- daty() %>% 
        dplyr::bind_rows(datx()) %>% 
        planktonr::pr_remove_outliers(2) %>% 
        dplyr::filter(.data$BioRegion %in% input$Site,
                      .data$Parameters %in% c(x, y)) %>%
        tidyr::pivot_wider(id_cols = dplyr::any_of(vars),
                           names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
        tidyr::drop_na() %>% 
        planktonr::pr_reorder()
      
    }) %>% bindCache(input$py, input$px, input$Site, input$smoother)
    
    # Parameter Definition
    output$ParamDefy <-   shiny::renderText({
      paste("<h6><strong>", planktonr::pr_relabel(input$py, style = "plotly"), ":</strong> ",
            pkg.env$ParamDef %>% dplyr::filter(.data$Parameter == input$py) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
    })
    output$ParamDefx <-   shiny::renderText({
      paste("<h6><strong>", planktonr::pr_relabel(input$px, style = "plotly"), ":</strong> ",
            pkg.env$ParamDef %>% dplyr::filter(.data$Parameter == input$px) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
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

          y <- rlang::string(input$py)
          x <- rlang::string(input$px)

      planktonr::pr_plot_scatter(selectedData(), x, y, trend)

      }) %>% bindCache(input$py, input$px, input$Site, input$smoother)
      
      output$scatter1 <- renderPlot({
        gg_out1()
      }, height = 300)
      
      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Scatter") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Scatter") # Download figure
      
    })
    
    ## scatter plot
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$RelCPR == 2}, {
      
      gg_out2 <- reactive({
        
      y <- rlang::string(input$py)
        
      planktonr::pr_plot_box(selectedData(), y)
        
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
