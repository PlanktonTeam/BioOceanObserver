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
      fRelationSidebar(id = id, tabsetPanel_id = "RelCPR", dat1 = datCPRz, dat2 = pkg.env$datCPRp, dat4 = pkg.env$PolCPR, 
                       dat3 = pkg.env$datNRSm), #TODO pkg.env$
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
      
      if(input$groupy %in% 'Zooplankton'){
        dat <- pkg.env$datCPRz %>%  
          dplyr::select(-c("Latitude", "Longitude")) 
        y <- rlang::string(input$pzy)
        # Parameter Definition
        output$ParamDefzy <-   shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$pzy, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$pzy) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        })
      } else if (input$groupy %in% 'Phytoplankton'){
        dat <- pkg.env$datCPRp %>%  
          dplyr::select(-c("Latitude", "Longitude")) 
        y <- rlang::string(input$ppy)
        # Parameter Definition
        output$ParamDefpy <-   shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$ppy, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$ppy) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        })
      } else if (input$groupy %in% 'Physical'){
        dat <- pkg.env$PolCPR %>% 
          dplyr::filter(.data$Parameters %in% c("SST", "chl_oc3")) %>% 
          dplyr::select(-c("sd", "anomaly", "Survey", "means"))
        y <- rlang::string(input$ppyy)
        # Parameter Definition
        output$ParamDefpyy <-   shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$ppyy, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$ppyy) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        })
      } 
      
      if(input$groupx %in% 'Zooplankton'){
        dat1 <- pkg.env$datCPRz %>%  
          dplyr::select(-c("Latitude", "Longitude")) 
        x <- rlang::string(input$pzx)
        # Parameter Definition
        output$ParamDefzx <-   shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$pzy, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$pzy) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        })
      } else if (input$groupx %in% 'Phytoplankton'){
        dat1 <- pkg.env$datCPRp %>%  
          dplyr::select(-c("Latitude", "Longitude")) 
        x <- rlang::string(input$ppx)
        # Parameter Definition
        output$ParamDefpx <-   shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$ppy, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$ppy) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        })
      } else if (input$groupx %in% 'Physical'){
        dat1 <- pkg.env$PolCPR %>% 
          dplyr::filter(.data$Parameters %in% c("SST", "chl_oc3")) %>% 
          dplyr::select(-c("sd", "anomaly", "Survey", "means"))
        x <- rlang::string(input$ppyx)
        # Parameter Definition
        output$ParamDefpyx <-   shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$ppyx, style = "plotly"), ":</strong> ",
                ParamDef %>% dplyr::filter(Parameter == input$ppyx) %>% dplyr::pull("Definition"), ".</h6>", sep = "") #TODO pkg.env$
        })
      } 
      
      vars <- c("BioRegion", "SampleTime_Local") 
      
      selectedData <- dat %>% 
        dplyr::bind_rows(dat1) %>% 
        planktonr::pr_remove_outliers(2) %>% 
        dplyr::filter(.data$BioRegion %in% input$Site,
                      .data$Parameters %in% c(x, y)) %>%
        tidyr::pivot_wider(id_cols = dplyr::any_of(vars),
                           names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
        tidyr::drop_na() %>% 
        planktonr::pr_reorder()
      
    }) %>% bindCache(input$pzy, input$ppy, input$ppyy, input$pzx, input$ppx, input$ppyx, input$Site, input$groupy, input$groupx, input$smoother)
    
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

        if(input$groupy %in% 'Zooplankton'){
          y <- rlang::string(input$pzy)
        } else if (input$groupy %in% 'Phytoplankton'){
          y <- rlang::string(input$ppy)
        } else if (input$groupy %in% 'Physical'){
          y <- rlang::string(input$ppyy)
        } 

        if(input$groupx %in% 'Zooplankton'){
          x <- rlang::string(input$pzx)
        } else if (input$groupx %in% 'Phytoplankton'){
          x <- rlang::string(input$ppx)
        } else if (input$groupx %in% 'Physical'){
          x <- rlang::string(input$ppyx)
        } 

      planktonr::pr_plot_scatter(selectedData(), x, y, trend)

      }) %>% bindCache(input$pzy, input$ppy, input$ppyy, input$pzx, input$ppx, input$ppyx, input$Site, input$groupy, input$groupx, input$smoother)
      
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
        
        if(input$groupy %in% 'Zooplankton'){
          y <- rlang::string(input$pzy)
        } else if (input$groupy %in% 'Phytoplankton'){
          y <- rlang::string(input$ppy)
        } else if (input$groupy %in% 'Physical'){
          y <- rlang::string(input$ppyy)
        }
        
        planktonr::pr_plot_box(selectedData(), y)
        
      }) %>% bindCache(input$pzy, input$ppy, input$ppyy, input$Site)
      
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
