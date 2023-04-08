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
                       dat3 = pkg.env$datNRSm, dat4 = ctd), #TODO pkg.env$
      fRelationPanel(id = id, tabsetPanel_id = "relNRS")
    )
  )
}

#' RelNRS Server Functions
#'
#' @noRd 
mod_RelNRS_server <- function(id){
  moduleServer(id, function(input, output, session, relNRS){
    
    selectedData <- reactive({
      
      if(input$groupy %in% 'Zooplankton'){
        dat <- pkg.env$datNRSz %>%
          dplyr::mutate(SampleDepth_m = 10)
        y <- rlang::string(input$pzy)
        # Parameter Definition
        output$ParamDefzy <-   shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$pzy, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$pzy) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        })
        } else if (input$groupy %in% 'Phytoplankton'){
        dat <- pkg.env$datNRSp %>%
          dplyr::mutate(SampleDepth_m = 10)
        y <- rlang::string(input$ppy)
        # Parameter Definition
        output$ParamDefpy <-   shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$ppy, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$ppy) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        })
      } else if (input$groupy %in% 'Microbes - NRS'){
        dat <- pkg.env$datNRSm %>% 
          dplyr::select(-c("TripCode_depth")) %>%
          dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10,0)*10) 
        y <- rlang::string(input$pmny)
        # Parameter Definition
        output$ParamDefmny <- shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$pmny, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$pmny) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        }) 
      } else if (input$groupy %in% 'Physical'){
        dat <- ctd  #TODO pkg.env$
        y <- rlang::string(input$ppyy)
        # Parameter Definition
        output$ParamDefpyy <- shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$ppyy, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$ppyy) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        }) 
      } 
      
      if(input$groupx %in% 'Zooplankton'){
        dat1 <- pkg.env$datNRSz %>%
          dplyr::mutate(SampleDepth_m = 10)
        x <- rlang::string(input$pzx)
        # Parameter Definition
        output$ParamDefzx <-   shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$pzx, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$pzx) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        })
      } else if (input$groupx %in% 'Phytoplankton'){
        dat1 <- pkg.env$datNRSp %>%
          dplyr::mutate(SampleDepth_m = 10)
        x <- rlang::string(input$ppx)
        # Parameter Definition
        output$ParamDefpx <-   shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$ppx, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$ppx) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        })
      } else if (input$groupx %in% 'Microbes - NRS'){
        dat1 <- pkg.env$datNRSm %>% 
          dplyr::select(-c("TripCode_depth")) %>%
          dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10,0)*10) 
        x <- rlang::string(input$pmnx)
        # Parameter Definition
        output$ParamDefmnx <- shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$pmnx, style = "plotly"), ":</strong> ",
                pkg.env$ParamDef %>% dplyr::filter(Parameter == input$pmnx) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
        }) 
      } else if (input$groupx %in% 'Physical'){
        dat1 <- ctd  #TODO pkg.env$
        x <- rlang::string(input$ppyx)
        # Parameter Definition
        output$ParamDefpyx <- shiny::renderText({
          paste("<h6><strong>", planktonr::pr_relabel(input$ppyx, style = "plotly"), ":</strong> ",
                ParamDef %>% dplyr::filter(Parameter == input$ppyx) %>% dplyr::pull("Definition"), ".</h6>", sep = "") #TODO pkg.env$
        }) 
      } 
      
      vars <- c("StationName", "StationCode", "SampleTime_Local", "SampleDepth_m") # only microbes has depth data
      
      selectedData <- dat %>%  
        dplyr::bind_rows(dat1) %>%
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% c(x, y)) %>%
        planktonr::pr_remove_outliers(2) %>% 
        tidyr::pivot_wider(id_cols = dplyr::any_of(vars),
                           names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
        tidyr::drop_na() %>% 
        planktonr::pr_reorder()
      
    }) %>% bindCache(input$Site, input$pzy, input$pzx, input$ppy, input$ppx, input$pmny, input$pmnx, input$ppyy, input$ppyx, input$groupy, input$groupx)
    
    # Sidebar Map
    output$plotmap <- renderPlot({
      planktonr::pr_plot_NRSmap(selectedData())
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
    observeEvent({input$relNRS == 1}, {
      
      gg_out1 <- reactive({
      
    trend <- input$smoother
    
      if(input$groupy %in% 'Zooplankton'){
        y <- rlang::string(input$pzy)
      } else if (input$groupy %in% 'Phytoplankton'){
        y <- rlang::string(input$ppy)
      } else if (input$groupy %in% c('Microbes - NRS')){
        y <- rlang::string(input$pmny)
      } else if (input$groupy %in% c('Physical')){
        y <- rlang::string(input$ppyy)
      } 
    
    if(input$groupx %in% 'Zooplankton'){
      x <- rlang::string(input$pzx)
    } else if (input$groupx %in% 'Phytoplankton'){
      x <- rlang::string(input$ppx)
    } else if (input$groupx %in% c('Microbes - NRS')){
      x <- rlang::string(input$pmnx)
    } else if (input$groupx %in% c('Physical')){
      x <- rlang::string(input$ppyx)
    } 
    
      planktonr::pr_plot_scatter(selectedData(), x, y, trend)

    }) %>% bindCache(input$Site, input$pzy, input$pzx, input$ppy, input$ppx, input$pmny, input$pmnx, input$ppyy, input$ppyx, input$groupy, input$groupx, input$smoother)

    output$scatter1 <- renderPlot({
      gg_out1()
    }, height = function() {
      if(input$groupy != 'Microbes - NRS')
      {300} else
      {length(unique(selectedData()$SampleDepth_m)) * 200}})

    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData(), "Scatter") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Scatter") # Download figure

    })
    
    ## scatter plot
    # Plot Trends -------------------------------------------------------------
    observeEvent({input$relNRS == 2}, {
      
      gg_out2 <- reactive({
        
        if(input$groupy %in% 'Zooplankton'){
          y <- rlang::string(input$pzy)
        } else if (input$groupy %in% 'Phytoplankton'){
          y <- rlang::string(input$ppy)
        } else if (input$groupy %in% c('Microbes - NRS')){
          y <- rlang::string(input$pmny)
        } else if (input$groupy %in% c('Physical')){
          y <- rlang::string(input$ppyy)
        } 
        
        planktonr::pr_plot_box(selectedData(), y)
        
      }) %>% bindCache(input$pzy, input$ppy, input$pmny, input$ppyy, input$Site, input$groupy, input$groupx)
      
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
