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
      fRelationSidebar(id = id, tabsetPanel_id = "relNRS", dat1 = pkg.env$datNRSz, dat2 = pkg.env$datNRSw, dat3 = pkg.env$datNRSp,
                       dat4 = pkg.env$datNRSm, dat5 = ctd), #TODO pkg.env$
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
      
      if(input$group %in% 'Zooplankton'){
        dat <- pkg.env$datNRSz
        dat1 <- pkg.env$datNRSw
        y <- rlang::string(input$p1)
        x <- rlang::string(input$p2)
      } else if (input$group %in% 'Phytoplankton'){
        dat <- pkg.env$datNRSp
        dat1 <- pkg.env$datNRSw
        y <- rlang::string(input$p3)
        x <- rlang::string(input$p2)
      } else if (input$group %in% 'Microbes - NRS'){
        dat <- pkg.env$datNRSm %>% 
          dplyr::select(-c("TripCode_depth")) %>%
          dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10,0)*10) 
        dat1 <- ctd ##TODO pkg.env$
        y <- rlang::string(input$p5)
        x <- rlang::string(input$p6)
      } 
      
      vars <- c("StationName", "StationCode", "SampleTime_Local", "SampleDepth_m") # only microbes has depth data
      
      selectedData <- dat %>%  
        dplyr::bind_rows(dat1) %>%
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% c(x, y)) %>%
        tidyr::pivot_wider(id_cols = dplyr::any_of(vars),
                           names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
        tidyr::drop_na()
      
    }) %>% bindCache(input$Site, input$p2, input$p1, input$p3, input$p5, input$p6, input$group)
    
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
    
      if(input$group %in% 'Zooplankton'){
        y <- rlang::string(input$p1)
        x <- rlang::string(input$p2)
      } else if (input$group %in% 'Phytoplankton'){
        y <- rlang::string(input$p3)
        x <- rlang::string(input$p2)
      } else if (input$group %in% c('Microbes - NRS')){
        y <- rlang::string(input$p5)
        x <- rlang::string(input$p6)
      } 
      
      planktonr::pr_plot_scatter(selectedData(), x, y, trend)

    }) %>% bindCache(input$p1, input$p2, input$p3, input$p5, input$Site, input$group, input$smoother)

    output$scatter1 <- renderPlot({
      gg_out1()
    }, height = function() {
      if(input$group != 'Microbes - NRS')
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
        
        if(input$group %in% 'Zooplankton'){
          y <- rlang::string(input$p1)
        } else if (input$group %in% 'Phytoplankton'){
          y <- rlang::string(input$p3)
        } else if (input$group %in% c('Microbes - NRS')){
          y <- rlang::string(input$p5)
        } 
        
        planktonr::pr_plot_box(selectedData(), y)
        
      }) %>% bindCache(input$p1, input$p3, input$p5, input$Site, input$group)
      
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
