#' MicroTsCS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_MicroTsCS_ui <- function(id){
  nsMicroTsCS <- NS(id)
  tagList(
    sidebarLayout(
      fPlanktonSidebar(id = id, tabsetPanel_id = "CSmts", dat = pkg.env$datCSm),
      fPLanktonPanel(id = id,  tabsetPanel_id = "CSmts"),

    )
  )
}


#' MicroTsCS Server Functions
#'
#' @noRd
mod_MicroTsCS_server <- function(id){
  moduleServer(id, function(input, output, session, CSmts){

    # Sidebar ----------------------------------------------------------
    observeEvent(input$all, {
      if(input$all == TRUE){
        params <- planktonr::pr_relabel(unique(pkg.env$datCSm$Parameters), style = "simple")
      } else {
        params <- planktonr::pr_relabel(unique((pkg.env$datCSm %>% 
                                                  dplyr::filter(grepl("Temperature_Index_KD|Abund|gene|ASV", .data$Parameters)))$Parameters), style = "simple")
      }
      shiny::updateSelectInput(session, 'parameterm', choices = params, selected = "Bacterial_Temperature_Index_KD")
    })
    
    
    selectedData <- reactive({

      selectedData <- pkg.env$datCSm %>%
        dplyr::filter(.data$State %in% input$Site,
                      .data$Parameters %in% input$parameterm,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels() %>%
        dplyr::mutate(name = as.factor(.data$Parameters)) %>% 
        dplyr::arrange(.data$State) %>% 
        tidyr::drop_na()

    }) %>% bindCache(input$parameterm, input$Site, input$DatesSlide[1], input$DatesSlide[2])

    shiny::exportTestValues(
      MicroTsC = {ncol(selectedData())},
      MicroTsCRows = {nrow(selectedData()) > 0},
      MicroTsCYearisNumeric = {class(selectedData()$Year_Local)},
      MicroTsCMonthisNumeric = {class(selectedData()$Month_Local)},
      MicroTsCDateisDate = {class(selectedData()$SampleTime_Local)},
      MicroTsCStationisChr = {class(selectedData()$StationName)},
      MicroTsCCodeisChr = {class(selectedData()$StationCode)},
      MicroTsCParametersisChr = {class(selectedData()$Parameters)},
      MicroTsCValuesisNumeric = {class(selectedData()$Values)}
    )

    # Sidebar Map
    output$plotmap <- renderPlot({
      planktonr::pr_plot_NRSmap(selectedData(), Survey = 'Coastal')
    }, bg = "transparent") %>% bindCache(input$Site)

    # Add text information
    output$PlotExp1 <- renderText({
      if (length(selectedData()$Parameters)>0){
        "A plot of selected microbial indices from the NRS around Australia, as a time series and a monthly climatology by station averaged across all depths."
      } else {
        paste("A plot of selected microbial indices from the NRS around Australia, as a time series and a monthly climatology by station averaged across all depths.<br> <br> <b>NOTE: Not enough data for plot</b>")
        }
    }) %>% bindCache(input$parameterm)
    output$PlotExp2 <- renderText({
      if (length(selectedData()$Parameters)>0){
        "A plot of selected indices from the NRS around Australia, as a time series, a monthly climatology and an annual mean averaged across all depths."
      } else {
        paste("A plot of selected indices from the NRS around Australia, as a time series, a monthly climatology and an annual mean averaged across all depths.<br> <br> <b>NOTE: Not enough data for plot</b>")
      }
      }) %>% bindCache(input$parameterm)
    output$PlotExp3 <- renderText({
      if (length(selectedData()$Parameters)>0){
        "A plot of microbial indices from the NRS around Australia, as a time series and a monthly climatology averaged to the nearest 10m."
      } else {
        paste("A plot of microbial indices from the NRS around Australia, as a time series and a monthly climatology averaged to the nearest 10m.<br> <br> <b>NOTE: Not enough data for plot</b>")
      }
      }) %>% bindCache(input$parameterm)


    # Plot Trends -------------------------------------------------------------

    observeEvent({input$CSmts == 1}, {

      gg_out1 <- reactive({

        if (length(selectedData()$Parameters) > 0){
          if(input$scaler1){
            trans <- 'log10'
            } else {
              trans <- 'identity' 
              }

        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", Survey = "Coastal", method = "lm", trans = trans)
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", Survey = "NRS", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())

        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")

        } else {
          ggplot2::ggplot + ggplot2::geom_blank()
        }
        
      }) %>% bindCache(input$parameterm, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)

      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {
        if(length(unique(selectedData()$StationName)) < 2) 
        {300} else 
        {length(unique(selectedData()$StationName)) * 200}})

      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData(), "Trend") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Trend") # Download figure

      # Parameter Definition
      output$ParamDefm <- shiny::renderText({
        paste("<h6><strong>", planktonr::pr_relabel(input$parameterm, style = "plotly"), ":</strong> ",
              pkg.env$ParamDef %>% 
                dplyr::filter(.data$Parameter == input$parameterm) %>% 
                dplyr::pull("Definition"), ".</h6>", sep = "")
      })
    })


    # Climatologies -----------------------------------------------------------

    # Plot abundance spectra by species
    observeEvent({input$CSmts == 2}, {

      gg_out2 <- reactive({

        if (length(selectedData()$Parameters) > 0){
          
        trans <- 'identity'
        if(input$scaler1){
          trans <- 'log10'
        }

        p1 <- planktonr::pr_plot_TimeSeries(selectedData(), Survey = "Coastal", trans = trans) +
          ggplot2::theme(legend.position = "none")

        p2 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "Coastal", Trend = "Month", trans = trans) +
          ggplot2::theme(legend.position = "none")

        p3 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "Coastal", Trend = "Year", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())

        #titley <- names(planktonr::pr_relabel(unique(selectedData()$Parameters), style = "simple"))

        # p1 / (p2 | p3) + patchwork::plot_layout(guides = "collect")
        p1 /
          (p2 + p3 + patchwork::plot_layout(ncol = 2, guides = "collect") & ggplot2::theme(legend.position = "bottom")) #+
        # patchwork::plot_annotation(title = titleplot)
        } else {
          ggplot2::ggplot + ggplot2::geom_blank()
        }
        
      }) %>% bindCache(input$parameterm, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)

      output$timeseries2 <- renderPlot({
        gg_out2()
      })

      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData(), "Climate") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2(), "Climate") # Download figure

    })

    # Plots by depths ---------------------------------------------------------

    observeEvent({input$CSmts == 3}, {

      gg_out3 <- reactive({

      if (length(selectedData()$Parameters) > 0){
          
        trend <-  input$smoother
        planktonr::pr_plot_Enviro(selectedData(), Trend = trend)
      } else {
        ggplot2::ggplot + ggplot2::geom_blank()
      }
      
      }) %>% bindCache(input$p1, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$smoother)

      output$timeseries3 <- renderPlot({
        gg_out3()
      }, height = function() {
        if(length(unique(selectedData()$SampleDepth_m)) < 2) 
        {300} else 
        {length(unique(selectedData()$SampleDepth_m)) * 200}})

      # Download -------------------------------------------------------
      output$downloadData3 <- fDownloadButtonServer(input, selectedData(), "Compare") # Download csv of data
      output$downloadPlot3 <- fDownloadPlotServer(input, gg_id = gg_out3(), "Compare") # Download figure

    })
  })
}
