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
      fPlanktonSidebar(id = id, tabsetPanel_id = "CSmts", dat = datCSm),#dat = pkg.env$datCSm),
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
    selectedData <- reactive({

      selectedData <- datCSm %>% #pkg.env$datCSm %>%
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% input$parameterm,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels() %>%
        dplyr::mutate(name = as.factor(.data$Parameters))

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
      planktonr::pr_plot_NRSmap(selectedData())
    }, bg = "transparent") %>% bindCache(input$Site)

    # Add text information
    output$PlotExp1 <- renderText({
      "A plot of selected microbial indices from the NRS around Australia, as a time series and a monthly climatology by station averaged across all depths."
    })
    output$PlotExp2 <- renderText({
      "A plot of selected indices from the NRS around Australia, as a time series, a monthly climatology and an annual mean averaged across all depths."
    })
    output$PlotExp3 <- renderText({
      "A contour plot of microbial indices from the NRS around Australia, as a time series and a monthly climatology by depth.
      If raw data is used the dots represent actual samples"
    })
    output$PlotExp4 <- renderText({
      "A plot of microbial indices against abundance measure from the NRS around Australia"
    })



    # Plot Trends -------------------------------------------------------------

    observeEvent({input$CSmts == 1}, {

      gg_out1 <- reactive({

        if (is.null(datCSm$StationName)) #pkg.env$datCSm$StationName))  ## was reading datNRSi() as function so had to change to this, there should always be a code
          return(NULL)

        if(input$scaler1){
          trans <- 'log10'
        } else {
          trans <- 'identity'
        }

        p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", Survey = "NRS", method = "lm", trans = trans)
        p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", Survey = "NRS", method = "loess", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())

        p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")

      }) %>% bindCache(input$parameterm, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)

      output$timeseries1 <- renderPlot({
        gg_out1()
      }, height = function() {length(unique(selectedData()$StationName)) * 200})

      # Download -------------------------------------------------------
      output$downloadData1 <- fDownloadButtonServer(input, selectedData(), "Trend") # Download csv of data
      output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Trend") # Download figure

      # Parameter Definition
      output$ParamDefm <- fParamDefServer(selectedData) # Download csv of data

    })


    # Climatologies -----------------------------------------------------------

    # Plot abundance spectra by species
    observeEvent({input$CSmts == 2}, {

      gg_out2 <- reactive({

        if (is.null(datCSm$StationName)) #pkg.env$datCSm$StationName))  ## was reading datNRSi() as function so had to change to this, there should always be a code
          return(NULL)

        trans <- 'identity'
        if(input$scaler1){
          trans <- 'log10'
        }

        p1 <- planktonr::pr_plot_TimeSeries(selectedData(), Survey = "NRS", trans = trans) +
          ggplot2::theme(legend.position = "none")

        p2 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Month", trans = trans) +
          ggplot2::theme(legend.position = "none")

        p3 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Year", trans = trans) +
          ggplot2::theme(axis.title.y = ggplot2::element_blank())

        #titley <- names(planktonr::pr_relabel(unique(selectedData()$Parameters), style = "simple"))

        # p1 / (p2 | p3) + patchwork::plot_layout(guides = "collect")
        p1 /
          (p2 + p3 + patchwork::plot_layout(ncol = 2, guides = "collect") & ggplot2::theme(legend.position = "bottom")) #+
        # patchwork::plot_annotation(title = titleplot)

      }) %>% bindCache(input$parameterm, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)

      output$timeseries2 <- renderPlot({
        gg_out2()
      })

      # Download -------------------------------------------------------
      output$downloadData2 <- fDownloadButtonServer(input, selectedData(), "Climate") # Download csv of data
      output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2(), "Climate") # Download figure

      # Parameter Definition
      output$ParamDefm <- fParamDefServer(selectedData)

    })

    # Plots by depths ---------------------------------------------------------

    ## Coastal samples are all at around 2m

    # Plots by Parameters ---------------------------------------------------------

    observeEvent({input$CSmts == 4}, {

      selectedData1 <- reactive({
        req(input$Site)
        req(input$p1)
        validate(need(!is.na(input$Site), "Error: Please select a station."))
        validate(need(!is.na(input$p1), "Error: Please select a parameter."))

        selectedData1 <- datCSm %>% #pkg.env$datCSm %>%
          dplyr::filter(.data$StationName %in% input$Site,
                        .data$Parameters %in% c(input$p1, input$p2),
                        dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
          tidyr::pivot_wider(id_cols = c("StationName", "SampleTime_Local"),
                             names_from = "Parameters", values_from = "Values", values_fn = mean)

      }) %>% bindCache(input$p1, input$p2, input$Site, input$DatesSlide[1], input$DatesSlide[2])

      gg_out4 <- reactive({

        # When we move to a planktonr function for this, we can use this:
        # pr_plot_scatter(selectedData1(), x = colnames(selectedData1()[, 5]), y = colnames(selectedData1()[, 4]))

        #TODO This needs to be converted to a planktonr function. At the moment it can't use planktonr colours without :::

        x <- rlang::sym(colnames(selectedData1()[, 4]))
        y <- rlang::sym(colnames(selectedData1()[, 3]))

        titlex <- planktonr::pr_relabel(rlang::as_string(x), style = "ggplot")
        titley <- planktonr::pr_relabel(rlang::as_string(y), style = "ggplot")

        ggplot2::ggplot(data = selectedData1()) +
          ggplot2::geom_point(ggplot2::aes(!!x, !!y, colour = .data$StationName)) +
          ggplot2::xlab(titlex) +
          ggplot2::ylab(titley) +
          ggplot2::scale_colour_manual(values = planktonr:::colNRSName) +
          planktonr::theme_pr()

      }) %>% bindCache(input$p1, input$p2, input$Site, input$DatesSlide[1], input$DatesSlide[2])

      output$timeseries4 <- renderPlot({
        gg_out4()
      })

      # Download -------------------------------------------------------
      output$downloadData4 <- fDownloadButtonServer(input, selectedData1(), "Compare") # Download csv of data
      output$downloadPlot4 <- fDownloadPlotServer(input, gg_id = gg_out4(), "Compare") # Download figure

    })
  })
}
