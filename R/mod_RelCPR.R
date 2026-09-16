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
  ns <- NS(id)
  
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

    # ── Combined CPR dataset ─────────────────────────────────────────────────
    # All three CPR data sources are bound into a single long-format tibble.
    # CPR data has no depth dimension (towed at ~10 m); SampleDepth_m is not
    # present in any of these datasets so no depth handling is needed here.
    # A "Group" column is added for consistency with the NRS pattern and to
    # support the grouped variable dropdown in fRelationSidebar().
    #
    # PolCPR (Physical) has extra columns (sd, anomaly, Survey, means) that
    # are not present in the plankton datasets; these are dropped on binding.
    datAll <- reactive({
      dplyr::bind_rows(
        pkg.env$datCPRz %>%
          dplyr::select(-c("Latitude", "Longitude")) %>%
          dplyr::mutate(Group = "Zooplankton"),
        pkg.env$datCPRp %>%
          dplyr::select(-c("Latitude", "Longitude")) %>%
          dplyr::mutate(Group = "Phytoplankton"),
        pkg.env$PolCPR %>%
          dplyr::filter(.data$Parameters %in% c("SST", "chl_oc3")) %>%
          dplyr::select(-dplyr::any_of(c("sd", "anomaly", "Survey", "means"))) %>%
          dplyr::mutate(Group = "Physical")
      )
    })

    # ── Selected data for plotting ───────────────────────────────────────────
    # Filters the combined dataset to the chosen parameters, then pivots to
    # wide format. CPR has no depth dimension so no depth filtering is needed.
    selectedData <- reactive({
      req(input$site, input$py, input$px)

      y <- rlang::string(input$py)
      x <- rlang::string(input$px)

      # id_cols: BioRegion is the CPR equivalent of StationName.
      id_cols <- c("BioRegion", "SampleTime_Local")

      datAll() %>%
        dplyr::filter(.data$BioRegion %in% input$site,
                      .data$Parameters %in% c(x, y)) %>%
        planktonr::pr_remove_outliers(2) %>%
        tidyr::pivot_wider(
          id_cols     = dplyr::any_of(id_cols),
          names_from  = "Parameters",
          values_from = "Values",
          values_fn   = mean
        ) %>%
        tidyr::drop_na() %>%
        planktonr:::pr_reorder()

    }) %>% bindCache(input$py, input$px, input$site)

    # ── Parameter definitions ────────────────────────────────────────────────
    output$ParamDefy <- shiny::renderText({
      paste("<p><strong>", planktonr:::pr_relabel(input$py, style = "plotly"), ":</strong> ",
            pkg.env$ParamDef %>%
              dplyr::filter(.data$Parameter == input$py) %>%
              dplyr::pull("Definition"), ".</p>", sep = "")
    })
    output$ParamDefx <- shiny::renderText({
      paste("<p><strong>", planktonr:::pr_relabel(input$px, style = "plotly"), ":</strong> ",
            pkg.env$ParamDef %>%
              dplyr::filter(.data$Parameter == input$px) %>%
              dplyr::pull("Definition"), ".</p>", sep = "")
    })

    # ── Sidebar map ──────────────────────────────────────────────────────────
    output$plotmap <- mapgl::renderMapboxgl({
      sites <- if (length(input$site) > 0) input$site else character(0)
      fMapboxMap(sites, Survey = "CPR", Type = "Relation")
    })

    observe({
      sites <- if (length(input$site) > 0) input$site else character(0)
      fMapboxUpdate("plotmap", session, sites, Survey = "CPR", Type = "Relation")
    }) %>% shiny::bindEvent(input$site, ignoreNULL = FALSE)

    # ── Plot explanatory text ────────────────────────────────────────────────
    output$PlotExp1 <- shiny::renderText({
      "A scatter plot of selected indices against oceanographic parameters measured from the CPR around Australia"
    })
    output$PlotExp2 <- shiny::renderText({
      "A box plot of selected indices showing range of each parameter at the CPR around Australia"
    })

    # ── Scatter plot ─────────────────────────────────────────────────────────
    gg_out1 <- reactive({
      trend <- input$smoother
      y     <- rlang::string(input$py)
      x     <- rlang::string(input$px)

      planktonr::pr_plot_scatter(selectedData(), x, y, trend)

    }) %>% bindCache(input$py, input$px, input$site, input$smoother)

    output$scatter1 <- renderPlot({
      req(is.null(input$RelCPR) || input$RelCPR == "1")
      gg_out1()
    }, height = 300)

    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Scatter")
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Scatter")

    # ── Box plot ─────────────────────────────────────────────────────────────
    gg_out2 <- reactive({
      y <- rlang::string(input$py)
      planktonr::pr_plot_box(selectedData(), y)
    }) %>% bindCache(input$py, input$site)

    output$box2 <- renderPlot({
      gg_out2()
    })

    # Download -------------------------------------------------------
    output$downloadData2 <- fDownloadButtonServer(input, selectedData, "Scatter")
    output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2, "Scatter")

  })
}
