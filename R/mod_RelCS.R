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
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      # dat1 = Microbes (y axis default group)
      # dat2 = CSChem   (x axis default group)
      # dat3/4/5 unused for CS but required by fRelationSidebar() signature
      fRelationSidebar(id = id, tabsetPanel_id = "RelCS",
                       dat1 = pkg.env$datCSm,  dat2 = pkg.env$CSChem,
                       dat3 = pkg.env$CSChem,  dat4 = pkg.env$CSChem,
                       dat5 = pkg.env$CSChem),
      fRelationPanel(id = id, tabsetPanel_id = "RelCS")
    )
  )
}

#' RelCS Server Functions
#'
#' @noRd 
mod_RelCS_server <- function(id){
  moduleServer(id, function(input, output, session, RelCS){

    # ── Combined CS dataset ──────────────────────────────────────────────────
    # Two CS data sources bound into a single long-format tibble.
    # Both datCSm (Microbes) and CSChem (Chemistry) carry real SampleDepth_m
    # values (rounded to 10 m bins in All_RData.R). A "Group" column is added
    # so fBuildChoices() can build the grouped variable dropdown.
    datAll <- reactive({
      dplyr::bind_rows(
        pkg.env$datCSm %>%
          dplyr::mutate(Group = "Microbes"),
        pkg.env$CSChem %>%
          dplyr::mutate(Group = "Chemical")
      ) %>%
        dplyr::mutate(
          SampleTime_Local = lubridate::floor_date(.data$SampleTime_Local, unit = "month")
        )
    })

    # ── Helper: build grouped choices list ───────────────────────────────────
    # Returns a named list suitable for selectizeInput() grouped optgroups.
    # When all_micro = FALSE only key microbial parameters are included.
    fBuildChoices <- function(dat, all_micro = FALSE) {
      micro_params <- if (all_micro) {
        unique(dat$Parameters[dat$Group == "Microbes"])
      } else {
        unique(dat$Parameters[dat$Group == "Microbes" &
                                grepl("Temperature_Index_KD|Abund|gene|ASV", dat$Parameters)])
      }
      list(
        "Microbes"  = planktonr:::pr_relabel(micro_params, style = "simple", named = TRUE),
        "Chemical"  = planktonr:::pr_relabel(
          unique(dat$Parameters[dat$Group == "Chemical"]),
          style = "simple", named = TRUE)
      )
    }

    # ── Helper: find valid depth choices based on actual data overlap ─────────
    # For each axis, only show depths where a join on StationCode+SampleTime_Local
    # with the OTHER parameter would produce at least one row. This prevents the
    # user from selecting a depth combination that yields an empty plot.
    #
    # Returns a list(y = char_vector, x = char_vector) of valid depth choices.
    # If a parameter is depth-integrated (all NA SampleDepth_m), returns
    # "Depth Integrated" for that axis.
    #
    # site_col: the column name used to filter by station ("State" for CS,
    #           "StationName" for NRS).
    fValidDepths <- function(dat, param_y, param_x, stations = NULL,
                             site_col = "State") {
      # Filter to selected stations
      if (!is.null(stations) && length(stations) > 0) {
        dat <- dat %>% dplyr::filter(.data[[site_col]] %in% stations)
      }

      dat_y <- dat %>%
        dplyr::filter(.data$Parameters == param_y) %>%
        dplyr::select("StationCode", "SampleTime_Local", depth_y = "SampleDepth_m") %>%
        dplyr::distinct()

      dat_x <- dat %>%
        dplyr::filter(.data$Parameters == param_x) %>%
        dplyr::select("StationCode", "SampleTime_Local", depth_x = "SampleDepth_m") %>%
        dplyr::distinct()

      # Check if either parameter is depth-integrated (all NA depths)
      y_integrated <- all(is.na(dat_y$depth_y))
      x_integrated <- all(is.na(dat_x$depth_x))

      if (y_integrated && x_integrated) {
        return(list(y = "Depth Integrated", x = "Depth Integrated"))
      }
      if (y_integrated) {
        return(list(y = "Depth Integrated",
                    x = as.character(sort(unique(stats::na.omit(dat_x$depth_x))))))
      }
      if (x_integrated) {
        return(list(y = as.character(sort(unique(stats::na.omit(dat_y$depth_y)))),
                    x = "Depth Integrated"))
      }

      # Both have real depths — inner join on StationCode + SampleTime_Local
      overlap <- dplyr::inner_join(dat_y, dat_x,
                                   by = c("StationCode", "SampleTime_Local"),
                                   relationship = "many-to-many") %>%
        dplyr::filter(!is.na(.data$depth_y), !is.na(.data$depth_x))

      if (nrow(overlap) == 0) {
        # No overlap at all — fall back to all available depths
        return(list(
          y = as.character(sort(unique(stats::na.omit(dat_y$depth_y)))),
          x = as.character(sort(unique(stats::na.omit(dat_x$depth_x))))
        ))
      }

      list(
        y = as.character(sort(unique(overlap$depth_y))),
        x = as.character(sort(unique(overlap$depth_x)))
      )
    }

    # ── Update grouped variable choices when input$all changes ───────────────
    observeEvent(input$all, {
      choices <- fBuildChoices(datAll(), all_micro = isTRUE(input$all))
      shiny::updateSelectizeInput(session, "py", choices = choices, selected = input$py)
      shiny::updateSelectizeInput(session, "px", choices = choices, selected = input$px)
    }, ignoreNULL = FALSE)

    # ── Update depth choices when selected Y parameter changes ───────────────
    observeEvent(input$py, {
      req(input$py, input$px)
      valid <- fValidDepths(datAll(), input$py, input$px,
                            stations = input$site, site_col = "State")
      shiny::updateSelectizeInput(session, "depthy",
                                  choices = valid$y, selected = valid$y[1])
      shiny::updateSelectizeInput(session, "depthx",
                                  choices = valid$x, selected = valid$x[1])
    })

    # ── Update depth choices when selected X parameter changes ───────────────
    observeEvent(input$px, {
      req(input$py, input$px)
      valid <- fValidDepths(datAll(), input$py, input$px,
                            stations = input$site, site_col = "State")
      shiny::updateSelectizeInput(session, "depthy",
                                  choices = valid$y, selected = valid$y[1])
      shiny::updateSelectizeInput(session, "depthx",
                                  choices = valid$x, selected = valid$x[1])
    })

    # ── Update depth choices when station selection changes ──────────────────
    observeEvent(input$site, {
      req(input$py, input$px)
      valid <- fValidDepths(datAll(), input$py, input$px,
                            stations = input$site, site_col = "State")
      selected_y <- if (isTRUE(input$depthy %in% valid$y)) input$depthy else valid$y[1]
      selected_x <- if (isTRUE(input$depthx %in% valid$x)) input$depthx else valid$x[1]
      shiny::updateSelectizeInput(session, "depthy",
                                  choices = valid$y, selected = selected_y)
      shiny::updateSelectizeInput(session, "depthx",
                                  choices = valid$x, selected = selected_x)
    }, ignoreNULL = FALSE)

    # ── Selected data for plotting ───────────────────────────────────────────
    # Filters to chosen parameters and depth slices, then pivots to wide format.
    # SampleDepth_m is excluded from id_cols so pr_plot_scatter() does not facet.
    # CS uses State as the site identifier (not StationName).
    #
    # Self-healing depth selection: when input$py or input$px changes, Shiny
    # may re-run selectedData() before the observeEvent() observers have had a
    # chance to update input$depthy / input$depthx to valid values for the new
    # variable pair. To guard against this, we call fValidDepths() here and
    # fall back to the first valid depth whenever the current input value is
    # not in the valid set. This eliminates blank plots caused by reactivity
    # ordering.
    selectedData <- reactive({
      req(input$site, input$py, input$px)

      y <- rlang::string(input$py)
      x <- rlang::string(input$px)

      # Re-validate depths internally — guards against stale input$depthy /
      # input$depthx when the variable just changed and observers haven't fired.
      valid      <- fValidDepths(datAll(), y, x,
                                 stations = input$site, site_col = "State")
      depthy_use <- if (isTRUE(input$depthy %in% valid$y)) input$depthy else valid$y[1]
      depthx_use <- if (isTRUE(input$depthx %in% valid$x)) input$depthx else valid$x[1]

      depthy_val <- if (isTRUE(depthy_use == "Depth Integrated")) NA_real_ else as.numeric(depthy_use)
      depthx_val <- if (isTRUE(depthx_use == "Depth Integrated")) NA_real_ else as.numeric(depthx_use)

      dat_y <- datAll() %>%
        dplyr::filter(.data$Parameters == y) %>%
        dplyr::filter(
          (is.na(depthy_val) & is.na(.data$SampleDepth_m)) |
          (!is.na(depthy_val) & !is.na(.data$SampleDepth_m) & .data$SampleDepth_m == depthy_val)
        )

      dat_x <- datAll() %>%
        dplyr::filter(.data$Parameters == x) %>%
        dplyr::filter(
          (is.na(depthx_val) & is.na(.data$SampleDepth_m)) |
          (!is.na(depthx_val) & !is.na(.data$SampleDepth_m) & .data$SampleDepth_m == depthx_val)
        )

      # id_cols: CS uses State as the spatial identifier.
      # SampleDepth_m intentionally excluded so pr_plot_scatter() does not facet.
      id_cols <- c("StationName", "StationCode", "SampleTime_Local", "State")

      dplyr::bind_rows(dat_y, dat_x) %>%
        dplyr::filter(.data$State %in% input$site,
                      .data$Parameters %in% c(x, y)) %>%
        planktonr::pr_remove_outliers(2) %>%
        tidyr::pivot_wider(
          id_cols     = dplyr::any_of(id_cols),
          names_from  = "Parameters",
          values_from = "Values",
          values_fn   = mean
        ) %>%
        tidyr::drop_na()

    }) %>% bindCache(input$site, input$py, input$px, input$depthy, input$depthx)

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
      fMapboxMap(sites, Survey = "Coastal", Type = "Zooplankton")
    })

    observe({
      sites <- if (length(input$site) > 0) input$site else character(0)
      fMapboxUpdate("plotmap", session, sites,
                    Survey = "Coastal", Type = "Zooplankton")
    }) %>% shiny::bindEvent(input$site, ignoreNULL = FALSE)

    # ── Plot explanatory text ────────────────────────────────────────────────
    output$PlotExp1 <- shiny::renderText({
      req(input$py, input$px)
      y <- rlang::string(input$py)
      x <- rlang::string(input$px)
      dat <- tryCatch(selectedData(), error = function(e) NULL)
      if (!is.null(dat) && y %in% colnames(dat) && x %in% colnames(dat)) {
        "A scatter plot of selected indices against oceanographic parameters measured from Coastal Stations around Australia"
      } else {
        paste("A scatter plot of selected indices against oceanographic parameters measured from Coastal Stations around Australia",
              "<br><br><b>NOTE: Not enough data for plot</b>")
      }
    }) %>% bindCache(input$py, input$px, input$site, input$depthy, input$depthx)

    output$PlotExp2 <- shiny::renderText({
      req(input$py)
      y <- rlang::string(input$py)
      dat <- tryCatch(selectedData(), error = function(e) NULL)
      if (!is.null(dat) && y %in% colnames(dat)) {
        "A box plot of selected indices showing range of each parameter at Coastal Stations around Australia"
      } else {
        paste("A box plot of selected indices showing range of each parameter at Coastal Stations around Australia",
              "<br><br><b>NOTE: Not enough data for plot</b>")
      }
    }) %>% bindCache(input$py, input$site)

    # ── Scatter plot ─────────────────────────────────────────────────────────
    gg_out1 <- reactive({
      req(input$py, input$px, input$smoother)
      trend <- input$smoother
      y     <- rlang::string(input$py)
      x     <- rlang::string(input$px)
      dat   <- tryCatch(selectedData(), error = function(e) NULL)

      if (!is.null(dat) && y %in% colnames(dat) && x %in% colnames(dat)) {
        planktonr::pr_plot_scatter(dat, x, y, trend)
      } else {
        ggplot2::ggplot() + ggplot2::geom_blank()
      }
    }) %>% bindCache(input$site, input$py, input$px, input$depthy, input$depthx, input$smoother)

    output$scatter1 <- renderPlot({
      req(is.null(input$RelCS) || input$RelCS == "1")
      gg_out1()
    }, height = 300)

    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData, "Scatter")
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1, "Scatter")

    # ── Box plot ─────────────────────────────────────────────────────────────
    gg_out2 <- reactive({
      req(input$py)
      y   <- rlang::string(input$py)
      dat <- tryCatch(selectedData(), error = function(e) NULL)

      if (!is.null(dat) && y %in% colnames(dat)) {
        planktonr::pr_plot_box(dat, y)
      } else {
        ggplot2::ggplot() + ggplot2::geom_blank()
      }
    }) %>% bindCache(input$py, input$site)

    output$box2 <- renderPlot({
      gg_out2()
    })

    # Download -------------------------------------------------------
    output$downloadData2 <- fDownloadButtonServer(input, selectedData, "Scatter")
    output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2, "Scatter")

  })
}
