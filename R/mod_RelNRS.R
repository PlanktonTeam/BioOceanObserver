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
  ns <- NS(id)

  tagList(
    sidebarLayout(
      fRelationSidebar(id = id, tabsetPanel_id = "relNRS", dat1 = pkg.env$datNRSz, dat2 = pkg.env$datNRSp,
                       dat3 = pkg.env$datNRSm, dat4 = pkg.env$ctd, dat5 = pkg.env$Nuts),
      fRelationPanel(id = id, tabsetPanel_id = "relNRS")
    )
  )
}

#' RelNRS Server Functions
#'
#' @noRd 
mod_RelNRS_server <- function(id){
  moduleServer(id, function(input, output, session, relNRS){

    # ── Combined NRS dataset ─────────────────────────────────────────────────
    # All five NRS data sources are bound into a single long-format tibble once
    # at startup. Zooplankton and Phytoplankton are depth-integrated (no real
    # SampleDepth_m); we mark them with NA so the depth dropdown can detect
    # this and show "Depth Integrated". Microbes, Physical (CTD), and Chemical
    # (Nuts) all carry real depth values and are passed through unchanged.
    #
    # A "Group" column is added so we can later filter the Microbes sub-group
    # when input$all changes (to show key vs. all microbial parameters).
    datAll <- reactive({
      dplyr::bind_rows(
        pkg.env$datNRSz %>%
          dplyr::mutate(Group = "Zooplankton",   SampleDepth_m = NA_real_),
        pkg.env$datNRSp %>%
          dplyr::mutate(Group = "Phytoplankton",  SampleDepth_m = NA_real_),
        pkg.env$datNRSm %>%
          dplyr::mutate(Group = "Microbes",
                        SampleDepth_m = round(.data$SampleDepth_m / 10, 0) * 10),
        pkg.env$ctd %>%
          dplyr::mutate(Group = "Physical"),
        pkg.env$Nuts %>%
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
        "Zooplankton"   = planktonr:::pr_relabel(
          unique(dat$Parameters[dat$Group == "Zooplankton"]),
          style = "simple", named = TRUE),
        "Phytoplankton" = planktonr:::pr_relabel(
          unique(dat$Parameters[dat$Group == "Phytoplankton"]),
          style = "simple", named = TRUE),
        "Microbes"      = planktonr:::pr_relabel(micro_params, style = "simple", named = TRUE),
        "Physical"      = planktonr:::pr_relabel(
          unique(dat$Parameters[dat$Group == "Physical"]),
          style = "simple", named = TRUE),
        "Chemical"      = planktonr:::pr_relabel(
          unique(dat$Parameters[dat$Group == "Chemical"]),
          style = "simple", named = TRUE)
      )
    }

    # ── Helper: find valid depth choices based on actual data overlap ─────────
    # Only shows depths where a join on StationCode+SampleTime_Local with the
    # OTHER parameter would produce at least one row. This prevents the user
    # from selecting a depth combination that yields an empty plot.
    #
    # Returns list(y = char_vector, x = char_vector).
    # "Depth Integrated" is returned for depth-integrated parameters (NA depths).
    fValidDepths <- function(dat, param_y, param_x, stations = NULL) {
      if (!is.null(stations) && length(stations) > 0) {
        dat <- dat %>% dplyr::filter(.data$StationName %in% stations)
      }

      dat_y <- dat %>%
        dplyr::filter(.data$Parameters == param_y) %>%
        dplyr::select("StationCode", "SampleTime_Local", depth_y = "SampleDepth_m") %>%
        dplyr::distinct()

      dat_x <- dat %>%
        dplyr::filter(.data$Parameters == param_x) %>%
        dplyr::select("StationCode", "SampleTime_Local", depth_x = "SampleDepth_m") %>%
        dplyr::distinct()

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
    # Rebuilds both py and px dropdowns so the Microbes sub-group reflects the
    # current checkbox state. Preserves the current selection where possible.
    observeEvent(input$all, {
      choices <- fBuildChoices(datAll(), all_micro = isTRUE(input$all))
      shiny::updateSelectizeInput(session, "py", choices = choices, selected = input$py)
      shiny::updateSelectizeInput(session, "px", choices = choices, selected = input$px)
    }, ignoreNULL = FALSE)

    # ── Update depth choices when selected Y parameter changes ───────────────
    observeEvent(input$py, {
      req(input$py, input$px)
      valid <- fValidDepths(datAll(), input$py, input$px, stations = input$site)
      shiny::updateSelectizeInput(session, "depthy",
                                  choices = valid$y, selected = valid$y[1])
      shiny::updateSelectizeInput(session, "depthx",
                                  choices = valid$x, selected = valid$x[1])
    })

    # ── Update depth choices when selected X parameter changes ───────────────
    observeEvent(input$px, {
      req(input$py, input$px)
      valid <- fValidDepths(datAll(), input$py, input$px, stations = input$site)
      shiny::updateSelectizeInput(session, "depthy",
                                  choices = valid$y, selected = valid$y[1])
      shiny::updateSelectizeInput(session, "depthx",
                                  choices = valid$x, selected = valid$x[1])
    })

    # ── Update depth choices when station selection changes ──────────────────
    # When the user adds/removes stations, re-derive valid depth choices.
    # Preserve the current selection if still valid; otherwise use shallowest.
    observeEvent(input$site, {
      req(input$py, input$px)
      valid <- fValidDepths(datAll(), input$py, input$px, stations = input$site)
      selected_y <- if (isTRUE(input$depthy %in% valid$y)) input$depthy else valid$y[1]
      selected_x <- if (isTRUE(input$depthx %in% valid$x)) input$depthx else valid$x[1]
      shiny::updateSelectizeInput(session, "depthy",
                                  choices = valid$y, selected = selected_y)
      shiny::updateSelectizeInput(session, "depthx",
                                  choices = valid$x, selected = selected_x)
    }, ignoreNULL = FALSE)

    # ── Selected data for plotting ───────────────────────────────────────────
    # Filters the combined dataset to the chosen parameters and depth slices,
    # then pivots to wide format. SampleDepth_m is intentionally excluded from
    # id_cols so pr_plot_scatter() does not receive it and therefore does not
    # facet. The wide data retains SampleDepth_m as a regular column (one value
    # per row after the depth filter) so it is present in the downloaded CSV.
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
      valid      <- fValidDepths(datAll(), y, x, stations = input$site)
      depthy_use <- if (isTRUE(input$depthy %in% valid$y)) input$depthy else valid$y[1]
      depthx_use <- if (isTRUE(input$depthx %in% valid$x)) input$depthx else valid$x[1]

      # Determine the numeric depth to filter on, or NA for depth-integrated.
      # Using explicit NA_real_ so downstream comparisons are type-safe.
      depthy_val <- if (isTRUE(depthy_use == "Depth Integrated")) NA_real_ else as.numeric(depthy_use)
      depthx_val <- if (isTRUE(depthx_use == "Depth Integrated")) NA_real_ else as.numeric(depthx_use)

      # Filter each axis to its chosen parameter and depth, then combine.
      # The depth filter uses explicit NA-aware logic: for depth-integrated
      # parameters SampleDepth_m is NA, so we match on is.na(); for depth-
      # resolved parameters we match on the exact numeric value.
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

      # id_cols for pivot_wider: standard temporal/spatial identifiers.
      # SampleDepth_m is deliberately excluded here so the wide data has no
      # depth column and pr_plot_scatter() produces a single unfaceted plot.
      id_cols <- c("StationName", "StationCode", "SampleTime_Local")

      dplyr::bind_rows(dat_y, dat_x) %>%
        dplyr::filter(.data$StationName %in% input$site,
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
      stationCodes <- if (length(input$site) > 0) {
        pkg.env$NRSStation %>%
          dplyr::filter(.data$StationName %in% input$site) %>%
          dplyr::pull(.data$StationCode)
      } else {
        character(0)
      }
      fMapboxMap(stationCodes, Survey = "NRS", Type = "Zooplankton")
    })

    observe({
      stationCodes <- if (length(input$site) > 0) {
        pkg.env$NRSStation %>%
          dplyr::filter(.data$StationName %in% input$site) %>%
          dplyr::pull(.data$StationCode)
      } else {
        character(0)
      }
      fMapboxUpdate("plotmap", session, stationCodes,
                    Survey = "NRS", Type = "Zooplankton")
    }) %>% shiny::bindEvent(input$site, ignoreNULL = FALSE)

    # ── Plot explanatory text ────────────────────────────────────────────────
    output$PlotExp1 <- shiny::renderText({
      req(input$py, input$px)
      y <- rlang::string(input$py)
      x <- rlang::string(input$px)
      dat <- tryCatch(selectedData(), error = function(e) NULL)
      if (!is.null(dat) && y %in% colnames(dat) && x %in% colnames(dat)) {
        "A scatter plot of selected indices against oceanographic parameters measured from the NRS around Australia"
      } else {
        paste("A scatter plot of selected indices against oceanographic parameters measured from the NRS around Australia",
              "<br><br><b>NOTE: Not enough data for plot</b>")
      }
    }) %>% bindCache(input$py, input$px, input$site, input$depthy, input$depthx)

    output$PlotExp2 <- shiny::renderText({
      req(input$py)
      y <- rlang::string(input$py)
      dat <- tryCatch(selectedData(), error = function(e) NULL)
      if (!is.null(dat) && y %in% colnames(dat)) {
        "A box plot of selected indices showing range of each parameter at the NRS around Australia"
      } else {
        paste("A box plot of selected indices showing range of each parameter at the NRS around Australia",
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
      req(is.null(input$relNRS) || input$relNRS == "1")
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
