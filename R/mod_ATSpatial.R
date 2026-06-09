#' ATSpatial UI Function
#'
#' @description Shiny module for IMOS Animal Tracking spatial explorer.
#' Renders a full-width Mapbox GL map of acoustic receiver stations with an
#' animated slide-in sidebar showing per-station species breakdowns and inline
#' SVG charts (sparklines, sex-ratio donut, length histogram).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ATSpatial_ui <- function(id) {
  ns <- NS(id)

  # Guard: render a friendly message if data failed to load
  if (!isTRUE(pkg.env$AT_data_loaded)) {
    return(tagList(
      shiny::fluidPage(
        shiny::tags$div(
          class = "alert alert-warning",
          style = "margin-top:2rem;",
          shiny::tags$strong("Animal Tracking data unavailable."),
          " The receiver and animal summary files could not be loaded."
        )
      )
    ))
  }

  tagList(
    # ---- Inline JavaScript (namespaced) ------------------------------------
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(sprintf("
        // Open the station sidebar when a receiver feature is clicked
        Shiny.addCustomMessageHandler('at_openSidebar_%s', function(msg) {
          var el = document.getElementById('%s');
          if (el) el.classList.add('open');
        });
      ", id, ns("at_sidebar"))))
    ),

    # ---- Outer container (map + floating panels + sidebar) -----------------
    shiny::tags$div(
      class = "at-map-container",

      # Full-width Mapbox map -------------------------------------------------
      mapgl::mapboxglOutput(ns("map"), width = "100%", height = "calc(100vh - 115px)"),

      # Species filter — floating panel top-left ------------------------------
      shiny::tags$div(
        class = "at-species-filter",
        shiny::tags$div(class = "at-legend-title", "Filter by Species"),
        shiny::selectizeInput(
          inputId  = ns("species_filter"),
          label    = NULL,
          choices  = pkg.env$AT_all_species,
          selected = NULL,
          multiple = TRUE,
          options  = list(
            placeholder = "Type to search species\u2026",
            plugins     = list("remove_button"),
            maxItems    = NULL
          )
        )
      ),

      # Detection count legend — floating panel bottom-left ------------------
      shiny::tags$div(
        class = "at-legend",
        shiny::tags$div(class = "at-legend-title", "Receiver Detections"),
        shiny::tags$div(
          style = paste0(
            "width:100%;height:14px;border-radius:3px;margin:6px 0 2px 0;",
            "background:linear-gradient(to right,",
            "rgba(59,110,143,0.1) 0%,",
            "rgba(59,110,143,0.5) 38%,",
            "rgba(59,110,143,0.67) 55%,",
            "rgba(59,110,143,0.83) 75%,",
            "rgba(59,110,143,1.0) 100%);"
          )
        ),
        shiny::tags$div(
          style = "display:flex;justify-content:space-between;font-size:10px;color:#595959;",
          shiny::tags$span("0"),
          shiny::tags$span("1K"),
          shiny::tags$span("10K"),
          shiny::tags$span("100K"),
          shiny::tags$span("1M+")
        )
      ),

      # Station detail sidebar -----------------------------------------------
      shiny::tags$div(
        id    = ns("at_sidebar"),
        class = "at-sidebar",

        # Header
        shiny::tags$div(
          class = "at-sidebar-header",
          shiny::uiOutput(ns("sidebar_title"), inline = TRUE),
          shiny::tags$button(
            class   = "at-sidebar-close",
            title   = "Close",
            onclick = sprintf(
              "document.getElementById('%s').classList.remove('open');",
              ns("at_sidebar")
            ),
            "\u00D7"   # ×
          )
        ),

        # Scrollable body
        shiny::tags$div(
          class = "at-sidebar-body",
          shiny::uiOutput(ns("sidebar_content"))
        )
      ) # /at-sidebar
    ) # /at-map-container
  )
}


#' ATSpatial Server Functions
#'
#' @noRd
mod_ATSpatial_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Guard: exit early if data is not available
    if (!isTRUE(pkg.env$AT_data_loaded)) return(invisible(NULL))

    # Convenience aliases to pkg.env data
    receivers       <- pkg.env$AT_receivers
    station_species <- pkg.env$AT_station_species
    individual_data <- pkg.env$AT_individual_data
    daily_summary   <- pkg.env$AT_daily_summary

    # Reactive state --------------------------------------------------------
    selected_station <- shiny::reactiveVal(NULL)
    expanded_species <- shiny::reactiveVal(NULL)

    # ---- MAP ---------------------------------------------------------------

    output$map <- mapgl::renderMapboxgl({
      mapgl::mapboxgl(
        access_token       = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
        projection         = "mercator",
        style              = mapgl::mapbox_style("light"),
        center             = c(134.0, -27.0),
        zoom               = 3.5,
        navigation_control = TRUE,
        min_zoom           = 2,
        max_zoom           = 16
      ) %>%
        mapgl::add_circle_layer(
          id                  = "receivers",
          source              = receivers,
          circle_color        = "#3B6E8F",
          circle_opacity      = list("get", "point_opacity"),
          circle_radius       = 7,
          circle_stroke_color = "#ffffff",
          circle_stroke_width = 1.5,
          popup               = "popup_html",
          tooltip             = "installation_name",
          hover_options = list(circle_radius = 11, circle_stroke_width = 2.5)
        )
    })

    # ---- SPECIES FILTER → update map layer filter -------------------------

    shiny::observeEvent(input$species_filter, {
      sel   <- input$species_filter
      proxy <- mapgl::mapboxgl_proxy("map")

      if (is.null(sel) || length(sel) == 0) {
        proxy %>% mapgl::set_filter("receivers", NULL)
      } else {
        matching <- station_species %>%
          dplyr::filter(.data$species_common_name %in% sel) %>%
          dplyr::pull(.data$installation_name) %>%
          unique()

        filt <- list("in",
                     list("get", "installation_name"),
                     list("literal", as.list(matching)))

        proxy %>% mapgl::set_filter("receivers", filt)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # ---- FEATURE CLICK → open sidebar -------------------------------------

    shiny::observeEvent(input$map_feature_click, {
      click <- input$map_feature_click
      if (is.null(click)) return()
      station_name <- click$properties$installation_name
      if (is.null(station_name) || nchar(trimws(station_name)) == 0) return()

      expanded_species(NULL)
      selected_station(station_name)
      session$sendCustomMessage(paste0("at_openSidebar_", id), list())
    })

    # ---- SPECIES CARD TOGGLE ----------------------------------------------

    shiny::observeEvent(input$species_card_click, {
      clicked <- input$species_card_click
      if (!is.null(expanded_species()) && expanded_species() == clicked) {
        expanded_species(NULL)
      } else {
        expanded_species(clicked)
      }
    })

    # ---- SIDEBAR TITLE ----------------------------------------------------

    output$sidebar_title <- shiny::renderUI({
      shiny::req(selected_station())
      shiny::tags$h3(selected_station(), style = "margin:0;")
    })

    # ---- SIDEBAR CONTENT --------------------------------------------------

    output$sidebar_content <- shiny::renderUI({
      shiny::req(selected_station())
      name <- selected_station()

      rx <- receivers %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(.data$installation_name == name) %>%
        dplyr::slice(1)

      if (nrow(rx) == 0) {
        return(shiny::tags$div(class = "at-no-data", "Station not found."))
      }

      # Metadata block
      deploy_str   <- if (!is.na(rx$deployment_date))
        format(as.Date(rx$deployment_date), "%d %b %Y") else "Unknown"
      recovery_str <- if (!is.na(rx$recovery_date))
        format(as.Date(rx$recovery_date), "%d %b %Y") else "Ongoing"
      is_active     <- isTRUE(rx$active)
      status_label  <- if (is_active) "Active"   else "Inactive"
      status_colour <- if (is_active) "#88C057"  else "#E76F51"

      meta_block <- shiny::tags$div(
        class = "at-station-meta",
        shiny::tags$p(paste0("Coordinates: ", rx$lon, "\u00b0E, ", rx$lat, "\u00b0")),
        shiny::tags$p(paste0(
          "Receiver deployments: ",
          format(rx$total_receivers, big.mark = ",")
        )),
        shiny::tags$p(paste0("Deployed: ", deploy_str, " \u2013 ", recovery_str)),
        shiny::tags$p(
          "Status: ",
          shiny::tags$strong(style = paste0("color:", status_colour, ";"), status_label)
        )
      )

      # Per-species cards (filter-aware) — computed first so stats can reflect filter
      sel_sp   <- input$species_filter
      spp      <- station_species %>%
        dplyr::filter(.data$installation_name == name) %>%
        { if (!is.null(sel_sp) && length(sel_sp) > 0)
            dplyr::filter(., .data$species_common_name %in% sel_sp)
          else . } %>%
        dplyr::arrange(dplyr::desc(.data$total_detections))

      # Derive stat values: use filtered aggregates when a species filter is active
      filter_active <- !is.null(sel_sp) && length(sel_sp) > 0
      stat_n_species   <- if (filter_active) nrow(spp)                    else rx$n_species
      stat_n_inds      <- if (filter_active) sum(spp$n_individuals)        else rx$n_individuals
      stat_detections  <- if (filter_active) sum(spp$total_detections)     else rx$total_detections

      # Summary stat cards
      stats_row <- shiny::tags$div(
        class = "at-summary-stats",
        shiny::tags$div(
          class = "at-stat-card",
          shiny::tags$div(class = "at-stat-value", stat_n_species),
          shiny::tags$div(class = "at-stat-label", "Species")
        ),
        shiny::tags$div(
          class = "at-stat-card",
          shiny::tags$div(class = "at-stat-value", format(stat_n_inds, big.mark = ",")),
          shiny::tags$div(class = "at-stat-label", "Individuals")
        ),
        shiny::tags$div(
          class = "at-stat-card",
          shiny::tags$div(class = "at-stat-value", format(stat_detections, big.mark = ",")),
          shiny::tags$div(class = "at-stat-label", "Detections")
        )
      )
      inds_all  <- individual_data %>% dplyr::filter(.data$installation_name == name)
      expanded  <- expanded_species()

      species_cards <- purrr::map(seq_len(nrow(spp)), function(j) {
        s           <- spp[j, ]
        sp_name     <- s$species_common_name
        is_expanded <- !is.null(expanded) && expanded == sp_name
        chevron     <- if (is_expanded) "\u25b2" else "\u25bc"

        inds      <- inds_all %>% dplyr::filter(.data$species_common_name == sp_name)
        daily_sp  <- daily_summary %>%
          dplyr::filter(
            .data$installation_name == name,
            .data$species_common_name == sp_name
          ) %>%
          dplyr::arrange(.data$date_UTC)
        aphia_id  <- s$WORMS_species_aphia_id

        detail <- if (is_expanded) {
          shiny::tagList(
            shiny::tags$div(
              class = "at-chart-section",
              shiny::tags$div(class = "at-chart-title", "Daily Individuals"),
              shiny::HTML(at_sparkline_html(
                daily_sp$date_UTC, daily_sp$n_individuals, colour = "#3B6E8F"
              ))
            ),
            shiny::tags$div(
              class = "at-chart-section",
              shiny::tags$div(class = "at-chart-title", "Daily Detections"),
              shiny::HTML(at_sparkline_html(
                daily_sp$date_UTC, daily_sp$total_detections, colour = "#54BCEB"
              ))
            ),
            shiny::tags$div(
              class = "at-chart-section",
              shiny::tags$div(class = "at-chart-title", "Sex Ratio"),
              shiny::HTML(at_sex_donut_html(inds))
            ),
            shiny::tags$div(
              class = "at-chart-section",
              shiny::tags$div(class = "at-chart-title", "Length Distribution"),
              shiny::HTML(at_length_bar_html(inds))
            ),
            # More Information links
            shiny::tags$div(
              class = "at-more-info",
              shiny::tags$div(class = "at-more-info-title", "More Information"),
              shiny::tags$ul(
                class = "at-more-info-links",
                if (!is.na(aphia_id) && !is.null(aphia_id)) {
                  shiny::tagList(
                    shiny::tags$li(shiny::tags$a(
                      href   = paste0("https://www.marinespecies.org/aphia.php?p=taxdetails&id=", aphia_id),
                      target = "_blank", rel = "noopener noreferrer",
                      shiny::tags$span(class = "at-more-info-icon", "\U0001F517"),
                      "World Register of Marine Species (WoRMS)"
                    )),
                    shiny::tags$li(shiny::tags$a(
                      href   = paste0("https://obis.org/taxon/", aphia_id),
                      target = "_blank", rel = "noopener noreferrer",
                      shiny::tags$span(class = "at-more-info-icon", "\U0001F517"),
                      "Ocean Biodiversity Information System (OBIS)"
                    ))
                  )
                } else {
                  shiny::tagList(
                    shiny::tags$li(
                      class = "at-more-info-unavailable",
                      shiny::tags$span(class = "at-more-info-icon", "\U0001F517"),
                      "WoRMS record not available."
                    ),
                    shiny::tags$li(
                      class = "at-more-info-unavailable",
                      shiny::tags$span(class = "at-more-info-icon", "\U0001F517"),
                      "OBIS record not available."
                    )
                  )
                },
                shiny::tags$li(shiny::tags$a(
                  href   = "https://animaltracking.aodn.org.au/transmitters/transmitter",
                  target = "_blank", rel = "noopener noreferrer",
                  shiny::tags$span(class = "at-more-info-icon", "\U0001F517"),
                  "IMOS Animal Tracking Database"
                ))
              )
            )
          )
        } else {
          NULL
        }

        shiny::tags$div(
          class   = if (is_expanded) "at-species-row at-species-row-expanded" else "at-species-row",
          # Use Shiny.setInputValue for the toggle so it works within module namespace
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
            ns("species_card_click"),
            gsub("'", "\\\\'", sp_name)
          ),
          style = "cursor:pointer;",

          shiny::tags$div(
            style = "display:flex;justify-content:space-between;align-items:flex-start",
            shiny::tags$div(
              shiny::tags$div(class = "at-species-name", sp_name),
              shiny::tags$div(
                style = "font-size:11px;color:#595959;font-style:italic;margin-bottom:5px;",
                s$species_scientific_name
              ),
              shiny::tags$div(
                class = "at-species-stats",
                shiny::tags$span(
                  class = "at-stat-badge",
                  paste0(format(s$n_individuals, big.mark = ","), " individuals")
                ),
                shiny::tags$span(
                  class = "at-stat-badge",
                  paste0(format(s$total_detections, big.mark = ","), " detections")
                )
              )
            ),
            shiny::tags$span(
              style = "font-size:11px;color:#BFBFBF;padding-top:2px;flex-shrink:0;",
              chevron
            )
          ),

          detail
        ) # /at-species-row
      }) # /lapply species_cards

      shiny::tagList(
        meta_block,
        stats_row,
        shiny::tags$div(
          class = "at-section-header",
          "Species Detected",
          shiny::tags$span(
            style = "font-weight:400;font-size:11px;color:#BFBFBF;margin-left:6px;",
            "(click to expand)"
          )
        ),
        shiny::tags$div(class = "at-species-list", species_cards)
      )
    }) # /renderUI sidebar_content

    shiny::outputOptions(output, "sidebar_title",   suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "sidebar_content", suspendWhenHidden = FALSE)

  }) # /moduleServer
}


## To be copied in the UI:
# mod_ATSpatial_ui("ATSpatial_1")

## To be copied in the server:
# mod_ATSpatial_server("ATSpatial_1")
