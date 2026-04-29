library(shiny)
library(mapgl)
library(sf)
library(dplyr)
library(htmltools)
library(purrr)

# ==============================================================================
# DATA PREPARATION
# ==============================================================================

receivers      <- readRDS("Output/ReceiverSummary.rds")   # sf POINT
animal_summary <- readRDS("Output/AnimalSummary.rds")     # tbl with Type/Value cols

# Station-level aggregate counts -----------------------------------------------
station_summary <- animal_summary %>%
  group_by(installation_name) %>%
  summarise(
    n_species        = n_distinct(species_common_name),
    n_individuals    = n_distinct(animal_id),
    total_detections = sum(total_detections),
    .groups = "drop"
  )

# Species breakdown per station (for sidebar cards) ----------------------------
station_species <- animal_summary %>%
  group_by(installation_name, species_common_name, species_scientific_name,
           WORMS_species_aphia_id) %>%
  summarise(
    n_individuals    = n_distinct(animal_id),
    total_detections = sum(total_detections),
    .groups = "drop"
  ) %>%
  arrange(installation_name, species_common_name)

# Individual-level data: one row per animal at each station --------------------
# (deduplicated - used for sex ratio and length charts)
individual_data <- animal_summary %>%
  distinct(animal_id, installation_name, species_common_name,
           species_scientific_name, animal_sex, Type, Value)

# Sorted species list for the selectize filter --------------------------------
all_species <- sort(unique(animal_summary$species_common_name))

# Daily time-series per station × species (for sparklines) --------------------
daily_summary <- animal_summary %>%
  group_by(installation_name, species_common_name, date_UTC) %>%
  summarise(
    n_individuals    = n_distinct(animal_id),
    total_detections = sum(total_detections),
    .groups = "drop"
  )

# Enrich receiver sf object — keep ONLY stations with detection data -----------
receivers <- receivers %>%
  left_join(station_summary, by = "installation_name") %>%
  filter(!is.na(n_species)) %>%                          # drop stations with no data
  mutate(
    has_data         = TRUE,
    n_species        = as.integer(n_species),
    n_individuals    = as.integer(n_individuals),
    total_detections = as.integer(total_detections),
    lon              = round(sf::st_coordinates(.)[, 1], 5),
    lat              = round(sf::st_coordinates(.)[, 2], 5),
    # Opacity: log scale from 0 detections (0.1) up to >= 1M detections (1.0)
    point_opacity    = pmin(1, pmax(0.1, log1p(total_detections) / log1p(1e6)))
  )

# Build popup HTML column (small Mapbox popup on click) ------------------------
receivers$popup_html <- map_chr(seq_len(nrow(receivers)), function(i) {
  r <- sf::st_drop_geometry(receivers[i, ])
  data_line <- paste0(
    '<span class="at-popup-has-data">&#10003;&nbsp;',
    r$n_species, " species &bull; ",
    format(r$n_individuals, big.mark = ","), " individuals",
    "</span>"
  )
  paste0(
    '<div class="at-popup">',
    '<div class="at-popup-header">RECEIVER STATION</div>',
    '<div class="at-popup-body">',
    '<div class="at-popup-name">', htmlEscape(r$installation_name), "</div>",
    '<div class="at-popup-coords">', r$lon, ", ", r$lat, "</div>",
    data_line,
    "</div></div>"
  )
})

# ==============================================================================
# SVG CHART HELPERS
# ==============================================================================

#' Inline SVG donut chart for sex ratio
#' @param inds tibble with animal_sex column (one row per individual)
sex_donut_html <- function(inds) {
  counts <- c(
    MALE    = sum(inds$animal_sex == "MALE",    na.rm = TRUE),
    FEMALE  = sum(inds$animal_sex == "FEMALE",  na.rm = TRUE),
    UNKNOWN = sum(inds$animal_sex == "UNKNOWN", na.rm = TRUE)
  )
  total <- sum(counts)
  if (total == 0) return('<p style="color:#718096;font-size:12px;padding:4px 0">No sex data available.</p>')

  pal    <- c(MALE = "#0077b6", FEMALE = "#e07b39", UNKNOWN = "#a0aec0")
  labels <- c(MALE = "Male",   FEMALE = "Female",  UNKNOWN = "Unknown")

  cx <- 60; cy <- 60; r_out <- 50; r_in <- 22
  angle <- -pi / 2   # start at 12-o'clock

  slices <- ""
  for (sex in c("MALE", "FEMALE", "UNKNOWN")) {
    n <- counts[sex]
    if (n == 0) next
    sweep      <- 2 * pi * n / total
    angle_end  <- angle + sweep
    large_arc  <- if (sweep > pi) 1 else 0
    col        <- pal[sex]

    if (abs(sweep - 2 * pi) < 1e-9) {
      # Full circle — use two arcs to avoid SVG degenerate arc issue
      slices <- paste0(slices,
        '<path d="',
        'M ', cx + r_out, ' ', cy,
        ' A ', r_out, ' ', r_out, ' 0 1 1 ', cx - r_out, ' ', cy,
        ' A ', r_out, ' ', r_out, ' 0 1 1 ', cx + r_out, ' ', cy,
        ' L ', cx + r_in, ' ', cy,
        ' A ', r_in, ' ', r_in, ' 0 1 0 ', cx - r_in, ' ', cy,
        ' A ', r_in, ' ', r_in, ' 0 1 0 ', cx + r_in, ' ', cy,
        ' Z" fill="', col, '"/>')
    } else {
      x1o <- round(cx + r_out * cos(angle),     2)
      y1o <- round(cy + r_out * sin(angle),     2)
      x2o <- round(cx + r_out * cos(angle_end), 2)
      y2o <- round(cy + r_out * sin(angle_end), 2)
      x1i <- round(cx + r_in  * cos(angle_end), 2)
      y1i <- round(cy + r_in  * sin(angle_end), 2)
      x2i <- round(cx + r_in  * cos(angle),     2)
      y2i <- round(cy + r_in  * sin(angle),     2)
      slices <- paste0(slices,
        '<path d="',
        'M ', x1o, ' ', y1o,
        ' A ', r_out, ' ', r_out, ' 0 ', large_arc, ' 1 ', x2o, ' ', y2o,
        ' L ', x1i, ' ', y1i,
        ' A ', r_in,  ' ', r_in,  ' 0 ', large_arc, ' 0 ', x2i, ' ', y2i,
        ' Z" fill="', col, '"/>')
    }
    angle <- angle_end
  }

  # Legend
  legend <- paste0(
    map_chr(c("MALE", "FEMALE", "UNKNOWN"), function(sex) {
      n   <- counts[sex]
      pct <- if (total > 0) round(100 * n / total) else 0
      paste0(
        '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;font-size:12px;color:#2d3748">',
        '<span style="width:10px;height:10px;border-radius:50%;background:', pal[sex], ';flex-shrink:0;display:inline-block"></span>',
        labels[sex], ': <strong>', n, '</strong> (', pct, '%)',
        '</div>'
      )
    }),
    collapse = ""
  )

  paste0(
    '<div style="display:flex;align-items:center;gap:16px;padding:8px 0 4px">',
    '<svg width="120" height="120" viewBox="0 0 120 120" style="flex-shrink:0">',
    slices,
    '</svg>',
    '<div>', legend, '</div>',
    '</div>'
  )
}

#' Inline SVG bar chart for length distribution
#' @param inds tibble with Value, Type, and animal_sex columns (one row per individual)
length_bar_html <- function(inds) {
  # Work only with rows that have a length value
  inds_v <- inds %>% filter(!is.na(Value))
  if (nrow(inds_v) == 0)
    return('<p style="color:#718096;font-size:12px;padding:4px 0">No length data available.</p>')

  # Measurement type label (use most common non-NA type)
  type_label <- inds_v %>%
    filter(!is.na(Type)) %>%
    count(Type, sort = TRUE) %>%
    slice(1) %>%
    pull(Type)
  if (length(type_label) == 0) type_label <- "Length"

  # Shared colour palette (matches sex donut)
  sex_pal <- c(MALE = "#0077b6", FEMALE = "#e07b39", UNKNOWN = "#a0aec0")
  sex_order <- c("MALE", "FEMALE", "UNKNOWN")

  # Normalise sex values
  inds_v <- inds_v %>%
    mutate(sex_grp = toupper(trimws(animal_sex)),
           sex_grp = ifelse(sex_grp %in% sex_order, sex_grp, "UNKNOWN"))

  # Build common bins
  vals   <- inds_v$Value
  n_bins <- max(3, min(8, length(unique(round(vals)))))
  h      <- hist(vals, breaks = n_bins, plot = FALSE)
  breaks <- h$breaks
  mids   <- h$mids
  n_bars <- length(mids)

  # Count individuals per bin × sex
  bin_idx <- findInterval(vals, breaks, rightmost.closed = TRUE)
  bin_idx <- pmin(bin_idx, n_bars)   # clamp edge case

  # Matrix: rows = bins, cols = sex categories
  counts_mat <- matrix(
    sapply(sex_order, function(s) {
      tabulate(bin_idx[inds_v$sex_grp == s], nbins = n_bars)
    }),
    nrow = n_bars, ncol = length(sex_order),
    dimnames = list(NULL, sex_order)
  )

  total_per_bin <- rowSums(counts_mat)
  max_c <- max(total_per_bin, 1)

  # SVG layout — extra bottom padding for legend
  sw <- 260; sh <- 155
  pl <- 28; pr <- 8; pt <- 10; pb <- 55
  pw <- sw - pl - pr
  ph <- sh - pt - pb
  bw <- pw / n_bars

  # Stacked bars — draw from bottom: UNKNOWN, FEMALE, MALE
  bars <- paste0(
    map_chr(seq_len(n_bars), function(i) {
      x     <- pl + (i - 1) * bw
      y_cur <- pt + ph   # start at axis bottom, move up
      segs  <- ""
      for (s in rev(sex_order)) {   # rev so MALE ends up on top
        n <- counts_mat[i, s]
        if (n == 0) next
        seg_h <- round(n / max_c * ph)
        y_cur <- y_cur - seg_h
        segs  <- paste0(segs,
          '<rect x="', round(x + 1), '" y="', y_cur,
          '" width="', round(bw - 2), '" height="', seg_h,
          '" fill="', sex_pal[s], '" opacity="0.85" rx="1"/>'
        )
      }
      segs
    }),
    collapse = ""
  )

  # X-axis tick labels (every other bar)
  x_ticks <- paste0(
    map_chr(seq_len(n_bars), function(i) {
      if (i %% 2 == 0 && i < n_bars) return("")
      x <- pl + (i - 0.5) * bw
      paste0(
        '<text x="', round(x), '" y="', pt + ph + 12,
        '" text-anchor="middle" font-size="9" fill="#718096">',
        round(mids[i]), '</text>'
      )
    }),
    collapse = ""
  )

  # Y-axis max label
  y_max_label <- paste0(
    '<text x="', pl - 3, '" y="', pt + 4,
    '" text-anchor="end" font-size="9" fill="#718096">', max_c, '</text>'
  )

  # Axis lines
  axes <- paste0(
    '<line x1="', pl, '" y1="', pt,      '" x2="', pl,      '" y2="', pt + ph, '" stroke="#e2e8f0" stroke-width="1"/>',
    '<line x1="', pl, '" y1="', pt + ph, '" x2="', pl + pw, '" y2="', pt + ph, '" stroke="#e2e8f0" stroke-width="1"/>'
  )

  # X axis title
  axis_title <- paste0(
    '<text x="', pl + pw / 2, '" y="', pt + ph + 24,
    '" text-anchor="middle" font-size="10" fill="#4a5568">',
    type_label, ' (cm)</text>'
  )

  # Inline sex legend below chart
  legend_y  <- pt + ph + 38
  leg_items <- paste0(
    map_chr(seq_along(sex_order), function(k) {
      s    <- sex_order[k]
      lx   <- pl + (k - 1) * 72
      label <- paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
      n_s  <- sum(counts_mat[, s])
      paste0(
        '<rect x="', lx, '" y="', legend_y,
        '" width="9" height="9" fill="', sex_pal[s], '" rx="2"/>',
        '<text x="', lx + 12, '" y="', legend_y + 9,
        '" font-size="9" fill="#4a5568">', label, ' (', n_s, ')</text>'
      )
    }),
    collapse = ""
  )

  paste0(
    '<div style="padding:4px 0 8px">',
    '<svg width="100%" viewBox="0 0 ', sw, ' ', sh,
    '" style="overflow:visible;display:block">',
    axes, bars, x_ticks, y_max_label, axis_title, leg_items,
    '</svg>',
    '</div>'
  )
}

#' Inline SVG sparkline (filled area + line)
#' @param dates  Date vector (x axis)
#' @param values numeric vector (y axis), same length as dates
#' @param colour hex fill / stroke colour
#' @param y_label character label shown at right-end of y axis
sparkline_html <- function(dates, values, colour, y_label = "") {
  if (length(dates) == 0 || all(is.na(values)))
    return(paste0('<p style="color:#718096;font-size:11px;padding:2px 0">No data available.</p>'))

  # Sort by date
  ord    <- order(dates)
  dates  <- dates[ord]
  values <- values[ord]

  # SVG layout
  sw <- 260; sh <- 55
  pl <- 6;  pr <- 6; pt <- 6; pb <- 16
  pw <- sw - pl - pr
  ph <- sh - pt - pb

  # Scale
  x_min  <- as.numeric(min(dates))
  x_max  <- as.numeric(max(dates))
  x_rng  <- max(x_max - x_min, 1)
  y_max  <- max(values, 1)

  to_px <- function(d, v) {
    px <- pl + (as.numeric(d) - x_min) / x_rng * pw
    py <- pt + ph - (v / y_max * ph)
    list(x = round(px, 1), y = round(py, 1))
  }

  pts <- mapply(to_px, dates, values, SIMPLIFY = FALSE)

  # Polyline points string
  poly_pts <- paste(
    sapply(pts, function(p) paste0(p$x, ",", p$y)),
    collapse = " "
  )

  # Area path: go along the line then back along the baseline
  first_x <- pts[[1]]$x
  last_x  <- pts[[length(pts)]]$x
  base_y  <- pt + ph
  area_d  <- paste0(
    "M ", first_x, ",", base_y,
    " L ", poly_pts,
    " L ", last_x, ",", base_y, " Z"
  )

  # X-axis date labels: start and end
  fmt_date <- function(d) format(d, "%b %Y")
  x_labels <- paste0(
    '<text x="', pl, '" y="', sh,
    '" font-size="8" fill="#a0aec0" text-anchor="start">', fmt_date(min(dates)), '</text>',
    '<text x="', sw - pr, '" y="', sh,
    '" font-size="8" fill="#a0aec0" text-anchor="end">',   fmt_date(max(dates)), '</text>'
  )

  # Y-axis max label (top-right)
  y_max_lbl <- paste0(
    '<text x="', sw - pr, '" y="', pt + 8,
    '" font-size="8" fill="#a0aec0" text-anchor="end">', y_max, '</text>'
  )

  paste0(
    '<div style="padding:2px 0 4px">',
    '<svg width="100%" viewBox="0 0 ', sw, ' ', sh,
    '" style="overflow:visible;display:block">',
    # Baseline
    '<line x1="', pl, '" y1="', pt + ph,
    '" x2="', sw - pr, '" y2="', pt + ph,
    '" stroke="#e2e8f0" stroke-width="1"/>',
    # Filled area
    '<path d="', area_d, '" fill="', colour,
    '" fill-opacity="0.15" stroke="none"/>',
    # Line
    '<polyline points="', poly_pts,
    '" fill="none" stroke="', colour,
    '" stroke-width="1.5" stroke-linejoin="round" stroke-linecap="round"/>',
    x_labels, y_max_lbl,
    '</svg>',
    '</div>'
  )
}

# ==============================================================================
# UI
# ==============================================================================

ui <- tagList(
  tags$html(lang = "en"),
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("IMOS Animal Tracking Explorer"),
    tags$link(rel = "stylesheet", href = "styles.css"),

    tags$script(HTML("
      // Slide sidebar in when a receiver with data is clicked
      Shiny.addCustomMessageHandler('openSidebar', function(msg) {
        document.getElementById('at_sidebar').classList.add('open');
      });

      // Checkbox toggle for receiver layer filter
      function filterReceiverLayer(cb) {
        Shiny.setInputValue('toggle_' + cb.value, cb.checked, {priority: 'event'});
      }

      // Expand/collapse a species card
      function toggleSpeciesCard(species) {
        Shiny.setInputValue('species_card_click', species, {priority: 'event'});
      }
    "))
  ),

  tags$div(
    class = "at-map-container",

    # Full-screen Mapbox map
    mapboxglOutput("map", width = "100%", height = "100vh"),

    # Species filter (top-left floating panel)
    tags$div(
      class = "at-species-filter",
      tags$div(class = "at-legend-title", "Filter by Species"),
      selectizeInput(
        inputId  = "species_filter",
        label    = NULL,
        choices  = all_species,
        selected = NULL,
        multiple = TRUE,
        options  = list(
          placeholder  = "Type to search species\u2026",
          plugins      = list("remove_button"),
          maxItems     = NULL
        )
      )
    ),

    # Floating legend (bottom-left) — detections colour bar
    tags$div(
      class = "at-legend",
      tags$div(class = "at-legend-title", "Receiver Detections"),
      # Gradient bar: low-opacity blue → full-opacity blue
      tags$div(
        style = paste0(
          "width:100%;height:14px;border-radius:3px;margin:6px 0 2px 0;",
          "background:linear-gradient(to right,",
          "rgba(0,119,182,0.1) 0%,",
          "rgba(0,119,182,0.5) 38%,",
          "rgba(0,119,182,0.67) 55%,",
          "rgba(0,119,182,0.83) 75%,",
          "rgba(0,119,182,1.0) 100%);"
        )
      ),
      # Tick labels beneath the bar
      tags$div(
        style = "display:flex;justify-content:space-between;font-size:10px;color:#4a5568;",
        tags$span("0"),
        tags$span("1K"),
        tags$span("10K"),
        tags$span("100K"),
        tags$span("1M+")
      )
    ),

    # Right sidebar
    tags$div(
      id    = "at_sidebar",
      class = "at-sidebar",
      tags$div(
        class = "at-sidebar-header",
        uiOutput("sidebar_title", inline = TRUE),
        tags$button(
          class   = "at-sidebar-close",
          title   = "Close",
          onclick = "document.getElementById('at_sidebar').classList.remove('open');",
          "\u00D7"
        )
      ),
      tags$div(
        class = "at-sidebar-body",
        uiOutput("sidebar_content")
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {

  # Which station is currently selected
  selected_station <- reactiveVal(NULL)

  # Which species card is currently expanded (NULL = all collapsed)
  expanded_species <- reactiveVal(NULL)

  # --------------------------------------------------------------------------
  # MAP
  # --------------------------------------------------------------------------

  output$map <- renderMapboxgl({
    mapboxgl(
      access_token       = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
      projection         = "mercator",
      style              = mapbox_style("light"),
      center             = c(147.0, -30.5),
      zoom               = 4,
      navigation_control = TRUE,
      min_zoom           = 2,
      max_zoom           = 16
    ) %>%
      add_circle_layer(
        id                  = "receivers",
        source              = receivers,
        circle_color        = "#0077b6",
        circle_opacity      = list("get", "point_opacity"),
        circle_radius       = 7,
        circle_stroke_color = "#ffffff",
        circle_stroke_width = 1.5,
        popup               = "popup_html",
        tooltip             = "installation_name",
        hover_options = list(circle_radius = 11, circle_stroke_width = 2.5)
      )
  })

  # --------------------------------------------------------------------------
  # SPECIES FILTER → show only receivers with matching species
  # --------------------------------------------------------------------------

  observeEvent(input$species_filter, {
    sel <- input$species_filter
    proxy <- mapboxgl_proxy("map")

    if (is.null(sel) || length(sel) == 0) {
      # No filter — clear filter to show every receiver
      proxy %>%
        set_filter("receivers", NULL)
    } else {
      # Stations that have at least one of the selected species
      matching <- station_species %>%
        filter(species_common_name %in% sel) %>%
        pull(installation_name) %>%
        unique()

      # Mapbox "in" expression: ["in", ["get", "installation_name"], ["literal", [...]]]
      filt <- list("in",
                   list("get", "installation_name"),
                   list("literal", as.list(matching)))

      proxy %>%
        set_filter("receivers", filt)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # FEATURE CLICK → open sidebar
  # --------------------------------------------------------------------------

  observeEvent(input$map_feature_click, {
    click <- input$map_feature_click
    if (is.null(click)) return()
    station_name <- click$properties$installation_name
    if (is.null(station_name) || nchar(trimws(station_name)) == 0) return()

    # Reset expanded card whenever a new station is selected
    expanded_species(NULL)
    selected_station(station_name)
    session$sendCustomMessage("openSidebar", list())
  })

  # --------------------------------------------------------------------------
  # SPECIES CARD EXPAND / COLLAPSE TOGGLE
  # --------------------------------------------------------------------------

  observeEvent(input$species_card_click, {
    clicked <- input$species_card_click
    if (!is.null(expanded_species()) && expanded_species() == clicked) {
      expanded_species(NULL)   # same card clicked again → collapse
    } else {
      expanded_species(clicked)
    }
  })

  # --------------------------------------------------------------------------
  # SIDEBAR TITLE
  # --------------------------------------------------------------------------

  output$sidebar_title <- renderUI({
    req(selected_station())
    tags$h3(selected_station(), style = "margin: 0;")
  })

  # --------------------------------------------------------------------------
  # SIDEBAR CONTENT
  # --------------------------------------------------------------------------

  output$sidebar_content <- renderUI({
    req(selected_station())
    name <- selected_station()

    rx <- receivers %>%
      sf::st_drop_geometry() %>%
      filter(installation_name == name) %>%
      slice(1)

    if (nrow(rx) == 0)
      return(tags$div(class = "at-no-data", "Station not found."))

    # Station metadata block
    deploy_str   <- if (!is.na(rx$deployment_date))
      format(as.Date(rx$deployment_date), "%d %b %Y") else "Unknown"
    recovery_str <- if (!is.na(rx$recovery_date))
      format(as.Date(rx$recovery_date), "%d %b %Y") else "Ongoing"
    is_active    <- isTRUE(rx$active)
    status_label <- if (is_active) "Active" else "Inactive"
    status_colour <- if (is_active) "#38a169" else "#e53e3e"

    meta_block <- tags$div(
      class = "at-station-meta",
      tags$p(paste0("Coordinates: ", rx$lon, "\u00b0E, ", rx$lat, "\u00b0")),
      tags$p(paste0("Deployed: ", deploy_str, " \u2013 ", recovery_str)),
      tags$p(
        "Status: ",
        tags$strong(
          style = paste0("color:", status_colour, ";"),
          status_label
        )
      )
    )

    # Top-level summary stat cards
    stats_row <- tags$div(
      class = "at-summary-stats",
      tags$div(class = "at-stat-card",
               tags$div(class = "at-stat-value", rx$n_species),
               tags$div(class = "at-stat-label", "Species")),
      tags$div(class = "at-stat-card",
               tags$div(class = "at-stat-value",
                        format(rx$n_individuals, big.mark = ",")),
               tags$div(class = "at-stat-label", "Individuals")),
      tags$div(class = "at-stat-card",
               tags$div(class = "at-stat-value",
                        format(rx$total_detections, big.mark = ",")),
               tags$div(class = "at-stat-label", "Detections"))
    )

    # Per-species breakdown rows (restrict to selected species when filter active)
    sel_sp <- input$species_filter
    spp <- station_species %>%
      filter(installation_name == name) %>%
      { if (!is.null(sel_sp) && length(sel_sp) > 0)
          filter(., species_common_name %in% sel_sp)
        else . }
    inds_all   <- individual_data %>% filter(installation_name == name)
    expanded   <- expanded_species()

    species_cards <- lapply(seq_len(nrow(spp)), function(j) {
      s           <- spp[j, ]
      sp_name     <- s$species_common_name
      is_expanded <- !is.null(expanded) && expanded == sp_name

      # Individual-level data for this species at this station
      inds <- inds_all %>% filter(species_common_name == sp_name)

      # The chevron indicator
      chevron <- if (is_expanded) "\u25b2" else "\u25bc"

      # Daily time-series for sparklines (station × species)
      daily_sp <- daily_summary %>%
        filter(installation_name == name,
               species_common_name == sp_name) %>%
        arrange(date_UTC)

      # Expandable detail section
      aphia_id <- s$WORMS_species_aphia_id

      detail <- if (is_expanded) {
        tagList(
          tags$div(
            class = "at-chart-section",
            tags$div(class = "at-chart-title", "Daily Individuals"),
            HTML(sparkline_html(daily_sp$date_UTC, daily_sp$n_individuals,
                                colour = "#2c7a7b"))
          ),
          tags$div(
            class = "at-chart-section",
            tags$div(class = "at-chart-title", "Daily Detections"),
            HTML(sparkline_html(daily_sp$date_UTC, daily_sp$total_detections,
                                colour = "#6b46c1"))
          ),
          tags$div(
            class = "at-chart-section",
            tags$div(class = "at-chart-title", "Sex Ratio"),
            HTML(sex_donut_html(inds))
          ),
          tags$div(
            class = "at-chart-section",
            tags$div(class = "at-chart-title", "Length Distribution"),
            HTML(length_bar_html(inds))
          ),
          # More Information section
          tags$div(
            class = "at-more-info",
            tags$div(class = "at-more-info-title", "More Information"),
            tags$ul(
              class = "at-more-info-links",
              if (!is.na(aphia_id) && !is.null(aphia_id)) {
                tagList(
                  tags$li(
                    tags$a(
                      href   = paste0("https://www.marinespecies.org/aphia.php?p=taxdetails&id=", aphia_id),
                      target = "_blank",
                      rel    = "noopener noreferrer",
                      tags$span(class = "at-more-info-icon", "\U0001F517"),
                      "World Register of Marine Species (WoRMS)"
                    )
                  ),
                  tags$li(
                    tags$a(
                      href   = paste0("https://obis.org/taxon/", aphia_id),
                      target = "_blank",
                      rel    = "noopener noreferrer",
                      tags$span(class = "at-more-info-icon", "\U0001F517"),
                      "Ocean Biodiversity Information System (OBIS)"
                    )
                  )
                )
              } else {
                tagList(
                  tags$li(
                    class = "at-more-info-unavailable",
                    tags$span(class = "at-more-info-icon", "\U0001F517"),
                    "WoRMS record not available."
                  ),
                  tags$li(
                    class = "at-more-info-unavailable",
                    tags$span(class = "at-more-info-icon", "\U0001F517"),
                    "OBIS record not available."
                  )
                )
              },
              tags$li(
                tags$a(
                  href   = "https://animaltracking.aodn.org.au/transmitters/transmitter",
                  target = "_blank",
                  rel    = "noopener noreferrer",
                  tags$span(class = "at-more-info-icon", "\U0001F517"),
                  "IMOS Animal Tracking Database"
                )
              )
            )
          )
        )
      } else NULL

      tags$div(
        class   = if (is_expanded) "at-species-row at-species-row-expanded" else "at-species-row",
        onclick = sprintf("toggleSpeciesCard('%s')",
                          gsub("'", "\\\\'", sp_name)),
        style   = "cursor:pointer;",

        # Card header
        tags$div(
          style = "display:flex;justify-content:space-between;align-items:flex-start",
          tags$div(
            tags$div(class = "at-species-name", sp_name),
            tags$div(
              style = "font-size:11px;color:#718096;font-style:italic;margin-bottom:5px;",
              s$species_scientific_name
            ),
            tags$div(
              class = "at-species-stats",
              tags$span(class = "at-stat-badge",
                        paste0(format(s$n_individuals, big.mark = ","),
                               " individuals")),
              tags$span(class = "at-stat-badge",
                        paste0(format(s$total_detections, big.mark = ","),
                               " detections"))
            )
          ),
          tags$span(
            style = "font-size:11px;color:#a0aec0;padding-top:2px;flex-shrink:0;",
            chevron
          )
        ),

        # Expanded charts
        detail
      )
    })

    tagList(
      meta_block,
      stats_row,
      tags$div(class = "at-section-header",
               "Species Detected",
               tags$span(style = "font-weight:400;font-size:11px;color:#a0aec0;margin-left:6px;",
                         "(click to expand)")),
      tags$div(class = "at-species-list", species_cards)
    )
  })

  outputOptions(output, "sidebar_title",   suspendWhenHidden = FALSE)
  outputOptions(output, "sidebar_content", suspendWhenHidden = FALSE)
}

# ==============================================================================
shinyApp(ui, server)
