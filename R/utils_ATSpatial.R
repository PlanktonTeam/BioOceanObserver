# ============================================================================
# Animal Tracking Spatial — Utility Functions
# ============================================================================
# Reusable helpers for data preparation and SVG chart generation.
# Colours inside inline SVG charts are kept as explicit hex values since SVG
# does not inherit CSS custom properties; all colours are mapped to the BOO
# IMOS palette (see inst/app/www/custom.css).
#   --primary-1  : #3B6E8F  (IMOS mid-blue)
#   --primary-2  : #182C3A  (IMOS dark navy)
#   --primary-5  : #E2ECF3  (IMOS light blue)
#   --text-2     : #3C3C3C
#   --grey-500   : #BFBFBF
#   --grey-700   : #595959
# ============================================================================

# ----------------------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------------------

#' Load and prepare all Animal Tracking data into pkg.env
#'
#' Called once from \code{.onLoad()} in \file{R/000.R}.
#' Populates \code{pkg.env$AT_receivers}, \code{pkg.env$AT_station_species},
#' \code{pkg.env$AT_individual_data}, \code{pkg.env$AT_all_species}, and
#' \code{pkg.env$AT_daily_summary}.
#'
#' @noRd
at_load_data <- function() {
  tryCatch({
    rds_dir <- system.file("extdata", package = "biooceanobserver")
    if (nchar(rds_dir) == 0 || !file.exists(file.path(rds_dir, "ReceiverSummary.rds"))) {
      # Fall back to data-raw during development
      rds_dir <- file.path("data-raw", "AnimalTracking")
    }

    receivers      <- readRDS(file.path(rds_dir, "ReceiverSummary.rds"))
    animal_summary <- readRDS(file.path(rds_dir, "AnimalSummary.rds"))
    species_summary <- readRDS(file.path(rds_dir, "SpeciesSummary.rds"))

    # Station-level aggregate counts
    station_summary <- animal_summary %>%
      dplyr::group_by(.data$installation_name) %>%
      dplyr::summarise(
        n_species        = dplyr::n_distinct(.data$species_common_name),
        n_individuals    = dplyr::n_distinct(.data$animal_id),
        total_detections = sum(.data$total_detections),
        .groups = "drop"
      )

    # Species breakdown per station
    station_species <- animal_summary %>%
      dplyr::group_by(
        .data$installation_name, .data$species_common_name,
        .data$species_scientific_name, .data$WORMS_species_aphia_id
      ) %>%
      dplyr::summarise(
        n_individuals    = dplyr::n_distinct(.data$animal_id),
        total_detections = sum(.data$total_detections),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$installation_name, .data$species_common_name)

    # Individual-level data (one row per animal per station)
    individual_data <- animal_summary %>%
      dplyr::distinct(
        .data$animal_id, .data$installation_name,
        .data$species_common_name, .data$species_scientific_name,
        .data$animal_sex, .data$Length_Type, .data$Length_cm
      )

    # Sorted species list for the selectize filter
    all_species <- sort(unique(animal_summary$species_common_name))

    # Daily time-series per station x species
    daily_summary <- animal_summary %>%
      dplyr::group_by(.data$installation_name, .data$species_common_name, .data$date_UTC) %>%
      dplyr::summarise(
        n_individuals    = dplyr::n_distinct(.data$animal_id),
        total_detections = sum(.data$total_detections),
        .groups = "drop"
      )

    # Enrich receiver sf object — keep only stations with detection data
    receivers <- receivers %>%
      dplyr::left_join(station_summary, by = "installation_name") %>%
      dplyr::filter(!is.na(.data$n_species)) %>%
      dplyr::mutate(
        has_data         = TRUE,
        n_species        = as.integer(.data$n_species),
        n_individuals    = as.integer(.data$n_individuals),
        total_detections = as.integer(.data$total_detections),
        lon              = round(sf::st_coordinates(.)[, 1], 5),
        lat              = round(sf::st_coordinates(.)[, 2], 5),
        point_opacity    = pmin(1, pmax(0.1, log1p(.data$total_detections) / log1p(1e6)))
      )

    # Build popup HTML column
    receivers$popup_html <- purrr::map_chr(seq_len(nrow(receivers)), function(i) {
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
        '<div class="at-popup-name">', htmltools::htmlEscape(r$installation_name), "</div>",
        '<div class="at-popup-coords">', r$lon, ", ", r$lat, "</div>",
        data_line,
        "</div></div>"
      )
    })

    pkg.env$AT_species_summary <- species_summary
    pkg.env$AT_receivers       <- receivers
    pkg.env$AT_station_species <- station_species
    pkg.env$AT_individual_data <- individual_data
    pkg.env$AT_all_species     <- all_species
    pkg.env$AT_daily_summary   <- daily_summary
    pkg.env$AT_data_loaded     <- TRUE

    cat(file = stderr(), "Animal Tracking data loaded successfully.\n")
  }, error = function(e) {
    cat(file = stderr(), paste0("Animal Tracking data failed to load: ", conditionMessage(e), "\n"))
    pkg.env$AT_data_loaded <- FALSE
  })
}


# ----------------------------------------------------------------------------
# SVG CHART HELPERS
# ----------------------------------------------------------------------------

#' Inline SVG donut chart for sex ratio
#'
#' @param inds tibble with an \code{animal_sex} column (one row per individual)
#' @return character HTML string
#' @noRd
at_sex_donut_html <- function(inds) {
  counts <- c(
    MALE    = sum(inds$animal_sex == "MALE",    na.rm = TRUE),
    FEMALE  = sum(inds$animal_sex == "FEMALE",  na.rm = TRUE),
    UNKNOWN = sum(inds$animal_sex == "UNKNOWN", na.rm = TRUE)
  )
  total <- sum(counts)
  if (total == 0) {
    return('<p style="color:var(--grey-700);font-size:12px;padding:4px 0">No sex data available.</p>')
  }

  # BOO-palette colours for sex categories
  pal    <- c(MALE = "#3B6E8F", FEMALE = "#E76F51", UNKNOWN = "#BFBFBF")
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

  legend <- paste0(
    purrr::map_chr(c("MALE", "FEMALE", "UNKNOWN"), function(sex) {
      n   <- counts[sex]
      pct <- if (total > 0) round(100 * n / total) else 0
      paste0(
        '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;',
        'font-size:12px;color:#3C3C3C">',
        '<span style="width:10px;height:10px;border-radius:50%;background:', pal[sex],
        ';flex-shrink:0;display:inline-block"></span>',
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


#' Inline SVG stacked bar chart for length distribution by sex
#'
#' @param inds tibble with \code{Value}, \code{Type}, and \code{animal_sex}
#'   columns (one row per individual)
#' @return character HTML string
#' @noRd
at_length_bar_html <- function(inds) {
  inds_v <- inds %>% dplyr::filter(!is.na(.data$Length_cm))
  if (nrow(inds_v) == 0) {
    return('<p style="color:var(--grey-700);font-size:12px;padding:4px 0">No length data available.</p>')
  }

  type_label <- inds_v %>%
    dplyr::filter(!is.na(.data$Length_Type)) %>%
    dplyr::count(.data$Length_Type, sort = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::pull(.data$Length_Type)
  if (length(type_label) == 0) type_label <- "Length"

  sex_pal   <- c(MALE = "#3B6E8F", FEMALE = "#E76F51", UNKNOWN = "#BFBFBF")
  sex_order <- c("MALE", "FEMALE", "UNKNOWN")

  inds_v <- inds_v %>%
    dplyr::mutate(
      sex_grp = toupper(trimws(.data$animal_sex)),
      sex_grp = ifelse(.data$sex_grp %in% sex_order, .data$sex_grp, "UNKNOWN")
    )

  vals   <- inds_v$Length_cm
  n_bins <- max(3, min(8, length(unique(round(vals)))))
  h      <- hist(vals, breaks = n_bins, plot = FALSE)
  breaks <- h$breaks
  mids   <- h$mids
  n_bars <- length(mids)

  bin_idx    <- findInterval(vals, breaks, rightmost.closed = TRUE)
  bin_idx    <- pmin(bin_idx, n_bars)
  counts_mat <- do.call(cbind, purrr::map(sex_order, function(s) {
    tabulate(bin_idx[inds_v$sex_grp == s], nbins = n_bars)
  }))
  dimnames(counts_mat) <- list(NULL, sex_order)

  total_per_bin <- rowSums(counts_mat)
  max_c <- max(total_per_bin, 1)

  sw <- 260; sh <- 155
  pl <- 28; pr <- 8; pt <- 10; pb <- 55
  pw <- sw - pl - pr
  ph <- sh - pt - pb
  bw <- pw / n_bars

  bars <- paste0(
    purrr::map_chr(seq_len(n_bars), function(i) {
      x     <- pl + (i - 1) * bw
      y_cur <- pt + ph
      segs  <- ""
      for (s in rev(sex_order)) {
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

  x_ticks <- paste0(
    purrr::map_chr(seq_len(n_bars), function(i) {
      if (i %% 2 == 0 && i < n_bars) return("")
      x <- pl + (i - 0.5) * bw
      paste0(
        '<text x="', round(x), '" y="', pt + ph + 12,
        '" text-anchor="middle" font-size="9" fill="#595959">',
        round(mids[i]), '</text>'
      )
    }),
    collapse = ""
  )

  y_max_label <- paste0(
    '<text x="', pl - 3, '" y="', pt + 4,
    '" text-anchor="end" font-size="9" fill="#595959">', max_c, '</text>'
  )

  axes <- paste0(
    '<line x1="', pl, '" y1="', pt,      '" x2="', pl,      '" y2="', pt + ph, '" stroke="#F0F0F0" stroke-width="1"/>',
    '<line x1="', pl, '" y1="', pt + ph, '" x2="', pl + pw, '" y2="', pt + ph, '" stroke="#F0F0F0" stroke-width="1"/>'
  )

  axis_title <- paste0(
    '<text x="', pl + pw / 2, '" y="', pt + ph + 24,
    '" text-anchor="middle" font-size="10" fill="#3C3C3C">',
    type_label, ' (cm)</text>'
  )

  legend_y  <- pt + ph + 38
  leg_items <- paste0(
    purrr::map_chr(seq_along(sex_order), function(k) {
      s     <- sex_order[k]
      lx    <- pl + (k - 1) * 72
      label <- paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
      n_s   <- sum(counts_mat[, s])
      paste0(
        '<rect x="', lx, '" y="', legend_y,
        '" width="9" height="9" fill="', sex_pal[s], '" rx="2"/>',
        '<text x="', lx + 12, '" y="', legend_y + 9,
        '" font-size="9" fill="#3C3C3C">', label, ' (', n_s, ')</text>'
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
#'
#' @param dates  Date vector (x-axis)
#' @param values numeric vector (y-axis), same length as \code{dates}
#' @param colour hex fill/stroke colour
#' @return character HTML string
#' @noRd
at_sparkline_html <- function(dates, values, colour) {
  if (length(dates) == 0 || all(is.na(values))) {
    return('<p style="color:var(--grey-700);font-size:11px;padding:2px 0">No data available.</p>')
  }

  # Ensure dates are coerced to Date (handles character, POSIXct, etc.)
  dates <- tryCatch(as.Date(dates), error = function(e) dates)

  # Drop any rows where either date or value is NA
  keep   <- !is.na(dates) & !is.na(values)
  dates  <- dates[keep]
  values <- values[keep]

  if (length(dates) == 0) {
    return('<p style="color:var(--grey-700);font-size:11px;padding:2px 0">No data available.</p>')
  }

  ord    <- order(dates)
  dates  <- dates[ord]
  values <- values[ord]

  sw <- 260; sh <- 55
  pl <- 6;  pr <- 6; pt <- 6; pb <- 16
  pw <- sw - pl - pr
  ph <- sh - pt - pb

  x_min <- as.numeric(min(dates))
  x_max <- as.numeric(max(dates))
  x_rng <- max(x_max - x_min, 1)
  y_max <- max(values, 1, na.rm = TRUE)

  to_px <- function(d, v) {
    list(
      x = round(pl + (as.numeric(d) - x_min) / x_rng * pw, 1),
      y = round(pt + ph - (v / y_max * ph), 1)
    )
  }

  pts <- purrr::map2(dates, values, to_px)

  poly_pts <- paste(
    purrr::map_chr(pts, function(p) paste0(p$x, ",", p$y)),
    collapse = " "
  )

  first_x <- pts[[1]]$x
  last_x  <- pts[[length(pts)]]$x
  base_y  <- pt + ph
  area_d  <- paste0(
    "M ", first_x, ",", base_y,
    " L ", poly_pts,
    " L ", last_x, ",", base_y, " Z"
  )

  fmt_date <- function(d) format(d, "%b %Y")
  x_labels <- paste0(
    '<text x="', pl, '" y="', sh,
    '" font-size="8" fill="#BFBFBF" text-anchor="start">', fmt_date(min(dates)), '</text>',
    '<text x="', sw - pr, '" y="', sh,
    '" font-size="8" fill="#BFBFBF" text-anchor="end">',   fmt_date(max(dates)), '</text>'
  )

  y_max_lbl <- paste0(
    '<text x="', sw - pr, '" y="', pt + 8,
    '" font-size="8" fill="#BFBFBF" text-anchor="end">', y_max, '</text>'
  )

  paste0(
    '<div style="padding:2px 0 4px">',
    '<svg width="100%" viewBox="0 0 ', sw, ' ', sh,
    '" style="overflow:visible;display:block">',
    '<line x1="', pl, '" y1="', pt + ph,
    '" x2="', sw - pr, '" y2="', pt + ph,
    '" stroke="#F0F0F0" stroke-width="1"/>',
    '<path d="', area_d, '" fill="', colour,
    '" fill-opacity="0.15" stroke="none"/>',
    '<polyline points="', poly_pts,
    '" fill="none" stroke="', colour,
    '" stroke-width="1.5" stroke-linejoin="round" stroke-linecap="round"/>',
    x_labels, y_max_lbl,
    '</svg>',
    '</div>'
  )
}
