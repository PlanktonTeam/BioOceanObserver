#' ZooBiomass UI Function
#'
#' @description Shiny module for visualising the Australian zooplankton biomass
#' generalised linear model output. Renders a full-width Mapbox GL map with a
#' 12-layer monthly SpatRaster (mg m-3) overlaid as a colourised PNG image
#' source. A floating month selector and annual/monthly range toggle sit
#' top-left; a viridis colourbar legend sits bottom-left; a download button
#' allows the user to save the currently visible layer as a GeoTIFF.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ZooBiomass_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Custom JS handler: updates the Mapbox GL image source in-place without
    # rebuilding the map, preserving the user's current zoom and pan position.
    # Uses the same HTMLWidgets.find() + getMap() pattern as the mapgl proxy.
    shiny::tags$script(shiny::HTML('
      Shiny.addCustomMessageHandler("zb_update_image", function(msg) {
        var widget = HTMLWidgets.find("#" + msg.mapId);
        if (!widget) return;
        var map = widget.getMap();
        if (!map) return;
        var src = map.getSource("biomass");
        if (src) {
          src.updateImage({ url: msg.url });
        }
      });
    ')),
    shiny::tags$div(
      class = "zb-map-container",

      # Full-width Mapbox map ------------------------------------------------
      mapgl::mapboxglOutput(ns("map"), width = "100%", height = "calc(100vh - 230px)"),

      # Month selector + range toggle — floating panel top-left --------------
      shiny::tags$div(
        class = "zb-month-filter",
        shiny::tags$div(class = "zb-legend-title", "Month"),
        shiny::selectInput(
          inputId  = ns("month"),
          label    = NULL,
          choices  = setNames(month.abb, month.name),
          selected = "Jan"
        ),
        shiny::tags$div(class = "zb-legend-title", "Colour range"),
        shiny::radioButtons(
          inputId  = ns("range_type"),
          label    = NULL,
          choices  = c("Annual", "Monthly"),
          selected = "Annual",
          inline   = TRUE
        ),
        # Download button sits inside the filter panel for compactness
        shiny::downloadButton(ns("download_tif"), label = "Download .tif",
                              class = "btn-sm zb-download-btn")
      ),

      # Colourbar legend — floating panel bottom-left ------------------------
      shiny::tags$div(
        class = "zb-legend",
        shiny::tags$div(class = "zb-legend-title",
                        "Zooplankton Biomass (mg m\u207B\u00B3)"),
        shiny::tags$div(class = "zb-legend-bar"),
        shiny::uiOutput(ns("legend_labels"))
      )
    )
  )
}


#' ZooBiomass Server Functions
#'
#' @noRd
mod_ZooBiomass_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Guard: exit early if raster data is not available --------------------
    if (is.null(pkg.env$ZooBiomass)) return(invisible(NULL))

    rast_all <- pkg.env$ZooBiomass  # 12-layer SpatRaster, names = month.abb

    # ── Annual 10th/90th percentile (all 12 layers) ───────────────────────
    # Wrapped in a reactive so terra::values() — which reads all pixel data
    # into RAM — is deferred until the user first visits this tab (i.e. until
    # input$month is first available). shiny::reactive() caches the result so
    # it is computed only once per session regardless of how many times it is
    # subsequently referenced.
    annual_range <- shiny::reactive({
      shiny::req(input$month)  # fires only after the tab is first rendered
      vals <- terra::values(rast_all, na.rm = TRUE)
      stats::quantile(vals, probs = c(0.10, 0.90), na.rm = TRUE)
    })

    # ── Bounding box corners for add_image_source ──────────────────────────
    # The raster is stored in EPSG:3857 (Web Mercator). Mapbox GL expects
    # corner coordinates in WGS84 lon/lat degrees. We project the four
    # corners of the raster extent back to EPSG:4326 to get the correct
    # geographic bounding box.
    # mapgl corner order: top-left, top-right, bottom-right, bottom-left.
    rast_ext <- terra::ext(rast_all)
    corners_3857 <- terra::vect(
      matrix(c(rast_ext$xmin, rast_ext$ymax,   # top-left
               rast_ext$xmax, rast_ext$ymax,   # top-right
               rast_ext$xmax, rast_ext$ymin,   # bottom-right
               rast_ext$xmin, rast_ext$ymin),  # bottom-left
             ncol = 2, byrow = TRUE),
      type = "points", crs = terra::crs(rast_all)
    )
    corners_wgs84  <- terra::project(corners_3857, "EPSG:4326")
    corners_coords <- terra::crds(corners_wgs84)  # matrix [lon, lat]
    bbox_corners <- lapply(seq_len(nrow(corners_coords)), function(i) {
      unname(corners_coords[i, ])
    })

    # ── Temp directory for PNG tiles ───────────────────────────────────────
    # A single session-scoped temp dir is created once; the PNG is overwritten
    # on each month/range change so disk usage stays minimal.
    zb_tmp_dir <- tempfile(pattern = "zb_tiles_")
    dir.create(zb_tmp_dir, recursive = TRUE)
    shiny::addResourcePath("zb_tiles", zb_tmp_dir)

    # ── Reactive: selected single-layer raster ─────────────────────────────
    selected_layer <- shiny::reactive({
      shiny::req(input$month)
      rast_all[[input$month]]
    })

    # ── Reactive: active value range (10th/90th percentile) ───────────────
    active_range <- shiny::reactive({
      shiny::req(input$range_type)
      if (input$range_type == "Annual") {
        annual_range()  # annual_range is now a reactive; call it to get the value
      } else {
        # Monthly: recompute for the selected layer only
        vals <- terra::values(selected_layer(), na.rm = TRUE)
        stats::quantile(vals, probs = c(0.10, 0.90), na.rm = TRUE)
      }
    })

    # ── Helper: write colourised PNG and return its Shiny URL ──────────────
    # Produces a standard PNG image using the viridis palette mapped to
    # active_range(). NA cells are rendered as fully transparent.
    # The raster is written row-by-row (north→south) as an RGBA matrix and
    # saved with grDevices::png() so the browser can display it directly.
    # The file is served from the session temp dir via addResourcePath.
    make_raster_png <- function(layer, range) {
      rng_lo <- range[[1]]
      rng_hi <- max(range[[2]], rng_lo + .Machine$double.eps)  # guard zero-width

      # Clamp and rescale to [0, 1]
      layer_scaled <- terra::clamp(layer, lower = rng_lo, upper = rng_hi)
      layer_scaled <- (layer_scaled - rng_lo) / (rng_hi - rng_lo)

      # 256-stop viridis palette (key colour stops)
      pal <- grDevices::colorRampPalette(
        c("#440154", "#443983", "#31688e", "#21908c",
          "#35b779", "#8fd744", "#fde725")
      )(256)

      # Map scaled [0,1] values to palette indices [1,256]
      raw_vals <- terra::values(layer_scaled, na.rm = FALSE)[, 1]
      idx      <- round(raw_vals * 255) + 1L
      is_na    <- is.na(idx)
      idx[is_na] <- 1L  # placeholder; alpha will be 0 for these cells

      # Build RGBA colour strings: opaque for data cells, transparent for NA
      hex_cols        <- pal[idx]
      hex_cols[is_na] <- "#00000000"  # fully transparent

      # Reshape to a nrow x ncol matrix (terra stores values row-major, N→S).
      # grDevices::as.raster() treats row 1 as the top of the image, which
      # matches terra's storage order — no vertical flip needed.
      nr  <- terra::nrow(layer)
      nc  <- terra::ncol(layer)
      col_mat <- matrix(hex_cols, nrow = nr, ncol = nc, byrow = TRUE)

      # Write PNG using the graphics device — pixel dimensions match raster grid
      out_path <- file.path(zb_tmp_dir, "biomass_layer.png")
      grDevices::png(out_path, width = nc, height = nr, bg = "transparent")
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()
      graphics::plot.window(xlim = c(0, nc), ylim = c(0, nr), xaxs = "i", yaxs = "i")
      graphics::rasterImage(grDevices::as.raster(col_mat), 0, 0, nc, nr, interpolate = FALSE)
      grDevices::dev.off()

      # Return a cache-busting URL so the browser fetches the new file
      paste0("zb_tiles/biomass_layer.png?v=", as.integer(Sys.time()))
    }

    # ── Reactive: PNG URL (recomputes on month or range change) ───────────
    raster_url <- shiny::reactive({
      shiny::req(selected_layer(), active_range())
      make_raster_png(selected_layer(), active_range())
    })

    # ── Bounding box for fit_bounds — WGS84 degrees (xmin, ymin, xmax, ymax)
    rast_bbox <- c(
      min(corners_coords[, 1]), min(corners_coords[, 2]),
      max(corners_coords[, 1]), max(corners_coords[, 2])
    )

    # ── Map render — fires ONCE on module initialisation ───────────────────
    # output$map does NOT depend on raster_url() so it never re-renders when
    # the month or colour range changes.  The initial PNG is added here;
    # subsequent updates are handled by the proxy observer below.
    output$map <- mapgl::renderMapboxgl({
      # isolate() reads the initial URL without registering raster_url() as a
      # reactive dependency — this render fires only once at module startup.
      # All subsequent layer changes are handled by the proxy observer below.
      init_url <- shiny::isolate(raster_url())
      shiny::req(init_url)

      mapgl::mapboxgl(
        access_token       = golem::get_golem_options("MAPBOX_PUBLIC_TOKEN"),
        style              = mapgl::mapbox_style("light"),
        projection         = "mercator",
        navigation_control = TRUE,
        min_zoom           = 2,
        max_zoom           = 12
      ) %>%
        mapgl::fit_bounds(bbox = rast_bbox, animate = FALSE) %>%
        mapgl::add_image_source(
          id          = "biomass",
          url         = init_url,
          coordinates = bbox_corners
        ) %>%
        mapgl::add_raster_layer(
          id             = "biomass_layer",
          source         = "biomass",
          raster_opacity = 0.7
        )
    })

    # ── Observer: update image source in-place via custom JS message ───────
    # Fires on every raster_url() change AFTER the initial render.
    # Calls map.getSource('biomass').updateImage({url}) on the client —
    # the correct Mapbox GL JS API for swapping an image source URL without
    # removing/re-adding the source (which would reset zoom and pan).
    shiny::observeEvent(raster_url(), {
      session$sendCustomMessage("zb_update_image", list(
        mapId = session$ns("map"),
        url   = raster_url()
      ))
    }, ignoreInit = TRUE)

    # ── Legend labels (min / max of active range) ──────────────────────────
    output$legend_labels <- shiny::renderUI({
      shiny::req(active_range())
      rng <- active_range()
      shiny::tags$div(
        class = "zb-legend-labels",
        shiny::tags$span(formatC(rng[[1]], format = "g", digits = 3)),
        shiny::tags$span(formatC(rng[[2]], format = "g", digits = 3))
      )
    })

    # ── Download handler: save current layer as GeoTIFF ───────────────────
    output$download_tif <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "ZooBiomass_", input$month, "_D",
          format(Sys.time(), "%Y%m%d", tz = "Australia/Hobart"),
          ".tif"
        )
      },
      content = function(file) {
        terra::writeRaster(selected_layer(), file, overwrite = TRUE)
      }
    )

  }) # /moduleServer
}


## To be copied in the UI:
# mod_ZooBiomass_ui("ZooBiomass_1")

## To be copied in the server:
# mod_ZooBiomass_server("ZooBiomass_1")
