
#' Create interactive Mapbox map of station locations
#'
#' @description Creates a mapboxgl map of station locations with colored points
#' and hover tooltips. Handles NRS, LTM, Coastal, CPR (polygons), GO-SHIP, and
#' HAB (state polygons + station points) survey types.
#'
#' @param sites A character vector of station codes/names/states to highlight
#' @param Survey Which Survey to plot ("NRS", "Coastal", "LTM", "CPR", "GO-SHIP", "HAB")
#' @param Type Must be "Phytoplankton" for SOTS to plot, or "Microbes" to drop NIN & ESP
#'
#' @return A mapboxgl map object ready for renderMapboxgl()
#'
#' @noRd
fMapboxMap <- function(sites, Survey = "NRS", Type = "Zooplankton") {
  
  # --- Survey-specific settings ---
  clon  <- 133.7751
  clat  <- -30.0
  zoom  <- 2.5
  
  if (Survey == "CPR") {
    clon <- 133.7751; clat <- -30.0; zoom <- 2.0
  } else if (Survey == "GO-SHIP") {
    clon <- -170.0; clat <- -40.0; zoom <- 2.0
  } else if (Survey == "HAB") {
    clon <- 150.0; clat <- -32.5; zoom <- 3.0
  } 
  
  # --- Build base map ---
  base_map <- mapgl::mapboxgl(
    access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
    style        = mapgl::mapbox_style("light"),
    center       = c(clon, clat),
    zoom         = zoom,
    projection   = "mercator"
  )
  
  # ---- CPR: fill polygons from planktonr::mbr ----
  if (Survey == "CPR") {
    mbr_df <- planktonr::mbr
    if (!inherits(mbr_df, "sf")) mbr_df <- sf::st_as_sf(mbr_df)
    mbr_df$REGION <- as.character(mbr_df$REGION)
    if (!"Colour" %in% colnames(mbr_df)) mbr_df$Colour <- "#AAAAAA"
    mbr_df$Colour <- as.character(mbr_df$Colour)
    mbr_df$FillCol <- ifelse(
      length(sites) > 0 & mbr_df$REGION %in% sites,
      mbr_df$Colour, "#EEEEEE"
    )
    return(
      base_map %>%
        mapgl::add_fill_layer(
          id           = "regions",
          source       = mbr_df,
          fill_color   = list("get", "FillCol"),
          fill_opacity = 0.7,
          tooltip      = "REGION"
        ) %>%
        mapgl::add_line_layer(
          id           = "regions_outline",
          source       = mbr_df,
          line_color   = "black",
          line_width   = 0.5
        )
    )
  }
  
  # ---- HAB: state fill polygons + station circle markers ----
  if (Survey == "HAB") {
    states_sf <- pkg.env$AusStatesSimple %>%
      dplyr::mutate(
        Selected    = .data$StateCode %in% sites,
        FillColor   = dplyr::if_else(.data$Selected, "#FF0000", "#000000"),
        FillOpacity = dplyr::if_else(.data$Selected, 0.2, 0.0)
      ) %>%
      sf::st_as_sf()
    
    stations_df <- pkg.env$datHABTrip %>%
      dplyr::distinct(.data$StationName, .data$Latitude, .data$Longitude, .data$State) %>%
      dplyr::mutate(
        Selected    = .data$StationName %in% sites,
        DotColor    = dplyr::if_else(.data$Selected, "#000000", "#FFFFFF"),
        DotOpacity  = dplyr::if_else(.data$Selected, 0.5, 0.0),
        DotRadius   = dplyr::if_else(.data$Selected, 4, 0.1)
      ) %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    
    return(
      base_map %>%
        mapgl::add_fill_layer(
          id              = "states",
          source          = states_sf,
          fill_color      = list("get", "FillColor"),
          fill_opacity    = list("get", "FillOpacity"),
          tooltip         = "StationName"
        ) %>%
        mapgl::add_circle_layer(
          id             = "stations",
          source         = stations_df,
          circle_color   = list("get", "DotColor"),
          circle_opacity = list("get", "DotOpacity"),
          circle_radius  = list("get", "DotRadius"),
          tooltip        = "StationName"
        )
    )
  }
  
  # ---- GO-SHIP: circle markers coloured by latitude range ----
  if (Survey == "GO-SHIP") {
    meta_data <- pkg.env$datGSm %>%
      dplyr::distinct(.data$Latitude, .data$Longitude, .keep_all = TRUE) %>%
      dplyr::mutate(StationName = .data$StationName) %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)
    
    lat_min <- if (length(sites) >= 2) sites[1] else min(meta_data$Latitude)
    lat_max <- if (length(sites) >= 2) sites[2] else max(meta_data$Latitude)
    
    # Data-driven colour: red if within selected latitude range, blue otherwise
    color_expr <- list(
      "case",
      list("all",
           list(">=", list("get", "Latitude"), lat_min),
           list("<=", list("get", "Latitude"), lat_max)),
      "#FF0000",
      "#3399FF"
    )
    
    return(
      base_map %>%
        mapgl::add_circle_layer(
          id             = "stations",
          source         = meta_data,
          circle_color   = color_expr,
          circle_opacity = 0.8,
          circle_radius  = 4,
          tooltip        = "StationName"
        )
    )
  }
  
  # ---- NRS / LTM / Coastal: circle markers ----
  if (Survey == "NRS") {
    meta_data <- pkg.env$NRSStation
  } else if (Survey == "LTM") {
    meta_data <- pkg.env$NRSStation %>%
      dplyr::filter(.data$StationCode %in% c("MAI", "PHB", "ROT"))
  } else if (Survey == "Coastal") {
    meta_data <- planktonr::csDAT
    if (inherits(meta_data, "sf")) meta_data <- sf::st_drop_geometry(meta_data)
  }
  
  # Add SOTS for phytoplankton
  if (Type == "Phytoplankton" && !Survey %in% c("CPR", "HAB")) {
    sots <- data.frame(
      StationName = "Southern Ocean Time Series",
      StationCode = "SOTS",
      Latitude    = -47.0,
      Longitude   = 142.0
    )
    meta_data <- dplyr::bind_rows(meta_data, sots)
  } else if (Type == "Microbes") {
    meta_data <- meta_data %>%
      dplyr::filter(!.data$StationCode %in% c("NIN", "ESP"))
  }
  
  # Colour by selection
  if (Survey == "Coastal") {
    meta_data <- meta_data %>%
      dplyr::mutate(
        Selected   = .data$State %in% sites,
        DotColor   = dplyr::if_else(.data$Selected, "#FF0000", "#3399FF"),
        DotRadius  = dplyr::if_else(.data$Selected, 8, 6)
      )
  } else {
    meta_data <- meta_data %>%
      dplyr::mutate(
        Selected   = .data$StationCode %in% sites,
        DotColor   = dplyr::if_else(.data$Selected, "#FF0000", "#3399FF"),
        DotRadius  = dplyr::if_else(.data$Selected, 8, 6)
      )
  }
  
  stations_sf <- meta_data %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  base_map %>%
    mapgl::add_circle_layer(
      id             = "stations",
      source         = stations_sf,
      circle_color   = list("get", "DotColor"),
      circle_opacity = 0.8,
      circle_radius  = list("get", "DotRadius"),
      tooltip        = "StationName"
    )
}


#' Update Mapbox sidebar map using mapboxgl_proxy
#'
#' @description Updates only the map layers on an existing mapboxgl sidebar map
#' without redrawing the entire map. Use this in an observe() block that watches
#' for changes in station selection.
#'
#' @param map_id Character string of the map output ID (e.g., "plotmap")
#' @param session The Shiny session object
#' @param sites A character vector of station codes/names/states to highlight,
#'   or a numeric vector of length 2 for GO-SHIP latitude range
#' @param Survey Which Survey to plot ("NRS", "Coastal", "LTM", "CPR", "GO-SHIP", "HAB")
#' @param Type Must be "Phytoplankton" for SOTS to plot, otherwise has no impact
#'
#' @return NULL (called for side effect of updating the map)
#'
#' @noRd
fMapboxUpdate <- function(map_id, session, sites, Survey = "NRS", Type = "Zooplankton") {
  
  proxy <- mapgl::mapboxgl_proxy(map_id, session = session)
  
  # ---- CPR: update fill polygon colours ----
  if (Survey == "CPR") {
    mbr_df <- planktonr::mbr
    if (!inherits(mbr_df, "sf")) mbr_df <- sf::st_as_sf(mbr_df)
    mbr_df$REGION  <- as.character(mbr_df$REGION)
    if (!"Colour" %in% colnames(mbr_df)) mbr_df$Colour <- "#AAAAAA"
    mbr_df$Colour  <- as.character(mbr_df$Colour)
    mbr_df$FillCol <- ifelse(
      length(sites) > 0 & mbr_df$REGION %in% sites,
      mbr_df$Colour, "#EEEEEE"
    )
    proxy %>%
      mapgl::set_source(layer_id = "regions", source = mbr_df) %>%
      mapgl::set_source(layer_id = "regions_outline", source = mbr_df)
    return(invisible(NULL))
  }
  
  # ---- HAB: update state fill + station circles ----
  if (Survey == "HAB") {
    states_sf <- pkg.env$AusStatesSimple %>%
      dplyr::mutate(
        Selected    = .data$StateCode %in% sites,
        FillColor   = dplyr::if_else(.data$Selected, "#FF0000", "#000000"),
        FillOpacity = dplyr::if_else(.data$Selected, 0.2, 0.0)
      ) %>%
      sf::st_as_sf()
    
    stations_df <- pkg.env$datHABTrip %>%
      dplyr::distinct(.data$StationName, .data$Latitude, .data$Longitude, .data$State) %>%
      dplyr::mutate(
        Selected    = .data$StationName %in% sites,
        DotColor    = dplyr::if_else(.data$Selected, "#000000", "#FFFFFF"),
        DotOpacity  = dplyr::if_else(.data$Selected, 0.5, 0.0),
        DotRadius   = dplyr::if_else(.data$Selected, 4, 0.1)
      ) %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    
    proxy %>%
      mapgl::set_source(layer_id = "states",   source = states_sf) %>%
      mapgl::set_source(layer_id = "stations", source = stations_df)
    return(invisible(NULL))
  }
  
  # ---- GO-SHIP: update paint expression with new latitude range ----
  if (Survey == "GO-SHIP") {
    lat_min <- if (length(sites) >= 2) sites[1] else -90
    lat_max <- if (length(sites) >= 2) sites[2] else  90
    
    color_expr <- list(
      "case",
      list("all",
           list(">=", list("get", "Latitude"), lat_min),
           list("<=", list("get", "Latitude"), lat_max)),
      "#FF0000",
      "#3399FF"
    )
    proxy %>%
      mapgl::set_paint_property(layer_id = "stations",
                                name     = "circle-color",
                                value    = color_expr)
    return(invisible(NULL))
  }
  
  # ---- NRS / LTM / Coastal: update circle marker colours ----
  if (Survey == "NRS") {
    meta_data <- pkg.env$NRSStation
  } else if (Survey == "LTM") {
    meta_data <- pkg.env$NRSStation %>%
      dplyr::filter(.data$StationCode %in% c("MAI", "PHB", "ROT"))
  } else if (Survey == "Coastal") {
    meta_data <- planktonr::csDAT
    if (inherits(meta_data, "sf")) meta_data <- sf::st_drop_geometry(meta_data)
  }
  
  # Add SOTS for phytoplankton
  if (Type == "Phytoplankton" && !Survey %in% c("CPR", "HAB")) {
    sots <- data.frame(
      StationName = "Southern Ocean Time Series",
      StationCode = "SOTS",
      Latitude    = -47.0,
      Longitude   = 142.0
    )
    meta_data <- dplyr::bind_rows(meta_data, sots)
  }
  
  # Colour by selection
  if (Survey == "Coastal") {
    meta_data <- meta_data %>%
      dplyr::mutate(
        Selected  = .data$State %in% sites,
        DotColor  = dplyr::if_else(.data$Selected, "#FF0000", "#3399FF"),
        DotRadius = dplyr::if_else(.data$Selected, 8, 6)
      )
  } else {
    meta_data <- meta_data %>%
      dplyr::mutate(
        Selected  = .data$StationCode %in% sites,
        DotColor  = dplyr::if_else(.data$Selected, "#FF0000", "#3399FF"),
        DotRadius = dplyr::if_else(.data$Selected, 8, 6)
      )
  }
  
  stations_sf <- meta_data %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  proxy %>%
    mapgl::set_source(layer_id = "stations", source = stations_sf)
  
  invisible(NULL)
}





#' Create interactive Mapbox progress map of IMOS plankton sampling coverage
#'
#' @description Replaces \code{planktonr::pr_plot_ProgressMap(interactive = TRUE)}
#'   with a \code{mapboxgl} implementation. Renders four toggleable layers:
#'   Marine Bioregions (CPR polygon fills), CPR samples with phyto/zoo counts,
#'   CPR samples with PCI data only (hidden by default), and NRS stations.
#'
#' @param dat A list with components \code{$NRS} and \code{$CPR}, as stored in
#'   \code{pkg.env$PMapData}. \code{$NRS} must contain \code{Name},
#'   \code{Latitude}, \code{Longitude}, \code{Start_Date}, \code{End_Date},
#'   \code{Samples}. \code{$CPR} must contain \code{Latitude},
#'   \code{Longitude}, \code{ZoopAbundance_m3}, \code{PhytoAbundance_CellsL},
#'   \code{PCI}, \code{Colour}, \code{Name}.
#'
#' @return A \code{mapboxgl} map object suitable for \code{renderMapboxgl()}.
#'
#' @noRd
fProgressMap <- function(dat) {
  
  # ---- Unpack data ----
  df_CPR <- dat$CPR %>%
    dplyr::filter(!is.na(.data$ZoopAbundance_m3) | !is.na(.data$PhytoAbundance_CellsL))
  
  df_PCI <- dat$CPR %>%
    dplyr::filter(is.na(.data$ZoopAbundance_m3) & is.na(.data$PhytoAbundance_CellsL))
  
  df_NRS <- dat$NRS
  
  # ---- Build popup HTML for NRS stations ----
  df_NRS <- df_NRS %>%
    dplyr::mutate(
      end_label = dplyr::if_else(
        lubridate::year(.data$End_Date) < 2020,
        paste0(as.character(.data$End_Date), " (discontinued)"),
        paste0(as.character(.data$End_Date), " (ongoing)")
      ),
      popup_html = paste0(
        "<strong>National Reference Station:</strong> ", .data$Name, "<br>",
        "<strong>First Sampling:</strong> ", as.character(.data$Start_Date), "<br>",
        "<strong>Last Sampling:</strong> ", .data$end_label, "<br>",
        "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
        "<strong>Longitude:</strong> ", .data$Longitude, "<br>",
        "<strong>Number of Sampling Trips:</strong> ", .data$Samples
      )
    )
  
  # ---- Build popup HTML for CPR phyto/zoo samples ----
  df_CPR <- df_CPR %>%
    dplyr::mutate(
      popup_html = paste0(
        "<strong>Bioregion:</strong> ", .data$Name, "<br>",
        "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
        "<strong>Longitude:</strong> ", .data$Longitude, "<br>",
        "<strong>Phytoplankton Abundance (L\u207B\u00B9):</strong> ",
        round(.data$PhytoAbundance_CellsL, 2), "<br>",
        "<strong>Zooplankton Abundance (m\u207B\u00B3):</strong> ",
        round(.data$ZoopAbundance_m3, 2)
      )
    )
  
  # ---- Build popup HTML for CPR PCI-only samples ----
  df_PCI <- df_PCI %>%
    dplyr::mutate(
      popup_html = paste0(
        "<strong>Bioregion:</strong> ", .data$Name, "<br>",
        "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
        "<strong>Longitude:</strong> ", .data$Longitude, "<br>",
        "<strong>Phytoplankton Colour Index:</strong> ", .data$PCI
      )
    )
  
  # ---- Build popup HTML for Coastal stations ----
  cs_colour <- "#2E8B57"
  cs_sf <- planktonr::csDAT %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      popup_html = paste0(
        "<strong>Coastal Station:</strong> ", .data$StationName, "<br>",
        "<strong>State:</strong> ", .data$State, "<br>",
        "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
        "<strong>Longitude:</strong> ", .data$Longitude
      )
    ) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

  # ---- Convert to sf ----
  nrs_sf <- df_NRS %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  cpr_sf <- df_CPR %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  pci_sf <- df_PCI %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  # ---- Marine Bioregions (mbr polygons from planktonr) ----
  mbr_sf <- planktonr::mbr
  if (!inherits(mbr_sf, "sf")) mbr_sf <- sf::st_as_sf(mbr_sf)
  mbr_sf$REGION  <- as.character(mbr_sf$REGION)
  mbr_sf$Colour  <- as.character(mbr_sf$Colour)
  
  # Build bioregion popup from CPRinfo (features description)
  cpr_info <- pkg.env$CPRinfo
  mbr_sf <- mbr_sf %>%
    dplyr::left_join(
      cpr_info %>% dplyr::select("BioRegion", "Features"),
      by = c("REGION" = "BioRegion")
    ) %>%
    dplyr::mutate(
      br_suffix = dplyr::if_else(.data$REGION == "Southern Ocean Region", "", " Bioregion"),
      popup_html = paste0(
        "<strong>The ", .data$REGION, .data$br_suffix, "</strong> is characterised by ",
        dplyr::coalesce(.data$Features, "")
      )
    )
  
  # ---- Build the map ----
  # Centre shifted south and zoom reduced to include the Southern Ocean region
  mapgl::mapboxgl(
    access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
    style        = mapgl::mapbox_style("light"),
    center       = c(134.0, -32.0),
    zoom         = 2.8,
    projection   = "mercator"
  ) %>%
    # Layer 1: Marine Bioregion fill polygons
    mapgl::add_fill_layer(
      id           = "bioregions",
      source       = mbr_sf,
      fill_color   = list("get", "Colour"),
      fill_opacity = 0.4,
      popup        = "popup_html"
    ) %>%
    mapgl::add_line_layer(
      id         = "bioregions_outline",
      source     = mbr_sf,
      line_color = list("get", "Colour"),
      line_width = 1
    ) %>%
    # Layer 2: CPR samples with phyto/zoo counts
    mapgl::add_circle_layer(
      id             = "cpr_counts",
      source         = cpr_sf,
      circle_color   = list("get", "Colour"),
      circle_opacity = 0.8,
      circle_radius  = 5,
      popup          = "popup_html"
    ) %>%
    # Layer 3: CPR samples with PCI data only (hidden by default)
    mapgl::add_circle_layer(
      id             = "cpr_pci",
      source         = pci_sf,
      circle_color   = list("get", "Colour"),
      circle_opacity = 0.8,
      circle_radius  = 3,
      popup          = "popup_html",
      visibility     = "none"
    ) %>%
    # Layer 4: NRS stations (orange, large)
    mapgl::add_circle_layer(
      id             = "nrs_stations",
      source         = nrs_sf,
      circle_color   = "#FFA500",
      circle_opacity = 0.8,
      circle_radius  = 10,
      popup          = "popup_html"
    ) %>%
    # Layer 5: Coastal stations (sea green, square-ish)
    mapgl::add_circle_layer(
      id                  = "coastal_stations",
      source              = cs_sf,
      circle_color        = cs_colour,
      circle_opacity      = 0.85,
      circle_radius       = 8,
      circle_stroke_color = "#ffffff",
      circle_stroke_width = 2,
      popup               = "popup_html"
    ) %>%
    # Layer toggle control — custom checkbox panel (top-right).
    # Each add_control() call MUST have a unique id= or they overwrite each other
    # in map$x$custom_controls (a named list keyed by control_id).
    # HTML attributes use single-quote delimiters; onchange uses double-quotes so
    # JS strings inside can safely use single quotes.
    mapgl::add_control(
      id       = "layer-toggle",
      position = "top-right",
      html = paste0(
        "<div style='background:rgba(255,255,255,0.92);padding:8px 12px;border-radius:6px;",
        "box-shadow:0 1px 4px rgba(0,0,0,0.25);",
        "font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif;",
        "font-size:13px;font-weight:normal;line-height:1.8;'>",
        # NRS stations — checked by default
        "<label style='display:flex;align-items:center;gap:6px;cursor:pointer;font-weight:normal;margin-bottom:2px;'>",
        "<input type='checkbox' checked style='cursor:pointer;width:14px;height:14px;'",
        " onchange=\"var m=this.closest('.mapboxgl-map').map;",
        "m.setLayoutProperty('nrs_stations','visibility',this.checked?'visible':'none');\">",
        "National Reference Stations</label>",
        # Coastal stations — checked by default
        "<label style='display:flex;align-items:center;gap:6px;cursor:pointer;font-weight:normal;margin-bottom:2px;'>",
        "<input type='checkbox' checked style='cursor:pointer;width:14px;height:14px;'",
        " onchange=\"var m=this.closest('.mapboxgl-map').map;",
        "m.setLayoutProperty('coastal_stations','visibility',this.checked?'visible':'none');\">",
        "Coastal Stations</label>",
        # CPR phyto/zoo counts — checked by default
        "<label style='display:flex;align-items:center;gap:6px;cursor:pointer;font-weight:normal;margin-bottom:2px;'>",
        "<input type='checkbox' checked style='cursor:pointer;width:14px;height:14px;'",
        " onchange=\"var m=this.closest('.mapboxgl-map').map;",
        "m.setLayoutProperty('cpr_counts','visibility',this.checked?'visible':'none');\">",
        "CPR (Phyto/Zoo Counts)</label>",
        # CPR PCI only — unchecked by default (hidden on initial render)
        "<label style='display:flex;align-items:center;gap:6px;cursor:pointer;font-weight:normal;margin-bottom:2px;'>",
        "<input type='checkbox' style='cursor:pointer;width:14px;height:14px;'",
        " onchange=\"var m=this.closest('.mapboxgl-map').map;",
        "m.setLayoutProperty('cpr_pci','visibility',this.checked?'visible':'none');\">",
        "CPR (PCI Only)</label>",
        # Marine Bioregions — checked by default
        "<label style='display:flex;align-items:center;gap:6px;cursor:pointer;font-weight:normal;'>",
        "<input type='checkbox' checked style='cursor:pointer;width:14px;height:14px;'",
        " onchange=\"var m=this.closest('.mapboxgl-map').map;",
        "m.setLayoutProperty('bioregions','visibility',this.checked?'visible':'none');",
        "m.setLayoutProperty('bioregions_outline','visibility',this.checked?'visible':'none');\">",
        "Marine Bioregions</label>",
        "</div>"
      )
    ) %>%
    # Title control (top-left) — unique id to avoid overwriting
    mapgl::add_control(
      id       = "map-title",
      html     = "<div style='background:rgba(255,255,255,0.85);padding:4px 10px;font-weight:bold;font-size:16px;border-radius:4px;'>Biological Ocean Observer Sampling Progress</div>",
      position = "top-left"
    ) %>%
    mapgl::add_control(
      id       = "map-subtitle",
      html     = "<div style='background:rgba(255,255,255,0.85);padding:2px 10px;font-size:12px;border-radius:4px;'>Click items of interest for details</div>",
      position = "top-left"
    ) %>%
    # Combined legend (bottom-left) as a custom HTML control.
    # This allows mixed shapes: circle for NRS stations, square for bioregions.
    # No legend title, as requested.
    mapgl::add_control(
      id       = "map-legend",
      position = "bottom-left",
      html     = {
        # Build bioregion rows: coloured square + region name
        mbr_distinct <- mbr_sf %>%
          sf::st_drop_geometry() %>%
          dplyr::distinct(.data$REGION, .keep_all = TRUE)
        
        bioregion_rows <- paste0(
          "<div style='display:flex;align-items:center;gap:6px;margin-bottom:3px;'>",
          "<span style='display:inline-block;width:14px;height:14px;border-radius:2px;",
          "background:", mbr_distinct$Colour, ";flex-shrink:0;'></span>",
          "<span>", mbr_distinct$REGION, "</span>",
          "</div>",
          collapse = ""
        )
        
        paste0(
          "<div style='background:rgba(255,255,255,0.92);padding:8px 12px;border-radius:6px;",
          "box-shadow:0 1px 4px rgba(0,0,0,0.25);",
          "font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif;",
          "font-size:12px;line-height:1.5;max-width:200px;'>",
          # NRS entry: circle marker
          "<div style='display:flex;align-items:center;gap:6px;margin-bottom:3px;'>",
          "<span style='display:inline-block;width:14px;height:14px;border-radius:50%;",
          "background:#FFA500;flex-shrink:0;'></span>",
          "<span>National Reference Stations</span>",
          "</div>",
          # Coastal Stations entry: circle marker
          "<div style='display:flex;align-items:center;gap:6px;margin-bottom:3px;'>",
          "<span style='display:inline-block;width:14px;height:14px;border-radius:50%;",
          "background:", cs_colour, ";flex-shrink:0;'></span>",
          "<span>Coastal Stations</span>",
          "</div>",
          bioregion_rows,
          "</div>"
        )
      }
    )
}







# MAPBOX (mapgl) SPATIAL HELPERS ----
# Used by mod_ZooSpatial and mod_PhytoSpatial main-panel maps.

#' Full Mapbox seasonal map (absence + presence in one render)
#'
#' Builds a complete \code{mapboxgl} map for a single season, including both
#' the grey absence layer (all sample locations) and the coloured presence
#' layer (species observations for that season).  This avoids the proxy timing
#' issue where \code{set_source()} arrives before the map layers are ready.
#'
#' @param df_abs  Data frame of \strong{all} sample locations (used for the
#'   absence layer).  Must contain \code{Latitude} and \code{Longitude}.
#'   Typically \code{pkg.env$fMapDataz} or \code{pkg.env$fMapDatap}.
#' @param df_pres  Data frame of species observations (already filtered to the
#'   selected species, all seasons).  Must contain \code{Latitude},
#'   \code{Longitude}, \code{Season}, \code{Survey}, \code{Species},
#'   \code{freqfac}.  Typically the \code{ZSdatar()} / \code{PSdatar()}
#'   reactive value.
#' @param season_label  Character; one of \code{"December - February"},
#'   \code{"March - May"}, \code{"June - August"},
#'   \code{"September - November"}.
#' @param Type  Character; \code{"frequency"} or \code{"PA"}.
#'
#' @return A \code{mapboxgl} map object suitable for \code{renderMapboxgl()}.
#' @noRd
MapboxSeason <- function(df_abs, df_pres, season_label, Type = "PA") {
  
  # --- Absence layer: all distinct sample locations ---
  abs_sf <- df_abs %>%
    dplyr::distinct(.data$Latitude, .data$Longitude) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  abs_opacity <- if (Type == "frequency") 0 else 1
  
  # --- Presence layer: species observations for this season ---
  sdf <- df_pres %>% dplyr::filter(.data$Season == season_label)
  
  Species <- if (nrow(sdf) > 0) unique(sdf$Species)[1] else ""
  
  if (Type == "frequency") {
    
    freq_levels <- c("Absent", "Seen in 25%", "50%", "75%", "100% of Samples")
    cpr_colors  <- c("#CCCCCC", "#99CCFF", "#3399FF", "#0066CC", "#003366")
    nrs_colors  <- c("#CCCCCC", "#CCFFCC", "#99FF99", "#009900", "#006600")
    
    sdf <- sdf %>%
      dplyr::mutate(
        freqfac_chr = as.character(.data$freqfac),
        dot_color = dplyr::case_when(
          .data$Survey == "CPR" ~ cpr_colors[match(.data$freqfac_chr, freq_levels)],
          .data$Survey == "NRS" ~ nrs_colors[match(.data$freqfac_chr, freq_levels)],
          TRUE ~ "#CCCCCC"
        ),
        popup_html = paste0(
          "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
          "<strong>Longitude:</strong> ", .data$Longitude, "<br>",
          "<strong>Frequency in sample:</strong> ", .data$freqfac_chr
        )
      )
    
    if (nrow(sdf) > 0) {
      pres_sf <- sdf %>%
        sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    } else {
      pres_sf <- abs_sf[0, ]
    }
    
    mapgl::mapboxgl(
      access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
      style        = mapgl::mapbox_style("light"),
      center       = c(134.0, -27.0),
      zoom         = 3.0,
      projection   = "mercator"
    ) %>%
      mapgl::add_circle_layer(
        id             = "absence",
        source         = abs_sf,
        circle_color   = "#CCCCCC",
        circle_opacity = abs_opacity,
        circle_radius  = 2
      ) %>%
      mapgl::add_circle_layer(
        id             = "presence",
        source         = pres_sf,
        circle_color   = list("get", "dot_color"),
        circle_opacity = 1,
        circle_radius  = 3,
        popup          = "popup_html"
      ) %>%
      mapgl::add_categorical_legend(
        legend_title = "CPR",
        values       = freq_levels,
        colors       = cpr_colors,
        position     = "bottom-left",
        add          = FALSE
      ) %>%
      mapgl::add_categorical_legend(
        legend_title = "NRS",
        values       = freq_levels,
        colors       = nrs_colors,
        position     = "bottom-left",
        add          = TRUE
      )
    
  } else {
    
    # PA mode
    if (nrow(sdf) > 0) {
      sdf <- sdf %>%
        dplyr::mutate(
          popup_html = paste0(
            "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
            "<strong>Longitude:</strong> ", .data$Longitude, "<br>",
            "<strong>Frequency in sample:</strong> ", as.character(.data$freqfac)
          )
        )
      pres_sf <- sdf %>%
        sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    } else {
      pres_sf <- abs_sf[0, ]
    }
    
    mapgl::mapboxgl(
      access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
      style        = mapgl::mapbox_style("light"),
      center       = c(134.0, -27.0),
      zoom         = 3.0,
      projection   = "mercator"
    ) %>%
      mapgl::add_circle_layer(
        id             = "absence",
        source         = abs_sf,
        circle_color   = "#CCCCCC",
        circle_opacity = abs_opacity,
        circle_radius  = 2
      ) %>%
      mapgl::add_circle_layer(
        id             = "presence",
        source         = pres_sf,
        circle_color   = "blue",
        circle_opacity = 1,
        circle_radius  = 3,
        popup          = "popup_html"
      ) %>%
      mapgl::add_categorical_legend(
        legend_title = "",
        values       = c("Seasonal Presence", "Seasonal Absence"),
        colors       = c("blue", "#CCCCCC"),
        position     = "bottom-left"
      )
  }
}


#' Base Mapbox map for all sample points (absence layer)
#'
#' Renders the initial mapboxgl map with a grey "absence" circle layer and a
#' transparent placeholder "presence" layer (empty sf).  The presence layer is
#' subsequently updated via \code{MapboxObs()} using \code{mapboxgl_proxy()}.
#'
#' @param df  Data frame with \code{Latitude} and \code{Longitude} columns
#'   (all sample locations — used for the absence layer).
#' @param Type Character; \code{"frequency"} or \code{"PA"} (presence/absence).
#'   Controls whether absence dots are visible (\code{"PA"}) or hidden
#'   (\code{"frequency"}).
#'
#' @return A \code{mapboxgl} map object suitable for \code{renderMapboxgl()}.
#' @noRd
MapboxBase <- function(df, Type = "PA") {
  
  # Absence layer: all distinct sample locations as grey dots
  # NOTE: a non-geometry property column (id) is required so that
  # geojsonsf::sf_geojson() produces a FeatureCollection (length 1)
  # rather than individual geometry strings (length n).
  abs_sf <- df %>%
    dplyr::distinct(.data$Latitude, .data$Longitude) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  # Empty presence layer placeholder (same CRS, zero rows)
  empty_sf <- abs_sf[0, ]
  
  abs_opacity <- if (Type == "frequency") 0 else 1
  
  mapgl::mapboxgl(
    access_token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
    style        = mapgl::mapbox_style("light"),
    center       = c(134.0, -27.0),
    zoom         = 3.0,
    projection   = "mercator"
  ) %>%
    mapgl::add_circle_layer(
      id             = "absence",
      source         = abs_sf,
      circle_color   = "#CCCCCC",
      circle_opacity = abs_opacity,
      circle_radius  = 2
    ) %>%
    mapgl::add_circle_layer(
      id             = "presence",
      source         = empty_sf,
      circle_color   = "blue",
      circle_opacity = 1,
      circle_radius  = 3,
      popup          = "popup_html"
    )
}


#' Update Mapbox presence layer with species observations
#'
#' Uses \code{mapboxgl_proxy()} to update the \code{"presence"} source layer
#' with new species data, and refreshes the legend.
#'
#' @param sdf   Data frame of species observations for one season, filtered to
#'   the selected species.  Must contain \code{Latitude}, \code{Longitude}, and
#'   either \code{freqfac} (frequency mode) or standard columns.
#' @param name  Character; the Shiny output ID of the target map (e.g.
#'   \code{"MapSum"}).
#' @param Type  Character; \code{"frequency"} or \code{"PA"}.
#' @param session  The Shiny session object (passed from the module server).
#'
#' @return Called for side-effects only (proxy update).
#' @noRd
MapboxObs <- function(sdf, name, Type = "PA", session) {
  
  Species <- unique(sdf$Species)
  proxy   <- mapgl::mapboxgl_proxy(name, session = session)
  
  if (Type == "frequency") {
    
    # Frequency palette: CPR (blues) and NRS (greens), matching LeafletObs()
    freq_levels <- c("Absent", "Seen in 25%", "50%", "75%", "100% of Samples")
    cpr_colors  <- c("#CCCCCC", "#99CCFF", "#3399FF", "#0066CC", "#003366")
    nrs_colors  <- c("#CCCCCC", "#CCFFCC", "#99FF99", "#009900", "#006600")
    
    # Map freqfac to colour per survey
    sdf <- sdf %>%
      dplyr::mutate(
        freqfac_chr = as.character(.data$freqfac),
        dot_color = dplyr::case_when(
          .data$Survey == "CPR" ~ cpr_colors[match(.data$freqfac_chr, freq_levels)],
          .data$Survey == "NRS" ~ nrs_colors[match(.data$freqfac_chr, freq_levels)],
          TRUE ~ "#CCCCCC"
        ),
        popup_html = paste0(
          "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
          "<strong>Longitude:</strong> ", .data$Longitude, "<br>",
          "<strong>Frequency in sample:</strong> ", .data$freqfac_chr
        )
      )
    
    pres_sf <- sdf %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    
    proxy %>%
      mapgl::set_source(layer_id = "presence", source = pres_sf) %>%
      mapgl::set_paint_property(layer_id = "presence", name = "circle-color",
                                value = list("get", "dot_color")) %>%
      mapgl::clear_legend() %>%
      mapgl::add_categorical_legend(
        legend_title = paste(Species, "CPR"),
        values       = freq_levels,
        colors       = cpr_colors,
        position     = "bottom-left",
        add          = FALSE
      ) %>%
      mapgl::add_categorical_legend(
        legend_title = paste(Species, "NRS"),
        values       = freq_levels,
        colors       = nrs_colors,
        position     = "bottom-left",
        add          = TRUE
      )
    
  } else {
    
    # Presence/Absence mode
    pres_sf <- sdf %>%
      dplyr::mutate(
        popup_html = paste0(
          "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
          "<strong>Longitude:</strong> ", .data$Longitude, "<br>",
          "<strong>Frequency in sample:</strong> ", as.character(.data$freqfac)
        )
      ) %>%
      sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    
    proxy %>%
      mapgl::set_source(layer_id = "presence", source = pres_sf) %>%
      mapgl::set_paint_property(layer_id = "presence", name = "circle-color",
                                value = "blue") %>%
      mapgl::clear_legend() %>%
      mapgl::add_categorical_legend(
        legend_title = Species,
        values       = c("Seasonal Presence", "Seasonal Absence"),
        colors       = c("blue", "#CCCCCC"),
        position     = "bottom-left"
      )
  }
}

