' Create interactive Mapbox map of station locations
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
    access_token = golem::get_golem_options("MAPBOX_PUBLIC_TOKEN"),
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
    access_token = golem::get_golem_options("MAPBOX_PUBLIC_TOKEN"),
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
      html     = "<div style='background:rgba(255,255,255,0.85);padding:4px 10px;font-weight:bold;font-size:16px;border-radius:4px;'>Sampling locations of data in the Biological Ocean Observer </div>",
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

# Shared frequency palette used by both MapboxSeason() and MapboxSeasonProxy().
# A single unified blue gradient encodes frequency for both CPR and NRS data.
# Survey type is distinguished by marker size: CPR = radius 5 (smaller),
# NRS = radius 8 (larger). This avoids the cognitive load of two colour schemes
# while still allowing the user to distinguish survey types visually.
.freq_levels <- c("Absent", "Seen in 25%", "50%", "75%", "100% of Samples")
.freq_colors <- c("#595959", "#99CCFF", "#3399FF", "#0066CC", "#003366")


#' Build presence sf layers split by survey type
#'
#' Internal helper used by both \code{MapboxSeason()} and
#' \code{MapboxSeasonProxy()}.  Filters \code{df_pres} to the requested season,
#' adds \code{dot_color} and \code{popup_html} columns, and returns a named
#' list with \code{$cpr} and \code{$nrs} sf objects (zero-row if no data).
#'
#' @param df_pres  Species-filtered data frame (all seasons).
#' @param abs_sf   Absence sf used as a zero-row template when no data exist.
#' @param season_label  Character season label.
#' @param Type  \code{"frequency"} or \code{"PA"}.
#' @return Named list: \code{$cpr} sf, \code{$nrs} sf.
#' @noRd
.build_presence_layers <- function(df_pres, abs_sf, season_label, Type) {

  sdf <- df_pres %>% dplyr::filter(.data$Season == season_label)

  if (Type == "frequency") {

    sdf <- sdf %>%
      dplyr::mutate(
        freqfac_chr = as.character(.data$freqfac),
        dot_color   = .freq_colors[match(.data$freqfac_chr, .freq_levels)],
        dot_color   = dplyr::if_else(is.na(.data$dot_color), "#CCCCCC", .data$dot_color),
        popup_html  = paste0(
          "<strong>Survey:</strong> ", .data$Survey, "<br>",
          "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
          "<strong>Longitude:</strong> ", .data$Longitude, "<br>",
          "<strong>Frequency in sample:</strong> ", .data$freqfac_chr
        )
      )

  } else {

    # PA mode: all present observations get the same blue; absent = grey absence layer
    sdf <- sdf %>%
      dplyr::mutate(
        dot_color  = "#3399FF",
        popup_html = paste0(
          "<strong>Survey:</strong> ", .data$Survey, "<br>",
          "<strong>Latitude:</strong> ", .data$Latitude, "<br>",
          "<strong>Longitude:</strong> ", .data$Longitude, "<br>",
          "<strong>Frequency in sample:</strong> ", as.character(.data$freqfac)
        )
      )
  }

  # Split by survey type; fall back to zero-row sf when no data
  to_sf <- function(d) {
    if (nrow(d) > 0) {
      sf::st_as_sf(d, coords = c("Longitude", "Latitude"), crs = 4326)
    } else {
      abs_sf[0, ]
    }
  }

  list(
    cpr = to_sf(dplyr::filter(sdf, .data$Survey == "CPR")),
    nrs = to_sf(dplyr::filter(sdf, .data$Survey == "NRS"))
  )
}


#' Layer-toggle HTML control for the species distribution map
#'
#' Produces the top-right checkbox panel that lets users show/hide the CPR and
#' NRS layers (both absence and presence) independently.  Each checkbox toggles
#' both the absence and presence layer for that survey type so that the size
#' distinction is preserved regardless of whether the species is present.
#' Uses the same inline-JS pattern as \code{fProgressMap()}.
#'
#' @return Character string of HTML.
#' @noRd
.species_map_toggle_html <- function() {
  paste0(
    "<div style='background:rgba(255,255,255,0.92);padding:8px 12px;border-radius:6px;",
    "box-shadow:0 1px 4px rgba(0,0,0,0.25);",
    "font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif;",
    "font-size:13px;font-weight:normal;line-height:1.8;'>",
    "<strong style='display:block;margin-bottom:4px;'>Show layers</strong>",
    # CPR toggle — controls both absence_cpr and presence_cpr
    "<label style='display:flex;align-items:center;gap:6px;cursor:pointer;font-weight:normal;margin-bottom:2px;'>",
    "<input type='checkbox' checked style='cursor:pointer;width:14px;height:14px;'",
    " onchange=\"var m=this.closest('.mapboxgl-map').map;",
    "var v=this.checked?'visible':'none';",
    "m.setLayoutProperty('absence_cpr','visibility',v);",
    "m.setLayoutProperty('presence_cpr','visibility',v);\">",
    "CPR</label>",
    # NRS toggle — controls both absence_nrs and presence_nrs
    "<label style='display:flex;align-items:center;gap:6px;cursor:pointer;font-weight:normal;'>",
    "<input type='checkbox' checked style='cursor:pointer;width:14px;height:14px;'",
    " onchange=\"var m=this.closest('.mapboxgl-map').map;",
    "var v=this.checked?'visible':'none';",
    "m.setLayoutProperty('absence_nrs','visibility',v);",
    "m.setLayoutProperty('presence_nrs','visibility',v);\">",
    "NRS</label>",
    "</div>"
  )
}


#' Legend HTML for the species distribution map
#'
#' Builds a single bottom-left legend that covers both PA and frequency modes.
#' In PA mode only the first two entries (Present / Absent) are meaningful; in
#' frequency mode the full gradient is shown.  A size key below the colour
#' entries explains the CPR (small) vs NRS (large) circle distinction.
#'
#' @param Type  \code{"frequency"} or \code{"PA"}.
#' @return Character string of HTML.
#' @noRd
.species_map_legend_html <- function(Type) {

  if (Type == "frequency") {
    colour_rows <- paste0(
      "<div style='display:flex;align-items:center;gap:6px;margin-bottom:3px;'>",
      "<span style='display:inline-block;width:12px;height:12px;border-radius:50%;",
      "background:", .freq_colors, ";flex-shrink:0;'></span>",
      "<span>", .freq_levels, "</span>",
      "</div>",
      collapse = ""
    )
    title_html <- "<strong style='display:block;margin-bottom:4px;'>Frequency</strong>"
  } else {
    colour_rows <- paste0(
      "<div style='display:flex;align-items:center;gap:6px;margin-bottom:3px;'>",
      "<span style='display:inline-block;width:12px;height:12px;border-radius:50%;",
      "background:#3399FF;flex-shrink:0;'></span>",
      "<span>Present</span>",
      "</div>",
      "<div style='display:flex;align-items:center;gap:6px;margin-bottom:3px;'>",
      "<span style='display:inline-block;width:12px;height:12px;border-radius:50%;",
      "background:#595959;flex-shrink:0;'></span>",
      "<span>Absent</span>",
      "</div>"
    )
    title_html <- "<strong style='display:block;margin-bottom:4px;'>Presence / Absence</strong>"
  }

  # Size key: CPR (small) vs NRS (large)
  size_key <- paste0(
    "<hr style='margin:6px 0;border-color:#ddd;'>",
    "<div style='display:flex;align-items:center;gap:6px;margin-bottom:3px;'>",
    "<span style='display:inline-block;width:10px;height:10px;border-radius:50%;",
    "background:#666;flex-shrink:0;'></span>",
    "<span>CPR</span>",
    "</div>",
    "<div style='display:flex;align-items:center;gap:6px;'>",
    "<span style='display:inline-block;width:16px;height:16px;border-radius:50%;",
    "background:#666;flex-shrink:0;'></span>",
    "<span>NRS </span>",
    "</div>"
  )

  paste0(
    "<div style='background:rgba(255,255,255,0.92);padding:8px 12px;border-radius:6px;",
    "box-shadow:0 1px 4px rgba(0,0,0,0.25);",
    "font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif;",
    "font-size:12px;line-height:1.5;max-width:180px;'>",
    title_html,
    colour_rows,
    size_key,
    "</div>"
  )
}

#' Title HTML for the species distribution map
#'
#' Builds a single top-left legend that covers both PA and frequency modes.
#'
#' @param Season Character string of season 
#' @return Character string of HTML.
#' @noRd
.species_map_title_style <- function(Season){
  paste0(
    "<div style='background:rgba(255,255,255,0.92);padding:8px 12px;border-radius:6px;",
    "box-shadow:0 1px 4px rgba(0,0,0,0.25);",
    "font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif;",
    "font-size:12px;line-height:1.5;max-width:180px;font-weight:bold;'>",
    Season,
    "</div>"
  )  
}

#' Full Mapbox seasonal map (absence + presence in one render)
#'
#' Builds a complete \code{mapboxgl} map for a single season.  The absence
#' layer (grey dots, all sample locations) is always visible.  The presence
#' layer is split into two sub-layers — \code{"presence_cpr"} (radius 5) and
#' \code{"presence_nrs"} (radius 8) — so the user can toggle each survey type
#' independently via the top-right control.  Both layers share the same unified
#' blue gradient palette (\code{.freq_colors}) in frequency mode, or a flat
#' blue in PA mode.
#'
#' @param df_abs  Data frame of \strong{all} sample locations (absence layer).
#'   Must contain \code{Latitude} and \code{Longitude}.
#' @param df_pres  Species-filtered data frame (all seasons).  Must contain
#'   \code{Latitude}, \code{Longitude}, \code{Season}, \code{Survey},
#'   \code{Species}, \code{freqfac}.
#' @param season_label  Character; one of \code{"December - February"},
#'   \code{"March - May"}, \code{"June - August"},
#'   \code{"September - November"}.
#' @param Type  Character; \code{"frequency"} or \code{"PA"}.
#'
#' @return A \code{mapboxgl} map object suitable for \code{renderMapboxgl()}.
#' @noRd
MapboxSeason <- function(df_abs, df_pres, season_label, Type = "PA") {

  # --- Absence layers: all distinct sample locations split by survey type ---
  # Splitting by Survey ensures NRS locations are always rendered at radius 8
  # and CPR locations at radius 5, regardless of whether the species is present.
  # This is the key fix: a single merged absence layer would force all absence
  # dots to the same radius, making NRS absence dots incorrectly small.
  abs_cpr_sf <- df_abs %>%
    dplyr::filter(.data$Survey == "CPR") %>%
    dplyr::distinct(.data$Latitude, .data$Longitude) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

  abs_nrs_sf <- df_abs %>%
    dplyr::filter(.data$Survey == "NRS") %>%
    dplyr::distinct(.data$Latitude, .data$Longitude) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

  # Use the CPR absence sf as the zero-row template for .build_presence_layers()
  abs_template_sf <- df_abs %>%
    dplyr::distinct(.data$Latitude, .data$Longitude) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

  # --- Split presence data into CPR and NRS layers ---
  layers <- .build_presence_layers(df_pres, abs_template_sf, season_label, Type)
  
  # --- Build map ---
  mapgl::mapboxgl(
    access_token = golem::get_golem_options("MAPBOX_PUBLIC_TOKEN"),
    style        = mapgl::mapbox_style("light"),
    center       = c(134.0, -27.0),
    zoom         = 3.0,
    projection   = "mercator"
  ) %>%
    # CPR absence dots — radius 5 (smaller), grey
    mapgl::add_circle_layer(
      id             = "absence_cpr",
      source         = abs_cpr_sf,
      circle_color   = "#595959",
      circle_opacity = 0.6,
      circle_radius  = 5
    ) %>%
    # NRS absence dots — radius 8 (larger), grey
    mapgl::add_circle_layer(
      id             = "absence_nrs",
      source         = abs_nrs_sf,
      circle_color   = "#595959",
      circle_opacity = 0.6,
      circle_radius  = 8
    ) %>%
    # CPR presence layer — radius 5, coloured by frequency/PA
    mapgl::add_circle_layer(
      id             = "presence_cpr",
      source         = layers$cpr,
      circle_color   = list("get", "dot_color"),
      circle_opacity = 0.9,
      circle_radius  = 5,
      popup          = "popup_html"
    ) %>%
    # NRS presence layer — radius 8, coloured by frequency/PA
    mapgl::add_circle_layer(
      id             = "presence_nrs",
      source         = layers$nrs,
      circle_color   = list("get", "dot_color"),
      circle_opacity = 0.9,
      circle_radius  = 8,
      popup          = "popup_html"
    ) %>%
    # Layer toggle — top-right
    mapgl::add_control(
      id       = "layer-toggle",
      position = "top-right",
      html     = .species_map_toggle_html()
    ) %>%
    # Single legend — bottom-left
    mapgl::add_control(
      id       = "map-legend",
      position = "bottom-left",
      html     = .species_map_legend_html(Type)
    ) %>%
    mapgl::add_control(
      id = 'season-label',
      position = "top-left",
      html = .species_map_title_style(season_label)
    )
}


#' Update Mapbox presence layers via proxy (season or type change)
#'
#' Uses \code{mapboxgl_proxy()} to update the \code{"presence_cpr"} and
#' \code{"presence_nrs"} source layers and refresh the legend control without
#' rebuilding the entire map.  Call this from an \code{observeEvent()} that
#' watches \code{input$season} or \code{input$scaler1}.
#'
#' @param df_abs   Data frame of all sample locations (for the zero-row
#'   template when no data exist for a survey type in this season).
#' @param df_pres  Species-filtered data frame (all seasons).
#' @param season_label  Character season label.
#' @param Type  \code{"frequency"} or \code{"PA"}.
#' @param map_id  Character; the Shiny output ID of the target map
#'   (e.g. \code{"MapSeason"}).
#' @param session  The Shiny session object.
#'
#' @return Called for side-effects only (proxy update).
#' @noRd
MapboxSeasonProxy <- function(df_abs, df_pres, season_label, Type = "PA",
                              map_id, session) {

  # Build the absence sf template (needed for zero-row fallback)
  abs_sf <- df_abs %>%
    dplyr::distinct(.data$Latitude, .data$Longitude) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

  layers <- .build_presence_layers(df_pres, abs_sf, season_label, Type)

  proxy <- mapgl::mapboxgl_proxy(map_id, session = session)

  proxy %>%
    mapgl::set_source(layer_id = "presence_cpr", source = layers$cpr) %>%
    mapgl::set_source(layer_id = "presence_nrs", source = layers$nrs) %>%
    mapgl::set_paint_property(
      layer_id = "presence_cpr",
      name     = "circle-color",
      value    = list("get", "dot_color")
    ) %>%
    mapgl::set_paint_property(
      layer_id = "presence_nrs",
      name     = "circle-color",
      value    = list("get", "dot_color")
    ) 
  
  invisible(NULL)
}

