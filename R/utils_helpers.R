# Add some additional datasets

#' EOV colour and transformation
#'
#' @noRd 
fEOVutilities <- function(vector = "col", Survey = "NRS"){
  
  if (Survey == "NRS"){
    disp <- data.frame(param = c("PigmentChla_mgm3", "Oxygen_umolL", "PhytoBiomassCarbon_pgL", 
                                 "ShannonPhytoDiversity", "Biomass_mgm3", "ShannonCopepodDiversity", "Nitrate_umolL",
                                 "Salinity", "Ammonium_umolL", "CTDTemperature_degC", "Silicate_umolL",
                                 "Phosphate_umolL"))
  } else if (Survey == "CPR"){
    # Change some of the names, but keep the same order so the colours are consistent
    
    disp <- data.frame(param = c("chl_oc3", "Oxygen_umolL", "PhytoBiomassCarbon_pgm3", 
                                 "ShannonPhytoDiversity", "BiomassIndex_mgm3", "ShannonCopepodDiversity", "Nitrate_umolL", 
                                 "Salinity", "Ammonium_umolL", "SST", "Silicate_umolL",
                                 "Phosphate_umolL"))
    
  } else if (Survey == "LTM"){
    # Change some of the names, but keep the same order so the colours are consistent
    
    disp <- data.frame(param = c("chl_oc3", "Oxygen_umolL", "PhytoBiomassCarbon_pgm3",
                                 "ShannonPhytoDiversity", "BiomassIndex_mgm3", "ShannonCopepodDiversity", "Nitrate_umolL", 
                                 "Salinity", "Ammonium_umolL", "Temperature_degC", "Silicate_umolL",
                                 "Phosphate_umolL"))
    
  } else if (Survey == "SOTS"){
    # Change some of the names, but keep the same order so the colours are consistent
    
    disp <- data.frame(param = c("ChlF_mgm3", "DissolvedOxygen_umolkg", "PhytoBiomassCarbon_pgL",
                                 "ShannonPhytoDiversity", "BiomassIndex_mgm3", "ShannonCopepodDiversity", "Nitrate_umolL", 
                                 "Salinity", "Ammonium_umolL", "Temperature_degC", "Silicate_umolL",
                                 "Phosphate_umolL"))
    
  }
  
  disp <- disp %>% 
    dplyr::mutate(col = pkg.env$col12,
                  trans = c("log10", "log10", "log10", "log10", "log10", "identity", "identity",
                            "identity", "identity", "identity", "log10", "identity"))
  
  if (vector == "col"){
    # Colours in named vector
    dat <- disp$col
    names(dat) <- disp$param  
  } else if (vector == "trans"){
    # Transformations in named vector
    dat <- disp$trans
    names(dat) <- disp$param
  }
  
  return(dat)
  
}


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
    clon <- 150.0; clat <- -32.5; zoom <- 4.0
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


#' BOO Plankton Sidebar
#'
#' @noRd 

fPlanktonSidebar <- function(id, tabsetPanel_id, dat, dat1 = NULL){ # dat1 added for SOTS phytoplankton
  
  ns <- NS(id)
  
  if (stringr::str_detect(id, "NRS")) { # NRS
    
    choices <- unique(sort(dat$StationName))
    selectedSite <- c("Maria Island", "Port Hacking", "Yongala")
    min_date <- as.POSIXct('2009-01-01 00:00', format = "%Y-%m-%d %H:%M", tz = "Australia/Hobart")
    idSite <- "site"
    
    if (!is.null(dat1)) {
      df <- dat %>%
        dplyr::bind_rows(dat1) %>%
        planktonr:::pr_reorder()
      choices <- unique(sort(df$StationName))
    }
    
    if (stringr::str_detect(id, "Micro")) { # Microbes + NRS
      selectedVar <- "Bacterial_Temperature_Index_KD"
      choicesp <- pkg.env$choicespNRSm
    } else if (stringr::str_detect(id, "Zoo")) { # Zoo + NRS
      selectedVar <- "Biomass_mgm3"
      choicesp <- pkg.env$choicespNRSz
    } else if (stringr::str_detect(id, "Phyto")) { # Phyto + NRS
      selectedVar <- "PhytoAbundance_CellsL"
      choicesp <- pkg.env$choicespNRSp
    }
  } else if (stringr::str_detect(id, "CPR")) { # CPR
    choices <- unique(sort(dat$BioRegion))
    selectedSite <- c("Temperate East", "South-east")
    idSite <- "site"
    min_date <- as.POSIXct('2009-01-01 00:00', format = "%Y-%m-%d %H:%M", tz = "Australia/Hobart")
    if (stringr::str_detect(id, "Zoo")) { # Zoo + CPR
      selectedVar <- "ZoopAbundance_m3"
      choicesp <- pkg.env$choicespCPRz
    } else if (stringr::str_detect(id, "Phyto")) { # Phyto + CPR
      selectedVar <- "PhytoAbundance_Cellsm3"
      choicesp <- pkg.env$choicespCPRp
    }
  } else if (stringr::str_detect(id, "CS")) { # Microbes Coastal
    choices <- unique(sort(dat$State))
    selectedSite <- c("GBR")
    idSite <- "site"
    selectedVar <- "Bacterial_Temperature_Index_KD"
    min_date <- as.POSIXct('2009-01-01 00:00', format = "%Y-%m-%d %H:%M", tz = "Australia/Hobart")
    choicesp <- pkg.env$choicespCSm
  } else if (stringr::str_detect(id, "HAB")) { # Coastal Phytoplankton
    choices <- unique(sort(dat$State))
    selectedSite <- c("NSW")
    idSite <- "site"
    selectedVar <- "PhytoAbundance_CellsL"
    min_date <- as.POSIXct(paste0(min(lubridate::year(dat$StartDate)), "-01-01 00:00"), format = "%Y-%m-%d %H:%M", tz = "Australia/Hobart")
    choicesp <- pkg.env$choicespHAB
  }
  
  shiny::sidebarPanel(
    
    # Put Map, Station names on all panels except HABS
    shiny::conditionalPanel(
      condition = paste0("!(input.navbar == 'Coastal Phytoplankton')"),
      # Use mapboxglOutput for NRS/CS (interactive points), plotOutput for CPR (static polygons)
      if(stringr::str_detect(id, "CPR")) {
        shiny::tagList(
          shiny::p("Note: There is very little data in the North and North-west regions", class = "small-text"),
          mapgl::mapboxglOutput(ns("plotmap"), height = "400px")
        )
      } else if(!stringr::str_detect(id, "HAB")) {
        shiny::tagList(
          shiny::p("Note: Hover cursor over circles for station name", class = "small-text"),
          mapgl::mapboxglOutput(ns("plotmap"), height = "400px")
        )
      }, 
      shiny::HTML("<h3>Select a station:</h3>"),
      shiny::fluidRow(class = "row_multicol", 
                      tags$div(align = "left", 
                               class = "multicol",
                               shiny::checkboxGroupInput(inputId = ns(idSite), 
                                                         label = NULL,
                                                         choices = choices, 
                                                         selected = selectedSite)))
    ),
    #Add state then multiple stations, and one genus or species options for HABs
    # have to do this in two stages as multiple cannot be changed dynamically with updateselectinput option
    shiny::conditionalPanel(
      condition = paste0("input.navbar == 'Coastal Phytoplankton' && input['", id, "-", tabsetPanel_id, "'] == 1"),
      shiny::tagList(
        shiny::p("Note: Hover cursor over circles for station name", class = "small-text"),
        mapgl::mapboxglOutput(ns("plotmap1"), height = "400px")
      ),
      shiny::HTML("<h3>Select a state:</h3>"),
      shiny::fluidRow(class = "row_multicol",
                      tags$div(align = "left",
                               class = "multicol",
                               shiny::checkboxGroupInput(inputId = ns("statepick1"),
                                                         label = NULL,
                                                         choices = choices, 
                                                         selected = c("NSW")))),
      shiny::HTML("<h3>Select one or more stations:</h3>"),
      shiny::selectInput(inputId = ns("station1"),
                         label = NULL,
                         choices = sort(unique(pkg.env$datHABTrip$StationName)),
                         selected = 'Bar Island',
                         multiple = TRUE),
      shiny::HTML("<h3>Select taxonomic level:</h3>"),
      shiny::fluidRow(class = "row_multicol",
                      tags$div(align = "left",
                               class = "multicol",
                               shiny::radioButtons(inputId = ns("tax1"),
                                                   label = NULL,
                                                   choices = c("genus", "species"),
                                                   selected = "genus"))),
      shiny::HTML("<h3>Select a genera or species:</h3>"),
      shiny::HTML("Only taxa present in the selected stations will be available in this list."),
      shiny::selectInput(inputId = ns("taxgs1"),
                         label = NULL,
                         choices = NULL, 
                         selected = "Alexandrium",
                         multiple = FALSE)
    ), 
    shiny::conditionalPanel(
      condition = paste0("input.navbar == 'Coastal Phytoplankton' && input['", id, "-", tabsetPanel_id, "'] == 2"),
      shiny::tagList(
        shiny::p("Note: Hover cursor over circles for station name", class = "small-text"),
        mapgl::mapboxglOutput(ns("plotmap2"), height = "400px")
      ),
      shiny::HTML("<h3>Select taxonomic level:</h3>"),
      shiny::fluidRow(class = "row_multicol",
                      tags$div(align = "left",
                               class = "multicol",
                               shiny::radioButtons(inputId = ns("tax2"),
                                                   label = NULL,
                                                   choices = c("genus", "species"),
                                                   selected = "genus"))),
      shiny::HTML("<h3>Select one or more genera or species:</h3>"),
      shiny::selectInput(inputId = ns("taxgs2"),
                         label = NULL,
                         choices = NULL, 
                         selected = "Alexandrium",
                         multiple = TRUE),
      shiny::HTML("<h3>Select a state:</h3>"),
      shiny::fluidRow(class = "row_multicol",
                      tags$div(align = "left",
                               class = "multicol",
                               shiny::radioButtons(inputId = ns("statepick2"),
                                                   label = NULL,
                                                   choices = choices, 
                                                   selected = c("NSW")))),
      shiny::HTML("<h3>Select a station:</h3>"),
      shiny::HTML("Only stations where this taxa is present will be available in this list."),
      shiny::selectInput(inputId = ns("station2"),
                         label = NULL,
                         choices = sort(unique(pkg.env$datHABTrip$StationName)),
                         selected = 'Bar Island',
                         multiple = FALSE)
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] <= 5"),
      shiny::HTML("<h3>Dates:</h3>"),
      shiny::sliderInput(ns("DatesSlide"), 
                         label = NULL, 
                         min = min_date, 
                         max = Sys.time(), 
                         value = c(min_date, Sys.time()-1), timeFormat="%m-%Y")),
    # Parameter selection for Microbes
    # All subtabs (ie 1-3) using this input need to be created together
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] <= 3 && input.navbar == 'Microbes'"), # Micro
      shiny::HTML("<h3>Select a parameter:</h3>"),
      shiny::selectInput(inputId = ns("parameterm"), 
                         label = NULL, 
                         choices = selectedVar, 
                         selected = selectedVar),
      shiny::htmlOutput(ns("ParamDefm")),
      shiny::checkboxInput(inputId = ns("all"), 
                           label = "Tick for more microbial parameters", 
                           value = FALSE)
    ),
    
    
    # Parameter Selection for Plankton (Tabs 1-2)
    # All subtabs (ie 1-2) using this input need to be created together
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] <= 2 && input.navbar != 'Microbes'"),
      shiny::HTML("<h3>Select a parameter:</h3>"),
      shiny::selectInput(inputId = ns("parameter"), 
                         label = NULL, 
                         choices = choicesp, 
                         selected = selectedVar),
      shiny::htmlOutput(ns("ParamDef")),
    ),
    
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 1 | input['", id, "-", tabsetPanel_id, "'] == 2"),
      shiny::checkboxInput(inputId = ns("scaler1"), 
                           label = "Change the plot scale to log10",
                           value = FALSE),
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 3 && input.navbar != 'Microbes'"), # Plankton
      shiny::checkboxInput(inputId = ns("scaler3"),
                           label = strong("Change the plot scale to proportion"),
                           value = FALSE),
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 3 && input.mic == 'mts' && input.navbar == 'Microbes'"), # MicroNRS
      shiny::selectizeInput(inputId = ns("interp"),
                            label = shiny::strong("Interpolate data?"),
                            choices = c("Interpolate", "Raw data"),
                            selected = "Raw data"),
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 3 && input.mic == 'mtsCS'"), # MicroCoastal
      shiny::HTML("<h3>Overlay trend line?</h3>"),
      shiny::selectizeInput(inputId = ns("smoother"),
                            label = NULL,
                            choices = c("None", "Linear", "Smoother"),
                            selected = "None"),
    )
  ) # End of shiny::sidebarpanel
  
}


#' Generic BOO Plankton Panel
#' 
#' @noRd
fPLanktonPanel <- function(id, tabsetPanel_id){
  ns <- NS(id)
  shiny::mainPanel(
    shiny::tabsetPanel(id = ns(tabsetPanel_id), type = "pills", selected = "1",
                       if(!tabsetPanel_id %in% c("pHABts")){
                         shiny::tabPanel("Trend analysis", value = "1",
                                         shiny::htmlOutput(ns("PlotExp1")),
                                         plotOutput(ns("timeseries1"), height = "auto") %>% 
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(class="download-button-container",
                                             fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action"))
                         )},
                       if(!tabsetPanel_id %in% c("pHABts")){
                         shiny::tabPanel("Climatologies", value = "2",
                                         shiny::htmlOutput(ns("PlotExp2")),  
                                         plotOutput(ns("timeseries2"), height = 800) %>% 
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(class="download-button-container",
                                             fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode2", label = "R Code Example", Type = "Action"))
                         )},
                       if(tabsetPanel_id %in% c("pHABts")){
                         shiny::tabPanel("Trend analysis by location", value = "1",
                                         shiny::htmlOutput(ns("PlotExp1")),
                                         plotOutput(ns("timeseries1"), height = "auto") %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(class="download-button-container",
                                             fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action"))
                         )},
                       if(tabsetPanel_id %in% c("pHABts")){
                         shiny::tabPanel("Trend analysis by taxa", value = "2",
                                         shiny::htmlOutput(ns("PlotExp2")),  
                                         plotOutput(ns("timeseries2"), height = "auto") %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(class="download-button-container",
                                             fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode2", label = "R Code Example", Type = "Action"))
                         )},
                       if(!tabsetPanel_id %in% c("NRSmts", "CSmts", "pHABts")){
                         shiny::tabPanel("Functional groups", value = "3",
                                         shiny::htmlOutput(ns("PlotExp3"), container = span),  
                                         plotOutput(ns("timeseries3"), height = "auto") %>% 
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(class="download-button-container",
                                             fButtons(id, button_id = "downloadPlot3", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData3", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode3", label = "R Code Example", Type = "Action"))
                         )
                       },
                       if (tabsetPanel_id %in% c("NRSmts", "CSmts")){
                         shiny::tabPanel("Trend analysis by depth", value = 3,
                                         shiny::htmlOutput(ns("PlotExp3")),
                                         plotOutput(ns("timeseries3"), height = "auto") %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(class="download-button-container",
                                             fButtons(id, button_id = "downloadPlot3", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData3", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode3", label = "R Code Example", Type = "Action"))
                         )
                       }
    )
  )
}


#' Generic BOO Plankton Spatial Sidebar
#' 
#' @noRd
fSpatialSidebar <- function(id, tabsetPanel_id, dat1, dat2, dat3){
  ns <- NS(id)
  
  if (stringr::str_detect(id, "Zoo")) { # Zoo
    selectedVar <- "Acartia danae"
    labeltext <- "Select a zooplankton species"
  } else { # Phyto
    selectedVar <- "Tripos furca"
    labeltext <- "Select a phytoplankton species"
  }
  
  shiny::sidebarPanel(
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 1"),
      selectizeInput(inputId = ns('species'), label = labeltext, choices = unique(dat1$Species),
                     selected = selectedVar),
      shiny::checkboxInput(inputId = ns("scaler1"),
                           label = "Change between frequency or Presence/Absence plot",
                           value = FALSE)
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 2"),
      selectizeInput(inputId = ns('species1'), label = labeltext, choices = unique(dat2$Species),
                     selected = selectedVar),
      shiny::p("This is a reduced species list that only contains species with enough data to create an STI plot")
      
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 3"),
      selectizeInput(inputId = ns('species2'), label = labeltext, choices = unique(dat3$Species),
                     selected = selectedVar),
      shiny::p("This is a reduced species list that only contains species with enough data to create a day night plot")
    ),
  )
}



#' Generic BOO Plankton Spatial Panel
#' 
#' @noRd
fSpatialPanel <- function(id, tabsetPanel_id){
  ns <- NS(id)
  shiny::mainPanel(
    tabsetPanel(id = ns(tabsetPanel_id), type = "pills",
                tabPanel("Observation maps", value = 1,
                         p(textOutput(ns("DistMapExp"), container = span)),
                         fluidRow(
                           shiny::column(width = 6,
                                         class = "col-no-spacing",
                                         shiny::h4("December - February"),
                                         mapgl::mapboxglOutput(ns("MapSum"), width = "99%", height = "300px") %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1")),
                           shiny::column(width = 6,
                                         class = "col-no-spacing",
                                         shiny::h4("March - May"),
                                         mapgl::mapboxglOutput(ns("MapAut"), width = "99%", height = "300px") %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1")
                           ),
                           shiny::column(width = 6,
                                         class = "col-no-spacing",
                                         shiny::h4("June - August"),
                                         mapgl::mapboxglOutput(ns("MapWin"), width = "99%", height = "300px") %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1")),
                           shiny::column(width = 6,
                                         class = "col-no-spacing",
                                         shiny::h4("September - November"),
                                         mapgl::mapboxglOutput(ns("MapSpr"), width = "99%", height = "300px") %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1"))
                         )
                ),
                tabPanel("Species Temperature Index graphs", value = 2, 
                         shiny::p(textOutput(ns("STIsExp"), container = span)),
                         plotOutput(ns("STIs"), height = 700) %>% 
                           shinycssloaders::withSpinner(color="#0dc5c1")
                ),
                tabPanel("Species Diurnal Behaviour", value = 3, 
                         shiny::p(textOutput(ns("SDBsExp"), container = span)),
                         plotOutput(ns("DNs"), height = 700) %>% 
                           shinycssloaders::withSpinner(color="#0dc5c1")
                )
    )
  )
}

#' Generic BOO Environmental Sidebar
#'
#' @noRd 
fEnviroSidebar <- function(id, dat = NULL){
  ns <- NS(id)
  
  if (id == "NutrientsBGC_ui_1") {
    selectedVar <- "Silicate_umolL"
    ignoreStat  <- c("PH4", "NIN", "ESP") # Stations to ignore
  } else if (id == "PicoBGC_ui_1") {
    selectedVar <- "Prochlorococcus_cellsmL"
    ignoreStat  <- c("PH4", "NIN", "ESP") # Stations to ignore
  } else if (id == "PigmentsBGC_ui_1") {
    selectedVar <- "TotalChla"
    ignoreStat  <- c("PH4") # Stations to ignore
  } else if (id == "WaterBGC_ui_1") {
    selectedVar <- "CTDTemperature_degC"
    ignoreStat  <- c("PH4") # Stations to ignore
  } else if (id == "MoorBGC_ui_1") {
    selectedVar <- NULL # No parameter selector for moorings
    ignoreStat  <- c("PH4", "NIN", "ESP", "VBM") # Stations to ignore
  }
  
  shiny::sidebarPanel(
    shiny::p("Note: Hover cursor over circles for station name", class = "small-text"),
    mapgl::mapboxglOutput(ns("plotmap"), height = "400px"),
    shiny::HTML("<h3>Select a station:</h3>"),
    shiny::fluidRow(tags$div(align = "left", 
                             class = "multicol",
                             shiny::checkboxGroupInput(inputId = ns("site"),
                                                       label = NULL,
                                                       choices = pkg.env$NRSStation %>% 
                                                         dplyr::filter(!.data$StationCode %in% ignoreStat) %>%
                                                         dplyr::pull(.data$StationName),
                                                       selected = c("North Stradbroke Island", "Maria Island")))),
    
    
    if (id != "MoorBGC_ui_1"){
      shiny::conditionalPanel(
        condition = "input.env != 'moor'",
        shiny::HTML("<h3>Select dates:</h3>"),
        sliderInput(ns("DatesSlide"), label = NULL, min = lubridate::ymd(20090101), max = Sys.Date(),
                    value = c(lubridate::ymd(20090101), Sys.Date()-1), timeFormat="%m-%Y")
      )
    },
    
    if (id != "MoorBGC_ui_1"){
      shiny::conditionalPanel(
        condition = "input.env != 'moor'",
        shiny::HTML("<h3>Select a parameter:</h3>"),
        shiny::selectInput(inputId = ns("parameter"), 
                           label = NULL, 
                           choices = planktonr:::pr_relabel(unique(dat$Parameters), style = "simple", named = TRUE), 
                           selected = selectedVar),
        shiny::htmlOutput(ns("ParamDefb")),
      )
    },
    
    
    # Select whether to overlay smooth trend line
    if (id %in% c("PigmentsBGC_ui_1")){
      shiny::conditionalPanel(
        condition = "input.env == 'pigs'",
        shiny::HTML("<h3>Overlay trend line?</h3>"),
        selectizeInput(inputId = ns("smoother"), 
                       label = NULL, 
                       choices = c("Smoother", "Linear", "None"), 
                       selected = "None"),
      )
    },
    
    # Select whether to interpolate 
    if (id %in% c("PicoBGC_ui_1", "NutrientsBGC_ui_1")){
      shiny::conditionalPanel(
        condition = "input.env == 'moor' | input.env == 'pico' | input.env == 'bgc'",
        shiny::HTML("<h3>Interpolate data?</h3>"),
        selectizeInput(inputId = ns("interp"), 
                       label = NULL, 
                       choices = c("Interpolate", "Raw data"), 
                       selected = "Interpolate")
      )
    },
    
    shiny::br(), # Give a bit of space for the menu to expand
    shiny::br()
  )
  
}

#' Generic BOO Environmental Panel
#' 
#' @noRd
fEnviroPanel <- function(id){
  ns <- NS(id)
  shiny::mainPanel(
    shiny::htmlOutput(ns("PlotExp")),
    plotOutput(ns("timeseries1"), height = "auto") %>% 
      shinycssloaders::withSpinner(color="#0dc5c1"),
    shiny::div(class="download-button-container",
               fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
               if (id == "MoorBGC_ui_1"){fButtons(id, button_id = "downloadData2", label = "Data TS", Type = "Download")},
               if (id == "MoorBGC_ui_1"){fButtons(id, button_id = "downloadData3", label = "Data Clim", Type = "Download")},
               if (id != "MoorBGC_ui_1"){fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download")},
               fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action"),
    )
  )
}



# Generic BOO relationships sidebar panel function
#' 
#' @noRd
fRelationSidebar <- function(id, tabsetPanel_id, dat1, dat2, dat3, dat4, dat5){ #dat 1-3 group data vars, dat4 physical, dat5 chemical params
  ns <- NS(id)
  
  if(stringr::str_detect(id, "CS")){
    ChoiceSite = unique(sort(dat1$State))
    ChoicesGroupy = 'Microbes - Coastal'
    ChoicesGroupx = 'Physical'
    SelectedVar = 'GBR'
    SelectedGroupy = 'Microbes - Coastal'
    SelectedGroupx = 'Physical'
    selectedParamy = 'Bacterial_Temperature_Index_KD'
    selectedParamx = 'Temperature_degC'
  } else if (stringr::str_detect(id, "NRS")){
    ChoiceSite = unique(sort(dat1$StationName))
    ChoicesGroupy = c("Zooplankton", "Phytoplankton", "Microbes - NRS", "Physical", "Chemical")
    ChoicesGroupx = c("Zooplankton", "Phytoplankton", "Microbes - NRS", "Physical", "Chemical")
    SelectedVar = 'Maria Island'
    SelectedGroupy = 'Zooplankton'
    SelectedGroupx = 'Physical'
    selectedParamy = 'Biomass_mgm3'
    selectedParamx = 'CTD_Temperature_degC'
  } else if (stringr::str_detect(id, "CPR")){
    ChoiceSite = unique(sort(dat1$BioRegion))
    ChoicesGroupy = c("Zooplankton", "Phytoplankton", "Physical")
    ChoicesGroupx = c("Zooplankton", "Phytoplankton", "Physical")
    SelectedVar = 'Temperate East'
    SelectedGroupy = 'Zooplankton'
    SelectedGroupx = 'Physical'
    selectedParamy = 'BiomassIndex_mgm3'
    selectedParamx = 'SST'
  }
  
  
  shiny::sidebarPanel(
    shiny::conditionalPanel(
      tags$head(tags$style(HTML("
                              .shiny-split-layout > div {overflow: visible;}
                                    "))),
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] <= 2"),
      
      # Use plotlyOutput for NRS/CS (interactive points), plotOutput for CPR (static polygons)
      if(stringr::str_detect(id, "CPR")) {
        shiny::tagList(
          shiny::p("Note: There is very little data in the North and North-west regions", class = "small-text"),
          mapgl::mapboxglOutput(ns("plotmap"), height = "400px")
        )
      } else {
        shiny::tagList(
          shiny::p("Note: Hover cursor over circles for station name", class = "small-text"),
          mapgl::mapboxglOutput(ns("plotmap"), height = "400px")
        )
      },
      # shiny::p("Note: Hover cursor over circles for station name", class = "small-text"),
      # plotly::plotlyOutput(ns("plotmap"), height = "auto"),   
      shiny::HTML("<h3>Select a station:</h3>"),
      shiny::checkboxGroupInput(inputId = ns("site"), label = NULL, choices = ChoiceSite, selected = SelectedVar),
      shiny::HTML("<h4>Select a group & variable for the y axis:</h4>"),
      shiny::splitLayout(
        shiny::selectizeInput(inputId = ns('groupy'), label = NULL, choices = ChoicesGroupy,
                              selected = SelectedGroupy),
        shiny::selectizeInput(inputId = ns('py'), label = NULL, choices = selectedParamy, selected = selectedParamy)
        
      ),
      shiny::htmlOutput(ns("ParamDefy")),
      shiny::checkboxInput(inputId = ns("all"), 
                           label = "Tick for more microbial parameters", 
                           value = FALSE),
    ),    
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 1"),
      shiny::HTML("<h4>Select a group & variable for the x axis:</h4>"),
      shiny::splitLayout(
        shiny::selectizeInput(inputId = ns('groupx'), label = NULL, choices = ChoicesGroupx,
                              selected = SelectedGroupx),
        shiny::selectizeInput(inputId = ns('px'), label = NULL, choices = selectedParamx, selected = selectedParamx)
      ),
      shiny::htmlOutput(ns("ParamDefx")),
      shiny::HTML("<h3>Overlay trend line?</h3>"),
      shiny::selectizeInput(inputId = ns("smoother"), label = NULL, 
                            choices = c("Smoother", "Linear", "None"), selected = "None")
    )
  )
}

#' Generic BOO Plankton Panel
#' 
#' @noRd
fRelationPanel <- function(id, tabsetPanel_id){
  ns <- NS(id)
  shiny::mainPanel( 
    shiny::tabsetPanel(id = ns(tabsetPanel_id), type = "pills",
                       shiny::tabPanel("Scatter plots", value = 1,
                                       shiny::htmlOutput(ns("PlotExp1")),
                                       plotOutput(ns("scatter1")) %>% 
                                         shinycssloaders::withSpinner(color="#0dc5c1"),
                                       div(class="download-button-container",
                                           fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                                           fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"))
                       ),
                       shiny::tabPanel("Box plots", value = 2,
                                       shiny::htmlOutput(ns("PlotExp2")),  
                                       plotOutput(ns("box2"), height = 800) %>%
                                         shinycssloaders::withSpinner(color="#0dc5c1"),
                                       div(class="download-button-container",
                                           fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
                                           fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"))
                       )
    )
  )
}



#' Download Button
#' 
#' @noRd
fButtons <- function(id, button_id, label, Type = "Download") {
  ns <- NS(id)
  
  shiny::tagList(
    if (Type == "Download"){
      shiny::downloadButton(ns(button_id), label = label, 
      )
    } else if (Type == "Action"){
      
      if (stringr::str_detect(id, "Pol")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/EssentialOceanVariables.html')"
      } else if (stringr::str_detect(id, "Micro")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Microbes.html')"
      } else if (stringr::str_detect(id, "Phyto")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Phytoplankton.html')"
      } else if (stringr::str_detect(id, "Zoo")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Zooplankton.html')"
      } else if (stringr::str_detect(id, "LFish")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/LarvalFish.html')"
      } else if (stringr::str_detect(id, "BGC")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Biogeochemistry.html')"
      } else {
        wsite <- "window.open('https://planktonteam.github.io/planktonr/index.html')"
      }
      
      shiny::actionButton(ns(button_id), label = label,
                          icon = shiny::icon("file-code"),
                          onclick = wsite)
    }
  )
}


#' Download Data - Server Side
#'
#' @noRd 
fDownloadButtonServer <- function(input, input_dat, gg_prefix) {
  
  downloadData <- shiny::downloadHandler(
    filename = function() {
      
      if (stringr::str_starts(gg_prefix, "Policy")){
        paste0(gg_prefix, "_",  input$site, "_D", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv") %>% 
          stringr::str_replace_all( " ", "")
      } else{
        paste0(gg_prefix, "_", 
               input$parameter, "_",
               data.frame(StationName = input$site) %>% 
                 planktonr::pr_add_StationCode() %>% 
                 dplyr::arrange(.data$StationCode) %>% 
                 dplyr::pull(.data$StationCode) %>% 
                 stringr::str_flatten(), "_",
               lubridate::year(input$DatesSlide[1]), "to", lubridate::year(input$DatesSlide[2]), "_D",
               format(Sys.time(), "%Y%m%d", tz = "Australia/Hobart"), ".csv") %>% 
          stringr::str_replace_all("__", "_") %>%  # Replace any double underscores with single ones
          stringr::str_replace_all( " ", "")
      }
    },
    content = function(file) {
      vroom::vroom_write(input_dat(), file, delim = ",")
    })
  return(downloadData)
}


#' Download Plot - Server Side
#'
#' @noRd
fDownloadPlotServer <- function(input, gg_id, gg_prefix, papersize = "A4r") {
  
  downloadPlot <- downloadHandler(
    filename = function() {
      if ((stringr::str_starts(gg_prefix, "Policy"))){
        paste0(gg_prefix, "_", input$site, "_D", format(Sys.time(), "%Y%m%d%H%M%S"), ".png") %>%
          stringr::str_replace_all( " ", "")
      } else{
        if (gg_prefix == "Scatter"){
          param <- paste0(input$px,"_v_",input$py)
        } else {
          param <- input$parameter
        }
        
        paste0(gg_prefix, "_",
               param, "_",
               data.frame(StationName = input$site) %>%
                 planktonr::pr_add_StationCode() %>%
                 dplyr::arrange(.data$StationCode) %>%
                 dplyr::pull(.data$StationCode) %>%
                 stringr::str_flatten(), "_",
               lubridate::year(input$DatesSlide[1]), "to", lubridate::year(input$DatesSlide[2]), "_D",
               format(Sys.time(), "%Y%m%d", tz = "Australia/Hobart"), ".png") %>%
          stringr::str_replace_all( " ", "")
      }
    },
    content = function(file) {
      # Create copyright statement with current date
      copyright_text <- paste0("\u00A9 IMOS Biological Ocean Observer. Downloaded: ",
                               lubridate::now(tzone = "Australia/Hobart") %>%
                                 lubridate::as_date() %>%
                                 format("%d-%b-%Y"), "")
      
      # Create a minimal plot with just the copyright text
      copyright_plot <- ggplot2::ggplot() +
        ggplot2::annotate("text",
                          x = 0.05,
                          y = 0.0,
                          label = copyright_text,
                          angle = 90,
                          hjust = 0,
                          vjust = 0,
                          size = 4,
                          fontface = "italic") +
        ggplot2::scale_x_continuous(limits = c(0,0.1), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(0,2), expand = c(0, 0)) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
      
      # Wrap the existing plot with copyright on the right, aligned to bottom
      gg_copy <- patchwork::wrap_plots(
        gg_id(),
        patchwork::free(copyright_plot, "space", "b"),
        widths = c(1, 0.02)
      )
      
      # NOTE: I have scaled the plot size to force the font size to be smaller in the downloads.
      sc <- 1.5
      # Save with appropriate dimensions
      
      if (gg_prefix == "Climate"){
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 297*sc, height = 200*sc, units = "mm")
      } else if (papersize == "A4r"){
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 297*sc, units = "mm")
      } else if (papersize == "A4") {
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 210*sc, height = 297*sc, units = "mm")
      } else if (papersize == "A3") {
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 297*sc, height = 420*sc, units = "mm")
      } else if (papersize == "A3r") {
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 420*sc, height = 297*sc, units = "mm")
      } else if (papersize == "A2") {
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 420*sc, height = 594*sc, units = "mm")
      }
      
      ## TODO If we include pdf downloads we can use code like this.
      # cairo_pdf fixes an error with displaying unicode symbols.
      # library(Cairo)
      # file = stringr::str_replace(file, ".png", ".pdf")
      # ggplot2::ggsave(file, plot = gg_id, width = 420, height = 594, units = "mm",
      #                 device = cairo_pdf, family="Arial Unicode MS")
    })
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
      circle_radius  = 3,
      popup          = "popup_html"
    ) %>%
    # Layer 3: CPR samples with PCI data only (hidden by default)
    mapgl::add_circle_layer(
      id             = "cpr_pci",
      source         = pci_sf,
      circle_color   = list("get", "Colour"),
      circle_opacity = 0.8,
      circle_radius  = 1,
      popup          = "popup_html"
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
    # Hide PCI-only layer by default (hidden on initial render)
    mapgl::set_layout_property(
      layer_id = "cpr_pci",
      name     = "visibility",
      value    = "none"
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
      html     = "<div style='background:rgba(255,255,255,0.85);padding:4px 10px;font-weight:bold;font-size:16px;border-radius:4px;'>Plankton sampling progress</div>",
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
          bioregion_rows,
          "</div>"
        )
      }
    )
}



fParamDefServer <- function(selectedData){
  shiny::renderText({
    paste("<p><strong>", planktonr:::pr_relabel(unique(selectedData()$Parameters), style = "plotly"), ":</strong> ",
          pkg.env$ParamDef %>%
            dplyr::filter(.data$Parameter %in% unique(selectedData()$Parameters)) %>%
            dplyr::pull("Definition"), ".</p>", sep = "")
  })
}


# ============================================================================
# MAPBOX (mapgl) SPATIAL HELPERS
# Used by mod_ZooSpatial and mod_PhytoSpatial main-panel maps.
# ============================================================================

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


