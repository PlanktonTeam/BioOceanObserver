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
    # Show the standard map + station selector for all modules EXCEPT when the
    # outer Phytoplankton tabsetPanel is on the HAB (Coastal) sub-tab.
    # input.phyto is the outer tabsetPanel(id="phyto") in app_ui.R.
    # For non-Phytoplankton modules input.phyto is undefined → condition is TRUE.
    # Non-HAB modules: show standard map + station selector
    if (!stringr::str_detect(id, "HAB")) {
      shiny::tagList(
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
        shiny::HTML("<h3>Select a station:</h3>"),
        shiny::fluidRow(class = "row_multicol",
                        tags$div(align = "left",
                                 class = "multicol",
                                 shiny::checkboxGroupInput(inputId = ns(idSite),
                                                           label = NULL,
                                                           choices = choices,
                                                           selected = selectedSite)))
      )
    },
    # HAB modules: single shared map for both tabs (same station dots shown regardless of tab)
    if (stringr::str_detect(id, "HAB")) {
      shiny::tagList(
        shiny::p("Note: Hover cursor over circles for station name", class = "small-text"),
        mapgl::mapboxglOutput(ns("plotmap1"), height = "400px"),
        tags$div(class = "hab-analysis-box",
          shiny::HTML("<h3>Select analysis type:</h3>"),
          shiny::radioButtons(inputId = ns("hab_analysis"),
                              label = NULL,
                              choices = c("Analysis by Location" = "location",
                                          "Analysis by Taxa"     = "taxa"),
                              selected = "location",
                              inline = TRUE)
        )
      )
    },
    if (stringr::str_detect(id, "HAB")) {
      shiny::tagList(
        shiny::conditionalPanel(
          condition = paste0("input['", id, "-hab_analysis'] == 'location' || input['", id, "-hab_analysis'] == null"),
          shiny::HTML("<h3>Select a state:</h3>"),
          shiny::fluidRow(class = "row_multicol",
                          tags$div(align = "left",
                                   class = "multicol",
                                   shiny::checkboxGroupInput(inputId = ns("statepick1"),
                                                             label = NULL,
                                                             choices = choices,
                                                             selected = c("NSW")))),
          shiny::HTML("<h3>Select stations:</h3>"),
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
                                                       choices = c("Genus", "Species"),
                                                       selected = "Genus"))),
          shiny::HTML("<h3>Select taxa:</h3>"),
          shiny::HTML("Only taxa present in the selected stations will be available in this list."),
          shiny::selectInput(inputId = ns("taxgs1"),
                             label = NULL,
                             choices = NULL,
                             selected = "Alexandrium",
                             multiple = FALSE)
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", id, "-hab_analysis'] == 'taxa'"),
          shiny::HTML("<h3>Select a state:</h3>"),
          shiny::fluidRow(class = "row_multicol",
                          tags$div(align = "left",
                                   class = "multicol",
                                   shiny::radioButtons(inputId = ns("statepick2"),
                                                       label = NULL,
                                                       choices = choices,
                                                       selected = c("NSW")))),
          shiny::HTML("<h3>Select taxonomic level:</h3>"),
          shiny::fluidRow(class = "row_multicol",
                          tags$div(align = "left",
                                   class = "multicol",
                                   shiny::radioButtons(inputId = ns("tax2"),
                                                       label = NULL,
                                                       choices = c("Genus", "Species"),
                                                       selected = "Genus"))),
          shiny::HTML("<h3>Select taxa:</h3>"),
          shiny::selectInput(inputId = ns("taxgs2"),
                             label = NULL,
                             choices = NULL,
                             selected = "Alexandrium",
                             multiple = TRUE),
          shiny::HTML("<h3>Select a station:</h3>"),
          shiny::HTML("Only stations where this taxa is present will be available in this list."),
          shiny::selectInput(inputId = ns("station2"),
                             label = NULL,
                             choices = sort(unique(pkg.env$datHABTrip$StationName)),
                             selected = 'Bar Island',
                             multiple = FALSE)
        )
      )
    },
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] <= 5"),
      shiny::HTML("<h3>Dates:</h3>"),
      shiny::sliderInput(ns("DatesSlide"), 
                         label = NULL, 
                         min = min_date, 
                         max = Sys.time(), 
                         value = c(min_date, Sys.time()-1), timeFormat="%m-%Y")),
    # Parameter selection for Microbes (tabs 1-3).
    # Wrapped in R-side if() so this block is only ever rendered inside a Micro
    # module's sidebar — no JS input.navbar guard needed or wanted.
    if (stringr::str_detect(id, "Micro")) {
      shiny::conditionalPanel(
        condition = paste0("input['", id, "-", tabsetPanel_id, "'] <= 3"),
        shiny::HTML("<h3>Select a parameter:</h3>"),
        shiny::selectInput(inputId = ns("parameterm"),
                           label = NULL,
                           choices = selectedVar,
                           selected = selectedVar),
        shiny::htmlOutput(ns("ParamDefm")),
        shiny::checkboxInput(inputId = ns("all"),
                             label = "Tick for more microbial parameters",
                             value = FALSE)
      )
    },

    # Parameter Selection for Plankton (tabs 1-2).
    # Wrapped in R-side if() so this block is only ever rendered inside a
    # Phyto/Zoo/CPR/HAB module's sidebar — no JS input.navbar guard needed.
    if (!stringr::str_detect(id, "Micro")) {
      shiny::conditionalPanel(
        condition = paste0("input['", id, "-", tabsetPanel_id, "'] <= 2"),
        shiny::HTML("<h3>Select a parameter:</h3>"),
        shiny::selectInput(inputId = ns("parameter"),
                           label = NULL,
                           choices = choicesp,
                           selected = selectedVar),
        shiny::htmlOutput(ns("ParamDef")),
      )
    },

    # log10 checkbox — shown on tabs 1 and 2 for all module types (including
    # Micro, which also uses input$scaler1 for log10 scaling).
    # Null guard added: when the inner navset_pill has not yet fired its initial
    # input event (tab not yet visited), the value is null; null == 1 is false
    # in JS, so without the guard the checkbox would be hidden on first load.
    shiny::conditionalPanel(
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == null || ",
                         "input['", id, "-", tabsetPanel_id, "'] == 1 || ",
                         "input['", id, "-", tabsetPanel_id, "'] == 2"),
      shiny::checkboxInput(inputId = ns("scaler1"),
                           label = "Change the plot scale to log10",
                           value = FALSE),
    ),

    # Proportion checkbox — tab 3, Phyto/Zoo only.
    # Wrapped in R-side if() to avoid rendering in Micro sidebars.
    if (!stringr::str_detect(id, "Micro")) {
      shiny::conditionalPanel(
        condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 3"),
        shiny::checkboxInput(inputId = ns("scaler3"),
                             label = strong("Change the plot scale to proportion"),
                             value = FALSE),
      )
    },

    # Interpolation selector — tab 3, MicroNRS only.
    # Wrapped in R-side if() so it is only rendered in the MicroTsNRS sidebar.
    # input.mic == 'mts' is the correct discriminator (outer navset_pill value).
    if (stringr::str_detect(id, "Micro") && stringr::str_detect(id, "NRS")) {
      shiny::conditionalPanel(
        condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 3 && input.mic == 'mts'"),
        shiny::selectizeInput(inputId = ns("interp"),
                              label = shiny::strong("Interpolate data?"),
                              choices = c("Interpolate", "Raw data"),
                              selected = "Raw data"),
      )
    },

    # Smoother selector — tab 3, MicroCoastal only.
    # Wrapped in R-side if() so it is only rendered in the MicroTsCS sidebar.
    # input.mic == 'mtsCS' is the correct discriminator.
    if (stringr::str_detect(id, "CS")) {
      shiny::conditionalPanel(
        condition = paste0("input['", id, "-", tabsetPanel_id, "'] == 3 && input.mic == 'mtsCS'"),
        shiny::HTML("<h3>Overlay trend line?</h3>"),
        shiny::selectizeInput(inputId = ns("smoother"),
                              label = NULL,
                              choices = c("None", "Linear", "Smoother"),
                              selected = "None"),
      )
    }
  ) # End of shiny::sidebarpanel
  
}


#' Generic BOO Plankton Panel
#' 
#' @noRd
fPLanktonPanel <- function(id, tabsetPanel_id){
  ns <- NS(id)

  # HAB module: use conditionalPanel driven by the hab_analysis radioButton
  # instead of navset_pill, to avoid bslib input-binding initialization issues.
  if (tabsetPanel_id %in% c("pHABts")) {
    return(shiny::mainPanel(
      shiny::conditionalPanel(
        condition = paste0("input['", id, "-hab_analysis'] == 'location' || input['", id, "-hab_analysis'] == null"),
        shiny::htmlOutput(ns("PlotExp1")),
        plotOutput(ns("timeseries1"), height = "auto") %>%
          shinycssloaders::withSpinner(color="#0dc5c1"),
        div(class="download-button-container",
            fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
            fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
            fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action"))
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", id, "-hab_analysis'] == 'taxa'"),
        shiny::htmlOutput(ns("PlotExp2")),
        plotOutput(ns("timeseries2"), height = "auto") %>%
          shinycssloaders::withSpinner(color="#0dc5c1"),
        div(class="download-button-container",
            fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
            fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"),
            fButtons(id, button_id = "downloadCode2", label = "R Code Example", Type = "Action"))
      )
    ))
  }

  shiny::mainPanel(
    bslib::navset_pill(id = ns(tabsetPanel_id),
                       if(!tabsetPanel_id %in% c("pHABts")){
                         bslib::nav_panel("Trend analysis", value = "1",
                                         shiny::htmlOutput(ns("PlotExp1")),
                                         plotOutput(ns("timeseries1"), height = "auto") %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(class="download-button-container",
                                             fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action"))
                         )},
                       if(!tabsetPanel_id %in% c("pHABts")){
                         bslib::nav_panel("Climatologies", value = "2",
                                         shiny::htmlOutput(ns("PlotExp2")),
                                         plotOutput(ns("timeseries2"), height = 800) %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(class="download-button-container",
                                             fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode2", label = "R Code Example", Type = "Action"))
                         )},
                       if(!tabsetPanel_id %in% c("NRSmts", "CSmts", "pHABts")){
                         bslib::nav_panel("Functional groups", value = "3",
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
                         bslib::nav_panel("Trend analysis by depth", value = "3",
                                         shiny::htmlOutput(ns("PlotExp3")),
                                         plotOutput(ns("timeseries3"), height = "auto") %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(class="download-button-container",
                                             fButtons(id, button_id = "downloadPlot3", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData3", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode3", label = "R Code Example", Type = "Action"))
                         )
                       },
                       selected = "1"
    )
  )
}


#' Generic BOO Plankton Spatial Sidebar
#'
#' Single-page layout: species selector, map type toggle, season radio buttons,
#' STI plot, and diurnal plot all live in the sidebar. The main panel shows a
#' single Mapbox map that updates via proxy when the season or type changes.
#'
#' @noRd
fSpatialSidebar <- function(id, dat1){
  ns <- NS(id)

  if (stringr::str_detect(id, "Zoo")) {
    selectedVar <- "Acartia danae"
    labeltext   <- "Select a zooplankton species"
  } else {
    selectedVar <- "Tripos furca"
    labeltext   <- "Select a phytoplankton species"
  }

  shiny::sidebarPanel(
    width = 4,

    shiny::h4("Species Distribution"),

    # ── Species selector ──────────────────────────────────────────────────────
    selectizeInput(
      inputId  = ns("species"),
      label    = labeltext,
      choices  = unique(dat1$Species),
      selected = selectedVar
    ),

    # ── Map type toggle ───────────────────────────────────────────────────────
    shiny::checkboxInput(
      inputId = ns("scaler1"),
      label   = "Show frequency of observations.",
      value   = FALSE
    ),

    # ── Season selector ───────────────────────────────────────────────────────
    shiny::radioButtons(
      inputId  = ns("season"),
      label    = shiny::strong("Mapped Season:"), 
      choices  = c(
        "Summer (Dec\u2013Feb)"  = "December - February",
        "Autumn (Mar\u2013May)"  = "March - May",
        "Winter (Jun\u2013Aug)"  = "June - August",
        "Spring (Sep\u2013Nov)"  = "September - November"
      ),
      selected = "December - February"
    ),

    shiny::hr(),

    # ── Species Temperature Index ─────────────────────────────────────────────
    shiny::h4("Species Temperature Index"),
    shiny::p(
      "Temperature range at which this species is most common.",
      "A bimodal shape may indicate a sub-species or two species being",
      "identified as the same. Note: This plot is not seasonal.",
      class = "small-text"
    ),
    # Warning message + available species list (rendered server-side when no data)
    shiny::uiOutput(ns("STISpeciesList")),
    plotOutput(ns("STIs"), height = "300px") %>%
      shinycssloaders::withSpinner(color = "#0dc5c1"),

    shiny::hr(),

    # ── Diurnal Behaviour ─────────────────────────────────────────────────────
    shiny::h4("Diurnal Behaviour"),
    shiny::p(
      "Diurnal abundances from CPR data (towed at ~10 m depth). Note: This plot is not seasonal.",
      class = "small-text"
    ),
    # Warning message + available species list (rendered server-side when no data)
    shiny::uiOutput(ns("DNSpeciesList")),
    plotOutput(ns("DNs"), height = "300px") %>%
      shinycssloaders::withSpinner(color = "#0dc5c1")
  )
}


#' Generic BOO Plankton Spatial Panel
#'
#' Single Mapbox map that fills the main panel. Season and type are controlled
#' by radio buttons and a checkbox in the sidebar; the map updates via proxy.
#'
#' @noRd
fSpatialPanel <- function(id){
  ns <- NS(id)
  shiny::mainPanel(
    width = 8,
    shiny::p(
      "Distribution map showing where this species has been observed across",
      "the standard NRS and CPR sampling locations plus ad-hoc sampling",
      "following these methods. Toggle between Presence/Absence and",
      "Frequency views using the sidebar checkbox.",
      class = "small-text"
    ),
    mapgl::mapboxglOutput(ns("MapSeason"), width = "100%", height = "750px") %>%
      shinycssloaders::withSpinner(color = "#0dc5c1")
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
      shiny::tagList(
        shiny::HTML("<h3>Select dates:</h3>"),
        sliderInput(ns("DatesSlide"), label = NULL, min = lubridate::ymd(20090101), max = Sys.Date(),
                    value = c(lubridate::ymd(20090101), Sys.Date()-1), timeFormat="%m-%Y")
      )
    },
    
    if (id != "MoorBGC_ui_1"){
      shiny::tagList(
        shiny::HTML("<h3>Select a parameter:</h3>"),
        shiny::selectInput(inputId = ns("parameter"),
                           label = NULL,
                           choices = planktonr:::pr_relabel(unique(dat$Parameters), style = "simple", named = TRUE),
                           selected = selectedVar),
        shiny::htmlOutput(ns("ParamDefb")),
      )
    },
    
    
    # Select whether to overlay smooth trend line — only for Pigments module
    if (id %in% c("PigmentsBGC_ui_1")){
      shiny::conditionalPanel(
        condition = "input.navbar === 'NRS Pigments'",
        shiny::HTML("<h3>Overlay trend line?</h3>"),
        selectizeInput(inputId = ns("smoother"),
                       label = NULL,
                       choices = c("Smoother", "Linear", "None"),
                       selected = "None"),
      )
    },
    
    # Select whether to interpolate — only for Picoplankton and Nutrients modules
    if (id %in% c("PicoBGC_ui_1", "NutrientsBGC_ui_1")){
      shiny::tagList(
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
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == null || input['", id, "-", tabsetPanel_id, "'] <= 2"),
      
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
      shiny::fluidRow(class = "row_multicol",
                      tags$div(align = "left",
                               class = "multicol",
                               shiny::checkboxGroupInput(inputId = ns("site"), label = NULL,
                                                         choices = ChoiceSite, selected = SelectedVar))),
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
      condition = paste0("input['", id, "-", tabsetPanel_id, "'] == null || input['", id, "-", tabsetPanel_id, "'] == 1"),
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
    bslib::navset_pill(id = ns(tabsetPanel_id),
                       bslib::nav_panel("Scatter plots", value = "1",
                                       shiny::htmlOutput(ns("PlotExp1")),
                                       plotOutput(ns("scatter1")) %>%
                                         shinycssloaders::withSpinner(color="#0dc5c1"),
                                       div(class="download-button-container",
                                           fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                                           fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"))
                       ),
                       bslib::nav_panel("Box plots", value = "2",
                                       shiny::htmlOutput(ns("PlotExp2")),
                                       plotOutput(ns("box2"), height = 800) %>%
                                         shinycssloaders::withSpinner(color="#0dc5c1"),
                                       div(class="download-button-container",
                                           fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
                                           fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"))
                       ),
                       selected = "1"
    )
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


