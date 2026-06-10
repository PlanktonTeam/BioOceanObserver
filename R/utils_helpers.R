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



fParamDefServer <- function(selectedData){
  shiny::renderText({
    paste("<p><strong>", planktonr:::pr_relabel(unique(selectedData()$Parameters), style = "plotly"), ":</strong> ",
          pkg.env$ParamDef %>%
            dplyr::filter(.data$Parameter %in% unique(selectedData()$Parameters)) %>%
            dplyr::pull("Definition"), ".</p>", sep = "")
  })
}


