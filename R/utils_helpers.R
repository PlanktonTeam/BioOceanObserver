# Add some additional datasets

#' EOV colour and transformation
#'
#' @noRd
fEOVutilities <- function(vector = "col", Survey = "NRS") {
  if (Survey == "NRS") {
    disp <- data.frame(param = c(
      "PigmentChla_mgm3", "Oxygen_umolL", "PhytoBiomassCarbon_pgL",
      "ShannonPhytoDiversity", "Biomass_mgm3", "ShannonCopepodDiversity", "Nitrate_umolL",
      "Salinity", "Ammonium_umolL", "CTDTemperature_degC", "Silicate_umolL",
      "Phosphate_umolL"
    ))
  } else if (Survey == "CPR") {
    # Change some of the names, but keep the same order so the colours are consistent

    disp <- data.frame(param = c(
      "chl_oc3", "Oxygen_umolL", "PhytoBiomassCarbon_pgm3",
      "ShannonPhytoDiversity", "BiomassIndex_mgm3", "ShannonCopepodDiversity", "Nitrate_umolL",
      "Salinity", "Ammonium_umolL", "SST", "Silicate_umolL",
      "Phosphate_umolL"
    ))
  } else if (Survey == "LTM") {
    # Change some of the names, but keep the same order so the colours are consistent

    disp <- data.frame(param = c(
      "chl_oc3", "Oxygen_umolL", "PhytoBiomassCarbon_pgm3",
      "ShannonPhytoDiversity", "BiomassIndex_mgm3", "ShannonCopepodDiversity", "Nitrate_umolL",
      "Salinity", "Ammonium_umolL", "Temperature_degC", "Silicate_umolL",
      "Phosphate_umolL"
    ))
  }


  disp <- disp %>%
    dplyr::mutate(
      col = pkg.env$col12,
      trans = c(
        "log10", "log10", "log10", "log10", "log10", "identity", "identity",
        "identity", "identity", "identity", "log10", "identity"
      )
    )

  if (vector == "col") {
    # Colours in named vector
    dat <- disp$col
    names(dat) <- disp$param
  } else if (vector == "trans") {
    # Transformations in named vector
    dat <- disp$trans
    names(dat) <- disp$param
  }

  return(dat)
}

#' BOO Plankton Sidebar
#'
#' @noRd
fPlanktonSidebar <- function(id, tabsetPanel_id, dat) {
  ns <- NS(id)

  if (stringr::str_detect(id, "NRS") == TRUE) { # NRS
    choices <- unique(sort(dat$StationName))
    selectedSite <- c("Maria Island", "Port Hacking", "Yongala")
    idSite <- "Site"

    if (stringr::str_detect(id, "Micro") == TRUE) { # Microbes + NRS
      selectedVar <- "Bacterial_Temperature_Index_KD"
    } else if (stringr::str_detect(id, "Zoo") == TRUE) { # Zoo + NRS
      selectedVar <- "Biomass_mgm3"
    } else if (stringr::str_detect(id, "Phyto") == TRUE) { # Phyto + NRS
      selectedVar <- "PhytoAbundance_CellsL"
    }
  } else if (stringr::str_detect(id, "CPR") == TRUE) { # CPR
    choices <- unique(sort(dat$BioRegion))
    selectedSite <- c("Temperate East", "South-east")
    idSite <- "region"
    if (stringr::str_detect(id, "Zoo") == TRUE) { # Zoo + CPR
      selectedVar <- "ZoopAbundance_m3"
    } else if (stringr::str_detect(id, "Phyto") == TRUE) { # Phyto + CPR
      selectedVar <- "PhytoAbundance_Cellsm3"
    }
  } else if (stringr::str_detect(id, "CS") == TRUE) { # Microbes Coastal
    choices <- unique(sort(dat$State))
    selectedSite <- c("GBR")
    idSite <- "Site"
    selectedVar <- "Bacterial_Temperature_Index_KD"
  }

  shiny::sidebarPanel(
    shiny::conditionalPanel(
      condition = "input.phyto == 'ptscpr' | input.zoo == 'ztscpr'",
      shiny::HTML("<p>Note there is very little data in the North and North-west regions<p>")
    ),

    # Put Map, Station names and date slider on all panels
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " <= 5"),
      shiny::plotOutput(ns("plotmap"),
        # height = "300px",
        width = "100%"
      ),
      shiny::HTML("<h3>Select a station:</h3>"),
      shiny::fluidRow(class = "row_multicol", tags$div(
        align = "left",
        class = "multicol",
        shiny::checkboxGroupInput(
          inputId = ns(idSite),
          label = NULL,
          choices = choices,
          selected = selectedSite
        )
      )),
      shiny::HTML("<h3>Dates:</h3>"),
      shiny::sliderInput(ns("DatesSlide"),
        label = NULL,
        min = as.POSIXct("2009-01-01 00:00",
          format = "%Y-%m-%d %H:%M",
          tz = "Australia/Hobart"
        ),
        max = Sys.time(),
        value = c(as.POSIXct("2009-01-01 00:00",
          format = "%Y-%m-%d %H:%M",
          tz = "Australia/Hobart"
        ), Sys.time() - 1), timeFormat = "%m-%Y"
      )
    ),

    # Parameter selection for Microbes
    # All subtabs (ie 1-3) using this input need to be created together
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " <= 3 && input.navbar == 'Microbes'"), # Micro
      shiny::HTML("<h3>Select a parameter:</h3>"),
      shiny::selectInput(
        inputId = ns("parameterm"),
        label = NULL,
        choices = selectedVar,
        selected = selectedVar
      ),
      shiny::htmlOutput(ns("ParamDefm")),
      shiny::checkboxInput(
        inputId = ns("all"),
        label = "Tick for more microbial parameters",
        value = FALSE
      )
    ),


    # Parameter Selection for Plankton (Tabs 1-2)
    # All subtabs (ie 1-2) using this input need to be created together
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " <= 2 && input.navbar != 'Microbes'"),
      shiny::HTML("<h3>Select a parameter:</h3>"),
      shiny::selectInput(
        inputId = ns("parameter"),
        label = NULL,
        choices = planktonr::pr_relabel(unique(dat$Parameters), style = "simple", named = TRUE),
        selected = selectedVar
      ),
      shiny::htmlOutput(ns("ParamDef")),
    ),
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 1 | input.", tabsetPanel_id, " == 2"),
      shiny::checkboxInput(
        inputId = ns("scaler1"),
        label = "Change the plot scale to log10",
        value = FALSE
      ),
    ),
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 3 && input.navbar != 'Microbes'"), # Plankton
      shiny::checkboxInput(
        inputId = ns("scaler3"),
        label = strong("Change the plot scale to proportion"),
        value = FALSE
      ),
    ),
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 3 && input.mic == 'mts' && input.navbar == 'Microbes'"), # MicroNRS
      shiny::selectizeInput(
        inputId = ns("interp"),
        label = shiny::strong("Interpolate data?"),
        choices = c("Interpolate", "Raw data"),
        selected = "Raw data"
      ),
    ),
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 3 && input.mic == 'mtsCS'"), # MicroCoastal
      shiny::HTML("<h3>Overlay trend line?</h3>"),
      shiny::selectizeInput(
        inputId = ns("smoother"),
        label = NULL,
        choices = c("None", "Linear", "Smoother"),
        selected = "None"
      ),
    )
  ) # End of shiny::sidebarpanel
}


#' Generic BOO Plankton Panel
#'
#' @noRd
fPLanktonPanel <- function(id, tabsetPanel_id) {
  ns <- NS(id)
  shiny::mainPanel(
    shiny::tabsetPanel(
      id = tabsetPanel_id, type = "pills",
      shiny::tabPanel("Trend Analysis",
        value = 1,
        shiny::htmlOutput(ns("PlotExp1")),
        plotOutput(ns("timeseries1"), height = "auto") %>%
          shinycssloaders::withSpinner(color = "#0dc5c1"),
        div(
          style = "display:inline-block; float:right; width:60%",
          fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
          fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
          fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action")
        )
      ),
      shiny::tabPanel("Climatologies",
        value = 2,
        shiny::htmlOutput(ns("PlotExp2")),
        plotOutput(ns("timeseries2"), height = 800) %>%
          shinycssloaders::withSpinner(color = "#0dc5c1"),
        div(
          style = "display:inline-block; float:right; width:60%",
          fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
          fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"),
          fButtons(id, button_id = "downloadCode2", label = "R Code Example", Type = "Action")
        )
      ),
      if (!tabsetPanel_id %in% c("NRSmts", "CSmts")) {
        shiny::tabPanel("Functional groups",
          value = 3,
          textOutput(ns("PlotExp3"), container = span),
          plotOutput(ns("timeseries3"), height = "auto") %>%
            shinycssloaders::withSpinner(color = "#0dc5c1"),
          div(
            style = "display:inline-block; float:right; width:60%",
            fButtons(id, button_id = "downloadPlot3", label = "Plot", Type = "Download"),
            fButtons(id, button_id = "downloadData3", label = "Data", Type = "Download"),
            fButtons(id, button_id = "downloadCode3", label = "R Code Example", Type = "Action")
          )
        )
      },
      if (tabsetPanel_id %in% c("NRSmts", "CSmts")) {
        shiny::tabPanel("Trend analysis by depth",
          value = 3,
          shiny::htmlOutput(ns("PlotExp3")),
          plotOutput(ns("timeseries3"), height = "auto") %>%
            shinycssloaders::withSpinner(color = "#0dc5c1"),
          div(
            style = "display:inline-block; float:right; width:60%",
            fButtons(id, button_id = "downloadPlot3", label = "Plot", Type = "Download"),
            fButtons(id, button_id = "downloadData3", label = "Data", Type = "Download"),
            fButtons(id, button_id = "downloadCode3", label = "R Code Example", Type = "Action")
          )
        )
      } # ,
      # if (tabsetPanel_id %in% c("NRSmts")){
      #   shiny::tabPanel("Scatter plots", value = 4,
      #                   h6(textOutput(ns("PlotExp4"), container = span)),
      #                   plotOutput(ns("timeseries4")) %>%
      #                     shinycssloaders::withSpinner(color="#0dc5c1"),
      #                   div(style="display:inline-block; float:right; width:60%",
      #                       fButtons(id, button_id = "downloadPlot4", label = "Plot", Type = "Download"),
      #                       fButtons(id, button_id = "downloadData4", label = "Data", Type = "Download"),
      #                       fButtons(id, button_id = "downloadCode4", label = "R Code Example", Type = "Action"))
      #   )
      # },
      # if (tabsetPanel_id %in% c("CSmts")){
      #   shiny::tabPanel("Traits", value = 5,
      #                   h6(textOutput(ns("PlotExp5"), container = span)),
      #                   plotOutput(ns("timeseries5")) %>%
      #                     shinycssloaders::withSpinner(color="#0dc5c1"),
      #                   div(style="display:inline-block; float:right; width:60%",
      #                       fButtons(id, button_id = "downloadPlot5", label = "Plot", Type = "Download"),
      #                       fButtons(id, button_id = "downloadData5", label = "Data", Type = "Download"),
      #                       fButtons(id, button_id = "downloadCode5", label = "R Code Example", Type = "Action"))
      #   )
      # }
    )
  )
}


#' Generic BOO Plankton Spatial Sidebar
#'
#' @noRd
fSpatialSidebar <- function(id, tabsetPanel_id, dat1, dat2, dat3) {
  ns <- NS(id)

  if (stringr::str_detect(id, "Zoo") == TRUE) { # Phyto
    selectedVar <- "Acartia danae"
    labeltext <- "Select a zooplankton species"
  } else { # Zoo
    selectedVar <- "Tripos furca"
    labeltext <- "Select a phytoplankton species"
  }

  shiny::sidebarPanel(
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 1"),
      selectizeInput(
        inputId = ns("species"), label = labeltext, choices = unique(dat1$Species),
        selected = selectedVar
      ),
      shiny::checkboxInput(
        inputId = ns("scaler1"),
        label = "Change between frequency or Presence/Absence plot",
        value = FALSE
      )
    ),
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 2"),
      selectizeInput(
        inputId = ns("species1"), label = labeltext, choices = unique(dat2$Species),
        selected = selectedVar
      ),
      shiny::p("This is a reduced species list that only contains species with enough data to create an STI plot")
    ),
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 3"),
      selectizeInput(
        inputId = ns("species2"), label = labeltext, choices = unique(dat3$Species),
        selected = selectedVar
      ),
      shiny::p("This is a reduced species list that only contains species with enough data to create a day night plot")
    ),
  )
}



#' Generic BOO Plankton Spatial Panel
#'
#' @noRd
fSpatialPanel <- function(id, tabsetPanel_id) {
  ns <- NS(id)
  shiny::mainPanel(
    tabsetPanel(
      id = tabsetPanel_id, type = "pills",
      tabPanel("Observation maps",
        value = 1,
        p(textOutput(ns("DistMapExp"), container = span)),
        fluidRow(
          shiny::column(
            width = 6,
            style = "padding:0px; margin:0px;",
            shiny::h4("December - February"),
            leaflet::leafletOutput(ns("MapSum"), width = "99%", height = "300px") %>%
              shinycssloaders::withSpinner(color = "#0dc5c1")
          ),
          shiny::column(
            width = 6,
            style = "padding:0px; margin:0px;",
            shiny::h4("March - May"),
            leaflet::leafletOutput(ns("MapAut"), width = "99%", height = "300px") %>%
              shinycssloaders::withSpinner(color = "#0dc5c1")
          ),
          shiny::column(
            width = 6,
            style = "padding:0px; margin:0px;",
            shiny::h4("June - August"),
            leaflet::leafletOutput(ns("MapWin"), width = "99%", height = "300px") %>%
              shinycssloaders::withSpinner(color = "#0dc5c1")
          ),
          shiny::column(
            width = 6,
            style = "padding:0px; margin:0px;",
            shiny::h4("September - November"),
            leaflet::leafletOutput(ns("MapSpr"), width = "99%", height = "300px") %>%
              shinycssloaders::withSpinner(color = "#0dc5c1")
          )
        )
      ),
      tabPanel("Species Temperature Index graphs",
        value = 2,
        shiny::p(textOutput(ns("STIsExp"), container = span)),
        plotOutput(ns("STIs"), height = 700) %>%
          shinycssloaders::withSpinner(color = "#0dc5c1")
      ),
      tabPanel("Species Diurnal Behaviour",
        value = 3,
        shiny::p(textOutput(ns("SDBsExp"), container = span)),
        plotOutput(ns("DNs"), height = 700) %>%
          shinycssloaders::withSpinner(color = "#0dc5c1")
      )
    )
  )
}


#' Generic BOO Environmental Panel
#'
#' @noRd
fEnviroPanel <- function(id) {
  ns <- NS(id)
  shiny::mainPanel(
    shiny::htmlOutput(ns("PlotExp")),
    plotOutput(ns("timeseries1"), height = "auto") %>%
      shinycssloaders::withSpinner(color = "#0dc5c1"),
    shiny::div(
      style = "display:inline-block; float:right; width:60%",
      fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
      if (id == "MoorBGC_ui_1") {
        fButtons(id, button_id = "downloadData2", label = "Data TS", Type = "Download")
      },
      if (id == "MoorBGC_ui_1") {
        fButtons(id, button_id = "downloadData3", label = "Data Clim", Type = "Download")
      },
      if (id != "MoorBGC_ui_1") {
        fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download")
      },
      fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action"),
    )
  )
}

#' Generic BOO Environmental Sidebar
#'
#' @noRd
fEnviroSidebar <- function(id, dat = NULL) {
  ns <- NS(id)

  if (id == "NutrientsBGC_ui_1") {
    selectedVar <- "Silicate_umolL"
    ignoreStat <- c("PH4", "NIN", "ESP") # Stations to ignore
  }
  if (id == "PicoBGC_ui_1") {
    selectedVar <- "Prochlorococcus_cellsmL"
    ignoreStat <- c("PH4", "NIN", "ESP") # Stations to ignore
  }
  if (id == "PigmentsBGC_ui_1") {
    selectedVar <- "TotalChla"
    ignoreStat <- c("PH4") # Stations to ignore
  }
  if (id == "WaterBGC_ui_1") {
    selectedVar <- "CTDTemperature_degC"
    ignoreStat <- c("PH4") # Stations to ignore
  }
  if (id == "MoorBGC_ui_1") {
    ignoreStat <- c("PH4", "NIN", "ESP", "VBM") # Stations to ignore
  }

  shiny::sidebarPanel(
    shiny::plotOutput(ns("plotmap"), width = "100%"),
    shiny::HTML("<h3>Select a station:</h3>"),
    shiny::fluidRow(tags$div(
      align = "left",
      class = "multicol",
      shiny::checkboxGroupInput(
        inputId = ns("station"),
        label = NULL,
        choices = pkg.env$NRSStation %>%
          dplyr::filter(!.data$StationCode %in% ignoreStat) %>%
          dplyr::pull(.data$StationName),
        selected = c("North Stradbroke Island", "Maria Island")
      )
    )),
    if (id != "MoorBGC_ui_1") {
      shiny::conditionalPanel(
        condition = "input.env != 'moor'",
        shiny::HTML("<h3>Select dates:</h3>"),
        sliderInput(ns("date"),
          label = NULL, min = lubridate::ymd(20090101), max = Sys.Date(),
          value = c(lubridate::ymd(20090101), Sys.Date() - 1), timeFormat = "%m-%Y"
        )
      )
    },
    if (id != "MoorBGC_ui_1") {
      shiny::conditionalPanel(
        condition = "input.env != 'moor'",
        shiny::HTML("<h3>Select a parameter:</h3>"),
        shiny::selectInput(
          inputId = ns("parameter"),
          label = NULL,
          choices = planktonr::pr_relabel(unique(dat$Parameters), style = "simple", named = TRUE),
          selected = selectedVar
        ),
        shiny::htmlOutput(ns("ParamDefb")),
      )
    },


    # Select whether to overlay smooth trend line
    if (id %in% c("PigmentsBGC_ui_1")) {
      shiny::conditionalPanel(
        condition = "input.env == 'pigs'",
        shiny::HTML("<h3>Overlay trend line?</h3>"),
        selectizeInput(
          inputId = ns("smoother"),
          label = NULL,
          choices = c("Smoother", "Linear", "None"),
          selected = "None"
        ),
      )
    },

    # Select whether to interpolate
    if (id %in% c("PicoBGC_ui_1", "NutrientsBGC_ui_1")) {
      shiny::conditionalPanel(
        condition = "input.env == 'moor' | input.env == 'pico' | input.env == 'bgc'",
        shiny::HTML("<h3>Interpolate data?</h3>"),
        selectizeInput(
          inputId = ns("interp"),
          label = NULL,
          choices = c("Interpolate", "Raw data"),
          selected = "Interpolate"
        )
      )
    },
    shiny::br(), # Give a bit of space for the menu to expand
    shiny::br()
  )
}

# Generic BOO relationships sidebar panel function
#'
#' @noRd
fRelationSidebar <- function(id, tabsetPanel_id, dat1, dat2, dat3, dat4, dat5) { # dat 1-3 group data vars, dat4 physical, dat5 chemical params
  ns <- NS(id)

  if (stringr::str_detect(id, "CS") == TRUE) {
    ChoiceSite <- unique(sort(dat1$State))
    ChoicesGroupy <- "Microbes - Coastal"
    ChoicesGroupx <- "Physical"
    SelectedVar <- "GBR"
    SelectedGroupy <- "Microbes - Coastal"
    SelectedGroupx <- "Physical"
    selectedParamy <- "Bacterial_Temperature_Index_KD"
    selectedParamx <- "Temperature_degC"
  } else if (stringr::str_detect(id, "NRS") == TRUE) {
    ChoiceSite <- unique(sort(dat1$StationName))
    ChoicesGroupy <- c("Zooplankton", "Phytoplankton", "Microbes - NRS", "Physical", "Chemical")
    ChoicesGroupx <- c("Zooplankton", "Phytoplankton", "Microbes - NRS", "Physical", "Chemical")
    SelectedVar <- "Maria Island"
    SelectedGroupy <- "Zooplankton"
    SelectedGroupx <- "Physical"
    selectedParamy <- "Biomass_mgm3"
    selectedParamx <- "CTD_Temperature_degC"
  } else if (stringr::str_detect(id, "CPR") == TRUE) {
    ChoiceSite <- unique(sort(dat1$BioRegion))
    ChoicesGroupy <- c("Zooplankton", "Phytoplankton", "Physical")
    ChoicesGroupx <- c("Zooplankton", "Phytoplankton", "Physical")
    SelectedVar <- "Temperate East"
    SelectedGroupy <- "Zooplankton"
    SelectedGroupx <- "Physical"
    selectedParamy <- "BiomassIndex_mgm3"
    selectedParamx <- "SST"
  }


  shiny::sidebarPanel(
    shiny::conditionalPanel(
      tags$head(tags$style(HTML("
                              .shiny-split-layout > div {overflow: visible;}
                                    "))),
      condition = "input.navbar == 'Relationships'",
      plotOutput(ns("plotmap")),
      shiny::HTML("<h3>Select a station:</h3>"),
      shiny::checkboxGroupInput(inputId = ns("Site"), label = NULL, choices = ChoiceSite, selected = SelectedVar),
      shiny::HTML("<h4>Select a group & variable for the y axis:</h4>"),
      shiny::splitLayout(
        shiny::selectizeInput(
          inputId = ns("groupy"), label = NULL, choices = ChoicesGroupy,
          selected = SelectedGroupy
        ),
        shiny::selectizeInput(inputId = ns("py"), label = NULL, choices = selectedParamy, selected = selectedParamy)
      ),
      shiny::htmlOutput(ns("ParamDefy")),
      shiny::checkboxInput(
        inputId = ns("all"),
        label = "Tick for more microbial parameters",
        value = FALSE
      ),
    ),
    shiny::conditionalPanel(
      condition = paste0("input.navbar == 'Relationships' && input.", tabsetPanel_id, " == 1"),
      shiny::HTML("<h4>Select a group & variable for the x axis:</h4>"),
      shiny::splitLayout(
        shiny::selectizeInput(
          inputId = ns("groupx"), label = NULL, choices = ChoicesGroupx,
          selected = SelectedGroupx
        ),
        shiny::selectizeInput(inputId = ns("px"), label = NULL, choices = selectedParamx, selected = selectedParamx)
      ),
      shiny::htmlOutput(ns("ParamDefx")),
      shiny::HTML("<h3>Overlay trend line?</h3>"),
      shiny::selectizeInput(
        inputId = ns("smoother"), label = NULL,
        choices = c("Smoother", "Linear", "None"), selected = "None"
      )
    )
  )
}

#' Generic BOO Plankton Panel
#'
#' @noRd
fRelationPanel <- function(id, tabsetPanel_id) {
  ns <- NS(id)
  shiny::mainPanel(
    shiny::tabsetPanel(
      id = tabsetPanel_id, type = "pills",
      shiny::tabPanel("Scatter plots",
        value = 1,
        shiny::htmlOutput(ns("PlotExp1")),
        plotOutput(ns("scatter1")) %>%
          shinycssloaders::withSpinner(color = "#0dc5c1"),
        div(
          style = "display:inline-block; float:right; width:60%",
          fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
          fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download")
        )
      ),
      shiny::tabPanel("Box plots",
        value = 2,
        shiny::htmlOutput(ns("PlotExp2")),
        plotOutput(ns("box2"), height = 800) %>%
          shinycssloaders::withSpinner(color = "#0dc5c1"),
        div(
          style = "display:inline-block; float:right; width:60%",
          fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
          fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download")
        )
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
    if (Type == "Download") {
      shiny::downloadButton(ns(button_id), label = label, )
    } else if (Type == "Action") {
      if (stringr::str_detect(id, "Pol")) {
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/EssentialOceanVariables.html')"
      } else if (stringr::str_detect(id, "Micro")) {
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Microbes.html')"
      } else if (stringr::str_detect(id, "Phyto")) {
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Phytoplankton.html')"
      } else if (stringr::str_detect(id, "Zoo")) {
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Zooplankton.html')"
      } else if (stringr::str_detect(id, "LFish")) {
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/LarvalFish.html')"
      } else if (stringr::str_detect(id, "BGC")) {
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Biogeochemistry.html')"
      } else {
        wsite <- "window.open('https://planktonteam.github.io/planktonr/index.html')"
      }

      shiny::actionButton(ns(button_id),
        label = label,
        icon = shiny::icon("file-code"),
        onclick = wsite
      )
    }
  )
}


#' Download Data - Server Side
#'
#' @noRd
fDownloadButtonServer <- function(input, input_dat, gg_prefix) {
  downloadData <- shiny::downloadHandler(
    filename = function() {
      if (stringr::str_starts(gg_prefix, "Policy")) {
        paste0(gg_prefix, "_", input$Site, "_D", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv") %>%
          stringr::str_replace_all(" ", "")
      } else {
        paste0(
          gg_prefix, "_",
          input$parameter, "_",
          data.frame(StationName = input$Site) %>%
            planktonr::pr_add_StationCode() %>%
            dplyr::arrange(.data$StationCode) %>%
            dplyr::pull(.data$StationCode) %>%
            stringr::str_flatten(), "_",
          lubridate::year(input$DatesSlide[1]), "to", lubridate::year(input$DatesSlide[2]), "_D",
          format(Sys.time(), "%Y%m%d", tz = "Australia/Hobart"), ".csv"
        ) %>%
          stringr::str_replace_all("__", "_") %>% # Replace any double underscores with single ones
          stringr::str_replace_all(" ", "")
      }
    },
    content = function(file) {
      vroom::vroom_write(input_dat(), file, delim = ",")
    }
  )
  return(downloadData)
}


#' Download Plot - Server Side
#'
#' @noRd
fDownloadPlotServer <- function(input, gg_id, gg_prefix, papersize = "A4r") {
  downloadPlot <- downloadHandler(
    filename = function() {
      if ((stringr::str_starts(gg_prefix, "Policy"))) {
        paste0(gg_prefix, "_", input$Site, "_D", format(Sys.time(), "%Y%m%d%H%M%S"), ".png") %>%
          stringr::str_replace_all(" ", "")
      } else {
        paste0(
          gg_prefix, "_",
          input$parameter, "_",
          data.frame(StationName = input$Site) %>%
            planktonr::pr_add_StationCode() %>%
            dplyr::arrange(.data$StationCode) %>%
            dplyr::pull(.data$StationCode) %>%
            stringr::str_flatten(), "_",
          lubridate::year(input$DatesSlide[1]), "to", lubridate::year(input$DatesSlide[2]), "_D",
          format(Sys.time(), "%Y%m%d", tz = "Australia/Hobart"), ".png"
        ) %>%
          stringr::str_replace_all(" ", "")
      }
    },
    content = function(file) {
      if (papersize == "A4r") {
        ggplot2::ggsave(file, plot = gg_id(), device = "png", dpi = 500, width = 297, height = 210, units = "mm")
      } else if (papersize == "A4") {
        ggplot2::ggsave(file, plot = gg_id(), device = "png", dpi = 500, width = 210, height = 297, units = "mm")
      } else if (papersize == "A3") {
        ggplot2::ggsave(file, plot = gg_id(), device = "png", dpi = 500, width = 297, height = 420, units = "mm")
      } else if (papersize == "A3r") {
        ggplot2::ggsave(file, plot = gg_id(), device = "png", dpi = 500, width = 420, height = 297, units = "mm")
      } else if (papersize == "A2") {
        ggplot2::ggsave(file, plot = gg_id(), device = "png", dpi = 500, width = 420, height = 594, units = "mm")
      }
      ## TODO If we include pdf downloads we can use code like this.
      # cairo_pdf fixes an error with displaying unicode symbols.
      # library(Cairo)
      # file = stringr::str_replace(file, ".png", ".pdf")
      # ggplot2::ggsave(file, plot = gg_id, width = 420, height = 594, units = "mm",
      #                 device = cairo_pdf, family="Arial Unicode MS")
    }
  )
}



#' Base leaflet plot for all sample points
#'
#' @noRd
LeafletBase <- function(df, Type = "PA") {
  if (Type == "frequency") {
    leaflet::leaflet(df %>%
      dplyr::distinct(.data$Latitude, .data$Longitude)) %>%
      leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>%
      leaflet::addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        color = "#CCCCCC",
        opacity = 0,
        fillOpacity = 0,
        radius = 0.25,
        group = "Absent"
      )
  } else {
    leaflet::leaflet(df %>%
      dplyr::distinct(.data$Latitude, .data$Longitude)) %>%
      leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>%
      leaflet::addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        color = "#CCCCCC",
        opacity = 1,
        fillOpacity = 1,
        radius = 0.25,
        group = "Absent"
      )
  }
}

#' Base leaflet plot for all sample points with observations for a particular species
#'
#' @noRd
LeafletObs <- function(sdf, name, Type = "PA") {
  Species <- unique(sdf$Species)

  if ("Abundance_1000m3" %in% colnames(sdf)) {
    labs <- lapply(seq(nrow(sdf)), function(i) {
      paste(
        "<strong>Date:</strong>", sdf$SampleTime_Local[i], "<br>",
        "<strong>Latitude:</strong>", sdf$Latitude[i], "<br>",
        "<strong>Longitude:</strong>", sdf$Longitude[i], "<br>",
        "<strong>Count:</strong>", sdf$Count[i], "<br>",
        "<strong>Abundance (1000 m\u207B\u00B3):</strong>", round(sdf$Abundance_1000m3[i], digits = 2), "<br>",
        "<strong>Temperature (\u00B0C):</strong>", sdf$Temperature_degC[i], "<br>",
        "<strong>Depth (m):</strong>", sdf$SampleDepth_m[i], "<br>"
      )
    })
  } else if ("freqfac" %in% colnames(sdf)) {
    labs <- lapply(seq(nrow(sdf)), function(i) {
      paste(
        "<strong>Latitude:</strong>", sdf$Latitude[i], "<br>",
        "<strong>Longitude:</strong>", sdf$Longitude[i], "<br>",
        "<strong>Frequency in sample:</strong>", sdf$freqfac[i], "<br>"
      )
    })
  } else {
    labs <- ""
  }

  if (Type == "frequency") {
    dfCPR <- sdf %>%
      dplyr::filter(.data$Survey == "CPR") %>%
      dplyr::arrange(.data$freqfac)
    dfNRS <- sdf %>%
      dplyr::filter(.data$Survey == "NRS") %>%
      dplyr::arrange(.data$freqfac)

    CPRpal <- leaflet::colorFactor(c("#CCCCCC", "#99CCFF", "#3399FF", "#0066CC", "#003366"), domain = sdf$freqfac)
    NRSpal <- leaflet::colorFactor(c("#CCCCCC", "#CCFFCC", "#99FF99", "#009900", "#006600"), domain = sdf$freqfac)

    leaf <- leaflet::leafletProxy(name, data = sdf) %>%
      leaflet::clearGroup(c("National Reference Stations", "Continuous Plankton Recorder")) %>%
      leaflet::addCircleMarkers(
        data = dfCPR,
        group = "Continuous Plankton Recorder",
        lng = ~Longitude,
        lat = ~Latitude,
        color = ~ CPRpal(freqfac),
        fill = ~ CPRpal(freqfac),
        radius = 3
      ) %>%
      leaflet::addCircleMarkers(
        data = dfNRS,
        group = "National Reference Stations",
        lng = ~Longitude,
        lat = ~Latitude,
        color = ~ NRSpal(freqfac),
        fill = ~ NRSpal(freqfac),
        radius = 3
      ) %>%
      leaflet::addLayersControl( # Layers control
        overlayGroups = c("National Reference Stations", "Continuous Plankton Recorder"),
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = FALSE, fill = NA)
      ) %>%
      leaflet::addLegend("bottomleft",
        pal = CPRpal, group = "Continuous Plankton Recorder",
        values = sdf$freqfac, title = paste(Species, "CPR")
      ) %>%
      leaflet::addLegend("bottomleft",
        pal = NRSpal, group = "National Reference Stations",
        values = sdf$freqfac, title = paste(Species, "NRS")
      )

    htmltools::browsable(
      htmltools::tagList(
        list(
          tags$head(
            tags$style(
              ".leaflet .legend {
                   line-height: 5px;
                   font-size: 5px;
                   }",
              ".leaflet .legend i{
                  width: 5px;
                  height: 5px;
                   }"
            )
          ),
          leaf
        )
      )
    )

    leaf
  } else {
    leaflet::leafletProxy(name, data = sdf) %>%
      leaflet::clearGroup("Present") %>%
      leaflet::addCircleMarkers(
        data = sdf,
        lng = ~Longitude,
        lat = ~Latitude,
        color = "blue",
        opacity = 1,
        fillOpacity = 1,
        radius = 2,
        group = "Present",
        label = lapply(labs, htmltools::HTML)
      ) %>%
      leaflet::addLegend("bottomleft",
        colors = c("blue", "#CCCCCC"),
        labels = c("Seasonal Presence", "Seasonal Absence"),
        title = Species,
        opacity = 1
      )
  }
}



fParamDefServer <- function(selectedData) {
  shiny::renderText({
    paste("<p><strong>", planktonr::pr_relabel(unique(selectedData()$Parameters), style = "plotly"), ":</strong> ",
      pkg.env$ParamDef %>%
        dplyr::filter(.data$Parameter == unique(selectedData()$Parameters)) %>%
        dplyr::pull("Definition"), ".</p>",
      sep = ""
    )
  })
}
