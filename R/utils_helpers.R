#' BOO Plankton Sidebar
#'
#' @noRd 
fPlanktonSidebar <- function(id, tabsetPanel_id, dat){
  ns <- NS(id)
  
  if (stringr::str_detect(id, "NRS") == TRUE){ # NRS
    choices <- unique(sort(dat$StationName))
    selectedSite <- c("Maria Island", "Port Hacking", "Yongala")
    idSite <- "Site"

    if (stringr::str_detect(id, "Micro") == TRUE){ # Microbes + NRS
      selectedVar <- "Bacterial_Temperature_Index_KD"
    } else if (stringr::str_detect(id, "Zoo") == TRUE){ # Zoo + NRS
      selectedVar <- "Biomass_mgm3"
    } else if (stringr::str_detect(id, "Phyto") == TRUE){ # Phyto + NRS
      selectedVar =  "PhytoAbundance_CellsL"
    }
  } else if (stringr::str_detect(id, "CPR") == TRUE){ # CPR
    choices <- unique(sort(dat$BioRegion))
    selectedSite <- "Temperate East"
    idSite <- "region"
    if (stringr::str_detect(id, "Zoo") == TRUE){ # Zoo + CPR
      selectedVar = "ZoopAbundance_m3"
    } else if (stringr::str_detect(id, "Phyto") == TRUE){ # Phyto + CPR
      selectedVar = "PhytoAbundance_Cellsm3"
    }
  }
  
  shiny::sidebarPanel(
    shiny::conditionalPanel(
      tags$head(tags$style(HTML(
        ".multicol{
          height:auto;
          -webkit-column-count: 2;
          -moz-column-count: 2;
          column-count: 2;}"))),
      condition = "input.phyto == 'ptscpr' | input.zoo == 'ztscpr'", 
      shiny::HTML("<h6>Note there is very little data in the North and North-west regions<h6>")
    ),
            
      shiny::conditionalPanel(
        condition = paste0("input.", tabsetPanel_id, " >= 1 | input.", tabsetPanel_id, " == 2 | input.", tabsetPanel_id, " == 3 | ", tabsetPanel_id, " == 4"), 
      shiny::plotOutput(ns("plotmap"),
                        height = "300px", 
                        width = "100%"),
      shiny::HTML("<h6><strong>Select a station:</strong></h5>"),
      shiny::fluidRow(tags$div(align = "left", 
                               class = "multicol",
                               shiny::checkboxGroupInput(inputId = ns(idSite), 
                                                         label = NULL,
                                                         choices = choices, 
                                                         selected = selectedSite))),
      shiny::HTML("<h5><strong>Dates:</strong></h5>"),
      shiny::sliderInput(ns("DatesSlide"), 
                         label = NULL, 
                         min = as.POSIXct('2009-01-01 00:00',
                                          format = "%Y-%m-%d %H:%M",
                                          tz = "Australia/Hobart"), 
                         max = Sys.time(), 
                         value = c(as.POSIXct('2009-01-01 00:00',
                                              format = "%Y-%m-%d %H:%M",
                                              tz = "Australia/Hobart"), Sys.time()-1), timeFormat="%Y-%m-%d")
    ),
    
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 1 | input.", tabsetPanel_id, " == 2 | input.", tabsetPanel_id, " == 3"), 
      shiny::HTML("<h5><strong>Select a parameter:</strong></h5>"),
      shiny::selectInput(inputId = ns("parameter"), 
                         label = NULL, 
                         choices = planktonr::pr_relabel(unique(dat$Parameters), style = "simple"), 
                         selected = selectedVar),
    ),
    
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 1 | input.", tabsetPanel_id, " == 2"), 
      shiny::checkboxInput(inputId = ns("scaler1"), 
                           label = strong("Change the plot scale to log10"), 
                           value = FALSE),
      shiny::br(),
      shiny::br()
    ),
    
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 3 && input.navbar != 'Microbes'"), # Plankton
      shiny::checkboxInput(inputId = ns("scaler3"),
                           label = strong("Change the plot scale to proportion"),
                           value = FALSE),
    ),
    
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 3 && input.navbar == 'Microbes'"), # Micro
      shiny::selectizeInput(inputId = ns("interp"),
                            label = strong("Interpolate data?"),
                            choices = c("Interpolate", "Raw data", "Interpolate with gap filling"),
                            selected = "Raw data"),
    ),
    
    shiny::conditionalPanel(
      condition = paste0("input.", tabsetPanel_id, " == 4 && input.navbar == 'Microbes'"),
      shiny::HTML("<h5><strong>Select a parameter:</strong></h5>"),
      selectInput(inputId = ns("p1"), label = 'Select an x parameter', 
                  choices = planktonr::pr_relabel(unique(datNRSm$Parameters), style = "simple"), selected = "Bacterial_Temperature_Index_KD"),
      selectInput(inputId = ns("p2"), label = 'Select a y parameter',
                  choices = planktonr::pr_relabel(unique(Pico$Parameters), style = "simple"), selected = "Prochlorococcus_cellsmL")
    ),
  )
}


#' Generic BOO Plankton Panel
#' 
#' @noRd
fPLanktonPanel <- function(id, tabsetPanel_id){
  ns <- NS(id)
  shiny::mainPanel(
    shiny::tabsetPanel(id = tabsetPanel_id, type = "pills",
                       shiny::tabPanel("Trend Analysis", value = 1,
                                       h6(textOutput(ns("PlotExp1"), container = span)),
                                       plotOutput(ns("timeseries1"), height = "auto") %>% 
                                         shinycssloaders::withSpinner(color="#0dc5c1"),
                                       div(style="display:inline-block; float:right; width:60%",
                                           fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                                           fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
                                           fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action"))
                       ),
                       shiny::tabPanel("Climatologies", value = 2,
                                       h6(textOutput(ns("PlotExp2"), container = span)),  
                                       plotOutput(ns("timeseries2"), height = 800) %>% 
                                         shinycssloaders::withSpinner(color="#0dc5c1"),
                                       div(style="display:inline-block; float:right; width:60%",
                                           fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
                                           fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"),
                                           fButtons(id, button_id = "downloadCode2", label = "R Code Example", Type = "Action"))
                       ),
                       if(tabsetPanel_id != "NRSmts"){
                         shiny::tabPanel("Functional groups", value = 3,
                                         h6(textOutput(ns("PlotExp3"), container = span)),  
                                         plotOutput(ns("timeseries3"), height = "auto") %>% 
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(style="display:inline-block; float:right; width:60%",
                                             fButtons(id, button_id = "downloadPlot3", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData3", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode3", label = "R Code Example", Type = "Action"))
                         )
                       },
                       if (tabsetPanel_id == "NRSmts"){
                         shiny::tabPanel("Trend analysis by depth", value = 3,
                                         h6(textOutput(ns("PlotExp3"), container = span)),
                                         plotOutput(ns("timeseries3"), height = 'auto') %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(style="display:inline-block; float:right; width:60%",
                                             fButtons(id, button_id = "downloadPlot3", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData3", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode3", label = "R Code Example", Type = "Action"))
                         )
                       },
                       if (tabsetPanel_id == "NRSmts"){
                         shiny::tabPanel("Cell counts vs Indices", value = 4,
                                         h6(textOutput(ns("PlotExp4"), container = span)),
                                         plotOutput(ns("timeseries4")) %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1"),
                                         div(style="display:inline-block; float:right; width:60%",
                                             fButtons(id, button_id = "downloadPlot4", label = "Plot", Type = "Download"),
                                             fButtons(id, button_id = "downloadData4", label = "Data", Type = "Download"),
                                             fButtons(id, button_id = "downloadCode4", label = "R Code Example", Type = "Action"))
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
  
    if (stringr::str_detect(id, "Zoo") == TRUE){ # Phyto
      selectedVar <- "Acartia danae"
      labeltext = "Select a zooplankton species"
    } else { # Zoo 
      selectedVar <- "Tripos furca"
      labeltext = "Select a phytoplankton species"
      }
      
      shiny::sidebarPanel(
        shiny::conditionalPanel(
          condition = paste0("input.", tabsetPanel_id, " == 1"), 
          selectizeInput(inputId = ns('species'), label = labeltext, choices = unique(dat1$Species), 
                         selected = selectedVar),
          shiny::checkboxInput(inputId = ns("scaler1"), 
                               label = strong("Change between frequency or Presence/Absence plot"), 
                               value = FALSE)
        ),
        shiny::conditionalPanel(
          condition = paste0("input.", tabsetPanel_id, " == 2"), 
          selectizeInput(inputId = ns('species1'), label = labeltext, choices = unique(dat2$Species), 
                         selected = selectedVar),
          h6("This is a reduced species list that only contains species with enough data to create an STI plot")
          
        ),
        shiny::conditionalPanel(
          condition = paste0("input.", tabsetPanel_id, " == 3"), 
          selectizeInput(inputId = ns('species2'), label = labeltext, choices = unique(dat3$Species), 
                         selected = selectedVar),
          h6("This is a reduced species list that only contains species with enough data to create a day night plot")
        ),
      )
}



#' Generic BOO Plankton Spatial Panel
#' 
#' @noRd
fSpatialPanel <- function(id, tabsetPanel_id){
  ns <- NS(id)
  shiny::mainPanel(
    tabsetPanel(id = tabsetPanel_id, type = "pills",
                tabPanel("Observation maps", value = 1, 
                         h6(textOutput(ns("DistMapExp"), container = span)),
                         fluidRow(
                           shiny::column(width = 6,
                                         style = "padding:0px; margin:0px;",
                                         shiny::h4("December - February"),
                                         leaflet::leafletOutput(ns("MapSum"), width = "99%", height = "300px") %>% 
                                           shinycssloaders::withSpinner(color="#0dc5c1")), 
                           shiny::column(width = 6,
                                         style = "padding:0px; margin:0px;",
                                         shiny::h4("March - May"),
                                         leaflet::leafletOutput(ns("MapAut"), width = "99%", height = "300px") %>%
                                           shinycssloaders::withSpinner(color="#0dc5c1")
                           ),
                           shiny::column(width = 6,
                                         style = "padding:0px; margin:0px;",
                                         shiny::h4("June - August"),
                                         leaflet::leafletOutput(ns("MapWin"), width = "99%", height = "300px") %>% 
                                           shinycssloaders::withSpinner(color="#0dc5c1")), 
                           shiny::column(width = 6,
                                         style = "padding:0px; margin:0px;",
                                         shiny::h4("September - February"),
                                         leaflet::leafletOutput(ns("MapSpr"), width = "99%", height = "300px") %>% 
                                           shinycssloaders::withSpinner(color="#0dc5c1"))
                         )
                ),        
                tabPanel("Species Temperature Index graphs", value = 2, 
                         h6(textOutput(ns("STIsExp"), container = span)),
                         plotOutput(ns("STIs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                ),
                tabPanel("Species Diurnal Behaviour", value = 3, 
                         h6(textOutput(ns("SDBsExp"), container = span)),
                         plotOutput(ns("DNs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                )
    )
  )
}


#' Generic BOO Environmental Panel
#' 
#' @noRd
fEnviroPanel <- function(id){
  ns <- NS(id)
  shiny::mainPanel(
    h6(textOutput(ns("PlotExp"), container = span)),
    plotOutput(ns("timeseries1")) %>% 
      shinycssloaders::withSpinner(color="#0dc5c1"),
    shiny::div(style="display:inline-block; float:right; width:60%",
               fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
               if (id == "MoorBGC_ui_1"){fButtons(id, button_id = "downloadData2", label = "Data TS", Type = "Download")},
               if (id == "MoorBGC_ui_1"){fButtons(id, button_id = "downloadData3", label = "Data Clim", Type = "Download")},
               if (id != "MoorBGC_ui_1"){fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download")},
               fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action"),
    )
  )
}

#' Generic BOO Environmental Sidebar
#'
#' @noRd 
fEnviroSidebar <- function(id, dat = NULL){
  ns <- NS(id)
  
  if (id == "NutrientsBGC_ui_1"){
    selectedVar = "Silicate_umolL"
    ignoreStat <- c("PH4", "NIN", "ESP") # Stations to ignore
  }
  if (id == "PicoBGC_ui_1"){
    selectedVar = "Prochlorococcus_cellsmL"
    ignoreStat <- c("PH4", "NIN", "ESP") # Stations to ignore
  }
  if (id == "PigmentsBGC_ui_1"){
    selectedVar = "TotalChla"
    ignoreStat <- c("PH4") # Stations to ignore
  }
  if (id == "WaterBGC_ui_1"){
    selectedVar = "CTDTemperature_degC"
    ignoreStat <- c("PH4") # Stations to ignore
  }
  if (id == "MoorBGC_ui_1"){
    ignoreStat <- c("PH4", "NIN", "ESP") # Stations to ignore
  }
  
  shiny::sidebarPanel(
    style = "padding:1%;",
    tags$head(tags$style(HTML( #TODO move to custom css
      ".multicol{
          height:auto;
          -webkit-column-count: 2;
          -moz-column-count: 2;
          column-count: 2;}"))),
    # shiny::div(
    # style = "padding:0px; margin:0px; max-height: 1000px;", #bottom: 0px; left: 0px; right: 0px; max-width: 1000px;  min-height: 10px
    shiny::plotOutput(ns("plotmap"), width = "100%"),
    # ),
    shiny::HTML("<h5><strong>Select a station:</strong></h5>"),
    shiny::fluidRow(tags$div(align = "left", 
                             class = "multicol",
                             shiny::checkboxGroupInput(inputId = ns("station"),
                                                       label = NULL,
                                                       choices = NRSStation %>% 
                                                         dplyr::filter(!.data$StationCode %in% ignoreStat) %>%
                                                         dplyr::pull(.data$StationName),
                                                       selected = "Port Hacking"))),
    
    
    if (id != "MoorBGC_ui_1"){
      shiny::conditionalPanel(
        condition = "input.env != 'moor'",
        shiny::HTML("<h5><strong>Select dates:</strong></h5>"),
        sliderInput(ns("date"), label = NULL, min = lubridate::ymd(20090101), max = Sys.Date(), 
                    value = c(lubridate::ymd(20090101), Sys.Date()-1), timeFormat="%Y-%m-%d")
      )
    },
    
    if (id != "MoorBGC_ui_1"){
      shiny::conditionalPanel(
        condition = "input.env != 'moor'",
        shiny::HTML("<h5><strong>Select a parameter:</strong></h5>"),
        shiny::selectInput(inputId = ns("parameter"), 
                           label = NULL, 
                           choices = planktonr::pr_relabel(unique(dat$Parameters), style = "simple"), 
                           selected = selectedVar)
      )
    },
    
    
    # Select whether to overlay smooth trend line
    if (id %in% c("PigmentsBGC_ui_1")){
      shiny::conditionalPanel(
        condition = "input.env == 'pigs'",
        shiny::HTML("<h5><strong>Overlay trend line?</strong></h5>"),
        selectizeInput(inputId = ns("smoother"), label = NULL, 
                       choices = c("Smoother", "Linear", "None"), selected = "None"),
      )
    },
    
    # Select whether to interpolate 
    if (id %in% c("PicoBGC_ui_1", "NutrientsBGC_ui_1")){
      shiny::conditionalPanel(
        condition = "input.env == 'moor' | input.env == 'pico' | input.env == 'bgc'",
        shiny::HTML("<h5><strong>Interpolate data?</strong></h5>"),
        selectizeInput(inputId = ns("interp"), label = NULL, 
                       choices = c("Interpolate", "Raw data", "Interpolate with gap filling"), 
                       selected = "Interpolate")
      )
    },
    
    shiny::br(), # Give a bit of space for the menu to expand
    shiny::br()
  )
  
}








#' Download Button
#' 
#' @noRd
fButtons <- function(id, button_id, label, Type = "Download") {
  ns <- NS(id)
  
  shiny::tagList(
    if (Type == "Download"){
      shiny::downloadButton(ns(button_id), label = label,  class = "btn-danger; btn-lg", #style = "width:48%"
      )
    } else if (Type == "Action"){
      
      if (stringr::str_detect(id, "NRS")){
        # ws <- "window.open('http://google.com', '_blank')"
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/NRS_ts.html')"
      } else if (stringr::str_detect(id, "CPR")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/CPR_ts.html')"
      } else {
        wsite <- "window.open('https://planktonteam.github.io/planktonr/index.html')"
      }
      
      shiny::actionButton(ns(button_id), label = label,  class = "btn-danger; btn-lg", 
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
      if (gg_prefix == "Policy"){
        paste0(gg_prefix, "_", format(Sys.time(), "%Y%m%d"), ".csv")
      } else{
        paste0(gg_prefix, "_", input$parameter, "_", format(Sys.time(), "%Y%m%d", tz = "Australia/Hobart"), ".csv")
      }
    },
    content = function(file) {
      vroom::vroom_write(input_dat, file, delim = ",")
    })
  return(downloadData)
}


#' Download Plot - Server Side
#'
#' @noRd 
fDownloadPlotServer <- function(input, gg_id, gg_prefix) {
  downloadPlot <- downloadHandler(
    filename = function() {
      if (gg_prefix == "Policy"){
        paste0(gg_prefix, "_", input$Site, "_", format(Sys.time(), "%Y%m%d"), ".png")
      } else{
        paste0(gg_prefix, "_", input$parameter, "_", format(Sys.time(), "%Y%m%d", tz = "Australia/Hobart"), ".png")
      }
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = gg_id, device = "png", dpi = 500)
    })
}



#' Base leaflet plot for all sample points
#'
#' @noRd 
LeafletBase <- function(df, Type = 'PA'){
  
  if(Type == 'frequency'){

    leaflet::leaflet(df %>% 
                       dplyr::distinct(.data$Latitude, .data$Longitude)) %>%
      leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>% 
      leaflet::addCircleMarkers(lng = ~ Longitude,
                                lat = ~ Latitude,
                                color = "#CCCCCC",
                                opacity = 0,
                                fillOpacity = 0,
                                radius = 0.25, 
                                group = "Absent") 
    
  } else {
    leaflet::leaflet(df %>% 
                     dplyr::distinct(.data$Latitude, .data$Longitude)) %>%
    leaflet::addProviderTiles(provider = "Esri", layerId = "OceanBasemap") %>% 
    leaflet::addCircleMarkers(lng = ~ Longitude,
                              lat = ~ Latitude,
                              color = "#CCCCCC",
                              opacity = 1,
                              fillOpacity = 1,
                              radius = 0.25, 
                              group = "Absent") 
    }
}

#' Base leaflet plot for all sample points with observations for a particular species
#'
#' @noRd 
LeafletObs <- function(sdf, name, Type = 'PA'){

  Species <- unique(sdf$Species)
  
  if("Abundance_1000m3" %in% colnames(sdf)){
    labs <- lapply(seq(nrow(sdf)), function(i) {
      paste("<strong>Date:</strong>", sdf$SampleTime_Local[i], "<br>",
          "<strong>Latitude:</strong>", sdf$Latitude[i], "<br>",
          "<strong>Longitude:</strong>", sdf$Longitude[i], "<br>",
          "<strong>Count:</strong>", sdf$Count[i], "<br>",
          "<strong>Abundance (1000 m\u207B\u00B3):</strong>", round(sdf$Abundance_1000m3[i], digits = 2), "<br>",
          "<strong>Temperature (\u00B0C):</strong>", sdf$Temperature_degC[i], "<br>",
          "<strong>Depth (m):</strong>", sdf$SampleDepth_m[i], "<br>")})
  } else if ("freqfac" %in% colnames(sdf)){
    labs <- lapply(seq(nrow(sdf)), function(i) {
      paste("<strong>Latitude:</strong>", sdf$Latitude[i], "<br>",
            "<strong>Longitude:</strong>", sdf$Longitude[i], "<br>",
            "<strong>Frequency in sample:</strong>", sdf$freqfac[i], "<br>")})
  } else {
    labs <- ''
  }
  
  if(Type == 'frequency'){
    dfCPR <- sdf %>% dplyr::filter(.data$Survey == 'CPR') %>% dplyr::arrange(.data$freqfac) 
    dfNRS <- sdf %>% dplyr::filter(.data$Survey == 'NRS') %>% dplyr::arrange(.data$freqfac) 
    
    CPRpal <- leaflet::colorFactor(c("#CCCCCC", "#99CCFF", "#3399FF", "#0066CC", "#003366"), domain = sdf$freqfac)
    NRSpal <- leaflet::colorFactor(c("#CCCCCC", "#CCFFCC", "#99FF99", "#009900", "#006600"), domain = sdf$freqfac)
    
    leaf <- leaflet::leafletProxy(name, data = sdf) %>%
      leaflet::clearGroup(c("National Reference Stations", "Continuous Plankton Recorder")) %>%
      leaflet::addCircleMarkers(data = dfCPR,
                                group = 'Continuous Plankton Recorder', 
                              lng = ~ Longitude,
                              lat = ~ Latitude,
                              color = ~CPRpal(freqfac),
                              fill = ~CPRpal(freqfac),
                              radius = 3) %>% 
      leaflet::addCircleMarkers(data = dfNRS,
                                group = 'National Reference Stations', 
                                lng = ~ Longitude,
                                lat = ~ Latitude,
                                color = ~NRSpal(freqfac),
                                fill = ~NRSpal(freqfac),
                                radius = 3) %>% 
      leaflet::addLayersControl( # Layers control
        overlayGroups = c("National Reference Stations", "Continuous Plankton Recorder"),
        position = "topright",
        options = leaflet::layersControlOptions(collapsed = FALSE, fill = NA)) %>% 
      leaflet::addLegend("bottomleft", pal = CPRpal, group = "Continuous Plankton Recorder", 
                         values = sdf$freqfac, title = paste(Species, 'CPR')) %>% 
      leaflet::addLegend("bottomleft", pal = NRSpal, group = "National Reference Stations", 
                         values = sdf$freqfac, title = paste(Species, 'NRS'))
      
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
          leaf)))

    leaf
    
  } else {
    leaflet::leafletProxy(name, data = sdf) %>%
      leaflet::clearGroup("Present") %>%
      leaflet::addCircleMarkers(data = sdf, 
                                lng = ~ Longitude,
                                lat = ~ Latitude,
                                color = 'blue',
                                opacity = 1,
                                fillOpacity = 1,
                                radius = 2,
                                group = "Present",
                                label = lapply(labs, htmltools::HTML)) %>% 
      leaflet::addLegend("bottomleft", 
                         colors = c("blue",  "#CCCCCC"),
                         labels = c("Seasonal Presence", "Seasonal Absence"),
                         title = Species,
                         opacity = 1)
  }
}
  












# Generic BOO Plamkton Server
# 
# @noRd
# fPLanktonPanelServer <- function(ns, panel_id, input){
# 
#   dat1 <- datNRSp
#   dat3 <- NRSfgp
#   
#   # Variable for CPR or NRS 
#   NRS <- 1
#   
#   # observeEvent({input$NRSpt == 1 | input$NRSpt == 2}, {
#   selectedData <- reactive({ #TODO - This reactive encompasses things from 1/2 AND 3. Can we split them?
#     req(input$Site)
#     req(input$parameter)
#     validate(need(!is.na(input$Site), "Error: Please select a station."))
#     validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
# 
#     selectedData <- dat1 %>%
#       dplyr::filter(.data$StationName %in% input$Site,
#                     .data$Parameters %in% input$parameter,
#                     dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
#       droplevels()
# 
#   }) %>% bindCache(input$parameter,input$Site, input$DatesSlide[1], input$DatesSlide[2])
#   # })
# 
#   output$plotmap <- renderPlot({
#     planktonr::pr_plot_NRSmap(selectedData())
#   }, bg = "transparent") %>% bindCache(input$Site)
# 
#   # add text information
#   output$PlotExp1 <- renderText({
#     "A plot of selected phytoplantkon Parameters from the NRS around Australia, as a time series and a monthly climatology by station."
#   })
#   output$PlotExp2 <- renderText({
#     "A plot of selected indicies from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
#   })
#   output$PlotExp3 <- renderText({
#     "A plot of functional groups from the light microscope phytoplankton counts from the NRS around Australia, as a time series and a monthly climatology"
#   })
# 
# 
#   # Plot Trends -------------------------------------------------------------
#   observeEvent({input$NRSpts == 1}, {
# 
#     gg_out1 <- reactive({
#       if (is.null(datNRSp$StationCode)) {return(NULL)}
#       trans <- dplyr::if_else(input$scaler1, "log10", "identity")
# 
#       p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", Survey = "NRS", method = "lm", trans = trans)
#       p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", Survey = "NRS", method = "loess", trans = trans) +
#         ggplot2::theme(axis.title.y = ggplot2::element_blank())
#       p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = "collect")
# 
#     }) %>% bindCache(input$parameter,input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
# 
#     output$timeseries1 <- renderPlot({
#       gg_out1()
#     }, height = function() {length(unique(selectedData()$StationName)) * 200})
# 
#     # Download -------------------------------------------------------
#     output$downloadData1 <- fButtonsServer(input, selectedData(), "Trend") # Download csv of data
#     output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Trend") # Download figure
#   })
# 
#   # Climatologies -----------------------------------------------------------
# 
#   # Plot abundance spectra by species
#   observeEvent({input$NRSpt == 2}, {
# 
#     gg_out2 <- reactive({
#       print("Tab 2")
#       if (is.null(datNRSp$StationCode)) {return(NULL)}
# 
#       trans <- dplyr::if_else(input$scaler1, "log10", "identity")
#       titleplot <- names(planktonr::pr_relabel(input$parameter, style = "simple"))
# 
#       p1 <- planktonr::pr_plot_TimeSeries(selectedData(), Survey = "NRS", trans = trans) +
#         ggplot2::theme(legend.position = "none")
#       p2 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Month", trans = trans) +
#         ggplot2::theme(legend.position = "none", axis.title.y = ggplot2::element_blank())
#       p3 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Year", trans = trans) +
#         ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "bottom")
# 
#       p1 / (p2 | p3) + patchwork::plot_layout(guides = "collect") +
#         patchwork::plot_annotation(title = titleplot)
# 
#     }) %>% bindCache(input$parameter, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
# 
#     output$timeseries2 <- renderPlot({
#       gg_out2()
#     })
# 
#     # Download -------------------------------------------------------
#     output$downloadData2 <- fButtonsServer(input, selectedData(), "Climate") # Download csv of data
#     output$downloadPlot2 <- fDownloadPlotServer(input, gg_id = gg_out2(), "Climate") # Download figure
#   })
# 
#   # Functional groups -------------------------------------------------------
#   observeEvent({input$NRSpt == 3}, {
# 
#     selectedDataFG <- reactive({
#       req(input$Site)
#       validate(need(!is.na(input$Site), "Error: Please select a station."))
# 
#       selectedDataFG <- NRSfgp %>%
#         dplyr::filter(.data$StationName %in% input$Site,
#                       dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
#         droplevels()
# 
#     })%>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2])
# 
#     gg_out3 <- reactive({
# 
#       if (is.null(NRSfgp$StationCode)) {return(NULL)}
#       scale <- dplyr::if_else(input$scaler3, "Percent", "Actual")
# 
#       p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
#       p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, Trend = "Month") +
#         ggplot2::theme(axis.title.y = ggplot2::element_blank(), legend.position = "none")
#       p1 + p2 + patchwork::plot_layout(widths = c(3,1))
# 
#     }) %>% bindCache(input$Site, input$scaler3, input$DatesSlide[1], input$DatesSlide[2])
# 
#     output$timeseries3 <- renderPlot({
#       gg_out3()
#     }, height = function() {length(unique(selectedData()$StationName)) * 200})
# 
#     # Download -------------------------------------------------------
#     output$downloadData3 <- fButtonsServer(input, selectedDataFG(), "FuncGroup") # Download csv of data
#     output$downloadPlot3 <- fDownloadPlotServer(input, gg_id = gg_out3(), "FuncGroup") # Download figure
# 
#   })
# 
# }

