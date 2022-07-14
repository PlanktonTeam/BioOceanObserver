#' BOO Plankton Sidebar
#'
#' @noRd 
fPlanktonSidebar <- function(id, panel_id, input, dat){
  ns <- NS(id)
  
  if (stringr::str_detect(id, "NRS") == TRUE){ # NRS
    choices <- unique(sort(dat$StationName))
    selectedSite <- c("Maria Island", "Port Hacking", "Yongala")
    idSite <- "Site"
    if (stringr::str_detect(id, "Zoo") == TRUE){ # Zoo + NRS
      selectedVar <- "Biomass_mgm3"
    } else if (stringr::str_detect(id, "Phyto") == TRUE){ # Phyto + NRS
      selectedVar =  "PhytoBiomassCarbon_pgL"
    }
  } else if (stringr::str_detect(id, "CPR") == TRUE){ # CPR
    choices <- unique(sort(dat$BioRegion))
    selectedSite <- "Temperate East"
    idSite <- "region"
    if (stringr::str_detect(id, "Zoo") == TRUE){ # Zoo + CPR
      selectedVar = "ZoopAbundance_m3"
    } else if (stringr::str_detect(id, "Phyto") == TRUE){ # Phyto + CPR
      selectedVar = "PhytoAbund_Cellsm3"
    }
  }
  
  
  shiny::sidebarPanel(
    shiny::conditionalPanel(
      condition = paste0("input.", panel_id, " == 1 | input.", panel_id, " == 2"), 
      # Select whether to overlay smooth trend line 
      shiny::checkboxInput(inputId = ns("scaler1"), 
                           label = strong("Change the plot scale to log10"), 
                           value = FALSE),
      shiny::selectInput(inputId = ns("parameter"), 
                         label = 'Select a parameter', 
                         choices = planktonr::pr_relabel(unique(dat$Parameters), style = "simple"), 
                         selected = selectedVar)
    ),
    # browser(),
    shiny::conditionalPanel(
      condition = paste0("input.", panel_id, " == 3"), 
      # Select whether to overlay smooth trend line
      shiny::checkboxInput(inputId = ns("scaler3"), 
                           label = strong("Change the plot scale to percent"), 
                           value = FALSE)
    ),
    shiny::absolutePanel(
      shiny::plotOutput(ns("plotmap")),
      shiny::checkboxGroupInput(inputId = ns(idSite), 
                                label = "Select a station", 
                                choices = choices, 
                                selected = selectedSite),
      shiny::sliderInput(ns("DatesSlide"), "Dates:", min = as.POSIXct('2009-01-01 00:00',
                                                                      format = "%Y-%m-%d %H:%M",
                                                                      tz = "Australia/Hobart"), max = Sys.time(), 
                         value = c(as.POSIXct('2009-01-01 00:00',
                                              format = "%Y-%m-%d %H:%M",
                                              tz = "Australia/Hobart"), Sys.time()-1), timeFormat="%Y-%m-%d"),
      fDownloadData(id, "Data"),
      fDownloadPlot(id, "Plot")
      
    )
  )
}




#' Download Data
#'
#' @noRd 
fDownloadData <- function(id, label) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadData"), label = label),
  )
}


#' Download Data - Server Side
#'
#' @noRd 
fDownloadDataServer <- function(input, dat) {
  
  downloadData <- shiny::downloadHandler(
    filename = function() {
      paste0(input$ycol,"_", format(Sys.time(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      vroom::vroom_write(dat, file, delim = ",")
    })
  return(downloadData)
}



#' Download Plot
#'
#' @noRd 
fDownloadPlot <- function(id, label) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadPlot"), label = label),
  )
}

#' Download Plot - Server Side
#'
#' @noRd 
fDownloadPlotServer <- function(input, gg_id) {
  downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$parameter,"_", format(Sys.time(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggsave(file, plot = gg_id, device = "png", dpi = 500)
    })
}