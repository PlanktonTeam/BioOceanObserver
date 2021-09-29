#' Snapshot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Snapshot_ui <- function(id){
  nsSnap <- NS(id)
  tagList(
    # use this in non shinydashboard app
    # setBackgroundColor(color = "ghostwhite"),
    shinyWidgets::useShinydashboard(),
    h1("By the Numbers"),
    fluidRow(
      shinydashboard::valueBoxOutput(nsSnap("PCount"), width = 4),
      shinydashboard::valueBoxOutput(nsSnap("PSpNo"), width = 4),
      shinydashboard::valueBoxOutput(nsSnap("PCommSp"), width = 4)),
    fluidRow(
      shinydashboard::valueBoxOutput(nsSnap("ZCount"), width = 4),
      shinydashboard::valueBoxOutput(nsSnap("ZSpNo"), width = 4),
      shinydashboard::valueBoxOutput(nsSnap("ZCommSp"), width = 4)),
    fluidRow(
      column(4,
             h1("Photo of the Day"),
             imageOutput(nsSnap("POD"))),
      column(8,
             h1("Graph of the Day"),
             plotly::plotlyOutput(nsSnap("PlotOD"))
      )
    )
  )
}

#' Snapshot Server Functions
#'
#' @noRd 
mod_Snapshot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Dashboard Boxes ---------------------------------------------------------
    
    output$ZSpNo <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        length(unique(obs$Taxon)), #input$count,
        "Zooplankton Species Identified",
        icon = icon("comment-dots"),
        color = "purple"
      )
    })
    output$PSpNo <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        length(unique(obs$Taxon)), #input$count,
        "Phytoplankton Species Identified",
        icon = icon("comment-dots"),
        color = "blue"
      )
    })
    
    output$ZCommSp <- shinydashboard::renderValueBox({
      ztax <- (obs %>% 
                 dplyr::group_by(Taxon) %>% 
                 dplyr::summarise(n = dplyr::n()) %>% 
                 dplyr::arrange(desc(n)) %>% 
                 dplyr::filter(stringr::str_detect(Taxon, 'spp', negate = TRUE))
      )$Taxon[1]
      
      aphiaID <- worrms::wm_name2id(ztax) # Speed this up by including this in a species list
      zLink <- stringr::str_c("javascript:void(window.open('http://www.marinespecies.org/aphia.php?p=taxdetails&id=", as.character(aphiaID),"', '_blank'))")
      
      shinydashboard::valueBox(
        ztax,
        "Most Common Zooplankton Species",
        icon = icon("microscope"),
        color = "red",
        href = zLink
      )
    })
    
    output$PCommSp <- shinydashboard::renderValueBox({
      
      ptax <- (obs %>% 
                 dplyr::group_by(Taxon) %>% 
                 dplyr::summarise(n = dplyr::n()) %>% 
                 dplyr::arrange(desc(n)) %>% 
                 dplyr::filter(stringr::str_detect(Taxon, 'spp', negate = TRUE))
      )$Taxon[1]
      
      aphiaID <- worrms::wm_name2id(ptax) # Speed this up by including this in a species list
      pLink <- stringr::str_c("javascript:void(window.open('http://www.marinespecies.org/aphia.php?p=taxdetails&id=", as.character(aphiaID),"', '_blank'))")
      
      shinydashboard::valueBox(
        ptax,
        "Most Common Phytoplankton Species",
        icon = icon("microscope"),
        color = "teal",
        href = pLink
      )
    })
    
    output$PCount <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        sum(obs$Counts), #input$count,
        "Phytoplankton Count",
        icon = icon("chart-bar"),
        color = "orange"
      )
    })
    
    output$ZCount <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        sum(obs$Counts), #input$count,
        "Zooplankton Count",
        icon = icon("chart-bar"),
        color = "green"
      )
    })    
    
    # Photos ---------------------------------------------------------
    
    output$POD <- renderImage({
      
      photo_list <- list.files(path = "inst/app/www/plankton", full.names = FALSE)
      i <- round(runif(1, min = 1, max = length(photo_list)))
      
      # Return a list containing the filename
      list(src = stringr::str_c("inst/app/www/plankton/",photo_list[i]),
           contentType = 'image/png',
           width = 400,
           # height = 300,
           alt = photo_list[i])
    }, deleteFile = FALSE)
    
    
    
    
    
    
    
    # Plot of the Day ---------------------------------------------------------
    
    # In the future I will write some code to choose random sites, and pr_code
    
    # Plot abundance spectra by species
    output$PlotOD <- plotly::renderPlotly({
      plots <- planktonr::pr_plot_tsclimate(datNRSz %>% dplyr::filter(StationCode == "PHB" & parameters == "Biomass_mgm3"), 'NRS', 'matter', 'log10')
    })
    
    
    
    
  })
}

## To be copied in the UI
# 

## To be copied in the server
# 
