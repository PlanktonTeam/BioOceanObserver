#' Snapshot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom stats runif
mod_Snapshot_ui <- function(id){
  nsSnap <- NS(id)
  tagList(
    # use this in non shinydashboard app
    # setBackgroundColor(color = "ghostwhite"),
    shinyWidgets::useShinydashboard(),
    fluidRow(
      h1("Plankton sampling progress"),
      plotOutput(nsSnap("progplot"), height = 800)),
    h1("By the Numbers"),
    fluidRow(
      shinydashboard::valueBoxOutput(nsSnap("PCount"), width = 3),
      shinydashboard::valueBoxOutput(nsSnap("PSpNo"), width = 3),
      shinydashboard::valueBoxOutput(nsSnap("PCommSp"), width = 6)),
    fluidRow(
      shinydashboard::valueBoxOutput(nsSnap("ZCount"), width = 3),
      shinydashboard::valueBoxOutput(nsSnap("ZSpNo"), width = 3),
      shinydashboard::valueBoxOutput(nsSnap("ZCommSp"), width = 6)),
    fluidRow(
      column(4,
             h1("Photo of the Day"),
             imageOutput(nsSnap("POD"))),
      column(8,
             h1("Graph of the Day"),
             plotOutput(nsSnap("PlotOD")))),
    fluidRow(
      shinydashboard::valueBoxOutput(nsSnap("Facts"), width = 12)),
    fluidRow(
      shinydashboard::valueBoxOutput(nsSnap("Papers"), width = 12)),
  )
}

#' Snapshot Server Functions
#'
#' @noRd 
mod_Snapshot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Dashboard Boxes ---------------------------------------------------------
    
    output$PCount <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        sum(planktonr::pr_get_SppCount("P")$n), #input$count,
        "Number of Phytoplankton Counted",
        icon = shiny::icon("chart-bar"),
        color = "yellow"
      )
    })
    
    output$ZCount <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        sum(planktonr::pr_get_SppCount("Z")$n), #input$count,
        "Number of Zooplankton Counted",
        icon = shiny::icon("chart-bar"),
        color =  "light-blue"
      )
    })  
    
    output$PSpNo <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        nrow(unique(planktonr::pr_get_SppCount("P"))), #input$count,
        "Phytoplankton Species Identified",
        icon = shiny::icon("comment-dots"),
        color = "orange"
      )
    })
    
    output$ZSpNo <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        nrow(unique(planktonr::pr_get_SppCount("Z"))), #input$count,
        "Zooplankton Species Identified",
        icon = shiny::icon("comment-dots"),
        color = "blue"
      )
    })
    
    output$PCommSp <- shinydashboard::renderValueBox({
      ptax <- planktonr::pr_get_SppCount("P")[1,]
      pLink <- stringr::str_c("javascript:void(window.open('http://www.marinespecies.org/aphia.php?p=taxdetails&id=", as.character(ptax$SPCode),"', '_blank'))")
      shinydashboard::valueBox(
        ptax$TaxonName,
        "Most Common Phytoplankton Species",
        icon = shiny::icon("microscope"),
        color = "red",
        href = pLink
      )
    })
    
    output$ZCommSp <- shinydashboard::renderValueBox({
      ztax <- planktonr::pr_get_SppCount("Z")[1,]
      zLink <- stringr::str_c("javascript:void(window.open('http://www.marinespecies.org/aphia.php?p=taxdetails&id=", as.character(ztax$SPCode),"', '_blank'))")
      shinydashboard::valueBox(
        ztax$TaxonName,
        "Most Common Zooplankton Species",
        icon = shiny::icon("microscope"),
        color = "navy",
        href = zLink
      )
    })
    
    
    
    
    
    output$Facts <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        "Fun Facts",
        planktonr::pr_get_Facts(),
        icon = shiny::icon("info-circle"),
        color = "green"
      )
    }) 
    
    output$Papers <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        "Scientific Paper using the data in the Biological Ocean Observer",
        planktonr::pr_get_Papers(),
        # tags$p(tags$b("Test "), tags$i("string")),
        icon = shiny::icon("newspaper"),
        color = "teal"
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
    
    
    # progress plot
    
    output$progplot <- renderPlot({
      
      planktonr::pr_plot_ProgressMap(PMapData)
      
      # MapOz <- rnaturalearth::ne_countries(scale = "medium", country = "Australia",
      #                                    returnclass = "sf")

          # PMapData2 <- PMapData %>%
    #   sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
    #   sf::st_as_sf() 
    
    # PMapSum <- merge(PMapData %>% dplyr::group_by(.data$Region, .data$Survey) %>% 
    #                    dplyr::summarise(Sums = dplyr::n(),
    #                                     .groups = 'drop') %>%
    #                    dplyr::mutate(label = paste0(.data$Region, ' = ', .data$Sums)),
    #                  PMapData %>% dplyr::group_by(.data$Region, .data$Survey) %>% 
    #                    dplyr::summarise(Lats = mean(.data$Latitude),
    #                                     Lons = mean(.data$Longitude),
    #                                     .groups = 'drop')) %>%
    #   sf::st_as_sf(coords = c("Lons", "Lats"), crs = 4326)
    
    # nudgex = c(-2,5.5,2,8.5,9.5,0,0,4)
    # nudgey = c(-3,0,1,0,0,7,-2,10)
    # # GAB, GBR, NA, NEAC, SEAC, SO, Tas, WA
    
    # ggplot2::ggplot() +
    #   ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
    #   ggplot2::geom_sf(data = PMapData2 %>% dplyr::filter(.data$Survey == 'CPR'), size = 1, ggplot2::aes(color = .data$Region),
    #                    show.legend = FALSE) +
    #   ggplot2::geom_sf_text(data = PMapSum %>% dplyr::filter(.data$Survey == 'CPR'), size = 5, ggplot2::aes(label = .data$label, color = .data$Region),
    #                         show.legend = FALSE, check_overlap = TRUE, nudge_x = nudgex, nudge_y = nudgey) +
    #   ggplot2::geom_sf(data = PMapData2 %>% dplyr::filter(.data$Survey == 'NRS'), size = 5) +
    #   ggplot2::geom_sf_text(data = PMapSum %>% dplyr::filter(.data$Survey == 'NRS'), size = 5, ggplot2::aes(label = .data$label),
    #                         show.legend = FALSE, nudge_x = 3) +
    #   ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(105, 170)) +
    #   ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(-54, -7)) +
    #   ggplot2::theme_void() +
    #   ggplot2::theme(axis.title = ggplot2::element_blank(),
    #                  axis.line = ggplot2::element_blank())
    
    
    })
    
    # Plot of the Day ---------------------------------------------------------
    
    # In the future I will write some code to choose additional plot types
    
    # Plot abundance spectra by species
    output$PlotOD <- renderPlot({
      
      st <- planktonr::pr_get_NRSStation()
      r1 <- round(runif(1, min = 1, max = nrow(st)))
      
      meth <- unique(datNRSz$parameters)
      r2 <- round(runif(1, min = 1, max = length(meth)))
      
      planktonr::pr_plot_tsclimate(datNRSz %>% 
                                     dplyr::filter(.data$StationCode == st$StationCode[r1] &
                                                               .data$parameters == meth[r2]), 'NRS', 'log10')
    })
    
    
    
    
  })
}
