#' PhytoSpatial UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PhytoSpatial_ui <- function(id){
  
  nsPhytoSpatial <- NS(id)
  
  tagList(
    sidebarLayout(

      fSpatialSidebar(id = id, tabsetPanel_id = "NRSspatp", dat1 = pkg.env$fMapDatap, dat2 = pkg.env$stip, dat3 = pkg.env$daynightp),
      fSpatialPanel(id= id, tabsetPanel_id = "NRSspatp")

      )
    )
}

#' PhytoSpatial Server Functions
#'
#' @noRd 
mod_PhytoSpatial_server <- function(id){
  moduleServer( id, function(input, output, session, NRSspatp){
    # Subset data
    
    type <- dplyr::if_else(input$scaler1, "frequency", "PA")
    
    AbsPdatar <- reactive({
      
      AbsPdatar <- pkg.env$fMapDatap %>%
        dplyr::distinct(.data$Longitude, .data$Latitude, .data$Season, .data$Survey) %>% 
        dplyr::mutate(Species = input$species, 
                      freqfac = factor('Absent'))
      
    }) %>% bindCache(input$species)
    
    PSdatar <- reactive({
      
      req(input$species)
      shiny::validate(need(!is.na(input$species), "Error: Please select a species"))

      type <- dplyr::if_else(input$scaler1, "frequency", "PA")
      
      PSdatar <- pkg.env$fMapDatap %>%
        dplyr::filter(.data$Species == input$species) 

      if(type == "frequency"){
        PSdatar <- PSdatar %>% dplyr::bind_rows(AbsPdatar()) %>%
          dplyr::mutate(freqfac = factor(.data$freqfac, levels = c("Absent", "Seen in 25%",'50%', '75%', '100% of Samples')))
      } else {
        PSdatar
      }
        
    }) %>% bindCache(input$species, input$scaler1)
    
    shiny::exportTestValues(
      PhytoSpatialRows = {nrow(PSdatar()) > 0},
      PhytoSpatialLatisNumeric = {class(PSdatar()$Latitude)},
      PhytoSpatialLongisNumeric = {class(PSdatar()$Longitude)},
      PhytoSpatialFreqisFactor = {class(PSdatar()$freqfac)},
      PhytoSpatialSeasonisChr = {class(PSdatar()$Season)},
      PhytoSpatialSpeciesisChr = {class(PSdatar()$Species)}
    )
    
    # add text information ------------------------------------------------------------------------------
    output$DistMapExp <- renderText({
      "This map is either a presence absence map based on the NRS and CPR data for each species or a frequency map based on the 
      number of times a species is seen in the sample location."
    }) 
    output$STIsExp <- renderText({
      paste("Figure of the species STI showing the temperature range at which this species is most common. 
            A bimodal shape may indicate a sub-species or two species being identified as the same species.")
    }) 
    output$SDBsExp <- renderText({
      paste("Figure of the diurnal abundances from CPR data. Note that the CPR is towed at ~10m depth so these abundances are representative of surface waters.")
    }) 
    
    # select initial map  ------------------------------------------------------------------------------
    observeEvent({input$NRSspatp == 1}, {

      # Create dot map of distribution
      # Summer
      output$MapSum <- leaflet::renderLeaflet({
        type <- dplyr::if_else(input$scaler1, "frequency", "PA")
        lf <- LeafletBase(AbsPdatar(), Type = type)
      return(lf)
    }) %>%  bindCache(input$species, input$scaler1)
    
    # Autumn
    output$MapAut <- leaflet::renderLeaflet({
      type <- dplyr::if_else(input$scaler1, "frequency", "PA")
      lf <- LeafletBase(AbsPdatar(), Type = type)
      return(lf)
    }) %>%  bindCache(input$species, input$scaler1)
    
    # Winter
    output$MapWin <- leaflet::renderLeaflet({
      type <- dplyr::if_else(input$scaler1, "frequency", "PA")
      lf <- LeafletBase(AbsPdatar(), Type = type)
      return(lf)
    }) %>%  bindCache(input$species, input$scaler1)
    
    # Spring
    output$MapSpr <- leaflet::renderLeaflet({
      type <- dplyr::if_else(input$scaler1, "frequency", "PA")
      lf <- LeafletBase(AbsPdatar(), Type = type)
      return(lf)
    }) %>%  bindCache(input$species, input$scaler1)

    observe ({
      type <- dplyr::if_else(input$scaler1, "frequency", "PA")
      LeafletObs(sdf = PSdatar() %>% dplyr::filter(.data$Season == "December - February"), name = "MapSum", Type = type)
      LeafletObs(sdf = PSdatar() %>% dplyr::filter(.data$Season == "September - November"), name = "MapAut", Type = type)
      LeafletObs(sdf = PSdatar() %>% dplyr::filter(.data$Season == "June - August"), name = "MapWin", Type = type)
      LeafletObs(sdf = PSdatar() %>% dplyr::filter(.data$Season == "March - May"), name = "MapSpr", Type = type)
    
    }) 
    
    })
    
    # STI plot -----------------------------------------------------------------------------------------
    # Subset data
    
    observeEvent({input$NRSspatp == 2}, {
      
      selectedSTI <- reactive({
        
        req(input$species1)
        shiny::validate(need(!is.na(input$species1), "Error: Please select a species"))
        
        selectedSTI <- pkg.env$stip %>% 
          dplyr::filter(.data$Species %in% input$species1) 

        
      }) %>% bindCache(input$species1)
      
      # sti plot
      output$STIs <- renderPlot({
        
        planktonr::pr_plot_STI(selectedSTI())
        
      }) %>% bindCache(input$species1)
      
    })
    
    # daynight plot -----------------------------------------------------------------------------------------
    # Subset data
    
    observeEvent({input$NRSspatp == 3}, {
      
      selecteddn <- reactive({
        
        req(input$species2)
        shiny::validate(need(!is.na(input$species2), "Error: Please select a species"))
        
        selecteddn <- pkg.env$daynightp %>% 
          dplyr::filter(.data$Species %in% input$species2) 

        
      }) %>% bindCache(input$species2)
      
      # daynight plot
      output$DNs <- renderPlot({
        
        plotdn <- planktonr::pr_plot_DayNight(selecteddn())
        plotdn
        
      }) %>% bindCache(input$species2)
      
    })
    
  })
}

