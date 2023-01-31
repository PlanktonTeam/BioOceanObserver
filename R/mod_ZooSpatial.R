#' ZooSpatial UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZooSpatial_ui <- function(id){
  
  nsZooSpatial <- NS(id)
  
  tagList(
    sidebarLayout(
      fSpatialSidebar(id = id, tabsetPanel_id = "NRSspatz", dat1 = fMapDataz, dat2 = stiz, dat3 = daynightz),
      fSpatialPanel(id= id, tabsetPanel_id = "NRSspatz")
    )
  )
}

#' ZooSpatial Server Functions
#'
#' @noRd 
mod_ZooSpatial_server <- function(id){
  moduleServer( id, function(input, output, session, NRSspatz){
    # Subset data
    
    type <- dplyr::if_else(input$scaler1, "frequency", "PA")
    
    AbsZdatar <- reactive({
      
      AbsZdatar <- fMapDataz %>%
        dplyr::distinct(.data$Longitude, .data$Latitude, .data$Season, .data$Survey) %>% 
        dplyr::mutate(Species = input$species, 
                      freqfac = factor('Absent'))
      
    }) %>% bindCache(input$species)
    
    ZSdatar <- reactive({
      
      req(input$species)
      validate(need(!is.na(input$species), "Error: Please select a species"))
      
      type <- dplyr::if_else(input$scaler1, "frequency", "PA")
      
      ZSdatar <- fMapDataz %>%
        dplyr::filter(.data$Species == input$species) 
      
      if(type == "frequency"){
        ZSdatar <- ZSdatar %>% dplyr::bind_rows(AbsZdatar()) %>%
          dplyr::mutate(freqfac = factor(.data$freqfac, levels = c("Absent", "Seen in 25%",'50%', '75%', '100% of Samples')))
      } else {
        ZSdatar
      }
      
    }) %>% bindCache(input$species, input$scaler1)
    
    shiny::exportTestValues(
      ZooSpatial = {ncol(ZSdatar())},
      ZooSpatialRows = {nrow(ZSdatar()) > 0},
      ZooSpatialLatisNumeric = {class(ZSdatar()$Lat)},
      ZooSpatialLongisNumeric = {class(ZSdatar()$Long)},
      ZooSpatialFreqisFactor = {class(ZSdatar()$Freqfac)},
      ZooSpatialSeasonisChr = {class(ZSdatar()$Season)}
    )
    
    # add text information ------------------------------------------------------------------------------
    output$DistMapExp <- renderText({
      "This map is either a presence absence map based on the NRS and CPR data for each species or a frequency map based on the 
      number of times a species is seen in the sample location"
    }) 
    output$STIsExp <- renderText({
      paste("Figure of the species STI showing the temperature range at which this species is most common. 
            A bimodal shape may indicate a sub-species or two species being identified as the same species")
    }) 
    output$SDBsExp <- renderText({
      paste("Figure of the diurnal abundances from CPR data. Note that the CPR is towed at ~10m depth so these abundances are representative of surface waters")
    }) 
    
    
    # select initial map  ------------------------------------------------------------------------------
    
    observeEvent({input$NRSspat == 1}, {
      # Create dot map of distribution
      # Summer
      output$MapSum <- leaflet::renderLeaflet({
        type <- dplyr::if_else(input$scaler1, "frequency", "PA")
        lf <- LeafletBase(AbsZdatar(), Type = type)
        return(lf)
      }) %>%  bindCache(input$species, input$scaler1)
      
      # Autumn
      output$MapAut <- leaflet::renderLeaflet({
        type <- dplyr::if_else(input$scaler1, "frequency", "PA")
        lf <- LeafletBase(AbsZdatar(), Type = type)
        return(lf)
      }) %>%  bindCache(input$species, input$scaler1)
      
      # Winter
      output$MapWin <- leaflet::renderLeaflet({
        type <- dplyr::if_else(input$scaler1, "frequency", "PA")
        lf <- LeafletBase(AbsZdatar(), Type = type)
        return(lf)
      }) %>%  bindCache(input$species, input$scaler1)
      
      # Spring
      output$MapSpr <- leaflet::renderLeaflet({
        type <- dplyr::if_else(input$scaler1, "frequency", "PA")
        lf <- LeafletBase(AbsZdatar(), Type = type)
        return(lf)
      }) %>%  bindCache(input$species, input$scaler1)
      
      observe ({
        type <- dplyr::if_else(input$scaler1, "frequency", "PA")
        LeafletObs(sdf = ZSdatar() %>% dplyr::filter(.data$Season == "December - February"), name = "MapSum", Type = type)
        LeafletObs(sdf = ZSdatar() %>% dplyr::filter(.data$Season == "September - November"), name = "MapAut", Type = type)
        LeafletObs(sdf = ZSdatar() %>% dplyr::filter(.data$Season == "June - August"), name = "MapWin", Type = type)
        LeafletObs(sdf = ZSdatar() %>% dplyr::filter(.data$Season == "March - May"), name = "MapSpr", Type = type)
        
      }) 
      
    })
    
    # STI plot -----------------------------------------------------------------------------------------
    # Subset data
    
    observeEvent({input$NRSspat == 2}, {
      
      selectedSTI <- reactive({
        
        req(input$species1)
        validate(need(!is.na(input$species1), "Error: Please select a species"))
        
        selectedSTI <- stiz %>% 
          dplyr::filter(.data$Species %in% input$species1) 
        
      }) %>% bindCache(input$species1)
      
      # sti plot
      output$STIs <- renderPlot({
        
        validate(
          need(nrow(selectedSTI()) > 20, "Not enough data for this copepod species")
        )
        
        planktonr::pr_plot_STI(selectedSTI())
        
        
      }) %>% bindCache(input$species1)
      
    })
    
    # daynight plot -----------------------------------------------------------------------------------------
    # Subset data
    
    observeEvent({input$NRSspat == 3}, {
      
      selecteddn <- reactive({
        
        req(input$species2)
        validate(need(!is.na(input$species2), "Error: Please select a species"))
        
        selecteddn <- daynightz %>% 
          dplyr::filter(.data$Species %in% input$species2) 
        
      }) %>% bindCache(input$species2)
      
      # daynight plot
      output$DNs <- renderPlot({
        
        validate(
          need(length(unique(selecteddn()$daynight)) == 2 | nrow(selecteddn()) > 20, "Not enough data for this copepod species to plot")
        )
        
        plotdn <- planktonr::pr_plot_DayNight(selecteddn())
        plotdn
        
      }) %>% bindCache(input$species2)
      
    })
    

    
    
  })
}

