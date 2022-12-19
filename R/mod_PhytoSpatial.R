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
      sidebarPanel(
        selectizeInput(inputId = nsPhytoSpatial('species'), label = "Select a phytoplankton species", choices = unique(fMapDatap$Species), 
                       selected = "Tripos furca"),
        shiny::checkboxInput(inputId = nsPhytoSpatial("scaler1"), 
                             label = strong("Change between frequency or Presence/Absence plot"), 
                             value = FALSE)
      ),
      mainPanel(
        tabsetPanel(id = "NRSspatp",
                    tabPanel("Observation maps", value = 1, 
                             h6(textOutput(nsPhytoSpatial("DistMapExp"), container = span)),
                             fluidRow(
                               shiny::column(width = 6,
                                             style = "padding:0px; margin:0px;",
                                             shiny::h4("December - February"),
                                             leaflet::leafletOutput(nsPhytoSpatial("PSMapSum"), width = "99%", height = "300px") %>% 
                                               shinycssloaders::withSpinner(color="#0dc5c1")), 
                               shiny::column(width = 6,
                                             style = "padding:0px; margin:0px;",
                                             shiny::h4("March - May"),
                                             leaflet::leafletOutput(nsPhytoSpatial("PSMapAut"), width = "99%", height = "300px") %>%
                                               shinycssloaders::withSpinner(color="#0dc5c1")
                               ),
                               shiny::column(width = 6,
                                             style = "padding:0px; margin:0px;",
                                             shiny::h4("June - August"),
                                             leaflet::leafletOutput(nsPhytoSpatial("PSMapWin"), width = "99%", height = "300px") %>% 
                                               shinycssloaders::withSpinner(color="#0dc5c1")), 
                               shiny::column(width = 6,
                                             style = "padding:0px; margin:0px;",
                                             shiny::h4("September - February"),
                                             leaflet::leafletOutput(nsPhytoSpatial("PSMapSpr"), width = "99%", height = "300px") %>% 
                                               shinycssloaders::withSpinner(color="#0dc5c1"))
                             )
                    ),        
                    #tabPanel("Species Distribution maps", value = 2, 
                    #                 h6(textOutput(nsPhytoSpatial("SDMsMapExp"), container = span))#,
                    #                 plotOutput(nsPhytoSpatial("SDMs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    #        ),
                    tabPanel("Species Temperature Index graphs", value = 2, 
                             h6(textOutput(nsPhytoSpatial("STIsExp"), container = span)),
                             plotOutput(nsPhytoSpatial("STIs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Species Diurnal Behaviour", value = 3, 
                             h6(textOutput(nsPhytoSpatial("SDBsExp"), container = span)),
                             plotOutput(nsPhytoSpatial("DNs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    )
        )
      )
    )
}

#' PhytoSpatial Server Functions
#'
#' @noRd 
mod_PhytoSpatial_server <- function(id){
  moduleServer( id, function(input, output, session, NRSspatp){
    # Subset data
    
    PSdatar <- reactive({
      
      req(input$species)
      validate(need(!is.na(input$species), "Error: Please select a species"))
      
      PSdatar <- fMapDatap %>%
        dplyr::filter(.data$Species == input$species) %>%
        dplyr::arrange(.data$freqfac)
      
    }) %>% bindCache(input$species)
    
    AbsPdatar <- reactive({
      
      AbsPdatar <- fMapDatap %>%
        dplyr::distinct(.data$Longitude, .data$Latitude, .data$Season) 
      
    }) %>% bindCache(input$species)
    
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
      "This map is a frequency of occurence map based on the NRS and CPR data for each species"
    }) 
    output$SDMsMapExp <- renderText({
      paste("This map is a modelled output of the relative distribution for a species.",
            "This is calculated using NRS and CPR data in a Tweedie model.",
            "The environmental variables are SST, Chla, depth, Month", sep =  "<br/>")
    }) 
    output$STIsExp <- renderText({
      paste("Figure of the species STI")
    }) 
    output$SDBsExp <- renderText({
      paste("Figure of the diurnal abundances from CPR data")
    }) 
    
    
    # select initial map  ------------------------------------------------------------------------------
    
    observeEvent({input$NRSspatp == 1}, {
      # Create dot map of distribution
    # Summer
      output$PSMapSum <- leaflet::renderLeaflet({
      lf <- LeafletBase(AbsPdatar())
      return(lf)
    }) %>%  bindCache(input$species, input$scaler1)
    
    # Autumn
    output$PSMapAut <- leaflet::renderLeaflet({
      lf <- LeafletBase(AbsPdatar())
      return(lf)
    }) %>%  bindCache(input$species, input$scaler1)
    
    # Winter
    output$PSMapWin <- leaflet::renderLeaflet({
      lf <- LeafletBase(AbsPdatar())
      return(lf)
    }) %>%  bindCache(input$species, input$scaler1)
    
    # Spring
    output$PSMapSpr <- leaflet::renderLeaflet({
      lf <- LeafletBase(AbsPdatar())
      return(lf)
    }) %>%  bindCache(input$species, input$scaler1)

    observe ({
      
      type <- dplyr::if_else(input$scaler1, "frequency", "Presence/Absence")
      
      LeafletObs(sdf = PSdatar() %>% dplyr::filter(.data$Season == "December - February"), name = "PSMapSum", Type = type)
      LeafletObs(sdf = PSdatar() %>% dplyr::filter(.data$Season == "September - November"), name = "PSMapAut", Type = type)
      LeafletObs(sdf = PSdatar() %>% dplyr::filter(.data$Season == "June - August"), name = "PSMapWin", Type = type)
      LeafletObs(sdf = PSdatar() %>% dplyr::filter(.data$Season == "March - May"), name = "PSMapSpr", Type = type)
    
    }) 
    
    })
    
    # # add SDM if it is available
    # output$SDMs <- renderImage({
    # 
    #   speciesName <- stringr::str_replace_all(input$species1, " ", "")
    #   filename <- paste("inst/app/www/SDMTweGAM_", speciesName, ".png", sep = "")
    # 
    #   list(src = filename,
    #        height = 500, #width = 600,
    #        alt = 'Species Distribution Map not available')
    # 
    # }, deleteFile = FALSE)
    
    # STI plot -----------------------------------------------------------------------------------------
    # Subset data
    
    observeEvent({input$NRSspatp == 2}, {
      
      selectedSTI <- reactive({
        
        req(input$species)
        validate(need(!is.na(input$species), "Error: Please select a species"))
        
        selectedSTI <- stip %>% 
          dplyr::filter(.data$Species %in% input$species) 
        
      }) %>% bindCache(input$species)
      
      # sti plot
      output$STIs <- renderPlot({
        
        validate(
          need(nrow(selectedSTI()) > 20, "Not enough data for this copepod species")
        )
        
        planktonr::pr_plot_STI(selectedSTI())
        
      }) %>% bindCache(input$species)
      
    })
    
    # daynight plot -----------------------------------------------------------------------------------------
    # Subset data
    
    observeEvent({input$NRSspatp == 3}, {
      
      selecteddn <- reactive({
        
        req(input$species)
        validate(need(!is.na(input$species), "Error: Please select a species"))
        
        selecteddn <- daynightp %>% 
          dplyr::filter(.data$Species %in% input$species) 
        
      }) %>% bindCache(input$species)
      
      # daynight plot
      output$DNs <- renderPlot({
        
        validate(
          need(length(unique(selecteddn()$daynight)) == 2 | nrow(selecteddn()) > 20, "Not enough data for this species to plot")
        )
        
        plotdn <- planktonr::pr_plot_DayNight(selecteddn())
        plotdn
        
      }) %>% bindCache(input$species)
      
    })
    
    
    # speciesName <- stringr::str_replace_all(Species, " ", "")
    # filename <- paste("inst/app/www/SDMTweGAM_", speciesName, ".png", sep = "")
    # img <- tryCatch(png::readPNG(filename), error = function(e){})
    # dft <-  data.frame(x=c(1,1,1,1), y=c(0,2,1,3), label = c('','No species distribution','map available',''))
    # imggrob <- tryCatch(grid::rasterGrob(img), error = function(e) {
    #   ggplot2::ggplot(dft) +
    #     ggplot2::geom_text(ggplot2::aes(x=.data$x, y=.data$y, label = .data$label), size = 20) +
    #     ggplot2::theme_void()
    # })
    
   
  })
}

