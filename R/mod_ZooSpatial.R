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
    sidebarPanel(
      selectizeInput(inputId = nsZooSpatial('species'), label = "Select a zooplankton species", choices = unique(fMapDataz$Taxon), 
                     selected = "Acartia danae")
    ),
    mainPanel(
      tabsetPanel(id = "NRSspat",
                  tabPanel("Observation maps", value = 1, 
                           h6(textOutput(nsZooSpatial("DistMapExp"), container = span)),
                           fluidRow(
                             column(width = 6,
                                    leaflet::leafletOutput(nsZooSpatial("plot2a"), width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1"),
                                    leaflet::leafletOutput(nsZooSpatial("plot2c"), width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                             ),
                             column(width = 6,
                                    leaflet::leafletOutput(nsZooSpatial("plot2b"), width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1"),
                                    leaflet::leafletOutput(nsZooSpatial("plot2d"), width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                             )
                           ),
                           column(width = 6, br(), 
                                  plotOutput(nsZooSpatial("plot2e"), height = 50)
                           )
                  ),
                  # tabPanel("Species Distribution maps", value = 2, 
                  #          h6(textOutput(nsZooSpatial("SDMsMapExp"), container = span)),
                  #          plotOutput(nsZooSpatial("SDMs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                  # ),
                  tabPanel("Species Temperature Index graphs", value = 2, 
                           h6(textOutput(nsZooSpatial("STIsExp"), container = span)),
                           plotOutput(nsZooSpatial("STIs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                  ),
                  tabPanel("Species Diurnal Behaviour", value = 3, 
                           h6(textOutput(nsZooSpatial("SDBsExp"), container = span)),
                           plotOutput(nsZooSpatial("DNs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                  ),
      )
    )
  )
}

#' ZooSpatial Server Functions
#'
#' @noRd 
mod_ZooSpatial_server <- function(id){
  moduleServer( id, function(input, output, session, NRSspat){
    # Subset data
    
    plotlist <- reactive({
      
      req(input$species)
      validate(need(!is.na(input$species), "Error: Please select a species"))
      
      selectedZS <- fMapDataz %>% 
        dplyr::mutate(Taxon = dplyr::if_else(.data$Taxon == 'Taxon', input$species, .data$Taxon)) %>%
        dplyr::filter(.data$Taxon %in% input$species) %>%
        dplyr::arrange(.data$freqfac)
      
      plotlist <- planktonr::pr_plot_FreqMap(selectedZS, species = input$species, interactive = TRUE)
      
    }) %>% bindCache(input$species)
    
    shiny::exportTestValues(
      ZooSpatial = {ncol(plotlist())},
      ZooSpatialRows = {nrow(plotlist()) > 0},
      ZooSpatialLatisNumeric = {class(plotlist()$Lat)},
      ZooSpatialLongisNumeric = {class(plotlist()$Long)},
      ZooSpatialFreqisFactor = {class(plotlist()$Freqfac)},
      ZooSpatialSeasonisChr = {class(plotlist()$Season)},
      ZooSpatialTaxonisChr = {class(plotlist()$Taxon)},
      ZooSpatialfreqsampisNumeric = {class(plotlist()$freqsamp)}
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
      paste("Figure of the diunral abundances from CPR data")
    }) 
    
    
    # select initial map  ------------------------------------------------------------------------------
    
    # Create dot map of distribution
    
    observeEvent({input$NRSspat == 1}, {
      output$plot2a <- leaflet::renderLeaflet({
        plotlist()[[1]]
      }) %>% bindCache(input$species)
      
      output$plot2b <- leaflet::renderLeaflet({
        plotlist()[[2]]
      }) %>% bindCache(input$species)
      
      output$plot2c <- leaflet::renderLeaflet({
        plotlist()[[3]]
      }) %>% bindCache(input$species)
      
      output$plot2d <- leaflet::renderLeaflet({
        plotlist()[[4]]
      }) %>% bindCache(input$species)
      
      output$plot2e <- renderPlot({
        legendPlot
      }) %>% bindCache(input$species)
      
    })  
    #   # add SDM if it is available
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
    
    observeEvent({input$NRSspat == 2}, {
      
      selectedSTI <- reactive({
        
        req(input$species)
        validate(need(!is.na(input$species), "Error: Please select a species"))
        
        selectedSTI <- stiz %>% 
          dplyr::filter(.data$Species %in% input$species) 
        
      }) %>% bindCache(input$species)
      
      # sti plot
      output$STIs <- renderPlot({
        
        validate(
          need(nrow(selectedSTI()) > 20, "Not enough data for this copepod species")
        )
        
        plotsti <- planktonr::pr_plot_STI(selectedSTI())
        plotsti
        
      }) %>% bindCache(input$species)
      
    })
    
    # daynight plot -----------------------------------------------------------------------------------------
    # Subset data
    
    observeEvent({input$NRSspat == 3}, {
      
      selecteddn <- reactive({
        
        req(input$species)
        validate(need(!is.na(input$species), "Error: Please select a species"))
        
        selecteddn <- daynightz %>% 
          dplyr::filter(.data$Species %in% input$species) 
        
      }) %>% bindCache(input$species)
      
      # daynight plot
      output$DNs <- renderPlot({
        
        validate(
          need(length(unique(selecteddn()$daynight)) == 2 | nrow(selecteddn()) > 20, "Not enough data for this copepod species to plot")
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

