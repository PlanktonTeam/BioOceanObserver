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
    sidebarPanel(
      conditionalPanel(
        condition="input.NRSspatp == 1",  
      #Species selector
      selectizeInput(inputId = nsPhytoSpatial('species'), label = "Select a phytoplankton species", choices = unique(fMapDatap$Taxon),
                     selected = "Tripos furca")
      ),
      conditionalPanel(
        condition="input.NRSspatp == 2",  
        # Species selector
        selectizeInput(inputId = nsPhytoSpatial('species1'), label = "Select a phytoplankton species", choices = unique(stip$Species), 
                       selected = "Tripos furca")
      )
    ),
    mainPanel(
      tabsetPanel(id = "NRSspatp",
        tabPanel("Observation maps", value = 1, 
                 h6(textOutput(nsPhytoSpatial("DistMapExp"), container = span)),
                 fluidRow(
                   column(width = 6,
                          leaflet::leafletOutput(nsPhytoSpatial("plot2a"), width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1"),
                          leaflet::leafletOutput(nsPhytoSpatial("plot2c"), width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                   ),
                   column(width = 6,
                          leaflet::leafletOutput(nsPhytoSpatial("plot2b"), width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1"),
                          leaflet::leafletOutput(nsPhytoSpatial("plot2d"), width = "100%") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                   )
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
        tabPanel("Species Diurnal Behviour", value = 2, 
                 h6(textOutput(nsPhytoSpatial("SDBsExp"), container = span)),
                 plotOutput(nsPhytoSpatial("DNs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
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
      
      plotlist <- reactive({
        
        req(input$species)
        validate(need(!is.na(input$species), "Error: Please select a species"))
        
        selectedZS <- fMapDatap %>%
          dplyr::mutate(Taxon = dplyr::if_else(.data$Taxon == 'Taxon', input$species, .data$Taxon)) %>%
          dplyr::filter(.data$Taxon == input$species) %>%
          dplyr::arrange(.data$freqfac)
        
        plotlist <- planktonr::pr_plot_FreqMap(selectedZS, species = input$species, interactive = TRUE)
        
      }) %>% bindCache(input$species)
      
      shiny::exportTestValues(
        PhytoSpatial = {ncol(selectedZS())},
        PhytoSpatialRows = {nrow(selectedZS()) > 0},
        PhytoSpatialLatisNumeric = {class(selectedZS()$Lat)},
        PhytoSpatialLongisNumeric = {class(selectedZS()$Long)},
        PhytoSpatialFreqisFactor = {class(selectedZS()$Freqfac)},
        PhytoSpatialSeasonisChr = {class(selectedZS()$Season)},
        PhytoSpatialTaxonisChr = {class(selectedZS()$Taxon)},
        PhytoSpatialfreqsampisNumeric = {class(selectedZS()$freqsamp)}
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
    
    selectedSTI <- reactive({
      
      req(input$species1)
      validate(need(!is.na(input$species1), "Error: Please select a species"))
      
      selectedSTI <- stip %>% 
        dplyr::filter(.data$Species %in% input$species1) 
      
    }) %>% bindCache(input$species1)
    
    # sti plot
    output$STIs <- renderPlot({
      
      validate(
        need(nrow(selectedSTI()) > 20, "Not enough data for this copepod species")
      )
      
      plotsti <- planktonr::pr_plot_STI(selectedSTI())
      plotsti

    }) %>% bindCache(input$species1)
    
    # daynight plot -----------------------------------------------------------------------------------------
    # Subset data
    
    selecteddn <- reactive({
      
      req(input$species1)
      validate(need(!is.na(input$species1), "Error: Please select a species"))
      
      selecteddn <- daynightp %>% 
        dplyr::filter(.data$Species %in% input$species1) 
      
    }) %>% bindCache(input$species1)
    
    # sti plot
    output$DNs <- renderPlot({

      validate(
        need(length(unique(selecteddn()$daynight)) == 2 | nrow(selecteddn()) > 20, "Not enough data for this copepod species to plot")
      )
      
      plotdn <- planktonr::pr_plot_DayNight(selecteddn())
      plotdn
      
    }) %>% bindCache(input$species1)

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
    
