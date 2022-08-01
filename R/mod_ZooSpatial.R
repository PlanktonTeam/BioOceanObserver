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
      conditionalPanel(
        condition="input.NRSspat == 1",  
        # Species selector
      selectizeInput(inputId = nsZooSpatial('species'), label = "Select a zooplankton species", choices = unique(fMapDataz$Taxon), 
                     selected = "Acartia danae")
      ),
      conditionalPanel(
        condition="input.NRSspat == 2",  
        # Species selector
        selectizeInput(inputId = nsZooSpatial('species1'), label = "Select a copepod species", choices = unique(stiz$Species), 
                       selected = "Acartia danae")
      )
    ),
    mainPanel(
      tabsetPanel(id = "NRSspat",
        tabPanel("Observation maps", value = 1, 
                 h6(textOutput(nsZooSpatial("DistMapExp"), container = span)),
                 plotOutput(nsZooSpatial("plot2"), height = 800) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                 ),
        tabPanel("Species Distribution maps", value = 2, 
                 h6(textOutput(nsZooSpatial("SDMsMapExp"), container = span)),
                 plotOutput(nsZooSpatial("SDMs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
        ),
        tabPanel("Species Temperature Index graphs", value = 2, 
                 h6(textOutput(nsZooSpatial("STIsExp"), container = span)),
                 plotOutput(nsZooSpatial("STIs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
        ),
        tabPanel("Species Diurnal Behviour", value = 2, 
                 h6(textOutput(nsZooSpatial("SDBsExp"), container = span)),
                 plotOutput(nsZooSpatial("DNs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
        )
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
      
      selectedZS <- reactive({
        
        req(input$species)
        validate(need(!is.na(input$species), "Error: Please select a species"))
        
        selectedZS <- fMapDataz %>% 
          dplyr::mutate(Taxon = dplyr::if_else(.data$Taxon == 'Taxon', input$species, .data$Taxon)) %>%
          dplyr::filter(.data$Taxon %in% input$species) %>%
          dplyr::mutate(freqfac = factor(.data$freqfac, levels = c("Absent", "Seen in 25%",'50%', '75%', "100 % of Samples"))) %>%
          dplyr::arrange(.data$freqfac)
        
      }) %>% bindCache(input$species)
      
      shiny::exportTestValues(
        ZooSpatial = {ncol(selectedZS())},
        ZooSpatialRows = {nrow(selectedZS()) > 0},
        ZooSpatialLatisNumeric = {class(selectedZS()$Lat)},
        ZooSpatialLongisNumeric = {class(selectedZS()$Long)},
        ZooSpatialFreqisFactor = {class(selectedZS()$Freqfac)},
        ZooSpatialSeasonisChr = {class(selectedZS()$Season)},
        ZooSpatialTaxonisChr = {class(selectedZS()$Taxon)},
        ZooSpatialfreqsampisNumeric = {class(selectedZS()$freqsamp)}
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
    output$plot2 <- renderPlot({
    
              plot2 <- planktonr::pr_plot_FreqMap(selectedZS())
              plot2
        
    }) %>% bindCache(input$species)
    
    # add SDM if it is available
    output$SDMs <- renderImage({

      speciesName <- stringr::str_replace_all(input$species1, " ", "")
      filename <- paste("inst/app/www/SDMTweGAM_", speciesName, ".png", sep = "")

      list(src = filename,
           height = 500, #width = 600,
           alt = 'Species Distribution Map not available')

    }, deleteFile = FALSE)

    # STI plot -----------------------------------------------------------------------------------------
    # Subset data
    
    selectedSTI <- reactive({
      
      req(input$species1)
      validate(need(!is.na(input$species1), "Error: Please select a species"))
      
      selectedSTI <- stiz %>% dplyr::rename(sst = .data$SST) %>% 
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
      
      selecteddn <- daynightz %>% 
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
    
