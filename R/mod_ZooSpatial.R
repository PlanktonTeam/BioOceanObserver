#' ZooSpatial UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZooSpatial_ui <- function(id){
  
  nsZooSpatial <- NS(id)
  
  tagList(
    sidebarPanel(
      # Species selector
      selectizeInput(inputId = nsZooSpatial('species'), label = "Select a species", choices = unique(fMapData$Taxon), 
                     selected = "Acartia danae")
    ),
    mainPanel(
      tabsetPanel(id = "NRSspat",
        tabPanel("Observation maps",
                 h6(textOutput(nsZooSpatial("DistMapExp"), container = span)),
                 plotOutput(nsZooSpatial("plot2"), height = 800) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                 ),
        tabPanel("Species Distribution maps",
                 h6(textOutput(nsZooSpatial("SDMsMapExp"), container = span)),
                 plotOutput(nsZooSpatial("SDMs"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
        ),
        tabPanel("Species Temperature Index graphs",
                 h6(textOutput(nsZooSpatial("STIsExp"), container = span))
        ),
        tabPanel("Species Diurnal Behviour",
                 h6(textOutput(nsZooSpatial("SDBsExp"), container = span))
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
        
        selectedZS <- fMapData %>% 
          dplyr::mutate(Taxon = ifelse(Taxon == "Taxon", input$species, .data$Taxon)) %>%
          dplyr::filter(.data$Taxon %in% input$species) %>%
          dplyr::mutate(freqfac = factor(.data$freqfac, levels = c("Absent", "Seen in 25%",'50%', '75%', "100 % of Samples"))) %>%
          dplyr::arrange(.data$freqfac)
        
      }) %>% bindCache(input$species)
      
      # add text information ------------------------------------------------------------------------------
      output$DistMapExp <- renderText({
        "This map is a frequency of occurence map based on the NRS and CPR data for each species"
      }) 
      output$SDMsMapExp <- renderText({
        paste("This map is a modelled output of the relative distribution for a species.",
              "This is calculated using NRS and CPR data in a Tweedie model.",
              "The environmental variables are SST, Chla, deth, Month", sep =  "<br/>")
      }) 
      output$STIsExp <- renderText({
        paste("Figure of the species STI")
      }) 
      output$SDBsExp <- renderText({
        paste("Figure of the diunral abundances from CPR data")
      }) 
      
      
      # select initial map  ------------------------------------------------------------------------------
      
    # Create plot object the plotOutput function is expecting
    output$plot2 <- renderPlot({
    
              plot2 <- planktonr::pr_plot_fmap(selectedZS())
              plot2
        
    }) %>% bindCache(input$species)
    
    # add SDM if it is available
    output$SDMs <- renderImage({

      speciesName <- stringr::str_replace_all(input$species, " ", "")
      filename <- paste("inst/app/www/SDMTweGAM_", speciesName, ".png", sep = "")

      list(src = filename,
           height = 500, #width = 600,
           alt = 'Species Distribution Map not available')

    }, deleteFile = FALSE)

    
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
    
