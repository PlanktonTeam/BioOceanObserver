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
      splitLayout(cellWidths = c("50%", "50%"), h6(textOutput(nsZooSpatial("DistMapExp"), container = span)), h6(htmlOutput(nsZooSpatial("SDMsMapExp")))),     
      plotOutput(nsZooSpatial("plot2"), height = 700) %>% shinycssloaders::withSpinner(color="#0dc5c1")
    )
  )
}
    
#' ZooSpatial Server Functions
#'
#' @noRd 
mod_ZooSpatial_server <- function(id){
  moduleServer( id, function(input, output, session){
    #      Subset data
    selectedZS <- reactive({
      
      req(input$species)
      validate(need(!is.na(input$species), "Error: Please select a species"))
      
      selectedZS <- fMapData %>% 
        dplyr::mutate(Taxon = ifelse(Taxon == "Taxon", input$species, Taxon)) %>%
        dplyr::filter(.data$Taxon %in% input$species) %>%
        dplyr::mutate(freqfac = factor(.data$freqfac, levels = c("Absent", "Seen in 25%",'50%', '75%', "100 % of Samples")))
      
    }) %>% bindCache(input$species)

    # Create plot object the plotOutput function is expecting
    output$plot2 <- renderPlot({
    
        Species <- input$species
        plot2 <- planktonr::pr_plot_fmap(selectedZS(), Species)
      
    }) %>% bindCache(input$species)
    
    # # add SDM if it is available
    # output$SDMs <- renderImage({
    #   
    #   speciesName <- stringr::str_replace_all(input$species, " ", "")
    #   filename <- paste("inst/app/www/SDMTweGAM_", speciesName, ".png", sep = "")
    #   
    #   list(src = filename,
    #        height = 500, #width = 600,
    #        alt = 'Species Distribution Map not available')
    #   
    # }, deleteFile = FALSE)
    # 
    
    # add text information 
    output$DistMapExp <- renderText({
      "This map is a frequency of occurence map based on the NRS and CPR data for each species"
    }) 
    output$SDMsMapExp <- renderText({
      paste("This map is a modelled output of the relative distribution for a species.",
      "This is calculated using NRS and CPR data in a Tweedie model.",
      "The environmental variables are SST, Chla, deth, Month", sep =  "<br/>")
    }) 
    
 
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
