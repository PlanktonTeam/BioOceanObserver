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
      selectizeInput(inputId = nsZooSpatial('species'), label = "Select a species", choices = unique(obs$Taxon), 
                     selected = "Acartia danae")
    ),
    mainPanel(
      h6(textOutput(nsZooSpatial("DistMapExp"), container = span)),      
      plotOutput(nsZooSpatial("plot2")) %>% shinycssloaders::withSpinner(color="#0dc5c1"),
      h6(textOutput(nsZooSpatial("SDMsMapExp"), container = span)),      
      imageOutput(nsZooSpatial("SDMs")) 
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
      
      obs %>%
        dplyr::select(.data$Sample, .data$Taxon, .data$Counts) %>%
        dplyr::filter(.data$Taxon %in% input$species) %>%
        dplyr::left_join(SampLocs, by="Sample") %>%
        tidyr::drop_na() %>%
        dplyr::group_by(.data$Season, .data$Taxon, .data$Lat, .data$Long) %>%
        dplyr::summarise(freq = n()) %>%
        dplyr::left_join(Samples, by=c("Lat", "Long", "Season")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(freqsamp = .data$freq/.data$samples,
               freqfac = as.factor(ifelse(.data$freqsamp<0.375, "Seen in 25%",
                                          ifelse(.data$freqsamp>0.875, "100 % of Samples",
                                                 ifelse(.data$freqsamp>0.375 & .data$freqsamp<0.625, '50%', '75%')))),
               Season = as.factor(.data$Season),
               Season = factor(.data$Season, levels = c("December - February","March - May","June - August","September - November")),
               Taxon = as.factor(.data$Taxon)) %>%
        dplyr::select(.data$Season, .data$Lat, .data$Long, .data$Taxon, .data$freqsamp, .data$freqfac) 
    }) %>% bindCache(input$species)
    
    # Create plot object the plotOutput function is expecting
    output$plot2 <- renderPlot({
      dat <- absences %>% mutate(Taxon = input$species) %>% rbind(selectedZS()) %>%
        mutate(freqfac = factor(.data$freqfac, levels = c("Absent", "Seen in 25%",'50%', '75%', "100 % of Samples")))
      cols <- c("lightblue1" ,"skyblue3", "dodgerblue2","blue1", "navyblue")
      colscale <- scale_colour_manual(name = '', values = cols, drop = FALSE)
      
      aust <- MapOz
      
      ggplot() + geom_sf(data = aust) +
        geom_point(data=dat, aes(x=.data$Long, y=.data$Lat, colour=.data$freqfac), size = 2) + facet_wrap( ~ .data$Season, dir = "v") +
        labs(title = input$species) + colscale +
        theme(strip.background = element_blank(),
              title = element_text(face = "italic"),
              legend.title = element_text(face = "plain"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_rect(fill = 'snow1'))
      
    }) %>% bindCache(input$species)
    
    # add SDM if it is available
    output$SDMs <- renderImage({
      
      speciesName <- stringr::str_replace_all(input$species, " ", "")
      filename <- paste("inst/app/www/SDMTweGAM_", speciesName, ".png", sep = "")
      
      list(src = filename,
           height = 400, width = 400,
           alt = 'Species Distribution Map not available')
      
    }, deleteFile = FALSE)
    
    # add text information 
    output$DistMapExp <- renderText({
      "This map is a frequency of occurence map based on the NRS and CPR data for each species"
    }) 
    output$SDMsMapExp <- renderText({
      "This map is a modelled output of the relative distribution for a species. This is calculated using NRS and CPR data in a Tweedie model. The environmental variables are SST, Chla, deth, Month"
    }) 
    
 
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
