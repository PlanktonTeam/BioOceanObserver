## Zooplankton
# Spatial

# source data required
load("data/ZooSpatial.RData")

# function for UI module
ZooSpatialUI <- function(id){
  
  nsZooSpatial <- NS(id)
  
  tagList(
    sidebarPanel(
      # Species selector
      selectizeInput(inputId = nsZooSpatial('species'), label = "Select a species", choices = unique(obs$Taxon), 
                     selected = "Acartia danae")
      ),
    mainPanel(
      h6(textOutput(nsZooSpatial("DistMapExp"), container = span)),      
      plotOutput(nsZooSpatial("plot2")) %>% withSpinner(color="#0dc5c1"),
      h6(textOutput(nsZooSpatial("SDMsMapExp"), container = span)),      
      imageOutput(nsZooSpatial("SDMs")) 
    )
  )
}

# function for server

ZooSpatial <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      #      Subset data
      selectedZS <- reactive({

        req(input$species)
        validate(need(!is.na(input$species), "Error: Please select a species"))
        
        obs %>%
          select(Sample, Taxon, Counts) %>%
          filter(Taxon %in% input$species) %>%
          left_join(SampLocs, by="Sample") %>%
          drop_na() %>%
          group_by(Season, Taxon, Lat, Long) %>%
          summarise(freq = n()) %>%
          left_join(Samples, by=c("Lat", "Long", "Season")) %>%
          ungroup() %>%
          mutate(freqsamp = freq/samples,
                 freqfac = as.factor(ifelse(freqsamp<0.375, "Seen in 25%",
                                            ifelse(freqsamp>0.875, "100 % of Samples",
                                                   ifelse(freqsamp>0.375 & freqsamp<0.625, '50%', '75%')))),
                 Season = as.factor(Season),
                 Season = factor(Season, levels = c("December - February","March - May","June - August","September - November")),
                 Taxon = as.factor(Taxon)) %>%
          select(Season, Lat, Long, Taxon, freqsamp, freqfac) 
      }) %>% bindCache(input$species)
      
     # Create plot object the plotOutput function is expecting
      output$plot2 <- renderPlot({
          dat <- absences %>% mutate(Taxon = input$species) %>% rbind(selectedZS()) %>%
          mutate(freqfac = factor(freqfac, levels = c("Absent", "Seen in 25%",'50%', '75%', "100 % of Samples")))
          cols <- c("lightblue1" ,"skyblue3", "dodgerblue2","blue1", "navyblue")
          colscale <- scale_colour_manual(name = '', values = cols, drop = FALSE)
          
          ggplot() + geom_sf(data = aus) +
          geom_point(data=dat, aes(x=Long, y=Lat, colour=freqfac), size = 2) + facet_wrap( ~ Season, dir = "v") +
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

        filename <- paste("www/SDMTweGAM_", input$species, ".png", sep = "")

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
    }
  )
}


