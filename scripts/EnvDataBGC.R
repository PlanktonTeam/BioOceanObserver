## Environmental variables
# BGC Parameters

# function for UI module

EnvDataBGCUI <- function(id){
  
  nsEnvDataBGC <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        # station selector
        selectInput(inputId = nsEnvDataBGC('station'), label = "Select a station", choices = unique(NRSBGCEnvData$Station), selected = 'Port Hacking', multiple = TRUE),
        # Date selector
        dateRangeInput(nsEnvDataBGC("date"), "Select a date range", start = "2009-01-01", end = "2020-11-30", min = "2009-01-01", max = Sys.Date()),
        # select parameter
        selectInput(inputId = nsEnvDataBGC('parameter'), label = 'Select a parameter', choices = unique(NRSBGCEnvData$name), selected = 'Silicate_umol_L', multiple = TRUE),
        selectInput(inputId = nsEnvDataBGC('depth'), label = 'Select a depth', choices = FALSE),
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = nsEnvDataBGC("smoother"), label = strong("Overlay smooth trend line"), value = FALSE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotlyOutput(nsEnvDataBGC("plot"))),
          tabPanel("Data table", DT::DTOutput(nsEnvDataBGC("table")))))  )
  )}

# function for server

EnvDataBGC <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
    # select depths
      NRSBGCEnvData <- read_csv("https://raw.githubusercontent.com/jaseeverett/IMOS_Toolbox/master/Plankton/Output/NRS_CombinedWaterQuality.csv") %>% 
        pivot_longer(-c(NRScode:IMOSsampleCode)) %>% drop_na()
      
      observe({
        updateSelectInput(session, "depth", "Select a depth", 
                          choices = NRSBGCEnvData[NRSBGCEnvData$Station == input$station & NRSBGCEnvData$name == input$parameter,]$SampleDepth_m)
      })
      
      # Subset data
      selected <- reactive({
        req(input$date)
        validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
        validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
        NRSBGCEnvData %>%
          filter(Station %in% input$station,
                 SampleDateLocal > as.POSIXct(input$date[1]) & SampleDateLocal < as.POSIXct(input$date[2]),
                 name %in% input$parameter,
                 SampleDepth_m == input$depth) %>%
          mutate(Station = as.factor(Station),
                 name = as.factor(name))
      })
      
      # Create timeseries object the plotOutput function is expecting
      output$plot <- renderPlotly({
        p <- ggplot(selected()) + geom_line(aes(SampleDateLocal, value, colour = Station)) +
          labs(x = "Time") +
          theme_bw()
        if(nlevels(unique(selected()$name)) > 1){
          p <- p + facet_grid(name~., scales = "free")
        }
        if(input$smoother){
          p <- p + geom_smooth(aes(SampleDateLocal, value, colour = Station))
        }
        ggplotly(p)
        
      })
      
      # # create table output
      output$table <- DT::renderDataTable(
        selected() )
})}