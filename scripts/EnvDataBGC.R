## Environmental Data
# BGC Parameters

# source data required
load("data/envDataBGC.RData")

# function for UI module

EnvDataBGCUI <- function(id){
  
  nsEnvDataBGC <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
        # station selector
        selectizeInput(inputId = nsEnvDataBGC('station'), label = "Select a station", choices = unique(NRSBGCEnvData$Station), 
                    selected = 'Port Hacking', multiple = TRUE),
        # Date selector
        dateRangeInput(nsEnvDataBGC("date"), "Select a date range", start = "2009-01-01", end = "2020-11-30", min = "2009-01-01", max = Sys.Date()),
        # select parameter
        selectizeInput(inputId = nsEnvDataBGC('parameter'), label = 'Select a parameter', choices = unique(NRSBGCEnvData$name), selected = 'Silicate_umolL', multiple = TRUE),
        selectInput(inputId = nsEnvDataBGC('depth'), label = 'Select a depth', choices = FALSE),
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = nsEnvDataBGC("smoother"), label = strong("Overlay smooth trend line"), value = FALSE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotlyOutput(nsEnvDataBGC("plot")) %>% withSpinner(color="#0dc5c1")),
          tabPanel("Data table", DT::DTOutput(nsEnvDataBGC("table"))))))
  )}

# function for server

EnvDataBGC <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
#     select depths
      observe({
         req(input$station)
         req(input$parameter)
         validate(need(!is.na(input$station), "Error: Please select a station."))
         validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
         updateSelectInput(session, "depth", "Select a depth",
                           choices = NRSBGCEnvData[NRSBGCEnvData$Station == input$station & NRSBGCEnvData$name == input$parameter,]$SampleDepth_m)
     })

#      Subset data
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
      }) %>% bindCache(input$station, input$parameter, input$date, input$depth)


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

      }) %>% bindCache(input$station, input$parameter, input$date, input$depth, input$smoother)

      # create table output
      output$table <- DT::renderDataTable(
        selected() ) 
    })}

