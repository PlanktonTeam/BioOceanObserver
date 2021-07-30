#' EnvDataBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_EnvDataBGC_ui <- function(id){
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
        selectizeInput(inputId = nsEnvDataBGC('depth'), label = 'Select a depth', choices = NULL, selected = '0'),
        # Select whether to overlay smooth trend line
        checkboxInput(inputId = nsEnvDataBGC("smoother"), label = strong("Overlay smooth trend line"), value = FALSE)
      ),
      mainPanel( 
        tabsetPanel(
          tabPanel("Plot", 
                   h6(textOutput(nsEnvDataBGC("PlotExp"), container = span)),
                   plotlyOutput(nsEnvDataBGC("plot")) %>% withSpinner(color="#0dc5c1")),
          tabPanel("Data table", DT::DTOutput(nsEnvDataBGC("table")))
          )
        )
      )
  )
}
    
#' EnvDataBGC Server Functions
#'
#' @noRd 
mod_EnvDataBGC_server <- function(id){
  moduleServer( id, function(input, output, session){
    #     select depths

    observe({
      req(input$station)
      req(input$parameter)
      validate(need(!is.na(input$station), "Error: Please select a station."))
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      updateSelectizeInput(session, "depth", "Select a depth", server = TRUE, 
                           choices = NRSBGCEnvData[NRSBGCEnvData$Station %in% input$station & NRSBGCEnvData$name %in% input$parameter,]$SampleDepth_m)
    })
    
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
        labs(x = "Time", y = input$parameter) +
        theme_bw() + theme(strip.background = element_blank(),
                           legend.position = "bottom", 
                           legend.title = element_blank())
      if(nlevels(unique(selected()$name)) > 1){
        p <- p + facet_grid(name~., scales = "free") +
          labs(y = "value")
             }
      if(input$smoother){
        p <- p + geom_smooth(aes(SampleDateLocal, value, colour = Station), method = 'loess', formula = y ~ x)
      }
      if(nlevels(unique(selected()$name)) < 1){
        np  <-  1
      } else {
        np <- nlevels(unique(selected()$name))
      }
      
      # prevent error messaging when depth isn't available for newly selected parameter
      b <- ggplotly(ggplot() + geom_blank() + theme_minimal() + theme(panel.border = element_blank()))
      tryCatch(ggplotly(p, height = 200 * np) %>%
        layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
                             x = 0.5, yanchor = "bottom")), error = function(e){b})
      
    }) %>% bindCache(input$station, input$parameter, input$date, input$depth, input$smoother)
    
    # add text information 
    output$PlotExp <- renderText({
      "A plot of selected environmental parameters from the NRS as timeseries at the selected depth"
    }) 
    
    # create table output
    output$table <- DT::renderDataTable(
      selected() ) 
 
  })
}
    
## To be copied in the UI
# mod_EnvDataBGC_ui("EnvDataBGC_ui_1")
    
## To be copied in the server
# mod_EnvDataBGC_server("EnvDataBGC_ui_1")
