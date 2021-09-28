#' PigmentsBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PigmentsBGC_ui <- function(id){
  nsPigmentsBGC <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        plotlyOutput(nsPigmentsBGC("plotmap")),
        # station selector
        checkboxGroupInput(inputId = nsPigmentsBGC('station'), label = "Select a station", choices = unique(sort(Pigs$StationName)), selected = 'Port Hacking'),
        # Date selector
        dateRangeInput(nsPigmentsBGC("date"), "Select a date range", start = "2009-01-01", end = "2020-11-30", min = "2009-01-01", max = Sys.Date()),
        # select parameter
        selectizeInput(inputId = nsPigmentsBGC('parameter'), label = 'Select a parameter', choices = unique(Pigs$parameters), selected = 'TotalChla', multiple = FALSE),
        #selectizeInput(inputId = nsPigmentsBGC('depth'), label = 'Select a depth', choices = NULL, selected = '0'),
        # Select whether to overlay smooth trend line
        selectizeInput(inputId = nsPigmentsBGC("smoother"), label = strong("Overlay trend line"), choices = c("Smoother", "Linear", "None"), selected = "None")
      ),
      mainPanel(
        h6(textOutput(nsPigmentsBGC("PlotExp"), container = span)),
        plotlyOutput(nsPigmentsBGC("plot")) %>% withSpinner(color="#0dc5c1")
      )
    )
  )
}

#' PigmentsBGC Server Functions
#'
#' @noRd 
mod_PigmentsBGC_server <- function(id){
  moduleServer( id, function(input, output, session){
    #     select depths
    
    observe({
      req(input$station)
      req(input$parameter)
      validate(need(!is.na(input$station), "Error: Please select a station."))
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      # updateSelectizeInput(session, "depth", "Select a depth", server = TRUE, 
      #                      choices = NRSBGCPigments[NRSBGCPigments$Station %in% input$station & NRSBGCPigments$name %in% input$parameter,]$SampleDepth_m)
    })
    
    selected <- reactive({
      req(input$date)
      validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      Pigs %>%
        filter(.data$StationName %in% input$station,
               .data$SampleDateLocal > as.POSIXct(input$date[1]) & .data$SampleDateLocal < as.POSIXct(input$date[2]),
               .data$parameters %in% input$parameter) %>%
        mutate(Station = as.factor(.data$StationName),
               name = as.factor(.data$parameters),
               SampleDepth_m = round(SampleDepth_m, -1)) %>%
        tidyr::drop_na() 
    }) %>% bindCache(input$station, input$parameter, input$date)
    
    
    # Create timeseries object the plotOutput function is expecting
    output$plot <- renderPlotly({
      n <- length(unique(selected()$StationName))
      pal <- 'matter'
      plotCols <- planktonr::pr_get_PlotCols(pal, n)
      titley <- unique(selected()$parameters)
      
      p <- ggplot2::ggplot(selected(), ggplot2::aes(.data$SampleDateLocal, .data$Values, colour = .data$StationName)) + 
        ggplot2::geom_line() +
        ggplot2::labs(x = "Time", y = titley) +
        ggplot2::facet_grid(SampleDepth_m ~., scales = "free") +
        ggplot2::theme_bw() + ggplot2::theme(strip.background = ggplot2::element_blank(),
                                             strip.text = ggplot2::element_blank(),
                                             legend.position = "bottom", 
                                             legend.title = ggplot2::element_blank())+
        ggplot2::scale_colour_manual(values = plotCols)
      
      if(input$smoother == "Smoother"){
        p <- p + ggplot2::geom_smooth(method = 'loess', formula = y ~ x)
      }
      if(input$smoother == "Linear"){
        p <- p + ggplot2::geom_smooth(method = 'lm', formula = y ~ x)
      }
      
      np <- length(unique(selected()$SampleDepth_m))
      
      p <- plotly::ggplotly(p, height = 200 * np)
      
      mdat <- selected() %>% group_by(StationName, Month, SampleDepth_m, parameters) %>%
        summarise(MonValues = mean(.data$Values, na.rm = TRUE), 
                  N = length(.data$Values),
                  sd = stats::sd(.data$Values, na.rm = TRUE),
                  se = sd / sqrt(.data$N),
                  .groups = 'drop')
      
      m <- ggplot2::ggplot(mdat, aes(.data$Month, .data$MonValues, colour = .data$StationName)) +
        ggplot2::geom_point() +
        ggplot2::facet_grid(SampleDepth_m ~., scales = "free") +
        ggplot2::geom_smooth(method = 'loess', formula = y ~ x) +
        scale_x_continuous(breaks= seq(1,12,length.out = 12), labels=c("J", "F", "M", "A", "M", "J","J","A","S","O","N","D")) + 
        ggplot2::scale_colour_manual(values = plotCols) +
        ggplot2::theme_bw() + 
        ggplot2::theme(strip.background = ggplot2::element_blank(), 
                       legend.title = ggplot2::element_blank())
      
      m <- plotly::ggplotly(m) %>% layout(legend = list(orientation = "h", xanchor = "center",  # use center of legend as anchor
                                                        x = 0.5, y = -0.1))
      
      plot <- plotly::subplot(plotly::style(p, showlegend = FALSE), m, widths = c(0.75,0.25))
      
    }) %>% bindCache(input$station, input$parameter, input$date, input$smoother)
    
    # add a map in sidebar
    output$plotmap <- renderPlotly({ 
      
      pmap <- planktonr::pr_plot_NRSmap(selected())
      
    }) %>% bindCache(input$station)
    
    # add text information 
    output$PlotExp <- renderText({
      "A plot of selected nutrient parameters from the NRS as timeseries at analysed depths"
    }) 
    
    # create table output
    output$table <- DT::renderDataTable(
      selected() ) 
    
  })
}

## To be copied in the UI
# mod_PigmentsBGC_ui("PigmentsBGC_ui_1")

## To be copied in the server
# mod_PigmentsBGC_server("PigmentsBGC_ui_1")
