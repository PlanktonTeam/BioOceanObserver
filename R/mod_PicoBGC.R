#' PicoBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PicoBGC_ui <- function(id){
  nsPicoBGC <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        style = "padding:1%;",
        tags$head(tags$style(HTML( #TODO move to custom css
          ".multicol{
          height:auto;
          -webkit-column-count: 2;
          -moz-column-count: 2;
          column-count: 2;}"))),
        # shiny::div(
        # style = "padding:0px; margin:0px; max-height: 1000px;", #bottom: 0px; left: 0px; right: 0px; max-width: 1000px;  min-height: 10px
        shiny::plotOutput(nsPicoBGC("plotmap"),
                          width = "100%"),
        # ),
        shiny::HTML("<h5><strong>Select a station:</strong></h5>"),
        shiny::fluidRow(tags$div(align = "left", 
                                 class = "multicol",
                                 shiny::checkboxGroupInput(inputId = nsPicoBGC("station"), 
                                                           label = NULL,
                                                           choices = NRSStation %>% 
                                                             dplyr::filter(!.data$StationCode %in% c("PH4", "NIN", "ESP")) %>% 
                                                             dplyr::pull(.data$StationName), 
                                                           selected = "Port Hacking"))),
        sliderInput(nsPicoBGC("date"), "Dates:", min = lubridate::ymd(20090101), max = Sys.Date(), 
                    value = c(lubridate::ymd(20090101), Sys.Date()-1), timeFormat="%Y-%m-%d"),
        # select parameter
        selectizeInput(inputId = nsPicoBGC('parameter'), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(Pico$Parameters), style = "simple"), selected = 'Prochlorococcus_cellsmL', multiple = FALSE),
        # Select whether to interpolate or not
        selectizeInput(inputId = nsPicoBGC("interp"), label = strong("Interpolate data?"), choices = c("Interpolate", "Raw data", "Interpolate with gap filling"), selected = "Interpolate"),
        shiny::br(), # Give a bit of space for the menu to expand
        shiny::br()
      ),
      fEnviroPanel(id = id)
    )
  )
}

#' PicoBGC Server Functions
#'
#' @noRd 
mod_PicoBGC_server <- function(id){
  moduleServer( id, function(input, output, session){
    #     select depths
    
    observe({
      req(input$station)
      req(input$parameter)
      validate(need(!is.na(input$station), "Error: Please select a station."))
      validate(need(!is.na(input$parameter), "Error: Please select a parameter."))
      # updateSelectizeInput(session, "depth", "Select a depth", server = TRUE, 
      #                      choices = NRSBGCPico[NRSBGCPico$Station %in% input$station & NRSBGCPico$name %in% input$parameter,]$SampleDepth_m)
    })
    
    selected <- reactive({
      req(input$date)
      validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
      validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
      Pico %>%
        dplyr::filter(.data$StationName %in% input$station,
                      .data$SampleTime_Local > as.POSIXct(input$date[1]) & .data$SampleTime_Local < as.POSIXct(input$date[2]),
                      .data$Parameters %in% input$parameter) %>%
        dplyr::mutate(name = as.factor(.data$Parameters)) %>%
        tidyr::drop_na() 
    }) %>% bindCache(input$station, input$parameter, input$date)
    
    shiny::exportTestValues(
      PicoBGC = {ncol(selected())},
      PicoBGCRows = {nrow(selected()) > 0},
      PicoBGCProjectisChr = {class(selected()$Project)},
      PicoBGCMonthisNumeric = {class(selected()$Month_Local)},
      PicoBGCDepthisNumeric = {class(selected()$SampleDepth_m)},
      PicoBGCDateisDate = {class(selected()$SampleTime_Local)},
      PicoBGCStationisFactor = {class(selected()$StationName)},
      PicoBGCCodeisChr = {class(selected()$StationCode)},
      PicoBGCParametersisChr = {class(selected()$Parameters)},
      PicoBGCValuesisNumeric = {class(selected()$Values)}
    )
    
    # Create timeseries object the plotOutput function is expecting
    gg_out1 <- reactive({
      
      interp <-  input$interp
      
      if(interp == 'Interpolate'){
        planktonr::pr_plot_NRSEnvContour(selected(), Interpolation = TRUE, Fill_NA = FALSE)
      } else if (interp == 'Interpolate with gap filling'){
        planktonr::pr_plot_NRSEnvContour(selected(), Interpolation = TRUE, Fill_NA = TRUE, maxGap = 3)
      } else {
        planktonr::pr_plot_NRSEnvContour(selected(), Interpolation = FALSE, Fill_NA = FALSE)
      }
      
    }) %>% bindCache(input$station, input$parameter, input$date, input$interp)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {length(unique(selected()$StationName)) * 200})
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selected(), "Pico") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Pico") # Download figure
    
    # add a map in sidebar
    output$plotmap <- renderPlot({ 
      
      planktonr::pr_plot_NRSmap(selected())
      
    }, bg = "transparent") %>% bindCache(input$station)
    
    # add text information 
    output$PlotExp <- renderText({
      "A contour plot of picoplankton from the NRS around Australia, as a time series and a monthly climatology by depth. 
      If raw data is used the dots represent actual samples."
    }) 
    
  })
}
