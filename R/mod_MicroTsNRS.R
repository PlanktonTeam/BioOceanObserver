#' MicroTsNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MicroTsNRS_ui <- function(id){
  nsMicroTsNRS <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # conditionalPanel(
        #   condition="input.NRSmts == 1",  
        #   # Select whether to overlay smooth trend line 
        #   checkboxInput(inputId = nsMicroTsNRS("scaler1"), label = strong("Change the plot scale to log10"), value = FALSE)
        # ),
        conditionalPanel(
          condition="input.NRSmts == 2", 
          selectizeInput(inputId = nsMicroTsNRS("smoother"), label = strong("Overlay trend line"), choices = c("Smoother", "Linear", "None"), selected = "None")
        ),
        conditionalPanel(
          condition="input.NRSmts == 2 | input.NRSmts == 1", 
          checkboxInput(inputId = nsMicroTsNRS("scaler1"), label = strong("Change the plot scale to log10"), value = FALSE),
          sliderInput(nsMicroTsNRS("DatesSlide"), "Dates:", min = as.POSIXct('2009-01-01 00:00',
                                                                             format = "%Y-%m-%d %H:%M",
                                                                             tz = "Australia/Hobart"), max = Sys.time(), 
                      value = c(as.POSIXct('2009-01-01 00:00',
                                           format = "%Y-%m-%d %H:%M",
                                           tz = "Australia/Hobart"), Sys.time()-1), timeFormat="%Y-%m-%d"),
          selectInput(inputId = nsMicroTsNRS("ycol"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datNRSm$Parameters), style = "simple"), selected = "Bacterial_Richness"),
        ),
        conditionalPanel(
          condition="input.NRSmts == 3", 
          selectInput(inputId = nsMicroTsNRS("p1"), label = 'Select an x parameter', choices = planktonr::pr_relabel(unique(datNRSm$Parameters), style = "simple"), selected = "Eukaryote_Chlorophyll_Index"),
          selectInput(inputId = nsMicroTsNRS("p2"), label = 'Select a y parameter', 
                      choices = planktonr::pr_relabel(c("Prochlorococcus_CellsmL", "Synecochoccus_CellsmL", "Picoeukaryotes_CellsmL"), 
                                                      style = "simple"), selected = "Prochlorococcus_CellsmL")
        ),
        absolutePanel(
          plotOutput(nsMicroTsNRS("plotmap")),
          checkboxGroupInput(inputId = nsMicroTsNRS("Site"), label = "Select a station", choices = unique(sort(datNRSm$StationName)), selected = c("Maria Island", "Port Hacking", "Yongala")),
          downloadButton(nsMicroTsNRS("downloadData"), "Data"),
          downloadButton(nsMicroTsNRS("downloadPlot"), "Plot"),
          downloadButton(nsMicroTsNRS("downloadNote"), "Notebook")
        )
      ),
      mainPanel(
        tabsetPanel(id = "NRSmts",
                    type = "pills",
                    tabPanel("Trend Analysis", value=1, 
                             h6(textOutput(nsMicroTsNRS("PlotExp1"), container = span)),
                             plotOutput(nsMicroTsNRS("timeseries1"), height = 'auto') %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Climatologies", value=1,
                             h6(textOutput(nsMicroTsNRS("PlotExp2"), container = span)),
                             plotOutput(nsMicroTsNRS("timeseries2")) %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Trend analysis by depth", value=2,
                             h6(textOutput(nsMicroTsNRS("PlotExp3"), container = span)),  
                             plotOutput(nsMicroTsNRS("timeseries3"), height = 1000) %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Cell counts vs Indices", value=3,
                             h6(textOutput(nsMicroTsNRS("PlotExp4"), container = span)),  
                             plotOutput(nsMicroTsNRS("timeseries4")) %>% 
                               shinycssloaders::withSpinner(color="#0dc5c1")
                    )
        )
      )
    )
  )
}


#' MicroTSNRS Server Functions
#'
#' @noRd 
mod_MicroTsNRS_server <- function(id){
  moduleServer(id, function(input, output, session, NRSmts){
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
      selectedData <- datNRSm %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% input$ycol,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        mutate(name = as.factor(.data$Parameters),
               # SampleDepth_m = dplyr::if_else(stringr::str_detect("WC", SampleDepth_m),
               #                                "WC",
               #                                as.character(round(as.numeric(.data$SampleDepth_m)/5,0)*5))
               ) %>%
        droplevels()
      
    }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    shiny::exportTestValues(
      MicroTs = {ncol(selectedData())},
      MicroTsRows = {nrow(selectedData()) > 0},
      MicroTsYearisNumeric = {class(selectedData()$Year)},
      MicroTsMonthisNumeric = {class(selectedData()$Month_Local)},
      MicroTsDepthisNumeric = {class(selectedData()$SampleDepth_m)},
      MicroTsDateisDate = {class(selectedData()$SampleTime_Local)},
      MicroTsStationisFactor = {class(selectedData()$StationName)},
      MicroTsCodeisChr = {class(selectedData()$StationCode)},
      MicroTsParametersisChr = {class(selectedData()$Parameters)},
      MicroTsValuesisNumeric = {class(selectedData()$Values)}
    )
    
    # Sidebar Map
    output$plotmap <- renderPlot({ 
     planktonr::pr_plot_NRSmap(selectedData())
    }) %>% bindCache(input$Site)
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected microbial indices from the NRS around Australia, as a time series and a monthly climatology by station averaged across all depths."
    }) 
    output$PlotExp2 <- renderText({
      "A plot of selected indices from the NRS around Australia, as a time series, a monthly climatology and an annual mean averaged across all depths."
    }) 
    output$PlotExp3 <- renderText({
      "A plot of microbial indices from the NRS around Australia, as a time series and a monthly climatology by depth"
    }) 
    output$PlotExp4 <- renderText({
      "A plot of microbial indices against abundance measure from the NRS around Australia"
    }) 
    
    
    
    # Plot Trends -------------------------------------------------------------
    
      ts1 <- reactive({
      
      if (is.null(datNRSm$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      
      if(input$scaler1){
        trans <- 'log10'
      } else {
        trans <- 'identity'
      }
      
      p1 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Raw", Survey = "NRS", method = "lm", trans = trans)
      p2 <- planktonr::pr_plot_Trends(selectedData(), Trend = "Month", Survey = "NRS", method = "loess", trans = trans) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

      p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = 'collect')

    }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
    
    output$timeseries1 <- renderPlot({
      ts1()
    }, height = function() {length(unique(selectedData()$StationName)) * 200}) 
    
    # output$timeseries1_ui <- renderUI({
    #   plotOutput("timeseries1", height = function() {length(unique(selectedData()$StationName)) * 200})
    # })
    
    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    output$timeseries2 <- renderPlot({
      
      if (is.null(datNRSm$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      
      trans <- 'identity'
      if(input$scaler1){
        trans <- 'log10'
      }
      
      p1 <- planktonr::pr_plot_TimeSeries(selectedData(), Survey = "NRS", trans = trans) + 
        ggplot2::theme(legend.position = "none")
      
      p2 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Month", trans = trans) + 
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      
      p3 <- planktonr::pr_plot_Climatology(selectedData(), Survey = "NRS", Trend = "Year", trans = trans) + 
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      
      #titley <- names(planktonr::pr_relabel(unique(selectedData()$Parameters), style = "simple"))
      
      p1 + p2 + p3 + patchwork::plot_layout(widths = c(3,1,3), guides = "collect")
      
      
    }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
    
    # Plots by depths ---------------------------------------------------------
    
    output$timeseries3 <- renderPlot({
      
      trend <-  input$smoother
      
      if(input$scaler1){
        trans <- "log10"
      } else {
        trans <- "identity"
      }
      
      planktonr::pr_plot_Enviro(selectedData(), Trend = trend, trans = trans)
      
    }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$smoother, input$scaler1)
    
    # Plots by Parameters ---------------------------------------------------------
    
    selectedData1 <- reactive({
      req(input$Site)
      req(input$p1)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      validate(need(!is.na(input$p1), "Error: Please select a parameter."))
      
      selectedData1 <- datNRSm %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% c(input$p1, input$p2)) %>%
        tidyr::pivot_wider(c(.data$StationName, .data$SampleDepth_m, .data$SampleTime_Local), names_from = .data$Parameters, values_from = .data$Values, values_fn = mean)
      
    }) %>% bindCache(input$p1, input$p2, input$Site)
    
    output$timeseries4 <- renderPlot({
      x <- rlang::sym(colnames(selectedData1()[, 5]))
      y <- rlang::sym(colnames(selectedData1()[, 4]))
      
      titlex <- planktonr::pr_relabel(rlang::as_string(x), style = "ggplot")
      titley <- planktonr::pr_relabel(rlang::as_string(y), style = "ggplot")
      
      ggplot2::ggplot(data = selectedData1()) +
        ggplot2::geom_point(ggplot2::aes(!!x, !!y, colour = .data$StationName)) +
        ggplot2::xlab(titlex) + ggplot2::ylab(titley) 
      
    }) %>% bindCache(input$p1, input$p2, input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    # Downloads ---------------------------------------------------------------
    
    # Table of selected dataset ----
    output$table <- renderTable({
      # datasetInput()
    })
    
    # Download -------------------------------------------------------
    #Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(input$ycol),"_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".csv")
      },
      content = function(file) {
        vroom::vroom_write(selectedData(), file, delim = ",")
      })
    
    
    # Download figure
    # output$downloadPlot <- downloadHandler(
    #   filename = function() {paste(input$ycol, '.png', sep='') },
    #   content = function(file) {
    #     ggsave(file, plot = plotInput(), device = "png")
    #   })
  })
}
