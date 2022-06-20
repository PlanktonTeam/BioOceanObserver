#' ZooTsNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZooTsNRS_ui <- function(id){
  nsZooTsNRS <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition="input.NRSzts == 1",  
          # Select whether to overlay smooth trend line 
          checkboxInput(inputId = nsZooTsNRS("scaler1"), label = strong("Change the plot scale to log10"), value = FALSE),
          selectInput(inputId = nsZooTsNRS("ycol"), label = 'Select a parameter', choices = planktonr::pr_relabel(unique(datNRSz$parameters), style = "simple"), selected = "Biomass_mgm3")
        ),
        conditionalPanel(
          condition="input.NRSzts == 2", 
          # Select whether to overlay smooth trend line
          checkboxInput(inputId = nsZooTsNRS("scaler3"), label = strong("Change the plot scale to percent"), value = FALSE)
        ),
        absolutePanel(
          plotOutput(nsZooTsNRS("plotmap")),
          checkboxGroupInput(inputId = nsZooTsNRS("Site"), label = "Select a station", choices = unique(sort(datNRSz$StationName)), selected = c("Maria Island", "Port Hacking", "Yongala")),
          sliderInput(nsZooTsNRS("DatesSlide"), "Dates:", min = lubridate::ymd(20090101), max = Sys.Date(), 
                      value = c(lubridate::ymd(20090101), Sys.Date()-1), timeFormat="%Y-%m-%d"),
          downloadButton(nsZooTsNRS("downloadData"), "Data"),
          downloadButton(nsZooTsNRS("downloadPlot"), "Plot"),
          downloadButton(nsZooTsNRS("downloadNote"), "Notebook")
        )
      ),
      mainPanel(
        tabsetPanel(id = "NRSzts",
                    tabPanel("Trend Analysis", value=1,
                             h6(textOutput(nsZooTsNRS("PlotExp1"), container = span)),
                             plotOutput(nsZooTsNRS("timeseries1"), height = 'auto') %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Climatologies", value=1,
                             h6(textOutput(nsZooTsNRS("PlotExp2"), container = span)),  
                             textOutput(nsZooTsNRS("selected_var")),
                             plotOutput(nsZooTsNRS("timeseries2"), height = 800) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("Functional groups", value=2,
                             h6(textOutput(nsZooTsNRS("PlotExp3"), container = span)),  
                             plotOutput(nsZooTsNRS("timeseries3"), height = 'auto') %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    )
        )
      )
    )
  )
}

#' ZooTsNRS Server Functions
#'
#' @noRd 
mod_ZooTsNRS_server <- function(id){
  moduleServer(id, function(input, output, session, NRSzts){
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
      req(input$Site)
      req(input$ycol)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      validate(need(!is.na(input$ycol), "Error: Please select a parameter."))
      
      selectedData <- datNRSz %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$parameters %in% input$ycol,
                      dplyr::between(.data$SampleDate_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
      
    }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    shiny::exportTestValues(
      ZtsNRS = {ncol(selectedData())},
      ZtsNRSRows = {nrow(selectedData()) > 0},
      ZtsNRSYearisNumeric = {class(selectedData()$Year)},
      ZtsNRSMonthisNumeric = {class(selectedData()$Month)},
      ZtsNRSDateisDate = {class(selectedData()$SampleDate_Local)},
      ZtsNRSStationisFactor = {class(selectedData()$StationName)},
      ZtsNRSCodeisChr = {class(selectedData()$StationCode)},
      ZtsNRSparametersisChr = {class(selectedData()$parameters)},
      ZtsNRSValuesisNumeric = {class(selectedData()$Values)}
    )
    
    # Sidebar Map
    output$plotmap <- renderPlot({ 
      ##planktonr::pr_plot_NRSmap(selectedData()) ## restore this after 16-06-22
      
      MapOz <- rnaturalearth::ne_countries(scale = "medium", country = "Australia",
                                           returnclass = "sf")
      
      meta_sf <- planktonr::pr_get_NRSTrips("Z") %>%
        dplyr::select(StationName, StationCode, Longitude, Latitude) %>%
        dplyr::distinct() %>%
        dplyr::rename(Code = StationCode, Station = StationName) %>%
        dplyr::filter(Station != 'Port Hacking 4') %>%
        sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
      
      meta2_sf <- meta_sf %>%
        sf::st_as_sf() %>% # This seems to strip away some of the tibble stuff that makes the filter not work...
        dplyr::filter(.data$Code %in% selectedData()$StationCode)
      
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = MapOz, size = 0.05, fill = "grey80") +
        ggplot2::geom_sf(data = meta_sf, colour = "blue", size = 5) +
        ggplot2::geom_sf(data = meta2_sf, colour = "red", size = 5) +
        ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(112, 155)) +
        ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(-45, -9)) +
        ggplot2::theme_void() +
        ggplot2::theme(axis.title = ggplot2::element_blank(),
                       axis.line = ggplot2::element_blank())
      
    }) %>% bindCache(input$Site)
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "A plot of selected zooplankton parameters from the NRS around Australia, as a time series and a monthly climatology by station."
    }) 
    
    output$PlotExp2 <- renderText({
      "A plot of selected indices from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
    }) 
    
    output$PlotExp3 <- renderText({
      "A plot of zooplankton functional groups from the NRS around Australia, as a time series and a monthly climatology by station"
    }) 
    
    
    # Plot Trends -------------------------------------------------------------
    ts1 <- reactive({
      
      if (is.null(datNRSz$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      
      if(input$scaler1){
        Scale <- 'log10'
      } else {
        Scale <- 'identity'
      }
      
      p1 <- planktonr::pr_plot_trends(selectedData(), trend = "Raw", survey = "NRS", method = "lm", pal = "matter", y_trans = Scale, output = "ggplot")
      p2 <- planktonr::pr_plot_trends(selectedData(), trend = "Month", survey = "NRS", method = "loess", pal = "matter", y_trans = Scale, output = "ggplot") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      p1 + p2 + patchwork::plot_layout(widths = c(3, 1), guides = 'collect')
      
    }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
    
    output$timeseries1 <- renderPlot({
      ts1()
    }, height = function() {length(unique(selectedData()$StationName)) * 200}) 
    
    # Climatologies -----------------------------------------------------------
    
    # Plot abundance spectra by species
    output$timeseries2 <- renderPlot({
      
      if (is.null(datNRSz$StationCode))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      
      Scale <- 'identity'
      if(input$scaler1){
        Scale <- 'log10'
      } 
      
      p1 <- planktonr::pr_plot_timeseries(selectedData(), 'NRS', 'matter', Scale) + ggplot2::theme(legend.position = 'none',
                                                                                                   axis.title.y = ggplot2::element_blank())
      
      p2 <- planktonr::pr_plot_climate(selectedData(), 'NRS', Month, 'matter', Scale) + ggplot2::theme(legend.position = 'bottom',
                                                                                            axis.title.y = ggplot2::element_blank())

      p3 <- planktonr::pr_plot_climate(selectedData(), 'NRS', Year, 'matter', Scale) + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                                                                                      legend.position = 'bottom')
      
      titleplot <- names(planktonr::pr_relabel(input$ycol, style = 'simple'))
      
      p1 / (p2 | p3) + patchwork::plot_layout(guides = 'collect') + patchwork::plot_annotation(title = titleplot)
      
      
    }) %>% bindCache(input$ycol, input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler1)
    

    # Functional groups -------------------------------------------------------
    
    selectedDataFG <- reactive({
      req(input$Site)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      
      selectedDataFG <- NRSfgz %>% 
        dplyr::mutate(SampleTime_Local = as.Date(SampleTime_Local)) %>%
        dplyr::filter(.data$StationName %in% input$Site,
                      dplyr::between(.data$SampleTime_Local, input$DatesSlide[1], input$DatesSlide[2])) %>%
        droplevels()
    
      }) %>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2])
    
    ts3 <- reactive({
      
      if(input$scaler3){
        scale <- 'Percent'
      } else {
        scale <- 'Actual'
      }
      
      p1 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale)
      p2 <- planktonr::pr_plot_tsfg(selectedDataFG(), Scale = scale, "Month") + 
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       legend.position = 'none')
      p1 + p2 + patchwork::plot_layout(widths = c(3,1))
        
    }) %>% bindCache(input$Site, input$DatesSlide[1], input$DatesSlide[2], input$scaler3)

    output$timeseries3 <- renderPlot({
      ts3()
    }, height = function() {length(unique(selectedDataFG()$StationName)) * 200}) 
  
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


