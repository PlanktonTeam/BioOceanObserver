#' MoorBGC UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MoorBGC_ui <- function(id){
  nsMoorBGC <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        plotOutput(nsMoorBGC("plotmap")),
        # station selector
        checkboxGroupInput(inputId = nsMoorBGC('station'), label = "Select a station", choices = unique(sort(MooringTS$StationName)), 
                           selected = 'Port Hacking')),
      mainPanel(
        h6(textOutput(nsMoorBGC("PlotExp"), container = span)),
        plotOutput(nsMoorBGC("timeseries1")) %>% shinycssloaders::withSpinner(color="#0dc5c1"),
        div(style="display:inline-block; float:right; width:60%",
            fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
            fButtons(id, button_id = "downloadData1", label = "Data TS", Type = "Download"),
            fButtons(id, button_id = "downloadData2", label = "Data Clim", Type = "Download"),
            fButtons(id, button_id = "downloadCode1", label = "R Code Example", Type = "Action"))
      )
    )
  )
}

#' MoorBGC Server Functions
#'
#' @noRd 
mod_MoorBGC_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    pr_get_MoorClimPlotData <- function(df, Station, noYear){
        df <- data.frame(SampleDate = seq.Date(to = lubridate::ceiling_date(Sys.Date(), "year"),
                                               from = lubridate::ceiling_date(Sys.Date() - lubridate::years(noYear), "year"),
                                               by = "day")) %>% 
              dplyr::mutate(TIME = lubridate::yday(.data$SampleDate), 
                            year = lubridate::year(.data$SampleDate)) %>% 
              dplyr::inner_join(df %>% dplyr::filter(.data$StationName %in% Station), by = 'TIME') %>%
              unique()
    }
    
    pr_plot_MoorClim <- function(df){
      # years <- paste0(unique(df$year), "-01")
      # noYears <- length(years) - 1
      # df$seq <- c(rep(1:(365 * noYears), each = ((max(df$DEPTH) + 1))), rep((365 * noYears) + 1, (max(df$DEPTH) + 1)))
      # labbreak <- c(1, (1 + (max(df$TIME)) * 1),
      #               (1 + (max(df$TIME)) * 2),
      #               (1 + (max(df$TIME)) * 3),
      #               (1 + (max(df$TIME)) * 4),
      #               (1 +(max(df$TIME)) * 5))
      legtit <- planktonr::pr_relabel("Temperature_degC", style = 'ggplot')
    
    climtsplot <- ggplot2::ggplot(df) +
      ggplot2::geom_raster(ggplot2::aes(x = .data$SampleDate, y = .data$DEPTH, fill = .data$CLIM), interpolate = TRUE) +
      ggplot2::scale_fill_viridis_c(option = 'plasma', name = legtit) +
      ggplot2::scale_color_viridis_c(option = 'plasma', name = legtit) +
      ggplot2::facet_wrap(~ .data$StationName, scales = 'free', ncol = 1) +
      ggplot2::scale_y_reverse(expand=c(0,0)) +
      #ggplot2::scale_x_continuous(breaks = labbreak, labels = years, expand=c(0,0)) +
      ggplot2::scale_x_date(breaks = '1 year', expand=c(0,0)) +
      ggplot2::labs(x = "Years", y = "Depth (m)") +
      ggplot2::theme_bw() + 
      ggplot2::theme(legend.position = 'bottom',
                     strip.background = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(), 
                     panel.grid.minor = ggplot2::element_blank())

    climtsplot
    }
    
    pr_get_MoorTSPlotData <- function(df, Station, noYear){
      df <- data.frame(SampleDate = seq.Date(to = lubridate::ceiling_date(Sys.Date(), "year"),
                                                  from = lubridate::ceiling_date(Sys.Date() - lubridate::years(noYear), "year"),
                                                  by = "day")) %>% 
        dplyr::mutate(DOY = lubridate::yday(.data$SampleDate)) %>% 
        dplyr::inner_join(MooringTS %>% dplyr::filter(.data$StationName %in% Station), by = "DOY") %>%
        dplyr::select(-"DOY") %>%
        tidyr::pivot_wider(c(.data$SampleDate, .data$StationName, .data$StationCode), names_from = "Names", values_from = "CLIM") 
    }
      
    pr_plot_MoorTS <- function(df){
      
      plot <- ggplot2::ggplot(df, ggplot2::aes(x = .data$SampleDate)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$Surface, colour = 'Surface')) +
        ggplot2::geom_line(ggplot2::aes(y = .data$MLD, colour = 'MLD')) +
        ggplot2::geom_line(ggplot2::aes(y = .data$Bottom, colour = 'Bottom')) +
        ggplot2::facet_wrap(~ .data$StationName, scales = "free", ncol = 1) +
        ggplot2::scale_color_manual(name = 'Depth', values = c('dark blue', 'blue', 'light blue')) +
        ggplot2::scale_x_date(breaks = '1 year') +
        ggplot2::labs(y = planktonr::pr_relabel("Temperature_degC", style = 'ggplot'), x = 'Years') +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = 'bottom')
      plot
      
    }
    
    selectedClim <- reactive({
      req(input$station)
      validate(need(!is.na(input$station), "Error: Please select a station."))
      
      selectedClim <- pr_get_MoorClimPlotData(MooringClim, input$station, 5)
      
    }) %>% bindCache(input$station)
    
    selectedTS <- reactive({
      req(input$station)
      validate(need(!is.na(input$station), "Error: Please select a station."))
      
      selectedTS <- pr_get_MoorTSPlotData(MooringTS, input$station, 5) 
      
    }) %>% bindCache(input$station)
    
    # shiny::exportTestValues(
    #   MoorTs = {ncol(selectedTS())},
    #   MoorTsRows = {nrow(selectedTS()) > 0},
    #   MoorTsDateisDate = {class(selectedTS()$SampleDate)},
    #   MoorTsStationisFactor = {class(selectedTS()$StationName)},
    #   MoorTsCodeisChr = {class(selectedTS()$StationCode)},
    #   MoorClim = {ncol(selectedClim())},
    #   MoorClimRows = {nrow(selectedClim()) > 0},
    #   MoorClimDateisDate = {class(selectedClim()$SampleDate)},
    #   MoorClimStationisFactor = {class(selectedClim()$StationName)},
    #   MoorClimCodeisChr = {class(selectedClim()$StationCode)},
    #   MoorClimTime = {class(selectedClim()$TIME)},
    #   MoorClimClim = {class(selectedClim()$CLIM)},
    #   MoorClimDepth = {class(selectedClim()$DEPTH)},
    # )
    
      # add a map in sidebar
    output$plotmap <- renderPlot({ 
      
      planktonr::pr_plot_NRSmap(selectedClim())
      
    }, bg = "transparent") %>% bindCache(input$station)
    
    # add climate plot
    gg_out1 <- reactive({ 
      
      p1 <- pr_plot_MoorClim(selectedClim())
      p2 <- pr_plot_MoorTS(selectedTS())
      
      p1 + p2
      
    }) %>% bindCache(input$station)
    
    output$timeseries1 <- renderPlot({
      gg_out1()
    }, height = function() {length(unique(selectedTS()$StationName)) * 200}) 
    
    
    # add text information 
    output$PlotExp <- renderText({
      "Plot showing 5 years of climatology from each stations and the timeseries of temperatures at the surface, the mean mixed layer depth and the bottom"
    }) 
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedTS(), "MooringTS") # Download csv of data
    output$downloadData2 <- fDownloadButtonServer(input, selectedClim(), "MooringCLim") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Mooring") # Download figure
    
  })
}
