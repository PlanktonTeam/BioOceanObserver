#' Policy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom stats runif
mod_Policy_ui <- function(id){
  nsPol <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
          plotlyOutput(nsPol("plotmap")),
          radioButtons(inputId = nsPol("Site"), label = "Select a station", choices = unique(sort(datNRSz$StationName)), selected = "Port Hacking"),
          downloadButton(nsPol("downloadData"), "Data"),
          downloadButton(nsPol("downloadPlot"), "Plot"),
          downloadButton(nsPol("downloadNote"), "Notebook")
          ),
      mainPanel(
        tabsetPanel(id = "PolNRS",
                    tabPanel("EOV Biomass by NRS", 
                             h6(textOutput(nsPol("PlotExp1"), container = span)),
                             plotOutput(nsPol("timeseries1"), height = 800) %>% shinycssloaders::withSpinner(color="#0dc5c1"), 
                             h6(textOutput(nsPol("PlotExp3"), container = span))
                    ),
                    tabPanel("EOV Diversity by NRS", 
                             h6(textOutput(nsPol("PlotExp2"), container = span)),
                             plotOutput(nsPol("timeseries2"), height = 800) %>% shinycssloaders::withSpinner(color="#0dc5c1"), 
                             h6(textOutput(nsPol("PlotExp4"), container = span))
                    )
        )
      )
    )
  )
}

#' Policy Server Functions
#'
#' @noRd 
mod_Policy_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Sidebar ----------------------------------------------------------
    selectedData <- reactive({
      req(input$Site)
      validate(need(!is.na(input$Site), "Error: Please select a station."))

      selectedData <- Pol %>% 
        dplyr::filter(.data$StationName %in% input$Site) %>%
        droplevels()
    }) %>% bindCache(input$Site)
      
      # Sidebar Map
    output$plotmap <- renderPlotly({ 
      pmap <- planktonr::pr_plot_NRSmap(selectedData())
    }) %>% bindCache(selectedData())
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "Biomass is an Essential Ocean Variables (EOVs) for plankton"
    }) 
    output$PlotExp2 <- renderText({
      "Diversity is an Essential Ocean Variables (EOVs) for plankton"
    }) 
    output$PlotExp3 <- renderText({
      "Then we can have something saying which of the trends are significant"
    }) 
    output$PlotExp4 <- renderText({
      "Then we can have something saying which of the trends are significant"
    }) 
    
    # Plot Trends -------------------------------------------------------------
    output$timeseries1 <- renderPlot({

      p1 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Biomass_mgm3"), 
                                      trend = "Raw", survey = "NRS", method = "lm", pal = "phase", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "dark blue") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "dark blue", fill = "light blue") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p1a <- ggplot2::ggplot(selectedData() %>% dplyr::filter(parameters == "Biomass_mgm3") %>%
                               dplyr::select(-Values) %>%
                               dplyr::rename(Values = anomaly), ggplot2::aes(SampleDateLocal, Values)) +
        ggplot2::geom_col(color = "dark blue") +
        ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
        ggplot2::labs(y = "Anomaly") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p2 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Biomass_mgm3"), 
                                      trend = "Month", survey = "NRS", method = "loess", pal = "phase", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "dark blue") +
        ggplot2::geom_smooth(formula = y ~ x, color = "dark blue", fill = "light blue") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
      
      p3 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "PhytoBiomassCarbon_pgL"), 
                                      trend = "Raw", survey = "NRS", method = "lm", pal = "algae", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "dark green") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "dark green", fill = "light green") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p3a <- ggplot2::ggplot(selectedData() %>% dplyr::filter(parameters == "PhytoBiomassCarbon_pgL") %>%
                               dplyr::select(-Values) %>%
                               dplyr::rename(Values = anomaly), ggplot2::aes(SampleDateLocal, Values)) +
        ggplot2::geom_col(color = "dark green") +
        ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
        ggplot2::labs(y = "Anomaly") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p4 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "PhytoBiomassCarbon_pgL"), 
                                      trend = "Month", survey = "NRS", method = "loess", pal = "algae", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "dark green") + 
        ggplot2::geom_smooth(color = "dark green", fill = "light green") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
      
      p5 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Temperature_degC"), 
                                      trend = "Raw", survey = "NRS", method = "lm", pal = "algae", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "violetred4") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "violetred4", fill = "violet") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p5a <- ggplot2::ggplot(selectedData() %>% dplyr::filter(parameters == "Temperature_degC") %>%
                               dplyr::select(-Values) %>%
                               dplyr::rename(Values = anomaly), ggplot2::aes(SampleDateLocal, Values)) +
        ggplot2::geom_col(color = "violetred4") +
        ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
        ggplot2::labs(y = "Anomaly") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p6 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Temperature_degC"), 
                                      trend = "Month", survey = "NRS", method = "loess", pal = "algae", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "violetred4") +
        ggplot2::geom_smooth(color = "violetred4", fill = "violet") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
      
      p7 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Chla_mgm3"), 
                                      trend = "Raw", survey = "NRS", method = "lm", pal = "algae", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "darkseagreen") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "darkseagreen", fill = "darkseagreen1") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p7a <- ggplot2::ggplot(selectedData() %>% dplyr::filter(parameters == "Chla_mgm3") %>%
                               dplyr::select(-Values) %>%
                               dplyr::rename(Values = anomaly), ggplot2::aes(SampleDateLocal, Values)) +
        ggplot2::geom_col(color = "darkseagreen") +
        ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
        ggplot2::labs(y = "Anomaly") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p8 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Chla_mgm3"), 
                                      trend = "Month", survey = "NRS", method = "loess", pal = "algae", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "darkseagreen") +
        ggplot2::geom_smooth(color = "darkseagreen", fill = "darkseagreen1") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
      
      p9 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Salinity_psu"), 
                                      trend = "Raw", survey = "NRS", method = "lm", pal = "algae", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "darkgoldenrod2") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "darkgoldenrod2", fill = "gold")  +
        ggplot2::theme()
      p9a <- ggplot2::ggplot(selectedData() %>% dplyr::filter(parameters == "Salinity_psu") %>%
                               dplyr::select(-Values) %>%
                               dplyr::rename(Values = anomaly), ggplot2::aes(SampleDateLocal, Values)) +
        ggplot2::geom_col(color = "darkgoldenrod2") +
        ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
        ggplot2::labs(y = "Anomaly", x = "Year") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p10 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Salinity_psu"), 
                                       trend = "Month", survey = "NRS", method = "loess", pal = "algae", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "darkgoldenrod2") +
        ggplot2::geom_smooth(color = "darkgoldenrod2", fill = "gold") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
      
      
     p1 + p1a + p2 + p3 + p3a + p4 + p5 + p5a + p6 + p7 + p7a + p8 + p9 + p9a + p10 +
        patchwork::plot_layout(widths = c(3, 3, 1))
      
    }) %>% bindCache(selectedData())
    
    output$timeseries2 <- renderPlot({
      
      p1 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "ShannonCopepodDiversity"), 
                                      trend = "Raw", survey = "NRS", method = "lm", pal = "phase", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "dark blue") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "dark blue", fill = "light blue") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p2 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "ShannonCopepodDiversity"), 
                                      trend = "Month", survey = "NRS", method = "loess", pal = "phase", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "dark blue") +
        ggplot2::geom_smooth(formula = y ~ x, color = "dark blue", fill = "light blue") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
      
      p3 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "ShannonPhytoDiversity"), 
                                      trend = "Raw", survey = "NRS", method = "lm", pal = "algae", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "dark green") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "dark green", fill = "light green") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p4 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "ShannonPhytoDiversity"), 
                                      trend = "Month", survey = "NRS", method = "loess", pal = "algae", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "dark green") + 
        ggplot2::geom_smooth(color = "dark green", fill = "light green") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
      
      p5 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Temperature_degC"), 
                                      trend = "Raw", survey = "NRS", method = "lm", pal = "algae", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "violetred4") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "violetred4", fill = "violet") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p6 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Temperature_degC"), 
                                      trend = "Month", survey = "NRS", method = "loess", pal = "algae", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "violetred4") +
        ggplot2::geom_smooth(color = "violetred4", fill = "violet") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
      
      p7 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Chla_mgm3"), 
                                      trend = "Raw", survey = "NRS", method = "lm", pal = "algae", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "darkseagreen") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "darkseagreen", fill = "darkseagreen1") +
        ggplot2::theme(axis.title.x = ggplot2::element_blank())
      p8 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Chla_mgm3"), 
                                      trend = "Month", survey = "NRS", method = "loess", pal = "algae", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "darkseagreen") +
        ggplot2::geom_smooth(color = "darkseagreen", fill = "darkseagreen1") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
      
      p9 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Salinity_psu"), 
                                      trend = "Raw", survey = "NRS", method = "lm", pal = "algae", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "darkgoldenrod2") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "darkgoldenrod2", fill = "gold")  +
        ggplot2::theme()
      p10 <- planktonr::pr_plot_trends(selectedData() %>% dplyr::filter(parameters == "Salinity_psu"), 
                                       trend = "Month", survey = "NRS", method = "loess", pal = "algae", y_trans = "identity", output = "ggplot") +
        ggplot2::geom_point(color = "darkgoldenrod2") +
        ggplot2::geom_smooth(color = "darkgoldenrod2", fill = "gold") +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       strip.text = ggplot2::element_blank())
      
      
      p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 +
        patchwork::plot_layout(widths = c(3, 1))
      
    }) %>% bindCache(selectedData())
    
})}
