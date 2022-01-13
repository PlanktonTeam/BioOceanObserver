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
                    tabPanel("EOV analysis by NRS", 
                             h6(textOutput(nsPol("PlotExp1"), container = span)),
                             plotOutput(nsPol("timeseries1")) %>% shinycssloaders::withSpinner(color="#0dc5c1")
                    ),
                    tabPanel("CPR - EOVS"
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
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Sidebar ----------------------------------------------------------
    selectedDataZ <- reactive({
      req(input$Site)
      validate(need(!is.na(input$Site), "Error: Please select a station."))

      selectedDataZ <- datNRSz %>% 
        dplyr::filter(.data$StationName %in% input$Site,
                      parameters == "Biomass_mgm3") %>%
        droplevels()
    }) %>% bindCache(input$Site)
      
      selectedDataP <- reactive({
        req(input$Site)
        validate(need(!is.na(input$Site), "Error: Please select a station."))
        
        selectedDataP <- datNRSp %>% 
          dplyr::filter(.data$StationName %in% input$Site,
                        parameters == "PhytoBiomassCarbon_pgL") %>%
          droplevels()
      }) %>% bindCache(input$Site)

        # Sidebar Map
    output$plotmap <- renderPlotly({ 
      pmap <- planktonr::pr_plot_NRSmap(selectedDataZ())
    }) %>% bindCache(selectedDataZ())
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "Time series summary of each NRS station using EOVs."
    }) 
    
    # Plot Trends -------------------------------------------------------------
    output$timeseries1 <- renderPlot({

      p1 <- planktonr::pr_plot_trends(selectedDataZ(), trend = "Raw", survey = "NRS", method = "lm", pal = "phase", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "dark blue") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "dark blue", fill = "light blue")
      p2 <- planktonr::pr_plot_trends(selectedDataZ(), trend = "Month", survey = "NRS", method = "loess", pal = "phase", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "dark blue") +
        ggplot2::geom_smooth(formula = y ~ x, color = "dark blue", fill = "light blue")
      
      p3 <- planktonr::pr_plot_trends(selectedDataP(), trend = "Raw", survey = "NRS", method = "lm", pal = "algae", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "dark green") +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = "dark green", fill = "light green")
      p4 <- planktonr::pr_plot_trends(selectedDataP(), trend = "Month", survey = "NRS", method = "loess", pal = "algae", y_trans = "log10", output = "ggplot") +
        ggplot2::geom_point(color = "dark green") +
        ggplot2::geom_smooth(color = "dark green", fill = "light green")
      
      p1 + p2 + p3 + p4
      
    }) %>% bindCache(selectedDataZ(), selectedDataP())

})}
