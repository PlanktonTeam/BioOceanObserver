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
        shinydashboard::menuSubItem(text = "Find out more about the NRS stations here", href = "https://github.com/PlanktonTeam/IMOS_BioOceanObserver/wiki/National-Reference-Stations"),
        shinydashboard::menuSubItem(text = "Find out more about EOVs here", href = "https://www.goosocean.org/index.php?option=com_content&view=article&layout=edit&id=283&Itemid=441"),
        plotlyOutput(nsPol("plotmap")),
        radioButtons(inputId = nsPol("Site"), label = "Select a station", choices = unique(sort(Pol$StationName)), selected = "Port Hacking"),
        downloadButton(nsPol("downloadData"), "Data"),
        downloadButton(nsPol("downloadPlot"), "Plot"),
        downloadButton(nsPol("downloadNote"), "Notebook")
          ),
      mainPanel(
        tabsetPanel(id = "PolNRS",
                    tabPanel("EOV Biomass by NRS", 
                             h6(textOutput(nsPol("PlotExp1"), container = span)),
                             plotOutput(nsPol("timeseries1"), height = 800) %>% shinycssloaders::withSpinner(color="#0dc5c1"), 
                             h6(verbatimTextOutput(nsPol("PlotExp3"))),
                             h6(textOutput(nsPol("plotExp5")))
                    ),
                    tabPanel("EOV Diversity by NRS", 
                             h6(textOutput(nsPol("PlotExp2"), container = span)),
                             plotOutput(nsPol("timeseries2"), height = 800) %>% shinycssloaders::withSpinner(color="#0dc5c1"), 
                             h6(verbatimTextOutput(nsPol("PlotExp4")))
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
    
    params <- Pol %>% dplyr::select(parameters) %>% unique()
    params <- params$parameters
    
    Harm <- function (theta, k = 4) {
      X <- matrix(0, length(theta), 2 * k)
      nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
      dimnames(X) <- list(names(theta), nam)
      m <- 0
      for (j in 1:k) {
        X[, (m <- m + 1)] <- cos(j * theta)
        X[, (m <- m + 1)] <- sin(j * theta)
      }
      X
    }
    
    coeffs <- function(params){
      lmdat <-  selectedData() %>% dplyr::filter(parameters == params) %>% tidyr::drop_na()
      m <- lm(Values ~ Year + Harm(Month, k = 1), data = lmdat) 
      lmdat <- data.frame(lmdat %>% dplyr::bind_cols(fv = m$fitted.values))
      ms <- summary(m)
      slope <- ifelse(ms$coefficients[2,1] < 0, 'decreasing', 'increasing')
      p <-  ifelse(ms$coefficients[2,4] < 0.005, 'significantly', 'but not significantly')
      df <-  data.frame(slope = slope, p = p, parameters = params)
      df <- lmdat %>% dplyr::inner_join(df, by = 'parameters')
    }
    
    pr_plot_EOV <- function(df, EOV = "Biomass_mgm3", trans = 'identity', pal = 'matter') {
      
      titley <- planktonr::pr_relabel(EOV, style = "ggplot")
      df <-  df %>% dplyr::filter(parameters == EOV)
      
      pals <- planktonr::pr_get_PlotCols(pal = pal, n = 20)
      col <- pals[15]
      colin <- pals[5]
      lims <- as.POSIXct(strptime(c("2010-01-01","2020-31-31"), format = "%Y-%m-%d"))
      
      p1 <- ggplot2::ggplot(df) + 
        ggplot2::geom_point(ggplot2::aes(x = SampleDateLocal, y = Values), colour = col) +
        ggplot2::geom_smooth(ggplot2::aes(x = SampleDateLocal, y = fv), method = "lm", formula = 'y ~ x', colour = col, fill = colin) +
        ggplot2::ylab(rlang::enexpr(titley)) +
        ggplot2::scale_y_continuous(trans = trans) +
        ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", limits = lims) +
        ggplot2::xlab("Year") +
        ggplot2::theme(legend.position = "none")
      
      p2 <- ggplot2::ggplot(df, ggplot2::aes(SampleDateLocal, anomaly)) +
        ggplot2::geom_col(fill = colin, colour = col) +
        ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", limits = lims) +
        ggplot2::xlab("Year") +
        ggplot2::labs(y = "Anomaly") 
      
      p3 <- ggplot2::ggplot(df) +
        ggplot2::geom_point(ggplot2::aes(x = Month, y = Values), colour = col) +
        ggplot2::geom_smooth(ggplot2::aes(x = Month, y = fv), method = "loess", formula = 'y ~ x', colour = col, fill = colin) +
        ggplot2::scale_y_continuous(trans = trans) +
        ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
        ggplot2::xlab("Month") +
        ggplot2::theme(legend.position = "none",
                       axis.title.y = ggplot2::element_blank())
      
      #plot <- gridExtra::grid.arrange(p1, p2, p3, nrow = 1, widths = c(3,3,1))
      p1 + p2 + p3 + patchwork::plot_layout(widths = c(3,3,2))
    }
    
    outputs <- reactive({
      output <- purrr::map_dfr(params, coeffs)
    }) %>% bindCache(input$Site)
    
    info <- reactive({
      info <- outputs() %>% dplyr::select(slope, p, parameters) %>% unique()
    }) %>% bindCache(input$Site)
    
    stationData <- reactive({
      stationData <- planktonr::pr_get_StationName() %>%
        dplyr::filter(StationName == input$Site)
    }) %>% bindCache(input$Site)
    
    # Sidebar Map
    output$plotmap <- renderPlotly({ 
      pmap <- planktonr::pr_plot_NRSmap(selectedData())
    }) %>% bindCache(selectedData())
    
    # Add text information 
    output$PlotExp1 <- renderText({
      "Biomass is an Essential Ocean Variables (EOVs) for plankton. 
      These are the important variables that scientists have identified to monitor our oceans.
      They are chosen based on impact of the measurement and the feasiblity to take consistent measurements.
      They are commonly measured by observing systems and frequently used in policy making and input into reporting such as State of Environment"
    }) 
    output$PlotExp2 <- renderText({
      "Diversity is an Essential Ocean Variables (EOVs) for plankton, 
      These are the important variables that scientists have identified to monitor our oceans.
      They are chosen based on impact of the measurement and the feasiblity to take consistent measurements.
      They are commonly measured by observing systems and frequently used in policy making and input into reporting such as State of Environment"
    }) 
    output$PlotExp3 <- renderText({
      paste(" Zooplankton biomass at", input$Site, "is", info()[1,1], info()[1,2],  "\n",
            "Phytoplankton carbon biomass at", input$Site, "is", info()[2,1], info()[2,2],  "\n",
            "Surface temperature at", input$Site, "is", info()[3,1], info()[3,2],  "\n",
            "Surface chlorophyll at", input$Site, "is", info()[7,1], info()[7,2],  "\n",
            "Surface salinity at", input$Site, "is", info()[6,1], info()[6,2])
    }) 
    output$PlotExp4 <- renderText({
      paste(" Copepod diversity at", input$Site, "is", info()[4,1], info()[4,2],  "\n",
            "Phytoplankton diveristy at", input$Site, "is", info()[5,1], info()[5,2],  "\n",
            "Surface temperature at", input$Site, "is", info()[3,1], info()[3,2],  "\n",
            "Surface chlorophyll at", input$Site, "is", info()[7,1], info()[7,2],  "\n",
            "Surface salinity at", input$Site, "is", info()[6,1], info()[6,2])
    }) 
    output$PlotExp5 <- renderText({
      "Why isn't this working"
    })
    
    # Plot Trends -------------------------------------------------------------
    layout <- c(
      patchwork::area(1,1,1,1),
      patchwork::area(1,2,1,2),
      patchwork::area(1,3,1,3),
      patchwork::area(2,1,2,3),
      patchwork::area(3,1,3,3),
      patchwork::area(4,1,4,3),
      patchwork::area(5,1,5,3)
    )
    
    p3 <-pr_plot_EOV(outputs(), "Temperature_degC", "identity", pal = "solar") 
    p4 <-pr_plot_EOV(outputs(), "Chla_mgm3", "log10", pal = "haline") 
    p5 <-pr_plot_EOV(outputs(), "Salinity_psu", "identity", pal = "dense")

        output$timeseries1 <- renderPlot({

      p1 <-pr_plot_EOV(outputs(), "Biomass_mgm3", "log10", pal = "matter") 
      p2 <-pr_plot_EOV(outputs(), "PhytoBiomassCarbon_pgL", "log10", pal = "algae") 
      p3 <-pr_plot_EOV(outputs(), "Temperature_degC", "identity", pal = "solar") 
      p4 <-pr_plot_EOV(outputs(), "Chla_mgm3", "log10", pal = "haline") 
      p5 <-pr_plot_EOV(outputs(), "Salinity_psu", "identity", pal = "dense")
      
      #gridExtra::grid.arrange(p1, p2, p3, p4, p5, nrow = 5)
      
      title <- input$Site
      p1 + p2 + p3 + p4 + p5 + patchwork::plot_layout(design = layout) +
        patchwork::plot_annotation(title = title)

    }) %>% bindCache(selectedData())
    
    output$timeseries2 <- renderPlot({
      
      p1 <-pr_plot_EOV(outputs(), "ShannonCopepodDiversity", "log10", pal = "matter")
      p2 <-pr_plot_EOV(outputs(), "ShannonPhytoDiversity", "log10", pal = "algae")

     # gridExtra::grid.arrange(p1, p2, p3, p4, p5, nrow = 5)
      title <- input$Site
      p1 + p2 + p3 + p4 + p5 + patchwork::plot_layout(design = layout) +
        patchwork::plot_annotation(title = title)
      
    }) %>% bindCache(selectedData())


})}
