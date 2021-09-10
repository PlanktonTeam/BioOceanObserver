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
        plotlyOutput(nsZooTsNRS("plotmap"), height = "200px"),
        checkboxGroupInput(inputId = nsZooTsNRS("Site"), label = "Select a station", choices = unique(datNRSi$Station), selected = "Maria Island"),
        selectInput(inputId = nsZooTsNRS("ycol"), label = 'Select a parameter', choices = unique(datNRSi$parameters), selected = "Biomass_mgm3"),
        downloadButton(nsZooTsNRS("downloadData"), "Data"),
        downloadButton(nsZooTsNRS("downloadPlot"), "Plot"),
        downloadButton(nsZooTsNRS("downloadNote"), "Notebook")
      ),
      mainPanel(
        h6(textOutput(nsZooTsNRS("PlotExp"), container = span)),  
        textOutput(nsZooTsNRS("selected_var")),
        plotly::plotlyOutput(nsZooTsNRS("timeseries"), height = "800px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
      )))
}
    
#' ZooTsNRS Server Functions
#'
#' @noRd 
mod_ZooTsNRS_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    selectedData <- reactive({
      req(input$Site)
      req(input$ycol)
      validate(need(!is.na(input$Site), "Error: Please select a station."))
      validate(need(!is.na(input$ycol), "Error: Please select a parameter."))
      
      selectedData <- datNRSi %>% dplyr::filter(.data$Station %in% input$Site,
                                                .data$parameters %in% input$ycol) %>%
        droplevels()
      
    }) %>% bindCache(input$ycol,input$Site)
    
    n <- length(unique(selectedData()$Station))
    pal <- 'matter'

    plotCols <- planktonr::pr_get_PlotCols(pal, n)
    aust <- MapOz

    pr_plot_timeseries <- function(df){
      p1 <- ggplot(df, aes(x = .data$SampleDateLocal, y = Values)) +
        geom_line(aes(group = Code, color = Code)) +
        geom_point(aes(group = Code, color = Code)) +
        scale_x_datetime() +
        labs(y = "") +
        scale_colour_manual(values = plotCols) 
      p1 <- ggplotly(p1) 
      return(p1)
    }
    
    pr_plot_climate <- function(df, x){
      x <- dplyr::enquo(arg = x)
      
      df_climate <- df %>% dplyr::filter(!!x != 'NA') %>% # need to drop NA from month, added to dataset by complete(Year, Code)
        dplyr::group_by(!!x, .data$Code) %>%
        dplyr::summarise(mean = mean(.data$Values, na.rm = TRUE),
                         N = length(.data$Values),
                         sd = stats::sd(.data$Values, na.rm = TRUE),
                         se = sd / sqrt(.data$N),
                         .groups = "drop")
      
      p2 <- ggplot(df_climate, aes(x = !!x, y = .data$mean, fill = .data$Code)) +
      geom_col(position = position_dodge()) +
      geom_errorbar(aes(ymin = .data$mean-.data$se, ymax = .data$mean+.data$se), 
                    width = .2,                    # Width of the error bars
                    position = position_dodge(.9)) +
      labs(y = input$ycol) +
      scale_fill_manual(values = plotCols) 
      
      p2 <- ggplotly(p2) 
      return(p2)
    }

    p1 <- pr_plot_timeseries(selectedData())
    p2 <- pr_plot_climate(selectedData(), Month)
    p3 <- pr_plot_climate(selectedData(), Year) %>%
      layout(legend = list(orientation = "h", y = -0.1))
    
          # Plot abundance spectra by species
    output$timeseries <- plotly::renderPlotly({
      
      if (is.null(datNRSi$Code))  ## was reading datNRSi() as function so had to change to this, there should always be a code
        return(NULL)
      
       subplot(style(p1, showlegend = FALSE), style(p2, showlegend = FALSE), p3, nrows = 3, titleY = TRUE, titleX = TRUE, margin = 0.05) 
      })
    
    output$plotmap <- renderPlotly({ 
      
      meta2_sf <- subset(meta_sf, meta_sf$Code %in% selectedData()$Code)
      
      pmap <- ggplot() +
        geom_sf(data = aust, size = 0.05, fill = "grey80") +
        geom_sf(data = meta_sf, colour = "blue", size = 1.5) +
        geom_sf(data = meta2_sf, colour = "red", size = 1.5) +
        scale_x_continuous(expand = c(0, 0), limits = c(112, 155)) +
        scale_y_continuous(expand = c(0, 0), limits = c(-45, -9)) +
        theme_void() +
        theme(axis.title = element_blank(), panel.background = element_rect(fill = NA, colour = NA))
      pmap <- ggplotly(pmap)
      
    }) %>% bindCache(input$ycol, selectedData())
    
    # add text information 
    output$PlotExp <- renderText({
      "A plot of selected indicies from the NRS around Australia, as a time series, a monthly climatology and an annual mean"
    }) 
    
    # Table of selected dataset ----
    output$table <- renderTable({
      datasetInput()
    })
    
    #Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {paste(input$ycol, ".csv", sep = "")},
      # colnames(selectedData)[colnames(selectedData)=="ycol"] <- paste(input$ycol),
      content = function(file) {
        write.table(selectedData(), file, row.names = FALSE, col.names = c("SampleDateLocal", "Month", "Code", input$ycol), sep = ",")
      }
    )
    
    # Download figure
    output$downloadPlot <- downloadHandler(
      filename = function() {paste(input$ycol, '.png', sep='') },
      content = function(file) {
        ggsave(file, plot = plotInput(), device = "png")
      }
    )
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
