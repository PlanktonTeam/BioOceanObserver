## Zooplankton
# Timeseries NRS

# function for UI module

load("data/ZoOTsNRS.RData")

ZooTsNRSUI <- function(id){
  
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
           textOutput(nsZooTsNRS("selected_var")),
           plotlyOutput(nsZooTsNRS("timeseries"), height = "800px") %>% withSpinner(color="#0dc5c1")
          )))
}

# function for server
ZooTsNRS <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
        # I need to figure out a way to pad out the dataframe to include all combinations so that the bars are the same width
          
          selectedData <- reactive({
            req(input$Site)
            req(input$ycol)
            validate(need(!is.na(input$Site), "Error: Please select a station."))
            validate(need(!is.na(input$ycol), "Error: Please select a parameter."))
            
            selectedData <- datNRSi %>% filter(Station %in% input$Site,
                                               parameters %in% input$ycol) %>%
              droplevels()

        }) %>% bindCache(input$ycol,input$Site)

        # Plot abundance spectra by species
        output$timeseries <- renderPlotly({
          
          if (is.null(datNRSi$Code))  ## was reading datNRSi() as function so had to change to this, there should always be a code
             return(NULL)

          p1 <- ggplot(selectedData(), aes(x = SampleDateLocal, y = Values)) +
            geom_line(aes(group = Code, color = Code)) +
            geom_point(aes(group = Code, color = Code)) +
            scale_x_datetime() +
            #labs(y = input$ycol) +
            theme(legend.position = "none")
          p1 <- ggplotly(p1) %>% layout(showlegend = FALSE)

          dat_mth <- selectedData() %>% filter(Month != 'NA') %>% # need to drop NA from month, added to dataset by complete(Year, Code)
            group_by(Month, Code) %>%
            summarise(mean = mean(Values, na.rm = TRUE),
                      N = length(Values),
                      sd = sd(Values, na.rm = TRUE),
                      se = sd / sqrt(N),
                      .groups = "drop")

          # Error bars represent standard error of the mean
          p2 <- ggplot(data = dat_mth, aes(x = Month, y = mean, fill = Code)) +
            geom_col(position = position_dodge()) +
            geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                          width = .2,                    # Width of the error bars
                          position = position_dodge(.9)) +
            labs(y = input$ycol) +
            theme(legend.position = "none")
          p2 <- ggplotly(p2) %>% layout(showlegend = FALSE)

          dat_yr <- selectedData() %>%
            group_by(Year, Code) %>%
            summarise(mean = mean(Values, na.rm = TRUE),
                      N = length(Values),
                      sd = sd(Values, na.rm = TRUE),
                      se = sd / sqrt(N),
                      .groups = "drop")

          # Error bars represent standard error of the mean
          p3 <- ggplot(data = dat_yr, aes(x = Year, y = mean, fill = Code)) +
            geom_col(position = position_dodge()) +
            geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                          width = .2,                    # Width of the error bars
                          position = position_dodge(.9)) +
            #labs(y = input$ycol) +
            theme(legend.position = "bottom",
                  legend.title = element_blank())
          p3 <- ggplotly(p3) %>%
            layout(legend = list(orientation = "h", y = -0.1))

          subplot(style(p1, showlegend = FALSE), style(p2, showlegend = FALSE), p3, nrows = 3, titleY = TRUE, titleX = TRUE, margin = 0.05) # Use plotly to arrange plots
          # need to sort out legends, can do this by using plotly to create the graphs rather than converting from ggplot
          #p1 / p2 / p3 # Use patchwork to arrange plots
        })

        output$plotmap <- renderPlotly({ # renderCachedPlot plot so cached version can be returned if it exists (code only run once per scenario per session)
           aust <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")

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

})}

        