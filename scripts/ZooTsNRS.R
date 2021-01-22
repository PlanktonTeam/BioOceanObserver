## Zooplankton
# Timeseries NRS

# function for UI module

ZooTsNRSUI <- function(id){
  
  nsZooTsNRS <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
           plotlyOutput(nsZooTsNRS("plotmap"), height = "200px"),
           uiOutput(nsZooTsNRS("Site")),
           uiOutput(nsZooTsNRS("ycol")),
           downloadButton(nsZooTsNRS("downloadData"), "Data"),
           downloadButton(nsZooTsNRS("downloadPlot"), "Plot"),
           downloadButton(nsZooTsNRS("downloadNote"), "Notebook")
          ),
        mainPanel(
           textOutput(nsZooTsNRS("selected_var")),
           plotlyOutput(nsZooTsNRS("timeseries"), height = "800px")
          )))
}

# function for server
ZooTsNRS <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      meta <- read.csv('NRS_Meta.csv') # probably should have this file, and access it from, the same place as the other files
      
      datNRSi <- read_csv("https://raw.githubusercontent.com/jaseeverett/IMOS_Toolbox/master/Plankton/Output/NRS_Indices.csv") %>% 
        mutate(Code = str_sub(NRScode, 1, 3),
               Name = str_c(Station, " (",Code,")"), # Create neat name for plotting
               Month = month(SampleDateLocal, label = TRUE, abbr = TRUE),
               Year = year(SampleDateLocal),
               Code = factor(Code),
               Name = factor(Name)) %>% 
        arrange(SampleDateLocal) %>% # Sort in ascending date order
        complete(Year, Code) # Turns implicit missing values into explicit missing values.
      
      output$Site <- renderUI({
        ns <- session$nsZooTsNRS
        checkboxGroupInput(session$ns("sta"),
                           label = h3("Select NRS Code"),
                           choiceValues = levels(datNRSi$Code),
                           choiceNames = levels(datNRSi$Name),
                           selected = "MAI")
        })

          # I need to figure out a way to pad out the dataframe to include all combinations so that the bars are the same width
          #selectedData <- complete(selectedData,

          selectedData <- reactive({
          selectedData <- as.data.frame(filter(datNRSi, datNRSi$Code %in% input$sta))

          # Drop unwanted factor levels from whole dataframe
          selectedData[] <- lapply(selectedData, function(x) if(is.factor(x)) factor(x) else x)
          selectedData$ycol <- selectedData[, colnames(selectedData) %in% input$ycol]

          return(selectedData)
        })

          output$ycol <- renderUI({
            ns <- session$nsZooTsNRS
            selectInput(session$ns("ycol"), 'Variable', names(select(datNRSi,ZoopAbundance_m3:CopepodEvenness)))
        })

        # Use this code if I want to print out something on the UI
        # output$selected_var <- renderText({

          # paste(selectedData()$Code)

        #})

        # Plot abundance spectra by species
        output$timeseries <- renderPlotly({
          
         if (is.null(datNRSi$Code))  ## was reading datNRSi() as function so had to change to this, there should always be a code
             return(NULL)

          p1 <- ggplot(selectedData(), aes(x = SampleDateLocal, y = ycol)) +
            geom_line(aes(group = Code, color = Code)) +
            geom_point(aes(group = Code, color = Code)) +
            scale_x_datetime() +
            labs(y = input$ycol)
          p1 <- ggplotly(p1)

          dat_mth <- selectedData() %>% filter(Month != 'NA') %>% # need to drop NA from month, added to dataset by complete(Year, Code)
            group_by(Month, Code) %>%
            summarise(mean = mean(ycol, na.rm = TRUE),
                      N = length(ycol),
                      sd = sd(ycol, na.rm = TRUE),
                      se = sd / sqrt(N))

          # Error bars represent standard error of the mean
          p2 <- ggplot(data = dat_mth, aes(x = Month, y = mean, fill = Code)) +
            geom_col(position = position_dodge()) +
            geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                          width = .2,                    # Width of the error bars
                          position = position_dodge(.9)) +
            labs(y = input$ycol) +
            theme(legend.text = element_blank(),
                  legend.title = element_blank())
          p2 <- ggplotly(p2)

          dat_yr <- selectedData() %>%
            group_by(Year, Code) %>%
            summarise(mean = mean(ycol, na.rm = TRUE),
                      N = length(ycol),
                      sd = sd(ycol, na.rm = TRUE),
                      se = sd / sqrt(N))

          # Error bars represent standard error of the mean
          p3 <- ggplot(data = dat_yr, aes(x = Year, y = mean, fill = Code)) +
            geom_col(position = position_dodge()) +
            geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                          width = .2,                    # Width of the error bars
                          position = position_dodge(.9)) +
            labs(y = input$ycol) +
            theme(legend.text = element_blank(),
                  legend.title = element_blank())
          p3 <- ggplotly(p3)

          subplot(p1, p2, p3, nrows = 3, titleY = TRUE, titleX = TRUE, margin = 0.05) # Use plotly to arrange plots
          # need to sort out legends, can do this by using plotly to create the graphs rather than converting from ggplot
          #p1 / p2 / p3 # Use patchwork to arrange plots
        })

        output$plotmap <- renderCachedPlot({ # caching plot so cahced version can be returned if it exists (code only run once per scenario per session) 

          aust <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")

          meta_sf <- meta %>%
            st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

          meta2_sf <- subset(meta_sf, meta_sf$STATION %in% selectedData()$Code)

          pmap <- ggplot() +
            geom_sf(data = aust, size = 0.05, fill = "grey80") +
            geom_sf(data = meta_sf, colour = "blue", size = 1.5) +
            geom_sf(data = meta2_sf, colour = "red", size = 1.5) +
            scale_x_continuous(expand = c(0, 0), limits = c(112, 155)) +
            scale_y_continuous(expand = c(0, 0), limits = c(-45, -9)) +
            theme_void() +
            theme(axis.title = element_blank(), panel.background = element_rect(fill = NA, colour = NA))
          
        cacheKeyExpr = { input$Site } # use cached version if input reverts to a previous state based on Site selection
          
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

})}

        