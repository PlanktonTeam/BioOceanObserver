function(input, output, session) {
  
  library(tidyverse)
  library(lubridate)
  library(rnaturalearth)
  library(sf)
  library(patchwork)
  
  meta <- read.csv('NRS_Meta.csv')
  
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
    checkboxGroupInput("sta", 
                       label = h3("Select NRS Code"), 
                       choiceValues = levels(datNRSi$Code),
                       choiceNames = levels(datNRSi$Name),
                       selected = "MAI")
  })
  
  # I need to figure out a way to pad out the dataframe to include all combinations so that the bars are the same width
  # selectedData <- complete(selectedData,
  
  selectedData <- reactive({
    selectedData <- as.data.frame(filter(datNRSi, datNRSi$Code %in% input$sta))
    
    # Drop unwanted factor levels from whole dataframe
    selectedData[] <- lapply(selectedData, function(x) if(is.factor(x)) factor(x) else x)
    selectedData$ycol <- selectedData[, colnames(selectedData) %in% input$ycol]
    
    return(selectedData)
  })
  
  output$ycol <- renderUI({
    selectInput('ycol', 'Variable', names(select(datNRSi,ZoopAbundance_m3:CopepodEvenness)))
  })
  
  # Use this code if I want to print out something on the UI
  output$selected_var <- renderText({ 
    
    # paste(selectedData()$Code)
    
  })
  
  
  # Plot abundance spectra by species
  output$timeseries <- renderPlot({
    # if (is.null(datNRSi()))
    #     return(NULL)
    
    p1 <- ggplot(selectedData(), aes(x = SampleDateLocal, y = ycol)) +
      geom_line(aes(group = Code, color = Code)) +
      geom_point(aes(group = Code, color = Code)) +
      scale_x_datetime() +
      labs(y = input$ycol)
    
    dat_mth <- selectedData() %>% 
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
      labs(y = input$ycol)
    
    ##
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
      labs(y = input$ycol)
    
    p1 / p2 / p3 # Use patchwork to arrange plots
  },
  height=600)
  
  
  output$plotmap <- renderPlot({
    
    aust <- ne_countries(scale = "medium", country = "Australia", returnclass = "sf")
    
    meta_sf <- meta %>% 
      st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
    
    meta2_sf <- subset(meta_sf, meta_sf$Code %in% selectedData()$Code)
    
    pmap <- ggplot() + 
      geom_sf(data = aust, size = 0.05, fill = "grey80") +
      geom_sf(data = meta_sf, colour = "blue", size = 1.5) +
      geom_sf(data = meta2_sf, colour = "red", size = 1.5) +
      scale_x_continuous(expand = c(0, 0), limits = c(112, 155)) +
      scale_y_continuous(expand = c(0, 0), limits = c(-45, -9)) +
      theme_void() +
      theme(axis.title = element_blank(), panel.background = element_rect(fill = NA, colour = NA))
    pmap
  })
  
  
  # # Table of selected dataset ----
  # output$table <- renderTable({
  #   datasetInput()
  # })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {paste(input$ycol, ".csv", sep = "")},
    # colnames(selectedData)[colnames(selectedData)=="ycol"] <- paste(input$ycol),
    content = function(file) {
      write.table(selectedData(), file, row.names = FALSE, col.names = c("SampleDateLocal", "Month", "Code", input$ycol), sep = ",")
    }
  )
  
  # # Download figure
  output$downloadPlot <- downloadHandler(
    filename = function() {paste(input$ycol, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png")
    }
  )
  
}
