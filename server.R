function(input, output, session) {
  
  library(tidyverse)
  library(lubridate)
  library(rnaturalearth)
  library(sf)
  library(patchwork)
  
  meta <- read.csv('NRS_Meta.csv')
  
  dat <- read_csv('PlanktonIndexes.csv') %>% 
    mutate(SAMPLE_DATE_UTC = dmy_hm(SAMPLE_DATE_UTC, tz = "UTC")) %>% 
    rename(Date_UTC = SAMPLE_DATE_UTC, Copepod_Abundance_ind_m3 = T_COP_ABUN_M3,
           Copepod_AvgSize_mm = COP_AVG_MM, Herbivore_Carnivore_Ratio = HERB_CARN_RAT,
           Number_Copepods = COPES_SAMPLES, Copepod_Diversity = COPE_DIVERSITY,
           Copepod_Evenness = COPE_EVEN, Carbon_Biomass_mg_m3 = CARBON_MGM3) %>% 
    mutate(STATION2 = STATION,
           STATION2 = str_replace_all(STATION2, "PHB", "Port Hacking (PHB)"),
           STATION2 = str_replace_all(STATION2, "DAR", "Darwin Harbour (DAR)"),
           STATION2 = str_replace_all(STATION2, "ESP", "Esperance (ESP)"),
           STATION2 = str_replace_all(STATION2, "KAI", "Kangaroo Island (KAI)"),
           STATION2 = str_replace_all(STATION2, "MAI", "Maria Island (MAI)"),
           STATION2 = str_replace_all(STATION2, "NIN", "Ningaloo Reef (NIN)"),
           STATION2 = str_replace_all(STATION2, "NSI", "North Stradbroke Island (NSI)"),
           STATION2 = str_replace_all(STATION2, "ROT", "Rottnest Island (ROT)"),
           STATION2 = str_replace_all(STATION2, "YON", "Yongala (YON)")) %>% 
    mutate(Month = month(Date_UTC, label = TRUE, abbr = TRUE),
           Year = year(Date_UTC),
           STATION = factor(STATION),
           STATION2 = factor(STATION2)) %>% 
    arrange(Date_UTC) %>% # Sort in ascending date order
    complete(Year, STATION) # Turns implicit missing values into explicit missing values.
  
  
  output$Site <- renderUI({
    checkboxGroupInput("sta", 
                       label = h3("Select NRS Station"), 
                       choiceValues = levels(dat$STATION),
                       choiceNames = levels(dat$STATION2),
                       selected = "MAI")
  })
  
  # I need to figure out a way to pad out the dataframe to include all combinations so that the bars are the same width
  # selectedData <- complete(selectedData,
  
  selectedData <- reactive({
    selectedData <- as.data.frame(filter(dat, dat$STATION %in% input$sta))
    
    # Drop unwanted factor levels from whole dataframe
    selectedData[] <- lapply(selectedData, function(x) if(is.factor(x)) factor(x) else x)
    selectedData$Month <- month(selectedData$Date_UTC, label = TRUE, abbr = TRUE)
    selectedData$Year <- year(selectedData$Date_UTC)
    selectedData$ycol <- selectedData[, colnames(selectedData) %in% input$ycol]
    
    return(selectedData)
  })
  
  output$ycol <- renderUI({
    selectInput('ycol', 'Variable', names(dat[5:10]))
  })
  
  # Use this code if I want to print out something on the UI
  output$selected_var <- renderText({ 
    
    # paste(selectedData()$STATION)
    
  })
  
  
  # Plot abundance spectra by species
  output$timeseries <- renderPlot({
    # if (is.null(dat()))
    #     return(NULL)
    
    p1 <- ggplot(selectedData(), aes(x = Date_UTC, y = ycol)) +
      geom_line(aes(group = STATION, color = STATION)) +
      geom_point(aes(group = STATION, color = STATION)) +
      scale_x_datetime() +
      labs(y = input$ycol)
    
    dat_mth <- selectedData() %>% 
      group_by(Month, STATION) %>% 
      summarise(mean = mean(ycol),
                N = length(ycol),
                sd = sd(ycol),
                se = sd / sqrt(N))
    
    # Error bars represent standard error of the mean
    p2 <- ggplot(data = dat_mth, aes(x = Month, y = mean, fill = STATION)) + 
      geom_col(position = position_dodge()) +
      geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                    width = .2,                    # Width of the error bars
                    position = position_dodge(.9)) +
      labs(y = input$ycol)
    
    ##
    dat_yr <- selectedData() %>% 
      group_by(Year, STATION) %>% 
      summarise(mean = mean(ycol),
                N = length(ycol),
                sd = sd(ycol),
                se = sd / sqrt(N))
    
    # Error bars represent standard error of the mean
    p3 <- ggplot(data = dat_yr, aes(x = Year, y = mean, fill = STATION)) + 
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
    
    meta2_sf <- subset(meta_sf, meta_sf$STATION %in% selectedData()$STATION)
    
    pmap <- ggplot() + 
      geom_sf(data = aust, size = 0.05, fill = "grey80") +
      geom_sf(data = meta_sf, colour = "blue", size = 1.5) +
      geom_sf(data = meta2_sf, colour = "red", size = 1.5) +
      scale_x_continuous(expand = c(0, 0), limits = c(112, 155)) +
      scale_y_continuous(expand = c(0, 0), limits = c(-45, -9)) +
      theme_void() +
      theme(axis.title = element_blank(), panel.background = element_rect(fill = NA, colour = NA))
  
  }, height = 200)
  
  
  # # Table of selected dataset ----
  # output$table <- renderTable({
  #   datasetInput()
  # })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {paste(input$ycol, ".csv", sep = "")},
    # colnames(selectedData)[colnames(selectedData)=="ycol"] <- paste(input$ycol),
    content = function(file) {
      write.table(selectedData(), file, row.names = FALSE, col.names = c("Date_UTC", "Month", "Station", input$ycol), sep = ",")
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
