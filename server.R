
function(input, output, session) {
  
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(gridExtra)
  library(rworldmap)
  library(tidyr)
  
  dat <- read.csv('PlanktonIndexes.csv')
  meta <- read.csv('NRS_Meta.csv')
  
  dat$SAMPLE_DATE_UTC <- as.Date(dat$SAMPLE_DATE_UTC,"%d/%m/%y",tz = "GMT")
  
  colnames(dat)[colnames(dat)=="SAMPLE_DATE_UTC"] <- "Date_UTC"
  colnames(dat)[colnames(dat)=="T_COP_ABUN_M3"] <- "Copepod_Abundance_ind_m3"
  colnames(dat)[colnames(dat)=="COP_AVG_MM"] <- "Copepod_AvgSize_mm"
  colnames(dat)[colnames(dat)=="HERB_CARN_RAT"] <- "Herbivore_Carnivore_Ratio"
  colnames(dat)[colnames(dat)=="COPES_SAMPLES"] <- "Number_Copepods"
  colnames(dat)[colnames(dat)=="COPE_DIVERSITY"] <- "Copepod_Diversity"
  colnames(dat)[colnames(dat)=="COPE_EVEN"] <- "Copepod_Evenness"
  colnames(dat)[colnames(dat)=="CARBON_MGM3"] <- "Carbon_Biomass_mg_m3"
  
  dat$STATION[dat$STATION == "PH4"] <- "PHB"
  
  dat$STATION2 <- dat$STATION
  dat$STATION2 <- revalue(dat$STATION2, c("PHB"="Port Hacking (PHB)", "DAR"="Darwin Harbour (DAR)", "ESP"="Esperance (ESP)", 
                                          "KAI"="Kangaroo Island (KAI)", "MAI"="Maria Island (MAI)", "NIN"="Ningaloo Reef (NIN)",
                                          "NSI"="North Stradbroke Island (NSI)", "ROT"="Rottnest Island (ROT)", "YON"="Yongala (YON)"))
  
  dat$Month <- month(dat$Date_UTC, label=TRUE, abbr=TRUE) 
  dat$Year <- year(dat$Date_UTC)
  
  dat <- complete(dat, Year, STATION)
  
  dat$STATION <- factor(dat$STATION) # Drop excess factors
  dat$STATION2 <- factor(dat$STATION2) # Drop excess factors
  
  
  # Sort in ascending date order
  dat <- arrange(dat, Date_UTC)
  
  output$Site <- renderUI({
    checkboxGroupInput("sta", label = h3("Select NRS Station"), 
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
    selectedData$Month <- month(selectedData$Date_UTC, label=TRUE, abbr=TRUE)
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
  
  output$affiliation1 <- renderText({ 
    "Developed by:"
  })
  output$affiliation2 <- renderText({ 
    "Dr Jason Everett (The University of QLD)"
  })
  output$affiliation3 <- renderText({ 
    "using IMOS data (www.imos.org.au)"
  })
  
  output$IMOS_Logo <- renderImage({ 
    list(src = "logo.png",
         width=165,
         height=55,
         alt = "This is alternate text")
  },deleteFile = FALSE)
  
  # browser()  
  
  plotInput <- reactive({
    par(mar = c(7.1, 4.1, 0, 1))
    p1 <- ggplot(selectedData(), aes(x=Date_UTC, y=ycol)) +
      geom_line(aes(group = STATION, color = STATION)) +
      geom_point(aes(group = STATION, color = STATION)) +
      scale_x_date() +
      labs(y = input$ycol)
    
    cdata <- ddply(selectedData(), c("Month","STATION"), summarise,
                   mean = mean(ycol),
                   N    = length(ycol),
                   sd   = sd(ycol),
                   se   = sd / sqrt(N)
    )
    
    # Error bars represent standard error of the mean
    p2 <- ggplot(data = cdata, aes(x=Month, y=mean, fill=STATION)) + 
      geom_col(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      labs(y = input$ycol)
    
    ##
    cdata2 <- ddply(selectedData(), c("Year","STATION"), summarise,
                    mean = mean(ycol),
                    N    = length(ycol),
                    sd   = sd(ycol),
                    se   = sd / sqrt(N)
    )
    
    # Error bars represent standard error of the mean
    p3 <- ggplot(data = cdata2, aes(x=Year, y=mean, fill=STATION)) + 
      geom_col(position=position_dodge(), stat="identity") +
      geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                    width=.2,                    # Width of the error bars
                    position=position_dodge(.9)) +
      labs(y = input$ycol)
    
    
    grid.arrange(p1, p2, p3, top=0, bottom=10, left=4.1, right=0.5)
  })
  
  output$plot1 <- renderPlot({
    print(plotInput())
  },
  height=600
  )
  
  
  
  plotMapInput <- reactive({  
    data("countryExData", envir=environment(), package="rworldmap")
    mymap <- joinCountryData2Map(countryExData, 
                                 joinCode = "ISO3", nameJoinColumn = "ISO3V10", mapResolution = "low")
    mymap <- fortify(mymap) 
    meta2<- subset(meta, meta$STATION %in% selectedData()$STATION)
    
    pmap <- ggplot() + 
      coord_map(projection = "mercator", xlim = c(110, 160), ylim = c(-45, -8)) +
      geom_polygon(data = mymap, aes(long, lat, group = group), 
                   color = "grey", fill = "grey", size = 0.3) +
      geom_point(data = meta, aes(x = Longitude, y = Latitude), color = "black", cex = 4) +
      geom_point(data = meta2, x = meta2$Longitude, y = meta2$Latitude, color = "red", cex = 4) +
      xlim(110, 160) +
      ylim(-50, -5) +
      labs(x = '', y = '') +
      coord_fixed()
    pmap
  })
  
  output$plotmap <- renderPlot({
    print(plotMapInput())
  }
  # height=200
  )
  
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