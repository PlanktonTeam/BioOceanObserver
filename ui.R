pageWithSidebar(
  headerPanel('IMOS National Reference Station Copepods'),
  sidebarPanel(
    imageOutput("IMOS_Logo",height="65px"),
    plotOutput('plotmap',height = "200px"),
    uiOutput("Site"),
    uiOutput("ycol"),
    
    # Button
    downloadButton("downloadData", "Download Data"),
    downloadButton('downloadPlot', 'Download Plot'),
    
    
    textOutput("affiliation1"),
    textOutput("affiliation2"),
    textOutput("affiliation3")
    ),
  
  mainPanel(
    textOutput("selected_var"),
    plotOutput('plot1')
    # plotOutput('plot2')
  )
)
