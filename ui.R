
library(shiny)
library(shinythemes)

navbarPage(title = div(img(src = "logo.png", style="margin-top: -10px; padding-right:5px;padding-bottom:2px", height = 40), "The Biological Ocean Observatory"),
           windowTitle="The Biological Ocean Observatory",
           theme = shinytheme("flatly"), 
           selected = "Zooplankton", 
           footer = "© 2021 Jason Everett (UQ, UNSW, CSIRO) and Claire Davies (CSIRO)",
           tabPanel("Welcome"),         
           tabPanel("Microbes"),
           tabPanel("Phytoplankton"),
           tabPanel("Zooplankton",
                    tabsetPanel(
                      tabPanel("Time Series",
                               sidebarLayout(
                                 sidebarPanel(
                                   plotOutput("plotmap", height = "200px"),
                                   uiOutput("Site"),
                                   uiOutput("ycol"),
                                   downloadButton("downloadData", "Data"),
                                   downloadButton("downloadPlot", "Plot"),
                                   downloadButton("downloadNote", "Notebook")),
                                 mainPanel(
                                   textOutput("selected_var"),
                                   plotOutput("timeseries")))),
                      tabPanel("Spatial Analysis",   h5("This is a static text")),
                      tabPanel("Climatology"),
                      tabPanel("Statistical Analysis"),
                      tabPanel("Environmental Drivers")
                    )),
           tabPanel("Larval Fish"),
           tabPanel("Environmenal Data")
)