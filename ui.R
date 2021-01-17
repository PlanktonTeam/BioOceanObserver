
library(shiny)
library(shinythemes)

navbarPage(title = div(img(src = "logo.png", style="margin-top: -10px; padding-right:5px;padding-bottom:2px", height = 40), "The Biological Ocean Observatory"),
           windowTitle="The Biological Ocean Observatory",
           theme = shinytheme("flatly"), 
           selected = "Zooplankton", 
           footer = "© 2021 Jason Everett (UQ, UNSW, CSIRO) and Claire Davies (CSIRO)",
           tabPanel("Welcome",
                    h3("A welcome page. Who we are, what we do, and what you will learn here")),         
           tabPanel("Microbes",
                    tabsetPanel(
                      tabPanel("Section 1"),
                      tabPanel("Section 2"),
                      tabPanel("Section 3"))),
           tabPanel("Phytoplankton",
                    tabsetPanel(
                      tabPanel("Section 1"),
                      tabPanel("Section 2"),
                      tabPanel("Section 3"))),
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
                      tabPanel("Spatial Analysis", 
                               h3("Some spatial maps can go here - For example CPR data or GAM outputs?")),
                      tabPanel("Climatology",
                               h3("Long term means")),
                      tabPanel("Environmental Drivers")
                    )),
           tabPanel("Larval Fish",
                    tabsetPanel(
                      tabPanel("Section 1"),
                      tabPanel("Section 2"),
                      tabPanel("Section 3"))),
           tabPanel("Environmenal Data",
                    tabsetPanel(
                      tabPanel("Section 1"),
                      tabPanel("Section 2"),
                      tabPanel("Section 3")))
)