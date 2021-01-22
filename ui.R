
library(shiny)
library(shinythemes)
library(plotly)

source("Scripts/ZooTsNRS.R")
source("Scripts/EnvDataBGC.R")

navbarPage(id = "navbar",  
           title = div(img(src = "logo.png", style="margin-top: -10px; padding-right:5px;padding-bottom:2px", height = 40), "The Biological Ocean Observatory"),
           windowTitle="The Biological Ocean Observatory",
           theme = shinytheme("flatly"), 
           selected = "Welcome", 
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
                      tabPanel("Time Series", ZooTsNRSUI("one")),
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
           tabPanel("Environmental Data",
                    tabsetPanel(
                      tabPanel("NRS BGC parameters", EnvDataBGCUI("one")),
                      tabPanel("Section 2"),
                      tabPanel("Section 3"))),
           navbarMenu("", icon = icon("question-circle"),
                      tabPanel("Help", icon = icon("question")),
                      tabPanel(actionLink("help_keyboard", "Keyboard shortcuts", icon = icon("keyboard-o"))),
                      tabPanel("About", uiOutput("help_about"), icon = icon("info")),
                      tabPanel(tags$a("", href = "https://github.com/jaseeverett/IMOS_BioOceanObserver/wiki", target = "_blank",
                                      list(icon("globe"), "Documentation"))),
                      tabPanel(tags$a("", href = "https://github.com/jaseeverett/IMOS_BioOceanObserver/issues", target = "_blank",
                                      list(icon("github"), "Report issue")))),
           navbarMenu("", icon = icon("share-alt"),
                      tabPanel(tags$a("", href = "https://twitter.com/intent/tweet?url=https%3A%2F%2Fjaseeverett.shinyapps.io%2FIMOS_BioOceanObserver&text=Check%20out%20the%20fantastic%20new%20IMOS%20Biological%20Ocean%20Obervatory%20being%20developed", target = "_blank",
                                      list(icon("twitter"), "Twitter"))),
                      tabPanel(tags$a("", href = "https://www.facebook.com.sharer/sharer.php?u=https%3A%2F%2Fjaseeverett.shinyapps.io%2FIMOS_BioOceanObserver", target = "_blank",
                                      list(icon("facebook"), "Facebook"))),
                      tabPanel(tags$a("", href = "http://pinterest.com/pin/create/button/?url=https%3A%2F%2Fjaseeverett.shinyapps.io%2FIMOS_BioOceanObserver", target = "_blank",
                                      list(icon("pinterest-p"), "Pinterest"))),
                      tabPanel(tags$a("", href = "http://www.linkedin.com/shareArticle?mini=true&url=https%3A%2F%2Fjaseeverett.shinyapps.io%2FIMOS_BioOceanObserver", target = "_blank",
                                      list(icon("linkedin"), "LinkedIn"))))
)
