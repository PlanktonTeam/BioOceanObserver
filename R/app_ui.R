#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
    # Your application UI logic 
    navbarPage(golem_add_external_resources(), # Leave this function for adding external resources
               id = "navbar",  
               title = div(img(src = "www/logo.png", style="margin-top: -10px; padding-right:5px;padding-bottom:2px", height = 40), "The Biological Ocean Observatory"),
               windowTitle = "The Biological Ocean Observatory",
               theme = shinythemes::shinytheme("flatly"), # not recommended to use themes anymore as used in bslib package
               selected = "Welcome", 
               footer = column(12, "\u00A9 2021 Jason Everett (UQ, UNSW, CSIRO) and Claire Davies (CSIRO)"), # \u00A9 is equivalent to © 
               tabPanel("Welcome",
                        titlePanel("Welcome to The Biological Ocean Observatory"),
                        sidebarLayout(
                          sidebarPanel(
                          ),
                          mainPanel(
                            p("The goal of this site is to Integrate, Analyse and Visualise data collected by the", tags$a(href="https://imos.org.au","Integrated Marince Observing System (IMOS)"),". We aim to enhance the availability and understanding of biological data and make it accessible to broader and non-specialist audiences in order to accelerate the generation of scientific insights."),
                            h3("Code"),
                            p("This project is entirely open source, as are all the IMOS data underlying it. All the code for this tool are freely available on GitHub. We welcome collaborators and pull requests are gratefully accepted."),
                            h3("Who we are"),
                            p("This tool was originally conceived and developed by Dr Jason Everett (UQ/CSIRO/UNSW) and Claire Davies (CSIRO). Jason is a biological oceanographer and Claire is a plankton ecologist. Both have a strong interest in open data science and encouraging increased data uptake to solve real world problems.")
                          ))),
               tabPanel("Snapshot",
                        fluidPage(
                          value = "snap", mod_Snapshot_ui("Snapshot_ui_1"))
                        ),
               tabPanel("Microbes",
                        tabsetPanel(id = 'mic',
                                    tabPanel("Section 1"),
                                    tabPanel("Section 2"),
                                    tabPanel("Section 3"))),
               tabPanel("Phytoplankton",
                        tabsetPanel(id = 'phyto',
                                    tabPanel(value = "pts", "Time Series NRS", mod_PhytoTsNRS_ui("PhytoTsNRS_ui_1")),
                                    tabPanel(value = "ptscpr", "Time Series CPR", mod_PhytoTsCPR_ui("PhytoTsCPR_ui_1")),
                                    tabPanel("Section 3"))),
               tabPanel("Zooplankton",
                        tabsetPanel(id = 'zoo',
                                    tabPanel(value = "zts", "Time Series NRS", mod_ZooTsNRS_ui("ZooTsNRS_ui_1")),
                                    tabPanel(value = "ztscpr", "Time Series CPR", mod_ZooTsCPR_ui("ZooTsCPR_ui_1")),
                                    tabPanel(value = "dist", "Distributions", mod_ZooSpatial_ui("ZooSpatial_ui_1")),
                                    tabPanel(value = "SA", "Spatial Analysis"),
                                    tabPanel("Climatology",
                                             h3("Long term means")),
                                    tabPanel("Environmental Drivers",
                                             h3("Good place for SDMs?"))
                        )),
               tabPanel("Larval Fish",
                        tabsetPanel(id = 'fish',
                                    tabPanel("Section 1"),
                                    tabPanel("Section 2"),
                                    tabPanel("Section 3"))),
               tabPanel("Environmental Data",
                        tabsetPanel(id = 'env',
                                    tabPanel(value = "bgc", "NRS BGC Nutrients", mod_NutrientsBGC_ui("NutrientsBGC_ui_1")),
                                    tabPanel(value = "pigs", "NRS BGC Pigments", mod_PigmentsBGC_ui("PigmentsBGC_ui_1")),
                                    tabPanel("Section 3"))),
               navbarMenu("", icon = icon("question-circle"),
                          tabPanel("Help", icon = icon("question")),
                          tabPanel(actionLink("help_keyboard", "Keyboard shortcuts", icon = icon("keyboard-o"))),
                          tabPanel("About", uiOutput("help_about"), icon = icon("info")),
                          tabPanel(tags$a("", href = "https://github.com/PlanktonTeam/IMOS_BioOceanObserver/wiki", target = "_blank",
                                          list(icon("globe"), "Documentation"))),
                          tabPanel(tags$a("", href = "https://github.com/PlanktonTeam/IMOS_BioOceanObserver/issues", target = "_blank",
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
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'imosboo'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

