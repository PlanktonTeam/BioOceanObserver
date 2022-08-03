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
             theme = bslib::bs_theme(version = 5, 
                                     bootswatch = "flatly",
                                     # primary = "#2C3E50"
                                     "border-width" = "5px",
                                     # "border-color" = "red",
                                     "enable-rounded" = TRUE), #https://rstudio.github.io/bslib/articles/bslib.html#custom
             selected = "Welcome", 
             tabPanel("Welcome",
                      sidebarLayout(
                        sidebarPanel(
                          # tags$style(".well {background-color:grey;}"),
                          img(src = "www/BOO_Hex2.png", width = "95%")),
                        mainPanel(
                          h2("Welcome to The Biological Ocean Observatory"),
                          p("The goal of this site is to Integrate, Analyse and Visualise data collected by the", 
                            tags$a(href="https://imos.org.au","Integrated Marine Observing System (IMOS)"),". 
                            We aim to enhance the availability and understanding of biological data and make it 
                            accessible to broader and non-specialist audiences in order to accelerate the next 
                            generation of scientific insights."),
                          p("Data was sourced from Australia's ",tags$a(href="https://imos.org.au","Integrated Marine Observing System (IMOS)"),
                            " - IMOS is enabled by the National Collaborative Research Infrastructure Strategy (NCRIS). 
                            It is operated by a consortium of institutions as an unincorporated joint venture, with 
                            the University of Tasmania as Lead Agent."),
                          h3("Code"),
                          p("This project is entirely open source, as are all the IMOS data underlying it. All the 
                            code for this tool are freely available on GitHub. We welcome collaborators and pull 
                            requests are gratefully accepted."),
                          h3("Who we are"),
                          p("This tool was originally conceived and developed by Dr Jason Everett (UQ/CSIRO/UNSW) 
                            and Claire Davies (CSIRO). Jason is a biological oceanographer and Claire is a plankton 
                            ecologist. Both have a strong interest in open data science and encouraging increased 
                            data uptake to solve real world problems."),
                          h3("The IMOS plankton team is on facebook"),
                          p("Like or follow us @", tags$a(href="https://www.facebook.com/imosaustralianplanktonsurvey", "IMOS plankton team on facebook")
                          )))),
             tabPanel("Snapshot",
                      fluidPage(
                        value = "snap", mod_Snapshot_ui("Snapshot_ui_1"))
             ),
             tabPanel("EOVs",
                      tabsetPanel(id = 'pol', type = "pills",
                                  tabPanel(value = "nrs", "National Reference Stations", mod_PolNRS_ui("PolNRS_ui_1")),
                                  tabPanel(value = "cpr", "CPR - bioregions", mod_PolCPR_ui("PolCPR_ui_1")),
                                  tabPanel(value = 'LTM', "Long term monitoring", mod_PolLTM_ui("PolLTM_ui_1"))
                      )),
             tabPanel("Microbes",
                      tabsetPanel(id = 'mic', type = "pills",
                                  tabPanel(value = "mts", "Time Series NRS", mod_MicroTsNRS_ui("MicroTsNRS_ui_1")),
                                  # tabPanel("Diversity"),
                                  # tabPanel("Composition")
                      )),
             tabPanel("Phytoplankton",
                      tabsetPanel(id = 'phyto', type = "pills",
                                  tabPanel(value = "pts", "Time Series NRS", mod_PhytoTsNRS_ui("PhytoTsNRS_ui_1")),
                                  tabPanel(value = "ptscpr", "Time Series CPR", mod_PhytoTsCPR_ui("PhytoTsCPR_ui_1")),
                                  tabPanel(value = "distp", "Species information", mod_PhytoSpatial_ui("PhytoSpatial_ui_1"))
                      )),
             tabPanel("Zooplankton",
                      tabsetPanel(id = 'zoo', type = "pills",
                                  tabPanel(value = "zts", "Time Series NRS", mod_ZooTsNRS_ui("ZooTsNRS_ui_1")),
                                  tabPanel(value = "ztscpr", "Time Series CPR", mod_ZooTsCPR_ui("ZooTsCPR_ui_1")),
                                  tabPanel(value = "dist", "Species information", mod_ZooSpatial_ui("ZooSpatial_ui_1")),
                                  # tabPanel(value = "SA", "Spatial Analysis")
                      )),
             tabPanel("Larval Fish",
                      tabsetPanel(id = 'fish', type = "pills",
                                  tabPanel("Time Series",
                                           img(src = "www/FishComingSoon.png", width = "40%", style="display: block; margin-left: auto; margin-right: auto;")),
                                  tabPanel("Spatial Analysis",
                                           img(src = "www/FishComingSoon.png", width = "40%", style="display: block; margin-left: auto; margin-right: auto;")),
                                  )),
             tabPanel("Environmental Data",
                      tabsetPanel(id = 'env', type = "pills",
                                  tabPanel(value = "bgc", "NRS BGC Nutrients", mod_NutrientsBGC_ui("NutrientsBGC_ui_1")),
                                  tabPanel(value = "pigs", "NRS BGC Pigments", mod_PigmentsBGC_ui("PigmentsBGC_ui_1")),
                                  tabPanel(value = "water", "NRS BGC Water", mod_WaterBGC_ui("WaterBGC_ui_1")),
                                  tabPanel("NRS Moorings",
                                           img(src = "www/FishComingSoon.png", width = "40%", style="display: block; margin-left: auto; margin-right: auto;")))),
             navbarMenu("", icon = icon("question-circle"),
                        tabPanel("Help", icon = icon("question")),
                        tabPanel(actionLink("help_keyboard", "Keyboard shortcuts", icon = icon("keyboard"))),
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
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    
    # Script for CSIRO branding tab ----
    # tags$script(type="text/javascript",
                # src="https://www.csiro.au/themes/default/js/csirotab.min.js"),
    # Google fonts ----
    tags$link(href="https://fonts.googleapis.com/css?family=Open+Sans",
              rel="stylesheet"),
    tags$link(href="https://fonts.googleapis.com/css?family=Montserrat",
              rel="stylesheet")
  )
}

