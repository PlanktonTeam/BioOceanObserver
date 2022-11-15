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
             # title = shiny::a(shiny::img(src = "www/NP-logo-hvit_engelsk.png", height = 90, style = "padding-right:10px; margin-top:-10px; margin-bottom:-10px"), "Weddell Sea Phase 2"),
             title = div(img(src = "www/logo.png", style="margin-top:-10px; padding-right:5px; padding-bottom:2px", height = 40), "The Biological Ocean Observer"),
             windowTitle = "The Biological Ocean Observer",
             theme = bslib::bs_theme(version = 5, 
                                     bootswatch = "flatly",
                                     # primary = "#2C3E50"
                                     "border-width" = "0px",
                                     # "border-color" = "red",
                                     "enable-rounded" = TRUE), #https://rstudio.github.io/bslib/articles/bslib.html#custom
             selected = "Welcome", 
             tabPanel("Welcome",
                      
                      fluidPage(
                        fluidRow(
                          column(4,
                                 img(src = "www/BOO_Hex2.png", width = "95%")),
                          column(8,
                                 shiny::h2("Welcome to The Biological Ocean Observer"),
                                 shiny::HTML("The goal of this site is to Integrate, Analyse and Visualise data collected by the 
                            <a href='https://imos.org.au'> Integrated Marine Observing System (IMOS)</a>. 
                            We aim to enhance the availability and understanding of biological data and make it 
                            accessible to broader and non-specialist audiences in order to accelerate the next 
                            generation of scientific insights."),
                                 shiny::br(),
                                 shiny::br(),
                                 shiny::HTML("This project is entirely open source, as are all the IMOS data underlying it. All the 
                            code for this tool are freely available on <a href='https://github.com/PlanktonTeam/IMOS_BioOceanObserver'>GitHub</a>. 
                            We welcome collaborators and pull requests are gratefully accepted."),
                                 shiny::br(),
                                 shiny::br(),
                                 shiny::HTML("This tool was originally conceived and developed by Dr Jason Everett (UQ/CSIRO/UNSW) 
                            and Claire Davies (CSIRO). Jason is a biological oceanographer and Claire is a plankton 
                            ecologist. Both have a strong interest in open data science and encouraging increased 
                            data uptake to solve real world problems."),
                                 shiny::h3("Citation"),
                                 shiny::HTML("If you use this app in any publication, please cite as: <br> <i>'Davies, Claire; Everett, Jason; Ord, Louise (2022): IMOS Biological Ocean Observer - Shiny APP. v3. CSIRO. Service Collection. <a href = http://hdl.handle.net/102.100.100/447365?index=1>http://hdl.handle.net/102.100.100/447365?index=1</a></i>."),
                                 shiny::br(),
                                 shiny::br(),
                                 shiny::HTML("All of the analysis and plotting contained in this application are powered by the <i>planktonr</i> package: <br>
                                      <i>Everett J, Davies C (2022). planktonr: Analysis and visualisation of plankton data. R package version 0.1.1.0000, <a href = https://github.com/PlanktonTeam/planktonr>https://github.com/PlanktonTeam/planktonr</a>.</i>"),
                                 shiny::h3("Acknowledging IMOS Data"),
                                 shiny::HTML("This application is developed using IMOS data, and therefore you are also required to clearly acknowledge the source material by including the following statement in any publications:"),
                                 shiny::br(),
                                 shiny::HTML("'<i>Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure Strategy (NCRIS). It is operated by a consortium of institutions as an unincorporated joint venture, with the University of Tasmania as Lead Agent'.</i>"),
                                 shiny::br(),
                                 shiny::HTML("For more information about acknowledging IMOS, see <a href = https://imos.org.au/acknowledging-us> here</a>."),
                                 shiny::h3("Licencing"),
                                 shiny::HTML("The code for this application is published under an <a href = 'https://github.com/PlanktonTeam/IMOS_BioOceanObserver/blob/master/LICENSE'> MIT licence</a>")
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
                                  tabPanel(value = 'moor', "NRS Moorings", mod_MoorBGC_ui("MoorBGC_ui_1")))),
             tabPanel("Help",
                      fluidPage(
                        value = "help", mod_help_ui("help_ui_1"))
             ),
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

