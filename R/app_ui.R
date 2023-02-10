#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  # Your application UI logic 
  shiny::navbarPage(golem_add_external_resources(), # Leave this function for adding external resources
                    id = "navbar",  
                    title = span(img(src = "www/logo.png", style="padding-right:5px", height = 40), "Biological Ocean Observer"),
                    windowTitle = "Biological Ocean Observer",
                    theme = bslib::bs_theme(version = 5, 
                                            bootswatch = "flatly",
                                            "border-width" = "0px",
                                            "enable-rounded" = TRUE), #https://rstudio.github.io/bslib/articles/bslib.html#custom
                    selected = "Home", 
                    shiny::tabPanel("Home", mod_home_ui("home_1")
                    ),
                    shiny::tabPanel("EOVs",
                                    shiny::tabsetPanel(id = 'pol', type = "pills",
                                                       shiny::tabPanel(value = "nrs", "National Reference Stations", mod_PolNRS_ui("PolNRS_ui_1")),
                                                       shiny::tabPanel(value = "cpr", "CPR - bioregions", mod_PolCPR_ui("PolCPR_ui_1")),
                                                       shiny::tabPanel(value = 'LTM', "Long term monitoring", mod_PolLTM_ui("PolLTM_ui_1"))
                                    )),
                    shiny::tabPanel("Microbes",
                                    shiny::tabsetPanel(id = 'mic', type = "pills",
                                                       shiny::tabPanel(value = "mts", "Time Series NRS", mod_MicroTsNRS_ui("MicroTsNRS_ui_1")),
                                                       # shiny::tabPanel("Diversity"),
                                                       # shiny::tabPanel("Composition")
                                    )),
                    shiny::tabPanel("Phytoplankton",
                                    shiny::tabsetPanel(id = 'phyto', type = "pills",
                                                       shiny::tabPanel(value = "pts", "Time Series NRS", mod_PhytoTsNRS_ui("PhytoTsNRS_ui_1")),
                                                       shiny::tabPanel(value = "ptscpr", "Time Series CPR", mod_PhytoTsCPR_ui("PhytoTsCPR_ui_1")),
                                                       shiny::tabPanel(value = "distp", "Species information", mod_PhytoSpatial_ui("PhytoSpatial_ui_1"))
                                    )),
                    shiny::tabPanel("Zooplankton",
                                    shiny::tabsetPanel(id = 'zoo', type = "pills",
                                                       shiny::tabPanel(value = "zts", "Time Series NRS", mod_ZooTsNRS_ui("ZooTsNRS_ui_1")),
                                                       shiny::tabPanel(value = "ztscpr", "Time Series CPR", mod_ZooTsCPR_ui("ZooTsCPR_ui_1")),
                                                       shiny::tabPanel(value = "dist", "Species information", mod_ZooSpatial_ui("ZooSpatial_ui_1")),
                                                       # shiny::tabPanel(value = "SA", "Spatial Analysis")
                                    )),
                    shiny::tabPanel("Larval Fish",
                                    shiny::tabsetPanel(id = 'fish', type = "pills",
                                                       shiny::tabPanel(value = "fspat", "Spatial", mod_LFishSpatial_ui("LFishSpatial_1")),
                                                       shiny::tabPanel(value = "fseas", "Seasonal", mod_LFishSeason_ui("LFishSeason_1")),
                                                       shiny::tabPanel(value = "fdata", "Species Data", mod_LFishData_ui("LFishData_1")),
                                    )),
                    shiny::tabPanel("Environmental Data",
                                    shiny::tabsetPanel(id = 'env', type = "pills",
                                                       shiny::tabPanel(value = "bgc", "NRS BGC Nutrients", mod_NutrientsBGC_ui("NutrientsBGC_ui_1")),
                                                       shiny::tabPanel(value = "pico", "NRS BGC Picoplankton", mod_PicoBGC_ui("PicoBGC_ui_1")),
                                                       shiny::tabPanel(value = "pigs", "NRS BGC Pigments", mod_PigmentsBGC_ui("PigmentsBGC_ui_1")),
                                                       shiny::tabPanel(value = "water", "NRS BGC Water", mod_WaterBGC_ui("WaterBGC_ui_1")),
                                                       shiny::tabPanel(value = 'moor', "NRS Moorings", mod_MoorBGC_ui("MoorBGC_ui_1"))
                                    )),
                    shiny::tabPanel("Information",
                                    fluidPage(
                                      value = "info", mod_info_ui("info_1"))
                    ),
                    navbarMenu("", icon = icon("share-nodes"),
                               shiny::tabPanel(tags$a("", href = "https://twitter.com/intent/tweet?url=https%3A%2F%2Fjaseeverett.shinyapps.io%2FIMOS_BioOceanObserver&text=Check%20out%20the%20fantastic%20new%20IMOS%20Biological%20Ocean%20Obervatory%20being%20developed", target = "_blank",
                                                      list(icon("twitter"), "Twitter"))))
                    # shiny::tabPanel(tags$a("", href = "https://www.facebook.com.sharer/sharer.php?u=https%3A%2F%2Fjaseeverett.shinyapps.io%2FIMOS_BioOceanObserver", target = "_blank",
                    # list(icon("facebook"), "Facebook"))),
                    # shiny::tabPanel(tags$a("", href = "http://pinterest.com/pin/create/button/?url=https%3A%2F%2Fjaseeverett.shinyapps.io%2FIMOS_BioOceanObserver", target = "_blank",
                    # list(icon("pinterest-p"), "Pinterest"))),
                    # shiny::tabPanel(tags$a("", href = "http://www.linkedin.com/shareArticle?mini=true&url=https%3A%2F%2Fjaseeverett.shinyapps.io%2FIMOS_BioOceanObserver", target = "_blank",
                    # list(icon("linkedin"), "LinkedIn")))
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
    tags$script(type="text/javascript", src="https://www.csiro.au/themes/default/js/csirotab.min.js"),
    # Custom CSIRO styling CSS for modal ----
    tags$link(rel = "stylesheet", type = "text/css", href = "css/csiromodal.css"),
    # Google fonts ----
    tags$link(href="https://fonts.googleapis.com/css?family=Open+Sans",
              rel="stylesheet"),
    tags$link(href="https://fonts.googleapis.com/css?family=Montserrat",
              rel="stylesheet")
  )
}

