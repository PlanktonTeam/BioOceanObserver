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
                                                       shiny::tabPanel(value = "mtsCS", "Time Series Coastal", mod_MicroTsCS_ui("MicroTsCS_ui_1")),
                                                       shiny::tabPanel(value = "GSlat", "Voyage data", mod_MicroLatGS_ui("MicroLatGS_ui_1"))
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
                                                       shiny::tabPanel(value = "bgc", "NRS Biogeochemistry", mod_NutrientsBGC_ui("NutrientsBGC_ui_1")),
                                                       shiny::tabPanel(value = "pico", "NRS Picoplankton", mod_PicoBGC_ui("PicoBGC_ui_1")),
                                                       shiny::tabPanel(value = "pigs", "NRS Pigments", mod_PigmentsBGC_ui("PigmentsBGC_ui_1")),
                                                       shiny::tabPanel(value = "water", "NRS CTD", mod_WaterBGC_ui("WaterBGC_ui_1")),
                                                       shiny::tabPanel(value = 'moor', "NRS Moorings", mod_MoorBGC_ui("MoorBGC_ui_1"))
                                    )),
                    shiny::tabPanel("Relationships",
                                    shiny::tabsetPanel(id = 'rel', type = "pills",
                                                       shiny::tabPanel(value = "nrsRel", "NRS relationship", mod_RelNRS_ui("RelNRS_ui_1")),
                                                       shiny::tabPanel(value = "csRel", "Coastal Stations relationship", mod_RelCS_ui("RelCS_ui_1")),
                                                       shiny::tabPanel(value = "cprRel", "CPR relationship", mod_RelCPR_ui("RelCPR_ui_1"))
                                    )),
                    shiny::tabPanel("Information",
                                    fluidPage(
                                      value = "info", mod_info_ui("info_1"))
                    ),
                    navbarMenu("", icon = icon("share-nodes"),
                               shiny::tabPanel(tags$a("", href = "https://twitter.com/intent/tweet?text=Check%20out%20the%20amazing%20new%20Biological%20Observer%20https%3A//shiny.csiro.au/BioOceanObserver/", target = "_blank",
                                                      list(icon("twitter"), "Twitter"))),
                               shiny::tabPanel(tags$a("", href = "https://www.facebook.com/sharer/sharer.php?u=https%3A//shiny.csiro.au/BioOceanObserver/", target = "_blank",
                                                      list(icon("facebook"), "Facebook"))),
                               shiny::tabPanel(tags$a("", href = "https://www.linkedin.com/shareArticle?mini=true&url=https%3A//shiny.csiro.au/BioOceanObserver/", target = "_blank",
                                                      list(icon("linkedin"), "LinkedIn")))),
                    navbarMenu("", icon = img(src = "www/github-mark/github-mark-white.png", style="padding-right:5px", height = 30),
                               shiny::tabPanel(tags$a("", href = "https://github.com/PlanktonTeam/BioOceanObserver", target = "_blank", "BioOceanObserver repo")),
                               shiny::tabPanel(tags$a("", href = "https://github.com/PlanktonTeam/planktonr", target = "_blank", "planktonr repo")))
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
      app_title = 'Biological Ocean Observer'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
    # Script for CSIRO branding tab ----
    tags$script(type="text/javascript", src="csirotab.min.js"),
    # Custom CSIRO styling CSS for modal ----
    tags$link(rel = "stylesheet", type = "text/css", href = "css/csiromodal.css"),
    # Google fonts ----
    tags$link(href="https://fonts.googleapis.com/css?family=Open+Sans",
              rel="stylesheet"),
    tags$link(href="https://fonts.googleapis.com/css?family=Montserrat",
              rel="stylesheet")
  )
}

