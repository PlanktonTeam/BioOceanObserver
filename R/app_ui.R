#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @noRd

# Helper: full-width section banner shown below the navbar ----------------
section_banner <- function(label) {
  shiny::div(
    class = "section-banner",
    shiny::h2(label)
  )
}

app_ui <- function(request) {
  # Your application UI logic
  bslib::page_navbar(
    id = "navbar",
    title = shiny::div(
      shiny::img(src = "www/IMOS_logo-wide-_Colour.png", height = 30),
      shiny::div("Biological Ocean Observer", class = "brand-text")),
    window_title = "Biological Ocean Observer",
    header = golem_add_external_resources(), # Add external resources in header
    theme = bslib::bs_theme(version = 5,
                            bootswatch = "flatly",
                            "border-width" = "0px",
                            "enable-rounded" = TRUE),
    selected = "Home",

    # ── 1. Home ──────────────────────────────────────────────────
    bslib::nav_panel("Home",
      mod_home_ui("home_1")
    ),

    # ── 2. Environmental Reporting ───────────────────────────────
    bslib::nav_menu(
      title = "Environmental Reporting",
      bslib::nav_panel("National Reference Stations",
        shiny::tagList(section_banner("National Reference Stations"), mod_PolNRS_ui("PolNRS_ui_1"))
      ),
      bslib::nav_panel("Marine Bioregions (CPR)",
        shiny::tagList(section_banner("Marine Bioregions (CPR)"), mod_PolCPR_ui("PolCPR_ui_1"))
      ),
      bslib::nav_panel("Long Term Monitoring ",
        shiny::tagList(section_banner("Long Term Monitoring"), mod_PolLTM_ui("PolLTM_ui_1"))
      ),
      bslib::nav_panel("Southern Ocean Time Series",
        shiny::tagList(section_banner("Southern Ocean Time Series"), mod_PolSOTS_ui("PolSOTS_ui_1"))
      )
    ),

    # ── 3. Biology ───────────────────────────────────────────────
    bslib::nav_menu(
      title = "Biology",
      bslib::nav_panel("Microbes",
        shiny::tagList(
          section_banner("Microbes"),
          bslib::navset_pill(id = "mic",
            bslib::nav_panel("NRS Time Series",     value = "mts",   mod_MicroTsNRS_ui("MicroTsNRS_ui_1")),
            bslib::nav_panel("Coastal Time Series", value = "mtsCS", mod_MicroTsCS_ui("MicroTsCS_ui_1")),
            bslib::nav_panel("Voyage Data",         value = "GSlat", mod_MicroLatGS_ui("MicroLatGS_ui_1"))
          )
        )
      ),
      bslib::nav_panel("Phytoplankton",
        shiny::tagList(
          section_banner("Phytoplankton"),
          bslib::navset_pill(id = "phyto",
            bslib::nav_panel("NRS Time Series",    value = "pts",    mod_PhytoTsNRS_ui("PhytoTsNRS_ui_1")),
            bslib::nav_panel("CPR Time Series",    value = "ptscpr", mod_PhytoTsCPR_ui("PhytoTsCPR_ui_1")),
            bslib::nav_panel("Coastal HABs",       value = "phab",   mod_PhytoTsHAB_ui("PhytoTsHAB_ui_1")),
            bslib::nav_panel("Species Information",value = "distp",  mod_PhytoSpatial_ui("PhytoSpatial_ui_1"))
          )
        )
      ),
      bslib::nav_panel("Zooplankton",
        shiny::tagList(
          section_banner("Zooplankton"),
          bslib::navset_pill(id = "zoo",
            bslib::nav_panel("NRS Time Series",    value = "zts",    mod_ZooTsNRS_ui("ZooTsNRS_ui_1")),
            bslib::nav_panel("CPR Time Series",    value = "ztscpr", mod_ZooTsCPR_ui("ZooTsCPR_ui_1")),
            bslib::nav_panel("Species Information",value = "dist",   mod_ZooSpatial_ui("ZooSpatial_ui_1"))
          )
        )
      ),
      bslib::nav_panel("Larval Fish",
        shiny::tagList(
          section_banner("Larval Fish"),
          bslib::navset_pill(id = "fish",
            bslib::nav_panel("Spatial",      value = "fspat", mod_LFishSpatial_ui("LFishSpatial_1")),
            bslib::nav_panel("Seasonal",     value = "fseas", mod_LFishSeason_ui("LFishSeason_1")),
            bslib::nav_panel("Species Data", value = "fdata", mod_LFishData_ui("LFishData_1"))
          )
        )
      ),
      bslib::nav_panel("Animal Tracking",
        shiny::tagList(
          section_banner("Animal Tracking"),
          bslib::navset_pill(id = "AT",
            bslib::nav_panel("Spatial",    value = "atspat",  mod_ATSpatial_ui("ATSpatial_1")),
            bslib::nav_panel("Statistics", value = "atstats", mod_ATStats_ui("ATStats_1"))
          )
        )
      )
    ),

    # ── 4. Biogeochemistry ───────────────────────────────────────
    bslib::nav_menu(
      title = "Biogeochemistry",
      bslib::nav_panel("Nutrients (NRS)",
        shiny::tagList(section_banner("Nutrients (NRS)"), mod_NutrientsBGC_ui("NutrientsBGC_ui_1"))
      ),
      bslib::nav_panel("Picoplankton (NRS)",
        shiny::tagList(section_banner("Picoplankton (NRS)"), mod_PicoBGC_ui("PicoBGC_ui_1"))
      ),
      bslib::nav_panel("Pigments (NRS)",
        shiny::tagList(section_banner("Pigments (NRS)"), mod_PigmentsBGC_ui("PigmentsBGC_ui_1"))
      ),
      bslib::nav_panel("CTD Profiles (NRS)",
        shiny::tagList(section_banner("CTD Profiles (NRS)"), mod_WaterBGC_ui("WaterBGC_ui_1"))
      )
    ),

    # ── 5. Data Analysis ─────────────────────────────────────────
    bslib::nav_menu(
      title = "Data Analytics",
      bslib::nav_panel("NRS Relationships",
        shiny::tagList(section_banner("NRS Relationships"), mod_RelNRS_ui("RelNRS_ui_1"))
      ),
      bslib::nav_panel("Coastal Stations Relationships",
        shiny::tagList(section_banner("Coastal Stations Relationships"), mod_RelCS_ui("RelCS_ui_1"))
      ),
      bslib::nav_panel("CPR Relationships",
        shiny::tagList(section_banner("CPR Relationships"), mod_RelCPR_ui("RelCPR_ui_1"))
      )
    ),

    # ── 6. Case Studies ──────────────────────────────────────────
    bslib::nav_menu(
      title = "Case Studies",
      bslib::nav_panel("Case Study 1",
        shiny::tagList(section_banner("Case Study 1"), mod_Case1_ui("Case1_1"))
      )
    ),

    # ── 7. GitHub menu (icon only, right-aligned) ─────────────────
    bslib::nav_menu(
      title = shiny::icon("github"),
      align = "right",
      bslib::nav_item(
        shiny::tags$a(
          href = "https://github.com/PlanktonTeam/BioOceanObserver",
          target = "_blank",
          "BioOceanObserver Repository"
        )
      ),
      bslib::nav_item(
        shiny::tags$a(
          href = "https://github.com/PlanktonTeam/planktonr",
          target = "_blank",
          shiny::tags$em("planktonr"), " Repository"
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  shiny::tags$head(
    # script for accessning google analytics from csiro server
    if( Sys.getenv('SHINY_PORT') != "" ) {
      shiny::includeHTML(app_sys('app/www/GoogleAnalytics.html'))
    },
    
    favicon(),    
    
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Biological Ocean Observer'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    
    # Script for navigating between modules ----
    tags$script(type="text/javascript", src="navigate.js"),
    # Script for custom Shiny input handlers (e.g. bslib tab binding fix) ----
    tags$script(type="text/javascript", src="handlers.js"),
    # Script for CSIRO branding tab ----
    shiny::tags$script(type="text/javascript", src="csirotab.min.js"),
    # Custom CSIRO styling CSS for modal ----
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "csiromodal.css"),
    # IMOS Custom CSS with typography and colors ----
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    # Google fonts - Poppins and Open Sans ----
    shiny::tags$link(href="https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&family=Open+Sans:wght@300;400;500;600;700&display=swap",
                     rel="stylesheet")
  )
}
