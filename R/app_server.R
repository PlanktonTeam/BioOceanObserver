#' The application server-side
#' 
#' @param input,output,session Internal Parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny patchwork
#' @importFrom rlang .data
#' @noRd
#' 

app_server <- function( input, output, session ) {
  # Your application server logic 
  
  ## global options and themes can go here
  # ggplot2::theme_set(ggplot2::theme_bw(base_size = 12) + ggplot2::theme(legend.position = "bottom"))
  options(na.action = "na.omit")
  
  # Informative pop-up modal; advises that DAP is inaccessible and historical data is being used.
  # new.data is defined in 000.R which is sourced first (alphabetical sorting)
  if (!pkg.env$new.data) {
    
    showModal(modalDialog(
      title = HTML(paste0('<span class="modal-logo-header">',
                          '<img class="logo modal-logo-img" src="https://www.csiro.au/~/media/Web-team/Images/CSIRO_Logo/logo.png" alt="CSIRO logo"></img></span>',
                          '<p class="modal-title-text">CSIRO Data Access Portal</p>')),
      HTML(paste0("<p>The IMOS data that populates the Biological Ocean Observer is sourced from a ",
                  "<a href = https://data.csiro.au/collection/csiro:54520>CSIRO Data Access Portal collection</a>",
                  " that cannot be reached at this time.</p>",
                  '<span class="modal-emphasis-text">Historical IMOS data is currently visualised on this site.</span>')),
      size = "m"
    ))
    
  }
  
  # Register all module servers once at startup.
  # Computation is deferred inside each module via req() gates on tab inputs.
  
  # ── Home ──────────────────────────────────────────────────────
  # Info content (Technical Information, References, Sampling Details,
  # Species Details) is inlined directly into mod_home_server — no
  # separate mod_info_server call needed.
  mod_home_server("home_1")

  # ── Environmental Reporting ───────────────────────────────────
  mod_PolNRS_server("PolNRS_ui_1")
  mod_PolCPR_server("PolCPR_ui_1")
  mod_PolLTM_server("PolLTM_ui_1")
  mod_PolSOTS_server("PolSOTS_ui_1")

  # ── Biology: Microbes ─────────────────────────────────────────
  mod_MicroTsNRS_server("MicroTsNRS_ui_1")
  mod_MicroTsCS_server("MicroTsCS_ui_1")
  mod_MicroLatGS_server("MicroLatGS_ui_1")

  # ── Biology: Phytoplankton (incl. Coastal HAB) ────────────────
  mod_PhytoTsNRS_server("PhytoTsNRS_ui_1")
  mod_PhytoTsCPR_server("PhytoTsCPR_ui_1")
  mod_PhytoSpatial_server("PhytoSpatial_ui_1")
  mod_PhytoTsHAB_server("PhytoTsHAB_ui_1")

  # ── Biology: Zooplankton ──────────────────────────────────────
  mod_ZooTsNRS_server("ZooTsNRS_ui_1")
  mod_ZooTsCPR_server("ZooTsCPR_ui_1")
  mod_ZooSpatial_server("ZooSpatial_ui_1")

  # ── Biology: Larval Fish ──────────────────────────────────────
  mod_LFishSpatial_server("LFishSpatial_1")
  mod_LFishSeason_server("LFishSeason_1")
  mod_LFishData_server("LFishData_1")

  # ── Biology: Animal Tracking ──────────────────────────────────
  mod_ATSpatial_server("ATSpatial_1")
  mod_ATStats_server("ATStats_1")

  # ── Biogeochemistry ───────────────────────────────────────────
  mod_NutrientsBGC_server("NutrientsBGC_ui_1")
  mod_PicoBGC_server("PicoBGC_ui_1")
  mod_PigmentsBGC_server("PigmentsBGC_ui_1")
  mod_WaterBGC_server("WaterBGC_ui_1")
  # mod_MoorBGC_server("MoorBGC_ui_1")  # No matching UI — kept commented until Moorings tab is restored

  # ── Data Analysis ─────────────────────────────────────────────
  mod_RelNRS_server("RelNRS_ui_1")
  mod_RelCS_server("RelCS_ui_1")
  mod_RelCPR_server("RelCPR_ui_1")

  # ── Case Studies ──────────────────────────────────────────────
  mod_Case1_server("Case1_1")
}
