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
  
  # Top-level navbar tabs
  mod_home_server("home_1")
  mod_PolNRS_server("PolNRS_ui_1")
  mod_MicroTsNRS_server("MicroTsNRS_ui_1")
  mod_PhytoTsNRS_server("PhytoTsNRS_ui_1")
  mod_PhytoTsHAB_server("PhytoTsHAB_ui_1")
  mod_ZooTsNRS_server("ZooTsNRS_ui_1")
  mod_LFishSpatial_server("LFishSpatial_1")
  mod_NutrientsBGC_server("NutrientsBGC_ui_1")
  mod_RelNRS_server("RelNRS_ui_1")
  mod_info_server("info_1")
  
  # EOVs sub-tabs
  mod_PolCPR_server("PolCPR_ui_1")
  mod_PolLTM_server("PolLTM_ui_1")
  mod_PolSOTS_server("PolSOTS_ui_1")
  
  # Phytoplankton sub-tabs
  mod_PhytoTsCPR_server("PhytoTsCPR_ui_1")
  mod_PhytoSpatial_server("PhytoSpatial_ui_1")
  
  # Zooplankton sub-tabs
  mod_ZooTsCPR_server("ZooTsCPR_ui_1")
  mod_ZooSpatial_server("ZooSpatial_ui_1")
  
  # Microbes sub-tabs
  mod_MicroTsCS_server("MicroTsCS_ui_1")
  mod_MicroLatGS_server("MicroLatGS_ui_1")
  
  # Larval Fish sub-tabs
  mod_LFishSeason_server("LFishSeason_1")
  mod_LFishData_server("LFishData_1")
  
  # Environmental sub-tabs
  mod_PigmentsBGC_server("PigmentsBGC_ui_1")
  mod_PicoBGC_server("PicoBGC_ui_1")
  mod_WaterBGC_server("WaterBGC_ui_1")
  mod_MoorBGC_server("MoorBGC_ui_1")
  
  # Relationships sub-tabs
  mod_RelCS_server("RelCS_ui_1")
  mod_RelCPR_server("RelCPR_ui_1")

  # Animal Tracking sub-tabs
  mod_ATSpatial_server("ATSpatial_1")
  mod_ATStats_server("ATStats_1")
  mod_ATCases_server("ATCases_1")
}
