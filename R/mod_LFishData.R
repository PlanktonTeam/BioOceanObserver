#' LFishData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_LFishData_ui <- function(id){
  nsLFishData <- NS(id)
  tagList(
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(width = 6, offset = 6,
                      selectizeInput(inputId = nsLFishData("species"), label = NULL, 
                                     choices = unique(pkg.env$LFData$Species), width = "100%",
                                     options = list(dropdownParent = 'body')),
        )),
      shiny::fluidRow(
        DT::DTOutput(nsLFishData("SpeciesTable")),
      ),
      
      div(style="display:inline-block; float:right; width:60%",
          fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"),
          fButtons(id, button_id = "downloadCode1", label = "Code", Type = "Action"))
      
    )
  )
}

#' LFishData Server Functions
#'
#' @noRd 
mod_LFishData_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    SpeciesTabler <- shiny::reactive({
      
      dat <- pkg.env$LFData %>% 
        dplyr::filter(input$species == .data$Species) %>%
        dplyr::arrange(.data$SampleTime_Local) %>% 
        dplyr::select(c("Project", "Latitude", "Longitude", "SampleTime_Local", "SampleDepth_m", "Count",
                        "Abundance_1000m3", "Temperature_degC", "Salinity_psu", "Volume_m3", "Vessel", "TowType",
                        "GearMesh_um", "Bathymetry_m")) %>% 
        dplyr::mutate(Temperature_degC = round(.data$Temperature_degC, digits = 1),
                      Salinity_psu = round(.data$Salinity_psu, digits = 2),
                      Abundance_1000m3 = round(.data$Abundance_1000m3, digits = 4),
                      Latitude = round(.data$Latitude, digits = 2),
                      Longitude = round(.data$Longitude, digits = 2),
                      Volume_m3 = round(.data$Volume_m3),
                      Vessel = as.factor(.data$Vessel),
                      Project = as.factor(.data$Project),
                      TowType = as.factor(.data$TowType),
                      GearMesh_um = as.factor(.data$GearMesh_um)) %>% 
        dplyr::rename("SampleTime (Local)" = "SampleTime_Local",
                      "Sample Depth (m)" = "SampleDepth_m",
                      "Abundance (1000 m\u00b3)" = "Abundance_1000m3",
                      "Temperature (\U00B0 C)" = "Temperature_degC",
                      "Salinity" = "Salinity_psu",
                      "Volume (m\u00b3)" = "Volume_m3",
                      "Tow Type" = "TowType",
                      "Gear-Mesh (\u00b5m)" = "GearMesh_um",
                      "Bathymetry (m)" = "Bathymetry_m")
      
      return(dat)
      
    }) %>% bindCache(input$species)
    
    
    output$SpeciesTable <- DT::renderDT(
      SpeciesTabler(),
      filter = "top",
      options = list(
        pageLength = 100))
    
    
    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, SpeciesTabler, 
                                                  stringr::str_remove_all(input$species, ":|\\(|\\)") %>% 
                                                    stringr::str_replace_all(" ", "_")) # Download csv of data
    
  })
  
}

## To be copied in the UI
# mod_LFishData_ui("LFishData_1")

## To be copied in the server
# mod_LFishData_server("LFishData_1")
