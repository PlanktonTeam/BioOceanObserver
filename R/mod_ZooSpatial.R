#' ZooSpatial UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZooSpatial_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      fSpatialSidebar(id = id, dat1 = pkg.env$fMapDataz),
      fSpatialPanel(id = id)
    )
  )
}

#' ZooSpatial Server Functions
#'
#' @noRd 
mod_ZooSpatial_server <- function(id){
  moduleServer(id, function(input, output, session){

    # ── Reactive: species-filtered data (all seasons) ──────────────────────────
    # Returns only rows for the selected species with freqfac re-levelled to
    # include "Absent". The absence background dots come from pkg.env$fMapDataz
    # inside MapboxSeason() itself — we must NOT add absent rows here or they
    # will appear in the presence layer coloured grey.
    ZSdatar <- reactive({
      req(input$species)
      shiny::validate(need(!is.na(input$species), "Error: Please select a species"))

      pkg.env$fMapDataz %>%
        dplyr::filter(.data$Species == input$species) %>%
        dplyr::mutate(freqfac = factor(
          .data$freqfac,
          levels = c("Absent", "Seen in 25%", "50%", "75%", "100% of Samples")
        ))
    }) %>% bindCache(input$species)

    shiny::exportTestValues(
      ZooSpatial            = { ncol(ZSdatar()) },
      ZooSpatialRows        = { nrow(ZSdatar()) > 0 },
      ZooSpatialLatisNumeric  = { class(ZSdatar()$Lat) },
      ZooSpatialLongisNumeric = { class(ZSdatar()$Long) },
      ZooSpatialFreqisFactor  = { class(ZSdatar()$Freqfac) },
      ZooSpatialSeasonisChr   = { class(ZSdatar()$Season) }
    )

    # ── Derived inputs ─────────────────────────────────────────────────────────
    # Map type: FALSE = PA, TRUE = frequency
    map_type <- reactive({
      if (isTRUE(input$scaler1)) "frequency" else "PA"
    })

    # Season label from radio button (defaults to "December - February")
    season_label <- reactive({
      req(input$season)
      input$season
    })

    # ── Initial full render ────────────────────────────────────────────────────
    # Full re-render when species or map type changes. Season changes are handled
    # by the proxy observer below to avoid rebuilding the entire map.
    # bindCache() on species + scaler1 + season means switching back to a
    # previously viewed combination is instant.
    output$MapSeason <- mapgl::renderMapboxgl({
      req(input$species)
      MapboxSeason(
        df_abs       = pkg.env$fMapDataz,
        df_pres      = ZSdatar(),
        season_label = season_label(),
        Type         = map_type()
      )
    }) %>% bindCache(input$species, input$scaler1, input$season)

    # ── Proxy update: season or type change ───────────────────────────────────
    # When only the season radio button or the PA/frequency checkbox changes,
    # update the two presence layers and the legend without rebuilding the map.
    # ignoreInit = TRUE prevents a double-render on first load (renderMapboxgl
    # already draws the correct season on initialisation).
    observeEvent(
      list(input$season, input$scaler1),
      {
        req(input$species, input$season)
        MapboxSeasonProxy(
          df_abs       = pkg.env$fMapDataz,
          df_pres      = ZSdatar(),
          season_label = season_label(),
          Type         = map_type(),
          map_id       = "MapSeason",
          session      = session
        )
      },
      ignoreInit = TRUE
    )

    # ── STI plot ───────────────────────────────────────────────────────────────
    selectedSTI <- reactive({
      req(input$species)
      shiny::validate(need(!is.na(input$species), "Error: Please select a species"))
      pkg.env$stiz %>% dplyr::filter(.data$Species %in% input$species)
    }) %>% bindCache(input$species)

    # Plot is blank (not a grey error box) when there is no data; the warning
    # message and species list are rendered above it by output$STISpeciesList.
    output$STIs <- renderPlot({
      req(nrow(selectedSTI()) > 0)
      planktonr::pr_plot_STI(selectedSTI())
    }) %>% bindCache(input$species)

    # Warning message + 3-column species list shown above the plot when the
    # selected species has no STI data. Returns NULL when data exist.
    output$STISpeciesList <- shiny::renderUI({
      req(input$species)
      if (nrow(selectedSTI()) > 0) return(NULL)
      shiny::p(
        shiny::strong(paste0(
          "No Species Temperature Index data available for '",
          input$species, "'."
        ))
      )
    })

    # ── Diurnal behaviour plot ─────────────────────────────────────────────────
    selecteddn <- reactive({
      req(input$species)
      shiny::validate(need(!is.na(input$species), "Error: Please select a species"))
      pkg.env$daynightz %>% dplyr::filter(.data$Species %in% input$species)
    }) %>% bindCache(input$species)

    output$DNs <- renderPlot({
      req(nrow(selecteddn()) > 0)
      planktonr::pr_plot_DayNight(selecteddn())
    }) %>% bindCache(input$species)

    # Warning message + 3-column species list shown above the plot when the
    # selected species has no diurnal data. Returns NULL when data exist.
    output$DNSpeciesList <- shiny::renderUI({
      req(input$species)
      if (nrow(selecteddn()) > 0) return(NULL)
      shiny::p(
        shiny::strong(paste0(
          "No diurnal behaviour data available for '",
          input$species, "'."
        ))
      )
    })

  })
}
