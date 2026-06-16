#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  MAPBOX_PUBLIC_TOKEN = "pk.eyJ1IjoiamFzZWV2ZXJldHQiLCJhIjoiY21tdHA1dDllMDJ3czJwcTdrcDU5OG93aiJ9.OO3DjqVgqcDL5Etpgk_qzg",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(MAPBOX_PUBLIC_TOKEN = MAPBOX_PUBLIC_TOKEN)
  )
}

