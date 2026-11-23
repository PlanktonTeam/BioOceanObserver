#' Case Study 1 UI Function
#'
#' @description A shiny Module for Case Study 1.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
imagesList <- c(
  "www/Noctiluca/Picture1.png",
  "www/Noctiluca/Picture2.png",
  "www/Noctiluca/Picture3.png",
  "www/Noctiluca/Picture4.png"
)
captionsList <- list(
  tagList("Figure 1: A ", tags$i("Noctiluca"), "cell"),
  tagList("Figure 2: Showing the changing range of ", tags$i("Noctiluca")),
  tagList("Figure 3: Increasing abundance at Maria Island"),
  tagList("Figure 4: Model output showing dependence on food availability")
)

mod_Case1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidPage(
      tags$div(
        style = "padding: 10px; line-height: 1.6; font-size: 16px; color: #333;",
        #fCaseHeader(tagList("Case Study 1: ", tags$i("Noctiluca"))),
        tags$div(
          #style = "display: flex; gap: 10px; width: 100%;",
          actionButton(
            inputId = ns("view_images"),
            "Open Image Gallery",
            class = "btn-default"
          )
        ),
        fCaseImageBox(
          images = imagesList,
          captions = captionsList
        ),
        shiny::h3("Introduction", style = "margin-top: 0;"),
        shiny::HTML(
          "<i>Noctiluca scintillans</i> is a large dinoflagellate that is conspicuous due to spectacular bioluminescence and red tide events. 
          It has very limited ichthyotoxicity and is not normally considered a harmful algae. <i>Noctiluca</i> is phagotrophic, meaning it 
          gets food by engulfing solid particles, i.e. diatoms, instead of absorbing nutrients or photosynthesising. "
        ),
        shiny::br(),
        shiny::br(),
        shiny::HTML(
          "<i>Noctiluca</i> was first reported in Australia in 1860 in Sydney Harbour and despite extensive sampling through 1950 - 1980s 
        <i>Noctiluca</i> was only found in a few locations.Therefore, before the 1990s, <i>Noctiluca</i> appeared to be a rare and ephemeral species in 
        warmer waters of Australia."
        ),
        shiny::h3("Findings", style = "margin-top: 0;"),
        shiny::HTML(
          "In the 1990s, <i>Noctiluca</i> expanded its distribution into southern NSW and Moreton Bay. It was first observed in Tasmania in 1994, 
          apparently carried by the East Australian Current, and in 2013 the SOCPR survey recorded the most southerly record globally at 45<sup>o</sup>S."
        ),
        shiny::br(),
        shiny::br(),
        shiny::HTML(
          "<i>Noctiluca</i> has now established overwintering populations in Tasmania as shown by a timeseries of abundances from Maria Island 
        National Reference Station, although abundances through the winter months remain low."
        ),
        shiny::h3("Implications", style = "margin-top: 0;"),
        shiny::HTML(
          "While <i>Noctiluca</i> blooms are largely driven by prey availability, these seasonal responses indicate that temperature plays an important 
          role in distribution. Temperature is an important driver but <i>Noctiluca's</i> preferred temperature range ( ~10 - 23<sup>o</sup>C) encompasses 
          Australia’s temperate climate. While <i>Noctiluca</i> in Tasmania favour summer temperatures, improved food availability is likely 
          to maintain its occurrence throughout the rest of the year.   "
        ),
        shiny::HTML(
          "<h3>Further reading</h3>
          <ul>
          <li>McLeod, D. J., Hallegraeff, G. M., Hosie, G. W., & Richardson, A. J. (2012). Climate-driven range expansion of the red-tide dinoflagellate <i>Noctiluca scintillans</i> into the Southern Ocean. Journal of Plankton Research, 34(4), 332-337. <a href = http://dx.doi.org/10.1093/plankt/fbr112 target = _blank> Website</a>.</li>
            <li>Hallegraeff G, Davies C, Rochester W. (2020) Range expansion of the red tide dinoflagellate Noctiluca scintillans. In Richardson A.J, Eriksen R, Moltmann T, Hodgson-Johnston I, Wallis J.R. (Eds). State and Trends of Australia’s Ocean Report. <a href = http://dx.doi.org/10.26198/5e16ad2e49e85 target = _blank> Website</a>.</li>
            <li>Hallegraeff, G. M., Albinsson, L., Dowdney, J., Holmes, A. K., Mansour, P., & Seger, A. (2019). Prey selection, environmental tolerances and ichthyotoxicity by the red-tide dinoflagellate Noctiluca scintillans cultured from Tasmanian waters. Journal of Plankton Research, 41, 407-418. <a href = https://doi.org/10.1093/plankt/fbz037 target = _blank> Website</a>.</li>
            </ul>"
        )
      )
    )
  )
}

#' Case Study 1 Server Function
#'
#' @noRd
mod_Case1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
   
   observeEvent(input$view_images, {
      fShowImageGallery(imagesList, captions = captionsList)
    })

  })
}
