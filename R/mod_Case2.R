#' Case Study 2 UI Function
#'
#' @description A shiny Module for Case Study 2.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_Case2_ui <- function(id) {
  ns <- NS(id)
  case_data <- fImagesCaptions("Tricho")
  imagesList <- case_data$imagesList
  captionsList <- case_data$captionsList
  tagList(
    shiny::fluidPage(
      tags$div(
        style = "padding: 10px; line-height: 1.6; font-size: 16px; color: #333;",
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
          "The filamentous cyanobacteria, genus <i>Trichodesmium</i>, is a marine diazotroph that converts atmospheric nitrogen into more bioavailable 
          forms and is therefore important in nutrient cycling in oligotrophic marine ecosystems such as the Great Barrier Reef. "
        ),
        shiny::br(),
        shiny::br(),
        shiny::HTML(
          "Studies using satellite images of Trichodesmium slicks have concluded that <i>Trichodesmium</i> is increasing in the North East of Australia.
          Long term IMOS plankton monitoring in the region from the National Reference Stations (NRS) and the Continuous Plankton Recorder survey (AusCPR)
          confirm that <i>Trichodesmium</i> abundance has increased over the last 16 years. "
        ),
        shiny::h3("Findings", style = "margin-top: 0;"),
        shiny::HTML(
          "At the Yongala National Reference Station <i>Trichodesmium</i> abundance has increased by approximately 5% each year over the past 16 years. 
          This finding is supported by data from the Continuous Plankton Recorder across the Coral Sea bioregion. Warmer sea temperatures are the strongest 
          drivers of increasing <i>Trichodesmium</i>, with lower phosphate and higher chlorophyl also associated with higher abundances. "
        ),
        shiny::br(),
        shiny::br(),
        shiny::HTML(
          "Genetic sampling determined that there are two dominant species in Australia, <i>T. erythraeum</i> and <i>T. thiebautii</i>. This data shows that 
          <i>T. thiebautii</i> is more dominant in the Great Barrier Reef region and along the East coast of Australia, with <i>T. erythraeum</i> being more 
          dominant on the West coast. <i>T. thiebautii</i> has a temperature niche of 26<sup>o</sup>C in Australia and <i>T. erythraeum</i> 22<sup>o</sup>C. 
          This is interesting because lab experiments indicate that <i>T. erythraeum</i> grows better in warmer temperatures, 24-28<sup>o</sup>C, however it 
          is relatively more abundant at Rottnest Island where the tmperatures range from ~18-24<sup>o</sup>C. So whilst we know that temperature is an important
           driver for high abundances, there are other factors that make conditions acceptable for <i>T. erythraeum</i> in cooler waters. "
        ),
        shiny::h3("Implications", style = "margin-top: 0;"),
        shiny::HTML(
          "Conditions in the GBR are conducive to strong <i>Trichodesmium</i> growth and nitrogen fixation, with warm, low nutrient waters, a low N:P ratio, good 
          light availability, and regular input of the dust (iron) that is required for growth from desert storms. Estimates from eReefs modelling put 
          <i>Trichodesmium’s</i> contribution to the GBR at approximately 0.5 million tonnes of new nitrogen a year, compared with average annual riverine 
          nitrogen inputs of 0.05–0.08 million tonnes. Increases in <i>Trichodesmium</i> in this oligotrophic region may be benficial to the nitrogen cycle. "
        ),
        shiny::br(),
        shiny::br(),
        shiny::HTML(
          "But more is not necessarily better. Large blooms can reduce 
          the light available to other photosynthetic organisms and deplete oxygen. They can also feed into the reef’s complex food web: laboratory experiments 
          show that crown-of-thorns starfish larvae eat <i>Trichodesmium</i>. and so increases in <i>Trichodesmium</i> could impact on CoTs numbers. However, 
          <i>Trichodesmium</i> is also grazed on by <i>Macrosetella gracilis<i/>, a harpacticoid copepod, which relies on <i>Trichodesmium</i> for food and habitat. "
        ),
        shiny::HTML(
          "<h3>Further reading</h3>
          <ul>
          <li>Davies, C. H., A. Bissett, F. Coman, F. R. McEnnulty, R. S. Eriksen, A. Slotwinski, M. Tonks, S. A. Jayasinghe, J. Uribe-Palomino, J. v. d. Kamp, M. Smith and A. J. Richardson (2026). Increasing abundance of nitrogen-fixing <i>Trichodesmium</i> in Australian waters. Marine and Freshwater Research 77(15). <a href = https://doi.org/26010.21071/MF26041 target = _blank> Website</a>.</li>
            <li>Davies C, Eriksen R, Richardson A.J. (2020) Spatial and Seasonal trends in Trichodesmium. In Richardson A.J, Eriksen R, Moltmann T, Hodgson-Johnston I, Wallis J.R. (Eds). State and Trends of Australia’s Ocean Report. <a href = http://dx.doi.org/10.26198/5e16abb949e81 target = _blank> Website</a>.</li>
            <li>Blondeau-Patissier, D., Brando, V. E., Lønborg, C., Leahy, S. M., & Dekker, A. G. (2018). Phenology of Trichodesmium spp. blooms in the Great Barrier Reef lagoon, Australia, from the ESA-MERIS 10-year mission. PLoS ONE, 13(12). <a href = https://doi.org/10.1371/journal.pone.0208010 target = _blank> Website</a>.</li>
            </ul>"
        )
      )
    )
  )
}

#' Case Study 1 Server Function
#'
#' @noRd
mod_Case2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
   
   observeEvent(input$view_images, {
      fShowImageGallery(imagesList, captions = captionsList)
    })

  })
}
