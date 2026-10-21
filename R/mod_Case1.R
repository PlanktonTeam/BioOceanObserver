#' Case Study 1 UI Function
#'
#' @description A shiny Module for Case Study 1.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Case1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidPage(
      #shiny::br(),
      #shiny::h2("Case Study 1 - Noctiluca"),
      #shiny::br(),
      # Wrap your plot inside a clickable action button element

      # Main content container
      tags$div(
        style = "padding: 10px; line-height: 1.6; font-size: 16px; color: #333;",

        # =========================================================================
        # THE FLOATING CONTAINER (Sits in top right corner)
        # =========================================================================
        tags$div(
          style = "
        float: right; 
        width: 600px;           /* Adjust width to control how much space it takes up */
        margin-left: 25px;      /* Creates a buffer zone so text doesn't touch the box */
        margin-bottom: 20px;    /* Buffer zone below the container */
        background-color: #fcfcfc; 
        border: 1px solid #ddd;
        border-radius: 12px; 
        padding: 15px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.05);
      ",

          # The Scrollable Image Box inside the floating container
          tags$div(
            style = "
          display: flex; 
          overflow-x: auto; 
          scroll-snap-type: x mandatory; 
          gap: 15px; 
          padding-bottom: 10px;
          -webkit-overflow-scrolling: touch;
        ",

            tags$figure(
              style = "scroll-snap-align: start; flex-shrink: 0; margin: 0; text-align: center; width: 100%;",
              tags$img(
                src = "www/csm_Zooplankton.png",
                style = "max-height: 250px; border-radius: 6px;"
              ),
              tags$figcaption(
                style = "margin-top: 8px; font-size: 13px; font-weight: bold; color: #555;",
                "Figure 1: Zooplankton"
              )
            ),
            tags$figure(
              style = "scroll-snap-align: start; flex-shrink: 0; margin: 0; text-align: center; width: 100%;",
              tags$img(
                src = "www/csm_Larval_Fish.png",
                style = "max-height: 250px; border-radius: 6px;"
              ),
              tags$figcaption(
                style = "margin-top: 8px; font-size: 13px; font-weight: bold; color: #555;",
                "Figure 1: Larval Fish"
              )
            ),
            tags$figure(
              style = "scroll-snap-align: start; flex-shrink: 0; margin: 0; text-align: center; width: 100%;",
              tags$img(
                src = "www/csm_Phytoplankton.png",
                style = "max-height: 250px; border-radius: 6px;"
              ),
              tags$figcaption(
                style = "margin-top: 8px; font-size: 13px; font-weight: bold; color: #555;",
                "Figure 2: Phytoplankton"
              )
            )
          ),

          # -----------------------------------------------------------------------
          # SIDE-BY-SIDE BUTTON CONTAINER
          # -----------------------------------------------------------------------
          tags$div(
            style = "display: flex; gap: 10px; width: 100%;",

            # Original View Gallery Button
            actionButton(
              inputId = ns("view_images"),
              "Open Image Gallery",
              class = "btn-primary btn-lg"
            ),

            # New Download Image Button
            downloadButton(
              outputId = "download_btn",
              label = "Download",
              class = "btn-success",
              style = "flex: 1;"
            )
          )
        ),

        # =========================================================================
        # THE WRAPPING TEXT (Flows around the floating container)
        # =========================================================================
        tags$h3("Introduction", style = "margin-top: 0;"),

        tags$p(
          "This paragraph starts at the very top of the page. Because the image box container on the right is set to float, 
    this text will fill up the remaining space on the left side of the screen. As you add more details, observations, and data discussions, 
    the sentences will perfectly conform to the dimensions of the container. This layout mimics standard editorial structures commonly 
    seen in professional data reports or news articles."
        ),

        tags$h3("Findings", style = "margin-top: 0;"),

        tags$p(
          "You can discuss the methodology here. For instance, the metrics captured in the figures on the right illustrate our core 
    telemetry data over the last fiscal quarter. Users can interact directly with the card element by scrolling horizontally to see multiple
     plots, or click the action button integrated at the base of the card to pull reports. The fluid layout ensures that everything stays 
     tightly bound together without breaking your typography structure."
        ),

        tags$h3("Implications", style = "margin-top: 0;"),

        tags$p(
          "You can discuss the methodology here. For instance, the metrics captured in the figures on the right illustrate our core 
    telemetry data over the last fiscal quarter. Users can interact directly with the card element by scrolling horizontally to see multiple
     plots, or click the action button integrated at the base of the card to pull reports. The fluid layout ensures that everything stays 
     tightly bound together without breaking your typography structure."
        ),

        tags$h3("References", style = "margin-top: 0;"),

        tags$ul(
          style = "margin-left: 20px; margin-bottom: 15px;", # Indents the bullets nicely

          tags$li("First reference"),
          tags$li("Second reference"),
          tags$li("Third reference"),
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
      showModal(modalDialog(
        title = "Scroll Through Gallery",
        size = "l", # Large size modal window
        easyClose = TRUE, # Click outside to close

        # Custom CSS container allowing smooth, horizontal scrolling for overflow images
        tags$div(
          style = "display: flex; overflow-x: auto; scroll-snap-type: x mandatory; gap: 15px; padding-bottom: 10px;",

          # Add your PNGs here (Make sure they live in your 'www/' folder)
          tags$img(
            src = "www/csm_Zooplankton.png",
            style = "max-height: 450px; scroll-snap-align: start; border-radius: 8px;"
          ),
          tags$img(
            src = "www/csm_Phytoplankton.png",
            style = "max-height: 450px; scroll-snap-align: start; border-radius: 8px;"
          ),
          tags$img(
            src = "www/csm_Larval_Fish.png",
            style = "max-height: 450px; scroll-snap-align: start; border-radius: 8px;"
          )
        ),

        footer = modalButton("Close")
      ))
    })

    # Server logic for handling the download process
    output$download_btn <- downloadHandler(
      filename = function() {
        "metric_figure.png"
      },
      content = function(file) {
        # Path to the actual file on your server/computer
        file.copy("www/image1.png", file)
      }
    )
  })
}
