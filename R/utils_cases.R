#' Create a scrollable image box for case studies
#' @param images A character vector of image paths
#' @param captions A character vector of captions (same length as images)
#' @return A tagList for the image box
fCaseImageBox <- function(images, captions) {
  stopifnot(length(images) == length(captions))
  tags$div(
    class = "case-image-box",
    tags$div(
      class = "case-image-scroll",
      lapply(seq_along(images), function(i) {
        tags$figure(
          class = "case-figure",
          tags$img(src = images[[i]], class = "case-img"),
          tags$figcaption(class = "case-caption", captions[[i]])
        )
      })
    )
  )
}

#' Standard h3 header for case studies
#' @param text The header text
fCaseHeader <- function(...) {
  tags$h3(class = "case-header", ...)
}

#' Show a scrollable modal gallery of images
#' @param images Character vector of image paths
#' @param title Modal title
fShowImageGallery <- function(
  images,
  captions = NULL,
  title = "Scroll Through Gallery"
) {
  showModal(modalDialog(
    title = NULL,
    size = "l",
    easyClose = TRUE,
tags$style(HTML("
  .modal-dialog .modal-content .modal-header {
    display: none !important;
    background: transparent !important;
    border: none !important;
    box-shadow: none !important;
    height: 0 !important;
    min-height: 0 !important;
    padding: 0 !important;
    margin: 0 !important;
  }
")),
    tags$div(
      style = paste(
        "display: flex;",
        "overflow-x: auto;",
        "overflow-y: hidden;",
        "scroll-snap-type: x mandatory;",
        "gap: 0;",
        "height: 45vh;",
        "width: 100%;",
        "max-width: 80vw;",
        "align-items: center;"
      ),
      purrr::map2(
        images,
        captions %||% rep("", length(images)),
        ~ tags$div(
          style = paste(
            "flex: 0 0 100%;",
            "height: 100%;",
            "display: flex;",
            "flex-direction: column;",
            "justify-content: center;",
            "align-items: center;",
            "scroll-snap-align: start;"
          ),
          tags$img(
            src = .x,
            style = paste(
              "max-width: 100%;",
              "max-height: 85%;",
              "width: auto;",
              "height: auto;",
              "margin: auto;"
            )
          ),
          tags$div(class = "modal-caption", .y)
        )
      )
    ),
    footer = modalButton("Close")
  ))
}
