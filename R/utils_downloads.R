
#' Download Button
#' 
#' @noRd
fButtons <- function(id, button_id, label, Type = "Download") {
  ns <- NS(id)
  
  shiny::tagList(
    if (Type == "Download"){
      shiny::downloadButton(ns(button_id), label = label, 
      )
    } else if (Type == "Action"){
      
      if (stringr::str_detect(id, "Pol")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/EssentialOceanVariables.html')"
      } else if (stringr::str_detect(id, "Micro")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Microbes.html')"
      } else if (stringr::str_detect(id, "Phyto")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Phytoplankton.html')"
      } else if (stringr::str_detect(id, "Zoo")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Zooplankton.html')"
      } else if (stringr::str_detect(id, "LFish")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/LarvalFish.html')"
      } else if (stringr::str_detect(id, "BGC")){
        wsite <- "window.open('https://planktonteam.github.io/planktonr/articles/Biogeochemistry.html')"
      } else {
        wsite <- "window.open('https://planktonteam.github.io/planktonr/index.html')"
      }
      
      shiny::actionButton(ns(button_id), label = label,
                          icon = shiny::icon("file-code"),
                          onclick = wsite)
    }
  )
}


#' Download Data - Server Side
#'
#' @noRd 
fDownloadButtonServer <- function(input, input_dat, gg_prefix) {
  
  downloadData <- shiny::downloadHandler(
    filename = function() {
      
      if (stringr::str_starts(gg_prefix, "Policy")){
        paste0(gg_prefix, "_",  input$site, "_D", format(Sys.time(), "%Y%m%d%H%M%S"), ".csv") %>% 
          stringr::str_replace_all( " ", "")
      } else{
        paste0(gg_prefix, "_", 
               input$parameter, "_",
               data.frame(StationName = input$site) %>% 
                 planktonr::pr_add_StationCode() %>% 
                 dplyr::arrange(.data$StationCode) %>% 
                 dplyr::pull(.data$StationCode) %>% 
                 stringr::str_flatten(), "_",
               lubridate::year(input$DatesSlide[1]), "to", lubridate::year(input$DatesSlide[2]), "_D",
               format(Sys.time(), "%Y%m%d", tz = "Australia/Hobart"), ".csv") %>% 
          stringr::str_replace_all("__", "_") %>%  # Replace any double underscores with single ones
          stringr::str_replace_all( " ", "")
      }
    },
    content = function(file) {
      vroom::vroom_write(input_dat(), file, delim = ",")
    })
  return(downloadData)
}


#' Download Plot - Server Side
#'
#' @noRd
fDownloadPlotServer <- function(input, gg_id, gg_prefix, papersize = "A4r") {
  
  downloadPlot <- downloadHandler(
    filename = function() {
      if ((stringr::str_starts(gg_prefix, "Policy"))){
        paste0(gg_prefix, "_", input$site, "_D", format(Sys.time(), "%Y%m%d%H%M%S"), ".png") %>%
          stringr::str_replace_all( " ", "")
      } else{
        if (gg_prefix == "Scatter"){
          param <- paste0(input$px,"_v_",input$py)
        } else {
          param <- input$parameter
        }
        
        paste0(gg_prefix, "_",
               param, "_",
               data.frame(StationName = input$site) %>%
                 planktonr::pr_add_StationCode() %>%
                 dplyr::arrange(.data$StationCode) %>%
                 dplyr::pull(.data$StationCode) %>%
                 stringr::str_flatten(), "_",
               lubridate::year(input$DatesSlide[1]), "to", lubridate::year(input$DatesSlide[2]), "_D",
               format(Sys.time(), "%Y%m%d", tz = "Australia/Hobart"), ".png") %>%
          stringr::str_replace_all( " ", "")
      }
    },
    content = function(file) {
      # Create copyright statement with current date
      copyright_text <- paste0("\u00A9 IMOS Biological Ocean Observer. Downloaded: ",
                               lubridate::now(tzone = "Australia/Hobart") %>%
                                 lubridate::as_date() %>%
                                 format("%d-%b-%Y"), "")
      
      # Create a minimal plot with just the copyright text
      copyright_plot <- ggplot2::ggplot() +
        ggplot2::annotate("text",
                          x = 0.05,
                          y = 0.0,
                          label = copyright_text,
                          angle = 90,
                          hjust = 0,
                          vjust = 0,
                          size = 4,
                          fontface = "italic") +
        ggplot2::scale_x_continuous(limits = c(0,0.1), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(0,2), expand = c(0, 0)) +
        ggplot2::theme_void() +
        ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
      
      # Wrap the existing plot with copyright on the right, aligned to bottom
      gg_copy <- patchwork::wrap_plots(
        gg_id(),
        patchwork::free(copyright_plot, "space", "b"),
        widths = c(1, 0.02)
      )
      
      # NOTE: I have scaled the plot size to force the font size to be smaller in the downloads.
      sc <- 1.5
      # Save with appropriate dimensions
      
      if (gg_prefix == "Climate"){
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 297*sc, height = 200*sc, units = "mm")
      } else if (papersize == "A4r"){
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 297*sc, units = "mm")
      } else if (papersize == "A4") {
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 210*sc, height = 297*sc, units = "mm")
      } else if (papersize == "A3") {
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 297*sc, height = 420*sc, units = "mm")
      } else if (papersize == "A3r") {
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 420*sc, height = 297*sc, units = "mm")
      } else if (papersize == "A2") {
        ggplot2::ggsave(file, plot = gg_copy, device = "png", dpi = 600, width = 420*sc, height = 594*sc, units = "mm")
      }
      
      ## TODO If we include pdf downloads we can use code like this.
      # cairo_pdf fixes an error with displaying unicode symbols.
      # library(Cairo)
      # file = stringr::str_replace(file, ".png", ".pdf")
      # ggplot2::ggsave(file, plot = gg_id, width = 420, height = 594, units = "mm",
      #                 device = cairo_pdf, family="Arial Unicode MS")
    })
}

