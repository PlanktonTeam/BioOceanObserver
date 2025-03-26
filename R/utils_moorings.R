
pr_get_MoorClimPlotData <- function(df, Station, noYear){ #TODO Move to planktonr
  
  # browser()
  
  df <- tibble::tibble(SampleDate = seq.Date(to = lubridate::ceiling_date(Sys.Date(), "year"),
                                         from = lubridate::ceiling_date(Sys.Date() - lubridate::years(noYear), "year"),
                                         by = "day")) %>% 
    dplyr::mutate(TIME = lubridate::yday(.data$SampleDate),
                  year = lubridate::year(.data$SampleDate)) %>% 
    dplyr::right_join(df %>% dplyr::filter(.data$StationName %in% Station), by = "TIME") %>% 
    unique()
    
    return(df)
}

pr_plot_MoorClim <- function(df){ #TODO Move to planktonr
  # years <- paste0(unique(df$year), "-01")
  # noYears <- length(years) - 1
  # df$seq <- c(rep(1:(365 * noYears), each = ((max(df$DEPTH) + 1))), rep((365 * noYears) + 1, (max(df$DEPTH) + 1)))
  # labbreak <- c(1, (1 + (max(df$TIME)) * 1),
  #               (1 + (max(df$TIME)) * 2),
  #               (1 + (max(df$TIME)) * 3),
  #               (1 + (max(df$TIME)) * 4),
  #               (1 +(max(df$TIME)) * 5))
  legtit <- planktonr::pr_relabel("Temperature_degC", style = 'ggplot')
  
  climtsplot <- ggplot2::ggplot(df) +
    ggplot2::geom_raster(ggplot2::aes(x = .data$SampleDate, y = .data$DEPTH, fill = .data$CLIM), interpolate = TRUE) +
    ggplot2::scale_fill_viridis_c(option = 'plasma', name = legtit) +
    ggplot2::scale_color_viridis_c(option = 'plasma', name = legtit) +
    ggplot2::facet_wrap(~ .data$StationName, scales = 'free', ncol = 1) +
    ggplot2::scale_y_reverse(expand=c(0,0)) +
    #ggplot2::scale_x_continuous(breaks = labbreak, labels = years, expand=c(0,0)) +
    ggplot2::scale_x_date(breaks = '1 year', expand=c(0,0), date_labels = "%Y") +
    ggplot2::labs(x = "Years", y = "Depth (m)") +
    ggplot2::theme_bw(base_size = 16) + 
    ggplot2::theme(legend.position = 'bottom',
                   strip.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank())
  
  return(climtsplot)
}

pr_get_MoorTSPlotData <- function(df, Station, noYear){
  df <- data.frame(SampleDate = seq.Date(to = lubridate::ceiling_date(Sys.Date(), "year"),
                                         from = lubridate::ceiling_date(Sys.Date() - lubridate::years(noYear), "year"),
                                         by = "day")) %>% 
    dplyr::mutate(DOY = lubridate::yday(.data$SampleDate) + 10956) %>% 
    dplyr::inner_join(df %>% dplyr::filter(.data$StationName %in% Station), by = 'DOY') %>%
    dplyr::select(-"DOY") %>%
    tidyr::pivot_wider(id_cols = c(.data$SampleDate, .data$StationName, .data$StationCode), names_from = 'Names', values_from = 'CLIM') 
}

pr_plot_MoorTS <- function(df){
  
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = .data$SampleDate)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Surface, colour = 'Surface')) +
    ggplot2::geom_line(ggplot2::aes(y = .data$MLD, colour = 'MLD')) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Bottom, colour = 'Bottom')) +
    ggplot2::facet_wrap(~ .data$StationName, scales = "free", ncol = 1) +
    ggplot2::scale_colour_manual(name = 'Depth', values = c('dark blue', 'blue', 'light blue')) +
    ggplot2::scale_x_date(breaks = '1 year', date_labels = "%Y") +
    ggplot2::labs(y = planktonr::pr_relabel("Temperature_degC", style = 'ggplot'), x = 'Years') +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(legend.position = 'bottom')
  return(plot)
  
}