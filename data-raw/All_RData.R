## script for all RData 
## Aim to change all source calls to planktonr functions

library(planktonr)

# NRS time series data
datNRSz <- planktonr::pr_get_tsdata("NRS", "Z")
datNRSp <- planktonr::pr_get_tsdata("NRS", "P")

# CPR time series data
datCPRz <- planktonr::pr_get_tsdata("CPR", "Z")
datCPRp <- planktonr::pr_get_tsdata("CPR", "P")

# FG time series data
NRSfgz <- planktonr::pr_get_fg("NRS", "Z")
NRSfgp <- planktonr::pr_get_fg("NRS", "P")
CPRfgz <- planktonr::pr_get_fg("CPR", "Z")
CPRfgp <- planktonr::pr_get_fg("CPR", "P")

# BGC Environmental variables data
Nuts <- planktonr::pr_get_nuts()
Pigs <- planktonr::pr_get_pigs()
Pico <- planktonr::pr_get_pico()

# Species distribution data
fMapDataz <- planktonr::pr_get_fMap_data("Z")
fMapDatap <- planktonr::pr_get_fMap_data("P")

# STI data
stiz <- planktonr::pr_get_sti("Z")
stip <- planktonr::pr_get_sti("P")

# daynight data (from CPR only)
daynightz <- planktonr::pr_get_daynight("Z")
daynightp <- planktonr::pr_get_daynight("P")

# Policy data set
Pol <- readr::read_csv(paste0(planktonr::pr_get_outputs(), "NRS_Indices.csv"), na = "NA", show_col_types = FALSE) %>%
  dplyr::select(SampleDateLocal, Year, Month, StationName, StationCode, Biomass_mgm3, PhytoBiomassCarbon_pgL, Temperature_degC, ShannonCopepodDiversity, 
                ShannonPhytoDiversity, Salinity_psu, Chla_mgm3) %>%
  tidyr::pivot_longer(-c(SampleDateLocal:StationCode), values_to = 'Values', names_to = "parameters")

means <- readr::read_csv(paste0(planktonr::pr_get_outputs(), "NRS_Indices.csv"), na = "NA", show_col_types = FALSE) %>%
  dplyr::select(StationName, Biomass_mgm3, PhytoBiomassCarbon_pgL, Temperature_degC, ShannonCopepodDiversity, 
                ShannonPhytoDiversity, Salinity_psu, Chla_mgm3) %>%
  tidyr::pivot_longer(-c(StationName), values_to = 'Values', names_to = "parameters") %>%
  dplyr::group_by(StationName, parameters) %>%
  dplyr::summarise(means = mean(Values, na.rm = TRUE), 
                  sd = stats::sd(Values, na.rm = TRUE),
                  .groups = 'drop')

Pol <- Pol %>% dplyr::left_join(means, by = c("StationName", "parameters")) %>%
  dplyr::mutate(anomaly = (Values - means)/sd)

params <- Pol %>% dplyr::select(parameters) %>% unique()
params <- params$parameters
dfc <- Pol %>% dplyr::filter(StationCode == 'NSI')

Harm <- function (theta, k = 4) {
  X <- matrix(0, length(theta), 2 * k)
  nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
  dimnames(X) <- list(names(theta), nam)
  m <- 0
  for (j in 1:k) {
    X[, (m <- m + 1)] <- cos(j * theta)
    X[, (m <- m + 1)] <- sin(j * theta)
  }
  X
}

coeffs <- function(params){
  lmdat <-  dfc %>% dplyr::filter(parameters == "Biomass_mgm3") %>% tidyr::drop_na()
  m <- lm(Values ~ Year + Harm(Month, k = 1), data = lmdat) 
  lmdat <- lmdat %>% dplyr::bind_cols(fv = m$fitted.values)
}

output <- purrr::map_dfr(params, coeffs)

#titley <- planktonr::pr_relabel(unique(df$parameters), style = "ggplot")
titley <- planktonr::pr_relabel("Biomass_mgm3", style = "ggplot")
p1 <- ggplot2::ggplot(lmdat ) + # do this logging as in pr_plot_tsclimate
  ggplot2::geom_point(ggplot2::aes(x = SampleDateLocal, y = Values)) +
  ggplot2::geom_smooth(ggplot2::aes(x = SampleDateLocal, y = fv), method = "lm") +
  ggplot2::ylab(rlang::enexpr(titley)) +
  ggplot2::scale_y_continuous(trans = "log10") +
  ggplot2::scale_x_datetime(date_breaks = "2 years", date_labels = "%Y") +
  ggplot2::xlab("Year") +
  ggplot2::theme(legend.position = "none",
                 axis.title.x = ggplot2::element_blank(),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(hjust = 0, size = 16))
p1

p2 <- ggplot2::ggplot(lmdat ) + # do this logging as in pr_plot_tsclimate
  ggplot2::geom_point(ggplot2::aes(x = Month, y = Values)) +
  ggplot2::geom_smooth(ggplot2::aes(x = Month, y = fv), method = "loess") +
  ggplot2::scale_y_continuous(trans = "log10") +
  ggplot2::scale_x_continuous(breaks = seq(1, 12, length.out = 12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  ggplot2::xlab("Month") +
  ggplot2::theme(legend.position = "none",
                 axis.title.y = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 strip.background = ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(hjust = 0, size = 16))
p2

lmdatm <- lmdat %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(value = mean(.data$fv, na.rm = TRUE),
                   .groups = "drop")

lmdatm <- lmdat

## microbial data
library(tidyverse)
datNRSm <- readr::read_csv("data/datNRSm.csv") %>%
  dplyr::mutate(SampleDepth_m = as.numeric(stringr::str_sub(TripCode_depth, -3, -1))) %>%
  dplyr::select(StationName, SampleDepth_m, StationCode, SampleDateLocal, Year, Month, 
         Prochlorococcus_cells_ml:Eukaryote_Chlorophyll_Index) %>%
  dplyr::rename(Prochlorococcus_Cellsml = Prochlorococcus_cells_ml,
                Synecochoccus_Cellsml = Synecochoccus_cells_ml,
                Picoeukaryotes_Cellsml = Picoeukaryotes_cells_ml) %>%
  tidyr::pivot_longer(-c(StationName:Month), values_to = "Values", names_to = "parameters")

# add data to sysdata.rda
usethis::use_data(Nuts, Pigs, fMapDataz, fMapDatap, Pico, Pol,
                  datCPRz, datCPRp, datNRSz, datNRSp, datNRSm,
                  NRSfgz, NRSfgp, CPRfgz, CPRfgp,
                  stiz, stip, daynightz, daynightp,
                  overwrite = TRUE, internal = TRUE)

## files for SDMs (this will only work for Claire at the moment)
listsdm <- list.files(path = "C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps")
files <- paste("C:/Users/dav649/Documents/GitHub/SDMs/SDM_maps/", listsdm, sep = "")
file.copy(from=files, to="inst/app/www")

