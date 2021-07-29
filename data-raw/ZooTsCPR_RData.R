## code to prepare `ZooTsCPR_RData` dataset goes here
library(tidyverse)

# source all the main functions from the IMOS_Toolbox, this is the default place for the package functions for now
source("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/IMOS_Plankton_functions.R", local = FALSE)

CPRSamps <- getCPRSamps()

datCPRi <- read_csv("https://raw.githubusercontent.com/PlanktonTeam/IMOS_Toolbox/master/Plankton/Output/CPR_Indices.csv") %>%
  select(SampleDateUTC, Year, Month, Bioregion, Biomass_mgm3, ZoopAbundance_m3)



# save data into data folder
usethis::use_data(, internal = FALSE)

