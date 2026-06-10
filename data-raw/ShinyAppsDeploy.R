
# First reinstall spatialplanr from github
# remotes::install_github('spatialplanning/spatialplanr')

library(tidyverse)

# Set packrat option (as in original code)
options(rsconnect.packrat = TRUE)

# 3. Deploy the application
rsconnect::deployApp(
  appName = "BioOceanObserver"
)

