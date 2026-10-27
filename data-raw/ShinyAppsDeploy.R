

library(tidyverse)

# Set packrat option (as in original code)
options(rsconnect.packrat = TRUE)

# 3. Deploy the application
rsconnect::deployApp(
  appName = "BioOceanObserver"
)

