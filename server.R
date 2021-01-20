
function(input, output, session) {
  
  library(lubridate)
  library(rgeos)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(patchwork)
  library(plotly)
  library(tidyverse)
  
  source("ZooTsNRS.R")
  source("EnvDataBGC.R")
  
  theme_set(theme_bw())

  ### NRS time series data  
  ZooTsNRS("one")
  
  ## Env Data - NRS BGC
  EnvDataBGC("one")
}
