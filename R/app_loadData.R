# Load .rda file if it exists and use it in the application instead of default data
datapath <- "./data/sysdata.rda"
if (file.exists(datapath)) {
  print(paste0("Loading data from ", datapath))
  # load(datapath)
  file.copy(datapath, "./R/sysdata.rda", overwrite = T)
}


