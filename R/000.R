pkg.env <- new.env(parent = emptyenv())

# Load up-to-date data
.onLoad <- function(libname, pkgname){
  cat(file=stderr(), "New data load call.\n")
  # Non-grep vars: LTnuts, datCPRw, PMapData
  data.vars <- c("Nuts", "Pigs", "Pico", "ctd", "CSChem",
                 "fMapDataz", "fMapDatap", 
                 "MooringTS", "MooringClim",
                 "PolNRS", "PolCPR", "PolLTM", 
                 "NRSinfo", "CPRinfo", "NRSStation",
                 "datCPRz", "datCPRp", "PCI",
                 "datNRSz", "datNRSp", "datNRSm", "datCSm", "datNRSw", "datGSm",
                 "NRSfgz", "NRSfgp", "CPRfgz", "CPRfgp", "PMapData",
                 "stiz", "stip", "daynightz", "daynightp",
                 "SpInfoP", "SpInfoZ", "LFData", "LFDataAbs",
                 "datNRSTrip", "datCPRTrip",
                 "PSpNRSAccum", "PSpCPRAccum", "ZSpNRSAccum", "ZSpCPRAccum",
                 "col12", "ParamDef")

  tryCatch({
    # Access data from local server (fastest)
    # The variable data.url is a string that is the url to the served data and is defined in a local config.rda file.
    # If config.rda doesn't exist, this step results in an error that is then handled.
    cat(file=stderr(), "Attempting to access data from Bowen\n")
    thredds_url <- "https://data-cbr.it.csiro.au/thredds/fileServer/catch_all/imosboo/BOODataUpload/archive/20250529_sysdata.rda"
    tmp <- tempfile(fileext='.rda')   
    httr::GET(thredds_url, httr::write_disk(tmp))
    load(tmp)
    cat(file=stderr(), paste0("Up-to-date data accessed from Bowen\n"))
    pkg.env$new.data <- TRUE
  }, error = function(e) { 
    cat(file=stderr(), as.character(e))
    tryCatch({
      # Access data from DAP (fallback)
      cat(file=stderr(), "Attempting to access data from CSIRO DAP.\n")
      dap.url <- "https://data.csiro.au/dap/ws/v2/collections/csiro:54520/data"
      dap.data <- jsonlite::fromJSON(rawToChar(httr::GET(dap.url)$content))
      file.req <- dap.data$file$filename 
      tmp <- tempfile(fileext='.rda')   
      httr::GET(dap.data$file$link$href[[which(dap.data$file$filename == "sysdata.rda")]], httr::write_disk(tmp))
      load(tmp)
      cat(file=stderr(), "Up-to-date data accessed from dap.\n")
      pkg.env$new.data <- TRUE
 
    }, error = function(e) { 
      # Warn that data is not up-to-date
      cat(file=stderr(), as.character(e))
      cat(file=stderr(), "Building the Biological Ocean Observer package using built in sysdata.rda. If this message appears when running the app, the data being served is not up-to-date.\n")
      pkg.env$new.data <- FALSE
    }) 
  })
  
  # Add data vars to package environment variable
  for (currVar in data.vars) {
    pkg.env[[currVar]] <- get(currVar)
  }
}

