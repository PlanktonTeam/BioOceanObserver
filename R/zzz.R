pkg.env <- new.env(parent = emptyenv())

# Load up-to-date data
.onLoad <- function(libname, pkgname){
  cat(file=stderr(), "New data load call.\n")
  # Non-grep vars: LTnuts, datCPRw, PMapData
  data.vars <- c("Nuts", "Pigs", "Pico", "LTnuts",
                 "fMapDataz", "fMapDatap", "legendPlot",
                 "MooringTS", "MooringClim",
                 "PolNRS", "PolCPR", "PolLTM", "NRSinfo", "CPRinfo", "NRSStation",
                 "datCPRz", "datCPRp", "datCPRw",
                 "datNRSz", "datNRSp", "datNRSm", "datNRSw",
                 "NRSfgz", "NRSfgp", "CPRfgz", "CPRfgp", "PMapData",
                 "stiz", "stip", "daynightz", "daynightp", "PMapData2",
                 "SpInfoP", "SpInfoZ", "LFData", "datNRSTrip", "datCPRTrip",
                 "PSpNRSAccum", "PSpCPRAccum", "ZSpNRSAccum", "ZSpCPRAccum",
                 "col12")
  tryCatch({
    # Access data from local server (fastest)
    cat(file=stderr(), "Attempting to access data from opendap.\n")
    opendap.url <- "https://data-cbr.it.csiro.au/files/sc-opendap-work/work/sc-artefact/imosboo/sysdata1.rda"
    tmp <- tempfile(fileext='.rda')   
    httr::GET(opendap.url, httr::write_disk(tmp))
    load(tmp)
    cat(file=stderr(), "Up-to-date data accessed from opendap.\n")
    pkg.env$new.data <- TRUE
  }, error = function(e) { 
    cat(file=stderr(), as.character(e))
    tryCatch({
      # Access data from DAP (fallback)
      cat(file=stderr(), "Attempting to access data from dap.\n")
      dap.url <- "https://data.csiro.au/dap/ws/v2/collections/csiro:54520/data"
      dap.data <- jsonlite::fromJSON(rawToChar(httr::GET(dap.url)$content))
      file.req <- dap.data$file$filename 
      tmp <- tempfile(fileext='.rda')   
      httr::GET(dap.data$file$link$href[[which(dap.data$file$filename == "sysdata1.rda")]], httr::write_disk(tmp))
      load(tmp)
      cat(file=stderr(), "Up-to-date data accessed from dap.\n")
      pkg.env$new.data <- TRUE
 
    }, error = function(e) { 
      # Warn that data is not up-to-date
      cat(file=stderr(), as.character(e))
      cat(file=stderr(), "Building the imosboo package using built in sysdata.rda. If this message appears when running the app, the data being served is not up-to-date.\n")
      pkg.env$new.data <- FALSE
    }) 
  })
  
  # Add data vars to package environment variable
  for (currVar in data.vars) {
    pkg.env[[currVar]] <- get(currVar)
  }
}

