pkg.env <- new.env(parent = emptyenv())

# Load up-to-date data
.onLoad <- function(libname, pkgname){
  cat(file=stderr(), "New data load call.\n")

  # Test for response to 'errors' in data read
  # data_error_str <- Sys.getenv("DATA_ERROR") # Try with env. variables
  # data_error_vec <- strsplit(data_error_str, ",")[[1]]
  data_error_vec <- c() #c("bowen", "github", "dap")

  # Non-grep vars: LTnuts, datCPRw, PMapData
  data.vars <- c("Nuts", "Pigs", "Pico", "ctd", "CSChem",
                 "fMapDataz", "fMapDatap",
                 "MooringTS", "MooringClim",
                 "PolNRS", "PolCPR", "PolLTM", "PolSOTS",
                 "NRSinfo", "CPRinfo", "SOTSinfo", "NRSStation",
                 "datCPRz", "datCPRp", "PCI",
                 "datNRSz", "datNRSp", "datNRSm", "datCSm", "datNRSw", "datGSm",
                 "datHABg", "datHABs", "datHABTrip", "datHABdataTable",
                 "NRSfgz", "NRSfgp", "CPRfgz", "CPRfgp", "PMapData",
                 "SOTSp", "SOTSfgp",
                 "datNRSp_all",
                 "choicespNRSp", "choicespNRSz", "choicespNRSm",
                 "choicespCPRp", "choicespCPRz", "choicespCSm", "choicespHAB",
                 "stiz", "stip", "daynightz", "daynightp",
                 "SpInfoP", "SpInfoZ", "LFData", "LFDataAbs",
                 "datNRSTrip", "datCPRTrip", "datCPRTripSO",
                 "PSpNRSAccum", "PSpCPRAccum", "ZSpNRSAccum", "ZSpCPRAccum",
                 "col12", "ParamDef", "AusStatesSimple",
                 "AT_species_summary", "AT_receivers", "AT_station_species",
                 "AT_individual_data", "AT_all_species", "AT_daily_summary")

  tryCatch({
    # Access data from local server (fastest)
    # The variable data.url is a string that is the url to the served data and is defined in a local config.rda file.
    # If config.rda doesn't exist, this step results in an error that is then handled.
    start_time <- Sys.time()
    cat(file=stderr(), "Attempting to access data from Bowen\n")
    
    if("bowen" %in% data_error_vec) {
        stop("Force Bowen read error")
    }
    
    thredds_url <- "https://data-cbr.it.csiro.au/thredds/fileServer/catch_all/imosboo/BOODataUpload/sysdata.rda"
    tmp <- tempfile(fileext='.rda')
    httr::GET(thredds_url, httr::write_disk(tmp))
    load(tmp)
    cat(file=stderr(), paste0("Up-to-date data accessed from Bowen\n"))
    pkg.env$new.data <- TRUE
    pkg.env$data.source <- "Bowen"
    pkg.env$load.time <- Sys.time() - start_time
    pkg.env$file.size <- file.size(tmp)
  }, error = function(e) {
    cat(file=stderr(), as.character(e))
    tryCatch({
      # Access from GitHub
      start_time <- Sys.time()
      cat(file=stderr(), "Attempting to access data from GitHub\n")
      
      if("github" %in% data_error_vec) {
        stop("Force GitHub read error")
      }
      
      github_url <- "https://github.com/PlanktonTeam/BioOceanObserver/raw/refs/heads/main/R/sysdata.rda"
      tmp <- tempfile(fileext='.rda')
      httr::GET(github_url, httr::write_disk(tmp))
      load(tmp)
      cat(file=stderr(), paste0("Up-to-date data accessed from GitHub\n"))
      pkg.env$new.data <- TRUE
      pkg.env$data.source <- "GitHub"
      pkg.env$load.time <- Sys.time() - start_time
      pkg.env$file.size <- file.size(tmp)
      
    }, error = function(e) {
      cat(file=stderr(), as.character(e))
      tryCatch({
        # Access data from DAP (fallback)
        start_time <- Sys.time()
        cat(file=stderr(), "Attempting to access data from CSIRO DAP.\n")
        
        if("dap" %in% data_error_vec) {
          stop("Force DAP read error")
        }
        
        dap.url <- "https://data.csiro.au/dap/ws/v2/collections/csiro:54520/data"
        dap.data <- jsonlite::fromJSON(rawToChar(httr::GET(dap.url)$content))
        file.req <- dap.data$file$filename
        tmp <- tempfile(fileext='.rda')
        httr::GET(dap.data$file$link$href[[which(dap.data$file$filename == "sysdata.rda")]], httr::write_disk(tmp))
        load(tmp)
        cat(file=stderr(), "Up-to-date data accessed from dap.\n")
        pkg.env$new.data <- TRUE
        
        pkg.env$data.source <- "DAP"
        pkg.env$load.time <- Sys.time() - start_time
        pkg.env$file.size <- file.size(tmp)
        
      }, error = function(e) {
        # Warn that data is not up-to-date
        cat(file=stderr(), as.character(e))
        cat(file=stderr(), "Building the Biological Ocean Observer package using built in sysdata.rda. If this message appears when running the app, the data being served is not up-to-date.\n")
        pkg.env$new.data <- FALSE
        
        pkg.env$data.source <- "Package"
        pkg.env$load.time <- 0
        pkg.env$file.size <- 0
      })
    })
    
   })

  # Add data vars to package environment variable
  for (currVar in data.vars) {
    pkg.env[[currVar]] <- get(currVar)
  }
}


