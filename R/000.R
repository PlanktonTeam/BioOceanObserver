pkg.env <- new.env(parent = emptyenv())

data.vars <- c("Pigs", "Pico", "ctd", "CSChem", "Nuts",
                 "fMapDataz", "fMapDatap",
                 "MooringTS", "MooringClim",
                 "PolNRS", "PolCPR", "PolLTM", "PolSOTS",
                 "NRSinfo", "CPRinfo", "SOTSinfo", "NRSStation", "SotsStation",
                 "datCPRz", "datCPRp", "PCI",
                 "datNRSz", "datNRSp", "datNRSm",
               "datCSm", "datNRSw", "datGSm",
                 "NRSfgz", "NRSfgp", "CPRfgz", "CPRfgp", "PMapData",
               'SOTSp', 'SOTSwater', 'NutsSots', 'SOTSfgp',
               "stiz", "stip", "daynightz", "daynightp",
                 "SpInfoP", "SpInfoZ", "LFData", "LFDataAbs",
                 "datNRSTrip", "datCPRTrip",
                 "PSpNRSAccum", "PSpCPRAccum", "ZSpNRSAccum", "ZSpCPRAccum",
                 "col12", "ParamDef")
pkg.env$new.data <- FALSE

# Add data vars to package environment variable
  for (currVar in data.vars) {
    pkg.env[[currVar]] <- get(currVar)
  }



