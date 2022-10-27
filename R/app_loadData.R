# Load .rda file if it exists and use it in the application instead of default data
datapath <- "./data/sysdata.rda"
if (file.exists(datapath)) {
  print(paste0("Loading data from ", datapath))
  load(datapath)
  usethis::use_data(Nuts, Pigs, Pico, LTnuts, 
                    fMapDataz, fMapDatap, legendPlot,
                    MooringTS, MooringClim,
                    PolNRS, PolCPR, PolLTM, NRSinfo, CPRinfo,
                    datCPRz, datCPRp, datCPRw,
                    datNRSz, datNRSp, datNRSm, datNRSw,
                    NRSfgz, NRSfgp, CPRfgz, CPRfgp, PMapData,
                    stiz, stip, daynightz, daynightp, PMapData2,
                    overwrite = TRUE, internal = TRUE)
}


