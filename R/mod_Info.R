#' Help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_info_ui <- function(id){
  nsInfo <- NS(id)
  shiny::tagList(
    tabsetPanel(id = "info", type = "pills",
                tabPanel("Frequently Asked Questions", value = 1,
                         shiny::fluidPage(
                           shiny::br(),
                           shiny::h3(shiny::strong("The App")),
                           shiny::h4("Does the app use real time data?"),
                           shiny::p("No, to keep the App efficient the data is harvested from the AODN monthly and pre-wrangled."),
                           shiny::h4("Where is the data behind the APP available from?"),
                           shiny::p("All the non-genomic data is accesible through direct download from the AODN portal or
                                    by using the pr_get_xxx functions in planktonr. The raw genomic data is available through AMB data portal,
                                    the calculated indices are available through the Australian Microbiome, Microbial Ocean Atlas git page"),
                           shiny::HTML("<li>AODN Portal <a href = https://portal.aodn.org.au/ target = _blank> Website</a>"),
                           shiny::HTML("<li>AMB portal <a href = https://data.bioplatforms.com/organization/about/australian-microbiome target = _blank> Website</a>"),
                           shiny::HTML("<li>AMB github <a href = https://github.com/AusMicrobiome/microbial_ocean_atlas target = _blank> Website</a>"),
                           shiny::h4("How can I make these figures myself?"),
                           shiny::p("Download the data and make your own plots using planktonr. 
                                    Vignettes are available in planktonr to help you navigate the data and package"),
                           shiny::h4("How can I access planktonr?"),
                           shiny::p("Via R studio use remotes::install_github('PlanktonTeam/planktonr', force = TRUE)"),
                           shiny::h4("How can I get more details on how these figures were put together?"),
                           shiny::p("Click on the 'R Code Example' button to follow a link to vignettes detailing the process")
                         )
                ),
                tabPanel("Technical Information", value = 2,
                         shiny::fluidPage(
                           shiny::br(),
                           shiny::h3("Data binning"),
                           shiny::h4("Depths"),
                           shiny::p("Depths binned for ease of plotting.
                                    Water column samples for the NRS stations have been removed, these were taken from the beginning of the program until July 2017.
                                    The samples taken at defined depths have been included here as they are more informative. All data can be sourced direct from the AODN or by using planktonr"),
                           shiny::h4("Contour plots"),
                           shiny::p("There is an option to interpolate the data in the contour plots used for the environmental data taken at defined depths. The base method does not fill in NAs in the dataset. 
                                     Use the option na.fill = TRUE if you want to interpolate."),
                           shiny::br(),
                           shiny::h3("Outliers"),
                           shiny::p("Outliers are removed when they are greater than 2 standard deviations from the mean. Negative values for nutrients etc. are also set 
                                    to 0. These options are used in BOO but if you want to keep the outliers you can download the data and plot the figures using planktonr with your chosen settings"),
                           shiny::br(),
                           shiny::h3("Assigning bioregions to CPR samples"),
                           shiny::p("CPR BioRegions have been selected using nearest neighbour. If there is a requriement for only bioregions 
                                    to be assigned when they are in the exact region, this can be done with the function in planktonr. 
                                    We use the pre-defined bioregions for the CPR samples (see State of the Environment 2016 website) except 
                                    for the Southern Ocean bioregion which has been determined by the SO_CPR project boundaries")
                         )
                ),
                tabPanel("ChangeLog", value = 3,
                         shiny::fluidPage(
                           shiny::br(),
                           shiny::h3("Changelog for the Biological Ocean Observer"),
                           shiny::br(),
                           shiny::h4("July 2021"),
                           shiny::HTML("<ul>     
                                          <li>Initial Prototype developed</li>
                                          <li>Add Zooplankton Data</li>
                                     </ul>"),
                           shiny::h4("September 2021"),
                           shiny::HTML("<ul>     
                                          <li>Add Phytoplankton Data</li>
                                          <li>Add Nutrients</li>
                                          <li>Add Pigments</li>
                                      </ul>"),
                           shiny::h4("December 2021"),
                           shiny::HTML("<ul>
                                          <li>Add Microbial Data</li>
                                      </ul>"),
                           shiny::h4("January 2022"),
                           shiny::HTML("<ul>
                                          <li>Add Essential Ocean Variables</li>
                                      </ul>"),
                           shiny::h4("July 2022"),
                           shiny::HTML("<ul>
                                          <li>Add CTD Data</li>
                                      </ul>"),
                           shiny::h4("August 2022"),
                           shiny::HTML("<ul>
                                          <li>Add Moorings</li>
                                      </ul>"),
                           shiny::h4("November 2022"),
                           shiny::HTML("<ul>
                                          <li>Add Larval Fish Data</li>
                                          <li>Add Picoplankton</li>
                                          <li>Add Information Page</li>
                                      </ul>"),
                           shiny::h4("February 2023"),
                           shiny::HTML("<ul>
                                          <li>Initial release Version uploaded</li>
                                      </ul>"),
                           shiny::h4("March 2023"),
                           shiny::HTML("<ul>
                                          <li>Add Relationships Page</li>
                                      </ul>"),
                           shiny::h4("July 2023"),
                           shiny::HTML("<ul>
                                          <li>Biological Ocean Observer released at Australian Marine Sciences Conference</li>
                                      </ul>"),
                         )
                ),
                tabPanel("References", value = 4,
                         shiny::fluidPage(
                           shiny::br(),
                           shiny::h4("References"),
                           shiny::h5("To further understand the data, collection methods etc."),
                           shiny::HTML("<li>Davies, CH., Sommerville, E. (Eds.) (2017). <em>National Reference Stations Biogeochemical Operations Manual</em>. Version 3.3.1. Integrated Marine Observing System. DOI:10.26198/5c4a56f2a8ae3. <a href = http://dx.doi.org/10.26198/5c4a56f2a8ae3 target = _blank> Website</a>."),
                           shiny::HTML("<li>Eriksen RS, Davies CH, Bonham P, Coman FE, Edgar S, McEnnulty FR, McLeod D, Miller MJ, Rochester W, Slotwinski A, Tonks ML, Uribe-Palomino J and Richardson AJ (2019). <em>Australia's Long-Term Plankton Observations: The Integrated Marine Observing System National Reference Station Network</em>. Front. Mar. Sci. 6:161. doi: 10.3389/fmars.2019.00161. <a href = https://www.frontiersin.org/articles/10.3389/fmars.2019.00161/full target = _blank> Website</a>."),
                           shiny::HTML("<li>A.J. Richardson, A.W. Walne, A.W.G. John, T.D. Jonas, J.A. Lindley, D.W. Sims, D. Stevens, M. Witt, (2006). <em>Using continuous plankton recorder data</em>. Progress in Oceanography, 68.1, doi: 10.1016/j.pocean.2005.09.011. <a href = https://www.sciencedirect.com/science/article/pii/S0079661105001424?via%3Dihub target = _blank> Website</a>."),
                           shiny::HTML("<li>IMOS National Reference Stations website. <a href = https://imos.org.au/facilities/nationalmooringnetwork/nrs target = _blank> Website</a>."),
                           shiny::HTML("<li>IMOS Continuous PLankton Recorder Survey website. <a href = https://imos.org.au/facilities/shipsofopportunity/auscontinuousplanktonrecorder target = _blank> Website</a>."),
                           shiny::HTML("<li> Australian MIcrobiome Scientific Manual. <a href = https://confluence.csiro.au/display/ASM/Ausmicrobiome+Scientific+Manual target = _blank> Website</a>."),
                           shiny::br(),
                           shiny::br(),
                           shiny::h5("These references are also good resources for visualising this data"),
                           shiny::HTML("<li>Richardson A.J, Eriksen R,S, Moltmann T, Hodgson-Johnston I, Wallis J.R. (2020). <em>State and Trends of Australia's Ocean Report</em>. Integrated Marine Observing System (IMOS). <a href = https://www.imosoceanreport.org.au/about/ target = _blank> Website</a>."),
                           shiny::HTML("<li>Richardson A.J, Eriksen R.S, Rochester, W. (2015) <em>Plankton 2015: State of Australia's Oceans</em>. CSIRO report. ISBN 978-1-4863-0566-7 (EPDF). <a href = https://imos.org.au/fileadmin/user_upload/shared/Data_Tools/15-00245_OA_Plankton2015_20ppBrochure_WEB_151116.pdf target = _blank> Website</a>."),
                           shiny::HTML("<li>Brown, M., van de Kamp, J., Ostrowski, M. <em>et al</em>. (2018). Systematic, <em>continental scale temporal monitoring of marine pelagic microbiota by the Australian Marine Microbial Biodiversity Initiative</em>. Sci Data 5, 180130. doi.org/10.1038/sdata.2018.130 <a href = https://doi.org/10.1038/sdata.2018.130 target = _blank> Website</a>."),
                           shiny::br(),
                           shiny::br(),
                           shiny::h4("Package citations"),
                           shiny::h5("This Web App is possible thanks to the following packages and the active R community"),
                           shiny::HTML("<li>Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). <em>shiny: Web Application Framework for R</em>. R package version 1.7.2, <a href = https://CRAN.R-project.org/package=shiny target = _blank> Website</a>."),
                           shiny::HTML("<li>Fay C, Guyader V, Rochette S, Girard C (2022). <em>golem: A Framework for Robust Shiny Applications</em>. R package version 0.3.3, <a href = https://CRAN.R-project.org/package=golem target = _blank> Website</a>.<br>"),
                           shiny::HTML("<br>"),
                           shiny::HTML("<li>Wickham H, Fran\u00E7ois R, Henry L, M\u00FCller K (2022). <em>dplyr: A Grammar of Data Manipulation</em>. R package version 1.0.10, <a href = https://CRAN.R-project.org/package=dplyr target = _blank> Website</a>."),
                           shiny::HTML("<li>Wickham H (2016). <em>ggplot2: Elegant Graphics for Data Analysis</em>. Springer-Verlag New York, <a href = https://ggplot2.tidyverse.org target = _blank> Website</a>."),
                           shiny::HTML("<li>Garrett Grolemund, Hadley Wickham (2011). <em>Dates and Times Made Easy with lubridate.</em>, Journal of Statistical Software, 40(3), 1-25. <a href = https://www.jstatsoft.org/v40/i03/ target = _blank> Website</a>."),
                           shiny::HTML("<li>Bache S, Wickham H (2022). <em>magrittr: A Forward-Pipe Operator for R</em>. R package version 2.0.3, <a href = https://CRAN.R-project.org/package=magrittr target = _blank> Website</a>."),
                           shiny::HTML("<li>Pedersen T (2020). <em>patchwork: The Composer of Plots</em>. R package version 1.1.1, <a href = https://CRAN.R-project.org/package=patchwork target = _blank> Website</a>."),
                           shiny::HTML("<li>Henry L, Wickham H (2020). <em>purrr: Functional Programming Tools</em>. R package version 0.3.4, <a href = https://CRAN.R-project.org/package=purrr target = _blank> Website</a>."),
                           shiny::HTML("<li>Henry L, Wickham H (2022). <em>rlang: Functions for Base Types and Core R and 'Tidyverse' Features</em>. R package version 1.0.4, <a href = https://CRAN.R-project.org/package=rlang target = _blank> Website</a>."),
                           shiny::HTML("<li>South A (2017). <em>rnaturalearth: World Map Data from Natural Earth</em>. R package version 0.1.0, <a href = https://CRAN.R-project.org/package=rnaturalearth target = _blank> Website</a>."),
                           shiny::HTML("<li>Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal 10 (1), 439-446, <a href = https://doi.org/10.32614/RJ-2018-009 target = _blank> Website</a>."),
                           shiny::HTML("<li>Attali, D, Edwards T (2021). <em>shinyalert: Easily Create Pretty Popup Messages (Modals) in 'Shiny'</em>. R package version 3.0.0, <a href = https://cran.r-project.org/web/packages/shinyalert/index.html target = _blank> Website</a>."),
                           shiny::HTML("<li>Sali A, Attali D (2020). <em>shinycssloaders: Add Loading Animations to a 'shiny' Output While It's Recalculating</em>. R package version 1.0.0, <a href = https://CRAN.R-project.org/package=shinycssloaders target = _blank> Website</a>."),
                           shiny::HTML("<li>Chang W, Borges Ribeiro B (2021). <em>shinydashboard: Create Dashboards with 'Shiny'</em>. R package version 0.7.2, <a href = https://CRAN.R-project.org/package=shinydashboard target = _blank> Website</a>."),
                           shiny::HTML("<li>Attali D (2021). <em>shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds</em>. R package version 2.1.0, <a href = https://CRAN.R-project.org/package=shinyjs target = _blank> Website</a>."),
                           shiny::HTML("<li>Wickham H (2019). <em>stringr: Simple, Consistent Wrappers for Common String Operations</em>. R package version 1.4.0, <a href = https://CRAN.R-project.org/package=stringr target = _blank> Website</a>."),
                           shiny::HTML("<li>M\u00FCller K, Wickham H (2022). <em>tibble: Simple Data Frames</em>. R package version 3.1.8, <a href = https://CRAN.R-project.org/package=tibble target = _blank> Website</a>."),
                           shiny::HTML("<li>Wickham H, Girlich M (2022). <em>tidyr: Tidy Messy Data</em>. R package version 1.2.0, <a href = https://CRAN.R-project.org/package=tidyr target = _blank> Website</a>."),
                           shiny::HTML("<li>Henry L, Wickham H (2022). <em>tidyselect: Select from a Set of Strings</em>. R package version 1.1.2, <a href = https://CRAN.R-project.org/package=tidyselect target = _blank> Website</a>.")
                         ),
                ),
                tabPanel("Sampling Details", value = 5,
                         shiny::fluidPage(
                           shiny::br(),
                           shiny::h2("NRS"),
                           shiny::h6("Note: Ningaloo and Esperance only operated for 3 years and were only sampled seasonally. The data is sparse for these stations and has often been removed for some analysis."),
                           shiny::h6("Note: The NRS sampling period goes from mid 2009 until present. Prior to this some environmental parameters were also collected at the 
                                     Long Term Monitoring Stations - ROT, MAI, PH4. These are shown on the Long Term Monitoring tab under EOVs. Generally though in this 
                                     APP we concentrate on the visualisation of the NRS period where more parameters have been collected in a consistent manner across stations"),
                           DT::DTOutput(nsInfo("NRSDataTable")),
                           shiny::h4("Zooplankton"),
                           shiny::h5("Zooplankton is collected with a Heron drop net sampling only on the descent, 60cm diameter, 100 micron mesh net. This is a depth integrated sample analysed by light microscopy"),
                           shiny::h4("Phytoplankton"),
                           shiny::h5("Equal volumes of water are collected from all niskin bottles (0-50m) to make up a 1L composite sample for phytoplankton analysis using light microscopy."),
                           shiny::h4("Nutrients & Salinity"),
                           shiny::h5("Duplicate samples are taken from each niskin bottle (0-50m), DAR and YON samples are only 0 - 20m. 
                                     PHB also collects at 75 and 100m. MAI at 60, 70 & 80m"),
                           shiny::h4("Carbon"),
                           shiny::h5("DIC & alkalinity samples are taken from each niskin bottle (0-50m), DAR and YON samples are only 0 - 20m. 
                                     PHB also collects at 60, 75 & 100m. MAI at 70 & 80m"),                         
                           shiny::h4("TSS"),
                           shiny::h5("Triplicate samples (~4L) and a blank of surface water"),                         
                           shiny::h4("Pigments"),
                           shiny::h5("Duplicate samples (~4L) from surface water and the lower WQM (~20m). Prior to July 2017 water column samples were taken, these have been excluded here."),                         
                           shiny::h4("Microbial & Picoplankton"),
                           shiny::h5("Samples (~2L microbes, ~50-100 uL Picoplankton) are taken from each niskin bottle (0-50m), DAR and YON samples are only 0 - 20m. 
                                     PHB also collects at 75 and 100m. MAI at 70 & 80m. Prior to July 2017 water column samples were taken, these have been excluded here."),
                           shiny::HTML('<center><img src = "www/NRSMooringDesign.png"></center>'),
                           shiny::HTML('<center>Schematic of the NRS moorings showing depths and instrumentation</center>'),
                           shiny::br(),
                           shiny::br(),
                           shiny::h2("Coastal Stations"),
                           shiny::h5("Nutrients, picoplankton and microbial samples are collected at the Coastal Station in Botany Bay, the Derwent, the GBR and Port Phillip Bay. 
                                     Details for this sampling can be found in the Australian Microbiome Scientifc Manual."), 
                           shiny::HTML("<a href = https://confluence.csiro.au/display/ASM/Ausmicrobiome+Scientific+Manual target = _blank> Website</a>."),
                           shiny::br(),
                           shiny::br(),
                           shiny::h2("CPR"),
                           DT::DTOutput(nsInfo("CPRDataTable")),
                           shiny::h4("PCI"),
                           shiny::h5("Silks are cut into 5nm segments. Phytoplankton colour index is counted on every segment towed"),
                           shiny::h4("Phytoplankton"),
                           shiny::h5("Phytoplankton is counted by light microscopy as a field of view count (0-20) on the silk. Every 4th segment is counted."),
                           shiny::h4("Zooplankton"),
                           shiny::h5("Zooplankton is washed into a Bogorov tray and counted by light microscopy. Every 4th segment is counted"),
                           shiny::h4("Biomass Index"),
                           shiny::h5("After counting the sample is dried at 60 degrees for 24 hours and weighed for biomass. Note that this index of biomass includes phytoplankton")
                         )
                ),
                tabPanel("Phytoplankton Species Details", value = 6, 
                         shiny::h2("Phytoplankton Species Information"),
                         DT::DTOutput(nsInfo("PDataTable")),
                ),
                tabPanel("Zooplankton Species Details", value = 7, 
                         shiny::h2("Zooplankton Species Information"),
                         DT::DTOutput(nsInfo("ZDataTable")),
                ),
    )
  )
}

#' Help Server Functions
#'
#' @noRd 
mod_info_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    observeEvent({input$Info == 5}, {
      output$NRSDataTable <- DT::renderDT(
        pkg.env$NRSStation %>% 
          dplyr::mutate(EndDate = dplyr::case_when(.data$StationCode %in% c('NIN', 'ESP') ~ "2012-03-01",
                                                   .data$StationCode == 'PH4' ~ '2009-02-24')) %>% 
          dplyr::select("StationCode":"StationStartDate", "EndDate", dplyr::everything()) %>% 
          dplyr::rename(Code = "StationCode", Station = "StationName", State = "StateCode", 
                        `Start Date` = "StationStartDate", `End Date` = "EndDate", `Water Depth (m)` = "StationDepth_m",
                        `Sampling Effort` = "SamplingEffort", Region = "ManagementRegion")
      )
      
      output$CPRDataTable <- DT::renderDT(
        
        pkg.env$datCPRTrip %>% 
          dplyr::group_by(.data$Region) %>% 
          dplyr::summarise(StartDate = min(.data$Year_Local, na.rm = TRUE),
                           EndDate = max(.data$Year_Local, na.rm = TRUE),
                           
                           MilesTowed = dplyr::n() * 5,
                           SamplesCounted = round(dplyr::n()/4,0),
                           .groups = 'drop') %>% 
          dplyr::rename(`Start Date` = "StartDate", `End Date` = "EndDate", `Miles Towed` = "MilesTowed",
                        `Samples Counted` = "SamplesCounted") %>% 
          dplyr::mutate(Project = ifelse(.data$Region == "Southern Ocean Region", "SO-CPR / AusCPR", "AusCPR"),
                        Institution = ifelse(.data$Region == "Southern Ocean Region", "AAD / CSIRO", "CSIRO"))
        
      )
    })
    
    observeEvent({input$Info == 6}, {
      output$PDataTable <- DT::renderDT(
        pkg.env$SpInfoP, 
        options = list(
          pageLength = 250))
    })
    
    observeEvent({input$Info == 7}, {
      output$ZDataTable <- DT::renderDT(
        pkg.env$SpInfoZ, 
        options = list(
          pageLength = 250))
    })
    
  })
}

## To be copied in the UI
# mod_info_ui("Info_1")

## To be copied in the server
# mod_info_server("Info_1")
