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
                           shiny::HTML("
                             <h3>The App</h3>
                             <h4>Does the app use real time data?</h4>
                             <p>No, to keep the App efficient the data is harvested from the AODN monthly and pre-wrangled.</p>
                             <h4>Where is the data behind the APP available from?</h4>
                             <p>All the non-genomic data is accesible through direct download from the AODN portal or
                             by using the pr_get_xxx functions in planktonr. The raw genomic data is available through AMB data portal,
                             the calculated indices are available through the Australian Microbiome, Microbial Ocean Atlas GitHub page.</p>
                             <ul>
                               <li>AODN Portal <a href = https://portal.aodn.org.au/ target = _blank> Website</a></li>
                               <li>AMB portal <a href = https://data.bioplatforms.com/organization/about/australian-microbiome target = _blank> Website</a></li>
                               <li>AMB github <a href = https://github.com/AusMicrobiome/microbial_ocean_atlas target = _blank> Website</a></li>
                             </ul>
                             <h4>How can I make these figures myself?</h4>
                             <p>Download the data and make your own plots using planktonr. 
                             Vignettes are available in planktonr to help you navigate the data and package</p>
                             <h4>How can I access planktonr?</h4>
                             <p>Via R studio use remotes::install_github('PlanktonTeam/planktonr', force = TRUE)</p>
                             <h4>How can I get more details on how these figures were put together?</h4>
                             <p>Click on the 'R Code Example' button to follow a link to vignettes detailing the process</p>
                             <h4>How can I get more information on the projects used in this App?</h4>
                             <p>Click on the references tab in this section to find out more background on the projects</p>
                           ")
                         )
                ),
                tabPanel("Technical Information", value = 2,
                         bslib::accordion(id = "samplingAccordion_details", 
                                          bslib::accordion_panel(
                                            title = shiny::HTML("<h3>Raise an issue</h3>"),
                                            shiny::HTML("If you find an error or something is not explained, please raise an issue in our 
                                            repo: <a href='https://github.com/PlanktonTeam/BioOceanObserver/issues' target='_blank'>issues</a>"))
                                          ,
                                          bslib::accordion_panel(
                                            title = shiny::HTML("<h3>Data Wrangling</h3>"),
                                            shiny::HTML("<h4>Depths</h4>
                              <p>Depths have been binned for ease of plotting. Water column samples for the NRS stations have been removed, 
                              these were taken from the beginning of the program until July 2017.
                              The samples taken at defined depths have been included here as they are more informative. All data can be sourced 
                              direct from the AODN or by using planktonr</p>
                              <h4>Contour plots</h4>
                              <p>There is an option to interpolate the data in the contour plots used for the environmental data taken at defined depths. 
                              The base method does not fill in NAs in the dataset. 
                              Use the option na.fill = TRUE if you want to interpolate.</p>
                              <h4>Outliers</h4>
                              <p>Outliers are removed when they are greater than 2 standard deviations from the mean. Negative values for nutrients etc. are also set 
                              to 0. These options are used in BOO but if you want to keep the outliers you can download the data and plot the figures using 
                              planktonr with your chosen settings</p>
                              <h4>Assigning bioregions to CPR samples</h4>
                              <p>CPR BioRegions have been selected using nearest neighbour. If there is a requriement for only bioregions 
                               to be assigned when they are in the exact region, this can be done with the function in planktonr. 
                               We use the pre-defined bioregions for the CPR samples (see State of the Environment 2016 website) except 
                               for the Southern Ocean bioregion which has been determined by the SO_CPR project boundaries</p>")
                                            ),
                                          bslib::accordion_panel(
                                            title = shiny::HTML("<h3>Statistical analysis</h3>"),
                                            shiny::HTML("<h4>Anomalies</h4>
                              <p>Anomalies are based against the mean value across teh 15 years of available data. This will be reassessed as the length of the time series increases</p>
                              <h4>Models for the time series trends</h4>
                              <p>These are based on a linear model where the value, i.e. abundance, is the response variable and the sample year and month are predictors. 
                              The month term is wrapped in a harmonic function to ensure that the cyclic nature of months is accounted for, i.e. Jan 2023 follows Dec 2022 </p>")
                                            )

                           )
                ),
                tabPanel("ChangeLog", value = 3,
                         shiny::fluidPage(
                           div(class = "references",
                           shiny::HTML("
                            <h3>Changelog for the Biological Ocean Observer</h3>
                            <h4>July 2025</h4>
                            <ul>
                              <li>Biological Ocean Observer updated to include automatic data upload and restyled as per the IMOS website</li>
                            </ul>
                            <h4>April 2025</h4>
                            <ul>
                              <li>Biological Ocean Observer updated to include Victorian Bonney Mooring (VBM)</li>
                            </ul>
                            <h4>July 2023</h4>
                            <ul>
                              <li>Biological Ocean Observer released at Australian Marine Sciences Conference</li>
                            </ul>
                            <h4>March 2023</h4>
                            <ul>
                              <li>Add Relationships Page</li>
                            </ul>
                            <h4>February 2023</h4>
                            <ul>
                              <li>Initial release Version uploaded</li>
                            </ul>
                            <h4>November 2022</h4>
                            <ul>
                              <li>Add Larval Fish Data</li>
                              <li>Add Picoplankton</li>
                              <li>Add Information Page</li>
                            </ul>
                            <h4>August 2022</h4>
                            <ul>
                              <li>Add Moorings</li>
                            </ul>
                            <h4>July 2022</h4>
                            <ul>
                              <li>Add CTD Data</li>
                            </ul>
                            <h4>January 2022</h4>
                            <ul>
                              <li>Add Essential Ocean Variables</li>
                            </ul>
                            <h4>December 2021</h4>
                            <ul>
                              <li>Add Microbial Data</li>
                            </ul>
                            <h4>September 2021</h4>
                            <ul>
                              <li>Add Phytoplankton Data</li>
                              <li>Add Nutrients</li>
                              <li>Add Pigments</li>
                            </ul>
                            <h4>July 2021</h4>
                            <ul>
                              <li>Initial Prototype developed</li>
                              <li>Add Zooplankton Data</li>
                            </ul>
                          ")
                           )
                         )
                ),
                tabPanel("References", value = 4,
                             shiny::fluidPage(
                           div(class = "references",
                           shiny::HTML("
                             <h3>References</h3>
                             <h5>To further understand the data, collection methods etc.</h5>
                             <h4>National Reference Stations</h4>
                             <ul>
                               <li>Davies, CH., Sommerville, E. (Eds.) (2017). <em>National Reference Stations Biogeochemical Operations Manual</em>. Version 3.3.1. Integrated Marine Observing System. DOI:10.26198/5c4a56f2a8ae3. <a href = http://dx.doi.org/10.26198/5c4a56f2a8ae3 target = _blank> Website</a>.</li>
                               <li>Eriksen RS, Davies CH, Bonham P, Coman FE, Edgar S, McEnnulty FR, McLeod D, Miller MJ, Rochester W, Slotwinski A, Tonks ML, Uribe-Palomino J and Richardson AJ (2019). <em>Australia's Long-Term Plankton Observations: The Integrated Marine Observing System National Reference Station Network</em>. Front. Mar. Sci. 6:161. doi: 10.3389/fmars.2019.00161. <a href = https://www.frontiersin.org/articles/10.3389/fmars.2019.00161/full target = _blank> Website</a>.</li>
                               <li>IMOS National Reference Stations website. <a href = https://imos.org.au/facilities/nationalmooringnetwork/nrs target = _blank> Website</a>.</li>
                               </ul>
                             <h4>Continuous Plankton Recorder</h4>
                             <ul>
                               <li>IMOS Continuous PLankton Recorder Survey website. <a href = https://imos.org.au/facilities/shipsofopportunity/auscontinuousplanktonrecorder target = _blank> Website</a>.</li>
                               <li>A.J. Richardson, A.W. Walne, A.W.G. John, T.D. Jonas, J.A. Lindley, D.W. Sims, D. Stevens, M. Witt, (2006). <em>Using continuous plankton recorder data</em>. Progress in Oceanography, 68.1, doi: 10.1016/j.pocean.2005.09.011. <a href = https://www.sciencedirect.com/science/article/pii/S0079661105001424?via%3Dihub target = _blank> Website</a>.</li>
                             </ul>
                             <h4>Australian Microbiome Initiative</h4>
                             <ul>
                             <li>Australian MIcrobiome Scientific Manual. <a href = https://confluence.csiro.au/display/ASM/Ausmicrobiome+Scientific+Manual target = _blank> Website</a>.</li>
                             </ul>
                             <h4>Souther Ocean Timeseries Mooring</h4>
                             <ul>
                             </ul>
                             <h5>These references are also good resources for visualising this data</h5>
                             <ul>
                               <li>Richardson A.J, Eriksen R,S, Moltmann T, Hodgson-Johnston I, Wallis J.R. (2020). <em>State and Trends of Australia's Ocean Report</em>. Integrated Marine Observing System (IMOS). <a href = https://www.imosoceanreport.org.au/about/ target = _blank> Website</a>.</li>
                               <li>Richardson A.J, Eriksen R.S, Rochester, W. (2015) <em>Plankton 2015: State of Australia's Oceans</em>. CSIRO report. ISBN 978-1-4863-0566-7 (EPDF). <a href = https://imos.org.au/fileadmin/user_upload/shared/Data_Tools/15-00245_OA_Plankton2015_20ppBrochure_WEB_151116.pdf target = _blank> Website</a>.</li>
                               <li>Brown, M., van de Kamp, J., Ostrowski, M. <em>et al</em>. (2018). Systematic, <em>continental scale temporal monitoring of marine pelagic microbiota by the Australian Marine Microbial Biodiversity Initiative</em>. Sci Data 5, 180130. doi.org/10.1038/sdata.2018.130 <a href = https://doi.org/10.1038/sdata.2018.130 target = _blank> Website</a>.</li>
                             </ul>
                             <h3>Package citations</h3>
                             <h5>This Web App is possible thanks to the following packages and the active R community</h5>
                             <ul>
                               <li>Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). <em>shiny: Web Application Framework for R</em>. R package version 1.7.2, <a href = https://CRAN.R-project.org/package=shiny target = _blank> Website</a>.</li>
                               <li>Fay C, Guyader V, Rochette S, Girard C (2022). <em>golem: A Framework for Robust Shiny Applications</em>. R package version 0.3.3, <a href = https://CRAN.R-project.org/package=golem target = _blank> Website</a>.</li>
                               <li>Wickham H, François R, Henry L, Müller K (2022). <em>dplyr: A Grammar of Data Manipulation</em>. R package version 1.0.10, <a href = https://CRAN.R-project.org/package=dplyr target = _blank> Website</a>.</li>
                               <li>Wickham H (2016). <em>ggplot2: Elegant Graphics for Data Analysis</em>. Springer-Verlag New York, <a href = https://ggplot2.tidyverse.org target = _blank> Website</a>.</li>
                               <li>Garrett Grolemund, Hadley Wickham (2011). <em>Dates and Times Made Easy with lubridate.</em>, Journal of Statistical Software, 40(3), 1-25. <a href = https://www.jstatsoft.org/v40/i03/ target = _blank> Website</a>.</li>
                               <li>Bache S, Wickham H (2022). <em>magrittr: A Forward-Pipe Operator for R</em>. R package version 2.0.3, <a href = https://CRAN.R-project.org/package=magrittr target = _blank> Website</a>.</li>
                               <li>Pedersen T (2020). <em>patchwork: The Composer of Plots</em>. R package version 1.1.1, <a href = https://CRAN.R-project.org/package=patchwork target = _blank> Website</a>.</li>
                               <li>Henry L, Wickham H (2020). <em>purrr: Functional Programming Tools</em>. R package version 0.3.4, <a href = https://CRAN.R-project.org/package=purrr target = _blank> Website</a>.</li>
                               <li>Henry L, Wickham H (2022). <em>rlang: Functions for Base Types and Core R and 'Tidyverse' Features</em>. R package version 1.0.4, <a href = https://CRAN.R-project.org/package=rlang target = _blank> Website</a>.</li>
                               <li>South A (2017). <em>rnaturalearth: World Map Data from Natural Earth</em>. R package version 0.1.0, <a href = https://CRAN.R-project.org/package=rnaturalearth target = _blank> Website</a>.</li>
                               <li>Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal 10 (1), 439-446, <a href = https://doi.org/10.32614/RJ-2018-009 target = _blank> Website</a>.</li>
                               <li>Attali, D, Edwards T (2021). <em>shinyalert: Easily Create Pretty Popup Messages (Modals) in 'Shiny'</em>. R package version 3.0.0, <a href = https://cran.r-project.org/web/packages/shinyalert/index.html target = _blank> Website</a>.</li>
                               <li>Sali A, Attali D (2020). <em>shinycssloaders: Add Loading Animations to a 'shiny' Output While It's Recalculating</em>. R package version 1.0.0, <a href = https://CRAN.R-project.org/package=shinycssloaders target = _blank> Website</a>.</li>
                               <li>Chang W, Borges Ribeiro B (2021). <em>shinydashboard: Create Dashboards with 'Shiny'</em>. R package version 0.7.2, <a href = https://CRAN.R-project.org/package=shinydashboard target = _blank> Website</a>.</li>
                               <li>Attali D (2021). <em>shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds</em>. R package version 2.1.0, <a href = https://CRAN.R-project.org/package=shinyjs target = _blank> Website</a>.</li>
                               <li>Wickham H (2019). <em>stringr: Simple, Consistent Wrappers for Common String Operations</em>. R package version 1.4.0, <a href = https://CRAN.R-project.org/package=stringr target = _blank> Website</a>.</li>
                               <li>Müller K, Wickham H (2022). <em>tibble: Simple Data Frames</em>. R package version 3.1.8, <a href = https://CRAN.R-project.org/package=tibble target = _blank> Website</a>.</li>
                               <li>Wickham H, Girlich M (2022). <em>tidyr: Tidy Messy Data</em>. R package version 1.2.0, <a href = https://CRAN.R-project.org/package=tidyr target = _blank> Website</a>.</li>
                               <li>Henry L, Wickham H (2022). <em>tidyselect: Select from a Set of Strings</em>. R package version 1.1.2, <a href = https://CRAN.R-project.org/package=tidyselect target = _blank> Website</a>.</li>
                             </ul>
                             <br>
                             <br>
                           ")
                           )
                         ),
                ),
                tabPanel("Sampling Details", value = 5,
                         bslib::accordion(id = "samplingAccordion_details", 
                                          bslib::accordion_panel(
                                                   title = shiny::HTML("<h3>National Reference Stations</h3>"),
                                                   shiny::HTML("
                                                      <p>Note: Ningaloo and Esperance only operated for 3 years and were only sampled seasonally. 
                                                      The data is sparse for these stations and has often been removed for some analysis.</p>
                                                      <p>Note: The NRS sampling period goes from mid 2009 until present. Prior to this some environmental parameters 
                                                      were also collected at the Long Term Monitoring Stations - ROT, MAI, PH4. These are shown on the Long Term 
                                                      Monitoring tab under EOVs. Generally though in this APP we concentrate on the visualisation of the NRS period 
                                                      where more parameters have been collected in a consistent manner across stations</p>
                                                      <br><br>"
                                                           ),
                                                      DT::DTOutput(nsInfo("NRSDataTable")),
                                                      div(
                                                        class = "key-data-banner",
                                                        h3("Key Data Streams"),
                                                        fluidRow(
                                                          column(1, actionButton(nsInfo("left"), "<")),
                                                          column(10, uiOutput(nsInfo("imageGallery"))),
                                                          column(1, actionButton(nsInfo("right"), ">"))
                                                        )
                                                      )
                                      ),
                                      bslib::accordion_panel(
                                        title = shiny::HTML("<h3>Continuous Plankton Recorder</h3>"),
                                                      shiny::HTML("Put some info in here"),
                                                      DT::DTOutput(nsInfo("CPRDataTable")),
                                                      shiny::HTML("<br><br>"),
                                                      div(
                                                        class = "key-data-banner",
                                                        h3("Key Data Streams"),
                                                        tags$ul(
                                                          class = "key-data-streams",
                                                          tags$li(img(src = "www/csm_PCI.png")),
                                                          tags$li(actionLink(nsInfo("go_phyto"), label = tags$img(src = "www/csm_Phytoplankton.png"))),
                                                          tags$li(img(src = "www/csm_Zooplankton.png"))
                                                        )
                                                      )
                                      ),
                         bslib::accordion_panel(
                           title = shiny::HTML("<h3>Southern Ocean Timeseries Mooring</h3>"),
                           shiny::HTML("Put some info in here"),
                           shiny::HTML("<br><br>"),
                           div(
                             class = "key-data-banner",
                             h3("Key Data Streams"),
                             tags$ul(
                               class = "key-data-streams",
                               tags$li(img(src = "www/csm_Phytoplankton.png"))
                             )
                           )
                         ),
                         bslib::accordion_panel(
                           title = shiny::HTML("<h3>Australian Microbiome Initiative</h3>"),
                           shiny::HTML("Put some info in here"),
                           shiny::HTML("<br><br>"),
                           div(
                             class = "key-data-banner",
                             h3("Key Data Streams"),
                             tags$ul(
                               class = "key-data-streams",
                               tags$li(img(src = "www/csm_eDNA.png")),
                               tags$li(img(src = "www/csm_eDNA.png"))
                             )
                           )
                         )
                         )
                ),
                tabPanel("Phytoplankton Species Details", value = 6, 
                         shiny::fluidPage(
                           shiny::HTML("<h3>Phytoplankton Species Information</h3>"),
                           DT::DTOutput(nsInfo("PDataTable"))
                         )
                ),
                tabPanel("Zooplankton Species Details", value = 7, 
                         shiny::fluidPage(
                           shiny::HTML("<h3>Zooplankton Species Information</h3>"),
                           DT::DTOutput(nsInfo("ZDataTable"))
                         )
                ),
    )
  )
}

#' Help Server Functions
#'
#' @noRd 
mod_info_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ## if entry on mod_info changes from EOV click
    
    observeEvent(input$go_phyto, {
      updateTabsetPanel(session, inputId = "phyto", selected = "ptscpr")
    })
    
    observeEvent({input$Info == 5}, {
      # banner of variables for NRS

     img_paths_nrs <- c(
        "www/csm_Chlorophyll.png",
        "www/csm_Phytoplankton.png",
        "www/csm_Zooplankton.png",
        "www/csm_Larval_Fish.png",
        "www/csm_Pigments.png",
        "www/csm_Macronutrients.png",
        "www/csm_eDNA.png",
        "www/csm_Salinity.png",
        "www/csm_Temperature.png",
        "www/csm_Turbidity.png"
      )
      
      # Reactive index
      startIndex <- reactiveVal(1)
      
      # Update index on button click
      observeEvent(input$left, {
        newIndex <- max(1, startIndex() - 4)
        startIndex(newIndex)
      })
      
      observeEvent(input$right, {
        newIndex <- min(length(img_paths_nrs) - 3, startIndex() + 4)
        startIndex(newIndex)
      })
      
      # Render image boxes
      output$imageGallery <- renderUI({
        idx <- startIndex()
        selected <- img_paths_nrs[idx:min(idx + 3, length(img_paths_nrs))]
        
        fluidRow(
          lapply(selected, function(path) {
            column(
              width = 3,
              div(class = "image-box",
                  img(src = path)
              )
            )
          })
        )
      })
      
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
