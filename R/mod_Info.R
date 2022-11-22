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
    tabsetPanel(id = "info",# type = "pills",
                tabPanel("Frequently Asked Questions", value = 1,
                         shiny::fluidPage(
                           shiny::h2(shiny::strong()), 
                           shiny::br(),
                           shiny::h3(shiny::strong("App")),
                           shiny::h4("Does the app use real time data?"),
                           shiny::p("No, to keep the App efficient the data is harvested from the AODN monthly and pre-wrangled."),
                           shiny::h4("Where is the data behind the APP available from?"),
                           shiny::p("All the data is accesible through direct download from the AODN portal or
                                    by using the pr_get_xxx functions in planktonr"),
                           shiny::h4("How can I make these figures myself?"),
                           shiny::p("Download the data and make your own plots using planktonr. 
                                    Vignettes are available in planktonr to help you navigate the data and package"),
                           shiny::h4("How can I access planktonr?"),
                           shiny::p("Via R studio use remotes::install_github('PlanktonTeam/planktonr', force = TRUE)"),
                           shiny::h4("How can I get more details on how these figures were put together?"),
                           shiny::p("Click on the R Code Example' button to follow a link to vignettes detailing the process")
                         )
                ),
                tabPanel("Technical Information", value = 2,
                         shiny::fluidPage(
                           shiny::h3("Data binning"),
                           shiny::h4("Depths"),
                           shiny::p("Depths binned for ease of plotting.
                                    Water column samples for the NRS stations have been removed - more information can be found at ...."),
                           shiny::br(),
                           shiny::h3("Outliers"),
                           shiny::p("Outliers are removed when they are greater than 2 standard deviations from the mean. If you want to keep
                                    the outliers you can download the data and plot the figures using planktonr"),
                           shiny::br(),
                           shiny::h3("Assigning bioregions to CPR samples"),
                           shiny::p("CPR BioRegions have been selected using nearest neighbour. If there is a requriement for only bioregions 
                                    to be assigned when they are in the exct region, this can be done with the function in planktonr. 
                                    We use the pre-defined bioregions for the CPR samples (see State of the Environment 2016 website) except 
                                    for the Southern Ocean bioregion which has been determined by the SO_CPR project boundaries")
                         )
                ),
                tabPanel("ChangeLog", value = 3,
                         shiny::fluidPage(
                           shiny::h3("February 2023"),
                           shiny::h5("Initial release Version")
                         )
                ),
                tabPanel("References", value = 4,
                         shiny::fluidPage(
                           shiny::h3("References"),
                           shiny::h5("To further understand the data, collection methods etc."),
                           shiny::HTML("<li>Eriksen RS, Davies CH, Bonham P, Coman FE, Edgar S, McEnnulty FR, McLeod D, Miller MJ, Rochester W, Slotwinski A, Tonks ML, Uribe-Palomino J and Richardson AJ (2019). <em>Australia’s Long-Term Plankton Observations: The Integrated Marine Observing System National Reference Station Network</em>. Front. Mar. Sci. 6:161. doi: 10.3389/fmars.2019.00161. <a href = https://www.frontiersin.org/articles/10.3389/fmars.2019.00161/full target = _blank> Website</a>."),
                           shiny::HTML("<li>A.J. Richardson, A.W. Walne, A.W.G. John, T.D. Jonas, J.A. Lindley, D.W. Sims, D. Stevens, M. Witt, (2006). <em>Using continuous plankton recorder data</em>. Progress in Oceanography, 68.1, doi: 10.1016/j.pocean.2005.09.011. <a href = https://www.sciencedirect.com/science/article/pii/S0079661105001424?via%3Dihub target = _blank> Website</a>."),
                           shiny::HTML("<li>IMOS National Reference Stations website. <a href = https://imos.org.au/facilities/nationalmooringnetwork/nrs target = _blank> Website</a>."),
                           shiny::HTML("<li>IMOS Continuous PLankton Recorder Survey website. <a href = https://imos.org.au/facilities/shipsofopportunity/auscontinuousplanktonrecorder target = _blank> Website</a>."),
                           
                           shiny::h5("These references have used this data"),
                           shiny::HTML("<li>Richardson A.J, Eriksen R, Moltmann T, Hodgson-Johnston I, Wallis J.R. (2020). <em>State and Trends of Australia’s Ocean Report</em>. Integrated Marine Observing System (IMOS). <a href = https://www.imosoceanreport.org.au/about/ target = _blank> Website</a>."),
                           shiny::br(),
                           shiny::br(),
                           shiny::h3("Package citations"),
                           shiny::h5("This Web App is possible thanks to the following packages"),
                           shiny::HTML("<li>Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2022). <em>shiny: Web Application Framework for R</em>. R package version 1.7.2, <a href = https://CRAN.R-project.org/package=shiny target = _blank> Website</a>."),
                           shiny::HTML("<li>Fay C, Guyader V, Rochette S, Girard C (2022). <em>golem: A Framework for Robust Shiny Applications</em>. R package version 0.3.3, <a href = https://CRAN.R-project.org/package=golem target = _blank> Website</a>.<br>"),
                           shiny::HTML("<br>"),
                           shiny::HTML("<li>Wickham H, François R, Henry L, Müller K (2022). <em>dplyr: A Grammar of Data Manipulation</em>. R package version 1.0.10, <a href = https://CRAN.R-project.org/package=dplyr target = _blank> Website</a>."),
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
                           shiny::h3("NRS"),
                           shiny::dataTableOutput(nsInfo("NRSDataTable")),
                           shiny::h3("Zooplankton"),
                           shiny::h5("Zooplankton is collected with a Heron drop net sampling only on the descent, 60cm diameter, 100 micron mesh net. This is a depth integrated sample analysed by light microscopy"),
                           shiny::h3("Phytoplankton"),
                           shiny::h5("Equal volumes of water are collected from all niskin bottles (0-50m) to make up a 1L composite sample for phytoplankton analysis using light microscopy."),
                           shiny::h3("Nutrients & Salinity"),
                           shiny::h5("Duplicate samples are taken from each niskin bottle (0-50m), DAR and YON samples are only 0 - 20m. 
                                     PHB also collects at 75 and 100m. MAI at 60, 70 & 80m"),
                           shiny::h3("Carbon"),
                           shiny::h5("DIC & alkalinity samples are taken from each niskin bottle (0-50m), DAR and YON samples are only 0 - 20m. 
                                     PHB also collects at 60, 75 & 100m. MAI at 70 & 80m"),                         
                           shiny::h3("TSS"),
                           shiny::h5("Triplicate samples (~4L) and a blank of surface water"),                         
                           shiny::h3("Pigments"),
                           shiny::h5("Duplicate samples (~4L) from surface water and the lower WQM (~20m)"),                         
                           shiny::h3("Microbial & Picoplankton"),
                           shiny::h5("Samples (~2L microbes, ~50-100 uL Picoplankton) are taken from each niskin bottle (0-50m), DAR and YON samples are only 0 - 20m. 
                                     PHB also collects at 75 and 100m. MAI at 70 & 80m.")
                           )
                ),
                tabPanel("Phytoplankton Species Details", value = 6, 
                         shiny::h2("Phytoplankton Species Information"),
                         shiny::dataTableOutput(nsInfo("PDataTable")),
                ),
                tabPanel("Zooplankton Species Details", value = 7, 
                         shiny::h2("Zooplankton Species Information"),
                         shiny::dataTableOutput(nsInfo("ZDataTable")),
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
      output$NRSDataTable <- shiny::renderDataTable(
        NRSStation)
    })

        observeEvent({input$Info == 6}, {
      output$PDataTable <- shiny::renderDataTable(
        SpInfoP, 
        options = list(
          pageLength = 250))
    })
    
    observeEvent({input$Info == 7}, {
      output$ZDataTable <- shiny::renderDataTable(
        SpInfoZ, 
        options = list(
          pageLength = 250))
    })
    
  })
}

## To be copied in the UI
# mod_info_ui("Info_1")

## To be copied in the server
# mod_info_server("Info_1")
