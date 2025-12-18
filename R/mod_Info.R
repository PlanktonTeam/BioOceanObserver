#' Help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_info_ui <- function(id) {
  nsInfo <- NS(id)
  shiny::tagList(
    tabsetPanel(
      id = "info", type = "pills",
      tabPanel("Technical Information",
        value = 2,
        shiny::HTML("<br>
        <p>This is a visualisation tool primarily to give users an overview of the data and information available. We have made some decisions in the background to make these visualisations
        simple and effective, whilst retaining as much flexibility as possible. There are several legitimate ways to display data and to conduct analysis, what we are showing 
        is just one way. Under the following tabs we document our decisions but explain how you can access the raw data, with ease, to analyse as you wish.</p> <br>"),
        bslib::accordion(
          id = nsInfo("samplingAccordion_tech"),
          open = character(0), # ensures all items are closed on render
          bslib::accordion_panel(
            title = shiny::HTML("<h3>Raise an issue</h3>"),
            value = "issue",
            shiny::HTML("If you find an error or have a query, please raise an issue in our
                        repo: <a href='https://github.com/PlanktonTeam/BioOceanObserver/issues' target='_blank'>issues</a>")
          ),
          bslib::accordion_panel(
            title = shiny::HTML("<h3>Data wrangling</h3>"),
            value = "wrangling",
            shiny::HTML("<h4>Depths</h4>
            <p>Depths have been binned for ease of plotting. The water column samples which were taken from the beginning of the program until July 2017
            for samples collected using the Niskin bottles at the NRS stations have been removed, Only the samples taken at defined depths have been included here
            as they are more informative. The complete data, including water column samples, can be sourced direct from the AODN or by using planktonr</p>
            <p> For temperature and salinity there are data at more depths available for the SOTS than are plotted here. To access the full data set go to the
            AODN thredds server to download. <a href = https://thredds.aodn.org.au/thredds/catalog/IMOS/DWM/catalog.html  target = _blank> Website</a> </P>
            <h4>Contour plots</h4>
            <p>There is an option to interpolate the data in the contour plots used for the environmental data taken at defined depths.
            The contouring uses the metR::geom_contour_filled function and if interpolation is selected, this is performed by metR::Impute2D.
            The default method is to do simple linear interpolation in x and y and average the result.</p>
            <h4>Outliers</h4>
            <p>Outliers are removed when they are greater than 2 standard deviations from the mean. Negative values for nutrients etc. are also set
            to 0. These options are used here for visualisations. The full dataset, including outliers, can be downloaded and plotted using
            planktonr with your chosen settings.</p>
            <h4>Flags</h4>
            <p>Only data flagged 1 or 2 is used in the visualisations. If you are interested in using data that is potentially good, this can be accessed through the planktonr functions.<p>
            <h4>Assigning bioregions to CPR samples</h4>
            <p>CPR BioRegions have been selected using nearest neighbour. If there is a requriement for only bioregions
            to be assigned when they are in the exact region, this can be done with the function in planktonr.
            We use the pre-defined bioregions for the CPR samples (see State of the Environment 2016 website) except
            for the Southern Ocean bioregion which has been determined by the SO_CPR project boundaries</p>")
          ),
          bslib::accordion_panel(
            title = shiny::HTML("<h3>Statistical analysis</h3>"),
            value = "stats",
            shiny::HTML("<h4>Anomalies</h4>
            <p>Anomalies are based against the mean value across the 15 years of available data.
            This will be reassessed as the length of the time series increases</p>
            <h4>Models for the time series trends</h4>
            <p>These are based on a linear model where the value, i.e. abundance, is the response variable and the sample year and month are predictors.
            The month term is wrapped in a harmonic function to ensure that the cyclic nature of months is accounted for, i.e. Jan 2023 follows Dec 2022.</p>
            <p>The overall model significance and R squared values are also extracted from the model output and displayed on the plots.</p>")
            ),
          bslib::accordion_panel(
            title = shiny::HTML("<h3>FAQs</h3>"),
            value = "faq",
            shiny::HTML("<h4>Does the app use real time data?</h4>
            <p>No, to keep the App efficient the data is harvested from the AODN monthly and pre-wrangled.</p>
            <h4>Where is the data behind the APP available from?</h4>
            <p>All the non-genomic data is accesible through direct download from the AODN portal, AODN Thredds server or
            by using the pr_get_xxx functions in planktonr. The raw genomic data is available through AMB data portal,
            the calculated indices are available through the Australian Microbiome Initiative, Microbial Ocean Atlas GitHub page.</p>
            <ul>
            <li>AODN Portal <a href = https://portal.aodn.org.au/ target = _blank> Website</a></li>
            <li>AMI portal <a href = https://data.bioplatforms.com/organization/about/australian-microbiome target = _blank> Website</a></li>
            <li>AMI github <a href = https://github.com/AusMicrobiome/microbial_ocean_atlas target = _blank> Website</a></li>
            </ul>
            <h4>How can I make these figures myself?</h4>
            <p>Download the data and make your own plots using planktonr.
            Vignettes are available in planktonr to help you navigate the data and package</p>
            <h4>How can I access planktonr?</h4>
            <p>Via R studio use remotes::install_github('PlanktonTeam/planktonr', force = TRUE)</p>
            <h4>How can I get more details on how these figures were put together?</h4>
            <p>Click on the 'R Code Example' button to follow a link to vignettes detailing the process</p>
            <h4>How can I get more information on the projects used in this App?</h4>
            <p>Click on the references tab in this section to find out more background on the projects</p>")
          ),
          bslib::accordion_panel(
            title = shiny::HTML("<h3>Change log</h3>"),
            value = "change",
            shiny::HTML("<h4>Changelog for the Biological Ocean Observer</h4>
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
            </ul>")
          )
        )
      ),
      tabPanel("References",
        value = 4,
        bslib::accordion(
          id = nsInfo("samplingAccordion_refs"),
          open = character(0), # ensures all items are closed on render
          bslib::accordion_panel(
            # div(class = "references",
            title = shiny::HTML("<h3>References</h3>"),
            value = "refs",
            shiny::HTML("<h5>To further understand the data, collection methods etc.</h5>
            <h4>National Reference Stations</h4>
            <ul>
            <li>Davies, CH., Sommerville, E. (Eds.) (2017). <em>National Reference Stations Biogeochemical Operations Manual</em>. Version 3.3.1. Integrated Marine Observing System. DOI:10.26198/5c4a56f2a8ae3. <a href = http://dx.doi.org/10.26198/5c4a56f2a8ae3 target = _blank> Website</a>.</li>
            <li>Eriksen RS, Davies CH, Bonham P, Coman FE, Edgar S, McEnnulty FR, McLeod D, Miller MJ, Rochester W, Slotwinski A, Tonks ML, Uribe-Palomino J and Richardson AJ (2019). <em>Australia's Long-Term Plankton Observations: The Integrated Marine Observing System National Reference Station Network</em>. Front. Mar. Sci. 6:161. doi: 10.3389/fmars.2019.00161. <a href = https://www.frontiersin.org/articles/10.3389/fmars.2019.00161/full target = _blank> Website</a>.</li>
            <li>IMOS National Reference Stations website. <a href = https://imos.org.au/facilities/nationalmooringnetwork/nrs target = _blank> Website</a>.</li>
            </ul>
            <h4>Continuous Plankton Recorder</h4>
            <ul>
            <li>A.J. Richardson, A.W. Walne, A.W.G. John, T.D. Jonas, J.A. Lindley, D.W. Sims, D. Stevens, M. Witt, (2006). <em>Using continuous plankton recorder data</em>. Progress in Oceanography, 68.1, doi: 10.1016/j.pocean.2005.09.011. <a href = https://www.sciencedirect.com/science/article/pii/S0079661105001424?via%3Dihub target = _blank> Website</a>.</li>
            <li>IMOS Continuous PLankton Recorder Survey website. <a href = https://imos.org.au/facilities/shipsofopportunity/auscontinuousplanktonrecorder target = _blank> Website</a>.</li>
            </ul>
            <h4>Australian Microbiome Initiative</h4>
            <ul>
            <li>Australian Microbiome Scientific Manual. <a href = https://confluence.csiro.au/display/ASM/Ausmicrobiome+Scientific+Manual target = _blank> Website</a>.</li>
            <li>IMOS Marine Microbiome Initiative website. <a href = https://imos.org.au/facility/marine-microbiome-initiative target = _blank> Website</a>.</li>
            <li>Australian Microbiome Initiative website. <a href = https://www.australianmicrobiome.com/ target = _blank> Website</a>.</li>
            </ul>
            <h4>Southern Ocean Time Series</h4>
            <ul>
            <li>Shadwick, E.H, Wynn-Edwards, C.A, Eriksen R,S, Jansen, P, Yang, X, Woodwoard, G, Davies, D. (2025). <em>The Southern Ocean Time Series: a climatological view of hydrography, biogeochemistry, phytoplankton community composition, and carbon export in the Subantarctic Zone</em>. Ocean Sci., 21:4. doi: 10.5194/os-21-1549-2025 <a href = https://os.copernicus.org/articles/21/1549/2025/ target = _blank> website<a/>. </li>
            <li>IMOS Southern Ocean Time Series website. <a href = https://imos.org.au/facility/deep-water-moorings/southern-ocean-time-series-observatory target = _blank> Website</a>.</li>
            </ul>
            <h5>These references are also good resources for visualising this data</h5>
            <ul>
            <li>Richardson A.J, Eriksen R,S, Moltmann T, Hodgson-Johnston I, Wallis J.R. (2020). <em>State and Trends of Australia's Ocean Report</em>. Integrated Marine Observing System (IMOS). <a href = https://www.imosoceanreport.org.au/about/ target = _blank> Website</a>.</li>
            <li>Richardson A.J, Eriksen R.S, Rochester, W. (2015) <em>Plankton 2015: State of Australia's Oceans</em>. CSIRO report. ISBN 978-1-4863-0566-7 (EPDF). <a href = https://imos.org.au/fileadmin/user_upload/shared/Data_Tools/15-00245_OA_Plankton2015_20ppBrochure_WEB_151116.pdf target = _blank> Website</a>.</li>
            <li>Brown, M., van de Kamp, J., Ostrowski, M. <em>et al</em>. (2018). Systematic, <em>continental scale temporal monitoring of marine pelagic microbiota by the Australian Marine Microbial Biodiversity Initiative</em>. Sci Data 5, 180130. doi.org/10.1038/sdata.2018.130 <a href = https://doi.org/10.1038/sdata.2018.130 target = _blank> Website</a>.</li>
            </ul>")
            # )
          ),
          bslib::accordion_panel(
            # div(class = "references",
            title = shiny::HTML("<h3>Package citations</h3>"),
            value = "cites",
            shiny::HTML("<h5>This Web App is possible thanks to the following packages and the active R community</h5>
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
            <br> <br>")
            # )
          )
        )
      ),
      tabPanel("Sampling Details",
        value = 5,
        bslib::accordion(
          id = nsInfo("samplingAccordion_details"),
          open = character(0), # ensures all items are closed on render
          bslib::accordion_panel(
            title = shiny::HTML("<h3>National Reference Stations</h3>"),
            value = "nrs",
            shiny::HTML("<p>Regular biogeochemical sampling is conducted at 8 National Reference Stations (nrs) across Australia. These are mostly collected on a
            monthly basis and are continuous since 2009. Data from these NRS are used alongside other reference station style datasets in this App. Details
            of the sampling are in the table below, or for more details, click the References tab. The NRS sampling period goes from mid 2009 until present. Prior to this some environmental parameters
            were also collected at the Long Term Monitoring Stations - ROT, MAI, PH4. These are shown on the Long Term Monitoring tab under EOVs. Generally
            though in this APP we concentrate on the visualisation of the NRS period where more parameters have been collected in a consistent manner across stations</p>
            <p>Note: Ningaloo and Esperance only operated for 3 years and were only sampled seasonally. The data is sparse for these stations and has often been removed for some analysis.
            The Victorian Bonney Mooring was only established in 2024, but is included here as it will be ongoing.</p>
            </p> <br>
            <h4>NRS Sampling details</h4>"),
            DT::DTOutput(nsInfo("NRSDataTable")),
            div(
              h4("Key Data Streams"),
              tags$ul(
                class = "image-row",
                img(src = "www/csm_Zooplankton.png", class = "image-box"),
                img(src = "www/csm_Phytoplankton.png", class = "image-box"),
                img(src = "www/csm_Larval_Fish.png", class = "image-box"),
                img(src = "www/csm_eDNA.png", class = "image-box"),
                img(src = "www/csm_Temperature.png", class = "image-box"),
                img(src = "www/csm_Macronutrients.png", class = "image-box"),
                img(src = "www/csm_Pigments.png", class = "image-box"),
                img(src = "www/csm_Salinity.png", class = "image-box")
              )
            )
          ),
          bslib::accordion_panel(
            title = shiny::HTML("<h3>Continuous Plankton Recorder</h3>"),
            value = "cpr",
            shiny::HTML("<p>The majority of the data in this App comes from the Australian Plankton Recorder Survey (AusCPR). The regular survey tows for AusCPR are through GBR 
            and down the East Australian Current across to Adelaide. These tows are collected using Ships of Opportumity (SOOP) and we acknowledge the effort made by these
            companies to help us collect our data: - ANL. Sealord group Ltd., Swores, Laeisz, Wallenius Wilhelmsen, Rio Tinto Marine.</p> 
            <p>Tows are also collected by RSV Nuyina in the austral summer through a collaboration with the AAD and UTAS. The phytoplankton data contains samples counted
            through this UTAS / AAD collaboration and the Southern Ocean zooplankton records are counted through the SO_CPR program at the AAD.</p> 
            <p> Adhoc samples are also collected from RV Investigator and other research vessels.</p><br>"),
            shiny::HTML("<h4>CPR Sampling details</h4>"),
            DT::DTOutput(nsInfo("CPRDataTable")),
            div(
              h4("Key Data Streams"),
              tags$ul(
                class = "image-row",
                img(src = "www/csm_PCI.png", class = "image-box"),
                img(src = "www/csm_Phytoplankton.png", class = "image-box"),
                img(src = "www/csm_Zooplankton.png", class = "image-box")
              )
            )
          ),
          bslib::accordion_panel(
            title = shiny::HTML("<h3>Southern Ocean Timeseries Mooring</h3>"),
            value = "sots",
            shiny::HTML("Put some info in here"),
            shiny::HTML("<br><br>"),
            div(
              h4("Key Data Streams"),
              tags$ul(
                class = "image-row",
                img(src = "www/csm_Temperature.png", class = "image-box"),
                img(src = "www/csm_Phytoplankton.png", class = "image-box"),
                img(src = "www/csm_Salinity.png", class = "image-box")
              )
            )
          )
        ),
        bslib::accordion_panel(
          title = shiny::HTML("<h3>Australian Microbiome Initiative</h3>"),
          value = "am",
          shiny::HTML("Put some info in here"),
          shiny::HTML("<br><br>"),
          div(
            h4("Key Data Streams"),
            tags$ul(
              class = "image-row",
              img(src = "www/csm_eDNA.png")
            )
          )
        )
      ),
      tabPanel("Species Details",
        value = 6,
        bslib::accordion(
          id = nsInfo("samplingAccordion_species"),
          open = character(0), # ensures all items are closed on render
          bslib::accordion_panel(
            title = shiny::HTML("<h3>Phytoplankton Species Information</h3>"),
            value = "p",
            DT::DTOutput(nsInfo("PDataTable"))
          ),
          bslib::accordion_panel(
            title = shiny::HTML("<h3>Zooplankton Species Information</h3>"),
            value = "z",
            DT::DTOutput(nsInfo("ZDataTable"))
          )
        )
      )
    )
  )
}

#' Help Server Functions
#'
#' @noRd
mod_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## make sure accordion buttons are closed on start up

    close_panels <- function(id, values) {
      bslib::accordion_panel_close(id = session$ns(id), value = values)
    }

    session$onFlushed(function() {
      close_panels("samplingAccordion_tech", c("issue", "wrangling", "stats", "change", "faqs"))
      close_panels("samplingAccordion_refs", c("refs", "cites"))
      close_panels("samplingAccordion_details", c("nrs", "cpr", "sots", "am"))
      close_panels("samplingAccordion_species", c("p", "z"))
    }, once = TRUE)


    observeEvent(
      {
        input$Info == 5
      },
      {
        # # banner of variables for NRS 
        # 
        # img_paths_nrs <- c(
        #   "www/csm_Chlorophyll.png",
        #   "www/csm_Phytoplankton.png",
        #   "www/csm_Zooplankton.png",
        #   "www/csm_Larval_Fish.png",
        #   "www/csm_Pigments.png",
        #   "www/csm_Macronutrients.png",
        #   "www/csm_eDNA.png",
        #   "www/csm_Salinity.png",
        #   "www/csm_Temperature.png",
        #   "www/csm_Turbidity.png"
        # )
        # 
        # # Reactive index
        # startIndex <- reactiveVal(1)
        # 
        # # Update index on button click
        # observeEvent(input$left, {
        #   newIndex <- max(1, startIndex() - 3)
        #   startIndex(newIndex)
        # })
        # 
        # observeEvent(input$right, {
        #   newIndex <- min(length(img_paths_nrs) - 2, startIndex() + 3)
        #   startIndex(newIndex)
        # })
        # 
        # # Render image boxes
        # output$imageGallery <- renderUI({
        #   idx <- startIndex()
        #   selected <- img_paths_nrs[idx:min(idx + 2, length(img_paths_nrs))]
        # 
        #   fluidRow(
        #     lapply(selected, function(path) {
        #       column(
        #         width = 4,
        #         div(
        #           class = "image-box",
        #           img(src = path)
        #         )
        #       )
        #     })
        #   )
        # })

        output$NRSDataTable <- DT::renderDT(
          pkg.env$NRSStation %>%
            dplyr::mutate(EndDate = dplyr::case_when(
              .data$StationCode %in% c("NIN", "ESP") ~ "2012-03-01",
              .data$StationCode == "PH4" ~ "2009-02-24"
            )) %>%
            dplyr::select("StationCode":"StationStartDate", "EndDate", dplyr::everything(), -dplyr::any_of("Region")) %>%
            dplyr::rename(
              Code = "StationCode", Station = "StationName", State = "StateCode",
              `Start Date` = "StationStartDate", `End Date` = "EndDate", `Water Depth (m)` = "StationDepth_m",
              `Sampling Effort` = "SamplingEffort", Region = "ManagementRegion"
            )
        )

        output$CPRDataTable <- DT::renderDT(
          pkg.env$datCPRTrip %>%
            dplyr::group_by(.data$Region) %>%
            dplyr::summarise(
              StartDate = min(.data$Year_Local, na.rm = TRUE),
              EndDate = max(.data$Year_Local, na.rm = TRUE),
              MilesTowed = dplyr::n() * 5,
              SamplesCounted = round(dplyr::n() / 4, 0),
              .groups = "drop"
            ) %>%
            dplyr::rename(
              `Start Date` = "StartDate", `End Date` = "EndDate", `Miles Towed` = "MilesTowed",
              `Samples Counted` = "SamplesCounted"
            ) %>%
            dplyr::mutate(
              Project = ifelse(.data$Region == "Southern Ocean Region", "SO-CPR / AusCPR", "AusCPR"),
              Institution = ifelse(.data$Region == "Southern Ocean Region", "AAD / CSIRO", "CSIRO")
            )
        )
      }
    )

    observeEvent(
      {
        input$Info == 2
      },
      {
        open <- character(0)
      }
    )

    observeEvent(
      {
        input$Info == 6
      },
      {
        output$PDataTable <- DT::renderDT(
          pkg.env$SpInfoP,
          options = list(
            pageLength = 10
          )
        )
      }
    )

    observeEvent(
      {
        input$Info == 7
      },
      {
        output$ZDataTable <- DT::renderDT(
          pkg.env$SpInfoZ,
          options = list(
            pageLength = 10
          )
        )
      }
    )
  })
}
