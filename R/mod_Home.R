#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id){
  nsHome <- NS(id)
  
  tagList(
    tabsetPanel(id = "home",# type = "pills",
                tabPanel("Welcome", value = 1,
                         shiny::fluidPage(
                           shiny::fluidRow(
                             column(4,
                                    img(src = "www/BOO_Hex.png", width = "95%"),
                                    shiny::hr(style = "border-top: 1px solid #000000;"),
                                    shiny::br(),
                                    shiny::h4("Funded by: "),
                                    div(img(src = "www/IMOS_logo-stacked-Colour.png", width = "95%"), style="text-align: center;"),
                                    shiny::br(),
                                    shiny::br(),
                                    shiny::h4("Supported by: "),
                                    div(img(src = "www/AODN_logo.png", width = "70%"), style="text-align: center;"),
                                    shiny::br(),
                                    shiny::h4("and"),
                                    shiny::br(),
                                    div(img(src = "www/ARDC_logo_RGB.png", width = "70%"), style="text-align: center;"),
                                    shiny::br(),
                                    shiny::br()),
                             column(8,
                                    shiny::h2("The Biological Ocean Observer"),
                                    shiny::HTML("The goal of this site is to Integrate, Analyse and Visualise data collected by the 
                            <a href='https://imos.org.au', target = '_blank'> Integrated Marine Observing System (IMOS)</a>. 
                            We aim to enhance the availability and understanding of biological data and make it 
                            accessible to broader and non-specialist audiences in order to accelerate the next 
                            generation of scientific insights."),
                                    shiny::br(),
                                    shiny::br(),
                                    shiny::HTML("This project is entirely open source, as are all the IMOS data underlying it. All the 
                            code for this tool are freely available on <a href='https://github.com/PlanktonTeam/IMOS_BioOceanObserver', target = '_blank'>GitHub</a>. 
                            We welcome collaborators and pull requests are gratefully accepted. If you encounter a problem with this website, 
                                             please <a href='https://github.com/PlanktonTeam/IMOS_BioOceanObserver/issues', target = '_blank'>log an issue</a>."),
                                    shiny::br(),
                                    shiny::br(),
                                    shiny::HTML("This tool was originally conceived and developed by Dr Jason Everett (UQ/CSIRO/UNSW) 
                            and Claire Davies (CSIRO). Jason is a biological oceanographer and Claire is a plankton 
                            ecologist. Both have a strong interest in open data science and encouraging increased 
                            data uptake to solve real world problems."),
                                    shiny::br(),
                                    shiny::br(),
                                    shiny::HTML("The major categories of data we provide within the app are found across the top bar, and include 
                                 microbial, phytoplankton, zooplankton, larval fish and environment (chemical) parameters. The snapshot and EOV
                                 tab include summary data that may be useful as data overviews for managers and policy makers. Within each 
                                 tab, the data is often designated by sampling regime, which is generally the 
                                 <a href = 'https://imos.org.au/facilities/nationalmooringnetwork/nrs', target = '_blank'> National Reference Stations (NRS)</a>
                                 or the <a href = 'https://imos.org.au/facilities/shipsofopportunity/auscontinuousplanktonrecorder', target = '_blank'> 
                                 Continuous Plankton Recorder (CPR)</a>. Due to the spatial nature of the CPR data, these data are summarised by 
                                 <a href = 'https://www.imosoceanreport.org.au/australias-oceans/', target = '_blank'> Australia's Marine Bioregions</a>.
                                 More information about the methods used in this tool, can be found in 'Technical Information' under the 'Information' tab."),
                                    shiny::h3("Citation"),
                                    shiny::HTML("If you use this app in any publication, please cite as: <br> <i>'Davies, Claire; Everett, Jason; Ord, Louise (2022): IMOS Biological Ocean Observer - Shiny APP. v3. CSIRO. Service Collection. <a href = http://hdl.handle.net/102.100.100/447365?index=1>http://hdl.handle.net/102.100.100/447365?index=1</a></i>."),
                                    shiny::br(),
                                    shiny::br(),
                                    shiny::HTML("All of the analysis and plotting contained in this application are powered by the <i>planktonr</i> package: <br>
                                      <i>Everett J, Davies C (2022). planktonr: Analysis and visualisation of plankton data. R package version 0.1.1.0000, <a href = 'https://github.com/PlanktonTeam/planktonr', target = '_blank'>https://github.com/PlanktonTeam/planktonr</a>.</i>"),
                                    shiny::h3("Acknowledging IMOS Data"),
                                    shiny::HTML("This application is developed using IMOS data, and therefore you are also required to <a href = 'https://imos.org.au/acknowledging-us', target = '_blank'> clearly acknowledge</a> the source material by including the following statement in any publications:"),
                                    shiny::br(),
                                    shiny::HTML("'<i>Data was sourced from Australia’s Integrated Marine Observing System (IMOS) – IMOS is enabled by the National Collaborative Research Infrastructure Strategy (NCRIS). It is operated by a consortium of institutions as an unincorporated joint venture, with the University of Tasmania as Lead Agent'.</i>"),
                                    shiny::br(),
                                    shiny::h3("Licencing"),
                                    shiny::HTML("The code for this application is published under an <a href = 'https://github.com/PlanktonTeam/IMOS_BioOceanObserver/blob/master/LICENSE', target = '_blank'> MIT licence</a>."),
                                    shiny::br(),
                                    shiny::br()
                             )))),
                tabPanel("Sampling Progress", value = 2,
                         leaflet::leafletOutput(nsHome("progplot"), height = 800) %>% 
                           shinycssloaders::withSpinner()
                ),
                tabPanel("Sampling Status", value = 3,
                         plotOutput(nsHome("gantt"), height = 700) %>% 
                           shinycssloaders::withSpinner(color="#0dc5c1")
                ),
                
                tabPanel("Sampling Summary", value = 4,
                         # Add plots to highlight species etc.
                         shiny::h3("What functional groups do we commonly see?"),
                         # shiny::fluidRow(
                         # shiny::column(2),
                         # shiny::column(8, 
                         plotOutput(nsHome("TaxaPie")) %>% 
                           shinycssloaders::withSpinner(color="#0dc5c1"),
                         # shiny::column(2),
                         shiny::h3("How many taxa are we identifying?"),
                         plotOutput(nsHome("SpAccum"), height = 700) %>% 
                           shinycssloaders::withSpinner(color="#0dc5c1"),
                         shiny::br(),
                         shiny::br(),  
                         shiny::h3("Fun Facts"),
                         shiny::h6("(Refresh the page for a different one)"),
                         shiny::textOutput(nsHome("Fact")),
                         shiny::br(),
                         shiny::br(),  
                         shiny::h3("IMOS Plankton Paper"),
                         shiny::h6("(Refresh the page for a different one)"),
                         shiny::textOutput(nsHome("Paper")),
                         shiny::br(),
                         shiny::br()
                         
                ),
                
    ) 
    
  )
}

#' Home Server Functions
#'
#' @noRd 
mod_home_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$nsHome
    
    
    observeEvent({input$home == 2}, {
      output$progplot <- leaflet::renderLeaflet({
        planktonr::pr_plot_ProgressMap(PMapData2, interactive = TRUE)
      })
    })
    
    
    observeEvent({input$home == 3}, {
      
      output$gantt <- shiny::renderPlot({
        ggCPR <- planktonr::pr_plot_Gantt(datCPRTrip, Survey = "CPR")
        ggNRS <- planktonr::pr_plot_Gantt(datNRSTRip, Survey = "NRS")
        
        p <- patchwork::wrap_plots(ggCPR, ggNRS, ncol = 1) &
          ggplot2::theme(text = ggplot2::element_text(size = 16, face = "bold"))
        return(p)
      })
    })
    
    
    observeEvent({input$home == 4}, {
      
      output$SpAccum <- shiny::renderPlot({
        p1 <- planktonr::pr_plot_TaxaAccum(PSpNRSAccum, Survey = "NRS", Type = "P")
        p2 <- planktonr::pr_plot_TaxaAccum(ZSpNRSAccum, Survey = "NRS", Type = "Z")
        p3 <- planktonr::pr_plot_TaxaAccum(PSpCPRAccum, Survey = "CPR", Type = "P")
        p4 <- planktonr::pr_plot_TaxaAccum(ZSpCPRAccum, Survey = "CPR", Type = "Z")
        
        p <- patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2) &
          ggplot2::theme(text = ggplot2::element_text(size = 16, face = "bold"))
        return(p)
      })
      
      output$Paper <- shiny::renderText({
        planktonr::pr_get_Papers()
      })
      
      output$Fact <- shiny::renderText({
        planktonr::pr_get_Facts()
      })
      
      output$TaxaPie <- shiny::renderPlot({
        
        p1 <- ggplot2::ggplot(data = NRSfgp %>% dplyr::group_by(Parameters) %>% dplyr::summarise(mean = mean(Values, na.rm = TRUE)), 
                              ggplot2::aes(x = "", y = mean, fill = Parameters)) +
          ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
          ggplot2::coord_polar("y", start = 0) +
          ggplot2::theme_void() +  # remove background, grid, numeric labels
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
          ggplot2::scale_fill_brewer(palette = "Set1") +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "Phytoplankton", nrow = 2, title.position = "top", title.hjust = 0.5, title.theme = ggplot2::element_text(face = "bold"))) +
          ggplot2::ggtitle("NRS")
        
        p2 <- ggplot2::ggplot(data = CPRfgp %>% dplyr::group_by(Parameters) %>% dplyr::summarise(mean = mean(Values, na.rm = TRUE)), 
                              ggplot2::aes(x = "", y = mean, fill = Parameters)) +
          ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
          ggplot2::coord_polar("y", start = 0) +
          ggplot2::theme_void() +  # remove background, grid, numeric labels
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
          ggplot2::scale_fill_brewer(palette = "Set1") +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "Phytoplankton", nrow = 2, title.position = "top", title.hjust = 0.5, title.theme = ggplot2::element_text(face = "bold"))) +
          ggplot2::ggtitle("CPR")
        
        p3 <- ggplot2::ggplot(data = NRSfgz %>% dplyr::group_by(Parameters) %>% dplyr::summarise(mean = mean(Values, na.rm = TRUE)), 
                              ggplot2::aes(x = "", y = mean, fill = Parameters)) +
          ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
          ggplot2::coord_polar("y", start = 0) +
          ggplot2::theme_void() +  # remove background, grid, numeric labels
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
          ggplot2::scale_fill_brewer(palette = "Set1") +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "Zooplankton", nrow = 2, title.position = "top", title.hjust = 0.5, title.theme = ggplot2::element_text(face = "bold"))) +
          ggplot2::ggtitle("NRS")
        
        p4 <- ggplot2::ggplot(data = CPRfgz %>% dplyr::group_by(Parameters) %>% dplyr::summarise(mean = mean(Values, na.rm = TRUE)), 
                              ggplot2::aes(x = "", y = mean, fill = Parameters)) +
          ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
          ggplot2::coord_polar("y", start = 0) +
          ggplot2::theme_void() +  # remove background, grid, numeric labels
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + 
          ggplot2::scale_fill_brewer(palette = "Set1") +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "Zooplankton", 
                                                       nrow = 2, 
                                                       title.position = "top", 
                                                       title.hjust = 0.5, 
                                                       title.theme = ggplot2::element_text(face = "bold"))) +
          ggplot2::ggtitle("CPR")
        
        p <- (patchwork::wrap_plots(p1, p2, guides = "collect", ncol = 2) &
                ggplot2::theme(text = ggplot2::element_text(size = 16), legend.position = "bottom", plot.title = element_text(face = "bold"))) | 
          (patchwork::wrap_plots(p3, p4, guides = "collect", ncol = 2) &
             ggplot2::theme(text = ggplot2::element_text(size = 16), legend.position = "bottom", plot.title = element_text(face = "bold")))
        
        return(p)
        
        
      })
      
    })
  }
  
  )}  
## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")