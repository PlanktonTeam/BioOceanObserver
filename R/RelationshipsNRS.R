#' RelNRS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal Parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_RelNRS_ui <- function(id){
  nsRelNRS <- NS(id)

  tagList(
    sidebarLayout(
      shiny::sidebarPanel(
        shiny::conditionalPanel(
          condition = "input.navbar == 'Relationships'",
          plotOutput(nsRelNRS("plotmap")),
          shiny::HTML("<h6><strong>Select a station:</strong></h5>"),
          shiny::checkboxGroupInput(inputId = nsRelNRS("Site"), label = NULL, choices = unique(sort(pkg.env$datNRSz$StationName)), selected = "Maria Island"),
          shiny::HTML("<h6><strong>Select a group:</strong></h5>"),
          shiny::selectizeInput(inputId = nsRelNRS('group'), label = NULL, choices = c("Zooplankton", "Phytoplankton", "Microbes"), 
                                selected = "Zooplankton")
          ),
        shiny::conditionalPanel(
          condition = "input.navbar == 'Relationships' && input.relNRSscat == 1",
          ns = nsRelNRS,
          shiny::HTML("<h6><strong>Overlay trend line?</strong></h6>"),
          shiny::selectizeInput(inputId = nsRelNRS("smoother"), label = NULL, 
                         choices = c("Smoother", "Linear", "None"), selected = "None")
        ),
      shiny::conditionalPanel(
          condition = "input.group == 'Zooplankton'",
          ns = nsRelNRS,
          shiny::HTML("<h6><strong>Select a y variable:</strong></h5>"),
          shiny::selectizeInput(inputId = nsRelNRS('p1'), label = NULL, choices = unique(pkg.env$datNRSz$Parameters), 
                       selected = "Biomass_mgm3")
          ),
      shiny::conditionalPanel(
        condition = "input.group != 'Microbes' && input.relNRSscat == 1",
        ns = nsRelNRS,
        shiny::HTML("<h6><strong>Select a x variable:</strong></h5>"),
          shiny::selectizeInput(inputId = nsRelNRS('p2'), label = NULL, choices = unique(pkg.env$datNRSw$Parameters), 
                              selected = "CTDTemperature_degC")
        ),
      shiny::conditionalPanel(
        condition = "input.group == 'Phytoplankton'",
        ns = nsRelNRS,
        shiny::HTML("<h6><strong>Select a y variable:</strong></h5>"),
        shiny::selectizeInput(inputId = nsRelNRS('p3'), label = NULL, choices = unique(pkg.env$datNRSp$Parameters), 
                       selected = "PhytoAbundance_CellsL")
      ),
      shiny::conditionalPanel(
        condition = "input.group == 'Microbes'",
        ns = nsRelNRS,
        shiny::HTML("<h6><strong>Select a y variable:</strong></h5>"),
        shiny::selectizeInput(inputId = nsRelNRS('p5'), label = NULL, choices = unique(pkg.env$datNRSm$Parameters), 
                       selected = "Bacterial_Temperature_Index_KD")
        ),
      shiny::conditionalPanel(
        condition = "input.group == 'Microbes' && input.relNRSscat == 1",
        ns = nsRelNRS,
        shiny::HTML("<h6><strong>Select a x variable:</strong></h5>"),
        shiny::selectizeInput(inputId = nsRelNRS('p6'), label = NULL, choices = unique(ctd$Parameters), #TODO pkg.env$
                              selected = "CTD_Temperature_degC")
      )
      ),
      mainPanel(id = "Rel NRS", 
                shiny::tabsetPanel(id = 'relNRSscat', type = "pills",
                                   shiny::tabPanel("Scatter plots", value = 1,
                                                   shiny::htmlOutput(nsRelNRS("PlotExp1")),
                                                   plotOutput(nsRelNRS("scatter1")) %>% 
                                                     shinycssloaders::withSpinner(color="#0dc5c1"),
                                                   div(style="display:inline-block; float:right; width:60%",
                                                       fButtons(id, button_id = "downloadPlot1", label = "Plot", Type = "Download"),
                                                       fButtons(id, button_id = "downloadData1", label = "Data", Type = "Download"))
                                                   ),
                                   shiny::tabPanel("Box plots", value = 2,
                                                   h6(textOutput(nsRelNRS("PlotExp2"), container = span))#,  
                                                   # plotOutput(nsRelNRS("box2"), height = 800) %>% 
                                                   #   shinycssloaders::withSpinner(color="#0dc5c1"),
                                                   # div(style="display:inline-block; float:right; width:60%",
                                                   #     fButtons(id, button_id = "downloadPlot2", label = "Plot", Type = "Download"),
                                                   #     fButtons(id, button_id = "downloadData2", label = "Data", Type = "Download"))
                                                   )
                                   )
                )
      )
    )
  }

#' RelNRS Server Functions
#'
#' @noRd 
mod_RelNRS_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    selectedData <- reactive({
      
      if(input$group %in% 'Zooplankton'){
        dat <- pkg.env$datNRSz
        dat1 <- pkg.env$datNRSw
        y <- rlang::string(input$p1)
        x <- rlang::string(input$p2)
      } else if (input$group %in% 'Phytoplankton'){
        dat <- pkg.env$datNRSp
        dat1 <- pkg.env$datNRSw
        y <- rlang::string(input$p3)
        x <- rlang::string(input$p4)
      } else if (input$group %in% 'Microbes'){
        dat <- pkg.env$datNRSm %>% 
          dplyr::select(-c("TripCode_depth")) %>%
          dplyr::mutate(SampleDepth_m = round(.data$SampleDepth_m/10,0)*10) 
        dat1 <- ctd ##TODO pkg.env$
        y <- rlang::string(input$p5)
        x <- rlang::string(input$p6)
      }
      
      vars <- c("StationName", "StationCode", "SampleTime_Local", "SampleDepth_m") # only microbes has depth data
      
      selectedData <- dat %>%  
        dplyr::bind_rows(dat1) %>%
        dplyr::filter(.data$StationName %in% input$Site,
                      .data$Parameters %in% c(x, y)) %>%
        tidyr::pivot_wider(id_cols = dplyr::any_of(vars),
                           names_from = "Parameters", values_from = "Values", values_fn = mean) %>%
        tidyr::drop_na()
      
    }) %>% bindCache(input$Site, input$p2, input$p1, input$p3, input$p4, input$p5, input$p6, input$group)
    
    # Parameter Definition
    output$ParamDefm1 <-   shiny::renderText({
      paste("<h6><strong>", planktonr::pr_relabel(input$p1, style = "plotly"), ":</strong> ",
            pkg.env$ParamDef %>% dplyr::filter(Parameter == input$p1) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
    })
    # Parameter Definition
    output$ParamDefm2 <- shiny::renderText({
      paste("<h6><strong>", planktonr::pr_relabel(input$p2, style = "plotly"), ":</strong> ",
            pkg.env$ParamDef %>% dplyr::filter(Parameter == input$p2) %>% dplyr::pull("Definition"), ".</h6>", sep = "")
    })
    
    # Sidebar Map
    output$plotmap <- renderPlot({
      planktonr::pr_plot_NRSmap(selectedData())
    }, bg = "transparent") %>% bindCache(input$Site)

    # Add text information 
    output$PlotExp1 <- shiny::renderText({
      "A scatter plot of selected indices against oceanographic parameters measured from the NRS around Australia"
    }) 
    # Add text information 
    output$PlotExp2 <- shiny::renderText({
      "A box plot of selected indices showing range of each parameter at the NRS around Australia"
    }) 
    
    ## scatter plot
    gg_out1 <- reactive({
      
    trend <- input$smoother
    
      if(input$group %in% 'Zooplankton'){
        y <- rlang::string(input$p1)
        x <- rlang::string(input$p2)
      } else if (input$group %in% 'Phytoplankton'){
        y <- rlang::string(input$p3)
        x <- rlang::string(input$p4)
      } else if (input$group %in% 'Microbes'){
        y <- rlang::string(input$p5)
        x <- rlang::string(input$p6)
      }
      
      planktonr::pr_plot_scatter(selectedData(), x, y, trend)

    }) %>% bindCache(input$p1, input$p2, input$p3, input$p4, input$p5, input$p6, input$Site, input$group, input$smoother)

    output$scatter1 <- renderPlot({
      gg_out1()
    }, height = function() {
      if(input$group != 'Microbes')
      {300} else
      {length(unique(selectedData()$SampleDepth_m)) * 200}})

    # Download -------------------------------------------------------
    output$downloadData1 <- fDownloadButtonServer(input, selectedData(), "Scatter") # Download csv of data
    output$downloadPlot1 <- fDownloadPlotServer(input, gg_id = gg_out1(), "Scatter") # Download figure

}
)
  
}
