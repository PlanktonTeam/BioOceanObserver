#' ATStats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ATStats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      shiny::sidebarPanel(width = 2, 
        shiny::HTML("<h4>Select a filter:</h4>"),
        shiny::fluidRow(tags$div(align = "left", 
                                 style = "background-color: #E2ECF3 !important;",
                                 shiny::radioButtons(inputId = ns("filter"), 
                                                     label = NULL,
                                                     choices = c("Species", "Receivers"), 
                                                     selected = character(0)))),
        shiny::uiOutput(ns("selections"))
    ),
      shiny::mainPanel(width = 10,
                       shiny::htmlOutput(ns("PlotExp3")),
                       fluidRow(column(12, plotOutput(ns("gg1")))) %>% 
                       shinycssloaders::withSpinner(color="#0dc5c1"),
                       shiny::htmlOutput(ns("PlotExp1")),
                       fluidRow(column(12, DT::DTOutput(ns("SpeciesDataTable")))),
                       shiny::htmlOutput(ns("PlotExp4")),
                       fluidRow(column(12, DT::DTOutput(ns("SpeciesInfoTable")))),
                       shiny::htmlOutput(ns("PlotExp2")),
                       fluidRow(column(12, DT::DTOutput(ns("locationsDataTable")))),
      )
    )
  )
}
    
#' ATStats Server Functions
#'
#' @noRd 
mod_ATStats_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
## render ui on initial choice
    output$selections <- renderUI({
      req(input$filter)
      
      if(input$filter == 'Species') {
        header_text1 <- "Select species:"
        header_text2 <- "Refine by installation:"
        choices_1 <-  c("All Species", pkg.env$AT_all_species) 
        selected_1  <-  "All Species"
        choices_2 <- c("All Receivers", sort(unique(pkg.env$AT_receivers$installation_name)))
        selected_2 <- "All Receivers"
      } else {
        header_text1 <- "Select installation:"
        header_text2 <-  "Refine by species:"
        choices_2 <-  pkg.env$AT_all_species 
        selected_2  <-  pkg.env$AT_all_species 
        choices_1 <- c("All Receivers", sort(unique(pkg.env$AT_receivers$installation_name))) 
        selected_1 <- "All Receivers"
      }

      tagList(
        h4(header_text1),
        shiny::selectizeInput(inputId = ns("select1"), 
                            label = NULL,
                            choices = choices_1, 
                            selected = selected_1,
                            multiple = TRUE ),
        h4(header_text2),
        shiny::selectizeInput(inputId = ns("select2"), 
                            label = NULL,
                            choices = choices_2, 
                            selected = selected_2,
                            multiple = TRUE,
                            options = list(
                              maxOptions = 10,       # Only shows 10 items at a time
                              placeholder = 'Type to search...'
                            ) ))

      })

    observeEvent(input$select1, {
      req(input$filter)
      
      if(input$filter == "Species"){
        if("All Species" %in% c(input$select1)){
          species <- pkg.env$AT_all_species
        } else {
          species <- input$select1
          }
        
        choices <- sort(unique(pkg.env$AT_station_species %>% 
                                 dplyr::filter(species_common_name %in% species) %>% 
                                 dplyr::pull(installation_name)))
        
        All <- "All Receivers"
        
      } else {
        if("All Receivers" %in% c(input$select1)){  ## i haven't set this as select1 yet, so that is what should happen here
          location <- sort(unique(pkg.env$AT_receivers$installation_name))
        } else {
          location <- input$select1
        }
        choices <- sort(unique(pkg.env$AT_station_species %>% 
                                 dplyr::filter(installation_name %in% location) %>% 
                                 dplyr::pull(species_common_name))) 
        All <- "All Species"
        }
        
        shiny::updateSelectizeInput(session, "select2", choices = c(All, choices), selected = All,
                                    options = list(
                                      maxOptions = 10,       # Only shows 10 items at a time
                                      placeholder = 'Type to search...'
                                      )
                                    )
          
    })

## first step for data prep        
   fdata <- reactive({
     req(input$filter)
     req(input$select1)

     if(input$filter == 'Species'){
       if("All Species" %in% c(input$select1)){
         species <- pkg.env$AT_all_species
       } else {
         species <- input$select1
       }
       
       if("All Receivers" %in% c(input$select2)){
         location <- sort(unique(pkg.env$AT_station_species %>% 
                                   dplyr::filter(species_common_name %in% species))$installation_name)
       } else {
         location <- input$select2
       }
       
     } else {
       if("All Receivers" %in% c(input$select1)){
         location <- sort(unique(pkg.env$AT_receivers$installation_name))
       } else {
         location <- input$select1
       }
       if ("All Species" %in% c(input$select2)){
         species <- sort(unique(pkg.env$AT_station_species %>% 
                                  dplyr::filter(installation_name %in% location))$species_common_name)
       } else {
         species <- input$select2
       }
     }

    fdata <- pkg.env$AT_species_summary %>% 
           dplyr::filter(species_common_name %in% species, 
                         installation_name %in% location)

    }) %>% bindCache(input$select1, input$select2, input$filter)
    
    ldata <- reactive({
      req(input$filter)
      
      ldata <- pkg.env$AT_receivers %>% 
           sf::st_drop_geometry() %>% 
           dplyr::filter(installation_name %in% fdata()$installation_name) %>% 
           dplyr::select(Installation = installation_name, Lon = lon, Lat = lat, `No Receivers` = total_receivers, `Start Date` = deployment_date, 
                         `End Date` = recovery_date, Active = active, `No Species` = n_species, `No Detections` = total_detections)

    }) %>% bindCache(input$select1, input$select2, input$filter)
    
## prepare data for tables to speed up  render
    fdataTable <- reactive({
      
      fdataTable <- fdata() %>%
          dplyr::group_by(species_common_name, installation_name) %>%
          dplyr::summarise(`First detection` = as.character(min(month_UTC)),
                    `Last Detection` = as.character(max(month_UTC)),
                    `No Detections` = sum(total_detections)) %>% 
        dplyr::left_join(pkg.env$AT_station_species %>% dplyr::select(species_common_name, installation_name, n_individuals), 
                         by = c("species_common_name", "installation_name")) %>% 
        dplyr::select(Species = species_common_name, Installation = installation_name, `No individuals` = n_individuals, everything()) %>% 
        dplyr::arrange(`No Detections`)
      
    }) %>% bindCache(input$select1, input$select2, input$filter)
    
    sdataTable <- reactive({
      sdataTable <- fdata()  %>% 
        dplyr::select(`Common Name` = species_common_name, `Scientific Name` = species_scientific_name, 
                      `Aphia id` = WORMS_species_aphia_id) %>% 
        dplyr::distinct()
    }) %>% bindCache(input$select1, input$select2, input$filter)
    
## render the tables
    output$SpeciesDataTable <- DT::renderDT({
      DT::datatable(fdataTable(), options = DToptions, fillContainer = FALSE)
    }, server = TRUE)
    
    output$SpeciesInfoTable <- DT::renderDT({
      DT::datatable(sdataTable(), options = DToptions, fillContainer = FALSE)
    }, server = TRUE)
    
    output$locationsDataTable <- DT::renderDT({
      DT::datatable(ldata(), options = DToptions, fillContainer = FALSE)
    }, server = TRUE)
    

## plot the graphs    
    gg_out1 <- reactive({
      
      daty <- ATbardata(fdata(), 'Year')
      ploty <- ATbarPlot(daty, 'Year')
      datm <- ATbardata(fdata(), 'Month')
      plotm <- ATbarPlot(datm, 'Month') +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

      ploty + plotm
      
    }) %>% bindCache(input$select1, input$select2, input$filter)
    
    output$gg1 <- renderPlot({
      shiny::validate(need(!is.na(input$filter), "Please select a filter."),
                      errorClass = "my-hint")
      gg_out1()
    })
    

## add text information
    output$PlotExp1 <- renderText({
      req(input$filter)
      HTML("<h4>Details of selected detections</h4>")
    })
    output$PlotExp4 <- renderText({
      req(input$filter)
      HTML("<h4>Details of selected species</h4>")
    })
    output$PlotExp2 <- renderText({
      req(input$filter)
      HTML("<h4>Details of selected receivers</h4>")
    })
    output$PlotExp3 <- renderText({
      req(input$filter)
      HTML("<h4>Selected detections yearly / monthly</h4>")
    })
  })
}
    
