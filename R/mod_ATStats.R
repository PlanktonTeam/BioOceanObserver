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
                                                     selected = "Species"))),
        h4(shiny::textOutput(ns("header_text1"))),
        shiny::selectizeInput(inputId = ns("select1"),
                              label = NULL,
                              choices = pkg.env$AT_all_species,
                              multiple = TRUE,
                              options = list(
                                maxOptions = 10,       # Only shows 10 items at a time
                                placeholder = "All species"
                              )),
        h4(shiny::textOutput(ns("header_text2"))),
        shiny::selectizeInput(inputId = ns("select2"),
                              label = NULL,
                              choices = c(sort(unique(pkg.env$AT_receivers$installation_name))),
                              multiple = TRUE,
                              options = list(
                                maxOptions = 10,       # Only shows 10 items at a time
                                placeholder = "All receivers"
                              )),
        shiny::HTML("<h4>Select dates:</h4>"),
        shiny::sliderInput(ns("datesslide"), label = NULL, min = lubridate::ymd(min(pkg.env$AT_receivers$deployment_date)), max = Sys.Date(), 
                           value = c(lubridate::ymd(min(pkg.env$AT_receivers$deployment_date)), Sys.Date()-1), 
                           timeFormat="%m-%Y"),
        shiny::br(),
        shiny::actionButton(ns("reset_filter"), "Reset to remove filters", icon = icon("undo"))
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

    output$header_text1 <- renderText({
      if (input$filter == "Species") "Select species:" else "Select receivers:"
    })
    
    output$header_text2 <- renderText({
      if (input$filter == "Species") "Refine by receivers:" else "Refine by species:"
    })
    
    observeEvent(list(input$filter,input$reset_filter), {
      req(input$filter)

      if(input$filter == 'Species') {
        choices_1 <-  c(pkg.env$AT_all_species)
        choices_2 <- c(sort(unique(pkg.env$AT_receivers$installation_name)))
        placeholder1 <- "All species"
        placeholder2 <- "All receivers"
      } else {
        choices_2 <-  pkg.env$AT_all_species
        choices_1 <- c(sort(unique(pkg.env$AT_receivers$installation_name)))
        placeholder1 <- "All receivers"
        placeholder2 <- "All species"
      }

        shiny::updateSelectizeInput(session, "select1",
                              label = NULL,
                              choices = choices_1,
                              options = list(
                                maxOptions = 10,       # Only shows 10 items at a time
                                placeholder = placeholder1
                              ))
        shiny::updateSelectizeInput(session, "select2",
                              label = NULL,
                              choices = choices_2,
                              options = list(
                                maxOptions = 10,       # Only shows 10 items at a time
                                placeholder = placeholder2
                              ))
        shiny::updateSliderInput(session, "datesslide", 
                                 min = lubridate::ymd(min(pkg.env$AT_receivers$deployment_date)), max = Sys.Date(), 
                                 value = c(lubridate::ymd(min(pkg.env$AT_receivers$deployment_date)), Sys.Date()-1), 
                                 timeFormat="%m-%Y")
    })

    observeEvent(input$select1, {
      req(input$filter)
      
      if(input$filter == "Species"){
        if(is.null(input$select1)){
          species <- pkg.env$AT_all_species
        } else {
          species <- input$select1
          }
        
        choices <- sort(unique(pkg.env$AT_station_species %>% 
                                 dplyr::filter(species_common_name %in% species) %>% 
                                 dplyr::pull(installation_name)))
        
      } else {
        if(is.null(input$select1)){  ## i haven't set this as select1 yet, so that is what should happen here
          location <- sort(unique(pkg.env$AT_receivers$installation_name))
        } else {
          location <- input$select1
        }
        choices <- sort(unique(pkg.env$AT_station_species %>% 
                                 dplyr::filter(installation_name %in% location) %>% 
                                 dplyr::pull(species_common_name))) 
        }
        
      dates <- pkg.env$AT_species_summary %>% 
        dplyr::filter(species_common_name %in% species) %>% 
        dplyr::pull(month_UTC)
      
      
        shiny::updateSelectizeInput(session, "select2", choices = choices, 
                                    options = list(
                                      maxOptions = 10
                                      )
                                    )
        
        shiny::updateSliderInput(session, "datesslide", min = min(dates), max = max(dates), 
                                 value = c(min(dates), max(dates)), timeFormat="%m-%Y")
          
    }, ignoreInit = FALSE) 
    

## first step for data prep        
   fdata <- reactive({
     req(input$filter)

     if(input$filter == 'Species'){
       if(is.null(input$select1)){
         species <- pkg.env$AT_all_species
       } else {
         species <- input$select1
       }
       
       if(is.null(input$select2)){
         location <- sort(unique(pkg.env$AT_station_species %>% 
                                   dplyr::filter(species_common_name %in% species))$installation_name)
       } else {
         location <- input$select2
       }
       
     } else {
       if(is.null(input$select1)){
         location <- sort(unique(pkg.env$AT_receivers$installation_name))
       } else {
         location <- input$select1
       }
       if (is.null(input$select2)){
         species <- sort(unique(pkg.env$AT_station_species %>% 
                                  dplyr::filter(installation_name %in% location))$species_common_name)
       } else {
         species <- input$select2
       }
     }

    fdata <- pkg.env$AT_species_summary %>% 
           dplyr::filter(species_common_name %in% species, 
                         installation_name %in% location,
                         dplyr::between(month_UTC, input$datesslide[1], input$datesslide[2]))

    }) %>% bindCache(input$select1, input$select2, input$filter, input$datesslide[1], input$datesslide[2])
    
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
          dplyr::summarise(`First detection` = as.character(min(month_UTC, na.rm = TRUE)),
                    `Last Detection` = as.character(max(month_UTC, na.rm = TRUE)),
                    `No Detections` = sum(total_detections, na.rm = TRUE)) %>% 
        dplyr::left_join(pkg.env$AT_station_species %>% dplyr::select(species_common_name, installation_name, n_individuals), 
                         by = c("species_common_name", "installation_name")) %>% 
        dplyr::select(Species = species_common_name, Installation = installation_name, `No individuals` = n_individuals, everything()) %>% 
        dplyr::arrange(desc(`No Detections`))
      
    }) %>% bindCache(input$select1, input$select2, input$filter, input$datesslide[1], input$datesslide[2])
    
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
      
    }) %>% bindCache(input$select1, input$select2, input$filter, input$datesslide[1], input$datesslide[2])
    
    output$gg1 <- renderPlot({
      shiny::validate(need(!is.na(input$filter), "Please select a filter to begin."),
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
    
