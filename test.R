## file for testing scripts in a small way before making into a module

library(shiny)

ui <- pageWithSidebar(
  headerPanel("renderImage example"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000,  value = 500)
  ),
  mainPanel(
    # Use imageOutput to place the image on the page
    imageOutput("myImage"),
    uiOutput("myImage")
    #img(src = "SDMTweGAM_Acartia danae.png", height = 400, width = 400)
  )
)

server <- function(input, output, session) {
  output$myImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- "SDMTweGAM_Acartia danae.png"
    
    # Return a list containing the filename and alt text
    list(src = filename,
         height = 400, width = 400,
         alt = input$n)
    
  }, deleteFile = FALSE)
}

shinyApp(ui, server)
