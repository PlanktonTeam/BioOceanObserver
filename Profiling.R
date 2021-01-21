library(profvis)
library(shiny)

source("ui.R")
source("server.R")

profvis({
  runApp()
})
