library(shinytest2)

options(shinytest2.load_timeout=500000)

## Policy
test_that("Initial status of Policy", {
  app <- AppDriver$new(name = "EOVs", load_timeout = 500000)
   app$set_inputs(navbar = "EOVs", wait_ = FALSE) # test pol nrs
   app$set_inputs(pol = "cpr", wait_ = FALSE) # test pol cpr
   app$set_inputs(pol = "LTM", wait_ = FALSE) # test pol ltm
   app$expect_values(export = TRUE)
})

## Zooplankton
test_that("Initial status of Zooplankton", {
  app <- AppDriver$new(name = "Zooplankton", load_timeout = 500000)
  app$set_inputs(navbar = "Zooplankton", wait_ = FALSE)
  app$set_inputs(zoo = "ztscpr", wait_ = FALSE)
  app$set_inputs(zoo = "dist", wait_ = FALSE)
  app$expect_values(export = TRUE)
})

## Phytoplankton
test_that("Initial status of Phytoplankton", {
  app <- AppDriver$new(name = "Phytoplankton", load_timeout = 500000)
  app$set_inputs(navbar = "Phytoplankton", wait_ = FALSE)
  app$set_inputs(phyto = "ptscpr", wait_ = FALSE)
  app$set_inputs(phyto = "distp", wait_ = FALSE)
  app$expect_values(export = TRUE)
})

## Microbes
test_that("Initial status of Microbes", {
  app <- AppDriver$new(name = "Microbes", load_timeout = 500000)
  app$set_inputs(navbar = "Microbes", wait_ = FALSE)
  app$expect_values(export = TRUE)
})

## Environmental data
test_that("Initial status of Env Data", {
  app <- AppDriver$new(name = "EnvData", load_timeout = 500000)
  app$set_inputs(navbar = "Environmental Data", wait_ = FALSE) # test nuts
  app$set_inputs(env = "pigs", wait_ = FALSE) # test pigs
  app$set_inputs(env = "water", wait_ = FALSE) # test water
  app$set_inputs(env = "bgc", wait_ = FALSE) # test pigs
  app$set_inputs(env = "pico", wait_ = FALSE) # test water
  app$set_inputs(env = "moor", wait_ = FALSE) # test moorings
  app$expect_values(export = TRUE)
})

