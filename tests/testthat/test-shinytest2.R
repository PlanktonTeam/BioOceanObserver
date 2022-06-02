library(shinytest2)

## Policy
test_that("Initial status of Policy", {
  app <- AppDriver$new(name = "Policy")
  app$set_inputs(navbar = "Policy", wait_ = FALSE) # test pol nrs
  app$set_inputs(pol = "cpr", wait_ = FALSE) # test pol cpr
  app$set_inputs(pol = "LTM", wait_ = FALSE) # test pol ltm
  app$expect_values(export = TRUE)
})

## Zooplankton
test_that("Initial status of Zooplankton", {
  app <- AppDriver$new(name = "Zooplankton")
  app$set_inputs(navbar = "Zooplankton", wait_ = FALSE)
  app$set_inputs(zoo = "ztscpr", wait_ = FALSE)
  app$set_inputs(zoo = "dist", wait_ = FALSE)
  app$expect_values(export = TRUE)
})

## Phytoplankton
test_that("Initial status of Phytoplankton", {
  app <- AppDriver$new(name = "Phytoplankton")
  app$set_inputs(navbar = "Phytoplankton", wait_ = FALSE)
  app$set_inputs(phyto = "ptscpr", wait_ = FALSE)
  app$set_inputs(phyto = "distp", wait_ = FALSE)
  app$expect_values(export = TRUE)
})

## Microbes
test_that("Initial status of Microbes", {
  app <- AppDriver$new(name = "Microbes")
  app$set_inputs(navbar = "Microbes", wait_ = FALSE)
  app$expect_values(export = TRUE)
})

## Environmental data
test_that("Initial status of Env Data", {
  app <- AppDriver$new(name = "Env Data")
  app$set_inputs(navbar = "Environmental Data", wait_ = FALSE) # test nuts
  app$set_inputs(env = "pigs", wait_ = FALSE) # test pigs
  app$expect_values(export = TRUE)
})

