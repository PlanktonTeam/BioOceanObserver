# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "dplyr") # using tidyverse as a dependency is discouraged as it is a meta-package
usethis::use_package( "ggplot2")
usethis::use_package( "lubridate")
usethis::use_package( "patchwork")
#usethis::use_package( "planktonr")
usethis::use_package( "rlang")
usethis::use_package( "rnaturalearth")
usethis::use_package( "shinycssloaders")
usethis::use_package( "shinythemes")
usethis::use_package( "stringr")
usethis::use_package( "thinkr")
usethis::use_package( "tidyr")
usethis::use_package( "httr")
usethis::use_package( "jsonlite")
usethis::use_package( "pracma")
usethis::use_package( "futile.logger")
usethis::use_pipe()

# usethis::use_dev_package("planktonr", type = "Imports", remote = "PlanktonTeam/planktonr@AddShinyFuncs")
remotes::install_github("PlanktonTeam/planktonr", force = TRUE)

#devtools::install_local("C:/Users/dav649/Documents/GitHub/planktonr", force = TRUE)

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "ZooTsNRS" ) # Name of the module
golem::add_module( name = "ZooTsCPR" ) # Name of the module
golem::add_module( name = "ZooSpatial" ) # Name of the module
golem::add_module( name = "EnvDataBGC" ) # Name of the module
golem::add_module( name = "Snapshot" ) # Name of the module
golem::add_module( name = "Help" ) # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# Add favicon
golem::use_favicon()

## Add internal datasets ----
## If you have data in your package
usethis::use_data(Nuts, Pigs, obs, Samples, SampLocs, absences, 
                  datCPRz, datCPRp, datNRSz, datNRSp, MapOz, 
                  overwrite = TRUE, internal = TRUE)


## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )
golem::add_rstudioconnect_file()

# Documentation

## Vignette ----
usethis::use_vignette("imosboo")
devtools::build_vignettes()

## Building the website
usethis::use_pkgdown()
pkgdown::build_site('https://github.com/PlanktonTeam/IMOS_BioOceanObserver', examples = FALSE)

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
## 
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action() 
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release() 
usethis::use_github_action_check_standard() 
usethis::use_github_action_check_full() 
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis() 
usethis::use_travis_badge() 

# AppVeyor 
usethis::use_appveyor() 
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

