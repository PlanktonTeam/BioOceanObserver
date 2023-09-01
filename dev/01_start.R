# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
## 
## /!\ Note: if you want to change the name of your app during development, 
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
## 
golem::fill_desc(
  pkg_name = "biooceanobserver", # The Name of the package containing the App 
  pkg_title = "Visualisation of biological and oceanographic data", # The Title of the package containing the App 
  pkg_description = "A visualisation tool for biologial and oceanographic data.", # The Description of the package containing the App 
  author_first_name = "Claire H.", # Your First Name
  author_last_name = "Davies", # Your Last Name
  author_email = "claire.davies@csiro.au", # Your Email
  repo_url = "https://github.com/PlanktonTeam/IMOS_BioOceanObserver" # The URL of the GitHub Repo (optional) 
)     

## Set {golem} options ----
golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
usethis::use_mit_license( name = "Claire Davies" )  # You can set another license here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )

## Add a data raw file
## If you have data in your package
usethis::use_data_raw(name = "EnvDataBGC_RData", open = FALSE)
usethis::use_data_raw(name = "ZooSpatial_RData", open = FALSE)
usethis::use_data_raw(name = "ZooTsNRS_RData", open = FALSE)
usethis::use_data_raw(name = "ZooTsCPR_RData", open = FALSE)

## Use git ----
usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Use Recommended Packages ----
golem::use_recommended_deps()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/icon". Can be an online file. 
golem::remove_favicon()

## Add helper functions ----
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

