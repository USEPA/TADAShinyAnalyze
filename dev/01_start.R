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
## Add meta data about your application and set some default {golem} options
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "TADAShinyAnalyze", # The name of the golem package containing the app (typically lowercase, no underscore or periods)
  pkg_title = "TADAShinyAnalyze: Analyze Water and Sediment Chemistry Data", # What the Package Does (One Line, Title Case, No Period)
  pkg_description = "The overall goal of the `TADAShinyAnalyze` shiny app is to provide a third module to the `TADAShiny` shiny app where users can complete batch and custom analyses by monitoring locations or assessment units (AUs) with the ultimate goal of supporting water quality management.", # What the package does (one paragraph).
  authors = c(person(given = "Cristina", # Your First Name
                     family = "Mullin", # Your Last Name
                     email = "mywaterway@epa.gov", # Your email
                     role = c("aut"),
                     comment = c(ORCID = "0000-0002-0615-6087")), # Your role (here author/creator)
              person(given = "Sheila", # Your First Name
                     family = "Saia", # Your Last Name
                     email = "sheila.saia@tetratech.com", # Your email
                     role = c("aut", "cre"),
                     comment = c(ORCID = "0000-0002-4943-0306")), # Your role (here author/creator)
              person(given = "Ben", # Your First Name
                     family = "Block", # Your Last Name
                     email = "ben.block@tetratech.com", # Your email
                     role = c("aut"),
                     comment = c(ORCID = "0000-0003-1427-4363")), # Your role (here author/creator)
              person(given = "Yu-Chen", # Your First Name
                     family = "Wang", # Your Last Name
                     email = NULL, # Your email
                     role = c("aut"))), # Your role (here author/creator)
  repo_url = "https://github.com/USEPA/TADAShinyAnalyze", # The URL of the GitHub repo (optional),
  pkg_version = "0.0.0.9000", # The version of the package containing the app
  set_options = TRUE # Set the global golem options
)
# note: can only have one maintainer (i.e., "cre" role) otherwise will get an error with devtools::check()


## Install the required dev dependencies ----
golem::install_dev_deps()
golem::use_recommended_deps()

## Create Common Files ----
## See ?usethis for more information
# usethis::use_mit_license("Golem User") # You can set another license here
# golem::use_readme_rmd(open = FALSE)
# devtools::build_readme()
# Note that `contact` is required since usethis version 2.1.5
# If your {usethis} version is older, you can remove that param
# usethis::use_code_of_conduct(contact = "Golem User")
usethis::use_cc0_license()  # recommended based on TADAShiny app
usethis::use_lifecycle_badge( "Experimental" )
# commenting others out for now (sheila)

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon() # path = "path/to/ico". Can be an online file.
# golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Use git ----
usethis::use_git()
## Sets the remote associated with 'name' to 'url'
usethis::use_git_remote(
  name = "origin",
  url = "https://github.com/USEPA/TADAShinyAnalyze.git"
)

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
