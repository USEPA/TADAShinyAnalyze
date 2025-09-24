# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# # Test your app
# 
# ## Run checks ----
# ## Check the package before sending to prod
# devtools::check()
# rhub::check_for_cran()
# 
# # Deploy
# 
# ## Local, CRAN or Package Manager ----
# ## This will build a tar.gz that can be installed locally,
# ## sent to CRAN, or to a package manager
# devtools::build()

## Docker ----
## If you want to deploy via a generic Dockerfile
# golem::add_dockerfile_with_renv()
## If you want to deploy to ShinyProxy
# golem::add_dockerfile_with_renv_shinyproxy()

## Posit ----
## If you want to deploy on Posit related platforms
# golem::add_positconnect_file()
golem::add_shinyappsio_file()
# golem::add_shinyserver_file()

# ## Deploy to Posit Connect or ShinyApps.io ----
# 
# ## Add/update manifest file (optional; for Git backed deployment on Posit )
# rsconnect::writeManifest()
# 
# ## In command line.
# rsconnect::deployApp(
#   appName = desc::desc_get_field("Package"),
#   appTitle = desc::desc_get_field("Package"),
#   appFiles = c(
#     # Add any additional files unique to your app here.
#     "R/",
#     "inst/",
#     "data/",
#     "NAMESPACE",
#     "DESCRIPTION",
#     "app.R"
#   ),
#   appId = rsconnect::deployments(".")$appID,
#   lint = FALSE,
#   forceUpdate = TRUE
# )


# # 1) QA before deploy
# devtools::test()
# devtools::check()
# devtools::build()

# # 2) Freeze deps (if using renv)
# renv::status()
# renv::snapshot(prompt = FALSE)

# 3) Deploy to Shinyapps.io
# rsconnect::setAccountInfo(name="yourname", token="YOUR_TOKEN", secret="YOUR_SECRET")
rsconnect::deployApp(appDir = ".", appName = "TADAShinyAnalyze")

# 4) View logs on failure
# rsconnect::showLogs(appName = "your-app-name", streaming = TRUE)


# Deploy to EPA Posit Connect----

# This is how to add the shiny sever file needed for any deployment
# golem::add_shinyserver_file() # already exists

# This is how to setup deployment to EPA's Posit Connect
# golem::add_positconnect_file() # already exists see rsconnect folder

# This is how to deploy, works for both TT shinyappsio and EPA posit connect
# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# Document and reload your package
golem::document_and_reload()
# Use packrat
# options(rsconnect.packrat = TRUE) # already done
# Deploy app to staging
# Add staging link here: https://rstudio-connect.dmap-stage.aws.epa.gov/content/019dcc7e-863b-426a-a2a5-1813f53f5702/ 
rsconnect::deployApp(
  appDir = getwd(),
  appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "R/", "inst/"),
  appName = "TADAShinyAnalyze",
  appTitle = "TADAShiny Module 3 Analyze",
  launch.browser = TRUE,
  forceUpdate = TRUE,
  appId = 1025
)

# To deploy to EPA posit connect production (public)
# We must reach out to the DMAP team
# Add public URL here when ready: 