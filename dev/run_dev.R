# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
# options(shiny.port = httpuv::randomPort())

# When developing, work within a project (do not commit the project file to GitHub).
# Set your working directory to the package directory on your local drive.
# Then use devtools to load TADAShiny
library(devtools)
devtools::load_all()

# Document and reload your package
devtools::document()

# Run check often to catch and fix warnings and notes
devtools::check()

# Alternatively, you can use golem to load TADAShiny
# Detach all loaded packages and clean your environment
library(golem)
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Detach all loaded packages and clean your environment
golem::detach_all_attached()

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()
