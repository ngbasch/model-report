# NOTE -----------------
# This is the R code needed for Nathan to set up the renv lock file based on his user library
# YOU DO NOT NEED TO RUN THIS CODE TO GET PACKAGES TO WORK
# IF YOU WANT INSTALL ALL PACKAGES IN LIBRARY, RUN renv::restore()

# Set up description file:
# https://github.com/rstudio/renv/issues/940

# Initialize project
# https://mirai-solutions.ch/techguides/initialize-an-renv-project.html
renv::init(
  # use the DESCRIPTION file to capture dependencies
  settings = list(snapshot.type = "explicit"),
  # do not install dependencies (done in a custom way)
  bare = TRUE
)

# Snapshot packages from Nathan's user library (to the project library) - (First time)
# renv::snapshot(library = "/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library")
renv::snapshot()
