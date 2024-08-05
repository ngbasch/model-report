#'=============================================================================
#' Load required R Libraries
#'============================================================================

#'----------------------------------------
#' Load Data
#'----------------------------------------
#' Grab data from snowflake
library(DBI)

#' Connect to AWS
library(paws)

#' Google sheets API
library(googlesheets4)
library(googledrive)

#' Load Excel files
library(readxl)

#' Write html files
library(xml2)
library(yaml)

#'----------------------------------------
#' Manipulate Data
#'----------------------------------------

#' Data manipulation
library(data.table)
library(tidyverse)
library(dtplyr)
library(reshape2)
#' Load parquet files
#' library(arrow)


#'----------------------------------------
#' Data Utilities
#'----------------------------------------
#' File paths
library(here)
#' Package management
library(renv)

#' Easily clean data
library(janitor)

#' Date manipulation
library(lubridate)

#' String manipulation
library(stringi)

#' Reading SQL strings
library(readr)

#' Multi-threading
library(parallel)
library(future)
library(future.apply)
library(doParallel)

#' Compute hash
library(digest)


#'----------------------------------------
#' Feature Engineering and Modeling
#'----------------------------------------

#' Multiple imputation
#' install.packages("remotes")
#' library(remotes)
#' remotes::install_github(repo = "amices/mice")
library(mice)

#' Outlier detection
library(isotree)

#' Modeling
library(randomForest)
library(xgboost)
library(ranger)
library(tidymodels)
library(stacks)
# library(keras)
library(finetune)
library(Cubist)
library(rules)
library(dbarts)
# library(tensorflow)
library(Metrics)
library(lightgbm)
library(bonsai)

# Bayesian
# library(bayesian)
# library(brms)




#' Model exploration
library(DALEX)
library(DALEXtra)
library(vip)
library(philentropy)
library(themis)


#'----------------------------------------
#' Visualization
#'----------------------------------------
#' Graphs
library(RColorBrewer)
library(ggplot2)
library(patchwork)
library(ggplotify)
library(ggExtra)
library(scales)
library(viridis)
library(rcartocolor)
library(tidybayes)
library(cowplot)
library(hues)
library(visdat)
library(plotly)
library(reactable)

#' R Markdown
# https://juba.github.io/rmdformats/
library(rmdformats)


#' Tables
library(formattable)

#' Correlation plots
library(corrplot)

#' Sequential density plots
library(ggridges)

#' Grids
library(gridExtra)

#' Themes
library(RColorBrewer)

#' Maps/geospatial
# library(leaflet)
# library(raster)
library(geojsonsf)
library(httr)
# library(sf)