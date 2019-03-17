##########################
####  Render reports  ####
##########################

library("purrr")
library("rmarkdown")

## Load the necessary objects from the ozone and PM2.5 analyses
ozone_dir <- "../ozone-caaqs-indicator/"
pm25_dir  <- "../pm25-caaqs-indicator/"

load(file.path(ozone_dir, "tmp/ozone_clean.RData"))
load(file.path(ozone_dir, "tmp/analysed.RData"))
load(file.path(ozone_dir, "tmp/plots.RData"))

load(file.path(pm25_dir, "tmp/pm25_clean.rda"))
load(file.path(pm25_dir, "tmp/analysed.RData"))

## Render all reports, named 01_northeast.Rmd, 02_central_interior.Rmd, etc.
## using default parameters
walk(
  list.files(pattern = "\\d{2}(.+)Rmd"),
  render,
  output_dir = "rendered_reports"
)

