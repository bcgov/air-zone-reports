##########################
####  Render reports  ####
##########################

library("here")
library("purrr")
library("rmarkdown")

## Run the ozone and PM2.5 analyses to generate the necessary objects. These
## paths will need to be replaced with the correct location where the analyses
## will be run.
ozone_dir <- "../ozone-caaqs-indicator/"
pm25_dir  <- "../pm25-caaqs-indicator/"

## Files from each analysis to run. We don't need all the saved outputs from
## these analyses, just the R objects, so this is a bit faster.
files_to_run <- c("01_load.R", "02_clean.R", "03_analysis.R")

## Need to run analyses in their own directories because they depend on files
## created within those directories during the course of the analysis
setwd(ozone_dir)
walk(files_to_run, source)

setwd(pm25_dir)
walk(files_to_run, source)

## Now return to air zone reports directory to render files
setwd(here())

## Render all reports, named 01_northeast.Rmd, 02_central_interior.Rmd, etc.
## using default parameters
walk(
  list.files(pattern = "\\d{2}(.+)Rmd"),
  render
)
