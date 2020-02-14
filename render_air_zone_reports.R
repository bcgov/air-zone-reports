##########################
####  Render reports  ####
##########################

library("purrr")
library("rmarkdown")
source("components/render.R")

## Load the necessary objects from the ozone and PM2.5 analyses
ozone_dir <- "../ozone-caaqs-indicator/"
pm25_dir  <- "../pm25-caaqs-indicator/"

load(file.path(ozone_dir, "tmp/ozone_clean.RData"))
load(file.path(ozone_dir, "tmp/analysed.RData"))
load(file.path(ozone_dir, "tmp/plots.RData"))

load(file.path(pm25_dir, "tmp/pm25_clean.rda"))
load(file.path(pm25_dir, "tmp/analysed.RData"))
load(file.path(pm25_dir, "tmp/plots.RData"))

## Render summary report
render("00_summary_report.Rmd", 
       output_dir = "final_reports")

## Render intermediate reports, named 01_northeast.Rmd, 02_central_interior.Rmd, etc.
## using default parameters
render_intermediate("01_northeast.Rmd")
render_intermediate("02_central_interior.Rmd")
render_intermediate("03_southern_interior.Rmd")
render_intermediate("04_lower_fraser_valley.Rmd")
render_intermediate("05_georgia_strait.Rmd")
render_intermediate("06_coastal.Rmd")

## To render a report and specify the stations to include in plots, 
## provide a vector of the ems_ids to the `ozone_ems_ids` and `pm25_ems_ids` 
## arguments. E.g.:
render_intermediate("01_northeast.Rmd", 
                    ozone_ems_ids = c("E227431", "E229797", "E231866"))

## Edit the text (or figure paths etc) in the .md files that are generated, 
## then run render_final() on those markdown files:

render_final("intermediate_reports/01_northeast_intermediate.md")
render_final("intermediate_reports/02_central_interior_intermediate.md")
render_final("intermediate_reports/03_southern_interior_intermediate.md")
render_final("intermediate_reports/04_lower_fraser_valley_intermediate.md")
render_final("intermediate_reports/05_georgia_strait_intermediate.md")
render_final("intermediate_reports/06_coastal_intermediate.md")
