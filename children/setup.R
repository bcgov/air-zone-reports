knitr::opts_chunk$set(echo = FALSE, warning = FALSE, fig.width = 6)

## Load packages, functions, etc.
source("components/functions.R")

## Filter data to air zone
airzone <- params$airzone
ozone <- filter(params$ozone, airzone == !!airzone)
pm25 <- filter(params$pm25, airzone == !!airzone)

# Get airzone-level metrics
o3_az <- filter(params$o3_az, airzone == !!airzone)
pm25_az <- filter(params$pm25_az, airzone == !!airzone)
pm25_mgmt <- filter(params$pm25_mgmt, airzone == !!airzone)

## Filter annual data to EMS ids provided if present, otherwise use the same
## ones as in the summary data
if (!all(is.na(params$ems_ids_ozone))) {
  annual_ozone <- filter(params$annual_ozone, ems_id %in% params$ems_ids_ozone)
} else {
  annual_ozone <- filter(params$annual_ozone, ems_id %in% ozone$ems_id)
}
if (!all(is.na(params$ems_ids_pm25))) {
  annual_pm25 <- filter(params$annual_pm25, ems_id %in% params$ems_ids_pm25)
} else {
  annual_pm25 <- filter(params$annual_pm25, ems_id %in% pm25$ems_id)
}
