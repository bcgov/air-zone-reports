###########################################################
####  Figure captions for individual air zone reports  ####
###########################################################

## Does not include table captions (these are specified in the respective
## table-generating functions in functions.R)

## Ozone by station
ozone_by_station_cap <- paste0(
  "Ozone concentrations in the ",
  airzone,
  " Air Zone, based on annual 4th highest daily 8-hour maxima, averaged over ",
  min(ozone$min_year, na.rm = TRUE),
  "-",
  max(ozone$max_year, na.rm = TRUE),
  ". Red dashed line identifies the CAAQS of 63 ppb."
)

## Annual ozone
annual_ozone_cap <- paste0(
  "Annual trends in ozone concentrations (",
  min(annual_ozone$year, na.rm = TRUE),
  "-",
  max(annual_ozone$year, na.rm = TRUE),
  "), based on annual 4th highest daily 8-hour maxima for a single year. Red dashed line identifies CAAQS of 63 ppb."
)

## PM2.5 by station
pm25_by_station_cap <- paste0(
  "PM$_{2.5}$ concentrations in the ",
  airzone,
  " Air Zone. Upper plot based on 24-hour concentration (annual 98th percentile, averaged over ",
  min(pm25$min_year, na.rm = TRUE),
  "-",
  max(pm25$max_year, na.rm = TRUE),
  "). Lower plot based on annual mean concentration (averaged over ",
  min(pm25$min_year, na.rm = TRUE),
  "-",
  max(pm25$max_year, na.rm = TRUE),
  "). Red dashed lines identify CAAQS of 28 $\\mu$g/m$^3$ (upper plot) and 10 $\\mu$g/m$^3$ (lower plot)."
)

## Annual PM2.5
annual_pm25_cap <- paste0(
  "Trends in PM$_{2.5}$ concentrations (",
  min(annual_pm25$year, na.rm = TRUE),
  "-",
  max(annual_pm25$year, na.rm = TRUE),
  "), based on annual mean concentrations from a single year. The CAAQS value of 10 $\\mu$g/m$^3$ is shown by the dashed line. PM$_{2.5}$ measurements prior to 2011 are reported at 25\\textdegree C and 1 atm. From 2011 onward, measurements are reported at local conditions."
)
