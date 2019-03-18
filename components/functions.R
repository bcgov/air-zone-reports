##############################################################
####  Functions to generate results for air zone reports  ####
##############################################################

## Plots -----------------------------------------------------

## Ozone concentration based on annual 4th highest daily 8-hour maxima, 3 year
## average bar chart by station (fig. 2)
plot_ozone_by_station <- function(data, airzone, caaqs = 63) {
  ymax_oz <- max(data$metric_value_ambient, na.rm = TRUE)
  ozone_year <- max(data$max_year)

  ggplot(
    data,
    aes(
      x = reorder(station_name, metric_value_ambient, sum),
      y = metric_value_ambient
    )
  ) +
    geom_bar(stat = "identity", fill = "#A488F7") +
    geom_text(aes(label = metric_value_ambient), nudge_y = 3, size = 2.82) +
    geom_hline(yintercept = caaqs, color = "red", lty = "dashed") +
    annotate(
      geom = "text",
      label = "CAAQS",
      x = 1,
      y = caaqs + 7,
      size = 2.82
    ) +
    ## Set y scale to ensure CAAQS text doesn't get cut off (but if the max ozone
    ## value is greater, take that value instead + 10 for padding)
    scale_y_continuous(limits = c(0, max(ymax_oz + 10, caaqs + 10))) +
    labs(
      x = "Station",
      y = "Ozone concentration (ppb)",
      title = paste0(airzone, " Air Zone ", ozone_year)
    ) +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5))
}

## Annual trends in ozone concentration (annual 4th highest daily 8-hour maxima)
## time series line chart (fig. 3)
plot_ozone_station_timeseries <- function(data, airzone, caaqs = 63) {
  ozone_year <- max(data$year)
  min_year <- min(data$year, na.rm = TRUE)
  max_year <- max(data$year, na.rm = TRUE)
  ymax_oz <- max(data$ann_4th_highest, na.rm = TRUE)

  ggplot(data, aes(x = year, y = ann_4th_highest, colour = station_name)) +
    geom_line() +
    geom_hline(yintercept = caaqs, color = "red", lty = "dashed") +
    annotate(
      geom = "text",
      label = "CAAQS",
      x = 2016,
      y = caaqs + 5,
      size = 2.82
    ) +
    labs(
      x = "Year",
      y = "Ozone concentration (ppb)",
      title = paste0(airzone, " Air Zone ", min_year, "-", max_year),
      colour = "Location"
    ) +
    scale_x_continuous(
      breaks = function(x) floor(pretty(seq(min(x), max(x), by = 1)))
    ) +
    scale_y_continuous(limits = c(0, max(ymax_oz + 5, 72))) +
    theme(plot.title = element_text(hjust = 0.5))
}

## PM2.5 concentrations bar charts (24-hour concentration and annual mean
## concentration (fig. 4)
plot_pm25_by_station <- function(data, caaqs_24h = 28, caaqs_annual = 10) {
  ## Filter data
  pm25_24h <- filter(data, metric == "pm2.5_24h")
  ymax_pm25_24h <- max(pm25_24h$metric_value_ambient, na.rm = TRUE)

  pm25_annual <- filter(data, metric == "pm2.5_annual")
  ymax_pm25_annual <- max(pm25_annual$metric_value_ambient, na.rm = TRUE)

  ## 24 hour
  plot_24h <- ggplot(
    pm25_24h,
    aes(
      x = reorder(station_name, metric_value_ambient, sum),
      y = metric_value_ambient
    )
  ) +
    geom_bar(stat = "identity", aes(fill = instrument_type)) +
    geom_text(aes(label = metric_value_ambient), nudge_y = 2.5, size = 2.82) +
    geom_hline(yintercept = caaqs_24h, color = "red", lty = "dashed") +
    annotation_custom(
      grob = textGrob("24h CAAQS", vjust = 1, gp = gpar(fontsize = 8)),
      xmin = -0.35,
      xmax = -0.35,
      ymin = caaqs_24h,
      ymax = caaqs_24h
    ) +
    scale_y_continuous(limits = c(0, max(ymax_pm25_24h + 3, caaqs_24h + 5))) +
    scale_fill_manual(values = c(FEM = "#4A8CE1", TEOM = "#070C72")) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Instrument",
      title = "24-Hr PM2.5"
    ) +
    theme(
      plot.margin = unit(c(2, 0, 0, 0), "lines"),
      plot.title = element_text(hjust = 0.5)
    ) +
    coord_flip(clip = "off")

  ## Annual
  plot_annual <- ggplot(
    pm25_annual,
    aes(
      x = reorder(station_name, metric_value_ambient, sum),
      y = metric_value_ambient
    )
  ) +
    geom_bar(stat = "identity", aes(fill = instrument_type)) +
    geom_text(aes(label = metric_value_ambient), nudge_y = 0.8, size = 2.82) +
    geom_hline(yintercept = caaqs_annual, color = "red", lty = "dashed") +
    annotate(
      geom = "text",
      label = "Annual\nCAAQS",
      x = 1,
      y = caaqs_annual + 2,
      size = 2.82
    ) +
    scale_y_continuous(
      limits = c(0, max(ymax_pm25_annual + 3, caaqs_annual + 6))
    ) +
    scale_fill_manual(values = c(FEM = "#81EDA1", TEOM = "#4A875B")) +
    labs(
      x = NULL,
      y = expression(PM[2.5]~Concentration~"("*mu~g/m^3~")"),
      fill = "Instrument",
      title = "Annual PM2.5"
    ) +
    theme(
      plot.margin = unit(c(2, 0, 0, 0), "lines"),
      plot.title = element_text(hjust = 0.5)
    ) +
    coord_flip(clip = "off")

  plot_24h / plot_annual

}

## Annual trends in mean PM2.5 concentration line chart
plot_pm25_station_timeseries <- function(data, airzone, caaqs_annual = 10) {
  ymax_pm25 <- max(data$ann_avg)
  min_year <- min(data$year, na.rm = TRUE)
  min_year <- min(data$year, na.rm = TRUE)

  ggplot(data, aes(x = year, y = ann_avg, colour = station_name)) +
    geom_line() +
    geom_point(aes(shape = instrument_type)) +
    geom_hline(yintercept = 10, color = "red", lty = "dashed") +
    annotate(
      geom = "text",
      label = "CAAQS",
      x = 2016,
      y = caaqs_annual + 1,
      size = 2.82
    ) +
    scale_x_continuous(
      breaks = function(x) floor(pretty(seq(min(x), max(x), by = 1)))
    ) +
    ## Set y scale from 0 to either: maximum pm2.5 value + a buffer, OR
    ## CAAQS + a buffer, whichever is larger
    scale_y_continuous(limits = c(0, max(ymax_pm25 + 3, caaqs_annual + 3))) +
    labs(
      x = "Year",
      y = expression(PM[2.5]~Concentration~"("*mu~g/m^3~")"),
      title = paste0(airzone, " Air Zone ", min_year, "-", max_year),
      colour = "Location",
      shape = "Instrument"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
}

## Captions --------------------------------------------------

ozone_by_station_cap <- function(airzone, ozone) {
  paste0(
    "Ozone concentrations in the ",
    airzone,
    " Air Zone, based on annual 4th highest daily 8-hour maxima, averaged over ",
    min(ozone$min_year, na.rm = TRUE),
    "-",
    max(ozone$max_year, na.rm = TRUE),
    ". Red dashed line identifies the CAAQS of 63 ppb."
  )
}

annual_ozone_cap <- function(annual_ozone) {
  paste0(
    "Annual trends in ozone concentrations (",
    min(annual_ozone$year, na.rm = TRUE),
    "-",
    max(annual_ozone$year, na.rm = TRUE),
    "), based on annual 4th highest daily 8-hour maxima for a single year. Red dashed line identifies CAAQS of 63 ppb."
  )
}

pm25_by_station_cap <- function(airzone, pm25) {
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
}

annual_pm25_cap <- function(annual_pm25) {
  annual_pm25_cap <- paste0(
    "Trends in PM$_{2.5}$ concentrations (",
    min(annual_pm25$year, na.rm = TRUE),
    "-",
    max(annual_pm25$year, na.rm = TRUE),
    "), based on annual mean concentrations from a single year. The CAAQS value of 10 $\\mu$g/m$^3$ is shown by the dashed line. PM$_{2.5}$ measurements prior to 2011 are reported at 25\\textdegree C and 1 atm. From 2011 onward, measurements are reported at local conditions."
  )
}

## Tables ----------------------------------------------------

## Choose color for cell based on management level
mgmt_level_color <- function(mgmt_level) {
  rcaaqs::get_colours(type = "management")[mgmt_level]
}

## Color cells by management level
color_by_mgmt_level <- function(value, mgmt_level) {
  cell_spec(
    value,
    "latex",
    color = text_color(mgmt_level),
    background = mgmt_level_color(mgmt_level)
  )
}

## If mgmt level is Actions for Achieving Air Zone CAAQS, text color should be
## white
text_color <- function(mgmt_level) {
  switch(
    as.character(mgmt_level),
    "Actions for Achieving Air Zone CAAQS" = "white",
    "black"
  )
}

create_ozone_table <- function(data, airzone) {
  overall_color <- mgmt_level_color(max(data$mgmt_level))
  overall_text <- text_color(max(data$mgmt_level))

  data %>%
    ## Select columns of interest
    select(
      station_name,
      n_years,
      metric_value_ambient,
      metric_value_mgmt,
      mgmt_level
    ) %>%
    ## Color management level cells
    mutate(
      metric_value_mgmt = map2_chr(
        metric_value_mgmt,
        mgmt_level,
        color_by_mgmt_level
      )
    ) %>%
    ## Group management level column to maximum value
    mutate(mgmt_level = max(mgmt_level)) %>%
    ## Rename column headers
    rename(
      Location = station_name,
      `No. Valid Years` = n_years,
      `As Measured` = metric_value_ambient,
      `TF/EE Influences Removed` = metric_value_mgmt,
      `Air Zone Management Level` = mgmt_level
    ) %>%
    kable(
      "latex",
      escape = FALSE,
      align = "c",
      caption = paste0(
        "Summary of ozone concentrations as measured and air zone management levels for the ",
        airzone,
        " Air Zone (based on ",
        min(data$min_year, na.rm = TRUE),
        "-",
        max(data$max_year, na.rm = TRUE),
        " data). All concentrations in ppb."
      )
    ) %>%
    column_spec(2, width = "0.5in") %>%
    column_spec(3:4, width = "0.75in") %>%
    ## Color in the Air Zone Management Level column
    column_spec(
      5,
      width = "1.5in",
      color = overall_text,
      background = overall_color
    ) %>%
    add_header_above(
      c(" " = 1, " " = 1, "4th Highest Daily \n 8-hour Maxima" = 2, " " = 1)
    ) %>%
    collapse_rows(columns = 5) %>%
    row_spec(0, background = "white") %>%
    kable_styling(latex_options = "hold_position")
}

create_pm25_table <- function(data, airzone) {
  ## Split the combined results and join to get separate columns for the metrics
  joined <- data %>%
    select(
      station_name,
      instrument_type,
      n_years,
      metric_value_ambient,
      metric_value_mgmt,
      mgmt_level,
      metric
    ) %>%
    split(.$metric) %>%
    Reduce(
      function(...) left_join(..., by = c("station_name", "instrument_type")),
      .
    )

  max_level <- max(joined$mgmt_level.x, joined$mgmt_level.y)
  overall_color <- mgmt_level_color(max_level)
  overall_text <- text_color(max_level)

  joined %>%
    ## Set mgmt_level column to maximum management level
    mutate(mgmt_level = max_level) %>%
    ## Color management columns
    mutate(
      metric_value_mgmt.x = map2_chr(
        metric_value_mgmt.x,
        mgmt_level.x,
        color_by_mgmt_level
      )
    ) %>%
    mutate(
      metric_value_mgmt.y = map2_chr(
        metric_value_mgmt.y,
        mgmt_level.y,
        color_by_mgmt_level
      )
    ) %>%
    ## Drop extra columns
    select(-mgmt_level.x, -mgmt_level.y, -n_years.y, -metric.x, -metric.y) %>%
    ## Create table
    kable(
      "latex",
      col.names = c(
        "Location",
        "Monitor Type",
        "No. Valid Years",
        "As Measured",
        "TF/EE Removed",
        "As Measured",
        "TF/EE Removed",
        "Air Zone Management Level"
      ),
      escape = FALSE,
      align = "c",
      caption = paste0(
        "Summary of PM$_{2.5}$ concentrations as measured and air zone management levels for the ",
        airzone,
        " Air Zone (based on ",
        min(data$min_year, na.rm = TRUE),
        "-",
        max(data$max_year, na.rm = TRUE),
        " data). All concentrations in $\\mu$g/m$^3$."
      )
    ) %>%
    column_spec(1, width = "1.25in") %>%
    column_spec(2, width = "0.5in") %>%
    column_spec(3, width = "0.3in") %>%
    column_spec(4:7, width = "0.5in") %>%
    column_spec(
      8,
      width = "1in",
      color = overall_text,
      background = overall_color
    ) %>%
    add_header_above(
      c(
        " " = 1,
        " " = 1,
        " " = 1,
        "Daily Mean (98th \n Percentile)" = 2,
        "Annual Mean" = 2,
        " " = 1
      )
    ) %>%
    collapse_rows(columns = 8) %>%
    row_spec(0, background = "white") %>%
    kable_styling(latex_options = "hold_position")
}

