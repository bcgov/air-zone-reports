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
    geom_text(aes(label = metric_value_ambient), nudge_y = 2) +
    geom_hline(yintercept = caaqs, color = "red", lty = "dashed") +
    annotate(geom = "text", label = "CAAQS", x = 1, y = caaqs + 5) +
    ## Set y scale to ensure CAAQS text doesn't get cut off (but if the max ozone
    ## value is greater, take that value instead + 5 for padding)
    scale_y_continuous(limits = c(0, max(ymax_oz + 5, caaqs + 8))) +
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
    annotate(geom = "text", label = "CAAQS", x = 2016, y = caaqs + 5) +
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
    aes(x = reorder(station_name, metric_value_ambient, sum), y = metric_value_ambient)
  ) +
    geom_bar(stat = "identity", aes(fill = instrument_type)) +
    geom_text(aes(label = metric_value_ambient), nudge_y = 2) +
    geom_hline(yintercept = caaqs_24h, color = "red", lty = "dashed") +
    annotate(geom = "text", label = "24h CAAQS", x = 2, y = caaqs_24h + 9) +
    scale_y_continuous(limits = c(0, max(ymax_pm25_24h + 3, caaqs_24h + 5))) +
    scale_fill_manual(values = c(FEM = "#4A8CE1", TEOM = "#070C72")) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Instrument",
      title = "24-Hr PM2.5"
    ) +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5))

  ## Annual
  plot_annual <- ggplot(
    pm25_annual,
    aes(x = reorder(station_name, metric_value_ambient, sum), y = metric_value_ambient)
  ) +
    geom_bar(stat = "identity", aes(fill = instrument_type)) +
    geom_text(aes(label = metric_value_ambient), nudge_y = 0.8) +
    geom_hline(yintercept = caaqs_annual, color = "red", lty = "dashed") +
    annotate(geom = "text", label = "Annual\nCAAQS", x = 2, y = caaqs_annual + 2) +
    scale_y_continuous(limits = c(0, max(ymax_pm25_annual + 1, caaqs_annual + 5))) +
    scale_fill_manual(values = c(FEM = "#81EDA1", TEOM = "#4A875B")) +
    labs(
      x = NULL,
      y = expression(PM[2.5]~Concentration~"("*mu~g/m^3~")"),
      fill = "Instrument",
      title = "Annual PM2.5"
    ) +
    coord_flip() +
    theme(plot.title = element_text(hjust = 0.5))

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
    annotate(geom = "text", label = "CAAQS", x = 2016, y = caaqs_annual + 1) +
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

## Tables ----------------------------------------------------
