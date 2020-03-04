##############################################################
####  Functions to generate results for air zone reports  ####
##############################################################

## Load packages
library("tidyverse")
library("patchwork")
library("kableExtra")
library("rcaaqs")
library("grid")
library("sf")
library("bcmaps")
library("ggrepel")

## Set theme for plots
theme_set(theme_grey(base_size = 10))

## Maps ------------------------------------------------------

airzone_map <- function(airzone) {
  ## Get bounding box to set plot limits
  box <- st_bbox(filter(airzones(), Airzone == airzone))

  ## Top cities by population
  top_pop <- bc_cities() %>%
    st_join(airzones()) %>%
    filter(Airzone == airzone) %>%
    top_n(5, POP_2000)

  ggplot() +
    ## Plot major boundaries, air zones, and top cities
    geom_sf(data = bc_neighbours(), aes(fill = iso_a2)) +
    geom_sf(data = airzones(), color = "red", fill = NA) +
    geom_sf(data = top_pop) +
    ## Label cities
    geom_label_repel(
      data = top_pop,
      aes(label = NAME, geometry = geometry),
      stat = "sf_coordinates",
      min.segment.length = 0,
      size = 3,
      label.size = NA
    ) +
    coord_sf(
      datum = NA,
      xlim = c(box$xmin, box$xmax),
      ylim = c(box$ymin, box$ymax)
    ) +
    scale_fill_manual(
      name = NULL,
      values = c(OC = "lightblue", CA = "white", US = "grey90")
    ) +
    theme_void() +
    theme(legend.position = "none",
          panel.border = element_rect(colour = "black", fill = NA, size = 1.5)) +
    labs(title = paste(airzone, "Air Zone"))
}

## Plots -----------------------------------------------------

## Ozone concentration based on annual 4th highest daily 8-hour maxima, 3 year
## average bar chart by station (fig. 2)
plot_ozone_by_station <- function(data, airzone, caaqs = 63) {
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
    geom_hline(
      aes(linetype = "CAAQS", yintercept = caaqs),
      color = "red"
    ) +
    scale_linetype_manual(
      name = NULL,
      values = 2,
      guide = guide_legend(override.aes = list(color = "red"))
    ) +
    labs(
      x = NULL,
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

  ggplot(data, aes(x = year, y = ann_4th_highest, colour = station_name)) +
    geom_line() +
    geom_hline(
      aes(linetype = "CAAQS", yintercept = caaqs),
      color = "red"
    ) +
    scale_linetype_manual(
      name = NULL,
      values = 2,
      guide = guide_legend(override.aes = list(color = "red"))
    ) +
    labs(
      x = "Year",
      y = "Ozone concentration (ppb)",
      title = paste0(airzone, " Air Zone\n", min_year, "-", max_year),
      colour = "Location"
    ) +
    scale_x_continuous(
      limits = c(max_year - 9, max_year),
      breaks = function(x) floor(pretty(seq(min(x), max(x), by = 1)))
    ) +
    theme(plot.title = element_text(hjust = 0.5))
}

## PM2.5 concentrations bar charts (24-hour concentration and annual mean
## concentration (fig. 4)
plot_pm25_by_station <- function(data, caaqs_24h = 28, caaqs_annual = 10) {
  ## Filter data
  pm25_24h <- filter(data, metric == "pm2.5_24h")
  pm25_annual <- filter(data, metric == "pm2.5_annual")

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
    geom_hline(
      aes(linetype = "24h CAAQS", yintercept = caaqs_24h),
      color = "red"
    ) +
    scale_linetype_manual(
      name = NULL,
      values = 2,
      guide = guide_legend(override.aes = list(color = "red"))
    ) +
    scale_fill_manual(values = c(FEM = "#4A8CE1", TEOM = "#070C72")) +
    guides(
      fill = guide_legend(order = 2),
      linetype = guide_legend(order = 1)
    ) +
    labs(
      x = NULL,
      y = NULL,
      fill = "Instrument",
      title = expression(paste("24-Hr", ~PM[2.5]))
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
    geom_hline(
      aes(linetype = "Annual CAAQS", yintercept = caaqs_annual),
      color = "red"
    ) +
    scale_linetype_manual(
      name = NULL,
      values = 2,
      guide = guide_legend(override.aes = list(color = "red"))
    ) +
    scale_fill_manual(values = c(FEM = "#81EDA1", TEOM = "#4A875B")) +
    guides(
      fill = guide_legend(order = 2),
      linetype = guide_legend(order = 1)
    ) +
    labs(
      x = NULL,
      y = expression(PM[2.5]~Concentration~"("*mu~g/m^3~")"),
      fill = "Instrument",
      title = expression(Annual~PM[2.5])
    ) +
    theme(
      plot.margin = unit(c(2, 0, 0, 0), "lines"),
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 20))
    ) +
    coord_flip(clip = "off")

  plot_24h / plot_annual

}

## Annual trends in mean PM2.5 concentration line chart
plot_pm25_station_timeseries <- function(data, airzone, caaqs_annual = 10) {
  min_year <- min(data$year, na.rm = TRUE)
  max_year <- max(data$year, na.rm = TRUE)

  ggplot(data, aes(x = year, y = ann_avg, colour = station_name)) +
    geom_line() +
    geom_point(aes(shape = instrument_type)) +
    geom_hline(
      aes(linetype = "CAAQS", yintercept = caaqs_annual),
      color = "red"
    ) +
    scale_linetype_manual(
      name = NULL,
      values = 2,
      guide = guide_legend(override.aes = list(color = "red"))
    ) +
    scale_x_continuous(
      limits = c(max_year - 9, max_year),
      breaks = function(x) floor(pretty(seq(min(x), max(x), by = 1)))
    ) +
    labs(
      x = "Year",
      y = expression(PM[2.5]~Concentration~"("*mu~g/m^3~")"),
      title = paste0(airzone, " Air Zone\n", min_year, "-", max_year),
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
    kable_styling(latex_options = "HOLD_position") %>% 
    bottom_hline()
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
    kable_styling(latex_options = "HOLD_position") %>%
    bottom_hline()
}

bottom_hline <- function(tex_table) {
  if (grepl("\\hline\n\\end{tabular}\n\\end{table}", tex_table, fixed = TRUE)) {
    return(tex_table)
  }
  gsub("\\end{tabular}\n\\end{table}",
       "\\\\\n\\hline\n\\end{tabular}\n\\end{table}",
       tex_table,
       fixed = TRUE)
}

achievement_level <- function(param) {
  rcaaqs::achievement_levels$upper_breaks[
    rcaaqs::achievement_levels$parameter == param & 
      rcaaqs::achievement_levels$labels == "Achieved"
  ]
}

calc_threshold <- function(achievement_status) {
  # convert to character in case it's a factor!
  achievement_status <- as.character(achievement_status)
  
  unname(
    c("Achieved" = "below", 
      "Not Achieved" = "above", 
      "Insufficient Data" = "Not enough data")[achievement_status]
  )
}

# caaqs_table

caaqs_standard_table <- function() {
  cat(
"
\\begingroup
\\renewcommand{\\arraystretch}{1.5}
\\captionof{table}{Air zone management framework for ground-level ozone and 
PM$_{2.5}$.The CAAQS define the upper threshold, separating the “red” and 
“orange” management levels.} \\label{tab:title}
\\begin{tabularx}{\\textwidth}{ |l| *{6}{Y|} }
\\hline
\\multicolumn{1}{|c|}{ } & \\multicolumn{2}{c|}{O$_3$ (ppb)} & \\multicolumn{2}{c|}{PM$_{2.5}$ - Annual ($\\mu$g/m$^3$)} & \\multicolumn{2}{c|}{PM$_{2.5}$ - 24h ($\\mu$g/m$^3$)}\\\\
\\cline{2-3} \\cline{4-5} \\cline{6-7}
Management Level & 2015 & 2020 & 2015 & 2020 & 2015 & 2020\\\\
\\hline
\\rowcolor{caaqs_red}
\\multicolumn{1}{|l|}{\\textbf{\\color{white}Red}} & \\multicolumn{6}{c|}{\\color{white}Actions for Achieving Air Zone CAAQS}\\\\
\\hline
\\hspace{1em}Threshold (CAAQS) & 63 & 62 & 10 & 8.8 & 28 & 27\\\\
\\hline
\\rowcolor{caaqs_orange}
\\multicolumn{1}{|l|}{\\textbf{Orange}} & \\multicolumn{6}{c|}{Actions for Preventing CAAQS Exceedence}\\\\
\\hline
\\hspace{1em}Threshold & \\multicolumn{2}{c|}{56} & \\multicolumn{2}{c|}{6.4} & \\multicolumn{2}{c|}{19}\\\\
\\hline
\\rowcolor{caaqs_yellow}
\\multicolumn{1}{|l|}{\\textbf{Yellow}} & \\multicolumn{6}{c|}{Actions for Preventing Air Quality Deterioration}\\\\
\\hline
\\hspace{1em}Threshold & \\multicolumn{2}{c|}{50} & \\multicolumn{2}{c|}{4} & \\multicolumn{2}{c|}{10}\\\\
\\hline
\\rowcolor{caaqs_green}
\\multicolumn{1}{|l|}{\\textbf{Green}} & \\multicolumn{6}{c|}{Actions for Keeping Clean Areas Clean}\\\\
\\hline
\\end{tabularx}
\\renewcommand{\\arraystretch}{1}
\\endgroup
"
)
}

table_1_caption <- function(){
  "Table 1. Air zone management framework for ground-level ozoneand PM2.5.The CAAQS definethe upper threshold, separating the “red” and “orange” management levels."
}

pm_achievement_sentence <- function(pm25_az_df) {
  pm24_az <- pm25_az_df %>%
    filter(metric == "pm2.5_24h") 
  
  pm_annual_az <- pm25_az_df %>%
    filter(metric == "pm2.5_annual")
  
  first_part <- paste(
    "Over this time period, a 24-hour average value of", 
    pm24_az$metric_value_ambient, 
    "$\\mu$g/m\\textsuperscript{3} and an annual mean of", 
    pm_annual_az$metric_value_ambient, 
    "$\\mu$g/m\\textsuperscript{3} were obtained."
  )
  
  which_achieved <- pm25_az_df$metric[as.character(pm25_az_df$caaqs_ambient) == "Achieved"]
  which_not_achieved <- pm25_az_df$metric[as.character(pm25_az_df$caaqs_ambient) == "Not Achieved"]
  both_achieved <- length(which_achieved) == 2L
  none_achieved <- length(which_not_achieved) == 2L
  
  second_part <- if (both_achieved || none_achieved) {
    paste(
      "This indcates that PM~2.5~ levels at this site achieved", 
      ifelse(both_achieved, "both", "neither"), 
      "the 24-hour", 
      ifelse(both_achieved, "and", "nor the"),
      "annual national standards of", 
      achievement_level("pm2.5_24h"), "and",
      achievement_level("pm2.5_annual"),  
      "$\\mu$g/m\\textsuperscript{3}, respectively."
    )
  } else {
    which_achieved_english <- gsub("h$", "-hour", strsplit(which_achieved, "_")[[1]][2])
    which_not_achieved_english <- gsub("h$", "-hour", strsplit(which_not_achieved, "_")[[1]][2])
    paste(
      "This indcates that PM~2.5~ levels at this site achieved the", 
      which_achieved_english, "annual national standard of", 
      achievement_level(which_achieved), "$\\mu$g/m\\textsuperscript{3} but not the", 
      which_not_achieved_english, "standard of", 
      achievement_level(which_not_achieved), "$\\mu$g/m\\textsuperscript{3}."
    )
  }
  
  paste(first_part, second_part)
}

render_child <- function(file) {
  res <- knitr::knit_child(file, 
                           envir = knitr::knit_global(), 
                           quiet = TRUE)
  cat(res, sep = "\n")
}
