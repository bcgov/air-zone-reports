#' Create graphs of the folder
#' 
source('./assets/00_setup.R')
dataDirectory <- './data/out/'

library(dplyr)
library(readr)
library(janitor)
library(rcaaqs)
library(lubridate)
library(stringr)
library(envreportutils)
library(sf)
library(bcmaps)
library(ggtext)

# load data----
result <- load_data(dataDirectory,'management_results.Rds')
pm25_results <- result[['management_all']]
  
#derive values extracted from source data----

pm25_24h_mgmt <- result[['pm25_24h_mgmt']]
pm25_annual_mgmt <- result[['pm25_annual_mgmt']]
ozone_mgmt <- result[['ozone_mgmt']]
no2_mgmt_hour <- result[['no2_mgmt_hour']]
no2_mgmt_annual <- result[['no2_mgmt_annual']]
so2_mgmt_hour <- result[['so2_mgmt_hour']]
so2_mgmt_annual <- result[['so2_mgmt_annual']]

#create list of sites based on parameters------
sites_pm25 <- pm25_results %>%
  arrange(airzone, site, metric) %>%
  filter(parameter == 'PM25') %>%
  pull(site) %>%
  sort() %>%
  unique()
sites_o3 <- pm25_results %>%
  arrange(airzone, site, metric) %>%
  filter(parameter == 'O3') %>%
  pull(site) %>%
  sort() %>%
  unique()
sites_no2 <- pm25_results %>%
  arrange(airzone, site, metric) %>%
  filter(parameter == 'NO2') %>%
  pull(site) %>%
  sort() %>%
  unique()
sites_so2 <- pm25_results %>%
  arrange(airzone, site, metric) %>%
  filter(parameter == 'SO2') %>%
  pull(site) %>%
  sort() %>%
  unique()

# generate plots for each parameter-----
stn_plots <- list('PM25','NO2','O3','SO2')

# -add pm25 graphs
for(s in sites_pm25) {
  message("Creating PM2.5 plots for ", s)

  try({
  g1 <- plot_caaqs(pm25_24h_mgmt, id = s, id_col = "site", year_min = 2013,
                   plot_std = FALSE, plot_mgmt = FALSE)
  g1 <- add_caaqs_historic(g1, metric = "pm2.5_24h")
  
  g2 <- plot_caaqs(pm25_annual_mgmt, id = s, id_col = "site", year_min = 2013,
                   plot_std = FALSE, plot_mgmt = FALSE)
  g2 <- add_caaqs_historic(g2, metric = "pm2.5_annual")
  
  #activate only if site is part of leaflet output
  # ggsave(paste0("leaflet_map/station_plots/", s, "_24h.svg"), g1, 
  #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  # 
  # ggsave(paste0("leaflet_map/station_plots/", s, "_annual.svg"), g2, 
  #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  # 
  # Save for print version
  stn_plots$PM25[[s]][["24-Hour"]] <- g1
  stn_plots$PM25[[s]][["Annual"]] <- g2
  })
}
# -add ozone graphs
for(s in sites_o3) {
  message("Creating O3 plots for ", s)
  
  try({
    g1 <- plot_caaqs(ozone_mgmt, id = s, id_col = "site", year_min = 2013,
                     plot_std = FALSE, plot_mgmt = FALSE)
    g1 <- add_caaqs_historic(g1, metric = "o3")
    
    stn_plots$O3[[s]][["8-Hour"]] <- g1
    
  })
}
# -add no2 graphs
for(s in sites_no2) {
  message("Creating NO2 plots for ", s)
  
  try({
    g1 <- plot_caaqs(no2_mgmt_annual, id = s, id_col = "site", year_min = 2013)
                     
    g2 <- plot_caaqs(no2_mgmt_hour, id = s, id_col = "site", year_min = 2013)
    
    #activate only if site is part of leaflet output
    # ggsave(paste0("leaflet_map/station_plots/", s, "_24h.svg"), g1, 
    #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
    # 
    # ggsave(paste0("leaflet_map/station_plots/", s, "_annual.svg"), g2, 
    #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
    # 
    # Save for print version
    stn_plots$NO2[[s]][["Annual"]] <- g1
    stn_plots$NO2[[s]][["1-Hour"]] <- g2
  })
}
# -add no2 graphs
for(s in sites_so2) {
  message("Creating SO2 plots for ", s)
  
  try({
    g1 <- plot_caaqs(so2_mgmt_annual, id = s, id_col = "site", year_min = 2013)
    g2 <- plot_caaqs(so2_mgmt_hour, id = s, id_col = "site", year_min = 2013)
    
    
    #activate only if site is part of leaflet output
    # ggsave(paste0("leaflet_map/station_plots/", s, "_24h.svg"), g1, 
    #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
    # 
    # ggsave(paste0("leaflet_map/station_plots/", s, "_annual.svg"), g2, 
    #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
    # 
    # Save for print version
    stn_plots$SO2[[s]][["Annual"]] <- g1
    stn_plots$SO2[[s]][["1-Hour"]] <- g2
  })
}

saveRDS(stn_plots,'./data/out/management_plots.Rds')
