#' Creates graphics for the management level graphs

source('./assets/00_setup.R')
dataDirectory <- './data/out/'
saveDirectory <- './data/plots/'

library(dplyr)
library(readr)
library(janitor)
library(rcaaqs)
library(lubridate)
library(stringr)
library(envreportutils)
library(sf)
library(bcmaps)
library(tidyr)
library(patchwork)

#create savedirectory
dir.create(saveDirectory)

#clear content of saveDirectory
del_files <- list.files(saveDirectory,full.names = TRUE)
del_files <- del_files[grepl('.svg',del_files)]
file.remove(del_files)

# access data created from "03_mgmt_analyze.R"
result_plots <- load_data(dataDirectory,'management_plots.Rds')


# PM2.5 plots-----
sites <- unique(names(result_plots$PM25))
for(s in sites) {
  message("Creating PM2.5 plots for ", s)
  
  if (0) {
    s <- sites[1]
  }
  try({
  pm25_24h_mgmt <- result_plots$PM25[[s]]$`24-Hour`
  pm25_annual_mgmt <- result_plots$PM25[[s]]$Annual
  
  g <- pm25_24h_mgmt/pm25_annual_mgmt
  
  ggsave(paste0(saveDirectory, s, "_pm25.svg"), g, 
         width = 778, height = 254*2, dpi = 72, units = "px", bg = "white")
  
  })
  # 
  # ggsave(paste0(saveDirectory, s, "_pm25_24h.svg"), pm25_24h_mgmt, 
  #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  # 
  # ggsave(paste0(saveDirectory, s, "_pm25_annual.svg"), pm25_annual_mgmt, 
  #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  
}

# O3 plots -----
sites <- unique(names(result_plots$O3))
for(s in sites) {
  message("Creating O3 plots for ", s)
  
  if (0) {
    s <- sites[1]
  }
  
  
  try({
  o3_8hr <- result_plots$O3[[s]]$`8-Hour`
  
  
  g <- o3_8hr
  
  ggsave(paste0(saveDirectory, s, "_o3.svg"), g, 
         width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  # 
  # ggsave(paste0(saveDirectory, s, "_pm25_24h.svg"), pm25_24h_mgmt, 
  #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  # 
  # ggsave(paste0(saveDirectory, s, "_pm25_annual.svg"), pm25_annual_mgmt, 
  #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  })
}
# NO2 plots-----
sites <- unique(names(result_plots$NO2))
for(s in sites) {
  message("Creating NO2 plots for ", s)
  
  if (0) {
    s <- sites[1]
  }
  
  try({
    no2_1hr <- result_plots$NO2[[s]]$`1-Hour`
    no2_annual <- result_plots$NO2[[s]]$Annual
    
    g <- no2_1hr/no2_annual
    
    ggsave(paste0(saveDirectory, s, "_no2.svg"), g, 
           width = 778, height = 254*2, dpi = 72, units = "px", bg = "white")
  })
  # 
  # ggsave(paste0(saveDirectory, s, "_pm25_24h.svg"), pm25_24h_mgmt, 
  #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  # 
  # ggsave(paste0(saveDirectory, s, "_pm25_annual.svg"), pm25_annual_mgmt, 
  #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  
}
# SO2 plots-----
sites <- unique(names(result_plots$SO2))
for(s in sites) {
  message("Creating SO2 plots for ", s)
  
  if (0) {
    s <- sites[1]
  }
  
  try({
    so2_1hr <- result_plots$SO2[[s]]$`1-Hour`
    so2_annual <- result_plots$SO2[[s]]$Annual
    
    g <- so2_1hr/so2_annual
    
    ggsave(paste0(saveDirectory, s, "_so2.svg"), g, 
           width = 778, height = 254*2, dpi = 72, units = "px", bg = "white")
    
  })
  # 
  # ggsave(paste0(saveDirectory, s, "_pm25_24h.svg"), pm25_24h_mgmt, 
  #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  # 
  # ggsave(paste0(saveDirectory, s, "_pm25_annual.svg"), pm25_annual_mgmt, 
  #        width = 778, height = 254, dpi = 72, units = "px", bg = "white")
  
}
