#' Data that are downloadable directly or from the interactive apps
#' 
source('./assets/00_setup.R')
library(dplyr)
library(tidyr)
folder_data <- './data/out'
filelist <- list.files(folder_data)
# paste(filelist,collapse =',')
df_mgmt <- load_data(datapath = folder_data,'management.csv')
df_mgmt_airzone <- load_data(datapath = folder_data,'management_airzones.csv')


colnames(df_mgmt)
df_mgmt <- df_mgmt %>% ungroup() %>%
  select(site,instrument,year,parameter,metric,metric_value,tfee,colour_text) %>%
  unique() %>%
  rename(management_level = colour_text) %>%
  mutate(tfee = ifelse(tfee,'TFEE adjusted','no TFEE adjustment'),
         metric_value = as.character(metric_value)) %>%
  pivot_wider(names_from = tfee, values_from = c(metric_value,management_level)) 

write_csv(df_mgmt,'./data/downloads/management_metrics_download.csv')

#summary for the air zone
colnames(df_mgmt_airzone)
df_mgmt_airzone <- df_mgmt_airzone %>% 
  ungroup() %>%distinct() %>%
  select(year,airzone,pollutant,tfee,colour_text) %>%
  mutate(tfee = ifelse(tfee,'TFEE adjusted','no TFEE adjustment')) %>%
  pivot_wider(names_from = tfee,values_from = colour_text) %>%
  arrange(airzone,pollutant,year) 

write_csv(df_mgmt,'./data/downloads/management_airzone_download.csv')
