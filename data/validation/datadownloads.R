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
lst_stations <- listBC_stations(use_CAAQS = TRUE)

try({
  lst_stations$STATION_NAME_FULL = gsub('[^[:alnum:]]',' ',lst_stations$STATION_NAME_FULL)
  lst_stations$STATION_NAME = gsub('[^[:alnum:]]',' ',lst_stations$STATION_NAME)
  lst_stations$STATION_NAME_FULL = gsub('  ',' ',lst_stations$STATION_NAME_FULL)
  lst_stations$STATION_NAME = gsub('  ',' ',lst_stations$STATION_NAME)
  lst_stations$site = gsub('[^[:alnum:]]',' ',lst_stations$site)
  lst_stations$site = gsub('  ',' ',lst_stations$site)
})



colnames(df_mgmt)
df_mgmt <- df_mgmt %>% ungroup() %>%
  select(site,instrument,year,parameter,metric,metric_value,tfee,colour_text) %>%
  unique() %>%
  rename(management_level = colour_text) %>%
  mutate(tfee = ifelse(tfee,'TFEE adjusted','no TFEE adjustment'),
         metric_value = as.character(metric_value)) %>%
  pivot_wider(names_from = tfee, values_from = c(metric_value,management_level)) 

#for the graphics
colnames(df_mgmt)
colnames(lst_stations)

lst_stations_filtered <- lst_stations %>%
  select(site,Label,AIRZONE) %>%
  group_by(site) %>% slice(1) %>% ungroup()

#this should be zero entry
df_mgmt %>%
  left_join(lst_stations_filtered) %>%
  # filter(year == 2021) %>%
  select(site,AIRZONE) %>% distinct() %>%
  filter(is.na(AIRZONE))

df_mgmt <- df_mgmt %>%
  left_join(lst_stations_filtered)
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


df_mgmt_airzone




View(df_mgmt_result)

#graphical results
colnames(df_mgmt_result)

#PM25
result_pm <- df_mgmt_result %>%
  select(Label,year,AIRZONE,metric,parameter,
         `management_level_no TFEE adjustment`,`management_level_TFEE adjusted`,
         `metric_value_TFEE adjusted`,`metric_value_no TFEE adjustment`) %>%
  filter(parameter == 'pm25',year == 2021) %>%
  arrange(AIRZONE,Label,metric)

write_csv(result_pm,'./data/downloads/management_metrics_pm25.csv')

#O3
result_o3 <- df_mgmt_result %>%
  select(Label,year,AIRZONE,metric,parameter,
         `management_level_no TFEE adjustment`,`management_level_TFEE adjusted`,
         `metric_value_TFEE adjusted`,`metric_value_no TFEE adjustment`) %>%
  filter(parameter == 'o3',year == 2021) %>%
  arrange(AIRZONE,Label,metric)

write_csv(result_o3,'./data/downloads/management_metrics_o3.csv')

#NO2
result_no2 <- df_mgmt_result %>%
  select(Label,year,AIRZONE,metric,parameter,
         `management_level_no TFEE adjustment`,`management_level_TFEE adjusted`,
         `metric_value_TFEE adjusted`,`metric_value_no TFEE adjustment`) %>%
  filter(parameter == 'no2',year == 2021) %>%
  arrange(AIRZONE,Label,metric)

write_csv(result_no2,'./data/downloads/management_metrics_no2.csv')

#SO2
result_so2 <- df_mgmt_result %>%
  select(Label,year,AIRZONE,metric,parameter,
         `management_level_no TFEE adjustment`,`management_level_TFEE adjusted`,
         `metric_value_TFEE adjusted`,`metric_value_no TFEE adjustment`) %>%
  filter(parameter == 'so2',year == 2021) %>%
  arrange(AIRZONE,Label,metric)

write_csv(result_so2,'./data/downloads/management_metrics_so2.csv')

