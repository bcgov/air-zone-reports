#' clean up some data files
#' 
source('./assets/00_setup.R')
dataDirectory <- './data/out'

library(dplyr)
library(janitor)

lst_stations <- load_data(dataDirectory,'liststations.csv') %>%
  clean_names()
df_data <- load_data(dataDirectory,'aq_data.Rds')

# -fix station names, remove extra space, and #
df_data$station_name <- gsub('  ',' ',df_data$station_name)
lst_stations$station_name <- gsub('  ',' ',lst_stations$station_name)
lst_stations$site <- gsub('  ',' ',lst_stations$site)

az <- airzones() %>% 
  st_make_valid()

station_clean <- lst_stations %>%
  select(site,station_name,label,lat,long,aqms) %>%
  rename(lon = long) %>%
  group_by(site) %>%
  slice(1) %>%
  filter(!is.na(lat)) %>%
  assign_airzone(airzones = az,station_id = 'site', coords = c("lon","lat")) %>%
  filter(!is.na(airzone)) 

write_csv(station_clean,'./data/out/station_clean.csv')
saveRDS(df_data,'./data/out/aq_data.Rds')
