#identify TFEE

library(envair)
library(dplyr)

validation_year <- 2021

pm25 <- importBC_data_avg('pm25',year = 2019:2021,averaging_type = '24hr')
tfee <- get_tfee()
tfee_pm25 <- tfee %>%
  filter(PARAMETER == 'PM25') %>%
  mutate(index = paste(STATION_NAME,DATE))

pm25_high_unlisted <- pm25 %>%
  mutate(index = paste(STATION_NAME,DATE)) %>%
  filter(RAW_VALUE_24h >27) %>%
  filter(!index %in% tfee_pm25$index)
