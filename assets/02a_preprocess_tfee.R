#The script here finds location of elevated PM2.5 levels
#these levels can be TFEE

pm25 <- importBC_data('pm25',2022)
lst_stations <- listBC_stations(use_CAAQS = TRUE)

remove_stations <- lst_stations %>%
  filter(AQMS == 'N') %>%
  pull(STATION_NAME)

pm25 %>%
  group_by(DATE,STATION_NAME,INSTRUMENT) %>%
  filter(!is.na(RAW_VALUE),!STATION_NAME %in% remove_stations) %>%
  dplyr::summarise(RAW_VALUE_24h = mean(RAW_VALUE,na.rm = TRUE)) %>%
  filter(RAW_VALUE_24h>27) %>%
  
  filter(ymd(DATE) >= ymd('2022-07-14'), ymd(DATE)<= ymd('2022-10-20')) %>%
  select(STATION_NAME,INSTRUMENT,DATE) %>%
  readr::write_csv('pm25_tfee_2022_prelim.csv')
