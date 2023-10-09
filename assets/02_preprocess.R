# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# The scripts here generates csv files
# that will be used as input for the shiny and web apps
# RUN only if:
#      - data content needs to be updated, such as, after completing Level 2 data validation
#      - if the test data folder is missing or has no data content


# add 00_setup.R functions
library(dplyr)
source('./assets/00_setup.R')

saveDirectory <- './data/out'


# Retrieve data
list.files('./data/out')
raw_data <- readRDS('./data/out/raw_data_caaqs.Rds')

#create file to save the frequency of exceedances----
#save into 'freq_annual','freq_month','freq_hour','freq_seasonal'
df_exceedances <- tribble(
  ~savefile,~outputtype,~savefile2
  'exceed_annual.Rds','freq_annual','exceed_annual.csv',
  'exceed_month.Rds','freq_month','exceed_month.csv',
  'exceed_seasonal.Rds','freq_seasonal','exceed_seasonal.csv',
  'exceedances.Rds','filter','exceedances.csv'
)

df_exceedances$savefile <- paste(saveDirectory,df_exceedances$savefile,sep='/')

for (outputtype in df_exceedances$outputtype) {
  pm25_freq <- calc_exceedances(df = raw_data,parameter = 'pm25',outputtype=outputtype)
  no2_freq <- calc_exceedances(df = raw_data,parameter = 'no2',outputtype=outputtype)
  so2_freq <- calc_exceedances(df = raw_data,parameter = 'so2',outputtype=outputtype)
  o3_freq <- calc_exceedances(df = raw_data,parameter = 'o3',outputtype=outputtype)
  
  df_freq <- pm25_freq %>%
    bind_rows(no2_freq) %>%
    bind_rows(so2_freq) %>%
    bind_rows(o3_freq)
  
  #rename the 'value' column
  if (outputtype == 'filter') {
    colnames(df_freq)[colnames(df_freq) == 'value'] <- 'measurement'
  }
  cols_exceed <- colnames(df_freq)[grepl('exceed',colnames(df_freq))]
  
  df_freq <- df_freq %>%
    tidyr::pivot_longer(cols = cols_exceed) %>%
    filter(!is.na(value)) 
  
  #remove data that is not exceedance
  if (outputtype == 'filter') {
    df_freq <- df_freq[df_freq$value == TRUE,]
  }
  
  saveRDS(df_freq,df_exceedances$savefile[df_exceedances$outputtype == outputtype])
  readr::write_csv(df_freq,df_exceedances$savefile2[df_exceedances$outputtype == outputtype])
}


#for some spatial averaging
#average all measurements from each air zone
#average the air zone altogether....
#removes bias from spatial, taking each air zone equal weight


#special look at PM2.5, 24-hour metric

lst_stations <- listBC_stations(use_CAAQS=TRUE)

lst_remove <- lst_stations %>%
  filter(AQMS =='N') %>%
  pull(STATION_NAME)

raw_data_pm25 <- raw_data %>%
  filter(PARAMETER == 'PM25') 

raw_data_PM25_excess <- raw_data_pm25 %>%
  
  filter(!is.na(RAW_VALUE)) %>%
  filter(!STATION_NAME %in% lst_remove) %>%
  group_by(STATION_NAME,INSTRUMENT,DATE) %>%
  dplyr::summarise(value_24 = mean(RAW_VALUE,na.rm = TRUE), valid_n =n()) %>%
  filter(valid_n>=0.75*24) %>%
  filter(envair::round2(value_24,0)>27) 

saveRDS(raw_data_PM25_excess,'./data/out/pm25_excess_dates.Rds')

excess_daily <- raw_data_PM25_excess %>%
  mutate(YEAR = year(DATE), MONTH = paste(month))
  group_by(DATE) %>%
  mutate()

  #create list of aqhi stations
  url_aqhi <- 'https://envistaweb.env.gov.bc.ca/aqo/setup/BC_AQHI_SITES_AQHIPlusSO2.csv'
  aqhi_stations <- read_csv(url_aqhi) %>%
    select(AQHI_AREA,LATITUDE,LONGITUDE) %>%
    group_by(AQHI_AREA) %>%
    slice(1) %>%
    get_airzone_df() %>%
    mutate(airzone = ifelse(grepl('Metro Vancouver',AQHI_AREA),'Lower Fraser Valley',airzone))
 
  
  
  readr::write_csv(aqhi_stations,'./data/out/liststations_aqhi.csv')
  
