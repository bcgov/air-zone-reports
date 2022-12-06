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
  ~savefile,~outputtype,
  'exceed_annual.Rds','freq_annual',
  'exceed_month.Rds','freq_month',
  'exceed_seasonal.Rds','freq_seasonal',
  'exceedances.Rds','filter'
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
  
}





