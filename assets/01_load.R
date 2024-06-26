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

# enter what year this i
#load sources----

source('./assets/00_setup.R')


# -for validated year
validation_year <- 2021
saveDirectory <- './data/out'

# -for test output
if (0) {
  validation_year <- 2023
  yrs <- 2013:validation_year
  saveDirectory <- './data/export'
  df_caaqs_annual <- create_caaqs_annual(years = yrs)
  readr::write_csv(df_caaqs_annual,paste(saveDirectory,'caaqs_results.csv',sep='/'))
  df_management_summary <- get_management_summary(datafile = paste(saveDirectory,'caaqs_results.csv',sep='/'))
  readr::write_csv(df_management_summary,paste(saveDirectory,'management.csv',sep='/'))
  
}

# set the working and save directory-----


dir.create(saveDirectory,recursive = TRUE)


yrs <- 2013:validation_year


print(getwd())

require(sf)
require(bcmaps)
require(envair)
require(envreportutils)
require(rcaaqs)
require(janitor)
require(lubridate)
require(dplyr)
library(readr)

# define where data files will be saved


# -saves the data from 2011 to current validation year
# retrieve pm, o3, no2, and so2 data
df_data <- importBC_data(c('pm25','no2','so2','o3'), years = 2011:validation_year, flag_TFEE = TRUE,merge_Stations = TRUE)
df_data <- clean_names(df_data) %>%
  mutate(date_time = date_pst - hours(1)) %>%
  select(date_time,date_pst,date,time,everything())

df_data <- df_data %>%
  select(-station_name_full) %>%
  distinct()

saveRDS(df_data,paste(saveDirectory,'aq_data.Rds',sep='/'))

# Calculate the caaqs values----
# Calculations are based on the rcaaqs package
# using envair for data retrieval
# this generates the file: caaqs_results.csv
# this function output will append on itself, so years can be operated in chunks

# -saves data into caaqs_results.csv
df_caaqs_annual <- create_caaqs_annual(years = yrs)
readr::write_csv(df_caaqs_annual,paste(saveDirectory,'caaqs_results.csv',sep='/'))

# Create management levels summary-----
# -create management.csv
df_management_summary <-  get_management_summary(datafile = paste(saveDirectory,'caaqs_results.csv',sep='/'))
readr::write_csv(df_management_summary,paste(saveDirectory,'management.csv',sep='/'))

# Create summary of achievement of CAAQS
df_achievement <- get_caaqs_achievement(caaqs_result_dir = paste(saveDirectory,'caaqs_results.csv',sep='/'))
readr::write_csv(df_achievement[['detailed']],paste(saveDirectory,'achievement.csv',sep='/'))


# Create NPRI data----
fileNPRI <- paste(saveDirectory,'NPRI.csv',sep='/')
df_NPRI <- envair::get_npri()

df_NPRI %>%
  filter(province == 'BC') %>%
  readr::write_csv(fileNPRI)

# save APEI data----
download.file(url = 'https://data-donnees.ec.gc.ca/data/substances/monitor/canada-s-air-pollutant-emissions-inventory/EN_APEI-Can-Prov_Terr.csv',
              destfile = paste(saveDirectory,'EN_APEI-Can-Prov_Terr.csv',sep='/')
)

# create quick list of bc stations-----
# add if active within the past 5 years
envair::listBC_stations(use_CAAQS = TRUE,merge_Stations = TRUE) %>%
  saveRDS(paste(saveDirectory,'liststations_merged.Rds',sep='/'))

#create tfee list
envair::get_tfee() %>%
  readr::write_csv(paste(saveDirectory,'tfee.csv',sep='/'))

#secondary_test tfee
# pm25_tfee_prelim <- readr::read_csv('./data/out/pm25_tfee_2022_prelim.csv') %>%
#   mutate(PARAMETER = 'PM25') 

pm25_tfee <- envair::get_tfee()

pm25_tfee %>%
  mutate(DATE = as.Date(DATE)) %>%
  bind_rows(pm25_tfee_prelim) %>%
  readr::write_csv(paste(saveDirectory,'tfee.csv',sep='/'))


#create quick mapping details
#change to (1) if update needed on the 
#airzone polygon map
#content update is not necessary once
#file is already there
if (0) {
  bcmaps::airzones() %>%
    sf::st_make_valid() %>%
    sf::st_transform(st_crs(bc_bound())) %>%
    sf::st_intersection(st_geometry(bc_bound())) %>%
    group_by(Airzone) %>%
    summarize() %>%
    sf::st_transform(4326) %>%
    saveRDS(paste(saveDirectory,'az_mgmt.Rds',sep='/'))
}


# OPTIONAL: Create annual metrics file----
# Calculations for YOY metrics are based on the envair package
# this generates the file: annual_results.csv
create_metrics_annual(years = 1980:max(years),savedirectory = saveDirectory)

# Load and Save Raw Data-----------
# This will take a long time also
save_data(parameters = c('pm25','no2','o3','so2','pm10','trs'),
          years = 1980:max(years),
          outputfile = paste(saveDirectory,'raw_data.Rds',sep='/'))

save_data(parameters = c('pm25','no2','o3','so2','pm10','trs'),
          years = 1980:max(years),
          outputfile = paste(saveDirectory,'raw_data_caaqs.Rds',sep='/'),merge_Stations = TRUE)

