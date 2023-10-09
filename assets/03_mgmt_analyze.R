#' Prepares data needed to create management level graphs
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




list.files(dataDirectory)

# retrieve air quality data-----
df_data <- load_data(dataDirectory,'aq_data.Rds')
station_clean <- load_data(dataDirectory,'station_clean.csv')

az <- airzones() %>% 
  st_make_valid()


# filter out non-AQMS sites (permittee)------

lst_remove <- station_clean %>%
  filter(aqms == 'N') %>%
  pull(site)

df_data <- df_data %>%
  clean_names() %>%
  filter(!station_name %in% lst_remove) %>%
  select(date_time,parameter,station_name,instrument,raw_value,flag_tfee) %>%
  rename(site = station_name) %>%
  filter(!is.na(raw_value))

# identify any data duplicates
df_data_dupl <- ungroup(df_data) %>%
  filter(!is.na(raw_value)) %>%
  group_by(site,parameter,date_time,instrument) %>%
  mutate(count = n()) %>%
  filter(count>1)

df_data_dupl %>%
  arrange(site,parameter,date_time) %>%
  View()

nrow(df_data_dupl)  #should be zero


df_data_tfee <- df_data %>%
  filter(!flag_tfee)




tfee_dates_pm25 <- df_data %>%
  clean_names() %>% 
  mutate(date = date(date_time)) %>%
  filter(flag_tfee,parameter == 'PM25') %>%
  select(site,instrument,date)
tfee_dates_o3 <- df_data %>%
  clean_names() %>% 
  mutate(date = date(date_time)) %>%
  filter(flag_tfee,parameter == 'O3') %>%
  select(site,instrument,date)
# calculate metrics for each parameter-----

# PM25------

# - 24-hr metric
pm25_24h_notfee <- rcaaqs::pm_24h_caaqs(filter(df_data,parameter == 'PM25'), 
                                     val = 'raw_value', 
                                     by = c('site','instrument'))

pm25_24h_tfee <- rcaaqs::pm_24h_caaqs(filter(df_data_tfee,parameter == 'PM25'), 
                                        val = 'raw_value', 
                                        by = c('site','instrument'))

pm25_24h_mgmt_notfee <- caaqs_management(pm25_24h_notfee, 
                                     exclude_df = tfee_dates_pm25, 
                                     exclude_df_dt = "date")
pm25_24h_mgmt_tfee <- caaqs_management(pm25_24h_tfee, 
                                          exclude_df = tfee_dates_pm25, 
                                          exclude_df_dt = "date")

# - annual metric
# - 24-hr metric
pm25_annual_notfee <- rcaaqs::pm_annual_caaqs(filter(df_data,parameter == 'PM25'), 
                                        val = 'raw_value', 
                                        by = c('site','instrument'))

pm25_annual_tfee <- rcaaqs::pm_annual_caaqs(filter(df_data_tfee,parameter == 'PM25'), 
                                      val = 'raw_value', 
                                      by = c('site','instrument'))
pm25_annual_mgmt_notfee <- caaqs_management(pm25_annual_notfee, 
                                     exclude_df = tfee_dates_pm25, 
                                     exclude_df_dt = "date")
pm25_annual_mgmt_tfee <- caaqs_management(pm25_annual_tfee, 
                                          exclude_df = tfee_dates_pm25, 
                                          exclude_df_dt = "date")

# fill up the value from TFEE-adjusted, to apply capture requirements
df_fill_24h <- pm25_24h_mgmt_tfee$caaqs %>%
  select(site,instrument,caaqs_year,metric, 
         metric_value_mgmt,mgmt_level)
df_fill_annual <- pm25_annual_mgmt_tfee$caaqs %>%
  select(site,instrument,caaqs_year,metric,
         metric_value_mgmt,mgmt_level)

# create the combined result for tfee and non-tfee adjusted values
pm25_24h_mgmt <- pm25_24h_mgmt_notfee
pm25_24h_mgmt$caaqs <- pm25_24h_mgmt$caaqs %>%
  select(- metric_value_mgmt,-mgmt_level) %>%
  left_join(df_fill_24h, by=c('site','instrument', 'caaqs_year', 'metric'))

pm25_annual_mgmt <- pm25_annual_mgmt_notfee
pm25_annual_mgmt$caaqs <- pm25_annual_mgmt$caaqs %>%
  select(- metric_value_mgmt,-mgmt_level) %>%
  left_join(df_fill_annual, by=c('site','instrument', 'caaqs_year', 'metric'))

# O3 ----

   # - calculate based on the 8-hour metric
o3_notfee <- rcaaqs::o3_caaqs(filter(df_data,parameter == 'O3'), 
                                        val = 'raw_value', 
                                        by = c('site'))

o3_tfee <-  rcaaqs::o3_caaqs(filter(df_data_tfee,parameter == 'O3'), 
                             val = 'raw_value', 
                             by = c('site'))

ozone_mgmt_notfee <- caaqs_management(o3_notfee, 
                                    exclude_df = tfee_dates_o3, 
                                    exclude_df_dt = "date")
ozone_mgmt_tfee <- caaqs_management(o3_tfee, 
                                      exclude_df = tfee_dates_o3, 
                                      exclude_df_dt = "date")

#combine into the management level

#fill with values from tfee-adjusted managment levels
df_fill_o3 <- ozone_mgmt_tfee$caaqs %>%
  select(site,caaqs_year,metric,metric_value_mgmt,mgmt_level)

#create the combined dataframe for ozone
ozone_mgmt <- ozone_mgmt_notfee 

ozone_mgmt$caaqs <- ozone_mgmt_notfee$caaqs %>%
  select(-metric_value_mgmt,-mgmt_level) %>%
  left_join(df_fill_o3, by=c('site', 'caaqs_year', 'metric'))

# NO2 -----

no2_hour <-  rcaaqs::no2_3yr_caaqs(filter(df_data,parameter == 'NO2'), 
                             val = 'raw_value', 
                             by = c('site'))
no2_annual <-  rcaaqs::no2_1yr_caaqs(filter(df_data,parameter == 'NO2'), 
                                   val = 'raw_value', 
                                   by = c('site'))

# get NO2 managmeent levels
no2_mgmt_hour <- rcaaqs::caaqs_management(no2_hour)
no2_mgmt_annual <- rcaaqs::caaqs_management(no2_annual)

# SO2 -----

so2_hour <-  rcaaqs::so2_3yr_caaqs(filter(df_data,parameter == 'SO2'), 
                                   val = 'raw_value', 
                                   by = c('site'))
so2_annual <-  rcaaqs::so2_1yr_caaqs(filter(df_data,parameter == 'SO2'), 
                                     val = 'raw_value', 
                                     by = c('site'))

# get SO2 managmeent levels
so2_mgmt_hour <- rcaaqs::caaqs_management(so2_hour)
so2_mgmt_annual <- rcaaqs::caaqs_management(so2_annual)

# Summary for air zone
#get air zone summaries
management_all <- (bind_rows(get_caaqs(pm25_annual_mgmt),
                         get_caaqs(pm25_24h_mgmt)) %>%
                 mutate(parameter = 'PM25')) %>%
  bind_rows(
    get_caaqs(ozone_mgmt) %>%
      mutate(parameter = 'O3')
  ) %>%
  bind_rows(
    bind_rows(get_caaqs(no2_mgmt_hour),
              get_caaqs(no2_mgmt_annual)) %>%
      mutate(parameter = 'NO2')
  ) %>%
  bind_rows(
    bind_rows(get_caaqs(so2_mgmt_hour),
              get_caaqs(so2_mgmt_annual)) %>%
      mutate(parameter = 'SO2')
  ) %>%
  left_join(station_clean, by = ("station_name" = "site" )) %>% 
  ungroup() %>%
  distinct() 

management_az_all <- management_all %>%
  group_by(airzone,caaqs_year,parameter) %>%
  slice_max(mgmt_level, with_ties = FALSE) 

management_az_metric <-  management_all %>%
  group_by(airzone,caaqs_year,parameter,metric) %>%
  slice_max(mgmt_level, with_ties = FALSE) 


# Overall Result-----

#create a list combining all these results
result <- list()
result[['pm25_24h_notfee']] <- pm25_24h_notfee
result[['pm25_24h_tfee']] <- pm25_24h_tfee
result[['pm25_annual_notfee']] <- pm25_annual_notfee
result[['pm25_annual_tfee']] <- pm25_annual_tfee
result[['pm25_24h_mgmt']] <- pm25_24h_mgmt
result[['pm25_annual_mgmt']] <- pm25_annual_mgmt
result[['o3_notfee']] <- o3_notfee
result[['o3_tfee']] <- o3_tfee
result[['ozone_mgmt']] <- ozone_mgmt
result[['no2_hour']] <- no2_hour
result[['no2_annual']] <- no2_annual
result[['no2_mgmt_hour']] <- no2_mgmt_hour
result[['no2_mgmt_annual']] <- no2_mgmt_annual
result[['so2_hour']] <- so2_hour
result[['so2_annual']] <- so2_annual
result[['so2_mgmt_hour']] <- so2_mgmt_hour
result[['so2_mgmt_annual']] <- so2_mgmt_annual
result[['management_all']] <- management_all
result[['management_az_all']] <- management_az_all
result[['management_az_metric']] <- management_az_metric


saveRDS(result,'./data/out/management_results.Rds')
