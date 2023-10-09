#' Creates graphics for the management level graphs

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
library(tidyr)

# access data created from 03
result <- readRDS('./data/out/management_results.Rds')
station_clean <- load_data(dataDirectory,'station_clean.csv')

lst_data <- names(result)

# Spatial data summaries --------------------------------------------
az <- airzones() %>%
  st_make_valid() %>%
  st_transform(st_crs(bc_bound())) %>%
  st_intersection(st_geometry(bc_bound())) %>% 
  group_by(airzone = Airzone) %>% 
  summarize() %>%
  clean_names()





az_ambient_sf <- az_ambient %>% 
  complete(airzone = az$airzone, metric)  # Ensure ALL airzones
left_join(az, ., by = "airzone") 
filter(!is.na(caaqs_ambient )) %>%
  mutate(caaqs_ambient = replace_na(caaqs_ambient, levels(caaqs_ambient)[1]))%>%
  nest(data = c(-metric)) %>%
  mutate(data = map(data, ~airzone_metric(., keep = "site", station_id = "site"))) %>%
  unnest(data) %>%
  select(airzone, metric, everything())


