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

# The functions here are meant to create functions and data to prepare shiny data



library(envair)
library(rcaaqs)
# CAAQS-related Calculatons-----

#' Calculate annual metrics
#'
#' @param years is vector listing the years for CAAQS calculation
#' @param savedirectory is the directory where the saved files will be located
#'
#' @return annual_results.csv file
create_metrics_annual <- function(years, savedirectory = NULL) {
  if (0) {
    years <- 2000:2010
    savedirectory <- './test_data'
  }
  
  if (is.null(savedirectory)) {
    savedirectory <- './'
  }
  
  
  if (0)
  {
    #debug, to retrieve from previous wide version
    #chose from these four
    savefile <- paste(savedirectory,'pm25_annual.csv',sep='/')
    savefile <- paste(savedirectory,'o3_annual.csv',sep='/')
    savefile <- paste(savedirectory,'no2_annual.csv',sep='/')
    savefile <- paste(savedirectory,'so2_annual.csv',sep='/')
    
    #run both
    saveDirectory <- '../data/out'
    df <- readr::read_csv(savefile)
    savefile <- paste(saveDirectory,'annual_results.csv',sep='/')
    list.files(saveDirectory)
  }
  
  # create annual metrics ----
  savefile_final <- paste(savedirectory,'annual_results.csv',sep='/')
  savefile <- tempfile()
  #define non-value columns
  cols_static <- c('parameter','year','station_name','site','instrument','station','station_name_full')
  
  
  #pm2.5
  # savefile <- paste(savedirectory,'pm25_annual.csv',sep='/')
  for (year in years) {
    
    try({
      df <- importBC_data_avg(parameter = 'pm25',years = year,
                              averaging_type = c('annual 98p 24h','annual mean 24h'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)
      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]
      
      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,tfee,year,metric,value)
      
      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })
    
  }
  
  #ozone
  # savefile <- paste(savedirectory,'o3_annual.csv',sep='/')
  for (year in years) {
    try({
      df <- importBC_data_avg(parameter = 'o3',years = year,
                              averaging_type = c('annual 4th d8hm'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)
      
      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]
      
      #make the result long
      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,tfee,year,metric,value)
      
      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })
    
  }
  
  #no2
  # savefile <- paste(savedirectory,'no2_annual.csv',sep='/')
  for (year in years) {
    
    try({
      df <- importBC_data_avg(parameter = 'no2',years = year,
                              averaging_type = c('annual 98p d1hm', 'annual mean 1hr'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)
      
      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]
      
      #make the result long
      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,tfee,year,metric,value)
      
      
      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })
    
  }
  
  #so2
  # savefile <- paste(savedirectory,'so2_annual.csv',sep='/')
  for (year in years) {
    
    try({
      df <- importBC_data_avg(parameter = 'so2',years = year,
                              averaging_type = c('annual 99p d1hm', 'annual mean 1hr'),
                              flag_TFEE = TRUE,
                              merge_Stations = TRUE)
      
      cols_static_ <- colnames(df)[tolower(colnames(df)) %in% tolower(cols_static)]
      
      #make the result long
      df <- df %>%
        tidyr::pivot_longer(cols = -cols_static_) %>%
        dplyr::rename(metric = name,
                      site = STATION_NAME,
                      parameter = PARAMETER,
                      year = YEAR,
                      instrument = INSTRUMENT) %>%
        mutate(tfee = grepl('_tfee',metric,ignore.case = TRUE)) %>%
        mutate(metric = gsub('_tfee','',metric,ignore.case = TRUE)) %>%
        select(parameter,site,instrument,tfee,year,metric,value)
      
      readr::write_csv(df,
                       file = savefile,
                       append = file.exists(savefile))
    })
    
  }
  
  #transfer from temp to final savefile
  file.copy(from = savefile, to=savefile_final,overwrite = TRUE)
  
  
  #create captures ------
  savefile_final <- paste(savedirectory,'captures.csv',sep='/')
  savefinal <- tempfile()
  for (param in c('pm25','o3','no2','so2')) {
    for (year in years) {
      try({
        df <- get_captures(param = param, years = year, merge_Stations = TRUE)
        readr::write_csv(df,
                         file = savefile,
                         append = file.exists(savefile))
      })
    }
  }
  file.copy(from = savefile, to=savefile_final,overwrite = TRUE)
  
  
  
}

#' Calculate annual CAAQS metrics
#'
#' @description This script is dependent on the rcaaqs package
#' it already properly applies GDAD in calculation of yearly metrics, and the CAAQS
#' Better than create_metrics_annual() but may take longer to complete
#'
#' @param years is vector listing the years for CAAQS calculation
#' @param savedirectory is the location where the result files are saved
#'
#' @return caaqs_results.csv file
create_caaqs_annual <- function(years, savedirectory = NULL) {
  if (0) {
    years <- 2019:2021
    savedirectory <- './test_data'
    
    for (files in list.files('././r/',full.names = TRUE)) {
      try(source(files))
    }
  }
  
  if (is.null(savedirectory)) {
    savedirectory <- '././test_data'
    list.files(savedirectory)
  }
  #where files will be saved
  savefile_final = paste(savedirectory,'caaqs_results.csv',sep='/')
  #temporary save filelocation
  savefile <- tempfile()
  #defines the resulting column names in this order
  cols_final <- c('parameter','site','instrument','year',
                  'tfee','metric_value','metric','flag_two_of_three_years')
  
  for (param in c('pm25','o3','no2','so2')) {
    df <- NULL
    
    #Retrieve data, different retrieval for ozone
    if(param != 'o3') {
      try(
        df <- importBC_data(param,years = (min(years)-2):max(years),
                            flag_TFEE = TRUE,merge_Stations = TRUE)
      )
    } else {
      try(
        df <- importBC_data(param,years = (min(years)-3):max(years),
                            flag_TFEE = TRUE,merge_Stations = TRUE)
      )
    }
    
    #if there was no data retrieved
    if (is.null(df)) {
      next
    }
    
    if (nrow(df) == 0) {
      next
    }
    
    #remove duplicate entries
    #note for pm, instrument is included in grouping
    if (param == 'pm25') {
      df <- df %>%
        ungroup() %>%
        dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
        dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
        filter(!is.na(value)) %>%
        group_by(date_time,site,instrument) %>%
        dplyr::mutate(index = 1:n()) %>%
        filter(index == 1) %>% select(-index) %>%
        ungroup()
    } else {
      df <- df %>%
        ungroup() %>%
        dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
        dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
        filter(!is.na(value)) %>%
        group_by(date_time,site) %>%
        dplyr::mutate(index = 1:n()) %>%
        filter(index == 1) %>% select(-index) %>%
        ungroup() 
    }
    
    
    if (param == 'pm25') {
      #without TFEE
      pm25_annual <- rcaaqs::pm_annual_caaqs(df,by=c('site','instrument'))
      pm25_24h <- rcaaqs::pm_24h_caaqs(df,by=c('site','instrument'))
      
      pm25_caaqs <- pm25_annual$caaqs %>%
        bind_rows(pm25_24h$caaqs) %>%
        mutate(parameter = 'PM25',tfee = FALSE) %>%
        dplyr::rename(year = caaqs_year) %>%
        select(cols_final)
      
      pm25_yoy <- pm25_annual$yearly_avg %>%
        bind_rows(pm25_24h$yearly_98)%>%
        mutate(parameter = 'PM25',tfee = FALSE,flag_two_of_three_years = NA) %>%
        select(parameter,site,instrument,year,tfee,ann_avg,ann_98_percentile,flag_two_of_three_years) %>%
        tidyr::pivot_longer(cols = c(ann_avg,ann_98_percentile)) %>%
        filter(!is.na(value)) %>%
        dplyr::rename(metric = name,
                      metric_value = value)%>%
        select(cols_final)
      
      df_result <- pm25_caaqs %>%
        bind_rows(pm25_yoy)
      
      #with TFEE
      pm25_annual <- rcaaqs::pm_annual_caaqs(df %>% filter(!flag_tfee),by=c('site','instrument'))
      pm25_24h <- rcaaqs::pm_24h_caaqs(df %>% filter(!flag_tfee),by=c('site','instrument'))
      
      pm25_caaqs <- pm25_annual$caaqs %>%
        bind_rows(pm25_24h$caaqs) %>%
        mutate(parameter = 'PM25',tfee = TRUE) %>%
        dplyr::rename(year = caaqs_year) %>%
        select(cols_final)
      
      pm25_yoy <- pm25_annual$yearly_avg %>%
        bind_rows(pm25_24h$yearly_98)%>%
        mutate(parameter = 'PM25',tfee = TRUE,flag_two_of_three_years = NA) %>%
        select(parameter,site,instrument,year,tfee,ann_avg,ann_98_percentile) %>%
        tidyr::pivot_longer(cols = c(ann_avg,ann_98_percentile)) %>%
        filter(!is.na(value)) %>%
        dplyr::rename(metric = name,
                      metric_value = value)
      
      df_result <- df_result %>%
        bind_rows(pm25_caaqs) %>%
        bind_rows(pm25_yoy) %>%
        mutate(metric = recode(metric,
                               'pm2.5_annual'='pm25_annual',
                               'pm2.5_24h'='pm25_24h',
                               'ann_98_percentile'='pm25_24hr(1yr)',
                               'ann_avg'='pm25_ann(1yr)'
        ))
      
      rm('pm25_annual')
      rm('pm25_24h')
    }
    
    if (param == 'o3') {
      
      #without TFEE
      o3_8h <- rcaaqs::o3_caaqs(df,by=c('site'))
      
      #with TFEE
      o3_8h_tfee <- rcaaqs::o3_caaqs(df %>% filter(!flag_tfee) ,by=c('site'))
      
      o3_caaqs <-  o3_8h$caaqs%>% mutate(tfee = FALSE) %>%
        bind_rows(o3_8h_tfee$caaqs%>% mutate(tfee = TRUE)) %>%
        mutate(parameter = 'O3', instrument = NA)%>%
        dplyr::rename(year = caaqs_year) %>%
        select(cols_final)
      
      o3_yoy <- o3_8h$ann_4th_highest %>%
        mutate(tfee = FALSE) %>%
        bind_rows(
          o3_8h_tfee$ann_4th_highest %>% mutate(tfee = TRUE)
        ) %>%
        select(site,year,tfee,ann_4th_highest) %>%
        dplyr::rename(metric_value = ann_4th_highest) %>%
        mutate(parameter = 'O3', instrument = NA,
               metric = 'o3_8h(1yr)',flag_two_of_three_years = NA) %>%
        select(cols_final)
      
      df_result <- o3_caaqs %>%
        bind_rows(o3_yoy) %>%
        mutate(metric = recode(metric,'o3'='o3_8h'))
      
      rm('o3_8h')
      rm('o3_8h_tfee')
    }
    
    if (param == 'no2') {
      
      #without TFEE
      no2_1hr <- rcaaqs::no2_3yr_caaqs(df,by=c('site'))
      no2_ann <- rcaaqs::no2_1yr_caaqs(df,by=c('site'))
      
      no2_caaqs <-  no2_1hr$caaqs%>%
        mutate(tfee = FALSE) %>%
        bind_rows(
          no2_ann$caaqs%>%
            mutate(tfee = FALSE)
        ) %>%
        dplyr::rename(year = caaqs_year) %>%
        mutate(parameter = 'NO2', instrument = NA) %>%
        select(cols_final)
      
      no2_yoy <- no2_1hr$yearly_98 %>%
        bind_rows(
          no2_ann$yearly_hr
        ) %>%
        select(site,year,ann_98_percentile,avg_yearly) %>%
        mutate(parameter = 'NO2',tfee = FALSE,instrument = NA,flag_two_of_three_years=NA) %>%
        tidyr::pivot_longer(cols = c(ann_98_percentile,avg_yearly)) %>%
        dplyr::rename(metric_value = value, metric = name) %>%
        filter(!is.na(metric_value)) %>%
        select(cols_final)
      
      df_result <- no2_caaqs %>%
        bind_rows(no2_yoy)%>%
        mutate(metric = recode(metric,
                               'no2_3yr'='no2_1hr',
                               'no2_1yr'='no2_ann',
                               'ann_98_percentile'='no2_1hr(1yr)',
                               'avg_yearly'='no2_ann(1yr)'
        ))
      
      rm('no2_1hr')
      rm('no2_ann')
      
    }
    
    if (param == 'so2') {
      
      
      #without TFEE
      so2_1hr <- rcaaqs::so2_3yr_caaqs(df,by=c('site'))
      so2_ann <- rcaaqs::so2_1yr_caaqs(df,by=c('site'))
      
      so2_caaqs <-  so2_1hr$caaqs%>% mutate(tfee = FALSE) %>%
        bind_rows(so2_ann$caaqs%>% mutate(tfee = FALSE)) %>%
        dplyr::rename(year = caaqs_year) %>%
        mutate(parameter = 'SO2', instrument = NA) %>%
        select(cols_final)
      
      so2_yoy <- so2_1hr$yearly_99 %>%
        bind_rows(so2_ann$yearly_hr) %>%
        select(site,year,ann_99_percentile,avg_yearly) %>%
        tidyr::pivot_longer(cols = c(ann_99_percentile,avg_yearly)) %>%
        dplyr::rename(metric = name,
                      metric_value = value) %>%
        filter(!is.na(metric_value)) %>%
        mutate(tfee = FALSE,parameter = 'SO2',
               instrument = NA,flag_two_of_three_years = NA) %>%
        select(cols_final)
      
      df_result <- so2_caaqs %>%
        bind_rows(so2_yoy) %>%
        select(cols_final)%>%
        mutate(metric = recode(metric,
                               'so2_3yr'='so2_1hr',
                               'so2_1yr'='so2_ann',
                               'ann_99_percentile'='so2_1hr(1yr)',
                               'avg_yearly'='so2_ann(1yr)'
        ))
      
      rm('so2_1hr')
      rm('so2_ann')
      
    }
    
    
    
    #save the result into a file
    #append if the file exist
    #standardize the names of columns
    df_result <- df_result %>%
      filter(year %in% years) %>%
      arrange(parameter,site,instrument,year,tfee)
    
    #Remove 2019-2021 from North Vancouver
    #Also remove sites that are not AQMS
    df_stations <- envair::listBC_stations(use_CAAQS = TRUE)
    
    list_stations_remove <- df_stations %>%
      mutate(AQMS = ifelse(is.na(AQMS),'N/A',AQMS)) %>%
      filter(AQMS == 'N') %>%
      pull(site)
    
    
    df_result <- df_result %>%
      filter(!(site %in% c("North Vancouver Second Narrows") & year %in% c(2019:2021))) %>%
      filter(!(site %in% list_stations_remove))
    
    readr::write_csv(df_result,file = savefile,append = file.exists(savefile))
  }
  
  #transfer to final savefile
 
  file.copy(from = savefile, to=savefile_final,overwrite = TRUE)
  df_result <- readr::read_csv(savefile)
  return(df_result)
}


#' INCOMPLETE:
#' Create files that will make the CAAQS bar graph
#'
#' @param filedirectory is the location of the data files
#'
#' @return creates a file called air_data_summary.csv in the file directory
create_CAAQS_graph_files <- function(filedirectory = NULL) {
  
  if (0) {
    filedirectory <- NULL
  }
  
  if (is.null(filedirectory)) {
    filedirectory <- '././test_data'
    list.files(filedirectory)
  }
  
  file_annual <- paste(filedirectory,'annual_results.csv',sep='/')
  file_captures <- paste(filedirectory,'annual_results.csv',sep='/')
  file_ <- paste(filedirectory,'annual_results.csv',sep='/')
}

# Air zone-related calcuations----

#' Calculate the management levels
#'   NOTE: needs future management, change from datafile to an actual dataframe entry
#'
#' @param datafile is the location of the file containing summarized CAAQS data.
#'
#' This dataset was created with the create_metrics_annual function
#'
get_management <- function(datafile = NULL) {
  
  if (0) {
    datafile <- './data/out/caaqs_results.csv'
  }
  #retrieve data
  
  if (is.null(datafile)) {
    datafile <- '../test_data/caaqs_results.csv'
    # list.files(datafile)
  }
  
  #' Assess the levels based on the df_levels table
  #' 
  #' @param df is the dataframe containing the data
  #' @param df_levels is dataframe containing level definition
  assess_levels <- function(df,df_levels) {
    
    if (0) {
      df <- df_2015
      df_levels <- df_levels_old
    }
    df <- df %>% 
      ungroup()%>%
      dplyr::mutate(idx0 = 1:n())
    
    df_levels <- df_levels%>%
      dplyr::mutate(idx1 = 1:n()) %>%
      ungroup()
      
    df_levels_ <- df_levels %>%
    select(metric,lower_breaks,upper_breaks,idx1) %>%
      
      dplyr::mutate(lower_breaks = ifelse(is.na(lower_breaks),-9999,lower_breaks)) %>%
      dplyr::mutate(upper_breaks = ifelse(is.na(upper_breaks),0,upper_breaks)) %>%
      dplyr::mutate(upper_breaks = ifelse(is.infinite(upper_breaks),99999999,upper_breaks))
    
    #conditions for assigning management levels
    
    df_ <-  df %>%
      left_join(df_levels_) %>%
      mutate(metric_value = ifelse(is.na(metric_value),-9999,metric_value)) %>%
      filter(metric_value > lower_breaks & metric_value <= upper_breaks) %>%
      # View()
      select(-lower_breaks,-upper_breaks) %>%
      mutate(metric_value = ifelse(metric_value == -9999,NA, metric_value)) %>%
      left_join(df_levels) %>%
      select(-idx0,-idx1)
    
    #add column called colour_order to put sorting or numerical order to the colours
    df_colour <- tribble(
      ~colour_text, ~colour_order,
      'grey',0,
      'green',1,
      'yellow',2,
      'orange',3,
      'red',4
    )
    df_ <- left_join(df_,df_colour)
    return(df_)
  }
  
  #specify only the ones that changed from Current Management Levels
  df_levels_old <- dplyr::tribble(
    ~CAAQS_name,~start_year,~end_year,~metric,~lower_breaks,~upper_breaks,~colour_text,
    '2015 CAAQS',2013,2019,'o3_8h',63,Inf,'red',
    '2015 CAAQS',2013,2019,'pm25_annual',10,Inf,'red',
    '2015 CAAQS',2013,2019,'pm25_24h',27,Inf,'red',
    
  )
  
  
  
  #retrieve most recent CAAQS
  #these are in the rcaaqs package

  df_levels_current <- rcaaqs::management_levels %>%
    dplyr::rename(metric = parameter) %>%
    mutate(metric = recode(metric,
                           'o3' = 'o3_8h',
                           'pm2.5_annual' = 'pm25_annual',
                           'pm2.5_24h' = 'pm25_24h',
                           'no2_1yr' = 'no2_ann',
                           'no2_3yr' = 'no2_1hr',
                           'so2_1yr' = 'so2_ann',
                           'so2_3yr' = 'so2_1hr')) %>%
    mutate(index = 1:n())
  
  df_levels_current$CAAQS_name <- '2020 CAAQS'
  
  #fill up missing information from the old CAAQS
  df_levels_old <- df_levels_old %>%
    left_join(df_levels_current %>%
    select(-CAAQS_name,-lower_breaks,-upper_breaks),
    by = c('metric','colour_text')
    )
  #add the other colors
  df_levels_old <- df_levels_old %>%
    dplyr::bind_rows(df_levels_current %>%
                       filter(!index %in% df_levels_old$index))
  
  print(paste('Reading data from:',datafile))
  df <- readr::read_csv(datafile)
    
  
  

  #Calculate 2020 CAAQS and onwards
  df_2015 <- df %>%
    filter(metric %in% df_levels_current$metric,
           year <2020)
  
  df_2020 <- df %>%
    filter(!(metric %in% df_levels_current$metric &
           year <2020))
  
  df_result_old <- assess_levels(df=df_2015,df_levels = df_levels_old)
  df_result_new <- assess_levels(df_2020,df_levels = df_levels_current) 
  #consideration for years
  #note that CAAQS was different in other years
  
  df_ <- bind_rows(df_result_old,df_result_new)
  return(df_)
  
  
  
  
}

#' Retrieves the management level summary of station and airzones
#'
#' @param outputtype is either 'complete','station', 'airzone'
#' 'complete' means that output is detailed for each metric, in each station
#' 'station' means that output is a summary of the management for the station. only metric with highest management level is displayed
#' 'airzone' means that output is a summary of the management for the airzones
#' @param df_preload is dataframe of preloaded data, generated in initial load only
#' @param datafile is the source data file
get_management_summary <- function(outputtype = 'complete',df_preload = NULL,
                                   datafile = NULL) {
  
  if (0) {
    outputtype = 'complete'
    df_preload = NULL
    datafile = paste(saveDirectory,'caaqs_results.csv',sep='/')
  }
  #define the parameter for each metric
  #arrange in terms of an order
  df_metric <- tribble(
    ~metric,~parameter,
    "pm25_annual",'pm25',
    "pm25_24h",'pm25',
    "o3_8h",'o3',
    "no2_ann",'no2',
    "no2_1hr",'no2',
    "so2_ann",'so2',
    "so2_1hr",'so2',
  )
  
  df <- df_preload
  
  if (is.null(df_preload)) {
    lst_stations <- envair::listBC_stations(use_CAAQS = TRUE,merge_Stations = TRUE) %>%
      dplyr::rename(latitude = LAT,
                    longitude = LONG,
                    airzone = AIRZONE,
                    label = Label)
    
    lst_stations <- lst_stations %>%
      select(-STATION_NAME_FULL) %>%
      group_by(site) %>%
      slice(1) %>%
      ungroup()
    df <- get_management(datafile = datafile)
    
    df <- df %>%
      select(site,instrument,year,metric,metric_value,colour,colour_text,colour_order,tfee) %>%
      left_join(lst_stations) %>%
      left_join(df_metric)
  }
  if (0) {
    readr::write_csv(df,'././test_data/management.csv')
  }
  #add order to the metric
  df$metric <- factor(df$metric,levels = df_metric$metric)
  #calculate and return result based on the type specified
  outputtype <- tolower(outputtype)
  if (outputtype == 'complete') {
    return(df)
  }
  
  if (outputtype == 'station') {
    
    df <- df %>%
      group_by(parameter,site,year,airzone,tfee) %>%
      dplyr::mutate(max_colour_order = max(colour_order)) %>%
      ungroup() %>%
      filter(colour_order == max_colour_order) %>%
      arrange(metric) %>%   #this gives priority to annual over 24h/1h metrics
      group_by(parameter,site,year,airzone,tfee) %>%
      dplyr::mutate(index =1:n()) %>%
      filter(index==1) %>% ungroup() %>% select(-index) %>%
      arrange(parameter,site,tfee,year) %>%
      select(-max_colour_order)
    
    return(df)
  }
  
  if (outputtype == 'airzone') {
    
    df <- df %>%
      arrange(airzone,metric_value) %>%
      group_by(parameter,metric,year,airzone,tfee) %>%
      dplyr::mutate(max_metric_value = max(metric_value,na.rm = TRUE)) %>%
      ungroup() %>%
      filter(metric_value == max_metric_value) %>%
      arrange(desc(colour_order), metric) %>%   #this gives prioroty to pm2.5annual or 24h, and no2_3yr over 1 yr
      group_by(parameter,year,airzone,tfee) %>%
      dplyr::mutate(max_colour_order = max(colour_order),index = 1:n()) %>%
      filter(colour_order == max_colour_order,index == 1) %>% ungroup() %>% select(-index) %>%
      COLUMN_REORDER(c('parameter','airzone','tfee','year')) %>%
      select(-max_colour_order,-max_metric_value) %>%
      arrange(parameter,airzone,tfee,year)
    
    return(df)
  }
  
}

#' Retrieve air quality data
#' 
#' @param parameters is a vector specifying the parameters
#' @param years is a vector listing the years of data
#' @param outputfile specifies the filename of the output
#' @param merge_Stations specifies if stations and instruments are merged as per air zone reporting process
save_data <- function(parameters, years, outputfile,merge_Stations = FALSE) {
  
  if (0) {
    parameters <- c('pm25','no2')
    years <- 2018:2019
    outputfile <- 'test.Rds'
  }
  
  #create temporary file
  savefiletmp <- tempfile()
  require(envair)
  
  for (year_ in years) {
    df <- NULL
    for (parameter_ in parameters) {
      
      print(paste('Retrieving data. Year:',year_,'Parameter:',parameter_))
      
      try({
        df_ <- importBC_data(parameter_or_station = parameter_,years = year_,flag_TFEE = TRUE,
                             merge_Stations = merge_Stations)
        df <- df %>%
          dplyr::bind_rows(df_)
      })
    }
    if (!is.null(df)) {
      readr::write_csv(df,file = savefiletmp,append = file.exists(savefiletmp))
      print(paste('Temporary File:',savefiletmp))
    }
  }
  
  #save file into binary
  df_ <- readr::read_csv(savefiletmp)
  
  saveRDS(df_,file = outputfile)
  print(paste('File saved in:',outputfile))
  return(file.info(outputfile))
}

#' Calculate the rolling n-hour value
#' 
#' @param df is the data frame containing the values.
#' It must have DATE_PST and RAW_VALUE column
#' @param k is the interval, use k=8 for 8-hour
rolling_nhr <- function(df, k=8) {
  
  min_date <- min(df$DATE_PST)
  max_date <- max(df$DATE_PST)
  df_dates <- tibble(DATE_PST = seq.POSIXt(from = min_date, to = max_date, by ='hour'))
  
  cols_nonvalue <- c('STATION_NAME','INSTRUMENT','PARAMETER')
  cols_value <- c('RAW_VALUE','value')
  
  cols_df <- colnames(df)
  cols_nonvalue <- cols_df[cols_df %in% cols_nonvalue]
  cols_value <- cols_df[cols_df %in% cols_value]
  
  
  df <- df %>%
    select(all_of(c('DATE_PST',cols_nonvalue,cols_value)))
  
  df_filler <- df %>%
    select(all_of(cols_nonvalue)) %>%
    distinct() %>%
    merge(df_dates) %>%
    arrange_at(cols_nonvalue)
  
  df_ <- df_filler %>%
    left_join(df) %>%
    dplyr::rename(value = cols_value)
  
  df <- df_ %>%
    mutate(valid = ifelse(is.na(value),0,1)) %>%
    mutate(value = ifelse(valid == 0,0,value)) %>%
    group_by_at(cols_nonvalue) %>%
    dplyr::mutate(value_8h = zoo::rollsum(value, k=8,align = 'right',fill=NA), 
                  valid_8h = zoo::rollsum(valid, k=8,align = 'right',fill=NA)) %>%
    ungroup() %>%
    filter(valid_8h>=0.75*8) %>%
    mutate(value = value_8h/valid_8h) %>%
    select(-valid_8h,-value_8h,-valid) %>%
    select(!!(c('DATE_PST',cols_nonvalue,'value')))
  
  return(df)
}


#' Calculate the number of exceedance to the standards
#' 
#' @param df is the dataframe containing data
#' @param parameter is the parameter
#' @param outputype is either 'filter','freq_annual','freq_month','freq_hour','freq_seasonal'
#' @param avg_type is a vector listing the averaging type
#' @param exceedance_level is a vector containing values that will be evaluated based on the avg_type
#' 
calc_exceedances <- function(df, parameter, outputtype = 'filter', avg_type = NULL, exceedance_level = NULL) {
  if (0) {
    df <- raw_data
    parameter <- 'PM25'
    thresold_level <- c(25,27)
    outputtype = 'filter'
    avg_type = NULL
    exceedance_level = NULL
  }
  
  #these are columns that are included in the evaluation
  cols_include <- c('DATE_PST','PARAMETER','STATION_NAME','INSTRUMENT','RAW_VALUE')
  
  df_thresholds <- tribble(
    ~PARAMETER, ~avg_type, ~exceedance,
    'PM25','24h','27',
    'PM25','24h','25',
    'NO2','d1hm','60',
    'SO2','d1hm','70',
    'O3','d8hm','62'
    
  )
  
  if (is.null(avg_type)) {
    avg_type <- unique(df_thresholds$avg_type[df_thresholds$PARAMETER == toupper(parameter)])
  }
  
  if (is.null(exceedance_level)) {
    exceedance_level <- unique(df_thresholds$exceedance[df_thresholds$PARAMETER == toupper(parameter)])
    exceedance_level <- sort(exceedance_level)
  }
  
  df <- df %>%
    select(all_of(cols_include)) %>%
    dplyr::rename(value = RAW_VALUE) %>%
    filter(tolower(PARAMETER) == tolower(parameter)) %>%
    filter(!is.na(value)) 
  
  #recalculate based on averaging_type
  #round off to whole numbers
  df$value <- envair::round2(df$value,n=0)
  if (avg_type == '24h') {
    df <- df %>%
      filter(!is.na(value)) %>%
      dplyr::mutate(DATE = DATE_PST - lubridate::hours(1)) %>%
      mutate(DATE = lubridate::date(DATE)) %>%
      group_by(STATION_NAME,INSTRUMENT,PARAMETER,DATE) %>%
      dplyr::summarise(value_24h = mean(value,na.rm = TRUE),valid_hrs = n()) %>%
      filter(valid_hrs >= 0.75*24) %>%
      dplyr::rename(value = value_24h) %>%
      select(-valid_hrs)
  }
  
  if (avg_type == 'd1hm') {
    df <- df %>%
      filter(!is.na(value)) %>%
      dplyr::mutate(DATE = DATE_PST - lubridate::hours(1)) %>%
      mutate(DATE = lubridate::date(DATE)) %>%
      group_by(STATION_NAME,INSTRUMENT,PARAMETER,DATE) %>%
      dplyr::summarise(value_max = max(value,na.rm = TRUE),valid_hrs = n()) %>%
      filter(valid_hrs >= 0.75*24) %>%
      dplyr::rename(value = value_max) %>%
      select(-valid_hrs)
  }
  
  if (avg_type == 'd8hm') {
    df_ <- rolling_nhr(df, k=8)
    
    df <- df_ %>%
      filter(!is.na(value)) %>%
      dplyr::mutate(DATE = DATE_PST - lubridate::hours(1)) %>%
      mutate(DATE = lubridate::date(DATE)) %>%
      group_by(STATION_NAME,INSTRUMENT,PARAMETER,DATE) %>%
      dplyr::summarise(value_max = max(value,na.rm = TRUE),valid_hrs = n()) %>%
      filter(valid_hrs >= 0.75*24) %>%
      dplyr::rename(value = value_max) %>%
      select(-valid_hrs)
    
  }
  
  
  
  exceedance_level <- as.numeric(exceedance_level)
  df <- ungroup(df)
  
  
  for (exceed_ in exceedance_level){
    
    df$exceed = df$value>exceed_
    
    #rename column
    colnames(df)[colnames(df) == 'exceed'] <- paste('exceed',exceed_, sep='_')
  }
  
  df_result <- df
  
  
  
  # List of outputtype options
  # 'filter',freq_annual','freq_month','freq_hour','freq_seasonal'
  if (tolower(outputtype) == 'filter') {
    return(df_result)
  }
  
  #create a correction if there is no date column
  #create a date column
  if (!'DATE' %in% toupper(colnames(df_result))) {
    df_result <- df_result %>%
      dplyr::mutate(DATE = DATE_PST - lubridate::hours(1)) %>%
      mutate(DATE = lubridate::date(DATE))
  }
  
  cols <- colnames(df_result)
  cols_exceed <- cols[grepl('exceed',cols)]
  df_result <- df_result %>%
    select(-value)
  
  
  
  if (tolower(outputtype) == 'freq_annual') {
    
    df_result <- df_result %>%
      mutate(YEAR = lubridate::year(DATE)) %>%
      tidyr::pivot_longer(cols=cols_exceed) %>%
      mutate(value = ifelse(value == TRUE,1,0)) %>%
      group_by(STATION_NAME,PARAMETER,INSTRUMENT,YEAR,name) %>%
      dplyr::summarise(count = sum(value,na.rm = TRUE)) %>% 
      tidyr::pivot_wider(names_from = name, values_from = count)
    
    
    return(df_result)
  }
  
  if (tolower(outputtype) == 'freq_month') {
    df_result <-  df_result %>%
      mutate(YEAR = lubridate::year(DATE),
             MONTH = lubridate::month(DATE)) %>%
      tidyr::pivot_longer(cols=cols_exceed) %>%
      mutate(value = ifelse(value,1,0)) %>%
      group_by(STATION_NAME,PARAMETER,INSTRUMENT,YEAR,MONTH,name) %>%
      dplyr::summarise(count = sum(value,na.rm = TRUE)) %>% 
      tidyr::pivot_wider(names_from = name, values_from = count)
    
    
    
    return(df_result)
  }
  
  if (tolower(outputtype) == 'freq_seasonal') {
    
    
    #declare seasons
    df_seasons <- tribble(
      ~MONTH,~SEASON,
      1,'Winter',
      2,'Winter',
      3,'Spring',
      4,'Spring',
      5,'Spring',
      6,'Summer',
      7,'Summer',
      8,'Summer',
      9,'Fall',
      10,'Fall',
      11,'Fall',
      12,'Winter',
    )
    
    df_result <- df_result %>%
      mutate(YEAR = lubridate::year(DATE),
             MONTH = lubridate::month(DATE)) %>%
      left_join(df_seasons) %>%
      tidyr::pivot_longer(cols=cols_exceed) %>%
      mutate(value = ifelse(value,1,0)) %>%
      group_by(STATION_NAME,PARAMETER,INSTRUMENT,YEAR,SEASON,name) %>%
      dplyr::summarise(count = sum(value,na.rm = TRUE)) %>% 
      tidyr::pivot_wider(names_from = name, values_from = count)
    
    return(df_result)
  }
  
  print('outputtype error. Please choose from specified list')
  return(NULL)
}

#' Determine the air zone based on lat longs
#' 
#' @param lat is the latitude, vector OK
#' @param long is the longitude, vector OK
get_airzone <- function(lat,long) {
  
  if (0) {
    latlong <- c(57.68,-120.614)
  }
  
  
  # 
  # 
  # az_mgmt <- readRDS(url(az_mgmt_gitURL))
  # a <- tempfile()
  # if (!file.exists(a)) {
  # download.file('https://github.com/bcgov/air-zone-reports/blob/master/data/out/az_mgmt.Rds?raw=true',a)
  # }
  # az_mgmt <- readRDS(a)
  # print('readRDS')
  az_mgmt <- az_mgmt0
  
  
  #----------------
  pnts <- data.frame(
    "x" = long,
    "y" = lat)
  
  # create a points collection
  pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(pnts), 
                                       function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 
  
  pnts_trans <- st_transform(pnts_sf, 2163) # apply transformation to pnts sf
  tt1_trans <- st_transform(az_mgmt, 2163)      # apply transformation to polygons sf
  
  # intersect and extract state name
  pnts$airzone <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                        function(col) { 
                          tt1_trans[which(col), ]$airzone
                        })
  
  return(pnts$airzone)
  
}

#' Determine the airzone from dataframe
#' 
#' @param lat is the column name for latitude
#' @param long is the column name for longitude
get_airzone_df <- function(df) {
  if (0) {
    url_aqhi <- 'https://envistaweb.env.gov.bc.ca/aqo/setup/BC_AQHI_SITES_AQHIPlusSO2.csv'
    df <- readr::read_csv(url_aqhi) %>%
      select(AQHI_AREA,LATITUDE,LONGITUDE) %>%
      group_by(AQHI_AREA) %>%
      slice(1) 
    lat <- 'LATITUDE'
    long <- 'LONGITUDE'
    
  }
  lat <- NULL
  long <- NULL
  
  # print(str(df))
  #preliminary list of lat,longs
  lst_lat <- c("latitude",'LATITUDE','lat','LAT','Latitude')
  lst_long <- c("longitude",'LONGITUDE','lon','LONG','Longitude')
  
  df <- df %>%
    ungroup() %>%
    mutate(index_airzone = 1:n()) 
  
  df_initial <- df  #to bse saved for later
  
  col_df <- colnames(df)
  
  #change the name t'o "lat", "long"
  if (!is.null(lat)) {
    colnames(df)[colnames(df) == lat] <- 'lat'
  } else {
    lat <- col_df[col_df %in% lst_lat]
    colnames(df)[colnames(df) == lat] <- 'lat'
    
  }
  
  if (!is.null(long)) {
    colnames(df)[colnames(df) == long] <- 'long'
  } else {
    long <- col_df[col_df %in% lst_long]
    colnames(df)[colnames(df) == long] <- 'long'
    
  }
  
  df <- df %>% ungroup() %>%
    select(index_airzone,lat,long)
  
  
  
  df_result <- NULL
  for (i in 1:nrow(df)) {
    df_ <- df[i,]
    try(
      df_result <- df_result %>%
        bind_rows(tibble(
          index_airzone = df_$index_airzone,
          airzone = get_airzone(lat =df_$lat,long = df_$long )
        )))
  }
  
  df_result <- df_initial %>%
    left_join(df_result, by = 'index_airzone') %>%
    select(-index_airzone)
  
  return(df_result)
  
}

#' Load csv or RDS Data
#' 
#' It loads data
#' 
#' @param datapath
#' @param filename
load_data <- function(datapath = './',filename) {
  if (0) {
    datapath <- './data/out/'
    filename <- 'tfee.csv'
  }
  
  filelist <- list.files(datapath)
  file_path <- paste(datapath,filename,sep='/')
  file_path <- gsub('//','/',file_path)
  # get the file extension
  file_ext <- tools::file_ext(file_path)
  
  # if the file is a CSV, use read.csv to load it
  if (file_ext == "csv") {
    data <- read.csv(file_path, header = TRUE)
  }
  
  # if the file is an RDS, use readRDS to load it
  else if (tolower(file_ext) == "rds") {
    data <- readRDS(file_path)
  }
  
  # if the file extension is not recognized, throw an error
  else {
    stop("File format not recognized.")
  }
  
  return(data)
}

#' Get a count of PM exceedances
#' 
#' 
get_PM_exceedancesummary <- function(dirs_location = './data/out') {
  
  
  # dirs_location <- './data/out'  #local location, two dots for final, one dot for debug
  if (0) {
    dirs_location <- './data/out'
  }
  
  # list.files(dirs_location)
  
  print('get PM exceedance')
  
  # df_stations <- readRDS(paste(dirs_location,'liststations_merged.Rds',sep='/'))
  df_stations <- envair::listBC_stations(use_CAAQS = TRUE,merge_Stations = TRUE)
  lst_remove <- df_stations %>%
    filter(AQMS == 'N') %>%
    pull(site)
  
  #retrieve complete list of stations, to be included in resulting list for that year
  
  df_stationdata <- readr::read_csv(paste(dirs_location,'annual_results.csv',sep='/')) %>%
    filter(!is.na(value)) %>%
    select(site,year) %>% distinct() %>%
    filter(!site %in% lst_remove) %>%
    left_join(df_stations %>% select(site,LAT,LONG,AIRZONE))
  
  #get TFEE list
  tfee_list <- readr::read_csv(paste(dirs_location,'tfee.csv',sep='/')) %>%
    left_join(df_stations %>% select(STATION_NAME,site)) %>%
    select(site,DATE,PARAMETER) %>% distinct() %>%
    filter(PARAMETER == 'PM25') %>%
    mutate(tfee = TRUE,
           DATE= as.Date(DATE))
  
  
  # df_exceed_annual <- readRDS(paste(dirs_location,'exceed_annual.Rds',sep='/')) %>%
  #   rename(site = STATION_NAME)
  # df_exceed_month<- readRDS(paste(dirs_location,'exceed_month.Rds',sep='/')) %>%
  #   rename(site = STATION_NAME)
  # df_exceed_seasonal <- readRDS(paste(dirs_location,'exceed_seasonal.Rds',sep='/')) %>%
  #   rename(site = STATION_NAME)
  
  # df_exceedances <- readRDS(paste(dirs_location,'exceedances.Rds',sep='/')) %>%
  #   rename(site = STATION_NAME)
  df_exceedances <- readr::read_csv(paste(dirs_location,'exceedances.csv',sep='/')) %>%
    rename(site = STATION_NAME)
  
  # colnames(df_stations)
  df_seasons <- tibble(
    month = 1:12
  )
  df_seasons$seasons[df_seasons$month %in% c(12,1,2)] <- 'Winter'
  df_seasons$seasons[df_seasons$month %in% c(3,4,5)] <- 'Spring'
  df_seasons$seasons[df_seasons$month %in% c(6,7,8)] <- 'Summer'
  df_seasons$seasons[df_seasons$month %in% c(9,10,11)] <- 'Fall'
  #need to summarize exceedances to air zone and entire province
  #do not add the number or will have spatial problems
  
  #count for air zones
  ##count for an entire year----
  #no wildfire assessment
  df_exceedances_year <- df_exceedances %>% 
    filter(PARAMETER == 'PM25',name == 'exceed_27') %>%
    filter(!site %in% lst_remove) %>%
    left_join(df_stations) %>%
    mutate(year = lubridate::year(DATE)) %>%
    ungroup() %>% 
    select(year,DATE,AIRZONE) %>% distinct() %>%
    group_by(year,AIRZONE) %>%
    dplyr::mutate(days_exceed = n()) %>%
    ungroup() 
  
  #wildfire (tfee) assessment
  df_exceedances_year_tfee <-
    df_exceedances %>% 
    filter((PARAMETER == 'PM25' & name == 'exceed_27')
    )%>%
    filter(!site %in% lst_remove) %>%
    left_join(df_stations) %>%
    left_join(tfee_list) %>% 
    filter(tfee == TRUE) %>%
    mutate(year = lubridate::year(DATE)) %>%
    ungroup() %>% 
    select(year,DATE,AIRZONE) %>% distinct() %>%
    group_by(year,AIRZONE) %>%
    dplyr::mutate(days_exceed = n()) %>%
    ungroup() 
  
  df_exceedances_year_bc <- df_exceedances_year %>%
    select(year,DATE) %>% distinct() %>%
    group_by(year) %>%
    summarise(days_exceed = n()) %>%
    mutate(AIRZONE = 'BC')
  
  df_exceedances_year_bc_tfee <- df_exceedances_year_tfee %>%
    select(year,DATE) %>% distinct() %>%
    group_by(year) %>%
    summarise(days_exceed = n()) %>%
    mutate(AIRZONE = 'BC')
  
  
  
  df_exceedances_year <- df_exceedances_year %>%
    bind_rows(df_exceedances_year_bc) %>%
    select(year,AIRZONE,days_exceed) %>%
    distinct() %>% arrange(year,AIRZONE)
  
  
  df_exceedances_year_tfee <- df_exceedances_year_tfee %>%
    bind_rows(df_exceedances_year_bc_tfee) %>%
    select(year,AIRZONE,days_exceed) %>%
    distinct() %>% arrange(year,AIRZONE)
  
  
  ##count for entire seasons-----
  df_exceedances_season <- df_exceedances %>%
    filter(PARAMETER == 'PM25',name == 'exceed_27') %>%
    filter(!site %in% lst_remove) %>% 
    left_join(df_stations) %>% 
    mutate(year = lubridate::year(DATE),
           month = lubridate::month(DATE)) %>%
    left_join(df_seasons) %>%
    select(year,seasons,DATE,AIRZONE) %>% distinct() %>%
    group_by(year,AIRZONE,seasons) %>%
    dplyr::mutate(days_exceed = n()) %>%
    ungroup()
  
  
  df_exceedances_season_tfee <- df_exceedances %>%
    filter(PARAMETER == 'PM25',name == 'exceed_27') %>%
    filter(!site %in% lst_remove) %>% 
    left_join(df_stations) %>% 
    left_join(tfee_list) %>%
    filter(tfee == TRUE) %>%
    mutate(year = lubridate::year(DATE),
           month = lubridate::month(DATE)) %>%
    left_join(df_seasons) %>%
    select(year,seasons,DATE,AIRZONE) %>% distinct() %>%
    group_by(year,AIRZONE,seasons) %>%
    dplyr::mutate(days_exceed = n()) %>%
    ungroup()
  
  df_exceedances_season_bc <- df_exceedances_season %>%
    select(year,seasons,DATE) %>% distinct() %>%
    group_by(year,seasons) %>%
    summarise(days_exceed = n())%>%
    mutate(AIRZONE = 'BC')
  
  df_exceedances_season_bc_tfee <- df_exceedances_season_tfee %>%
    select(year,seasons,DATE) %>% distinct() %>%
    group_by(year,seasons) %>%
    summarise(days_exceed = n())%>%
    mutate(AIRZONE = 'BC')
  
  df_exceedances_season <- df_exceedances_season %>%
    bind_rows(df_exceedances_season_bc)%>%
    select(year,seasons,AIRZONE,days_exceed) %>%
    distinct() %>% arrange(year,AIRZONE)
  
  df_exceedances_season_tfee <- df_exceedances_season_tfee %>%
    bind_rows(df_exceedances_season_bc_tfee)%>%
    select(year,seasons,AIRZONE,days_exceed) %>%
    distinct() %>% arrange(year,AIRZONE)
  
  
  
  #create for each station
  df_exceedances_station_year <- df_exceedances%>% 
    filter(PARAMETER == 'PM25',name == 'exceed_27') %>%
    filter(!site %in% lst_remove) %>%
    left_join(df_stations) %>%
    mutate(year = lubridate::year(DATE)) %>%
    ungroup() %>% 
    select(year,site,DATE,AIRZONE) %>% distinct() %>%
    group_by(year,site,AIRZONE) %>%
    dplyr::mutate(days_exceed = n()) %>%
    ungroup() %>%
    select(year,site,AIRZONE,days_exceed) %>%
    distinct()
  
  df_exceedances_station_year_tfee <- df_exceedances%>% 
    filter(PARAMETER == 'PM25',name == 'exceed_27') %>%
    filter(!site %in% lst_remove) %>%
    left_join(df_stations) %>%
    left_join(tfee_list) %>%
    filter(tfee == TRUE) %>%
    mutate(year = lubridate::year(DATE)) %>%
    ungroup() %>% 
    select(year,site,DATE,AIRZONE) %>% distinct() %>%
    group_by(year,site,AIRZONE) %>%
    dplyr::mutate(days_exceed = n()) %>%
    ungroup() %>%
    select(year,site,AIRZONE,days_exceed) %>%
    distinct()
  
  
  
  df_exceedances_station_seasonal <- df_exceedances %>%
    filter(PARAMETER == 'PM25',name == 'exceed_27') %>%
    filter(!site %in% lst_remove) %>% 
    left_join(df_stations) %>% 
    mutate(year = lubridate::year(DATE),
           month = lubridate::month(DATE)) %>%
    left_join(df_seasons) %>%
    select(year,site,seasons,DATE,AIRZONE) %>% distinct() %>%
    group_by(year,site,AIRZONE,seasons) %>%
    dplyr::mutate(days_exceed = n()) %>%
    ungroup()%>%
    select(year,seasons,site,AIRZONE,days_exceed) %>%
    distinct()
  
  df_exceedances_station_seasonal_tfee <- df_exceedances %>%
    filter(PARAMETER == 'PM25',name == 'exceed_27') %>%
    filter(!site %in% lst_remove) %>% 
    left_join(df_stations) %>% 
    left_join(tfee_list) %>%
    filter(tfee == TRUE) %>%
    mutate(year = lubridate::year(DATE),
           month = lubridate::month(DATE)) %>%
    left_join(df_seasons) %>%
    select(year,site,seasons,DATE,AIRZONE) %>% distinct() %>%
    group_by(year,site,AIRZONE,seasons) %>%
    dplyr::mutate(days_exceed = n()) %>%
    ungroup()%>%
    select(year,seasons,site,AIRZONE,days_exceed) %>%
    distinct()
  
  
  #fix df_stationdata duplicates
  df_stationdata <- df_stationdata %>% 
    group_by(site,AIRZONE,year) %>%
    dplyr::mutate(index=1:n()) %>%
    filter(index==1) %>%
    select(-index) %>% ungroup()
  
  #left join to display zero values
  df_exceedances_year <- df_stationdata %>% 
    select(AIRZONE,year) %>% 
    distinct() %>%
    bind_rows(tibble(
      AIRZONE ='BC',
      year = min(df_stationdata$year):max(df_stationdata$year) 
    )) %>%
    left_join(df_exceedances_year) %>%
    mutate(days_exceed = ifelse(is.na(days_exceed),0,days_exceed))
  
  df_exceedances_year_tfee <- df_stationdata %>% 
    select(AIRZONE,year) %>%
    
    distinct() %>%
    bind_rows(tibble(
      AIRZONE ='BC',year = min(df_stationdata$year):max(df_stationdata$year) 
    )) %>%
    filter(year>=2014) %>%
    left_join(df_exceedances_year_tfee) %>%
    mutate(days_exceed = ifelse(is.na(days_exceed),0,days_exceed))
  
  
  
  df_exceedances_season <- df_stationdata %>% 
    select(AIRZONE,year) %>% 
    merge(tibble(seasons = c('Winter','Spring','Summer','Fall'))) %>%
    distinct() %>%
    bind_rows(tibble(
      AIRZONE ='BC',
      year = min(df_stationdata$year):max(df_stationdata$year) 
    ) %>% 
      merge(tibble(seasons = c('Winter','Spring','Summer','Fall')))
    ) %>%
    left_join(df_exceedances_season) %>%
    mutate(days_exceed = ifelse(is.na(days_exceed),0,days_exceed))
  
  
  
  df_exceedances_season_tfee <- df_stationdata %>% 
    select(AIRZONE,year) %>% 
    
    merge(tibble(seasons = c('Winter','Spring','Summer','Fall'))) %>%
    distinct() %>%
    bind_rows(tibble(
      AIRZONE ='BC',
      year = min(df_stationdata$year):max(df_stationdata$year) 
    ) %>% 
      merge(tibble(seasons = c('Winter','Spring','Summer','Fall')))
    ) %>%
    filter(year >=2014) %>%
    left_join(df_exceedances_season_tfee) %>%
    mutate(days_exceed = ifelse(is.na(days_exceed),0,days_exceed))
  
  
  
  
  df_exceedances_station_year <- df_stationdata %>% 
    select(site,AIRZONE,year,LAT,LONG) %>% 
    distinct() %>%
    left_join(df_exceedances_station_year) %>%
    mutate(days_exceed = ifelse(is.na(days_exceed),0,days_exceed)) 
  
  df_exceedances_station_year_tfee <- df_stationdata %>% 
    select(site,AIRZONE,year,LAT,LONG) %>% 
    distinct() %>%
    filter(year>=2014) %>%
    left_join(df_exceedances_station_year_tfee) %>%
    mutate(days_exceed = ifelse(is.na(days_exceed),0,days_exceed)) 
  
  
  
  df_exceedances_station_seasonal <- df_stationdata %>% 
    select(site,AIRZONE,year,LAT,LONG) %>% 
    merge(tibble(seasons = c('Winter','Spring','Summer','Fall'))) %>%
    distinct() %>%
    left_join(df_exceedances_station_seasonal) %>%
    mutate(days_exceed= ifelse(is.na(days_exceed),0,days_exceed))
  
  df_exceedances_station_seasonal_tfee <- df_stationdata %>% 
    select(site,AIRZONE,year,LAT,LONG) %>% 
    merge(tibble(seasons = c('Winter','Spring','Summer','Fall'))) %>%
    distinct() %>%
    filter(year>=2014) %>%
    left_join(df_exceedances_station_seasonal_tfee) %>%
    mutate(days_exceed= ifelse(is.na(days_exceed),0,days_exceed))
  
  
  
  
  df_exceedance <- list(annual = df_exceedances_year,
                        annual_tfee = df_exceedances_year_tfee,
                        season = df_exceedances_season, 
                        season_tfee = df_exceedances_season_tfee,
                        annual_stations =df_exceedances_station_year,
                        annual_stations_tfee = df_exceedances_station_year_tfee,
                        season_stations = df_exceedances_station_seasonal,
                        season_stations_tfee = df_exceedances_station_seasonal_tfee,
                        stations = df_stationdata)
  
  
  return(df_exceedance)
}

#' Get table of the management levels
#' This will be useful for creating management level table
#' 
#' @param parameter is the parameter of either 'pm25','o3','no2','so2'
get_tbl_management <- function(parameter) {
  
  if (0) {
    parameter <- 'pm25'
  }
  
  library(tidyr)
  library(kableExtra)
  
  param_filter <- parameter
  df_params <- tibble(
    metric_old = c('no2_1yr','no2_3yr','o3','pm2.5_24h','pm2.5_annual','so2_1yr','so2_3yr'),
    metric = c('Annual Metric','1-Hour Metric',
               '8-Hour Metric',
               '24-Hour Metric','Annual Metric',
               'Annual Metric','1-Hour Metric'),
    parameter = c('no2','no2','o3','pm25','pm25','so2','so2')
  )
  
 
  
  tbl_mgmt <- rcaaqs::management_levels %>%
    select(parameter,lower_breaks,upper_breaks,val_labels,colour,colour_text) %>%
    rename(metric_old = parameter) %>%
    left_join(df_params,by='metric_old') %>% select(-metric_old) %>%
    filter(colour_text != 'grey') %>%
    mutate(val_labels = gsub('\\^3','<sup>3</sup>',val_labels)) %>%
    mutate(val_labels = gsub('ug','µg',val_labels)) %>%
    mutate(val_labels = gsub('<=','≤',val_labels)) 
  
  tbl_colour <- tbl_mgmt %>%
    select(colour,colour_text) %>% unique()
  df_actions <- tibble(
    colour_text = c('red','orange','yellow','green'),
    
    actions = c('Achieve CAAQS','Prevent CAAQS Exceedance','Prevent Air Quality Deterioration','Keep Clean Areas Clean')
  ) %>%
    left_join(tbl_colour)
  
  
  
  tbl_mgmt_colour <- 
    tbl_mgmt %>%ungroup() %>%
    filter(parameter == param_filter) %>%
    select(parameter,metric,colour,colour_text) %>%
    # View()
    
    pivot_wider(names_from = metric, values_from = colour) %>%
    mutate(index=1:n()) %>%
    arrange(desc(index)) %>% select(-index) 
  
  tbl_mgmt_value <- 
    tbl_mgmt %>%ungroup() %>%
    filter(parameter == param_filter) %>%
    select(parameter,metric,val_labels,colour_text) %>%
    # View()
    
    pivot_wider(names_from = metric, values_from = val_labels) %>%
    mutate(index=1:n()) %>%
    arrange(desc(index)) %>% select(-index) %>%
    select(-parameter) %>%
    left_join(df_actions) %>%
    mutate(colour_text = toupper(colour_text)) %>%
    select(-colour) %>%
    rename('Management Level' = colour_text,
           'Management <br>Actions' = actions)
  
  
  t <- kable(tbl_mgmt_value, escape = F, format = 'html') %>%
    
    kable_styling(full_width = T) %>%
    
    # cell_spec(0:3,background = '#A50026' )
    row_spec(1, background = '#A50026') %>%
    row_spec(2, background = '#F46D43') %>%
    row_spec(3, background = '#FEE08B') %>%
    row_spec(4, background = '#A6D96A') 
  
  return(t)
}

