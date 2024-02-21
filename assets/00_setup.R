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

validation_year <- 2021

library(envair)
library(rcaaqs)
library(tibble)
library(readr)
library(ggplot2)
library(plotly)

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
  
  #- defines the resulting column names in this order
  cols_final <- c('parameter','site','instrument','year',
                  'tfee','metric_value','metric','flag_two_of_three_years')
  
  df_data <- importBC_data(c('pm25','o3','no2','so2'),
                           years = (min(years)-3):max(years),
                           flag_TFEE = TRUE,merge_Stations = TRUE)
  
  for (param in c('pm25','o3','no2','so2')) {
    
    message(paste('Processing:',param))
    if (0) {
      param <- 'pm25'
      df_data <- importBC_data(c('pm25'),
                               years = (min(years)-3):max(years),
                               flag_TFEE = TRUE,merge_Stations = TRUE)
      df_data %>%
        clean_names %>%
        mutate(year = year(date), quarter = quarter(date)) %>%
        filter(parameter == 'PM25') %>%
        filter(station_name == 'Quesnel') %>%
        filter(!is.na(raw_value)) %>%
        group_by(station_name,instrument,year,date) %>%
        summarize(days = n()) %>%
        filter(days >= 0.75*24) %>%
        ungroup() %>%
        group_by(station_name,instrument,year) %>%
        summarize(valid_days = n())
    }
  # for (param in c('o3')) {
    df <- NULL
    
    # -Retrieve data, add extra year for ozone
    if(param != 'o3') {
      try(
        df <- df_data %>%
          filter(tolower(PARAMETER) %in% tolower(param)) %>%
          mutate(year = year(DATE)) %>%
          filter(lubridate::year(DATE) %in% (min(years)-2):max(years))
        
      )
    } else {
      try(
        
        df <- df_data %>%
          filter(tolower(PARAMETER) %in% tolower(param)) %>%
          mutate(year = year(DATE)) %>%
          filter(lubridate::year(DATE) %in% (min(years)-3):max(years))
        
        # df <- importBC_data(param,years = (min(years)-3):max(years),
        #                     flag_TFEE = TRUE,merge_Stations = TRUE)
      )
    }
    
    # -checks for data
    #if there was no data retrieved
    if (is.null(df)) {
      next
    }
    
    if (nrow(df) == 0) {
      next
    }
    
    # -remove duplicate entries
    # note for pm, instrument is included in grouping
    if (param == 'pm25') {
      
      df <- df %>%
        ungroup() %>%
        dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
        dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
        filter(!is.na(value)) %>%
        group_by(date_time,site,instrument) %>%
        dplyr::mutate(index = 1:n()) %>%
        filter(index == 1) %>% select(-index) %>%
        ungroup() %>%
        select(date_time,site,instrument,value,flag_tfee)
      
      # - pad data to include missing years
      start_date <- min(df$date_time)
      end_date <- max(df$date_time)
      
      df_fill <- df %>%
        select(site,instrument) %>%
        distinct() %>%
        cross_join(
          tibble(
            date_time = seq(from = start_date,to = end_date, by = 'hour')
          )
        )
      
      df <- left_join(df_fill,df)
    } else {
      df <- df %>%
        ungroup() %>%
        dplyr::mutate(date_time = DATE_PST - lubridate::hours(1)) %>%
        dplyr::rename(value = RAW_VALUE, site = STATION_NAME, instrument = INSTRUMENT) %>%
        filter(!is.na(value)) %>%  
        group_by(date_time,site) %>%
        dplyr::mutate(index = 1:n()) %>%
        filter(index == 1) %>% select(-index) %>%
        ungroup() %>%
        select(date_time,site,instrument,value,flag_tfee)
      
      # - pad data to include missing years
      start_date <- min(df$date_time)
      end_date <- max(df$date_time)
      
      df_fill <- df %>%
        select(site) %>%
        distinct() %>%
        cross_join(
          tibble(
            date_time = seq(from = start_date,to = end_date, by = 'hour')
          )
        )
      
      df <- left_join(df_fill,df)
    }
    
  
    
    
    
    # -perform calculations for PM2.5
    if (param == 'pm25') {
      
      if (0) {
        # a <- df
        df <- a %>%
          filter(!is.na(value))
        df %>%
          # filter(!is.na(value)) %>%
          nrow()
      }
      
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
      
      
      #add the values with TFEE
      #with TFEE
      # -retrieve tfee dates
      tfee_dates <- df %>%
        filter(flag_tfee) %>%
        # Ceiling TFEE to capture original dates
        mutate(date = ceiling_date(date_time, unit = "hour"),
               date = as_date(date)) %>%
        select(site, instrument, date) %>%
        distinct()
      
      pm25_ann_mgmt <- caaqs_management(pm25_annual, 
                                        exclude_df = tfee_dates, 
                                        exclude_df_dt = "date")
      pm25_ann_tfee <- pm25_ann_mgmt$caaqs %>%
        select(site,instrument,caaqs_year,metric,metric_value_mgmt) %>%
        rename(metric_value  = metric_value_mgmt)
      pm25_24h_mgmt <- caaqs_management(pm25_24h, 
                                        exclude_df = tfee_dates, 
                                        exclude_df_dt = "date")
      pm25_24h_tfee <- pm25_24h_mgmt$caaqs %>%
        select(site,instrument,caaqs_year,metric,metric_value_mgmt) %>%
        rename(metric_value  = metric_value_mgmt)
      
      # - copy formatting from non-tfee values, then replace with calculated tfee value
      pm25_annual$caaqs <- pm25_annual$caaqs %>%
        select(-metric_value) %>%
        left_join(pm25_ann_tfee)
      
      pm25_24h$caaqs <- pm25_24h$caaqs %>%
        select(-metric_value) %>%
        left_join(pm25_24h_tfee)
      

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
      gc()
    }
    
    if (param == 'o3') {
      
      #without TFEE
      o3_8h <- rcaaqs::o3_caaqs(df,by=c('site'))
      
      # calculate tfee
      tfee_dates <- df %>%
        filter(flag_tfee) %>%
        # Ceiling TFEE to capture original dates
        mutate(date = ceiling_date(date_time, unit = "hour"),
               date = as_date(date)) %>%
        select(site, instrument, date) %>%
        distinct()
      
      o3_mgmt <- caaqs_management(o3_8h, 
                                  exclude_df = tfee_dates, 
                                  exclude_df_dt = "date")
      #with TFEE
      # - copy formatting from non-tfee values, then replace with calculated tfee value
      o3_tfee <- o3_mgmt$caaqs %>%
        select(site,caaqs_year,metric,metric_value_mgmt) %>%
        rename(metric_value  = metric_value_mgmt)
      
      # - copy formatting from non-tfee values, then replace with calculated tfee value
      o3_8h_tfee <- o3_8h
      o3_8h_tfee$caaqs <- o3_8h_tfee$caaqs %>%
        select(-metric_value) %>%
        left_join(o3_tfee)
      
      
        
      
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
      gc()
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
      gc()
      
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
      gc()
      
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
    datafile <- './data/out/caaqs_results.csv'
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
  
  
  try({
    df$site <- gsub('[^[:alnum:]]',' ',df$site)
    df$site <- gsub('  ',' ',df$site)
    
  })
  
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
    datafile = paste('./data/out/caaqs_results.csv',sep='/')
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
  
  if (is.null(datafile)) {
    datafile = paste('./data/out/caaqs_results.csv',sep='/')
  }
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
      select(site,instrument,year,metric,metric_value,
             colour,colour_text,colour_order,tfee,flag_two_of_three_years) %>%
      left_join(lst_stations) %>%
      left_join(df_metric)
  }
  if (0) {
    readr::write_csv(df,'././test_data/management.csv')
  }
  #add order to the metric
  df$metric <- factor(df$metric,levels = df_metric$metric)
  #calculate and return result based on the type specified
  
  #if previously red, and then current does not haveenough data, make it insufficient
  

 # df_select <- 
  #list of years and sites with just one value of PM avaiable
  df <- df %>%
    mutate(index = paste(site,instrument,year,tfee))
  
  lst_ <- df %>%
   filter(tolower(parameter) == 'pm25') %>%
    group_by(site,instrument,year,tfee) %>%
    mutate(count =n())  %>%
    filter(count<2,colour_text != 'red') %>%
    mutate(srch_index = paste(site,instrument,year-1,tfee)) %>%
    pull(srch_index)
  
  #the previously red 
  lst2_ <- df %>%
    filter(index %in% lst_) %>%
    mutate(srch_index = paste(site,instrument,year+1,tfee)) %>%
    pull(srch_index)
  #check if previous was red
  df <-  df %>%
    filter(!index %in% lst2_) %>%
    select(-index)
  
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
    filename <- 'management_airzones.csv'
    filename <- 'managementsummary_tables.Rds'
    datapath <- 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/data/out'  #local location, two dots for final, one dot for debug
    
  }
  
  filelist <- list.files(datapath)
  file_path <- paste(datapath,filename,sep='/')
  # file_path <- gsub('//','/',file_path)
  # get the file extension
  file_ext <- tools::file_ext(file_path)
  
  # if the file is a CSV, use read.csv to load it
  if (file_ext == "csv" ) {
    
    if (!grepl('http',datapath,ignore.case = TRUE)) {
      data <- read.csv(file_path, header = TRUE)
    } else {
      data <- readr::read_csv(file_path)
    }
  }
  
  # if the file is an RDS, use readRDS to load it
  else if (tolower(file_ext) == "rds") {
    try(data <- readRDS(url(file_path)),silent = TRUE)
    try(data <- readRDS((file_path)),silent = TRUE)
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
#' @param outputtype is either "all", "html"
get_tbl_management <- function(parameter, outputtype = 'html') {
  
  if (0) {
    parameter <- 'pm25'
  }
  
  library(tidyr)
  library(kableExtra)
  library(stringr)
  library(dplyr)
  
  param_filter <- parameter
  df_params <- tibble(
    metric_old = c('no2_1yr','no2_3yr','o3','pm2.5_24h','pm2.5_annual','so2_1yr','so2_3yr'),
    metric = c('Annual Metric','1-Hour Metric',
               '8-Hour Metric',
               '24-Hour Metric','Annual Metric',
               'Annual Metric','1-Hour Metric'),
    parameter = c('no2','no2','o3','pm25','pm25','so2','so2'),
    decimals = c(1,0,0,0,1,1,0)
  )
  
  
  
  tbl_mgmt <- rcaaqs::management_levels %>%
    select(parameter,lower_breaks,upper_breaks,val_labels,colour,colour_text) %>%
    rename(metric_old = parameter) %>%
    dplyr::left_join(df_params,by='metric_old') %>% select(-metric_old) %>%
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
    dplyr::left_join(tbl_colour)
  
  
  
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
    dplyr::left_join(df_actions) %>%
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
  
  #create a list - added 2023-08-21
  result <- list()
  result$table <- t
  
  #transform the table to simplified version
  
  #special functions
  extract_numbers_after_arrow <- function(input_string) {
    numbers <- str_extract_all(input_string, "(?<=\\> )\\d+(\\.\\d+)?")
    numbers <- unlist(numbers)
    return(numbers)
  }
  # Function to remove leading and repetitive spaces
  remove_spaces <- function(input_string) {
    lines <- strsplit(input_string, "\n")[[1]]
    cleaned_lines <- gsub("^\\s+", "", lines)
    cleaned_lines <- gsub("\\s+", " ", cleaned_lines)
    cleaned_string <- paste(cleaned_lines, collapse = "\n")
    return(cleaned_string)
  }
  
  
  tbl_mgmt_value_ <- tbl_mgmt_value %>%
    select(c(-`Management <br>Actions`)) %>%
    mutate(parameter = param_filter) %>% ungroup() %>%
    pivot_longer(cols = -c('parameter','Management Level')) %>%
    rename(metric = name) %>%
    dplyr::left_join(df_params %>% select(-metric_old))
  
  #fix the value text
  #this will conver from the complex numbers to simpler version
  
  
  
  for (i in 1:nrow(tbl_mgmt_value_)) {
    if (0) {
      i <- 5
    }
    
    
    i_ <- tbl_mgmt_value_[i,]
    value <- i_$value
    
    #sup changed temporarily to prevent interference
    value <- gsub('<sup>','???',value)
    value <- gsub('</sup>','~~~',value)
    
    if (grepl('&',i_$value,ignore.case = TRUE)) {
      
      value <- gsub('&','to',value)
      value <- gsub('≤','',value)
      #get the number after ">", and add 0.1 or 1 depends on significant figures
      orig_number <- extract_numbers_after_arrow(value)
      if (i_$decimals == 1) {
        value <-
          gsub(paste('> ',as.character(orig_number),sep=''), 
               as.character(as.numeric(orig_number) + 0.1), value)
        
      } else {
        value <-
          gsub(paste('> ',as.character(orig_number),sep=''), 
               as.character(as.numeric(orig_number) + 1), value)
      }
      
      value <- gsub('>','',value)
      value <- remove_spaces(value)
      
      # print(value)
    } 
    value <- gsub('> ','>',value)
    value <- gsub('≤ ','≤',value)
    value <- gsub('\\?\\?\\?','<sup>',value)
    value <- gsub('\\~~~','</sup>',value)
    tbl_mgmt_value_[i,]$value[1] <- value[[1]]
  }
  
  tbl_mgmt_value_wide <- tbl_mgmt_value_ %>%
    select(-decimals) %>%
    ungroup() %>%
    distinct() %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    select(-parameter)
  
  
  
  cols_tbl <- colnames(tbl_mgmt_value_wide)
  cols_tbl_remove <- cols_tbl[!cols_tbl %in% c('Management Level')]
  
  tbl_mgmt_wide <- tbl_mgmt_value %>%
    select(-any_of(cols_tbl_remove)) %>%
    dplyr::left_join(tbl_mgmt_value_wide) %>%
    select(colnames(tbl_mgmt_value)) %>%
    ungroup()
  
  
  t2 <-
    kable(tbl_mgmt_wide, escape = F, format = 'html') %>%
    
    kable_styling(full_width = T) %>%
    
    # cell_spec(0:3,background = '#A50026' )
    row_spec(1, background = '#A50026',color = 'white') %>%
    row_spec(2, background = '#F46D43',color = 'white') %>%
    row_spec(3, background = '#FEE08B',color = 'black') %>%
    row_spec(4, background = '#A6D96A',color = 'black') 
  
  result$table_friendly_text <- tbl_mgmt_wide
  result$table_friendly <- t2
  
  
  if (tolower(outputtype) == 'html') {
    result <- t2
  }

  return(result)
}




#' BACKEND Create a table that summarize the management level
#' 
#' @param dataDirectory is the location of the management.csv file
#' @param current_year is the year to be displayed

get_tbl_management_summary_ <- function(dataDirectory = '../data/out',current_year = NULL) {
  
  if (0) {
    dataDirectory = 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/data/out/'
    current_year <- 2019
  }
  require(dplyr)
  require(ggplot2)
  require(patchwork)
  require(kableExtra)
  #input options
  df_mgmt_results <- NULL
  try(df_mgmt_results <- load_data(datapath = dataDirectory,filename = 'management.csv') %>%
        select(site,instrument,year,tfee,parameter,metric,metric_value,colour,colour_text) %>%
        distinct() )
  
  
  if (is.null(df_mgmt_results)) {
    print(paste('Missing File:"',dataDirectory,'/management.csv"',sep=''))
    return(NULL)
  }
  
  df_colour_levels <- tribble(
    ~colour_text,~colour_order,~colour,
    'grey',0,'#dbdbdb',
    'green',1,'#A6D96A',
    'yellow',2,'#FEE08B',
    'orange',3,'#F46D43',
    'red',4,'#A50026'
  )
  
  
  df_station <- envair::listBC_stations(use_CAAQS=TRUE) %>%
    mutate(AQMS =ifelse(is.na(AQMS),'NA',AQMS)) %>%
    filter(AQMS != 'N') %>%
    dplyr::rename(airzone = AIRZONE) %>%
    select(site,airzone,Label) %>%
    distinct()
  
  #list of all airzones
  lst_airzone <- bcmaps::airzones() %>% 
    as_tibble() %>%
    rename(airzone = Airzone) %>%
    select(airzone) 
  
  
  
  
  
  #identify parameters with no TFEE adjustment
  lst_no_tfee_param <- df_mgmt_results %>%
    filter(
      !parameter %in% 
        df_mgmt_results$parameter[df_mgmt_results$tfee]
    ) %>%
    pull(parameter) %>%
    unique()
  
  #reproduce lst_no_tfee_param with tfee=TRUE
  df_mgmt_results <- df_mgmt_results %>%
    bind_rows(
      df_mgmt_results %>% 
        filter(parameter %in% lst_no_tfee_param) %>%
        mutate(tfee = TRUE))
  
  # colnames(df_mgmt_results)
  
  #add fillers
  df_fill <- df_mgmt_results %>%
    select(year,metric,metric_value,colour,colour_text) %>%
    distinct()
  
  df_count <- df_mgmt_results %>%
    left_join(df_station) %>%
    filter(!is.na(metric_value)) %>%
    select(year,parameter,site,tfee,airzone) %>%
    distinct() %>%
    group_by(year,parameter,airzone,tfee) %>%
    dplyr::summarise(total_sites = n()) 
  
  
  df_mgmt_airzone <-
    df_mgmt_results %>%
    left_join(df_station) %>%
    left_join(df_colour_levels) %>%
    left_join(df_count) %>%
    group_by(year,parameter,airzone,tfee) %>%
    dplyr::mutate(max_colour_order = max(colour_order)) %>%
    filter(colour_order == max_colour_order) %>%
    dplyr::mutate(sites = list(Label),
                  metrics = list(metric)) %>%
    ungroup() %>%
    select(-max_colour_order,-site,-metric,-metric_value,-Label,-instrument) %>%
    distinct()
  
  df_mgmt_airzone$metrics <- lapply(df_mgmt_airzone$metrics,unique)
  df_mgmt_airzone$sites <- lapply(df_mgmt_airzone$sites,unique)
  df_mgmt_airzone$sites_count <- unlist(lapply(df_mgmt_airzone$sites,length))
  
  df_mgmt_airzone <- df_mgmt_airzone %>%
    filter(!is.na(airzone))
  
  
  
  
  
  #add special text
  df_mgmt_airzone_table <- df_mgmt_airzone %>%
    mutate(perc_sites = sites_count/total_sites * 100) %>%
    arrange(parameter,year,airzone) %>% ungroup() %>%
    dplyr::mutate(index = 1:n()) %>%
    group_by(index) %>%
    mutate(sites = paste(unlist(sites),collapse=', ')) %>% 
    mutate(special_txt = ifelse(perc_sites==100 & total_sites >2,
                                paste('All',total_sites,'sites',sep=' '),
                                ifelse(sites_count<=3,sites,
                                       paste(sites_count,'of',total_sites,'sites',sep=' '))
    )) 
  
  df_mgmt_airzone_table$special_txt[is.na(df_mgmt_airzone_table$special_txt)] <- 'Data Not Available'
  
  #add Northwest air zone
  df_mgmt_airzone_table <- df_mgmt_airzone_table %>%
    bind_rows(
      tribble(
        ~airzone,~colour,~colour_order,~colour_text,~special_txt,
        'Northwest','#dbdbdb',0,'grey','Data Not Available'
      ) %>%
        merge(
          df_mgmt_airzone_table %>% ungroup() %>%
            select(year,tfee,parameter) %>%
            distinct()
        )
    )
  
  # added new
  table_mgmt <- df_mgmt_airzone_table %>%
    filter(tfee == TRUE) %>%
    ungroup() %>%
    filter(year == current_year) 
  
  #add fillers to table_mgmt
  colnames(table_mgmt)
  df_filler <- table_mgmt %>%
    select(airzone) %>% distinct() %>%
    merge(
      table_mgmt %>%
        select(parameter,tfee) %>% unique(),
      all =  TRUE
    ) %>%
    merge(
      table_mgmt %>%
        select(year) %>%
        distinct()
    )
  
  
  table_mgmt_ <- df_filler %>%
    left_join(table_mgmt)
  
  table_mgmt_$special_txt[is.na(table_mgmt_$colour)] <- 'Data Not Available'
  table_mgmt_$colour_text[is.na(table_mgmt_$colour_text)] <- 'grey'
  table_mgmt_$colour_order[is.na(table_mgmt_$colour_order)] <- 0
  table_mgmt_$colour[is.na(table_mgmt_$colour)] <- '#dbdbdb'
  
  table_mgmt <- table_mgmt_
  #rename the parameter
  df_parameter <- tribble(
    ~display, ~parameter,
    'PM\u2082.\u2085','pm25',
    'Ozone','o3',
    'NO\u2082','no2',
    'SO\u2082','so2'
  )
  
  df_parameter$display <- factor(df_parameter$display,levels =df_parameter$display)
  
  #add the html detals
  
  #create a table summary for display
  table_mgmt_display <- table_mgmt %>%
    select(airzone,parameter,special_txt) %>%
    rename(`Air Zone` = airzone) %>%
    left_join(df_parameter) %>%
    arrange(display) %>%
    select(-parameter) %>%
    # mutate(special_txt = gsub(', ','<br>',special_txt)) %>%
    tidyr::pivot_wider(names_from = display,values_from = special_txt)
  
  
  
  
  #for reference: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Installation
  tbl_output <- table_mgmt_display %>%
    arrange(`Air Zone`) %>%
    # bind_rows(tribble(
    #   ~`Air Zone`,
    #   'Red',
    #   'Orange',
    #   'Yellow',
    #   'Green',
    #   'Gray'
    # )) %>%
    kbl('html',escape = F) %>%
    kable_styling("bordered",position = 'center') %>%
    kableExtra::column_spec(column=1,width="15em",color = 'black') %>%
    kableExtra::column_spec(column=2:ncol(table_mgmt_display),width="30em",color = 'black') %>%
    kableExtra::row_spec(row=0,color = 'white',background = 'black',align = 'c')
  
  #add colour details
  # start by assigning the column numbrs for the parameters
  df_column <- tribble(
    ~parameter,~colnum,
    'pm25',2,
    'o3',3,
    'no2',4,
    'so2',5,
  )
  
  for (param in unique(table_mgmt$parameter)) {
    if (0) {
      param <- 'o3'
    }
    print(paste('creating table:',param))
    colour_assign <- table_mgmt %>%
      filter(parameter == param,tfee) %>%
      arrange(airzone) %>%
      mutate(txt_colour = ifelse(colour_text %in% c('red','grey'),'white','black'))
    
    colnum <- df_column$colnum[df_column$parameter == param]
    
    tbl_output <- tbl_output %>%
      kableExtra::column_spec(column = colnum,
                              background = colour_assign$colour,
                              color = colour_assign$txt_colour,
                              popover = spec_popover(content = colour_assign$colour_text,
                                                     trigger = 'hover',
                                                     position = 'auto'))
  }
  
  result <- list(graph = tbl_output, data = table_mgmt_display,raw_data = df_mgmt_airzone_table)
  return(result)
  
}

#' Create a table that summarize the management level
#' 
#' @param dataDirectory is the location of the management.csv file
#' @param current_year is the year to be displayed
#' 
#' @output is a list of graph_<year>, data_<year>, and raw_data
get_tbl_management_summary <-  function(dataDirectory = '../data/out',current_year = NULL) {
  if (0) {
    dataDirectory = 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/data/out/'
    current_year <- NULL
  }
  dataDirectory_ <- dataDirectory
  current_year_ <- current_year
  if (!is.null(current_year)) {
    result <- get_tbl_management_summary_(dataDirectory = dataDirectory_, current_year = current_year_)
    return(result)
  } else {
    #means current_year not defined, so user wants ALL years 2013 to present
    #first determine the latest data year, start with 2013
    result_2013 <- get_tbl_management_summary_(dataDirectory = dataDirectory_, current_year = 2013)
    years <- (2013+1):max(result_2013$raw_data$year,na.rm = TRUE)
    result <- list()
    result[['graph_2013']] <-  result_2013$graph
    result[['data_2013']] <-  result_2013$data
    result[['raw_data']] <- result_2013$raw_data
    for (yr in years) {
      df <- get_tbl_management_summary_(dataDirectory = dataDirectory_, current_year = yr)
      result[[paste('graph_',yr,sep='')]] <-  df$graph
      result[[paste('data_',yr,sep='')]] <-  df$data
      
    }
    
    return(result)
  }
}


#' Add TFEE = TRUE for parameter without it
#' 
#' @param df is the dataframe
#' @param category is the column that data is categorized
#' 
#' 
add_tfee_filler <- function(df, category = 'parameter') {
  
  if (0) {
    df <- df_sites
    category = 'metric'
  }
  
  df <- ungroup(df)
  cols <- colnames(df)
  
  cols_tfee <- cols[grepl('tfee',cols,ignore.case = TRUE)][[1]]
  cols_cat <- category
  
  cols <- cols[!(cols %in% cols_tfee)]
  
  #rename columns 
  df <- df %>%
    rename('tfee_column' = cols_tfee,
           'category_column' = cols_cat)
  
  #identify cateofry with missing tfee value
  all_cat <- df %>%
    select(tfee_column, category_column) %>%
    distinct()
  
  lst_cat <- all_cat %>% pull(category_column) %>% unique()
  
  lst_cat_true <- all_cat %>%
    filter(tfee_column == TRUE) %>%
    pull(category_column) %>% unique()
  lst_cat_true <-   lst_cat[!lst_cat %in% lst_cat_true]
  
  lst_cat_false <- all_cat %>%
    filter(tfee_column == FALSE) %>%
    pull(category_column) %>% unique()
  lst_cat_false <-   lst_cat[!lst_cat %in% lst_cat_false]
  
  #add tfee = TRUE value
  if (length(lst_cat_true)>0) {
    df_ <- df %>%
      filter(category_column %in% lst_cat_true) %>%
      mutate(tfee_column = TRUE)
    
    df <- df %>%
      bind_rows(df_)
  }
  
  #add tfee = FALSE value
  if (length(lst_cat_false)>0) {
    df_ <- df %>%
      filter(category_column %in% lst_cat_false) %>%
      mutate(tfee_column = FALSE)
    
    df <- df %>%
      bind_rows(df_)
  }
  
  #rename columns back to original
  
  
  colnames(df)[colnames(df) == 'tfee_column'][[1]] <- cols_tfee
  colnames(df)[colnames(df) == 'category_column'][[1]] <- cols_cat
  
  return(df)
}


#' Create management level legend table
#' This is generalized
add_mgmt_legend <- function() {
  
  library(DT)
  df_colour_levels <- tribble(
    ~colour_text,~colour_order,~colour,~actions,~txt_colour,
    'N/A',0,'#dbdbdb','No Data','black',
    'Green',1,'#A6D96A','Keep Clean Areas Clean','black',
    'Yellow',2,'#FEE08B','Prevent Air Quality Deterioration','black',
    'Orange',3,'#F46D43','Prevent CAAQS Exceedance','black',
    'Red',4,'#A50026','Achieve CAAQS','white'
  ) %>%
    arrange(desc(colour_order))
  
  a <- DT::datatable(df_colour_levels %>%
                       select(colour_text,actions) %>%
                       rename(`Management Level` = colour_text,
                              `Recommended Management Actions` = actions),
                     rownames = FALSE,
                     options = list(
                       autoWidth = TRUE,
                       borders = TRUE,
                       scrollX = FALSE,
                       paging = FALSE,
                       ordering = FALSE,
                       info =FALSE,
                       searching = FALSE
                     )
  ) %>%
    
    DT::formatStyle('Management Level',target = 'row',backgroundColor = styleEqual(df_colour_levels$colour_text,df_colour_levels$colour),
                    Color = styleEqual(df_colour_levels$colour_text,df_colour_levels$txt_colour)) 
  
  return(a)
}


#' Pad dataframe
#' 
#' @description This data frame does not have to be a date-time dataframe
pad_df <- function(df,static_columns,filled_columns) {
  if (0) {
    df <- df_data
    static_columns <- c('site')
    filled_columns <- c('parameter', 'metric')
    colnames(df)
  }
  
  df <- ungroup(df)
  cols_df <- colnames(df)
  
  static_columns <- static_columns[static_columns %in% cols_df]
  filled_columns <- filled_columns[filled_columns %in% cols_df]
  
  df_ <- df %>%
    select_at(static_columns) %>%
    distinct()
  
  df_filled <- df %>%
    select_at(filled_columns) %>%
    distinct()
  
  df_ <- df_ %>%
    merge(
      df_filled
    ) 
  
  df_ %>%
    left_join(df) %>% 
    return()
}


#create management level tables


#' Create a Comprehensive Result of Management Levels
#' 
#' @description outputs a list of manamgenet summary tables
#' 
#' @param data_directory is the source of data, default uses github data
#' @param data_years is a vector list of years to include here
get_management_summary_complete <- function(data_directory = NULL,data_years = NULL){
  
  
  if (0) {
    data_directory <- NULL
    data_years = NULL
  }
  df_param_list <- tibble(
    parameter = c('PM25','O3','NO2','SO2'),
    label = c('PM<sub>2.5</sub>','O<sub>3</sub>','NO<sub>2</sub>','SO<sub>2</sub>')
  )
  
  #' 
  library(dplyr)
  library(tidyr)
  library(kableExtra)
  try(source('./assets/00_setup.R'))
  
  dirs_location <- data_directory
  years <- data_years
  if (is.null(dirs_location)) {
    # dirs_location <- './data/out'
    dirs_location <- 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/data/out/'
  }
  
  if (is.null(data_years)) {
    # dirs_location <- './data/out'
    df_data <- load_data(datapath = dirs_location,filename = 'caaqs_results.csv')
    years <- unique(df_data$year)
    years <- sort(years)
    current_year <- max(years)
  }
  
  
  
  
  
  result <- list() #initialize
  #retrieves managemenet levels
  #through several years (2013-present)
  tbl_mgmt_airzone <- get_tbl_management_summary(dataDirectory = dirs_location)
  result <- tbl_mgmt_airzone
  # saveRDS(tbl_mgmt_airzone,'./data/out/management_summary.Rds')
  
  if (0) {
    df_sites <- read_csv('./data/out/management_sites.csv')
    df_data <- read_csv('./data/out/caaqs_results.csv')
  }
  df_sites <- load_data(datapath = dirs_location,filename = 'management_sites.csv')
  df_data <- load_data(datapath = dirs_location,filename = 'caaqs_results.csv')
  
  #change the site name
  df_sites$site <- gsub('[^[:alnum:]]',' ',df_sites$site)
  df_data$site <- gsub('[^[:alnum:]]',' ',df_data$site)
  
  df_data <- df_data %>%
    filter(year %in% df_sites$year)
  
  df_stations <- df_sites %>%
    arrange(airzone) %>%
    select(site,latitude,longitude,airzone) %>%
    group_by(site) %>%
    slice(1) %>% ungroup()
  #some metrics do no have value for when TFEE is true
  #this means there are no tfee adustments made on these
  #so the lines below would duplicate these parameters for tfee = TRUE version of data
  
  df_data <- add_tfee_filler(df_data,category = 'metric')
  df_sites <- add_tfee_filler(df_sites,category = 'metric')
  
  
  #make sure only one data for each site
  df_data <- df_data %>%
    ungroup() %>%
    filter(!is.na(metric_value)) %>%
    group_by(year,parameter,site,metric,tfee) %>%
    slice(1) %>% ungroup()
  
  
  #combine data and management result
  colnames(df_sites)
  colnames(df_data)
  
  #rename columns so the two can merge properly
  df_sites <- df_sites %>%
    rename(parameter = pollutant,
           basis = metric,
           basis_value = metric_value) 
  
  
  
  df_sites <- df_sites %>%
    left_join(df_data) 
  
  #check for discrepancy
  
  #this should be zero if all else are correct
  colnames(df_sites)
  df_sites %>%
    filter(basis == metric) %>%
    filter(basis_value != metric_value) 
  
  lst_stations <- load_data(datapath = dirs_location, filename = 'liststations.csv')
  lst_stations <- envair::listBC_stations(use_CAAQS = TRUE)
  
  
  lst_exclude <- lst_stations %>%
    filter(AQMS == 'N') %>%
    pull(site) %>%
    unique()
  
  #fix for those with multiple PM instrument
  df_sites <-
    df_sites %>%
    group_by(site,year,tfee,parameter,basis) %>%
    mutate(count = n(), index = 1:n()) %>%
    # filter(count>1) 
    filter(index == 1) %>% select(-index, -count)
  
  df_sites <- df_sites %>%
    filter(!site %in% lst_exclude)
  
  # readr::write_csv(df_sites,'./data/out/management_sites.csv')
  # readr::write_csv(lst_stations,'./data/out/liststations.csv')
  
  
  
  lst_sites <- unique(df_sites$site[df_sites$year == current_year])
  lst_sites <- sort(lst_sites)
  
  #generate table by simplifying, make multiple subtables
  colnames(df_sites)
  colnames(df_data)
  #start by padding empty year and data 
  df_sites <- pad_df(df_sites,static_columns = c('site'),
                     filled_columns = c('parameter','tfee','year'))
  df_data <- pad_df(df_data,static_columns = c('site'),
                    filled_columns = c('parameter','metric','tfee','year'))
  
  #site colour table
  df_site_colour <- df_sites %>%
    ungroup() %>%
    select(parameter,site,tfee,year, colour,colour_text) %>%
    distinct()
  
  colnames(df_sites)
  
  #set parameter order
  order_param <- c('pm25_annual','pm25_24h','o3_8h',
                   'no2_ann','no2_1hr','so2_ann','so2_1hr')
  
  
  
  #this creates heading label for the metric
  df_metric_label <-
    df_data %>% ungroup() %>%
    arrange((metric_value)) %>%
    select(metric,parameter) %>%
    filter(!grepl('(1yr)',metric)) %>%
    mutate(metric = factor(metric,levels = order_param)) %>%
    distinct() %>%
    mutate(metric_ = recode(metric,
                            "pm25_24h" = '24-hour',
                            'pm25_annual'='annual',
                            'o3_8h'='8-hour',
                            'no2_1hr'='1-hour',
                            'no2_ann'='annual',
                            'so2_ann'='annual',
                            'so2_1hr'='1-hour')
    ) %>%
    group_by(parameter) %>%
    mutate(metric_label = paste(metric_,collapse = '/')) %>%
    select(parameter,metric_label) %>% distinct()
  
  # Create management level table  for each station 
  {
    #the metrics to display
    df_site_metric <-
      df_data %>%
      ungroup() %>%
      
      select(parameter,site,tfee,year,metric,metric_value) %>%
      filter(!grepl('(1yr)',metric)) %>%
      mutate(metric = factor(metric,levels = order_param)) %>%
      arrange(metric) %>%
      mutate(metric_value = ifelse(is.na(metric_value),'-',as.character(metric_value))) %>%
      group_by(parameter,site,tfee,year) %>%
      summarise(metric_display = paste(metric_value,collapse = '/'))
    
    
    
    
    tbl_mgmt_display <-  df_site_metric %>%
      #mutate(parameter = factor(parameter,levels = df_param_list$parameter)) %>%
      # filter(site == site_) %>% 
      filter(tfee == TRUE) %>%
      group_by(site,year,parameter) %>%
      slice(1) %>%
      ungroup() %>%
      select(site,year,metric_display,parameter) %>% 
      # View()
      pivot_wider(names_from = parameter, values_from = metric_display) %>%
      arrange((year)) %>%
      envair::COLUMN_REORDER(c('site','year','PM25','O3','NO2','SO2'))
    
    tbl_mgmt_colour <-
      df_site_colour %>%
      # filter(site == site_) %>% View()
      filter(tfee == TRUE) %>%
      mutate(colour = ifelse(is.na(colour),'#DBDBDB',colour)) %>%
      arrange(desc(colour)) %>%
      group_by(site,year,parameter,tfee) %>%
      
      slice(1) %>%
      ungroup() %>%
      select(site,year,colour,parameter) %>% 
      
      pivot_wider(names_from = parameter, values_from = colour,values_fill = '#DBDBDB')%>%
      envair::COLUMN_REORDER(c('site','year','PM25','O3','NO2','SO2'))
    
    
    # View(tbl_mgmt_colour)
    list_management <- list(header = df_metric_label,
                            colour = tbl_mgmt_colour,
                            display = tbl_mgmt_display)
    
    
    for (site_ in lst_sites) {
      if (0) {
        site_ <- 'Williams Lake Columneetza School'
      }
      print(paste('Generating summary for:',site_))
      colnames(df_sites)
      
      
      tbl_display<-  list_management$display %>%
        filter(site == site_) %>%
        select(-site)
      tbl_colour <- list_management$colour %>%
        filter(site == site_)  %>%
        select(-site)
      
      a <- tbl_display %>%
        rename(Year = year) %>%
        mutate(Year = paste((Year-2),Year,sep='-')) 
      
      #rename columns to change the header
      colnames(a) <- c('Reporting<br>Period',
                       'PM<sub>2.5</sub>, µg/m<sup>3</sup><br>(annual/24-hr)',
                       'O<sub>3</sub>, ppb<br>(8-hour)',
                       'NO<sub>2</sub>, ppb<br>(annual/1-hour)',
                       'SO<sub>2</sub>, ppb<br>(annual/1-hour)')
      
      try({p <- a %>%
        kbl('html', escape = F,caption = site_) %>%
        kable_styling("bordered",position = 'center') %>%
        kableExtra::column_spec(column=1,width="15em",color = 'black') %>%
        kableExtra::column_spec(column=2:ncol(tbl_display),width="30em",color = 'black') %>%
        kableExtra::row_spec(row=0,color = 'white',background = 'black',align = 'c') %>%
        kableExtra::row_spec(row=1:nrow(tbl_display),color = 'black',align = 'c') %>%
        kableExtra::column_spec(column = 2,
                                background = tbl_colour$PM25,
                                color = 'black') %>%
        kableExtra::column_spec(column = 3,
                                background = tbl_colour$O3,
                                color = 'black') %>%
        kableExtra::column_spec(column = 4,
                                background = tbl_colour$NO2,
                                color = 'black') %>%
        kableExtra::column_spec(column = 5,
                                background = tbl_colour$SO2,
                                color = 'black') %>%
        kableExtra::row_spec(row = nrow(tbl_display),
                             extra_css = "border-top: 2px solid black; 
                           border-bottom: 2px solid black;")
      result[[site_]] <- p})
      
    }
    
    result[["legend"]] <- add_mgmt_legend()
    }
  
  #end of station entries
  
  #create management levels for all for each year
  {
    #combine air zone and station results
    df_airzone <- tbl_mgmt_airzone$raw_data %>%
      ungroup() %>%
      select(parameter,year,tfee,colour,colour_text,airzone) %>%
      mutate(site = paste('!',airzone,'<br>Air Zone'),
             parameter = toupper(parameter))
    
    
    #for data before 2020, remove NO2 and SO2 management levels
    df_airzone$colour[df_airzone$year<2020 & 
                        df_airzone$parameter %in% c('NO2','SO2')] <- '#BFBFBF'
    df_airzone$colour_text[df_airzone$year<2020 & 
                             df_airzone$parameter %in% c('NO2','SO2')] <- 
      'NO<sub>2</sub>,SO<sub>2</sub> CAAQS<br>Not applied before 2020'
    
    #value for Northwest air zone
    df_airzone$colour[grepl('northwest',df_airzone$site,ignore.case = TRUE)] <- '#BFBFBF'
    df_airzone$colour_text[grepl('northwest',df_airzone$site,ignore.case = TRUE)] <- 'No Data Available'
    
    df_ <- df_site_colour %>%
      left_join(df_stations)%>%
      left_join(df_site_metric) 
    
    
    
    
    df_ %>%
      
      filter(is.na(airzone))
    
    colnames(df_airzone)
    colnames(df_site_colour)
    
    # View(df_tbl_complete)
    
    
    #prepare colour and text tables
    
    df_tbl_complete_colourtxt <- df_ %>%
      
      bind_rows(df_airzone) %>% ungroup() %>%
      # filter(year == yr_) %>%
      filter(!is.na(colour_text)) %>%
      mutate(parameter = factor(parameter, levels = df_param_list$parameter)) %>%
      arrange(parameter,year,airzone,site) %>%
      filter(tfee == TRUE) %>%
      select(parameter,year,site,colour_text,airzone,metric_display) %>%
      pad_df(static_columns = c('site','year','airzone'),filled_columns = c('parameter')) %>%
      mutate(colour_text = ifelse(is.na(colour_text),'-',colour_text)) %>%
      mutate(display = ifelse(grepl('!',site),toupper(colour_text),metric_display)) %>% 
      select(site,year,parameter,airzone,display) %>%
      pivot_wider(names_from = parameter, values_from = display) %>%
      mutate(PM25 = ifelse(is.na(PM25),'No Valid Data',PM25),
             O3 = ifelse(is.na(O3),'No Valid Data',O3),
             NO2 = ifelse(is.na(NO2),'No Valid Data',NO2),
             SO2 = ifelse(is.na(SO2),'No Valid Data',SO2))
    
    
    df_tbl_complete_colour <- df_ %>%
      bind_rows(df_airzone) %>% ungroup() %>%
      # filter(year == yr_) %>%
      filter(!is.na(colour_text)) %>%
      mutate(parameter = factor(parameter, levels = df_param_list$parameter)) %>%
      arrange(parameter,year,airzone,site) %>%
      filter(tfee == TRUE) %>%
      select(parameter,year,site,colour,airzone) %>%
      pad_df(static_columns = c('site','year','airzone'),filled_columns = c('parameter')) %>%
      mutate(colour = ifelse(is.na(colour),'#DBDBDB',colour)) %>%
      pivot_wider(names_from = parameter, values_from = colour)
    
    df_tbl_complete_export <- df_ %>%
      bind_rows(df_airzone) %>% ungroup() %>%
      # filter(year == yr_) %>%
      filter(!is.na(colour_text)) %>%
      mutate(parameter = factor(parameter, levels = df_param_list$parameter)) %>%
      arrange(parameter,year,airzone,site) %>%
      filter(tfee == TRUE) %>%
      select(parameter,year,site,colour,airzone,colour_text,metric_display) %>%
      pad_df(static_columns = c('site','year','airzone'),filled_columns = c('parameter')) %>%
      mutate(colour = ifelse(is.na(colour),'#DBDBDB',colour)) %>%
      pivot_wider(names_from = parameter, values_from = c('colour_text','metric_display')) %>%
      mutate(site = gsub('! ','',site)) %>%
      mutate(site = gsub('<br>','',site))
    
    
    
    for (yr_ in min(df_site_colour$year,na.rm = TRUE): max(df_site_colour$year,na.rm = TRUE)) {
      if (0) {
        yr_ <- 2017
      }
      
      #switch site to label, to form friend name
      df_sites_name <- df_sites %>%
        arrange(desc(label)) %>%
        select(site,label) %>% distinct() %>%
        mutate(sitenew = ifelse(is.na(label),site,label)) %>%
        group_by(site) %>%
        slice(1) %>%
        ungroup() %>%
        select(site,sitenew)
      
      df_tbl_complete_colour <- df_tbl_complete_colour %>%
        left_join(df_sites_name) %>%
        mutate(site = ifelse(is.na(sitenew),site,sitenew)) %>%
        select(-sitenew)
      
      df_tbl_complete_colourtxt <-  df_tbl_complete_colourtxt %>%
        left_join(df_sites_name) %>%
        mutate(site = ifelse(is.na(sitenew),site,sitenew)) %>%
        select(-sitenew)
      
      print(paste('Creating management levels for year:',yr_))
      df_tbl_complete_colour_ <- df_tbl_complete_colour %>%
        filter(year == yr_)%>%
        select(-year) %>%
        arrange(airzone,site)
      
      lst_airzonesite <- which(grepl('!',df_tbl_complete_colour_$site))
      #find locations of airzone within the table
      #thisis to make it clear where those are located
      
      
      
      a <- df_tbl_complete_colourtxt %>%
        filter(year == yr_) %>%
        select(-year) %>%
        arrange(airzone,site) %>%
        mutate(site = gsub("!",'<b>',site)) %>%
        select(-airzone) 
      
      colnames(a) <- c('Site',
                       'PM<sub>2.5</sub>, µg/m<sup>3</sup><br>(annual/24-hr)',
                       'O<sub>3</sub>, ppb<br>(8-hour)',
                       'NO<sub>2</sub>, ppb<br>(annual/1-hour)',
                       'SO<sub>2</sub>, ppb<br>(annual/1-hour)')
      p <- a %>%
        kbl('html', escape = F,
            caption = paste('Reporting Period: ',(yr_-2),'-',yr_,sep='')) %>%
        kable_styling("bordered",position = 'center') %>%
        kableExtra::column_spec(column=1,width="15em",color = 'black') %>%
        kableExtra::column_spec(column=2:ncol(a),width="30em",color = 'black') %>%
        kableExtra::row_spec(row=0,color = 'white',background = 'black',align = 'c') %>% 
        kableExtra::row_spec(row=1:nrow(df_tbl_complete_colour_),align = 'r') %>% 
        kableExtra::row_spec(row=c(lst_airzonesite),align='l',
                             background = 'lightblue') %>%
        # kableExtra::row_spec(row=1:nrow(df_tbl_complete_colourtxt),color = 'black',align = 'c') 
        kableExtra::column_spec(column = 2,
                                background = df_tbl_complete_colour_$PM25,
                                
                                color = 'black') %>%
        kableExtra::column_spec(column = 3,
                                background = df_tbl_complete_colour_$O3,
                                
                                color = 'black') %>%
        kableExtra::column_spec(column = 4,
                                background = df_tbl_complete_colour_$NO2,
                                
                                color = 'black') %>%
        kableExtra::column_spec(column = 5,
                                background = df_tbl_complete_colour_$SO2,
                                
                                color = 'black') %>%
        kableExtra::row_spec(row=c(lst_airzonesite),
                             extra_css = "border-top: 3px solid black") 
      # kableExtra::row_spec(row = nrow(tbl_display),extra_css = "border: 2px solid black")
      result[[paste('management_',yr_,sep='')]] <- p
      
      
      #repeat for each airzone
      lst_airzones <- unique(df_tbl_complete_colourtxt$airzone)
      for (airzone_ in lst_airzones) {
        if (0) {
          airzone_ <- lst_airzones[1]
        }
        
        
        
        
        
        df_tbl_complete_colour_ <- df_tbl_complete_colour %>%
          filter(year == yr_)%>%
          filter(airzone == airzone_) %>%
          select(-year) %>%
          arrange(airzone,site)
        
        
        a <- df_tbl_complete_colourtxt %>%
          filter(airzone == airzone_) %>%
          filter(year == yr_) %>%
          select(-year) %>%
          arrange(airzone,site) %>%
          mutate(site = gsub("!",'<b>',site)) %>%
          select(-airzone) 
        
        
        
        
        colnames(a) <- c('Site',
                         'PM<sub>2.5</sub>, µg/m<sup>3</sup><br>(annual/24-hr)',
                         'O<sub>3</sub>, ppb<br>(8-hour)',
                         'NO<sub>2</sub>, ppb<br>(annual/1-hour)',
                         'SO<sub>2</sub>, ppb<br>(annual/1-hour)')
        p <-
          a %>%
          kbl('html', escape = F,
              caption = paste('Reporting Period: ',(yr_-2),'-',yr_,sep='')) %>%
          kable_styling("bordered",position = 'center') %>%
          
          kableExtra::column_spec(column=1,width="15em",color = 'black') %>%
          kableExtra::column_spec(column=2:ncol(a),width="30em",color = 'black') %>%
          kableExtra::row_spec(row=0,color = 'white',background = 'black',align = 'c') %>%
          kableExtra::row_spec(row=1:nrow(df_tbl_complete_colour_),align = 'r') %>%
          kableExtra::row_spec(row=1,align='l',
                               background = 'lightblue') %>%
          # kableExtra::row_spec(row=1:nrow(df_tbl_complete_colourtxt),color = 'black',align = 'c') 
          kableExtra::column_spec(column = 2,
                                  background = df_tbl_complete_colour_$PM25,
                                  
                                  color = 'black') %>%
          kableExtra::column_spec(column = 3,
                                  background = df_tbl_complete_colour_$O3,
                                  
                                  color = 'black') %>%
          kableExtra::column_spec(column = 4,
                                  background = df_tbl_complete_colour_$NO2,
                                  
                                  color = 'black') %>%
          kableExtra::column_spec(column = 5,
                                  background = df_tbl_complete_colour_$SO2,
                                  
                                  color = 'black') %>%
          kableExtra::row_spec(row=c(1),
                               extra_css = "border-top: 3px solid black") 
        # kableExtra::row_spec(row = nrow(tbl_display),extra_css = "border: 2px solid black")
        result[[paste('management_',airzone_,'_',yr_,sep='')]] <- p
      }
    }
  }
  
  result[['all stations']] <- df_tbl_complete_export
  
  return(result)
}


#these scripts generate the bar plots

#' Create a ranked bar graph (backend version)
#'
#' This is the back end of the plot_bar_ranked function
plot_bar_ranked0 <- function(df,metric,year,airzone = NULL,df_stations = NULL) {
  
  if (0) {
    df_caaqs_results <- readr::read_csv('./data/out/caaqs_results.csv')
    unique(df_caaqs_results$metric)
    metric <- c('pm25_annual')
    
    metric<- c('no2_ann')
    airzone <- 'Central Interior'
    airzone <- NULL
    df_stations = NULL
    year <- 2015
    
    df_caaqs_results <- df_caaqs_results
    metric <- 'pm25_annual'
    year <- 2021
    airzone <- NULL
    
    plot_bar_ranked0(df_caaqs_results = df_caaqs_results, metric = 'pm25_annual', year = 2021)
  }
  
  df_caaqs_results <- df
  
  #plotly version
  df_unit_plotly <- tribble(
    ~metric,~units,
    'pm25_annual',"Annual PM<sub>2.5</sub> Metric(&mu;g/m<sup>3</sup>)",
    'pm25_24h',"24-Hour PM<sub>2.5</sub> Metric(&mu;g/m<sup>3</sup>)",
    'o3_8h',"8-Hour O<sub>3</sub> Metric (ppb)",
    'no2_1hr',"1-Hour NO<sub>2</sub> Metric (ppb)",
    'no2_ann',"Annual NO<sub>2</sub> Metric (ppb)",
    'so2_1hr',"1-Hour SO<sub>2</sub> Metric (ppb)",
    'so2_ann',"Annual SO<sub>2</sub> Metric (ppb)"
  )
  
  #ggplot version
  df_unit <- tribble(
    ~metric,~units,
    'pm25_annual',bquote(~"Average "~PM[2.5]~","~mu~g/m^3),
    'pm25_24h',bquote(~"98th Percentile "~PM[2.5]~","~mu~g/m^3),
    'pm25_ann(1yr)',bquote("Annual "~PM[2.5]~","~mu~g/m^3),
    'pm25_24hr(1yr)',bquote(~"98th Percentile "~PM[2.5]~","~mu~g/m^3),
    'o3_8h',bquote("4th Highest"~O[3]~",ppb"),
    'o3_8h(1yr)',bquote("4th Highest"~O[3]~",ppb"),
    'no2_1hr',bquote("98th Percentile "~NO[2]~",ppb"),
    'no2_ann',bquote("Average "~NO[2]~",ppb"),
    'no2_1hr(1yr)',bquote("98th Percentile "~NO[2]~",ppb"),
    'no2_ann(1yr)',bquote("Average "~NO[2]~",ppb"),
    'so2_1hr',bquote("99th Percentile "~SO[2]~",ppb"),
    'so2_ann',bquote("Average "~SO[2]~",ppb"),
    'so2_1hr(1yr)',bquote("99th Percentile "~SO[2]~",ppb"),
    'so2_ann(1yr)',bquote("Average "~SO[2]~",ppb")
  )
  
  #define the CAAQS and the axis scale limits for display purposes
  #includes 2015 and 2020 CAAQS, based on the year
  if (year>=2020) {
    df_axis <- tribble(
      ~metric,~caaqs,~lbl_caaqs,~xmin,~xlab,
      'pm25_annual',8.8,'2020 CAAQS',12,9.5,
      'pm25_24h',27,'2020 CAAQS',30,29,
      'o3_8h',62,'2020 CAAQS',70,64,
      'no2_1hr',60,'2020 CAAQS',70,62,
      'no2_ann',17,'2020 CAAQS',20,19,
      'so2_1hr',70,'2020 CAAQS',80,72,
      'so2_ann',5,'2020 CAAQS',10,6
    )
  } else {
    df_axis <- tribble(
      ~metric,~caaqs,~lbl_caaqs,~xmin,~xlab,
      'pm25_annual',10,'2015 CAAQS',12,9.5,
      'pm25_24h',28,'2015 CAAQS',30,29,
      'o3_8h',63,'2015 CAAQS',70,64,
      'no2_1hr',60,'2020 CAAQS',70,62,
      'no2_ann',17,'2020 CAAQS',20,19,
      'so2_1hr',70,'2020 CAAQS',80,72,
      'so2_ann',5,'2020 CAAQS',10,6
    )
  }
  
  
  
  
  
  
  
  #rename instruments to simplified versions
  #this helps simplify display
  
  
  
  #redefined filtering variables to avoid confusion with column names
  airzone_filter <- airzone
  metric_filter <- metric
  year_filter <- year
  
  if (is.null(df_stations)) {
    df_stations <- envair::listBC_stations(use_CAAQS = TRUE, merge_Stations = TRUE)%>%
      dplyr::rename(label = Label,
                    latitude  = LAT,
                    longitude = LONG,
                    airzone = AIRZONE) %>%
      select(site,label,airzone,latitude,longitude) %>%
      group_by(site) %>%
      slice(1) %>% ungroup() %>%
      filter(!is.na(airzone))
  } else {
    df_stations <- df_stations%>%
      dplyr::rename(label = Label,
                    latitude  = LAT,
                    longitude = LONG,
                    airzone = AIRZONE) %>%
      select(site,label,airzone,latitude,longitude) %>%
      group_by(site) %>%
      slice(1) %>% ungroup() %>%
      filter(!is.na(airzone))
  }
  
  
  
  if (is.null(airzone_filter)) {
    airzone_filter <- unique(df_stations$airzone)
  }
  
  #these are to define FEM and non-FEM analyzers
  #will only display them when there are multiple PM instruments
  df_instrument_rename <- tibble(
    instrument = c('PM25 SHARP5030','PM25_SHARP5030i','SHARP','BAM1020/SHARP',
                   'PM25_T640','TEOM/SHARP','BAM1020/TEOM','PM25_R&P_TEOM','BAM1020'),
    instrument_new = c('FEM','FEM','FEM','FEM','FEM','FEM/non-FEM','FEM/non-FEM','non-FEM','FEM')
  )
  #prepare data for plotting
  #add meta-data from station list and the labels
  #note that it is possible that there are stations without "Labels" value
  df <- df_caaqs_results %>%
    left_join(df_stations,by='site') %>%
    left_join(df_unit_plotly,by='metric') %>%
    left_join(df_axis,by='metric') %>%
    mutate(label = ifelse(is.na(label),site,label)) %>%
    left_join(df_instrument_rename) %>%
    mutate(instrument = ifelse(is.na(instrument_new),
                               instrument,instrument_new))
  
  #fix for stations with multiple instruments
  #add the instrument name to the site name and call it label
  df <-
    ungroup(df) %>% 
    filter(!is.na(metric_value)) %>%
    group_by(parameter,metric,label,year,tfee) %>%
    dplyr::mutate(count =n()) %>%
    ungroup() %>%
    
    dplyr::mutate(label = ifelse((count >1 & parameter == 'PM25'),
                                 paste(label,'<br>On ',instrument,' Analyzer',sep=''),
                                 label)) %>%
    # View()
    select(-count)
  
  
  
  
  df <- df %>%
    filter(year %in% year_filter) %>%
    filter(metric %in% metric_filter) %>%
    filter(tolower(airzone) %in% tolower(airzone_filter))
  
  units <- unique(df$units)[[1]]
  
  units_suffix <-  gsub("[\\(\\)]", "", regmatches(units, gregexpr("\\(.*?\\)", units))[[1]])
  units_prefix <- gsub("\\(.*?\\)", "", units)
  xmax <- round(max(df$metric_value*1.1,df$xmin,na.rm = TRUE))
  caaqs <- unique(df$caaqs)[1]
  caaqs_label <- unique(df$lbl_caaqs)[1]
  xlab <- unique(df$xlab)[1]
  
  
  #identify scale limits
  
  
  #set order of site
  lvls_site <- df %>%
    filter(!tfee) %>%
    arrange(metric_value) %>%
    filter(!is.na(metric_value)) %>%
    pull(label) %>%
    unique()
  
  
  
  if (any(df$tfee)) {
    
    #there is TFEE to plot
    
    df <-
      
      ungroup(df) %>%
      mutate(label=factor(label,levels=lvls_site)) %>%
      filter(!is.na(metric_value)) %>%
      mutate(`Data Adjustment (TFEE)` = ifelse(tfee,
                                               'Wildfire-adjusted\n(wildfire data removed)',
                                               'No Adjustment\n(wildfire data included)')) %>%
      mutate(`Data Adjustment (TFEE)` = factor(`Data Adjustment (TFEE)`,levels = c(
        'No Adjustment\n(wildfire data included)',
        'Wildfire-adjusted\n(wildfire data removed)'
      ))) %>%
      group_by(parameter,site,instrument,year,metric) %>%
      arrange(desc(metric_value)) %>%
      mutate(metric_tfee = min(metric_value,na.rm = TRUE),
             metric_notfee = max(metric_value,na.rm = TRUE),
             count = n()) %>%
      ungroup() %>% 
      mutate(tooltip_ = ifelse((metric_tfee == metric_notfee | count ==1),
                               paste('<i>',units_prefix,'</i><br>   ',
                                     metric_value,' ',units_suffix,sep=''),  #no wildfire adjustment 
                               
                               
                               paste('<i>',units_prefix,'</i><br>',
                                     '   Including wildfire:    ',metric_notfee,' ',units_suffix,'<br>',
                                     '   Wildfire data removed: ',metric_tfee,' ',units_suffix,
                                     
                                     sep='')  #there is wildfireand non-wildfire adjusted data
      )) %>%
      mutate(tooltiptext = paste('<b>',toupper(label),' (',year,')','</b><br>',tooltip_,'<br><br><i>',
                                 caaqs_label,'</i><br>       ',
                                 caaqs,' ',units_suffix,
                                 
                                 sep='')) %>% select(-tooltip_)
    
    
    
    p <- 
      ggplotly(
        df %>% 
          ungroup() %>%
          
          # View()
          ggplot2::ggplot(aes(x=label, y=metric_value,fill = 
                                `Data Adjustment (TFEE)`,
                              text = tooltiptext)) +#fill = `Data Adjustment (TFEE)`)) +
          geom_col(position='identity',colour = 'black',width = 0.8) +
          coord_flip() +
          ylab(units) +
          scale_y_continuous(expand = c(0,0),limits = c(0,xmax)) +
          #add CAAQS line and text
          geom_hline(yintercept = caaqs, colour = 'red', linetype = 'dashed')+
          
          theme(panel.background = element_blank(),
                panel.border = element_rect(fill=NA, colour='black'),
                axis.title.y = element_blank(),
                legend.position = c(0.9,0.1)) +
          # annotate("text",x=nrow(df%>%select(site)%>%distinct())/4, y=xlab,
          #          label = caaqs_label, angle = '90', colour = 'red') +
          scale_fill_manual(values = c('slategray3','cornflowerblue')),
        tooltip = c('text')
      )%>%
      layout(legend = list(
        orientation = "h",
        title = ''
      ),
      hoverlabel = list(align = 'left',bgcolor = 'yellow')
      )
    
    
    
    
  } else {
    #no TFEE to plot
    if (0) {
      df_ <- df
      df <- df_
    }
    
    df <-  ungroup(df) %>%
      mutate(label=factor(label,levels=lvls_site)) %>%
      filter(!is.na(metric_value)) %>%
      mutate(tooltiptext = paste('<b>',toupper(label),' (',year,')','</b><br>',
                                 '<i>',units_prefix,'</i><br>      ',
                                 metric_value,' ',units_suffix,'<br><br>',
                                 caaqs_label,'<br>       ',
                                 caaqs,' ',units_suffix,
                                 
                                 sep='' ))
    
    
    p <- ggplotly( 
      df %>% 
        ungroup() %>%
        ggplot2::ggplot(aes(x=label, y=metric_value, text = tooltiptext)) +
        geom_col(position='dodge',colour = 'black',fill='cornflowerblue',width = 0.8) +
        coord_flip() +
        ylab(units) +
        scale_y_continuous(expand = c(0,0),limits = c(0,xmax)) +
        #add CAAQS line and text
        geom_hline(yintercept = caaqs, colour = 'red', linetype = 'dashed') +
        # annotate("text",x=nrow(df%>%select(site)%>%distinct())/4, y=xlab,
        #          label = caaqs_label, angle = 90, colour = 'red') +
        # ylab(expression(PM[2.5])) +
        theme(panel.background = element_blank(),
              panel.border = element_rect(fill=NA, colour='black'),
              axis.title.y = element_blank(),
              legend.position = 'none') +
        scale_fill_manual(values = c('cornflowerblue')),
      tooltip = c('text')) %>%
      layout(legend = list(
        orientation = "h",
        title = ''
      ),
      hoverlabel = list(align = 'left',bgcolor = 'yellow')
      )
    
  }
  
  return(p)
  
}


#' Create bar graph of the metrics
#' 
#' @param pollutant is the pollutant
#' @param metric is the pollutant metric. use definition in caaqs_results.csv
#' @param year is the datayear
#' @param df is the dataframe containing summary data. If NULL, it retrieved from github
#' @param airzone is the airzzone. If null, it displays all sites
plot_bar_caaqs <- function(metric, year, df = NULL,airzone = NULL) {
  
  if (0) {
    metric <- 'test'
    metric <- 'pm25_annual'
    airzone <- NULL
    df <- NULL
    year <- 2021
  }
  
  library(readr)
  library(dplyr)
  library(envair)
  
  
  url_data <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/caaqs_results.csv'
  url_stations <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/liststations.csv'
  
  
  lst_stations <- read_csv(url(url_stations))
  if (is.null(df)) {
    df <- read_csv(url(url_data))
    
  } 
  
  
  
  metric <- tolower(metric)
  df$metric <- tolower(df$metric)
  
  if (!metric %in% df$metric) {
    stop('metric is not registered. Please check in caaqs_results.csv')
  }
  if (!year %in% df$year) {
    stop('"year" is not registered. Please check in caaqs_results.csv')
  }
  
  if (tolower(airzone) == 'all') {
    airzone <- NULL
  }
  #pre-assign
  df_ <- df
  metric_ <- metric
  airzone_ <- airzone
  year_ <- year
  p <- plot_bar_ranked0(df =  df_,year = year_,
                        metric = metric_, airzone = airzone_,df_stations = lst_stations)
  
  return(p)
}
#end of functions----------

#' This creates a list containing all the bar graphs for all years and air zones
#' 
#' This is pre-requisite for shiny
#' 
#' Should save output to ./data/out/bargraph.Rds
plot_bar_caaqs_complete <- function() {
  
  url_data <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/caaqs_results.csv'
  url_stations <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/liststations.csv'
  
  
  lst_stations <- read_csv(url(url_stations))
  # if (is.null(df)) {
  df <- read_csv(url(url_data))
  
  # } 
  
  lst_metric <- df %>%
    select(metric) %>%
    distinct() %>%
    filter(!grepl('(1yr)',metric)) %>%
    pull(metric)
  
  lst_airzone <- unique(lst_stations$AIRZONE)
  lst_airzone <- lst_airzone[!is.na(lst_airzone)]
  lst_airzone <- c('ALL',lst_airzone)
  
  lst_yrs <- unique(df$year)
  
  
  result <- list()
  
  
  for (metric_ in lst_metric) {
    
    for (airzone_ in lst_airzone) {
      print(airzone_)
      for (yr_ in lst_yrs) {
        listname <- paste(metric_,airzone_,yr_,sep='_')
        print(listname)
        try(
          result[[listname]] <- plot_bar_caaqs(metric = metric_,year = yr_,df,airzone = airzone_)
        )
      }
    }
  }
  
  result[['data']] <- df
  return(result)
}


#' CREATE TRENDS of air quality in BC
#' 
#' @param dirs_location is the location of the data files
get_trends <- function(dirs_location = './data/out',reporting_year=NULL,airzone_filter = 'BC') {
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(plotly)
  
  # dirs_location <- './data/out'  #local location, two dots for final, one dot for debug
  if (0) {
    dirs_location <- './data/out'
    reporting_year <- 2021
    airzone_filter <- 'Central Interior'
  }
  
  
  list.files(dirs_location)
  
  df_data_trends_caaqs <- readr::read_csv(paste(dirs_location,'caaqs_results.csv',sep='/')) 
  df_data_trends_annual <- readr::read_csv(paste(dirs_location,'annual_results.csv',sep='/')) %>%
    filter(!is.na(value),value>-10)
  
  if (is.null(reporting_year)) {
    reporting_year <- max(df_data_trends_caaqs$year)
  }
  
  maxyear <- reporting_year
  
  df_stations <- readr::read_csv(paste(dirs_location,'liststations.csv',sep='/')) %>%
    mutate(AQMS = ifelse(is.na(AQMS),'N/A',AQMS)) %>%
    filter(AQMS != 'N') %>%
    filter(site %in% df_data_trends_annual$site)
  
  colnames(df_stations)
  
  df_plot_metric <- df_data_trends_annual%>%
    select(metric,parameter) %>%
    distinct()
  
  #rolling 3-year average needed for
  #pm25 annual
  #pm25 24-hour
  #o3_8hr
  #no2 1-hour
  #so2 1-hour
  
  #calculate 3-year running average
  df_data_trends_annual_3yr <- df_data_trends_annual %>%
    mutate(index = 1:n())
  df_data_trends_annual_3yr_ <- NULL
  for (i in 0:2) {
    df_data_trends_annual_3yr_ <- df_data_trends_annual_3yr_ %>%
      bind_rows(
        df_data_trends_annual_3yr %>%
          mutate(year=year+i)
        
        
      )
  }
  
  #those with metric "MEAN_1HR" are averaged over 1-year
  #all the rest are averaged over 3 years
  df_data_trends_annual_3yr <- df_data_trends_annual_3yr_ %>%
    mutate(valid_count = ifelse(is.na(value),0,1)) %>%
    group_by(parameter,site,instrument,tfee,year,metric) %>%
    dplyr::mutate(value_3yr = sum(value,na.rm = TRUE),valid_n =sum(valid_count)) %>%
    filter(valid_n>=2) %>%
    ungroup() %>%
    mutate(value = ifelse(grepl('MEAN_1HR',metric,ignore.case = TRUE),value,value_3yr/valid_n)) %>%
    select(-value_3yr,-valid_count) %>%
    filter(year<=maxyear)
  
  #bug found, 2023-06-21
  #add 
  if (0) {
    unique(df_data_trends_annual_3yr$parameter)
    unique(df_data_trends_annual_3yr_$parameter)
    unique(df_data_trends_annual_3yr_$metric)
  }
  #summarize for air zone plot
  df_data_trends_annual_airzone <- df_data_trends_annual_3yr %>%
    left_join(df_stations %>%
                select(site,AIRZONE)) %>%
    filter(!is.na(AIRZONE)) %>%
    group_by(parameter,tfee,year,metric,AIRZONE) %>%
    dplyr::summarise(value_avg = mean(value,na.rm = TRUE),
                     value_min = min(value,na.rm = TRUE),
                     value_max = max(value,na.rm = TRUE))
  
  df_data_trends_annual_overall <- df_data_trends_annual_3yr %>%
    left_join(df_stations %>%
                select(site,AIRZONE)) %>%
    filter(!is.na(AIRZONE)) %>%
    group_by(parameter,tfee,year,metric) %>%
    dplyr::summarise(value_avg = mean(value,na.rm = TRUE),
                     value_min = min(value,na.rm = TRUE),
                     value_max = max(value,na.rm = TRUE)) %>%
    mutate(AIRZONE = "BC")
  
  df_data_trends_annual_airzone <- df_data_trends_annual_airzone %>%
    bind_rows(df_data_trends_annual_overall) %>%
    filter(!tfee) %>%
    filter(grepl('RAW',metric,ignore.case = TRUE))
  
  #create reference years
  #this shows how many percent increase or decrease in value
  # result_table <- 
  df_BC_summary_ref<- df_data_trends_annual_airzone %>%
    filter(year %in% c(1990,2000,2010,maxyear)) %>%
    select(parameter,year,metric,AIRZONE,value_avg) %>% 
    tidyr::pivot_wider(names_from = year, values_from = value_avg) %>%
    mutate(perc_2000 = envair::round2((`2021`-`2000`)/`2000`*100),
           perc_1990 = envair::round2((`2021`-`1990`)/`1990`*100))
  
  #plot
  lst_parameters <- df_data_trends_annual_airzone %>%
    filter(!tfee) %>%
    filter(AIRZONE == airzone_filter) %>%
    filter(year >=1990) %>%
    mutate(parameter_label = paste(parameter,metric)) %>%
    pull(parameter_label) %>%
    unique()
  # paste(lst_parameters,collapse=',')
  #define parameters for recoding
  df_parameters <- tribble(
    ~parameter_label,~label,~CAAQS,~order,
    'NO2 RAW_ANNUAL_98P_D1HM','NO2 (1-Hour)',60,5,
    'NO2 RAW_ANNUAL_MEAN_1HR','NO2 (Annual)',17,4,
    'O3 RAW_ANNUAL_4TH_D8HM','O3 (8-Hour)',62,3,
    'PM25 RAW_ANNUAL_98P_24h','PM2.5 (24-Hour)',27,2,
    'PM25 RAW_ANNUAL_MEAN_24h','PM2.5 (Annual)',8.8,1,
    'SO2 RAW_ANNUAL_99P_D1HM','SO2 (1-Hour)',70,7,
    'SO2 RAW_ANNUAL_MEAN_1HR','SO2 (Annual)',5,6
  )
  
  a <- df_data_trends_annual_airzone %>%
    filter(!tfee) %>%
    filter(AIRZONE == airzone_filter) %>%
    filter(year >=1990) %>%
    mutate(parameter_label = paste(parameter,metric)) %>%
    left_join(df_parameters) %>%
    mutate(percentAbove = (value_avg - CAAQS)/CAAQS *100) %>%
    ungroup() 
  
  result_ggplot <- a %>%
    ggplot(aes(x=year,y=percentAbove,colour = reorder(label,order))) +
    geom_line() +
    geom_hline(yintercept = 0, colour='red',linetype = 'dashed') +
    annotate("text",x=2010, y=10,label = 'Current CAAQS') +
    theme(legend.position = 'bottom', legend.title = element_blank(),
          legend.key = element_blank(),
          panel.background = element_rect(fill=NA,colour = 'black'),
          axis.title.x = element_blank()) +
    ylab('Percent Above/Below Current CAAQS')
  
  result_plotly <- a %>%
    mutate(percentAbove = envair::round2(percentAbove,n=1)) %>%
    mutate(hovertext = paste(percentAbove,'%',sep='')) %>%
    mutate(label = gsub('PM2.5','PM<sub>2.5</sub>',label)) %>%
    mutate(label = gsub('O3','O<sub>3</sub>',label)) %>%
    mutate(label = gsub('NO2','NO<sub>2</sub>',label)) %>%
    mutate(label = gsub('SO2','SO<sub>2</sub>',label)) %>%
    
    plotly::plot_ly(x=~year,y=~percentAbove,color =~reorder(label,order),
                    type='scatter',mode='lines+markers',showlegend =T,
                    hoverinfo ='y',
                    hovertemplate = paste('%{y:.1f}','%',sep='')
                    
    ) %>%
    layout(title = 'Trends in Pollutant Levels',
           legend = list(orientation = 'h'),
           yaxis = list(title = 'Percent Above/Below CAAQS'),
           xaxis = list(title = 'Annual Reporting Period')
    ) %>%
    plotly::layout(hovermode = 'x unified')
  
  return(list(table = df_BC_summary_ref,ggplot = result_ggplot,plotly = result_plotly,data = a))
}


#' Adds an up and down arrow
#' 
#' #' if value is negative, it will show a downward arrod
#' in a css format
#' 
#' @param value is a number that is either positve or negative
#' 
#' @returns css text with up or down arrow depending on the polarity of the number
add_arrow <- function(value) {
  
  
  if(value>0) {
    result <- paste('<span style="color:red">↑</span>',abs(value),sep='')  
  } else {
    result <-  paste('<span style="color:blue">↓</span>',abs(value),sep='') 
  }
  return(result)
}

#' List of metric and parameters
#'
#' @description This contains a list of metrics and the parameters these are related to.
#' Note that there are two types of parameters listed, these are created to ensure coverage for all
#' envair and rcaaqs have different list of metrics
#' parameter is from rcaaqs
#' metric is from envair
#'
#' @export
#'
df_metric_list <- function() {
  #define levels to put metrics and parameters in order
  levels_parameter <- c('pm2.5_annual','pm2.5_24h','o3','no2_1yr','no2_3yr','so2_1yr','so2_3yr')
  levels_metric <- c('pm25_annual','pm25_24h','o3_8h','no2_ann','no2_1hr','so2_ann','so2_1hr')
  df_result <- tribble(
    ~pollutant,~parameter,~metric,
    'PM25','pm2.5_annual','pm25_annual',
    'O3','o3','o3_8h',
    'PM25','pm2.5_24h','pm25_24h',
    'NO2','no2_1yr','no2_ann',
    'NO2','no2_3yr','no2_1hr',
    'SO2','so2_1yr','so2_ann',
    'SO2','so2_3yr','so2_1hr'
  )
  
  df_result$parameter <- factor(df_result$parameter, levels = levels_parameter)
  df_result$metric <- factor(df_result$metric, levels = levels_metric)
  
  return(df_result)
}

add_caaqs_historic <- function(g, metric) {
  hist_caaqs <- tribble(
    ~parameter,     ~labels,                                            ~lower_breaks, ~upper_breaks,
    "o3",           "Actions for Keeping Clean Areas Clean",            0,             50,
    "o3",           "Actions for Preventing Air Quality Deterioration", 50,            56,
    "o3",           "Actions for Preventing CAAQS Exceedance",          56,            63,
    "o3",           "Actions for Achieving Air Zone CAAQS",             63,            Inf,
    
    "pm2.5_annual", "Actions for Keeping Clean Areas Clean",            0,             4.0,
    "pm2.5_annual", "Actions for Preventing Air Quality Deterioration", 4.0,           6.4,
    "pm2.5_annual", "Actions for Preventing CAAQS Exceedance",          6.4,           10.0,
    "pm2.5_annual", "Actions for Achieving Air Zone CAAQS",             10.0,          Inf,
    
    "pm2.5_24h",    "Actions for Keeping Clean Areas Clean",            0,             10,
    "pm2.5_24h",    "Actions for Preventing Air Quality Deterioration", 10,            19,
    "pm2.5_24h",    "Actions for Preventing CAAQS Exceedance",          19,            28,
    "pm2.5_24h",    "Actions for Achieving Air Zone CAAQS",             28,            Inf) %>%
    filter(parameter == .env$metric, !is.na(lower_breaks)) %>%
    left_join(select(rcaaqs::management_levels, labels, colour) %>% distinct(), 
              by = "labels")
  
  #' #added to customized legend order and labels
  mgmt_breaks <- c(
    'Insufficient Data', 
    'Actions for Achieving Air Zone CAAQS' ,
    'Actions for Preventing CAAQS Exceedance',
    'Actions for Preventing Air Quality Deterioration',
    'Actions for Keeping Clean Areas Clean',
    "No Adjustment",  
    "TF/EE Adjusted"
  )
  mgmt_labels <- c(
    'Insufficient Data', 
    'RED: Achieve CAAQS' ,
    'ORANGE: Prevent CAAQS Exceedance',
    'YELLOW: Prevent Air Quality Deterioration',
    'GREEN: Keep Clean Areas Clean',
    "No Adjustment (wildfire-influenced data)",  
    "TF/EE Adjusted (adjusted for wildfire)"
  )
  mgmt_values <- c('Insufficient Data' = '#dbdbdb',
                   'Actions for Preventing Air Quality Deterioration' = '#FEE08B',
                   'Actions for Keeping Clean Areas Clean' = '#A6D96A',
                   'Actions for Preventing CAAQS Exceedance' = '#F46D43',
                   'Actions for Achieving Air Zone CAAQS' = '#A50026',
                   "No Adjustment" = "#b4acb3", 
                   "TF/EE Adjusted" = "#8f94a6")
  
  
  years <- select(g$data, caaqs_year) %>%
    distinct() %>%
    nrow() 
  
  current_caaqs <- rcaaqs::management_levels %>%
    filter(parameter == .env$metric, !is.na(lower_breaks))
  
  ylim <- max(g$data$raw, na.rm = TRUE) * 1.1
  if(ylim < (max(hist_caaqs$lower_breaks, na.rm = TRUE) * 1.1)) {
    g <- g + 
      ggplot2::scale_y_continuous(
        expand = c(0,0), limits = c(NA, max(hist_caaqs$lower_breaks, na.rm = TRUE) * 1.1),
        breaks = scales::breaks_extended(n = 7))
  }
  
  g <- g + geom_rect(
    data = hist_caaqs, xmin = -Inf, xmax = years - 0.5, 
    aes(ymin = lower_breaks, ymax = upper_breaks, fill = labels), 
    inherit.aes = FALSE, alpha = 0.55)
  
  g <- g + 
    geom_rect(data = current_caaqs, xmin = years - 0.5, xmax = Inf,
              aes(ymin = lower_breaks, ymax = upper_breaks, fill = labels), 
              inherit.aes = FALSE, alpha = 0.55)
  
  g$layers <- list(g$layers[[2]], g$layers[[3]], g$layers[[1]])
  
  line_df <- data.frame(y = c(max(hist_caaqs$lower_breaks, na.rm = TRUE),
                              max(hist_caaqs$lower_breaks, na.rm = TRUE),
                              max(current_caaqs$lower_breaks, na.rm = TRUE),
                              max(current_caaqs$lower_breaks, na.rm = TRUE)),
                        x = c(0, years-0.5, years-0.5, Inf))
  
  g +
    geom_line(data = line_df, aes(x = x, y = y, colour = "CAAQS Achievement"), 
              inherit.aes = FALSE, linetype = "dashed", size = 1) +
    scale_fill_manual(breaks = mgmt_breaks,
                      labels = mgmt_labels,
                      values = mgmt_values) +
    scale_colour_manual(values = last(hist_caaqs$colour))
}
