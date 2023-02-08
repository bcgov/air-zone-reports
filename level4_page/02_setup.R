#define functions-------
#' Get a count of PM exceedances
#' 
#' 
get_PM_exceedancesummary <- function(dirs_location = './data/out') {
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(plotly)
  
  # dirs_location <- './data/out'  #local location, two dots for final, one dot for debug
  if (0) {
    dirs_location <- './data/out'
  }
  
  list.files(dirs_location)
  
  
  
  df_stations <- readRDS(paste(dirs_location,'liststations_merged.Rds',sep='/'))
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
  df_exceedances <- readRDS(paste(dirs_location,'exceedances.Rds',sep='/')) %>%
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
