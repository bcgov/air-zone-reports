library(DT)
library(shiny)
library(dplyr)
library(lubridate)
library(tidyverse)
library(leaflet)
library(patchwork)
library(ggpattern)
library(scales)
library(envreportutils)
library(plotly)
library(sf)
library(bcmaps)
library(readr)
library(ggplot2)
library(envair)
library(rcaaqs)

dirs_location <- 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/data/out'
if (0) {
  dirs_location <- './data/out'
  dirs_location <- '../data/out'  #local location, two dots for final, one dot for debug
  list.files(dirs_location)
  test <- read_csv('./data/out/annual_results.csv') %>%
    filter(parameter=='PM25')
}



#define functions-------

map_exceedance_dep <- function(map_a = NULL,exceedances,az_mgmt,year,size = c('200px','400px'),
                           airzone = NULL) {
  
  if (0) {
    source('./level4_page/03_setup.R')
    source('./level4_page/02_setup.R')
    dirs_location <- './data/out'
    year = 2017
    map_a <- NULL
    exceedances <- get_PM_exceedancesummary()
    
    az_mgmt <- readr::read_rds(paste(dirs_location,'az_mgmt.Rds',sep='/')) %>%
      left_join(df_colour)
    
    size <- c('200px','400px')
    
  }
  
  df_colour <- tribble(
    ~airzone,~colour_01,
    "Northeast",'#CDC08C',
    "Georgia Strait"  ,'#F4B5BD',
    "Southern Interior",'#9C964A',
    "Lower Fraser Valley",'#85D4E3',
    "Central Interior" ,'#FAD77B',
    "Coastal",'#CEAB07',
    "Northwest","#24281A"
  )
  
  airzone_select <- airzone
  
  lst_airzones <- az_mgmt %>%
    pull(airzone)
  
  airzone_exceedance_season <- exceedances$season
  airzone_exceedance <- exceedances$annual
  station_exceedance <- exceedances$annual_stations
  station_exceedance_season <- exceedances$season_stations
  lst_stations <- exceedances$stations
  year_select <- year
  
  
  colfunc <- colorRampPalette(c("blue", "red"))
  
  
  # station_exceedance$colorscaled <- color_scales[station_exceedance$days_exceed]
  
  if (is.null(map_a)) {
    #create map for annual station
    a <-  leaflet(width = size[1],height = size[2],
                  options = leafletOptions(attributionControl=FALSE, dragging = TRUE, minZoom = 4, maxZoom=10)) %>%
      set_bc_view(zoom=3.5) %>%
      # setView(zoom =5) %>%
      setMaxBounds(lng1 = -110,lat1=45,lng2=-137,lat2=62) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(opacity = 1)
      ) %>%
      # addProviderTiles(providers$Stamen.TonerLabels) %>%
      add_bc_home_button()
  } else {
    a <- map_a
  }
  
  #add colour for the station circles
  max_days <- station_exceedance %>%
    filter(year == year_select) %>%
    pull(days_exceed) %>%
    max()
  
  #develop levels for the exceedance legends
  color_scales <- colfunc(max_days)
  
  
  
  for (airzone_ in lst_airzones) {
    
    if (0) {
      airzone_ <- lst_airzones[1]
    }
    
    
    liststations_ <- lst_stations %>% 
      filter(AIRZONE == airzone_, year == year_select)
    
    station_exceedance_ <- station_exceedance %>% 
      filter(AIRZONE == airzone_, year == year_select)
    
    lst_sites <- station_exceedance %>%
      filter(AIRZONE == airzone_) %>%
      pull(site) %>% unique()
    a <- a %>%
      
      addPolygons(data = az_mgmt %>% filter(airzone == airzone_),
                  layerId = airzone_,
                  color = 'black',
                  fillColor = ~colour_01,
                  weight = 1, opacity = 1, fillOpacity = 0.6,
                  label = paste(airzone_,'Air Zone'),
                  labelOptions = labelOptions(textsize = "15px"),
                  highlight = highlightOptions(weight = 3,
                                               color = "blue",
                                               bringToFront = FALSE))
    
    
    #add stations
    if (nrow(liststations_) >0) {
      # message('trace inside map function')
      # message(nrow(liststations_))
      # message(year_select)
      
      
      
      
      a <- a %>%
        #remove all stations from that airzone
        removeMarker( layerId = lst_sites) %>%
        addCircleMarkers(lng=station_exceedance_$LONG,
                         lat = station_exceedance_$LAT,
                         layerId = station_exceedance_$site,
                         label = station_exceedance_$site,
                         color = color_scales[station_exceedance_$days_exceed+1],
                         radius=3
                         
        )
      # addMarkers(lng=liststations_$LONG,
      #            lat=liststations_$LAT,
      #            # layerId = liststations$AIRZONE,
      #            # group = airzone_,
      #            label = liststations_$site,
      #            options=markerOptions())
      
    }
  }
  
  if (is.null(map_a)) {
  #add legend based on color_scales
  tot_ <- length(color_scales)
  scl <- c(color_scales[5],color_scales[tot_/2],color_scales[tot_])
  lbl <- c('<5 days',paste(round(tot_/2),'days'),paste('>',round(tot_*0.8),' days',sep=''))
  a <- a %>%
    addLegend(position ="bottomleft", colors = scl,label = lbl)
  }
  #add for selected airzone
  if (!is.null(airzone_select)) {
    message(paste('updating,highlighting area',airzone_select))
    a <- a %>%
      addPolylines(data = az_mgmt %>% filter(airzone == airzone_select),
                   layerId = 'selectedairzone',
                   group = 'airzonehighlight',
                   color = 'blue',weight = 5)
  } else {
    a <- a %>%
      clearGroup('airzonehighlight')
  }
  plot_a <- a
  
  
  
  
  
  
  
  return(a)
}
map_exceedance(exceedances = exceedances, az_mgmt = az_mgmt, year = 2010)


#define functions-------

map_exceedance <- function(map_a = NULL,exceedances,az_mgmt,year,size = c('200px','400px'),
                           airzone = NULL) {
  
  if (0) {
    source('./level4_page/03_setup.R')
    source('./level4_page/02_setup.R')
    dirs_location <- './data/out'
    year = 2017
    map_a <- NULL
    exceedances <- get_PM_exceedancesummary()
    airzone = NULL
    az_mgmt <- readr::read_rds(paste(dirs_location,'az_mgmt.Rds',sep='/')) %>%
      left_join(df_colour)
    
    size <- c('200px','400px')
    
  }
  
  df_colour <- tribble(
    ~airzone,~colour_01,
    "Northeast",'#CDC08C',
    "Georgia Strait"  ,'#F4B5BD',
    "Southern Interior",'#9C964A',
    "Lower Fraser Valley",'#85D4E3',
    "Central Interior" ,'#FAD77B',
    "Coastal",'#CEAB07',
    "Northwest","#24281A"
  )
  
  airzone_select <- airzone
  
  lst_airzones <- az_mgmt %>%
    pull(airzone)
  
  airzone_exceedance_season <- exceedances$season
  airzone_exceedance <- exceedances$annual
  station_exceedance <- exceedances$annual_stations
  station_exceedance_season <- exceedances$season_stations
  lst_stations <- exceedances$stations
  year_select <- year
  
  
  colfunc <- colorRampPalette(c("blue", "red"))
  
  
  # station_exceedance$colorscaled <- color_scales[station_exceedance$days_exceed]
  
  if (is.null(map_a)) {
    #create map for annual station
    a <-  leaflet(width = size[1],height = size[2],
                  options = leafletOptions(attributionControl=FALSE, dragging = TRUE, minZoom = 4, maxZoom=10)) %>%
      set_bc_view(zoom=3.5) %>%
      # setView(zoom =5) %>%
      setMaxBounds(lng1 = -110,lat1=45,lng2=-137,lat2=62) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(opacity = 1)
      ) %>%
      # addProviderTiles(providers$Stamen.TonerLabels) %>%
      add_bc_home_button()
  } else {
    a <- map_a
  }
  
  #add colour for the station circles
  max_days <- station_exceedance %>%
    filter(year == year_select) %>%
    pull(days_exceed) %>%
    max()
  
  #develop levels for the exceedance legends
  color_scales <- colfunc(max_days)
  
  
  
  for (airzone_ in lst_airzones) {
    
    if (0) {
      airzone_ <- lst_airzones[1]
    }
    
    
    liststations_ <- lst_stations %>% 
      filter(AIRZONE == airzone_, year == year_select)
    
    station_exceedance_ <- station_exceedance %>% 
      filter(AIRZONE == airzone_, year == year_select)
    
    lst_sites <- station_exceedance %>%
      filter(AIRZONE == airzone_) %>%
      pull(site) %>% unique()
    a <- a %>%
      
      addPolygons(data = az_mgmt %>% filter(airzone == airzone_),
                  layerId = airzone_,
                  color = 'black',
                  fillColor = ~colour_01,
                  weight = 1, opacity = 1, fillOpacity = 0.6,
                  label = paste(airzone_,'Air Zone'),
                  labelOptions = labelOptions(textsize = "15px"),
                  highlight = highlightOptions(weight = 3,
                                               color = "blue",
                                               bringToFront = FALSE))
    
    
    #add stations
    if (nrow(liststations_) >0) {
      # message('trace inside map function')
      # message(nrow(liststations_))
      # message(year_select)
      
      
      
      
      a <- a %>%
        #remove all stations from that airzone
        removeMarker( layerId = lst_sites) %>%
        addCircleMarkers(lng=station_exceedance_$LONG,
                         lat = station_exceedance_$LAT,
                         layerId = station_exceedance_$site,
                         label = station_exceedance_$site,
                         color = color_scales[station_exceedance_$days_exceed+1],
                         radius=3
                         
        )
      # addMarkers(lng=liststations_$LONG,
      #            lat=liststations_$LAT,
      #            # layerId = liststations$AIRZONE,
      #            # group = airzone_,
      #            label = liststations_$site,
      #            options=markerOptions())
      
    }
  }
  
  if (is.null(map_a)) {
    #add legend based on color_scales
    tot_ <- length(color_scales)
    lbl <- c(round(tot_/10*1),
             round(tot_/10*2),
             round(tot_/10*3),
             round(tot_/10*4),
             round(tot_/10*5),
             round(tot_/10*6),
             round(tot_/10*7),
             round(tot_/10*8),
             round(tot_/10*9),
             round(tot_/10*10))
    scl <- c(color_scales[tot_/10*1],
             color_scales[tot_/10*2],
             color_scales[tot_/10*3],
             color_scales[tot_/10*4],
             color_scales[tot_/10*5],
             color_scales[tot_/10*6],
             color_scales[tot_/10*7],
             color_scales[tot_/10*8],
             color_scales[tot_/10*9],
             color_scales[tot_/10*10])
    
    
    # lbl <- c('<5 days',
    #          paste(round(tot_/2),'days'),
    #          paste('>',round(tot_*0.8),
    #                ' days',sep=''))
    a <-    
      a %>%
      addLegend(position ="bottomleft", colors = rev(scl),label = rev(lbl),
                title = 'Days')
    
    
    
    
  }
  #add for selected airzone
  if (!is.null(airzone_select)) {
    message(paste('updating,highlighting area',airzone_select))
    a <- a %>%
      addPolylines(data = az_mgmt %>% filter(airzone == airzone_select),
                   layerId = 'selectedairzone',
                   group = 'airzonehighlight',
                   color = 'blue',weight = 5)
  } else {
    a <- a %>%
      clearGroup('airzonehighlight')
  }
  plot_a <- a
  
  
  
  
  
  
  
  return(a)
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
  
  message('get PM exceedance')
  
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
  df_exceedances <- read_csv(paste(dirs_location,'exceedances.csv',sep='/')) %>%
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
#' Determine the air zone based on lat longs
#' 
#' @param lat is the latitude, vector OK
#' @param long is the longitude, vector OK
get_airzone <- function(lat,long) {
  
  if (0) {
    latlong <- c(57.68,-120.614)
  }
  
  az_mgmt_gitURL <- 'https://github.com/bcgov/air-zone-reports/blob/master/data/out/az_mgmt.Rds?raw=true'
  
  az_mgmt <- readRDS(url(az_mgmt_gitURL))
  
  
  
  
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
#' create graphs for days exceeding
#' 
#' @param exceedances is the result of get_PM_exceedance() function
graph_exceedance <- function(exceedances,AIRZONE = NULL,year = NULL) {
  if (0) {
    source('./level4_page/02_setup.R')
    dirs_location <- './data/out'
    AIRZONE <- NULL
    
  }
  
  require(tidyr)
  #set order for the seasons
  df_seasons <- tribble(
    ~seasons,~order,
    'Winter',4,
    'Spring',3,
    'Summer',2,
    'Fall',1
  )
  year_select <- year
  df_colour <- tribble(
    ~airzone,~colour_01,
    "Northeast",'#CDC08C',
    "Georgia Strait"  ,'#F4B5BD',
    "Southern Interior",'#9C964A',
    "Lower Fraser Valley",'#85D4E3',
    "Central Interior" ,'#FAD77B',
    "Coastal",'#CEAB07',
    "Northwest","#24281A"
  )
  
  list_airzones  <- df_colour$airzone
  
  # View(exceedances$season_stations)
  if (is.null(AIRZONE)) {
    AIRZONE <- 'BC'
  }
  df_annual <- exceedances$annual
  
  df_seasonal_tfee <- exceedances$season_tfee %>%
    mutate(tfee = TRUE)
  
  df_seasonal <- exceedances$season %>%
    mutate(tfee = FALSE) %>%
    bind_rows(df_seasonal_tfee) %>%
    pivot_wider(names_from = tfee, values_from =days_exceed) %>%
    dplyr::rename(`Wildfire Days` = `TRUE`,`Total` = `FALSE`) %>%
    mutate(`Wildfire Days` = ifelse(is.na(`Wildfire Days`),0,`Wildfire Days`))%>%
    mutate(`No Wildfire`= `Total` - `Wildfire Days`) %>%
    select(-Total) %>%
    pivot_longer(cols = c(`Wildfire Days`,`No Wildfire`)) %>%
    mutate(seasons = paste(seasons,'(',name,')',sep=''))
  
  
  
  
  
  
  
  
  
  vline <- function(x = 0, color = "red") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color,dash = 'dash')
      
    )
  }
  
  p_annual <-
    df_annual %>%
    filter(!AIRZONE %in% c('BC',NA)) %>% 
    filter(year>=1997) %>%
    plotly::plot_ly(x=~year,y=~days_exceed,color = ~AIRZONE,
                    type='scatter',mode='lines',showlegend =T,
                    hoverinfo ='y',
                    hovertemplate = paste('%{y:,.0f}',' days',sep='')
                    
    ) %>%
    layout(title = 'High PM<sub>2.5</sub> Levels in Air Zones',
           legend = list(orientation = 'h'),
           yaxis = list(title = 'Number of Days with High PM<sub>2.5</sub> Levels'),
           xaxis = list(title = 'Year')
    ) %>%
    plotly::layout(hovermode = 'x unified',
                   barmode = 'stack') %>%
    layout(shapes = list(vline(year_select)))
    # event_register("plotly_click")
  
  
  
  #create results for BC, and all air zones
  {
    p_list <- NULL
    i_list <- NULL
    i <- 0
    for (AIRZONE_ in c('BC',lst_airzones)) {
      i <- i +1
      
      p_seasonal <- df_seasonal %>%
        left_join(df_seasons,by = 'seasons') %>%
        filter(AIRZONE == AIRZONE_) %>%
        filter(year >=2000) %>%
        
        plotly::plot_ly(x=~year,y=~value,color = ~reorder(seasons,order),
                        type='bar',mode='bar',showlegend =T,
                        hoverinfo ='y',
                        hovertemplate = paste('%{y:,.0f}',' days',sep=''),
                        colors = c("navajowhite2",
                                   "navajowhite3",
                                   "seagreen3",
                                   "seagreen4",
                                   "red3",
                                   "red4",
                                   "slategray2",
                                   'slategray4'
                                   
                                   
                        )
                        
        ) %>%
        layout(title = paste('<br>High PM<sub>2.5</sub> Levels for',
                             ifelse(AIRZONE == 'BC',AIRZONE_,
                                    paste('the',AIRZONE_,'Air Zone')
                             )),
               
               legend = list(orientation = 'h'),
               yaxis = list(title = 'Number of Days with High PM<sub>2.5</sub> Levels'),
               xaxis = list(title = 'Year')
        ) %>%
        plotly::layout(hovermode = 'x unified',
                       barmode = 'stack',legend = list(x = 0.01, y = 0.9))%>%
        layout(shapes = list(vline(year_select)))
      
      
        # layout(annotations = list(x=2014,y=-1,yref='paper',xref='paper',text = 'wildfire counts only from 2014 onwards'))
      
      p_list[[i]] <- p_seasonal
      i_list <- c(i_list,AIRZONE_)
      
    }
    
    result <- list(plot_annual = p_annual,plot_seasonal = p_list,plot_definition = i_list, data = exceedances)
    }
  return(result)
  
}


#end of functions-----

#define colours-----

df_colour <- tribble(
  ~airzone,~colour_01,
  "Northeast",'#CDC08C',
  "Georgia Strait"  ,'#F4B5BD',
  "Southern Interior",'#9C964A',
  "Lower Fraser Valley",'#85D4E3',
  "Central Interior" ,'#FAD77B',
  "Coastal",'#CEAB07',
  "Northwest","#24281A"
)

lst_airzones <- df_colour$airzone

#define external data------
exceedances <- get_PM_exceedancesummary(dirs_location)
az_mgmt <- readr::read_rds(paste(dirs_location,'az_mgmt.Rds',sep='/')) %>%
  left_join(df_colour)
message('Load az_mgmt complete')
plots_list <- graph_exceedance(exceedances = exceedances,year =  max(exceedances$annual$year))
message('Load Complete')


#---SHINY SECTION----------------
# Actual shiny part
##ui section----
ui <- {
  fluidPage(
    h4(HTML('Fine Particulate Matter (PM<sub>2.5</sub>) Pollution')),
    tags$head(
      tags$style(HTML("
      body { background-color: #f2efe9; }
      .container-fluid { background-color: #fff; width: 1000px; padding: 5px; }
      .topimg { width: 0px; display: block; margin: 0px auto 0px auto; }
      .title { text-align: center; }
      .toprow { margin: 5px 0px; padding: 5px; background-color: #38598a; }
      .filters { margin: 0px auto; }
      .shiny-input-container { width:100% !important; }
      .table { padding: 0px; margin-top: 0px; }
      .leaflet-top { z-index:999 !important; }
      "))
    ),
    # h1("Fine Particulate Pollution", class = "title"),
    fluidRow(class = "toprow",
             fluidRow(class = 'filters',
                      column(6,
                             tags$style(type='text/css', 
                                        '.selectize-input { font-size: 15px; line-height: 10px;} 
                          .selectize-dropdown { font-size: 16px; line-height: 20px; }
                          .control-label {font-size: 24px; color: white !important;}
                          .irs-min {font-size: 0px; color: white; !important}
                          .irs-max {font-size: 0px; color: white;}
                          .irs-single {font-size: 20px; color: white;}
                          .irs-grid-text {font-size: 10px; color: white;}'
                             ),
                             sliderInput('year_slider',label ='Year',
                                         min = 1997,
                                         max = max(exceedances$annual$year),
                                         value = max(exceedances$annual$year),
                                         sep='')
                      ))),
    
    fluidRow(
      
      column(4,h6(HTML("Frequency (in days) of elevated PM<sub>2.5</sub> levels in B.C. air zones.</br>Click the map to select an air zone")),
             # fluidRow(
             leaflet::leafletOutput("map",height = '400px',width = '400px')),
      column(8,h6(HTML("Click on the graph to view details of days with high PM<sub>2.5</sub> Levels.</br>
                       Wildfire counts before 2014 are not included.")),
             # div(style='height:400px;overflow-y: scroll;'),
             plotlyOutput("plot1",height = '400px',width = '600px')
             )
    )
    
  )
}

##server section----
server <- {shinyServer(function(input, output) {
  
  if (0) {
    map_exceedance(exceedances = exceedances, az_mgmt = az_mgmt, year = 2010)
  }
  
  #reactive_plot1 can carry over plots across events
  reactive_plot1 <- reactiveVal(plots_list)
  
  
  a <-    map_exceedance(map_a = NULL,exceedances = exceedances ,az_mgmt = az_mgmt,year = max(exceedances$annual$year))
  
  output$map <- renderLeaflet(a)
  
  
  output$plot1 <- renderPlotly({plot_out <- plots_list$plot_annual
  plot_out %>% ggplotly(source = 'plot1') 
  #%>% event_register("plotly_click")
  })
  
  
  
  observeEvent(input$year_slider,
               {
                 message('Slider')
                 message(input$year_slider)
                 
                 leafletProxy("map") %>%
                   map_exceedance(exceedances = exceedances ,az_mgmt = az_mgmt,year =input$year_slider)
                 
                 
                 plots_list <- graph_exceedance(exceedances = exceedances,year = input$year_slider)
                 reactive_plot1(plots_list)   #pass on value to reactive_plot1
                 # output$plot1 <- renderPlotly(plots_list$plot_annual)
                 
                 output$plot1 <- renderPlotly({
                   plot_out <- plots_list$plot_annual
                 p <- plot_out %>% ggplotly(source = 'plot1') 
                 
                 p
                 
                 })
               })
  
  #clicking the map, an airzone is selected
  observeEvent(input$map_shape_click, {
    
    
    p <- input$map_shape_click
    
    try({
      airzone_select <- get_airzone(p$lat,p$lng)
      message(p$lat)
      message(p$lng)
      message(airzone_select)
      plots_list <- reactive_plot1()
      if (airzone_select != 'Northwest') {
        output$plot1 <- renderPlotly({
          
          p <- plots_list$plot_seasonal[[which(plots_list$plot_definition == airzone_select)]]
          # Add a footnote
          footnote <- list(
            font = list(size = 10),
            x = 2000, y = 8,
            # xref = "paper",
            # yref = "paper",
            text = NA,#"*Counts for wildfire days before 2014 are not included",
            showarrow = FALSE
          )
          
          p <- layout(p, annotations = list(footnote))
          message(p) # Check if the footnote is included in the annotations list
          message('annotations inserted')
          # Return the plotly graph
          p
          })
        message('Plot Refreshed')
      }
      
      leafletProxy("map") %>%
        map_exceedance(exceedances = exceedances ,az_mgmt = az_mgmt,year =input$year_slider,airzone = airzone_select)
    })
    
    
  })
  
  #clicking on the graph
  # observeEvent(event_data("plotly_click", source = "plot1"), { 
  #   values$plot.click.results <- event_data("plotly_click", source = "plot1") 
  #   message(values$plot.click.results)
  # })
  
})
}
shinyApp(ui, server)

