library(leaflet)
library(shiny)
library(dplyr)
require(rcaaqs)
require(envreportutils)
require(sf)
require(bcmaps)
require(stringr)

#function
get_stationlist <- function() {
  az_mgmt_gitURL <- 'https://github.com/bcgov/air-zone-reports/blob/master/data/out/az_mgmt.Rds?raw=true'
  liststations_URL <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/liststations.csv'
  results_URL <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/annual_results.csv'
  
  az_mgmt0 <- readRDS(url(az_mgmt_gitURL)) 
  
  station_data <-  readr::read_csv(results_URL) %>%
    filter(year>=2020) %>%
    select(site) %>% distinct()
  
  # aqhi_stations <- readr::read_csv('https://envistaweb.env.gov.bc.ca/aqo/setup/BC_AQHI_SITES_AQHIPlusSO2.csv')
  
  
  liststations <- envair::listBC_stations(use_CAAQS = TRUE) %>%
    filter(site %in% station_data$site) %>%
    group_by(STATION_NAME) %>%
    slice(1) %>% ungroup() %>%
    mutate(monitors = ifelse(pm25 == 'Y','PM<sub>2.5</sub>','')) %>%
    mutate(monitors = ifelse(o3== 'Y',paste(monitors,',O<sub>3</sub>',sep=''),
                             monitors)) %>%
    mutate(monitors = ifelse(no2== 'Y',paste(monitors,',NO<sub>2</sub>',sep=''),
                             monitors)) %>%
    mutate(monitors = ifelse(so2== 'Y',paste(monitors,',SO<sub>2</sub>',sep=''),
                             monitors)) %>%
    mutate(monitors = stringr::str_trim(monitors,side = "left")) %>%
    mutate(monitors = gsub("^,+", "", monitors)) %>%
    
    # mutate(AQMS = ifelse(is.na(AQMS),'UNKNOWN',AQMS)) %>%
    select(Label,LAT,LONG,STATUS,AQMS,AIRZONE,OWNER,monitors) %>%
    ungroup()
  return(liststations)
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
    df <- read_csv(url_aqhi) %>%
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

az_mgmt_gitURL <- 'https://github.com/bcgov/air-zone-reports/blob/master/data/out/az_mgmt.Rds?raw=true'
url_aqhi <- 'https://envistaweb.env.gov.bc.ca/aqo/setup/BC_AQHI_SITES_AQHIPlusSO2.csv'


az_mgmt0 <- readRDS(url(az_mgmt_gitURL))


aqhi_stations <- read_csv(url_aqhi) %>%
  select(AQHI_AREA,LATITUDE,LONGITUDE) %>%
  group_by(AQHI_AREA) %>%
  slice(1) %>%
  get_airzone_df() %>%
  mutate(airzone = ifelse(grepl('Metro Vancouver',AQHI_AREA),'Lower Fraser Valley',airzone))

# define a function that takes in a text vector and outputs a comma-separated string
to_comma_string <- function(vec) {
  if (length(vec) == 1) {
    return(vec)
  } else if (length(vec) == 2) {
    return(paste(vec, collapse = ", and "))
  } else {
    return(paste(vec[-length(vec)], collapse = ", ")  %>% 
             paste(vec[length(vec)], sep = ", and ", collapse = ""))
  }
}
library(dplyr)

stations <- get_stationlist() %>%
  # select(Label, AQMS, OWNER,AIRZONE) %>%
  distinct()

