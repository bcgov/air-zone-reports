library(dplyr)
library(httr)
library(jsonlite)
library(stringdist)
library(sf)

savedir<- './data/out'
# display the cities
# visit https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger.cfm?Lang=E
# to retrieve the BC Census divisins
# for cities.csv, visit https://geogratis.gc.ca/services/geoname/en/geonames?province=59, copy and paste to Excel

#' Determine the air zone based on lat longs
#' 
#' @param lat is the latitude, vector OK
#' @param long is the longitude, vector OK
get_airzone <- function(lat,long) {
  
  if (0) {
    latlong <- c(50.02306,-125.2436)
    lat <- latlong[1]
    long <- latlong[2]
  }
  
  # az_mgmt_gitURL <- 'https://github.com/bcgov/air-zone-reports/blob/master/data/out/az_mgmt.Rds?raw=true'
  
  # az_mgmt <- readRDS(url(az_mgmt_gitURL))
  az_mgmt <- readRDS('./data/out/az_mgmt.Rds')
  
  
  
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
  try({pnts$airzone <- apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                        function(col) { 
                          tt1_trans[which(col), ]$airzone
      
                                            })
  return(pnts$airzone)
  })
 return(NULL)
  
}
#' Retrieves the lat long of a given B.C. municipality
get_citylatlong <- function(city) {
  
  if (0) {
    city_ <- 'Victoria'
    city_ = 'Prince George'
    city_ = 'Nelson'
  }
  api_query <-'https://geogratis.gc.ca/services/geoname/en/geonames?q='

    
  lat_list <- NULL
  lon_list <- NULL
  az_list <- NULL
  
  for (city_ in city) {
    print(paste('Analyzing:', city_))
    city_ <- gsub('One Hundred Mile House','100 Mile House',city_)
    city_ <- gsub('Sun Peaks Mountain','Sun Peaks',city_)
    city_ <- gsub('(Part)','',city_)
    
    city_ <- gsub(' ','%20',city_)
    
    url <- paste(api_query,city_,'&province=59',sep='')
    lat <- 0
    lon <- 0
    # az <- NA
    try({
      response <- GET(url)
      # extract the JSON content from the response
      content <- content(response, as = "text")
      df_result <- NULL
      # parse the JSON content into a dataframe
      df_result <- fromJSON(content)
      
      name <- df_result$items$name
      dist <- stringdist(city_,name)
      i <- which.min(dist)
      
      
      lat <- df_result$items$latitude[i]
      lon <- df_result$items$longitude[i]
      
      # az <- get_airzone(lat,lon)
      
      
      
      
    })
    lat_list <- c(lat_list,lat)
    lon_list <- c(lon_list,lon)
    # az_list <- c(az_list,az)
  }
  
  result <- list(latitude = lat_list,longitude = lon_list)
  return(result)
}


#use the lines below to create population list from StatCanada
if (0) {
  download.file('https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm?Lang=E&FILETYPE=CSV&GEONO=025',
                destfile = 'C:/Temp/statcan_bc_csd.zip')
  
  df_csd <- readr::read_csv('C:/temp/CSD_temp.csv') 
  
  df_csd %>%
    filter(CHARACTERISTIC_NAME == 'Population, 2021') %>% 
    readr::write_csv(paste(savedir,'population.csv',sep='/'))
}

# colnames(df_csd)
df_csd <- readr::read_csv(paste(savedir,'population.csv',sep='/')) %>%
  filter(!grepl('(RDA)',GEO_NAME)) %>%
  select(GEO_NAME,C1_COUNT_TOTAL) %>%
  group_by(GEO_NAME) %>%
  slice(1) %>%
  mutate(Name = unlist(stringr::str_split(GEO_NAME, ","))[1]) %>%
  # filter(C1_COUNT_TOTAL>1000) %>%
  ungroup() 

df_latlong <- NULL
for (i in 1:nrow(df_csd)) {
  latlon <- NULL
  # az <- NULL
  try({
    city <- df_csd$Name[i]
  latlon <- get_citylatlong(city)
  # az <- get_airzone(latlon$latitude,latlon$longitude)
  })
  
  if (!is.null(latlon)) {
    try({df_latlong <- df_latlong %>%
      bind_rows(
        tibble(
          Name = city,
          latitude = latlon$latitude,
          longitude = latlon$longitude,
          airzone = try(get_airzone(latlon$latitude[1],latlon$longitude[1]))
        )
      )})
  }
  
}


df_csd <- df_csd %>%
  left_join(df_latlong) 

#repair NA air zone
df_csd %>%
  filter(is.na(airzone))

df_csd$airzone[df_csd$Name == 'Campbell River'] <- 'Georgia Strait'
df_csd$airzone[df_csd$Name == 'Delta'] <- 'Lower Fraser Valley'
df_csd$airzone[df_csd$Name == 'Lantzville'] <- 'Georgia Strait'
df_csd$airzone[df_csd$Name == 'Lions Bay'] <- 'Georgia Strait'
df_csd$airzone[df_csd$Name == 'Musqueam 2'] <- 'Georgia Strait'
df_csd$airzone[df_csd$Name == 'Port McNeill'] <- 'Georgia Strait'
df_csd$airzone[df_csd$Name == 'Prince Rupert'] <- 'Coastal'
df_csd$airzone[df_csd$Name == 'Richmond'] <- 'Lower Fraser Valley'
df_csd$airzone[df_csd$Name == 'Tofino'] <- 'Georgia Strait'

readr::write_csv(df_csd,'./data/out/cities.csv')

#render Rmd to html
rmd_files <- list.files('./level2_page/www',full.names = TRUE)
rmd_files <- rmd_files[grepl('.Rmd',rmd_files)]


for (filelist in rmd_files) {
  rmarkdown::render(filelist)
}

