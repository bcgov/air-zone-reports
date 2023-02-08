

map_exceedance <- function(exceedances,az_mgmt,year,size = c('200px','400px'),map_a = NULL) {
  
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
    addProviderTiles(providers$Stamen.TonerLite,
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
  
  color_scales <- colfunc(max_days)
  for (airzone_ in lst_airzones) {
    
    if (0) {
      airzone_ <- lst_airzones[1]
    }
    liststations_ <- lst_stations %>% 
      filter(AIRZONE == airzone_, year == year_select)
    station_exceedance_ <- station_exceedance %>% 
      filter(AIRZONE == airzone_, year == year_select)
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
      a <- a %>%
        addCircleMarkers(lng=station_exceedance_$LONG,
                         lat = station_exceedance_$LAT,
                         layerId = station_exceedance_$site,
                         label = station_exceedance_$site,
                         color = color_scales[station_exceedance$days_exceed],
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
  plot_a <- a
  
  
  if (is.null(map_a)) {
    #create map for annual
    b <-  leaflet(width = size[1],height = size[2],
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
    b <- map_a
  }
  
  
  
  
  
  return(a)
}
