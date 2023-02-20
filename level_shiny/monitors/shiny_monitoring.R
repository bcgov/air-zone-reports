library(leaflet)
library(shiny)
library(dplyr)
require(rcaaqs)
require(envreportutils)
require(sf)
require(bcmaps)
require(stringr)

az_mgmt_gitURL <- 'https://github.com/bcgov/air-zone-reports/blob/master/data/out/az_mgmt.Rds?raw=true'
liststations_URL <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/liststations.csv'
results_URL <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/annual_results.csv'



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

#----FUNCTIONS--------------
#' Create a graph of the airzones
#' This is for information only, to discuss the air zones
#' No content detail included
graph_airzone <- function(polygon_a = NULL,airzone=NULL, size = c("900px","700px")) {
  
  
  if (is.null(airzone)) {
    markerON <- FALSE
  } else {
    markerON <- TRUE
    
  }
  
  
  if (0) {
    airzone <- NULL
    size = c("300px","300px")
    polygon_a = NULL
    airzone <- 'NULL'
  }
  
  
  #list of all airzones
  # lst_airzone <- bcmaps::airzones() %>% 
  #   as_tibble() %>%
  #   rename(airzone = Airzone) %>%
  #   select(airzone) 
  
  
  
  #create the airzone background colour
  #colour list below is default for airzone == NULL
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
  
  
  
  airzone <- tolower(airzone)
  # df_colour$colour[tolower(df_colour$airzone) == airzone] <- '#F2300F'
  df_colour$colour_01[tolower(df_colour$airzone) != airzone] <- 'white'
  
  
  #shortcut to retrieve from github instead
  # az_mgmt <- airzones() %>%
  #   st_make_valid() %>%
  #   st_transform(st_crs(bc_bound())) %>%
  #   st_intersection(st_geometry(bc_bound())) %>%
  #   group_by(airzone = Airzone) %>%
  #   summarize() %>%
  #   st_transform(4326)
  #   saveRDS(az_mgmt,'./data/out/az_mgmt.Rds')
  # 
  
  
  
  # left_join(df_colour,by='airzone')
  
  
  
  
  
  az_mgmt <- readRDS(url(az_mgmt_gitURL)) %>%
    left_join(df_colour,by='airzone')
  
  if (is.null(polygon_a)) {
    
    
    
    
    a <-
      leaflet(width = size[1],height = size[2],
              options = leafletOptions(attributionControl=FALSE, dragging = TRUE, minZoom = 4, maxZoom=10)) %>%
      set_bc_view(zoom=4) %>%
      setMaxBounds(lng1 = -110,lat1=45,lng2=-137,lat2=62) %>%
      addProviderTiles(providers$Esri.WorldStreetMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      add_bc_home_button()
    
    for (airzone_ in df_colour$airzone) {
      liststations_ <- liststations %>% filter(AIRZONE == airzone_)
      a <- a %>%
        
        addPolygons(data = az_mgmt %>% filter(airzone == airzone_),
                    layerId = airzone_,
                    color = 'black',
                    fillColor = ~colour_01,
                    weight = 1, opacity = 1, fillOpacity = 0.7,
                    label = paste(airzone_,'Air Zone'),
                    labelOptions = labelOptions(textsize = "15px"),
                    highlight = highlightOptions(weight = 3,
                                                 color = "blue",
                                                 bringToFront = TRUE))
      if (markerON) {
        try({
          a <- a %>%
            addMarkers(lng=liststations_$LONG,
                       lat=liststations_$LAT,
                       # layerId = airzone_,
                       # group = airzone_,
                       popup = (liststations_$monitors),
                       label = liststations_$Label,
                       clusterOptions = markerClusterOptions())
        })
      }
    }
    
  } else {
    a <- polygon_a
    
    for (airzone_ in df_colour$airzone) {
      a <- a %>%
        
        addPolygons(data = az_mgmt %>% filter(airzone == airzone_),
                    layerId = airzone_,
                    color = 'black',
                    fillColor = ~colour_01,
                    # fillColor = df_mgmt_results$colour[df_mgmt_results$airzone == 'Central Interior'],
                    weight = 1, opacity = 1, fillOpacity = 0.7,
                    # popup = '<img src="https://www2.gov.bc.ca/assets/gov/british-columbians-our-governments/services-policies-for-government/policies-procedures-standards/web-content-development-guides/corporate-identity-assets/visid-illustrations/c00_img_bcmark_desc.gif" alt="Girl in a jacket">',
                    label = paste(airzone_,'Air Zone'),
                    labelOptions = labelOptions(textsize = "15px"),
                    
                    highlight = highlightOptions(weight = 3,
                                                 color = "blue",
                                                 bringToFront = TRUE)) 
    }
    if (markerON) {
      try({
        print('adding marker')
        print(airzone)
        liststations_ <- liststations %>% 
          filter(tolower(AIRZONE) == tolower(airzone))
        print(nrow(liststations_))
        try(a <- a %>%
              removeMarker( layerId = 'stations')
        )
        try(a <- a %>%
              addMarkers(lng=liststations_$LONG,
                         lat=liststations_$LAT,
                         # layerId = 'stations',
                         popup = (liststations_$monitors),
                         group = 'stations',
                         label = liststations_$Label,
                         clusterOptions = markerClusterOptions()))
      })
    }
    
    
    
    
  }
  return(a)
}

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
                         
        ) %>%
        addMarkers(lng=liststations_$LONG,
                   lat=liststations_$LAT,
                   # layerId = liststations$AIRZONE,
                   # group = airzone_,
                   label = liststations_$site,
                   options=markerOptions())
      
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


#' Determine the air zone based on lat longs
#' 
#' @param lat is the latitude, vector OK
#' @param long is the longitude, vector OK
get_airzone <- function(lat,long) {
  
  if (0) {
    latlong <- c(57.68,-120.614)
  }
  
  # az_mgmt_gitURL <- 'https://github.com/bcgov/air-zone-reports/blob/master/data/out/az_mgmt.Rds?raw=true'
  # 
  # az_mgmt <- readRDS(url(az_mgmt_gitURL))
  
  
  
  
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



#----END of FUNCTIONS----------------


#---SHINY SECTION----------------
# Actual shiny part

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(h6("Click on the map to select an air zone"),leafletOutput("map"),width=4),
    mainPanel (
      uiOutput("md_file"),width=6
    ))
  
)



server <- shinyServer(function(input, output) {
  
  
  #initial map
  www_git_url <- 'https://github.com/bcgov/air-zone-reports/raw/master/level2_page/www/'
  
  #debug for use of local files
  if (0) {
    www_git_url <- './www/'
  }
  
  #t_ready means it is ready to receive clicks
  a <- graph_airzone(polygon_a=NULL,airzone=NULL,size = c('200px','400px'))
  output$map <- renderLeaflet(a)
  output$md_file <- renderUI({
    file <- paste(www_git_url,'station_intro.Rmd',sep='')
    includeMarkdown(file)
  })
  
  airzone_select_previous <- ''
  # t0 <- Sys.time()
  # observe the marker click info and print to console when it is changed.
  observeEvent(input$map_shape_click, {
    
    
    p <- input$map_shape_click # map because that is the name of the output
    
    print(p$lat)
    # text <- paste("lat ", p$lat," lon ", p$lng) # shows lat/lon in console
    try({
      airzone_select <- get_airzone(p$lat,p$lng)
      
      if (airzone_select != airzone_select_previous) {
        
        # map redrawn after click
        leafletProxy("map")%>%
          graph_airzone(airzone=airzone_select,size = c('200px','400px'))
        #insert text here  
        html_file <- paste(stringr::str_replace_all(string=airzone_select, pattern=" ", repl=""),'.Rmd',sep='')
        html_file <- paste(www_git_url,'station_',html_file,sep='')
        print(html_file)
        
        try({output$md_file <- renderUI({
          file <- html_file
          includeMarkdown(file)
        })
        })
        airzone_select_previous <- airzone_select
      } 
    })
  })
  
  
})

shinyApp(ui, server)
