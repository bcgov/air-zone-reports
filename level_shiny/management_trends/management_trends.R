#' This R script was made to create an interactive map
#' To disply trends in management levels

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
library(stringr)
library(janitor)


dirs_location <- 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/data'  #local location, two dots for final, one dot for debug
if (0) {
  dirs_location <- './data/out'
  list.files('./data/out')
}

rep_year <- 2021
#load from files----
df_management_airzones <- readr::read_csv(paste(dirs_location,'out/management_airzones.csv',sep='/'))
az_mgmt_polygon <- readr::read_rds(paste(dirs_location,'out/az_mgmt.Rds',sep='/'))
df_stations <- readr::read_csv(paste(dirs_location,'out/liststations.csv',sep='/'))
df_stn_mgmt <- readr::read_csv(paste(dirs_location,'out/management.csv',sep='/'))
df_az_mgmt <- readr::read_csv(paste(dirs_location,'out/management_airzones.csv',sep='/'))

graph_mgmt_trends <-  readr::read_rds(paste(dirs_location,'out/management_plots.Rds',sep='/'))
#download data
df_downloads <- readr::read_csv(paste(dirs_location,'out/management.csv',sep='/')) %>% 
  ungroup() %>%
  select(site,instrument,year,parameter,metric,metric_value,tfee,colour_text) %>%
  unique() %>%
  rename(management_level = colour_text) %>%
  mutate(tfee = ifelse(tfee,'TFEE adjusted','no TFEE adjustment'),
         metric_value = as.character(metric_value)) %>%
  pivot_wider(names_from = tfee, values_from = c(metric_value,management_level)) 

df_downloads_airzone <- readr::read_csv(paste(dirs_location,'out/management_airzones.csv',sep='/')) %>% 
  ungroup() %>%distinct() %>%
  select(year,airzone,pollutant,tfee,colour_text) %>%
  mutate(tfee = ifelse(tfee,'TFEE adjusted','no TFEE adjustment')) %>%
  pivot_wider(names_from = tfee,values_from = colour_text) %>%
  arrange(airzone,pollutant,year)

year_max <- max(df_stn_mgmt$year)


#cleanup----

#rename columns, apply tfee for PM, O3
df_stn_mgmt <- clean_names(df_stn_mgmt) %>%
  mutate(parameter = tolower(parameter)) %>%
  rename(lat = latitude, 
         lon = longitude) %>%
  filter((parameter %in% c('pm25','o3') & tfee) |
           (parameter %in% c('no2','so2') & !tfee)
  )

df_az_mgmt <-df_az_mgmt %>%
  dplyr::rename(parameter = pollutant) %>%
  mutate(parameter = tolower(parameter)) %>%
  filter((parameter %in% c('pm25','o3') & tfee) |
           (parameter %in% c('no2','so2') & !tfee)
  )

df_stations <- clean_names(df_stations) %>%
  rename(lon = long)

#remove double space and # character
df_stn_mgmt$site <- gsub('#',' ',df_stn_mgmt$site)
df_stn_mgmt$site <- gsub('  ',' ',df_stn_mgmt$site)
df_stations$site <- gsub('#',' ',df_stations$site)
df_stations$site <- gsub('  ',' ',df_stations$site)


#analyze----



#select only the stations with data from the recent 3 years
lst_stations <- df_stn_mgmt %>%
  filter(year %in% ((year_max-2):year_max)) %>%
  pull(site) %>% unique()

if (0) {
  # df <- df_stations
  df_stations <- df
}
df_stations <- ungroup(df_stations) %>%
  filter(site %in% lst_stations |
           station_name %in% lst_stations) %>%
  select(station_name,site,label,lat,lon,airzone) %>%
  group_by(site) %>% slice(1) %>%
  ungroup()

#merge station details to caaqs data
df_stn_mgmt <- df_stn_mgmt %>%
  ungroup() %>%
  filter(!is.na(lat) & !is.na(lon))


lst_airzone <- az_mgmt %>% select(airzone) %>% distinct %>% pull(airzone)

df_datacatalogue <- tibble(
  parameter = c('PM25','PM25','O3','NO2','NO2','SO2','SO2'),
  metric = c('Annual','24-Hour','8-Hour','Annual','1-Hour','Annual','1-Hour')
)



# define constants -----
# define constants such as parameter and 
df_parameter <- tribble(
  ~display, ~parameter,
  'PM\u2082.\u2085','pm25',
  'O\u2083','o3',
  'NO\u2082','no2',
  'SO\u2082','so2'
)

df_metric <- tibble(
  parameter = c('pm25','pm25',
                'no2','no2',
                'so2','so2',
                'o3'),
  metric = c('24-Hour','Annual',
             '1-Hour','Annual',
             '1-Hour','Annual',
             '8-Hour')
)


df_metric <- df_metric %>%
  left_join(df_parameter)

# define functions---------------


#' Create management level legend table
#' This is generalized
add_mgmt_legend <- function() {
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
    
    formatStyle('Management Level',target = 'row',backgroundColor = styleEqual(df_colour_levels$colour_text,df_colour_levels$colour),
                Color = styleEqual(df_colour_levels$colour_text,df_colour_levels$txt_colour)) 
  
  return(a)
}

#' Create maps of air zone and management levels
#' 
#' @param stn_mgmt is the data frame containing management level data for stations
#' @param az_mgmt is the dataframe containing management level for air zone
#' @param parameter is the pollutant
#' @param year is the data year
#' @param polygon_a is the polygon. Prevents redrawing
map_mgmt <- function(polygon_a = NULL,stn_mgmt,az_mgmt,parameter,year,
                     size = c('200px','400px'),
                     airzone = NULL) 
{
  
  
  
  if (0) {
    polygon_a = NULL
    stn_mgmt <- df_stn_mgmt
    az_mgmt <- df_az_mgmt
    parameter <- 'PM25'
    year <- year_max
    size = c('600','400px')
    airzone = NULL
  }
  
  
  
  #clean----
  az_mgmt <- clean_names(az_mgmt)
  az_mgmt$parameter <- tolower(az_mgmt$parameter)
  stn_mgmt <- clean_names(stn_mgmt)
  #remove the tfee, select only needed for az_mgmt
  if ('tfee' %in% colnames(az_mgmt)) {
    az_mgmt <- az_mgmt %>%
      filter((tfee & parameter %in% c('pm25','o3')) |
               (!tfee & parameter %in% c('no2','so2'))) %>%
      select(-tfee)
  }
  try(if (tolower(airzone) == 'all') {
    airzone <- NULL
  }, silent = TRUE)
  
  #redefine input parameters, prevent conflict with colnames
  parameter_select <- tolower(parameter)
  airzone_select <- airzone
  year_select <- year
  
  message(paste('map_airzone function',is.null(polygon_a)))
  
  
  # -cleanup station
  #ensure all sites and location are the same
  df_fill_stn <- stn_mgmt %>%
    select(site,lat,lon) %>%
    distinct() %>%
    group_by(site) %>% slice(1)
  
  stn_mgmt <- stn_mgmt %>%
    select(-lat,-lon) %>%
    left_join(df_fill_stn, by='site')
  
  #process airzone----
  #create filler to have list of airzone and parameters
  #this is to populate with all possible values
  lst_airzones <- bcmaps::airzones() %>%
    clean_names() %>%
    ungroup() %>%
    pull(airzone)
  
  df_fill <- tibble(airzone = lst_airzones) %>%
    merge(
      az_mgmt %>% 
        select(year,parameter)%>% 
        distinct()
    )
  
  
  az_mgmt <- df_fill %>%
    left_join(az_mgmt)
  
  
  # -define values for the Northwest and air zone with no data
  az_mgmt$colour[is.na(az_mgmt$colour)] <- '#666565'  #grey colour for air zones with no value
  az_mgmt$colour_order[is.na(az_mgmt$colour_order)] <- 0
  az_mgmt$colour_text[is.na(az_mgmt$colour_text)] <- 'N/A'
  #add pop-up
  az_mgmt <- az_mgmt %>%
    ungroup() %>%
    
    mutate(index = 1:n()) %>%
    group_by(index) %>%
    mutate(hover_txt =HTML(paste0('<p>',airzone,
                                  ' Air Zone<br>Management Level:',
                                  toupper(colour_text),'</p>',sep=''))) %>%
    ungroup() %>% select(-index)
  
  
  az_mgmt <- az_mgmt %>%
    filter(year == year_max,
           parameter == parameter_select) %>%
    left_join(az_mgmt_polygon)
  
  #process station----
  #create a list of stations with that parameter
  #list stations since 2011
  lst_stns <- stn_mgmt %>%
    select(site,parameter,lat,lon) %>%
    distinct()
  
  #get the correct management level for stations
  
  #colour codes
  #add the gray NA value
  df_colour <- stn_mgmt %>%
    select(colour, colour_text,colour_order) %>%
    distinct()
  
  #tfee adjustment applies to pm2,5 and o3
  #include all stations reporting data since 2011
  stn_mgmt_mapped <- stn_mgmt %>%
    filter((tfee & parameter %in% c('pm25','o3')) |
             (!tfee & parameter %in% c('no2','so2'))) %>%
    filter(year %in% year_select) %>%
    group_by(site,parameter) %>%
    summarise(colour_order = max(colour_order,na.rm = TRUE)) 
  
  #include all other stations that has not reported data
  stn_mgmt_mapped <- lst_stns %>%
    left_join(stn_mgmt_mapped, by=c('site','parameter'))%>%
    left_join(df_colour, by = 'colour_order')
  
  #put values for blanks
  stn_mgmt_mapped$colour_order[is.na(stn_mgmt_mapped$colour_order)] <- 0
  stn_mgmt_mapped$colour[is.na(stn_mgmt_mapped$colour)] <- '#666565'
  stn_mgmt_mapped$colour_text[is.na(stn_mgmt_mapped$colour_text)] <- 'N/A'
  
  #select for the parameter
  stn_mgmt_mapped <- stn_mgmt_mapped %>%
    filter(parameter == parameter_select)
  
  
  #map----
  if (is.null(polygon_a)) {
    # a <- 
    leaflet(width = size[1],height = size[2],
            options = leafletOptions(attributionControl=FALSE, dragging = TRUE, minZoom = 4, maxZoom=10)) %>%
      set_bc_view(zoom=3.5) %>%
      # setView(zoom =5) %>%
      setMaxBounds(lng1 = -110,lat1=45,lng2=-137,lat2=62) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(opacity = 1)
      ) %>%
      # addProviderTiles(providers$Stamen.TonerLabels) %>%
      add_bc_home_button() %>%
      
      #PM25 and other pollutants
      addPolygons(data = az_mgmt$geometry,
                  layerId = az_mgmt$airzone,
                  color = az_mgmt$colour, weight = 0, opacity = 0.5, fillOpacity = 0.7,
                  label = az_mgmt$hover_txt,
                  labelOptions = labelOptions(textsize = "15px")) %>%
      
      addPolylines(data = az_mgmt$geometry,
                   layerId = paste(az_mgmt$airzone,'01'),
                   color = 'black',weight = 2) %>%
      addCircleMarkers(data = stn_mgmt_mapped,
                 lng=~lon,
                 lat=~lat,
                 fill = ~colour,
                 color = 'black',
                 group = 'Stations'
      )
    icon = ~leaflet::makeIcon(icons, iconWidth = icon_wh[1], iconHeight = icon_wh[2],
                              iconAnchorX = icon_anchor[1], iconAnchorY = icon_anchor[2],
                              popupAnchorX = popup_anchor[1], popupAnchorY = popup_anchor[2]),
    # popup = (liststations_$monitors[1]),
    label = ~Label)

  } 
  
  
  #add for selected airzone
  if (!is.null(airzone_select) & !is.null(polygon_a)) {
    print(paste('updating,highlighting area',airzone_select))
    a <- polygon_a %>%
      addPolylines(data = df$geometry[df$airzone == airzone_select],
                   layerId = 'selectedairzone',
                   color = 'blue',weight = 5)
  }
  
  #slider actions
  #need to refresh the air zone colours
  if (is.null(airzone_select) & !is.null(polygon_a)) {
    print(paste('updating,highlighting area',airzone_select))
    a <- polygon_a %>%
      addPolygons(data = df$geometry,
                  layerId = df$airzone,
                  color = df$colour, weight = 0, opacity = 0, fillOpacity = 0.7,
                  label = df$hover_txt,
                  labelOptions = labelOptions(textsize = "15px"))%>%
      addPolylines(data = df$geometry,
                   layerId = 'selectedairzone',
                   color = 'blue',weight = 0)
  }
  
  #debug for ozone 2019 issue
  
  if (0) {
    View(df)
    print(paste('Rows of data',nrow(df)))
    print(paste('year',unique(df$year)))
    print(paste('tfee',unique(df$tfee)))
    df_debug <- unique(df$colour_text[df$colour_order == max(df$colour_order)])
    print(paste('Management level, highest',df_debug))
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
  
  az_mgmt <- az_mgmt0 %>%
    left_join(df_colour,by='airzone')
  
  print('graph airzone function')
  if (is.null(polygon_a)) {
    
    print('NULL Polygon')
    
    
    a <-
      leaflet(width = size[1],height = size[2],
              options = leafletOptions(attributionControl=FALSE, dragging = TRUE, minZoom = 4, maxZoom=15)) %>%
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
            addMarkers(data = liststations_,
                       lng=~LONG,
                       lat=~LAT,
                       
                       group = 'Stations',
                       icon = ~leaflet::makeIcon(icons, iconWidth = icon_wh[1], iconHeight = icon_wh[2],
                                                 iconAnchorX = icon_anchor[1], iconAnchorY = icon_anchor[2],
                                                 popupAnchorX = popup_anchor[1], popupAnchorY = popup_anchor[2]),
                       popup = (liststations_$monitors[1]),
                       label = ~Label) %>%
            addMarkers(data = liststations_aqhi_,
                       lng=~LONGITUDE,
                       lat=~LATITUDE,
                       
                       group = 'AQHI',
                       icon = ~leaflet::makeIcon(icons, iconWidth = icon_wh[1], iconHeight = icon_wh[2],
                                                 iconAnchorX = icon_anchor[1], iconAnchorY = icon_anchor[2],
                                                 popupAnchorX = popup_anchor[1], popupAnchorY = popup_anchor[2]),
                       
                       label = ~AQHI_AREA) %>%
            addLayersControl(baseGroups =  c('Stations','AQHI'),
                             options = layersControlOptions(collapsed = FALSE))
          # clusterOptions = markerClusterOptions())
        })
      }
    }
    
  } else {
    print('not null polygon')
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
        
        liststations_aqhi_ <- liststations_aqhi %>%
          filter(tolower(AIRZONE) == tolower(airzone))
        df_map_settings_ <- df_map_settings %>%
          filter(tolower(AIRZONE) == tolower(airzone))
        
        print(paste('zoom:', df_map_settings_$zoom))
        print(df_map_settings_)
        print(nrow(liststations_))
        print(nrow(liststations_aqhi))
        a <- a %>%
          setView(lng = df_map_settings_$long, lat = df_map_settings_$lat, zoom = df_map_settings_$zoom) %>%
          clearGroup('Stations') %>%
          clearGroup('AQHI')
        
        a <- a %>%
          addMarkers(data = liststations_,
                     lng=~LONG,
                     lat=~LAT,
                     icon = ~leaflet::makeIcon(icons, iconWidth = icon_wh[1], iconHeight = icon_wh[2],
                                               iconAnchorX = icon_anchor[1], iconAnchorY = icon_anchor[2],
                                               popupAnchorX = popup_anchor[1], popupAnchorY = popup_anchor[2]),
                     # color = liststations_$OWNER,
                     # layerId =liststations_$index,
                     popup = ~display,
                     group = 'Stations',
                     label = ~Label) %>%
          addMarkers(data = liststations_aqhi_,
                     lng=~LONGITUDE,
                     lat=~LATITUDE,
                     
                     group = 'AQHI',
                     icon = ~leaflet::makeIcon(icons, iconWidth = icon_wh[1], iconHeight = icon_wh[2],
                                               iconAnchorX = icon_anchor[1], iconAnchorY = icon_anchor[2],
                                               popupAnchorX = popup_anchor[1], popupAnchorY = popup_anchor[2]),
                     
                     label = ~paste('AQHI Area:',AQHI_AREA)) %>%
          addLayersControl(baseGroups = c('Stations','AQHI'),
                           options = layersControlOptions(collapsed = FALSE)
          )
        # clusterOptions = markerClusterOptions())
      })
      
      
      
    }
    
    
    
    
  }
  return(a)
}




#initial values---------
pollutant_initial <- df_parameter$display[1]
year_initial <- max(df_management_airzones$year)
tfee_initial <- TRUE

#---SHINY SECTION----------------
# Actual shiny part
#ui----
{
  ui <-
    
    (fluidPage(
      h4(HTML('Air Quality Management Levels')),
      tags$head(
        tags$style(HTML("
      body { background-color: #f2efe9; }
      .container-fluid { background-color: #fff; width: auto; padding: 5px; }
      .topimg { width: 0px; display: block; margin: 0px auto 0px auto; }
      .title { text-align: center; }
      .toprow { margin: 5px 5px; padding: 5px; background-color: #38598a; }
      .filters { margin: 3px auto; }
      .shiny-input-container { width:90% !important; }
      .table { padding: 0px; margin-top: 0px; }
      .leaflet-top { z-index:999 !important; }
      "))),
      fluidRow(class = "toprow",
               fluidRow(class = 'filters',
                        column(2,
                               tags$style(type='text/css', 
                                          '.selectize-input { font-size: 15px; line-height: 10px;} 
                          .selectize-dropdown { font-size: 12px; line-height: 15px; }
                          .control-label {font-size: 12px; color: white !important;}
                          .irs-min {font-size: 0px; color: white; !important}
                          .irs-max {font-size: 0px; color: white;}
                          .irs-single {font-size: 14px; color: white;}
                          .irs-grid-text {font-size: 10px; color: white;}'
                               ),
                               # selectizeInput('pollutant',label = 'Pollutant:',
                               #                choices = df_parameter$display,width = "5%"),
                        ),
                        column(12, align = "center",
                               
                               actionButton(inputId = 'centralinterior',label = HTML('PM<sub>2.5</sub>'),width = "12.5%",title = 'Central Interior Air Zone'),
                               actionButton(inputId = 'coastal',label = HTML('O<sub>3</sub>'),width = "12.5%",title = 'Coastal Air Zone'),
                               actionButton(inputId = 'georgiastrait',label = HTML('NO<sub>2</sub>'),width = "12.5%",title = 'Georgia Strait Air Zone'),
                               actionButton(inputId = 'lowerfraservalley',label = HTML('SO<sub>2</sub>'),width = "12.5%",title = 'Lower Fraser Valley Air Zone'),
                        ),
                        # column(6,
                        #        sliderInput('year_slider',label ='Year',
                        #                    min = min(df_management_airzones$year),
                        #                    max = year_max,
                        #                    value = year_initial,width = "100%",
                        #                    sep='')
                        # )
                        
               )),
      
      fluidRow(
        
        column(12,
               leaflet::leafletOutput("map",height = '400px'),
               #legend table
               DT::dataTableOutput("table1",width = '90%', height = '400px')),
        # column(9,
        #        #h6("Use vertical scrollbar (right side of graph) to reveal more bar graphs."),
        #        fluidRow(
        #          column(3,actionButton('button','Switch to 24-Hour Metric')),
        #          column(9,htmlOutput("Title"))
        #        ),
        #        (div(style='height:800px;overflow-y: scroll;',
        #             plotlyOutput("plot1",height = "800px"))))
        
      ),
      
      fluidRow(column(9,downloadLink('downloadData2', 'Download Air Zone Management Level'))),
      fluidRow(column(9,downloadLink('downloadData', 'Download Station Data')))
      
      
      
      #        sidebarLayout(
      # sidebarPanel(radioButtons("no Wildfire","wildfire-adjusted"),
      # c('tfee' = 'tfee','notfee'='notfee')
      # ))
      # sidebarLayout(
      #   sidebarPanel(leafletOutput("map"),width=5),
      #   mainPanel (
      #     uiOutput("md_file"),width=7
      #   ))
      
    ))
  
}


#server----
{
  server <- shinyServer(function(input, output,session) {
    
    
    #graph defines the graph that will be taken from the 'graph_barcaaqs' list
    
    #the function get_datacatalogue retrieves this value
    # bargraph <- reactiveValues(plot=NULL)
    parameter_select <- reactiveValues(params = NULL)
    #initial value
    observe({
      parameter_select$param <- list(parameter = 'PM25',metric = 'annual',airzone = 'ALL',year = year_max)
      # bar_<- get_datacatalogue(parameter = 'PM25',metric = 'annual',
      #                   airzone = 'ALL',year = year_max,lst_graphs = lst_graphs)
      # bargraph$plot <- graph_barcaaqs[[bar_]]
      # title <- paste(df_parameter$display[tolower(df_parameter$parameter) == tolower(parameter_select$param$pollutant)],
      #                parameter_select$param$metric,
      #                'Metric')
      # print(title)
      # output$Title <- renderText(title)
      
    })
    
    airzone_select <- NULL
    a <-    map_airzone(polygon_a=NULL,df=df_management_airzones,az_mgmt = az_mgmt,parameter = pollutant_initial,
                        year = year_initial)
    b <- graph_airzone(airzone = NULL)
    output$map <- renderLeaflet(a)
    
    output$plot1 <- renderPlotly({
      print('update map')
      
      bar_<- get_datacatalogue(parameter = parameter_select$param$parameter,metric = parameter_select$param$metric,
                               airzone = parameter_select$param$airzone,year = parameter_select$param$year,lst_graphs = lst_graphs)
      print(bar_)    
      if (!is.null(bar_)) {
        graph_barcaaqs[[bar_]]
      } else {
        ggplot() +
          
          annotate("text", x = 0.5, y = 0.5, label = "No Valid Data Available",size=12,vjust =-1) +
          theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
                axis.title.x = element_blank(), axis.title.y = element_blank(),
                panel.background = element_rect(fill = NA)) 
      }
    })
    
    output$Title <- renderText({
      print('Update Tile')
      
      #Create a title that will say
      #PM2.5 Annual Metric for the Central Interior Air Zone (2019-2021 Period)
      title <- paste('<b><h4>',parameter_select$param$parameter,'  ',parameter_select$param$metric,
                     ' Metric For ',parameter_select$param$airzone,' Air Zone Sites ',
                     '(',parameter_select$param$year-2,'-',parameter_select$param$year,')</h4>',sep='')
      
      
      str_to_title(title)
      title <- gsub('pm25','PM<sub>2.5</sub>',title,ignore.case = TRUE)
      title <- gsub('no2','NO<sub>2</sub>',title,ignore.case = TRUE)
      title <- gsub('so2','SO<sub>2</sub>',title,ignore.case = TRUE)
      title <- gsub('o3','O<sub>3</sub>',title,ignore.case = TRUE)
      title <- gsub(' ALL Air Zone',' B.C',title,ignore.case = TRUE)
      title <- gsub('annual','Annual',title,ignore.case = TRUE)
      title <- gsub(' 1-hour','1-Hour',title,ignore.case = TRUE)
      title <- gsub(' 8-hour','8-Hour',title,ignore.case = TRUE)
      title <- gsub(' 24-hour','24-Hour',title,ignore.case = TRUE)
      return(title)
    })
    
    data <- df_downloads 
    data2 <- df_downloads_airzone
    output$table1 <- DT::renderDT(add_mgmt_legend())
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(data, con)
      }
    )
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste('airzone_mgmt-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(data2, con)
      }
    )
    
    
    
    observeEvent(input$map_shape_click, {
      
      
      p <- input$map_shape_click
      
      try({
        airzone_select <- get_airzone(p$lat,p$lng)
        print(p$lat)
        print(p$lng)
        print(airzone_select)
        print(input$pollutant)
        
        leafletProxy("map") %>%
          map_airzone(df=df_management_airzones,az_mgmt = az_mgmt,
                      parameter = input$pollutant,
                      year = input$year_slider,
                      airzone = airzone_select)
        
        parameter_select$param$airzone = airzone_select
        
        
      })
    })
    
    observeEvent(input$pollutant,
                 {
                   print('change pollutant')
                   if (!is.null(airzone_select)) {
                     print(airzone_select)
                   }
                   leafletProxy("map") %>%
                     map_airzone(df=df_management_airzones,az_mgmt = az_mgmt,
                                 parameter = input$pollutant,
                                 year = input$year_slider,
                                 
                                 airzone = NULL)
                   leafletProxy("map") %>%
                     map_airzone(df=df_management_airzones,az_mgmt = az_mgmt,
                                 parameter = input$pollutant,
                                 year = input$year_slider,
                                 airzone =  parameter_select$param$airzone)
                   
                   
                   
                   
                   # change the switch button
                   parameter_select$param$parameter <- df_parameter$parameter[df_parameter$display == input$pollutant]
                   param_select <- tolower(parameter_select$param$parameter)
                   metric_options <- unique(df_metric$metric[tolower(df_metric$parameter) == param_select])
                   metric_select <- tolower(metric_options[1])
                   
                   
                   
                   if (length(metric_options) >1) {
                     print('udpate buttons')
                     metric_notselected <- metric_select
                     metric_select <- metric_options[tolower(metric_options) != tolower(metric_select)][1]
                     updateActionButton(session, "button", label = paste("Switch to",metric_notselected,'metric'))
                     parameter_select$param$metric <-  tolower(metric_select)
                   } else {
                     print('Hiding button')
                     parameter_select$param$metric <-  tolower(metric_select)
                     updateActionButton(session, "button", label = paste(metric_select,'metric'))
                     # hide('button')
                   }
                   
                 })
    
    observeEvent(input$button, {
      metric_select <- tolower(parameter_select$param$metric)
      param_select <- tolower(parameter_select$param$parameter)
      print(param_select)
      metric_options <- unique(df_metric$metric[tolower(df_metric$parameter) == param_select])
      
      print(paste('Button pushed:',metric_select))
      
      # print(paste('Metric Options:',length(metric_options)))
      
      if (length(metric_options) >1) {
        print('udpate buttons')
        metric_notselected <- metric_select
        metric_select <- metric_options[tolower(metric_options) != tolower(metric_select)][1]
        updateActionButton(session, "button", label = paste("Switch to",metric_notselected,'metric'))
        parameter_select$param$metric <-  tolower(metric_select)
      }
      # } else {
      #   print('Hiding button')
      #   # hide('button')
      # }
      
    })
    
    
    
  })
}
shinyApp(ui, server)


