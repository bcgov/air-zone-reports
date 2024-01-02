# Copyright 2023 Province of British Columbia
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
library(rcaaqs)


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

#location of plots
dir_plots <- 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/data/plots/'

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


lst_airzone <- df_az_mgmt %>% select(airzone) %>% distinct %>% pull(airzone)

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
map_mgmt <- function(stn_mgmt,az_mgmt,parameter,year,
                     size = c('600px','1200px'),
                     airzone = NULL) 
{
  
  
  
  if (0) {
    polygon_a = NULL
    stn_mgmt <- df_stn_mgmt
    az_mgmt <- df_az_mgmt
    parameter <- 'pm25'
    year <- year_max
    size = c('600','400px')
    airzone = NULL
  }
  
  #load
  polygon_a = NULL
  asset_url <- 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/level_shiny/management_trends/assets/'
  
  #clean----
  az_mgmt <- clean_names(az_mgmt)
  # az_mgmt$parameter <- tolower(az_mgmt$parameter)
  stn_mgmt <- clean_names(stn_mgmt)
  
  #redefine input parameters, prevent conflict with colnames
  parameter_select <- tolower(parameter)
  airzone_select <- airzone
  year_select <- year
  
  parameter <- NULL
  #filter the parameter
  az_mgmt <- filter(az_mgmt,parameter == parameter_select)
  stn_mgmt <- filter(stn_mgmt,parameter == parameter_select)
  
  
  #remove the tfee, select only needed for az_mgmt
  if ('tfee' %in% colnames(az_mgmt)) {
    az_mgmt <- az_mgmt %>%
      filter((tfee & parameter %in% c('pm25','o3')) |
               (!tfee & parameter %in% c('no2','so2'))) %>%
      select(-tfee)
  }
  
  
  message(paste('getting map for:',parameter_select))
  
  
  # -cleanup station
  #ensure all sites and location are the same
  df_fill_stn <- stn_mgmt %>%
    select(site,label,lat,lon) %>%
    distinct() %>%
    group_by(site) %>% slice(1)
  
  stn_mgmt <- stn_mgmt %>%
    select(-lat,-lon) %>%
    left_join(df_fill_stn, by=c('site','label'))
  
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
  print('test')
  print(str(az_mgmt))
  # -add pop-up
  
  # convert to displayable parameter
  parameter_display <- parameter_select
  parameter_display <- gsub('pm25','PM<sub>2.5</sub>',parameter_display,ignore.case = TRUE)
  parameter_display <- gsub('o3','O<sub>3</sub>',parameter_display,ignore.case = TRUE)
  parameter_display <- gsub('no2','NO<sub>2</sub>',parameter_display,ignore.case = TRUE)
  parameter_display <- gsub('so2','SO<sub>2</sub>',parameter_display,ignore.case = TRUE)
  
  az_mgmt <- az_mgmt %>%
    ungroup() %>%
    mutate(index = 1:n()) %>%
    group_by(index) %>%
    mutate(hover_txt =HTML(paste0('<p>',airzone,
                                  ' Air Zone<br>',
                                  parameter_display,' Management Level:',
                                  toupper(colour_text),'</p>',sep=''))) %>%
    ungroup() %>% select(-index)
  
  
  az_mgmt <- az_mgmt %>%
    filter(year == year_max,
           parameter == parameter_select) %>%
    left_join(az_mgmt_polygon)
  
  #process station----
  #create a list of stations with that parameter
  #selected stations that are around within the airzone reporting period
  
  # -this selects or filters stations,removes those not enough data
  # -we'll have to include core sites, important stations
  
  # -list as of 2023-12-01
  lst_core_stations <- c('Fort St John Key Learning Centre',
                         'Kelowna',
                         'Kamloops Federal Building',
                         'Quesnel',
                         'Prince George Plaza 400','Vernon Science Centre',
                         'Victoria Topaz',
                         'Williams Lake Columneetza School')
  
  lst_stns <- stn_mgmt %>%
    filter(year %in% (year_select-2):year_select |
             site %in% lst_core_stations) %>%
    select(site,label,parameter,lat,lon) %>%
    distinct()
  
  #get the correct management level for stations
  
  #colour codes and url links
  #add the gray NA value
  df_colour <- stn_mgmt %>%
    select(colour, colour_text,colour_order) %>%
    distinct() %>%
    mutate(marker_url = 
             paste(asset_url,'marker_',colour_text,'.svg',sep='')) %>%
    bind_rows(tibble(
      colour = '#666565',
      colour_order = 0,
      colour_text = 'N/A',
      marker_url = paste(asset_url,'marker_grey.svg',sep='')
    )
    )
  
  #tfee adjustment applies to pm2,5 and o3
  #include all stations reporting data since 2011
  stn_mgmt_mapped <- stn_mgmt %>%
    filter((tfee & parameter %in% c('pm25','o3')) |
             (!tfee & parameter %in% c('no2','so2'))) %>%
    filter(year %in% year_select) %>%
    group_by(site,label,parameter) %>%
    summarise(colour_order = max(colour_order,na.rm = TRUE)) 
  
  #include all other stations that has not reported data
  stn_mgmt_mapped <- lst_stns %>%
    left_join(stn_mgmt_mapped, by=c('site','label','parameter'))
  
  stn_mgmt_mapped$colour_order[is.na(stn_mgmt_mapped$colour_order)] <- 0
  
  stn_mgmt_mapped <- stn_mgmt_mapped %>%
    left_join(df_colour, by = 'colour_order')
  
  
  #select for the parameter
  stn_mgmt_mapped <- stn_mgmt_mapped %>%
    filter(parameter == parameter_select) %>%
    arrange((colour_order))
  
  
  
  #map----
  # - create icons
  icon_size <- 30
  markers <- icons( # file locations have to be relative (can't use here())
    iconUrl = stn_mgmt_mapped$marker_url,
    iconWidth = icon_size, iconHeight = icon_size,
    iconAnchorX = icon_size/2, iconAnchorY = icon_size,
    shadowUrl = paste(asset_url,"marker_shadow.svg",sep=''),
    shadowWidth = icon_size * 0.75, shadowHeight = icon_size * 0.75,
    shadowAnchorX = 1, shadowAnchorY = icon_size * 0.75)
  
  # Popup Options
  ppo <- popupOptions(autoPanPaddingTopLeft = c(10, 10),
                      autoPanPaddingBottomRight = c(10, 400),
                      closeOnEscapeKey = TRUE, 
                      keepInView = TRUE)
  # - create initial map
  # Management labels
  labels_mgmt <- rcaaqs::management_levels %>%
    filter(str_detect(parameter, "pm2.5")) %>%
    select(labels, colour, colour_text, units_html) %>%
    distinct() %>%
    mutate(icons = paste0("assets/marker_", colour_text, ".svg"),
           text_colour = c("white", "black", "black", "white", "white"))
  
  message('displaying map')
  print(unique(az_mgmt$parameter))
  a <-  leaflet(width = size[1],height = size[2],
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
    
    leaflet::addMarkers(data = stn_mgmt_mapped,
                        icon = markers, label = ~label,
                        # popup = ~popup,
                        # Stick to marker, not mouse
                        labelOptions = labelOptions(sticky = FALSE,
                                                    offset = c(0, -icon_size/2))
    ) %>%
    #                       # Legend
    addLegend("bottomleft", group = "Annual",
              data = stn_mgmt_mapped,
              # Ensure we get all levels
              colors = rev(labels_mgmt$colour),
              labels = rev(labels_mgmt$labels),
              opacity = 1,
              title = htmltools::HTML(
                "<h3>Management Levels</h3>"))
  
  
  return(a)
  
  # 
  # #add for selected airzone
  # if (!is.null(airzone_select) & !is.null(polygon_a)) {
  #   message(paste('updating,highlighting area',airzone_select))
  #   a <- polygon_a %>%
  #     addPolylines(data = df$geometry[df$airzone == airzone_select],
  #                  layerId = 'selectedairzone',
  #                  color = 'blue',weight = 5)
  # }
  
  #slider actions
  #need to refresh the air zone colours
  # if (is.null(airzone_select) & !is.null(polygon_a)) {
  #   print(paste('updating,highlighting area',airzone_select))
  #   a <- polygon_a %>%
  #     addPolygons(data = df$geometry,
  #                 layerId = df$airzone,
  #                 color = df$colour, weight = 0, opacity = 0, fillOpacity = 0.7,
  #                 label = df$hover_txt,
  #                 labelOptions = labelOptions(textsize = "15px"))%>%
  #     addPolylines(data = df$geometry,
  #                  layerId = 'selectedairzone',
  #                  color = 'blue',weight = 0)
  # }
  
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


#initial values---------
pollutant_initial <- df_parameter$display[1]
year_initial <- max(df_management_airzones$year)
tfee_initial <- TRUE

#---SHINY SECTION----------------
# Actual shiny part
#ui----
{
  ui <-
    
    # -heading
    (fluidPage(
      # h4(HTML(paste('Air Quality Management Levels'))),
      # h4(HTML(uiOutput("text_header"))),
      uiOutput('text_header'),
      h6('Map markers are monitoring stations. Click for details on their management levels.'),
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
                        # -tab selecting the pollutant, initial is PM2.5
                        # h6('Click to select pollutant and display management level'),
                        column(12, align = "center",
                               
                               actionButton(inputId = 'pm25',label = HTML('PM<sub>2.5</sub>'),width = "12.5%",title = 'Central Interior Air Zone'),
                               actionButton(inputId = 'o3',label = HTML('O<sub>3</sub>'),width = "12.5%",title = 'Coastal Air Zone'),
                               actionButton(inputId = 'no2',label = HTML('NO<sub>2</sub>'),width = "12.5%",title = 'Georgia Strait Air Zone'),
                               actionButton(inputId = 'so2',label = HTML('SO<sub>2</sub>'),width = "12.5%",title = 'Lower Fraser Valley Air Zone'),
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
               leaflet::leafletOutput("map",width = "100%"),
        ),
      ),
      
      fluidRow(column(9,downloadLink('downloadData2', 'Download Air Zone Management Level'))),
      fluidRow(column(9,downloadLink('downloadData', 'Download Station Data')))
      
      
      
    ))
  
}


#server----
{
  server <- shinyServer(function(input, output,session) {
    
    # configure input options-------
    parameter_select <- reactiveValues(param = NULL)
    
    
    #initial value----
    observe({
      parameter_select$param <- list(parameter = 'pm25')
      output$text_header <- renderUI({h4(HTML(
        ('Air Quality Management Level for PM<sub>2.5</sub>')
      ))})
    })
    
    airzone_select <- NULL
    message('before map rendering')
    message(nrow(df_stn_mgmt))
    
    #initial map
    a <-    map_mgmt(stn_mgmt = df_stn_mgmt,az_mgmt = df_az_mgmt,parameter = 'pm25',
                     year = rep_year)
    output$map <- renderLeaflet(a)
    
    
    
    
    # - data downloads-----
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
    
    
    # -action items---------
    
    ##  pressing pm2.5, o3, no2, so2 buttons-----
    observeEvent(input$pm25, {
      message('pm25 clicked')
      parameter_select$param$parameter <- 'pm25'
      output$text_header <- renderUI({h4(HTML(
        ('Air Quality Management Level for PM<sub>2.5</sub>')
      ))})
      a <-    map_mgmt(stn_mgmt = df_stn_mgmt,az_mgmt = df_az_mgmt,parameter = 'pm25',
                       year = rep_year)
      output$map <- renderLeaflet(a)
    })
    observeEvent(input$o3, {
      message('o3 clicked')
      output$text_header <- renderUI({h4(HTML(
        ('Air Quality Management Level for O<sub>3</sub>')
      ))})
      parameter_select$param$parameter <- 'o3'
      a <-    map_mgmt(stn_mgmt = df_stn_mgmt,az_mgmt = df_az_mgmt,parameter = 'o3',
                       year = rep_year)
      output$map <- renderLeaflet(a)
    })
    observeEvent(input$no2, {
      message('no2 clicked')
      output$text_header <- renderUI({h4(HTML(
        ('Air Quality Management Level for NO<sub>2</sub>')
      ))})
      parameter_select$param$parameter <-  'no2'
      a <-    map_mgmt(stn_mgmt = df_stn_mgmt,az_mgmt = df_az_mgmt,parameter = 'no2',
                       year = rep_year)
      output$map <- renderLeaflet(a)
    })
    observeEvent(input$so2, {
      message('so2 clicked')
      output$text_header <- renderUI({h4(HTML(
        ('Air Quality Management Level for SO<sub>2</sub>')
      ))})
      parameter_select$param$parameter <-  'so2'
      a <-    map_mgmt(stn_mgmt = df_stn_mgmt,az_mgmt = df_az_mgmt,parameter = 'so2',
                       year = rep_year)
      output$map <- renderLeaflet(a)
      
    })
    
    
    # -clicking on a marker-----
    observeEvent(input$map_marker_click,{
      pressed <- NULL
      pressed <-input$map_marker_click
      print('marker clicked')
      
      print(str(pressed$lat))
      print((pressed$lat))
      print((pressed$lng))
      
      # -marker is selected if true
      if (is.numeric(pressed$lat)){
        print(nrow(df_stn_mgmt))
        
        parameter_select <- parameter_select$param$parameter
        str(parameter_select)
        
        #calculate and determine closest marker to clicked item
        # -looks like longiture is only basis for click proximity
        # -due to icon offsets on the latitude
        
        df_detail <- df_stn_mgmt %>%
          filter(parameter == parameter_select) %>%
          mutate(dlat = abs(lat - pressed$lat),
                 dlon = abs(lon - pressed$lng)
          ) %>%
          mutate(delta = (0*dlat^2 + dlon^2)) %>%
          arrange((delta))
        print(df_detail$site[1])
        
        # -display the management trend
        # -management graphics are here: graph_mgmt_trends
        
        # -determine the url of image
        img_url <- paste(dir_plots,df_detail$site[1],'_',parameter_select,'.svg',sep='')
        message(paste('Retriving image from:',img_url))
        
        # -prepare display, heading to have subscripts for paramter
        title_display <- paste(df_detail$label[1],' Management Level for ',parameter_select,sep='')
        title_display <- gsub('pm25','PM<sub>2.5</sub>',title_display,ignore.case = TRUE)
        title_display <- gsub('o3','O<sub>3</sub>',title_display,ignore.case = TRUE)
        title_display <- gsub('no2','NO<sub>2</sub>',title_display,ignore.case = TRUE)
        title_display <- gsub('so2','SO<sub>2</sub>',title_display,ignore.case = TRUE)
        
        showModal(
          modalDialog(
            title = HTML(title_display),
            tags$img(src = img_url,
                     width = "100%"),
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
        
      }
      
    })
    
    
    # -clicking on air zone-----
    # -currently no action applied, but can add popup modal action
    observeEvent(input$map_shape_click, {
      
      p <- input$map_shape_click
      
      try({
        airzone_select <- get_airzone(p$lat,p$lng)
        airzone_select <- airzone_select[1]
        print(p$lat)
        print(p$lng)
        print(airzone_select)
        parameter_select$param$airzone = airzone_select
        parameter_select <- parameter_select$param$parameter
        
        if (!is.null(airzone_select)){
          # -prepare display, heading to have subscripts for paramter
          title_display <- paste(airzone_select,' Air Zone Overall Mangement Level for ',parameter_select,sep='')
          title_display <- gsub('pm25','PM<sub>2.5</sub>',title_display,ignore.case = TRUE)
          title_display <- gsub('o3','O<sub>3</sub>',title_display,ignore.case = TRUE)
          title_display <- gsub('no2','NO<sub>2</sub>',title_display,ignore.case = TRUE)
          title_display <- gsub('so2','SO<sub>2</sub>',title_display,ignore.case = TRUE)
          
          message(title_display)
          # showModal(
          #   modalDialog(
          #     title = HTML(title_display),
          #     tags$img(src = 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/data/plots/Abbotsford%20A%20Columbia%20Street_no2.svg',
          #              width = "100%"),
          #     footer = modalButton("Close"),
          #     size = "l"
          #   )
          # )
          
        }
        
      })
    })
    
    
    
    
    
  })
}
shinyApp(ui, server)


