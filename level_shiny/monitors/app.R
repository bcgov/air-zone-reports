library(leaflet)
library(shiny)
library(dplyr)
require(rcaaqs)
require(envreportutils)
require(sf)
require(bcmaps)
require(stringr)
# library(kableExtra)

az_mgmt_gitURL <- 'https://github.com/bcgov/air-zone-reports/blob/master/data/out/az_mgmt.Rds?raw=true'
liststations_URL <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/liststations.csv'
# 
az_mgmt0 <- readRDS(url(az_mgmt_gitURL))
liststations0 <- readr::read_csv(liststations_URL)


az_mgmt_gitURL <- 'https://github.com/bcgov/air-zone-reports/blob/master/data/out/az_mgmt.Rds?raw=true'
liststations_URL <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/liststations.csv'
results_URL <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/annual_results.csv'
liststationsaqhi_URL <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/liststations_aqhi.csv'
#define station icons
icons_URL <- 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/assets/photos'
buttons_URL <- 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/assets/photos'
# print(list.files(buttons_URL))
# Define icon and popup anchor points
icon_anchor <- c(10, 40)
popup_anchor <- c(0, -50)
icon_wh <- c(20,40)

#define graphic components (icons and buttons)
df_icons <- tibble(
  OWNER = c("MVRD","INDUSTRY","ENV","INDUSTRY-BCH","INDUSTRY-PRPA","AQHI"),
  icons = c("station_mvrd_icon.png",'station_industry_icon.png',
            'station_env_icon.png','station_bchydro_icon.png','station_prpa_icon.png','station_aqhi_icon.png')
)

df_buttons <- tibble(
  AIRZONE = c("All",'Central Interior','Coastal','Georgia Strait',
              'Lower Fraser Valley','Northeast','Northwest','Southern Interior'),
  buttons = c('btn_all.png','btn_central.png','btn_coastal.png','btn_georgia.png','btn_lfv.png',
              'btn_northeast.png','btn_northwest.png','btn_southern.png')
)

df_icons$icons <- paste(icons_URL,df_icons$icons,sep='/')
df_buttons$buttons <- paste(buttons_URL,df_buttons$buttons,sep='/')
station_data <-  readr::read_csv(results_URL) %>%
  filter(year>=2020) %>%
  select(site) %>% distinct()

# aqhi_stations <- readr::read_csv('https://envistaweb.env.gov.bc.ca/aqo/setup/BC_AQHI_SITES_AQHIPlusSO2.csv')

liststations_aqhi <- readr::read_csv(liststationsaqhi_URL) %>%
  mutate(icons = df_icons$icons[df_icons$OWNER == 'AQHI']) %>%
  rename(AIRZONE = airzone)
liststations <- readr::read_csv(liststations_URL) %>%
  # envair::listBC_stations(use_CAAQS = TRUE) %>%
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
  ungroup() %>%
  mutate(index = 1:n()) %>%
  left_join(df_icons,by='OWNER') %>%
  mutate(display = paste(Label,'<br>','Monitors:<br>',monitors,'<br><img src="',
                         icons,'" alt = "operator" style ="width:50px;height:50px">',sep=''))


#define centroid and zoom for each airzone
#when the airzone is selected, it will zoom into the air zone
df_map_settings <- tribble(
  ~AIRZONE,~lat,~long,~zoom,
  'Central Interior',53.95,-123.58,5,
  'Coastal',52.93,-128.08,5,
  'Georgia Strait',49.54,-123.86,7,
  'Lower Fraser Valley',49.51,-122.12,7.5,
  'Northeast',58.240,-122.44,5,
  'Northwest',58.699,-129.547,5,
  'Southern Interior',50.596,-118.608,5
)

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
  
  
  #----------------
  pnts <- data.frame(
    "x" = long,
    "y" = lat)
  
  # create a points collection
  pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(pnts), 
                                       function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 
  
  pnts_trans <- st_transform(pnts_sf, 2163) # apply transformation to pnts sf
  tt1_trans <- st_transform(az_mgmt0, 2163)      # apply transformation to polygons sf
  
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
  
  fluidRow(class = 'toprow',
           
           h6("Select an Air Zone or Click on Map",style = "color:white"),
           column(12, align = "left",
                  # actionButton(inputId = 'reset',label = 'Reset'),
                  # tags$button(
                  #   id = "centralinterior",
                  #   class = "btn action-button",
                  #   tags$img(src = df_buttons$buttons[df_buttons$AIRZONE == 'Central Interior'],height = '50px',width ='100px'),
                  #   
                  # ),
                  # actionButton(inputId = 'centralinterior',class = "action-button",label = '',
                  #              style = "background-image: url('https://raw.githubusercontent.com/bcgov/air-zone-reports/master/assets/photos/btn_central.png');,
                  #              height = 50, width = 50, style = border:none; padding:0; margin: 0"
                  #              ),
                  actionButton(inputId = 'reset',label = 'Reset',width = "7.5%",title = 'Reset map'),
                  actionButton(inputId = 'centralinterior',label = 'Central',width = "12.5%",title = 'Central Interior Air Zone'),
                  actionButton(inputId = 'coastal',label = 'Coastal',width = "12.5%",title = 'Coastal Air Zone'),
                  actionButton(inputId = 'georgiastrait',label = 'Georgia',width = "12.5%",title = 'Georgia Strait Air Zone'),
                  actionButton(inputId = 'lowerfraservalley',label = HTML('LFV'),width = "12.5%",title = 'Lower Fraser Valley Air Zone'),
                  actionButton(inputId = 'northeast',label = 'Northeast',width = "12.5%",title = 'Northeast Air Zone'),
                  actionButton(inputId = 'northwest',label = 'Northwest',width = "12.5%",title = 'Northwest Air Zone'),
                  actionButton(inputId = 'southerninterior',label = 'Southern',width = "12.5%",title = 'Southern Interior Air Zone')
           )),
  sidebarLayout(
    sidebarPanel(h6("Click on the map to select an air zone"),leafletOutput("map"),width=4),
    mainPanel (
      uiOutput("fileOutputs"),width=6
    ))
  
)



server <- shinyServer(function(input, output) {
  
  
  #initial map
  # www_git_url <- 'https://github.com/bcgov/air-zone-reports/raw/master/level2_page/www/'
  www_git_url <- 'https://github.com/bcgov/air-zone-reports/raw/master/level_shiny/monitors/www/'
  #debug for use of local files
  if (0) {
    www_git_url <- './www/'
  }
  
  #t_ready means it is ready to receive clicks
  a <- graph_airzone(polygon_a=NULL,airzone=NULL,size = c('200px','400px'))
  output$map <- renderLeaflet(a)
  output$fileOutputs  <- renderUI({
    html_file <- paste(www_git_url,'station_intro.html',sep='')
    # includeHTML(html_file)
    tags$div(style = "height: 500px; overflow: auto;",includeHTML(html_file))
  })
  
  v <- reactiveValues(data = '')
  
  update <- function(airzone_selected = '') {
    leafletProxy("map")%>%
      graph_airzone(airzone=airzone_selected,size = c('200px','400px'))
    #insert text here  
    html_file <- paste(stringr::str_replace_all(string=airzone_selected , pattern=" ", repl=""),'.html',sep='')
    html_file <- paste(www_git_url,'station_',html_file,sep='')
    print(html_file)
    
    try({output$fileOutputs  <- renderUI({
      
      # html_file <- paste(www_git_url,'station_intro.html',sep='')
      # includeHTML(html_file)
      tags$div(style = "height: 500px; overflow: auto;",includeHTML(html_file))
    })
    })
  }
  
  # t0 <- Sys.time()
  # observe the marker click info and print to console when it is changed.
  observeEvent(input$map_shape_click, {
    
    
    p <- input$map_shape_click # map because that is the name of the output
    
    print(p$lat)
    # text <- paste("lat ", p$lat," lon ", p$lng) # shows lat/lon in console
    try({
      airzone_select <- get_airzone(p$lat,p$lng)
      
      # if (airzone_select != airzone_select_previous) 
      {
        
        # map redrawn after click
        leafletProxy("map")%>%
          graph_airzone(airzone=airzone_select,size = c('200px','400px'))
        #insert text here  
        html_file <- paste(stringr::str_replace_all(string=airzone_select, pattern=" ", repl=""),'.html',sep='')
        html_file <- paste(www_git_url,'station_',html_file,sep='')
        print(html_file)
        
        try({output$fileOutputs  <- renderUI({
          
          # html_file <- paste(www_git_url,'station_intro.html',sep='')
          # includeHTML(html_file)
          tags$div(style = "height: 500px; overflow: auto;",includeHTML(html_file))
        })
        })
        
      } 
    })
  })
  
  #observeevents for the shortcut buttons
  
  observeEvent(input$reset, {
    print('reset button clicked')
    v$data <- ''
    a <- graph_airzone(polygon_a=NULL,airzone=NULL,size = c('200px','400px'))
    output$map <- renderLeaflet(a)
    output$fileOutputs  <- renderUI({
      html_file <- paste(www_git_url,'01_airzone_intro.html',sep='')
      # includeHTML(html_file)
      tags$div(style = "height: 500px; overflow: auto;",includeHTML(html_file))
    })
    
  })
  
  observeEvent(input$centralinterior, {
    v$data <- 'Central Interior'
    update(v$data)  
    
  })
  observeEvent(input$coastal, {
    v$data <- 'Coastal'
    
    update(v$data)    
    
  })
  
  observeEvent(input$georgiastrait, {
    v$data <- 'Georgia Strait'
    
    update(v$data)    
    
  })
  
  observeEvent(input$lowerfraservalley, {
    v$data <- 'Lower Fraser Valley'
    
    update(v$data)    
    
  })
  
  observeEvent(input$northeast, {
    v$data <- 'Northeast'
    
    update(v$data)    
    
  })
  observeEvent(input$northwest, {
    v$data <- 'Northwest'
    
    update(v$data)    
    
  })
  observeEvent(input$southerninterior, {
    v$data <- 'Southern Interior'
    
    update(v$data)    
    
  })
})

shinyApp(ui, server)
