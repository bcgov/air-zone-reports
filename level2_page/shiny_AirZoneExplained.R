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


#----FUNCTIONS--------------
#' Create a graph of the airzones
#' This is for information only, to discuss the air zones
#' No content detail included
graph_airzone <- function(polygon_a = NULL,airzone=NULL, size = c("900px","700px")) {
  
  markerON <- FALSE
  
  
  
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
  
  if (is.null(polygon_a)) {
    
    
    liststations <- liststations0 %>%
      mutate(AQMS = ifelse(is.na(AQMS),'UNKNOWN',AQMS)) %>%
      select(Label,LAT,LONG,STATUS,AQMS,AIRZONE) %>%
      filter(AQMS != 'N',STATUS == 'ACTIVE') %>%
      group_by(Label) %>%
      dplyr::mutate(index = 1:n()) %>%
      filter(index ==1) %>% select(-index) %>%
      ungroup()
    
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
                       # layerId = liststations$AIRZONE,
                       # group = airzone_,
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
  www_git_url <- 'https://github.com/bcgov/air-zone-reports/raw/master/level2_page/www/'
  
  #debug for use of local files
  if (0) {
    www_git_url <- './www/'
  }
  
  #t_ready means it is ready to receive clicks
  a <- graph_airzone(polygon_a=NULL,airzone=NULL,size = c('200px','400px'))
  output$map <- renderLeaflet(a)
  output$fileOutputs  <- renderUI({
    file <- paste(www_git_url,'01_airzone_intro.html',sep='')
    includeHTML(file)
  })
  
  v <- reactiveValues(data = '')
  
  update <- function(airzone_selected = '') {
    leafletProxy("map")%>%
      graph_airzone(airzone=airzone_selected,size = c('200px','400px'))
    #insert text here  
    html_file <- paste(stringr::str_replace_all(string=airzone_selected, pattern=" ", repl=""),'.html',sep='')
    html_file <- paste(www_git_url,html_file,sep='')
    print(html_file)
    
    try({output$fileOutputs  <- renderUI({
      file <- html_file
      includeHTML(file)
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
        html_file <- paste(www_git_url,html_file,sep='')
        print(html_file)
        
        try({output$fileOutputs  <- renderUI({
          file <- html_file
          includeHTML(file)
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
      file <- paste(www_git_url,'01_airzone_intro.html',sep='')
      includeHTML(file)
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
