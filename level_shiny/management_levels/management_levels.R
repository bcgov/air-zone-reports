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


dirs_location <- 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/data'  #local location, two dots for final, one dot for debug
if (0) {
  dirs_location <- './data/out'
}


df_management_airzones <- readr::read_csv(paste(dirs_location,'out/management_airzones.csv',sep='/'))
az_mgmt <- readr::read_rds(paste(dirs_location,'out/az_mgmt.Rds',sep='/'))
df_stations <- readr::read_rds(paste(dirs_location,'out/liststations_merged.Rds',sep='/'))
df_caaqs_results <- readr::read_csv(paste(dirs_location,'out/caaqs_results.csv',sep='/'))
graph_barcaaqs <- read_rds(paste(dirs_location,'out/caaqs_bargraph.Rds',sep='/'))
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

year_max <- max(df_caaqs_results$year)

#create a catalogue of the graphs

lst_graphs <- names(graph_barcaaqs)
lst_airzone <- az_mgmt %>% select(airzone) %>% distinct %>% pull(airzone)

df_datacatalogue <- tibble(
  parameter = c('PM25','PM25','O3','NO2','NO2','SO2','SO2'),
  metric = c('Annual','24-Hour','8-Hour','Annual','1-Hour','Annual','1-Hour')
)




#' Function that retrieves the names of the graph on the list
#' 
get_datacatalogue <- function(parameter, metric, airzone, year,lst_graphs) {
  
  #note to remove valley from LFV
  #tp prevent confusing with ALL
  lst_airzone <- c("ALL","Central Interior","Coastal","Georgia Strait",     
                   "Lower Fraser Valley","Northeast","Northwest","Southern Interior" )

  if (0) {
    parameter <- 'PM25'
    metric <- 'annual'
    airzone <- 'all'
    airzone <- 'Northwest'
    year <- 2021
    lst_graphs <- lst_graphs
  }

  lst_metric <- tibble(
     metric = c('annual','1-hour','1-hour','1-hour','8-hour','8-hour','8-hour','24-hour','24-hour'),
     altname = c('ann','1hr','1h','1-hr','8hr','8h','8-hr','24hr','24h')
  )
  flt_metric <- c(metric,lst_metric$altname[lst_metric$metric == metric])
  lst_ <- tolower(lst_graphs)
  lst_ <- lst_[grepl(parameter,lst_,ignore.case = TRUE)]
  
  lst_ <- lst_[grep(paste(flt_metric, collapse = "|"), lst_,lst_graphs,ignore.case = TRUE)]
  
  airzone <- strsplit(airzone,' ')[[1]][1]
  
  lst_ <- lst_[grepl(airzone,lst_,ignore.case = TRUE)]
  lst_ <- lst_[grepl(year,lst_,ignore.case = TRUE)][1]
  lst_graphs_ <- lst_graphs[grepl(lst_,lst_graphs,ignore.case = TRUE)]
  print(paste('get datacatalogue:',lst_graphs_))
  if (all(is.na(lst_graphs_))) {
    return(NULL)
  }
  return(lst_graphs_)
  }

#catalogue the graphs 


#define functions---------------
{
#define constants such as parameter and 
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
#' @param data_path is the path where the data files are located
#' @param parameter is the pollutant
#' @param year is the data year
#' @param tfee is boolean if wildfire or TFEE are included or not
#' @param polygon_a is the polygon. Prevents redrawing
map_airzone <- function(polygon_a = NULL,df,az_mgmt,parameter,year,
                        tfee = NULL,size = c('200px','400px'),
                        airzone = NULL) {
  
  
  
  if (0) {
    data_path =NULL
    parameter <- 'O3'
    year <- 2019
    tfee <- TRUE
    size <- c('200px','400px')
    polygon_a = NULL
    airzone <- 'all'
    df <- readr::read_csv('./data/out/management.csv')
  }
  
  try(if (tolower(airzone) == 'all') {
    airzone <- NULL
  })
  parameter <- toupper(df_parameter$parameter[df_parameter$display == parameter])
  airzone_select <- airzone
  print(paste('map_airzone function',is.null(polygon_a)))
  
  if (is.null(tfee)) {
    tfee <- TRUE
  }
  
  year_select <- year
  tfee_select <- tfee
  
  
  #include all tfee options for NO2, SO2,
  #there is no TFEE adjustment
  if (parameter %in% c('NO2','SO2')) {
    tfee_select <- FALSE
  }
  
  
  
  df <- az_mgmt %>%
    left_join(
      df %>% 
        filter(pollutant == parameter,
               year == year_select,
               tfee == tfee_select),
      by='airzone'
    )
  
  
  
  
  df$colour[is.na(df$colour)] <- '#666565'  #grey colour for air zones with no value
  
  #add pop-up
  df <- df %>%
    ungroup() %>%
    mutate(index = 1:n()) %>%
    group_by(index) %>%
    mutate(hover_txt =HTML(paste0('<p>',airzone,
                                  ' Air Zone<br>Management Level:',
                                  toupper(colour_text),'</p>',sep=''))) %>%
    ungroup() %>% select(-index)
  
  
  
  
  if (is.null(polygon_a)) {
    a <- leaflet(width = size[1],height = size[2],
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
      addPolygons(data = df$geometry,
                  layerId = df$airzone,
                  color = df$colour, weight = 0, opacity = 0, fillOpacity = 0.7,
                  label = df$hover_txt,
                  labelOptions = labelOptions(textsize = "15px")) %>%
      addPolylines(data = df$geometry,
                   layerId = paste(df$airzone,'01'),
                   color = 'black',weight = 1) 
    
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



#initial values---------
pollutant_initial <- df_parameter$display[1]
year_initial <- max(df_management_airzones$year)
tfee_initial <- TRUE
}
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
                             selectizeInput('pollutant',label = 'Pollutant:',
                                            choices = df_parameter$display,width = "5%")
                      ),
                      
                      column(6,
                             sliderInput('year_slider',label ='Year',
                                         min = min(df_management_airzones$year),
                                         max = year_max,
                                         value = year_initial,width = "50%",
                                         sep='')
                      )
                      
                      )),
    
    fluidRow(
      
      column(3,fluidRow(
        column(3,actionButton('rst','Reset')),
        column(8,("Use map to select and view air zone data"))
             
             ),
             leaflet::leafletOutput("map",height = '400px'),
             DT::dataTableOutput("table1",width = '90%', height = '400px')),
      column(9,
             #h6("Use vertical scrollbar (right side of graph) to reveal more bar graphs."),
             fluidRow(
               column(3,actionButton('button','Switch to 24-Hour Metric')),
               column(9,htmlOutput("Title"))
               ),
             (div(style='height:800px;overflow-y: scroll;',
                  plotlyOutput("plot1",height = "800px"))))
      
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
  

  observeEvent(input$rst, {
    print("reset")
    leafletProxy("map") %>%
      map_airzone(df=df_management_airzones,az_mgmt = az_mgmt,
                  parameter = input$pollutant,
                  year = input$year_slider,
                  
                  airzone = NULL)
    
    parameter_select$param$airzone <- 'ALL'
    
      
   
  })
  
  
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
  
  observeEvent(input$year_slider,
               {
                 print('Slider')
                 if (!is.null(airzone_select)) {
                   print(airzone_select)
                 }
                 parameter_select$param$year = input$year_slider
                 leafletProxy("map") %>%
                   map_airzone(df=df_management_airzones,az_mgmt = az_mgmt,
                               parameter = input$pollutant,
                               year = input$year_slider,
                               
                               airzone = NULL)
                 leafletProxy("map") %>%
                   map_airzone(df=df_management_airzones,az_mgmt = az_mgmt,
                               parameter = input$pollutant,
                               year = input$year_slider,
                               
                               airzone = parameter_select$param$airzone)
                 
                 
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


