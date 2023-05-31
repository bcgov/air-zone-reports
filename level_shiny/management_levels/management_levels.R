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


dirs_location <- 'https://raw.githubusercontent.com/bcgov/air-zone-reports/master/data/out'  #local location, two dots for final, one dot for debug
if (0) {
  dirs_location <- './data/out'
}
df_management_airzones <- readr::read_csv(paste(dirs_location,'management_airzones.csv',sep='/'))
az_mgmt <- readr::read_rds(paste(dirs_location,'az_mgmt.Rds',sep='/'))
df_stations <- readr::read_rds(paste(dirs_location,'liststations_merged.Rds',sep='/'))
df_caaqs_results <- readr::read_csv(paste(dirs_location,'caaqs_results.csv',sep='/'))

year_max <- max(df_caaqs_results$year)
#define functions---------------

df_parameter <- tribble(
  ~display, ~parameter,
  'PM\u2082.\u2085','pm25',
  'O\u2083','o3',
  'NO\u2082','no2',
  'SO\u2082','so2'
)

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
    airzone <- NULL
    df <- readr::read_csv('./data/out/management.csv')
  }
  
  
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

#' Create a ranked bar graph for each metric
#'
#' @param df_caaqs_results is a dataframe containing caaqs results.
#' These are created by the create_caaqs_annual() function
#' @param pollutant is either pm25,o3,no2,so2
#' @param year is the year to display
#' @param airzone is the airzone that will be displayed. If NULL, it will include all airzones
#' @param df_stations is a dataframe containing the list of stations.
#' If NULL, it will retrieve using the listBC_stations() function
plot_bar_ranked <- function(df_caaqs_results,pollutant,year,airzone = NULL,df_stations = NULL) {
  if (0) {
    df_caaqs_results <- readr::read_csv('./data/out/caaqs_results.csv')
    df_caaqs_results1 <- readr::read_csv('../test_data/caaqs_results.csv')
    
    # metric_ <- c('pm25_annual')
    # metric_ <- c('pm25_24h')
    airzone <- 'Central Interior'
    df_stations = NULL
    year <- 2021
    pollutant <- df_parameter$display[1]
    df_stations <- df_stations
    a <- plot_bar_ranked(df_caaqs_results,pollutant,2021,airzone= 'Central Interior',df_stations)
    
  }
  
  library(patchwork)
  print(paste('plot_bar_ranked() function. pollutant=',pollutant))
  
  pollutant <- toupper(df_parameter$parameter[df_parameter$display == pollutant])
  pollutant_filter <- pollutant
  print(pollutant_filter)
  #list of metrics
  df_metric <- tribble(
    ~pollutant,~parameter,~metric,
    'PM25','pm2.5_annual','pm25_annual',
    'O3','o3','o3_8h',
    'PM25','pm2.5_24h','pm25_24h',
    'NO2','no2_1yr','no2_ann',
    'NO2','no2_3yr','no2_1hr',
    'SO2','so2_1yr','so2_ann',
    'SO2','so2_3yr','so2_1hr'
  ) %>%
    dplyr::filter(tolower(pollutant) == tolower(pollutant_filter))
  
  #escape for Northwest air zone
  #create a blank plot
  try(
    if (airzone == 'Northwest') {
      
      a <- ggplot() +
        scale_x_continuous(limits = c(0,1)) +
        scale_y_continuous(limits = c(0,1)) +
        theme(legend.position = 'none',axis.text = element_blank(),
              panel.background = element_blank(),
              panel.border = element_rect(fill=NA, colour='black'),
              axis.title.x = element_blank(),axis.title.y = element_blank()) +
        annotate('text',x=0,y=1,label = 'Northwest air zone does \nnot have enough data',
                 hjust = 0)
      return(a)
      
    })
  
  df_caaqs_results_ <- df_caaqs_results
  year_ <- year
  airzone_ <- airzone
  df_stations_ <- df_stations
  
  print(paste('Metric:',df_metric$metric[1]))
  
  a <- plot_bar_ranked0(df_caaqs_results = df_caaqs_results_,metric = df_metric$metric[1],df_stations = df_stations_,
                        year = year_, airzone = airzone_)
  
  
  if (nrow(df_metric) == 2)
  {
    b <- plot_bar_ranked0(df_caaqs_results = df_caaqs_results_,metric = df_metric$metric[2],df_stations = df_stations_,year = year_, airzone = airzone_)
    b <- b + theme(legend.position = 'bottom')
    a <- a + theme(legend.position = 'none')
    a <- a/b
  }
  return(a)
}



#' Create a ranked bar graph (backend version)
#'
#' This is the back end of the plot_bar_ranked function
plot_bar_ranked0 <- function(df_caaqs_results,metric,year,airzone = NULL,df_stations = NULL) {
  
  if (0) {
    df_caaqs_results <- readr::read_csv('./data/out/caaqs_results.csv')
    metric <- c('pm25_annual')
    airzone <- 'Central Interior'
    df_stations = NULL
    year <- 2015
  }
  
  #plotly version
  df_unit_plotly <- tribble(
    ~metric,~units,
    'pm25_annual',"Annual PM<sub>2.5</sub> Metric(&mu;g/m<sup>3</sup>)",
    'pm25_24h',"24-Hour PM<sub>2.5</sub> Metric(&mu;g/m<sup>3</sup>)",
    'o3_8h',"8-Hour O<sub>3</sub> Metric (ppb)",
    'no2_1hr',"1-Hour NO<sub>2</sub> Metric (ppb)",
    'no2_ann',"Annual NO<sub>2</sub> Metric (ppb)",
    'so2_1hr',"1-Hour SO<sub>2</sub> Metric (ppb)",
    'so2_ann',"Annual SO<sub>2</sub> Metric (ppb)"
  )
  
  #ggplot version
  df_unit <- tribble(
    ~metric,~units,
    'pm25_annual',bquote(~"Average "~PM[2.5]~","~mu~g/m^3),
    'pm25_24h',bquote(~"98th Percentile "~PM[2.5]~","~mu~g/m^3),
    'pm25_ann(1yr)',bquote("Annual "~PM[2.5]~","~mu~g/m^3),
    'pm25_24hr(1yr)',bquote(~"98th Percentile "~PM[2.5]~","~mu~g/m^3),
    'o3_8h',bquote("4th Highest"~O[3]~",ppb"),
    'o3_8h(1yr)',bquote("4th Highest"~O[3]~",ppb"),
    'no2_1hr',bquote("98th Percentile "~NO[2]~",ppb"),
    'no2_ann',bquote("Average "~NO[2]~",ppb"),
    'no2_1hr(1yr)',bquote("98th Percentile "~NO[2]~",ppb"),
    'no2_ann(1yr)',bquote("Average "~NO[2]~",ppb"),
    'so2_1hr',bquote("99th Percentile "~SO[2]~",ppb"),
    'so2_ann',bquote("Average "~SO[2]~",ppb"),
    'so2_1hr(1yr)',bquote("99th Percentile "~SO[2]~",ppb"),
    'so2_ann(1yr)',bquote("Average "~SO[2]~",ppb")
  )
  
  #define the CAAQS and the axis scale limits for display purposes
  #includes 2015 and 2020 CAAQS, based on the year
  if (year>=2020) {
    df_axis <- tribble(
      ~metric,~caaqs,~lbl_caaqs,~xmin,~xlab,
      'pm25_annual',8.8,'2020 CAAQS',10,9.5,
      'pm25_24h',27,'2020 CAAQS',30,29,
      'o3_8h',62,'2020 CAAQS',70,64,
      'no2_1hr',60,'2020 CAAQS',70,62,
      'no2_ann',17,'2020 CAAQS',20,19,
      'so2_1hr',70,'2020 CAAQS',80,72,
      'so2_ann',5,'2020 CAAQS',10,6
    )
  } else {
    df_axis <- tribble(
      ~metric,~caaqs,~lbl_caaqs,~xmin,~xlab,
      'pm25_annual',10,'2015 CAAQS',10,9.5,
      'pm25_24h',28,'2015 CAAQS',30,29,
      'o3_8h',63,'2015 CAAQS',70,64,
      'no2_1hr',60,'2020 CAAQS',70,62,
      'no2_ann',17,'2020 CAAQS',20,19,
      'so2_1hr',70,'2020 CAAQS',80,72,
      'so2_ann',5,'2020 CAAQS',10,6
    )
  }
  
  #redefined filtering variables to avoid confusion with column names
  airzone_filter <- airzone
  metric_filter <- metric
  year_filter <- year
  
  if (is.null(df_stations)) {
    df_stations <- envair::listBC_stations(use_CAAQS = TRUE, merge_Stations = TRUE)%>%
      dplyr::rename(label = Label,
                    latitude  = LAT,
                    longitude = LONG,
                    airzone = AIRZONE) %>%
      select(site,label,airzone,latitude,longitude) %>%
      group_by(site) %>%
      slice(1) %>% ungroup() %>%
      filter(!is.na(airzone))
  } else {
    df_stations <- df_stations%>%
      dplyr::rename(label = Label,
                    latitude  = LAT,
                    longitude = LONG,
                    airzone = AIRZONE) %>%
      select(site,label,airzone,latitude,longitude) %>%
      group_by(site) %>%
      slice(1) %>% ungroup() %>%
      filter(!is.na(airzone))
  }
  
  
  
  if (is.null(airzone_filter)) {
    airzone_filter <- unique(df_stations$airzone)
  }
  
  #prepare data for plotting
  #add meta-data from station list and the labels
  #note that it is possible that there are stations without "Labels" value
  df <- df_caaqs_results %>%
    filter(metric %in% metric_filter) %>%
    left_join(df_stations,by='site') %>%
    filter(tolower(airzone) %in% tolower(airzone_filter)) %>%
    left_join(df_unit,by='metric') %>%
    left_join(df_axis,by='metric') %>%
    mutate(label = ifelse(is.na(label),site,label))
  
  
  
  df <- df %>%
    filter(year %in% year_filter)
  
  units <- unique(df$units)[[1]]
  xmax <- max(df$metric_value*1.1,df$xmin,na.rm = TRUE)
  caaqs <- unique(df$caaqs)[1]
  caaqs_label <- unique(df$lbl_caaqs)[1]
  xlab <- unique(df$xlab)[1]
  
  
  #identify scale limits
  
  
  #fix for stations with multiple instruments
  df <-  df %>%
    group_by(parameter,label,year,tfee) %>%
    dplyr::mutate(count =n()) %>%
    ungroup() %>%
    dplyr::mutate(label = ifelse(count >1,
                                 paste(label,'(',tolower(instrument),')',sep=''),
                                 label)) %>%
    mutate(label = gsub('pm25 ','',label,ignore.case = TRUE)) %>%
    mutate(label = gsub('pm25_','',label,ignore.case = TRUE)) %>%
    mutate(label = gsub('_','',label,ignore.case = TRUE)) %>%
    select(-count)
  #set order of site
  lvls_site <- df %>%
    filter(!tfee) %>%
    arrange(metric_value) %>%
    filter(!is.na(metric_value)) %>%
    pull(label) %>%
    unique()
  
  
  
  if (any(df$tfee)) {
    
    #there is TFEE to plot
    
    df <-  df %>%
      mutate(label=factor(label,levels=lvls_site)) %>%
      filter(!is.na(metric_value)) %>%
      mutate(`Data Adjustment (TFEE)` = ifelse(tfee,
                                               'Wildfire-adjusted\n(wildfire data removed)',
                                               'No Adjustment\n(wildfire data included)')) %>%
      mutate(`Data Adjustment (TFEE)` = factor(`Data Adjustment (TFEE)`,levels = c(
        'Wildfire-adjusted\n(wildfire data removed)','No Adjustment\n(wildfire data included)'
      )))
    a <- df %>%
      # head(20) %>%  #debug purposes, comment out
      
      ggplot2::ggplot(aes(x=label, y=metric_value,fill = `Data Adjustment (TFEE)`)) +
      geom_col(position='identity',colour = 'black',width = 0.8) +
      coord_flip() +
      ylab(units) +
      scale_y_continuous(expand = c(0,0),limits = c(0,xmax)) +
      #add CAAQS line and text
      geom_hline(yintercept = caaqs, colour = 'red', linetype = 'dashed') +
      annotate("text",x=nrow(df%>%select(site)%>%distinct())/4, y=xlab,
               label = caaqs_label, angle = 90, colour = 'red') +
      # ylab(expression(PM[2.5])) +
      theme(panel.background = element_blank(),
            panel.border = element_rect(fill=NA, colour='black'),
            axis.title.y = element_blank(),
            legend.position = 'bottom') +
      scale_fill_manual(values = c('cornflowerblue','slategray3'))
    
    # plotly::ggplotly(a)
  } else {
    #no TFEE to plot
    df <-  df %>%
      mutate(label=factor(label,levels=lvls_site)) %>%
      filter(!is.na(metric_value))
    
    a <-
      df %>%
      
      # head(20) %>%  #debug purposes, comment out
      
      ggplot2::ggplot(aes(x=label, y=metric_value)) +
      geom_col(position='dodge',colour = 'black',fill='cornflowerblue',width = 0.8) +
      coord_flip() +
      ylab(units) +
      scale_y_continuous(expand = c(0,0),limits = c(0,xmax)) +
      #add CAAQS line and text
      geom_hline(yintercept = caaqs, colour = 'red', linetype = 'dashed') +
      annotate("text",x=nrow(df%>%select(site)%>%distinct())/4, y=xlab,
               label = caaqs_label, angle = 90, colour = 'red') +
      # ylab(expression(PM[2.5])) +
      theme(panel.background = element_blank(),
            panel.border = element_rect(fill=NA, colour='black'),
            axis.title.y = element_blank(),
            legend.position = 'none') +
      scale_fill_manual(values = c('cornflowerblue'))
  }
  
  return(a)
  
}

#end of functions----------




#initial values---------
pollutant_initial <- df_parameter$display[1]
year_initial <- max(df_management_airzones$year)
tfee_initial <- TRUE

#---SHINY SECTION----------------
# Actual shiny part

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
                      ))),
    
    fluidRow(
      
      column(3,h6("Click the map to select an air zone"),
             
             leaflet::leafletOutput("map",height = '400px')),
      column(9,h6("Use vertical scrollbar (right side of graph) to reveal more bar graphs."),
             (div(style='height:400px;overflow-y: scroll;',
                  plotOutput("plot1",height = "1200px"))))),
    fluidRow(tags$head(
      tags$style(HTML("
      #table1_wrapper {
        width: 90% !important;
      }
    "))),DT::dataTableOutput("table1")),
    downloadLink('downloadData', 'Download Data')
    
    
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



server <- shinyServer(function(input, output) {
  
  observe({
    # query <- parseQueryString(session$clientData$url_search)
    # if (!is.null(query[['prel']])) {
    #   print('preliminary')
    #   # showTab(inputId = "navbar", target = "panel01")
    # } else {
    #   print('not preliminary')
    #   # hideTab(inputId = "navbar", target = "panel01")
    # }
  })
  a <-    map_airzone(polygon_a=NULL,df=df_management_airzones,az_mgmt = az_mgmt,parameter = pollutant_initial,
                      year = year_initial)
  
  output$map <- renderLeaflet(a)
  
  output$plot1 <- renderPlot(height = "1200px",
                             plot_bar_ranked(df_caaqs_results = df_caaqs_results,
                                             pollutant = pollutant_initial,
                                             year=year_initial,
                                             airzone = NULL,
                                             df_stations = df_stations))
  
  data <- df_caaqs_results
  output$table1 <- DT::renderDT(add_mgmt_legend())
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data, con)
    }
  )
  
  
  
  observeEvent(input$map_shape_click, {
    
    
    p <- input$map_shape_click
    
    try({
      airzone_select <- get_airzone(p$lat,p$lng)
      print(p$lat)
      print(p$lng)
      print(airzone_select)
      
      
      leafletProxy("map") %>%
        map_airzone(df=df_management_airzones,az_mgmt = az_mgmt,
                    parameter = input$pollutant,
                    year = input$year_slider,
                    
                    airzone = airzone_select)
      
      output$plot1 <- renderPlot(
        plot_bar_ranked(df_caaqs_results = df_caaqs_results,
                        pollutant = input$pollutant,
                        year=input$year_slider,
                        airzone = airzone_select,
                        df_stations = df_stations))
      
    })
  })
  
  observeEvent(input$year_slider,
               {
                 print('Slider')
                 leafletProxy("map") %>%
                   map_airzone(df=df_management_airzones,az_mgmt = az_mgmt,
                               parameter = input$pollutant,
                               year = input$year_slider,
                               
                               airzone = NULL)
                 
                 output$plot1 <- renderPlot(
                   plot_bar_ranked(df_caaqs_results = df_caaqs_results,
                                   pollutant = input$pollutant,
                                   year=input$year_slider,
                                   airzone = NULL,
                                   df_stations = df_stations))
               })
  
  observeEvent(input$pollutant,
               {
                 print('change pollutant')
                 leafletProxy("map") %>%
                   map_airzone(df=df_management_airzones,az_mgmt = az_mgmt,
                               parameter = input$pollutant,
                               year = input$year_slider,
                               
                               airzone = NULL)
                 
                 output$plot1 <- renderPlot(
                   plot_bar_ranked(df_caaqs_results = df_caaqs_results,
                                   pollutant = input$pollutant,
                                   year=input$year_slider,
                                   airzone = NULL,
                                   df_stations = df_stations))
                 
               })
  
})
shinyApp(ui, server)


