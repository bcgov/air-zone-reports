


graph_managementmap <- function(current_year,tfee_filter,dataDirectory,
                                size = c("900px","700px")) {
  
  require(leaflet)
  require(rcaaqs)
  require(envreportutils)
  require(sf)
  require(bcmaps)
  require(dplyr)
  
  # parameter <- 'PM25'
  # current_year <- 2021
  # tfee_filter <- TRUE
  # dataDirectory <- './data/out'
  
  df_colour_levels <- tribble(
    ~colour_text,~colour_order,~colour,
    'grey',0,'#666565',
    'green',1,'#A6D96A',
    'yellow',2,'#FEE08B',
    'orange',3,'#F46D43',
    'red',4,'#A50026'
  )
  
  df_metric <- tribble(
    ~PARAMETER,~metric,~units,~units_ggplot,~caption,
    'PM25',"pm25_annual","Annual PM<sub>2.5</sub> Metric(&mu;g/m<sup>3</sup>)",bquote(~"Annual "~PM[2.5]~"Metric*,"~mu~g/m^3),'*Annual Average of Daily Values over 3-Years',
    'PM25',"pm25_24h", "24-Hour PM<sub>2.5</sub> Metric(&mu;g/m<sup>3</sup>)",bquote(~"24-Hour "~PM[2.5]~"Metric*,"~mu~g/m^3),'*98th Percentile of Daily Values over 3-Years',
    'O3',"o3_8h","8-Hour O<sub>3</sub> Metric (ppb)", bquote(~O[3]~"8-Hour Metric,ppb"), 'Average of Annual 4th Highest Values over 3-Years',     
    'NO2',"no2_1hr","1-Hour NO<sub>2</sub> Metric (ppb)", bquote("98th Percentile "~NO[2]~",ppb"),   'Annual Average of Daily Values over 3-Years', 
    'NO2',"no2_ann", "Annual NO<sub>2</sub> Metric (ppb)", bquote("Average "~NO[2]~",ppb"),   'Annual Average of Daily Values over 3-Years',
    'SO2',"so2_1hr","1-Hour SO<sub>2</sub> Metric (ppb)", bquote("99th Percentile "~SO[2]~",ppb"), 'Annual Average of Daily Values over 3-Years',  
    'SO2',"so2_ann" , "Annual SO<sub>2</sub> Metric (ppb)",bquote("Average "~SO[2]~",ppb"),'Annual Average of Daily Values over 3-Years'
  )
  
  #list of all airzones
  lst_airzone <- bcmaps::airzones() %>% 
    as_tibble() %>%
    rename(airzone = Airzone) %>%
    select(airzone) 
  
  #list of all airzones, remove AQMS == 'N' (permittee)
  df_station <- envair::listBC_stations(use_CAAQS=TRUE) %>%
    mutate(AQMS =ifelse(is.na(AQMS),'NA',AQMS)) %>%
    filter(AQMS != 'N') %>%
    dplyr::rename(airzone = AIRZONE) %>%
    select(site,airzone,Label) %>%
    distinct()
  
  df_mgmt_results <- readr::read_csv(paste(dataDirectory,'management.csv',sep='/')) %>%
    select(site,instrument,year,tfee,metric,metric_value,colour,colour_text,AQMS) %>%
    distinct() %>%
    filter(site %in% df_station$site) %>%
    left_join(df_metric) %>%
    left_join(df_colour_levels %>% select(colour_text,colour_order)) %>%
    left_join(df_station) %>%
    group_by(PARAMETER,year,tfee,airzone) %>%
    dplyr::summarise(colour_order_airzone = max(colour_order,na.rm = TRUE)) %>%
    
    #ensure NO2 and SO2 (no TFEE) will be included
    filter(year %in% current_year, 
           tfee == tfee_filter | !PARAMETER %in% c('PM25','O3')) %>%
    rename(colour_order = colour_order_airzone) %>%
    left_join(df_colour_levels)
  
  #filler for airzone
  fill_airzone <- df_mgmt_results %>%
    ungroup() %>%
    select(PARAMETER,year,tfee) %>%
    distinct() %>%
    merge(lst_airzone) %>%
    left_join(df_mgmt_results) 
  
  
  fill_airzone <- fill_airzone %>%
    mutate(colour_order = ifelse(is.na(colour_order),0,colour_order),
           colour_text= ifelse(is.na(colour_text),'grey',colour_text),
           colour= ifelse(is.na(colour),'#666565',colour))
  
  df_mgmt_results <- fill_airzone
  
  #create the airzone background-----
  az_mgmt <- airzones() %>%
    st_make_valid() %>%
    st_transform(st_crs(bc_bound())) %>%
    st_intersection(st_geometry(bc_bound())) %>%
    group_by(airzone = Airzone) %>%
    summarize() %>%
    st_transform(4326) %>%
    left_join(df_mgmt_results %>% select(airzone,PARAMETER,tfee,year,colour,colour_text))
  
  
  
  
  leaflet(width = size[1],height = size[2],
          options = leafletOptions(attributionControl=FALSE)) %>%
    set_bc_view() %>%
    addProviderTiles(providers$Stamen.TonerLines,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    add_bc_home_button() %>%
    
    addPolygons(data = az_mgmt %>% filter(PARAMETER == 'PM25'),
                color = 'black',
                fillColor = ~colour,
                # fillColor = df_mgmt_results$colour[df_mgmt_results$airzone == 'Central Interior'],
                weight = 1, opacity = 1, fillOpacity = 0.5,
                popup = '<img src="https://www2.gov.bc.ca/assets/gov/british-columbians-our-governments/services-policies-for-government/policies-procedures-standards/web-content-development-guides/corporate-identity-assets/visid-illustrations/c00_img_bcmark_desc.gif" alt="Girl in a jacket">',
                
                group = 'PM<sub>2.5</sub>') %>%
    
    addPolygons(data = az_mgmt %>% filter(PARAMETER == 'O3'),
                color = 'black',
                fillColor = ~colour,
                # fillColor = df_mgmt_results$colour[df_mgmt_results$airzone == 'Central Interior'],
                weight = 1, opacity = 1, fillOpacity = 0.5,
                group = 'O<sub>3</sub>') %>%
    
    addPolygons(data = az_mgmt %>% filter(PARAMETER == 'NO2'),
                color = 'black',
                fillColor = ~colour,
                # fillColor = df_mgmt_results$colour[df_mgmt_results$airzone == 'Central Interior'],
                weight = 1, opacity = 1, fillOpacity = 0.5,
                group = 'NO<sub>2</sub>') %>%
    addPolygons(data = az_mgmt %>% filter(PARAMETER == 'SO2'),
                color = 'black',
                fillColor = ~colour,
                # fillColor = df_mgmt_results$colour[df_mgmt_results$airzone == 'Central Interior'],
                weight = 1, opacity = 1, fillOpacity = 0.5,
                group = 'SO<sub>2</sub>') %>%
    addLayersControl(
      baseGroups = c("PM<sub>2.5</sub>", "O<sub>3</sub>", "NO<sub>2</sub>","SO<sub>2</sub>"),
      # overlayGroups = c("Include wildfires"),
      position = "topleft",
      options = layersControlOptions(collapsed = FALSE)
      
    ) %>%
  return()
  
}
