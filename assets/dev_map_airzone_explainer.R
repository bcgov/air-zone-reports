

#' Create a graph of the airzones
#' This is for information only, to discuss the air zones
#' No content detail included
graph_airzone <- function(airzone=NULL, size = c("900px","700px")) {
  
  require(leaflet)
  require(rcaaqs)
  require(envreportutils)
  require(sf)
  require(bcmaps)
  require(dplyr)
  
  if (0) {
    airzone <- NULL
    size = c("900px","700px")
  }
  
  
  #list of all airzones
  lst_airzone <- bcmaps::airzones() %>% 
    as_tibble() %>%
    rename(airzone = Airzone) %>%
    select(airzone) 
  
  
  
  #create the airzone background colour
  #colour list below is default for airzone == NULL
  df_colour <- tribble(
    ~airzone,~colour,
    "Northeast",'#85D4E3',
    "Georgia Strait"  ,'#F4B5BD',
    "Southern Interior",'#9C964A',
    "Lower Fraser Valley",'#CDC08C',
    "Central Interior" ,'#FAD77B',
    "Coastal",'#CEAB07',
    "Northwest","#24281A"
  )
  
  
  if (!is.null(airzone)) {
    airzone <- tolower(airzone)
    # df_colour$colour[tolower(df_colour$airzone) == airzone] <- '#F2300F'
    df_colour$colour[tolower(df_colour$airzone) != airzone] <- 'white'
  }
  
  
  az_mgmt <- airzones() %>%
    st_make_valid() %>%
    st_transform(st_crs(bc_bound())) %>%
    st_intersection(st_geometry(bc_bound())) %>%
    group_by(airzone = Airzone) %>%
    summarize() %>%
    st_transform(4326) %>%
    left_join(df_colour)
  
  
  
  
  
  
  
  a <- leaflet(width = size[1],height = size[2],
               options = leafletOptions(attributionControl=FALSE)) %>%
    set_bc_view(zoom=4) %>%
    addProviderTiles(providers$Stamen.TonerLines,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    add_bc_home_button() %>%
    
    addPolygons(data = az_mgmt,
                color = 'black',
                fillColor = ~colour,
                # fillColor = df_mgmt_results$colour[df_mgmt_results$airzone == 'Central Interior'],
                weight = 1, opacity = 1, fillOpacity = 0.5,
                # popup = '<img src="https://www2.gov.bc.ca/assets/gov/british-columbians-our-governments/services-policies-for-government/policies-procedures-standards/web-content-development-guides/corporate-identity-assets/visid-illustrations/c00_img_bcmark_desc.gif" alt="Girl in a jacket">',
                label = paste(az_mgmt$airzone,'Air Zone'),
                labelOptions = labelOptions(textsize = "15px"),
                group = 'PM<sub>2.5</sub>',
                highlight = highlightOptions(weight = 3,
                                             color = "blue",
                                             bringToFront = TRUE)) 
  
  
  
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
  
  defaultW <- getOption("warn")
  options(warn = -1)
  az_mgmt <- airzones() %>%
    st_make_valid() %>%
    st_transform(st_crs(bc_bound())) %>%
    st_intersection(st_geometry(bc_bound())) %>%
    group_by(airzone = Airzone) %>%
    summarize() %>%
    st_transform(4326) 
  options(warn = defaultW)
  
  
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
