


# The following are stepwise process
# to generate data
# once created, please put into github

dirs_location <- './data/out'
df_management <- readr::read_csv(paste(dirs_location,'management.csv',sep='/'))
validation_year <- 2021

#fix for Port Alice Lake Road, and Rumble Beach
sort(unique(df_management$site))

df_management <- df_management[df_management$site != 'Port Alice Lake Road',]

labels_mgmt <- rcaaqs::management_levels %>%
  filter(stringr::str_detect(parameter, "pm2.5")) %>%
  select(labels, colour, colour_text, units_html) %>%
  distinct() %>%
  mutate(icons = paste0("assets/marker_", colour_text, ".svg"),
         text_colour = c("white", "black", "black", "white", "white"))

df_current_list <-  df_management %>%
  left_join(df_metric_list() %>%select(-parameter)) %>%
  select(site,tfee,instrument,year,latitude,longitude,pollutant,metric,metric_value,airzone,colour_order,colour,label,colour_text) %>%
  ungroup() %>%
  group_by(site,year,pollutant,tfee) %>%
  arrange(desc(metric),desc(colour_order)) %>%
  dplyr::mutate(mgmt = max(colour_order,na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::filter(mgmt == colour_order) %>%
  group_by(site,year,pollutant,tfee) %>%
  slice(1) %>% select(-mgmt) %>% ungroup() %>%
  left_join(labels_mgmt %>% select(colour_text,labels)) %>%
  mutate(popup = paste(site,'<br>',pollutant,'Management Action:',colour_text,
                       '<br>',labels))

#Create a summary for the airzone
#grabs the highest TFEE values
df_current_list_airzone <- df_management %>%
  left_join(df_metric_list() %>%select(-parameter)) %>%
  select(site,tfee,instrument,year,
         latitude,longitude,pollutant,
         metric,metric_value,airzone,colour_order,
         colour,label,colour_text) %>%
  ungroup() %>%
  group_by(airzone,year,pollutant,tfee) %>%
  arrange(desc(metric),desc(colour_order)) %>%
  dplyr::mutate(mgmt = max(colour_order,na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::filter(mgmt == colour_order) %>% 
  group_by(airzone,year,pollutant,tfee) %>%
  select(-latitude,-longitude,-metric_value,-instrument,-site) %>%
  distinct() %>%
  dplyr::mutate(labels = list(label)) %>%
  select(-label,-metric) %>% distinct() %>%
  ungroup() %>% 
  mutate(index = 1:n()) %>% group_by(index) %>%
  dplyr::mutate(labels_txt = paste(unlist(labels),collapse = ',')) %>%
  ungroup() %>%select(-labels,-index,-mgmt) %>%
  left_join(labels_mgmt %>% select(colour_text,labels)) %>%
  mutate(popup = paste(airzone,'<br>',pollutant,'Management Action:',colour_text,
                       '<br>',labels))

#save to file
df_current_list %>%
  filter(year <= validation_year) %>%
  readr::write_csv(paste(dirs_location,'management_sites.csv',sep='/'))

df_current_list_airzone %>%
  filter(year <= validation_year) %>%
  readr::write_csv(paste(dirs_location,'management_airzones.csv',sep='/'))

