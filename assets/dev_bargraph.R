
dataDirectory <- './data/out'
list.files(dataDirectory)
require(dplyr)
require(ggplot2)
require(patchwork)
#input options
current_year <- 2021
parameter <- 'PM25'




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

#define the CAAQS and the axis scale limits for display purposes
#includes 2015 and 2020 CAAQS, based on the year
if (current_year>=2020) {
  df_axis <- tribble(
    ~metric,~caaqs,~lbl_caaqs,~xmin,~xlab,~colour,
    'pm25_annual',8.8,'2020 CAAQS',10,9.5,c('slategray3','cornflowerblue'),
    'pm25_24h',27,'2020 CAAQS',30,29,c('slategray3','cornflowerblue'),
    'o3_8h',62,'2020 CAAQS',70,64,c('slategray3','cornflowerblue'),
    'no2_1hr',60,'2020 CAAQS',70,62,c('slategray3','cornflowerblue'),
    'no2_ann',17,'2020 CAAQS',20,19,c('slategray3','cornflowerblue'),
    'so2_1hr',70,'2020 CAAQS',80,72,c('slategray3','cornflowerblue'),
    'so2_ann',5,'2020 CAAQS',10,6,c('slategray3','cornflowerblue')
  )
} else {
  df_axis <- tribble(
    ~metric,~caaqs,~lbl_caaqs,~xmin,~xlab,~colour,
    'pm25_annual',10,'2015 CAAQS',10,9.5,c('slategray3','cornflowerblue'),
    'pm25_24h',28,'2015 CAAQS',30,29,c('slategray3','cornflowerblue'),
    'o3_8h',63,'2015 CAAQS',70,64,c('slategray3','cornflowerblue'),
    'no2_1hr',60,'2020 CAAQS',70,62,c('slategray3','cornflowerblue'),
    'no2_ann',17,'2020 CAAQS',20,19,c('slategray3','cornflowerblue'),
    'so2_1hr',70,'2020 CAAQS',80,72,c('slategray3','cornflowerblue'),
    'so2_ann',5,'2020 CAAQS',10,6,c('slategray3','cornflowerblue')
  )
}



metric_filter <- df_metric$metric[df_metric$PARAMETER == parameter]
# df_caaqs_results <- readr::read_csv(paste(dataDirectory,'caaqs_results.csv',sep='/'))
# df_annual_results <- readr::read_csv(paste(dataDirectory,'annual_results.csv',sep='/'))
df_mgmt_results <- readr::read_csv(paste(dataDirectory,'management.csv',sep='/')) %>%
  select(site,instrument,year,tfee,metric,metric_value,colour,colour_text,AQMS) %>%
  distinct()



stations_include <- df_mgmt_results %>%
  left_join(df_metric) %>%
  filter(year == current_year, 
         PARAMETER == parameter,tfee == TRUE) %>%
  filter(!is.na(metric_value),tfee) %>%
  pull(site) %>% unique()

df_station <- envair::listBC_stations(use_CAAQS=TRUE)

df_station <-
  df_station %>%
  mutate(AQMS =ifelse(is.na(AQMS),'NA',AQMS)) %>%
  filter(AQMS != 'N',
         STATION_NAME %in% stations_include) %>%
  dplyr::rename(airzone = AIRZONE) %>%
  select(site,airzone,Label)





df_mgmt_results <-
  df_mgmt_results %>%
  filter(year == current_year,
         site %in% df_station$site,
         metric %in% metric_filter) %>%
  select(site,instrument,year,metric,metric_value,colour,colour_text,tfee) %>%
  distinct() %>%
  left_join(df_station) 

#remove duplicates

if (0) {
  # test <- df_mgmt_results
  df_mgmt_results <- test
}





df_mgmt_results <-
  df_mgmt_results %>%
  select(site,instrument,Label,year,metric,tfee,metric_value) %>%
  group_by(site,year,metric,tfee) %>%
  dplyr::mutate(count =n(),index =1:n()) %>%
  ungroup() %>%
  filter(index==1) %>%
  select(-count,-index) %>%
  mutate(tfee_txt = ifelse(tfee,'TFEE','NO_TFEE')) %>% select(-tfee) %>%  
  tidyr::pivot_wider(names_from = tfee_txt,values_from = metric_value) %>%
  mutate(delta_TFEE = NO_TFEE - TFEE) %>% select(-NO_TFEE) %>%
  tidyr::pivot_longer(cols = c(TFEE,delta_TFEE))




#add plot one metric at a time for each parameter
b <- NULL
for (metric_filter_ in metric_filter) {
  
  if (0) {
    metric_filter_ <- metric_filter[1]
  }
  
  # df_axis, df_metric contains plot options
  graph_xmin <- df_axis$xmin[df_axis$metric == metric_filter_]
  graph_xlab <- df_axis$xlab[df_axis$metric == metric_filter_]
  graph_xlab_txt <- df_axis$lbl_caaqs[df_axis$metric == metric_filter_]
  
  # df_metric
  graph_ylab_units <- df_metric$units_ggplot[df_metric$metric == metric_filter_] [[1]]
  graph_txt_captions <- df_metric$caption[df_metric$metric == metric_filter_] 
  
  df <- df_mgmt_results %>%
    ungroup() %>%
    filter(metric == metric_filter_)

  #plot metric[1]
  a <- df %>%
  filter(metric == metric_filter_,
         !is.na(value)) %>% 
  # View()
  ggplot(aes(x=reorder(Label,value),y=value, fill = name)) +
  geom_col(colour = 'black') +
  coord_flip() +
  geom_hline(yintercept = graph_xlab, colour = 'red', linetype = 'dashed') +
  annotate("text",x=nrow(df%>%select(site)%>%distinct())/4, y=graph_xlab,
           label = graph_xlab_txt, angle = 90, colour = 'red') +
  # ylab(expression(PM[2.5])) +
  scale_fill_manual(values = c('slategray3','cornflowerblue')) +
    ylab(graph_ylab_units) +
    xlab('') +
    labs(caption = graph_txt_captions) + 
    scale_y_continuous(expand = c(0,0)) +
    theme(panel.background = element_blank(),
          panel.border = element_rect(fill=NA, colour='black'),
          # legend.position = c(0.8,0.1),
          legend.position = 'none',
          legend.background = element_rect(fill=NA),
          legend.title = element_blank())
# facet_wrap(~airzone,scales = 'free',ncol=1) 
  
  if (is.null(b)) {
    b <- a
  } else {
    b <- a|b
  }
}
