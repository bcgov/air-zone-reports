#' CREATE TRENDS of air quality in BC
#' 
#' @param dirs_location is the location of the data files
get_trends <- function(dirs_location = './data/out',reporting_year=NULL) {
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# dirs_location <- './data/out'  #local location, two dots for final, one dot for debug
if (0) {
  dirs_location <- './data/out'
  reporting_year <- 2021
}


list.files(dirs_location)

df_data_trends_caaqs <- readr::read_csv(paste(dirs_location,'caaqs_results.csv',sep='/')) 
df_data_trends_annual <- readr::read_csv(paste(dirs_location,'annual_results.csv',sep='/')) %>%
  filter(!is.na(value),value>-10)

if (is.null(reporting_year)) {
  reporting_year <- max(df_data_trends_caaqs$year)
}

maxyear <- reporting_year

df_stations <- readr::read_csv(paste(dirs_location,'liststations.csv',sep='/')) %>%
  mutate(AQMS = ifelse(is.na(AQMS),'N/A',AQMS)) %>%
  filter(AQMS != 'N') %>%
  filter(site %in% df_data_trends_annual$site)

colnames(df_stations)

df_plot_metric <- df_data_trends_annual%>%
  select(metric,parameter) %>%
  distinct()

#rolling 3-year average needed for
#pm25 annual
#pm25 24-hour
#o3_8hr
#no2 1-hour
#so2 1-hour

#calculate 3-year running average
df_data_trends_annual_3yr <- df_data_trends_annual %>%
  mutate(index = 1:n())
df_data_trends_annual_3yr_ <- NULL
for (i in 0:2) {
  df_data_trends_annual_3yr_ <- df_data_trends_annual_3yr_ %>%
    bind_rows(
      df_data_trends_annual_3yr %>%
        mutate(year=year+i)
      
      
    )
}

#those with metric "MEAN_1HR" are averaged over 1-year
#all the rest are averaged over 3 years
df_data_trends_annual_3yr <- df_data_trends_annual_3yr_ %>%
  mutate(valid_count = ifelse(is.na(value),0,1)) %>%
  group_by(parameter,site,instrument,tfee,year,metric) %>%
  dplyr::mutate(value_3yr = sum(value,na.rm = TRUE),valid_n =sum(valid_count)) %>%
  filter(valid_n>=2) %>%
  ungroup() %>%
  mutate(value = ifelse(grepl('MEAN_1HR',metric,ignore.case = TRUE),value,value_3yr/valid_n)) %>%
  select(-value_3yr,-valid_count) %>%
  filter(year<=maxyear)


#summarize for air zone plot
df_data_trends_annual_airzone <- df_data_trends_annual_3yr %>%
  left_join(df_stations %>%
              select(site,AIRZONE)) %>%
  filter(!is.na(AIRZONE)) %>%
  group_by(parameter,tfee,year,metric,AIRZONE) %>%
  dplyr::summarise(value_avg = mean(value,na.rm = TRUE),
                   value_min = min(value,na.rm = TRUE),
                   value_max = max(value,na.rm = TRUE))

df_data_trends_annual_overall <- df_data_trends_annual_3yr %>%
  left_join(df_stations %>%
              select(site,AIRZONE)) %>%
  filter(!is.na(AIRZONE)) %>%
  group_by(parameter,tfee,year,metric) %>%
  dplyr::summarise(value_avg = mean(value,na.rm = TRUE),
                   value_min = min(value,na.rm = TRUE),
                   value_max = max(value,na.rm = TRUE)) %>%
  mutate(AIRZONE = 'BC')

df_data_trends_annual_airzone <- df_data_trends_annual_airzone %>%
  bind_rows(df_data_trends_annual_overall) %>%
  filter(!tfee) %>%
  filter(grepl('RAW',metric,ignore.case = TRUE))

#create reference years
#this shows how many percent increase or decrease in value
# result_table <- 
df_BC_summary_ref<- df_data_trends_annual_airzone %>%
  filter(year %in% c(1990,2000,2010,maxyear)) %>%
  select(parameter,year,metric,AIRZONE,value_avg) %>% 
  tidyr::pivot_wider(names_from = year, values_from = value_avg) %>%
  mutate(perc_2000 = envair::round2((`2021`-`2000`)/`2000`*100))

#plot
lst_parameters <- df_data_trends_annual_airzone %>%
  filter(!tfee) %>%
  filter(AIRZONE == 'BC') %>%
  filter(year >=1990) %>%
  mutate(parameter_label = paste(parameter,metric)) %>%
  pull(parameter_label) %>%
  unique()
# paste(lst_parameters,collapse=',')
#define parameters for recoding
df_parameters <- tribble(
  ~parameter_label,~label,~CAAQS,~order,
  'NO2 RAW_ANNUAL_98P_D1HM','NO2 (1-Hour)',60,5,
  'NO2 RAW_ANNUAL_MEAN_1HR','NO2 (Annual)',17,4,
  'O3 RAW_ANNUAL_4TH_D8HM','O3 (8-Hour)',62,3,
  'PM25 RAW_ANNUAL_98P_24h','PM2.5 (24-Hour)',27,2,
  'PM25 RAW_ANNUAL_MEAN_24h','PM2.5 (Annual)',8.8,1,
  'SO2 RAW_ANNUAL_99P_D1HM','SO2 (1-Hour)',70,7,
  'SO2 RAW_ANNUAL_MEAN_1HR','SO2 (Annual)',5,6
)

a <- df_data_trends_annual_airzone %>%
  filter(!tfee) %>%
  filter(AIRZONE == 'BC') %>%
  filter(year >=1990) %>%
  mutate(parameter_label = paste(parameter,metric)) %>%
  left_join(df_parameters) %>%
  mutate(percentAbove = (value_avg - CAAQS)/CAAQS *100) %>%
  ungroup() 
  
result_ggplot <- a %>%
ggplot(aes(x=year,y=percentAbove,colour = reorder(label,order))) +
  geom_line() +
  geom_hline(yintercept = 0, colour='red',linetype = 'dashed') +
  annotate("text",x=2010, y=10,label = 'Current CAAQS') +
  theme(legend.position = 'bottom', legend.title = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill=NA,colour = 'black'),
        axis.title.x = element_blank()) +
  ylab('Percent Above/Below Current CAAQS')
  
result_plotly <- a %>%
  mutate(percentAbove = envair::round2(percentAbove,n=1)) %>%
  mutate(hovertext = paste(percentAbove,'%',sep='')) %>%
  mutate(label = gsub('PM2.5','PM<sub>2.5</sub>',label)) %>%
  mutate(label = gsub('O3','O<sub>3</sub>',label)) %>%
  mutate(label = gsub('NO2','NO<sub>2</sub>',label)) %>%
  mutate(label = gsub('SO2','SO<sub>2</sub>',label)) %>%
  
plotly::plot_ly(x=~year,y=~percentAbove,color =~reorder(label,order),
                type='scatter',mode='lines+markers',showlegend =T,
                hoverinfo ='y',
                hovertemplate = paste('%{y:.1f}','%',sep='')
                
                ) %>%
  layout(legend = list(orientation = 'h'),
         yaxis = list(title = 'Percent Above/Below CAAQS'),
         xaxis = list(title = 'Annual Reporting Period')
  ) %>%
  plotly::layout(hovermode = 'x unified')

return(list(table = df_BC_summary_ref,ggplot = result_ggplot,plotly = result_plotly,data = a))
}
