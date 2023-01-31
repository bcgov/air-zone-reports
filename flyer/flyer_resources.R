library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(magick)

# dirs_location <- './data/out'  #local location, two dots for final, one dot for debug
if (0) {
  dirs_location <- './data/out'
}



#' Create a table that summarize the management level
#' 
#' @param dataDirectory is the location of the management.csv file

table_management_level <- function(dataDirectory = '../data/out',current_year) {
  
  if (0) {
    dataDirectory = './data/out'
    current_year <- 2020
  }
  require(dplyr)
  require(ggplot2)
  require(patchwork)
  require(kableExtra)
  #input options
  
  if (!file.exists(paste(dataDirectory,'management.csv',sep='/'))) {
    print(paste('Missing File:"',dataDirectory,'/management.csv"',sep=''))
    return(NULL)
  }
  
  df_colour_levels <- tribble(
    ~colour_text,~colour_order,~colour,
    'grey',0,'#dbdbdb',
    'green',1,'#A6D96A',
    'yellow',2,'#FEE08B',
    'orange',3,'#F46D43',
    'red',4,'#A50026'
  )
  
  
  df_station <- envair::listBC_stations(use_CAAQS=TRUE) %>%
    mutate(AQMS =ifelse(is.na(AQMS),'NA',AQMS)) %>%
    filter(AQMS != 'N') %>%
    dplyr::rename(airzone = AIRZONE) %>%
    select(site,airzone,Label) %>%
    distinct()
  
  #list of all airzones
  lst_airzone <- bcmaps::airzones() %>% 
    as_tibble() %>%
    rename(airzone = Airzone) %>%
    select(airzone) 
  
  
  df_mgmt_results <- readr::read_csv(paste(dataDirectory,'management.csv',sep='/')) %>%
    select(site,instrument,year,tfee,parameter,metric,metric_value,colour,colour_text) %>%
    distinct() 
  
  
  #identify parameters with no TFEE adjustment
  lst_no_tfee_param <- df_mgmt_results %>%
    filter(
      !parameter %in% 
        df_mgmt_results$parameter[df_mgmt_results$tfee]
    ) %>%
    pull(parameter) %>%
    unique()
  
  #reproduce lst_no_tfee_param with tfee=TRUE
  df_mgmt_results <- df_mgmt_results %>%
    bind_rows(
      df_mgmt_results %>% 
        filter(parameter == lst_no_tfee_param) %>%
        mutate(tfee = TRUE))
  
  # colnames(df_mgmt_results)
  
  #add fillers
  df_fill <- df_mgmt_results %>%
    select(year,metric,metric_value,colour,colour_text) %>%
    distinct()
  
  df_count <- df_mgmt_results %>%
    left_join(df_station) %>%
    filter(!is.na(metric_value)) %>%
    select(year,parameter,site,tfee,airzone) %>%
    distinct() %>%
    group_by(year,parameter,airzone,tfee) %>%
    dplyr::summarise(total_sites = n()) 
  
  
  df_mgmt_airzone <-
    df_mgmt_results %>%
    left_join(df_station) %>%
    left_join(df_colour_levels) %>%
    left_join(df_count) %>%
    group_by(year,parameter,airzone,tfee) %>%
    dplyr::mutate(max_colour_order = max(colour_order)) %>%
    filter(colour_order == max_colour_order) %>%
    dplyr::mutate(sites = list(Label),
                  metrics = list(metric)) %>%
    ungroup() %>%
    select(-max_colour_order,-site,-metric,-metric_value,-Label,-instrument) %>%
    distinct()
  
  df_mgmt_airzone$metrics <- lapply(df_mgmt_airzone$metrics,unique)
  df_mgmt_airzone$sites <- lapply(df_mgmt_airzone$sites,unique)
  df_mgmt_airzone$sites_count <- unlist(lapply(df_mgmt_airzone$sites,length))
  
  df_mgmt_airzone <- df_mgmt_airzone %>%
    filter(!is.na(airzone))
  
  
  
  
  
  #add special text
  df_mgmt_airzone_table <- df_mgmt_airzone %>%
    mutate(perc_sites = sites_count/total_sites * 100) %>%
    arrange(parameter,year,airzone) %>% ungroup() %>%
    dplyr::mutate(index = 1:n()) %>%
    group_by(index) %>%
    mutate(sites = paste(unlist(sites),collapse=', ')) %>% 
    mutate(special_txt = ifelse(perc_sites==100 & total_sites >2,
                                paste('All',total_sites,'sites',sep=' '),
                                ifelse(sites_count<=3,sites,
                                       paste(sites_count,'of',total_sites,'sites',sep=' '))
    )) 
  
  df_mgmt_airzone_table$special_txt[is.na(df_mgmt_airzone_table$special_txt)] <- 'Data Not Available'
  
  #add Northwest air zone
  df_mgmt_airzone_table <- df_mgmt_airzone_table %>%
    bind_rows(
      tribble(
        ~airzone,~colour,~colour_order,~colour_text,~special_txt,
        'Northwest','#dbdbdb',0,'grey','Data Not Available'
      ) %>%
        merge(
          df_mgmt_airzone_table %>% ungroup() %>%
            select(year,tfee,parameter) %>%
            distinct()
        )
    )
  
  # added new
  table_mgmt <- df_mgmt_airzone_table %>%
    filter(tfee == TRUE) %>%
    ungroup() %>%
    filter(year == current_year) 
  
  #rename the parameter
  df_parameter <- tribble(
    ~display, ~parameter,
    'PM\u2082.\u2085','pm25',
    'Ozone','o3',
    'NO\u2082','no2',
    'SO\u2082','so2'
  )
  
  df_parameter$display <- factor(df_parameter$display,levels =df_parameter$display)
  
  #add the html detals
  
  #create a table summary for display
  table_mgmt_display <- table_mgmt %>%
    select(airzone,parameter,special_txt) %>%
    rename(`Air Zone` = airzone) %>%
    left_join(df_parameter) %>%
    arrange(display) %>%
    select(-parameter) %>%
    # mutate(special_txt = gsub(', ','<br>',special_txt)) %>%
    tidyr::pivot_wider(names_from = display,values_from = special_txt)
  
  
  
  
  #for reference: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Installation
  tbl_output <- table_mgmt_display %>%
    arrange(`Air Zone`) %>%
    # bind_rows(tribble(
    #   ~`Air Zone`,
    #   'Red',
    #   'Orange',
    #   'Yellow',
    #   'Green',
    #   'Gray'
    # )) %>%
    kbl('html',escape = F) %>%
    kable_styling("bordered",position = 'center') %>%
    kableExtra::column_spec(column=1,width="15em",color = 'black') %>%
    kableExtra::column_spec(column=2:ncol(table_mgmt_display),width="30em",color = 'black') %>%
    kableExtra::row_spec(row=0,color = 'white',background = 'black',align = 'c')
  
  #add colour details
  # start by assigning the column numbrs for the parameters
  df_column <- tribble(
    ~parameter,~colnum,
    'pm25',2,
    'o3',3,
    'no2',4,
    'so2',5,
  )
  
  for (param in unique(table_mgmt$parameter)) {
    
    colour_assign <- table_mgmt %>%
      filter(parameter == param,tfee) %>%
      arrange(airzone) %>%
      mutate(txt_colour = ifelse(colour_text %in% c('red','grey'),'white','black'))
    
    colnum <- df_column$colnum[df_column$parameter == param]
    
    tbl_output <- tbl_output %>%
      kableExtra::column_spec(column = colnum,
                              background = colour_assign$colour,
                              color = colour_assign$txt_colour,
                              popover = spec_popover(content = colour_assign$colour_text,
                                                     trigger = 'hover',
                                                     position = 'auto'))
  }
  
  
  return(tbl_output)
  
}


tbl <- table_management_level('./data/out',2021)

tbl %>%
  kableExtra::as_image( zoom=10,file ='./flyer/management_level.jpg')


tbl_2020 <- table_management_level('./data/out',2020)

tbl_2020 %>%
  kableExtra::as_image( zoom=10,file ='./flyer/management_level_2020.jpg')

source('./level4_page/00_setup.R')

trends <- get_trends()

data_trends <- trends$data

#change the label
paste(data_trends$metric %>% unique(),collapse = ',')

df_metric <- tribble(
  ~metric,~metric_label,
  'RAW_ANNUAL_98P_D1HM','1-Hour Metric',
  'RAW_ANNUAL_MEAN_1HR','Annual Metric',
  'RAW_ANNUAL_4TH_D8HM','8-Hour Metric',
  'RAW_ANNUAL_98P_24h','24-Hour Metric',
  'RAW_ANNUAL_MEAN_24h','Annual Metric',
  'RAW_ANNUAL_99P_D1HM','1-Hour Metric'
)

df_parameter_order <- tribble(
  ~parameter,~label_param,~order,
  'PM25','PM\u2082.\u2085',1,
  'O3','O\u2083',2,
  'NO2','NO\u2082',3,
  'SO2','SO\u2082',4
)



a <- data_trends %>%
  left_join(df_metric) %>%
  left_join(df_parameter_order) %>%
  ggplot(aes(x=year,y=percentAbove,colour = metric_label)) +
  geom_line(size=1.0) +
  geom_hline(yintercept = 0, colour='red',linetype = 'dashed') +
  annotate("text",x=2010, y=10,label = 'Current CAAQS') +
  theme(legend.position = 'top', legend.title = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill=NA,colour = 'black'),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=12),
        strip.text = element_text(size = 16, margin = margin())) +
  ylab('Percent Above/Below Current CAAQS') +
  facet_wrap(~reorder(parameter,order))

ggplot2::ggsave('./flyer/trends.jpg',a)

#retrieve text
trends$table %>% 
  mutate(perc_1990 = -100*(`1990`-`2021`)/`1990`) %>%
  View()


source('./level4_page/01_setup.R')

emissions <- get_emissions()

emissions$totals

df_emissions <- emissions$data

unique(df_emissions$name)
paste(unique(df_emissions$Source), collapse =',')

df_sources <- tribble(
  ~Source,~Sourcetype,
  'Ore and Mineral Industries','Mining',
  'Oil and Gas Industry','Oil and Gas',
  'Electric Power Generation (Utilities)','Others',
  'Manufacturing','Manufacturing',
 'Transportation and Mobile Equipment','Vehicles',
  'Agriculture','Agriculture',
  'Commercial / Residential / Institutional','Others',
  'Incineration and Waste','Others',
  'Paints and Solvents','Paints',
  'Dust','Dust',
  'Fires','Fires'
)

b <-
df_emissions %>%
  filter(!(Source %in% c('GRAND TOTAL','Fires')),
         name %in% c('PM25','NOx','SOx','VOC')) %>%
  rename(parameter = name) %>%
  left_join(df_parameter_order) %>%
  left_join(df_sources) %>%
   group_by(Sourcetype,Year,parameter,order) %>%
  dplyr::summarize(value = sum(value,na.rm = TRUE)) %>%
  ungroup() %>%  
ggplot(aes(x=Year,y=value/1000,fill = Sourcetype)) +
  # geom_line(size=1.0) +
  geom_col(colour = 'black')+
  theme(legend.position = 'bottom', legend.title = element_blank(),
        legend.key = element_blank(),legend.box="horizontal",
        legend.text = element_text(size=12),
        panel.background = element_rect(fill=NA,colour = 'black'),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.x = element_text(size=12),
        strip.text = element_text(size = 16, margin = margin())) +
  ylab('Thousand Tonnes Per Year') +
  facet_wrap(~reorder(parameter,order),scales ='free')

ggplot2::ggsave('./flyer/emissions.jpg',b)
