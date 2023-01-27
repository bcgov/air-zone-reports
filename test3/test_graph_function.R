

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
    pollutant <- 'pm25'
    df_stations <- df_stations
    
  }
  
  library(patchwork)
  print(paste('plot_bar_ranked() function. pollutant=',pollutant))
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
    b <- b + theme(legend.position = 'right')
    a <- a + theme(legend.position = 'none')
    a <- a|b
  }
  return(a)
}



#' Create a ranked bar graph (backend version)
#'
#' This is the back end of the plot_bar_ranked function
plot_bar_ranked0 <- function(df_caaqs_results,metric,year,airzone = NULL,df_stations = NULL) {
  
  if (0) {
    df_caaqs_results <- readr::read_csv('../test_data/caaqs_results.csv')
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
    df_stations <- listBC_stations(use_CAAQS = TRUE, merge_Stations = TRUE)%>%
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
    left_join(df_stations) %>%
    filter(tolower(airzone) %in% tolower(airzone_filter)) %>%
    left_join(df_unit) %>%
    left_join(df_axis) %>%
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
                                 paste(label,'\n(',tolower(instrument),')',sep=''),
                                 label)) %>%
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
      geom_col(position='identity',colour = 'black') +
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
      geom_col(position='dodge',colour = 'black',fill='cornflowerblue') +
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
