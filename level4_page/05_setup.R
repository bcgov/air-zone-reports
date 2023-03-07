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






#' Create a bar graph of the APEI
#'
#' @param pollutant can be pm25, nox, sox, vox, nh3,all
#' @param df is the pollutant data. This can be retrieved with from get_apei
#' @param categorytype is either source, sector, or subsector. Default is source
#' @param URL is the ECCC URL for the NPRI
#' @param output is the output type of either 'basic' or 'plotly'
#'
#' @export
plot_apei <- function(pollutant,df=NULL,categorytype = 'Source',URL=NULL,output = 'plotly') {
  if (0) {
    pollutant <- 'pm25'
    pollutant='all'
    categorytype <- 'Source'
    output = 'plotly'
    URL=NULL
    df <-readr::read_csv(paste(dirs_location,'EN_APEI-Can-Prov_Terr.csv',sep='/'))
  }
  
  require(ggplot2)
  
  pollutant_select = pollutant
  
  if (is.null(df)) {
    df <- envair::get_apei(categorytype = categorytype, URL = URL)
  }
  
  
  df_apei <- df %>%
    filter(Region == 'BC') %>%
    dplyr::rename(groupingcolumn= categorytype)
  
  
  if (tolower(pollutant_select) != 'all') { #change pollutant for labelling purposes
    label <- pollutant_select
    
    # df <- envair::get_apei
    df_apei <- df_apei %>%
      filter(grepl(pollutant_select,pollutant,ignore.case = TRUE))
    
    if (grepl('pm25',pollutant_select,ignore.case = TRUE)) {
      label <- expression(PM[2.5]*' tonnes/year')
    }
    
    if (grepl('pm10',pollutant_select,ignore.case = TRUE)) {
      label <- expression(PM[10]*' tonnes/year')
    }
    
    if (grepl('nh3',pollutant_select,ignore.case = TRUE)) {
      label <- expression(NH[3]*' tonnes/year')
    }
    
    if (grepl('nox',pollutant_select,ignore.case = TRUE)) {
      label <- expression(NO[x]*' tonnes/year')
    }
    if (grepl('sox',pollutant_select,ignore.case = TRUE)) {
      label <- expression(SO[x]*' tonnes/year')
    }
    if (grepl('voc',pollutant_select,ignore.case = TRUE)) {
      label <- 'VOC tonnes/year'
    }
    
    
    #to arrange based on value
    levels_grouping <- df_apei %>%
      # filter(Year == max(df_apei$Year)) %>%
      arrange((value)) %>%
      pull(groupingcolumn) %>%
      unique()
    
    if (tolower(output) == 'basic') {
      
      
      a <- df_apei %>%
        filter(!is.na(groupingcolumn)) %>%
        # filter(tolower(groupingcolumn) != 'dust') %>%
        dplyr::mutate(groupingcolumn = factor(groupingcolumn,levels = levels_grouping)) %>%
        # pull(groupingcolumn) %>% unique()
        ggplot(aes(x=Year,y=value,fill = groupingcolumn)) +
        geom_col(colour = 'black') +
        theme(legend.position = 'bottom',
              legend.title = element_blank(),
              panel.background = element_rect(fill=NA,colour = 'black')) +
        ylab(label) +
        scale_x_continuous(expand=c(0,0)) +
        guides(fill=guide_legend(ncol=3,reverse = TRUE))
      
      return(a)
    }
    
    if (tolower(output) == 'plotly') {
      require(plotly)
      
      #define pollutant labels
      df_labels <- tribble(
        ~pollutant,~label,
        'pm25','Fine Particulate Matter (PM<sub>2.5</sub>)',
        'pm10','Coarse Particulate Matter (PM<sub>10</sub>)',
        'nh3','Ammonia (NH<sub>3</sub>)',
        'sox','Sulfur Oxides (SO<sub>x</sub>)',
        'nox','Nitrogen Oxides (NO<sub>x</sub>)',
        'voc','Volatile Organic Carbon (VOC)'
      )
      
      pollutant_label <- df_labels$label[df_labels$pollutant == pollutant][1]
      a <- {
        plot_ly(df_apei,x=~Year, y= ~value, color = ~groupingcolumn, type = 'bar', source = 'scatter',
                marker = list(line = list(width = 1,color = 'rgb(0, 0, 0)')),
                showlegend =T,
                hoverinfo ='y',
                hovertemplate = paste('%{y:,.0f}',' tonnes/yr',sep='')
        ) %>%
          layout(barmode = 'stack',yaxis = list(title = paste(pollutant_label,',tonnes/year',sep=''))) %>%
          plotly::layout(hovermode = 'x unified',
                         barmode = 'stack') 
      }
      return(a)
    }
    
    
  } else {
    
    #create a list of pollutants and their display labels
    df_pollutants <- tribble(
      ~pollutant,~pollutant_label,~order,
      'PM10 (t)','PM<sub>10</sub>',2,
      'PM25 (t)','PM<sub>2.5</sub>',1,
      'SOX (t)','SO<sub>x</sub>',4,
      'NOX (t)','NO<sub>x</sub>',3,
      'VOC (t)','VOC',5,
      'CO (t)','CO',6
      
    )
    # a <- 
    
    #results to compare as percent to 1990
    df_apei_1990 <- 
      df_apei%>%
      filter(Year == 1990) %>%
      group_by(Region,pollutant) %>%
      summarise(value_total = sum(value,na.rm = TRUE)) %>%
      rename(value_total_1990 = value_total) 
    
    data_a <- df_apei %>%  
      filter(!is.na(groupingcolumn)) %>%
      left_join(df_pollutants) %>%
      
      group_by(Region,Year,pollutant,pollutant_label,order) %>%
      # View()
      summarise(value_total = sum(value,na.rm = TRUE)) %>% 
      left_join(df_apei_1990) %>%
      mutate(perc_1990= envair::round2((value_total-value_total_1990)/value_total_1990*100,0)) %>%
      mutate(perc_1990_txt = ifelse(perc_1990>0,
                                paste('+',perc_1990,sep=''),
                                paste('\2191',perc_1990,sep=''))) %>%
      # View()
      filter(pollutant %in% df_pollutants$pollutant) %>% ungroup()
    
    a <- data_a %>%
      plotly::plot_ly(x=~Year,y=~perc_1990,color = ~reorder(pollutant_label,order),
                      type = 'scatter', mode='lines+markers',
                      # marker = list(line = list(width = 1)),
                      showlegend =T,
                      hoverinfo ='y',
                      hovertemplate = paste('%{y:,.0f}',' % (from 1990)',sep='')
                      
      ) %>%
      layout(title = 'Trends in Pollutant Emissions',
             legend = list(orientation = 'h'),
             yaxis = list(title = 'Percent Change From 1990'),
             xaxis = list(title = 'Year')
      ) %>%
      plotly::layout(hovermode = 'x unified',
                     barmode = 'stack') 
    result <- list(data = data_a, plot = a)
    return(result)
    
    
    
  }
}



#end of functions list------

#' Plot APEI, complete version
#' 
#' @param df_apei is the dataframe containing the apei. use envair::get_apei() function
plot_apei_complete <- function(df_apei = NULL) {
  #pre-process data------
  
  df <- df_apei
  if (is.null(df)) {
    df <- envair::get_apei(categorytype = 'Source') 
  }
  lst_source <- df %>%
    pull(Source) %>%
    unique()
  
  lst_pollutant <- df %>%
    pull(pollutant) %>%
    unique()
  #remove dust and fires
  df_dust <- df
  
  df_nodust <- df %>%
    filter(!Source %in% c('Dust','Fires'))
  
  plot_nodust <- plot_apei(pollutant = 'all', df= df_nodust)
  plot_dust <- plot_apei(pollutant = 'all', df_dust)
  plot_pm25 <- plot_apei(pollutant = 'pm25',df=df_nodust,output = 'plotly')
  plot_nox <- plot_apei(pollutant = 'nox',df=df_nodust,output = 'plotly')
  plot_sox <- plot_apei(pollutant = 'sox',df=df_nodust,output = 'plotly')
  plot_voc <- plot_apei(pollutant = 'voc',df=df_nodust,output = 'plotly')
  
  plot_pollutants <- list(all_nodust = plot_nodust, all_dust = plot_dust,pm25 = plot_pm25,nox=plot_nox,sox = plot_sox,voc = plot_voc)
  
  return(plot_pollutants)
  
}

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
