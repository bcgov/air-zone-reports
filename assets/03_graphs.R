# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

source('./assets/00_setup.R')
dataDirectory <- './data/out'
list.files(dataDirectory)

require(dplyr)
require(ggplot2)
#create seasonal graphs
df_seasonal <- readRDS(paste(dataDirectory,'exceed_seasonal.Rds',sep='/'))

param <- 'PM25'
station <- 'Prince George Plaza 400'

#' Plot Seasonal Trend of Exceedance Days
#' NOTE: Need to update, make more interactive
#' 
#' @param df_seasonal is the seasonal data
#' @param param is the pollutant
#' @param station is the station name
plot_seasonal_exceedance <- function(df_seasonal,param,station) {
  
  param <- toupper(param)
  
  #defines the 
  df_standard <- tribble(
    ~PARAMETER,~standard,
    'PM25','exceed_27',
    'NO2','exceed_60',
    'SO2','exceed_70',
    'O3','exceed_62'
  )
  
  std_criteria <- df_standard$standard[df_standard$PARAMETER == param]
  
  lvl_seasons <- c('Winter','Spring','Summer','Fall')
  #define the labels
  df_label <- tribble(
    ~PARAMETER,~label,
    'PM25',bquote(~"No. of Days Above 24-Hour Standard (CAAQS) for "~PM[2.5]),
    'NO2',bquote(~"No. of Days Above 1-Hour Standard (CAAQS) for "~NO[2]),
    'SO2',bquote(~"No. of Days Above 1-Hour Standard (CAAQS) for "~SO[2]),
    'O3',bquote(~"No. of Days Above Standard (CAAQS) for "~O[3])
  )
  
  
  df_seasonal <-  df_seasonal %>%
    group_by(PARAMETER,STATION_NAME,YEAR,name, SEASON) %>%
    arrange(desc(value)) %>%
    dplyr::mutate(count = n(), index = 1:n()) %>%
    # filter(count >1) %>% arrange(PARAMETER,STATION_NAME,YEAR,SEASON,name) %>% View()
    filter(index == 1) %>%
    filter(PARAMETER == param, STATION_NAME == station) %>%
    filter(name == std_criteria)
  
  
  #label for axis
  label <- df_label$label[df_label$PARAMETER == param][[1]]
  # ymax <- max(df_seasonal$value[df_seasonal$STATION_NAME == station], na.rm = TRUE)
  a <- df_seasonal %>%
    
    # View()
    ggplot(aes(x=YEAR, y=value,fill = factor(SEASON,levels = lvl_seasons), group_by(INSTRUMENT))) +
    geom_col(colour = 'black') +
    scale_x_continuous(expand = c(0,0.1))+
    scale_y_continuous(expand = c(0,0.5)) +
    ylab(label)+
    theme(panel.background = element_rect(fill=NA, colour ='black'),
          legend.title = element_blank())
  return(a)
}

