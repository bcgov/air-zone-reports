# Copyright 2020 Province of British Columbia
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

# app_ui <- function() {
## this is the shell of an app.R scriptwith the bcgov style template
## ensure that the .css file and logo are in the www folder in your app directory.
# Define UI ----
parameters <- c('PM\u2082.\u2085',
                'Ozone',
                'NO\u2082',
                'SO\u2082')

source('01_load.R')
df_parameter <- tribble(
  ~display, ~parameter,
  'PM\u2082.\u2085','pm25',
  'Ozone','o3',
  'NO\u2082','no2',
  'SO\u2082','so2'
)
# parameters <- c('<h5>PM<sub>2.5</sub></h5>',
#                'O<sub>3</sub>',
#                'NO<sub>2</sub>',
#                'SO<sub>2</sub>')
parameters_npri <- c('pm25','pm10','nh3','nox','sox'
)

airzones <- sort(c('Central Interior','Northeast','Georgia Strait','Lower Fraser Valley',
                   'Southern Interior','Northwest','Coastal'))

metrics <- c('pm25_24h','pm25_annual','o3_8h','no2_1hr','no2_ann','so2_1hr','so2_ann')

jscode <- "
shinyjs.disableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.bind('click.tab', function(e) {
e.preventDefault();
return false;
});
tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
var tab = $('.nav li a[data-value=' + name + ']');
tab.unbind('click.tab');
tab.removeClass('disabled');
}
"
css <- "
.nav li a.disabled {
background-color: #aaa !important;
color: #333 !important;
cursor: not-allowed !important;
border-color: #aaa !important;
}"
library(shinyjs)

ui <- fluidPage(
  # titlePanel(""),
  # # shinyjs::useShinyjs(),
  # # shinyjs::extendShinyjs(text = jscode, functions = c("disableTab","enableTab")),
  # # shinyjs::inlineCSS(css),
  # # useShinyjs(),
  # # tags$style(type='text/css', "nav.navbar.navbar-default.navbar-static-top{border-color: #f5f5f5;background-color: #f5f5f5;}"),
  # # tags$style(type='text/css', ".navbar{min-height: 0px; margin-bottom: 0px;}"),
  # # tags$style(type='text/css', ".navbar-brand{height: 0px; padding: 0px 0px;}"),
  # # tabsetPanel(id = "navbar",  # add yellow highlighted text to theme the navigation bar
  navbarPage(title = "", theme = "../azreport_ui/www/bcgov.css",id='navbar',
             tabPanel("Station Summary",value = 'panel01',
                            selectInput("Parameter", "Select Pollutant to Display:",
                                  list(`Parameter` = parameters)
                      ),
                      uiOutput("stationSelect"),
                      plotOutput("plot1"),
                      plotly::plotlyOutput("plot2")
             ),
             tabPanel('Emission Inventory',value = 'panel02',
                     
                      
                      selectInput("pollutant", "Select Air Pollutant to display:",
                                  list(`Parameter` = parameters_npri)
                      ),
                      plotly::plotlyOutput('plot3')
                      
             ),
             tabPanel('Long Term Trends',value = 'panel03',
                      
                      selectInput("pollutant trend",'Select Air Pollutant to display',
                                  list(`Parameter` = df_parameter$display)
                      ),
                      selectInput("airzone",'Select Airzone',
                                  list(`Air Zone` = c('All',airzones))
                      ),
                      uiOutput("stationSelect2"),
                      plotOutput('plot4')
                      
             ),
             tabPanel('Map',value = 'panel04',
                      
                      leaflet::leafletOutput("mymap2"),
                      p()
                      # actionButton("recalcPM25", "PM2.5"),
                      # actionButton("recalcOzone", "Ozone"),
                      # actionButton("recalcNO2", "NO2"),
                      # actionButton("recalcSO2", "SO2")
                      
                      
             ),
             tabPanel('Bar Graph',value = 'panel05',
                     
                      selectInput("pollutants_bar","Select Pollutant to Display",
                                  list(`Metrics` = parameters)
                      ),
                      selectInput("airzone_bar",'Select Airzone',
                                  list(`Air Zone` = airzones)
                      ),
                      sliderInput("year_bar",label = 'Year',
                                  min = min(yearlist), max = max(yearlist),
                                  value = max(yearlist),sep=''
                      ),
                      # selectInput("year_bar",'Select Year',
                      #             list(`Year` = yearlist)
                      
                      plotOutput('plot5')
             ),
             tabPanel('Air Zone Emission Sources',value = 'panel06',
                      
                      
                      selectInput("pollutant_emission_airzone", "Select Air Pollutant to display:",
                                  list(`Parameter` = df_parameter$display)
                      ),
                      selectInput("AirZone_emission", "Select Air Zone:",
                                  list(`Airzone` = c('All',airzones))
                      ),
                      plotly::plotlyOutput('plot6')
                      
             ),
             tabPanel('Wood Smoke Reduction Program',value = 'panel07',
                    
                      selectInput("Woodstove_exchange", "Select Air Zone:",
                                  list(`Airzone` = c('All',airzones))
                      ),
                      plotly::plotlyOutput('plot7')
                      
             )))


# }
