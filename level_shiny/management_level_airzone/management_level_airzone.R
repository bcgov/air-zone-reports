# Observe the changes in management levels from each site through the years

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

dirs_location <- '././data/out/'
dirs_location <- 'https://github.com/bcgov/air-zone-reports/raw/master/data/out/'

#this data was created using get_management_summary_complete()
# it is located in /assets/00_setup.R
mgmt_data <- readRDS(url(paste(dirs_location,'managementsummary_tables.Rds',sep='')))



lst_sites <- mgmt_data$`all stations` %>% 
  select(site) %>%
  distinct() %>%
  arrange(site) %>%
  filter(!grepl('Air Zone',site,ignore.case = TRUE)) %>%
  pull(site)

lst_years <- mgmt_data$`all stations` %>% 
  select(year) %>%
  distinct() %>%
  arrange(year)
#' Create management level legend table
#' This is generalized
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

#---SHINY SECTION----------------
# Actual shiny part

ui <-
  
  (fluidPage(
    h4(HTML('Air Quality Management Levels')),
    tags$head(
      tags$style(HTML("
      body { background-color: #f2efe9; }
      .container-fluid { background-color: #fff; width: auto; padding: 5px; }
      .topimg { width: 0px; display: block; margin: 0px auto 0px auto; }
      .title { text-align: center; }
      .toprow { margin: 5px 5px; padding: 5px; background-color: #38598a; }
      .filters { margin: 3px auto; }
      .shiny-input-container { width:90% !important; }
      .table { padding: 0px; margin-top: 0px; }
      .leaflet-top { z-index:999 !important; }
      "))),
    
    
    fluidRow(class = "toprow",
             fluidRow(class = 'filters',
                      column(8,
                             tags$style(type='text/css', 
                                        '.selectize-input { font-size: 15px; line-height: 10px;} 
                          .selectize-dropdown { font-size: 12px; line-height: 15px; }
                          .control-label {font-size: 12px; color: white !important;}
                          .irs-min {font-size: 0px; color: white; !important}
                          .irs-max {font-size: 0px; color: white;}
                          .irs-single {font-size: 14px; color: white;}
                          .irs-grid-text {font-size: 10px; color: white;}'
                             ),
                             
                                    sliderInput('year_slider',label ='Year',
                                                min = min(lst_years),
                                                max = max(lst_years),
                                                value = max(lst_years),width = "50%",
                                                sep='')
                          
                             
                      )
                      
                      
             )),
    # fluidRow(tableOutput("table2")),
    fluidRow(
      column(12,h6("Management Levels for the Reporting Period"),
             (div(style='height:auto;',
                  tableOutput("table2"))))),
    
    fluidRow(tags$head(
      tags$style(HTML("
      #table1_wrapper {
        width: 90% !important;
      }
    "))),
      DT::dataTableOutput("table1")),
    downloadLink('downloadData', 'Download Data')
    
    
    #        sidebarLayout(
    # sidebarPanel(radioButtons("no Wildfire","wildfire-adjusted"),
    # c('tfee' = 'tfee','notfee'='notfee')
    # ))
    # sidebarLayout(
    #   sidebarPanel(leafletOutput("map"),width=5),
    #   mainPanel (
    #     uiOutput("md_file"),width=7
    #   ))
    
  ))



server <- shinyServer(function(input, output) {
  
  output$table1 <- DT::renderDT(add_mgmt_legend())
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('management_leveldata-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data, con)
    }
  )
  
  
  
  
  
  observeEvent(input$year_slider,
               {
                 print(paste('change site',input$year_slider))
                 data <- mgmt_data$raw_data
                 
                 output$table2 <- function() {mgmt_data[[paste('management_',input$year_slider,sep='')]]}
                 # output$plot1 <- renderPlot(
                 #   plot_bar_ranked(df_caaqs_results = df_caaqs_results,
                 #                   pollutant = input$pollutant,
                 #                   year=input$year_slider,
                 #                   airzone = NULL,
                 #                   df_stations = df_stations))mgmt
                 
               })
  
})
shinyApp(ui, server)



