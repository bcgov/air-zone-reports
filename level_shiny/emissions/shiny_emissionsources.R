# This creates a shiny app that visualizes emissions

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
library(shinyBS)


#List of Functions ----------------

#' Create a bar graph of the APEI
#'
#' @param pollutant can be pm25, nox, sox, vox, nh3,all
#' @param df is the pollutant data. This can be retrieved with from get_apei
#' @param categorytype is either source, sector, or subsector. Default is source
#' @param URL is the ECCC URL for the NPRI
#' @param output is the output type of either 'basic' or 'plotly'

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
          layout(barmode = 'stack',yaxis = list(title = paste(pollutant_label,',tonnes/year',sep='')),
                 xaxis = list(title='')) %>%
          plotly::layout(hovermode = 'x unified',
                         barmode = 'stack')  %>%
          layout(title = paste('Source of',pollutant_label,'Emissions'),
                 legend = list(orientation = 'h'))
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
             yaxis = list(title = 'Change from 1990 Emissions (%)'),
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
  plot_pm10 <- plot_apei(pollutant = 'pm10',df=df_nodust,output = 'plotly')
  plot_nox <- plot_apei(pollutant = 'nox',df=df_nodust,output = 'plotly')
  plot_sox <- plot_apei(pollutant = 'sox',df=df_nodust,output = 'plotly')
  plot_voc <- plot_apei(pollutant = 'voc',df=df_nodust,output = 'plotly')
  
  plot_pollutants <- list(all_nodust = plot_nodust, all_dust = plot_dust,pm25 = plot_pm25,pm10 = plot_pm10,nox=plot_nox,sox = plot_sox,voc = plot_voc)
  
  return(plot_pollutants)
  
}

#end of functions----------------
#initiation-------------

apei <- plot_apei_complete()


#ui shiny-----------
ui <- fluidPage(
  h4(HTML('Emissions Sources')),
  tags$head(
    tags$style(HTML("
      body { background-color: #f2efe9; }
      .container-fluid { background-color: #fff; width: auto; padding: 5px; }
      .topimg { width: 0px; display: block; margin: 0px auto 0px auto; }
      .title { text-align: center; }
      .toprow { margin: 5px 0px; padding: 5px; background-color: #38598a; }
      .filters { margin: 0px auto; }
      .shiny-input-container { width:90% !important; }
      .table { padding: 0px; margin-top: 0px; }
      .leaflet-top { z-index:999 !important; }
      "))),
  #     
  
  #     "))
  # ),
  # h1("Emission Sources", class = "title"),
  
  fluidRow(class = "toprow",
           fluidRow(class = 'filters',
                    h6("Select Pollutant to Display B.C. Emission Sources",style = "color:white"),
                    actionButton(inputId = 'all',label = htmltools::HTML('<b>All</b>'),title = 'Display summary for all pollutants'),
                    actionButton(inputId = 'pm25',label = htmltools::HTML('PM<sub>2.5</sub>'),title = 'Fine Particulate Matter (PM2.5)'),
                    actionButton(inputId = 'pm10',label = htmltools::HTML('PM<sub>10</sub>'),title = 'Particulate Matter 10 Microns'),
                    actionButton(inputId = 'nox',label = htmltools::HTML('NO<sub>x</sub>'), title = 'Nitrogen Oxides'),
                    actionButton(inputId = 'sox',label = htmltools::HTML('SO<sub>x</sub>'), title = 'Sulphur Oxides'),
                    actionButton(inputId = 'voc',label = htmltools::HTML('VOC'),title = 'Volatile Organic Compounds')
                    
           )),
  fluidRow(div(style='height:400px;overflow-y: hidden;overflow-x: hidden;',plotlyOutput("plot1")))#,height = "600px",width = '600px')
  
  #(div(style='height:400px;overflow-y: scroll;',
  # plotOutput("plot1",height = "1200px")))))
  # fluidRow(p("Excludes dust, and smoke from wildfires, residential sources, and open burn",
  #            style="color:red;text-align: right"))
)

#server shiny--------------
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  v <- reactiveValues(data = apei$all_nodust$plot %>%
                        layout(annotations =  
                                 list(x = 0, y = -0.1, 
                                      text = "Excludes dust, and smoke from wildfires, residential sources, and open burn", 
                                      showarrow = F, 
                                      xref='paper', 
                                      yref='paper')
                        ))
  
  observeEvent(input$all, {
    v$data <- apei$all_nodust$plot %>%
      layout(annotations =  
               list(x = 0, y = -0.1, 
                    text = "Excludes dust, and smoke from wildfires, residential sources, and open burn", 
                    showarrow = F, 
                    xref='paper', 
                    yref='paper')
      )%>% 
      layout(yaxis =list(title='Change from 1990 Emissions (%)'))
  })
  observeEvent(input$pm25, {
    v$data <- apei$pm25 %>%
      layout(annotations =  
               list(x = 0, y = -0.1, 
                    text = "Excludes dust, and smoke from wildfires, residential sources, and open burn", 
                    showarrow = F, 
                    xref='paper', 
                    yref='paper')
      )%>% 
      layout(yaxis =list(title='B.C. Annual Emission (tonnes/year)'))
  })
  observeEvent(input$pm10, {
    v$data <- apei$pm10 %>%
      layout(annotations =  
               list(x = 0, y = -0.1, 
                    text = "Excludes dust, and smoke from wildfires, residential sources, and open burn", 
                    showarrow = F, 
                    xref='paper', 
                    yref='paper')
      )%>% 
      layout(yaxis =list(title='B.C. Annual Emission (tonnes/year)'))
  })
  observeEvent(input$nox, {
    v$data <- apei$nox %>%
      layout(annotations =  
               list(x = 0, y = -0.1, 
                    text = "Excludes dust, and smoke from wildfires, residential sources, and open burn", 
                    showarrow = F, 
                    xref='paper', 
                    yref='paper')
      )%>% 
      layout(yaxis =list(title='B.C. Annual Emission (tonnes/year)'))
  })
  observeEvent(input$sox, {
    v$data <- apei$sox%>% 
      layout(yaxis =list(title='B.C. Annual Emission (tonnes/year)'))
  })
  observeEvent(input$voc, {
    v$data <- apei$voc  %>% 
      layout(yaxis =list(title='B.C. Annual Emission (tonnes/year)'))
  })
  
  
  output$plot1 <- renderPlotly(v$data %>% 
                                 layout(xaxis = list(title =''),
                                        showlegend = FALSE)
                                 )
}

# Run the application 
shinyApp(ui = ui, server = server)
