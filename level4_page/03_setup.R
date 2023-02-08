#' create graphs for days exceeding
#' 
#' @param exceedances is the result of get_PM_exceedance() function
graph_exceedance <- function(exceedances,airzone = NULL) {
  if (0) {
    source('./level4_page/02_setup.R')
    dirs_location <- './data/out'
  }
  
  require(tidyr)
  #set order for the seasons
  df_seasons <- tribble(
    ~seasons,~order,
    'Winter',4,
    'Spring',3,
    'Summer',2,
    'Fall',1
  )
  
  exceedances <- get_PM_exceedancesummary()
  
  # View(exceedances$season_stations)
  if (is.null(airzone)) {
    airzone <- 'BC'
  }
  df_annual <- exceedances$annual
  
  df_seasonal_tfee <- exceedances$season_tfee %>%
     mutate(tfee = TRUE)
  
  df_seasonal <- exceedances$season %>%
    mutate(tfee = FALSE) %>%
    bind_rows(df_seasonal_tfee) %>%
    pivot_wider(names_from = tfee, values_from =days_exceed) %>%
    dplyr::rename(`Wildfire Days` = `TRUE`,`Total` = `FALSE`) %>%
    mutate(`Wildfire Days` = ifelse(is.na(`Wildfire Days`),0,`Wildfire Days`))%>%
    mutate(`No Wildfire`= `Total` - `Wildfire Days`) %>%
    select(-Total) %>%
    pivot_longer(cols = c(`Wildfire Days`,`No Wildfire`)) %>%
    mutate(seasons = paste(seasons,'(',name,')',sep=''))
    
    
  
  
  
  
  
  
  # result_totals <-
  p_seasonal <- df_seasonal %>%
    left_join(df_seasons) %>%
    filter(AIRZONE %in% airzone) %>%
    filter(year >=2000) %>%
   
    plotly::plot_ly(x=~year,y=~value,color = ~reorder(seasons,order),
                    type='bar',mode='bar',showlegend =T,
                    hoverinfo ='y',
                    hovertemplate = paste('%{y:,.0f}',' days',sep=''),
                    colors = c("navajowhite2",
                               "navajowhite3",
                               "seagreen3",
                               "seagreen4",
                               "red3",
                               "red4",
                               "slategray2",
                               'slategray4'
                                            
                                            
                                            )
                    
    ) %>%
    layout(legend = list(orientation = 'h'),
           yaxis = list(title = 'Number of Days with High PM<sub>2.5</sub> Levels'),
           xaxis = list(title = 'Year')
    ) %>%
    plotly::layout(hovermode = 'x unified',
                   barmode = 'stack',legend = list(x = 0.1, y = 0.9))
  
  p_annual <-   df_annual %>%
    filter(!AIRZONE %in% c('BC',NA)) %>% 
    filter(year>=2000) %>%
    plotly::plot_ly(x=~year,y=~days_exceed,color = ~AIRZONE,
                    type='scatter',mode='lines',showlegend =T,
                    hoverinfo ='y',
                    hovertemplate = paste('%{y:,.0f}',' days',sep='')
                    
    ) %>%
    layout(legend = list(orientation = 'h'),
           yaxis = list(title = 'Number of Days with High PM<sub>2.5</sub> Levels'),
           xaxis = list(title = 'Year')
    ) %>%
    plotly::layout(hovermode = 'x unified',
                   barmode = 'stack',legend = list(x = 0.1, y = 0.9))
  
  
  result <- list(plot_seasonal = p_seasonal,plot_annual = p_annual, data = exceedances)
 
   return(result)
  
}



