get_emissions <- function(dirs_location = './data/out'){
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# dirs_location <- './data/out'  #local location, two dots for final, one dot for debug
if (0) {
  dirs_location <- './data/out'
}

list.files(dirs_location)


df_apei <- read_csv(paste(dirs_location,'EN_APEI-Can-Prov_Terr.csv',sep='/')) %>%
  filter(Region == 'BC')

#find the columns for the pollutants
cols_apei <- colnames(df_apei)
cols_select <- c('PM25','NOx','SOx','VOC','PM10','TPM','Pb')
df_cols_select <- NULL
for (cols_ in cols_select) {
  cols_select_ <- cols_apei[grepl(cols_,cols_apei,ignore.case = TRUE)]
  if (length(cols_select_)>0) {
    df_cols_select <- df_cols_select %>%
      bind_rows(tibble(
        column_name_new = cols_,
        column_name_old = cols_select_[1]
      ))
    colnames(df_apei)[colnames(df_apei) == cols_select_[1]] <- cols_
  }
}
cols_select <- df_cols_select$column_name_new
cols_apei <- colnames(df_apei)
sectors <- unique(df_apei$Sector)
subsectors <- unique(df_apei$Subsector)
source <- unique(df_apei$Source)

a <- df_apei %>%
  filter(tolower(`Is it a total`) == 'y') %>%
  filter(is.na(Sector)) %>%
  select_at(c('Source','Year',cols_select)) %>%
  tidyr::pivot_longer(cols = -c(Source,Year)) 

cols_select <- unlist(cols_select)
result_bar_stack <- NULL
for (pollutant in cols_select) {
  print(pollutant)
  result_bar_stack_ <- {
    a %>%
      filter(Source != 'GRAND TOTAL') %>%
      filter(name == pollutant) %>%
      plotly::plot_ly(x=~Year,y=~value,color =~Source,
                      type = 'bar',fill = ~Source) %>%
      layout(barmode = 'stack') %>%
    layout(legend = list(orientation = 'h'),
           yaxis = list(title = paste(pollutant,'(tonnes/year)')),
           xaxis = list(title = 'Year')
    )
  }
  
  if (pollutant == 'PM25') {
    result_bar_stackpm25 <- result_bar_stack_
  }
  if (pollutant == 'NOx') {
    result_bar_stacknox <- result_bar_stack_
  }
  if (pollutant == 'SOx') {
    result_bar_stacksox <- result_bar_stack_
  }
  if (pollutant == 'VOC') {
    result_bar_stackvoc <- result_bar_stack_
  }
  if (pollutant == 'PM10') {
    result_bar_stackpm10 <- result_bar_stack_
  }
  if (pollutant == 'TPM') {
    result_bar_stacktpm <- result_bar_stack_
  }
  if (pollutant == 'Pb') {
    result_bar_stackpb <- result_bar_stack_
  }
  
}

result_bar_stack <- list(PM25 = result_bar_stackpm25,PM10 = result_bar_stackpm10,
                         SOx = result_bar_stacksox, NOx = result_bar_stacknox,
                         VOC = result_bar_stackvoc, TPM = result_bar_stacktpm,
                         Pb = result_bar_stackpb)

df_inventory_order <- tribble(
  ~name,~order,
  "PM25",1,
  "NOx" ,2,
  "SOx",3,
  "VOC",4,
  "PM10",5,
  "TPM",6,
  "Pb",7,
  'TPM',8
)
#make line for all
result_totals <- a %>%
  filter(Source == 'GRAND TOTAL') %>%
  filter(!name %in% c('TPM')) %>%
  left_join(df_inventory_order) %>%
plotly::plot_ly(x=~Year,y=~value,color =~reorder(name,order),
                type='scatter',mode='lines+markers',showlegend =T,
                hoverinfo ='y',
                hovertemplate = paste('%{y:,.0f}',sep='')
                
) %>%
  layout(legend = list(orientation = 'h'),
         yaxis = list(title = 'Tonnes per Year'),
         xaxis = list(title = 'Inventory Reporting Year')
  ) %>%
  plotly::layout(hovermode = 'x unified')


#create summary of comparison
result_table <- a %>%
  filter(Year %in% c(1990,2000,2010,max(a$Year))) %>%
  filter(!is.na(value)) %>%
  arrange(Source,Year,name) %>%
  tidyr::pivot_wider(names_from = Year, values_from = value) %>%
  mutate(ref_1990 = 100 *-(`1990` - `2020`)/`1990`) %>%
  mutate(ref_2000 = 100 *-(`2000` - `2020`)/`2000`) %>%
  mutate(ref_2010 = 100 *-(`2010` - `2020`)/`2010`) 

result <- list(totals = result_totals, pollutants = result_bar_stack, table =result_table, data =a)

return(result)
}
