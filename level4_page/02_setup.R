library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# dirs_location <- './data/out'  #local location, two dots for final, one dot for debug
if (0) {
  dirs_location <- './data/out'
}

list.files(dirs_location)


df_exceed_annual <- readRDS(paste(dirs_location,'exceed_annual.Rds',sep='/')) 
df_exceed_month<- readRDS(paste(dirs_location,'exceed_month.Rds',sep='/')) 
df_exceed_seasonal <- readRDS(paste(dirs_location,'exceed_seasonal.Rds',sep='/')) 
