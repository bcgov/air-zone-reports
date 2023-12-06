#creates the files required to update this R shiny script
#please save into github to update public content

 
source('./assets/00_setup.R')

#create management level
management <- get_management_summary_complete('./data/out')
saveRDS(management,'./data/out/managementsummary_tables.Rds')

#create bar graphs
caaqs_bargraph <- plot_bar_caaqs_complete()
saveRDS(caaqs_bargraph,'./data/out/caaqs_bargraph.Rds')
