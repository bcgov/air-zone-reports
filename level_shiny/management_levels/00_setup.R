#creates the files required to update this R shiny script

source('./assets/00_setup.R')

management <- get_management_summary_complete()

saveRDS(management,'./data/out/managementsummary_tables.Rds')
