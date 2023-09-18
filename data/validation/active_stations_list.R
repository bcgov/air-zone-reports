#summarise stations in air zone reports

list.files('./data/out')
lst_stations <- envair::listBC_stations(use_CAAQS = TRUE) %>%
  select(site,STATION_NAME,AIRZONE)

lst_include <- readr::read_csv('./data/out/caaqs_results.csv') %>%
  filter(year %in% 2021) %>%
  select(parameter,site) %>% distinct() %>%
  mutate(active = TRUE) %>%
    left_join(lst_stations) %>%
  distinct()


  tibble(
    parameter = unique(lst_include$parameter)
  ) %>%
    merge(lst_include %>% select(site,STATION_NAME,AIRZONE) %>%distinct()) %>%
    left_join(lst_include) %>% 
    mutate(active = ifelse(is.na(active),FALSE,active)) %>%
    tidyr::pivot_wider(names_from = parameter, values_from = active) %>%
    
    write_csv('./data/validation/caaqs_stations.csv')
    
