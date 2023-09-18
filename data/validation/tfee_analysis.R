#retrieves TFEE details, create a summary

library(envair)
library(dplyr)
library(readr)


lst_tfee <- envair::get_tfee()

# lst_tfee <- 
lst_tfee <- lst_tfee %>%
  select(DATE,PARAMETER,STATION_NAME) %>%
  distinct() %>%
  group_by(DATE,PARAMETER) %>%
  dplyr::summarise(stations = paste(STATION_NAME,collapse = ';')) %>%
  mutate(DATE = format(DATE,'%Y-%m-%d'))


#expoer
write_csv(lst_tfee,'./data/out/tfee_grouped.csv')

#create a list of tfee and summarize effects of TFEE
colnames(lst_tfee)
lst_tfee %>%
  group_by(COMMENT)
