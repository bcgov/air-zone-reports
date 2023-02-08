
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
