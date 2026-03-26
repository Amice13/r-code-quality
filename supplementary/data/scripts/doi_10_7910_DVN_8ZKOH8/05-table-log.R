rm(list=ls())
gc()
library(tidyverse)
library(gt)
library(xtable)
library(kableExtra)
library(scales)


files =list.files('log')
files = files[grepl('csv',files)]
files1 = files[grep('registration-model', files)]
files2 = files[!grepl('registration',files)]
#files = files[!grepl('full-', files)]

files = c(files2,files1)
rm(files1,files2)


l = lapply(paste0('log/',files), FUN = function(x){
    read_csv(x)%>%
        mutate(Training = as.character(Training),
               Test = as.character(Test))
})

l = do.call(bind_rows,l)%>%
    select(bias:Type, `N (Train)` = n_units_training,
           `States (Train)` = n_state_training,
           `N (Test)` = n_units_test,
           `States (Test)` = n_state_test


           )


w = l %>%
    filter(Type %in% c( 'WLS'))


as_tibble(t(w %>%
          select(Model, Unit, Training, Test, bias:rmse.pct, `N (Train)`, `States (Train)`, `N (Test)`, `States (Test)`)%>%
              mutate(bias = comma(bias, accuracy=1),
                     rmse = comma(rmse, accuracy=1),
                     bias.pct = paste0(round(bias.pct, 1),'%'),
                     rmse.pct = paste0(round(rmse.pct, 1),'%'),
                     `N (Train)` = comma(`N (Train)`, accuracy = 1),
                      `N (Test)` = comma(`N (Test)`, accuracy = 1)

                     )))%>%
    slice(c(3:n()))%>%
   # mutate(across(V1:V23, as.numeric))%>%
    mutate(names = c('Training',
                     'Test',
                     'Bias (Total)' ,
                     'Bias (% of Avg. Vote)',
           'RMSE (Total)' ,
           'RMSE (% of Avg. Vote)',
           'Units (Train)',
           'States (Train)',
           'Units (Test)',
           'States (Test)') ) %>%
    select(names, V3:V6, V1,V2)%>%
    gt%>%
    cols_label(
        names = '',
        V3 = "",
        V4 = "",
        V5 = "",
        V6 = "",
        V1 = "",
        V2 = ""


    )%>%
    tab_spanner(label = 'County', columns = c(2:3)) %>%

    tab_spanner(label = 'District', columns =4:5) %>%
  tab_spanner(label = 'CountyXRM', columns = 6) %>%
  tab_spanner(label = 'DistrictXRM', columns = 7) %>%
    tab_spanner(label = 'Registration', columns = 2:5) %>%
  tab_spanner(label = 'Lagged Vote', columns = 6:7) %>%

  tab_style(style = cell_borders(sides = c("bottom")),
            locations = cells_body(rows = c(6)))  %>%



    as_latex()%>%as.character()%>%
    # substr(.,start=50, stop = nchar(.))%>%
    str_replace_all('longtable','tabular')%>%
    str_replace_all('XRM','')%>%
    write_file('log/model-eval-1-weighted-logged.tex')

