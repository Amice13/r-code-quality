rm(list=ls())
gc()
library(tidyverse)
library(gt)
library(xtable)
library(kableExtra)
library(scales)


files =list.files('results')
files1 = files[grep('registration-model', files)]
files2 = files[!grepl('registration',files)]
files = files[!grepl('full-', files)]

files = c(files2,files1)
rm(files1,files2)


l = lapply(paste0('results/',files), FUN = function(x){
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
u = l %>%
    filter(Type %in% c( 'OLS'))

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
    select(names, V10:V13, V8,V9)%>%
    gt%>%
    cols_label(
        names = '',
        V10 = "",
        V11 = "",
        V12 = "",
        V13 = "",
        V8 = "",
        V9 = ""
        

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
    write_file('tables/model-eval-1-weighted.tex')

as_tibble(t(u %>%
              select(Model, Unit, Training, Test, bias:rmse.pct, `N (Train)`, `States (Train)`, `N (Test)`, `States (Test)`)%>%
              mutate(bias = comma(bias, accuracy = 1),
                     rmse = comma(rmse, accuracy = 1),
                     bias.pct = paste0(round(bias.pct, 1),'%'),
                     rmse.pct = paste0(round(rmse.pct, 1),'%'),
                     `N (Train)` = comma(`N (Train)`, accuracy = 1),
                     `N (Test)` = comma(`N (Test)`, accuracy = 1)
                     
              ))) %>%
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
  select(names, V10:V13, V8,V9)%>%
  gt%>%
  cols_label(
    names = '',
    V10 = "",
    V11 = "",
    V12 = "",
    V13 = "",
    V8 = "",
    V9 = ""
    
    
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

    write_file('tables/model-eval-1-unweighted.tex')




####
as_tibble(t(w %>%
              select(Model, Unit, Training, Test, bias:rmse.pct, `N (Train)`, `States (Train)`, `N (Test)`, `States (Test)`)%>%
              mutate(bias = comma(bias, accuracy = 1),
                     rmse = comma(rmse, accuracy = 1),
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
    select(names,V2:V7, V1)%>%
    gt%>%
    cols_label(
        names = '',
        V2 = "",
        V3 = "",
        V4 = "",
        V5 = "",
        V6 = "",
        V7 = "",
        V1 = ""
        
    )%>%
    tab_spanner(label = 'County', columns = c(2:3)) %>%
    tab_spanner(label = 'District', columns =4:5) %>%
  tab_spanner(label = 'CountyXRM', columns = c(6:7)) %>%
  tab_spanner(label = 'DistrictXRM', columns = c(8)) %>%
  
    tab_spanner(label = 'Demographics', columns = 2:5) %>%
  tab_spanner(label = 'Early Vote', columns = 6:7) %>%
  tab_spanner(label = 'Competition', columns = 8) %>%
  
  tab_style(style = cell_borders(sides = c("bottom")),
            locations = cells_body(rows = c(6)))  %>% 
    as_latex()%>%as.character()%>%
    # substr(.,start=50, stop = nchar(.))%>%
    str_replace_all('longtable','tabular')%>%
    str_replace_all('XRM','')%>%
    write_file('tables/model-eval-2-weighted.tex')

as_tibble(t(u %>%
              select(Model, Unit, Training, Test, bias:rmse.pct, `N (Train)`, `States (Train)`, `N (Test)`, `States (Test)`)%>%
              mutate(bias = comma(bias, accuracy = 1),
                     rmse = comma(rmse, accuracy = 1),
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
  select(names,V2:V7, V1)%>%
  gt%>%
  cols_label(
    names = '',
    V2 = "",
    V3 = "",
    V4 = "",
    V5 = "",
    V6 = "",
    V7 = "",
    V1 = ""
    
  )%>%
  tab_spanner(label = 'County', columns = c(2:3)) %>%
  tab_spanner(label = 'District', columns =4:5) %>%
  tab_spanner(label = 'CountyXRM', columns = c(6:7)) %>%
  tab_spanner(label = 'DistrictXRM', columns = c(8)) %>%
  
  tab_spanner(label = 'Demographics', columns = 2:5) %>%
  tab_spanner(label = 'Early Vote', columns = 6:7) %>%
  tab_spanner(label = 'Competition', columns = 8) %>%
  
  tab_style(style = cell_borders(sides = c("bottom")),
            locations = cells_body(rows = c(6)))  %>% 
  as_latex()%>%as.character()%>%
  # substr(.,start=50, stop = nchar(.))%>%
  str_replace_all('longtable','tabular')%>%
  str_replace_all('XRM','')%>%
  write_file('tables/model-eval-2-unweighted.tex')



### combined models
rm(list=ls())
gc()

files =list.files('results')
files = files[grepl('registration-',files)]
files = files[!grepl('registration-model', files)]
files = files[!grepl('full-', files)]



l = lapply(paste0('results/',files), FUN = function(x){
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
u = l %>%
  filter(Type %in% c( 'OLS'))

as_tibble(t(w %>%
              select(Model, Unit, Training, Test, bias:rmse.pct, `N (Train)`, `States (Train)`, `N (Test)`, `States (Test)`)%>%
              mutate(bias = comma(bias, accuracy = 1),
                     rmse = comma(rmse, accuracy = 1),
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
  select(names, V8,V9,V2:V7, V1)%>%
  gt%>%
  cols_label(
    names = '',
    V8 = "",
    V9 = "",
    V2 = "",
    V3 = "",
    V4 = "",
    V5 = "",
    V6 = "",
    V7 = "",
    V8 = "",
    V9 = "",
    V1 = ""
  )%>%
  tab_spanner(label = 'County', columns = 2) %>%
  
  tab_spanner(label = 'District', columns =3) %>%
  tab_spanner(label = 'CountyXRM', columns = 4:5) %>%
  tab_spanner(label = 'DistrictXRM', columns = 6:7) %>%
  tab_spanner(label = 'CountyXRM2', columns = 8:9) %>%
  tab_spanner(label = 'DistrictXRM2', columns = 10) %>%
  tab_spanner(label = 'Registration', columns = 1) %>%
  tab_spanner(label = '+ Lagged Vote', columns = 2:3) %>%
  tab_spanner(label = '+ Demographics', columns = 4:7) %>%
  tab_spanner(label = '+ Early Vote', columns = 8:9) %>%
  tab_spanner(label = '+ Competition', columns = 10) %>%
  
  tab_style(style = cell_borders(sides = c("bottom")),
            locations = cells_body(rows = c(6)))  %>% 
  
  
  
  as_latex()%>%as.character()%>%
  # substr(.,start=50, stop = nchar(.))%>%
  str_replace_all('longtable','tabular')%>%
  str_replace_all('XRM2','')%>%
  
  str_replace_all('XRM','')%>%
  write_file('tables/model-eval-combined-weighted.tex')

as_tibble(t(u %>%
              select(Model, Unit, Training, Test, bias:rmse.pct, `N (Train)`, `States (Train)`, `N (Test)`, `States (Test)`)%>%
              mutate(bias = comma(bias, accuracy = 1),
                     rmse = comma(rmse, accuracy = 1),
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
  select(names, V8,V9,V2:V7, V1)%>%
  gt%>%
  cols_label(
    names = '',
    V8 = "",
    V9 = "",
    V2 = "",
    V3 = "",
    V4 = "",
    V5 = "",
    V6 = "",
    V7 = "",
    V8 = "",
    V9 = "",
    V1 = ""
  )%>%
  tab_spanner(label = 'County', columns = 2) %>%
  
  tab_spanner(label = 'District', columns =3) %>%
  tab_spanner(label = 'CountyXRM', columns = 4:5) %>%
  tab_spanner(label = 'DistrictXRM', columns = 6:7) %>%
  tab_spanner(label = 'CountyXRM2', columns = 8:9) %>%
  tab_spanner(label = 'DistrictXRM2', columns = 10) %>%
  tab_spanner(label = 'Registration', columns = 1) %>%
  tab_spanner(label = '+ Lagged Vote', columns = 2:3) %>%
  tab_spanner(label = '+ Demographics', columns = 4:7) %>%
  tab_spanner(label = '+ Early Vote', columns = 8:9) %>%
  tab_spanner(label = '+ Competition', columns = 10) %>%
  
  tab_style(style = cell_borders(sides = c("bottom")),
            locations = cells_body(rows = c(6)))  %>% 
  
  
  as_latex()%>%as.character()%>%
  # substr(.,start=50, stop = nchar(.))%>%
  str_replace_all('longtable','tabular')%>%
  str_replace_all('XRM2','')%>%
  
  str_replace_all('XRM','')%>%
  write_file('tables/model-eval-combined-unweighted.tex')




# full models

rm(list=ls())
gc()

files =list.files('results')
files = files[grepl('full-', files)]



l = lapply(paste0('results/',files), FUN = function(x){
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
u = l %>%
  filter(Type %in% c( 'OLS'))

as_tibble(t(w %>%
              select(Model, Unit, Training, Test, bias:rmse.pct, `N (Train)`, `States (Train)`, `N (Test)`, `States (Test)`)%>%
              mutate(bias = comma(bias, accuracy = 1),
                     rmse = comma(rmse, accuracy = 1),
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
  select(names, V2,V1)%>%
  gt%>%
  cols_label(
    names = '',
    V1 = "",
    V2 = ""
  )%>%
  tab_spanner(label = 'County', columns = 2) %>%
  
  tab_spanner(label = 'District', columns =3) %>%
  tab_spanner(label = 'Registration', columns = 1) %>%
  tab_spanner(label = '\\shortstack{+ Lagged Vote\\\\ + Demographics\\\\ + Early Vote}', columns = 2) %>%
  tab_spanner(label = '\\shortstack{+ Lagged Vote\\\\ + Demographics\\\\ + Competition}', columns = 3) %>%
  
  tab_style(style = cell_borders(sides = c("bottom")),
            locations = cells_body(rows = c(6)))  %>% 
  
  
  
  as_latex()%>%as.character()%>%
  # substr(.,start=50, stop = nchar(.))%>%
  str_replace_all('longtable','tabular')%>%

  str_replace_all('XRM','')%>%
  write_file('tables/model-eval-full-weighted.tex')

as_tibble(t(u %>%
              select(Model, Unit, Training, Test, bias:rmse.pct, `N (Train)`, `States (Train)`, `N (Test)`, `States (Test)`)%>%
              mutate(bias = comma(bias, accuracy = 1),
                     rmse = comma(rmse, accuracy = 1),
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
  select(names, V2,V1)%>%
  gt%>%
  cols_label(
    names = '',
    V1 = "",
    V2 = ""
  )%>%
  tab_spanner(label = 'County', columns = 2) %>%
  
  tab_spanner(label = 'District', columns =3) %>%
  tab_spanner(label = 'Registration', columns = 1) %>%
  tab_spanner(label = '\\shortstack{+ Lagged Vote\\\\ + Demographics\\\\ + Early Vote}', columns = 2) %>%
  tab_spanner(label = '\\shortstack{+ Lagged Vote\\\\ + Demographics\\\\ + Competition}', columns = 3) %>%
  
  tab_style(style = cell_borders(sides = c("bottom")),
            locations = cells_body(rows = c(6))) %>% 
  
  
  as_latex()%>%as.character()%>%
  # substr(.,start=50, stop = nchar(.))%>%
  str_replace_all('longtable','tabular')%>%

  write_file('tables/model-eval-full-unweighted.tex')

## overfitting

rm(list=ls())
l = c(
  "registration-model-county-stats-weighted.csv" ,
  "registration-model-district-stats-weighted.csv",
  "full-registration-lagged-vote-demographics-early-vote-model-county-stats-weighted.csv",
  "full-registration-lagged-vote-demographics-competition-model-district-stats-weighted.csv"
  ) |>
  lapply(FUN = function(x){
    read_csv(paste0('results/',x))%>%
      mutate(Training = as.character(Training),
             Test = as.character(Test))
  }) |>
  do.call(what = 'bind_rows') |>
  filter()|>
  drop_na()

l = l %>%
  select(Model, Training, Test, Unit, bias,
         bias.train,
         bias.pct,
         bias.pct.train,
         rmse,
         rmse.train,
         rmse.pct,
         rmse.pct.train
)


