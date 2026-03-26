library(tidyverse)        #for data management, manipulation, and visualization
library(cluster)          #for PAM and Agnes clustering
library(NbClust)          #for cluster count horserace
library(janitor)          #for easy frequency tables with tabyl()
library(factoextra)       #for cluster evaluation, distance, and PCA plots
library(openxlsx)         #for write.xlsx()
library(fmsb)             #for radar plots
###
variable = c("polity_squared_scale_mean", 
             'PopYouthBulgeBy15_new_scale_mean', 
             'gdppc_ln_scale_mean',
             'nAC_perc_scale_mean',
             'imr_annual_mean_centered_scale_mean',
             'sum_PDIS_EDIS_scale_mean')
###

df <- readxl::read_xlsx('output/3clusters_summary clusterfix 20220210.xlsx')
df_case <- df %>% 
  select(c(method, Cluster,polity_squared_scale_mean:sum_PDIS_EDIS_scale_mean)) %>% 
  filter(str_detect(method, '_pitf_')) # _pitf_ or _prio_

### Global Min-Max
df_case_max <- df_case %>% 
  summarise(across(polity_squared_scale_mean:sum_PDIS_EDIS_scale_mean, max))%>% 
  mutate(method="Max")
df_case_min <- df_case %>% 
  summarise(across(polity_squared_scale_mean:sum_PDIS_EDIS_scale_mean, min)) %>% 
  mutate(method="Min")


### Binding with Cluster data
cl = 1
df_case_cl <- df_case %>% filter(Cluster==cl)
df_case_cl_maxmin <- df_case_cl %>%
  bind_rows(df_case_max, df_case_min,.) %>% select(-Cluster) %>%
  rename('GDP per capita' = 'gdppc_ln_scale_mean',
         'Infant Mortality Rate' = 'imr_annual_mean_centered_scale_mean',
         'Governance' = 'polity_squared_scale_mean',
         'Discrimination' = 'sum_PDIS_EDIS_scale_mean',
         'Border conflicts' = 'nAC_perc_scale_mean',
         'Youth bulge' = 'PopYouthBulgeBy15_new_scale_mean') %>%
  column_to_rownames(var='method')
###
### Radar Plot
areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25),
           rgb(0, 0, 1, 0.25))
#
spider_plot_cl <- radarchart(
  df_case_cl_maxmin,
  pcol=1:8, plty=1, plwd = 3, # line color, type, width
  #pfcol = areas, # filling areas
  cglcol="grey", cglty=4, cglwd=3, # grid line color, type, width
  axistype=1, axislabcol="grey", # axis type and color
  vlcex=1.5 # vlabel font size
)
legend(
       x=1, y=.6,
       legend =  toupper(rownames(df_case_cl_maxmin)[3:10]),
       bty = "n", pch=20, col=1:8,
       text.col = 'black', cex=.9, pt.cex=2,
       y.intersp = .5, x.intersp = .2
)


