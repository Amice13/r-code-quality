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
#-------------
### sample mean
df_master <- readxl::read_xlsx('data/Minerva_Clustering_20220209_MARupdated_Vdemadded.xlsx')
df_master <- df_master %>% 
  select(-c(v2x_libdem:population_ln)) %>% filter(complete.cases(.)) %>% 
  mutate_at(vars(polity_squared:sum_PDIS_EDIS), 
            list(scale = ~as.numeric(scale(., center=min(.), scale=max(.)-min(.)))))
df_master_mean <- df_master %>% 
  summarise(across(polity_squared_scale:sum_PDIS_EDIS_scale, mean))%>% 
  mutate(method="Globalmean")
#------------
### PITF mean
df_pitf <- readxl::read_xlsx('data/PITF_2yrpeace_2yrlag.xlsx')
df_pitf <- df_master %>% 
  filter(country_year_iv %in% df_pitf$country_year_iv)
df_pitf_mean <- df_pitf %>% 
  summarise(across(polity_squared_scale:sum_PDIS_EDIS_scale, mean))%>% 
  mutate(method="PITFmean")
#------------
### cluster mean
df_cluster <- readxl::read_xlsx('output/3clust_eucl_pitf.xlsx', sheet = "membership")
df_cluster <- df_cluster %>% 
  mutate(country_year_iv = paste(country,"_",year-2,sep = "" ))
df_cluster_1 <- df_master %>% 
  filter(country_year_iv %in% df_cluster[df_cluster$agnes_cluster==1,]$country_year_iv)
df_cluster_1_mean <- df_cluster_1 %>% 
  summarise(across(polity_squared_scale:sum_PDIS_EDIS_scale, mean))%>% 
  mutate(method="C1mean")
#
df_cluster_2 <- df_master %>% 
  filter(country_year_iv %in% df_cluster[df_cluster$agnes_cluster==2,]$country_year_iv)
df_cluster_2_mean <- df_cluster_2 %>% 
  summarise(across(polity_squared_scale:sum_PDIS_EDIS_scale, mean))%>% 
  mutate(method="C2mean")
#
df_cluster_3<- df_master %>% 
  filter(country_year_iv %in% df_cluster[df_cluster$agnes_cluster==3,]$country_year_iv)
df_cluster_3_mean <- df_cluster_3 %>% 
  summarise(across(polity_squared_scale:sum_PDIS_EDIS_scale, mean))%>% 
  mutate(method="C3mean")
#----------
### Rbind
df_mean_bind <- bind_rows(df_master_mean, df_pitf_mean,
                      df_cluster_1_mean, df_cluster_2_mean, df_cluster_3_mean)

df_mean_bind_max <- df_mean_bind %>% 
  summarise(across(polity_squared_scale:sum_PDIS_EDIS_scale, max))%>% 
  mutate(method="Max")
df_mean_bind_min <- df_mean_bind %>% 
  summarise(across(polity_squared_scale:sum_PDIS_EDIS_scale, min)) %>% 
  mutate(method="Min")

df_mean_bind<- bind_rows(df_mean_bind_max, df_mean_bind_min, df_mean_bind) %>% 
rename('GDP per capita' = 'gdppc_ln_scale',
       'Infant Mortality Rate' = 'imr_annual_mean_centered_scale',
       'Governance' = 'polity_squared_scale',
       'Discrimination' = 'sum_PDIS_EDIS_scale',
       'Border conflicts' = 'nAC_perc_scale',
       'Youth bulge' = 'PopYouthBulgeBy15_new_scale') %>%
  column_to_rownames(var='method')

### Radar Plot
spider_plot_cl1 <- radarchart(
  df_mean_bind[c(1:4, 5),],
  pcol=c(2,3, rgb(0.2,0.5,0.5,0.5)), 
  plty=c(4,4,1), plwd = 3, # line color, type, width
  pfcol=c(rgb(0.5,0.5,0.5,0.25),
          rgb(0.5,0.5,0.5,0.25),
          rgb(0.2,0.5,0.5,0.5)
          ), # filling areas
  cglcol="grey", cglty=1, cglwd=3, # grid line color, type, width
  axistype=1, axislabcol="grey", # axis type and color
  vlcex=1.5 # vlabel font size
)
legend(
       x=.5, y=1.3,
       legend =  c(rownames(df_mean_bind[c(3:4),]), "Anocratic - Younger - Less Developed"),
       bty = "n", pch=20, col=c(2,3, rgb(0.2,0.5,0.5,0.5)),
       text.col = 'black', cex=.9, pt.cex=2,
       y.intersp = .5, x.intersp = .2
)

spider_plot_cl2 <- radarchart(
  df_mean_bind[c(1:4, 6),],
  pcol=c(2,3, rgb(0.2,0.2,0.5,0.5)), 
  plty=c(4,4,1), plwd = 3, # line color, type, width
  pfcol=c(rgb(0.5,0.5,0.5,0.25),
          rgb(0.5,0.5,0.5,0.25),
          rgb(0.2,0.2,0.5,0.5)
  ), # filling areas
  cglcol="grey", cglty=1, cglwd=3, # grid line color, type, width
  axistype=1, axislabcol="grey", # axis type and color
  vlcex=1.5 # vlabel font size
)
legend(
  x=.5, y=1.3,
  legend =  c(rownames(df_mean_bind[c(3:4),]), "Older - Wealthier - Moderate Discrimination"),
  bty = "n", pch=20, col=c(2,3, rgb(0.2,0.2,0.5,0.5)),
  text.col = 'black', cex=.9, pt.cex=2,
  y.intersp = .5, x.intersp = .2
)

spider_plot_cl3 <- radarchart(
  df_mean_bind[c(1:4, 7),],
  pcol=c(2,3, rgb(0.7,0.5,0.1,0.5)), 
  plty=c(4,4,1), plwd = 3, # line color, type, width
  pfcol=c(rgb(0.5,0.5,0.5,0.25),
          rgb(0.5,0.5,0.5,0.25),
          rgb(0.7,0.5,0.1,0.5)
  ), # filling areas
  cglcol="grey", cglty=1, cglwd=3, # grid line color, type, width
  axistype=1, axislabcol="grey", # axis type and color
  vlcex=1.5 # vlabel font size
)
legend(
  x=.5, y=1.3,
  legend =  c(rownames(df_mean_bind[c(3:4),]), "Higher Discrimination - Worse Neighborhood - Younger"),
  bty = "n", pch=20, col=c(2,3, rgb(0.7,0.5,0.1,0.5)),
  text.col = 'black', cex=.9, pt.cex=2,
  y.intersp = .5, x.intersp = .2
)
