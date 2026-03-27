rm(list=ls())

if (!require(foreign)) {
  install.packages("foreign", repos = "http://cran.us.r-project.org")
  require(foreign)
}
if (!require(rgeos)) {
  install.packages("rgeos", repos = "http://cran.us.r-project.org")
  require(rgeos)
}
if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
if (!require(raster)) {
  install.packages("raster", repos = "http://cran.us.r-project.org")
  require(raster)
}
if(!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cloud.r-project.org")
  require(ggplot2)
}
if(!require(viridis)) {
  install.packages("viridis", repos="http://cloud.r-project.org")
  require(viridis)
}
if(!require(dplyr)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org/")
  require(dplyr)
}
if(!require(gtable)) {
  install.packages("gtable", repos = "https://cloud.r-project.org/")
  require(gtable)
}
if(!require(grid)) {
  install.packages("grid", repos = "https://cloud.r-project.org/")
  require(grid)
}
if(!require(readxl)) {
  install.packages("readxl", repos = "https://cloud.r-project.org/")
  require(readxl)
}
if(!require(magrittr)) {
  install.packages("magrittr", repos = "https://cloud.r-project.org/")
  require(magrittr)
}
if(!require(readstata13)) {
  install.packages("readstata13", repos = "https://cloud.r-project.org/")
  require(readstata13)
}

if(!require(maptools)) {
  install.packages("maptools", repos = "https://cloud.r-project.org/")
  require(maptools)
}

if(!require(geobr)) {
  install.packages("geobr", repos = "https://cloud.r-project.org/")
  require(geobr)
}

if(!require(tidyverse)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org/")
  require(tidyverse)
}

if(!require(here)) {
  install.packages("here", repos = "https://cloud.r-project.org/")
  require(here)
}

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(), 
      panel.background = element_blank(), 
      legend.position = "bottomleft",
      panel.border = element_blank(),
      ...
    )
}


munics <- read_municipality(year=1980) %>% 
              rename(cod1980=code_muni) %>% 
              mutate(cod1980=floor(cod1980/10))

states <- read_state(year=1980)

voting <- read.dta13(here("replication_package","master","output","data","voteshare_1989_v13.dta"))

map_data <- munics %>% left_join(voting,by="cod1980")

first_round<-map_data %>% filter(turno==1)
second_round<-map_data %>% filter(turno==2)

ds_mun_globo <- read.dta13(here("replication_package","master","output","data","mun_globo1.dta"))
ds_globo <- munics %>% left_join(ds_mun_globo,by="cod1980")

col_scale1<-c("0"="NA","1"="red","2"="yellow","3"="orange")
map_all<-ggplot()+geom_sf(data=ds_globo,aes(fill=factor(grupo)),color=NA)+coord_sf()+
  scale_fill_manual(values=col_scale1,labels=c("","Rede Globo","Bandeirantes/SBT","Both"))+
  geom_sf(data=states,color="black",fill=NA,size=1)+
  theme_map()+theme(legend.title = element_blank())+
  theme(legend.position = c(0.3,0.25),legend.text = element_text(size=11))
map_all
ggsave(here("replication_package","master","output","figures","vote_PT_first,path=path_output"), width = 16, height = 9, dpi = 100)


vote_PT_first <- ggplot() +
  geom_sf(data = first_round, aes(fill=sh_Tpt_),color=NA) +
  geom_sf(data=states,color="black",fill=NA,size=1)+
  coord_sf()+scale_fill_gradient(low = "orange", high = "black", space = "Lab",
                                    na.value = "white", guide = guide_legend(title = "Share Lula"))+theme_map()+theme(legend.position = c(0.3,0.25),legend.text = element_text(size=11))
vote_PT_first
ggsave(here("replication_package","master","output","figures","All_stations.png"),plot=vote_PT_first,path=path_output, width = 16, height = 9, dpi = 100)  



vote_PT_second <- ggplot() +
  geom_sf(data = second_round, aes(fill=sh_Tpt_),color=NA) +
  geom_sf(data=states,color="black",fill=NA,size=1)+
  coord_sf()+scale_fill_gradient(low = "orange", high = "black", space = "Lab",
                                 na.value = "white", guide = guide_legend(title = "Share Lula"))+theme_map()+theme(legend.position = c(0.3,0.25),legend.text = element_text(size=11))
vote_PT_second
ggsave(here("replication_package","master","output","figures","Vote_Lula_second.png"),plot=vote_PT_second,path=path_output, width = 16, height = 9, dpi = 100)  


vote_PRN_first <- ggplot() +
  geom_sf(data = first_round, aes(fill=sh_Tprn_),color=NA) +
  geom_sf(data=states,color="black",fill=NA,size=1)+
  coord_sf()+scale_fill_gradient(low = "lightblue", high = "darkblue", space = "Lab",
                                 na.value = "white", guide = guide_legend(title = "Share Collor"))+theme_map()+theme(legend.position = c(0.3,0.25),legend.text = element_text(size=11))
vote_PRN_first
ggsave(here("replication_package","master","output","figures","Vote_Collor_first.png"),plot=vote_PRN_first,path=path_output, width = 16, height = 9, dpi = 100)  



vote_PRN_second <- ggplot() +
  geom_sf(data = second_round, aes(fill=sh_Tprn_),color=NA) +
  geom_sf(data=states,color="black",fill=NA,size=1)+
  coord_sf()+scale_fill_gradient(low = "lightblue", high = "darkblue", space = "Lab",
                                 na.value = "white", guide = guide_legend(title = "Share Collor"))+theme_map()+theme(legend.position = c(0.3,0.25),legend.text = element_text(size=11))
vote_PRN_second
ggsave(here("replication_package","master","output","figures","Vote_Collor_second.png"),plot=vote_PRN_second,path=path_output, width = 16, height = 9, dpi = 100)  




col_scale1<-c("1"="#E69F00","2"="steelblue")
lula_collor_first<-ggplot()+
  geom_sf(data=first_round,aes(fill=factor(winner)),color=NA)+
  coord_sf()+
  geom_sf(data=states,fill=NA,color="black")+
  theme_map()+
  theme(legend.title = element_blank())+
  scale_fill_manual(values=col_scale1,labels=c("Lula","Collor",""))+
  theme(legend.position = c(0.3,0.25),legend.text = element_text(size=11))
lula_collor_first
ggsave(here("replication_package","master","output","figures","Lula_Collor_first.png"),plot=lula_collor_first,path=path_output, width = 16, height = 9, dpi = 100)  


col_scale1<-c("1"="#E69F00","2"="steelblue")
lula_collor_second<-ggplot()+
geom_sf(data=second_round,aes(fill=factor(winner)),color=NA)+
  coord_sf()+
  geom_sf(data=states,fill=NA,color="black")+
  theme_map()+
  theme(legend.title = element_blank())+
  scale_fill_manual(values=col_scale1,labels=c("Lula","Collor",""))+
  theme(legend.position = c(0.3,0.25),legend.text = element_text(size=11))
lula_collor_second
ggsave(here("replication_package","master","output","figures","Lula_Collor_second.png"),plot=lula_collor_second,path=path_output, width = 16, height = 9, dpi = 100)  
