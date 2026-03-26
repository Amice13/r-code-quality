#**********************************************************************************************
#R version 4.2.1 
#**********************************************************************************************



#install packages
install.packages(c("devtools","tidyverse","dplyr","data.table",
                   "magrittr","leaflet","leaflet.minicharts"))
install_github("wch/webshot")

#load packages----
library(devtools)
library(tidyverse)
library(dplyr)
library(data.table)
library(magrittr)
library(leaflet)
library(leaflet.minicharts)
library(webshot)
webshot::install_phantomjs()
library(htmlwidgets)


#**********************************************************************************************
#load data files----
#**********************************************************************************************

collection_data <- read.csv("Mosquito_collection_data.csv")
garithe_data <- read.csv("Garithe_data.csv") 
house_coordinates <- read.csv("house_coordinates.csv")


#**********************************************************************************************
#Mosquito counts----
#counts indoor_outdoor
#**********************************************************************************************

Mosquito_counts <- collection_data %>%
  select(site, Anopheles_gambiae, Anopheles_funestus, Culex_spp, other_Anopheles) %>%
  gather(key = "species", value = "count",
         Anopheles_gambiae, Anopheles_funestus, Culex_spp, other_Anopheles) %>%
  group_by(species,site) %>%
  summarise(n=sum(count))

#**********************************************************************************************
##percent per species
#**********************************************************************************************

Percent_per_species <- collection_data %>%
  select(site, Anopheles_gambiae, Anopheles_funestus, Culex_spp, other_Anopheles) %>%
  gather(key = "species", value = "count",
         Anopheles_gambiae, Anopheles_funestus, Culex_spp, other_Anopheles) %>%
  group_by(species) %>%
  summarise(n=sum(count),
            percent=(n/18802)*100)


#**********************************************************************************************
#An gambiae complex stats
#**********************************************************************************************

An_gambiae_sl_counts <- garithe_data %>%
  filter(mosquito.genus=="An. gambiae") %>% 
  group_by(species) %>%
  count() %>%
  mutate(percent=(n/518)*100)

#**********************************************************************************************
#Bloodmeal source----
##Table 2----
#**********************************************************************************************

Table_2 <- garithe_data %>% 
  filter(mosquito.genus == "An. gambiae" & status == "BF" ) %>% 
  select(species, BM_ELISA,site) %>% 
  group_by(species,BM_ELISA,site) %>% 
  count() %>% 
  group_by(species) %>% 
  mutate(BI= (n/sum(n)),
         percent= BI*100)


Table_2

#**********************************************************************************************
#Figures----
##Figure 1----
#**********************************************************************************************


Figure_1 <- garithe_data %>% 
  ggplot(aes(mosquito.genus,fill=species)) +
  geom_bar(position = "fill") +
  ylab("proportions") +
  xlab("mosquito genus") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold.italic",size = "12"),
        legend.text = element_text(face = "bold.italic",size = "12"),
        legend.title = element_text(face = "bold"),
        axis.text = element_text(size = "12",face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "top") +
  facet_grid(.~mosquito.genus, scales = "free")

Fig_1 <- Figure_1 + theme(text = element_text(size = 20))
Fig_1

tiff(filename = "Fig_1.png",width = 8,  height = 10, units = 'in',
     res = 600, compression = "lzw", type = "cairo")
print(Fig_1)
dev.off()


##Figure 2----
Figure_2 <- garithe_data  %>% 
  filter(mosquito.genus == "An. gambiae" & allele != "not_genotyped") %>%
  ggplot(aes(species, fill=allele)) +
  geom_bar(stat = "count",position = "fill") +
  theme_bw() +
  theme(strip.text = element_text(face = "bold.italic"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(face = "bold"),
        legend.text = element_text(face = "bold.italic"),
        legend.position = "top") + 
  facet_wrap(~species,scales = "free") +
  ylab("proportion")

Fig_2 <- Figure_2 +theme(text = element_text(size = 20))
Fig_2

tiff(filename = "Figure 2.png",width = 8,  height = 10, units = 'in',
     res = 600, compression = "lzw", type = "cairo")
print(Fig_2)
dev.off()


##Figure 3----
Figure_3 <- garithe_data  %>% 
  filter(mosquito.genus == "An. gambiae" & allele != "not_genotyped") %>% 
  ggplot(aes(as.factor(house), fill=allele)) +
  geom_bar(stat = "count",position = "fill") +
  theme_bw() +
  theme(strip.text = element_text(face = "italic"),
        legend.text = element_text(face = "italic"),
        legend.position = "top") + 
  facet_wrap(~species,scales = "free") +
  ylab("proportion") +
  xlab("house")

Fig_3 <- Figure_3 + theme(text = element_text(size = 20))
Fig_3

tiff(filename = "Fig. 3.png",width = 12,  height = 7, units = 'in',
     res = 600, compression = "lzw", type = "cairo")
print(Fig_3)
dev.off()



#**********************************************************************************************
#Maps----
#**********************************************************************************************

##reshape data

allele_house <- garithe_data %>%
  select(house,allele) %>% 
  filter(allele != "") %>% 
  group_by(house) %>% 
  count(allele) %>% 
  spread(allele,n)%>% 
  dplyr::left_join(house_coordinates, by = "house")



Map <- leaflet(allele_house) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircleMarkers(lng = allele_house$longitude,
                   lat = allele_house$latitude,
                   radius = 1.5) %>% 
  addMinicharts(
    allele_house$longitude, allele_house$latitude,
    type = "pie",
    chartdata = allele_house[, c("*R2/R2","*R3/R3", "*R3/S2", "*S1/S1","*S2/S2")]
  ) 

Map



###Supplementary Figure1----

Supplementary_Figure1 <- leaflet(house_coordinates) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircles(lng = house_coordinates$longitude,
             lat = house_coordinates$latitude,
             color = "blue",
             label = as.character(house_coordinates$house), 
             labelOptions = labelOptions(noHide = T, direction = "left",))


Supplementary_Figure1

#**********************************************************************************************
##Supplementary tables
#**********************************************************************************************


Table_S1 <- garithe_data %>% 
  filter(sporozoite_pcr == "negative" |sporozoite_pcr == "positive") %>% 
  group_by(species, allele, sporozoite_pcr) %>%
  summarise(count = n())

Table_S1


Table_S2 <- garithe_data %>% 
  filter(species == "An. merus" |species == "An. arabiensis") %>%
  filter(sporozoite_pcr != "not_done") %>% 
  group_by(species, site, sporozoite_pcr) %>% 
  summarise(number_of_mosquitoes = n()) %>%
  group_by(species, site) %>% 
  mutate(sum1 = sum(number_of_mosquitoes),
         percent = number_of_mosquitoes/sum1*100) %>% 
  select(-sum1)

Table_S2

#**********************************************************************************************
##END
#**********************************************************************************************
