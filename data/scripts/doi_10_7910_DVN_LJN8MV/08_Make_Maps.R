rm(list = ls()) ##Clean up workspace
library(ggplot2)
library(dplyr)
library(reshape2)
library(scales)
library(lubridate)
library(RColorBrewer)
library(foreign)
library(pals)

birth <- read.csv("Data/Clean/FULL Birth record CLEAN.csv")

birth <- filter(birth, anio_nac ==2013 | anio_nac == 2014)

birth$Treat <- ifelse(birth$treatment == "True",1,0)

#Clean birthweight
birth$peso <- as.character(birth$peso)
birth <- birth %>%
  mutate(peso = ifelse(peso == "Sin información" | peso == "999.0" | peso == "999" | peso == "99.0" | peso == "9999.0" | peso == "9999"| peso == "0.0", "",peso))
birth$peso <- as.numeric(birth$peso)

#Clean pre-natal visits
birth$con_pren <- as.character(birth$con_pren)
birth <- birth %>%
  mutate(con_pren = ifelse(con_pren == "Sin información" | con_pren == "99.0","",con_pren))
birth$con_pren <- as.numeric(birth$con_pren)
birth <- birth %>%
  mutate(con_pren = ifelse(con_pren>=1,1,con_pren))

#Clean birth-care variables:
birth$asis_por <- as.character(birth$asis_por)
birth <- birth %>%
  mutate(asis_por = ifelse(asis_por == "1.0", "Médico/a",asis_por)) %>%
  mutate(asis_por = ifelse(asis_por == "2.0", "Obstetriz/Obstetra",asis_por)) %>%
  mutate(asis_por = ifelse(asis_por == "3.0", "Enfermero/a",asis_por)) %>%
  mutate(asis_por = ifelse(asis_por == "4.0", "Auxiliar de enfermería",asis_por)) %>%
  mutate(asis_por = ifelse(asis_por == "5.0", "Partero/a calificado/a",asis_por)) %>%
  mutate(asis_por = ifelse(asis_por == "6.0", "Partero/a no calilficado/a",asis_por)) %>%
  mutate(asis_por = ifelse(asis_por == "7.0", "Otro",asis_por)) %>%
  mutate(trained_care = ifelse(asis_por == "Médico/a" | asis_por == "Obstetriz/Obstetra" | asis_por == "Enfermero/a"  | asis_por == "Partero/a calilficado/a",1,0))

#generate home-birth and public-birth variables
birth$lugar_ocur <- as.character(birth$lugar_ocur)
birth <- birth %>%
  mutate(home_birth = ifelse(lugar_ocur == "Casa" | lugar_ocur == "Otro" | lugar_ocur == "5.0" | lugar_ocur == "6.0",1,0)) %>%
  mutate(public_birth = ifelse(lugar_ocur == "Establecimiento del Ministerio de Salud" | lugar_ocur == "Establecimiento del IESS" | lugar_ocur == "Establecimiento de la Junta de Beneficencia" |lugar_ocur == "Otro Establecimiento Público" | lugar_ocur == "1.0" | lugar_ocur == "2.0" | lugar_ocur == "3.0" | lugar_ocur == "4.0",1,0))

#Collapse on the necessary variables
birth_coll <- birth %>%
  group_by(CODIGO) %>%
  dplyr::summarize(mean_weight = mean(peso,na.rm=TRUE), mean_visits = mean(con_pren, na.rm=TRUE),
                   mean_train = mean(trained_care, na.rm=TRUE), mean_home = mean(home_birth, na.rm=TRUE), 
                   mean_public = mean(public_birth, na.rm=TRUE), stunting = mean(Stunting, na.rm = TRUE),
                   Treatment = first(treatment))

library (sp)
library (rgdal)
library(rgeos)
library(classInt)
Ecua <- readOGR(dsn = "Data/Geographic/ecu_adm_inec_20190724_shp",layer = "ecu_admbnda_adm3_inec_20190724")
Prov <- readOGR(dsn ="Data/Geographic/ecu_adm_inec_20190724_shp", layer = "ecu_admbnda_adm1_inec_20190724")
Ecua$CODIGO <- gsub("EC", "", Ecua$ADM3_PCODE)
for (i in 1:length(Ecua$CODIGO)){
  if (substr(Ecua$CODIGO[i],1,1) =="0"){
    Ecua$CODIGO[i] = substr(Ecua$CODIGO[i], start = 2, stop = 6)
  }
}
Ecua$CODIGO <- as.numeric(Ecua$CODIGO)

Ecuador <- merge(Ecua, birth_coll, by.x="CODIGO", by.y = "CODIGO", indicator = )

scale.parameter = 0.35  # scaling paramter. less than 1 is zooming in, more than 1 zooming out. 
xshift = 5.5  # Shift to right in map units. 
original.bbox = Ecuador@bbox  # Pass bbox of your Spatial* Object. 

# Just copy-paste the following
edges <- original.bbox

edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,]) + xshift
my_colors <- c("antiquewhite1","lightgoldenrod1","peru","coral3","coral4")
my_colors_switch <- c("coral4","coral3", "peru", "lightgoldenrod1", "antiquewhite1")

prov_layer <- list("sp.lines", Prov, col = "black",lwd=2, first = FALSE)

x <- spplot(Ecuador, "mean_weight" , sp.layout=prov_layer, col.regions=coolwarm(16),col = "gray51",lwd = .05,xlim = edges[1, ], par.settings = list(axis.line = list(col = 'transparent')),main=list(label="Mean birth weight by parroquia, 2013-2014"))
png(filename = "Output/Figures/Figure_6_weight_map_2013-2014.png")
print(x)
dev.off()

breaks.qt <- c(-.1,.5,.7,.8,.9,1.1)
ltext <- c("0", ".5", ".7", ".8", ".9", "1")
y <- spplot(Ecuador, "mean_visits" ,col.regions = coolwarm(16),sp.layout=prov_layer, col = "gray51",lwd = .05, xlim = edges[1, ], par.settings = list(axis.line = list(col = 'transparent')),main=list(label="Attended at least one antenatal medical visit, 2013-2014"))
png(filename = "Output/Figures/Figure_6_visits_map_2013-2014.png")
print(y)
dev.off()

#breaks.qt <- classIntervals(Ecuador$mean_train, n = 5, style = "quantile", intervalClosure = "right")
z <- spplot(Ecuador, "mean_train" ,col.regions = coolwarm(16),sp.layout=prov_layer, col = "gray51", lwd = .05,xlim = edges[1, ], par.settings = list(axis.line = list(col = 'transparent')),main=list(label="Trained birth care by parroquia, 2013-2014"))
png(filename = "Output/Figures/Figure_6_train_map_2013-2014.png")
print(z)
dev.off()

breaks.qt <- c(-.1,.3,.5,.7,.9,1.1)
ltext <- c("0", ".3", ".5", ".7", ".9", "1")
a <- spplot(Ecuador, "mean_home" ,col.regions = coolwarm(16), sp.layout=prov_layer,col = "gray51",lwd = .05, xlim = edges[1, ], par.settings = list(axis.line = list(col = 'transparent')),main=list(label="Proportion of home births by parroquia, 2013-2014"))
png(filename = "Output/Figures/Figure_6_home_map_2013-2014.png")
print(a)
dev.off()


#### Stunting maps
Ecuador$stunting <- Ecuador$stunting/100
breaks.qt <- c(-.1,.2,.35,.5,.75,1)
ltext <- c("0", ".2", ".35", ".5",".75", "1")
y <- spplot(Ecuador, "stunting" ,col.regions = my_colors, at = breaks.qt,colorkey= list(labels = list(at =breaks.qt, labels = ltext)),sp.layout=prov_layer, col = "gray51",lwd = .05, xlim = edges[1, ], par.settings = list(axis.line = list(col = 'transparent')),main=list(label="Predicted stunting"))
png(filename = "Output/Figures/Figure_1_new_stunting_map.png")
print(y)
dev.off()

my_palette <- c("white","coral4")
Ecuador$trt <- as.factor(Ecuador$Treatment)
ltext <- c("Control", "Treatment")
breaks.qt <- c(-.5,.5)

x <- spplot(Ecuador, "trt",sp.layout=prov_layer, col.regions = my_palette, cuts = 2, col = "transparent",xlim = edges[1, ], par.settings = list(axis.line = list(col = 'black')))

#z <- spplot(Ecuador, "Treatment",col.regions = my_colors,colorkey= F, sp.layout=prov_layer, col = "gray51",lwd = .05, xlim = edges[1, ], par.settings = list(axis.line = list(col = 'transparent')),main=list(label="Treatment parroquias"))
png(filename = "Output/Figures/Figure_1_New_treat_map.png")
print(x)
dev.off()
nokey <- spplot(Ecuador, c("trt"), col.regions=my_palette,
                scales=list(draw = TRUE), colorkey=F)



