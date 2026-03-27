rm(list = ls()) ##Clean up workspace
setwd("") ## SET YOUR DIRECTORY HERE
library(dplyr)
library(foreign)

RAS <- read.spss("Data/Raw/01 Recursos y Actividades de Salud 2012.sav")
RAS <- data.frame(RAS)

Pub_fac <- filter(RAS, Sector == "Sector Público")

library (sp)
library (rgdal)
library(rgeos)
library(classInt)
Ecua <- readOGR(dsn = "Data/Geographic/ecu_adm_inec_20190724_shp",layer = "ecu_admbnda_adm3_inec_20190724")
Prov <- readOGR(dsn ="Data/Geographic/ecu_adm_inec_20190724_shp", layer = "ecu_admbnda_adm1_inec_20190724")

df <- data.frame(Ecua)
df <- select(df, c(ADM1_ES,ADM1_PCODE))
df <- dplyr::distinct(df)
df$ADM1_PCODE <- substr(df$ADM1_PCODE,3,nchar(df$ADM1_PCODE))

Pub_fac$Prov_ubi <- stri_trans_general(Pub_fac$Prov_ubi, "Latin-ASCII")
df$ADM1_ES <- stri_trans_general(df$ADM1_ES , "Latin-ASCII")

Pub_fac <- merge(Pub_fac,df,by.x = "Prov_ubi", by.y = "ADM1_ES", all.x = TRUE)
Pub_fac <- filter(Pub_fac, !is.na(ADM1_PCODE))

Pub_fac$CODIGO <- paste(Pub_fac$ADM1_PCODE, Pub_fac$Cant_ubie2, Pub_fac$Parr_ubie2, sep = "")

Ecua$CODIGO <- gsub("EC", "", Ecua$ADM3_PCODE)

Pub_coll <- Pub_fac %>%
  group_by(CODIGO) %>%
  dplyr::summarise(Pub_health_fac = n())

Ecuador <- merge(Ecua, Pub_coll, by="CODIGO", all.x = TRUE)
Ecuador$Pub_health_fac  <- ifelse(is.na(Ecuador$Pub_health_fac),0,Ecuador$Pub_health_fac)

Ecuador$Fac_per_area <- Ecuador$Pub_health_fac/Ecuador$Shape_Area
merger <- data.frame(Ecuador)
write.csv(merger, "Data/Clean/Pub_fac_geo_var.csv", row.names = FALSE)

