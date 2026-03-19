#maps of colombian army bases
#april 6, 2018

date()

#load and format data####
library(foreign)
library(car)
library(stringr)
library(stringi)

all_muni <- read.dta("code_muni_cede.dta")
bases_muni <- read.csv("military_bases.csv")
bases_muni <- subset(bases_muni, bases_muni$year <= 2000)

coords <- read.csv("colombia_latlong.csv", stringsAsFactors = FALSE,encoding="UTF-8")
coords <- coords[-1:-4,]
coords <- coords[,-9]
coords <- coords[,-10]
colnames(coords) <- c("dept_code","muni_code","pobl_code","departamento","municipio","poblado",
                      "pobl_type","longitude","latitude", "muni_type","metro_area")
coords$dept_code <- as.numeric(coords$dept_code)
coords$muni_code <- as.numeric(coords$muni_code)
coords$pobl_code <- as.numeric(coords$pobl_code)

coords$keep <- ifelse(coords$pobl_code %% 1000 == 000, 1, 0) #keep only cabecera municipal

coords <- subset(coords, keep == 1)
coords <- coords[,-6:-7]
coords <- coords[,-8:-10]

coords$municipio <- stri_trans_general(coords$municipio,"Latin-ASCII")
#coords$municipio <- iconv(coords$municipio, to="ASCII//TRANSLIT") 
coords$municipio <- str_replace_all(coords$municipio, "\'", "")
coords$municipio <- str_replace_all(coords$municipio, "\\~", "")
coords$municipio <- tolower(coords$municipio)

coords$departamento <- stri_trans_general(coords$departamento,"Latin-ASCII")
#coords$departamento <- iconv(coords$departamento, to="ASCII//TRANSLIT") 
coords$departamento  <- str_replace_all(coords$departamento , "\'", "")
coords$departamento  <- str_replace_all(coords$departamento , "\\~", "")
coords$departamento <- tolower(coords$departamento)

bases_muni$municipio <- tolower(bases_muni$municipio)
bases_muni$departamento <- tolower(bases_muni$departamento)

#correction for formatting
coords$departamento <- ifelse(coords$departamento == "bogota, d. c.","bogota",coords$departamento)
coords$municipio <- ifelse(coords$municipio == "bogota, d.c.","bogota",coords$municipio)
bases_muni$departamento <- ifelse(bases_muni$departamento == "valle","valle del cauca",bases_muni$departamento)
bases_muni$departamento <- ifelse(bases_muni$departamento == "san andres","archipielago de san andres, providencia y ",bases_muni$departamento)
bases_muni$municipio <- ifelse(bases_muni$municipio == "buga","guadalajara de buga",bases_muni$municipio)
bases_muni$municipio <- ifelse(bases_muni$municipio == "puerto inirida","inirida",bases_muni$municipio)
bases_muni$municipio <- ifelse(bases_muni$municipio == "san jose","san jose del guaviare",bases_muni$municipio)
bases_muni$municipio <- ifelse(bases_muni$municipio == "san pedro uraba","san pedro de uraba",bases_muni$municipio)

#merge
bases_muni <- merge(bases_muni, coords, by=c("municipio","departamento"), all.x="TRUE")

bases_muni$muni_code.x <- bases_muni$muni_code.y
colnames(bases_muni)[4] <- "muni_code"
#bases_muni <- na.omit(bases_muni)

#create maps####

army_muni <- subset(bases_muni, bases_muni$force == "Army")
all_muni$army <- ifelse(all_muni$muni_code %in% army_muni$muni_code,1,0)
all_muni$army <- as.factor(all_muni$army)
levels(all_muni$army) <- c("No Base","Base")

library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(plyr)

#load shape file and fortify
colom.shape <- readOGR(dsn=path.expand("mpio.shp"), layer =  "mpio")
colom.map.data <- fortify(colom.shape, region="MPIOS")

colom.shape2 <- colom.shape@data

#format for merging
colnames(colom.map.data)[colnames(colom.map.data) == "id"] <- "muni_code"
colom.map.data$muni_code <- as.integer(colom.map.data$muni_code)

colnames(colom.shape2)[colnames(colom.shape2) == "MPIOS"] <- "muni_code"
colom.shape2$muni_code <- as.integer(as.character(colom.shape2$muni_code))
colom.shape2 <- left_join(colom.shape2, all_muni, by="muni_code")

#fill missing data
colom.shape2$army[is.na(colom.shape2$army)] <- "No Base"

#merge with shapefile
map.data <- merge(colom.map.data, colom.shape2, by = "muni_code", all.x = TRUE)

#remove islands
map.data <- subset(map.data, map.data$NOMBRE_DPT != "ARCHIPIELAGO DE SAN ANDRES PROVIDENCIA Y SANTA CATALINA")

#create

map <- ggplot() +
  geom_polygon(data=map.data, aes(y=lat, x=long, group=group, fill=army),color = "gray50")

map <- map + coord_equal()

map <- map + theme(axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.line.x = element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.background = element_blank())

map <- map + scale_fill_manual(values=c("white","gray30")) + theme(legend.position="none")

ggsave("figurea2-a.png", plot = map, width = 10, height = 9)


sessionInfo()
date()
