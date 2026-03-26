install.packages(c("readxl","dplyr","haven","sf","geodata", "ggplot2", "classInt", "ggpubr"), dependencies = TRUE)

#Set the working directory as "Replication_AJPS/data"
setwd("...")

library(readxl)
library(dplyr)
library(haven)
library(sf)
library(geodata)
library(ggplot2)
library(classInt)
library(ggpubr)

## MALI ##
#Cercles shapefile
mali2<- gadm(country="MLI", level=2, path=tempdir())
mali2<- st_as_sf(mali2)
plot(st_geometry(mali2))

#AfB r2
r2 <- read.csv("raw/afrobarometer/afb_full_r2.csv")
print(unique(r2$country))
r2<-subset(r2, country==5)
r2$round<-2
r2$countryname<-"Mali"
names(r2)
r2<-r2 %>% dplyr::select(round, respno,latitude, longitude,dateintr,countryname)
print(unique(r2$dateintr))

#AfB r3
r3 <- read.csv("raw/afrobarometer/afb_full_r3.csv")
print(unique(r3$country))
r3<-subset(r3, country==9)
r3$round<-3
r3$countryname<-"Mali"
names(r3)
r3<-r3 %>% dplyr::select(round, respno,latitude, longitude,dateintr,countryname)
print(unique(r3$dateintr))

#AfB r4
r4 <- read.csv("raw/afrobarometer/afb_full_r4.csv")
print(unique(r4$country))
r4<-subset(r4, country==11)
r4$countryname<-"NA"
r4$countryname[which(r4$country==11)]<-"Mali"
r4$round<-4
names(r4)
r4<-r4 %>% dplyr::select(round, respno,latitude, longitude,dateintr,countryname)

#AfB r5
r5 <- read.csv("raw/afrobarometer/afb_full_r5.csv")
r5<-subset(r5, country==11)
r5$round<-5
r5$countryname<-"NA"
r5$countryname[which(r5$country==11)]<-"Mali"
r5<-r5 %>% dplyr::select(round, respno,latitude, longitude,dateintr,countryname)

#AfB r6
r6 <- read.csv("raw/afrobarometer/afb_full_r6.csv")
r6<-subset(r6, country==18)
r6$round<-6
r6$countryname<-"NA"
r6$countryname[which(r6$country==18)]<-"Mali"
r6<-r6 %>% dplyr::select(round, respno,latitude, longitude,dateintr,countryname)

#AfB r7
r7 <- read.csv("raw/afrobarometer/afb_full_r7.csv", sep=";")
names(r7)<- tolower(names(r7))
r7$round<-7
r7<-subset(r7, country=="Mali") 
r7$countryname[which(r7$country=="Mali")]<-"Mali"
r7$latitude<-r7$ea_gps_la
r7$longitude<-r7$ea_gps_lo
r7<-r7 %>% dplyr::select(round, respno,latitude, longitude,dateintr,countryname)

all_afb <- rbind(r2, r3, r4, r5, r6, r7)

#Assign AfB to cercles
all_afb <- st_as_sf(all_afb, coords=c("longitude", "latitude")) 
plot(st_geometry(mali2), col="grey")
plot(st_geometry(all_afb),  pch=".", add=T)
mali2<-mali2 %>% st_set_crs(st_crs(all_afb))
all_afb_geo <- st_join(all_afb, left = TRUE, mali2)
write.csv(all_afb_geo,'raw/afb_todistricts.csv')

#PKO to cercles
pko_gis <- read.csv("raw/Geo_PKO_v.2_corrected.csv", sep=",")
pko_mli<-subset(pko_gis, mission=="MINUSMA")
pko_mli<-subset(pko_gis, country=="Mali")
pko_mli$lat<-pko_mli$latitude
pko_mli$longit<-pko_mli$longitude
pko_mli <- st_as_sf(pko_mli, coords=c("longitude", "latitude")) 
pko_mli <- pko_mli %>% st_set_crs(st_crs(all_afb))
plot(st_geometry(mali2))
plot(st_geometry(pko_mli), add=T)
pko_mli <- st_join(pko_mli, left = FALSE, mali2)

write.csv(pko_mli,'raw/pko_todistricts.csv')

#Link conflict data to units
acled <- read.csv("raw/acled_africa.csv")
acled<-subset(acled, country=="Mali")
acled <- st_as_sf(acled, coords=c("longitude", "latitude")) 
acled <- acled %>% st_set_crs(st_crs(all_afb))
plot(st_geometry(mali2))
plot(st_geometry(acled), add=T)

acled <- st_join(acled, left = FALSE, mali2)
write.csv(acled,'raw/acled_todistricts.csv')

####MAPS####
afb_mli<-all_afb_geo
pko_mli<-subset(pko_mli, country=="Mali")
mli<-mali2

#Figure 3
pko_mli$nopolice<-pko_mli$fpu.no + pko_mli$mp
avg_nopolice <- pko_mli %>% 
  group_by(GID_2) %>% 
  dplyr::summarise(avg_nopolice=mean(nopolice, na.rm=TRUE)) %>% ## use sum for total
  ungroup()
mli <- st_join(mli, avg_nopolice["avg_nopolice"], left = TRUE)

pk_dummy <- pko_mli %>% 
  group_by(GID_2) %>% 
  dplyr::summarise(pk_dummy=n()) %>% 
  ungroup()
mli_unp <- st_join(mli, pk_dummy["pk_dummy"], left = FALSE)

totresp <- afb_mli %>% 
  group_by(GID_2) %>% 
  dplyr::summarise(totresp=n()) %>% 
  ungroup()
mli <- st_join(mli, totresp["totresp"], left = TRUE)

mli$sh_resp<-mli$totresp/10648
quant <- classIntervals(mli$sh_resp, n=4, style="quantile")
sh_resp_q <- cut(mli$sh_resp, breaks = c(quant$brks), dig.lab = 4)

pdf("Figure3.pdf",width=10,height=6)
figure3<-ggplot() +
  geom_sf(data = mli, fill="white", color="black") +
  geom_sf(data = mli, aes(fill=sh_resp_q), color="black") +
  geom_sf(data = pko_mli, aes(color ="black"), size=2) +
  scale_fill_brewer("Share Afb respondents (quartiles)", palette = "BuPu", direction=1, na.value = "gray", labels = c('0.001-0.010', '0.010-0.014', '0.014-0.23', '0.023-0.118')) +
  scale_color_identity(name="", labels = c(black = "MINUSMA Base"), guide = "legend") +
  theme_void() +
  theme(legend.position = c(1, 0.17),
        legend.title=element_text(size=13), 
        legend.text=element_text(size=11))
figure3
dev.off()

#Figure5
pdf("Figure5.pdf",width=10,height=6)
figure5<-ggplot() +
  geom_sf(data = mli, fill="white", color="darkgrey") +
  geom_sf(data = subset(pko_mli, year==2014), aes(color ="firebrick4"), shape=1, size=4.5) +
  geom_sf(data = subset(pko_mli, year==2017), aes(color ="firebrick2"), shape=4, size=4) +
  scale_color_identity(name="", labels = c(firebrick4 = "MINUSMA 2014", firebrick2 = "MINUSMA 2017"), guide = "legend") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title=element_text(size=13), 
        legend.text=element_text(size=11))
figure5
dev.off()

#Figure A1
mli_road<-st_read("raw/mali_roads_WorldBank/Mali_Roads.shp")
pdf("FigureA1.pdf",width=10,height=6)
figure_a1<-ggplot() +
  geom_sf(data = mli, fill="white", color="gray") +
  geom_sf(data = afb_mli, aes(color ="coral"), size=0.8, shape=1, alpha=0.2) +
  geom_sf(data = mli_road, aes(color="black"), linewidth=0.2) + 
  scale_color_identity(name="", labels = c(black = "Main road network", coral="AfB respondents"), guide = "legend") +
  theme_void() +
  theme(legend.position = c(0.15, 0.8), 
        legend.title=element_text(size=13), 
        legend.text=element_text(size=11))+
  guides(colour = guide_legend(override.aes = list(alpha=1)))
figure_a1
dev.off()

#Figure 1
policestation<-read_dta("policestation.dta")
policestation <- st_as_sf(policestation, coords = c("latitude", "longitude"))
policestation<-policestation %>% st_set_crs(st_crs(mli))
plot(st_geometry(mli))
plot(st_geometry(policestation), add=T)

sampled2 <- subset(policestation, round==2) %>% 
  group_by(gid_2) %>% 
  dplyr::summarise(sampled=n()) %>% 
  dplyr::rename(sampled2=sampled) %>% 
  ungroup()
sampled3 <- subset(policestation, round==3) %>% 
  group_by(gid_2) %>% 
  dplyr::summarise(sampled=n()) %>% 
  dplyr::rename(sampled3=sampled) %>% 
  ungroup()
sampled4 <- subset(policestation, round==4) %>% 
  group_by(gid_2) %>% 
  dplyr::summarise(sampled=n()) %>% 
  dplyr::rename(sampled4=sampled) %>% 
  ungroup()
sampled5 <- subset(policestation, round==5) %>% 
  group_by(gid_2) %>% 
  dplyr::summarise(sampled=n()) %>%
  dplyr::rename(sampled5=sampled) %>% 
  ungroup()
sampled6 <- subset(policestation, round==6) %>% 
  group_by(gid_2) %>% 
  dplyr::summarise(sampled=n()) %>% 
  dplyr::rename(sampled6=sampled) %>% 
  ungroup()
sampled7 <- subset(policestation, round==7) %>% 
  group_by(gid_2) %>% 
  dplyr::summarise(sampled=n()) %>% 
  dplyr::rename(sampled7=sampled) %>% 
  ungroup()

mali_policest <- st_join(mli, sampled2["sampled2"], left = TRUE)
mali_policest <- st_join(mali_policest, sampled3["sampled3"], left = TRUE)
mali_policest <- st_join(mali_policest, sampled4["sampled4"], left = TRUE)
mali_policest <- st_join(mali_policest, sampled5["sampled5"], left = TRUE)
mali_policest <- st_join(mali_policest, sampled6["sampled6"], left = TRUE)
mali_policest <- st_join(mali_policest, sampled7["sampled7"], left = TRUE)

police2<-ggplot() +
  geom_sf(data = mli_unp, aes(fill="slateblue"), color="transparent", alpha=0.4) +
  geom_sf(data = mli, fill="white", color="lightgrey") +
  geom_sf(data = subset(mali_policest, sampled2>0), aes(fill="white"), color="black") +
  geom_sf(data = subset(policestation, round==2 & policestation==1), aes(color ="black"), size=1, shape=1) +
  scale_fill_identity(name="", labels = c(slateblue = "MINUSMA", white="Afb sampled"), guide = "legend") +
  scale_color_identity(name="", labels = c(black = "Malian police stations"), guide = "legend") +
  theme_void() +
  theme( plot.title = element_text(size = 11),
         plot.margin=unit(c(0, 0, 1, 0),"cm"),
         legend.text=element_text(size=11)) +
  labs(x = "", y = "",
       title = "a) Police stations, round 2")

police3<-ggplot() +
  geom_sf(data = mli_unp, aes(fill="slateblue"), color="transparent", alpha=0.4) +
  geom_sf(data = mli, fill="white", color="lightgrey") +
  geom_sf(data = subset(mali_policest, sampled3>0), aes(fill="white"), color="black") +
  geom_sf(data = subset(policestation, round==3 & policestation==1), aes(color ="black"), size=1, shape=1) +
  scale_fill_identity(name="", labels = c(slateblue = "MINUSMA", white="Afb sampled"), guide = "legend") +
  scale_color_identity(name="", labels = c(black = "Malian police stations"), guide = "legend") +
  theme_void() +
  theme( plot.title = element_text(size = 11),
         plot.margin=unit(c(0, 0, 1, 0),"cm"),
         legend.text=element_text(size=11)) +
  labs(x = "", y = "",
       title = "b) Police stations, round 3")

police4<-ggplot() +
  geom_sf(data = mli_unp, aes(fill="slateblue"), color="transparent", alpha=0.4) +
  geom_sf(data = mli, fill="white", color="lightgrey") +
  geom_sf(data = subset(mali_policest, sampled4>0), aes(fill="white"), color="black") +
  geom_sf(data = subset(policestation, round==4 & policestation==1), aes(color ="black"), size=1, shape=1) +
  scale_fill_identity(name="", labels = c(slateblue = "MINUSMA", white="Afb sampled"), guide = "legend") +
  scale_color_identity(name="", labels = c(black = "Malian police stations"), guide = "legend") +
  theme_void() + 
  theme( plot.title = element_text(size = 11),
         plot.margin=unit(c(0, 0, 1, 0),"cm"),
         legend.text=element_text(size=11))  +
  labs(x = "", y = "",
       title = "c) Police stations, round 4")

police5<-ggplot() +
  geom_sf(data = mli_unp, aes(fill="slateblue"), color="transparent", alpha=0.4) +
  geom_sf(data = mli, fill="white", color="lightgrey") +
  geom_sf(data = subset(mali_policest, sampled5>0), aes(fill="white"), color="black") +
  geom_sf(data = subset(policestation, round==5 & policestation==1), aes(color ="black"), size=1, shape=1) +
  scale_fill_identity(name="", labels = c(slateblue = "MINUSMA", white="Afb sampled"), guide = "legend") +
  scale_color_identity(name="", labels = c(black = "Malian police stations"), guide = "legend") +
  theme_void() +
  theme( plot.title = element_text(size = 11),
         plot.margin=unit(c(0, 0, 1, 0),"cm"),
         legend.text=element_text(size=11))  +
  labs(x = "", y = "",
       title = "d) Police stations, round 5")

police6<-ggplot() +
  geom_sf(data = mli, fill="white", color="lightgrey") +
  geom_sf(data = subset(mali_policest, sampled6>0), aes(fill="white"), color="black") +
  geom_sf(data = subset(policestation, round==6 & policestation==1), aes(color ="black"), size=1, shape=1) +
  geom_sf(data = mli_unp, aes(fill="slateblue"), color="transparent", alpha=0.5) +
  scale_fill_identity(name="", labels = c(slateblue = "MINUSMA", white="Afb sampled"), guide = "legend") +
  scale_color_identity(name="", labels = c(black = "Malian police stations"), guide = "legend") +
  theme_void() +
  theme( plot.title = element_text(size = 11),
         plot.margin=unit(c(0, 0, 1, 0),"cm"),
         legend.text=element_text(size=11))  +
  labs(x = "", y = "",
       title = "e) Police stations, round 6")

police7<-ggplot() +
  geom_sf(data = mli, fill="white", color="lightgrey") +
  geom_sf(data = subset(mali_policest, sampled7>0), aes(fill="white"), color="black") +
  geom_sf(data = subset(policestation, round==7 & policestation==1), aes(color ="black"), size=1, shape=1) +
  geom_sf(data = mli_unp, aes(fill="slateblue"), color="transparent", alpha=0.5) +
  scale_fill_identity(name="", labels = c(slateblue = "MINUSMA", white="Afb sampled"), guide = "legend") +
  scale_color_identity(name="", labels = c(black = "Malian police stations"), guide = "legend") +
  theme_void() +
  theme( plot.title = element_text(size = 11),
         plot.margin=unit(c(0, 0, 1, 0),"cm"),
         legend.text=element_text(size=11))  +
  labs(x = "", y = "",
       title = "f) Police stations, round 7")

pdf("Figure1.pdf",width=10,height=10)
figure1<-ggarrange(police2, police3, police4,
                   police5, 
                   police6,
                   police7, ncol = 2, nrow=3, common.legend=TRUE,legend="bottom")
figure1
dev.off()
