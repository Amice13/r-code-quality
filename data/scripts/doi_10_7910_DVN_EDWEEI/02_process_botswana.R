# Bibiane Kan
# bk4462@princeton.edu
# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(sf)
library(tidyverse)

source(here("Code", "19_wrp", "fun", "process_and_plot.R"))

sf.BWA <- st_read(here("Data", "input", "world_admin_boundaries", "botswana", "bwa_admbnda_adm3_2011.shp"))
sf.BWA <- sf.BWA %>% rename(NAME_3 = ADM3_EN, NAME_2 = ADM2_EN)

sf.BWA$REG_ID <- NA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MMATHUBUDUKWANE"] <- 1 # Mmathubudukwane
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOCHUDI"] <- 2 # MOCHUDI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "RAMOTSWA"] <- 3 # RAMOTSWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "TLOKWENG"] <- 4 # TLOKWENG
sf.BWA$REG_ID[sf.BWA$NAME_3 == "GABORONE"] <- 5 # GABORONE
sf.BWA$REG_ID[sf.BWA$NAME_3 == "HUKUNTSI"] <- 6 # HUKUNTSI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "TSABONG"] <- 7 # TSABONG
sf.BWA$REG_ID[sf.BWA$NAME_3 == "KANG"] <- 8 # KANG
sf.BWA$REG_ID[sf.BWA$NAME_3 == "LETLHAKENG"] <- 9 # LETLHAKENG
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOLEPOLOLE"] <- 10 # MOLEPOLOLE
sf.BWA$REG_ID[sf.BWA$NAME_3 == "GOOD HOPE"] <- 11 # GOOD HOPE
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOSHUPA"] <- 12 # MOSHUPA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MABUTSANE"] <- 13 # MABUTSANE
sf.BWA$REG_ID[sf.BWA$NAME_3 == "KANYE"] <- 14 # KANYE
sf.BWA$REG_ID[sf.BWA$NAME_3 == "JWANENG"] <- 15 # JWANENG
sf.BWA$REG_ID[sf.BWA$NAME_3 == "CHARLES HILL"] <- 16 # CHARLES HILL
sf.BWA$REG_ID[sf.BWA$NAME_3 == "GHANZI"] <- 17 # GHANZI

# Ngami: parliamentary constituency within the North-West District,
# it encompasses Sehithwa, Bodibeng, Kareng, and Gumare.
sf.BWA$REG_ID[sf.BWA$NAME_3 %in% c("SEHITHWA", "BODIBENG", "KARENG", "GUMARE")] <- 18
sf.BWA$REG_ID[sf.BWA$NAME_3 %in% c("NGAMILAND DELTA", "OKAVANGO")] <- 19 # NGAMILAND DELTA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "KASANE"] <- 20 # KASANE

# MISSING sf.BWA$REG_ID[sf.BWA$NAME_3 == "BOBIRWA"] <- 21 #
# https://en.wikipedia.org/wiki/Bobirwa
# Central District

sf.BWA$REG_ID[sf.BWA$NAME_3 == "MATHATHANE"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOTLHABANENG"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "TSETSEBJWE"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "KOBOJANGO"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "BOBONONG"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOLALATAU"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "TOBANE"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MABOLWE"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "SEMOLALE"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "LEPOKOLE"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOLETEMANE"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "SEFOPHE"] <- 21 # BOBIRWA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "LENTSWELEMORITI"] <- 21 # BOBIRWA

# MISSING sf.BWA$REG_ID[sf.BWA$NAME_3 == "BOTETI"] <- 22 #
# https://www.trip.com/travel-guide/destination/boteti-1726541/#
# https://osm.hlidskjalf.is/settlements.php?idc=105&region=Central%20District&sub=Central%20Boteti

sf.BWA$REG_ID[sf.BWA$NAME_3 == "KEDIA"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "KHWEE"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "KUMAGA"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "LETLHAKANE"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MMADIKOLA"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MMATSHUMO"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOKOBOXANE"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOPIPI"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOREOMAOTO"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOSU"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOTOPI"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "TOROMOJA"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "XERE"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "XHUMO"] <- 22 # BOTETI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MAHALAPYE"] <- 23 # MAHALAPYE
sf.BWA$REG_ID[sf.BWA$NAME_2 == "SEROWE PALAPYE"] <- 24 # SEROWE PALAPYE
sf.BWA$REG_ID[sf.BWA$NAME_3 == "TUTUME"] <- 25 # TUTUME
sf.BWA$REG_ID[sf.BWA$NAME_3 == "TONOTA"] <- 26 # TONOTA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "LERALA"] <- 27 # LERALA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "SEFHARE"] <- 28 # SEFHARE

sf.BWA$REG_ID[sf.BWA$NAME_3 == "PAJE"] <- 29 # PAJE
# https://en.wikipedia.org/wiki/Paje,_Botswana

sf.BWA$REG_ID[sf.BWA$NAME_3 == "SHOSHONG"] <- 30 # SHOSHONG
sf.BWA$REG_ID[sf.BWA$NAME_3 == "SEBINA"] <- 31 # SEBINA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "NATA"] <- 32 # NATA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "TSIENYANE/RAKOPS"] <- 33 # RAKOPS
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MMADINARE"] <- 34 # MMADINARE
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MMAPHASHALALA"] <- 35 # MMAPHASHALALA
sf.BWA$REG_ID[sf.BWA$NAME_3 == "MOGOROSI"] <- 36 # MOGOROSI
sf.BWA$REG_ID[sf.BWA$NAME_3 == "FRANCISTOWN"] <- 37 # FRANCISTOWN
sf.BWA$REG_ID[sf.BWA$NAME_2 == "NORTH EAST"] <- 38

sf.BWA$GID_0 <- "BWA"
st_geometry(sf.BWA) <- "geom"

process_and_plot(subset(sf.BWA, !is.na(REG_ID)), "BWA")

rm(sf.BWA)
