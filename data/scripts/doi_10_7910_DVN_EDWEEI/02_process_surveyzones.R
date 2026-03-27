# ===========================================================
# Survey Zone Region ID Mapping - Codebook & Implementation
# ===========================================================
# 
# This file maps World Risk Poll survey region IDs to administrative
# boundaries for 137 countries. Each country block documents coding
# decisions for transparency and reproducibility.
#
# NAVIGATION: Use RStudio's document outline (Ctrl/Cmd + Shift + O)
#             to jump between country sections.
#
# METHODOLOGY: Each country follows the pattern:
#   1. Subset geographic data by ISO country code
#   2. Map survey REG_ID to administrative region names
#   3. Validate with process_and_plot()
#   4. Clean up temporary objects
#
# Alexander Gazmararian: agazmararian@gmail.com
# Bibiane Kan: bk4462@princeton.edu

library(countrycode)
library(data.table)
library(here)
library(readxl)
library(sf)
library(terra)
library(tidyverse)

g <- readRDS(here("Data", "inter", "19_wrp", "regions4coding.rds"))

source(here("Code", "19_wrp", "fun", "process_and_plot.R"))

## Albania----
sf_alb <- subset(g, GID_0 == "ALB")
g <- subset(g, GID_0 != "ALB")
sf_alb$REG_ID[sf_alb$NAME_2 == "Beratit"] <- 1
sf_alb$REG_ID[sf_alb$NAME_2 == "Bulqizës"] <- 2
sf_alb$REG_ID[sf_alb$NAME_2 == "Delvinës"] <- 3
sf_alb$REG_ID[sf_alb$NAME_2 == "Devollit"] <- 4
sf_alb$REG_ID[sf_alb$NAME_2 == "Dibrës"] <- 5
sf_alb$REG_ID[sf_alb$NAME_2 == "Durrësit"] <- 6
sf_alb$REG_ID[sf_alb$NAME_2 == "Elbasanit"] <- 7
sf_alb$REG_ID[sf_alb$NAME_2 == "Fierit"] <- 8
sf_alb$REG_ID[sf_alb$NAME_2 == "Gjirokastrës"] <- 9
sf_alb$REG_ID[sf_alb$NAME_2 == "Gramshit"] <- 10
sf_alb$REG_ID[sf_alb$NAME_2 == "Has"] <- 11
sf_alb$REG_ID[sf_alb$NAME_2 == "Kavajës"] <- 12
sf_alb$REG_ID[sf_alb$NAME_2 == "Kolonjës"] <- 13
sf_alb$REG_ID[sf_alb$NAME_2 == "Korçës"] <- 14
sf_alb$REG_ID[sf_alb$NAME_2 == "Krujës"] <- 15
sf_alb$REG_ID[sf_alb$NAME_2 == "Kuçovës"] <- 16
sf_alb$REG_ID[sf_alb$NAME_2 == "Kukësit"] <- 17
sf_alb$REG_ID[sf_alb$NAME_2 == "Kurbinit"] <- 18
sf_alb$REG_ID[sf_alb$NAME_2 == "Lezhës"] <- 19
sf_alb$REG_ID[sf_alb$NAME_2 == "Librazhdit"] <- 20
sf_alb$REG_ID[sf_alb$NAME_2 == "Lushnjës"] <- 21
sf_alb$REG_ID[sf_alb$NAME_2 == "Malësi e Madhe"] <- 22
sf_alb$REG_ID[sf_alb$NAME_2 == "Mallakastrës"] <- 23
sf_alb$REG_ID[sf_alb$NAME_2 == "Matit"] <- 24
sf_alb$REG_ID[sf_alb$NAME_2 == "Mirditës"] <- 25
sf_alb$REG_ID[sf_alb$NAME_2 == "Peqinit"] <- 26
sf_alb$REG_ID[sf_alb$NAME_2 == "Përmetit"] <- 27
sf_alb$REG_ID[sf_alb$NAME_2 == "Pogradecit"] <- 28
sf_alb$REG_ID[sf_alb$NAME_2 == "Pukës"] <- 29
sf_alb$REG_ID[sf_alb$NAME_2 == "Sarandës"] <- 30
sf_alb$REG_ID[sf_alb$NAME_2 == "Shkodrës"] <- 31
sf_alb$REG_ID[sf_alb$NAME_2 == "Skraparit"] <- 32
sf_alb$REG_ID[sf_alb$NAME_2 == "Tepelenës"] <- 33
sf_alb$REG_ID[sf_alb$NAME_2 == "Tiranës"] <- 34
sf_alb$REG_ID[sf_alb$NAME_2 == "Tropojës"] <- 35
sf_alb$REG_ID[sf_alb$NAME_2 == "Vlorës"] <- 36
process_and_plot(sf_alb, "alb")
rm(sf_alb)


## United Arab Emirates----
sf.ARE <- subset(g, GID_0 == "ARE")
g <- subset(g, GID_0 != "ARE")
sf.ARE$REG_ID[sf.ARE$NAME_1 == "Abu Dhabi"] <- 1
sf.ARE$REG_ID[sf.ARE$NAME_1 == "Dubai"] <- 3
sf.ARE$REG_ID[sf.ARE$NAME_1 == "Sharjah"] <- 4
sf.ARE$REG_ID[sf.ARE$NAME_1 == "Ajman"] <- 5
sf.ARE$REG_ID[sf.ARE$NAME_1 == "Umm al-Qaywayn"] <- 6
sf.ARE$REG_ID[sf.ARE$NAME_1 == "Ras Al-Khaimah"] <- 7
sf.ARE$REG_ID[sf.ARE$NAME_1 == "Fujairah"] <- 8
process_and_plot(sf.ARE, "are")
rm(sf.ARE)

## Argentina----
sf.ARG <- subset(g, GID_0 == "ARG")
g <- subset(g, GID_0 != "ARG")
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Ciudad de Buenos Aires"] <- 1
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Buenos Aires"] <- 2
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Catamarca"] <- 3
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Chaco"] <- 4
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Chubut"] <- 5
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Córdoba"] <- 6
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Corrientes"] <- 7
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Entre Ríos"] <- 8
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Formosa"] <- 9
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Jujuy"] <- 10
sf.ARG$REG_ID[sf.ARG$NAME_1 == "La Pampa"] <- 11
sf.ARG$REG_ID[sf.ARG$NAME_1 == "La Rioja"] <- 12
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Mendoza"] <- 13
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Misiones"] <- 14
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Neuquén"] <- 15
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Río Negro"] <- 16
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Salta"] <- 17
sf.ARG$REG_ID[sf.ARG$NAME_1 == "San Juan"] <- 18
sf.ARG$REG_ID[sf.ARG$NAME_1 == "San Luis"] <- 19
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Santa Cruz"] <- 20
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Santa Fe"] <- 21
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Santiago del Estero"] <- 22
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Tierra del Fuego"] <- 23
sf.ARG$REG_ID[sf.ARG$NAME_1 == "Tucumán"] <- 24
process_and_plot(sf.ARG, "arg")
rm(sf.ARG)

## Armenia----
sf.ARM <- subset(g, GID_0 == "ARM")
g <- subset(g, GID_0 != "ARM")
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Erevan"] <- 1
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Aragatsotn"] <- 2
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Ararat"] <- 3
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Armavir"] <- 4
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Gegharkunik"] <- 5
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Lori"] <- 6
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Kotayk"] <- 7
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Shirak"] <- 8
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Syunik"] <- 9
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Vayots Dzor"] <- 10
sf.ARM$REG_ID[sf.ARM$NAME_1 == "Tavush"] <- 11
process_and_plot(sf.ARM, "arm")
rm(sf.ARM)

## Australia----
sf.AUS <- subset(g, GID_0 == "AUS")
g <- subset(g, GID_0 != "AUS")
table(sf.AUS$NAME_2)
sf.AUS$REG_ID[sf.AUS$NAME_2 == "Sydney"] <- 1 # Sydney
nsw_excl_sydney <- subset(sf.AUS, NAME_1 == "New South Wales" & NAME_2 != "Sydney")
sf.AUS$REG_ID[sf.AUS$NAME_2 %in% nsw_excl_sydney$NAME_2] <- 2
sf.AUS$REG_ID[sf.AUS$NAME_2 == "Melbourne"] <- 3 # Melbourne
vic_excl_melbourne <- subset(sf.AUS, NAME_1 == "Victoria" & NAME_2 != "Melbourne")
sf.AUS$REG_ID[sf.AUS$NAME_2 %in% vic_excl_melbourne$NAME_2] <- 4
sf.AUS$REG_ID[sf.AUS$NAME_2 == "Brisbane"] <- 5 # Brisbane
qld_excl_bris <- subset(sf.AUS, NAME_1 == "Queensland" & NAME_2 != "Brisbane")
sf.AUS$REG_ID[sf.AUS$NAME_2 %in% qld_excl_bris$NAME_2] <- 6
sf.AUS$REG_ID[sf.AUS$NAME_2 == "Adelaide"] <- 7 # Adelaide
sa_excl_adelaide <- subset(sf.AUS, NAME_1 == "South Australia" & NAME_2 != "Adelaide")
sf.AUS$REG_ID[sf.AUS$NAME_2 %in% sa_excl_adelaide$NAME_2] <- 8
sf.AUS$REG_ID[sf.AUS$NAME_2 == "Perth"] <- 9 # Perth
wa_excl_perth <- subset(sf.AUS, NAME_1 == "Western Australia" & NAME_2 != "Perth")
sf.AUS$REG_ID[sf.AUS$NAME_2 %in% wa_excl_perth$NAME_2] <- 10
sf.AUS$REG_ID[sf.AUS$NAME_2 == "Hobart"] <- 11 # Hobart
tas_exl_hobart <- subset(sf.AUS, NAME_1 == "Tasmania" & NAME_2 != "Hobart")
sf.AUS$REG_ID[sf.AUS$NAME_2 %in% tas_exl_hobart$NAME_2] <- 12
sf.AUS$REG_ID[sf.AUS$NAME_1 == "Australian Capital Territory"] <- 13 # Australian Capital Territory (ACT)
sf.AUS$REG_ID[sf.AUS$NAME_2 == "Darwin"] <- 14 # Darwin
nt_excl_darwin <- subset(sf.AUS, NAME_1 == "Northern Territory" & NAME_2 != "Darwin")
sf.AUS$REG_ID[sf.AUS$NAME_2 %in% nt_excl_darwin$NAME_2] <- 15
sf.AUS <- subset(sf.AUS, !is.na(REG_ID))
process_and_plot(sf.AUS, "aus")
rm(sf.AUS)

## Austria----
sf.AUT <- subset(g, GID_0 == "AUT")
g <- subset(g, GID_0 != "AUT")
sf.AUT$REG_ID[sf.AUT$NAME_1 == "Burgenland"] <- 18011 # Burgenland
sf.AUT$REG_ID[sf.AUT$NAME_1 == "Niederösterreich"] <- 18012 # Lower Austria (Niederosterreich)
sf.AUT$REG_ID[sf.AUT$NAME_1 == "Wien"] <- 18013 # Vienna (Wien)
sf.AUT$REG_ID[sf.AUT$NAME_1 == "Kärnten"] <- 18021 # Carinthia (Karnten)
sf.AUT$REG_ID[sf.AUT$NAME_1 == "Steiermark"] <- 18022 # Styria (Steiermark)
sf.AUT$REG_ID[sf.AUT$NAME_1 == "Oberösterreich"] <- 18031 # Upper Austria (Oberosterreich)
sf.AUT$REG_ID[sf.AUT$NAME_1 == "Salzburg"] <- 18032 # Salzburg
sf.AUT$REG_ID[sf.AUT$NAME_1 == "Tirol"] <- 18033 # Tyrol (Tirol)
sf.AUT$REG_ID[sf.AUT$NAME_1 == "Vorarlberg"] <- 18034 # Vorarlberg
process_and_plot(sf.AUT, "aut")
rm(sf.AUT)

## Azerbaijan----
sf.AZE <- subset(g, GID_0 == "AZE")
g <- subset(g, GID_0 != "AZE")
sf.AZE$REG_ID[sf.AZE$NAME_2 == "Bakı"] <- 1 # Baku
sf.AZE$REG_ID[sf.AZE$NAME_1 == "Absheron" & sf.AZE$NAME_2 != "Bakı"] <- 2 # Absheron
sf.AZE$REG_ID[sf.AZE$NAME_1 == "Aran"] <- 3 # Aran
sf.AZE$REG_ID[sf.AZE$NAME_1 == "Ganja-Qazakh"] <- 4 # Gandja-Gazakh
sf.AZE$REG_ID[sf.AZE$NAME_1 == "Lankaran"] <- 5 # Lankaran
sf.AZE$REG_ID[sf.AZE$NAME_1 == "Daglig-Shirvan"] <- 6 # Mountainous Shirvan
sf.AZE$REG_ID[sf.AZE$NAME_1 == "Shaki-Zaqatala"] <- 7 # Sheki-Zagatala
sf.AZE$REG_ID[sf.AZE$NAME_1 == "Yukhari-Karabakh"] <- 8 # Yukhari-Karabakh
sf.AZE$REG_ID[sf.AZE$NAME_1 == "Kalbajar-Lachin"] <- 9 # Kalbajar-Lachin
process_and_plot(sf.AZE, "aze")
rm(sf.AZE)

## Belgium----
sf.BEL <- subset(g, GID_0 == "BEL")
g <- subset(g, GID_0 != "BEL")
sf.BEL$REG_ID[sf.BEL$NAME_2 == "Bruxelles"] <- 10 # Region de Bruxelles-Capitale / Brussels Hoofdstedelijk Gewest
sf.BEL$REG_ID[sf.BEL$NAME_2 == "Antwerpen"] <- 21 # Prov. Antwerpen
sf.BEL$REG_ID[sf.BEL$NAME_2 == "Limburg"] <- 22 # Prov. Limburg (B)
sf.BEL$REG_ID[sf.BEL$NAME_2 == "Oost-Vlaanderen"] <- 23 # Prov. Oost-Vlaanderen
sf.BEL$REG_ID[sf.BEL$NAME_2 == "Vlaams Brabant"] <- 24 # Prov. Vlaams-Brabant
sf.BEL$REG_ID[sf.BEL$NAME_2 == "West-Vlaanderen"] <- 25 # Prov. West-Vlaanderen
sf.BEL$REG_ID[sf.BEL$NAME_2 == "Brabant Wallon"] <- 31 # Prov. Brabant Wallon
sf.BEL$REG_ID[sf.BEL$NAME_2 == "Hainaut"] <- 32 # Prov. Hainaut
sf.BEL$REG_ID[sf.BEL$NAME_2 == "Liège"] <- 33 # Prov. Liege
sf.BEL$REG_ID[sf.BEL$NAME_2 == "Luxembourg"] <- 34 # Prov. Luxembourg (B)
sf.BEL$REG_ID[sf.BEL$NAME_2 == "Namur"] <- 35 # Prov. Namur
process_and_plot(sf.BEL, "bel")
rm(sf.BEL)

## Benin----
sf.BEN <- subset(g, GID_0 == "BEN")
g <- subset(g, GID_0 != "BEN")
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Banikoara"] <- 1 # Banikoara
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Gogounou"] <- 2 # Gogounou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Kandi"] <- 3 # Kandi
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Karimama"] <- 4 # Karimama
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Malanville"] <- 5 # Malanville
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Ségbana"] <- 6 # Segbana
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Boukoumbé"] <- 7 # Boukoumbe
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Cobly"] <- 8 # Cobly
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Kérou"] <- 9 # Kerou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Kouandé"] <- 10 # Kouande
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Matéri"] <- 11 # Materi
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Natitingou"] <- 12 # Natitingou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Péhunco"] <- 13 # Pehunko
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Tanguiéta"] <- 14 # Tanguieta
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Toucountouna"] <- 15 # Toucountouna
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Abomey-Calavi"] <- 16 # Abomey-Calavi
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Allada"] <- 17 # Allada
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Kpomassè"] <- 18 # Kpomasse
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Ouidah"] <- 19 # Ouidah
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Sô-Ava"] <- 20 # So-Ava
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Toffo"] <- 21 # Toffo
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Tori-Bossito"] <- 22 # Tori-Bossito
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Zè"] <- 23 # Ze
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Bembèrèkè"] <- 24 # Bembereke
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Kalalé"] <- 25 # Kalale
sf.BEN$REG_ID[sf.BEN$NAME_2 == "N'Dali"] <- 26 # N Dali
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Nikki"] <- 27 # Nikki
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Parakou"] <- 28 # Parakou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Pèrèrè"] <- 29 # Perere
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Sinendé"] <- 30 # Sinende
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Tchaourou"] <- 31 # Tchaourou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Bantè"] <- 32 # Bante
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Dassa-Zoumè"] <- 33 # Dassa-Zoume
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Glazoué"] <- 34 # Glazoue
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Ouèssè"] <- 35 # Ouesse
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Savalou"] <- 36 # Savalou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Savè"] <- 37 # Save
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Bassila"] <- 38 # Bassila
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Copargo"] <- 39 # Copargo
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Djougou Rural"] <- 40 # Djougou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Djougou Urbain"] <- 40 # Djougou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Ouaké"] <- 41 # Ouake
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Aplahoué"] <- 42 # Aplahoue
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Djakotomey"] <- 43 # Djakotomey
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Klouékanmè"] <- 44 # Klouekanme
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Lalo"] <- 45 # Lalo
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Toviklin"] <- 46 # Toviklin
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Cotonou"] <- 47 # Cotonou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Athiémè"] <- 48 # Athieme
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Bopa"] <- 49 # Bopa
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Comè"] <- 50 # Come
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Grand-Popo"] <- 51 # Grand-Popo
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Houéyogbé"] <- 52 # Houeyogbe
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Lokossa"] <- 53 # Lokossa
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Adjarra"] <- 54 # Adjarra
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Adjohoun"] <- 55 # Adjohoun
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Aguégués"] <- 56 # Aguegues
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Akpro-Missérété"] <- 57 # Akpro-Misserete
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Avrankou"] <- 58 # Avrankou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Bonou"] <- 59 # Bonou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Dangbo"] <- 60 # Dangbo
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Porto-Novo"] <- 61 # Porto Novo
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Sèmè-Kpodji"] <- 62 # Seme-Kpodji
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Ifangni"] <- 63 # Ifangni
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Adja-Ouèrè"] <- 64 # Adja-Ouere
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Kétou"] <- 65 # Ketou
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Pobè"] <- 66 # Pobe
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Sakété"] <- 67 # Sakete
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Abomey"] <- 68 # Abomey
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Agbangnizoun"] <- 69 # Agbangnizoun
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Bohicon"] <- 70 # Bohicon
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Covè"] <- 71 # Cove
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Djidja"] <- 72 # Djidja
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Ouinhi"] <- 73 # Ouinhi
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Za-Kpota"] <- 74 # Za-Kpota
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Zagnanado"] <- 75 # Zangnanado
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Zogbodomey"] <- 76 # Zogbodomey
sf.BEN$REG_ID[sf.BEN$NAME_2 == "Dogbo"] <- 77 # Dogbo
process_and_plot(sf.BEN, "ben")
rm(sf.BEN)

## Burkina Faso----
sf.BFA <- subset(g, GID_0 == "BFA")
g <- subset(g, GID_0 != "BFA")
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Balé"] <- 1 # Bale
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Bam"] <- 2 # Bam
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Banwa"] <- 3 # Banwa
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Bazèga"] <- 4 # Bazega
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Bougouriba"] <- 5 # Bougouriba
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Boulgou"] <- 6 # Boulgou
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Boulkiemdé"] <- 7 # Boulkiemde
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Comoé"] <- 8 # Comoe
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Ganzourgou"] <- 9 # Ganzourgou
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Gnagna"] <- 10 # Gnagna
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Gourma"] <- 11 # Gourma
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Houet"] <- 12 # Houet
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Ioba"] <- 13 # Ioba
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Kadiogo"] <- 14 # Kadiogo
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Kénédougou"] <- 15 # Kenedougou
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Komandjoari"] <- 16 # Komandjoari
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Kompienga"] <- 17 # Kompienga
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Kossi"] <- 18 # Kossi
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Koulpélogo"] <- 19 # Koulpelogo
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Kouritenga"] <- 20 # Kouritenga
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Kourwéogo"] <- 21 # Kourweogo
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Léraba"] <- 22 # Leraba
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Loroum"] <- 23 # Loroum
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Mouhoun"] <- 24 # Mouhoun
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Nahouri"] <- 25 # Nahouri
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Namentenga"] <- 26 # Namentenga
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Nayala"] <- 27 # Nayala
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Noumbiel"] <- 28 # Noumbiel
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Oubritenga"] <- 29 # Oubritenga
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Oudalan"] <- 30 # Oudalan
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Passoré"] <- 31 # Passore
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Poni"] <- 32 # Poni
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Sanguié"] <- 33 # Sanguie
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Sanmatenga"] <- 34 # Sanmatenga
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Séno"] <- 35 # Seno
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Sissili"] <- 36 # Sissili
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Soum"] <- 37 # Soum
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Sourou"] <- 38 # Sourou
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Tapoa"] <- 39 # Tapoa
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Tuy"] <- 40 # Tuy
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Yagha"] <- 41 # Yagha
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Yatenga"] <- 42 # Yatenga
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Ziro"] <- 43 # Ziro
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Zondoma"] <- 44 # Zondoma
sf.BFA$REG_ID[sf.BFA$NAME_2 == "Zoundwéogo"] <- 45 # Zoundweogo
process_and_plot(sf.BFA, "bfa")
rm(sf.BFA)

## Bangladesh----
sf.BGD <- subset(g, GID_0 == "BGD")
g <- subset(g, GID_0 != "BGD")
sf.BGD$REG_ID[sf.BGD$NAME_1 == "Dhaka"] <- 1 # Dhaka
sf.BGD$REG_ID[sf.BGD$NAME_1 == "Chittagong"] <- 2 # Chittagong
sf.BGD$REG_ID[sf.BGD$NAME_1 == "Khulna"] <- 3 # Khulna
sf.BGD$REG_ID[sf.BGD$NAME_1 == "Rajshahi"] <- 4 # Rajshahi
sf.BGD$REG_ID[sf.BGD$NAME_1 == "Sylhet"] <- 5 # Sylhet
sf.BGD$REG_ID[sf.BGD$NAME_1 == "Barisal"] <- 6 # Barisal
sf.BGD$REG_ID[sf.BGD$NAME_1 == "Rangpur"] <- 7 # Rangpur
# No region code in the codebook for Mymensingh
process_and_plot(sf.BGD, "bgd")
rm(sf.BGD)

## Bulgaria----
sf.BGR <- subset(g, GID_0 == "BGR")
g <- subset(g, GID_0 != "BGR")
table(sf.BGR$NAME_1)
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Blagoevgrad"] <- 1 # Blagoevgrad
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Burgas"] <- 2 # Burgas
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Varna"] <- 3 # Varna
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Veliko Tarnovo"] <- 4 # Veliko Tyrnovo
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Vidin"] <- 5 # Vidin
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Vratsa"] <- 6 # Vraca
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Gabrovo"] <- 7 # Gabrovo
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Dobrich"] <- 8 # Dobrich
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Kardzhali"] <- 9 # Kyrdzhali
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Kyustendil"] <- 10 # Kiustendil
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Lovech"] <- 11 # Lovech
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Montana"] <- 12 # Montana
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Pazardzhik"] <- 13 # Pazardzhik
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Pernik"] <- 14 # Pernik
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Pleven"] <- 15 # Pleven
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Plovdiv"] <- 16 # Plovdiv
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Razgrad"] <- 17 # Razgrad
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Ruse"] <- 18 # Ruse
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Silistra"] <- 19 # Silistra
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Sliven"] <- 20 # Sliven
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Smolyan"] <- 21 # Smolian
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Grad Sofiya"] <- 22 # Sofiia-grad
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Sofia"] <- 23 # Sofiia-region
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Stara Zagora"] <- 24 # Stara Zagora
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Targovishte"] <- 25 # Tyrgovishte
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Haskovo"] <- 26 # Haskovo
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Shumen"] <- 27 # Shumen
sf.BGR$REG_ID[sf.BGR$NAME_1 == "Yambol"] <- 28 # Iambol
process_and_plot(sf.BGR, "bgr")
rm(sf.BGR)

## Bahrain----
sf.BHR <- subset(g, GID_0 == "BHR")
g <- subset(g, GID_0 != "BHR")
sf.BHR$REG_ID[sf.BHR$NAME_1 == "Capital"] <- 1 # Capital
sf.BHR$REG_ID[sf.BHR$NAME_1 == "Muharraq"] <- 2 # Muharraq
sf.BHR$REG_ID[sf.BHR$NAME_1 == "Northern"] <- 3 # Northern
sf.BHR$REG_ID[sf.BHR$NAME_1 == "Southern"] <- 5 # South
process_and_plot(sf.BHR, "bhr")
rm(sf.BHR)

## Bosnia and Herzegovina ----
sf.BIH <- subset(g, GID_0 == "BIH")
g <- subset(g, GID_0 != "BIH")
sf.BIH$REG_ID[sf.BIH$NAME_2 == "Bosnian Podrinje"] <- 1 # Bosnian Podrinje Canton (5), F B&H
sf.BIH$REG_ID[sf.BIH$NAME_2 == "Central Bosnia"] <- 2 # Central Bosnia Canton (6),F B&H
sf.BIH$REG_ID[sf.BIH$NAME_2 == "Herzegovina-Neretva"] <- 3 # Herzegovina-Neretva Canton (7), F B&H
sf.BIH$REG_ID[sf.BIH$NAME_2 == "Posavina"] <- 4 # Posavina Canton (2), F B&H
sf.BIH$REG_ID[sf.BIH$NAME_2 == "Sarajevo"] <- 5 # Sarajevo Canton (9), F B&H
sf.BIH$REG_ID[sf.BIH$NAME_2 == "Tuzla"] <- 6 # Tuzla Canton (3), F B&H
sf.BIH$REG_ID[sf.BIH$NAME_2 == "Una-Sana"] <- 7 # Una-Sana Canton (1), F B&H
sf.BIH$REG_ID[sf.BIH$NAME_2 == "Canton 10"] <- 8 # Canton 10 (West Bosnia) F B&H
sf.BIH$REG_ID[sf.BIH$NAME_2 == "West Herzegovina"] <- 9 # West Herzegovina Canton (8), F B&H
sf.BIH$REG_ID[sf.BIH$NAME_2 == "Zenica-Doboj"] <- 10 # Zenica-Doboj Canton (4), F B&H
sf.BIH$REG_ID[sf.BIH$NAME_2 %in% c("Banja Luka", "Doboj")] <- 11 # RS West (Republika Srpska)
sf.BIH$REG_ID[sf.BIH$NAME_2 %in% c("Bijeljina", "Vlasenica", "Istočno Sarajevo")] <- 12 # RS East (Republika Srpska)
sf.BIH$REG_ID[sf.BIH$NAME_2 %in% c("Trebinje", "Foča")] <- 13 # RS South (Republika Srpska)
sf.BIH$REG_ID[sf.BIH$NAME_2 == "Brčko"] <- 14 # Brcko District
process_and_plot(sf.BIH, "bih")
rm(sf.BIH)

## Belarus ----
sf.BLR <- subset(g, GID_0 == "BLR")
g <- subset(g, GID_0 != "BLR")
sf.BLR$REG_ID[sf.BLR$NAME_2 == "Minsk"] <- 1 # Minsk
sf.BLR$REG_ID[sf.BLR$NAME_1 == "Minsk" & sf.BLR$NAME_2 != "Minsk"] <- 2 # Minsk oblast
sf.BLR$REG_ID[sf.BLR$NAME_1 == "Brest"] <- 3 # Brest oblast
sf.BLR$REG_ID[sf.BLR$NAME_1 == "Vitebsk"] <- 4 # Vitebsk oblast
sf.BLR$REG_ID[sf.BLR$NAME_1 == "Gomel"] <- 5 # Gomel oblast
sf.BLR$REG_ID[sf.BLR$NAME_1 == "Grodno"] <- 6 # Grodno oblast
sf.BLR$REG_ID[sf.BLR$NAME_1 == "Mogilev"] <- 7 # Mogilev oblast
process_and_plot(sf.BLR, "blr")
rm(sf.BLR)

## Bolivia----
sf.BOL <- subset(g, GID_0 == "BOL")
g <- subset(g, GID_0 != "BOL")
sf.BOL$REG_ID[sf.BOL$NAME_1 == "Santa Cruz"] <- 101 # Santa Cruz
sf.BOL$REG_ID[sf.BOL$NAME_1 == "Beni"] <- 102 # Beni
sf.BOL$REG_ID[sf.BOL$NAME_1 == "Pando"] <- 103 # Pando
sf.BOL$REG_ID[sf.BOL$NAME_1 == "Cochabamba"] <- 201 # Cochabamba
sf.BOL$REG_ID[sf.BOL$NAME_1 == "Chuquisaca"] <- 202 # Chuquisaca
sf.BOL$REG_ID[sf.BOL$NAME_1 == "Tarija"] <- 203 # Tarija
sf.BOL$REG_ID[sf.BOL$NAME_1 == "La Paz"] <- 301 # La Paz
sf.BOL$REG_ID[sf.BOL$NAME_1 == "Oruro"] <- 302 # Oruro
sf.BOL$REG_ID[sf.BOL$NAME_1 == "Potosí"] <- 303 # PotosÃ
process_and_plot(sf.BOL, "bol")
rm(sf.BOL)

## Brazil----
sf.BRA <- subset(g, GID_0 == "BRA")
g <- subset(g, GID_0 != "BRA")
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Roraima"] <- 11 # Roraima
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Amazonas"] <- 13 # Amazonas
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Pará"] <- 15 # ParÃ¡
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Tocantins"] <- 17 # Tocantins
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Maranhão"] <- 21 # MaranhÃ£o
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Piauí"] <- 22 # PiauÃ£o
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Ceará"] <- 23 # CearÃ¡£o
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Rio Grande do Norte"] <- 24 # Rio Grande do Norte
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Paraíba"] <- 25 # ParaÃba
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Pernambuco"] <- 26 # Pernambuco
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Alagoas"] <- 27 # Alagoas
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Sergipe"] <- 28 # Sergipe
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Bahia"] <- 29 # Bahia
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Minas Gerais"] <- 31 # Minas Gerais
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Rio de Janeiro"] <- 33 # Rio de Janeiro
sf.BRA$REG_ID[sf.BRA$NAME_1 == "São Paulo"] <- 35 # SÃ£o Paulo
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Paraná"] <- 41 # ParanÃ¡
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Santa Catarina"] <- 42 # Santa Catarina
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Rio Grande do Sul"] <- 43 # Rio Grande do Sul
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Mato Grosso do Sul"] <- 50 # Mato Grosso do Sul
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Mato Grosso"] <- 51 # Mato Grosso
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Goiás"] <- 52 # GoiÃ¡s
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Distrito Federal"] <- 53 # Distrito Federal
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Espírito Santo"] <- 54 # Espirito Santo
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Amapá"] <- 55 # AmapÃ¡
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Rondônia"] <- 56 # Rondonia
sf.BRA$REG_ID[sf.BRA$NAME_1 == "Acre"] <- 57 # Acre
process_and_plot(sf.BRA, "bra")
rm(sf.BRA)

## Canada----
sf.CAN <- subset(g, GID_0 == "CAN")
g <- subset(g, GID_0 != "CAN")
table(sf.CAN$NAME_1)

# https://en.wikipedia.org/wiki/Atlantic_Canada
sf.CAN$REG_ID[sf.CAN$NAME_1 %in% c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick")] <- 1
sf.CAN$REG_ID[sf.CAN$NAME_3 == "Montréal"] <- 2 # Montreal CMA
sf.CAN$REG_ID[sf.CAN$NAME_1 == "Québec" & sf.CAN$NAME_3 != "Montréal"] <- 3 # Rest of Quebec (excluding Montreal)
sf.CAN$REG_ID[sf.CAN$NAME_3 == "Toronto"] <- 4 # Toronto CMA
sf.CAN$REG_ID[sf.CAN$NAME_3 != "Toronto" & sf.CAN$NAME_1 == "Ontario"] <- 5 # Rest of Ontario (excluding Toronto)

# https://en.wikipedia.org/wiki/Canadian_Prairies
sf.CAN$REG_ID[sf.CAN$NAME_1 %in% c("Alberta", "Saskatchewan", "Manitoba")] <- 6 # Praires
# namely Alberta, Saskatchewan, and Manitoba

sf.CAN$REG_ID[sf.CAN$NAME_3 == "Vancouver"] <- 7 # Vancouver CMA
sf.CAN$REG_ID[sf.CAN$NAME_3 != "Vancouver" & sf.CAN$NAME_1 == "British Columbia"] <- 8 # Rest of British Columbia (excluding Vancouver)
process_and_plot(sf.CAN, "can")
rm(sf.CAN)

# Note: there's no Yukon, Northwest Territories, or Nunavut code in the Gallup WRP;
# take this as an indication there's no survey responses from these sparsely populated areas

## Switzerland----
sf.CHE <- subset(g, GID_0 == "CHE")
g <- subset(g, GID_0 != "CHE")
CHE_reg1 <- unique(sf.CHE$NAME_1)
CHE_reg1 <- data.frame(NAME_1 = CHE_reg1, REG_ID = 1:length(CHE_reg1))
sf.CHE <- subset(sf.CHE, select = -REG_ID)
sf.CHE <- left_join(sf.CHE, CHE_reg1, by = "NAME_1")
process_and_plot(sf.CHE, "che")
rm(sf.CHE, CHE_reg1)

## Chile----
sf.CHL <- subset(g, GID_0 == "CHL")
g <- subset(g, GID_0 != "CHL")
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Tarapacá"] <- 1 # TarapacÃ¡
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Antofagasta"] <- 2 # Antofagasta
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Atacama"] <- 3 # Atacama
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Coquimbo"] <- 4 # Coquimbo
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Valparaíso"] <- 5 # ValparaÃso
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Libertador General Bernardo O'Hi"] <- 6 # OHiggins
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Maule"] <- 7 # Maule
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Bío-Bío"] <- 8 # BiobÃo
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Araucanía"] <- 9 # La AraucanÃa
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Los Lagos"] <- 10 # Los Lagos
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Aysén del General Ibañez del Cam"] <- 11 # AysÃ©n
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Magallanes y Antártica Chilena"] <- 12 # Magallanes y la AntÃ¡rtica
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Ñuble"] <- 13 # Chilena---I think this is referring to the Nuble region
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Santiago Metropolitan"] <- 14 # Metropolitana
sf.CHL$REG_ID[sf.CHL$NAME_1 == "Los Ríos"] <- 15 # Los RÃos
process_and_plot(sf.CHL, "chl")
rm(sf.CHL)

## China----
sf.CHN <- subset(g, GID_0 == "CHN")
g <- subset(g, GID_0 != "CHN")
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Anhui"] <- 1 # Anhui
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Fujian"] <- 2 # Fujian
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Gansu"] <- 3 # Gansu
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Guangdong"] <- 4 # Guangdong
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Guizhou"] <- 5 # Guizhou
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Hainan"] <- 6 # Hainan
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Hebei"] <- 7 # Hebei
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Heilongjiang"] <- 8 # Heilongjiang
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Henan"] <- 9 # Henan
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Hubei"] <- 10 # Hubei
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Hunan"] <- 11 # Hunan
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Jiangsu"] <- 12 # Jiangsu
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Jiangxi"] <- 13 # Jiangxi
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Jilin"] <- 14 # Jilin
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Liaoning"] <- 15 # Liaoning
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Qinghai"] <- 16 # Qinghai
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Shaanxi"] <- 17 # Shaanxi
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Shandong"] <- 18 # Shandong
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Shanxi"] <- 19 # Shanxi
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Sichuan"] <- 20 # Sichuan
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Yunnan"] <- 21 # Yunnan
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Zhejiang"] <- 22 # Zhejiang
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Guangxi"] <- 23 # Guangxi
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Nei Mongol"] <- 24 # Nei Mongol
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Ningxia Hui"] <- 25 # Ningxia
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Xinjiang Uygur"] <- 26 # Xinjiang Uygur
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Xizang"] <- 27 # Xizang (Tibet)
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Beijing"] <- 28 # Beijing municipality
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Chongqing"] <- 29 # Chongqing municipality
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Shanghai"] <- 30 # Shanghai municipality
sf.CHN$REG_ID[sf.CHN$NAME_1 == "Tianjin"] <- 31 # Tianjin municipality
sf.CHN <- subset(sf.CHN, NAME_1 != "Hong Kong")
process_and_plot(sf.CHN, "chn")

## Ivory Coast  ----
sf.CIV <- subset(g, GID_0 == "CIV")
g <- subset(g, GID_0 != "CIV")
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Abidjan"] <- 1 # Abidjan
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Bas-Sassandra"] <- 2 # Bas-Sassandra
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Comoé"] <- 3 # Comoé
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Denguélé"] <- 4 # Denguélé
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Gôh-Djiboua"] <- 5 # Gôh-Djiboua
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Lacs"] <- 6 # Lacs
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Lagunes"] <- 7 # Lagunes
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Montagnes"] <- 8 # Montagnes
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Sassandra-Marahoué"] <- 9 # Sassandra-Marahoué
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Savanes"] <- 10 # Savanes
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Vallée du Bandama"] <- 11 # Vallée du Bandama
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Woroba"] <- 12 # Woroba
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Yamoussoukro"] <- 13 # Yamoussoukro 
sf.CIV$REG_ID[sf.CIV$NAME_1 == "Zanzan"] <- 14 # Zanzan
process_and_plot(sf.CIV, "CIV")
rm(sf.CIV)

## Cameroon  ----
sf.CMR <- subset(g, GID_0 == "CMR")
g <- subset(g, GID_0 != "CMR")
sf.CMR$REG_ID[sf.CMR$NAME_1 == "Adamaoua"] <- 1 # Adamaoua
sf.CMR$REG_ID[sf.CMR$NAME_1 == "Centre"] <- 2 # Centre
sf.CMR$REG_ID[sf.CMR$NAME_1 == "Est"] <- 3 # Est
sf.CMR$REG_ID[sf.CMR$NAME_1 == "Extrême-Nord"] <- 4 # Extrême-Nord
sf.CMR$REG_ID[sf.CMR$NAME_1 == "Littoral"] <- 5 # Littoral
sf.CMR$REG_ID[sf.CMR$NAME_1 == "Nord"] <- 6 # Nord
sf.CMR$REG_ID[sf.CMR$NAME_1 == "Nord-Ouest"] <- 7 # Nord-Ouest
sf.CMR$REG_ID[sf.CMR$NAME_1 == "Sud"] <- 8 # Sud
sf.CMR$REG_ID[sf.CMR$NAME_1 == "Sud-Ouest"] <- 9 # Sud-Ouest
sf.CMR$REG_ID[sf.CMR$NAME_1 == "Ouest"] <- 10 # Ouest

sf.CMR$REG_ID[sf.CMR$NAME_3 == "Douala 1"] <- 11 # Douala
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Douala 2"] <- 11 # Douala
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Douala 3"] <- 11 # Douala
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Douala 4"] <- 11 # Douala
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Douala 5"] <- 11 # Douala
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Douala 6"] <- 11 # Douala

sf.CMR$REG_ID[sf.CMR$NAME_3 == "Yaoundé 1"] <- 12 # Yaoundé
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Yaoundé 2"] <- 12 # Yaoundé
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Yaoundé 3"] <- 12 # Yaoundé
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Yaoundé 4"] <- 12 # Yaoundé
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Yaoundé 5"] <- 12 # Yaoundé
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Yaoundé 6"] <- 12 # Yaoundé
sf.CMR$REG_ID[sf.CMR$NAME_3 == "Yaoundé 7"] <- 12 # Yaoundé

process_and_plot(sf.CMR, "CMR")
rm(sf.CMR)

## Congo Brazzaville  ----
sf.COG <- subset(g, GID_0 == "COG")
g <- subset(g, GID_0 != "COG")
sf.COG$REG_ID[sf.COG$NAME_1 == "Bouenza"] <- 1 # Bouenza
sf.COG$REG_ID[sf.COG$NAME_1 == "Brazzaville"] <- 2 # Brazzaville
sf.COG$REG_ID[sf.COG$NAME_1 == "Cuvette"] <- 3 # Cuvette
sf.COG$REG_ID[sf.COG$NAME_1 == "Cuvette-Ouest"] <- 4 # Cuvette-Ouest
sf.COG$REG_ID[sf.COG$NAME_1 == "Kouilou"] <- 5 # Kouilou
sf.COG$REG_ID[sf.COG$NAME_1 == "Lékoumou"] <- 6 # Lékoumou
sf.COG$REG_ID[sf.COG$NAME_1 == "Likouala"] <- 7 # Likouala
sf.COG$REG_ID[sf.COG$NAME_1 == "Niari"] <- 8 # Niari
sf.COG$REG_ID[sf.COG$NAME_1 == "Plateaux"] <- 9 # Plateaux
sf.COG$REG_ID[sf.COG$NAME_1 == "Pool"] <- 10 # Pool
sf.COG$REG_ID[sf.COG$NAME_1 == "Sangha"] <- 11 # Sangha
sf.COG$REG_ID[sf.COG$NAME_1 == "Pointe Noire"] <- 12 # Pointe Noire
process_and_plot(sf.COG, "COG")
rm(sf.COG)

## Colombia  ----
sf.COL <- subset(g, GID_0 == "COL")
g <- subset(g, GID_0 != "COL")
sf.COL$REG_ID[sf.COL$NAME_1 == "Antioquia"] <- 5 # Antioquia
sf.COL$REG_ID[sf.COL$NAME_1 == "Atlántico"] <- 8 # Atlántico
sf.COL$REG_ID[sf.COL$NAME_1 == "Bogotá D.C."] <- 11 # Bogotá D.C.
sf.COL$REG_ID[sf.COL$NAME_1 == "Bolívar"] <- 13 # Bolívar
sf.COL$REG_ID[sf.COL$NAME_1 == "Boyacá"] <- 15 # Boyacá
sf.COL$REG_ID[sf.COL$NAME_1 == "Caldas"] <- 17 # Caldas
sf.COL$REG_ID[sf.COL$NAME_1 == "Caquetá"] <- 18 # Caquetá
sf.COL$REG_ID[sf.COL$NAME_1 == "Cauca"] <- 19 # Cauca
sf.COL$REG_ID[sf.COL$NAME_1 == "Cesar"] <- 20 # Cesar
sf.COL$REG_ID[sf.COL$NAME_1 == "Córdoba"] <- 23 # Córdoba
sf.COL$REG_ID[sf.COL$NAME_1 == "Cundinamarca"] <- 25 # Cundinamarca
sf.COL$REG_ID[sf.COL$NAME_1 == "Huila"] <- 41 # Huila
sf.COL$REG_ID[sf.COL$NAME_1 == "La Guajira"] <- 44 # La Guajira
sf.COL$REG_ID[sf.COL$NAME_1 == "Magdalena"] <- 47 # Magdalena
sf.COL$REG_ID[sf.COL$NAME_1 == "Meta"] <- 50 # Meta
sf.COL$REG_ID[sf.COL$NAME_1 == "Nariño"] <- 52 # Nariño
sf.COL$REG_ID[sf.COL$NAME_1 == "Norte de Santander"] <- 54 # Norte de Santander
sf.COL$REG_ID[sf.COL$NAME_1 == "Quindío"] <- 63 # Quindío
sf.COL$REG_ID[sf.COL$NAME_1 == "Risaralda"] <- 66 # Risaralda
sf.COL$REG_ID[sf.COL$NAME_1 == "Santander"] <- 68 # Santander
sf.COL$REG_ID[sf.COL$NAME_1 == "Sucre"] <- 70 # Sucre
sf.COL$REG_ID[sf.COL$NAME_1 == "Tolima"] <- 73 # Tolima
sf.COL$REG_ID[sf.COL$NAME_1 == "Valle del Cauca"] <- 76 # Valle del Cauca
sf.COL$REG_ID[sf.COL$NAME_1 == "Chocó"] <- 77 # Chocó
sf.COL$REG_ID[sf.COL$NAME_1 == "Arauca"] <- 81 # Arauca
sf.COL$REG_ID[sf.COL$NAME_1 == "Casanare"] <- 85 # Casanare
sf.COL$REG_ID[sf.COL$NAME_1 == "Putumayo"] <- 86 # Putumayo
sf.COL$REG_ID[sf.COL$NAME_1 == "Amazonas"] <- 91 # Amazonas
sf.COL$REG_ID[sf.COL$NAME_1 == "Vaupés"] <- 97 # Vaupés
sf.COL$REG_ID[sf.COL$NAME_1 == "Vichada"] <- 99 # Vichada
sf.COL$REG_ID[sf.COL$NAME_1 == "Guainía"] <- 100 # Guainía
sf.COL$REG_ID[sf.COL$NAME_1 == "Guaviare"] <- 101 # Guaviare
sf.COL$REG_ID[sf.COL$NAME_1 == "San Andrés y Providencia"] <- 102 # San Andrés y Providencia
process_and_plot(sf.COL, "COL")
rm(sf.COL)

## Costa Rica   ----
sf.CRI <- subset(g, GID_0 == "CRI")
g <- subset(g, GID_0 != "CRI")
sf.CRI$REG_ID[sf.CRI$NAME_1 == "San José"] <- 1 # San José
sf.CRI$REG_ID[sf.CRI$NAME_1 == "Alajuela"] <- 2 # Alajuela
sf.CRI$REG_ID[sf.CRI$NAME_1 == "Cartago"] <- 3 # Cartago
sf.CRI$REG_ID[sf.CRI$NAME_1 == "Heredia"] <- 4 # Heredia
sf.CRI$REG_ID[sf.CRI$NAME_1 == "Guanacaste"] <- 5 # Guanacaste
sf.CRI$REG_ID[sf.CRI$NAME_1 == "Puntarenas"] <- 6 # Puntarenas
sf.CRI$REG_ID[sf.CRI$NAME_1 == "Limón"] <- 7 # Limón
process_and_plot(sf.CRI, "CRI")
rm(sf.CRI)

## Cyprus  ----
sf.CYP <- subset(g, GID_0 == "CYP")
g <- subset(g, GID_0 != "CYP")
sf.CYP$REG_ID[sf.CYP$NAME_1 == "Nicosia"] <- 1 # Nicosia
sf.CYP$REG_ID[sf.CYP$NAME_1 == "Limassol"] <- 2 # Limassol
sf.CYP$REG_ID[sf.CYP$NAME_1 == "Larnaca"] <- 3 # Larnaca
sf.CYP$REG_ID[sf.CYP$NAME_1 == "Famagusta"] <- 4 # Famagusta
sf.CYP$REG_ID[sf.CYP$NAME_1 == "Paphos"] <- 5 # Paphos
process_and_plot(sf.CYP, "CYP")
rm(sf.CYP)

## Germany  ----
sf.DEU <- subset(g, GID_0 == "DEU")
g <- subset(g, GID_0 != "DEU")
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Schleswig-Holstein"] <- 4001 # Schleswig-Holstein
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Hamburg"] <- 4002 # Hamburg
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Niedersachsen"] <- 4003 # Niedersachsen
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Bremen"] <- 4004 # Bremen
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Nordrhein-Westfalen"] <- 4005 # Nordrhein-Westfalen
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Hessen"] <- 4006 # Hessen
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Rheinland-Pfalz"] <- 4007 # Rheinland-Pfalz
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Baden-Württemberg"] <- 4008 # Baden-Württemberg
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Bayern"] <- 4009 # Bayern
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Saarland"] <- 4010 # Saarland
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Berlin"] <- 4011 # Berlin
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Brandenburg"] <- 4012 # Brandenburg
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Mecklenburg-Vorpommern"] <- 4013 # Mecklenburg-Vorpommern
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Sachsen"] <- 4014 # Sachsen
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Sachsen-Anhalt"] <- 4015 # Sachsen-Anhalt
sf.DEU$REG_ID[sf.DEU$NAME_1 == "Thüringen"] <- 4016 # Thüringen
process_and_plot(sf.DEU, "DEU")
rm(sf.DEU)

## Denmark  ----
sf.DNK <- subset(g, GID_0 == "DNK")
g <- subset(g, GID_0 != "DNK")
sf.DNK$REG_ID[sf.DNK$NAME_1 == "Hovedstaden"] <- 3001 # Hovedstaden
sf.DNK$REG_ID[sf.DNK$NAME_1 == "Sjælland"] <- 3002 # Sjælland
sf.DNK$REG_ID[sf.DNK$NAME_1 == "Syddanmark"] <- 3003 # Syddanmark
sf.DNK$REG_ID[sf.DNK$NAME_1 == "Midtjylland"] <- 3004 # Midtjylland
sf.DNK$REG_ID[sf.DNK$NAME_1 == "Nordjylland"] <- 3005 # Nordjylland
process_and_plot(sf.DNK, "DNK")
rm(sf.DNK)

## Dominican Republic  ----
sf.DOM <- subset(g, GID_0 == "DOM")
g <- subset(g, GID_0 != "DOM")
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Distrito Nacional"] <- 1 # Distrito Nacional
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Azua"] <- 2 # Azua
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Bahoruco"] <- 3 # Bahoruco
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Barahona"] <- 4 # Barahona
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Dajabón"] <- 5 # Dajabón
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Duarte"] <- 6 # Duarte
sf.DOM$REG_ID[sf.DOM$NAME_1 == "La Estrelleta"] <- 7 # La Estrelleta
sf.DOM$REG_ID[sf.DOM$NAME_1 == "El Seybo"] <- 8 # El Seybo
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Espaillat"] <- 9 # Espaillat
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Independencia"] <- 10 # Independencia
sf.DOM$REG_ID[sf.DOM$NAME_1 == "La Altagracia"] <- 11 # La Altagracia
sf.DOM$REG_ID[sf.DOM$NAME_1 == "La Romana"] <- 12 # La Romana
sf.DOM$REG_ID[sf.DOM$NAME_1 == "La Vega"] <- 13 # La Vega
sf.DOM$REG_ID[sf.DOM$NAME_1 == "María Trinidad Sánchez"] <- 14 # María Trinidad Sánchez
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Monte Cristi"] <- 15 # Monte Cristi
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Pedernales"] <- 16 # Pedernales
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Peravia"] <- 17 # Peravia
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Puerto Plata"] <- 18 # Puerto Plata
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Salcedo"] <- 19 # Salcedo
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Samaná"] <- 20 # Samaná
sf.DOM$REG_ID[sf.DOM$NAME_1 == "San Cristóbal"] <- 21 # San Cristóbal
sf.DOM$REG_ID[sf.DOM$NAME_1 == "San Juan"] <- 22 # San Juan
sf.DOM$REG_ID[sf.DOM$NAME_1 == "San Pedro de Macorís"] <- 23 # San Pedro de Macorís
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Sánchez Ramírez"] <- 24 # Sánchez Ramírez
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Santiago"] <- 25 # Santiago
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Santiago Rodríguez"] <- 26 # Santiago Rodríguez
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Valverde"] <- 27 # Valverde
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Monseñor Nouel"] <- 28 # Monseñor Nouel
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Monte Plata"] <- 29 # Monte Plata
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Hato Mayor"] <- 30 # Hato Mayor
sf.DOM$REG_ID[sf.DOM$NAME_1 == "San José de Ocoa"] <- 31 # San José de Ocoa
sf.DOM$REG_ID[sf.DOM$NAME_1 == "Santo Domingo"] <- 32 # Santo Domingo
process_and_plot(sf.DOM, "DOM")
rm(sf.DOM)

## Algeria  ----
sf.DZA <- subset(g, GID_0 == "DZA")
g <- subset(g, GID_0 != "DZA")
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Adrar"] <- 1 # Adrar
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Chlef"] <- 2 # Chlef
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Laghouat"] <- 3 # Laghouat
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Oum el Bouaghi"] <- 4 # Oum el Bouaghi
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Batna"] <- 5 # Batna
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Béjaïa"] <- 6 # Béjaïa
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Biskra"] <- 7 # Biskra
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Béchar"] <- 8 # Béchar
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Blida"] <- 9 # Blida
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Bouira"] <- 10 # Bouira
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Tamanghasset"] <- 11 # Tamanghasset
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Tébessa"] <- 12 # Tébessa
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Tlemcen"] <- 13 # Tlemcen
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Tiaret"] <- 14 # Tiaret
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Tizi Ouzou"] <- 15 # Tizi Ouzou
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Alger"] <- 16 # Alger
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Djelfa"] <- 17 # Djelfa
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Jijel"] <- 18 # Jijel
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Sétif"] <- 19 # Sétif
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Saïda"] <- 20 # Saïda
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Skikda"] <- 21 # Skikda
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Sidi Bel Abbès"] <- 22 # Sidi Bel Abbès
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Annaba"] <- 23 # Annaba
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Guelma"] <- 24 # Guelma
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Constantine"] <- 25 # Constantine
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Médéa"] <- 26 # Médéa
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Mostaganem"] <- 27 # Mostaganem
sf.DZA$REG_ID[sf.DZA$NAME_1 == "M'Sila"] <- 28 # M'Sila
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Mascara"] <- 29 # Mascara
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Ouargla"] <- 30 # Ouargla
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Oran"] <- 31 # Oran
sf.DZA$REG_ID[sf.DZA$NAME_1 == "El Bayadh"] <- 32 # El Bayadh
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Illizi"] <- 33 # Illizi
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Bordj Bou Arréridj"] <- 34 # Bordj Bou Arréridj
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Boumerdès"] <- 35 # Boumerdès
sf.DZA$REG_ID[sf.DZA$NAME_1 == "El Tarf"] <- 36 # El Tarf
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Tindouf"] <- 37 # Tindouf
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Tissemsilt"] <- 38 # Tissemsilt
sf.DZA$REG_ID[sf.DZA$NAME_1 == "El Oued"] <- 39 # El Oued
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Khenchela"] <- 40 # Khenchela
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Souk Ahras"] <- 41 # Souk Ahras
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Tipaza"] <- 42 # Tipaza
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Mila"] <- 43 # Mila
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Aïn Defla"] <- 44 # Aïn Defla
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Naâma"] <- 45 # Naâma
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Aïn Témouchent"] <- 46 # Aïn Témouchent
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Ghardaïa"] <- 47 # Ghardaïa
sf.DZA$REG_ID[sf.DZA$NAME_1 == "Relizane"] <- 48 # Relizane
process_and_plot(sf.DZA, "DZA")
rm(sf.DZA)

## Ecuador  ----
sf.ECU <- subset(g, GID_0 == "ECU")
g <- subset(g, GID_0 != "ECU")
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Azuay"] <- 1 # Azuay
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Bolivar"] <- 2 # Bolivar
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Carchi"] <- 3 # Carchi
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Cotopaxi"] <- 4 # Cotopaxi
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Chimborazo"] <- 5 # Chimborazo
sf.ECU$REG_ID[sf.ECU$NAME_1 == "El Oro"] <- 6 # El Oro
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Esmeraldas"] <- 7 # Esmeraldas
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Guayas"] <- 8 # Guayas
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Imbabura"] <- 9 # Imbabura
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Loja"] <- 10 # Loja
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Los Rios"] <- 11 # Los Rios
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Manabi"] <- 12 # Manabi
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Morona Santiago"] <- 13 # Morona Santiago
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Napo"] <- 14 # Napo
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Orellana"] <- 15 # Orellana
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Pichincha"] <- 16 # Pichincha
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Sucumbios"] <- 17 # Sucumbios
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Tungurahua"] <- 18 # Tungurahua
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Zamora Chinchipe"] <- 19 # Zamora Chinchipe
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Pastaza"] <- 20 # Pastaza
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Santa Elena"] <- 21 # Santa Elena
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Santo Domingo de los Tsachilas"] <- 22 # Santo Domingo de los Tsachilas
sf.ECU$REG_ID[sf.ECU$NAME_1 == "Cañar"] <- 23 # Cañar
# sf.ECU$REG_ID[sf.ECU$NAME_1 == "Galápagos"] <- 24 #Galápagos
# Bibiane: *not sure about 24, Galapagos is in sf.ECU but not Gallup
# Alex: we can exclude Galapagos. I don't think they collected sample there.
process_and_plot(sf.ECU, "ECU")
rm(sf.ECU)

## Egypt  ----
sf.EGY <- subset(g, GID_0 == "EGY")
g <- subset(g, GID_0 != "EGY")
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Ad Daqahliyah"] <- 1 # Ad Daqahliyah
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Fayyum"] <- 2 # Al Fayyum
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Ash Sharqiyah"] <- 3 # Ash Sharqiyah
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Aswan"] <- 4 # Aswan
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Qahirah"] <- 5 # Al Qahirah
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Isma`iliyah"] <- 6 # Al Isma`iliyah
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Minya"] <- 7 # Al Minya
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Iskandariyah"] <- 8 # Al Iskandariyah
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Gharbiyah"] <- 9 # Al Gharbiyah
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Suhaj"] <- 10 # Suhaj
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Jizah"] <- 11 # Al Jizah
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Bur Sa`id"] <- 12 # Bur Sa`id
sf.EGY$REG_ID[sf.EGY$NAME_1 == "As Suways"] <- 13 # As Suways
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Dumyat"] <- 14 # Dumyat
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Qalyubiyah"] <- 15 # Al Qalyubiyah
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Kafr ash Shaykh"] <- 16 # Kafr ash Shaykh
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Minufiyah"] <- 17 # Al Minufiyah
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Buhayrah"] <- 18 # Al Buhayrah
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Bani Suwayf"] <- 19 # Bani Suwayf
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Asyut"] <- 20 # Asyut
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Qina"] <- 21 # Qina
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Uqsur"] <- 22 # Al Uqsur
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Matrouh"] <- 23 # Matrouh
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Bahr al Ahmar"] <- 24 # Al Bahr al Ahmar
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Al Wadi al Jadid"] <- 25 # Al Wadi al Jadid
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Shamal Sina'"] <- 26 # Shamal Sina'
sf.EGY$REG_ID[sf.EGY$NAME_1 == "Janub Sina'"] <- 27 # Janub Sina'
process_and_plot(sf.EGY, "EGY")
rm(sf.EGY)

## Spain  ----
sf.ESP <- subset(g, GID_0 == "ESP")
g <- subset(g, GID_0 != "ESP")
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Galicia"] <- 1 # Galicia
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Principado de Asturias"] <- 2 # Principado de Asturias
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Cantabria"] <- 3 # Cantabria
sf.ESP$REG_ID[sf.ESP$NAME_1 == "País Vasco"] <- 4 # País Vasco
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Comunidad Foral de Navarra"] <- 5 # Comunidad Foral de Navarra
sf.ESP$REG_ID[sf.ESP$NAME_1 == "La Rioja"] <- 6 # La Rioja
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Aragón"] <- 7 # Aragón
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Comunidad de Madrid"] <- 8 # Comunidad de Madrid
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Castilla y León"] <- 9 # Castilla y León
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Castilla-La Mancha"] <- 10 # Castilla-La Mancha
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Extremadura"] <- 11 # Extremaudra
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Cataluña"] <- 12 # Cataluña
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Comunidad Valenciana"] <- 13 # Comunidad Valenciana
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Islas Baleares"] <- 14 # Islas Baleares
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Andalucía"] <- 15 # Andalucía
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Región de Murcia"] <- 16 # Región de Murcia
sf.ESP$REG_ID[sf.ESP$NAME_2 == "Ceuta"] <- 17 # Ceuta
sf.ESP$REG_ID[sf.ESP$NAME_2 == "Melilla"] <- 18 # Melilla
sf.ESP$REG_ID[sf.ESP$NAME_1 == "Islas Canarias"] <- 19 # Islas Canarias
process_and_plot(sf.ESP, "ESP")
rm(sf.ESP)

## Estonia  ----
sf.EST <- subset(g, GID_0 == "EST")
g <- subset(g, GID_0 != "EST")
sf.EST$REG_ID[sf.EST$NAME_1 == "Harju"] <- 1 # Harju
sf.EST$REG_ID[sf.EST$NAME_1 == "Hiiu"] <- 2 # Hiiu
sf.EST$REG_ID[sf.EST$NAME_1 == "Ida-Viru"] <- 3 # Ida-Viru
sf.EST$REG_ID[sf.EST$NAME_1 == "Järva"] <- 4 # Järva
sf.EST$REG_ID[sf.EST$NAME_1 == "Jõgeva"] <- 5 # Jõgeva
sf.EST$REG_ID[sf.EST$NAME_1 == "Lääne"] <- 6 # Lääne
sf.EST$REG_ID[sf.EST$NAME_1 == "Lääne-Viru"] <- 7 # Lääne-Viru
sf.EST$REG_ID[sf.EST$NAME_1 == "Pärnu"] <- 8 # Pärnu
sf.EST$REG_ID[sf.EST$NAME_1 == "Põlva"] <- 9 # Põlva
sf.EST$REG_ID[sf.EST$NAME_1 == "Rapla"] <- 10 # Rapla
sf.EST$REG_ID[sf.EST$NAME_1 == "Saare"] <- 11 # Saare
sf.EST$REG_ID[sf.EST$NAME_1 == "Tartu"] <- 12 # Tartu
sf.EST$REG_ID[sf.EST$NAME_1 == "Valga"] <- 13 # Valga
sf.EST$REG_ID[sf.EST$NAME_1 == "Viljandi"] <- 14 # Viljandi
sf.EST$REG_ID[sf.EST$NAME_1 == "Võru"] <- 15 # Võru
sf.EST$REG_ID[sf.EST$NAME_1 == "Peipsi"] <- 16 # Peipsi
process_and_plot(sf.EST, "EST")
rm(sf.EST)

## Ethiopia  ----
sf.ETH <- subset(g, GID_0 == "ETH")
g <- subset(g, GID_0 != "ETH")
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Addis Abeba"] <- 1 # Addis Abeba
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Afar"] <- 2 # Afar
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Amhara"] <- 3 # Amhara
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Benshangul-Gumaz"] <- 4 # Benshangul-Gumaz
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Dire Dawa"] <- 5 # Dire Dawa
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Gambela Peoples"] <- 6 # Gambela Peoples
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Harari People"] <- 7 # Harari People
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Oromia"] <- 8 # Oromia
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Somali"] <- 9 # Somali
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Southern Nations, Nationalities"] <- 10 # Southern Nations, Nationalities
sf.ETH$REG_ID[sf.ETH$NAME_1 == "Tigray"] <- 11 # Tigray
process_and_plot(sf.ETH, "ETH")
rm(sf.ETH)

## Finland  ----
sf.FIN <- subset(g, GID_0 == "FIN")
g <- subset(g, GID_0 != "FIN")

sf_aland <- st_read(here("Data", "input", "world_admin_boundaries", "finland", "ALA_adm0.shp"))
sf_aland$GID_0 <- "FIN"
sf_aland$REG_ID <- 1
sf_aland <- subset(sf_aland, select = c(GID_0, REG_ID, geometry))
st_geometry(sf_aland) <- "geom"

sf.FIN$REG_ID[sf.FIN$NAME_2 == "Central Finland"] <- 2 # Central Finland
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Central Ostrobothnia"] <- 3 # Central Ostrobothnia
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Finland Proper"] <- 4 # Finland Proper
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Kainuu"] <- 5 # Kainuu
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Kymenlaakso"] <- 6 # Kymenlaakso
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Lapland"] <- 7 # Lapland
sf.FIN$REG_ID[sf.FIN$NAME_2 == "North Karelia"] <- 8 # North Karelia
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Northern Ostrobothnia"] <- 9 # Northern Ostrobothnia
sf.FIN$REG_ID[sf.FIN$NAME_2 == "North Savonia"] <- 10 # North Savonia
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Ostrobothnia"] <- 11 # Ostrobothnia
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Päijänne Tavastia"] <- 12 # Päijänne Tavastia
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Pirkanmaa"] <- 13 # Pirkanmaa
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Satakunta"] <- 14 # Satakunta
sf.FIN$REG_ID[sf.FIN$NAME_2 == "South Karelia"] <- 15 # South Karelia
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Southern Ostrobothnia"] <- 16 # Southern Ostrobothnia
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Southern Savonia"] <- 17 # Southern Savonia
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Tavastia Proper"] <- 18 # Tavastia Proper
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Uusimaa"] <- 19 # Uusimaa
sf.FIN$REG_ID[sf.FIN$NAME_2 == "Eastern Uusimaa"] <- 19 # Eastern Uusimaa
# Bibiane: *not sure about 20, Eastern Uusimaa is in sf.FIN but not Gallup
# Alex: It consolidated with Uusimaa in 2011: 
# https://en.wikipedia.org/wiki/Eastern_Uusimaa#:~:text=Eastern%20Uusimaa%20or%2C%20officially%2C%20Itä,Päijät%2DHäme)%20and%20Kymenlaakso.

sf.FIN <- bind_rows(sf.FIN, sf_aland)

process_and_plot(sf.FIN, "FIN")
rm(sf.FIN)

## France  ----
sf.FRA <- subset(g, GID_0 == "FRA")
g <- subset(g, GID_0 != "FRA")
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Grand Est"] <- 1 # Grand Est
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Nouvelle-Aquitaine"] <- 2 # Nouvelle-Aquitaine
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Auvergne-Rhône-Alpes"] <- 3 # Auvergne-Rhône-Alpes
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Bourgogne-Franche-Comté"] <- 4 # Bourgogne-Franche-Comté
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Bretagne"] <- 5 # Bretagne
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Centre-Val de Loire"] <- 6 # Centre-Val de Loire
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Corse"] <- 7 # Corse
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Île-de-France"] <- 8 # Île-de-France
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Occitanie"] <- 9 # Occitanie
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Hauts-de-France"] <- 10 # Hauts-de-France
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Normandie"] <- 11 # Normandie
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Pays de la Loire"] <- 12 # Pays de la Loire
sf.FRA$REG_ID[sf.FRA$NAME_1 == "Provence-Alpes-Côte d'Azur"] <- 13 # Provence-Alpes-Côte d'Azur
process_and_plot(sf.FRA, "FRA")
rm(sf.FRA)

## Gabon  ----
sf.GAB <- subset(g, GID_0 == "GAB")
g <- subset(g, GID_0 != "GAB")
sf.GAB$REG_ID[sf.GAB$NAME_1 == "Estuaire"] <- 1 # Estuaire
sf.GAB$REG_ID[sf.GAB$NAME_1 == "Haut-Ogooué"] <- 2 # Haut-Ogooué
sf.GAB$REG_ID[sf.GAB$NAME_1 == "Moyen-Ogooué"] <- 3 # Moyen-Ogooué
sf.GAB$REG_ID[sf.GAB$NAME_1 == "Ngounié"] <- 4 # Ngounié
sf.GAB$REG_ID[sf.GAB$NAME_1 == "Nyanga"] <- 5 # Nyanga
sf.GAB$REG_ID[sf.GAB$NAME_1 == "Ogooué-Ivindo"] <- 6 # Ogooué-Ivindo
sf.GAB$REG_ID[sf.GAB$NAME_1 == "Ogooué-Lolo"] <- 7 # Ogooué-Lolo
sf.GAB$REG_ID[sf.GAB$NAME_1 == "Ogooué-Maritime"] <- 8 # Ogooué-Maritime
sf.GAB$REG_ID[sf.GAB$NAME_1 == "Wouleu-Ntem"] <- 9 # Wouleu-Ntem
process_and_plot(sf.GAB, "GAB")
rm(sf.GAB)

## United Kingdom  ----
# https://en.wikipedia.org/wiki/ITL_1_statistical_regions_of_England

sf.GBR <- subset(g, GID_0 == "GBR")
g <- subset(g, GID_0 != "GBR")
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Derby"] <- 1 # East Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Derbyshire"] <- 1 # East Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Leicestershire"] <- 1 # East Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Leicester"] <- 1 # East Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Lincolnshire"] <- 1 # East Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Northamptonshire"] <- 1 # East Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Nottinghamshire"] <- 1 # East Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Nottingham"] <- 1 # East Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Rutland"] <- 1 # East Midlands

sf.GBR$REG_ID[sf.GBR$NAME_2 == "Bedford"] <- 2 # East of England
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Luton"] <- 2 # East of England
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Central Bedfordshire"] <- 2 # East of England
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Peterborough"] <- 2 # East of England
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Cambridgeshire"] <- 2 # East of England
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Essex"] <- 2 # East of England
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Hertfordshire"] <- 2 # East of England
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Norfolk"] <- 2 # East of England
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Suffolk"] <- 2 # East of England
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Southend-on-Sea"] <- 2 # East of England
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Thurrock"] <- 2 # East of England

sf.GBR$REG_ID[sf.GBR$NAME_2 == "Greater London"] <- 3 # London

sf.GBR$REG_ID[sf.GBR$NAME_2 == "Northumberland"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Newcastle upon Tyne"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "North Tyneside"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Gateshead"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "South Tyneside"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Sunderland"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "County Durham"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Darlington"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Hartlepool"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Stockton-on-Tees"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Redcar and Cleveland"] <- 4 # North East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Middlesbrough"] <- 4 # North East

sf.GBR$REG_ID[sf.GBR$NAME_2 == "Cumbria"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Cheshire East"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Cheshire West and Chester"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Warrington"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Manchester"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Salford"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Trafford"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Stockport"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Tameside"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Bolton"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Wigan"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Bury"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Oldham"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Rochdale"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Blackburn with Darwen"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Blackpool"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Lancashire"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Knowsley"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "St. Helens"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Halton"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Liverpool"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Sefton"] <- 5 # North West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Wirral"] <- 5 # North West

sf.GBR$REG_ID[sf.GBR$NAME_1 == "Scotland"] <- 6 # Scotland

sf.GBR$REG_ID[sf.GBR$NAME_2 == "West Berkshire"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Reading"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Wokingham"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Bracknell Forest"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Windsor and Maidenhead"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Slough"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Buckinghamshire"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Milton Keynes"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "East Sussex"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Brighton and Hove"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Hampshire"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Southampton"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Portsmouth"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Isle of Wight"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Kent"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Medway"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Oxfordshire"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Surrey"] <- 7 # South East
sf.GBR$REG_ID[sf.GBR$NAME_2 == "West Sussex"] <- 7 # South East

sf.GBR$REG_ID[sf.GBR$NAME_2 == "Bath and North East Somerset"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "North Somerset"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Somerset"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Bristol, City of"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "South Gloucestershire"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Gloucestershire"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Swindon"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Wiltshire"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Dorset"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Bournemouth, Christchurch and Po"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Devon"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Torbay"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Plymouth"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Isles of Scilly"] <- 8 # South West
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Cornwall"] <- 8 # South West

sf.GBR$REG_ID[sf.GBR$NAME_1 == "Wales"] <- 9 # Wales

sf.GBR$REG_ID[sf.GBR$NAME_2 == "Herefordshire, County of"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Worcestershire"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Warwickshire"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Telford and Wrekin"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Shropshire"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Stoke-on-Trent"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Staffordshire"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Birmingham"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Solihull"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Coventry"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Dudley"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Sandwell"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Walsall"] <- 10 # West Midlands
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Wolverhampton"] <- 10 # West Midlands

sf.GBR$REG_ID[sf.GBR$NAME_2 == "Kingston upon Hull, City of"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "East Riding of Yorkshire"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "North Lincolnshire"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "North East Lincolnshire"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "York"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "North Yorkshire"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Barnsley"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Doncaster"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Rotherham"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Sheffield"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Bradford"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Leeds"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Calderdale"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Kirklees"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_2 == "Wakefield"] <- 11 # Yorkshire and The Humber
sf.GBR$REG_ID[sf.GBR$NAME_1 == "Northern Ireland"] <- 12 # Northern Ireland

process_and_plot(sf.GBR, "GBR")
rm(sf.GBR)

## Ghana  ----
sf.GHA <- subset(g, GID_0 == "GHA")
g <- subset(g, GID_0 != "GHA")

sf.GHA$REG_ID[sf.GHA$NAME_1 == "Ashanti"] <- 1 # Ashanti

sf.GHA$REG_ID[sf.GHA$NAME_1 == "Ahafo"] <- 2 # Brong-Ahafo
sf.GHA$REG_ID[sf.GHA$NAME_1 == "Bono"] <- 2 # Brong-Ahafo
sf.GHA$REG_ID[sf.GHA$NAME_1 == "Bono East"] <- 2 # Brong-Ahafo

sf.GHA$REG_ID[sf.GHA$NAME_1 == "Central"] <- 3 # Central
sf.GHA$REG_ID[sf.GHA$NAME_1 == "Eastern"] <- 4 # Eastern
sf.GHA$REG_ID[sf.GHA$NAME_1 == "Greater Accra"] <- 5 # Greater Accra

sf.GHA$REG_ID[sf.GHA$NAME_1 == "Northern"] <- 6 # Northern
sf.GHA$REG_ID[sf.GHA$NAME_1 == "North East"] <- 6 # Northern
sf.GHA$REG_ID[sf.GHA$NAME_1 == "Savannah"] <- 6 # Northern

sf.GHA$REG_ID[sf.GHA$NAME_1 == "Upper East"] <- 7 # Upper East
sf.GHA$REG_ID[sf.GHA$NAME_1 == "Upper West"] <- 8 # Upper West

sf.GHA$REG_ID[sf.GHA$NAME_1 == "Volta"] <- 9 # Volta
sf.GHA$REG_ID[sf.GHA$NAME_1 == "Oti"] <- 9 # Volta

sf.GHA$REG_ID[sf.GHA$NAME_1 == "Western"] <- 10 # Western
sf.GHA$REG_ID[sf.GHA$NAME_1 == "Western North"] <- 10 # Western

#* #https://en.wikipedia.org/wiki/2018_Ghanaian_new_regions_referendum
# Ghana created new regions in 2019
# https://theconversation.com/ghanas-regions-why-creating-new-territories-has-caused-problems-203607

process_and_plot(sf.GHA, "GHA")
rm(sf.GHA)

## Guinea  ----
sf.GIN <- subset(g, GID_0 == "GIN")
g <- subset(g, GID_0 != "GIN")
sf.GIN$REG_ID[sf.GIN$NAME_1 == "Boké"] <- 1 # Boké
sf.GIN$REG_ID[sf.GIN$NAME_1 == "Conakry"] <- 2 # Conakry
sf.GIN$REG_ID[sf.GIN$NAME_1 == "Faranah"] <- 3 # Faranah
sf.GIN$REG_ID[sf.GIN$NAME_1 == "Kankan"] <- 4 # Kankan
sf.GIN$REG_ID[sf.GIN$NAME_1 == "Kindia"] <- 5 # Kindia
sf.GIN$REG_ID[sf.GIN$NAME_1 == "Labé"] <- 6 # Labé
sf.GIN$REG_ID[sf.GIN$NAME_1 == "Mamou"] <- 7 # Mamou
sf.GIN$REG_ID[sf.GIN$NAME_1 == "Nzérékoré"] <- 8 # Nzérékoré
process_and_plot(sf.GIN, "GIN")
rm(sf.GIN)

# Gambia  ----
gmb.2 <- st_read(here("Data", "input", "world_admin_boundaries", "gambia", "gmb_admbnda_adm2_ndma_20220901.shp"))
gmb.3 <- st_read(here("Data", "input", "world_admin_boundaries", "gambia", "gmb_admbnda_adm3_ndma_20220901.shp"))
sf.GMB <- bind_rows(gmb.2, gmb.3)
sf.GMB$REG_ID[sf.GMB$ADM1_EN == "Kanifing Municipal Council"] <- 1 # Kanifing
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Kombo North/St Marie"] <- 2 # Kombo Saint Mary
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Kombo South"] <- 3 # Kombo South
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Kombo Central"] <- 4 # Kombo Central
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Kombo East"] <- 5 # Kombo East
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Foni Bondali"] <- 6 # Foni Bondali
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Foni Brefet"] <- 7 # Foni Brefet
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Foni Kansala"] <- 8 # Foni Kansala
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Foni Bintang Karenai"] <- 9 # Foni Bintang Karanai
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Foni Jarrol"] <- 10 # Foni Jarrol
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Janjanbureh"] <- 11 # Janjanbureh
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Lower Fuladu West"] <- 12 # Fulladu West
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Upper Fuladu West"] <- 12 # Fulladu West
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Niamina East"] <- 13 # Niamina East
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Niamina West"] <- 14 # Niamina West
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Niamina Dankunku"] <- 15 # Niamina Dankunku
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Lower Saloum"] <- 16 # Lower Saloum
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Upper Saloum"] <- 17 # Upper Saloum
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Nianija"] <- 18 # Nianija
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Niani"] <- 19 # Niani
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Sami"] <- 20 # Sami
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Kiang Central"] <- 21 # Kiang Central
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Kiang East"] <- 22 # Kiang East
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Kiang West"] <- 23 # Kiang West
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Jarra Central"] <- 24 # Jarra Central
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Jarra East"] <- 25 # Jarra East
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Jarra West"] <- 26 # Jarra West
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Lower Niumi"] <- 27 # Lower Niumi
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Upper Niumi"] <- 28 # Upper Niumi
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Jokadu"] <- 29 # Jokadu
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Central Baddibu"] <- 30 # Central Baddibu
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Illiassa"] <- 31 # Central Baddibu
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Lower Baddibu"] <- 32 # Lower Baddibu
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Sabach - Sanjal"] <- 33 # Lower Baddibu
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Kantora"] <- 34 # Kantora
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Tumana"] <- 35 # Tumana
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Basse"] <- 36 # Fulladu East
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Jimara"] <- 37 # Jimara
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Wuli West"] <- 38 # Wuli West
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Wuli East"] <- 39 # Wuli East
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Sandu"] <- 40 # Sandu
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Banjul South"] <- 41 # Banjul South
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Banjul Central"] <- 42 # Banjul Central
sf.GMB$REG_ID[sf.GMB$ADM2_EN == "Banjul North"] <- 43 # Banjul North
sf.GMB$GID_0 <- "GMB"
st_geometry(sf.GMB) <- "geom"
process_and_plot(sf.GMB, "GMB")
rm(sf.GMB)

## Greece  ----
sf.GRC <- subset(g, GID_0 == "GRC")
g <- subset(g, GID_0 != "GRC")
sf.GRC$REG_ID[sf.GRC$NAME_2 == "Attica"] <- 1 # Attica
sf.GRC$REG_ID[sf.GRC$NAME_2 == "Central Greece"] <- 2 # Central Greece
sf.GRC$REG_ID[sf.GRC$NAME_2 == "Central Macedonia"] <- 3 # Central Macedonia
sf.GRC$REG_ID[sf.GRC$NAME_2 == "Crete"] <- 4 # Crete
sf.GRC$REG_ID[sf.GRC$NAME_2 == "East Macedonia and Thrace"] <- 5 # East Macedonia and Thrace
sf.GRC$REG_ID[sf.GRC$NAME_2 == "Epirus"] <- 6 # Epirus
sf.GRC$REG_ID[sf.GRC$NAME_2 == "Ionian Islands"] <- 7 # Ionian Islands
sf.GRC$REG_ID[sf.GRC$NAME_2 == "North Aegean"] <- 8 # North Aegean
sf.GRC$REG_ID[sf.GRC$NAME_2 == "Peloponnese"] <- 9 # Peloponnese
sf.GRC$REG_ID[sf.GRC$NAME_2 == "South Aegean"] <- 10 # South Aegean
sf.GRC$REG_ID[sf.GRC$NAME_2 == "Thessaly"] <- 11 # Thessaly
sf.GRC$REG_ID[sf.GRC$NAME_2 == "West Greece"] <- 12 # West Greece
sf.GRC$REG_ID[sf.GRC$NAME_2 == "West Macedonia"] <- 13 # West Macedonia

# sf.GRC$REG_ID[sf.GRC$NAME_2 == "Athos"] <- 14 #Athos
# Bibiane: not sure about 14, Athos is in the sf.GRC but not Gallup
# Alex: it's a small island: https://en.wikipedia.org/wiki/Mount_Athos
process_and_plot(sf.GRC, "GRC")
rm(sf.GRC)

## Guatemala  ----
sf.GTM <- subset(g, GID_0 == "GTM")
g <- subset(g, GID_0 != "GTM")
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Guatemala"] <- 1 # Guatemala
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Huehuetenango"] <- 2 # Huehuetenango
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Alta Verapaz"] <- 3 # Alta Verapaz
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Quiché"] <- 4 # Quiché
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Izabal"] <- 5 # Izabal
sf.GTM$REG_ID[sf.GTM$NAME_1 == "El Progreso"] <- 6 # El Progreso
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Sacatepéquez"] <- 7 # Sacatepéquez
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Chimaltenango"] <- 8 # Chimaltenango
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Escuintla"] <- 9 # Escuintla
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Santa Rosa"] <- 10 # Santa Rosa
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Sololá"] <- 11 # Sololá
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Totonicapán"] <- 12 # Totonicapán
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Quezaltenango"] <- 13 # Quezaltenango
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Suchitepéquez"] <- 14 # Suchitepéquez
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Retalhuleu"] <- 15 # Retalhuleu
sf.GTM$REG_ID[sf.GTM$NAME_1 == "San Marcos"] <- 16 # San Marcos
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Baja Verapaz"] <- 17 # Baja Verapaz
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Petén"] <- 18 # Petén
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Zacapa"] <- 19 # Zacapa
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Chiquimula"] <- 20 # Chiquimula
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Jalapa"] <- 21 # Jalapa
sf.GTM$REG_ID[sf.GTM$NAME_1 == "Jutiapa"] <- 22 # Jutiapa
process_and_plot(sf.GTM, "GTM")
rm(sf.GTM)

## Honduras  ----
sf.HND <- subset(g, GID_0 == "HND")
g <- subset(g, GID_0 != "HND")
sf.HND$REG_ID[sf.HND$NAME_1 == "Atlántida"] <- 1 # Atlántida
sf.HND$REG_ID[sf.HND$NAME_1 == "Choluteca"] <- 2 # Choluteca
sf.HND$REG_ID[sf.HND$NAME_1 == "Colón"] <- 3 # Colón
sf.HND$REG_ID[sf.HND$NAME_1 == "Comayagua"] <- 4 # Comayagua
sf.HND$REG_ID[sf.HND$NAME_1 == "Copán"] <- 5 # Copán
sf.HND$REG_ID[sf.HND$NAME_1 == "Cortés"] <- 6 # Cortés
sf.HND$REG_ID[sf.HND$NAME_1 == "El Paraíso"] <- 7 # El Paraíso
sf.HND$REG_ID[sf.HND$NAME_1 == "Francisco Morazán"] <- 8 # Francisco Morazán
sf.HND$REG_ID[sf.HND$NAME_1 == "Gracias a Dios"] <- 9 # Gracias a Dios
sf.HND$REG_ID[sf.HND$NAME_1 == "Intibucá"] <- 10 # Intibucá
sf.HND$REG_ID[sf.HND$NAME_1 == "Islas de la Bahía"] <- 11 # Islas de la Bahía
sf.HND$REG_ID[sf.HND$NAME_1 == "La Paz"] <- 12 # La Paz
sf.HND$REG_ID[sf.HND$NAME_1 == "Lempira"] <- 13 # Lempira
sf.HND$REG_ID[sf.HND$NAME_1 == "Ocotepeque"] <- 14 # Ocotepeque
sf.HND$REG_ID[sf.HND$NAME_1 == "Olancho"] <- 15 # Olancho
sf.HND$REG_ID[sf.HND$NAME_1 == "Santa Bárbara"] <- 16 # Santa Bárbara
sf.HND$REG_ID[sf.HND$NAME_1 == "Valle"] <- 17 # Valle
sf.HND$REG_ID[sf.HND$NAME_1 == "Yoro"] <- 18 # Yoro
process_and_plot(sf.HND, "HND")
rm(sf.HND)

## Croatia  ----
sf.HRV <- subset(g, GID_0 == "HRV")
g <- subset(g, GID_0 != "HRV")
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Bjelovarska-Bilogorska"] <- 1 # Bjelovarska-Bilogorska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Brodsko-Posavska"] <- 2 # Brodsko-Posavska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Dubrovacko-Neretvanska"] <- 3 # Dubrovacko-Neretvanska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Istarska"] <- 4 # Istarska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Karlovacka"] <- 5 # Karlovacka
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Koprivničko-Križevačka"] <- 6 # Koprivničko-Križevačka
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Krapinsko-Zagorska"] <- 7 # Krapinsko-Zagorska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Licko-Senjska"] <- 8 # Licko-Senjska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Medimurska"] <- 9 # Medimurska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Osjecko-Baranjska"] <- 10 # Osjecko-Baranjska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Požeško-Slavonska"] <- 11 # Požeško-Slavonska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Primorsko-Goranska"] <- 12 # Primorsko-Goranska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Šibensko-Kninska"] <- 13 # Šibensko-Kninska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Sisacko-Moslavacka"] <- 14 # Sisacko-Moslavacka
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Splitsko-Dalmatinska"] <- 15 # Splitsko-Dalmatinska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Varaždinska"] <- 16 # Varaždinska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Viroviticko-Podravska"] <- 17 # Viroviticko-Podravska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Vukovarsko-Srijemska"] <- 18 # Vukovarsko-Srijemska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Zadarska"] <- 19 # Zadarska
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Zagrebačka"] <- 20 # Zagrebačka
sf.HRV$REG_ID[sf.HRV$NAME_1 == "Grad Zagreb"] <- 21 # Grad Zagreb
process_and_plot(sf.HRV, "HRV")
rm(sf.HRV)

## Hungary  ----
sf.HUN <- subset(g, GID_0 == "HUN")
g <- subset(g, GID_0 != "HUN")
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Budapest"] <- 1 # Budapest
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Bács-Kiskun"] <- 2 # Bács-Kiskun
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Baranya"] <- 3 # Baranya
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Békés"] <- 4 # Békés
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Borsod-Abaúj-Zemplén"] <- 5 # Borsod-Abaúj-Zemplén
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Csongrád"] <- 6 # Csongrád
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Fejér"] <- 7 # Fejér
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Gyor-Moson-Sopron"] <- 8 # Gyor-Moson-Sopron
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Hajdú-Bihar"] <- 9 # Hajdú-Bihar
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Heves"] <- 10 # Heves
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Jász-Nagykun-Szolnok"] <- 11 # Jász-Nagykun-Szolnok
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Komárom-Esztergom"] <- 12 # Komárom-Esztergom
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Nógrád"] <- 13 # Nógrád
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Pest"] <- 14 # Pest
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Somogy"] <- 15 # Somogy
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Szabolcs-Szatmár-Bereg"] <- 16 # Szabolcs-Szatmár-Bereg
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Tolna"] <- 17 # Tolna
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Vas"] <- 18 # Vas
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Veszprém"] <- 19 # Veszprém
sf.HUN$REG_ID[sf.HUN$NAME_1 == "Zala"] <- 20 # Zala
process_and_plot(sf.HUN, "HUN")
rm(sf.HUN)

## Indonesia  ----
sf.IDN <- subset(g, GID_0 == "IDN")
g <- subset(g, GID_0 != "IDN")
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Aceh"] <- 1 # Aceh
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Sumatera Utara"] <- 2 # Sumatera Utara
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Sumatera Barat"] <- 3 # Sumatera Barat
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Riau"] <- 4 # Riau
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Jambi"] <- 5 # Jambi
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Sumatera Selatan"] <- 6 # Sumatera Selatan
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Bengkulu"] <- 7 # Bengkulu
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Lampung"] <- 8 # Lampung
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Bangka Belitung"] <- 9 # Bangka Belitung
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Kepulauan Riau"] <- 10 # Kepulauan Riau
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Jakarta Raya"] <- 11 # Jakarta Raya
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Jawa Barat"] <- 12 # Jawa Barat
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Jawa Tengah"] <- 13 # Jawa Tengah
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Yogyakarta"] <- 14 # Yogyakarta
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Jawa Timur"] <- 15 # Jawa Timur
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Banten"] <- 16 # Banten
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Bali"] <- 17 # Bali
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Nusa Tenggara Barat"] <- 18 # Nusa Tenggara Barat
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Nusa Tenggara Timur"] <- 19 # Nusa Tenggara Timur
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Kalimantan Barat"] <- 20 # Kalimantan Barat
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Kalimantan Tengah"] <- 21 # Kalimantan Tengah
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Kalimantan Selatan"] <- 22 # Kalimantan Selatan
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Kalimantan Timur"] <- 23 # Kalimantan Timur
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Sulawesi Utara"] <- 24 # Sulawesi Utara
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Sulawesi Tengah"] <- 25 # Sulawesi Tengah
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Sulawesi Selatan"] <- 26 # Sulawesi Selatan
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Sulawesi Tenggara"] <- 27 # Sulawesi Tenggara
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Gorontalo"] <- 28 # Gorontalo
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Sulawesi Barat"] <- 29 # Sulawesi Barat
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Maluku"] <- 30 # Maluku
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Maluku Utara"] <- 31 # Maluku Utara
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Papua"] <- 32 # Papua
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Papua Barat"] <- 33 # Papua Barat
sf.IDN$REG_ID[sf.IDN$NAME_1 == "Kalimantan Utara"] <- 34 # Kalimantan Utara
process_and_plot(sf.IDN, "IDN")
rm(sf.IDN)

## India  ----
sf.IND <- subset(g, GID_0 == "IND")
g <- subset(g, GID_0 != "IND")
table(sf.IND$NAME_1)
sf.IND$REG_ID[sf.IND$NAME_1 == "Andhra Pradesh"] <- 1 # Andhra Pradesh
sf.IND$REG_ID[sf.IND$NAME_1 == "Arunachal Pradesh"] <- 2 # Arunachal Pradesh
sf.IND$REG_ID[sf.IND$NAME_1 == "Assam"] <- 3 # Assam
sf.IND$REG_ID[sf.IND$NAME_1 == "Bihar"] <- 4 # Bihar
sf.IND$REG_ID[sf.IND$NAME_1 == "Chandigarh"] <- 5 # Chandigarh
sf.IND$REG_ID[sf.IND$NAME_1 == "Chhattisgarh"] <- 6 # Chhattisgarh
sf.IND$REG_ID[sf.IND$NAME_1 == "NCT of Delhi"] <- 7 # NCT of Delhi
sf.IND$REG_ID[sf.IND$NAME_1 == "Goa"] <- 8 # Goa
sf.IND$REG_ID[sf.IND$NAME_1 == "Gujarat"] <- 9 # Gujarat
sf.IND$REG_ID[sf.IND$NAME_1 == "Haryana"] <- 10 # Haryana
sf.IND$REG_ID[sf.IND$NAME_1 == "Himachal Pradesh"] <- 11 # Himachal Pradesh
sf.IND$REG_ID[sf.IND$NAME_1 == "Jharkhand"] <- 13 # Jharkhand
sf.IND$REG_ID[sf.IND$NAME_1 == "Karnataka"] <- 14 # Karnataka
sf.IND$REG_ID[sf.IND$NAME_1 == "Kerala"] <- 15 # Kerala
sf.IND$REG_ID[sf.IND$NAME_1 == "Madhya Pradesh"] <- 16 # Madhya Pradesh
sf.IND$REG_ID[sf.IND$NAME_1 == "Maharashtra"] <- 17 # Maharashtra
sf.IND$REG_ID[sf.IND$NAME_1 == "Manipur"] <- 18 # Manipur
sf.IND$REG_ID[sf.IND$NAME_1 == "Meghalaya"] <- 19 # Meghalaya
sf.IND$REG_ID[sf.IND$NAME_1 == "Mizoram"] <- 20 # Mizoram
sf.IND$REG_ID[sf.IND$NAME_1 == "Nagaland"] <- 21 # Nagaland
sf.IND$REG_ID[sf.IND$NAME_1 == "Odisha"] <- 22 # Odisha
sf.IND$REG_ID[sf.IND$NAME_1 == "Puducherry"] <- 23 # Puducherry
sf.IND$REG_ID[sf.IND$NAME_1 == "Punjab"] <- 24 # Punjab
sf.IND$REG_ID[sf.IND$NAME_1 == "Rajasthan"] <- 25 # Rajasthan
sf.IND$REG_ID[sf.IND$NAME_1 == "Sikkim"] <- 26 # Sikkim
sf.IND$REG_ID[sf.IND$NAME_1 == "Tamil Nadu"] <- 27 # Tamil Nadu
sf.IND$REG_ID[sf.IND$NAME_1 == "Tripura"] <- 28 # Tripura
sf.IND$REG_ID[sf.IND$NAME_1 == "Uttar Pradesh"] <- 29 # Uttar Pradesh
sf.IND$REG_ID[sf.IND$NAME_1 == "Uttarakhand"] <- 30 # Uttarakhand
sf.IND$REG_ID[sf.IND$NAME_1 == "West Bengal"] <- 31 # West Bengal
sf.IND$REG_ID[sf.IND$NAME_1 == "Telangana"] <- 32 # Telangana
# sf.IND$REG_ID[sf.IND$NAME_1 == "Andaman and Nicobar"] <- 33 #Andaman and Nicobar
# Islands: https://en.wikipedia.org/wiki/Andaman_and_Nicobar_Islands
# sf.IND$REG_ID[sf.IND$NAME_1 == "Dadra and Nagar Haveli"] <- 34 #Dadra and Nagar Haveli
# Very small region
# https://en.wikipedia.org/wiki/Dadra_and_Nagar_Haveli
# sf.IND$REG_ID[sf.IND$NAME_1 == "Daman and Diu"] <- 35 #Daman and Diu
# Very small administrative unit
# https://en.wikipedia.org/wiki/Daman_and_Diu
# sf.IND$REG_ID[sf.IND$NAME_1 == "Lakshadweep"] <- 36 #Lakshadweep
# Islands: https://en.wikipedia.org/wiki/Lakshadweep
# Bibiane: not sure about 33-36, appear in sf.IND but not in the Gallup
# Alex: these are extremely small admin  units, which is why they're not in Gallup
process_and_plot(sf.IND, "IND")
rm(sf.IND)

## Ireland  ----
sf.IRL <- subset(g, GID_0 == "IRL")
g <- subset(g, GID_0 != "IRL")
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Carlow"] <- 1 # Carlow
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Cavan"] <- 2 # Cavan
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Clare"] <- 3 # Clare

sf.IRL$REG_ID[sf.IRL$NAME_1 == "Cork"] <- 4 # Cork
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Cork City"] <- 4 # Cork City

sf.IRL$REG_ID[sf.IRL$NAME_1 == "Donegal"] <- 5 # Donegal
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Dublin"] <- 6 # Dublin
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Galway"] <- 7 # Galway
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Kerry"] <- 8 # Kerry
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Kildare"] <- 9 # Kildare
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Kilkenny"] <- 10 # Kilkenny
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Laois"] <- 11 # Laois
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Leitrim"] <- 12 # Leitrim
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Limerick"] <- 13 # Limerick
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Longford"] <- 14 # Longford
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Louth"] <- 15 # Louth
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Mayo"] <- 16 # Mayo
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Meath"] <- 17 # Meath
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Monaghan"] <- 18 # Monaghan
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Offaly"] <- 19 # Offaly
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Roscommon"] <- 20 # Roscommon
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Sligo"] <- 21 # Sligo
sf.IRL$REG_ID[sf.IRL$NAME_2 %in% c("Nenagh", "Thurles")] <- 22 # North Tipperary
sf.IRL$REG_ID[sf.IRL$NAME_2 %in% c("Carrick-on-Suir", "Clonmel", "Tipperary-Cahir-Cashel")] <- 23 # North Tipperary
# https://en.wikipedia.org/wiki/South_Tipperary

sf.IRL$REG_ID[sf.IRL$NAME_1 == "Waterford"] <- 24 # Waterford
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Westmeath"] <- 25 # Westmeath
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Wexford"] <- 26 # Wexford
sf.IRL$REG_ID[sf.IRL$NAME_1 == "Wicklow"] <- 27 # Wicklow

process_and_plot(sf.IRL, "IRL")
rm(sf.IRL)

## Iran  ----
sf.IRN <- subset(g, GID_0 == "IRN")
g <- subset(g, GID_0 != "IRN")
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Alborz"] <- 1 # Alborz
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Ardebil"] <- 2 # Ardebil
sf.IRN$REG_ID[sf.IRN$NAME_1 == "East Azarbaijan"] <- 3 # East Azarbaijan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "West Azarbaijan"] <- 4 # West Azarbaijan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Bushehr"] <- 5 # Bushehr
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Chahar Mahall and Bakhtiari"] <- 6 # Chahar Mahall and Bakhtiari
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Fars"] <- 7 # Fars
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Gilan"] <- 8 # Gilan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Golestan"] <- 9 # Golestan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Hamadan"] <- 10 # Hamadan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Hormozgan"] <- 11 # Hormozgan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Ilam"] <- 12 # Ilam
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Esfahan"] <- 13 # Esfahan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Kerman"] <- 14 # Kerman
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Kermanshah"] <- 15 # Kermanshah
sf.IRN$REG_ID[sf.IRN$NAME_1 == "North Khorasan"] <- 16 # North Khorasan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Razavi Khorasan"] <- 17 # Razavi Khorasan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "South Khorasan"] <- 18 # South Khorasan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Khuzestan"] <- 19 # Khuzestan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Kohgiluyeh and Buyer Ahmad"] <- 20 # Kohgiluyeh and Buyer Ahmad
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Kordestan"] <- 21 # Kordestan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Lorestan"] <- 22 # Lorestan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Markazi"] <- 23 # Markazi
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Mazandaran"] <- 24 # Mazandaran
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Qazvin"] <- 25 # Qazvin
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Qom"] <- 26 # Qom
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Semnan"] <- 27 # Semnan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Sistan and Baluchestan"] <- 28 # Sistan and Baluchestan
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Tehran"] <- 29 # Tehran
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Yazd"] <- 30 # Yazd
sf.IRN$REG_ID[sf.IRN$NAME_1 == "Zanjan"] <- 31 # Zanjan
process_and_plot(sf.IRN, "IRN")
rm(sf.IRN)

# Iraq  ----
sf.IRQ <- subset(g, GID_0 == "IRQ")
g <- subset(g, GID_0 != "IRQ")
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Baghdad"] <- 1 # Baghdad
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Al-Basrah"] <- 2 # Al-Basrah
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "An-Najaf"] <- 3 # An-Najaf
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Al-Anbar"] <- 4 # Al-Anbar
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "At-Ta'mim"] <- 5 # At-Ta'mim
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "As-Sulaymaniyah"] <- 6 # As-Sulaymaniyah
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Arbil"] <- 7 # Arbil
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Diyala"] <- 8 # Diyala
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Ninawa"] <- 9 # Ninawa
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Dhi-Qar"] <- 10 # Dhi-Qar
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Babil"] <- 11 # Babil
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Maysan"] <- 12 # Maysan
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Wasit"] <- 13 # Wasit
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Sala ad-Din"] <- 14 # Sala ad-Din
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Al-Muthannia"] <- 15 # Al-Muthannia
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Al-Qadisiyah"] <- 16 # Al-Qadisiyah
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Karbala'"] <- 17 # Karbala'
sf.IRQ$REG_ID[sf.IRQ$NAME_1 == "Dihok"] <- 18 # Dihok
process_and_plot(sf.IRQ, "IRQ")
rm(sf.IRQ)

## Israel  ----
sf.ISR <- subset(g, GID_0 == "ISR")
g <- subset(g, GID_0 != "ISR")
sf.ISR$REG_ID[sf.ISR$NAME_1 == "Jerusalem"] <- 1 # Jerusalem
sf.ISR$REG_ID[sf.ISR$NAME_1 == "Golan"] <- 2 # Golan
sf.ISR$REG_ID[sf.ISR$NAME_1 == "Haifa"] <- 3 # Haifa
sf.ISR$REG_ID[sf.ISR$NAME_1 == "HaMerkaz"] <- 4 # HaMerkaz
sf.ISR$REG_ID[sf.ISR$NAME_1 == "Tel Aviv"] <- 5 # Tel Aviv
sf.ISR$REG_ID[sf.ISR$NAME_1 == "HaDarom"] <- 6 # HaDarom
process_and_plot(sf.ISR, "ISR")
rm(sf.ISR)

## Italy  ----
sf.ITA <- subset(g, GID_0 == "ITA")
g <- subset(g, GID_0 != "ITA")
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Abruzzo"] <- 1 # Abruzzo
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Valle d'Aosta"] <- 2 # Valle d'Aosta
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Apulia"] <- 3 # Apulia
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Basilicata"] <- 4 # Basilicata
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Calabria"] <- 5 # Calabria
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Campania"] <- 6 # Campania
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Emilia-Romagna"] <- 7 # Emilia-Romagna
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Friuli-Venezia Giulia"] <- 8 # Friuli-Venezia Giulia
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Lazio"] <- 9 # Lazio
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Liguria"] <- 10 # Liguria
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Lombardia"] <- 11 # Lombardia
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Marche"] <- 12 # Marche
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Molise"] <- 13 # Molise
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Piemonte"] <- 14 # Piemonte
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Sardegna"] <- 15 # Sardegna
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Sicily"] <- 16 # Sicily
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Trentino-Alto Adige"] <- 17 # Trentino-Alto Adige
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Toscana"] <- 18 # Toscana
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Umbria"] <- 19 # Umbria
sf.ITA$REG_ID[sf.ITA$NAME_1 == "Veneto"] <- 20 # Veneto
process_and_plot(sf.ITA, "ITA")
rm(sf.ITA)

## Jamaica  ----
sf.JAM <- subset(g, GID_0 == "JAM")
g <- subset(g, GID_0 != "JAM")
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Kingston"] <- 1 # Kingston
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Saint Andrew"] <- 2 # Saint Andrew
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Saint Thomas"] <- 3 # Saint Thomas
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Portland"] <- 4 # Portland
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Saint Mary"] <- 5 # Saint Mary
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Saint Ann"] <- 6 # Saint Ann
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Trelawny"] <- 7 # Trelawny
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Saint James"] <- 8 # Saint James
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Hanover"] <- 9 # Hanover
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Westmoreland"] <- 10 # Westmoreland
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Saint Elizabeth"] <- 11 # Saint Elizabeth
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Manchester"] <- 12 # Manchester
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Clarendon"] <- 13 # Clarendon
sf.JAM$REG_ID[sf.JAM$NAME_1 == "Saint Catherine"] <- 14 # Saint Catherine
process_and_plot(sf.JAM, "JAM")
rm(sf.JAM)

## Jordan  ----
sf.JOR <- subset(g, GID_0 == "JOR")
g <- subset(g, GID_0 != "JOR")
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Amman"] <- 1 # Amman
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Zarqa"] <- 2 # Zarqa
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Balqa"] <- 3 # Balqa
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Madaba"] <- 4 # Madaba
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Irbid"] <- 5 # Irbid
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Mafraq"] <- 6 # Mafraq
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Jarash"] <- 7 # Jarash
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Ajlun"] <- 8 # Ajlun
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Karak"] <- 9 # Karak
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Tafilah"] <- 10 # Tafilah
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Ma`an"] <- 11 # Ma`an
sf.JOR$REG_ID[sf.JOR$NAME_1 == "Aqaba"] <- 12 # Aqaba
process_and_plot(sf.JOR, "JOR")
rm(sf.JOR)

## Japan   ----
sf.JPN <- subset(g, GID_0 == "JPN")
g <- subset(g, GID_0 != "JPN")
table(sf.JPN$NAME_1)

sf.JPN$REG_ID[sf.JPN$NAME_1 == "Hokkaido"] <- 1 # Hokkaido

# Tohoku
# Akita, Aomori, Fukushima, Iwate, Miyagi, and Yamagata.
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Akita"] <- 2
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Aomori"] <- 2
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Fukushima"] <- 2
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Iwate"] <- 2
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Miyagi"] <- 2
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Yamagata"] <- 2

# Kanto
# Gunma, Tochigi, Ibaraki, Saitama, Tokyo, Chiba, and Kanagawa
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Gunma"] <- 3
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Tochigi"] <- 3
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Ibaraki"] <- 3
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Saitama"] <- 3
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Tokyo"] <- 3
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Chiba"] <- 3
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Kanagawa"] <- 3

# Hokuriku
# Ishikawa, Fukui, Niigata and Toyama
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Ishikawa"] <- 4
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Fukui"] <- 4
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Niigata"] <- 4
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Toyama"] <- 4

# Koshinetsu
# Yamanashi, Nagano, and Niigata
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Yamanashi"] <- 5
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Nagano"] <- 5
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Niigata"] <- 5


# https://en.wikipedia.org/wiki/Chūbu_region
# https://en.wikipedia.org/wiki/Chūkyō_metropolitan_area
# The three Tōkai prefectures centered on Nagoya (Aichi, Gifu, and Mie) have particularly strong economic ties, and the parts of these prefectures that are closest to the city comprise the Chūkyō Metropolitan Area
aichi_chukyo <- c(
  "Aisai", "Chita", "Handa", "Ichinomiya/Owari-ichinomiya", "Inazawa", "Inuyama", "Iwakura", "Kasugai", "Komaki", "Kitanagoya", "Kiyosu", "Kōnan", "Nagakute", "Nisshin", "Ōbu", "Owariasahi", "Seto", "Tōkai", "Tokoname", "Toyoake", "Tsushima", "Yatomi",
  "Anjō", "Chiryū", "Gamagōri", "Hekinan", "Kariya", "Miyoshi", "Nishio", "Okazaki", "Tahara", "Takahama", "Toyohashi", "Toyokawa", "Toyota"
)
gifu_chukyo <- c("Ena", "Gifu", "Hashima", "Kaizu", "Kakamigahara", "Kani", "Minokamo", "Mizuho", "Mizunami", "Motosu", "Nakatsugawa", "Ōgaki", "Tajimi", "Toki")
mie_chukyo <- c("Inabe", "Kuwana", "Suzuka", "Yokkaichi")

# Tokai
# Shizuoka, Aichi, Gifu and Mie
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Shizuoka"] <- 6
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Aichi" & !(sf.JPN$NAME_2 %in% aichi_chukyo)] <- 6
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Gifu" & !(sf.JPN$NAME_2 %in% gifu_chukyo)] <- 6
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Mie" & !(sf.JPN$NAME_2 %in% mie_chukyo)] <- 6

# Chukyo
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Aichi" & sf.JPN$NAME_2 %in% aichi_chukyo] <- 7
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Gifu" & sf.JPN$NAME_2 %in% gifu_chukyo] <- 7
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Mie" & sf.JPN$NAME_2 %in% mie_chukyo] <- 7

# Kansai
# Nara, Wakayama, Kyoto, Osaka, Hyōgo and Shiga
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Nara"] <- 8
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Wakayama"] <- 8
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Kyoto"] <- 8
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Osaka"] <- 8
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Hyōgo"] <- 8
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Shiga"] <- 8

# Chugoku
# Hiroshima, Okayama, Shimane, Tottori and Yamaguchi
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Hiroshima"] <- 9
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Okayama"] <- 9
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Shimane"] <- 9
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Tottori"] <- 9
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Yamaguchi"] <- 9

# Shikoku
# Ehime, Kagawa, Kōchi, and Tokushima.
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Ehime"] <- 10
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Kagawa"] <- 10
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Kochi"] <- 10
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Tokushima"] <- 10

# Kyushu
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Fukuoka"] <- 11
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Kumamoto"] <- 11
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Nagasaki"] <- 11
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Oita"] <- 11
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Saga"] <- 11
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Kagoshima"] <- 11
sf.JPN$REG_ID[sf.JPN$NAME_1 == "Miyazaki"] <- 11


process_and_plot(sf.JPN, "JPN")
rm(sf.JPN)


## Kazakhstan  ----
sf.KAZ <- subset(g, GID_0 == "KAZ")
g <- subset(g, GID_0 != "KAZ")

sf.KAZ$REG_ID[sf.KAZ$NAME_2 == "Almaty (Alma-Ata)"] <- 1 # Almaty

# https://en.wikipedia.org/wiki/Astana
# previously known as Akmolinsk, Tselinograd, Akmola, and most recently Nur-Sultan
# https://en.wikipedia.org/wiki/Tselinograd_District
sf.KAZ$REG_ID[sf.KAZ$NAME_2 == "Tselinogradskiy"] <- 2 # Astana

sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "Qostanay"] <- 3 # Qostanay
sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "North Kazakhstan"] <- 4 # North Kazakhstan
sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "Pavlodar"] <- 5 # Pavlodar

sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "Aqmola" & sf.KAZ$NAME_2 != "Tselinogradskiy"] <- 6 # Aqmola

sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "Qaraghandy"] <- 7 # Qaraghandy
sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "East Kazakhstan"] <- 8 # East Kazakhstan

sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "Almaty" & sf.KAZ$NAME_2 != "Almaty (Alma-Ata)"] <- 9 # East Kazakhstan

sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "Zhambyl"] <- 10 # Zhambyl
sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "Qyzylorda"] <- 11 # Qyzylorda

# https://en.wikipedia.org/wiki/Regions_of_Kazakhstan
sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "South Kazakhstan" & sf.KAZ$NAME_2 != "Shymkent"] <- 12 # Turkistan Â oblast

sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "Aqtöbe"] <- 13 # Aqtöbe
sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "Atyrau"] <- 14 # Atyrau
sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "West Kazakhstan"] <- 15 # West Kazakhstan
sf.KAZ$REG_ID[sf.KAZ$NAME_1 == "Mangghystau"] <- 16 # Mangghystau

sf.KAZ$REG_ID[sf.KAZ$NAME_2 == "Shymkent"] <- 17 # Shymkent

process_and_plot(sf.KAZ, "KAZ")
rm(sf.KAZ)


## Kenya  ----

sf.KEN <- subset(g, GID_0 == "KEN")
g <- subset(g, GID_0 != "KEN")
table(sf.KEN$NAME_1)
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Baringo"] <- 1 # Baringo
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Bomet"] <- 2 # Bomet
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Bungoma"] <- 3 # Bungoma
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Busia"] <- 4 # Busia
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Elgeyo-Marakwet"] <- 5 # Elgeyo-Marakwet
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Embu"] <- 6 # Embu
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Garissa"] <- 7 # Garissa
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Homa Bay"] <- 8 # Homa Bay
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Isiolo"] <- 9 # Isiolo
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Kajiado"] <- 10 # Kajiado
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Kakamega"] <- 11 # Kakamega
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Kericho"] <- 12 # Kericho
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Kiambu"] <- 13 # Kiambu
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Kilifi"] <- 14 # Kilifi
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Kirinyaga"] <- 15 # Kirinyaga
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Kisii"] <- 16 # Kisii
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Kisumu"] <- 17 # Kisumu
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Kitui"] <- 18 # Kitui
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Kwale"] <- 19 # Kwale
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Laikipia"] <- 20 # Laikipia
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Lamu"] <- 21 # Lamu
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Machakos"] <- 22 # Machakos
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Makueni"] <- 23 # Makueni
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Mandera"] <- 24 # Mandera
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Marsabit"] <- 25 # Marsabit
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Meru"] <- 26 # Meru
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Migori"] <- 27 # Migori
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Mombasa"] <- 28 # Mombasa
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Murang'a"] <- 29 # Murang'a
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Nairobi"] <- 30 # Nairobi
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Nakuru"] <- 31 # Nakuru
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Nandi"] <- 32 # Nandi
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Narok"] <- 33 # Narok
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Nyamira"] <- 34 # Nyamira
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Nyandarua"] <- 35 # Nyandarua
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Nyeri"] <- 36 # Nyeri
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Samburu"] <- 37 # Samburu
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Siaya"] <- 38 # Siaya
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Taita Taveta"] <- 39 # Taita Taveta
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Tana River"] <- 40 # Tana River
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Tharaka-Nithi"] <- 41 # Tharaka-Nithi
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Trans Nzoia"] <- 42 # Trans Nzoia
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Turkana"] <- 43 # Turkana
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Uasin Gishu"] <- 44 # Uasin Gishu
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Vihiga"] <- 45 # Vihiga
sf.KEN$REG_ID[sf.KEN$NAME_1 == "Wajir"] <- 46 # Wajir
sf.KEN$REG_ID[sf.KEN$NAME_1 == "West Pokot"] <- 47 # West Pokot

# 24, 25, 46, 37, 9
process_and_plot(sf.KEN, "KEN")
rm(sf.KEN)

## Kyrgyzstan  ----
sf.KGZ <- subset(g, GID_0 == "KGZ")
g <- subset(g, GID_0 != "KGZ")
sf.KGZ$REG_ID[sf.KGZ$NAME_1 == "Biškek"] <- 1 # Biškek
sf.KGZ$REG_ID[sf.KGZ$NAME_1 == "Chüy"] <- 2 # Chüy
sf.KGZ$REG_ID[sf.KGZ$NAME_1 == "Talas"] <- 3 # Talas
sf.KGZ$REG_ID[sf.KGZ$NAME_1 == "Naryn"] <- 4 # Naryn
sf.KGZ$REG_ID[sf.KGZ$NAME_1 == "Ysyk-Köl"] <- 5 # Ysyk-Köl
sf.KGZ$REG_ID[sf.KGZ$NAME_1 == "Osh"] <- 6 # Osh
sf.KGZ$REG_ID[sf.KGZ$NAME_1 == "Osh (city)"] <- 7 # Osh (city)
sf.KGZ$REG_ID[sf.KGZ$NAME_1 == "Jalal-Abad"] <- 8 # Jalal-Abad
sf.KGZ$REG_ID[sf.KGZ$NAME_1 == "Batken"] <- 9 # Batken
process_and_plot(sf.KGZ, "KGZ")
rm(sf.KGZ)

## Cambodia  ----
sf.KHM <- subset(g, GID_0 == "KHM")
g <- subset(g, GID_0 != "KHM")
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Bântéay Méanchey"] <- 1 # Bântéay Méanchey
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Batdâmbâng"] <- 2 # Batdâmbâng
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Kâmpóng Cham"] <- 3 # Kâmpóng Cham
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Kâmpóng Chhnang"] <- 4 # Kâmpóng Chhnang
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Kâmpóng Spœ"] <- 5 # Kâmpóng Spœ
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Kâmpóng Thum"] <- 6 # Kâmpóng Thum
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Kâmpôt"] <- 7 # Kâmpôt
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Kândal"] <- 8 # Kândal
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Kaôh Kong"] <- 9 # Kaôh Kong
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Kep"] <- 10 # Kep
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Krâchéh"] <- 11 # Krâchéh
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Môndól Kiri"] <- 12 # Môndól Kiri
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Otdar Mean Chey"] <- 13 # Otdar Mean Chey
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Krong Pailin"] <- 14 # Krong Pailin
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Phnom Penh"] <- 15 # Phnom Penh
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Krong Preah Sihanouk"] <- 16 # Krong Preah Sihanouk
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Preah Vihéar"] <- 17 # Preah Vihéar
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Pouthisat"] <- 18 # Pouthisat
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Prey Vêng"] <- 19 # Prey Vêng
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Rôtânôkiri"] <- 20 # Rôtânôkiri
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Siemréab"] <- 21 # Siemréab
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Stœng Trêng"] <- 22 # Stœng Trêng
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Svay Rieng"] <- 23 # Svay Rieng
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Takêv"] <- 24 # Takêv
sf.KHM$REG_ID[sf.KHM$NAME_1 == "Tbong Khmum"] <- 25 # Tbong Khmum
process_and_plot(sf.KHM, "KHM")
rm(sf.KHM)


## South Korea  ----
sf.KOR <- subset(g, GID_0 == "KOR")
g <- subset(g, GID_0 != "KOR")
table(sf.KOR$NAME_1)
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Seoul"] <- 1 # Seoul
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Busan"] <- 2 # Busan
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Daegu"] <- 3 # Daegu
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Incheon"] <- 4 # Incheon
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Gwangju"] <- 5 # Gwangju
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Daejeon"] <- 6 # Daejeon
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Ulsan"] <- 7 # Ulsan
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Gyeonggi-do"] <- 8 # Gyeonggi-do
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Gangwon-do"] <- 9 # Gangwon-do
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Chungcheongbuk-do"] <- 10 # Chungcheongbuk-do
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Chungcheongnam-do"] <- 11 # Chungcheongnam-do
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Jeollabuk-do"] <- 12 # Jeollabuk-do
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Jeollanam-do"] <- 13 # Jeollanam-do
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Gyeongsangbuk-do"] <- 14 # Gyeongsangbuk-do
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Gyeongsangnam-do"] <- 15 # Gyeongsangnam-do
sf.KOR$REG_ID[sf.KOR$NAME_1 == "Jeju"] <- 16 # Jeju

# sf.KOR$REG_ID[sf.KOR$NAME_1 == "Sejong"] <- 17 #Sejong
# Bibiane: *not sure about 17, sejong is not in the Gallup but is in the sf.KOR
# Alex: it's next to Chungcheong South/North,but not in Gallup so no need to assign a code
# https://en.wikipedia.org/wiki/Sejong_City
process_and_plot(sf.KOR, "KOR")
rm(sf.KOR)

## Laos  ----
sf.LAO <- subset(g, GID_0 == "LAO")
g <- subset(g, GID_0 != "LAO")
table(sf.LAO$NAME_1)
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Attapu"] <- 1 # Attapu
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Bokeo"] <- 2 # Bokeo
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Bolikhamxai"] <- 3 # Bolikhamxai
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Champasak"] <- 4 # Champasak
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Houaphan"] <- 5 # Houaphan
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Khammouan"] <- 6 # Khammouan
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Louang Namtha"] <- 7 # Louang Namtha
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Louangphrabang"] <- 8 # Louangphrabang
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Oudômxai"] <- 9 # Oudômxai
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Phôngsali"] <- 10 # Phôngsali
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Saravan"] <- 11 # Saravan
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Savannakhét"] <- 12 # Savannakhét
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Vientiane [prefecture]"] <- 13 # Vientiane [prefecture]
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Vientiane"] <- 14 # Vientiane
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Xaignabouri"] <- 15 # Xaignabouri
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Xaisômboun"] <- 16 # Xaisômboun
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Xékong"] <- 17 # Xékong
sf.LAO$REG_ID[sf.LAO$NAME_1 == "Xiangkhoang"] <- 18 # Xiangkhoang
process_and_plot(sf.LAO, "LAO")
rm(sf.LAO)


## Lebanon  ----

sf.LBN <- subset(g, GID_0 == "LBN")
g <- subset(g, GID_0 != "LBN")

# https://www.lebanesearabicinstitute.com/areas-beirut/#The_Green_Line_East_Beirut_West_Beirut_and_Ras_Beirut
# Achrafieh, officially a sector (63) as well as a quarter subdivided into ten sectors, is also a
# common designation of the neighbourhoods of the former East Beirut as a whole, i.e. the predominantly Christian neighbourhoods belonging to the Achrafieh, Medawar, Rmeil, and Saifi quarters.
# One area which largely escaped the war-time polarization and has managed to retain a relatively pluralistic character is Ras Beirut. Although it is located on the western side of the city, Ras Beirut is often considered a sort of third Beirut, neither “East” nor “West.”

# Beirut. East Center
# Beirut. West Center
beirut_east <- c("Achrafieh", "Medawar", "Remeil", "Saifé")
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Beirut" & sf.LBN$NAME_3 %in% beirut_east] <- 1 # East Beirut
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Beirut" & !(sf.LBN$NAME_3 %in% beirut_east)] <- 3 # West Beirut

sf.LBN$REG_ID[sf.LBN$NAME_2 == "Tripoli"] <- 5 # Tripoli
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Koura"] <- 6 # Koura
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Zgharta"] <- 7 # Zgharta
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Batroun"] <- 8 # Batroun
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Akkar"] <- 9 # Akkar
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Saida"] <- 10 # Saida
# Bibiane: *not sure about 31, Sour is not in the Gallup but is in the sf.LBN
# Alex: Sour is the Arabic name for Tyre https://www.hotelibanais.com/travel/tyre-south-lebanon/#:~:text=Tyre%3A%20The%20Queen%20of%20the%20Seas&text=One%20of%20the%20most%20important,in%20the%20country%27s%20Southern%20Province.
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Sour"] <- 11 # Tyre
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Nabatiyeh"] <- 12 # Nabatiyeh
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Zahleh"] <- 13 # Zahleh
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Baalbeck"] <- 14 # Baalbeck
sf.LBN$REG_ID[sf.LBN$NAME_2 == "West Bekaa"] <- 15 # West Bekaa
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Jubail"] <- 17 # Jubail
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Chouf"] <- 18 # Chouf
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Aley"] <- 19 # Aley
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Jezzine"] <- 20 # Jezzine
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Bint Jbayl"] <- 21 # Bint Jbayl
sf.LBN$REG_ID[sf.LBN$NAME_2 == "El Metn"] <- 22 # El Metn
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Bcharre"] <- 23 # Bcharre
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Marjaayoun"] <- 24 # Marjaayoun
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Hasbaya"] <- 25 # Hasbaya
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Rachiaya"] <- 26 # Rachiaya
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Hermel"] <- 27 # Hermel
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Kasrouane"] <- 28 # Kasrouane
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Minieh-Danieh"] <- 29 # Minieh-Danieh
sf.LBN$REG_ID[sf.LBN$NAME_2 == "Baabda"] <- 30 # Baabda
process_and_plot(sf.LBN, "LBN")
rm(sf.LBN)


## Liberia  ----
sf.LBR <- subset(g, GID_0 == "LBR")
g <- subset(g, GID_0 != "LBR")
table(sf.LBR$NAME_1)
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Bomi"] <- 1 # Bomi
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Bong"] <- 2 # Bong
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Gbapolu"] <- 3 # Gbapolu
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Grand Bassa"] <- 4 # Grand Bassa
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Grand Cape Mount"] <- 5 # Grand Cape Mount
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Grand Gedeh"] <- 6 # Grand Gedeh
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Grand Kru"] <- 7 # Grand Kru
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Lofa"] <- 8 # Lofa
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Margibi"] <- 9 # Margibi
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Maryland"] <- 10 # Maryland
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Montserrado"] <- 11 # Montserrado
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Nimba"] <- 12 # Nimba
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Rivercess"] <- 13 # Rivercess
sf.LBR$REG_ID[sf.LBR$NAME_1 == "River Gee"] <- 14 # River Gee
sf.LBR$REG_ID[sf.LBR$NAME_1 == "Sinoe"] <- 15 # Sinoe
process_and_plot(sf.LBR, "LBR")
rm(sf.LBR)


## Sri Lanka  ----
sf.LKA <- subset(g, GID_0 == "LKA")
g <- subset(g, GID_0 != "LKA")
table(sf.LKA$NAME_1)
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Colombo"] <- 1 # Western
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Gampaha"] <- 1 # Western
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Kalutara"] <- 1 # Western

sf.LKA$REG_ID[sf.LKA$NAME_1 == "Kandy"] <- 2 # Central
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Matale"] <- 2 # Central
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Nuwara Eliya"] <- 2 # Central

sf.LKA$REG_ID[sf.LKA$NAME_1 == "Galle"] <- 3 # Southern
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Hambantota"] <- 3 # Southern
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Matara"] <- 3 # Southern

sf.LKA$REG_ID[sf.LKA$NAME_1 == "Jaffna"] <- 4 # Northern
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Kilinochchi"] <- 4 # Northern
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Mannar"] <- 4 # Northern
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Mullaitivu"] <- 4 # Northern
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Vavuniya"] <- 4 # Northern

sf.LKA$REG_ID[sf.LKA$NAME_1 == "Ampara"] <- 5 # Eastern
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Batticaloa"] <- 5 # Eastern
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Trincomalee"] <- 5 # Eastern

sf.LKA$REG_ID[sf.LKA$NAME_1 == "Kurunegala"] <- 6 # Northwest
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Puttalam"] <- 6 # Northwest

sf.LKA$REG_ID[sf.LKA$NAME_1 == "Anuradhapura"] <- 7 # North Central
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Polonnaruwa"] <- 7 # North Central

sf.LKA$REG_ID[sf.LKA$NAME_1 == "Badulla"] <- 8 # Uva
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Moneragala"] <- 8 # Uva

sf.LKA$REG_ID[sf.LKA$NAME_1 == "Kegalle"] <- 9 # Sabaragamuwa
sf.LKA$REG_ID[sf.LKA$NAME_1 == "Ratnapura"] <- 9 # Sabaragamuwa

process_and_plot(sf.LKA, "LKA")
rm(sf.LKA)


## Lesotho  ----
sf.LSO <- subset(g, GID_0 == "LSO")
g <- subset(g, GID_0 != "LSO")
table(sf.LSO$NAME_1)
sf.LSO$REG_ID[sf.LSO$NAME_1 == "Berea"] <- 1 # Berea
sf.LSO$REG_ID[sf.LSO$NAME_1 == "Butha-Buthe"] <- 2 # Butha-Buthe
sf.LSO$REG_ID[sf.LSO$NAME_1 == "Leribe"] <- 3 # Leribe
sf.LSO$REG_ID[sf.LSO$NAME_1 == "Mafeteng"] <- 4 # Mafeteng
sf.LSO$REG_ID[sf.LSO$NAME_1 == "Maseru"] <- 5 # Maseru
sf.LSO$REG_ID[sf.LSO$NAME_1 == "Mohale's Hoek"] <- 6 # Mohale's Hoek
sf.LSO$REG_ID[sf.LSO$NAME_1 == "Mokhotlong"] <- 7 # Mokhotlong
sf.LSO$REG_ID[sf.LSO$NAME_1 == "Qacha's Nek"] <- 8 # Qacha's Nek
sf.LSO$REG_ID[sf.LSO$NAME_1 == "Quthing"] <- 9 # Quthing
sf.LSO$REG_ID[sf.LSO$NAME_1 == "Thaba-Tseka"] <- 10 # Thaba-Tseka
process_and_plot(sf.LSO, "LSO")
rm(sf.LSO)

## Lithuania  ----
sf.LTU <- subset(g, GID_0 == "LTU")
g <- subset(g, GID_0 != "LTU")
table(sf.LTU$NAME_1)
sf.LTU$REG_ID[sf.LTU$NAME_1 == "Alytaus"] <- 1 # Alytaus
sf.LTU$REG_ID[sf.LTU$NAME_1 == "Kauno"] <- 2 # Kauno
sf.LTU$REG_ID[sf.LTU$NAME_1 == "Klaipedos"] <- 3 # Klaipedos
sf.LTU$REG_ID[sf.LTU$NAME_1 == "Marijampoles"] <- 4 # Marijampoles
sf.LTU$REG_ID[sf.LTU$NAME_1 == "Panevezio"] <- 5 # Panevezio
sf.LTU$REG_ID[sf.LTU$NAME_1 == "Šiauliai"] <- 6 # Šiauliai
sf.LTU$REG_ID[sf.LTU$NAME_1 == "Taurages"] <- 7 # Taurages
sf.LTU$REG_ID[sf.LTU$NAME_1 == "Telšiai"] <- 8 # Telšiai
sf.LTU$REG_ID[sf.LTU$NAME_1 == "Utenos"] <- 9 # Utenos
sf.LTU$REG_ID[sf.LTU$NAME_1 == "Vilniaus"] <- 10 # Vilniaus
process_and_plot(sf.LTU, "LTU")
rm(sf.LTU)

## Latvia  ----
sf.LVA <- subset(g, GID_0 == "LVA")
g <- subset(g, GID_0 != "LVA")
# https://en.wikipedia.org/wiki/Statistical_regions_of_Latvia
sf.LVA$REG_ID[sf.LVA$NAME_2 == "Riga"] <- 1 # Riga
sf.LVA$REG_ID[sf.LVA$NAME_1 == "Vidzeme"] <- 2 # Vidzeme
sf.LVA$REG_ID[sf.LVA$NAME_1 == "Kurzeme"] <- 3 # Kurzeme
sf.LVA$REG_ID[sf.LVA$NAME_1 == "Zemgale"] <- 4 # Zemgale
sf.LVA$REG_ID[sf.LVA$NAME_1 == "Latgale"] <- 5 # Latgale
sf.LVA$REG_ID[sf.LVA$NAME_1 == "Riga" & sf.LVA$NAME_2 != "Riga"] <- 6 # Pieriga
process_and_plot(sf.LVA, "LVA")
rm(sf.LVA)

## Morocco ----
sf.MAR <- subset(g, GID_0 == "MAR")
g <- subset(g, GID_0 != "MAR")
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Laâyoune"] <- 4 # Laâyoune
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Assa-Zag"] <- 5 # Assa-Zag
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Essaouira"] <- 6 # Essaouira
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Guelmim"] <- 7 # Guelmim
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Tan-Tan"] <- 8 # Tan-Tan
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Tata"] <- 9 # Tata
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Agadir-Ida ou Tanane"] <- 10 # Agadir-Ida ou Tanane
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Chtouka-Aït Baha"] <- 11 # Chtouka-Aït Baha
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Inezgane-Aït Melloul"] <- 12 # Inezgane-Aït Melloul
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Ouarzazate" & sf.MAR$NAME_3 != "NA (Tinghir)"] <- 13 # Ouarzazate
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Taroudannt"] <- 14 # Taroudannt
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Tiznit" & sf.MAR$NAME_3 != "NA (Sidi Ifni)"] <- 15 # Tiznit
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Zagora"] <- 16 # Zagora
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Kénitra"] <- 17 # Kénitra
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Sidi Kacem" & sf.MAR$NAME_3 != "Ouezzane"] <- 18 # Sidi Kacem
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Ben Slimane"] <- 19 # Ben Slimane
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Khouribga"] <- 20 # Khouribga
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Settat" & sf.MAR$NAME_3 != "NA (Berrechid)"] <- 21 # Settat
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Al Haouz"] <- 22 # Al Haouz
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Chichaoua"] <- 23 # Chichaoua
sf.MAR$REG_ID[sf.MAR$NAME_2 == "El Kelaâ des Sraghna"] <- 24 # El Kelaâ des Sraghna
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Essaouira"] <- 25 # Essaouira
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Marrakech"] <- 26 # Marrakech

sf.MAR$REG_ID[sf.MAR$NAME_2 == "Berkane Taourirt" & sf.MAR$NAME_3 %in% c("Ahfir", "Aklim", "NA (Ahfir)", "NA (Ain Erreggada)", "NA (Aklim)", "NA (Berkane)", "NA (Saidia)", "NA (Sidi Slimane Echcherraa)")] <- 27 # Berkane

sf.MAR$REG_ID[sf.MAR$NAME_2 == "Figuig"] <- 28 # Figuig
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Jerada"] <- 29 # Jerada
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Nador" & sf.MAR$NAME_3 != "Driouch"] <- 30 # Nador
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Oujda Angad"] <- 31 # Oujda Angad

sf.MAR$REG_ID[sf.MAR$NAME_2 == "Berkane Taourirt" & sf.MAR$NAME_3 %in% c("El Aioun", "NA (El Aioun Sidi Mellouk)", "NA (Taourirt)", "Taourirt")] <- 32 # Berkane

sf.MAR$REG_ID[sf.MAR$NAME_2 == "Casablanca" & !(sf.MAR$NAME_3 %in% c("NA (Mediouna)", "NA (Nouaceur)"))] <- 33 # Casablanca
sf.MAR$REG_ID[sf.MAR$NAME_3 == "NA (Mediouna)"] <- 34 # MEDIOUNA
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Mohammedia"] <- 35 # Mohammedia
sf.MAR$REG_ID[sf.MAR$NAME_3 == "NA (Nouaceur)"] <- 36 # NOUACEUR
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Khémisset"] <- 37 # Khémisset
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Rabat"] <- 38 # Rabat
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Salé"] <- 39 # Salé
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Skhirate-Témara"] <- 40 # Skhirate-Témara
sf.MAR$REG_ID[sf.MAR$NAME_2 == "El Jadida" & sf.MAR$NAME_3 != "NA (Sidi Bennour)"] <- 41 # El Jadida
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Safi" & sf.MAR$NAME_3 != "NA (Youssoufia)"] <- 42 # Safi
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Azilal"] <- 43 # Azilal
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Béni Mellal" & sf.MAR$NAME_3 != "NA (Fquih Ben Salah)"] <- 44 # Béni Mellal
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Meknès"] <- 45 # Meknès
sf.MAR$REG_ID[sf.MAR$NAME_2 == "El Hajeb"] <- 46 # El Hajeb
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Errachidia"] <- 47 # Errachidia
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Ifrane"] <- 48 # Ifrane
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Khénifra"] <- 49 # Khénifra
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Boulemane"] <- 50 # Boulemane
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Fès"] <- 51 # Fès
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Sefrou"] <- 52 # Sefrou
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Zouagha-Moulay Yacoub"] <- 53 # Zouagha-Moulay Yacoub
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Al Hoceïma"] <- 54 # Al Hoceïma
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Taounate"] <- 55 # Taounate
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Taza" & sf.MAR$NAME_3 != "Guercif"] <- 56 # Taza
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Chefchaouen"] <- 57 # Chefchaouen
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Fahs Anjra"] <- 58 # Fahs Anjra
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Larache"] <- 59 # Larache
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Tanger-Assilah"] <- 60 # Tanger-Assilah
sf.MAR$REG_ID[sf.MAR$NAME_2 == "Tétouan" & sf.MAR$NAME_3 != "NA (M'Diq)"] <- 61 # Tétouan
sf.MAR$REG_ID[sf.MAR$NAME_3 == "NA (Tinghir)"] <- 62 # TINGHIR
sf.MAR$REG_ID[sf.MAR$NAME_3 == "NA (Berrechid)"] <- 63 # BERRECHID
sf.MAR$REG_ID[sf.MAR$NAME_3 == "NA (Sidi Bennour)"] <- 64 # SIDI BENNOUR
sf.MAR$REG_ID[sf.MAR$NAME_3 == "NA (Youssoufia)"] <- 65 # YOUSSOUFIA
sf.MAR$REG_ID[sf.MAR$NAME_3 == "Driouch"] <- 66 # DRIOUCH
sf.MAR$REG_ID[sf.MAR$NAME_3 == "NA (Fquih Ben Salah)"] <- 67 # FQUIH BEN SALAH
sf.MAR$REG_ID[sf.MAR$NAME_3 == "Ouezzane"] <- 68 # OUEZZANE
sf.MAR$REG_ID[sf.MAR$NAME_3 == "NA (Sidi Ifni)"] <- 69 # SIDI IFNI
sf.MAR$REG_ID[sf.MAR$NAME_3 == "NA (M'Diq)"] <- 72 # M DIQ-FNIDEQ
sf.MAR$REG_ID[sf.MAR$NAME_3 == "Guercif"] <- 75 # GUERCIF
process_and_plot(sf.MAR, "MAR")
rm(sf.MAR)


## Moldova  ----
sf.MDA <- subset(g, GID_0 == "MDA")
g <- subset(g, GID_0 != "MDA")
table(sf.MDA$NAME_1)
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Chişinău"] <- 1 # Chişinău
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Bălţi"] <- 2 # Bălţi
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Anenii Noi"] <- 3 # Anenii Noi
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Basarabeasca"] <- 4 # Basarabeasca
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Briceni"] <- 5 # Briceni
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Cahul"] <- 6 # Cahul
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Cantemir"] <- 7 # Cantemir
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Calarasi"] <- 8 # Calarasi
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Causeni"] <- 9 # Causeni
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Cimişlia"] <- 10 # Cimişlia
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Criuleni"] <- 11 # Criuleni
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Donduseni"] <- 12 # Donduseni
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Drochia"] <- 13 # Drochia
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Edineţ"] <- 14 # Edineţ
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Făleşti"] <- 15 # Făleşti
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Floreşti"] <- 16 # Floreşti
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Glodeni"] <- 17 # Glodeni
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Hîncesti"] <- 18 # Hîncesti
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Ialoveni"] <- 19 # Ialoveni
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Leova"] <- 20 # Leova
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Nisporeni"] <- 21 # Nisporeni
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Ocniţa"] <- 22 # Ocniţa
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Orhei"] <- 23 # Orhei
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Rezina"] <- 24 # Rezina
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Rîşcani"] <- 25 # Rîşcani
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Sîngerei"] <- 26 # Sîngerei
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Soroca"] <- 27 # Soroca
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Străşeni"] <- 28 # Străşeni
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Şoldăneşti"] <- 29 # Şoldăneşti
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Ştefan Voda"] <- 30 # Ştefan Voda
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Taraclia"] <- 31 # Taraclia
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Teleneşti"] <- 32 # Teleneşti
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Ungheni"] <- 33 # Ungheni
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Găgăuzia"] <- 34 # Găgăuzia
sf.MDA$REG_ID[sf.MDA$NAME_1 == "Dubăsari"] <- 35 # Dubăsari

# sf.MDA$REG_ID[sf.MDA$NAME_1 == "Bender"] <- 36 #Bender
# sf.MDA$REG_ID[sf.MDA$NAME_1 == "Transnistria"] <- 37 #Transnistria
# Bibiane: not sure about 36 & 37, Bender & Transnistria are in sf.MDA but not in Gallup
# Alex: we can exclude because independent
process_and_plot(sf.MDA, "MDA")
rm(sf.MDA)

## Madagascar  ----
sf.MDG <- subset(g, GID_0 == "MDG")
g <- subset(g, GID_0 != "MDG")
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Alaotra-Mangoro"] <- 1 # Alaotra-Mangoro
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Amoron'i mania"] <- 2 # Amoron'i mania
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Analamanga"] <- 3 # Analamanga
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Analanjirofo"] <- 4 # Analanjirofo
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Androy"] <- 5 # Androy
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Anosy"] <- 6 # Anosy
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Atsimo-Andrefana"] <- 7 # Atsimo-Andrefana
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Atsimo-Atsinana"] <- 8 # Atsimo-Atsinana
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Atsinanana"] <- 9 # Atsinanana
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Betsiboka"] <- 10 # Betsiboka
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Boeny"] <- 11 # Boeny
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Bongolava"] <- 12 # Bongolava
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Diana"] <- 13 # Diana
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Haute matsiatra"] <- 14 # Haute matsiatra
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Ihorombe"] <- 15 # Ihorombe
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Itasy"] <- 16 # Itasy
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Melaky"] <- 17 # Melaky
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Menabe"] <- 18 # Menabe
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Sava"] <- 19 # Sava
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Sofia"] <- 20 # Sofia
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Vakinankaratra"] <- 21 # Vakinankaratra
sf.MDG$REG_ID[sf.MDG$NAME_2 == "Vatovavy Fitovinany"] <- 22 # Vatovavy Fitovinany
process_and_plot(sf.MDG, "MDG")
rm(sf.MDG)


## Mexico  ----
sf.MEX <- subset(g, GID_0 == "MEX")
g <- subset(g, GID_0 != "MEX")
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Aguascalientes"] <- 1 # Aguascalientes
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Baja California"] <- 2 # Baja California
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Baja California Sur"] <- 3 # Baja California Sur
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Campeche"] <- 4 # Campeche
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Chiapas"] <- 5 # Chiapas
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Chihuahua"] <- 6 # Chihuahua
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Coahuila"] <- 7 # Coahuila
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Colima"] <- 8 # Colima
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Distrito Federal"] <- 9 # Distrito Federal/Ciudad de Mexico
sf.MEX$REG_ID[sf.MEX$NAME_1 == "México"] <- 9 # Distrito Federal/Ciudad de Mexico
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Durango"] <- 10 # Durango
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Guanajuato"] <- 11 # Guanajuato
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Guerrero"] <- 12 # Guerrero
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Hidalgo"] <- 13 # Hidalgo
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Jalisco"] <- 14 # Jalisco
sf.MEX$REG_ID[sf.MEX$NAME_1 == "México"] <- 15 # México
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Michoacán"] <- 16 # Michoacán
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Morelos"] <- 17 # Morelos
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Nayarit"] <- 18 # Nayarit
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Nuevo León"] <- 19 # Nuevo León
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Oaxaca"] <- 20 # Oaxaca
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Puebla"] <- 21 # Puebla
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Querétaro"] <- 22 # Querétaro
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Quintana Roo"] <- 23 # Quintana Roo
sf.MEX$REG_ID[sf.MEX$NAME_1 == "San Luis Potosí"] <- 24 # San Luis Potosí
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Sinaloa"] <- 25 # Sinaloa
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Sonora"] <- 26 # Sonora
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Tabasco"] <- 27 # Tabasco
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Tamaulipas"] <- 28 # Tamaulipas
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Tlaxcala"] <- 29 # Tlaxcala
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Veracruz"] <- 30 # Veracruz
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Yucatán"] <- 31 # Yucatán
sf.MEX$REG_ID[sf.MEX$NAME_1 == "Zacatecas"] <- 32 # Zacatecas
process_and_plot(sf.MEX, "MEX")
rm(sf.MEX)

## Macedonia  ----
sf.MKD <- subset(g, GID_0 == "MKD")
g <- subset(g, GID_0 != "MKD")
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Berovo"] <- 1 # Eastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Češinovo-Obleševo"] <- 1 # Eastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Delčevo"] <- 1 # Eastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Karbinci"] <- 1 # Eastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Kočani"] <- 1 # Eastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Makedonska Kamenica"] <- 1 # Eastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Pehčevo"] <- 1 # Eastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Probištip"] <- 1 # Eastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Štip"] <- 1 # Eastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Vinitsa"] <- 1 # Eastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Zrnovci"] <- 1 # Eastern

sf.MKD$REG_ID[sf.MKD$NAME_1 == "Kratovo"] <- 2 # Northeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Kriva Palanka"] <- 2 # Northeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Kumanovo"] <- 2 # Northeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Lipkovo"] <- 2 # Northeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Rankovce"] <- 2 # Northeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Staro Nagoričane"] <- 2 # Northeastern

sf.MKD$REG_ID[sf.MKD$NAME_1 == "Bitola"] <- 3 # Pelagonia
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Demir Hisar"] <- 3 # Pelagonia
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Dolneni"] <- 3 # Pelagonia
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Krivogaštani"] <- 3 # Pelagonia
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Kruševo"] <- 3 # Pelagonia
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Mogila"] <- 3 # Pelagonia
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Novatsi"] <- 3 # Pelagonia
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Prilep"] <- 3 # Pelagonia
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Resen"] <- 3 # Pelagonia

sf.MKD$REG_ID[sf.MKD$NAME_1 == "Bogovinje"] <- 4 # Polog
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Brvenica"] <- 4 # Polog
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Gostivar"] <- 4 # Polog
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Jegunovtse"] <- 4 # Polog
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Mavrovo and Rostuša"] <- 4 # Polog
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Tearce"] <- 4 # Polog
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Tetovo"] <- 4 # Polog
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Vrapčište"] <- 4 # Polog
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Želino"] <- 4 # Polog

sf.MKD$REG_ID[sf.MKD$NAME_1 == "Aracinovo"] <- 5 # Skopje
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Čučer Sandevo"] <- 5 # Skopje
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Ilinden"] <- 5 # Skopje
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Petrovec"] <- 5 # Skopje
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Sopište"] <- 5 # Skopje
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Studeničani"] <- 5 # Skopje
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Zelenikovo"] <- 5 # Skopje

sf.MKD$REG_ID[sf.MKD$NAME_1 == "Bogdanci"] <- 6 # Southeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Bosilovo"] <- 6 # Southeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Star Dojran"] <- 6 # Southeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Gevgelija"] <- 6 # Southeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Konče"] <- 6 # Southeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Novo Selo"] <- 6 # Southeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Radoviš"] <- 6 # Southeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Strumitsa"] <- 6 # Southeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Valandovo"] <- 6 # Southeastern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Vasilevo"] <- 6 # Southeastern

sf.MKD$REG_ID[sf.MKD$NAME_1 == "Centar župa"] <- 7 # Southwestern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Debar"] <- 7 # Southwestern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Debarca"] <- 7 # Southwestern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Kičevo"] <- 7 # Southwestern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Makedonski Brod"] <- 7 # Southwestern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Ohrid"] <- 7 # Southwestern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Plasnica"] <- 7 # Southwestern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Struga"] <- 7 # Southwestern
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Vevčani"] <- 7 # Southwestern

sf.MKD$REG_ID[sf.MKD$NAME_1 == "Čaška"] <- 8 # Vardar
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Demir Kapija"] <- 8 # Vardar
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Gradsko"] <- 8 # Vardar
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Kavadartsi"] <- 8 # Vardar
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Lozovo"] <- 8 # Vardar
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Negotino"] <- 8 # Vardar
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Rosoman"] <- 8 # Vardar
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Sveti Nikole"] <- 8 # Vardar
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Veles"] <- 8 # Vardar

# below are regions that are not included in https://en.wikipedia.org/wiki/Statistical_regions_of_North_Macedonia, but are in the sf.MKD
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Šuto Orizari"] <- 9 # ?
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Saraj"] <- 9 # ?
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Lake Ohrid"] <- 9 # ?
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Kisela Voda"] <- 9 # ?
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Karpoš"] <- 9 # ?
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Gjorče Petrov"] <- 9 # ?
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Gazi Baba"] <- 9 # ?
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Centar"] <- 9 # ?
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Čair"] <- 9 # ?
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Butel"] <- 9 # ?
sf.MKD$REG_ID[sf.MKD$NAME_1 == "Aerodrom"] <- 9 # ?

process_and_plot(sf.MKD, "MKD")
rm(sf.MKD)

## Mali  ----
sf.MLI <- subset(g, GID_0 == "MLI")
g <- subset(g, GID_0 != "MLI")
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Bandiagara"] <- 1 # Bandiagara
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Bankass"] <- 2 # Bankass
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Djenné"] <- 3 # Djenné
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Douentza"] <- 4 # Douentza
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Koro"] <- 5 # Koro
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Mopti"] <- 6 # Mopti
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Ténenkou"] <- 7 # Ténenkou
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Youwarou"] <- 8 # Youwarou
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Ansongo"] <- 9 # Ansongo
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Bourem"] <- 10 # Bourem
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Gao"] <- 11 # Gao
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Ménaka"] <- 12 # Ménaka
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Abeïbara"] <- 13 # Abeïbara
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Kidal"] <- 14 # Kidal
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Tessalit"] <- 15 # Tessalit
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Tin-Essako"] <- 16 # Tin-Essako
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Diré"] <- 17 # Diré
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Goundam"] <- 18 # Goundam
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Gourma-Rharous"] <- 19 # Gourma-Rharous
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Niafunké"] <- 20 # Niafunké
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Tombouctou"] <- 21 # Tombouctou
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Bla"] <- 22 # Bla
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Barouéli"] <- 23 # Barouéli
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Macina"] <- 24 # Macina
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Niono"] <- 25 # Niono
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Ségou"] <- 26 # Ségou
sf.MLI$REG_ID[sf.MLI$NAME_2 == "San"] <- 27 # San
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Tominian"] <- 28 # Tominian
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Bougouni"] <- 29 # Bougouni
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Kolondiéba"] <- 30 # Kolondiéba
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Kadiolo"] <- 31 # Kadiolo
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Koutiala"] <- 32 # Koutiala
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Sikasso"] <- 33 # Sikasso
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Yanfolila"] <- 34 # Yanfolila
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Yorosso"] <- 35 # Yorosso
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Bamako"] <- 36 # Bamako
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Banamba"] <- 37 # Banamba
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Dioïla"] <- 38 # Dioïla
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Kangaba"] <- 39 # Kangaba
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Koulikoro"] <- 40 # Koulikoro
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Kolokani"] <- 41 # Kolokani
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Kati"] <- 42 # Kati
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Nara"] <- 43 # Nara
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Bafoulabé"] <- 44 # Bafoulabé
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Diéma"] <- 45 # Diéma
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Kita"] <- 46 # Kita
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Kéniéba"] <- 47 # Kéniéba
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Kayes"] <- 48 # Kayes
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Nioro"] <- 49 # Nioro
sf.MLI$REG_ID[sf.MLI$NAME_2 == "Yélimané"] <- 50 # Yélimané
process_and_plot(sf.MLI, "MLI")
rm(sf.MLI)

## Malta  ----
sf.MLT <- subset(g, GID_0 == "MLT")
g <- subset(g, GID_0 != "MLT")
table(sf.MLT$NAME_2)
sf.MLT$REG_ID[sf.MLT$NAME_1 == "Nofsinhar"] <- 1 # Southern Harbour District
sf.MLT$REG_ID[sf.MLT$NAME_1 == "Ċentrali"] <- 2 # Northern Harbour District
sf.MLT$REG_ID[sf.MLT$NAME_1 == "Xlokk"] <- 3 # South Eastern District

sf.MLT$REG_ID[sf.MLT$NAME_2 == "Ħ'Attard"] <- 4 # Western District
sf.MLT$REG_ID[sf.MLT$NAME_2 == "Ħad-Dingli"] <- 4 # Western District
sf.MLT$REG_ID[sf.MLT$NAME_2 == "Ħal Balzan"] <- 4 # Western District
sf.MLT$REG_ID[sf.MLT$NAME_2 == "L-Iklin"] <- 4 # Western District
sf.MLT$REG_ID[sf.MLT$NAME_2 == "Ħal Lija"] <- 4 # Western District
sf.MLT$REG_ID[sf.MLT$NAME_2 == "L-Imdina"] <- 4 # Western District
sf.MLT$REG_ID[sf.MLT$NAME_2 == "L-Imtarfa"] <- 4 # Western District
sf.MLT$REG_ID[sf.MLT$NAME_2 == "Ir-Rabat"] <- 4 # Western District
sf.MLT$REG_ID[sf.MLT$NAME_2 == "Is-Siġġiewi"] <- 4 # Western District
sf.MLT$REG_ID[sf.MLT$NAME_2 == "Ħaż-Żebbuġ"] <- 4 # Western District

sf.MLT$REG_ID[sf.MLT$NAME_1 == "Tramuntana"] <- 5 # North District
sf.MLT$REG_ID[sf.MLT$NAME_1 == "Għawdex"] <- 6 # Gozo and Comino
process_and_plot(sf.MLT, "MLT")
rm(sf.MLT)

## Myanmar  ----
sf.MMR <- subset(g, GID_0 == "MMR")
g <- subset(g, GID_0 != "MMR")
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Chin"] <- 1 # Chin
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Kachin"] <- 2 # Kachin
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Kayah"] <- 3 # Kayah
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Kayin"] <- 4 # Kayin
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Mon"] <- 5 # Mon
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Rakhine"] <- 6 # Rakhine
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Shan"] <- 7 # Shan
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Ayeyarwady"] <- 8 # Ayeyarwady
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Bago"] <- 9 # Bago
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Magway"] <- 10 # Magway
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Mandalay"] <- 11 # Mandalay
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Sagaing"] <- 12 # Sagaing
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Tanintharyi"] <- 13 # Tanintharyi
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Yangon"] <- 14 # Yangon
sf.MMR$REG_ID[sf.MMR$NAME_1 == "Naypyitaw"] <- 15 # Naypyitaw
process_and_plot(sf.MMR, "MMR")
rm(sf.MMR)


## Mongolia  ----
sf.MNG <- subset(g, GID_0 == "MNG")
g <- subset(g, GID_0 != "MNG")
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Arhangay"] <- 1 # Arhangay
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Bayan-Ölgiy"] <- 2 # Bayan-Ölgiy
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Bayanhongor"] <- 3 # Bayanhongor
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Bulgan"] <- 4 # Bulgan
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Govi-Altay"] <- 5 # Govi-Altay
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Dornogovi"] <- 6 # Dornogovi
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Dornod"] <- 7 # Dornod
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Dundgovi"] <- 8 # Dundgovi
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Dzavhan"] <- 9 # Dzavhan
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Övörhangay"] <- 10 # Övörhangay
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Ömnögovi"] <- 11 # Ömnögovi
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Sühbaatar"] <- 12 # Sühbaatar
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Selenge"] <- 13 # Selenge
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Töv"] <- 14 # Töv
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Uvs"] <- 15 # Uvs
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Hovd"] <- 16 # Hovd
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Hövsgöl"] <- 17 # Hövsgöl
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Hentiy"] <- 18 # Hentiy
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Darhan-Uul"] <- 19 # Darhan-Uul
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Orhon"] <- 20 # Orhon
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Govisümber"] <- 21 # Govisümber
sf.MNG$REG_ID[sf.MNG$NAME_1 == "Ulaanbaatar"] <- 22 # Ulaanbaatar
process_and_plot(sf.MNG, "MNG")
rm(sf.MNG)

## Mozambique  ----
sf.MOZ <- subset(g, GID_0 == "MOZ")
g <- subset(g, GID_0 != "MOZ")
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Cabo Delgado"] <- 1 # Cabo Delgado
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Gaza"] <- 2 # Gaza
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Inhambane"] <- 3 # Inhambane
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Manica"] <- 4 # Manica
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Maputo City"] <- 5 # Maputo City
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Nampula"] <- 6 # Nampula
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Nassa"] <- 7 # Nassa
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Sofala"] <- 8 # Sofala
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Tete"] <- 9 # Tete
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Zambezia"] <- 10 # Zambezia
sf.MOZ$REG_ID[sf.MOZ$NAME_1 == "Maputo"] <- 11 # Maputo
process_and_plot(sf.MOZ, "MOZ")
rm(sf.MOZ)

## Mauritania  ----
sf.MRT <- subset(g, GID_0 == "MRT")
g <- subset(g, GID_0 != "MRT")
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Adrar"] <- 1 # Adrar
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Assaba"] <- 2 # Assaba
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Brakna"] <- 3 # Brakna
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Dakhlet Nouadhibou"] <- 4 # Dakhlet Nouadhibou
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Gorgol"] <- 5 # Gorgol
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Guidimaka"] <- 6 # Guidimaka
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Hodh ech Chargui"] <- 7 # Hodh ech Chargui
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Hodh el Gharbi"] <- 8 # Hodh el Gharbi
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Inchiri"] <- 9 # Inchiri
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Nouakchott"] <- 10 # Nouakchott
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Tagant"] <- 11 # Tagant
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Tiris Zemmour"] <- 12 # Tiris Zemmour
sf.MRT$REG_ID[sf.MRT$NAME_1 == "Trarza"] <- 13 # Trarza
process_and_plot(sf.MRT, "MRT")
rm(sf.MRT)

## Mauritius  ----
sf.MUS <- subset(g, GID_0 == "MUS")
g <- subset(g, GID_0 != "MUS")
sf.MUS$REG_ID[sf.MUS$NAME_1 == "Black River"] <- 1 # Black River
sf.MUS$REG_ID[sf.MUS$NAME_1 == "Flacq"] <- 2 # Flacq
sf.MUS$REG_ID[sf.MUS$NAME_1 == "Grand Port"] <- 3 # Grand Port
sf.MUS$REG_ID[sf.MUS$NAME_1 == "Moka"] <- 4 # Moka
sf.MUS$REG_ID[sf.MUS$NAME_1 == "Pamplemousses"] <- 5 # Pamplemousses
sf.MUS$REG_ID[sf.MUS$NAME_1 == "Plaines Wilhems"] <- 6 # Plaines Wilhems
sf.MUS$REG_ID[sf.MUS$NAME_1 == "Port Louis"] <- 7 # Port Louis
sf.MUS$REG_ID[sf.MUS$NAME_1 == "Rivière du Rempart"] <- 8 # Rivière du Rempart
sf.MUS$REG_ID[sf.MUS$NAME_1 == "Savanne"] <- 9 # Savanne
sf.MUS$REG_ID[sf.MUS$NAME_1 == "Rodriguez"] <- 10 # Rodriguez

# sf.MUS$REG_ID[sf.MUS$NAME_1 == "Saint Brandon"] <- 11 #Saint Brandon
# sf.MUS$REG_ID[sf.MUS$NAME_1 == "Agalega Islands"] <- 12 #Agalega Islands
# Bibiane: not sure about 11 & 12, were not in Gallup but are in sf.MUS
# Alex: we can exclude since not in the Gallup regions
process_and_plot(sf.MUS, "MUS")
rm(sf.MUS)

## Malawi  ----
sf.MWI <- subset(g, GID_0 == "MWI")
g <- subset(g, GID_0 != "MWI")

sf.MWI$REG_ID[sf.MWI$NAME_1 == "Dedza"] <- 1 # Dedza
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Dowa"] <- 2 # Dowa
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Kasungu"] <- 3 # Kasungu
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Lilongwe"] <- 4 # Lilongwe
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Mchinji"] <- 5 # Mchinji
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Nkhotakota"] <- 6 # Nkhotakota
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Ntcheu"] <- 7 # Ntcheu
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Ntchisi"] <- 8 # Ntchisi
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Salima"] <- 9 # Salima
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Chitipa"] <- 10 # Chitipa
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Karonga"] <- 11 # Karonga
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Likoma"] <- 12 # Likoma
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Mzimba"] <- 13 # Mzimba
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Nkhata Bay"] <- 14 # Nkhata Bay
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Rumphi"] <- 15 # Rumphi
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Balaka"] <- 16 # Balaka
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Blantyre"] <- 17 # Blantyre
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Chikwawa"] <- 18 # Chikwawa
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Chiradzulu"] <- 19 # Chiradzulu
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Machinga"] <- 20 # Machinga
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Mangochi"] <- 21 # Mangochi
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Mulanje"] <- 22 # Mulanje
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Mwanza"] <- 23 # Mwanza
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Nsanje"] <- 24 # Nsanje
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Thyolo"] <- 25 # Thyolo
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Phalombe"] <- 26 # Phalombe
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Zomba"] <- 27 # Zomba
sf.MWI$REG_ID[sf.MWI$NAME_1 == "Neno"] <- 28 # Neno (Mwanza)

process_and_plot(sf.MWI, "MWI")
rm(sf.MWI)

## Malaysia  ----
sf.MYS <- subset(g, GID_0 == "MYS")
g <- subset(g, GID_0 != "MYS")
table(sf.MYS$NAME_1)
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Johor"] <- 1 # Johor
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Kedah"] <- 2 # Kedah
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Kelantan"] <- 3 # Kelantan
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Pahang"] <- 4 # Pahang
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Perak"] <- 5 # Perak
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Selangor"] <- 6 # Selangor
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Trengganu"] <- 7 # Trengganu
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Negeri Sembilan"] <- 8 # Negeri Sembilan
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Perlis"] <- 9 # Perlis
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Melaka"] <- 10 # Melaka
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Pulau Pinang"] <- 11 # Pulau Pinang
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Putrajaya"] <- 12 # Putrajaya
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Kuala Lumpur"] <- 13 # Kuala Lumpur
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Sabah"] <- 14 # Sabah
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Sarawak"] <- 15 # Sarawak
sf.MYS$REG_ID[sf.MYS$NAME_1 == "Labuan"] <- 16 # Labuan
process_and_plot(sf.MYS, "MYS")
rm(sf.MYS)

## Namibia  ----
sf.NAM <- subset(g, GID_0 == "NAM")
g <- subset(g, GID_0 != "NAM")

sf.NAM$REG_ID[sf.NAM$NAME_1 == "Zambezi"] <- 1 # Zambezi
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Erongo"] <- 2 # Erongo
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Hardap"] <- 3 # Hardap
sf.NAM$REG_ID[sf.NAM$NAME_1 == "!Karas"] <- 4 # !Karas

# https://en.wikipedia.org/wiki/Kavango_East
# https://en.wikipedia.org/wiki/Kavango_West
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Kavango" & sf.NAM$NAME_2 %in% c("Mashare", "Mukwe", "Ndiyona", "Rundu Rural East", "Rundu Rural West", "Rundu Urban")] <- 5 # East Kavango
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Kavango" & sf.NAM$NAME_2 %in% c("Kapako", "Mpungu", "Kahenge")] <- 6 # West Kavango

sf.NAM$REG_ID[sf.NAM$NAME_1 == "Khomas"] <- 7 # Khomas
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Kunene"] <- 8 # Kunene
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Ohangwena"] <- 9 # Ohangwena
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Omaheke"] <- 10 # Omaheke
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Omusati"] <- 11 # Omusati
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Oshana"] <- 12 # Oshana
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Oshikoto"] <- 13 # Oshikoto
sf.NAM$REG_ID[sf.NAM$NAME_1 == "Otjozondjupa"] <- 14 # Otjozondjupa
process_and_plot(sf.NAM, "NAM")
rm(sf.NAM)

## Niger  ----
sf.NER <- subset(g, GID_0 == "NER")
g <- subset(g, GID_0 != "NER")
table(sf.NER$NAME_1)
sf.NER$REG_ID[sf.NER$NAME_1 == "Agadez"] <- 1 # Agadez
sf.NER$REG_ID[sf.NER$NAME_1 == "Diffa"] <- 2 # Diffa
sf.NER$REG_ID[sf.NER$NAME_1 == "Dosso"] <- 3 # Dosso
sf.NER$REG_ID[sf.NER$NAME_1 == "Maradi"] <- 4 # Maradi
sf.NER$REG_ID[sf.NER$NAME_1 == "Niamey"] <- 5 # Niamey
sf.NER$REG_ID[sf.NER$NAME_1 == "Tahoua"] <- 6 # Tahoua
sf.NER$REG_ID[sf.NER$NAME_1 == "Tillabéry"] <- 7 # Tillabéry
sf.NER$REG_ID[sf.NER$NAME_1 == "Zinder"] <- 8 # Zinder
process_and_plot(sf.NER, "NER")
rm(sf.NER)

## Nigeria  ----
sf.NGA <- subset(g, GID_0 == "NGA")
g <- subset(g, GID_0 != "NGA")
table(sf.NGA$NAME_1)
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Abia"] <- 1 # Abia
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Federal Capital Territory"] <- 2 # Federal Capital Territory
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Adamawa"] <- 3 # Adamawa
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Akwa Ibom"] <- 4 # Akwa Ibom
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Anambra"] <- 5 # Anambra
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Bauchi"] <- 6 # Bauchi
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Bayelsa"] <- 7 # Bayelsa
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Benue"] <- 8 # Benue
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Borno"] <- 9 # Borno
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Cross River"] <- 10 # Cross River
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Delta"] <- 11 # Delta
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Ebonyi"] <- 12 # Ebonyi
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Edo"] <- 13 # Edo
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Ekiti"] <- 14 # Ekiti
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Enugu"] <- 15 # Enugu
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Gombe"] <- 16 # Gombe
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Imo"] <- 17 # Imo
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Jigawa"] <- 18 # Jigawa
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Kaduna"] <- 19 # Kaduna
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Kano"] <- 20 # Kano
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Katsina"] <- 21 # Katsina
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Kebbi"] <- 22 # Kebbi
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Kogi"] <- 23 # Kogi
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Kwara"] <- 24 # Kwara
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Lagos"] <- 25 # Lagos
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Nasarawa"] <- 26 # Nasarawa
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Niger"] <- 27 # Niger
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Ogun"] <- 28 # Ogun
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Ondo"] <- 29 # Ondo
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Osun"] <- 30 # Osun
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Oyo"] <- 31 # Oyo
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Plateau"] <- 32 # Plateau
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Rivers"] <- 33 # Rivers
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Sokoto"] <- 34 # Sokoto
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Taraba"] <- 35 # Taraba
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Yobe"] <- 36 # Yobe
sf.NGA$REG_ID[sf.NGA$NAME_1 == "Zamfara"] <- 37 # Zamfara
process_and_plot(sf.NGA, "NGA")
rm(sf.NGA)

## Nicaragua   ----
sf.NIC <- subset(g, GID_0 == "NIC")
g <- subset(g, GID_0 != "NIC")
table(sf.NIC$NAME_1)
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Boaco"] <- 1 # Boaco
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Carazo"] <- 2 # Carazo
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Chinandega"] <- 3 # Chinandega
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Chontales"] <- 4 # Chontales
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Estelí"] <- 5 # Estelí

sf.NIC$REG_ID[sf.NIC$NAME_1 == "Granada"] <- 6 # Granada
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Lago Nicaragua"] <- 6 # Granada
# https://en.wikipedia.org/wiki/Lake_Nicaragua

sf.NIC$REG_ID[sf.NIC$NAME_1 == "Jinotega"] <- 7 # Jinotega
sf.NIC$REG_ID[sf.NIC$NAME_1 == "León"] <- 8 # León
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Madriz"] <- 9 # Madriz
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Managua"] <- 10 # Managua
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Masaya"] <- 11 # Masaya
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Matagalpa"] <- 12 # Matagalpa
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Nueva Segovia"] <- 13 # Nueva Segovia
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Rivas"] <- 14 # Rivas
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Río San Juan"] <- 15 # Río San Juan
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Atlántico Norte"] <- 16 # Atlántico Norte
sf.NIC$REG_ID[sf.NIC$NAME_1 == "Atlántico Sur"] <- 17 # Atlántico Sur

process_and_plot(sf.NIC, "NIC")
rm(sf.NIC)


## Netherlands  ----
sf.NLD <- subset(g, GID_0 == "NLD")
g <- subset(g, GID_0 != "NLD")
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Groningen"] <- 17020 # Groningen
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Fryslân"] <- 17021 # Fryslân
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Drenthe"] <- 17022 # Drenthe
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Overijssel"] <- 17023 # Overijssel
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Gelderland"] <- 17024 # Gelderland
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Flevoland"] <- 17025 # Flevoland
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Utrecht"] <- 17026 # Utrecht
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Noord-Holland"] <- 17027 # Noord-Holland
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Zuid-Holland"] <- 17028 # Zuid-Holland
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Zeeland"] <- 17029 # Zeeland
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Noord-Brabant"] <- 17030 # Noord-Brabant
sf.NLD$REG_ID[sf.NLD$NAME_1 == "Limburg"] <- 17031 # Limburg
# sf.NLD$REG_ID[sf.NLD$NAME_1 == "IJsselmeer"] <- 17032 #IJsselmeer
# sf.NLD$REG_ID[sf.NLD$NAME_1 == "Zeeuwse meren"] <- 17033 #Zeeuwse meren
# sf.NLD$REG_ID[sf.NLD$NAME_1 == "Zuid Hollandse Meren"] <- 17034 #Zuid Hollandse Meren
# Bibiane: *not sure about 17032 - 17034, they show up in the sf.NLD but not in the Gallup
# Alex: these are lakes so we can exclude them
process_and_plot(sf.NLD, "NLD")
rm(sf.NLD)

## Norway  ----
sf.NOR <- subset(g, GID_0 == "NOR")
g <- subset(g, GID_0 != "NOR")
table(sf.NOR$NAME_1)
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Akershus"] <- 1 # Akershus
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Aust-Agder"] <- 2 # Aust-Agder
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Buskerud"] <- 3 # Buskerud
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Finnmark"] <- 4 # Finnmark
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Hedmark"] <- 5 # Hedmark
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Hordaland"] <- 6 # Hordaland
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Møre og Romsdal"] <- 7 # Møre og Romsdal
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Nordland"] <- 8 # Nordland
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Oppland"] <- 10 # Oppland
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Oslo"] <- 11 # Oslo
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Ãstfold"] <- 12 # Ãstfold
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Rogaland"] <- 13 # Rogaland
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Sogn og Fjordane"] <- 14 # Sogn og Fjordane
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Telemark"] <- 16 # Telemark
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Troms"] <- 17 # Troms
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Vest-Agder"] <- 18 # Vest-Agder
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Vestfold"] <- 19 # Vestfold
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Sør-Trøndelag"] <- 20 # Trøndelag
sf.NOR$REG_ID[sf.NOR$NAME_1 == "Nord-Trøndelag"] <- 20 # Trøndelag
process_and_plot(sf.NOR, "NOR")
rm(sf.NOR)

## Nepal  ----
sf.NPL <- subset(g, GID_0 == "NPL")
g <- subset(g, GID_0 != "NPL")
sf.NPL$REG_ID[sf.NPL$NAME_3 == "Kathmandu"] <- 1 # Kathmandu
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Bagmati" & sf.NPL$NAME_3 != "Kathmandu"] <- 2 # Bagmati
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Bheri"] <- 3 # Bheri
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Dhaualagiri"] <- 4 # Dhaualagiri
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Gandaki"] <- 5 # Gandaki
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Janakpur"] <- 6 # Janakpur
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Karnali"] <- 7 # Karnali
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Koshi"] <- 8 # Koshi
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Lumbini"] <- 9 # Lumbini
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Mahakali"] <- 10 # Mahakali
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Mechi"] <- 11 # Mechi
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Narayani"] <- 12 # Narayani
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Rapti"] <- 13 # Rapti
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Sagarmatha"] <- 14 # Sagarmatha
sf.NPL$REG_ID[sf.NPL$NAME_2 == "Seti"] <- 15 # Seti
process_and_plot(sf.NPL, "NPL")
rm(sf.NPL)

## New Zealand ----
sf.NZL <- subset(g, GID_0 == "NZL")
g <- subset(g, GID_0 != "NZL")
table(sf.NZL$NAME_1)
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Northland"] <- 1 # Northland
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Auckland"] <- 2 # Auckland
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Waikato"] <- 3 # Waikato
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Bay of Plenty"] <- 4 # Bay of Plenty

sf.NZL$REG_ID[sf.NZL$NAME_1 == "Gisborne"] <- 5 # East Bay

sf.NZL$REG_ID[sf.NZL$NAME_1 == "Hawke's Bay"] <- 6 # Hawke's Bay
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Taranaki"] <- 7 # Taranaki
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Manawatu-Wanganui"] <- 8 # Manawatu-Wanganui
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Wellington"] <- 9 # Wellington
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Tasman"] <- 10 # Tasman
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Nelson"] <- 11 # Nelson
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Marlborough"] <- 12 # Marlborough
sf.NZL$REG_ID[sf.NZL$NAME_1 == "West Coast"] <- 13 # West Coast
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Canterbury"] <- 14 # Canterbury
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Otago"] <- 15 # Otago
sf.NZL$REG_ID[sf.NZL$NAME_1 == "Southland"] <- 16 # Southland

# sf.NZL$REG_ID[sf.NZL$NAME_1 == "Chatham Islands"] <- 17 #Chatham Islands
# sf.NZL$REG_ID[sf.NZL$NAME_1 == "Northern Islands"] <- 19 #Northern Islands
# sf.NZL$REG_ID[sf.NZL$NAME_1 == "Southern Islands"] <- 20 #Southern Islands
# Bibiane: *not sure about 17-20, appear in sf.NZL but not in Gallup
# Alex: I've adjuste the coding for Gisborne, which is the East Bay region; the other islands we can exclude since there's no gallup code
process_and_plot(sf.NZL, "NZL")
rm(sf.NZL)

## Pakistan  ----
sf.PAK <- subset(g, GID_0 == "PAK")
g <- subset(g, GID_0 != "PAK")
table(sf.PAK$NAME_3)
sf.PAK$REG_ID[sf.PAK$NAME_1 == "Sindh"] <- 1 # Sindh
sf.PAK$REG_ID[sf.PAK$NAME_1 == "Punjab"] <- 2 # Punjab
sf.PAK$REG_ID[sf.PAK$NAME_1 == "Khyber-Pakhtunkhwa"] <- 3 # Khyber-Pakhtunkhwa
sf.PAK$REG_ID[sf.PAK$NAME_1 == "Balochistan"] <- 4 # Balochistan
sf.PAK$REG_ID[sf.PAK$NAME_3 == "Disputed Area 2"] <- 5 # AJK (Azad Kashmir)
sf.PAK$REG_ID[sf.PAK$NAME_1 == "Federally Administered Tribal Ar"] <- 6 # Federally Administered Tribal Ar
sf.PAK$REG_ID[sf.PAK$NAME_1 == "Disputed Area 1"] <- 7 # Gilgit-Baltistan (Northern Areas)
sf.PAK$REG_ID[sf.PAK$NAME_1 == "Islamabad"] <- 8 # Islamabad
process_and_plot(sf.PAK, "PAK")
rm(sf.PAK)

## Panama  ----
sf.PAN <- subset(g, GID_0 == "PAN")
g <- subset(g, GID_0 != "PAN")
table(sf.PAN$NAME_1)
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Bocas del Toro"] <- 1 # Bocas del Toro
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Chiriquí"] <- 2 # Chiriquí
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Coclé"] <- 3 # Coclé
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Colón"] <- 4 # Colón
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Darién"] <- 5 # Darién
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Herrera"] <- 6 # Herrera
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Los Santos"] <- 7 # Los Santos
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Panamá"] <- 8 # Panamá
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Panamá Oeste"] <- 9 # Panamá Oeste
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Veraguas"] <- 10 # Veraguas
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Ngöbe Buglé"] <- 11 # Ngöbe Buglé
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Emberá"] <- 12 # Emberá
sf.PAN$REG_ID[sf.PAN$NAME_1 == "Kuna Yala"] <- 13 # Kuna Yala
process_and_plot(sf.PAN, "PAN")
rm(sf.PAN)

## Peru  ----
sf.PER <- subset(g, GID_0 == "PER")
g <- subset(g, GID_0 != "PER")
table(sf.PER$NAME_1)
sf.PER$REG_ID[sf.PER$NAME_1 == "Amazonas"] <- 1 # Amazonas
sf.PER$REG_ID[sf.PER$NAME_1 == "Ancash"] <- 2 # Ancash
sf.PER$REG_ID[sf.PER$NAME_1 == "Apurímac"] <- 3 # Apurímac
sf.PER$REG_ID[sf.PER$NAME_1 == "Arequipa"] <- 4 # Arequipa
sf.PER$REG_ID[sf.PER$NAME_1 == "Ayacucho"] <- 5 # Ayacucho
sf.PER$REG_ID[sf.PER$NAME_1 == "Cajamarca"] <- 6 # Cajamarca
sf.PER$REG_ID[sf.PER$NAME_1 == "Cusco"] <- 7 # Cusco
sf.PER$REG_ID[sf.PER$NAME_1 == "Huancavelica"] <- 8 # Huancavelica
sf.PER$REG_ID[sf.PER$NAME_1 == "Huánuco"] <- 9 # Huánuco
sf.PER$REG_ID[sf.PER$NAME_1 == "Ica"] <- 10 # Ica
sf.PER$REG_ID[sf.PER$NAME_1 == "Junín"] <- 11 # Junín
sf.PER$REG_ID[sf.PER$NAME_1 == "La Libertad"] <- 12 # La Libertad
sf.PER$REG_ID[sf.PER$NAME_1 == "Lambayeque"] <- 13 # Lambayeque

sf.PER$REG_ID[sf.PER$NAME_1 == "Lima"] <- 14 # Lima
sf.PER$REG_ID[sf.PER$NAME_1 == "Lima Province"] <- 14 # Lima Province
sf.PER$REG_ID[sf.PER$NAME_1 == "Callao"] <- 14 # Callao

sf.PER$REG_ID[sf.PER$NAME_1 == "Loreto"] <- 15 # Loreto
sf.PER$REG_ID[sf.PER$NAME_1 == "Moquegua"] <- 17 # Moquegua
sf.PER$REG_ID[sf.PER$NAME_1 == "Pasco"] <- 18 # Pasco
sf.PER$REG_ID[sf.PER$NAME_1 == "Piura"] <- 19 # Piura
sf.PER$REG_ID[sf.PER$NAME_1 == "Puno"] <- 20 # Puno
sf.PER$REG_ID[sf.PER$NAME_1 == "San Martín"] <- 21 # San Martín
sf.PER$REG_ID[sf.PER$NAME_1 == "Tacna"] <- 22 # Tacna
sf.PER$REG_ID[sf.PER$NAME_1 == "Tumbes"] <- 23 # Tumbes
sf.PER$REG_ID[sf.PER$NAME_1 == "Ucayali"] <- 24 # Ucayali
sf.PER$REG_ID[sf.PER$NAME_1 == "Madre de Dios"] <- 25 # Madre de Dios

# https://en.wikipedia.org/wiki/Lima_Province
# Bibiane: *not sure about 26 & 27, appear in sf.PER but not in Gallup
# Alex: I've assigned these to Lima
process_and_plot(sf.PER, "PER")
rm(sf.PER)

## Philippines  ----
sf.PHL <- subset(g, GID_0 == "PHL")
g <- subset(g, GID_0 != "PHL")
table(sf.PHL$NAME_1)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Metropolitan Manila"] <- 1 # National Capital Region (NCR)
# https://en.wikipedia.org/wiki/Metro_Manila

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Abra"] <- 2 # Cordillera (CAR)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Apayao"] <- 2 # Cordillera (CAR)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Benguet"] <- 2 # Cordillera (CAR)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Ifugao"] <- 2 # Cordillera (CAR)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Kalinga"] <- 2 # Cordillera (CAR)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Mountain Province"] <- 2 # Cordillera (CAR)
# https://en.wikipedia.org/wiki/Cordillera_Administrative_Region

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Ilocos Norte"] <- 3 # Ilocos (Region I)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Ilocos Sur"] <- 3 # Ilocos (Region I)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "La Union"] <- 3 # Ilocos (Region I)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Pangasinan"] <- 3 # Ilocos (Region I)
# https://en.wikipedia.org/wiki/Ilocos_Region

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Batanes"] <- 4 # Cagayan Valley (Region II)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Cagayan"] <- 4 # Cagayan Valley (Region II)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Isabela"] <- 4 # Cagayan Valley (Region II)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Nueva Vizcaya"] <- 4 # Cagayan Valley (Region II)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Quirino"] <- 4 # Cagayan Valley (Region II)
# https://en.wikipedia.org/wiki/Cagayan_Valley

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Aurora"] <- 5 # Central Luzon (Region III)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Bataan"] <- 5 # Central Luzon (Region III)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Bulacan"] <- 5 # Central Luzon (Region III)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Nueva Ecija"] <- 5 # Central Luzon (Region III)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Pampanga"] <- 5 # Central Luzon (Region III)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Tarlac"] <- 5 # Central Luzon (Region III)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Zambales"] <- 5 # Central Luzon (Region III)
# https://en.wikipedia.org/wiki/Central_Luzon

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Batanes"] <- 6 # Calabarzon (Region IV-A)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Cavite"] <- 6 # Calabarzon (Region IV-A)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Laguna"] <- 6 # Calabarzon (Region IV-A)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Quezon"] <- 6 # Calabarzon (Region IV-A)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Rizal"] <- 6 # Calabarzon (Region IV-A)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Batangas"] <- 6 # Calabarzon (Region IV-A)
# https://en.wikipedia.org/wiki/Calabarzon

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Occidental Mindoro"] <- 7 # Mimaropa (Region IV-B)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Oriental Mindoro"] <- 7 # Mimaropa (Region IV-B)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Marinduque"] <- 7 # Mimaropa (Region IV-B)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Romblon"] <- 7 # Mimaropa (Region IV-B)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Palawan"] <- 7 # Mimaropa (Region IV-B)
# https://en.wikipedia.org/wiki/Mimaropa

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Albay"] <- 8 # Bicol (Region V)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Camarines Norte"] <- 8 # Bicol (Region V)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Camarines Sur"] <- 8 # Bicol (Region V)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Sorsogon"] <- 8 # Bicol (Region V)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Catanduanes"] <- 8 # Bicol (Region V)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Masbate"] <- 8 # Bicol (Region V)
# https://en.wikipedia.org/wiki/Bicol_Region

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Aklan"] <- 9 # Western Visayas (Region VI)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Antique"] <- 9 # Western Visayas (Region VI)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Capiz"] <- 9 # Western Visayas (Region VI)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Guimaras"] <- 9 # Western Visayas (Region VI)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Iloilo"] <- 9 # Western Visayas (Region VI)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Negros Occidental"] <- 9 # Western Visayas (Region VI)
# https://en.wikipedia.org/wiki/Western_Visayas

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Cebu"] <- 10 # Central Visayas (Region VII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Bohol"] <- 10 # Central Visayas (Region VII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Negros Oriental"] <- 10 # Central Visayas (Region VII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Siquijor"] <- 10 # Central Visayas (Region VII)
# https://en.wikipedia.org/wiki/Central_Visayas

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Samar"] <- 11 # Eastern Visayas (Region VIII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Leyte"] <- 11 # Eastern Visayas (Region VIII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Biliran"] <- 11 # Eastern Visayas (Region VIII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Northern Samar"] <- 11 # Eastern Visayas (Region VIII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Eastern Samar"] <- 11 # Eastern Visayas (Region VIII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Southern Leyte"] <- 11 # Eastern Visayas (Region VIII)
# https://en.wikipedia.org/wiki/Eastern_Visayas

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Zamboanga del Norte"] <- 12 # Zamboanga Peninsula (Region IX)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Zamboanga Sibugay"] <- 12 # Zamboanga Peninsula (Region IX)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Zamboanga del Sur"] <- 12 # Zamboanga Peninsula (Region IX)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Isabela"] <- 12 # Zamboanga Peninsula (Region IX)
# https://en.wikipedia.org/wiki/Zamboanga_Peninsula

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Bukidnon"] <- 13 # Northern Mindanao (Region X)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Camiguin"] <- 13 # Northern Mindanao (Region X)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Misamis Occidental"] <- 13 # Northern Mindanao (Region X)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Misamis Oriental"] <- 13 # Northern Mindanao (Region X)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Lanao del Norte"] <- 13 # Northern Mindanao (Region X)
# https://en.wikipedia.org/wiki/Northern_Mindanao

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Davao del Norte"] <- 14 # Davao (Region XI)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Davao del Sur"] <- 14 # Davao (Region XI)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Davao Oriental"] <- 14 # Davao (Region XI)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Compostela Valley"] <- 14 # Davao (Region XI)
# https://en.wikipedia.org/wiki/Davao_Region

sf.PHL$REG_ID[sf.PHL$NAME_1 == "North Cotabato"] <- 15 # SOCCSKSARGEN (Region XII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "South Cotabato"] <- 15 # SOCCSKSARGEN (Region XII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Sultan Kudarat"] <- 15 # SOCCSKSARGEN (Region XII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Sarangani"] <- 15 # SOCCSKSARGEN (Region XII)
# https://en.wikipedia.org/wiki/Soccsksargen

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Agusan del Norte"] <- 16 # Caraga (Region XIII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Agusan del Sur"] <- 16 # Caraga (Region XIII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Dinagat Islands"] <- 16 # Caraga (Region XIII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Surigao del Norte"] <- 16 # Caraga (Region XIII)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Surigao del Sur"] <- 16 # Caraga (Region XIII)
# https://en.wikipedia.org/wiki/Caraga

sf.PHL$REG_ID[sf.PHL$NAME_1 == "Basilan"] <- 17 # Bangsamoro Autonomous Region in Muslim Mindanao (BARMM)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Lanao del Sur"] <- 17 # Bangsamoro Autonomous Region in Muslim Mindanao (BARMM)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Maguindanao"] <- 17 # Bangsamoro Autonomous Region in Muslim Mindanao (BARMM)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Sulu"] <- 17 # Bangsamoro Autonomous Region in Muslim Mindanao (BARMM)
sf.PHL$REG_ID[sf.PHL$NAME_1 == "Tawi-Tawi"] <- 17 # Bangsamoro Autonomous Region in Muslim Mindanao (BARMM)
# https://en.wikipedia.org/wiki/Bangsamoro


process_and_plot(sf.PHL, "PHL")
rm(sf.PHL)

## Poland  ----
sf.POL <- subset(g, GID_0 == "POL")
g <- subset(g, GID_0 != "POL")
table(sf.POL$NAME_1)
sf.POL$REG_ID[sf.POL$NAME_1 == "Łódzkie"] <- 1 # Łódzkie
sf.POL$REG_ID[sf.POL$NAME_1 == "Mazowieckie"] <- 2 # Mazowieckie
sf.POL$REG_ID[sf.POL$NAME_1 == "Małopolskie"] <- 3 # Małopolskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Śląskie"] <- 4 # Śląskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Lubelskie"] <- 5 # Lubelskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Podkarpackie"] <- 6 # Podkarpackie
sf.POL$REG_ID[sf.POL$NAME_1 == "Wielkopolskie"] <- 7 # Wielkopolskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Podlaskie"] <- 8 # Podlaskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Świętokrzyskie"] <- 9 # Świętokrzyskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Zachodniopomorskie"] <- 10 # Zachodniopomorskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Lubuskie"] <- 11 # Lubuskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Dolnośląskie"] <- 12 # Dolnośląskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Opolskie"] <- 13 # Opolskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Kujawsko-Pomorskie"] <- 14 # Kujawsko-Pomorskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Warmińsko-Mazurskie"] <- 15 # Warmińsko-Mazurskie
sf.POL$REG_ID[sf.POL$NAME_1 == "Pomorskie"] <- 16 # Pomorskie
process_and_plot(sf.POL, "POL")
rm(sf.POL)

## Portugal  ----
sf.PRT <- subset(g, GID_0 == "PRT")
g <- subset(g, GID_0 != "PRT")
table(sf.PRT$NAME_1)
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Aveiro"] <- 1 # Aveiro
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Azores"] <- 2 # Azores
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Beja"] <- 3 # Beja
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Braga"] <- 4 # Braga
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Bragança"] <- 5 # Bragança
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Castelo Branco"] <- 6 # Castelo Branco
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Coimbra"] <- 7 # Coimbra
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Évora"] <- 8 # Évora
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Faro"] <- 9 # Faro
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Guarda"] <- 10 # Guarda
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Leiria"] <- 11 # Leiria
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Lisboa"] <- 12 # Lisboa
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Madeira"] <- 13 # Madeira
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Portalegre"] <- 14 # Portalegre
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Porto"] <- 15 # Porto
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Santarém"] <- 16 # Santarém
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Setúbal"] <- 17 # Setúbal
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Viana do Castelo"] <- 18 # Viana do Castelo
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Vila Real"] <- 19 # Vila Real
sf.PRT$REG_ID[sf.PRT$NAME_1 == "Viseu"] <- 20 # Viseu
process_and_plot(sf.PRT, "PRT")
rm(sf.PRT)


## Paraguay ----
sf.PRY <- subset(g, GID_0 == "PRY")
g <- subset(g, GID_0 != "PRY")
table(sf.PRY$NAME_1)
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Asunción"] <- 0 # Asunción
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Concepción"] <- 1 # Concepción
sf.PRY$REG_ID[sf.PRY$NAME_1 == "San Pedro"] <- 2 # San Pedro
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Cordillera"] <- 3 # Cordillera
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Guairá"] <- 4 # Guairá
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Caaguazú"] <- 5 # Caaguazú
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Caazapá"] <- 6 # Caazapá
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Itapúa"] <- 7 # Itapúa
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Misiones"] <- 8 # Misiones
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Paraguarí"] <- 9 # Paraguarí
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Alto Paraná"] <- 10 # Alto Paraná
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Central"] <- 11 # Central
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Ñeembucú"] <- 12 # Ñeembucú
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Amambay"] <- 13 # Amambay
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Canindeyú"] <- 14 # Canindeyú
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Presidente Hayes"] <- 15 # Presidente Hayes
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Boquerón"] <- 16 # Boquerón
sf.PRY$REG_ID[sf.PRY$NAME_1 == "Alto Paraguay"] <- 17 # Alto Paraguay
process_and_plot(sf.PRY, "PRY")
rm(sf.PRY)

## Romania  ----
sf.ROU <- subset(g, GID_0 == "ROU")
g <- subset(g, GID_0 != "ROU")
table(sf.ROU$NAME_1)
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Bacău"] <- 11 # Bacău
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Botoșani"] <- 12 # Botoșani
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Iași"] <- 13 # Iași
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Neamț"] <- 14 # Neamț
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Suceava"] <- 15 # Suceava
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Vaslui"] <- 16 # Vaslui
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Brăila"] <- 21 # Brăila
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Buzău"] <- 22 # Buzău
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Constanța"] <- 23 # Constanța
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Galați"] <- 24 # Galați
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Tulcea"] <- 25 # Tulcea
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Vrancea"] <- 26 # Vrancea
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Argeș"] <- 31 # Argeș
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Călărași"] <- 32 # Călărași
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Dâmbovița"] <- 33 # Dâmbovița
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Giurgiu"] <- 34 # Giurgiu
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Ialomița"] <- 35 # Ialomița
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Prahova"] <- 36 # Prahova
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Teleorman"] <- 37 # Teleorman
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Dolj"] <- 41 # Dolj
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Gorj"] <- 42 # Gorj
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Mehedinți"] <- 43 # Mehedinți
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Olt"] <- 44 # Olt
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Vâlcea"] <- 45 # Vâlcea
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Arad"] <- 51 # Arad
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Caraș-Severin"] <- 52 # Caraș-Severin
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Hunedoara"] <- 53 # Hunedoara
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Timiș"] <- 54 # Timiș
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Bihor"] <- 61 # Bihor
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Bistrița-Năsăud"] <- 62 # Bistrița-Năsăud
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Cluj"] <- 63 # Cluj
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Maramureș"] <- 64 # Maramureș
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Satu Mare"] <- 65 # Satu Mare
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Sălaj"] <- 66 # Sălaj
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Alba"] <- 71 # Alba
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Brașov"] <- 72 # Brașov
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Covasna"] <- 73 # Covasna
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Harghita"] <- 74 # Harghita
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Mureș"] <- 75 # Mureș
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Sibiu"] <- 76 # Sibiu
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Bucharest"] <- 81 # Bucharest
sf.ROU$REG_ID[sf.ROU$NAME_1 == "Ilfov"] <- 82 # Ilfov
process_and_plot(sf.ROU, "ROU")
rm(sf.ROU)

## Russia  ----
sf.RUS <- subset(g, GID_0 == "RUS")
g <- subset(g, GID_0 != "RUS")
table(sf.RUS$NAME_3)
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Altay"] <- 1 # Altay
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Gorno-Altay"] <- 2 # Gorno-Altay
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Amur"] <- 3 # Amur
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Arkhangel'sk"] <- 4 # Arkhangel'sk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Astrakhan'"] <- 5 # Astrakhan'
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Bashkortostan"] <- 6 # Bashkortostan
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Belgorod"] <- 7 # Belgorod
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Bryansk"] <- 8 # Bryansk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Buryat"] <- 9 # Buryat
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Chechnya"] <- 10 # Chechnya
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Chelyabinsk"] <- 11 # Chelyabinsk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Chukot"] <- 12 # Chukot
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Chuvash"] <- 13 # Chuvash
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Dagestan"] <- 14 # Dagestan
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Irkutsk"] <- 15 # Irkutsk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Ivanovo"] <- 16 # Ivanovo
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Yevrey"] <- 17 # Yevrey
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Kabardin-Balkar"] <- 18 # Kabardin-Balkar
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Kaliningrad"] <- 19 # Kaliningrad
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Kaluga"] <- 20 # Kaluga
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Kamchatka"] <- 21 # Kamchatka
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Karachay-Cherkess"] <- 22 # Karachay-Cherkess
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Kemerovo"] <- 23 # Kemerovo
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Khabarovsk"] <- 24 # Khabarovsk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Khanty-Mansiy"] <- 25 # Khanty-Mansiy
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Kirov"] <- 26 # Kirov
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Komi"] <- 27 # Komi
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Kostroma"] <- 28 # Kostroma
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Krasnodar"] <- 29 # Krasnodar
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Krasnoyarsk"] <- 30 # Krasnoyarsk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Kurgan"] <- 31 # Kurgan
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Kursk"] <- 32 # Kursk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Leningrad"] <- 33 # Leningrad
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Lipetsk"] <- 34 # Lipetsk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Magadan"] <- 35 # Magadan
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Moscow City"] <- 36 # Moscow City
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Moskva"] <- 37 # Moskva
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Murmansk"] <- 38 # Murmansk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Nenets"] <- 39 # Nenets
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Nizhegorod"] <- 40 # Nizhegorod
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Novgorod"] <- 41 # Novgorod
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Novosibirsk"] <- 42 # Novosibirsk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Omsk"] <- 43 # Omsk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Orel"] <- 44 # Orel
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Orenburg"] <- 45 # Orenburg
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Penza"] <- 46 # Penza
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Perm'"] <- 47 # Perm'
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Primor'ye"] <- 48 # Primor'ye
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Pskov"] <- 49 # Pskov
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Adygey"] <- 50 # Adygey
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Ingush"] <- 51 # Ingush
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Kalmyk"] <- 52 # Kalmyk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Karelia"] <- 53 # Karelia
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Khakass"] <- 54 # Khakass
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Mariy-El"] <- 55 # Mariy-El
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Mordovia"] <- 56 # Mordovia
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Sakha"] <- 57 # Sakha
sf.RUS$REG_ID[sf.RUS$NAME_1 == "North Ossetia"] <- 58 # North Ossetia
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Tatarstan"] <- 59 # Tatarstan
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Tuva"] <- 60 # Tuva
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Rostov"] <- 61 # Rostov
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Ryazan'"] <- 62 # Ryazan'
sf.RUS$REG_ID[sf.RUS$NAME_1 == "City of St. Petersburg"] <- 63 # City of St. Petersburg
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Sakhalin"] <- 64 # Sakhalin
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Samara"] <- 65 # Samara
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Saratov"] <- 66 # Saratov
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Smolensk"] <- 67 # Smolensk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Stavropol'"] <- 68 # Stavropol'
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Sverdlovsk"] <- 69 # Sverdlovsk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Tambov"] <- 70 # Tambov
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Tomsk"] <- 71 # Tomsk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Tula"] <- 72 # Tula
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Tver'"] <- 73 # Tver'
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Tyumen'"] <- 74 # Tyumen'
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Udmurt"] <- 75 # Udmurt
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Ul'yanovsk"] <- 76 # Ul'yanovsk
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Vladimir"] <- 77 # Vladimir
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Volgograd"] <- 78 # Volgograd
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Vologda"] <- 79 # Vologda
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Voronezh"] <- 80 # Voronezh
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Yamal-Nenets"] <- 81 # Yamal-Nenets
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Yaroslavl'"] <- 82 # Yaroslavl'
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Zabaykal'ye"] <- 83 # Zabaykal'ye
sf.RUS$REG_ID[sf.RUS$NAME_1 == "Zabaykal'ye"] <- 83 # Zabaykal'ye
# sf.UKR$REG_ID[sf.UKR$NAME_1 == "Crimea"] <- 84 #Crimea
# sf.UKR$REG_ID[sf.UKR$NAME_1 == "Sevastopol'"] <- 85 #Sevastopol'

# Adjust for region 12
x <- sf.RUS
x.12 <- x[x$REG_ID == 12, ]
x <- x[!x$REG_ID == 12, ]
x.12.out <- unique(subset(st_drop_geometry(x.12), select = c(REG_ID, GID_0)))
x.12.out$geom <- st_combine(x.12)
x_agg <- x %>%
  group_by(REG_ID, GID_0) %>%
  summarize(geom = st_union(geom), .groups = "drop")
x.12.out <- st_as_sf(x.12.out)
x_agg <- rbind(x_agg, x.12.out)
x_agg <- subset(x_agg, select = c(GID_0, REG_ID, geom))
saveRDS(x_agg, file = paste0("Data/inter/19_wrp/world_admin_boundaries/", "RUS", ".rds"))
rm(x_agg)
rm(sf.RUS)

## Rwanda  ----
sf.RWA <- subset(g, GID_0 == "RWA")
g <- subset(g, GID_0 != "RWA")
table(sf.RWA$NAME_2)
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Bugesera"] <- 1 # Bugesera
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Gatsibo"] <- 2 # Gatsibo
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Kayonza"] <- 3 # Kayonza
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Kirehe"] <- 4 # Kirehe
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Ngoma"] <- 5 # Ngoma
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Nyagatare"] <- 6 # Nyagatare
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Rwamagana"] <- 7 # Rwamagana
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Gasabo"] <- 8 # Gasabo
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Kicukiro"] <- 9 # Kicukiro
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Nyarugenge"] <- 10 # Nyarugenge
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Burera"] <- 11 # Burera
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Gakenke"] <- 12 # Gakenke
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Gicumbi"] <- 13 # Gicumbi
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Musanze"] <- 14 # Musanze
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Rulindo"] <- 15 # Rulindo
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Gisagara"] <- 16 # Gisagara
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Huye"] <- 17 # Huye
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Kamonyi"] <- 18 # Kamonyi
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Muhanga"] <- 19 # Muhanga
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Nyamagabe"] <- 20 # Nyamagabe
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Nyanza"] <- 21 # Nyanza
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Nyaruguru"] <- 22 # Nyaruguru
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Ruhango"] <- 23 # Ruhango
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Karongi"] <- 24 # Karongi
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Ngororero"] <- 25 # Ngororero
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Nyabihu"] <- 26 # Nyabihu
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Nyamasheke"] <- 27 # Nyamasheke
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Rubavu"] <- 28 # Rubavu
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Rusizi"] <- 29 # Rusizi
sf.RWA$REG_ID[sf.RWA$NAME_2 == "Rutsiro"] <- 30 # Rutsiro
process_and_plot(sf.RWA, "RWA")
rm(sf.RWA)

## Saudi Arabia  ----
sf.SAU <- subset(g, GID_0 == "SAU")
g <- subset(g, GID_0 != "SAU")
table(sf.SAU$NAME_1)
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Ar Riyad"] <- 1 # Ar Riyad
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Al Qassim"] <- 2 # Al Qassim
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Makkah"] <- 3 # Makkah
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Al Madinah"] <- 4 # Al Madinah
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Ash-Sharqīyah"] <- 5 # Ash-Sharqīyah
sf.SAU$REG_ID[sf.SAU$NAME_1 == "'Asir"] <- 6 #' Asir
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Tabuk"] <- 7 # Tabuk
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Ḥaʼil"] <- 8 # Ḥaʼil
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Najran"] <- 9 # Najran
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Al Bahah"] <- 10 # Al Bahah
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Jizan"] <- 11 # Jizan
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Al Hudud ash Shamaliyah"] <- 12 # Al Hudud ash Shamaliyah
sf.SAU$REG_ID[sf.SAU$NAME_1 == "Al Jawf"] <- 13 # Al Jawf
process_and_plot(sf.SAU, "SAU")
rm(sf.SAU)

## Senegal  ----
sf.SEN <- subset(g, GID_0 == "SEN")
g <- subset(g, GID_0 != "SEN")
table(sf.SEN$NAME_1)
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Dakar"] <- 1 # Dakar
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Diourbel"] <- 2 # Diourbel
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Fatick"] <- 3 # Fatick
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Kaolack"] <- 5 # Kaolack
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Kolda"] <- 7 # Kolda
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Louga"] <- 8 # Louga
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Matam"] <- 9 # Matam
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Saint-Louis"] <- 10 # Saint-Louis
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Tambacounda"] <- 12 # Tambacounda
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Thiès"] <- 13 # Thiès
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Ziguinchor"] <- 14 # Ziguinchor
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Kaffrine"] <- 15 # Kaffrine
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Kédougou"] <- 16 # Kédougou
sf.SEN$REG_ID[sf.SEN$NAME_1 == "Sédhiou"] <- 17 # Sédhiou
process_and_plot(sf.SEN, "SEN")
rm(sf.SEN)

## Sierra Leone  ----
sf.SLE <- subset(g, GID_0 == "SLE")
g <- subset(g, GID_0 != "SLE")
table(sf.SLE$NAME_2)
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Bo"] <- 1 # Bo
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Bombali"] <- 2 # Bombali
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Bonthe"] <- 3 # Bonthe
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Kailahun"] <- 4 # Kailahun
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Kambia"] <- 5 # Kambia
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Kenema"] <- 6 # Kenema
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Koinadugu"] <- 7 # Koinadugu
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Kono"] <- 8 # Kono
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Moyamba"] <- 9 # Moyamba
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Port Loko"] <- 10 # Port Loko
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Pujehun"] <- 11 # Pujehun
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Tonkolili"] <- 12 # Tonkolili
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Western Urban"] <- 13 # Western Urban
sf.SLE$REG_ID[sf.SLE$NAME_2 == "Western Rural"] <- 14 # Western Rural
process_and_plot(sf.SLE, "SLE")
rm(sf.SLE)

## El Salvador  ----
sf.SLV <- subset(g, GID_0 == "SLV")
g <- subset(g, GID_0 != "SLV")
table(sf.SLV$NAME_1)
sf.SLV$REG_ID[sf.SLV$NAME_1 == "Ahuachapán"] <- 1 # Ahuachapán
sf.SLV$REG_ID[sf.SLV$NAME_1 == "Cabañas"] <- 2 # Cabañas
sf.SLV$REG_ID[sf.SLV$NAME_1 == "Chalatenango"] <- 3 # Chalatenango
sf.SLV$REG_ID[sf.SLV$NAME_1 == "Cuscatlán"] <- 4 # Cuscatlán
sf.SLV$REG_ID[sf.SLV$NAME_1 == "La Libertad"] <- 5 # La Libertad
sf.SLV$REG_ID[sf.SLV$NAME_1 == "La Paz"] <- 6 # La Paz
sf.SLV$REG_ID[sf.SLV$NAME_1 == "La Unión"] <- 7 # La Unión
sf.SLV$REG_ID[sf.SLV$NAME_1 == "Morazán"] <- 8 # Morazán
sf.SLV$REG_ID[sf.SLV$NAME_1 == "San Miguel"] <- 9 # San Miguel
sf.SLV$REG_ID[sf.SLV$NAME_1 == "San Salvador"] <- 10 # San Salvador
sf.SLV$REG_ID[sf.SLV$NAME_1 == "San Vicente"] <- 11 # San Vicente
sf.SLV$REG_ID[sf.SLV$NAME_1 == "Santa Ana"] <- 12 # Santa Ana
sf.SLV$REG_ID[sf.SLV$NAME_1 == "Sonsonate"] <- 13 # Sonsonate
sf.SLV$REG_ID[sf.SLV$NAME_1 == "Usulután"] <- 14 # Usulután
process_and_plot(sf.SLV, "SLV")
rm(sf.SLV)

## Serbia  ----
sf.SRB <- subset(g, GID_0 == "SRB")
g <- subset(g, GID_0 != "SRB")
table(sf.SRB$NAME_1)
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Grad Beograd"] <- 1 # Grad Beograd
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Severno-Bački"] <- 2 # Severno-Bački
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Srednje-Banatski"] <- 3 # Srednje-Banatski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Severno-Banatski"] <- 4 # Severno-Banatski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Južno-Banatski"] <- 5 # Južno-Banatski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Zapadno-Bački"] <- 6 # Zapadno-Bački
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Južno-Bački"] <- 7 # Južno-Bački
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Sremski"] <- 8 # Sremski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Mačvanski"] <- 9 # Mačvanski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Kolubarski"] <- 10 # Kolubarski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Podunavski"] <- 11 # Podunavski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Braničevski"] <- 12 # Braničevski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Šumadijski"] <- 13 # Šumadijski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Pomoravski"] <- 14 # Pomoravski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Borski"] <- 15 # Borski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Zaječarski"] <- 16 # Zaječarski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Zlatiborski"] <- 17 # Zlatiborski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Moravički"] <- 18 # Moravički
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Raški"] <- 19 # Raški
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Rasinski"] <- 20 # Rasinski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Nišavski"] <- 21 # Nišavski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Toplički"] <- 22 # Toplički
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Pirotski"] <- 23 # Pirotski
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Jablanički"] <- 24 # Jablanički
sf.SRB$REG_ID[sf.SRB$NAME_1 == "Pčinjski"] <- 25 # Pčinjski
process_and_plot(sf.SRB, "SRB")
rm(sf.SRB)

## Slovakia  ----
sf.SVK <- subset(g, GID_0 == "SVK")
g <- subset(g, GID_0 != "SVK")
table(sf.SVK$NAME_1)
sf.SVK$REG_ID[sf.SVK$NAME_1 == "Bratislavský"] <- 1 # Bratislavský
sf.SVK$REG_ID[sf.SVK$NAME_1 == "Trnavský"] <- 2 # Trnavský
sf.SVK$REG_ID[sf.SVK$NAME_1 == "Trenčiansky"] <- 3 # Trenčiansky
sf.SVK$REG_ID[sf.SVK$NAME_1 == "Nitriansky"] <- 4 # Nitriansky
sf.SVK$REG_ID[sf.SVK$NAME_1 == "Žilinský"] <- 5 # Žilinský
sf.SVK$REG_ID[sf.SVK$NAME_1 == "Banskobystrický"] <- 6 # Banskobystrický
sf.SVK$REG_ID[sf.SVK$NAME_1 == "Prešovský"] <- 7 # Prešovský
sf.SVK$REG_ID[sf.SVK$NAME_1 == "Košický"] <- 8 # Košický
process_and_plot(sf.SVK, "SVK")
rm(sf.SVK)

## Slovenia  ----
sf.SVN <- subset(g, GID_0 == "SVN")
g <- subset(g, GID_0 != "SVN")
table(sf.SVN$NAME_1)
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Pomurska"] <- 1 # Pomurska
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Podravska"] <- 2 # Podravska
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Koroška"] <- 3 # Koroška
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Savinjska"] <- 4 # Savinjska
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Zasavska"] <- 5 # Zasavska
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Spodnjeposavska"] <- 6 # Spodnjeposavska
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Gorenjska"] <- 7 # Gorenjska
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Notranjsko-kraška"] <- 8 # Notranjsko-kraška
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Goriška"] <- 9 # Goriška
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Obalno-kraška"] <- 10 # Obalno-kraška
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Jugovzhodna Slovenija"] <- 11 # Jugovzhodna Slovenija
sf.SVN$REG_ID[sf.SVN$NAME_1 == "Osrednjeslovenska"] <- 12 # Osrednjeslovenska
process_and_plot(sf.SVN, "SVN")
rm(sf.SVN)

## Sweden  ----
sf.SWE <- subset(g, GID_0 == "SWE")
g <- subset(g, GID_0 != "SWE")
table(sf.SWE$NAME_1)
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Blekinge"] <- 1 # Blekinge
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Dalarna"] <- 2 # Dalarna
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Gävleborg"] <- 3 # Gävleborg
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Gotland"] <- 4 # Gotland
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Halland"] <- 5 # Halland
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Jämtland"] <- 6 # Jämtland
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Jönköping"] <- 7 # Jönköping
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Kalmar"] <- 8 # Kalmar
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Kronoberg"] <- 9 # Kronoberg
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Norrbotten"] <- 10 # Norrbotten
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Orebro"] <- 11 # Orebro
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Östergötland"] <- 12 # Östergötland
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Skåne"] <- 13 # Skåne
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Södermanland"] <- 14 # Södermanland
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Stockholm"] <- 15 # Stockholm
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Uppsala"] <- 16 # Uppsala
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Värmland"] <- 17 # Värmland
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Västerbotten"] <- 18 # Västerbotten
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Västernorrland"] <- 19 # Västernorrland
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Västmanland"] <- 20 # Västmanland
sf.SWE$REG_ID[sf.SWE$NAME_1 == "Västra Götaland"] <- 21 # Västra Götaland
process_and_plot(sf.SWE, "SWE")
rm(sf.SWE)

## Swaziland  ----
sf.SWZ <- subset(g, GID_0 == "SWZ")
g <- subset(g, GID_0 != "SWZ")
table(sf.SWZ$NAME_1)
sf.SWZ$REG_ID[sf.SWZ$NAME_1 == "Hhohho"] <- 1 # Hhohho
sf.SWZ$REG_ID[sf.SWZ$NAME_1 == "Lubombo"] <- 2 # Lubombo
sf.SWZ$REG_ID[sf.SWZ$NAME_1 == "Manzini"] <- 3 # Manzini
sf.SWZ$REG_ID[sf.SWZ$NAME_1 == "Shiselweni"] <- 4 # Shiselweni
process_and_plot(sf.SWZ, "SWZ")
rm(sf.SWZ)

## Chad  ----
sf.TCD <- subset(g, GID_0 == "TCD")
g <- subset(g, GID_0 != "TCD")
table(sf.TCD$NAME_1)
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Batha"] <- 1 # Batha

sf.TCD$REG_ID[sf.TCD$NAME_1 == "Borkou"] <- 2 # Borkou
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Tibesti"] <- 2 # Tibesti
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Ennedi Est"] <- 2 # Ennedi Est
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Ennedi Ouest"] <- 2 # Ennedi Ouest

sf.TCD$REG_ID[sf.TCD$NAME_1 == "Chari-Baguirmi"] <- 3 # Chari-Baguirmi
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Ville de N'Djamena"] <- 4 # Ville de N'Djamena
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Guéra"] <- 5 # Guéra
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Hadjer-Lamis"] <- 6 # Hadjer-Lamis
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Kanem"] <- 7 # Kanem
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Lac"] <- 8 # Lac
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Logone Oriental"] <- 9 # Logone Oriental
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Logone Occidental"] <- 10 # Logone Occidental
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Mandoul"] <- 11 # Mandoul
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Mayo-Kebbi Est"] <- 12 # Mayo-Kebbi Est
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Mayo-Kebbi Ouest"] <- 13 # Mayo-Kebbi Ouest
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Moyen-Chari"] <- 14 # Moyen-Chari
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Ouaddaï"] <- 15 # Ouaddaï
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Salamat"] <- 16 # Salamat
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Tandjilé"] <- 17 # Tandjilé
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Wadi Fira"] <- 18 # Wadi Fira
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Barh el Ghazel"] <- 19 # Barh el Ghazel
sf.TCD$REG_ID[sf.TCD$NAME_1 == "Sila"] <- 20 # Sila
# Bibiane: *not sure about 21-23, appear in sf.TCD but not in Gallup
# Alex: coded them as part of "Borkou-Ennedi-Tibesti"
process_and_plot(sf.TCD, "TCD")
rm(sf.TCD)

## Togo  ----
sf.TGO <- subset(g, GID_0 == "TGO")
g <- subset(g, GID_0 != "TGO")
table(sf.TGO$NAME_2)
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Blitta"] <- 1 # Blitta
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Sotouboua"] <- 2 # Sotouboua
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Tchamba"] <- 3 # Tchamba
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Tchaudjo"] <- 4 # Tchaudjo
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Assoli"] <- 5 # Assoli
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Bassar"] <- 6 # Bassar
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Bimah"] <- 7 # Bimah
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Dankpen"] <- 8 # Dankpen
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Doufelgou"] <- 9 # Doufelgou
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Kéran"] <- 10 # Kéran
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Kozah"] <- 11 # Kozah
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Avé"] <- 12 # Avé
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Bas-Mono"] <- 13 # Bas-Mono
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Golfe"] <- 14 # Golfe
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Lacs"] <- 15 # Lacs
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Lomé"] <- 16 # Lomé
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Vo"] <- 17 # Vo
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Yoto"] <- 18 # Yoto
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Zio"] <- 19 # Zio
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Agou"] <- 20 # Agou
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Akébou"] <- 21 # Akébou
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Amou"] <- 22 # Amou
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Anié"] <- 23 # Anié
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Danyi"] <- 24 # Danyi
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Est-Mono"] <- 25 # Est-Mono
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Haho"] <- 26 # Haho
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Kloto"] <- 27 # Kloto
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Kpélé"] <- 28 # Kpélé
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Moyen-Mono"] <- 29 # Moyen-Mono
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Ogou"] <- 30 # Ogou
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Wawa"] <- 31 # Wawa
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Cinkassé"] <- 32 # Cinkassé
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Kpendjal"] <- 33 # Kpendjal

sf.TGO$REG_ID[sf.TGO$NAME_2 == "Oti"] <- 34 # Oti
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Oti-Sud"] <- 34 # Oti-Sud

sf.TGO$REG_ID[sf.TGO$NAME_2 == "Tandjouaré"] <- 35 # Tandjouaré
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Tône"] <- 36 # Tône

# https://trip-suggest.com/togo/savanes/naki-ouest
sf.TGO$REG_ID[sf.TGO$NAME_2 == "Naki-Ouest"] <- 36 # Naki-Ouest


# sf.TGO$REG_ID[sf.TGO$NAME_2 == "Agoe-Nyive"] <- 37 #Agoe-Nyive
# sf.TGO$REG_ID[sf.TGO$NAME_2 == "Mô"] <- 38 #Mô
# Bibiane: *not sure about 37-40, appear in sf.TGO but not in Gallup
# Alex: if not in gallup region, we can exclude them here; and I fixed some above

process_and_plot(sf.TGO, "TGO")
rm(sf.TGO)

## Thailand----
sf.THA <- subset(g, GID_0 == "THA")
g <- subset(g, GID_0 != "THA")
table(sf.THA$NAME_1)
sf.THA$REG_ID[sf.THA$NAME_1 == "Chiang Mai"] <- 1 # Chiang Mai
sf.THA$REG_ID[sf.THA$NAME_1 == "Chiang Rai"] <- 2 # Chiang Rai
sf.THA$REG_ID[sf.THA$NAME_1 == "Lampang"] <- 3 # Lampang
sf.THA$REG_ID[sf.THA$NAME_1 == "Lamphun"] <- 4 # Lamphun
sf.THA$REG_ID[sf.THA$NAME_1 == "Mae Hong Son"] <- 5 # Mae Hong Son
sf.THA$REG_ID[sf.THA$NAME_1 == "Nan"] <- 6 # Nan
sf.THA$REG_ID[sf.THA$NAME_1 == "Phayao"] <- 7 # Phayao
sf.THA$REG_ID[sf.THA$NAME_1 == "Phrae"] <- 8 # Phrae
sf.THA$REG_ID[sf.THA$NAME_1 == "Uttaradit"] <- 9 # Uttaradit
sf.THA$REG_ID[sf.THA$NAME_1 == "Kanchanaburi"] <- 10 # Kanchanaburi
sf.THA$REG_ID[sf.THA$NAME_1 == "Phetchaburi"] <- 11 # Phetchaburi
sf.THA$REG_ID[sf.THA$NAME_1 == "Prachuap Khiri Khan"] <- 12 # Prachuap Khiri Khan
sf.THA$REG_ID[sf.THA$NAME_1 == "Ratchaburi"] <- 13 # Ratchaburi
sf.THA$REG_ID[sf.THA$NAME_1 == "Tak"] <- 14 # Tak
sf.THA$REG_ID[sf.THA$NAME_1 == "Amnat Charoen"] <- 15 # Amnat Charoen
sf.THA$REG_ID[sf.THA$NAME_1 == "Buri Ram"] <- 16 # Buri Ram
sf.THA$REG_ID[sf.THA$NAME_1 == "Chaiyaphum"] <- 17 # Chaiyaphum
sf.THA$REG_ID[sf.THA$NAME_1 == "Kalasin"] <- 18 # Kalasin
sf.THA$REG_ID[sf.THA$NAME_1 == "Khon Kaen"] <- 19 # Khon Kaen
sf.THA$REG_ID[sf.THA$NAME_1 == "Loei"] <- 20 # Loei
sf.THA$REG_ID[sf.THA$NAME_1 == "Maha Sarakham"] <- 21 # Maha Sarakham
sf.THA$REG_ID[sf.THA$NAME_1 == "Mukdahan"] <- 22 # Mukdahan
sf.THA$REG_ID[sf.THA$NAME_1 == "Nakhon Phanom"] <- 23 # Nakhon Phanom
sf.THA$REG_ID[sf.THA$NAME_1 == "Nakhon Ratchasima"] <- 24 # Nakhon Ratchasima
sf.THA$REG_ID[sf.THA$NAME_1 == "Nong Bua Lam Phu"] <- 25 # Nong Bua Lam Phu
sf.THA$REG_ID[sf.THA$NAME_1 == "Nong Khai"] <- 26 # Nong Khai
sf.THA$REG_ID[sf.THA$NAME_1 == "Roi Et"] <- 27 # Roi Et
sf.THA$REG_ID[sf.THA$NAME_1 == "Sakon Nakhon"] <- 28 # Sakon Nakhon
sf.THA$REG_ID[sf.THA$NAME_1 == "Si Sa Ket"] <- 29 # Si Sa Ket
sf.THA$REG_ID[sf.THA$NAME_1 == "Surin"] <- 30 # Surin
sf.THA$REG_ID[sf.THA$NAME_1 == "Ubon Ratchathani"] <- 31 # Ubon Ratchathani
sf.THA$REG_ID[sf.THA$NAME_1 == "Udon Thani"] <- 32 # Udon Thani
sf.THA$REG_ID[sf.THA$NAME_1 == "Yasothon"] <- 33 # Yasothon
sf.THA$REG_ID[sf.THA$NAME_1 == "Ang Thong"] <- 34 # Ang Thong
sf.THA$REG_ID[sf.THA$NAME_1 == "Phra Nakhon Si Ayutthaya"] <- 35 # Phra Nakhon Si Ayutthaya
sf.THA$REG_ID[sf.THA$NAME_1 == "Bangkok Metropolis"] <- 36 # Bangkok Metropolis
sf.THA$REG_ID[sf.THA$NAME_1 == "Chai Nat"] <- 37 # Chai Nat
sf.THA$REG_ID[sf.THA$NAME_1 == "Kamphaeng Phet"] <- 38 # Kamphaeng Phet
sf.THA$REG_ID[sf.THA$NAME_1 == "Lop Buri"] <- 39 # Lop Buri
sf.THA$REG_ID[sf.THA$NAME_1 == "Nakhon Nayok"] <- 40 # Nakhon Nayok
sf.THA$REG_ID[sf.THA$NAME_1 == "Nakhon Pathom"] <- 41 # Nakhon Pathom
sf.THA$REG_ID[sf.THA$NAME_1 == "Nakhon Sawan"] <- 42 # Nakhon Sawan
sf.THA$REG_ID[sf.THA$NAME_1 == "Nonthaburi"] <- 43 # Nonthaburi
sf.THA$REG_ID[sf.THA$NAME_1 == "Pathum Thani"] <- 44 # Pathum Thani
sf.THA$REG_ID[sf.THA$NAME_1 == "Phetchabun"] <- 45 # Phetchabun
sf.THA$REG_ID[sf.THA$NAME_1 == "Phichit"] <- 46 # Phichit
sf.THA$REG_ID[sf.THA$NAME_1 == "Phitsanulok"] <- 47 # Phitsanulok
sf.THA$REG_ID[sf.THA$NAME_1 == "Sukhothai"] <- 48 # Sukhothai
sf.THA$REG_ID[sf.THA$NAME_1 == "Samut Prakan"] <- 49 # Samut Prakan
sf.THA$REG_ID[sf.THA$NAME_1 == "Samut Sakhon"] <- 50 # Samut Sakhon
sf.THA$REG_ID[sf.THA$NAME_1 == "Samut Songkhram"] <- 51 # Samut Songkhram
sf.THA$REG_ID[sf.THA$NAME_1 == "Saraburi"] <- 52 # Saraburi
sf.THA$REG_ID[sf.THA$NAME_1 == "Sing Buri"] <- 53 # Sing Buri
sf.THA$REG_ID[sf.THA$NAME_1 == "Suphan Buri"] <- 54 # Suphan Buri
sf.THA$REG_ID[sf.THA$NAME_1 == "Uthai Thani"] <- 55 # Uthai Thani
sf.THA$REG_ID[sf.THA$NAME_1 == "Chachoengsao"] <- 56 # Chachoengsao
sf.THA$REG_ID[sf.THA$NAME_1 == "Chanthaburi"] <- 57 # Chanthaburi
sf.THA$REG_ID[sf.THA$NAME_1 == "Chon Buri"] <- 58 # Chon Buri
sf.THA$REG_ID[sf.THA$NAME_1 == "Prachin Buri"] <- 59 # Prachin Buri
sf.THA$REG_ID[sf.THA$NAME_1 == "Rayong"] <- 60 # Rayong
sf.THA$REG_ID[sf.THA$NAME_1 == "Sa Kaeo"] <- 61 # Sa Kaeo
sf.THA$REG_ID[sf.THA$NAME_1 == "Trat"] <- 62 # Trat
sf.THA$REG_ID[sf.THA$NAME_1 == "Chumphon"] <- 63 # Chumphon
sf.THA$REG_ID[sf.THA$NAME_1 == "Krabi"] <- 64 # Krabi
sf.THA$REG_ID[sf.THA$NAME_1 == "Nakhon Si Thammarat"] <- 65 # Nakhon Si Thammarat
sf.THA$REG_ID[sf.THA$NAME_1 == "Narathiwat"] <- 66 # Narathiwat
sf.THA$REG_ID[sf.THA$NAME_1 == "Pattani"] <- 67 # Pattani
sf.THA$REG_ID[sf.THA$NAME_1 == "Phangnga"] <- 68 # Phangnga
sf.THA$REG_ID[sf.THA$NAME_1 == "Phatthalung"] <- 69 # Phatthalung
sf.THA$REG_ID[sf.THA$NAME_1 == "Phuket"] <- 70 # Phuket
sf.THA$REG_ID[sf.THA$NAME_1 == "Ranong"] <- 71 # Ranong
sf.THA$REG_ID[sf.THA$NAME_1 == "Satun"] <- 72 # Satun
sf.THA$REG_ID[sf.THA$NAME_1 == "Songkhla"] <- 73 # Songkhla
sf.THA$REG_ID[sf.THA$NAME_1 == "Surat Thani"] <- 74 # Surat Thani
sf.THA$REG_ID[sf.THA$NAME_1 == "Trang"] <- 75 # Trang
sf.THA$REG_ID[sf.THA$NAME_1 == "Yala"] <- 76 # Yala
sf.THA$REG_ID[sf.THA$NAME_1 == "Bueng Kan"] <- 77 # Bueng Kan
process_and_plot(sf.THA, "THA")
rm(sf.THA)

## Tajikistan  ----
sf.TJK <- subset(g, GID_0 == "TJK")
g <- subset(g, GID_0 != "TJK")
table(sf.TJK$NAME_1)
sf.TJK$REG_ID[sf.TJK$NAME_1 == "Sughd"] <- 1 # Sughd
sf.TJK$REG_ID[sf.TJK$NAME_1 == "Khatlon"] <- 2 # Khatlon
sf.TJK$REG_ID[sf.TJK$NAME_1 == "Dushanbe"] <- 3 # Dushanbe
sf.TJK$REG_ID[sf.TJK$NAME_1 == "Districts of Republican Subordin"] <- 4 # Districts of Republican Subordin
sf.TJK$REG_ID[sf.TJK$NAME_1 == "Gorno-Badakhshan"] <- 5 # Gorno-Badakhshan
process_and_plot(sf.TJK, "TJK")
rm(sf.TJK)

## Tunisia  ----
sf.TUN <- subset(g, GID_0 == "TUN")
g <- subset(g, GID_0 != "TUN")
table(sf.TUN$NAME_1)
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Tunis"] <- 1 # Tunis
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Bizerte"] <- 2 # Bizerte
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Sousse"] <- 3 # Sousse
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Nabeul"] <- 4 # Nabeul
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Kairouan"] <- 5 # Kairouan
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Jendouba"] <- 6 # Jendouba
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Sfax"] <- 7 # Sfax
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Médenine"] <- 8 # Médenine
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Monastir"] <- 9 # Monastir
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Mahdia"] <- 10 # Mahdia
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Ben Arous (Tunis Sud)"] <- 11 # Ben Arous (Tunis Sud)
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Béja"] <- 12 # Béja
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Ariana"] <- 13 # Ariana
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Manubah"] <- 14 # Manubah
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Gabès"] <- 15 # Gabès
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Gafsa"] <- 16 # Gafsa
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Sidi Bou Zid"] <- 17 # Sidi Bou Zid
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Le Kef"] <- 18 # Le Kef
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Kassérine"] <- 19 # Kassérine
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Tozeur"] <- 20 # Tozeur
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Zaghouan"] <- 21 # Zaghouan
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Siliana"] <- 22 # Siliana
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Kebili"] <- 23 # Kebili
sf.TUN$REG_ID[sf.TUN$NAME_1 == "Tataouine"] <- 24 # Tataouine
process_and_plot(sf.TUN, "TUN")
rm(sf.TUN)

# TUR----
library(TRmaps)
sf.TUR <- tr_nuts2
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR10"] <- 1 # TR10-Istanbul
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR21"] <- 2 # TR21-Tekirdag
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR22"] <- 3 # TR22-Balikesir
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR31"] <- 4 # TR31-Izmir
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR32"] <- 5 # TR32-Aydin
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR33"] <- 6 # TR33-Manisa
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR41"] <- 7 # TR41-Bursa
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR42"] <- 8 # TR42-Kocaeli
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR51"] <- 9 # TR51-Ankara
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR52"] <- 10 # TR52-Konya
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR61"] <- 11 # TR61-Antalya
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR62"] <- 12 # TR62-Adana
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR63"] <- 13 # TR63-Hatay
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR71"] <- 14 # TR71-Kirikkale
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR72"] <- 15 # TR72-Kayseri
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR81"] <- 16 # TR81-Zonguldak
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR82"] <- 17 # TR82-Kastamonu
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR83"] <- 18 # TR83-Samsun
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TR90"] <- 19 # TR90-Trabzon
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TRA1"] <- 20 # TR91-Erzurum
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TRA2"] <- 21 # TRA2-Agri
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TRB1"] <- 22 # TRB1-Malatya
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TRB2"] <- 23 # TRB2-Van
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TRC1"] <- 24 # TRC1-Gaziantep
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TRC2"] <- 25 # TRC2-Sanliurfa
sf.TUR$REG_ID[sf.TUR$NUTS2_code == "TRC3"] <- 26 # TRC3-Mardin
sf.TUR <- subset(sf.TUR, select = c(REG_ID, geometry))
sf.TUR$GID_0 <- "TUR"
st_geometry(sf.TUR) <- "geom"
process_and_plot(sf.TUR, "tur")
rm(sf.TUR)

# TZA----
sf.TZA <- subset(g, GID_0 == "TZA")
g <- subset(g, GID_0 != "TZA")
table(sf.TZA$NAME_1)
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Dar es Salaam"] <- 1 # Dar es Salaam
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Dodoma"] <- 2 # Dodoma
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Tabora"] <- 3 # Tabora
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Singida"] <- 4 # Singida
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Shinyanga"] <- 5 # Shinyanga
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Ruvuma"] <- 6 # Ruvuma
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Rukwa"] <- 7 # Rukwa
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Pwani"] <- 8 # Pwani
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Kusini Unguja"] <- 9 # South Unguja
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Kaskazini Unguja"] <- 10 # North Unguja
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Kusini Pemba"] <- 11 # South Pemba
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Kaskazini Pemba"] <- 12 # North Pemba
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Mwanza"] <- 13 # Mwanza
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Mtwara"] <- 14 # Mtwara
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Morogoro"] <- 15 # Morogoro
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Mbeya"] <- 16 # Mbeya
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Mara"] <- 17 # Mara
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Manyara"] <- 18 # Manyara
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Songwe"] <- 19 # Songwe
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Kilimanjaro"] <- 20 # Kilimanjaro
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Kigoma"] <- 21 # Kigoma
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Kagera"] <- 22 # Kagera
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Iringa"] <- 23 # Iringa
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Mjini Magharibi"] <- 24 # Urban West
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Arusha"] <- 25 # Arusha
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Tanga"] <- 26 # Arusha
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Simiyu"] <- 27 # Simiyu
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Katavi"] <- 28 # Katavi
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Geita"] <- 29 # Geita
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Njombe"] <- 30 # Njombe
sf.TZA$REG_ID[sf.TZA$NAME_1 == "Lindi"] <- 31 # Lindi
process_and_plot(sf.TZA, "TZA")
rm(sf.TZA)

## Uganda ----
sf.UGA <- st_read(here("Data", "input", "world_admin_boundaries", "uganda", "uga_admbnda_adm1_ubos_20200824.shp"))
sf.UGA$GID_0 <- "UGA"
sf.UGA$REG_ID[sf.UGA$ADM1_EN == "Central"] <- 1 # Central Region
sf.UGA$REG_ID[sf.UGA$ADM1_EN == "Eastern"] <- 2 # Central Region
sf.UGA$REG_ID[sf.UGA$ADM1_EN == "Northern"] <- 3 # Norther Region
sf.UGA$REG_ID[sf.UGA$ADM1_EN == "Western"] <- 4 # Western Region
st_geometry(sf.UGA) <- "geom"
process_and_plot(sf.UGA, "UGA")
rm(sf.UGA)

# UKR----
sf.UKR <- subset(g, GID_0 == "UKR")
g <- subset(g, GID_0 != "UKR")
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Kiev City"] <- 1 # Kiev City
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Dnipropetrovs'k"] <- 2 # Dnipropetrovsk region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Donets'k"] <- 3 # Donetsk region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Zaporizhia"] <- 4 # Zaporizhzhya region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Luhans'k"] <- 5 # Luhansk region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Kharkiv"] <- 6 # Kharkiv region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Ivano-Frankivs'k"] <- 7 # Ivano-Frankivsk region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Volyn"] <- 8 # Volyn region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Zakarpattia"] <- 9 # Zakarpattya region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "L'viv"] <- 10 # Lviv region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Rivne"] <- 11 # Rivne region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Ternopil'"] <- 12 # Ternopil region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Chernivtsi"] <- 13 # Chernivtsi region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Zhytomyr"] <- 14 # Zhytomyr region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Kiev"] <- 15 # Kyiv region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Sumy"] <- 16 # Sumy region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Chernihiv"] <- 17 # Chernihiv region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Vinnytsya"] <- 18 # Vinnitsya region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Kirovohrad"] <- 19 # Kirovograd region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Poltava"] <- 20 # Poltava region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Khmel'nyts'kyy"] <- 21 # Khmelnitskiy region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Cherkasy"] <- 22 # Cherkasy region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Mykolayiv"] <- 24 # Mykolayiv region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Odessa"] <- 25 # Odessa region
sf.UKR$REG_ID[sf.UKR$NAME_1 == "Kherson"] <- 26 # Kherson region
process_and_plot(sf.UKR, "UKR")
rm(sf.UKR)


# URY----
sf.URY <- st_read(here("Data", "input", "world_admin_boundaries", "uruguay", "ury_admbnda_adm1_2020.shp"))
sf.URY$REG_ID[sf.URY$ADM1_ES == "Montevideo"] <- 1 # Montevideo
sf.URY$REG_ID[sf.URY$ADM1_ES == "Artigas"] <- 2 # Artigas
sf.URY$REG_ID[sf.URY$ADM1_ES == "Canelones"] <- 3 # Canelones
sf.URY$REG_ID[sf.URY$ADM1_ES == "Cerro Largo"] <- 4 # Cerro Largo
sf.URY$REG_ID[sf.URY$ADM1_ES == "Colonia"] <- 5 # Colonia
sf.URY$REG_ID[sf.URY$ADM1_ES == "Durazno"] <- 6 # Durazno
sf.URY$REG_ID[sf.URY$ADM1_ES == "Flores"] <- 7 # Flores
sf.URY$REG_ID[sf.URY$ADM1_ES == "Florida"] <- 8 # Florida
sf.URY$REG_ID[sf.URY$ADM1_ES == "Lavalleja"] <- 9 # Lavalleja
sf.URY$REG_ID[sf.URY$ADM1_ES == "Maldonado"] <- 10 # Maldonado
sf.URY$REG_ID[sf.URY$ADM1_ES == "Paysandú"] <- 11 # PaysandÃº
sf.URY$REG_ID[sf.URY$ADM1_ES == "Río Negro"] <- 12 # Rio Negro
sf.URY$REG_ID[sf.URY$ADM1_ES == "Rivera"] <- 13 # Rivera
sf.URY$REG_ID[sf.URY$ADM1_ES == "Rocha"] <- 14 # Rocha
sf.URY$REG_ID[sf.URY$ADM1_ES == "Salto"] <- 15 # Salto
sf.URY$REG_ID[sf.URY$ADM1_ES == "San José"] <- 16 # San JosÃ©
sf.URY$REG_ID[sf.URY$ADM1_ES == "Soriano"] <- 17 # Soriano
sf.URY$REG_ID[sf.URY$ADM1_ES == "Tacuarembó"] <- 18 # TacuarembÃ³
sf.URY$REG_ID[sf.URY$ADM1_ES == "Treinta y Tres"] <- 19 # Treinta y Tres
sf.URY$GID_0 <- "URY"
st_geometry(sf.URY) <- "geom"
process_and_plot(sf.URY, "URY")
rm(sf.URY)

# USA----
sf.USA <- subset(g, GID_0 == "USA")
g <- subset(g, GID_0 != "USA")
sf.USA$REG_ID[sf.USA$NAME_1 == "Alabama"] <- 1
sf.USA$REG_ID[sf.USA$NAME_1 == "Alaska"] <- 2
sf.USA$REG_ID[sf.USA$NAME_1 == "Arizona"] <- 4
sf.USA$REG_ID[sf.USA$NAME_1 == "Arkansas"] <- 5
sf.USA$REG_ID[sf.USA$NAME_1 == "California"] <- 6
sf.USA$REG_ID[sf.USA$NAME_1 == "Colorado"] <- 8
sf.USA$REG_ID[sf.USA$NAME_1 == "Connecticut"] <- 9
sf.USA$REG_ID[sf.USA$NAME_1 == "Delaware"] <- 10
sf.USA$REG_ID[sf.USA$NAME_1 == "District of Columbia"] <- 11
sf.USA$REG_ID[sf.USA$NAME_1 == "Florida"] <- 12
sf.USA$REG_ID[sf.USA$NAME_1 == "Georgia"] <- 13
sf.USA$REG_ID[sf.USA$NAME_1 == "Hawaii"] <- 15
sf.USA$REG_ID[sf.USA$NAME_1 == "Idaho"] <- 16
sf.USA$REG_ID[sf.USA$NAME_1 == "Illinois"] <- 17
sf.USA$REG_ID[sf.USA$NAME_1 == "Indiana"] <- 18
sf.USA$REG_ID[sf.USA$NAME_1 == "Iowa"] <- 19
sf.USA$REG_ID[sf.USA$NAME_1 == "Kansas"] <- 20
sf.USA$REG_ID[sf.USA$NAME_1 == "Kentucky"] <- 21
sf.USA$REG_ID[sf.USA$NAME_1 == "Louisiana"] <- 22
sf.USA$REG_ID[sf.USA$NAME_1 == "Maine"] <- 23
sf.USA$REG_ID[sf.USA$NAME_1 == "Maryland"] <- 24
sf.USA$REG_ID[sf.USA$NAME_1 == "Massachusetts"] <- 25
sf.USA$REG_ID[sf.USA$NAME_1 == "Michigan"] <- 26
sf.USA$REG_ID[sf.USA$NAME_1 == "Minnesota"] <- 27
sf.USA$REG_ID[sf.USA$NAME_1 == "Mississippi"] <- 28
sf.USA$REG_ID[sf.USA$NAME_1 == "Missouri"] <- 29
sf.USA$REG_ID[sf.USA$NAME_1 == "Montana"] <- 30
sf.USA$REG_ID[sf.USA$NAME_1 == "Nebraska"] <- 31
sf.USA$REG_ID[sf.USA$NAME_1 == "Nevada"] <- 32
sf.USA$REG_ID[sf.USA$NAME_1 == "New Hampshire"] <- 33
sf.USA$REG_ID[sf.USA$NAME_1 == "New Jersey"] <- 34
sf.USA$REG_ID[sf.USA$NAME_1 == "New Mexico"] <- 35
sf.USA$REG_ID[sf.USA$NAME_1 == "New York"] <- 36
sf.USA$REG_ID[sf.USA$NAME_1 == "North Carolina"] <- 37
sf.USA$REG_ID[sf.USA$NAME_1 == "North Dakota"] <- 38
sf.USA$REG_ID[sf.USA$NAME_1 == "Ohio"] <- 39
sf.USA$REG_ID[sf.USA$NAME_1 == "Oklahoma"] <- 40
sf.USA$REG_ID[sf.USA$NAME_1 == "Oregon"] <- 41
sf.USA$REG_ID[sf.USA$NAME_1 == "Pennsylvania"] <- 42
sf.USA$REG_ID[sf.USA$NAME_1 == "Rhode Island"] <- 44
sf.USA$REG_ID[sf.USA$NAME_1 == "South Carolina"] <- 45
sf.USA$REG_ID[sf.USA$NAME_1 == "South Dakota"] <- 46
sf.USA$REG_ID[sf.USA$NAME_1 == "Tennessee"] <- 47
sf.USA$REG_ID[sf.USA$NAME_1 == "Texas"] <- 48
sf.USA$REG_ID[sf.USA$NAME_1 == "Utah"] <- 49
sf.USA$REG_ID[sf.USA$NAME_1 == "Vermont"] <- 50
sf.USA$REG_ID[sf.USA$NAME_1 == "Virginia"] <- 51
sf.USA$REG_ID[sf.USA$NAME_1 == "Washington"] <- 53
sf.USA$REG_ID[sf.USA$NAME_1 == "West Virginia"] <- 54
sf.USA$REG_ID[sf.USA$NAME_1 == "Wisconsin"] <- 55
sf.USA$REG_ID[sf.USA$NAME_1 == "Wyoming"] <- 56
process_and_plot(sf.USA, "USA")
rm(sf.USA)

# UZB----
sf.UZB <- subset(g, GID_0 == "UZB")
g <- subset(g, GID_0 != "UZB")
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Qaraqalpaqstan"] <- 1 # Karakalpakistan Republic
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Andijon"] <- 2 # Andijan province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Buxoro"] <- 3 # Buhara province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Jizzax"] <- 4 # Djizak province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Qashqadaryo"] <- 5 # Kashkadarya province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Navoiy"] <- 6 # Navoyi province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Namangan"] <- 7 # Namangan province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Samarqand'"] <- 8 # Samarkand province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Surxondaryo"] <- 9 # Surhandarya province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Sirdaryo"] <- 10 # Syrdarya province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Toshkent"] <- 11 # Tashkent province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Farg'ona"] <- 12 # Fergana province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Xorazm"] <- 13 # Horezm province
sf.UZB$REG_ID[sf.UZB$NAME_1 == "Toshkent Shahri"] <- 14 # Tashkent capital city
process_and_plot(sf.UZB, "UZB")
rm(sf.UZB)

# VEN----
sf.VEN <- subset(g, GID_0 == "VEN")
g <- subset(g, GID_0 != "VEN")
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Distrito Capital"] <- 1 # Dto Capital
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Amazonas"] <- 2 # Amazonas
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Anzoátegui"] <- 3 # Anzoategui
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Aragua"] <- 4 # Aragua
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Barinas"] <- 5 # Barinas
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Bolívar"] <- 6 # Bolivar
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Carabobo"] <- 7 # Carabobo
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Cojedes"] <- 8 # Cojedes
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Falcón"] <- 9 # Falcon
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Guárico"] <- 10 # Guarico
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Lara"] <- 11 # Lara
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Mérida"] <- 12 # MÃ©rida
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Miranda"] <- 13 # Miranda
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Monagas"] <- 14 # Monagas
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Portuguesa"] <- 15 # Portuguesa
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Sucre"] <- 16 # Sucre
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Táchira"] <- 17 # Tachira
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Trujillo"] <- 18 # Trujillo
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Vargas"] <- 19 # Vargas
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Yaracuy"] <- 20 # Yaracuy
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Zulia"] <- 21 # Zulia
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Apure"] <- 22 # Apure
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Nueva Esparta"] <- 23 # Nueva Esparta
sf.VEN$REG_ID[sf.VEN$NAME_1 == "Delta Amacuro"] <- 24 # Delta Amacuro
process_and_plot(sf.VEN, "VEN")
rm(sf.VEN)

# VNM----
sf.VNM <- subset(g, GID_0 == "VNM")
g <- subset(g, GID_0 != "VNM")
sf.VNM$REG_ID[sf.VNM$NAME_1 == "An Giang"] <- 1 # An Giang
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Bắc Giang"] <- 2 # Bac Giang
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Bắc Kạn"] <- 3 # Bac Kan
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Bạc Liêu"] <- 4 # Bac Lieu
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Bắc Ninh"] <- 5 # Bac Ninh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Bà Rịa - Vũng Tàu"] <- 6 # Ba Ria-Vung Tau
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Bến Tre"] <- 7 # Ben Tre
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Bình Định"] <- 8 # Binh Dinh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Bình Dương"] <- 9 # Binh Duong
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Bình Phước"] <- 10 # Binh Phuoc
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Bình Thuận"] <- 11 # Binh Thuan
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Cà Mau"] <- 12 # Ca Mau
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Cao Bằng"] <- 13 # Cao Bang
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Đắk Lắk"] <- 14 # Dac Lak
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Đắk Nông"] <- 15 # Dac Nong
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Điện Biên"] <- 16 # Dien Bien
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Đồng Nai"] <- 17 # Dong Nai
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Đồng Tháp"] <- 18 # Dong Thap
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Gia Lai"] <- 19 # Gia Lai
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hà Giang"] <- 20 # Ha Giang
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hà Nam"] <- 21 # Ha Nam
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hoà Bình"] <- 22 # Ha Tay
# Merged into Hanoi
# https://en.wikipedia.org/wiki/Hà_Tây_province
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hà Tĩnh"] <- 23 # Ha Tinh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hải Dương"] <- 24 # Hai Duong
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hậu Giang"] <- 25 # Hau Giang
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hoà Bình"] <- 26 # Hoa Binh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hưng Yên"] <- 27 # Hung Yen
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Khánh Hòa"] <- 28 # Khanh Hoa
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Kiên Giang"] <- 29 # Kien Giang
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Kon Tum"] <- 30 # Kon Tum
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Lai Châu"] <- 31 # Lai Chau
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Lâm Đồng"] <- 32 # Lam Dong
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Lạng Sơn"] <- 33 # Lang Son
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Lào Cai"] <- 34 # Lao Cai
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Long An"] <- 35 # Long An
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Nam Định"] <- 36 # Nam Dinh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Nghệ An"] <- 37 # Nghe An
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Ninh Bình"] <- 38 # Ninh Binh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Ninh Thuận"] <- 39 # Ninh Thuan
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Phú Thọ"] <- 40 # Phu Tho
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Phú Yên"] <- 41 # Phu Yen
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Quảng Bình"] <- 42 # Quang Binh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Quảng Nam"] <- 43 # Quang Nam
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Quảng Ngãi"] <- 44 # Quang Ngai
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Quảng Ninh"] <- 45 # Quang Ninh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Quảng Trị"] <- 46 # Quang Tri
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Sóc Trăng"] <- 47 # Soc Trang
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Sơn La"] <- 48 # Son La
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Tây Ninh"] <- 49 # Tay Ninh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Thái Bình"] <- 50 # Thai Binh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Thái Nguyên"] <- 51 # Thai Nguyen
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Thanh Hóa"] <- 52 # Thanh Hoa
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Thừa Thiên Huế"] <- 53 # Thua Thien-Hue
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Tiền Giang"] <- 54 # Tien Giang
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Trà Vinh"] <- 55 # Tra Vinh
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Tuyên Quang"] <- 56 # Tuyen Quang
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Vĩnh Long"] <- 57 # Vinh Long
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Vĩnh Phúc"] <- 58 # Vinh Phuc
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Yên Bái"] <- 59 # Yen Bai
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Cần Thơ"] <- 60 # Can Tho
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Đà Nẵng"] <- 61 # Da Nang
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hải Phòng"] <- 62 # Hai Phong
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hà Nội"] <- 63 # Hanoi
sf.VNM$REG_ID[sf.VNM$NAME_1 == "Hồ Chí Minh"] <- 64 # Ho Chi Minh
process_and_plot(sf.VNM, "VNM")
rm(sf.VNM)

# YEM----
sf.YEM <- subset(g, GID_0 == "YEM")
g <- subset(g, GID_0 != "YEM")
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Amanat Al Asimah"] <- 1 # Sana a City
# https://en.wikipedia.org/wiki/Sanaa
sf.YEM$REG_ID[sf.YEM$NAME_1 == "`Adan"] <- 2 # Aden
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Dhamar"] <- 3 # Dhamar
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Sa`dah"] <- 4 # Saadah
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Ta`izz"] <- 5 # Taiz
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Abyan"] <- 6 # Abyan
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Ibb"] <- 7 # Ibb
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Al Hudaydah"] <- 8 # al-Hudaydah
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Hadramawt"] <- 9 # Hadramawt
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Ma'rib"] <- 10 # Mareb
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Shabwah"] <- 11 # Shabwah
sf.YEM$REG_ID[sf.YEM$NAME_1 == "San`a'"] <- 12 # Sanaa
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Al Mahwit"] <- 13 # al-Mahweet
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Hajjah"] <- 14 # Hajjah
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Al Dali'"] <- 15 # al Dhale
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Lahij"] <- 16 # Lahj
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Al Bayda'"] <- 17 # al-Bayda
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Al Mahrah"] <- 18 # al-Mahrah
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Raymah"] <- 19 # Raymah
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Al Jawf"] <- 20 # al-Jawf
sf.YEM$REG_ID[sf.YEM$NAME_1 == "Amran"] <- 21 # Amran
process_and_plot(sf.YEM, "YEM")
rm(sf.YEM)


# ZAF----
sf.ZAF <- subset(g, GID_0 == "ZAF")
g <- subset(g, GID_0 != "ZAF")
sf.ZAF$REG_ID[sf.ZAF$NAME_1 == "Eastern Cape"] <- 1 # Eastern Cape
sf.ZAF$REG_ID[sf.ZAF$NAME_1 == "Free State"] <- 2 # Free State
sf.ZAF$REG_ID[sf.ZAF$NAME_1 == "Gauteng"] <- 3 # Gauteng
sf.ZAF$REG_ID[sf.ZAF$NAME_1 == "KwaZulu-Natal"] <- 4 # KwaZulu Natal
sf.ZAF$REG_ID[sf.ZAF$NAME_1 == "Limpopo"] <- 5 # Limpopo
sf.ZAF$REG_ID[sf.ZAF$NAME_1 == "Mpumalanga"] <- 6 # Mpumalanga
sf.ZAF$REG_ID[sf.ZAF$NAME_1 == "North West"] <- 7 # North West
sf.ZAF$REG_ID[sf.ZAF$NAME_1 == "Northern Cape"] <- 8 # Northern Cape
sf.ZAF$REG_ID[sf.ZAF$NAME_1 == "Western Cape"] <- 9 # Western Cape
process_and_plot(sf.ZAF, "ZAF")
rm(sf.ZAF)

# ZMB----
sf.ZMB <- subset(g, GID_0 == "ZMB")
g <- subset(g, GID_0 != "ZMB")
sf.ZMB$REG_ID[sf.ZMB$NAME_1 == "Central"] <- 1 # Central
sf.ZMB$REG_ID[sf.ZMB$NAME_1 == "Copperbelt"] <- 2 # Copper Belt
sf.ZMB$REG_ID[sf.ZMB$NAME_1 == "Eastern"] <- 3 # Eastern
sf.ZMB$REG_ID[sf.ZMB$NAME_1 == "Luapula"] <- 4 # Lua pula
sf.ZMB$REG_ID[sf.ZMB$NAME_1 == "Lusaka"] <- 5 # Lusaka
sf.ZMB$REG_ID[sf.ZMB$NAME_1 == "Northern"] <- 6 # Northern
sf.ZMB$REG_ID[sf.ZMB$NAME_1 == "North-Western"] <- 7 # North Western
sf.ZMB$REG_ID[sf.ZMB$NAME_1 == "Southern"] <- 8 # Southern
sf.ZMB$REG_ID[sf.ZMB$NAME_1 == "Western"] <- 9 # Western
sf.ZMB$REG_ID[sf.ZMB$NAME_1 == "Muchinga"] <- 10 # Muchinga
process_and_plot(sf.ZMB, "ZMB")
rm(sf.ZMB)

# Singapore----
sf.SGP <- subset(g, GID_0 == "SGP")
g <- subset(g, GID_0 != "SGP")
sf.SGP$REG_ID <- 1
process_and_plot(sf.SGP, "SGP")
rm(sf.SGP)

# Palestine----
sf.PSE <- subset(g, GID_0 == "PSE")
g <- subset(g, GID_0 != "PSE")
table(sf.PSE$NAME_2)
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Jenin"] <- 1
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Tubas"] <- 2
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Qalqilya"] <- 3
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Tulkarm"] <- 4
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Nablus"] <- 5
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Salfit"] <- 6
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Ramallah and Al-Bireh"] <- 7
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Jerusalem"] <- 8
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Jericho"] <- 9
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Bethlehem"] <- 10
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Hebron"] <- 11
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Gaza"] <- 12
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Khan Yunis"] <- 13
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Rafah"] <- 14
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Gaza ash Shamaliyah"] <- 15
sf.PSE$REG_ID[sf.PSE$NAME_2 == "Deir Al-Balah"] <- 16
process_and_plot(sf.PSE, "PSE")
rm(sf.PSE)

# Kosovo----
sf.KOS <- sf::st_read(here("Data", "input", "world_admin_boundaries", "kosovo", "XK_EA_2018.shp"))
sf.KOS$GID_0 <- "XKO"
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Deçan"] <- 1
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Gjakovë"] <- 2
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Drenas"] <- 3
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Gjilan"] <- 4
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Dragash"] <- 5
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Istog"] <- 6
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Kaçanik"] <- 7
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Klinë"] <- 8
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Fushë Kosovë"] <- 9
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Enclave-Kamenicë"] <- 10
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Kamenicë"] <- 10
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Mitrovicë"] <- 11
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Leposaviq"] <- 12
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Lipjan"] <- 13
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Obiliq"] <- 14
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Rahovec"] <- 15
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Pejë"] <- 16
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Podujevë"] <- 17
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Prishtinë"] <- 18
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Enclave-Prishtine"] <- 18
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Prizren"] <- 19
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Skënderaj"] <- 20
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Shtime"] <- 21
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Shtërpcë"] <- 22
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Suharekë"] <- 23
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Ferizaj"] <- 24
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Viti"] <- 25
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Vushtrri"] <- 26
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Zubin Potok"] <- 27
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Zveçan"] <- 28
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Malishevë"] <- 29
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Hani i Elezit"] <- 30
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Graçanicë"] <- 31
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Junik"] <- 32
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Kllokot"] <- 33
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Mamushë"] <- 34
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Partesh"] <- 35
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Ranillug"] <- 36
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Mitrovicë e Veriut"] <- 37
sf.KOS$REG_ID[sf.KOS$XK_NAME=="Novobërdë"] <- 38
sf.KOS <- sf.KOS %>% rename(geom = geometry)

sf.KOS <- transform(sf.KOS, crs(g))
process_and_plot(sf.KOS, "XKO")
rm(sf.KOS)

## Georgia----
sf.GEO <- subset(g, GID_0 == "GEO")
g <- subset(g, GID_0 != "GEO")
sf.GEO$REG_ID[sf.GEO$NAME_1 == "Tbilisi"] <- 1
sf.GEO$REG_ID[sf.GEO$NAME_1 == "Imereti"] <- 2
sf.GEO$REG_ID[sf.GEO$NAME_1 == "Ajaria"] <- 3
sf.GEO$REG_ID[sf.GEO$NAME_1 == "Samegrelo-Zemo Svaneti"] <- 4
sf.GEO$REG_ID[sf.GEO$NAME_1 == "Shida Kartli"] <- 5
sf.GEO$REG_ID[sf.GEO$NAME_1 == "Kvemo Kartli"] <- 6
sf.GEO$REG_ID[sf.GEO$NAME_1 == "Mtskheta-Mtianeti"] <- 7
sf.GEO$REG_ID[sf.GEO$NAME_1 == "Samtskhe-Javakheti"] <- 8
sf.GEO$REG_ID[sf.GEO$NAME_1 == "Kakheti"] <- 9
sf.GEO$REG_ID[sf.GEO$NAME_1 == "Guria"] <- 10
process_and_plot(sf.GEO, "GEO")

#Montenegro----
sf.MNE <- st_read(here("Data", "input", "world_admin_boundaries", "montenegro", "geoBoundaries-MNE-ADM1.shp"))
sf.MNE$GID_0 <- "MNE"
sf.MNE$REG_ID[sf.MNE$shapeName == "Andrijevica Municipality"] <- 1
sf.MNE$REG_ID[sf.MNE$shapeName == "Bar Municipality"] <- 2
sf.MNE$REG_ID[sf.MNE$shapeName == "Berane Municipality"] <- 3
sf.MNE$REG_ID[sf.MNE$shapeName == "Bijelo Polje Municipality"] <- 4
sf.MNE$REG_ID[sf.MNE$shapeName == "Budva Municipality"] <- 5
sf.MNE$REG_ID[sf.MNE$shapeName == "Cetinje Municipality"] <- 6
sf.MNE$REG_ID[sf.MNE$shapeName == "Danilovgrad Municipality"] <- 7
sf.MNE$REG_ID[sf.MNE$shapeName == "Herceg Novi Municipality"] <- 8
sf.MNE$REG_ID[sf.MNE$shapeName == "Kolašin Municipality"] <- 9
sf.MNE$REG_ID[sf.MNE$shapeName == "Kotor Municipality"] <- 10
sf.MNE$REG_ID[sf.MNE$shapeName == "Mojkovac Municipality"] <- 11
sf.MNE$REG_ID[sf.MNE$shapeName == "Nikšić Municipality"] <- 12
sf.MNE$REG_ID[sf.MNE$shapeName == "Plav Municipality"] <- 13
sf.MNE$REG_ID[sf.MNE$shapeName == "Plužine Municipality"] <- 14
sf.MNE$REG_ID[sf.MNE$shapeName == "Pljevlja Municipality"] <- 15
sf.MNE$REG_ID[sf.MNE$shapeName == "Podgorica Municipality"] <- 16
sf.MNE$REG_ID[sf.MNE$shapeName == "Rožaje Municipality"] <- 17
sf.MNE$REG_ID[sf.MNE$shapeName == "Šavnik Municipality"] <- 18
sf.MNE$REG_ID[sf.MNE$shapeName == "Tivat Municipality"] <- 19
sf.MNE$REG_ID[sf.MNE$shapeName == "Ulcinj Municipality"] <- 20
sf.MNE$REG_ID[sf.MNE$shapeName == "Žabljak Municipality"] <- 21
sf.MNE$REG_ID[sf.MNE$shapeName == "Petnjica Municipality"] <- 22
sf.MNE$REG_ID[sf.MNE$shapeName == "Gusinje Municipality"] <- 23

# Tuzi shapefile
coordinates <- list(list(
  c(19.270778040975, 42.273675361358),
  c(19.28074836731, 42.282325542069),
  c(19.299287796021, 42.341480548263),
  c(19.291755460922, 42.345046455644),
  c(19.312334060669, 42.358226796749),
  c(19.312677383423, 42.416296606449),
  c(19.379968643188, 42.410213207464),
  c(19.407434463501, 42.417310448912),
  c(19.419107437134, 42.429982096689),
  c(19.411554336548, 42.447718100747),
  c(19.397134780884, 42.474565921371),
  c(19.404001235962, 42.480136667924),
  c(19.422540664673, 42.484694182598),
  c(19.433526992798, 42.485706918552),
  c(19.460306167603, 42.485706918552),
  c(19.487771987915, 42.492289302636),
  c(19.518671035767, 42.507982961648),
  c(19.547510147095, 42.520889309066),
  c(19.575993541926, 42.541373234117),
  c(19.591369628906, 42.526266167162),
  c(19.561157226562, 42.515638518737),
  c(19.56184387207, 42.499440530921),
  c(19.522705078125, 42.453861188491),
  c(19.520645141602, 42.444741336388),
  c(19.50553894043, 42.437140445353),
  c(19.497985839844, 42.424470244639),
  c(19.471893310547, 42.394051313624),
  c(19.454040527344, 42.384415576936),
  c(19.43000793457, 42.374778361114),
  c(19.421768188477, 42.358036523533),
  c(19.414215087891, 42.34788778389),
  c(19.416961669922, 42.331138780821),
  c(19.397735595703, 42.315400805),
  c(19.373016357422, 42.299658892534),
  c(19.374389648438, 42.288484824628),
  c(19.377822875977, 42.284929023633),
  c(19.364776611328, 42.278832897843),
  c(19.350357055664, 42.283405047481),
  c(19.332504272461, 42.267147008152),
  c(19.324951171875, 42.27476848631),
  c(19.321517944336, 42.267655135367),
  c(19.324951171875, 42.259016415706),
  c(19.32975769043, 42.253934262992),
  c(19.331130981445, 42.247326852177),
  c(19.316711425781, 42.250884774776),
  c(19.313278198242, 42.245293663449),
  c(19.296798706055, 42.245801966774),
  c(19.270778040975, 42.273675361358)
))
coords_matrix <- do.call(rbind, coordinates[[1]])
tuzi <- st_polygon(list(coords_matrix))
tuzi_obj <- st_sf(
  shapeName = "Tuzi Municipality",
  GID_0 = "MNE",
  geometry = st_sfc(tuzi),
  crs = 4326 # WGS84 Coordinate System
)
sf.MNE <- bind_rows(sf.MNE, tuzi_obj)

sf.MNE$REG_ID[sf.MNE$shapeName == "Tuzi Municipality"] <- 24

sf.MNE <- sf.MNE %>% rename(geom = geometry)

process_and_plot(sf.MNE, "MNE")

#Kuwait----
sf.KWT <- subset(g, GID_0 == "KWT")
table(sf.KWT$NAME_1)
sf.KWT$REG_ID[sf.KWT$NAME_1 == "Al Kuwayt"] <- 1
sf.KWT$REG_ID[sf.KWT$NAME_1 == "Hawalli"] <- 2
sf.KWT$REG_ID[sf.KWT$NAME_1 == "Al Farwaniyah"] <- 3
sf.KWT$REG_ID[sf.KWT$NAME_1 == "Al Ahmadi"] <- 4
sf.KWT$REG_ID[sf.KWT$NAME_1 == "Al Jahrah"] <- 5
sf.KWT$REG_ID[sf.KWT$NAME_1 == "Mubarak Al-Kabeer"] <- 6

process_and_plot(sf.KWT, "KWT")

#Hong Kong---
sf.HK <- st_read(here("Data", "input", "world_admin_boundaries", "hongkong", "HKDistrict18.shp"))

#1 Hong Kong Island
sf.HK$REG_ID[sf.HK$ENAME == "CENTRAL & WESTERN"] <- 1
sf.HK$REG_ID[sf.HK$ENAME == "EASTERN"] <- 1
sf.HK$REG_ID[sf.HK$ENAME == "SOUTHERN"] <- 1
sf.HK$REG_ID[sf.HK$ENAME == "WAN CHAI"] <- 1

# 2 Kowloon
sf.HK$REG_ID[sf.HK$ENAME == "KOWLOON CITY"] <- 2
sf.HK$REG_ID[sf.HK$ENAME == "KWUN TONG"] <- 2
sf.HK$REG_ID[sf.HK$ENAME == "SHAM SHUI PO"] <- 2
sf.HK$REG_ID[sf.HK$ENAME == "WONG TAI SIN"] <- 2
sf.HK$REG_ID[sf.HK$ENAME == "YAU TSIM MONG"] <- 2

# 3 New Territories
sf.HK$REG_ID[sf.HK$ENAME == "ISLANDS"] <- 3
sf.HK$REG_ID[sf.HK$ENAME == "KWAI TSING"] <- 3
sf.HK$REG_ID[sf.HK$ENAME == "NORTH"] <- 3
sf.HK$REG_ID[sf.HK$ENAME == "SAI KUNG"] <- 3
sf.HK$REG_ID[sf.HK$ENAME == "SHA TIN"] <- 3
sf.HK$REG_ID[sf.HK$ENAME == "TAI PO"] <- 3
sf.HK$REG_ID[sf.HK$ENAME == "TSUEN WAN"] <- 3
sf.HK$REG_ID[sf.HK$ENAME == "TUEN MUN"] <- 3
sf.HK$REG_ID[sf.HK$ENAME == "YUEN LONG"] <- 3

sf.HK$GID_0 <- "HKG"

sf.HK <- sf.HK %>% rename(geom = geometry)

crs(sf.HK) == crs(g)
sf.HK <- st_transform(sf.HK, crs(g))

process_and_plot(sf.HK, "HKG")

# AFG----
sf.AFG <- subset(g, GID_0 == "AFG")
for (i in 1:length(unique(sf.AFG$NAME_1))) {
  sf.AFG$REG_ID[sf.AFG$NAME_1 == unique(sf.AFG$NAME_1)[i]] <- i
}
process_and_plot(sf.AFG, "AFG")

# LUX ----
sf.LUX <- subset(g, GID_0 == "LUX")
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Clervaux"] <- 1
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Diekirch"] <- 2
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Redange"] <- 3
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Vianden"] <- 4
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Wiltz"] <- 5
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Echternach"] <- 6
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Grevenmacher"] <- 7
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Remich"] <- 8
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Capellen"] <- 9
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Esch-sur-Alzette"] <- 10
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Luxembourg"] <- 11
sf.LUX$REG_ID[sf.LUX$NAME_2 == "Mersch"] <- 12
process_and_plot(sf.LUX, "LUX")

# TWN ----
sf.TWN <- subset(g, GID_0 == "TWN")
table(sf.TWN$NAME_2)
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Chiayi County"] <- 1
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Chiayi City"] <- 2
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Changhua"] <- 3
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Hsinchu County"] <- 4
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Hsinchu City"] <- 5
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Hualien"] <- 6
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Kaohsiung"] <- 8
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Keelung"] <- 9
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Kinmen"] <- 10
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Lienkiang"] <- 11
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Miaoli"] <- 12
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Nantou"] <- 13
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Penghu"] <- 14
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Pingtung"] <- 15
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Taichung"] <- 17
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Tainan"] <- 19
sf.TWN$REG_ID[sf.TWN$NAME_2 == "New Taipei"] <- 20
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Taipei"] <- 21
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Taitung"] <- 22
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Taoyuan"] <- 23
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Yilan"] <- 24
sf.TWN$REG_ID[sf.TWN$NAME_2 == "Yulin"] <- 25
process_and_plot(sf.TWN, "TWN")

# LBY----
sf.LBY <- subset(g, GID_0 == "LBY")
table(sf.LBY$NAME_1)
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Al Kufrah"] <- 1
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Benghazi"] <- 2
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Tripoli"] <- 3
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Al Butnan"] <- 4
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Al Jabal al Akhdar"] <- 5
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Al Jifarah"] <- 6
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Al Marj"] <- 7
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Al Marqab"] <- 8
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Al Wahat"] <- 9
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Az Zawiyah"] <- 10
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Darnah"] <- 11
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Misratah"] <- 12
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Al Jabal al Gharbi"] <- 13
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Al Jufrah"] <- 14
sf.LBY$REG_ID[sf.LBY$NAME_1 == "An Nuqat al Khams"] <- 15
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Ghat"] <- 16
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Murzuq"] <- 17
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Nalut"] <- 18
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Sabha"] <- 19
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Surt"] <- 20
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Wadi al Hayat"] <- 21
sf.LBY$REG_ID[sf.LBY$NAME_1 == "Wadi ash Shati'"] <- 22
process_and_plot(sf.LBY, "LBY")

# TKM ----
sf.TKM <- subset(g, GID_0 == "TKM")
table(sf.TKM$NAME_1)
sf.TKM$REG_ID[sf.TKM$NAME_1 == "Aşgabat"] <- 1
sf.TKM$REG_ID[sf.TKM$NAME_1 == "Ahal"] <- 2
sf.TKM$REG_ID[sf.TKM$NAME_1 == "Balkan"] <- 3
sf.TKM$REG_ID[sf.TKM$NAME_1 == "Lebap"] <- 4
sf.TKM$REG_ID[sf.TKM$NAME_1 == "Daşoguz"] <- 5
sf.TKM$REG_ID[sf.TKM$NAME_1 == "Mary"] <- 6
process_and_plot(sf.TKM, "TKM")

# ZWE ----
sf.ZWE <- subset(g, GID_0 == "ZWE")
table(sf.ZWE$NAME_1)
for (i in 1:length(unique(sf.ZWE$NAME_1))) {
  sf.ZWE$REG_ID[sf.ZWE$NAME_1 == unique(sf.ZWE$NAME_1)[i]] <- i
}
process_and_plot(sf.ZWE, "ZWE")

# Remove regions4coding.rds----
file_to_remove <- here("Data", "inter", "19_wrp", "regions4coding.rds")
if (file.exists(file_to_remove)) {
  file.remove(file_to_remove)
  cat("Removed:", file_to_remove, "\n")
} else {
  cat("File not found:", file_to_remove, "\n")
}
