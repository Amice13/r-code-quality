# A global panel dataset of dyadic dual citizenship acceptance
# Maarten Vink, Luuk van der Baaren, David Reichels
# International Migration Review, 2025
# Replication script for descriptive plots and tables in paper and supplementary materials

#start with clean workspace
rm(list=ls(all=TRUE))

#load packages
library(scales)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(gmodels)
library(gridExtra)
library(gridtext)
library(grid)
library(ggrepel)
library(WDI)
library(janitor)
library(JWileymisc)
library(data.table)
library(naniar)
library(rworldmap)
library(mapproj)
library(ggstatsplot)
library(patchwork)
library(ggiraph)
library(countrycode)
library(openxlsx)
library(ggalluvial)
library(paletteer)
library(kableExtra)
library(xtable)
library(vdemdata)
library(data.table)

# -----------------------------------------------------
# Figure 1 + S1: Monadic trend
# import monadic data
dat_mon <- read.csv("GcDDCAD_v1.0_monadic.csv") # this file is included in the paper's dataverse and can be reproduced with dc_dataprep.R
View(dat_mon) #12.663 obs of 23 variables
# values in a dataset (without NA)
dat_mon %>% drop_na(aldc2_bin) %>%
  count(year) # n = 196
#label main variable
dat_mon$dualcit_combi <- factor(dat_mon$aldc_cat,
                                labels = c("None", "Only citizens naturalising abroad", "Only foreigners naturalising in country", "All"))
#count
dat_mon %>% drop_na(dualcit_combi) %>%
  count(dualcit_combi) 
# dualcit_combi    n
# 1                                    None 3625
# 2       Only citizens naturalising abroad 2103
# 3 Only immigrants naturalising in country 2159
# 4                                     All 2570
#label dc1
dat_mon$dualcit1_combi <- factor(dat_mon$aldc1_cat,
                                 labels = c("None", "Only citizens naturalising abroad", "Only foreigners naturalising in country", "All"))
dat_mon %>% drop_na(dualcit1_combi) %>%
  count(dualcit1_combi) 

#label dc2
dat_mon$dualcit2_combi <- factor(dat_mon$aldc2_cat,
                                 labels = c("None", "Only citizens naturalising abroad", "Only foreigners naturalising in country", "All"))
dat_mon %>% drop_na(dualcit2_combi) %>%
  count(dualcit2_combi) 
# dualcit_combi2    n
# 1                                    None 2645
# 2       Only citizens naturalising abroad 2649
# 3 Only immigrants naturalising in country 1836
# 4                                     All 3327
# count by year
dat_mon %>% drop_na(dualcit_combi) %>%
  count(dualcit_combi, year) 
#dualcit_combi
#1960
#All:     6   
#Total n: 87
#2022
#All:     91
#Total n: 196
dat_mon %>% drop_na(dualcit_combi) %>%
  count(dualcit2_combi, year) 
#dualcit_combi2
#1960
#All:     12    
#Total n: 87
#2022
#All:     103
#Total n: 196

#count number of countries in selected years
dat_mon %>%
  drop_na(dualcit_combi) %>%
  filter(year %in% c(1960, 1980, 2000, 2022)) %>%
  count(year) 
# year   n
# 1 1960  87
# 2 1980 154
# 3 2000 192
# 4 2022 196

#count dual cit policy in selected years and calculate percentage
dat_mon %>%
  drop_na(dualcit_combi) %>%
  filter(year %in% c(1960, 1980, 2000, 2022)) %>%
  group_by(year) %>%
  count(dualcit_combi) %>%
  mutate(perc = 100*n/sum(n))
# year dualcit_combi                               n  perc
# 1  1960 None                                       39 44.8 
# 2  1960 Only citizens naturalising abroad          17 19.5 
# 3  1960 Only foreigners naturalising in country    25 28.7 
# 4  1960 All                                         6  6.90

# 13  2022 None                                       42 21.4 
# 14  2022 Only citizens naturalising abroad          45 23.0 
# 15  2022 Only immigrants naturalising in country    18  9.18
# 16  2022 All                                        91 46.4 

#make world map
shap.df <- map_data("world")
#coordinate country codes between map and dataset
shap.df <- shap.df %>%
  mutate(iso3 = countrycode(region, "country.name", "iso3c"))
# Caused by warning in `countrycode_convert()`:
#   ! Some values were not matched unambiguously: 
# Ascension Island, Azores, Barbuda, Bonaire, Canary Islands, Chagos Archipelago, Grenadines, Heard Island, 
# Kosovo, Madeira Islands, Micronesia, Saba, Saint Martin, Siachen Glacier, Sint Eustatius, Virgin Islands 

#select data 1960, 1980, 2000, 2022
shap.df3 <- left_join(filter(dat_mon, year %in% c(1960, 1980, 2000, 2022)), 
                      shap.df, by = c("iso3"))
##copy value Soviet Union to all 15 republics: Russia, Ukraine, Georgia, Belorussia, Uzbekistan, Armenia, Azerbaijan, Kazakhstan, Kyrgyzstan, Moldova, Turkmenistan, Tajikistan, Latvia, Lithuania and Estonia [1960, 1980]
shap.df3$dualcit_combi[shap.df3$iso3 =="RUS" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="RUS" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="UKR" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="UKR" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="GEO" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="GEO" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="BLR" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="BLR" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="UZB" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="UZB" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="ARM" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="ARM" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="AZE" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="AZE" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="KAZ" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="KAZ" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="KGZ" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="KGZ" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="MDA" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="MDA" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="TKM" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="TKM" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="TJK" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="TJK" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="LVA" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="LVA" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="LTU" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="LTU" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="EST" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="EST" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SUN" & shap.df3$year =="1980"]
##copy value Sudan to South Sudan [1960, 1980, 2000]
shap.df3$dualcit_combi[shap.df3$iso3 =="SSD" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SDN" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="SSD" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SDN" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="SSD" & shap.df3$year =="2000"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="SDN" & shap.df3$year =="2000"]
##copy value CZESLO to CZE and SL [1960, 1980]
shap.df3$dualcit_combi[shap.df3$iso3 =="CZE" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="CSK" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="SVK" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="CSK" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="CZE" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="CSK" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="SVK" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="CSK" & shap.df3$year =="1980"]
##copy value YUGO to SRB and HRV and BIH and SVN [1960, 1980]
shap.df3$dualcit_combi[shap.df3$iso3 =="SRB" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="YUG" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="HRV" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="YUG" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="BIH" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="YUG" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="SVN" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="YUG" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="SRB" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="YUG" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="HRV" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="YUG" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="BIH" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="YUG" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="SVN" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="YUG" & shap.df3$year =="1980"]
##copy value Denmark to Greenland and France to French Guyana [1960, 1980]
shap.df3$dualcit_combi[shap.df3$iso3 =="GRL" & shap.df3$year =="1960"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="DNK" & shap.df3$year =="1960"]
shap.df3$dualcit_combi[shap.df3$iso3 =="GRL" & shap.df3$year =="1980"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="DNK" & shap.df3$year =="1980"]
shap.df3$dualcit_combi[shap.df3$iso3 =="GRL" & shap.df3$year =="2000"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="DNK" & shap.df3$year =="2000"]
shap.df3$dualcit_combi[shap.df3$iso3 =="GRL" & shap.df3$year =="2022"] <- shap.df3$dualcit_combi[shap.df3$iso3 =="DNK" & shap.df3$year =="2022"]
#map
map1 <- shap.df3 |>
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = dualcit_combi)) +
  geom_path(data = shap.df3, aes(long, lat, group = group), colour = "honeydew2") +
  scale_fill_brewer(palette = "BuGn", direction = 1) +
  labs(title = "",
       fill = "",
       caption = "",
       x = "", y = "") +
  facet_wrap(~year) +
  theme_minimal()+
  theme(legend.position="bottom")+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  theme(text = element_text(size = 20))+
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.background = element_rect(fill = "gray100"),
        plot.title = element_text(hjust = 0.5)) +
  coord_map(xlim = c(-165, 165), ylim = c(-55, 90))
map1

jpeg('Fig1.dc.trend_2x2.jpeg',  width = 12, height = 8, units = 'in', res = 400)
map1
dev.off()

#Save as .eps file
setEPS()
postscript("Figure1.eps", width = 12, height = 8)
map1
dev.off()

###plot comparing monadic trend by dc operationalisation for SM

#calculate mean dualcit by year
mon_dc_all <- dat_mon %>% 
  group_by(year) %>%
  summarise(mean_dc_all_year = mean(dualcit_combi=="All", na.rm = TRUE)) %>%
  ungroup()
mon_dc_all
#count dual cit policy in selected years and calculate percentage
mon_dc_all %>%
  filter(year %in% c(1960, 1980, 2000, 2022)) %>%
  group_by(year)
# year mean_dc_all_year
# 1  1960           0.0690
# 2  1980           0.136 
# 3  2000           0.245 
# 4  2022           0.464 
#monadic trend v immigrants
mon_dc_imm <- dat_mon %>% 
  group_by(year) %>%
  summarise(mean_dc_imm_year = mean((dualcit_combi=="All" | dualcit_combi=="Only foreigners naturalising in country"), na.rm = TRUE)) %>%
  ungroup()
mon_dc_imm
#monadic trend v emigrants
mon_dc_emi <- dat_mon %>% 
  group_by(year) %>%
  summarise(mean_dc_emi_year = mean((dualcit_combi=="All" | dualcit_combi=="Only citizens naturalising abroad"), na.rm = TRUE)) %>%
  ungroup()
mon_dc_emi

#dc2 alternative measure
#calculate mean dualcit by year
mon_dc2_all <- dat_mon %>% 
  group_by(year) %>%
  summarise(mean_dc2_all_year = mean(dualcit2_combi=="All", na.rm = TRUE)) %>%
  ungroup()
mon_dc2_all

#count dual cit policy in selected years and calculate percentage
mon_dc2_all %>%
  filter(year %in% c(1960, 1980, 2000, 2022)) %>%
  group_by(year)
# year mean_dc2_all_year
# 1  1960             0.138
# 2  1980             0.201
# 3  2000             0.323
# 4  2022             0.526
mon_dc2_imm <- dat_mon %>% 
  group_by(year) %>%
  summarise(mean_dc2_imm_year = mean((dualcit2_combi=="All" | dualcit2_combi=="Only foreigners naturalising in country"), na.rm = TRUE)) %>%
  ungroup()
mon_dc2_emi <- dat_mon %>% 
  group_by(year) %>%
  summarise(mean_dc2_emi_year = mean((dualcit2_combi=="All" | dualcit2_combi=="Only citizens naturalising abroad"), na.rm = TRUE)) %>%
  ungroup()

#plot trends for alternative measures
p1a <- plot(ggplot(mon_dc_all, aes(year, mean_dc_all_year), show.legend = FALSE) + 
              geom_line(linewidth=1.5) +
              # stat_smooth(method = "loess", formula = y ~ x, size = 1)+  #add smoothened line using a locally weighted regression
              theme_bw() +
              theme(axis.title = element_blank())+
              theme(plot.title = element_text(size = 14))+
              geom_line(data=na.omit(mon_dc_emi), 
                        aes(year, mean_dc_emi_year), show.legend = FALSE)+
              geom_text_repel(data=subset(mon_dc_emi, year==2006), 
                              aes(year, mean_dc_emi_year, label = "Citizens abroad", vjust=0.35, hjust = -0.25), show.legend = FALSE)+
              geom_line(data=na.omit(mon_dc_imm), 
                        aes(year, mean_dc_imm_year), show.legend = FALSE, linetype="dashed")+
              geom_text_repel(data=subset(mon_dc_imm, year==1964), 
                              aes(year, mean_dc_imm_year, label = "Foreigners in country", vjust=-0.4, hjust = -0.25), show.legend = FALSE)+
              scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,0.85), breaks = seq(0, 0.80, 0.10))+
              scale_x_continuous(breaks = seq(1960, 2030, 10))+
              annotate(geom = "text", x = 2005, y = 0.11, label = "Full dual ctizenship acceptance\n(46% in 2022)", fontface = "bold", size = 4))+
  labs(title = "a. Any dual citizenship restriction")+
  labs(caption = "[variable: aldc_cat]")
p1a

p2a0 <- plot(ggplot(mon_dc2_all, aes(year, mean_dc2_all_year), show.legend = FALSE) + 
              geom_line(linewidth=1.5) +
              # stat_smooth(method = "loess", formula = y ~ x, size = 1)+  #add smoothened line using a locally weighted regression
              theme_bw() +
              theme(axis.title = element_blank())+
              theme(plot.title = element_text(size = 14))+
              geom_line(data=na.omit(mon_dc2_emi), 
                        aes(year, mean_dc2_emi_year), show.legend = FALSE)+
              geom_text_repel(data=subset(mon_dc2_emi, year==2005), 
                              aes(year, mean_dc2_emi_year, label = "Citizens abroad", vjust=0.35, hjust = -0.25), show.legend = FALSE)+
              geom_line(data=na.omit(mon_dc2_imm), 
                        aes(year, mean_dc2_imm_year), show.legend = FALSE, linetype="dashed")+
              geom_text_repel(data=subset(mon_dc2_imm, year==1964), 
                              aes(year, mean_dc2_imm_year, label = "Foreigners in country", vjust=-0.4, hjust = -0.25), show.legend = FALSE)+
              scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,0.85), breaks = seq(0, 0.80, 0.10))+
              scale_x_continuous(breaks = seq(1960, 2030, 10))+
              annotate(geom = "text", x = 2005, y = 0.18, label = "Full dual ctizenship acceptance\n(53% in 2022)", fontface = "bold", size = 4))+
  labs(title = "b. Consistent dual citizenship restriction")+
  labs(caption = "[variable: aldc2_cat]")
p2a0

#Save as jpeg file
jpeg("Figure_S1_dc.mon.trend.2measures.jpeg", width=10, height=4, units="in", res=500)
grid.arrange(arrangeGrob(p1a, p2a0, ncol=2))
dev.off()

# -----------------------------------------------------
# Figure 2 + S2,S3: Dyadic trend

#Data prep
dat <- read.csv("GcDDCAD_v1.0_dyadic.csv") 
nrow(dat) #1.804.563

#General descriptive statistics of the dataset
# number of dyads
n_dyads <- length(unique(dat$dyad))
n_dyads #39988
## [1] 
# Number of countries
n_iso3_o <- length(unique(dat$iso3_o))
n_iso3_d <- length(unique(dat$iso3_d))
n_iso3_o
## [1] 201
n_iso3_d
## [1] 201
# check maximum possible dyads based on number of origin and destination countries
n_iso3_o*n_iso3_d
## [1] 40401 total dyadic combinations
dyads1960 <- dplyr::filter(dat, year == 1960) %>% nrow()
dyads1960 #7745
dyads2022 <- dplyr::filter(dat, year == 2022) %>% nrow()
dyads2022 #38220

#Figure 2: combi plot
#a dyadic combinations: alluvial plot
d2 <- dat %>%
  group_by(year) %>%
  mutate(ldc_bin_c = ifelse(ldc_bin_c == 0, 'No', 'Yes')) %>%
  mutate(adc_bin_c = ifelse(adc_bin_c == 0, 'No', 'Yes')) %>%
  mutate(dc_dy_bin_c = ifelse(dc_dy_bin_c == 0, 'No', 'Yes')) %>%
  count(ldc_bin_c, adc_bin_c, dc_dy_bin_c)
filter(dat, ldc_bin_c == 1, adc_bin_c == 1, dc_dy_bin_c == 0)
#select 2022 for plot
d3 <- filter(d2, year == 2022)
#plot p2a
p2a <- ggplot(as.data.frame(d3),
              aes(y = n, axis1 = ldc_bin_c, axis2 = adc_bin_c)) +
  geom_alluvium(aes(fill = factor(dc_dy_bin_c)), width = 1/12) +  
  geom_stratum(width = 1/12, fill = "white", color = "grey") +
  geom_text(stat = "alluvium",
            aes(label = percent(after_stat(prop), accuracy = 1)))+
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Origin", "Destination"), expand = c(.05, .05)) +
  theme_minimal() +
  scale_fill_manual(values = c("slategray1", "springgreen4"))+
  theme(plot.title = element_text(size = 12, hjust = 0.05))+
  theme(legend.position = "none")+
  # theme(axis.title = element_blank())+
  ggtitle("a. Dyadic dual citizenship acceptance, 2022")+
  theme(plot.title = element_text(face="bold", size = 14)) +
  ylab("number of country dyads\n") 
p2a

#Figure 2b: plot dyadic trend
#calculate mean dualcit by year, for dyadic, and for origin, and destination
daty <- dat %>% 
  group_by(year) %>%
  summarise(mean_dc_year = mean(dc_dy_bin_c, na.rm = TRUE)) %>%
  ungroup()
daty_o <- dat %>% 
  group_by(year) %>%
  summarise(mean_dc_o_year = mean(ldc_bin_c, na.rm = TRUE)) %>%
  ungroup()
daty_d <- dat %>% 
  group_by(year) %>%
  summarise(mean_dc_d_year = mean(adc_bin_c, na.rm = TRUE)) %>%
  ungroup()

## data visualisation trend dyadic dual citizenship acceptance based on conservative definition and dyadic corrections
#count % in 1960
dat %>% dplyr::filter(year == 1960)  %>% count(ldc_bin_c, adc_bin_c, dc_dy_bin_c) %>% mutate(prp = n/sum(n))
# ldc_bin_c adc_bin_c dc_dy_bin_c    n        prp
# 1         0         0           0 3581 0.46236281
# 2         0         1           0 2018 0.26055520
# 3         1         0           0 1378 0.17792124
# 4         1         1           1  768 0.09916075

#count % in 2022
dat %>% dplyr::filter(year == 2022)  %>% count(ldc_bin_c, adc_bin_c, dc_dy_bin_c) %>% mutate(prp = n/sum(n))
# ldc_bin_c adc_bin_c dc_dy_bin_c     n       prp
# 1         0         0           0  5105 0.1335688
# 2         0         1           0  6458 0.1689691
# 3         1         0           0 11710 0.3063841
# 4         1         1           1 14947 0.3910780

#plot Fig2b
p2b <- plot(ggplot(daty, aes(year, mean_dc_year), show.legend = FALSE) + 
              geom_line(size=1.5) +
              # stat_smooth(method = "loess", formula = y ~ x, size = 1)+  #add smoothened line using a locally weighted regression
              theme_minimal() +
              theme(plot.title = element_text(size = 12))+
              geom_line(data=na.omit(daty_o), 
                        aes(year, mean_dc_o_year), show.legend = FALSE)+
              geom_text_repel(data=subset(daty_o, year==2012), 
                              aes(year, mean_dc_o_year, label = "Origin country acceptance", vjust=-0.40, hjust = 1.3), show.legend = FALSE)+
              geom_line(data=na.omit(daty_d), 
                        aes(year, mean_dc_d_year), show.legend = FALSE, linetype="dashed")+
              geom_text_repel(data=subset(daty_d, year==1964), 
                              aes(year, mean_dc_d_year, label = "Destination country acceptance", vjust=-0.7, hjust = -0.1), show.legend = FALSE)+
              scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,0.7), breaks = seq(0, 0.70, 0.10))+
              scale_x_continuous(breaks = seq(1960, 2030, 10))+
              annotate(geom = "text", x = 2003, y = 0.13, label = "Dyadic dual citizenship acceptance\n(10% in 1960 | 39% in 2022)"))+
  ylab("percentage of dyads\n")+
  xlab("")+
  ggtitle("b. Dyadic trend")+
  theme(plot.title = element_text(face="bold", size=14))
p2b

#plot Fig2c: global migrants
# Global migration stocks. 
#Data from the [United Nations Population Division](https://www.un.org/en/development/desa/population/migration/data/estimates2/estimates19.asp). It includes several tables on the total international migration stock, by age and sex and by destination and origin. 
#With the below code, we load the data from the United Nations website, and rearrange them into one file that contains observations by country of destination and origin.
dm <- read.xlsx("https://www.un.org/development/desa/pd/sites/www.un.org.development.desa.pd/files/undesa_pd_2020_ims_stock_by_sex_destination_and_origin.xlsx",
               sheet = "Table 1", startRow = 11, na.strings = c("..", ""))
names(dm) <- c("Index", "dest_country", "dest_notes", "dest_code", "dest_type",
               "orig_country", "orig_code", 
               "T1990", "T1995", "T2000", "T2005", "T2010", "T2015", "T2020",
               "M1990", "M1995", "M2000", "M2005", "M2010", "M2015", "M2020",
               "F1990", "F1995", "F2000", "F2005", "F2010", "F2015", "F2020")
dm <- dm %>%
  filter(dest_code < 900) %>%
  filter(!is.na(dest_code)) %>%
  filter(orig_code < 900) %>%
  filter(!str_detect(dest_country, "Channel Islands")) %>%
  filter(!str_detect(orig_country, "Channel Islands")) %>%
  select(dest_country, orig_country, 
         T1990, T1995, T2000, T2005, T2010, T2015, T2020,
         M1990, M1995, M2000, M2005, M2010, M2015, M2020,
         F1990, F1995, F2000, F2005, F2010, F2015, F2020) %>%
  pivot_longer(T1990:F2020, 
               names_to = "S_year", 
               values_to = "value") %>%
  mutate(dest_country = str_trim(dest_country),
         orig_country = str_replace_all(orig_country, "\\.", " "),
         orig_country = str_trim(orig_country),
         sex = str_sub(S_year, 1, 1),
         year = str_sub(S_year, 2, 5), 
         year = as.numeric(year)) %>%
  mutate(orig_country = ifelse(orig_country == "Eswatini", "Swaziland", 
                               orig_country),
         dest_country = ifelse(dest_country == "Eswatini", "Swaziland", 
                               dest_country),
         iso3_o = countrycode(orig_country, "country.name", "iso3c"),
         iso3_d = countrycode(dest_country, "country.name", "iso3c")) %>%
  select(iso3_o, iso3_d, year, sex, migrant_stock_dyadic = value)
#select only Total
dm <- dm %>%
  filter(sex == 'T') %>%
  select(iso3_o, iso3_d, year, migrant_stock_dyadic)
#merge with dyadic data
nrow(dat) #check N: 1804563
dat <- dat %>%
  left_join(dm, by = c("iso3_o", "iso3_d", "year"))
nrow(dat) #check N:1804563
rm(dm)

#select years and make summary variables by dc
d_mig <- dat %>%
  filter(year %in% seq(1990, 2020, 5)) %>%
  group_by(year, dc_dy_bin_c) %>%
  summarise(n_obs = n(),
            migrants = sum(migrant_stock_dyadic, na.rm = TRUE)) %>%
  group_by(year) %>%
  mutate(dualcit_share = n_obs/sum(n_obs),
         migrants_share = round(100*migrants/sum(migrants)),
         dualcit_acceptance = ifelse(dc_dy_bin_c == 1, "yes", "no"))
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
d_mig

p2c <- ggplot() +
  geom_col(data = d_mig, 
           aes(year, migrants, fill = dualcit_acceptance)) +
  scale_y_continuous("number (and %) of global migrants\n", labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_x_continuous("", breaks = seq(1990, 2020, 5)) +
  geom_point(data = filter(d_mig, dualcit_acceptance == "yes"), 
             aes(year, migrants, colour = "Percentage among migrants"),
             size = 12, colour = 'grey') + 
  theme_minimal()+
  scale_fill_brewer(palette = "BuGn", direction = 1) +
  geom_text(data = filter(d_mig, dualcit_acceptance == "yes"), 
            aes(year, migrants, label = paste0(migrants_share, "%"))) +
  theme(legend.position = "top")+
  labs(fill = "Dyadic dual citizenship acceptance")+
  ggtitle("c. Global migrants")+
  theme(plot.title = element_text(face="bold", size=14))
p2c

#Figure 2d: dyadic dc by political regime
# Import VDEM data
# Follow instructions from https://www.dante-project.org/vignettes/clean-vdem
# alternatively downloaded data from https://www.v-dem.net/data/the-v-dem-dataset/
# select and download the vdem data 
vd <- data.table::setDT(vdemdata::vdem) |>
  filter(year > 1959) |>
  mutate(iso3 = country_text_id) |>
  select(iso3, year, v2x_regime)
summary(vd)
# iso3                year        v2x_regime   
# Length:10550       Min.   :1960   Min.   :0.000  
# Class :character   1st Qu.:1976   1st Qu.:0.000  
# Mode  :character   Median :1993   Median :1.000  
#                    Mean   :1992   Mean   :1.239  
#                    3rd Qu.:2008   3rd Qu.:2.000  
#                    nMax.  :2023  Max.   :3.000  
#                                   NA's   :2

#create two dataframes for origin and destination
vd_o <- vd %>%
  rename(iso3_o = iso3, v2x_regime_o = v2x_regime)
vd_d <- vd %>%
  rename(iso3_d = iso3, v2x_regime_d = v2x_regime)
# merge with dc data
nrow(dat)#1804563
dat <- dat %>%
  left_join(vd_d, by = c("iso3_d", "year")) %>%
  left_join(vd_o, by = c("iso3_o", "year"))
nrow(dat) #1804563
rm(vd); rm(vd_d); rm(vd_o)

# check overall regime type data
# v2x_regime
# 0 == closed autocracy
# 1 == electoral autocracy
# 2 == electoral democracy
# 3 == liberal democracy

#create dataframe with labelled variables
d_reg <- dat %>%
  group_by(year, v2x_regime_o, v2x_regime_d) %>%
  summarise(dc_dy_bin_c = mean(dc_dy_bin_c),
            n_obs = n()) %>%
  mutate(toolt = paste0(n_obs, " country pairs"),
         v2x_regime_o = case_when(
           v2x_regime_o == 0 ~ "clsd. autocracy",
           v2x_regime_o == 1 ~ "elect. autocracy",
           v2x_regime_o == 2 ~ "elect. democracy",
           v2x_regime_o == 3 ~ "lib. democracy"),
         v2x_regime_d = case_when(
           v2x_regime_d == 0 ~ "closed autocracy",
           v2x_regime_d == 1 ~ "electoral autocracy",
           v2x_regime_d == 2 ~ "electoral democracy",
           v2x_regime_d == 3 ~ "liberal democracy"),
         regime_dyad = case_when(
           (v2x_regime_o == "clsd. autocracy" | v2x_regime_o == "elect. autocracy") & (v2x_regime_d == "closed autocracy" | v2x_regime_d == "electoral autocracy") ~ "autocracy - autocracy",
           v2x_regime_o == "elect. autocracy" | v2x_regime_o == "clsd. autocracy" & (v2x_regime_d == "electoral democracy" | v2x_regime_d == "liberal democracy") ~ "autocracy - democracy",
           v2x_regime_o == "elect. democracy" | v2x_regime_o == "lib. democracy" & (v2x_regime_d == "closed autocracy" | v2x_regime_d == "electoral autocracy") ~ "democracy - autocracy",
           v2x_regime_o == "elect. democracy" | v2x_regime_o == "lib. democracy" & (v2x_regime_d == "electoral democracy" | v2x_regime_d == "liberal democracy") ~ "democracy - democracy"))
## `summarise()` has grouped output by 'v2x_regime_o'. You can override using the `.groups` argument.

#Create Fig 2d
p2d <- drop_na(d_reg) %>%
  filter(year == 2022) %>%
  ggplot() +
  geom_tile_interactive(aes(v2x_regime_o, v2x_regime_d,  
                            fill = round(100*dc_dy_bin_c),
                            tooltip = toolt)) +
  theme_minimal() +
  geom_text(aes(v2x_regime_o, v2x_regime_d, label = round(100*dc_dy_bin_c)),
            size = 8) +
  labs(x = "\nOrigin",
       y = "Destination\n")+
  coord_equal() +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 0, vjust = 0, hjust = 0.5, size =12),
        axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5, size =11)) +
  scale_fill_gradient(low = "grey", high = "forestgreen")+
  ggtitle("d. Dyadic acceptance by political regime, 2022")+
  theme(plot.title = element_text(face="bold", size=14))
p2d

#combi plot - save as high res
jpeg('Figure2.jpeg',  width = 16, height = 12, units = 'in', res = 800)
grid.arrange(arrangeGrob(p2a, p2b, p2c,p2d,nrow=2, ncol=2))
dev.off()

#Save as .eps file
# NB does not reproduce alluvial plot in p2a!
postscript("Figure2.eps", width = 16, height = 12)
grid.arrange(arrangeGrob(p2a, p2b, p2c,p2d))
dev.off()

# Supplementary materials

# SM2: Summary statistics variables in descriptive trend analyses

summary(dat)

# Calculate Summary statistics with min, max, mean, sd, and N
summary_stats_descr <- dat %>%
  reframe(
    Variable = c("Monadic dc acceptance, categorical (any restriction)", "Monadic dc acceptance, categorical (consistent restriction)", 
      "Dyadic dc acceptance, categorical (any restriction)", "Dyadic dc acceptance, categorical (consistent restriction)", 
                 "Dyadic dc acceptance, binary (any restriction)", "Dyadic dc acceptance, binary (consistent restriction)",
                 "Dyadic migrant stock", "Regime type (destination)", "Regime type (origin)"),
    Min = c(min(dat_mon$aldc_cat, na.rm = TRUE),
            min(dat_mon$aldc2_cat, na.rm = TRUE),
            min(dat$dc_dy_cat_c, na.rm = TRUE),
            min(dat$dc2_dy_cat_c, na.rm = TRUE),
            min(dat$dc_dy_bin_c, na.rm = TRUE),
            min(dat$dc2_dy_bin_c, na.rm = TRUE),
            min(dat$migrant_stock_dyadic, na.rm = TRUE),
            min(dat$v2x_regime_d, na.rm = TRUE),
            min(dat$v2x_regime_o, na.rm = TRUE)),
    Max = c(max(dat_mon$aldc_cat, na.rm = TRUE),
            max(dat_mon$aldc2_cat, na.rm = TRUE),
            max(dat$dc_dy_cat_c, na.rm = TRUE),
            max(dat$dc2_dy_cat_c, na.rm = TRUE),
            max(dat$dc_dy_bin_c, na.rm = TRUE),
            max(dat$dc2_dy_bin_c, na.rm = TRUE),
            max(dat$migrant_stock_dyadic, na.rm = TRUE),
            max(dat$v2x_regime_d, na.rm = TRUE),
            max(dat$v2x_regime_o, na.rm = TRUE)),
    Mean = c(mean(dat_mon$aldc_cat, na.rm = TRUE),
             mean(dat_mon$aldc2_cat, na.rm = TRUE),
             mean(dat$dc_dy_cat_c, na.rm = TRUE),
             mean(dat$dc2_dy_cat_c, na.rm = TRUE),
             mean(dat$dc_dy_bin_c, na.rm = TRUE),
             mean(dat$dc2_dy_bin_c, na.rm = TRUE),
             mean(migrant_stock_dyadic, na.rm = TRUE),
             mean(dat$v2x_regime_d, na.rm = TRUE),
             mean(dat$v2x_regime_o, na.rm = TRUE)),
    SD = c(sd(dat_mon$aldc_cat, na.rm = TRUE),
           sd(dat_mon$aldc2_cat, na.rm = TRUE),
           sd(dat$dc_dy_cat_c, na.rm = TRUE),
           sd(dat$dc2_dy_cat_c, na.rm = TRUE),
           sd(dat$dc_dy_bin_c, na.rm = TRUE),
           sd(dat$dc2_dy_bin_c, na.rm = TRUE),
           sd(dat$migrant_stock_dyadic, na.rm = TRUE),
           sd(dat$v2x_regime_d, na.rm = TRUE),
           sd(dat$v2x_regime_o, na.rm = TRUE)),
    N = c(sum(!is.na(dat_mon$aldc_cat)),
          sum(!is.na(dat_mon$aldc2_cat)),
          sum(!is.na(dat$dc_dy_cat_c)),
          sum(!is.na(dat$dc2_dy_cat_c)),
          sum(!is.na(dat$dc_dy_bin_c)),
          sum(!is.na(dat$dc2_dy_bin_c)),
          sum(!is.na(dat$migrant_stock_dyadic)),
          sum(!is.na(dat$v2x_regime_d)),
          sum(!is.na(dat$v2x_regime_o)))
)

print(xtable(summary_stats_descr, 
             caption = "Summary statistics of variables in descriptive trend analysis", 
             type = "latex", digits=c(0,0,0,0,2,0,0)), include.rownames = FALSE, file = "TableS1_sum_stat_desc.tex")

#Figures S2/S3 dyadic trend with alternative measures

#  percentages
count(dat, ldc_bin_c, adc_bin_c, dc_dy_bin_c) %>% mutate(prp = n/sum(n))
# ldc_bin_c adc_bin_c dc_dy_bin_c      n       prp
# 1         0         0           0 528127 0.2926620
# 2         0         1           0 427528 0.2369150
# 3         1         0           0 447974 0.2482451
# 4         1         1           1 400934 0.2221779
count(dat, ldc2_bin_c, adc2_bin_c, dc2_dy_bin_c) %>% mutate(prp = n/sum(n))
# ldc2_bin_c adc2_bin_c dc2_dy_bin_c      n       prp
# 1          0          0            0 376396 0.2085801
# 2          0          1            0 355011 0.1967296
# 3          1          0            0 528552 0.2928975
# 4          1          1            1 544604 0.3017927

## data visualisation trend dyadic dual citizenship acceptance based on conservative definition and dyadic corrections
#count % in 1960
dat %>% dplyr::filter(year == 1960)  %>% count(ldc_bin_c, adc_bin_c, dc_dy_bin_c) %>% mutate(prp = n/sum(n))
# ldc_bin_c adc_bin_c dc_dy_bin_c    n        prp
# 1         0         0           0 3581 0.46236281
# 2         0         1           0 2018 0.26055520
# 3         1         0           0 1378 0.17792124
# 4         1         1           1  768 0.09916075

## data visualisation trend dyadic dual citizenship acceptance based on broad definition and dyadic corrections
#count % in 1960
dat %>% dplyr::filter(year == 1960)  %>% count(ldc2_bin_c, adc2_bin_c, dc2_dy_bin_c) %>% mutate(prp = n/sum(n))
# ldc2_bin_c adc2_bin_c dc2_dy_bin_c    n       prp
# 1          0          0            0 2756 0.3558425
# 2          0          1            0 2139 0.2761782
# 3          1          0            0 1595 0.2059393
# 4          1          1            1 1255 0.1620400

#count % in 2022
dat %>% dplyr::filter(year == 2022)  %>% count(ldc_bin_c, adc_bin_c, dc_dy_bin_c) %>% mutate(prp = n/sum(n))
# ldc_bin_c adc_bin_c dc_dy_bin_c     n       prp
# 1         0         0           0  5105 0.1335688
# 2         0         1           0  6458 0.1689691
# 3         1         0           0 11710 0.3063841
# 4         1         1           1 14947 0.3910780

dat %>% dplyr::filter(year == 2022)  %>% count(ldc2_bin_c, adc2_bin_c, dc2_dy_bin_c) %>% mutate(prp = n/sum(n))
# ldc2_bin_c adc2_bin_c dc2_dy_bin_c     n        prp
# 1          0          0            0  2925 0.07653061
# 2          0          1            0  4197 0.10981162
# 3          1          0            0 12746 0.33349032
# 4          1          1            1 18352 0.48016745
#label categories
#most conservative operationalisation (dc)
d1a <- dat %>%
  mutate(dc_dy_cat_c = ifelse(dc_dy_cat_c == 0, "None",
                              ifelse(dc_dy_cat_c == 1, "Only destination", 
                                     ifelse(dc_dy_cat_c == 2, "Only origin",
                                            ifelse(dc_dy_cat_c == 3, "Both", NA))))) %>%
  mutate(dc_dy_cat_c = fct_relevel(dc_dy_cat_c, "None", "Only destination", "Only origin")) %>%
  count(year, dc_dy_cat_c) %>%
  group_by(year) %>%
  na.omit() %>%
  mutate(prp = round(n/sum(n)*100),
         n_obs = sum(n),
         toolt = paste0(year, ": ", dc_dy_cat_c, " ", prp, " % (", n_obs, " dyads)"))

# data visualisation trend dyadic dual citizenship acceptance based on broad definition and dyadic correction

#plot
p1a <- d1a %>%
  ggplot() +
  geom_col(aes(year, prp, fill = dc_dy_cat_c)) +
  geom_hline(yintercept = 50, linetype = 2, colour = grey(0.9)) +
  theme_minimal()+
  theme(text = element_text(size = 20))+
  scale_fill_brewer(palette = "BuGn", direction = 1) +
  scale_y_continuous("") +
  scale_x_continuous("", breaks = seq(1960,2020,10)) +
  annotate(geom = "text", x = 1961.5, y = 105, label = "% in 1960", size = 6, fontface = 'bold')+
  annotate(geom = "text", x = 1961.5, y = 75, label = "46%", size = 6)+
  annotate(geom = "text", x = 1961.5, y = 38, label = "26%", size = 6)+
  annotate(geom = "text", x = 1961.5, y = 20, label = "18%", size = 6, colour = 'black')+
  annotate(geom = "text", x = 1961.5, y = 06, label = "10%", size = 6, colour = 'white')+
  annotate(geom = "text", x = 2019.5, y = 105, label = "% in 2022", size = 6, fontface = 'bold')+
  annotate(geom = "text", x = 2019.5, y = 91, label = "13%", size = 6)+
  annotate(geom = "text", x = 2019.5, y = 76, label = "17%", size = 6)+
  annotate(geom = "text", x = 2019.5, y = 54, label = "31%", size = 6, colour = 'black')+
  annotate(geom = "text", x = 2019.5, y = 21, label = "39%", size = 6, colour = 'white')+
  labs(title = "",
       fill = "",
       caption = "")
p1a

jpeg('FigS2.dc.dyadic.trend.jpeg',  width = 16, height = 8, units = 'in', res = 400)
p1a
dev.off()


# most inclusive operationalisation (dc2)
d1c <- dat %>%
  mutate(dc2_dy_cat_c = ifelse(dc2_dy_cat_c == 0, "None",
                               ifelse(dc2_dy_cat_c == 1, "Only destination", 
                                      ifelse(dc2_dy_cat_c == 2, "Only origin",
                                             ifelse(dc2_dy_cat_c == 3, "Both", NA))))) %>%
  mutate(dc2_dy_cat_c = fct_relevel(dc2_dy_cat_c, "None", "Only destination", "Only origin")) %>%
  count(year, dc2_dy_cat_c) %>%
  group_by(year) %>%
  na.omit() %>%
  mutate(prp = round(n/sum(n)*100),
         n_obs = sum(n),
         toolt = paste0(year, ": ", dc2_dy_cat_c, " ", prp, " % (", n_obs, " dyads)"))

#plot d1c
p1c <- d1c %>%
  ggplot() +
  geom_col(aes(year, prp, fill = dc2_dy_cat_c)) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = grey(0.9)) +
  theme_minimal()+
  theme(text = element_text(size = 20))+
  scale_fill_brewer(palette = "BuGn", direction = 1) +
  scale_y_continuous("") +
  scale_x_continuous("", breaks = seq(1960,2020,10)) +
  annotate(geom = "text", x = 1961.5, y = 105, label = "% in 1960", size = 6, fontface = 'bold')+
  annotate(geom = "text", x = 1961.5, y = 85, label = "36%", size = 6)+
  annotate(geom = "text", x = 1961.5, y = 58, label = "28%", size = 6)+
  annotate(geom = "text", x = 1961.5, y = 30, label = "21%", size = 6, colour = 'black')+
  annotate(geom = "text", x = 1961.5, y = 10, label = "16%", size = 6, colour = 'white')+
  annotate(geom = "text", x = 2019.5, y = 105, label = "% in 2022", size = 6, fontface = 'bold')+
  annotate(geom = "text", x = 2019.5, y = 95, label = "8%", size = 6)+
  annotate(geom = "text", x = 2019.5, y = 86, label = "11%", size = 6)+
  annotate(geom = "text", x = 2019.5, y = 65, label = "33%", size = 6, colour = 'black')+
  annotate(geom = "text", x = 2019.5, y = 25, label = "48%", size = 6, colour = 'white')+
  labs(title = "",
       fill = "",
       caption = "")
p1c

jpeg('FigS3.dc2.dyadic.trend.jpeg',  width = 16, height = 8, units = 'in', res = 400)
p1c
dev.off()

# Figure S4: trend plot by regime dyad (binarised)

d_reg$regime_dyad <- factor(d_reg$regime_dyad, levels=c("autocracy - autocracy","democracy - autocracy", 
                                                        "autocracy - democracy", "democracy - democracy"))
p4_trend_regime_dyad <- d_reg %>% 
  drop_na() %>%
  ggplot(aes(year, dc_dy_bin_c), show.legend = FALSE) + 
  geom_point(shape = 21, fill = "lightgray",
             color = "grey", size = 1)+
  geom_smooth(method="loess", se=TRUE, fullrange=FALSE, level=0.95, color = "darkgreen")+
  facet_wrap(~regime_dyad, ncol=5)+
  theme_minimal() +
  theme(text = element_text(size = 20))+
  theme(axis.title = element_blank())+
  theme(plot.title = element_text())+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(title = "")
p4_trend_regime_dyad

jpeg('FigS4.dc.trend_regime_hor.jpeg',  width = 16, height = 6, units = 'in', res = 400)
p4_trend_regime_dyad
dev.off()

### END ###
