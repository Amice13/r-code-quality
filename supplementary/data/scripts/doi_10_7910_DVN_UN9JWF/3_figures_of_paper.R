rm(list = ls())
loadPkg=function(toLoad){
  for(lib in toLoad){
    if(! lib %in% installed.packages()[,1])
    {install.packages(lib, repos='http://cran.rstudio.com/')}
    suppressMessages( library(lib, character.only=TRUE))}}

# Load libraries
packs=c("cshapes","readstata13", "DataCombine", "countrycode","arm", "rgeos", "raster", "tidyr", "pwt9",
        "texreg", "reshape2","ggplot2", 'foreign', 'car', 'lme4', 'dplyr', "gridExtra","readr","readr",
        "pscl", "stargazer","stringi","multiwayvcov", "lmtest","scales","stringi","haven","readxl",
        "coefplot", "ggthemes", "sf", "tidyverse","dplyr","extrafont","showtext","ggmap")
loadPkg(packs)
font_add('KaiTi', 'KaiTi.ttf')
showtext_auto(enable = TRUE)
#set ggplot theme
theme_set(theme_gray())



######################  setting working directory)

setwd("Replication")



# load panel grid-level  data 
load("./Clean_data/data.RData")

# load GTD overlaid on grid
load("./Clean_data/gtd_grid.RData")

# get unique country code
ccode <- unique(data$gwno)

# get a shapefile for the world and then subset for africa
world <- cshp(date = as.Date("2012-12-31"), useGW = F)
world <- world[which(world@data$COWCODE %in% ccode),]
world_shp <- fortify(world , region = "COWCODE")
world_shp$id <- as.integer(world_shp$id)
world_shp <- world_shp[order(world_shp$order), ] 

# load the raw GTD data for 1970 - 2016 
load("Raw_data/GTD_0617dist/globalterrorismdb_0617dist.RData")
gtd <- globalterrorismdb_0617dist
rm(globalterrorismdb_0617dist)

########################################################################################################
########################  Figure 1##############
########################  Figure 1a ##############
########################################################################################################

#make time-series 
gtd_grid <-  gtd_grid[which(gtd_grid$ccode %in% ccode), ]
gtd_year <- gtd_grid %>% 
  dplyr::mutate(Assassination = ifelse(attacktype1 ==1, 1, 0),
                Armed_Assault = ifelse(attacktype1 == 2, 1, 0),
                Bombing_Explosion = ifelse(attacktype1 == 3, 1, 0),
                Hijacking = ifelse(attacktype1 == 4, 1, 0),
                Hostage_Taking = ifelse(attacktype1 == 5, 1, 0),
                Hostage_Kidnapping = ifelse(attacktype1 == 6, 1, 0),
                Facility_Infrastructure = ifelse(attacktype1 == 7, 1, 0),
                Unarmed_Assault = ifelse(attacktype1 == 8, 1, 0),
                Unknown = ifelse(attacktype1 == 9, 1, 0)) %>% 
  dplyr::group_by(iyear) %>% 
  dplyr::summarise(Assassination = sum(Assassination),
                   Armed_Assault = sum(Armed_Assault),
                   Bombing_Explosion = sum(Bombing_Explosion),
                   Hijacking = sum(Hijacking),
                   Hostage_Taking = sum(Hostage_Taking),
                   Hostage_Kidnapping = sum(Hostage_Kidnapping),
                   Facility_Infrastructure = sum(Facility_Infrastructure),
                   Unarmed_Assault = sum(Unarmed_Assault),
                   Unknown = sum(Unknown)) #%>% 
# dplyr::mutate(dateid = row_number())
map(gtd_year[,2:9], sum)

#x-label
gtd_year %>%
  ggplot(aes(x = iyear)) +
  geom_line(aes(y = Assassination), colour = "tan", linetype = 1, size = 1) + 
  geom_line(aes(y = Armed_Assault), colour = "gold", linetype = 2, size = 1) + 
  geom_line(aes(y = Bombing_Explosion), colour = "gray33", linetype = 3, size = 1.5) + 
  #geom_line(aes(y = Hijacking), colour = "#D55E00", linetype = 4, size = .4) + 
  #geom_line(aes(y = Hostage_Taking), colour = "#CC79A7", linetype = 5, size = .5) + 
  geom_line(aes(y = Hostage_Kidnapping), colour = "black", linetype = 4, size = .6) + 
  geom_line(aes(y = Facility_Infrastructure), colour = "blue", linetype = 5, size = .7) + 
  geom_line(aes(y = Unarmed_Assault), colour = "gold", linetype = 6, size = .8)+ 
  #geom_line(aes(y = Unknown), colour = "red", linetype = 3, size = .9) +
  scale_x_continuous(breaks = seq(1970, 2016, by= 3)) +
  # Aesthetics
  labs(title = "非洲恐怖主义的时间分布（1970-2016年）", x = "年份",
       y = "频次",
       caption="作者自制",
       subtitle = "数据来源: GTD") +
  theme(text = element_text(size = 14, family = "KaiTi"),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size=16),
        axis.title=element_text(size=16)) +
  theme_tufte(base_family = "KaiTi") + 
  annotate("segment", x = 1988, xend = 1988, y = 300, yend = 220,
           colour="gray33", arrow=arrow(length=unit(0.1, "inches"))) + 
  annotate("text", x = 1988, y = 301, parse = F, 
           label = "爆炸",size=4, family = "KaiTi") +
  annotate("segment", x = 2000, xend = 2000, y = 250, yend = 168,
           colour="gold", arrow=arrow(length=unit(0.1, "inches"))) + 
  annotate("text", x = 2000, y = 251, parse = F, 
           label = "武装袭击",size=4, family = "KaiTi") +
  annotate("segment", x = 2016, xend = 2016, y = 479, yend = 398,
           colour="black", arrow=arrow(length=unit(0.1, "inches"))) + 
  annotate("text", x = 2016, y = 480, parse = F, 
           label = "绑架",size=4, family = "KaiTi") +
  annotate("segment", x = 2015, xend = 2015, y = 250, yend = 170,
           colour="tan", arrow=arrow(length=unit(0.1, "inches"))) + 
  annotate("text", x = 2015, y = 251, parse = F, 
           label = "暗杀",size=4, family = "KaiTi") + 
  annotate("segment", x = 2015, xend = 2015, y = 140, yend = 77,
           colour="blue", arrow=arrow(length=unit(0.1, "inches"))) + 
  annotate("text", x = 2016, y = 141, parse = F, 
           label = "破坏设施",size=3, family = "KaiTi") +
  annotate("segment", x = 2016, xend = 2016, y = 60, yend = 6,
           colour="gold", arrow=arrow(length=unit(0.1, "inches"))) + 
  annotate("text", x = 2016, y = 61, parse = F, 
           label = "非武装袭击",size=3, family = "KaiTi") 
ggsave("TEX/figures/gtdts.png",units = "cm", width = 20, height = 12)

########################################################################################################
########################  Figure 1b-d ##############
########################################################################################################

gtdtotal <- gtd %>% 
  dplyr::mutate(ccode = countrycode(country_txt, "country.name", "cown")) %>% 
  filter(!is.na(ccode)) %>% 
  filter(!is.na(latitude)) %>% 
  filter(!is.na(longitude)) 

gtdtotal <- gtdtotal[which(gtdtotal$ccode %in% ccode), ]
########################################################################################################
########################  Figure 1b ##############
########################################################################################################

pdf("TEX/figures/africa_terr.pdf",width = 12, height = 12)
par(mar=c(1,1,1,1))
plot(world)
points(gtdtotal$longitude, gtdtotal$latitude, col = "blue", pch = 16, cex = .7)
dev.off()
#######################################################################################################
########################  Figure 1c ##############
#######################################################################################################

## only make 2004 and 2014
gtdtotal2004 <- gtdtotal %>%
              filter(iyear == 2004)


pdf("TEX/figures/africa_terr2004.pdf",width = 12, height = 12)
par(mar=c(1,1,1,1))
plot(world)
points(gtdtotal2004$longitude, gtdtotal2004$latitude, col = "blue", pch = 16, cex = .7)
dev.off()
#######################################################################################################
########################  Figure 1d ##############
#######################################################################################################

gtdtotal2014 <- gtdtotal %>%
            filter(iyear == 2014)

pdf("TEX/figures/africa_terr2014.pdf",width = 12, height = 12)
par(mar=c(1,1,1,1))
plot(world)
points(gtdtotal2014$longitude, gtdtotal2014$latitude, col = "blue", pch = 16, cex = .7)
dev.off()
#################################################################

#######################################################################################################
####### Figure 2
#######################################################################################################


load("./Clean_data/gtd_grid.RData")

terr_Nigeria <- gtd_grid %>% 
  filter(ccode == 475) %>% 
  filter(iyear > 2008 & iyear <2013) %>% 
  filter(!is.na(xcoord)) %>% 
  filter(!is.na(ycoord))

terr_Nigeria2 <- gtdtotal  %>% 
  filter(ccode == 475) %>% 
  filter(iyear > 2008 & iyear <2013) %>% 
  filter(!is.na(longitude)) %>% 
  filter(!is.na(latitude))
yearlytotal2 <- terr_Nigeria  %>% 
      group_by(iyear) %>% summarise(total = n())
worldmap <- cshp(date = as.Date("2015-12-31"), useGW = F)
nigeriamap <- worldmap[which(worldmap@data$CNTRY_NAME == "Nigeria"),] ##cowcod = 475
plot(nigeriamap)
nigeriamap_sh <- fortify(nigeriamap, region = "COWCODE")
load('./Clean_Data/nigeriaStamenLines.rda')

ggplot(terr_Nigeria, aes(map_id = ccode, x=xcoord,y=ycoord)) + 
  geom_map(map=nigeriamap_sh, fill='white',linetype=1, colour='grey30') +
  geom_point(shape=15, size=4.5,
             alpha=0.25) + 
  geom_point(data = terr_Nigeria2, aes(x=longitude,y=latitude),shape=16,
             size=1.5,color = "blue",alpha=0.25)+
  facet_wrap(~iyear) + 
  xlab('') + ylab('') +
  theme_map()+
  theme(
    line = element_blank(),
    rect = element_blank(), #defien the margin line
    legend.position = "right",
    legend.title=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    text = element_text(size=16, family = 'KaiTi')	
  )
ggsave("TEX/figures/terr_nigeria_map.pdf",width = 7, height = 6.5)  
#################################################################

#######################################################################################################
## Figure 3: postestimation
#################################################################
#######################################################################################################

library(viridis)
library(ggthemes)
library(viridis)
library(ggridges)
#######################################################################################################
## Figure 3a: postestimation
#######################################################################################################

#### insample: density kernel for distribution
load("./Clean_data/insample_df.RData")

ac_df <- insample_df %>%
  dplyr::group_by(year, failure) %>%
  dplyr::summarise(n = n()) %>%
  tidyr::spread(failure, n) %>%
  ungroup()
names(ac_df) <- c("year","cured","attack" )

ac_df %>%        
  ggplot(aes(x = year, y = cured)) +
  # Data
  geom_line(aes(y = cured), color = "blue",linetype = 6, size = 1.5)+
  geom_line(aes(y = attack*30), colour = "black", linetype = 1, size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~./30, name = "遭袭网格数量")) + 
  scale_x_continuous(breaks = seq(1994, 2013, 2)) + 
  theme_bw() +
  ylab("未被袭击的网格数量") + xlab("年份") + 
  annotate("segment", x = 1998, xend = 1998, y = 9000, yend = 10500,
           colour="blue", arrow=arrow(length=unit(0.2,"cm"))) + 
  annotate("text", x = 1998, y = 8850, parse = F, 
           label = "未被袭击的网格数量",size=4) +
  annotate("segment", x = 2002, xend = 2002, y = 5000, yend = 3550,
           colour="black", arrow=arrow(length=unit(0.2,"cm"))) + 
  annotate("text", x = 2002, y = 5100, parse = F, 
           label = "遭袭网格数量量",size=4) +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=12, family = 'KaiTi')) 
ggsave("TEX/figures/attack_grid.pdf", width = 8, height = 6)  

#######################################################################################################
## Figure 3b: postestimation
#######################################################################################################

risk_pred <- insample_df %>% 
  dplyr::filter(crisk < .05) %>%
  # dplyr::filter(ccure < .95) %>%
  #  dplyr::select(year, truncateccure, chazard, crisk) %>% 
  dplyr::mutate(year = factor(year),
                crisk = crisk,
                chazard = chazard) %>% 
  dplyr::mutate(year = paste0(year, "年"))

ggplot(risk_pred, aes(x = crisk, y = factor(year), height=..density.., fill = factor(year))) +
  geom_density_ridges(col = "grey70", scale = 1.2, show.legend = F) +
  scale_fill_viridis(discrete = TRUE) +
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2) +
  theme_ridges(font_size = 12, grid = F, center_axis_labels = T) +
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=12, family = 'KaiTi'))

ggsave("TEX/figures/low_pi.pdf", width = 8, height = 6)  

#################################################################

#######################################################################################################
## Figure 4: postestimation
#################################################################
#######################################################################################################

### in-sample prediction for conditional cured: loglog
cc_Nigeria <- insample_df %>% 
  filter(gwno == 475) %>% 
  filter(!is.na(xcoord)) %>% 
  filter(!is.na(ycoord))
terr_Nigeria <- cc_Nigeria %>% filter(failure ==1)

worldmap <- cshp(date = as.Date("2013-12-31"), useGW = F)
nigeriamap <- worldmap[which(worldmap@data$CNTRY_NAME == "Nigeria"),] ##cowcod = 475
plot(nigeriamap)
nigeriamap_sh <- fortify(nigeriamap, region = "COWCODE")

ggplot(cc_Nigeria, aes(map_id = gwno, x=xcoord,y=ycoord)) + 
  geom_map(map=nigeriamap_sh, fill='white',linetype=1, colour='grey30') +
  inset_ggmap(ngaLines) +
  geom_point(data=cc_Nigeria, aes(x=xcoord,y=ycoord, color=ccure), 
             shape=15, size=4.5,
             alpha=0.25) + 
  geom_point(data=terr_Nigeria, aes(x=xcoord,y=ycoord), size=1.5)+
  scale_color_viridis(na.value="#FFFFFF00")+  
  facet_wrap(~year) + 
  xlab('') + ylab('') +
  theme(
    line = element_blank(),
    rect = element_blank(), #defien the margin line
    legend.position = "right",
    legend.title=element_blank(),
    panel.border=element_blank(),
    panel.grid=element_blank(),
    axis.ticks=element_blank(),
    axis.text=element_blank(),
    strip.text.x = element_text(color='white'),
    strip.background = element_rect(fill = "#525252", color='#525252')		
  )

ggsave("TEX/figures/cured_nigeria_map.pdf",width = 12, height = 10)  

#################################################################

#######################################################################################################
## Figure 5: postestimation
#################################################################
#######################################################################################################

library(gridExtra)
source("./Code_paper/figure_setup.R")

##########################################################
## load data
load("./Clean_data/data_full.RData")
#######################################################################################################
## Figure 5b: postestimation
#######################################################################################################

data_full <- data_full %>%
  dplyr::group_by(gwno, year)%>%
  dplyr::mutate(cty_terr_total = sum(nterr),
                cty_terr_total_t1 = sum(nterr_t1))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(year)%>%
  dplyr::mutate(year_terr_total = sum(nterr),
                year_terr_total_t1 = sum(nterr_t1))%>%
  dplyr::mutate(af_totalexcl = year_terr_total - cty_terr_total,
                af_totalexcl_t1 = year_terr_total_t1 - cty_terr_total_t1) %>%
  dplyr::ungroup()



#### lag all IVs
data_full <- data_full %>%
  dplyr::mutate(neighboringInterStateWar_t1 = lag(neighboringInterStateWar, 1),
                neighboringcivilwar_t1 = lag(neighboringcivilwar, 1),
                polity2_t1 = lag(polity2, 1),
                lngdppc_t1 = lag(lngdppc, 1),
                lntpop_t1 = lag(lntpop, 1),
                milexpr_t1 = lag(milexpr, 1),
                irregular_dummy_t1 = lag(irregular_dummy, 1),
                number.total.conflict_t1 = lag(number.total.conflict, 1),
                nlights_max_t1 = lag(nlights_max, 1),
                excluded_t1 = lag(excluded, 1),
                state_vi_t1 = lag(state_vi, 1))%>%
  filter(year > 1993)
#notice that in the GTD all records of terrorist attacks during 1993 were lost


## DV: duration & atrisk
## IVs: 
# subnational:  nlights_max, mountains_mean, capdist,bdist2, excluded, state_vi,petroleum_s
# country-level: polity2, lngdppc, lntpop, milexpr, irregular_dummy, number.total.conflict, 
# international; neighboringInterStateWar, neighboringcivilwar, year_terr_total cty_terr_total
## spatial lags: nterr_s1, nterr_s2
data_full <- as.data.frame(data_full)

ccode <- unique(data_full$gwno)
africamap <- cshp(date = as.Date("2010-12-31"), useGW = F)
africamap <- africamap[which(africamap@data$COWCODE %in% ccode),]
africamap_shp <- fortify(africamap, region = "COWCODE")
africamap_shp$id <- as.integer(africamap_shp$id)
africamap_shp <- africamap_shp[order(africamap_shp$order), ] 
# if selected, gray; otherwise white
africamap_shp <- africamap_shp %>%
  dplyr::mutate(cols = ifelse(id == 490 | id == 615, 1, 0))


capitals <- data.frame(name = c("Kinshasa", "Algiers"),
                       xcoord = c(15.3150000, 3.0505560),
                       ycoord = c(-4.3297220, 36.763050))

# Congo, DRC 2342042.8446 Kinshasa 15.3150000 -4.3297220 ccode =490
# Algeria 2326148.4413 Algiers 3.0505560 36.763050 ccode = 615

odds_df <- data_full %>% filter(gwno == 490 | gwno == 615) %>%
  dplyr::select(gid, year,gwno, terr_dum, nterr, xcoord, ycoord, 
                neighboringInterStateWar_t1, neighboringcivilwar_t1,  
                year_terr_total_t1 , cty_terr_total_t1 , polity2_t1 , 
                lngdppc_t1 , lntpop_t1 , milexpr_t1 , irregular_dummy_t1 , 
                number.total.conflict_t1 , nlights_max_t1 , mountains_mean ,
                capdist , bdist2 , excluded_t1 , state_vi_t1 , petroleum_s ,
                nterr_t1 , nterr_s1 ,  nterr_s2)
# select four gids
cooridates2010 <- data_full %>% 
  filter(year ==2010) %>%
  filter(gid == 135058 | gid ==127852 | gid == 182529 | gid == 172432) %>%
  dplyr::select(gid, xcoord, ycoord)

cooridates_df <- odds_df %>%
  filter(gid == 135058 | gid ==127852 | gid == 182529 | gid == 172432)


cooridates_df_wide <- cooridates_df %>%
  dplyr::select(gid, year, nterr) %>%
  tidyr::spread(gid, nterr) 
names(cooridates_df_wide)[2:5] <- paste("gid", names(cooridates_df_wide)[2:5], sep = "_")

cooridates_df_wide %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = gid_135058), colour = "blue", linetype = 6, size = 1.5) + 
  geom_line(aes(y = gid_182529), colour = "black", linetype = 1, size = 1)+
  scale_x_continuous(breaks = seq(1994, 2013, 2)) + 
  ylab("袭击数量") + xlab("年份") + 
  theme_bw() +
  annotate("segment", x = 2005, xend = 2008, y = 1, yend = 1,
           colour="blue", arrow=arrow(length=unit(0.2,"cm"))) + 
  annotate("text", x = 2004, y = 1, parse = F, 
           label = "网格 135058",size=4,family = 'KaiTi') +
  annotate("segment", x = 2005, xend = 2008, y = 13, yend = 13,
           colour="black", arrow=arrow(length=unit(0.2,"cm"))) + 
  annotate("text", x = 2004, y = 13, parse = F, 
           label = "网格 182529",size=4,family = 'KaiTi') +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size=12, family = 'KaiTi')) 
ggsave("TEX/figures/tworid_lines.pdf", width = 8, height = 6)  

#######################################################################################################
## Figure 5a: postestimation
#######################################################################################################


library(ggrepel)
pdf("TEX/figures/sel_points.pdf",width = 8, height = 8)
ggplot() + 
  geom_polygon(data = africamap_shp, 
               aes(x = long, y = lat, group = group, fill = factor(cols)) , 
               color = "black",
               size = 0.25) + coord_fixed() +
  geom_point(data = cooridates2010, aes(x = xcoord, y = ycoord), 
             shape = 15, size = 3, color = "blue") +
  geom_text_repel(data = cooridates2010,
                  aes(xcoord, ycoord,label = gid, group = gid),
                  color = 'blue',
                  size  = 3,
                  box.padding = 0.4, point.padding = 0.5) +
  geom_point(data = capitals, aes(x = xcoord, y = ycoord), 
             shape = 17, size = 3.5, color = "black") +
  geom_text_repel(data = capitals,
                  aes(xcoord, ycoord,label = name, group = name),
                  color = 'black',
                  size  = 3,
                  box.padding = 0.4, point.padding = 0.7) +
  scale_colour_distiller(type = "seq", palette = 1, na.value = "gray", guide = "colourbar") +
  scale_fill_manual(values = c("white","gray"), 
                    name= "", guide = guide_legend(reverse = TRUE)) + 
  theme(line = element_blank(),
        legend.position = "none",
        rect = element_blank(), #defien the margin line
        axis.text = element_blank(),
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0,"cm"),
        panel.spacing = unit(0, "lines"), 
        text = element_text(size=16, family = 'KaiTi'),
        plot.margin = unit(c(0,  0, 0, 0), "lines")) 
dev.off()

#######################################################################################################
## Figure 5c: postestimation
#######################################################################################################


cooridates_chazard <- insample_df %>%
  filter(gid == 135058 | gid ==127852 | gid == 182529 | gid == 172432)
library(ggrepel)
cooridates_chazard %>%
  dplyr::mutate(label = if_else(year ==max(year), as.character(gid), NA_character_)) %>%
  ggplot(aes(x = year, y = crisk, group = gid, linetype = factor(gid))) + 
  geom_line() + 
  geom_label_repel(aes(label = label), nudge_x =1, na.rm = TRUE) + 
  scale_x_continuous(breaks = seq(1994, 2013, 2)) + 
  ylab("危险率") + xlab("年份") + 
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=12, family = 'KaiTi')) 
ggsave("TEX/figures/twogid_chazard.pdf", width = 8, height = 6)   

#######################################################################################################
## Figure 5d-5g: postestimation
#######################################################################################################


gids <- cooridates2010$gid

for (i in 1:length(gids)) {
  
  df_points <- data_full %>% 
    filter(gid == gids[i] ) %>% 
    dplyr::select(neighboringInterStateWar_t1, neighboringcivilwar_t1,  
                  year_terr_total_t1 , cty_terr_total_t1 , polity2_t1 , 
                  lngdppc_t1 , lntpop_t1 , milexpr_t1 , irregular_dummy_t1 , 
                  number.total.conflict_t1 , nlights_max_t1 , mountains_mean ,
                  capdist , bdist2 , excluded_t1 , state_vi_t1 , petroleum_s ,
                  nterr_t1 , nterr_s1 ,nterr_s2)
  
  vals <- c(1, as.numeric(df_points[17,])) # use year = 2010, add 1 as intercept
  
  ht <- hazardpred_plot_df(spdur_loglog, xvals =  vals, zvals = vals)
  ggplot(ht, aes(x=Time, y=surv)) +
    geom_line() + 
    xlab(' 存活时间(年)') + ylab('危险率') +
    geom_ribbon(aes(ymin=lo95,ymax=up95), alpha=0.15) +
    scale_x_continuous(breaks = seq(1, 30, 5)) + 
    theme_bw() +
    theme(legend.position='bottom',
          legend.spacing.x = unit(.5, 'cm'),
          legend.title=element_blank(),
          axis.ticks=element_blank(), 
          panel.border = element_blank(),
          legend.text=element_text(size=15),
          text = element_text(size=15, family = 'KaiTi'),
          axis.title.y = element_text(margin = margin(1,1,1,1)),
          axis.text = element_text(size=15),
          axis.title=element_text(size=15))
  
  ggsave(paste0(paste( "TEX/figures/harzard",gids[i], sep = "_"),".pdf"), width = 8, height = 6)
  
}

#######################################################################################################
##############################################################################
## Figure 6: coefficient
#######################################################################################################

##
load("./Clean_data/models.RData")
### no lags
m_loglog <- plot_spdm_coef(spdur_loglog)
p_duration_loglog <- m_loglog[[1]]   
p_duration_df <- m_loglog[[1]]$data
p_duration_df$vars <- factor(p_duration_df$vars, levels = c("(Intercept)", 
                                                            "bdist2",
                                                            "capdist",
                                                            "mountains_mean",
                                                            "petroleum_s",
                                                            "nlights_max",
                                                            "excluded",
                                                            "state_vi",
                                                            "nterr_t1",
                                                            "nterr_s1", 
                                                            "nterr_s2",
                                                            "lngdppc",
                                                            "lntpop",
                                                            "milexpr",
                                                            "polity2",
                                                            "number.total.conflict",
                                                            "cty_terr_total",
                                                            "irregular_dummy" ,
                                                            "neighboringcivilwar" ,
                                                            "neighboringInterStateWar",
                                                            "year_terr_total"
))

p_duration_loglog <- ggplot(p_duration_df, aes(x=vars, y=beta, color=color_sig)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=lo95, ymax=hi95),alpha = 1, size = 1.5) + 
  coord_flip()  +
  theme_bw() +
  xlab('') + ylab('') +
  theme(legend.position="none",
        legend.title=element_blank(),
        axis.text = element_text(size=14),
        text = element_text(size=14, family = 'KaiTi'),
        plot.title = element_text(hjust = .5, size = 16, face = "bold"))

p_duration_loglog <- p_duration_loglog + scale_x_discrete(labels=c("截距",
                                                                   "网格到最近邻国距离",
                                                                   "网格距首都距离",
                                                                   "网格的山地面积百分比",
                                                                   "网格蕴藏石油",
                                                                   "网格夜间灯光(最大值)",
                                                                   "网格中被排斥族群",
                                                                   "网格中政府暴力数",
                                                                   "网格中恐袭总数，t-1",
                                                                   "邻近网格遭袭的数量，t-1",
                                                                   "邻近网格遭袭的数量，t-2",
                                                                   "本国人均GDP(对数)",
                                                                   "本国总人口(对数)",
                                                                   "本国军费开支",
                                                                   "本国政体分值",
                                                                   "本国内战数量",
                                                                   "本国恐袭总数",
                                                                   "本国非正常领导更迭",
                                                                   "邻国处于内战",
                                                                   "邻国处于国际冲突",
                                                                   "非洲恐袭总数")) + ggtitle("持续期方程") 
p_risk_loglog <- m_loglog[[2]]   
p_risk_df <- m_loglog[[2]]$data
p_risk_df$vars <- factor(p_risk_df$vars, levels = c("(Intercept)", 
                                                    "bdist2",
                                                    "capdist",
                                                    "mountains_mean",
                                                    "petroleum_s",
                                                    "nlights_max",
                                                    "excluded",
                                                    "state_vi",
                                                    "nterr_t1",
                                                    "nterr_s1", 
                                                    "nterr_s2",
                                                    "lngdppc",
                                                    "lntpop",
                                                    "milexpr",
                                                    "polity2",
                                                    "number.total.conflict",
                                                    "cty_terr_total",
                                                    "irregular_dummy" ,
                                                    "neighboringcivilwar" ,
                                                    "neighboringInterStateWar",
                                                    "year_terr_total"
))

p_risk_loglog <- ggplot(p_risk_df, aes(x=vars, y=beta, color=color_sig)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_point(size=3) + 
  geom_linerange(aes(ymin=lo95, ymax=hi95),alpha = 1, size = 1.5) + 
  coord_flip()  +
  theme_bw() +
  xlab('') + ylab('') +
  theme(legend.position="none",
        legend.title=element_blank(),
        axis.text = element_text(size=14),
        text = element_text(size=14, family = 'KaiTi'),
        plot.title = element_text(hjust = .5, size = 16, face = "bold"))

p_risk_loglog <- p_risk_loglog + scale_x_discrete(labels=c("截距",
                                                           "网格到最近邻国距离",
                                                           "网格距首都距离",
                                                           "网格的山地面积百分比",
                                                           "网格蕴藏石油",
                                                           "网格夜间灯光(最大值)",
                                                           "网格中被排斥族群",
                                                           "网格中政府暴力数",
                                                           "网格中恐袭总数，t-1",
                                                           "邻近网格遭袭的数量，t-1",
                                                           "邻近网格遭袭的数量，t-2",
                                                           "本国人均GDP(对数)",
                                                           "本国总人口(对数)",
                                                           "本国军费开支",
                                                           "本国政体分值",
                                                           "本国内战数量",
                                                           "本国恐袭总数",
                                                           "本国非正常领导更迭",
                                                           "邻国处于内战",
                                                           "邻国处于国际冲突",
                                                           "非洲恐袭总数")) + ggtitle("风险方程") 

p_alpha_loglog <- m_loglog[[3]] + scale_x_discrete(labels=expression(log(alpha))) + ggtitle("形状参数") 
library(gridExtra)
pdf("TEX/figures/coefplot_p_loglog.pdf",width = 12, height = 6.5)
grid.arrange(arrangeGrob(p_duration_loglog, p_alpha_loglog, ncol = 1,heights = c(6,1)), p_risk_loglog, ncol = 2)
dev.off() 

#######################################################################################################
###############################################################
## Figure 7
#######################################################################################################
### Figure 7a:
###############################################################

## conditional hazard: no lag
bdist2_low <- hazard_plot_df(spdur_loglog, xpred_name = "bdist2", xpred_value = 10,
                             scenario = "距离10千米")
bdist2_high <- hazard_plot_df(spdur_loglog, xpred_name = "bdist2", xpred_value = 500,
                              scenario = "距离500千米")
scenario_bdist2 <- bind_rows(bdist2_low,  bdist2_high)
### two togther
ggplot(scenario_bdist2 , aes(x=Time, y=surv, fill=scenario, color=scenario)) +
  geom_line() + 
  xlab(' 时间(年)') + ylab('条件危险') +
  scale_x_continuous(breaks = seq(1, 30, 5))+
  geom_ribbon(aes(ymin=lo95,ymax=up95), alpha=0.15) +
  theme_bw() +
  theme(legend.position='none',
        legend.spacing.x = unit(.5, 'cm'),
        legend.title=element_blank(),
        axis.ticks=element_blank(), 
        panel.border = element_blank(),
        text = element_text(size=15, family = 'KaiTi'),
        axis.title.y = element_text(margin = margin(1,1,1,1)),
        axis.text = element_text(size=15),
        axis.title=element_text(size=15)) + 
  annotate("segment", x = 9, xend = 3, y = 0.012, yend = 0.012,
           colour="black", arrow=arrow(length=unit(0.2,"cm"))) + 
  annotate("text", x = 10, y = 0.012, parse = F, 
           label = "距离10千米",size=4) + 
  annotate("segment", x = 9, xend = 2.5, y = 0.006, yend = 0.004,
           colour="black", arrow=arrow(length=unit(0.2,"cm"))) + 
  annotate("text", x = 10, y = 0.006, parse = F, 
           label = "距离500千米",size=4) 
ggsave("TEX/figures/ch_bdist2loglog.pdf", width = 8, height = 6.5)
#######################################################################################################
#############################################################
### Figure 7b
#######################################################################################################


#conditional hazard: no lag
capdist_low <- hazard_plot_df(spdur_loglog, xpred_name = "capdist", xpred_value = 100,
                              scenario = "距离100千米")
capdist_high <- hazard_plot_df(spdur_loglog, xpred_name = "capdist", xpred_value = 1000,
                               scenario = "距离1000千米")
scenario_capdist <- bind_rows(capdist_low, capdist_high)
### two togther
ggplot(scenario_capdist, aes(x=Time, y=surv, fill=scenario, color=scenario)) +
  geom_line() + 
  xlab(' 时间(年)') + ylab('条件危险') +
  geom_ribbon(aes(ymin=lo95,ymax=up95), alpha=0.15) +
  scale_x_continuous(breaks = seq(1, 30, 5))+
  theme_bw() +
  theme(legend.position='none',
        legend.spacing.x = unit(.5, 'cm'),
        legend.title=element_blank(),
        axis.ticks=element_blank(), 
        panel.border = element_blank(),
        text = element_text(size=15, family = 'KaiTi'),
        axis.title.y = element_text(margin = margin(1,1,1,1)),
        axis.text = element_text(size=15),
        axis.title=element_text(size=15)) + 
  annotate("segment", x = 9, xend = 1.75, y = 0.02, yend = 0.02,
           colour="black", arrow=arrow(length=unit(0.2,"cm"))) + 
  annotate("text", x = 10, y = 0.02, parse = F, 
           label = "距离100千米",size=4) + 
  annotate("segment", x = 9, xend = 2.26, y = 0.01, yend = 0.0057,
           colour="black", arrow=arrow(length=unit(0.2,"cm"))) + 
  annotate("text", x = 10, y = 0.01, parse = F, 
           label = "距离1000千米",size=4) 
ggsave("TEX/figures/ch_capdist_loglog.pdf", width = 8, height = 6.5)  


#######################################################################################################
### Figure 7c
#######################################################################################################
# conditional hazard and conditional risk

### ### hazard Plot no lag
quantile(data_full$nlights_max, probs = c(0.025, 0.975))
nlight_low <- hazard_plot_df(spdur_loglog, xpred_name = "nlights_max", xpred_value = 5,
                             scenario = "低夜光")
nlight_high <- hazard_plot_df(spdur_loglog, xpred_name = "nlights_max", xpred_value = 30,
                              scenario = "高夜光")
scenario_nlight <- bind_rows(nlight_low, nlight_high)
### two togther
ggplot(scenario_nlight, aes(x=Time, y=surv, fill=scenario, color=scenario)) +
  geom_line() + 
  xlab(' 时间(年)') + ylab('条件危险') +
  geom_ribbon(aes(ymin=lo95,ymax=up95), alpha=0.15) +
  scale_x_continuous(breaks = seq(1, 30, 5))+
  theme_bw() +
  theme(legend.position='none',
        legend.spacing.x = unit(.5, 'cm'),
        legend.title=element_blank(),
        axis.ticks=element_blank(), 
        panel.border = element_blank(),
        legend.text=element_text(size=15),
        text = element_text(size=15, family = 'KaiTi'),
        axis.title.y = element_text(margin = margin(1,1,1,1)),
        axis.text = element_text(size=15),
        axis.title=element_text(size=15)) + 
  annotate("segment", x = 9, xend = 1.5, y = 0.04, yend = 0.04,
           colour="black", arrow=arrow(length=unit(0.2,"cm"))) + 
  annotate("text", x = 10, y = 0.04, parse = F, 
           label = "高夜光",size=4) + 
  annotate("segment", x = 9, xend = 1.5, y = 0.02, yend = 0.008,
           colour="black", arrow=arrow(length=unit(0.2,"cm"))) + 
  annotate("text", x = 10, y = 0.02, parse = F, 
           label = "低夜光",size=4) 
ggsave("TEX/figures/ch_nighttime_loglog.pdf", width = 8, height = 6.5)


#######################################################################################################
### Figure 7d
#######################################################################################################
## conditional hazard and conditional risk
## nterr_s2
##### conditional hazard: with lag
quantile(data_full$nterr_s2, probs = c(0.025, 0.975))
nterr_s2_low <- hazard_plot_df(lag_spdur_loglog, xpred_name = "nterr_s2", xpred_value = 0,
                               scenario = "0次")
nterr_s2_high <- hazard_plot_df(lag_spdur_loglog, xpred_name = "nterr_s2", xpred_value = 30,
                                scenario = "30次")
scenario_nterr_s2 <- bind_rows(nterr_s2_low, nterr_s2_high)
### two togther
ggplot(scenario_nterr_s2, aes(x=Time, y=surv, fill=scenario, color=scenario)) +
  geom_line() + 
  xlab(' 时间(年)') + ylab('条件危险') +
  scale_x_continuous(breaks = seq(1, 30, 5))+
  geom_ribbon(aes(ymin=lo95,ymax=up95), alpha=0.15) +
  theme_bw() +
  theme(legend.position='bottom',
        legend.spacing.x = unit(.5, 'cm'),
        legend.title=element_blank(),
        axis.ticks=element_blank(), 
        panel.border = element_blank(),
        text = element_text(size=15, family = 'KaiTi'),
        axis.title.y = element_text(margin = margin(1,1,1,1)),
        axis.text = element_text(size=15),
        axis.title=element_text(size=15))
ggsave("TEX/figures/ch_nterr_s2lagloglog.pdf", width = 8, height = 6.5) 


#######################################################################################################
## Figure 8a
#######################################################################################################
## conditional hazard and conditional risk
################ out-of-sample ROC and PR

load( "./Clean_data/train_spdur_loglog.RData")
load("./Clean_data/outsample_df.RData")
## ROC for out of sample
trainroc_spdur<- spdur_roc_plot(ModelResults = list(train_spdur_loglog), 
                                modelname <- c("训练集(1994-2011)")) 

#test_ROC
test_roc = Map(function(x, y) roc(x,y), list(outsample_df[,"terr_dum"]),
               list(outsample_df[,"chazard"]))

names(test_roc) <- "测试集(2012-2013)"

testroc_spdur = lapply(test_roc, function(x) FUN= data.frame(
  plotx = x$specificities,
  ploty = rev(x$sensitivities),
  name = paste("AUC =",
               sprintf("%.3f",x$auc)))) %>%
  map_df(., rbind, .id="modelname")

outofsample <- bind_rows(trainroc_spdur, testroc_spdur)


ggplot(data= outofsample, aes(x=plotx, y=ploty, linetype=modelname)) + 
  scale_x_continuous(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  geom_line() +
  geom_abline(slope=1, color="gray", alpha=0.5) +
  scale_linetype_manual(name = '',
                        values=c("solid", "dashed"))+
  labs(x="假阳性(False Positive Rate)", y="真阳性(True positive rate)") + 
  theme_bw() +
  theme(legend.position=c(.7, 0.3),
        text = element_text(size=15, family = 'KaiTi'),
        axis.title.y = element_text(margin = margin(1,1,1,1)),
        axis.text = element_text(size=15),
        axis.title=element_text(size=15))
ggsave("TEX/figures/outofsample_roc.pdf", width = 6.5, height = 6.5)

#######################################################################################################
### Figure 8b
library(pROC)
library(ROCR)
#######################################################################################################

### PR plot: out-of-sample
trainpr_spdur <- spdur_pr_plot(ModelResults = list(train_spdur_loglog), 
                               modelname <- c("训练集(1994-2011)"))

testpr_df = Map(function(x, y)  prediction(x, y), list(outsample_df[,"chazard"]),
                list(outsample_df[,"terr_dum"]))

testpr_df <- lapply(testpr_df,function(x) FUN = performance(x, "prec", "rec"))

names(testpr_df) <- "测试集(2012-2013)"

testpr_spdur = lapply(testpr_df, function(x) FUN= data.frame(
  rec = x@x.values[[1]],
  prec = x@y.values[[1]]))%>%
  map_df(., rbind, .id="modelname")
testpr_spdur[1,3] <- 0

outofsamplepr_df <- bind_rows(trainpr_spdur,testpr_spdur)

ggplot(data=outofsamplepr_df, aes(x=rec, y=prec, linetype=modelname)) + 
  geom_line() + 
  scale_x_continuous(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  geom_abline(slope= -1, intercept = 1, color="gray", alpha=0.5) +
  labs(x="查全率(Recall)", y="查准率(Precision)") + 
  scale_linetype_manual(name = '',
                        values=c("solid", "dashed"))+
  theme_bw() + 
  theme(legend.position=c(.75, 0.7),
        text = element_text(size=15, family = 'KaiTi'),
        axis.title.y = element_text(margin = margin(1,1,1,1)),
        axis.text = element_text(size=15),
        axis.title=element_text(size=15)) 
ggsave("TEX/figures/outofsample_prc.pdf", width = 6.5, height = 6.5)

################################################################################
rm(list = ls())

