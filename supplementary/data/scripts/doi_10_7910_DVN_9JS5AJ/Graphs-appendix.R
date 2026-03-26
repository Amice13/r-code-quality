#####################################################################################
# Create all figures for the appendix
# Soeren Henn
# 2022-08-21
#####################################################################################


rm(list=ls())

## Set working directory

## Load libraries
library(lfe)        # to run linear fixed effects models
library(ggplot2)    # to draw graphs
library(rgdal)      # to read shapefiles
library(plyr)       # to join two data frames
library(rdd)        # for McCrary test
library(ggsn)       # to add scale bar
library(rgeos)      # to calculate gDifference
library(ggmap)      # to download maps from google
library(dplyr)
library(cowplot)    # for plot_grid

## !!!!! Note that you have specify a google key here
register_google(key="ADD HERE")





####################################################################################
############ Figure A1: Bin-scatter between state capacity and distance ############
####################################################################################

## Log Distance Afrobarometer
load("DataRegression.RData")

data.reg <- data.all
data.reg <- data.reg[which(data.reg$distance_hq<100),]
data.reg$logdistance <- log(data.reg$distance_hq)

ggplot(data.reg, aes(x=logdistance,y=state_capacity)) +
  geom_histogram(mapping=aes(x=logdistance, y = (..density..*0.2)), data=data.reg, bins=100, color='grey', alpha=I(.2), position=position_nudge(x = 0, y = -1)) +
  stat_summary_bin(fun.y='mean', bins=100,
                   color='orange', size=2, geom='point') +
  geom_smooth() +
  geom_smooth(method = 'lm', color='red') +
  theme_bw() +
  ylab("State Capacity Index") +
  xlab("Log Distance to HQ") +
  ylim(-1,1) +
  theme(text=element_text(size=20))

ggsave("figure_A1_LogDistance.pdf", width = 5, height = 5)


## Log Distance DHS
load("DataRegressionDHS.RData")

data.reg <- data.all
data.reg <- data.reg[which(data.reg$distance_hq<100),]
data.reg$logdistance <- log(data.reg$distance_hq)

ggplot(data.reg, aes(x=logdistance,y=state_capacity)) +
  geom_histogram(mapping=aes(x=logdistance, y = (..density..*0.2)), data=data.reg, bins=100, color='grey', alpha=I(.2), position=position_nudge(x = 0, y = -1)) +
  stat_summary_bin(fun.y='mean', bins=100,
                   color='orange', size=2, geom='point') +
  geom_smooth() +
  geom_smooth(method = 'lm', color='red') +
  theme_bw() +
  ylab("State Capacity Index") +
  xlab("Log Distance to HQ") +
  ylim(-1,1)

ggsave("figure_A1_LogDistanceDHS.pdf", width = 5, height = 5)


#####################################################################################
###################### Figure A2: Illustration of Identification ####################
#####################################################################################
## Nigeria
## Yobe and Borno state

########## topography
## code source: https://gis.stackexchange.com/questions/155334/ggmap-clip-a-map

## data from soerenhenn.com/data
shape <- readOGR(dsn=".", layer="NGA_adm1_2017")  # load shapefile


## just subset to two states
shape@data$sample <- 0
shape@data$sample[which(shape@data$statename=="Yobe")] <- 1
shape@data$sample[which(shape@data$statename=="Borno")] <- 1

shape <- shape[which(shape@data$sample==1),]

ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}

bb <- bbox(shape)
ll_means <- rowMeans(bb)
sq_map <- get_googlemap(center = c(lon=ll_means[1],lat=ll_means[2]), maptype = "satellite",  zoom = 7, key = "AIzaSyD_Hu54Gni7f4Kd__NYM_BsMq9Quxb9jXQ")

sq_map.rast <- ggmap_rast(map = sq_map) # convert google map to raster object

sq_map.only <- crop(sq_map.rast, bb)
#sq_map.only <- sq_map.rast
sq.df <- data.frame(rasterToPoints(sq_map.only))

bw_map <- get_googlemap(center =  c(lon=ll_means[1],lat=ll_means[2]), zoom = 7,
                        color = "bw",
                        style = "feature:road|visibility:on&style=element:labels|visibility:off&style=feature:administrative|visibility:off")


## load headquarters
hq <- read.csv("state_2017.csv")

hq$sample <- 0
hq$sample[which(hq$state=="Yobe State")] <- 1
hq$sample[which(hq$state=="Borno State")] <- 1
hq <- hq[which(hq$sample==1),]
hq$type <- "State Capital"

## two hypothetical villages
#villages <- as.data.frame(cbind(c(11.9, 11.88), c(12.210, 12.33)))
villages <- as.data.frame(cbind(c(11.7611521, 11.7385957), c(12.2949253, 12.1563445)))
colnames(villages) <- c("latitude", "longitude")
villages$type <- "Village"


## prepare data for ggplot
shape@data$id = rownames(shape@data)
shape@data$admin = "State Boundaries"
shape.points = fortify(shape, region="id")
shape.df = join(shape.points, shape@data, by="id")



borders <- as(shape,"SpatialLines")
border.trans <- spTransform(borders, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

border.trans <- SpatialLinesDataFrame(border.trans, as.data.frame(rbind(c(1), c(1))), match.ID=F)
x <- as.data.frame(cbind(c(1, 2), c(1, 2)))
border.trans@data <- x
border.trans@data$admin = "State Boundaries"


ggmap(bw_map) + 
  geom_path(data=border.trans, aes(long,lat,group=group, size="State Boundary"), color="black") +
  geom_point(data=hq, aes(x=longitude,y=latitude, shape=type), size=3) +
  geom_text(data=hq, aes(x=longitude,y=latitude, label=capital),vjust=0, nudge_x =-.1, nudge_y =-.12) +
  geom_point(data=villages, aes(x=longitude,y=latitude, shape=type), size=3) +
  scale_size_manual(values=c(1)) +
  scale_shape_manual(values=c(15, 17)) +
  north(symbol = 16, scale = 0.15, location="bottomright", x.min = bb[1,1], x.max=bb[1,2], y.min = bb[2,1], y.max= bb[2,2]) +
  scalebar(dist = 50, model = "WGS84", height=0.01, transform=T, dist_unit="km", x.min = bb[1,1], x.max=bb[1,2], y.min = bb[2,1], y.max= bb[2,2], location = "bottomright") +
  coord_fixed(ylim=c(bb[2,1],bb[2,2]), xlim=c(bb[1,1],bb[1,2])) +
  theme(text=element_text(size=20), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank(),
        legend.title=element_blank(), legend.position=c(0.1,0.1), legend.key = element_blank(), legend.background = element_blank())

ggsave("figure_A2_example.pdf", width = 7.53, height = 5.55)


#####################################################################################
#################### Figure A3: Results of Leaving out Countries ####################
#####################################################################################
load("RegressionSample.RData")

data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg <- data.reg[which(data.reg$distance_hq<150),]
data.reg$treatment <- data.reg$treatment_log5
data.reg$treatment_int <- data.reg$treatment_int_log5

data.reg$distance_border[which(data.reg$treatment==0)] <- -data.reg$distance_border[which(data.reg$treatment==0)]

data.reg<- data.reg[-which(is.na(data.reg$treatment)), ]

data.reg$treatment_int <- data.reg$treatment_int * (data.reg$dist_coeff_log*-1)

data.reg$dist_treat <- data.reg$treatment * data.reg$distance_border

data.reg.temp <- data.reg[which(data.reg$country!=2),]
model1a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model1b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=3),]
model2a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model2b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=4),]
model3a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model3b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=5),]
model4a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model4b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=6),]
model5a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model5b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=8),]
model6a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model6b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=9),]
model65a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model65b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=10),]
model7a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model7b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=11),]
model75a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model75b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=12),]
model8a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model8b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=13),]
model85a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model85b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=14),]
model9a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model9b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=15),]
model10a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model10b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=16),]
model11a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model11b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=17),]
model12a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model12b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=20),]
model13a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model13b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=21),]
model14a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model14b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=22),]
model15a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model15b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=23),]
model16a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model16b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=24),]
model17a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model17b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=25),]
model18a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model18b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=26),]
model19a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model19b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=29),]
model20a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model20b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=30),]
model21a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + popd | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model21b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + popd | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=32),]
model22a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + popd | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model22b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + popd | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=33),]
model23a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + popd | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model23b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + popd | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)

data.reg.temp <- data.reg[which(data.reg$country!=34),]
model24a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + popd | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==1)
model24b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + popd | fixed_effects |0| district_id, data.reg.temp, subset = inst_chiefs_dum==0)


model1aFrame <- data.frame(Variable = rownames(summary(model1a)$coef),
                           Coefficient = summary(model1a)$coef[3, 1],
                           SE = summary(model1a)$coef[3, 2],
                           modelName = "Recognized",
                           country = "Benin")
model1bFrame <- data.frame(Variable = rownames(summary(model1b)$coef),
                           Coefficient = summary(model1b)$coef[3, 1],
                           SE = summary(model1b)$coef[3, 2],
                           modelName = "Not Recognized",
                           country = "Benin")
model2aFrame <- data.frame(Variable = rownames(summary(model2a)$coef),
                           Coefficient = summary(model2a)$coef[3, 1],
                           SE = summary(model2a)$coef[3, 2],
                           modelName = "Recognized",
                           country = "Botswana")
model2bFrame <- data.frame(Variable = rownames(summary(model2b)$coef),
                           Coefficient = summary(model2b)$coef[3, 1],
                           SE = summary(model2b)$coef[3, 2],
                           modelName = "Not Recognized",
                           country = "Botswana")
model3aFrame <- data.frame(Variable = rownames(summary(model3a)$coef),
                           Coefficient = summary(model3a)$coef[3, 1],
                           SE = summary(model3a)$coef[3, 2],
                           modelName = "Recognized",
                           country = "Burkina Faso")
model3bFrame <- data.frame(Variable = rownames(summary(model3b)$coef),
                           Coefficient = summary(model3b)$coef[3, 1],
                           SE = summary(model3b)$coef[3, 2],
                           modelName = "Not Recognized",
                           country = "Burkina Faso")
model4aFrame <- data.frame(Variable = rownames(summary(model4a)$coef),
                           Coefficient = summary(model4a)$coef[3, 1],
                           SE = summary(model4a)$coef[3, 2],
                           modelName = "Recognized",
                           country = "Burundi")
model4bFrame <- data.frame(Variable = rownames(summary(model4b)$coef),
                           Coefficient = summary(model4b)$coef[3, 1],
                           SE = summary(model4b)$coef[3, 2],
                           modelName = "Not Recognized",
                           country = "Burundi")
model5aFrame <- data.frame(Variable = rownames(summary(model5a)$coef),
                           Coefficient = summary(model5a)$coef[3, 1],
                           SE = summary(model5a)$coef[3, 2],
                           modelName = "Recognized",
                           country = "Cameroon")
model5bFrame <- data.frame(Variable = rownames(summary(model5b)$coef),
                           Coefficient = summary(model5b)$coef[3, 1],
                           SE = summary(model5b)$coef[3, 2],
                           modelName = "Not Recognized",
                           country = "Cameroon")
model6aFrame <- data.frame(Variable = rownames(summary(model6a)$coef),
                           Coefficient = summary(model6a)$coef[3, 1],
                           SE = summary(model6a)$coef[3, 2],
                           modelName = "Recognized",
                           country = "Cote d'Ivoire")
model6bFrame <- data.frame(Variable = rownames(summary(model6b)$coef),
                           Coefficient = summary(model6b)$coef[3, 1],
                           SE = summary(model6b)$coef[3, 2],
                           modelName = "Not Recognized",
                           country = "Cote d'Ivoire")
model65aFrame <- data.frame(Variable = rownames(summary(model65a)$coef),
                            Coefficient = summary(model65a)$coef[3, 1],
                            SE = summary(model65a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Gabon")
model65bFrame <- data.frame(Variable = rownames(summary(model65b)$coef),
                            Coefficient = summary(model65b)$coef[3, 1],
                            SE = summary(model65b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Gabon")
model7aFrame <- data.frame(Variable = rownames(summary(model7a)$coef),
                           Coefficient = summary(model7a)$coef[3, 1],
                           SE = summary(model7a)$coef[3, 2],
                           modelName = "Recognized",
                           country = "Ghana")
model7bFrame <- data.frame(Variable = rownames(summary(model7b)$coef),
                           Coefficient = summary(model7b)$coef[3, 1],
                           SE = summary(model7b)$coef[3, 2],
                           modelName = "Not Recognized",
                           country = "Ghana")
model75aFrame <- data.frame(Variable = rownames(summary(model75a)$coef),
                            Coefficient = summary(model75a)$coef[3, 1],
                            SE = summary(model75a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Guinea")
model75bFrame <- data.frame(Variable = rownames(summary(model75b)$coef),
                            Coefficient = summary(model75b)$coef[3, 1],
                            SE = summary(model75b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Guinea")
model8aFrame <- data.frame(Variable = rownames(summary(model8a)$coef),
                           Coefficient = summary(model8a)$coef[3, 1],
                           SE = summary(model8a)$coef[3, 2],
                           modelName = "Recognized",
                           country = "Kenya")
model8bFrame <- data.frame(Variable = rownames(summary(model8b)$coef),
                           Coefficient = summary(model8b)$coef[3, 1],
                           SE = summary(model8b)$coef[3, 2],
                           modelName = "Not Recognized",
                           country = "Kenya")
model85aFrame <- data.frame(Variable = rownames(summary(model85a)$coef),
                            Coefficient = summary(model85a)$coef[3, 1],
                            SE = summary(model85a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Lesotho")
model85bFrame <- data.frame(Variable = rownames(summary(model85b)$coef),
                            Coefficient = summary(model85b)$coef[3, 1],
                            SE = summary(model85b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Lesotho")
model9aFrame <- data.frame(Variable = rownames(summary(model9a)$coef),
                           Coefficient = summary(model9a)$coef[3, 1],
                           SE = summary(model9a)$coef[3, 2],
                           modelName = "Recognized",
                           country = "Liberia")
model9bFrame <- data.frame(Variable = rownames(summary(model9b)$coef),
                           Coefficient = summary(model9b)$coef[3, 1],
                           SE = summary(model9b)$coef[3, 2],
                           modelName = "Not Recognized",
                           country = "Liberia")
model10aFrame <- data.frame(Variable = rownames(summary(model10a)$coef),
                            Coefficient = summary(model10a)$coef[3, 1],
                            SE = summary(model10a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Madagascar")
model10bFrame <- data.frame(Variable = rownames(summary(model10b)$coef),
                            Coefficient = summary(model10b)$coef[3, 1],
                            SE = summary(model10b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Madagascar")
model11aFrame <- data.frame(Variable = rownames(summary(model11a)$coef),
                            Coefficient = summary(model11a)$coef[3, 1],
                            SE = summary(model11a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Malawi")
model11bFrame <- data.frame(Variable = rownames(summary(model11b)$coef),
                            Coefficient = summary(model11b)$coef[3, 1],
                            SE = summary(model11b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Malawi")
model12aFrame <- data.frame(Variable = rownames(summary(model12a)$coef),
                            Coefficient = summary(model12a)$coef[3, 1],
                            SE = summary(model12a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Mali")
model12bFrame <- data.frame(Variable = rownames(summary(model12b)$coef),
                            Coefficient = summary(model12b)$coef[3, 1],
                            SE = summary(model12b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Mali")
model13aFrame <- data.frame(Variable = rownames(summary(model13a)$coef),
                            Coefficient = summary(model13a)$coef[3, 1],
                            SE = summary(model13a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Mozambique")
model13bFrame <- data.frame(Variable = rownames(summary(model13b)$coef),
                            Coefficient = summary(model13b)$coef[3, 1],
                            SE = summary(model13b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Mozambique")
model14aFrame <- data.frame(Variable = rownames(summary(model14a)$coef),
                            Coefficient = summary(model14a)$coef[3, 1],
                            SE = summary(model14a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Namibia")
model14bFrame <- data.frame(Variable = rownames(summary(model14b)$coef),
                            Coefficient = summary(model14b)$coef[3, 1],
                            SE = summary(model14b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Namibia")
model15aFrame <- data.frame(Variable = rownames(summary(model15a)$coef),
                            Coefficient = summary(model15a)$coef[3, 1],
                            SE = summary(model15a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Niger")
model15bFrame <- data.frame(Variable = rownames(summary(model15b)$coef),
                            Coefficient = summary(model15b)$coef[3, 1],
                            SE = summary(model15b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Niger")
model16aFrame <- data.frame(Variable = rownames(summary(model16a)$coef),
                            Coefficient = summary(model16a)$coef[3, 1],
                            SE = summary(model16a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Nigeria")
model16bFrame <- data.frame(Variable = rownames(summary(model16b)$coef),
                            Coefficient = summary(model16b)$coef[3, 1],
                            SE = summary(model16b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Nigeria")
model17aFrame <- data.frame(Variable = rownames(summary(model17a)$coef),
                            Coefficient = summary(model17a)$coef[3, 1],
                            SE = summary(model17a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Senegal")
model17bFrame <- data.frame(Variable = rownames(summary(model17b)$coef),
                            Coefficient = summary(model17b)$coef[3, 1],
                            SE = summary(model17b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Senegal")
model18aFrame <- data.frame(Variable = rownames(summary(model18a)$coef),
                            Coefficient = summary(model18a)$coef[3, 1],
                            SE = summary(model18a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Sierra Leone")
model18bFrame <- data.frame(Variable = rownames(summary(model18b)$coef),
                            Coefficient = summary(model18b)$coef[3, 1],
                            SE = summary(model18b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Sierra Leone")
model19aFrame <- data.frame(Variable = rownames(summary(model19a)$coef),
                            Coefficient = summary(model19a)$coef[3, 1],
                            SE = summary(model19a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "South Africa")
model19bFrame <- data.frame(Variable = rownames(summary(model19b)$coef),
                            Coefficient = summary(model19b)$coef[3, 1],
                            SE = summary(model19b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "South Africa")
model20aFrame <- data.frame(Variable = rownames(summary(model20a)$coef),
                            Coefficient = summary(model20a)$coef[3, 1],
                            SE = summary(model20a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Tanzania")
model20bFrame <- data.frame(Variable = rownames(summary(model20b)$coef),
                            Coefficient = summary(model20b)$coef[3, 1],
                            SE = summary(model20b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Tanzania")
model21aFrame <- data.frame(Variable = rownames(summary(model21a)$coef),
                            Coefficient = summary(model21a)$coef[3, 1],
                            SE = summary(model21a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Togo")
model21bFrame <- data.frame(Variable = rownames(summary(model21b)$coef),
                            Coefficient = summary(model21b)$coef[3, 1],
                            SE = summary(model21b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Togo")
model22aFrame <- data.frame(Variable = rownames(summary(model22a)$coef),
                            Coefficient = summary(model22a)$coef[3, 1],
                            SE = summary(model22a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Uganda")
model22bFrame <- data.frame(Variable = rownames(summary(model22b)$coef),
                            Coefficient = summary(model22b)$coef[3, 1],
                            SE = summary(model22b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Uganda")
model23aFrame <- data.frame(Variable = rownames(summary(model23a)$coef),
                            Coefficient = summary(model23a)$coef[3, 1],
                            SE = summary(model23a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Zambia")
model23bFrame <- data.frame(Variable = rownames(summary(model23b)$coef),
                            Coefficient = summary(model23b)$coef[3, 1],
                            SE = summary(model23b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Zambia")
model24aFrame <- data.frame(Variable = rownames(summary(model24a)$coef),
                            Coefficient = summary(model24a)$coef[3, 1],
                            SE = summary(model24a)$coef[3, 2],
                            modelName = "Recognized",
                            country = "Zimbabwe")
model24bFrame <- data.frame(Variable = rownames(summary(model24b)$coef),
                            Coefficient = summary(model24b)$coef[3, 1],
                            SE = summary(model24b)$coef[3, 2],
                            modelName = "Not Recognized",
                            country = "Zimbabwe")

allModelFrame <- rbind.data.frame(model1aFrame, model2aFrame, model3aFrame, model4aFrame, model5aFrame, model6aFrame, model65aFrame, model7aFrame, model75aFrame, model8aFrame, model85aFrame, model9aFrame, model10aFrame, model11aFrame, model12aFrame, model13aFrame, model14aFrame, model15aFrame, model16aFrame, model17aFrame, model18aFrame, model19aFrame, model20aFrame, model21aFrame, model22aFrame, model23aFrame, model24aFrame, model1bFrame, model2bFrame, model3bFrame, model4bFrame, model5bFrame, model6bFrame, model65bFrame, model7bFrame, model75bFrame, model8bFrame, model85bFrame, model9bFrame, model10bFrame, model11bFrame, model12bFrame, model13bFrame, model14bFrame, model15bFrame, model16bFrame, model17bFrame, model18bFrame, model19bFrame, model20bFrame, model21bFrame, model22bFrame, model23bFrame, model24bFrame)

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
ggplot(allModelFrame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = country, ymin = Coefficient - SE*interval1, 
                     ymax = Coefficient + SE*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = country, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape=modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_flip(ylim = c(-0.2, 0.2)) +
  coord_flip() +
  theme_bw() +
  theme(text=element_text(size=20), legend.title=element_blank()) +
  ylab("Effect of Treatment on Traditional Leader Z-score") +
  xlab("Country Left Out")

ggsave("figure_A3_bycountry.pdf", width=8, height=12)









###############################################################################
################ Figure A4: Graph for Border Region Assignment ################ 
###############################################################################


########## Combine all district and headquarter shapefiles
## Data can be found on soerenhenn.com/data
shape.MWI <- readOGR(dsn=".", layer="MWI_adm1_2017")

## Combine Shapefiles into list
shape.all <- list("shape.MWI"=shape.MWI)

shapelow.MWI <- readOGR(dsn=".", layer="MWI_adm2_2017")

## Combine Shapefiles into list
shapelow.all <- list("shape.MWI"=shapelow.MWI)




## load headquarters
hq.MWI <- read.csv("district_2017.csv")
hq.MWI <- subset(hq.MWI, select = c(latitude, longitude))
hq.MWI$country <- "MWI"
hq.MWI$division <- "District"

hq.all <- rbind(hq.MWI)


## observations at border
load("RegressionSample.RData")

#data.reg <- data.reg[which(data.reg$distance_hq<150),]

data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg <- data.reg[which(data.reg$treatment_log5>=0),]

data.reg <- data.reg[which(data.reg$adminlevel==2),]

data.reg$country_code <- NA
data.reg[which(data.reg[,"country"]==16),"country_code"] <- "MWI"





########## Admin 2
shape <- shapelow.all[["shape.MWI"]]
shapehigh <- shape.all[["shape.MWI"]]
hq <- hq.all[which(hq.all$country=="MWI"),]
obs <- data.reg[which(data.reg$country_code=="MWI"),]

division <- hq$division[1]


hq$type <- "Headquarter"
obs$type <- "Observation"

## prepare data for ggplot
shape@data$id = rownames(shape@data)
shape.points = fortify(shape, region="id")
shape.df = join(shape.points, shape@data, by="id")

## Border
borders2 <- as(shape,"SpatialLines")
border.trans2 <- SpatialLinesDataFrame(borders2, as.data.frame(matrix(1, nrow = length(borders2), ncol = 1)), match.ID=F)

## Higher level border
borders <- gDifference(as(shapehigh,"SpatialLines"),
                       as(gUnaryUnion(shapehigh),"SpatialLines"),
                       byid=TRUE)
border.trans <- SpatialLinesDataFrame(borders, as.data.frame(matrix(1, nrow = length(borders), ncol = 1)), match.ID=F)


## get elevation
bb <- bbox(shape.MWI)
ll_means <- rowMeans(bb)
bw_map <- get_googlemap(center =  c(lon=ll_means[1],lat=ll_means[2]), zoom = 8,
                        color = "bw",
                        style = "feature:road|visibility:on&style=element:labels|visibility:off&style=feature:administrative|visibility:off")


centroids.df <- as.data.frame(coordinates(shapelow.MWI))
names(centroids.df) <- c("Longitude", "Latitude")
centroids.df$id <- shapelow.MWI@data$DISTRICT
randomMap.df <- data.frame(id = idList, shading = runif(length(idList)), centroids.df)
geom_text(data=centroids.df, aes(label = id, x = Longitude, y = Latitude)) +
  
  
obs$borderregion <- NA
obs$borderregion[which(obs$fixed_effects=="16.4.2.12.29")] <- "Dedza-Lilongwe" 
obs$borderregion[which(obs$fixed_effects=="16.4.2.9.29")] <- "Dowa-Lilongwe"
obs$borderregion[which(obs$fixed_effects=="16.5.2.12.13")] <- "Dedza-Ntcheu"
obs$borderregion[which(obs$fixed_effects=="16.5.2.7.10")] <- "Salima-Nkhotakota"
obs$borderregion[which(obs$fixed_effects=="16.5.2.8.9")] <- "Dowa-Ntchisi"
obs$borderregion[which(obs$fixed_effects=="16.5.2.9.29")] <- "Dowa-Lilongwe"

## Plot
zoom <- ggplot() + 
  geom_path(data=border.trans2, aes(long,lat,group=group), color="grey10") +
  geom_text(data=centroids.df, aes(label = id, x = Longitude, y = Latitude)) +
  geom_path(data=border.trans, aes(long,lat,group=group), color="black", size=1) +
  geom_point(data=hq, aes(x=longitude,y=latitude), shape=15, color="black", size=2) +
  geom_point(data=obs, aes(x=longitude,y=latitude, shape=borderregion, color=borderregion), size=2) +
  scale_shape_manual("Border Region", values=c(65, 66, 67, 68, 69)) +
  #  geom_rect(mapping=aes(xmin=33, xmax=34.75, ymin=-14.5, ymax=-13.35), color="black", alpha=0.5) +
  scale_color_manual("Border Region", values=c('firebrick', 'blue', 'green', 'purple', 'orange', 'black')) +
  #  north(symbol = 16, scale = 0.15, location="bottomright", x.min = bb[1,1], x.max=bb[1,2], y.min = bb[2,1], y.max= bb[2,2]) +
  #  scalebar(dist = 50, model = "WGS84", height=0.01, transform=T, dist_unit="km", x.min = bb[1,1], x.max=bb[1,2], y.min = bb[2,1], y.max= bb[2,2]) +
  #  coord_fixed(ylim=c(bb[2,1],bb[2,2]), xlim=c(bb[1,1],bb[1,2])) +
  coord_fixed(xlim = c(33, 34.75),ylim = c(-14.5, -13.25)) +
  theme_bw() + 
  theme(panel.grid.major =  element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank(), axis.line = element_blank(),
        legend.position=c(.12,.18), legend.key = element_blank(), legend.background = element_blank())
## Save
ggsave("figure_A4_MWI2zoom.pdf", width=6, height=6)

mwi <- ggplot() + 
  geom_path(data=border.trans2, aes(long,lat,group=group), color="grey10") +
  #  geom_text(data=centroids.df, aes(label = id, x = Longitude, y = Latitude)) +
  geom_path(data=border.trans, aes(long,lat,group=group), color="black", size=1) +
  geom_point(data=hq, aes(x=longitude,y=latitude), shape=15, color="black", size=2) +
  geom_point(data=obs, aes(x=longitude,y=latitude), shape=20, color='firebrick', size=2) +
  scale_shape_manual(values=c(65, 66, 67, 68, 69)) +
  scale_color_manual(values=c('firebrick', 'blue', 'green', 'purple', 'orange', 'black')) +
  geom_rect(mapping=aes(xmin=33, xmax=34.75, ymin=-14.5, ymax=-13.35), color="black", alpha=0.5) +
  #  north(symbol = 16, scale = 0.15, location="bottomright", x.min = bb[1,1], x.max=bb[1,2], y.min = bb[2,1], y.max= bb[2,2]) +
  #  scalebar(dist = 50, model = "WGS84", height=0.01, transform=T, dist_unit="km", x.min = bb[1,1], x.max=bb[1,2], y.min = bb[2,1], y.max= bb[2,2]) +
  coord_fixed(ylim=c(bb[2,1],bb[2,2]), xlim=c(bb[1,1],bb[1,2])) +
  #  coord_fixed(xlim = c(33, 34.75),ylim = c(-14.5, -13.25)) +
  #  ggtitle(division) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank(),
        legend.title=element_blank(), legend.position=c(.1,.18), legend.key = element_blank(), legend.background = element_blank())
## Save
ggsave("figure_A4_MWI2.pdf", width=3, height=6)

## Combined Figure
plot_grid(zoom, mwi, rel_widths = c(2,1), scale=c(1,1))
ggsave("figure_A4_MWI2combined_alt.pdf", width=12, height=6)
