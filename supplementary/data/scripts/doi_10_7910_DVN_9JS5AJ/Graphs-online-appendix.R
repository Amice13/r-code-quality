#####################################################################################
# Create all fiigure for the online appendix
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
################ Figure B2: Borders, Headquarters, and Observations ################
####################################################################################

########## Combine all district and headquarter shapefiles
shape.BDI <- readOGR(dsn=".", layer="BDI_adm1_2017")

## Combine Shapefiles into list
shape.all <- list("shape.BDI"=shape.BDI)


## load headquarters
hq.BDI <- read.csv("province_2017.csv")
hq.BDI <- subset(hq.BDI, select = c(latitude, longitude))
hq.BDI$country <- "BDI"
hq.BDI$division <- "Province"

hq.all <- rbind(hq.BDI)


## observations at border
load("RegressionSample.RData")


data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg <- data.reg[which(data.reg$treatment_log5>=0),]

data.reg <- data.reg[which(data.reg$adminlevel==1),]

data.reg$country_code <- NA
data.reg[which(data.reg[,"country"]==5),"country_code"] <- "BDI"



## get elevation
bb <- bbox(shape.BDI)
ll_means <- rowMeans(bb)
bw_map <- get_googlemap(center =  c(lon=ll_means[1],lat=ll_means[2]), zoom = 8,
                        color = "bw",
                        style = "feature:road|visibility:on&style=element:labels|visibility:off&style=feature:administrative|visibility:off")




########## Admin 1
shape <- shape.all[["shape.BDI"]]
hq <- hq.all[which(hq.all$country=="BDI"),]
obs <- data.reg[which(data.reg$country_code=="BDI"),]

division <- hq$division[1]

hq$type <- "Headquarter"
obs$type <- "Observation"

## Higher level border
borders <- as(shape,"SpatialLines")
border.trans <- SpatialLinesDataFrame(borders, as.data.frame(matrix(1, nrow = length(borders), ncol = 1)), match.ID=F)


## prepare data for ggplot
shape@data$id = rownames(shape@data)
shape.points = fortify(shape, region="id")
shape.df = join(shape.points, shape@data, by="id")

## Plot
ggmap(bw_map) + 
  geom_path(data=border.trans, aes(long,lat,group=group),  color="grey10") +
  geom_point(data=hq, aes(x=longitude,y=latitude, shape=type, color=type), size=2) +
  geom_point(data=obs, aes(x=longitude,y=latitude, shape=type, color=type), size=2) +
  scale_shape_manual(values=c(20, 13)) +
  scale_color_manual(values=c('Black', 'firebrick')) +
  north(symbol = 16, scale = 0.15, location="bottomright", x.min = bb[1,1], x.max=bb[1,2], y.min = bb[2,1], y.max= bb[2,2]) +
  scalebar(dist = 50, model = "WGS84", height=0.01, transform=T, dist_unit="km", x.min = bb[1,1], x.max=bb[1,2], y.min = bb[2,1], y.max= bb[2,2]) +
  coord_fixed(ylim=c(bb[2,1],bb[2,2]), xlim=c(bb[1,1],bb[1,2])) +
  ggtitle(division) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank(),
        legend.title=element_blank(), legend.position=c(.88,.2), legend.key = element_blank(), legend.background = element_blank())
## Save
ggsave("figure_a6_BDI1.pdf", width=6, height=6)




###################################### Admin 2 #####################################

########## Combine all district and headquarter shapefiles
shapelow.BDI <- readOGR(dsn=".", layer="BDI_adm2_2017")

## Combine Shapefiles into list
shapelow.all <- list("shape.BDI"=shapelow.BDI)


## load headquarters
hq.BDI <- read.csv("commune_2017.csv")
hq.BDI <- subset(hq.BDI, select = c(latitude, longitude))
hq.BDI$country <- "BDI"
hq.BDI$division <- "Commune"

hq.all <- rbind(hq.BDI)


## observations at border
load("RegressionSample.RData")

#data.reg <- data.reg[which(data.reg$distance_hq<150),]

data.reg <- data.reg[which(data.reg$distance_neigh5<30),]
data.reg <- data.reg[which(data.reg$treatment_log5>=0),]

data.reg <- data.reg[which(data.reg$adminlevel==2),]

data.reg$country_code <- NA
data.reg[which(data.reg[,"country"]==5),"country_code"] <- "BDI"





########## Admin 2

shape <- shapelow.all[["shape.BDI"]]
shapehigh <- shape.all[["shape.BDI"]]
hq <- hq.all[which(hq.all$country=="BDI"),]
obs <- data.reg[which(data.reg$country_code=="BDI"),]

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

## Plot
ggmap(bw_map) + 
  geom_path(data=border.trans2, aes(long,lat,group=group), color="grey10") +
  geom_path(data=border.trans, aes(long,lat,group=group), color="black", size=1) +
  geom_point(data=hq, aes(x=longitude,y=latitude, shape=type, color=type), size=2) +
  geom_point(data=obs, aes(x=longitude,y=latitude, shape=type, color=type), size=2) +
  scale_shape_manual(values=c(20, 13)) +
  scale_color_manual(values=c('Black', 'firebrick')) +
  north(symbol = 16, scale = 0.15, location="bottomright", x.min = bb[1,1], x.max=bb[1,2], y.min = bb[2,1], y.max= bb[2,2]) +
  scalebar(dist = 50, model = "WGS84", height=0.01, transform=T, dist_unit="km", x.min = bb[1,1], x.max=bb[1,2], y.min = bb[2,1], y.max= bb[2,2]) +
  coord_fixed(ylim=c(bb[2,1],bb[2,2]), xlim=c(bb[1,1],bb[1,2])) +
  ggtitle(division) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank(),
        legend.title=element_blank(), legend.position=c(.88,.2), legend.key = element_blank(), legend.background = element_blank())
## Save
ggsave("figure_B2_BDI2.pdf", width=6, height=6)



#####################################################################################
################# Figure B3: McCrary Test with Different Bandwidths #################
#####################################################################################

load("RegressionSample.RData")
data.all <- data.reg


########### 3km
data.reg <- data.all

data.reg$treatment <- data.reg$treatment_log3
data.reg$treatment_int <- data.reg$treatment_int_log3

data.reg$distance_border[which(data.reg$treatment==0)] <- -data.reg$distance_border[which(data.reg$treatment==0)]

data.reg<- data.reg[-which(is.na(data.reg$treatment)), ]

pdf('figure_b3_McCrary3.pdf')
DCdensity(data.reg$distance_border, bw=3)
dev.off()


############# 4km
data.reg <- data.all

data.reg$treatment <- data.reg$treatment_log4
data.reg$treatment_int <- data.reg$treatment_int_log4

data.reg$distance_border[which(data.reg$treatment==0)] <- -data.reg$distance_border[which(data.reg$treatment==0)]

data.reg<- data.reg[-which(is.na(data.reg$treatment)), ]

pdf('figure_b3_McCrary4.pdf')
DCdensity(data.reg$distance_border, bw=4)
dev.off()


############ 5km
data.reg <- data.all
#data.reg <- data.reg[which(data.reg$distance_neigh5<30),]

data.reg$treatment <- data.reg$treatment_log5
data.reg$treatment_int <- data.reg$treatment_int_log5

data.reg$distance_border[which(data.reg$treatment==0)] <- -data.reg$distance_border[which(data.reg$treatment==0)]

data.reg<- data.reg[-which(is.na(data.reg$treatment)), ]

pdf('figure_b3_McCrary5.pdf')
DCdensity(data.reg$distance_border, bw=5)
dev.off()

############ 6km
data.reg <- data.all

data.reg$treatment <- data.reg$treatment_log6
data.reg$treatment_int <- data.reg$treatment_int_log6

data.reg$distance_border[which(data.reg$treatment==0)] <- -data.reg$distance_border[which(data.reg$treatment==0)]

data.reg<- data.reg[-which(is.na(data.reg$treatment)), ]

pdf('figure_b3_McCrary6.pdf')
DCdensity(data.reg$distance_border, bw=6)
dev.off()



#####################################################################################
########################## Figure B4-6: Bandwidth Controls ##########################
#####################################################################################


rm(allModelFrame)

setwd(path)
load("RegressionSample.RData")

data.reg[,"urban"] <- NA
data.reg[which(data.reg[,"urbrur"]==1),"urban"] <- 1
data.reg[which(data.reg[,"urbrur"]>1),"urban"] <- 0
data.reg <- data.reg[which(data.reg$distance_hq<150),]
## drop CIV second admin because headquarters are too inaccurate
data.reg$drop <- 0
data.reg$drop[which(data.reg$adminlevel==2 & data.reg$country==8)] <- 1
data.reg <- data.reg[which(data.reg$drop==0),]

data.reg.afro <- subset(data.reg, select = c(country, distance_hq, distance_border, neighbordist, traveltime, dist_coeff_log,
                                             distance_neigh1, distance_neigh2 , distance_neigh3, distance_neigh4, distance_neigh5, distance_neigh6, distance_neigh7, distance_neigh8, distance_neigh9, distance_neigh10, distance_neigh11, distance_neigh12, distance_neigh13, distance_neigh14, distance_neigh15, distance_neigh16, distance_neigh17, distance_neigh18, distance_neigh19, distance_neigh20,
                                             treatment_int_log1, treatment_int_log2, treatment_int_log3, treatment_int_log4, treatment_int_log5, treatment_int_log6, treatment_int_log7, treatment_int_log8, treatment_int_log9, treatment_int_log10, treatment_int_log11, treatment_int_log12, treatment_int_log13, treatment_int_log14, treatment_int_log15, treatment_int_log16, treatment_int_log17, treatment_int_log18, treatment_int_log19, treatment_int_log20,
                                             treatment_log1, treatment_log2, treatment_log3, treatment_log4, treatment_log5, treatment_log6, treatment_log7, treatment_log8, treatment_log9, treatment_log10, treatment_log11, treatment_log12, treatment_log13, treatment_log14, treatment_log15, treatment_log16, treatment_log17, treatment_log18, treatment_log19, treatment_log20,
                                             urban, distance_capital, distance_natborder, distcoast, elevation, ruggedness, malaria, agriculture, missions, cities, distrail, area_sqkm, chief_zscore, z_influence_tradition, z_trust_tradition, z_corrupt_traditiontemp, z_contact_tradition, state_capacity, inst_chiefs_dum, fixed_effects, district_id))

setwd(path)
load("RegressionSampleDHS.RData")

data.reg[,"urban"] <- NA
data.reg[which(data.reg[,"urbrur"]==1),"urban"] <- 1
data.reg[which(data.reg[,"urbrur"]>1),"urban"] <- 0
data.reg <- data.reg[which(data.reg$distance_hq<75),]

data.reg.dhs <- subset(data.reg, select = c(country, distance_hq, distance_border, neighbordist, traveltime, dist_coeff_log,
                                            distance_neigh1, distance_neigh2 , distance_neigh3, distance_neigh4, distance_neigh5, distance_neigh6, distance_neigh7, distance_neigh8, distance_neigh9, distance_neigh10, distance_neigh11, distance_neigh12, distance_neigh13, distance_neigh14, distance_neigh15, distance_neigh16, distance_neigh17, distance_neigh18, distance_neigh19, distance_neigh20,
                                            treatment_int_log1, treatment_int_log2, treatment_int_log3, treatment_int_log4, treatment_int_log5, treatment_int_log6, treatment_int_log7, treatment_int_log8, treatment_int_log9, treatment_int_log10, treatment_int_log11, treatment_int_log12, treatment_int_log13, treatment_int_log14, treatment_int_log15, treatment_int_log16, treatment_int_log17, treatment_int_log18, treatment_int_log19, treatment_int_log20,
                                            treatment_log1, treatment_log2, treatment_log3, treatment_log4, treatment_log5, treatment_log6, treatment_log7, treatment_log8, treatment_log9, treatment_log10, treatment_log11, treatment_log12, treatment_log13, treatment_log14, treatment_log15, treatment_log16, treatment_log17, treatment_log18, treatment_log19, treatment_log20,
                                            urban, distance_capital, distance_natborder, distcoast, elevation, ruggedness, malaria, agriculture, missions, cities, distrail, area_sqkm, state_capacity, electricity, registered, time.to.water, read.all, wealth.index, infant.mortality, trad_med, kids.gone, men.always, wm.always, inst_chiefs_dum, fixed_effects, district_id))

data.reg <- bind_rows(data.reg.afro, data.reg.dhs)

data.all <- data.reg



########## Create empty vector for results

allmodel1Frame <- data.frame(Variable = "test",
                             Coefficient_1 = 1,
                             SE_1 = 1,
                             Coefficient_2 = 1,
                             SE_2 = 1,
                             Coefficient_3 = 1,
                             SE_3 = 1,
                             Coefficient_4 = 1,
                             SE_4 = 1,
                             Coefficient_5 = 1,
                             SE_5 = 1,
                             Coefficient_6 = 1,
                             SE_6 = 1,
                             Coefficient_7 = 1,
                             SE_7 = 1,
                             Coefficient_8 = 1,
                             SE_8 = 1,
                             Coefficient_9 = 1,
                             SE_9 = 1,
                             Coefficient_10 = 1,
                             SE_10 = 1,
                             Coefficient_11 = 1,
                             SE_11 = 1,
                             modelName = "test",
                             bandwidth= 3)

allmodel1Frame$bandwidth <- as.integer(allmodel1Frame$bandwidth)

for(i in 3:20) {
  data.reg <- data.all
  
  data.reg <- data.reg[which(data.reg[,paste("distance_neigh", i, sep="")]<30),]
  
  
  data.reg$treatment <- data.reg[,paste("treatment_log", i, sep="")]
  data.reg$treatment_int <- data.reg[,paste("treatment_int_log", i, sep="")]
  
  data.reg$treatment_int <- data.reg$treatment_int * (data.reg$dist_coeff_log*-1)
  
  data.reg$distance_border[which(data.reg$treatment==0)] <- -data.reg$distance_border[which(data.reg$treatment==0)]

  data.reg$dist_treat <- data.reg$treatment*data.reg$distance_border
  
  model1 <- felm(distance_capital ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  model2 <- felm(distance_natborder ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  model3 <- felm(distcoast ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  model4 <- felm(elevation ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  model5 <- felm(ruggedness ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  model6 <- felm(agriculture ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  model7 <- felm(cities ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  model8 <- felm(malaria ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  model9 <- felm(missions ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  model10 <- felm(distrail ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  model11 <- felm(area_sqkm ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  # Put model estimates into temporary data.frames:
  model1aFrame <- data.frame(Variable = rownames(summary(model1)$coef),
                             Coefficient_1 = summary(model1)$coef[4, 1],
                             SE_1 = summary(model1)$coef[4, 2],
                             Coefficient_2 = summary(model2)$coef[4, 1],
                             SE_2 = summary(model2)$coef[4, 2],
                             Coefficient_3 = summary(model3)$coef[4, 1],
                             SE_3 = summary(model3)$coef[4, 2],
                             Coefficient_4 = summary(model4)$coef[4, 1],
                             SE_4 = summary(model4)$coef[4, 2],
                             Coefficient_5 = summary(model5)$coef[4, 1],
                             SE_5 = summary(model5)$coef[4, 2],
                             Coefficient_6 = summary(model6)$coef[4, 1],
                             SE_6 = summary(model6)$coef[4, 2],
                             Coefficient_7 = summary(model7)$coef[4, 1],
                             SE_7 = summary(model7)$coef[4, 2],
                             Coefficient_8 = summary(model8)$coef[4, 1],
                             SE_8 = summary(model8)$coef[4, 2],
                             Coefficient_9 = summary(model9)$coef[4, 1],
                             SE_9 = summary(model9)$coef[4, 2],
                             Coefficient_10 = summary(model10)$coef[4, 1],
                             SE_10 = summary(model10)$coef[4, 2],
                             Coefficient_11 = summary(model11)$coef[4, 1],
                             SE_11 = summary(model11)$coef[4, 2],
                             modelName = "Treatment",
                             bandwidth= i)
  model1bFrame <- data.frame(Variable = rownames(summary(model1)$coef),
                             Coefficient_1 = summary(model1)$coef[17, 1],
                             SE_1 = summary(model1)$coef[17, 2],
                             Coefficient_2 = summary(model2)$coef[17, 1],
                             SE_2 = summary(model2)$coef[17, 2],
                             Coefficient_3 = summary(model3)$coef[17, 1],
                             SE_3 = summary(model3)$coef[17, 2],
                             Coefficient_4 = summary(model4)$coef[17, 1],
                             SE_4 = summary(model4)$coef[17, 2],
                             Coefficient_5 = summary(model5)$coef[17, 1],
                             SE_5 = summary(model5)$coef[17, 2],
                             Coefficient_6 = summary(model6)$coef[17, 1],
                             SE_6 = summary(model6)$coef[17, 2],
                             Coefficient_7 = summary(model7)$coef[17, 1],
                             SE_7 = summary(model7)$coef[17, 2],
                             Coefficient_8 = summary(model8)$coef[17, 1],
                             SE_8 = summary(model8)$coef[17, 2],
                             Coefficient_9 = summary(model9)$coef[17, 1],
                             SE_9 = summary(model9)$coef[17, 2],
                             Coefficient_10 = summary(model10)$coef[17, 1],
                             SE_10 = summary(model10)$coef[17, 2],
                             Coefficient_11 = summary(model11)$coef[17, 1],
                             SE_11 = summary(model11)$coef[17, 2],
                             modelName = "Treatment X Recognized",
                             bandwidth= i)
  # Combine these data.frames
  allmodel1Frame <- data.frame(rbind(allmodel1Frame, model1aFrame, model1bFrame))
  
  
}



allmodel1Frame <- allmodel1Frame[-which(allmodel1Frame$Variable=="test"),]


# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier


## Distance National Capital
ggplot(allmodel1Frame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient_1 - SE_1*interval1, 
                     ymax = Coefficient_1 + SE_1*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient_1, ymin = Coefficient_1 - SE_1*interval2,
                      ymax = Coefficient_1 + SE_1*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=14),legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Distance Capital") +
  xlab("Bandwidth (km)")

ggsave("figure_b4_1_bandwidth.pdf", width=6, height=6)

## Distance National Border
ggplot(allmodel1Frame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient_2 - SE_2*interval1, 
                     ymax = Coefficient_2 + SE_2*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient_2, ymin = Coefficient_2 - SE_2*interval2,
                      ymax = Coefficient_2 + SE_2*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=14),legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Distance Border") +
  xlab("Bandwidth (km)")

ggsave("figure_b4_2_bandwidth.pdf", width=6, height=6)

## Distance Coast
ggplot(allmodel1Frame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient_3 - SE_3*interval1, 
                     ymax = Coefficient_3 + SE_3*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient_3, ymin = Coefficient_3 - SE_3*interval2,
                      ymax = Coefficient_3 + SE_3*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=14),legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Distance Coast") +
  xlab("Bandwidth (km)")

ggsave("figure_b4_3_bandwidth.pdf", width=6, height=6)

## Distance Elevation
ggplot(allmodel1Frame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient_4 - SE_4*interval1, 
                     ymax = Coefficient_4 + SE_4*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient_4, ymin = Coefficient_4 - SE_4*interval2,
                      ymax = Coefficient_4 + SE_4*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=14),legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Elevation") +
  xlab("Bandwidth (km)")

ggsave("figure_b4_4_bandwidth.pdf", width=6, height=6)

## Distance Ruggedness
ggplot(allmodel1Frame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient_5 - SE_5*interval1, 
                     ymax = Coefficient_5 + SE_5*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient_5, ymin = Coefficient_5 - SE_5*interval2,
                      ymax = Coefficient_5 + SE_5*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=14),legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Ruggedness") +
  xlab("Bandwidth (km)")

ggsave("figure_b5_5_bandwidth.pdf", width=6, height=6)

## Agriculture
ggplot(allmodel1Frame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient_6 - SE_6*interval1, 
                     ymax = Coefficient_6 + SE_6*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient_6, ymin = Coefficient_6 - SE_6*interval2,
                      ymax = Coefficient_6 + SE_6*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=14),legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Agriculture") +
  xlab("Bandwidth (km)")

ggsave("figure_b5_6_bandwidth.pdf", width=6, height=6)

## Historical Cities
ggplot(allmodel1Frame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient_7 - SE_7*interval1, 
                     ymax = Coefficient_7 + SE_7*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient_7, ymin = Coefficient_7 - SE_7*interval2,
                      ymax = Coefficient_7 + SE_7*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=14),legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Hist Cities") +
  xlab("Bandwidth (km)")

ggsave("figure_b5_7_bandwidth.pdf", width=6, height=6)

## Malaria
ggplot(allmodel1Frame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient_8 - SE_8*interval1, 
                     ymax = Coefficient_8 + SE_8*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient_8, ymin = Coefficient_8 - SE_8*interval2,
                      ymax = Coefficient_8 + SE_8*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=14),legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Malaria") +
  xlab("Bandwidth (km)")

ggsave("figure_b5_8_bandwidth.pdf", width=6, height=6)

## Distance Missions
ggplot(allmodel1Frame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient_9 - SE_9*interval1, 
                     ymax = Coefficient_9 + SE_9*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient_9, ymin = Coefficient_9 - SE_9*interval2,
                      ymax = Coefficient_9 + SE_9*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=14),legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Distance Missions") +
  xlab("Bandwidth (km)")

ggsave("figure_b6_9_bandwidth.pdf", width=6, height=6)

## Distance Railways
ggplot(allmodel1Frame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient_10 - SE_10*interval1, 
                     ymax = Coefficient_10 + SE_10*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient_10, ymin = Coefficient_10 - SE_10*interval2,
                      ymax = Coefficient_10 + SE_10*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=14),legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Distance Rail") +
  xlab("Bandwidth (km)")

ggsave("figure_b6_10_bandwidth.pdf", width=6, height=6)
