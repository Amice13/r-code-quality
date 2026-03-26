#####################################################################################
# Create all Graphs for main part of the paper
# Soeren Henn
# 2022-08-15
#####################################################################################


rm(list=ls())

## Set working directory


library(lfe)        # to run linear fixed effects models
library(ggplot2)    # to draw graphs
library(rgdal)      # to read shapefiles
library(plyr)       # to join two data frames



#####################################################################################
############################## Figure 1: Map of Sample ##############################
#####################################################################################


########## Countries in Sample

## Load shapefile of African borders 
shape <- readOGR(dsn=".", layer="AfricanCountries_clean")  # load shapefile

## who is in the sample?
shape@data$sample <- 0
shape@data$sample[which(shape@data$COUNTRY=="Benin")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Botswana")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Burkina Faso")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Burundi")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Cameroon")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="CÃfÂ´te d'Ivoire")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Gabon")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Ghana")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Guinea")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Kenya")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Lesotho")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Liberia")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Madagascar")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Malawi")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Mali")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Mozambique")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Namibia")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Niger")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Nigeria")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Senegal")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Sierra Leone")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="South Africa")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Tanzania")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Togo")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Uganda")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Zambia")] <- 1
shape@data$sample[which(shape@data$COUNTRY=="Zimbabwe")] <- 1

## Including DHS
shape@data$sample[which(shape@data$COUNTRY=="Congo DRC")] <- 0.3
shape@data$sample[which(shape@data$COUNTRY=="South Africa")] <- 0.6
shape@data$sample[which(shape@data$COUNTRY=="Botswana")] <- 0.6
shape@data$sample[which(shape@data$COUNTRY=="Madagascar")] <- 0.6
shape@data$sample[which(shape@data$COUNTRY=="Gabon")] <- 0.6
shape@data$sample[which(shape@data$COUNTRY=="Guinea")] <- 0.6
shape@data$sample[which(shape@data$COUNTRY=="Lesotho")] <- 0.6

## prepare data for ggplot
shape@data$id = rownames(shape@data)
shape.points = fortify(shape, region="id")
shape.df = join(shape.points, shape@data, by="id")

## create map
ggplot(shape.df, aes(long,lat,group=group, fill = factor(sample))) +
  geom_polygon() +
  geom_path(color="black") +
  coord_equal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank(),
        legend.title=element_blank(), legend.position=c(0.23,0.3)) +
  scale_fill_manual(values = alpha(c("grey40", "grey60", "grey80", "white")), breaks=c( "1", "0.6", "0.3", "0"), labels=c("Afrobarometer and DHS", "Afrobarometer Only", "DHS Only", "Not in sample"))
ggsave("figure_1_sample.pdf", width = 5, height = 5)



########## Map of countries with chiefs institutionalized in constitution
shape@data$sample3 <- 0
shape@data$sample3[which(shape@data$COUNTRY=="Benin")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Botswana")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="Burkina Faso")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Burundi")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Cameroon")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="CÃfÂ´te d'Ivoire")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Congo DRC")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="Gabon")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Ghana")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="Guinea")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Kenya")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Lesotho")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="Liberia")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Madagascar")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Malawi")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="Mali")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Mozambique")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Namibia")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="Niger")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="Nigeria")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Senegal")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Sierra Leone")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="South Africa")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="Tanzania")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Togo")] <- 1
shape@data$sample3[which(shape@data$COUNTRY=="Uganda")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="Zambia")] <- 2
shape@data$sample3[which(shape@data$COUNTRY=="Zimbabwe")] <- 2


## prepare data for ggplot
shape@data$id = rownames(shape@data)
shape.points = fortify(shape, region="id")
shape.df = join(shape.points, shape@data, by="id")

## create map
ggplot(shape.df, aes(long,lat,group=group, fill = factor(sample3))) +
  geom_polygon() +
  geom_path(color="black") +
  coord_equal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank(),
        legend.title=element_blank(), legend.position=c(0.23,0.3)) +
  scale_fill_manual(values = alpha(c("grey70", "grey90", "white")), breaks=c("2", "1", "0"), labels=c("Recognized" ,"Not Recognized", "Not in Sample"))
ggsave("figure_1_inst.pdf", width = 5, height = 5)



###############################################################################################
## Figure 2:  Correlation between state capacity and distance by country and admin. division ##
###############################################################################################


load("DataRegression.RData")
data <- data.all

data[which(data$distance_capital<0.1),"distance_capital"] <- 0.1
data$log_distance_capital <- log(data$distance_capital)

data.reg <- data

model1 <- felm(state_capacity ~ log_distance_hq | round , data.reg, subset=adminlevel==1)
model1b <- felm(state_capacity ~ log_distance_hq | round , data.reg, subset=adminlevel==2)

model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          modelName = "Admin 1",
                          adminlevel= 1,
                          countryname = "All Countries",
                          country = 0)

model1bFrame <- data.frame(Variable = rownames(summary(model1b)$coef),
                           Coefficient = summary(model1b)$coef[, 1],
                           SE = summary(model1b)$coef[, 2],
                           modelName = "Admin 2",
                           adminlevel= 2,
                           countryname = "All Countries",
                           country = 0)

# Combine these data.frames
allModelFrame <- data.frame(rbind(model1Frame, model1bFrame))

########## by country
countries <- c(2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 20, 21, 22, 23, 24, 25, 26, 29, 30, 32, 33, 34)
country.name <- c("Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cote d'Ivoire", "Gabon",
                  "Ghana", "Guinea", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", 
                  "Mozambique", "Namibia", "Niger", "Nigeria", "Senegal", "Sierra Leone", "South Africa", "Tanzania", "Togo", "Uganda", 
                  "Zambia", "Zimbabwe")

## loop through countries
for(i in 1:length(countries)) {
  country <- countries[i]
  
  data.reg <- data[which(data$country==country),]
  
  model1 <- felm(state_capacity ~ log_distance_hq | round, data.reg, subset=adminlevel==1)
  
  if (!(country %in% c(3,12,13,14,32))) {
    model1b <- felm(state_capacity ~ log_distance_hq | round, data.reg, subset=adminlevel==2)
  }
  
  
  
  # Put model estimates into temporary data.frames:
  model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                            Coefficient = summary(model1)$coef[, 1],
                            SE = summary(model1)$coef[, 2],
                            modelName = "Admin 1",
                            adminlevel= 1,
                            countryname = country.name[i],
                            country = countries[i])
  
  if (!(country %in% c(3,12,13,14,32))) {
    model1bFrame <- data.frame(Variable = rownames(summary(model1b)$coef),
                               Coefficient = summary(model1b)$coef[, 1],
                               SE = summary(model1b)$coef[, 2],
                               modelName = "Admin 2",
                               adminlevel= 2,
                               countryname = country.name[i],
                               country = countries[i])}
  
  
  # Combine these data.frames
  if (country %in% c(3,12,13,14,32)) {
    allModelFrame <- data.frame(rbind(allModelFrame, model1Frame))
  } else{
    allModelFrame <- data.frame(rbind(allModelFrame, model1Frame, model1bFrame))
  }
}

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
ggplot(allModelFrame, aes(colour = modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = countryname, ymin = Coefficient - SE*interval1, 
                     ymax = Coefficient + SE*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = countryname, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") + 
  #geom_point(aes(x="Burundi", y=-0.28, color="Admin 2"), lwd = 2, shape = 21, fill = "WHITE") +
  #geom_point(aes(x="Senegal", y=0.05, color="Admin 2"), lwd = 2, shape = 21, fill = "WHITE") +
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  #coord_flip(ylim = c(-0.4, 0.1)) +
  coord_flip() +
  theme_bw() +
  theme(legend.title=element_blank()) +
  ylab("Effect of Log Distance to HQ on State Capacity") +
  xlab("Country")
ggsave("figure_2_firststage.pdf", width=6, height=8)




#####################################################################################
######################### Figure 3: Raw data around cut-off #########################
#####################################################################################

load("MainRegressionSample.RData")

# Subset to Institutionalized
data.sub1 <- data.reg[which(data.reg$inst_chiefs_dum==1),]

# Seperate different sides of the border
data.over <- data.sub1[which(data.sub1$treatment==0),]
data.under <- data.sub1[which(data.sub1$treatment==1),]

# Calculate correlation and store intercept and SE
model.left.I <- felm(chief_zscore ~ distance_border | 0 |0| 0, data.over)
model.right.I <- felm(chief_zscore ~ distance_border | 0 |0| 0, data.under)
jump <- model.right.I$coefficients[1,1] - model.left.I$coefficients[1,1]
se <- sqrt((model.right.I$se[1]*model.right.I$se[1])+(model.left.I$se[1]*model.left.I$se[1]))
bar.data1 <- as.data.frame(cbind(jump,se))

# Plot raw data
ggplot(data.sub1, aes(x=distance_border,y=chief_zscore)) +
  geom_point(data=data.over,color="grey",size=2,alpha = .4) +
  geom_point(data=data.under,color="grey",size=2,alpha = .4) +
  stat_summary_bin(fun.y='mean', bins=25,
                   color="#009E73", size=2, geom='point', data=data.over) +
  stat_summary_bin(fun.y='mean', bins=25,
                   color="#CC79A7", size=2, geom='point', data=data.under) +
  geom_smooth(method = 'lm', color='black', data=data.over) +
  geom_smooth(method = 'lm', color='black', data=data.under) +
  theme_bw() +
  coord_cartesian(ylim = c(-1.5, 2)) +
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size=1) +
  ylab("Traditional Leader Z-score") +
  xlab("Distance to Border") + 
  annotate(geom="text", x=2.5, y=2, label="Far from Headquarter", color="black") +
  annotate(geom="text", x=-2.5, y=2, label="Close to Headquarter", color="black")
ggsave("figure_3_institutionalized.pdf", width=4, height=4)

# repeat for not institutionalized
data.sub2 <- data.reg[which(data.reg$inst_chiefs_dum==0),]

data.over <- data.sub2[which(data.sub2$treatment==0),]
data.under <- data.sub2[which(data.sub2$treatment==1),]

ggplot(data.sub2, aes(x=distance_border,y=chief_zscore)) +
  geom_point(data=data.over,color="grey",size=2,alpha = .4) +
  geom_point(data=data.under,color="grey",size=2,alpha = .4) +
  stat_summary_bin(fun.y='mean', bins=25,
                   color="#009E73", size=2, geom='point', data=data.over) +
  stat_summary_bin(fun.y='mean', bins=25,
                   color="#CC79A7", size=2, geom='point', data=data.under) +
  geom_smooth(method = 'lm', color='black', data=data.over) +
  geom_smooth(method = 'lm', color='black', data=data.under) +
  theme_bw() +
  coord_cartesian(ylim = c(-1.5, 2)) +
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size=1) +
  ylab("Traditional Leader Z-score") +
  xlab("Distance to Border") +
  annotate(geom="text", x=2.5, y=2, label="Far from Headquarter", color="black") +
  annotate(geom="text", x=-2.5, y=2, label="Close to Headquarter", color="black")
ggsave("figure_3_notinstitutionalized.pdf", width=4, height=4)

model.left.NI <- felm(chief_zscore ~ distance_border | 0 |0| 0, data.over)
model.right.NI <- felm(chief_zscore ~ distance_border | 0 |0| 0, data.under)
jump <- model.right.NI$coefficients[1,1] - model.left.NI$coefficients[1,1]
se <- sqrt((model.right.NI$se[1]*model.right.NI$se[1])+(model.left.NI$se[1]*model.left.NI$se[1]))
bar.data2 <- as.data.frame(cbind(jump,se))

# combine intercept and SE for both cases
bar.data1$sys <- "Recognized"
bar.data2$sys <- "Not Recognized"
bar.data <- as.data.frame(rbind(bar.data1, bar.data2))

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot jump at border
ggplot(bar.data, aes(color=sys)) + 
  geom_linerange(aes(x = sys, ymin = jump - se*interval1, 
                     ymax = jump + se*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = sys, y = jump, ymin = jump - se*interval2,
                      ymax = jump + se*interval2, shape=sys),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  geom_hline(yintercept = 0, linetype="dotted", color = "black", size=1) +
  ylab("Jump at Border") +
  theme_bw() +
  theme(axis.title.x=element_blank()) +
  coord_cartesian(ylim = c(-1.5, 2)) +
  theme(legend.position = "none")
ggsave("figure_3_jump.pdf", width=2.4, height=4)


################################### Same for DHS ####################################
load("MainRegressionSampleDHS.RData")

# Subset to Institutionalized
data.sub1 <- data.reg[which(data.reg$inst_chiefs_dum==1),]

# Seperate different sides of the border
data.over <- data.sub1[which(data.sub1$treatment==0),]
data.under <- data.sub1[which(data.sub1$treatment==1),]

# Calculate correlation and store intercept and SE
model.left.I <- felm(development ~ distance_border | 0 |0| 0, data.over)
model.right.I <- felm(development ~ distance_border | 0 |0| 0, data.under)
jump <- model.right.I$coefficients[1,1] - model.left.I$coefficients[1,1]
se <- sqrt((model.right.I$se[1]*model.right.I$se[1])+(model.left.I$se[1]*model.left.I$se[1]))
bar.data1 <- as.data.frame(cbind(jump,se))

# plot data
ggplot(data.sub1, aes(x=distance_border,y=development)) +
  geom_point(data=data.over,color="grey",size=2,alpha = .4) +
  geom_point(data=data.under,color="grey",size=2,alpha = .4) +
  stat_summary_bin(fun.y='mean', bins=25,
                   color="#009E73", size=2, geom='point', data=data.over) +
  stat_summary_bin(fun.y='mean', bins=25,
                   color="#CC79A7", size=2, geom='point', data=data.under) +
  geom_smooth(method = 'lm', color='black', data=data.over) +
  geom_smooth(method = 'lm', color='black', data=data.under) +
  theme_bw() +
  coord_cartesian(ylim = c(-1.5, 2)) +
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size=1) +
  ylab("Development Index") +
  xlab("Distance to Border") + 
  annotate(geom="text", x=2.5, y=2, label="Far from Headquarter", color="black") +
  annotate(geom="text", x=-2.5, y=2, label="Close to Headquarter", color="black")
ggsave("figure_3_institutionalizedDHS.pdf", width=4, height=4)

# Same for non institutionalized
data.sub2 <- data.reg[which(data.reg$inst_chiefs_dum==0),]

data.over <- data.sub2[which(data.sub2$treatment==0),]
data.under <- data.sub2[which(data.sub2$treatment==1),]

ggplot(data.sub2, aes(x=distance_border,y=development)) +
  geom_point(data=data.over,color="grey",size=2,alpha = .4) +
  geom_point(data=data.under,color="grey",size=2,alpha = .4) +
  stat_summary_bin(fun.y='mean', bins=25,
                   color="#009E73", size=2, geom='point', data=data.over) +
  stat_summary_bin(fun.y='mean', bins=25,
                   color="#CC79A7", size=2, geom='point', data=data.under) +
  geom_smooth(method = 'lm', color='black', data=data.over) +
  geom_smooth(method = 'lm', color='black', data=data.under) +
  theme_bw() +
  coord_cartesian(ylim = c(-1.5, 2)) +
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size=1) +
  ylab("Development Index") +
  xlab("Distance to Border") +
  annotate(geom="text", x=2.5, y=2, label="Far from Headquarter", color="black") +
  annotate(geom="text", x=-2.5, y=2, label="Close to Headquarter", color="black")
ggsave("figure_3_notinstitutionalizedDHS.pdf", width=4, height=4)

model.left.NI <- felm(development ~ distance_border | 0 |0| 0, data.over)
model.right.NI <- felm(development ~ distance_border | 0 |0| 0, data.under)
jump <- model.right.NI$coefficients[1,1] - model.left.NI$coefficients[1,1]
se <- sqrt((model.right.NI$se[1]*model.right.NI$se[1])+(model.left.NI$se[1]*model.left.NI$se[1]))
bar.data2 <- as.data.frame(cbind(jump,se))

# combine intercept and SE for both cases
bar.data1$sys <- "Recognized"
bar.data2$sys <- "Not Recognized"
bar.data <- as.data.frame(rbind(bar.data1, bar.data2))

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot jump at border
ggplot(bar.data, aes(color=sys)) + 
  geom_linerange(aes(x = sys, ymin = jump - se*interval1, 
                     ymax = jump + se*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = sys, y = jump, ymin = jump - se*interval2,
                      ymax = jump + se*interval2, shape=sys),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  geom_hline(yintercept = 0, linetype="dotted", color = "black", size=1) +
  ylab("Jump at Border") +
  theme_bw() +
  theme(axis.title.x=element_blank()) +
  coord_cartesian(ylim = c(-1.5, 2)) +
  theme(legend.position = "none")
ggsave("figure_3_jumpDHS.pdf", width=2.4, height=4)



#####################################################################################
###################################### Bandwidth ####################################
#####################################################################################


rm(allModelFrame)

load("RegressionSample.RData")

data.reg <- data.reg[which(data.reg$distance_hq<150),]

data.all <- data.reg



allModelFrame <- data.frame(Variable = "test",
                            Coefficient = 1,
                            SE = 1,
                            modelName = "test",
                            bandwidth= 3)

allModelFrame$bandwidth <- as.integer(allModelFrame$bandwidth)


for(i in 3:20) {
  data.reg <- data.all
  data.reg$treatment <- data.reg[,paste("treatment_log", i, sep="")]
  data.reg$treatment_int <- data.reg[,paste("treatment_int_log", i, sep="")]
  
  data.reg$distance_border[which(data.reg$treatment==0)] <- -data.reg$distance_border[which(data.reg$treatment==0)]
  data.reg<- data.reg[-which(is.na(data.reg$treatment)), ]
  
  data.reg$treatment_int <- data.reg$treatment_int * (data.reg$dist_coeff_log*-1)
  data.reg$dist_treat <- data.reg$treatment * data.reg$distance_border
  
  
  data.reg <- data.reg[which(data.reg[,paste("distance_neigh", i, sep="")]<30),]
  
  data.reg$treatment_int <- scale(data.reg$treatment_int)
  data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
  data.reg$chief_zscore <- scale(data.reg$chief_zscore)
  
  model2 <- felm(chief_zscore ~ distance_border*inst_chiefs_dum + dist_treat*inst_chiefs_dum + treatment_int*inst_chiefs_dum + distance_capital*inst_chiefs_dum + distance_natborder*inst_chiefs_dum + distcoast*inst_chiefs_dum + elevation*inst_chiefs_dum + ruggedness*inst_chiefs_dum + agriculture*inst_chiefs_dum + cities*inst_chiefs_dum + malaria*inst_chiefs_dum + missions*inst_chiefs_dum + distrail*inst_chiefs_dum + area_sqkm*inst_chiefs_dum | fixed_effects |0| district_id, data.reg)
  
  # Put model estimates into temporary data.frames:
  model1aFrame <- data.frame(Variable = rownames(summary(model2)$coef),
                             Coefficient = summary(model2)$coef[4, 1],
                             SE = summary(model2)$coef[4, 2],
                             modelName = "Treatment",
                             bandwidth= i)
  model1bFrame <- data.frame(Variable = rownames(summary(model2)$coef),
                             Coefficient = summary(model2)$coef[18, 1],
                             SE = summary(model2)$coef[18, 2],
                             modelName = "Treatment x Recognized",
                             bandwidth= i)
  # Combine these data.frames
  allModelFrame <- data.frame(rbind(allModelFrame, model1aFrame, model1bFrame))
}



allModelFrame <- allModelFrame[-which(allModelFrame$Variable=="test"),]


# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier



ggplot(allModelFrame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient - SE*interval1, 
                     ymax = Coefficient + SE*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=20), legend.title=element_blank(), legend.background = element_rect(fill="transparent"), legend.position = 'bottom') +
  ylab("Effect of Treatment on Z-score") +
  xlab("Bandwidth (km)")

ggsave("figure_4_bandwidth.pdf", width=6, height=6)


## Panel B
load("RegressionSample.RData")

data.reg <- data.reg[which(data.reg$distance_hq<150),]

data.all <- data.reg



allModelFrame <- data.frame(Variable = "test",
                            Coefficient = 1,
                            SE = 1,
                            modelName = "test",
                            bandwidth= 3)

allModelFrame$bandwidth <- as.integer(allModelFrame$bandwidth)



for(i in 3:20) {
  data.reg <- data.all
  data.reg$treatment <- data.reg[,paste("treatment_log", i, sep="")]
  data.reg$treatment_int <- data.reg[,paste("treatment_int_log", i, sep="")]
  
  data.reg$distance_border[which(data.reg$treatment==0)] <- -data.reg$distance_border[which(data.reg$treatment==0)]
  data.reg<- data.reg[-which(is.na(data.reg$treatment)), ]
  
  data.reg$treatment_int <- data.reg$treatment_int * (data.reg$dist_coeff_log*-1)
  data.reg$dist_treat <- data.reg$treatment * data.reg$distance_border
  
  
  data.reg <- data.reg[which(data.reg[,paste("distance_neigh", i, sep="")]<30),]
  
  data.reg$treatment_int <- scale(data.reg$treatment_int)
  data.reg$treatment_int <- data.reg$treatment_int - min(data.reg$treatment_int, na.rm = TRUE)
  data.reg$chief_zscore <- scale(data.reg$chief_zscore)
  
  model1a <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg, subset = inst_chiefs_dum==0)
  model1b <- felm(chief_zscore ~ distance_border + dist_treat + treatment_int + distance_capital + distance_natborder + distcoast + elevation + ruggedness + agriculture + cities + malaria + missions + distrail + area_sqkm | fixed_effects |0| district_id, data.reg, subset = inst_chiefs_dum==1)
  
  
  # Put model estimates into temporary data.frames:
  model1aFrame <- data.frame(Variable = rownames(summary(model1a)$coef)[3],
                             Coefficient = summary(model1a)$coef[3, 1],
                             SE = summary(model1a)$coef[3, 2],
                             modelName = "Not Recognized",
                             bandwidth= i)
  
  model1bFrame <- data.frame(Variable = rownames(summary(model1b)$coef)[3],
                             Coefficient = summary(model1b)$coef[3, 1],
                             SE = summary(model1b)$coef[3, 2],
                             modelName = "Recognized",
                             bandwidth= i)
  # Combine these data.frames
  allModelFrame <- data.frame(rbind(allModelFrame, model1aFrame, model1bFrame))
}


allModelFrame <- allModelFrame[-which(allModelFrame$Variable=="test"),]


# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier


ggplot(allModelFrame, aes(color=modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = bandwidth, ymin = Coefficient - SE*interval1, 
                     ymax = Coefficient + SE*interval1),lwd = 1, 
                 position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = bandwidth, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2, shape = modelName),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  fill = "WHITE") + 
  scale_color_manual(values=c("#B2182B", "#92C5DE")) + 
  scale_shape_manual(values=c(21, 22)) +
  #coord_cartesian(ylim=c(2.5,-1.)) +
  theme_bw() +
  theme(text=element_text(size=20), legend.title=element_blank(), legend.position = 'bottom', legend.background = element_rect(fill="transparent")) +
  ylab("Effect of Treatment on Z-score") +
  xlab("Bandwidth (km)")

ggsave("figure_4b_bandwidth.pdf", width=6, height=6)