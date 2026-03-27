# Set directory and load libraries
setwd("C:/Users/swift 5/Documents/Publications/Landscape plots")
library(lme4)
library(AICcmodavg)

# Load data and creating factor and numeric variables
wetlands = read.csv("Urbanbatsandwetlands_Straka et al.2016.csv", sep = ";")
wetlands$Habitat[wetlands$Habitat == 0] <- 2
wetlands$Moon <- as.factor(wetlands$Moon)
wetlands$Habitat <- as.factor(wetlands$Habitat)
wetlands$Season <- as.factor(wetlands$Season)
wetlands$Size <- as.numeric(wetlands$Size)
wetlands$SQQ <- as.numeric(wetlands$SQQ)
wetlands$Light5km <- as.numeric(wetlands$Light5km)
wetlands$Distwater <- as.numeric(wetlands$Distwater)
wetlands$Distbushland <- as.numeric(wetlands$Distbushland)
wetlands$TreeCoverage <- as.numeric(wetlands$TreeCoverage)
wetlands$Temp <- as.numeric(wetlands$Temp)

# Scaling the explanatory variables
data_st <- cbind(wetlands[,c(1:20)], apply(X=wetlands[,c(21:27)], MARGIN=2, FUN=scale))
save(data_st, file = "data_st2.R")

##GLMMs for (Species) Activity and Bat Species Richness
sp_rich <- glmer(BatSpecies ~ Habitat + Light5km + Distbushland + TreeCoverage + Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"),family = poisson, data =data_st)
activity<- glmer(BatActivity ~ Habitat + Light5km + Distbushland + TreeCoverage + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
CG <- glmer (Chgouldii ~ Habitat + Light5km + Distbushland + TreeCoverage  + Temp*Moon + (1|Season)+ (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
VV <- glmer(Vvulturnus ~ Habitat + Light5km + TreeCoverage  + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
VD <- glmer (Vdarlingtoni ~ Habitat + Light5km + Distbushland + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
CM<- glmer(Chmorio ~ Habitat + Light5km + Distbushland +  Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
AA<- glmer(Aaustralis ~ Light5km + Distbushland + Distwater +  Size + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
Scot <- glmer (Scotspp. ~ Habitat + Light5km + Distbushland + TreeCoverage  + Temp*Moon +  (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
CGMorm <- glmer(CG.Mormlp ~ Habitat + Light5km + Distbushland + TreeCoverage + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
VR <- glmer(Vregulus ~ Light5km + Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
Nyct <- glmer(Nyctspp ~ Habitat + Light5km + TreeCoverage  + Temp*Moon + (1|Season)  + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
Msch <- glmer (Mschocean ~ Habitat + Distbushland + Distwater + Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
MM <- glmer(Mmacropus ~ Light5km + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)