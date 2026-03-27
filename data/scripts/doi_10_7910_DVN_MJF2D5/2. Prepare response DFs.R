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

Distbushland <- sort(unique(data_st$Distbushland))
Distwater<- sort(unique(data_st$Distwater))
TreeCoverage <- sort(unique(data_st$TreeCoverage))
Size <- sort(unique(data_st$Size))
Light5km  <- sort(unique(data_st$Light5km))

######################################################################################

# Preparing data for plots
# Species Richness
sp_rich <- glmer(BatSpecies ~ Habitat + Light5km + Distbushland + TreeCoverage + Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"),family = poisson, data =data_st)
b <- fixef(sp_rich)
rich_hab1 <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 
rich_hab2 <- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 
rich_light <- b[1]+ b[2] * 0 + b[3] * Light5km + b[4] * 0 + b[5] * 0 + b[6] * 0 
rich_bush <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * Distbushland + b[5] * 0 + b[6] * 0 
rich_tree <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * TreeCoverage + b[6] * 0 

rich_light <- exp(rich_light)
rich_bush <- exp(rich_bush)
rich_tree <- exp(rich_tree)
rich_hab1 <- exp(rich_hab1)
rich_hab2 <- exp(rich_hab2)

######################################################################################
# Bat activity
activity<- glmer(BatActivity ~ Habitat + Light5km + Distbushland + TreeCoverage + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(activity)
act_hab1 <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
act_hab2 <- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
act_light <- b[1]+ b[2] * 0 + b[3] * Light5km + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
act_bush <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * Distbushland + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
act_tree <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * TreeCoverage + b[6] * 0 + b[7] * 0 + b[8] * 0

act_light <- exp(act_light)
act_bush <- exp(act_bush)
act_tree <- exp(act_tree)
act_hab1 <- exp(act_hab1)
act_hab2 <- exp(act_hab2)

######################################################################################
# Chalinolobus gouldii
CG <- glmer (Chgouldii ~ Habitat + Light5km + Distbushland + TreeCoverage  + Temp*Moon + (1|Season)+ (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(CG)
CG_hab1 <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
CG_hab2 <- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
CG_light <- b[1]+ b[2] * 0 + b[3] * Light5km + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
CG_bush <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * Distbushland + b[5] * 0 + b[6] * 0 + b[7] * 0 + b [8] * 0
CG_tree<- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * TreeCoverage + b[6] * 0 + b[7] * 0 + b[8] * 0

CG_hab1 <- exp(CG_hab1)
CG_hab2 <- exp(CG_hab2)
CG_light <- exp(CG_light)
CG_bush <- exp(CG_bush)
CG_tree <- exp(CG_tree)

######################################################################################
# Vespadelus vulturnus
VV <- glmer(Vvulturnus ~ Habitat + Light5km + TreeCoverage  + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(VV)
VV_hab1 <- b[1]+ b[2] * 0+ b[3] * 0  + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
VV_hab2 <- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
VV_light <- b[1]+ b[2] * 0+ b[3] * Light5km  + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
VV_tree <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * TreeCoverage + b[5] * 0 + b[6] * 0 + b[7] * 0

VV_light <- exp(VV_light)
VV_tree <- exp(VV_tree)
VV_hab1 <- exp(VV_hab1)
VV_hab2 <- exp(VV_hab2)

######################################################################################
# Vespadelus darlingtoni
VD <- glmer (Vdarlingtoni ~ Habitat + Light5km + Distbushland + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(VD)
VD_hab1 <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
VD_hab2 <- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
VD_light <- b[1]+ b[2] * 0 + b[3] * Light5km + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
VD_bush <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * Distbushland + b[5] * 0 + b[6] * 0 + b[7] * 0 

VD_hab1 <- exp(VD_hab1)
VD_hab2 <- exp(VD_hab2)
VD_light <- exp(VD_light)
VD_bush <- exp(VD_bush)


######################################################################################
# Chalinolobus morio
CM<- glmer(Chmorio ~ Habitat + Light5km + Distbushland +  Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(CM)
CM_hab1 <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
CM_hab2 <- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
CM_light <- b[1]+ b[2] * 0 + b[3] * Light5km + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
CM_bush <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * Distbushland + b[5] * 0 + b[6] * 0 + b[7] * 0

CM_light <- exp(CM_light)
CM_bush <- exp(CM_bush)
CM_hab1 <- exp(CM_hab1)
CM_hab2 <- exp(CM_hab2)

######################################################################################
# Austronomus australis
AA<- glmer(Aaustralis ~ Light5km + Distbushland + Distwater +  Size + Temp*Moon + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(AA)
AA_light <- b[1]+ b[2] * Light5km + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
AA_bush <- b[1]+ b[2] * 0 + b[3] * Distbushland + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
AA_wat <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * Distwater + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
AA_sz <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * Size + b[6] * 0 + b[7] * 0 + b[8] * 0

AA_light <- exp(AA_light)
AA_bush <- exp(AA_bush)
AA_wat <- exp(AA_wat)
AA_sz <- exp(AA_sz)

######################################################################################
# Scotorepens spp.
Scot <- glmer (Scotspp. ~ Habitat + Light5km + Distbushland + TreeCoverage  + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(Scot)
Scot_hab1 <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
Scot_hab2 <- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
Scot_light <- b[1]+ b[2] * 0 + b[3] * Light5km + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
Scot_bush <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * Distbushland + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
Scot_tree <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * TreeCoverage + b[6] * 0 + b[7] * 0 + b[8] * 0

Scot_hab1 <- exp(Scot_hab1)
Scot_hab2 <- exp(Scot_hab2)
Scot_light <- exp(Scot_light)
Scot_bush <- exp(Scot_bush)
Scot_tree <- exp(Scot_tree)

######################################################################################
# CG/Morm group
CGMorm <- glmer(CG.Mormlp ~ Habitat + Light5km + Distbushland + TreeCoverage + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(CGMorm)
CGMorm_hab1 <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
CGMorm_hab2 <- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
CGMorm_light <- b[1]+ b[2] * 0 + b[3] * Light5km + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
CGMorm_bush <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * Distbushland + b[5] * 0 + b[6] * 0 + b[7] * 0 + b[8] * 0
CGMorm_tree <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * TreeCoverage + b[6] * 0 + b[7] * 0 + b[8] * 0

CGMorm_hab1 <- exp(CGMorm_hab1)
CGMorm_hab2 <- exp(CGMorm_hab2)
CGMorm_light <- exp(CGMorm_light)
CGMorm_bush <- exp(CGMorm_bush)
CGMorm_tree <- exp(CGMorm_tree)


######################################################################################
# Vespadelus
VR <- glmer(Vregulus ~ Light5km + Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(VR)

VR_light <- b[1]+ b[2] * Light5km + b[3] * 0 

VR_light <- exp(VR_light)


######################################################################################

NS <- glmer(Nyctspp ~ Habitat + Light5km + TreeCoverage  + Temp*Moon + (1|Season)  + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(NS)

NS_hab1<- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
NS_hab2<- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
NS_light<- b[1]+ b[2] * 0 + b[3] *  Light5km + b[4] * 0 + b[5] * 0 + b[6] * 0 + b[7] * 0
NS_tree<- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * TreeCoverage + b[5] * 0 + b[6] * 0 + b[7] * 0

NS_light <- exp(NS_light)
NS_tree <- exp(NS_tree)
NS_hab1 <- exp(NS_hab1)
NS_hab2 <- exp(NS_hab2)

######################################################################################

Msch <- glmer (Mschocean ~ Habitat + Distbushland + Distwater + Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(Msch)

Msch_hab1 <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * 0 
Msch_hab2 <- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 
Msch_bush <- b[1]+ b[2] * 0 + b[3] * Distbushland + b[4] * 0 + b[5] * 0 
Msch_wat <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * Distwater + b[5] * 0 

Msch_hab1 <- exp(Msch_hab1)
Msch_hab2 <- exp(Msch_hab2)
Msch_bush <- exp(Msch_bush)
Msch_wat <- exp(Msch_wat)

######################################################################################

MM <- glmer(Mmacropus ~ Light5km + Temp*Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"), family = poisson, data =data_st)
b <- fixef(MM)
MM_light <- b[1]+ b[2] * Light5km + b[3] * 0  + b[4] * 0 + b[5] * 0
MM_tree <- b[1]+ b[2] * 0 + b[3] * TreeCoverage  + b[4] * 0 + b[5] * 0
MM_light <- exp(MM_light)



######################################################################################
######################################################################################
hab <- unname(c(CG_hab1, VV_hab1, VD_hab1, CM_hab1, Scot_hab1, CGMorm_hab1, NS_hab1, Msch_hab1, CG_hab2, VV_hab2, VD_hab2, CM_hab2, Scot_hab2, CGMorm_hab2, NS_hab2, Msch_hab2))
habs<- scale(hab)
hab <- data.frame(hab1 = habs[1:8], hab2 = habs[9:16])

light <- data.frame(CG_light, VV_light, VD_light, CM_light, AA_light, Scot_light, CGMorm_light, VR_light, NS_light, MM_light)
light<- data.frame(apply(X=light, MARGIN=2, FUN=scale))

sz <- data.frame(AA_sz)
sz<- data.frame(apply(X=sz, MARGIN=2, FUN=scale))

bush <- data.frame(CG_bush, VD_bush, CM_bush, AA_bush, Scot_bush, CGMorm_bush, Msch_bush)
bush<- data.frame(apply(X=bush, MARGIN=2, FUN=scale))

tree <- data.frame(CG_tree, VV_tree, Scot_tree, CGMorm_tree, NS_tree)
tree<- data.frame(apply(X=tree, MARGIN=2, FUN=scale))

wat <- data.frame(AA_wat, Msch_wat)
wat<- data.frame(apply(X=wat, MARGIN=2, FUN=scale))

responses_norichact <- list(hab, light, sz, bush, tree, wat)
save(responses_norichact, file = "response_DFs_norichact2.R")

