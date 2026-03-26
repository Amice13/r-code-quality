##Rensch's Rule Code

#Code author: Kaia J. Tombak

##The beginning of this code is the same as for the SSD paper (data prep and processing)
##(lines 1-170)

setwd("INSERT FILE PATH HERE")

require('ape')
require('geiger')
require('ggplot2')
require('plyr')
require('viridis')
require('phytools')
require('lmodel2')

#----Read in Data-------------------------------------------------

RR<-read.csv('Renschs Rule Mass Data.csv')


#-------------------BODY MASS ISO/ALLOMETRY---------------------------


#FIGURE 1: Log-log plot of mean male vs. mean female body mass,
##with separate regression lines plotted for each dimorphism category:

#Put it in a vertical orientation:
a<-ggplot(RR[RR$massDimorphism=='Female-Biased Dimorphic',], aes(x=log10(massM), y=log10(massF)))+
  geom_point(fill='darkorchid4', pch=21, show.legend=T, size=3.5, alpha=0.7, stroke=NA)+
  geom_smooth(method='lm', col='black', se=T)+
  xlab('')+
  ylab('')+
  xlim(c(0,7.5))+
  ylim(c(0,7.5))+
  annotate('text', label='Line of Isometry', x=6.4, y=6.7, angle=45, col='red', size=4.5)+
  geom_abline(intercept=0, slope=1, col='red', linetype='dashed')+
  ggtitle('Female-Biased Dimorphic Species')+
  theme_bw()+
  theme(text=element_text(size=17))

b<-ggplot(RR[RR$massDimorphism=='Monomorphic',], aes(x=log10(massM), y=log10(massF)))+
  geom_point(fill='goldenrod', pch=21, show.legend=T, size=3.5, alpha=0.7, stroke=NA)+
  geom_smooth(method='lm', se=T, col='black')+
  xlab('')+
  ylab('log(Mean Female Mass)')+
  xlim(c(0,7.5))+
  ylim(c(0,7.5))+
  annotate('text', label='Line of Isometry', x=6.4, y=6.7, angle=45, col='red', size=4.5)+
  geom_abline(intercept=0, slope=1, col='red', linetype='dashed')+
  ggtitle('Monomorphic Species')+
  theme_bw()+
  theme(text=element_text(size=17))

c<-ggplot(RR[RR$massDimorphism=='Male-Biased Dimorphic',], aes(x=log10(massM), y=log10(massF)))+
  geom_point(fill='#21918c', pch=21, show.legend=T, size=3.5, alpha=0.7, stroke=NA)+
  geom_smooth(method='lm', se=T, col='black')+
  xlab('log(Mean Male Mass)')+
  ylab('')+
  xlim(c(0,7.5))+
  ylim(c(0,7.5))+
  annotate('text', label='Line of Isometry', x=6.4, y=6.7, angle=45, col='red', size=4.5)+
  geom_abline(intercept=0, slope=1, col='red', linetype='dashed')+
  ggtitle('Male-Biased Dimorphic Species')+
  theme_bw()+
  theme(text=element_text(size=17))

#jpeg('RenschRule_Fig1_R2_vert.jpg', height=15, width=5, units='in', res=300)
gridExtra::grid.arrange(a, b, c, ncol=1)
#dev.off()

#note: Fig. 1 modified in ppt to add an insert showing the slopes predicted by Rensch's rule



###--------------ANALYSIS WITH PHYLOGENETIC CORRECTIONS:

#Run PGLS regressions

#Used the guidance from this blog to run the PGLS analyses:
#https://rpubs.com/alicehotopp/875379

#Read in the tree:
upham<-read.nexus('MamPhy_fullPosterior_BDvr_DNAonly_4098sp_topoFree_NDexp_v2_sample100_nexus.trees')

#This file contains 100 credible phylogenetic trees for Mammalia. 

#Use the first tree in the file to match the names of each spp between data frames
up<-upham[[1]]
class(up)#The class for this file is now phylo

#Make the scientific names the row.names in the RR data frame 
#and separate the genus and species with an underscore to match how it is in the tree
RR_phy<-RR
row.names(RR_phy)<-gsub(' ','_', RR$Genus_Sp_phy)

#check that species names match between the data frame and the phylogenetic tree
name.check(up, RR_phy)#no - the upham file has family and order tacked on

up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
head(up$tip.label)

name.check(up, RR_phy)
#33 of the species in RR_phy are not in the tree
#3703 species in the tree are not in RR_phy.

#Prune the tree:
up_pr<-drop.tip(up, name.check(up, RR_phy)$tree_not_data)

#exclude the taxa in the data that aren't in the tree
nm<-name.check(up_pr, RR_phy)

RR_phy<-RR_phy[which(!(row.names(RR_phy) %in% nm[[2]])),]

name.check(up_pr, RR_phy)#OK. Now the names match!


#Create a Genus-species column without the underscore:
RR_phy$Genus_Sp_phy<-gsub(' ', '_', RR_phy$Genus_Sp_phy)

#Do RR_phy rownames match the column RR_phy$Genus_Sp_phy? 
unique(RR_phy$Genus_Sp_phy == row.names(RR_phy))#yes

#Now test out a PGLS on the pooled data to determine what evolutionary model to use:

#1 . Assuming Brownian motion in rates of evolution:
pgls1<-gls(log10(massF) ~ log10(massM), data=RR_phy, 
           correlation = corBrownian(1, phy=up_pr, form=~Genus_Sp_phy))

summary(pgls1)
intervals(pgls1)
#95% confint for slope is 0.9576 to 0.9881, est=0.9729
AIC(pgls1)#-866.3

#2. Integrate Pagel's lambda model:
pgls2<-gls(log10(massF) ~ log10(massM), data=RR_phy, 
           correlation = corPagel(1, phy=up_pr, form=~Genus_Sp_phy))

summary(pgls2)
#lambda=0.774. Using the PGLS with Pagel's lamda PGLS is likely appropriate.
intervals(pgls2)
#95% confint for slope is 0.9655 to 0.9871, est=0.9763 (similar to the Brownian motion model)
AIC(pgls2)#-1023.3 (lower AIC than the model above)

#Now ready to run 100 PGLS's with Pagel's lambda using all the trees in the Upham data frame.

###Run the PGLS on each dimorphism category separately:
RR_phy_M<-RR_phy[RR_phy$massDimorphism=='Male-Biased Dimorphic',]
RR_phy_F<-RR_phy[RR_phy$massDimorphism=='Female-Biased Dimorphic',]
RR_phy_Mo<-RR_phy[RR_phy$massDimorphism=='Monomorphic',]


#1. a) Spp. with larger males, with log(male mass) on the x-axis:

#Start by creating an empty data frame to store the PGLS results:
PGLS100_M.m<-data.frame(matrix(ncol=4, nrow=100))
colnames(PGLS100_M.m)<-c('slope', 'log10(massM)_lower95CI', 'log10(massM)_upper95CI', 'lambda')

#now run a for loop, cycling through each PGLS using each tree & filling in the new df
for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_M)$tree_not_data)
  pgls<-gls(log10(massF) ~ log10(massM), data=RR_phy_M,
            correlation = corPagel(1, phy=up_pr, form=~Genus_Sp_phy))
  PGLS100_M.m[i,1]<-coef(pgls)[[2]]
  PGLS100_M.m[i,2]<-confint(pgls)[[2]]
  PGLS100_M.m[i,3]<-confint(pgls)[[4]]
  PGLS100_M.m[i,4]<-summary(pgls)$modelStruct$corStruct[[1]]
}

#Results:
#estimated slope:
mean(PGLS100_M.m[,1])#0.980
#lower CI for slope:
mean(PGLS100_M.m[,2])#0.966
#upper CI for slope:
mean(PGLS100_M.m[,3])#0.995
#lambda:
mean(PGLS100_M.m[,4])#0.884

#1. b) Spp. with larger males, with log(female mass) on the x-axis:

#Start by creating an empty data frame to store the PGLS results:
PGLS100_M.f<-data.frame(matrix(ncol=4, nrow=100))
colnames(PGLS100_M.f)<-c('slope', 'log10(massM)_lower95CI', 'log10(massM)_upper95CI', 'lambda')

#now run a for loop, cycling through each PGLS using each tree & filling in the new df
for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_M)$tree_not_data)
  pgls<-gls(log10(massM) ~ log10(massF), data=RR_phy_M,
            correlation = corPagel(1, phy=up_pr, form=~Genus_Sp_phy))
  PGLS100_M.f[i,1]<-coef(pgls)[[2]]
  PGLS100_M.f[i,2]<-confint(pgls)[[2]]
  PGLS100_M.f[i,3]<-confint(pgls)[[4]]
  PGLS100_M.f[i,4]<-summary(pgls)$modelStruct$corStruct[[1]]
}

#Results:
#estimated slope:
mean(PGLS100_M.f[,1])#1.009
#lower CI for slope:
mean(PGLS100_M.f[,2])#0.994
#upper CI for slope:
mean(PGLS100_M.f[,3])#1.024
#lambda:
mean(PGLS100_M.f[,4])#0.909

#2. a) Spp with larger females, with log(male mass) on the x-axis
PGLS100_F.m<-data.frame(matrix(ncol=4, nrow=100))
colnames(PGLS100_F.m)<-c('slope', 'log10(massM)_lower95CI', 'log10(massM)_upper95CI', 'lambda')

for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_F)$tree_not_data)
  pgls<-gls(log10(massF) ~ log10(massM), data=RR_phy_F,
            correlation = corPagel(1, phy=up_pr, form=~Genus_Sp_phy))
  PGLS100_F.m[i,1]<-coef(pgls)[[2]]
  PGLS100_F.m[i,2]<-confint(pgls)[[2]]
  PGLS100_F.m[i,3]<-confint(pgls)[[4]]
  PGLS100_F.m[i,4]<-summary(pgls)$modelStruct$corStruct[[1]]
}

#Results for spp with larger females:
#estimated slope:
mean(PGLS100_F.m[,1])#0.998
#lower CI for slope:
mean(PGLS100_F.m[,2])#0.992
#upper CI for slope:
mean(PGLS100_F.m[,3])#1.005
#lambda:
mean(PGLS100_F.m[,4])#0.0342

#2. b) Spp with larger females, with log(female mass) on the x-axis
PGLS100_F.f<-data.frame(matrix(ncol=4, nrow=100))
colnames(PGLS100_F.f)<-c('slope', 'log10(massM)_lower95CI', 'log10(massM)_upper95CI', 'lambda')

for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_F)$tree_not_data)
  pgls<-gls(log10(massM) ~ log10(massF), data=RR_phy_F,
            correlation = corPagel(1, phy=up_pr, form=~Genus_Sp_phy))
  PGLS100_F.f[i,1]<-coef(pgls)[[2]]
  PGLS100_F.f[i,2]<-confint(pgls)[[2]]
  PGLS100_F.f[i,3]<-confint(pgls)[[4]]
  PGLS100_F.f[i,4]<-summary(pgls)$modelStruct$corStruct[[1]]
}

#Results for spp with larger females:
#estimated slope:
mean(PGLS100_F.f[,1])#1.00
#lower CI for slope:
mean(PGLS100_F.f[,2])#0.994
#upper CI for slope:
mean(PGLS100_F.f[,3])#1.007
#lambda:
mean(PGLS100_F.f[,4])#0.0429

#3. a) Monomorphic species with log(male mass) on the x-axis:
PGLS100_Mo.m<-data.frame(matrix(ncol=4, nrow=100))
colnames(PGLS100_Mo.m)<-c('slope', 'log10(massM)_lower95CI', 'log10(massM)_upper95CI', 'lambda')

for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_Mo)$tree_not_data)
  pgls<-gls(log10(massF) ~ log10(massM), data=RR_phy_Mo,
            correlation = corPagel(1, phy=up_pr, form=~Genus_Sp_phy))
  PGLS100_Mo.m[i,1]<-coef(pgls)[[2]]
  PGLS100_Mo.m[i,2]<-confint(pgls)[[2]]
  PGLS100_Mo.m[i,3]<-confint(pgls)[[4]]  
  PGLS100_Mo.m[i,4]<-summary(pgls)$modelStruct$corStruct[[1]]
}

#Results for monomorphic spp:
#estimated slope:
mean(PGLS100_Mo.m[,1])#0.998
#lower CI for slope:
mean(PGLS100_Mo.m[,2])#0.992
#upper CI for slope:
mean(PGLS100_Mo.m[,3])#1.003
#lambda:
mean(PGLS100_Mo.m[,4])#0.353

#3. b) Monomorphic species with log(female mass) on the x-axis:
PGLS100_Mo.f<-data.frame(matrix(ncol=4, nrow=100))
colnames(PGLS100_Mo.f)<-c('slope', 'log10(massM)_lower95CI', 'log10(massM)_upper95CI', 'lambda')

for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_Mo)$tree_not_data)
  pgls<-gls(log10(massM) ~ log10(massF), data=RR_phy_Mo,
            correlation = corPagel(1, phy=up_pr, form=~Genus_Sp_phy))
  PGLS100_Mo.f[i,1]<-coef(pgls)[[2]]
  PGLS100_Mo.f[i,2]<-confint(pgls)[[2]]
  PGLS100_Mo.f[i,3]<-confint(pgls)[[4]]  
  PGLS100_Mo.f[i,4]<-summary(pgls)$modelStruct$corStruct[[1]]
}

#Results for monomorphic spp:
#estimated slope:
mean(PGLS100_Mo.f[,1])#1.001
#lower CI for slope:
mean(PGLS100_Mo.f[,2])#0.995
#upper CI for slope:
mean(PGLS100_Mo.f[,3])#1.006
#lambda:
mean(PGLS100_Mo.f[,4])#0.326



###Now run RMA's on each dimorphism category and with male and female body mass in the x-axis in turn


#4. a) RMA for male-biased dimorphic species, log(male mass) on the x-axis:
PRMA100_M.m<-data.frame(matrix(ncol=2, nrow=100))
colnames(PRMA100_M.m)<-c('slope', 'lambda')

for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_M)$tree_not_data)
  RR_phy_rmaM<-RR_phy_M[up_pr$tip.label,]
  ## pull out & log transform variables
  logBodyMassM<-setNames(log10(RR_phy_rmaM$massM),
                         rownames(RR_phy_rmaM))
  logBodyMassF<-setNames(log10(RR_phy_rmaM$massF),
                         rownames(RR_phy_rmaM))
  ## fit RMA regression & print results
  fitted.rma<-phyl.RMA(logBodyMassM,logBodyMassF,
                       up_pr)
  PRMA100_M.m[i,1]<-fitted.rma$RMA.beta[[2]]
  PRMA100_M.m[i,2]<-fitted.rma$lambda
}

ciM.m <- quantile(PRMA100_M.m$slope, probs = c(0.025, 0.975))
meanM.m <- mean(PRMA100_M.m$slope)


#4. b) RMA for male-biased dimorphic species, log(female mass) on the x-axis:
PRMA100_M.f<-data.frame(matrix(ncol=2, nrow=100))
colnames(PRMA100_M.f)<-c('slope', 'lambda')

for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_M)$tree_not_data)
  RR_phy_rmaM<-RR_phy_M[up_pr$tip.label,]
  ## pull out & log transform variables
  logBodyMassM<-setNames(log10(RR_phy_rmaM$massM),
                         rownames(RR_phy_rmaM))
  logBodyMassF<-setNames(log10(RR_phy_rmaM$massF),
                         rownames(RR_phy_rmaM))
  ## fit RMA regression & print results
  fitted.rma<-phyl.RMA(logBodyMassF,logBodyMassM,
                       up_pr)
  PRMA100_M.f[i,1]<-fitted.rma$RMA.beta[[2]]
  PRMA100_M.f[i,2]<-fitted.rma$lambda
}

ciM.f <- quantile(PRMA100_M.f$slope, probs = c(0.025, 0.975))
meanM.f <- mean(PRMA100_M.f$slope)

#5. a) RMA for female-biased dimorphic species, log(male mass) on the x-axis:

PRMA100_F.m<-data.frame(matrix(ncol=2, nrow=100))
colnames(PRMA100_F.m)<-c('slope', 'lambda')

for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_F)$tree_not_data)
  RR_phy_rmaF<-RR_phy_F[up_pr$tip.label,]
## pull out & log transform variables
  logBodyMassM<-setNames(log10(RR_phy_rmaF$massF),
                     rownames(RR_phy_rmaF))
  logBodyMassF<-setNames(log10(RR_phy_rmaF$massM),
                      rownames(RR_phy_rmaF))
## fit RMA regression & print results
  fitted.rma<-phyl.RMA(logBodyMassM,logBodyMassF,
                     up_pr)
  PRMA100_F.m[i,1]<-fitted.rma$RMA.beta[[2]]
  PRMA100_F.m[i,2]<-fitted.rma$lambda
}

ciF.m <- quantile(PRMA100_F.m$slope, probs = c(0.025, 0.975))
meanF.m <- mean(PRMA100_F.m$slope)


#5. b) RMA for female-biased dimorphic species, log(female mass) on the x-axis:

PRMA100_F.f<-data.frame(matrix(ncol=2, nrow=100))
colnames(PRMA100_F.f)<-c('slope', 'lambda')

for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_F)$tree_not_data)
  RR_phy_rmaF<-RR_phy_F[up_pr$tip.label,]
  ## pull out & log transform variables
  logBodyMassM<-setNames(log10(RR_phy_rmaF$massF),
                         rownames(RR_phy_rmaF))
  logBodyMassF<-setNames(log10(RR_phy_rmaF$massM),
                         rownames(RR_phy_rmaF))
  ## fit RMA regression & print results
  fitted.rma<-phyl.RMA(logBodyMassF,logBodyMassM,
                       up_pr)
  PRMA100_F.f[i,1]<-fitted.rma$RMA.beta[[2]]
  PRMA100_F.f[i,2]<-fitted.rma$lambda
}

ciF.f <- quantile(PRMA100_F.f$slope, probs = c(0.025, 0.975))
meanF.f <- mean(PRMA100_F.f$slope)

#6. a) RMA for monomorphic species, log(male mass) on the x-axis:
PRMA100_Mo.m<-data.frame(matrix(ncol=2, nrow=100))
colnames(PRMA100_Mo.m)<-c('slope', 'lambda')

for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_Mo)$tree_not_data)
  RR_phy_rmaMo<-RR_phy_Mo[up_pr$tip.label,]
  ## pull out & log transform variables
  logBodyMassM<-setNames(log10(RR_phy_rmaMo$massM),
                         rownames(RR_phy_rmaMo))
  logBodyMassF<-setNames(log10(RR_phy_rmaMo$massF),
                         rownames(RR_phy_rmaMo))
  ## fit RMA regression & print results
  fitted.rma<-phyl.RMA(logBodyMassM,logBodyMassF,
                       up_pr)
  PRMA100_Mo.m[i,1]<-fitted.rma$RMA.beta[[2]]
  PRMA100_Mo.m[i,2]<-fitted.rma$lambda
}

ciMo.m <- quantile(PRMA100_Mo.m$slope, probs = c(0.025, 0.975))
meanMo.m <- mean(PRMA100_Mo.m$slope)


#6. b) RMA for monomorphic species, log(female mass) on the x-axis:
PRMA100_Mo.f<-data.frame(matrix(ncol=2, nrow=100))
colnames(PRMA100_Mo.f)<-c('slope', 'lambda')

for(i in 1:100){
  up<-upham[[i]]
  up$tip.label<-stringr::str_extract(up$tip.label, "[^_]*_[^_]*")
  up_pr<-drop.tip(up, name.check(up, RR_phy_Mo)$tree_not_data)
  RR_phy_rmaMo<-RR_phy_Mo[up_pr$tip.label,]
  ## pull out & log transform variables
  logBodyMassM<-setNames(log10(RR_phy_rmaMo$massM),
                         rownames(RR_phy_rmaMo))
  logBodyMassF<-setNames(log10(RR_phy_rmaMo$massF),
                         rownames(RR_phy_rmaMo))
  ## fit RMA regression & print results
  fitted.rma<-phyl.RMA(logBodyMassF,logBodyMassM,
                       up_pr)
  PRMA100_Mo.f[i,1]<-fitted.rma$RMA.beta[[2]]
  PRMA100_Mo.f[i,2]<-fitted.rma$lambda
}

ciMo.f <- quantile(PRMA100_Mo.f$slope, probs = c(0.025, 0.975))
meanMo.f <- mean(PRMA100_Mo.f$slope)


###----FAMILY-SPECIFIC REGRESSION SLOPES FOR EACH DIMORPHISM CATEGORY----

fam_SSD<-as.data.frame(table(RR$massDimorphism, group_by=RR$Family))
fam_SSD<-fam_SSD[fam_SSD$Freq>3,]#filter families by a minimum sample size 
#of 4 species, following Webb and Freckleton (2007)
fam_SSD<-fam_SSD[,c(2,1,3)]
colnames(fam_SSD)<-c('Family', 'massDimorphism', 'Freq')
length(unique(fam_SSD$Family))
#20 out of the 83 families have at least 4 species in at least one dimorphism category

#create a data frame of a subset of RR containing only these families
RR_fam<-RR[RR$Family %in% fam_SSD$Family,]
length(unique(RR_fam$Family))#20 families

#Create a column in each data frame for the family-dimorphism category group
fam_SSD$Fam_massDimorphism<-paste0(fam_SSD$Family, '_', fam_SSD$massDimorphism)

RR_fam$Fam_massDimorphism<-paste0(RR_fam$Family, '_', RR_fam$massDimorphism)

RR_fam<-RR_fam[RR_fam$Fam_massDimorphism %in% fam_SSD$Fam_massDimorphism,]

#Create a table of slopes and their 95% CI's and sample sizes for each 
#dimorphism category with at least 4 species within each of these 27 family-dimorphism categories

# Break up RR_fam by Fam_massDimorphism, then fit the log-log model to each group 
#and return a list
##(both OLS and RMA model results are given in each regression in the list generated below)
rmamodels <- dlply(RR_fam, "Fam_massDimorphism", function(RR_fam) 
  lmodel2(log10(massF) ~ log10(massM), data = RR_fam, nperm=100))
#Note that this throws a notice that 'RMA was not requested: it will not be computed.' But here
##R is referring to a 'ranged major axis' regression, and the 'reduced major axis' regression
##we want is produced in the output under 'SMA', or 'standard major axis' regression,
##which is the same as a 'reduced major axis' regression.

#To view results for each family-dimorphism group regression (listed in Table 1 of the manuscript)
##view each in turn like this (replace the '1' to switch to another family-dimorphism group):
rmamodels[1]

#And now rerun the family-specific models with the axes reversed:
rmamodels_rev <- dlply(RR_fam, "Fam_massDimorphism", function(RR_fam) 
  lmodel2(log10(massM) ~ log10(massF), data = RR_fam, nperm=100))



#Additional analyses:

#Create a simplified data frame for the family-dimorphism groups analyzed
##(those with at least 4 species in the dataset):
Fam_Corrs<-data.frame(matrix(ncol=5, nrow=length(unique(RR_fam$Fam_massDimorphism))))
colnames(Fam_Corrs)<-c('Fam_massDimorphism', 'Slope', 'lower95CI', 'upper95CI', 'Sample')

Fam_Corrs[,1]<-unique(RR_fam$Fam_massDimorphism)

#Add basic info on just the OLS slopes (for the family-specific analyses, when OLS slopes differed
##from 1, so did the RMA slopes and so did the slopes for regressions with axes reversed, 
##see Table 1):
# Break up RR_fam by Fam_massDimorphism, then fit the log-log model to each group 
#and return a list
models <- dlply(RR_fam, "Fam_massDimorphism", function(RR_fam) 
  lm(log(massF) ~ log(massM), data = RR_fam))

# Apply coef to each model and return a data frame
coefs<-ldply(models, coef)

# Apply confint to each model and return a data frame
confint<-ldply(models, confint)
#remove the confidence intervals for intercepts
confint2<-confint[seq(2, nrow(confint), 2),]

#Fill in the Fam_Corrs data frame with the coefficients and confidence intervals
Fam_Corrs[,2]<-coefs[match(Fam_Corrs[,1], coefs[,1]),3]
Fam_Corrs[,3]<-confint2[match(Fam_Corrs[,1], confint2[,1]),2]
Fam_Corrs[,4]<-confint2[match(Fam_Corrs[,1], confint2[,1]),3]
Fam_Corrs[,5]<-fam_SSD[match(Fam_Corrs[,1], fam_SSD[,4]),3]

Fam_Corrs$Family<-gsub("_.*", "", Fam_Corrs$Fam_massDimorphism)
Fam_Corrs$massDimorphism<-gsub(".*_", "", Fam_Corrs$Fam_massDimorphism)
Fam_Corrs$Order<-RR_fam[match(Fam_Corrs$Family, RR_fam$Family),]$Order


#Is sample size higher in families in which Rensch's rule was supported?
Fam_Corrs$Isometric<-ifelse(Fam_Corrs$lower95CI<1 & Fam_Corrs$upper95CI>1, 'yes', 'no')
Fam_Corrs$Support<-ifelse(Fam_Corrs$massDimorphism!="Monomorphic" & Fam_Corrs$Slope<1 & Fam_Corrs$Isometric=='no', 'yes', 'no')
Fam_Corrs$Support<-ifelse(Fam_Corrs$massDimorphism=="Monomorphic", 'na', Fam_Corrs$Support)

#Run a Wilcoxon Rank Sum test to see whether sample size is higher in those clades that were not isometric
wilcox.test(Fam_Corrs[Fam_Corrs$Isometric=='yes',]$Sample, Fam_Corrs[Fam_Corrs$Isometric=='no',]$Sample)
#W = 53.5, p=1
#No evidence of a difference in sample size between isometric and non-isometric groups


#Is sample size higher in families in which Rensch's rule was supported?
Fam_Corrs$Isometric<-ifelse(Fam_Corrs$lower95CI<1 & Fam_Corrs$upper95CI>1, 'yes', 'no')
Fam_Corrs$Support<-ifelse(Fam_Corrs$massDimorphism!="Monomorphic" & Fam_Corrs$Slope<1 & Fam_Corrs$Isometric=='no', 'yes', 'no')
Fam_Corrs$Support<-ifelse(Fam_Corrs$massDimorphism=="Monomorphic", 'na', Fam_Corrs$Support)

#Run a Wilcoxon Rank Sum test to see whether sample size is higher in those clades that were not isometric
wilcox.test(Fam_Corrs[Fam_Corrs$Isometric=='yes',]$Sample, Fam_Corrs[Fam_Corrs$Isometric=='no',]$Sample)
#W = 53.5, p=1
#No evidence of a difference in sample size between isometric and non-isometric groups

#Add columns for number of species in the family (data from Burgin et al. 2018)
##and proportion of family represented in sample
Fam_Corrs$Spp_in_Fam<-c(31,297,93,64,214,214,214,197,493,493,76,111,111,440,440,67,89,89,160,
                        20,21,21,792,792,792,69,834,834,834,298,298)
Fam_Corrs$Prop_Spp_Rep<-Fam_Corrs$Sample/Fam_Corrs$Spp_in_Fam


#FIGURE 2: Plot the spread of slopes in each family-dimorphism group

#Order the plot by order, then family, and color by dimorphism category

Fam_Corrs$PhyloPosition<-c(4,5,6,7,9,10,11,8,13,12,3,1,2,15,14,21,16,17,18,20,19,23,24,26,25,22,29,28,27,31,30)

#Now make each point's size proportional to the sample size:
colnames(Fam_Corrs)[7]<-'Category'

#tiff('RenschRule_SlopeDistributionsByFam_PhyloPositions_SampleSize.tiff', height=7.8, width=10, units='in', res=300)

ggplot(Fam_Corrs, aes(y=PhyloPosition, x=Slope, size=Sample))+
  geom_pointrange(aes(xmin=lower95CI, xmax=upper95CI, col=Category, fill=Category), 
                  linetype='solid', shape=21, alpha=0.5, linewidth=1)+
  ylab('')+
  xlab('Slope of log(female mass) ~ log(male mass) Regression\n(error bars represent the 95% CIs for the slope)')+
  scale_fill_manual(values=c('darkorchid4','#21918c', 'orange'))+
  scale_color_manual(values=c('darkorchid4','#21918c', 'orange4'))+
  geom_vline(xintercept=1.00, col='red', linetype='dashed', linewidth=1)+
  labs(size='Sample Size (no. species)')+
  scale_y_reverse(breaks=seq(1, 31, by=1), limits=c(32,0), expand=c(0,0))+
  theme_bw()+
  annotate('text', label='Line of Isometry', x=0.87, y=1, angle=0, col='red', size=6)+
  theme(text=element_text(size=17),
        legend.title=element_text(size=15),
        legend.text=element_text(size=14, margin=margin(l=15)),
        axis.text.y=element_blank(),
        legend.position=c(0.825, 0.79),
        legend.box.margin=margin(1,1,20,20),
        legend.box.background=element_rect(colour='black'),
        legend.box.just="left")

dev.off()

#Note that the y-axis labels and phylogeny were added to this figure manually in Powerpoint.
