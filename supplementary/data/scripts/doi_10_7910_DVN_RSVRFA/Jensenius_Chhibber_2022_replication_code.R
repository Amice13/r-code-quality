#################################################################################
### Replication code for "Privileging one's own? Voting patterns and politicized spending in India"
### by Francesca R. Jensenius and Pradeep Chhibber
################################################################################

##Code analyzing 2009 electoral patterns and LS15 (2009-14) MPlads spending 
rm(list = ls())

library(Hmisc)
library(scales)
library(SemiPar)
library(arm)
library(memisc)
library(apsrtable)

setwd("") #Set to the folder the data is saved in

load("Jensenius_Chhibber_2022_LS15.RData")
dim(DTA)
names(DTA)

table(DTA$census_units) ##The variable census_units tells us how many villages vote in a PS: numbers smaller than 0 indicate that there are multiple PS to a village
prop.table(table(DTA$census_units==1)) #One PS to a village
prop.table(table(DTA$census_units>1)) #More than one village vote in a PS
prop.table(table(DTA$census_units<0.3)) #The share of the data that has more than 3 PS in the same village unit

DTA <-DTA[DTA$census_units>0.3,] #Kick out localities with more than 3 polling stations. 
DTA <-DTA[DTA$PCA11_POP<10000,]  #Kick out 81 places that had large population sizes, suggesting they do have more than three PS even if our data does not reflect that

#################################################################################################
###Table A.5 -- summarizing main variables
#################################################################################################

names(DTA)
myindex<-c(9, 8, 18, 28, 29, 19, 24, 25, 26, 27)
names(DTA)[c(9, 8, 18, 28, 29, 19, 24, 25, 26, 27)]

mytable<-matrix(nrow=length(myindex), ncol=5)
for (i in 1:length(myindex)){
DTA[, myindex[i]]<-as.numeric(as.character(DTA[, myindex[i]]))
mytable[i,1]<-round(mean(DTA[, myindex[i]], na.rm=T),2)
mytable[i,2]<-round(sd(DTA[, myindex[i]], na.rm=T),2)
mytable[i,3]<-round(min(DTA[, myindex[i]], na.rm=T),2)
mytable[i,4]<-round(median(DTA[, myindex[i]], na.rm=T),2)
mytable[i,5]<-round(max(DTA[, myindex[i]], na.rm=T),0)
}

library(xtable)
row.names(mytable)<-names(DTA)[myindex]
mytable
row.names(mytable)<-c("Margin of Victory for the MP", "Vote-share for MP", "Any project", "Allocated funds (in thousand rs.)", "Allocated funds (logged)", "Completed project", "Population size 2011", "Literacy rate 2011", "Share SCs 2011", "Share STs 2011")
colnames(mytable)<-c("Mean", "Standard div.", "Min.", "Median", "Max.")
xtable(mytable, digits=c(2,2, 2,2,2,0))

#################################################################################################
##Figure A.1 and A.2 descirbing the variables village vote-share and village MoV
#########################################################################################


pdf(file = "Figures/Fig_A1.pdf", height = 4, width = 5)
par(mfrow=c(1,1), mar=c(4, 1, .5, .5))
plot(density(DTA$PS_PCwinner_percent[complete.cases(DTA$PS_PCwinner_percent)]), xlab="Village vote-share for MP", xlim=c(0, 100), ylim=c(0, .03),las=1, main="", yaxt="n")
polygon(density(DTA$PS_PCwinner_percent[complete.cases(DTA$PS_PCwinner_percent)]), col = "gray90")
abline(v=mean(DTA$PS_PCwinner_percent, na.rm=T), lty = 2, lwd=2, col = "black")
text(x=mean(DTA$PS_PCwinner_percent, na.rm=T), y=0.025, pos=4, paste("Average village vote-share ", round(mean(DTA$PS_PCwinner_percent, na.rm=T), 1), "%", sep=""))
dev.off()

pdf(file = "Figures/Fig_A2.pdf", height = 4, width = 5)
par(mfrow=c(1,1), mar=c(4, 1, .5, .5))
plot(density(DTA$villMoV[complete.cases(DTA$villMoV)]), xlab="Village MoV for MP", xlim=c(-100, 100), ylim=c(0, .02),las=1, main="", yaxt="n")
polygon(density(DTA$villMoV[complete.cases(DTA$villMoV)]), col = "gray90")
abline(v=mean(DTA$villMoV, na.rm=T), lty = 2, lwd=2, col = "black")
text(x=mean(DTA$villMoV, na.rm=T), y=0.018, pos=4, paste("Average Village MoV ", round(mean(DTA$villMoV, na.rm=T), 1), "%", sep=""))
dev.off()


#################################################################################################
##MPLADS spending and Vllage MoV 
#########################################################################################

DTA$villMoV_bins<-cut(DTA$villMoV, breaks=seq(-100, 100, 5), include.lowest=T)
tapply(DTA$anyproject, DTA$villMoV_bins, mean)
tapply(DTA$anyproject, DTA$villMoV_bins, table)

pdf("Figures/Fig_1.pdf", width=8, height =3)
par(mfrow=c(1,3), mar=c(4.5,4, 2.5,.5))
plot(100*tapply(DTA$anyproject, DTA$villMoV_bins, mean), pch=21, col="gray20", bg=alpha("gray70", 0.6),
cex=(tapply(DTA$anyproject[complete.cases(DTA$anyproject)], DTA$villMoV_bins[complete.cases(DTA$anyproject)], length)/8000)+0.5, 
ylim=c(0,20), xaxt="n",
xlab="MoV for MP in village in 2009", ylab="% getting a project", las=1,
main="Any project")
mgp.axis(1, at=c(1, 10, 20, 30, 40), labels=c("-100", "-50", "0", "50", "100"), mgp = c(3, 0.8, 0), las=1)
abline(v=20, lty=2)
text(y=18, x=1, paste("N PCs =", length(unique(DTA$UniquePC[complete.cases(DTA$anyproject)])), 
"\nN vill.=", length(DTA$anyproject[complete.cases(DTA$anyproject)])), pos=4)
points(x=39, y=1, pch=21, col="gray20", bg=alpha("gray80"), cex=(10000/8000)+0.5)
text(x=38, y=1, "10,000 obs.", pos=2)
points(x=39, y=3, pch=21, col="gray20", bg=alpha("gray80"), cex=(500/8000)+0.5)
text(x=38, y=3, "500 obs.", pos=2)

plot(tapply(DTA$cost_th, DTA$villMoV_bins, mean),pch=21, col="gray20", bg=alpha("gray70", 0.6),
cex=(tapply(DTA$cost_th[complete.cases(DTA$cost_th)], DTA$villMoV_bins[complete.cases(DTA$cost_th)], length)/8000)+0.5, 
ylim=c(0,100),
xlab="MoV for MP in village in 2009", ylab="Funds in thousand rs.", las=1, xaxt="n",
main="Allocated funds")
mgp.axis(1, at=c(1, 10, 20, 30, 40), labels=c("-100", "-50", "0", "50", "100"), mgp = c(3, 0.8, 0), las=1)
abline(v=20, lty=2)
text(y=90, x=1, paste("N PCs =", length(unique(DTA$UniquePC[complete.cases(DTA$cost_th)])), 
"\nN vill.=", length(DTA$cost_th[complete.cases(DTA$cost_th)])), pos=4)
points(x=39, y=5, pch=21, col="gray20", bg=alpha("gray80"), cex=(10000/8000)+0.5)
text(x=38, y=5, "10,000 obs.", pos=2)
points(x=39, y=15, pch=21, col="gray20", bg=alpha("gray80"), cex=(500/8000)+0.5)
text(x=38, y=15, "500 obs.", pos=2)

plot(100*tapply(DTA$anyproject_complete, DTA$villMoV_bins, mean), pch=21, col="gray20", bg=alpha("gray70", 0.6),
cex=(tapply(DTA$anyproject_complete[complete.cases(DTA$anyproject_complete)], DTA$villMoV_bins[complete.cases(DTA$anyproject_complete)], length)/8000)+0.5, 
ylim=c(0,20), xaxt="n",
xlab="MoV for MP in village in 2009", ylab="% with a completed project", las=1,
main="Completed project")
mgp.axis(1, at=c(1, 10, 20, 30, 40), labels=c("-100", "-50", "0", "50", "100"), mgp = c(3, 0.8, 0), las=1)
abline(v=20, lty=2)
text(y=18, x=1, paste("N PCs =", length(unique(DTA$UniquePC[complete.cases(DTA$anyproject_complete)])), 
"\nN vill.=", length(DTA$anyproject_complete[complete.cases(DTA$anyproject_complete)])), pos=4)
points(x=39, y=1, pch=21, col="gray20", bg=alpha("gray80"), cex=(10000/8000)+0.5)
text(x=38, y=1, "10,000 obs.", pos=2)
points(x=39, y=3, pch=21, col="gray20", bg=alpha("gray80"), cex=(500/8000)+0.5)
text(x=38, y=3, "500 obs.", pos=2)
dev.off()

#Robustness check using the logged version of funds

pdf("Figures/Fig_B1.pdf", width=8, height =3)
par(mfrow=c(1,3), mar=c(4.5,4, 2.5,.5))
plot(100*tapply(DTA$anyproject, DTA$villMoV_bins, mean), pch=21, col="gray20", bg=alpha("gray70", 0.6),
cex=(tapply(DTA$anyproject[complete.cases(DTA$anyproject)], DTA$villMoV_bins[complete.cases(DTA$anyproject)], length)/8000)+0.5, 
ylim=c(0,30), xaxt="n",
xlab="MoV for MP in village in 2009", ylab="% getting a project", las=1,
main="Any project")
mgp.axis(1, at=c(1, 10, 20, 30, 40), labels=c("-100", "-50", "0", "50", "100"), mgp = c(3, 0.8, 0), las=1)
abline(v=20, lty=2)
text(y=28, x=1, paste("N PCs =", length(unique(DTA$UniquePC[complete.cases(DTA$anyproject)])), 
"\nN vill.=", length(DTA$anyproject[complete.cases(DTA$anyproject)])), pos=4)
points(x=39, y=1, pch=21, col="gray20", bg=alpha("gray80"), cex=(10000/8000)+0.5)
text(x=38, y=1, "10,000 obs.", pos=2)
points(x=39, y=3, pch=21, col="gray20", bg=alpha("gray80"), cex=(500/8000)+0.5)
text(x=38, y=3, "500 obs.", pos=2)

plot(tapply(DTA$cost_log, DTA$villMoV_bins, mean),pch=21, col="gray20", bg=alpha("gray70", 0.6),
cex=(tapply(DTA$cost_log[complete.cases(DTA$cost_log)], DTA$villMoV_bins[complete.cases(DTA$cost_log)], length)/8000)+0.5, 
ylim=c(0,3),
xlab="MoV for MP in village in 2009", ylab="Funds in INR (logged)", las=1, xaxt="n",
main="Allocated funds")
mgp.axis(1, at=c(1, 10, 20, 30, 40), labels=c("-100", "-50", "0", "50", "100"), mgp = c(3, 0.8, 0), las=1)
abline(v=20, lty=2)
text(y=2.8, x=1, paste("N PCs =", length(unique(DTA$UniquePC[complete.cases(DTA$cost_log)])), 
"\nN vill.=", length(DTA$cost_log[complete.cases(DTA$cost_log)])), pos=4)
points(x=39, y=.1, pch=21, col="gray20", bg=alpha("gray80"), cex=(10000/8000)+0.5)
text(x=38, y=.1, "10,000 obs.", pos=2)
points(x=39, y=.3, pch=21, col="gray20", bg=alpha("gray80"), cex=(500/8000)+0.5)
text(x=38, y=.3, "500 obs.", pos=2)

plot(100*tapply(DTA$anyproject_complete, DTA$villMoV_bins, mean), pch=21, col="gray20", bg=alpha("gray70", 0.6),
cex=(tapply(DTA$anyproject_complete[complete.cases(DTA$anyproject_complete)], DTA$villMoV_bins[complete.cases(DTA$anyproject_complete)], length)/8000)+0.5, 
ylim=c(0,30), xaxt="n",
xlab="MoV for MP in village in 2009", ylab="% with a completed project", las=1,
main="Completed project")
mgp.axis(1, at=c(1, 10, 20, 30, 40), labels=c("-100", "-50", "0", "50", "100"), mgp = c(3, 0.8, 0), las=1)
abline(v=20, lty=2)
text(y=28, x=1, paste("N PCs =", length(unique(DTA$UniquePC[complete.cases(DTA$anyproject_complete)])), 
"\nN vill.=", length(DTA$anyproject_complete[complete.cases(DTA$anyproject_complete)])), pos=4)
points(x=39, y=1, pch=21, col="gray20", bg=alpha("gray80"), cex=(10000/8000)+0.5)
text(x=38, y=1, "10,000 obs.", pos=2)
points(x=39, y=3, pch=21, col="gray20", bg=alpha("gray80"), cex=(500/8000)+0.5)
text(x=38, y=3, "500 obs.", pos=2)
dev.off()

##Robustness check using vote-share

DTA$PS_PCwinner_percent_bins<-cut(DTA$PS_PCwinner_percent, breaks=seq(0, 100, 2.5), include.lowest=T)

pdf("Figures/Fig_B2.pdf", width=8, height =3)
par(mfrow=c(1,3), mar=c(4.5,4, 2.5,.5))
plot(100*tapply(DTA$anyproject, DTA$PS_PCwinner_percent_bins, mean), pch=21, col="gray20", bg=alpha("gray70", 0.6),
cex=(tapply(DTA$anyproject[complete.cases(DTA$anyproject)], DTA$PS_PCwinner_percent_bins[complete.cases(DTA$anyproject)], length)/8000)+0.5, 
ylim=c(0,30), xaxt="n",
xlab="Vote-share for MP in village in 2009", ylab="% getting a project", las=1,
main="Any project")
mgp.axis(1, at=c(1, 10, 20, 30, 40), labels=c("0", "25", "50", "75", "100"), mgp = c(3, 0.8, 0), las=1)
text(y=28, x=1, paste("N PCs =", length(unique(DTA$UniquePC[complete.cases(DTA$anyproject)])), 
"\nN vill.=", length(DTA$anyproject[complete.cases(DTA$anyproject)])), pos=4)
points(x=39, y=1, pch=21, col="gray20", bg=alpha("gray80"), cex=(10000/8000)+0.5)
text(x=38, y=1, "10,000 obs.", pos=2)
points(x=39, y=3, pch=21, col="gray20", bg=alpha("gray80"), cex=(500/8000)+0.5)
text(x=38, y=3, "500 obs.", pos=2)

plot(tapply(DTA$cost_th, DTA$PS_PCwinner_percent_bins, mean),pch=21, col="gray20", bg=alpha("gray70", 0.6),
cex=(tapply(DTA$cost_th[complete.cases(DTA$cost_th)], DTA$PS_PCwinner_percent_bins[complete.cases(DTA$cost_th)], length)/8000)+0.5, 
ylim=c(0,150),
xlab="Vote-share for MP in village in 2009", ylab="Funds in thousand rs.", las=1, xaxt="n",
main="Allocated funds")
mgp.axis(1, at=c(1, 10, 20, 30, 40), labels=c("0", "25", "50", "75", "100"), mgp = c(3, 0.8, 0), las=1)
text(y=140, x=1, paste("N PCs =", length(unique(DTA$UniquePC[complete.cases(DTA$cost_th)])), 
"\nN vill.=", length(DTA$cost_th[complete.cases(DTA$cost_th)])), pos=4)
points(x=39, y=5, pch=21, col="gray20", bg=alpha("gray80"), cex=(10000/8000)+0.5)
text(x=38, y=5, "10,000 obs.", pos=2)
points(x=39, y=15, pch=21, col="gray20", bg=alpha("gray80"), cex=(500/8000)+0.5)
text(x=38, y=15, "500 obs.", pos=2)

plot(100*tapply(DTA$anyproject_complete, DTA$PS_PCwinner_percent_bins, mean), pch=21, col="gray20", bg=alpha("gray70", 0.6),
cex=(tapply(DTA$anyproject_complete[complete.cases(DTA$anyproject_complete)], DTA$PS_PCwinner_percent_bins[complete.cases(DTA$anyproject_complete)], length)/8000)+0.5, 
ylim=c(0,30), xaxt="n",
xlab="Vote-share for MP in village in 2009", ylab="% getting a project", las=1,
main="Any project")
mgp.axis(1, at=c(1, 10, 20, 30, 40), labels=c("0", "25", "50", "75", "100"), mgp = c(3, 0.8, 0), las=1)
text(y=28, x=1, paste("N PCs =", length(unique(DTA$UniquePC[complete.cases(DTA$anyproject_complete)])), 
"\nN vill.=", length(DTA$anyproject_complete[complete.cases(DTA$anyproject_complete)])), pos=4)
points(x=39, y=1, pch=21, col="gray20", bg=alpha("gray80"), cex=(15000/8000)+0.5)
text(x=38, y=1, "10,000 obs.", pos=2)
points(x=39, y=3, pch=21, col="gray20", bg=alpha("gray80"), cex=(500/8000)+0.5)
text(x=38, y=3, "500 obs.", pos=2)
dev.off()


#################################################################################
############# MULTIVARIATE MODELS
#################################################################################

model1_OLS<-lm(anyproject ~ rescale(villMoV) +I(rescale(villMoV)^2) , data=DTA)
summary(model1_OLS)

model2_OLS<-lm(anyproject ~ rescale(villMoV) +I(rescale(villMoV)^2) + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST) +as.factor(UniquePC), data=DTA)
summary(model2_OLS)

model3_OLS<-lm(cost_log ~ rescale(villMoV) +I(rescale(villMoV)^2) , data=DTA)
summary(model3_OLS)

model4_OLS<-lm(cost_log ~ rescale(villMoV) +I(rescale(villMoV)^2) + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) +rescale(PCA11_prop_ST) +as.factor(UniquePC), data=DTA)
summary(model4_OLS)

model5_OLS<-lm(anyproject_complete ~ rescale(villMoV) +I(rescale(villMoV)^2) , data=DTA)
summary(model5_OLS)

model6_OLS<-lm(anyproject_complete ~ rescale(villMoV) +I(rescale(villMoV)^2) + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) +rescale(PCA11_prop_ST) +as.factor(UniquePC), data=DTA)
summary(model6_OLS)

###Creating clustered SEs
clusterSE<-function(model, data, cluster){
require(sandwich, quietly = TRUE)
require(lmtest, quietly = TRUE)
cluster<-as.factor(as.character(data[as.numeric(row.names(model.matrix(model))),cluster]))
M <- length(unique(cluster))
N <- length(cluster)
K <- model$rank
dfc <- (M/(M-1))*((N-1)/(N-K))
u.clust<-apply(estfun(model),2, function(x) tapply(x, cluster, sum))
cl.vcov<- dfc*sandwich(model, meat=crossprod(u.clust)/N)
print(coeftest(model, cl.vcov))
return(cl.vcov)
}

model1_OLS$se<-clusterSE(model1_OLS, DTA, "UniquePC")
model2_OLS$se<-clusterSE(model2_OLS, DTA, "UniquePC")
model3_OLS$se<-clusterSE(model3_OLS, DTA, "UniquePC")
model4_OLS$se<-clusterSE(model4_OLS, DTA, "UniquePC")
model5_OLS$se<-clusterSE(model5_OLS, DTA, "UniquePC")
model6_OLS$se<-clusterSE(model6_OLS, DTA, "UniquePC")

table1<-apsrtable(model1_OLS, model2_OLS, model3_OLS, model4_OLS, model5_OLS, model6_OLS, se="robust", digits=3, stars="default", omitcoef=c(8:238), 
coef.names=c("Intercept", "Village MoV", "Village MoV$^2$", "Village population", "Village literacy", "Proportion SCs", "Proportion STs"))
table1



#Robustness: Table B.1: Vote-share for MP
model1_OLS_voteshare<-lm(anyproject ~ rescale(PS_PCwinner_percent) +I(rescale(PS_PCwinner_percent)^2) , data=DTA)
summary(model1_OLS_voteshare)

model2_OLS_voteshare<-lm(anyproject ~ rescale(PS_PCwinner_percent) +I(rescale(PS_PCwinner_percent)^2) + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC)+rescale(PCA11_prop_ST) +as.factor(UniquePC), data=DTA)
summary(model2_OLS_voteshare)

model3_OLS_voteshare<-lm(cost_log ~ rescale(PS_PCwinner_percent) +I(rescale(PS_PCwinner_percent)^2) , data=DTA)
summary(model3_OLS_voteshare)

model4_OLS_voteshare<-lm(cost_log ~ rescale(PS_PCwinner_percent) +I(rescale(PS_PCwinner_percent)^2) + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) +rescale(PCA11_prop_ST)+as.factor(UniquePC), data=DTA)
summary(model4_OLS_voteshare)

model5_OLS_voteshare<-lm(anyproject_complete ~ rescale(PS_PCwinner_percent) +I(rescale(PS_PCwinner_percent)^2) , data=DTA)
summary(model5_OLS_voteshare)

model6_OLS_voteshare<-lm(anyproject_complete ~ rescale(PS_PCwinner_percent) +I(rescale(PS_PCwinner_percent)^2) + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) +rescale(PCA11_prop_ST)+as.factor(UniquePC), data=DTA)
summary(model6_OLS_voteshare)

###Creating clustered SEs
model1_OLS_voteshare$se<-clusterSE(model1_OLS_voteshare, DTA, "UniquePC")
model2_OLS_voteshare$se<-clusterSE(model2_OLS_voteshare, DTA, "UniquePC")
model3_OLS_voteshare$se<-clusterSE(model3_OLS_voteshare, DTA, "UniquePC")
model4_OLS_voteshare$se<-clusterSE(model4_OLS_voteshare, DTA, "UniquePC")
model5_OLS_voteshare$se<-clusterSE(model5_OLS_voteshare, DTA, "UniquePC")
model6_OLS_voteshare$se<-clusterSE(model6_OLS_voteshare, DTA, "UniquePC")

table1_robust1<-apsrtable(model1_OLS_voteshare, model2_OLS_voteshare, model3_OLS_voteshare, model4_OLS_voteshare, model5_OLS_voteshare, model6_OLS_voteshare, se="robust", digits=3, stars="default", omitcoef=c(8:238), 
coef.names=c("Intercept", "Vote-share for MP", "Vote-share for MP$^2$", "Village population", "Village literacy", "Proportion SCs", "Proportion STs"))
table1_robust1



##############################################################
###BY TYPES OF PARTY
##############################################################

#Correlation between the two measures of party embeddedness
tapply(DTA$P_member_caste_n, DTA$PCembedded, summary)
cor(DTA$P_member_caste_n, as.numeric(DTA$PCembedded))

##Qualitative party coding
theparties<-names(table(DTA$PCembedded))
thelabels<-paste(theparties, "party", sep=" ")
color<-c("gray70", "gray10")

pdf("Figures/Fig_3.pdf", width=8, height =3)
par(mfrow=c(1,3), mar=c(4.5,4,2.5,.5))

#Anyproject (multiplied by 100 to make it percent)
anyproject_NE<-spm(I(DTA$anyproject[DTA$PCembedded=="Non-embedded"])*100 ~ f(DTA$villMoV[DTA$PCembedded=="Non-embedded"]),omit.missing=T)
anyproject_E<-spm(I(DTA$anyproject[DTA$PCembedded=="Embedded"])*100 ~ f(DTA$villMoV[DTA$PCembedded=="Embedded"]),omit.missing=T)

plot.spm(anyproject_NE, ylim=c(0, 30), xlim=c(-100,100), ylab="% getting a project", xlab="Village MoV", main="Any project", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(v=0, lty=3)
par(new=TRUE)

plot.spm(anyproject_E, ylim=c(0, 30), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)
legend("topleft",  lty=c(1,1), lwd=c(2, 2), legend=thelabels, col=color, bg="white")


#Cost_log
cost_log_NE<-spm(DTA$cost_log[DTA$PCembedded=="Non-embedded"] ~ f(DTA$villMoV[DTA$PCembedded=="Non-embedded"]),omit.missing=T)
cost_log_E<-spm(DTA$cost_log[DTA$PCembedded=="Embedded"] ~ f(DTA$villMoV[DTA$PCembedded=="Embedded"]),omit.missing=T)

plot.spm(cost_log_NE, ylim=c(0, 3), xlim=c(-100,100), ylab="Funds in INR (logged)", xlab="Village MoV", main="Allocated funds", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(v=0, lty=3)
par(new=TRUE)

plot.spm(cost_log_E, ylim=c(0, 3), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)

#Completed project
anyproject_complete_NE<-spm(I(DTA$anyproject_complete[DTA$PCembedded=="Non-embedded"])*100 ~ f(DTA$villMoV[DTA$PCembedded=="Non-embedded"]),omit.missing=T)
anyproject_complete_E<-spm(I(DTA$anyproject_complete[DTA$PCembedded=="Embedded"])*100 ~ f(DTA$villMoV[DTA$PCembedded=="Embedded"]),omit.missing=T)

plot.spm(anyproject_complete_NE, ylim=c(0, 20), xlim=c(-100,100), ylab="% with a completed project", xlab="Village MoV", main="Completed project", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(v=0, lty=3)
par(new=TRUE)

plot.spm(anyproject_complete_E, ylim=c(0, 20), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)
dev.off()


####NES coding
DTA$P_member_caste_n_dummy <-as.factor(DTA$P_member_caste_n_dummy)
DTA$P_member_caste_n_dummy<-factor(DTA$P_member_caste_n_dummy, levels(DTA$P_member_caste_n_dummy)[c(2, 1)])

theparties<-names(table(DTA$P_member_caste_n_dummy))
thelabels<-paste(theparties, "party", sep=" ")

pdf("Figures/Fig_B3.pdf", width=8, height =3)
par(mfrow=c(1,3), mar=c(4.5,4,2.5,.5))

#Anyproject (multiplied by 100 to make it percent)
anyproject_NE<-spm(I(DTA$anyproject[DTA$P_member_caste_n_dummy=="Non-embedded"])*100 ~ f(DTA$villMoV[DTA$P_member_caste_n_dummy=="Non-embedded"]),omit.missing=T)
anyproject_E<-spm(I(DTA$anyproject[DTA$P_member_caste_n_dummy=="Embedded"])*100 ~ f(DTA$villMoV[DTA$P_member_caste_n_dummy=="Embedded"]),omit.missing=T)

plot.spm(anyproject_NE, ylim=c(0, 30), xlim=c(-100,100), ylab="% getting a project", xlab="Village MoV", main="Any project", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(v=0, lty=3)
par(new=TRUE)

plot.spm(anyproject_E, ylim=c(0, 30), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)
legend("topleft",  lty=c(1,1), lwd=c(2, 2), legend=thelabels, col=color, bg="white")


#Cost_log
cost_log_NE<-spm(DTA$cost_log[DTA$P_member_caste_n_dummy=="Non-embedded"] ~ f(DTA$villMoV[DTA$P_member_caste_n_dummy=="Non-embedded"]),omit.missing=T)
cost_log_E<-spm(DTA$cost_log[DTA$P_member_caste_n_dummy=="Embedded"] ~ f(DTA$villMoV[DTA$P_member_caste_n_dummy=="Embedded"]),omit.missing=T)

plot.spm(cost_log_NE, ylim=c(0, 3), xlim=c(-100,100), ylab="Funds in INR (logged)", xlab="Village MoV", main="Allocated funds", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(v=0, lty=3)
par(new=TRUE)

plot.spm(cost_log_E, ylim=c(0, 3), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)

#Completed project
anyproject_complete_NE<-spm(I(DTA$anyproject_complete[DTA$P_member_caste_n_dummy=="Non-embedded"])*100 ~ f(DTA$villMoV[DTA$P_member_caste_n_dummy=="Non-embedded"]),omit.missing=T)
anyproject_complete_E<-spm(I(DTA$anyproject_complete[DTA$P_member_caste_n_dummy=="Embedded"])*100 ~ f(DTA$villMoV[DTA$P_member_caste_n_dummy=="Embedded"]),omit.missing=T)

plot.spm(anyproject_complete_NE, ylim=c(0, 20), xlim=c(-100,100), ylab="% with a completed project", xlab="Village MoV", main="Completed project", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(v=0, lty=3)
par(new=TRUE)

plot.spm(anyproject_complete_E, ylim=c(0, 20), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)
dev.off()


#################################################################################
############# MULTIVARIATE MODELS BY PARTY 
#################################################################################


##Table 2
model_embedded1<-lmer(anyproject ~ rescale(villMoV)*PCembedded +I(rescale(villMoV)^2)* PCembedded + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded1)

model_embedded2<-lmer(anyproject ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded2)

model_embedded3<-lmer(cost_log ~ rescale(villMoV)*PCembedded +I(rescale(villMoV)^2)* PCembedded + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded3)

model_embedded4<-lmer(cost_log ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded4)

model_embedded5<-lmer(anyproject_complete ~ rescale(villMoV)*PCembedded +I(rescale(villMoV)^2)* PCembedded + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded5)

model_embedded6<-lmer(anyproject_complete ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded6)

table_party <- mtable(model_embedded1, model_embedded2 , model_embedded3, model_embedded4 , model_embedded5, model_embedded6, summary.stats=T, digits=3)
table_party
table_party <- relabel(table_party,
  "(Intercept)" = "Constant",
   "rescale(villMoV)" = "Village MoV",
   "I(rescale(villMoV)^2)"="Village MoV$^2$",
     "rescale(PCA11_POP)" = "Village population",
      "rescale(PCA11_prop_lit)" = "Village literacy",
      "rescale(PCA11_prop_SC)" = "Village SC percent",
      "rescale(PCA11_prop_ST)" = "Village ST percent",
"PCembeddedEmbedded"="Embedded party (qualitative)",
"P_member_caste_n"="Embedded party (NES)",
"rescale(villMoV) x PCembeddedEmbedded"="Village MoV*embedded (qual)",
"PCembeddedEmbedded x I(rescale(villMoV)^2)"="Village MoV$^2$*embedded (qual)",
"rescale(villMoV) x P_member_caste_n"="Village MoV*embedded (NES)",
"P_member_caste_n x I(rescale(villMoV)^2)"="Village MoV$^2$*embedded (NES)"
)      
toLatex(table_party) 

 
##Robustness with vote-share for the MP: Table B.2
model_embed_robust1<-lmer(anyproject ~ rescale(PS_PCwinner_percent)*PCembedded +I(rescale(PS_PCwinner_percent)^2)* PCembedded + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embed_robust1)

model_embed_robust2<-lmer(anyproject ~ rescale(PS_PCwinner_percent)* P_member_caste_n +I(rescale(PS_PCwinner_percent)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embed_robust2)

model_embed_robust3<-lmer(cost_log ~ rescale(PS_PCwinner_percent)*PCembedded +I(rescale(PS_PCwinner_percent)^2)* PCembedded + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embed_robust3)

model_embed_robust4<-lmer(cost_log ~ rescale(PS_PCwinner_percent)* P_member_caste_n +I(rescale(PS_PCwinner_percent)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embed_robust4)

model_embed_robust5<-lmer(anyproject_complete ~ rescale(PS_PCwinner_percent)*PCembedded +I(rescale(PS_PCwinner_percent)^2)* PCembedded + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embed_robust5)

model_embed_robust6<-lmer(anyproject_complete ~ rescale(PS_PCwinner_percent)* P_member_caste_n +I(rescale(PS_PCwinner_percent)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embed_robust6)

table_party <- mtable(model_embed_robust1, model_embed_robust2 , model_embed_robust3, model_embed_robust4 , model_embed_robust5, model_embed_robust6, summary.stats=T, digits=3)
table_party
table_party <- relabel(table_party,
  "(Intercept)" = "Constant",
   "rescale(PS_PCwinner_percent)" = "Vote-share for MP",
   "I(rescale(PS_PCwinner_percent)^2)"="Vote-share$^2$",
     "rescale(PCA11_POP)" = "Village population",
      "rescale(PCA11_prop_lit)" = "Village literacy",
      "rescale(PCA11_prop_SC)" = "Village SC percent",
      "rescale(PCA11_prop_ST)" = "Village ST percent",
"PCembeddedEmbedded"="Embedded party (qualitative)",
"P_member_caste_n"="Embedded party (NES)",
"rescale(PS_PCwinner_percent) x PCembeddedEmbedded"="Vote-share*embedded (qual)",
"PCembeddedEmbedded x I(rescale(PS_PCwinner_percent)^2)"="Vote-share$^2$*embedded (qual)",
"rescale(PS_PCwinner_percent) x P_member_caste_n"="Vote-share*embedded (NES)",
"P_member_caste_n x I(rescale(PS_PCwinner_percent)^2)"="Vote-share$^2$*embedded (NES)"
)      
toLatex(table_party) 


#### Robustness, excluding BJP and Congress Table B.3
table(DTA$PC_party)

model_embedded1<-lmer(anyproject ~ rescale(villMoV)*PCembedded +I(rescale(villMoV)^2)* PCembedded + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA[DTA$PC_party!="BJP" & DTA$PC_party!="INC",])
summary(model_embedded1)

model_embedded2<-lmer(anyproject ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA[DTA$PC_party!="BJP" & DTA$PC_party!="INC",])
summary(model_embedded2)

model_embedded3<-lmer(cost_log ~ rescale(villMoV)*PCembedded +I(rescale(villMoV)^2)* PCembedded + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA[DTA$PC_party!="BJP" & DTA$PC_party!="INC",])
summary(model_embedded3)

model_embedded4<-lmer(cost_log ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA[DTA$PC_party!="BJP" & DTA$PC_party!="INC",])
summary(model_embedded4)

model_embedded5<-lmer(anyproject_complete ~ rescale(villMoV)*PCembedded +I(rescale(villMoV)^2)* PCembedded + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA[DTA$PC_party!="BJP" & DTA$PC_party!="INC",])
summary(model_embedded5)

model_embedded6<-lmer(anyproject_complete ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA[DTA$PC_party!="BJP" & DTA$PC_party!="INC",])
summary(model_embedded6)

table_party <- mtable(model_embedded1, model_embedded2 , model_embedded3, model_embedded4 , model_embedded5, model_embedded6, summary.stats=T, digits=3)
table_party
table_party <- relabel(table_party,
  "(Intercept)" = "Constant",
   "rescale(villMoV)" = "Village MoV",
   "I(rescale(villMoV)^2)"="Village MoV$^2$",
     "rescale(PCA11_POP)" = "Village population",
      "rescale(PCA11_prop_lit)" = "Village literacy",
      "rescale(PCA11_prop_SC)" = "Village SC percent",
      "rescale(PCA11_prop_ST)" = "Village ST percent",
"PCembeddedEmbedded"="Embedded party (qualitative)",
"P_member_caste_n"="Embedded party (NES)",
"rescale(villMoV) x PCembeddedEmbedded"="Village MoV*embedded (qual)",
"PCembeddedEmbedded x I(rescale(villMoV)^2)"="Village MoV$^2$*embedded (qual)",
"rescale(villMoV) x P_member_caste_n"="Village MoV*embedded (NES)",
"P_member_caste_n x I(rescale(villMoV)^2)"="Village MoV$^2$*embedded (NES)"
)      
toLatex(table_party) 

#Robustness check, pattern by year, Figure B.4

table(DTA$PCembedded)

DTA$anyproject_2009<-ifelse(DTA$Nproject_2009>0, 1, 0)
DTA$anyproject_2010<-ifelse(DTA$Nproject_2010>0, 1, 0)
DTA$anyproject_2011<-ifelse(DTA$Nproject_2011>0, 1, 0)
DTA$anyproject_2012<-ifelse(DTA$Nproject_2012>0, 1, 0)
DTA$anyproject_2013<-ifelse(DTA$Nproject_2013>0, 1, 0)
DTA$anyproject_2014<-ifelse(DTA$Nproject_2014>0, 1, 0)

DTAembed<-DTA[DTA$PCembedded =="Embedded",]
dim(DTAembed)

DTAnonembed<-DTA[DTA$PCembedded =="Non-embedded",]
dim(DTAnonembed)

pdf("Figures/Fig_B4.pdf", width = 10, height = 6)
par(mfrow=c(2,3), mar=c(4,4,1,1))

se2009<-spm(DTAnonembed$anyproject_2009 ~ f(DTAnonembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

plot.spm(se2009, ylim=c(0, 0.07), xlim=c(-100,100),ylab="Any project 2009", xlab="Village MoV", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(h=1000, lty=3)

se2009<-spm(DTAembed$anyproject_2009 ~ f(DTAembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

par(new=TRUE)
plot.spm(se2009, ylim=c(0, 0.07), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)
legend("topleft",  lty=c(1,1), lwd=c(2, 2), legend=c("Non-embedded", "Embedded"), col=color, bg="white")

se2010<-spm(DTAnonembed$anyproject_2010 ~ f(DTAnonembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

plot.spm(se2010, ylim=c(0, 0.07), xlim=c(-100,100),ylab="Any project 2010", xlab="Village MoV", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(h=1000, lty=3)

se2010<-spm(DTAembed$anyproject_2010 ~ f(DTAembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

par(new=TRUE)
plot.spm(se2010, ylim=c(0, 0.07), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)
legend("topleft",  lty=c(1,1), lwd=c(2, 2), legend=c("Non-embedded", "Embedded"), col=color, bg="white")

se2011<-spm(DTAnonembed$anyproject_2011 ~ f(DTAnonembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

plot.spm(se2011, ylim=c(0, 0.07), xlim=c(-100,100),ylab="Any project 2011", xlab="Village MoV", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(h=1000, lty=3)

se2011<-spm(DTAembed$anyproject_2011 ~ f(DTAembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

par(new=TRUE)
plot.spm(se2011, ylim=c(0, 0.07), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)
legend("topleft",  lty=c(1,1), lwd=c(2, 2), legend=c("Non-embedded", "Embedded"), col=color, bg="white")

se2012<-spm(DTAnonembed$anyproject_2012 ~ f(DTAnonembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

plot.spm(se2012, ylim=c(0, 0.07), xlim=c(-100,100),ylab="Any project 2012", xlab="Village MoV", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(h=1000, lty=3)

se2012<-spm(DTAembed$anyproject_2012 ~ f(DTAembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

par(new=TRUE)
plot.spm(se2012, ylim=c(0, 0.07), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)
legend("topleft",  lty=c(1,1), lwd=c(2, 2), legend=c("Non-embedded", "Embedded"), col=color, bg="white")

se2013<-spm(DTAnonembed$anyproject_2013 ~ f(DTAnonembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

plot.spm(se2013, ylim=c(0, 0.07), xlim=c(-100,100),ylab="Any project 2013", xlab="Village MoV", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(h=1000, lty=3)

se2013<-spm(DTAembed$anyproject_2013 ~ f(DTAembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

par(new=TRUE)
plot.spm(se2013, ylim=c(0, 0.07), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)
legend("topleft",  lty=c(1,1), lwd=c(2, 2), legend=c("Non-embedded", "Embedded"), col=color, bg="white")

se2014<-spm(DTAnonembed$anyproject_2014 ~ f(DTAnonembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

plot.spm(se2014, ylim=c(0, 0.07), xlim=c(-100,100),ylab="Any project 2014", xlab="Village MoV", rug.col="transparent", col= color[1], shade.col=alpha(color[1], 0.3))
abline(h=1000, lty=3)

se2014<-spm(DTAembed$anyproject_2014 ~ f(DTAembed$villMoV),
family="gaussian",spar.method="REML",omit.missing=T)

par(new=TRUE)
plot.spm(se2014, ylim=c(0, 0.07), xlim=c(-100,100),ylab="", xlab="", rug.col="transparent", col= color[2], shade.col=alpha(color[2], 0.3))
abline(v=0, lty=3)
legend("topleft",  lty=c(1,1), lwd=c(2, 2), legend=c("Non-embedded", "Embedded"), col=color, bg="white")
dev.off()



#Main model run for each year in the data (NES coding), Table B.4
model_embedded_2009<-lmer(anyproject_2009 ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded_2009)

model_embedded_2010<-lmer(anyproject_2010 ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded_2010)

model_embedded_2011<-lmer(anyproject_2011 ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded_2011)

model_embedded_2012<-lmer(anyproject_2012 ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded_2012)

model_embedded_2013<-lmer(anyproject_2013 ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded_2013)

model_embedded_2014<-lmer(anyproject_2014 ~ rescale(villMoV)* P_member_caste_n +I(rescale(villMoV)^2)* P_member_caste_n + rescale(PCA11_POP)+ rescale(PCA11_prop_lit) + rescale(PCA11_prop_SC) + rescale(PCA11_prop_ST)+ (1| State_no2011) + (1| UniquePC), data=DTA)
summary(model_embedded_2014)

table_party <- mtable(model_embedded_2009, model_embedded_2010 , model_embedded_2011, model_embedded_2012 , model_embedded_2013, model_embedded_2014, summary.stats=T, digits=3)
table_party
table_party <- relabel(table_party,
  "(Intercept)" = "Constant",
   "rescale(villMoV)" = "Village MoV",
   "I(rescale(villMoV)^2)"="Village MoV$^2$",
     "rescale(PCA11_POP)" = "Village population",
      "rescale(PCA11_prop_lit)" = "Village literacy",
      "rescale(PCA11_prop_SC)" = "Village SC percent",
      "rescale(PCA11_prop_ST)" = "Village ST percent",
"PCembeddedEmbedded"="Embedded party (qualitative)",
"P_member_caste_n"="Embedded party (NES)",
"rescale(villMoV) x PCembeddedEmbedded"="Village MoV*embedded (qual)",
"PCembeddedEmbedded x I(rescale(villMoV)^2)"="Village MoV$^2$*embedded (qual)",
"rescale(villMoV) x P_member_caste_n"="Village MoV*embedded (NES)",
"P_member_caste_n x I(rescale(villMoV)^2)"="Village MoV$^2$*embedded (NES)"
)      
toLatex(table_party) 


#################################################################################################
### DELIMTITATION ANALYSIS BASED ON DATA FROM 2004-2009
#################################################################################################

load("Jensenius_Chhibber_2022_LS15.RData")
dim(vill)
names(vill)

#Set up outcome variables
vill$anyproject_2004<-ifelse(vill$Nproject_2004>0, 1, 0)
vill$anyproject_2005<-ifelse(vill$Nproject_2005>0, 1, 0)
vill$anyproject_2006<-ifelse(vill$Nproject_2006>0, 1, 0)
vill$anyproject_2007<-ifelse(vill$Nproject_2007>0, 1, 0)
vill$anyproject_2008<-ifelse(vill$Nproject_2008>0, 1, 0)
vill$anyproject_2009<-ifelse(vill$Nproject_2009>0, 1, 0)

vill$anyproject_middle<-ifelse((vill$anyproject_2005==1 | vill$anyproject_2006==1), 1, 0)
vill$anyproject_after<-ifelse((vill$anyproject_2007==1 | vill$anyproject_2008==1 | vill$anyproject_2009==1), 1, 0)

vill$anyproject_2004_complete<-ifelse(vill$Nproject_2004_complete>0, 1, 0)
vill$anyproject_2005_complete<-ifelse(vill$Nproject_2005_complete>0, 1, 0)
vill$anyproject_2006_complete<-ifelse(vill$Nproject_2006_complete>0, 1, 0)
vill$anyproject_2007_complete<-ifelse(vill$Nproject_2007_complete>0, 1, 0)
vill$anyproject_2008_complete<-ifelse(vill$Nproject_2008_complete>0, 1, 0)
vill$anyproject_2009_complete<-ifelse(vill$Nproject_2009_complete>0, 1, 0)

vill$anyproject_middle_complete<-ifelse((vill$anyproject_2005_complete==1 | vill$anyproject_2006_complete==1), 1, 0)
vill$anyproject_after_complete<-ifelse((vill$anyproject_2007_complete==1 | vill$anyproject_2008_complete==1 | vill$anyproject_2009_complete==1), 1, 0)

vill$cost_before_log<-log(vill$cost_2004+1)
vill$cost_middle_log<-log(vill$cost_2005+ vill$cost_2006+1)
vill$cost_after_log<-log(vill$cost_2007+ vill$cost_2008+ vill$cost_2009+1)


###Matching villages
#Tr 1 is changing PC  compared to staying in same PC that keeps same reservation status
#Matching 1 is changing PC compared to staying in same PC that keeps same reservation status, matched on location
#Matching 2 is changing PC  compared to staying in same PC that keeps same reservation status, villages matched on location, pop, propSC, proplit, and whether they got a project in 2004, caliper on all

vill<-vill[complete.cases(vill$PC_no01) & !is.na(vill$PC_no01),]

PCs<-unique(vill$PC_no01)
PCs<-PCs[complete.cases(PCs)]

for (h in c(1:length(PCs))){

vill_small<-vill[vill$PC_no01==PCs[h],]
vill_small<-vill_small[complete.cases(vill_small[,1]),]

##Identify which villages stay in same PC and which change 
vill_small$newPC<-names(which(tapply(vill_small$TOT_POP, vill_small$PC_no2008, sum)==max(tapply(vill_small$TOT_POP, vill_small$PC_no2008, sum))))
vill_small$newPCprop<-tapply(vill_small$TOT_POP, vill_small$PC_no2008, sum)[which(tapply(vill_small$TOT_POP, vill_small$PC_no2008, sum)==max(tapply(vill_small$TOT_POP, vill_small$PC_no2008, sum)))]/sum(tapply(vill_small$TOT_POP, vill_small$PC_no2008, sum))
vill_small$newPCtype<-vill_small$PC_type200[vill_small$PC_no2008==vill_small$newPC][1]
vill_small$PCchange_dummy<-ifelse(vill_small$PC_no2008==vill_small$newPC, 0, 1)

##Identify treatment group for Treatment 1
vill_small$Tr1<-ifelse(vill_small$PCchange_dummy==0  &  vill_small$PC_type01==vill_small$newPCtype, 0, NA)
vill_small$Tr1<-ifelse(vill_small$PCchange_dummy==1  & vill_small$PC_type01==vill_small$newPCtype, 1, vill_small$Tr1)

vill_small$matching1<-NA
vill_small$matching2<-NA

#Now identify matches based on Tr1
if(length(table(vill_small$Tr1))==2 & table(vill_small$Tr1)[2]>1){

matchdta<-vill_small[complete.cases(vill_small$Tr1),]
detach()
attach(matchdta)
dim(matchdta)
library(Matching)

X<-as.data.frame(cbind(coords.x1, coords.x2))
genout<-GenMatch(Tr=Tr1, X=X, BalanceMatrix=X, estimand="ATT", pop.size=300, max.generations=100, wait.generations=10, print.level=0, replace=FALSE, caliper=c(2, 2))
Matched_norep<-Match(Y=NULL, Tr=Tr1, X=X, estimand="ATT", Weight.matrix=genout, replace=FALSE, caliper=c(2, 2))

if(!is.na(Matched_norep[1])){
treated<-	matchdta$C_CODE01[Matched_norep$index.treated]
control<-	matchdta$C_CODE01[Matched_norep$index.control]
vill_small$matching1<-ifelse(vill_small$C_CODE01 %in% treated, 1, NA)
vill_small$matching1<-ifelse(vill_small$C_CODE01 %in% control, 0, vill_small$matching1)
} 

X2<-as.data.frame(cbind(coords.x1, coords.x2, TOT_POP, pca01_prop_SC, pca01_prop_lit, Nproject_2004))
genout<-GenMatch(Tr=Tr1, X=X2, BalanceMatrix=X2, estimand="ATT", pop.size=300, max.generations=100, wait.generations=10, print.level=0, replace=F, caliper=c(2), exact=F)
Matched_norep<-Match(Y=NULL, Tr=Tr1, X=X2, estimand="ATT", Weight.matrix=genout, replace=F, caliper=c(2), exact=F)

if(!is.na(Matched_norep[1])){
treated<-	matchdta$C_CODE01[Matched_norep$index.treated]
control<-	matchdta$C_CODE01[Matched_norep$index.control]
vill_small$matching2<-ifelse(vill_small$C_CODE01 %in% treated, 1, NA)
vill_small$matching2<-ifelse(vill_small$C_CODE01 %in% control, 0, vill_small$matching2)
} 
detach(matchdta)
rm(matchdta)
}
if (h==1){
	LS14vill<-vill_small
} else {
	LS14vill<-rbind(LS14vill, vill_small)
}
}

names(LS14vill)

#robust standard errors
clusterSE<-function(model, data, cluster, output){
require(sandwich, quietly = TRUE)
require(lmtest, quietly = TRUE)
cluster<-as.factor(as.character(data[,cluster]))
#cluster<-as.factor(as.character(data[as.numeric(row.names(model.matrix(model))),cluster]))
M <- length(unique(cluster))
N <- length(cluster)
K <- model$rank
dfc <- (M/(M-1))*((N-1)/(N-K))
u.clust<-apply(estfun(model),2, function(x) tapply(x, cluster, sum))
cl.vcov<- dfc*sandwich(model, meat=crossprod(u.clust)/N)
print(coeftest(model, cl.vcov))
if(output=="SEs"){
mySEs<-as.data.frame(cbind(sqrt(diag(vcov)), sqrt(diag(rob.vcov)), sqrt(diag(cl.vcov))))
names(mySEs)<-c("standard SEs", "robust SEs", "Clustered SEs")
print(mySEs)} else if (output=="cl.vcov"){
return(cl.vcov)} else {
coeftest(model, cl.vcov) 	
}
}


#Number of PCs
table(LS14vill$PCchange_dummy) 
length(unique(LS14vill$PC_no01))
length(unique(LS14vill$PC_no01[!is.na(LS14vill$Tr1) & LS14vill$newPCprop>=.75]))
length(unique(LS14vill$PC_no01[!is.na(LS14vill$matching1 )  & LS14vill$newPCprop>=.75]))
length(unique(LS14vill$PC_no01[!is.na(LS14vill$matching2) & LS14vill$newPCprop>=.75]))

table(LS14vill$Tr1) 
table(LS14vill$Tr1[LS14vill$newPCprop>=.5]) 
table(LS14vill$Tr1[LS14vill$newPCprop>=.75]) 

table(LS14vill$matching1[LS14vill$newPCprop>=.5])
table(LS14vill$matching2[ LS14vill$newPCprop>=.5])

table(LS14vill$Tr1[LS14vill$newPCprop>=.75])
table(LS14vill$matching2[ LS14vill$newPCprop>=.75])

##Table C.1
mytable<-matrix(nrow=3, ncol=2)
mytable[1,]<-table(LS14vill$Tr1[LS14vill$newPCprop>=.5])
mytable[2,]<-table(LS14vill$matching1[LS14vill$newPCprop>=.5])
mytable[3,]<-table(LS14vill$matching2[LS14vill$newPCprop>=.75])
 
colnames(mytable)<-c("Control","Treated")
row.names(mytable)<-c("Changed PC", "Matched on location (50%)",  "Matched with covariates (75%)")

library(xtable)
xtable(mytable)

##Figure C.1
balanceMat <-function(cov, data, my_names, Tr, matching1, matching2
                         ){                                    
# Calculate the number of covariates
    n <-length(cov)
	Tr<-data[,which(names(data)==Tr)]
	matching1<-data[,which(names(data)==matching1)]
	matching2<-data[,which(names(data)==matching2)]
# Construct the matrix of statistics and attach it to the covariate names
    z <- matrix(nrow=22, ncol=n)
    
    for (f in 1:n){
    mydta<-data[,which(names(data)==cov[f])]
    m1<-lm(mydta[complete.cases(Tr, mydta)]~Tr[complete.cases(Tr, mydta)])
    m2<-lm(mydta[complete.cases(matching1, mydta)]~matching1[complete.cases(matching1, mydta)])
    m3<-lm(mydta[complete.cases(matching2, mydta)]~matching2[complete.cases(matching2, mydta)])
    
     z[1:15,f]<-
     c(round(mean(mydta[Tr==1], na.rm=T),8),
     round(mean(mydta[Tr==0], na.rm=T),8),
     round(m1$coef[2],8),
     
     round(mean(mydta[matching1==1], na.rm=T),8),
     round(mean(mydta[matching1==0], na.rm=T),8),
     round(m2$coef[2],8),
     
     round(mean(mydta[matching2==1], na.rm=T),8),
     round(mean(mydta[matching2==0], na.rm=T),8),
     round(m3$coef[2],8),

    round(t.test(mydta[Tr==0], mydta[Tr==1])$p.value,3),
	round(t.test(mydta[matching1==0], mydta[matching1==1])$p.value,3),
	round(t.test(mydta[matching2==0], mydta[matching2==1])$p.value,3),
	
	round(clusterSE(m1, data[complete.cases(Tr, mydta),], "PC_name01", output="mytest")[[8]],3),
    round(clusterSE(m2, data[complete.cases(matching1, mydta),], "PC_name01", output="mytest")[[8]],3),
    round(clusterSE(m3, data[complete.cases(matching2, mydta),], "PC_name01", output="mytest")[[8]], 3)
	)
	
Cov <- clusterSE(m1, data[complete.cases(Tr, mydta),], "PC_name01", output="cl.vcov")
tt <-qt(c(0.025,0.975),summary(m1)$df[2])
se <- sqrt(diag(Cov))

z[16:17,f]<-(coef(m1) + se %o% tt)[2,]


Cov <- clusterSE(m2, data[complete.cases(matching1, mydta),], "PC_name01", output="cl.vcov")
tt <-qt(c(0.025,0.975),summary(m2)$df[2])
se <- sqrt(diag(Cov))

z[18:19,f]<-(coef(m2) + se %o% tt)[2,]

Cov <- clusterSE(m3, data[complete.cases(matching2, mydta),], "PC_name01", output="cl.vcov")
tt <-qt(c(0.025,0.975),summary(m2)$df[2])
se <- sqrt(diag(Cov))

z[20:21,f]<-(coef(m3) + se %o% tt)[2,]

z[22,f]<-c(round(mean(mydta, na.rm=T),8))
	}

    z<-t(z)
    row.names(z)<-(my_names)
      colnames(z)<- c("Mean Tr. BM",
                     "Mean Con. BM",
                     "BM diff.",
                     "Mean Tr. AM",
                     "Mean Con. AM",
                     "AM diff.",
                     "Mean Tr. AM2",
                     "Mean Con. AM2",
                     "AM2 diff.",
                     "BM t p-value",
                     "AM t p-value",
                      "AM2 t p-value",
                     "BM clustered p",
                     "AM clustered p",
                     "AM2 clustered p",
                     "BM CI1",
                     "BM CI2",
                     "AM CI1", 
                     "AM CI2",
                      "AM2 CI1", 
                     "AM2 CI2",
                     "Mean var.")
           
#return(mat)
return(z)  
  }
  
  
 
mybalance<-balanceMat(
cov=c("pc01_vd_paved", "pc01_vd_comm", "pc01_vd_transport", "pc01_vd_power_dom","pc01_vd_edu", "pc01_vd_water", "pca01_prop_W", "pca01_prop_SC", "pca01_prop_lit"),
data=LS14vill[LS14vill$newPCprop>=.5,],
my_names=c("Paved road (%)","Communication (%)", "Transportation (%)",  "Domestic electricity (%)", "Educational Facility (%)",  "Tap water (%)", "Workers (%)", "SCs (%)", "Literate (%)"), 
Tr="Tr1", matching1="matching1", matching2="matching2")

#Changing the proportions to percent
mybalance[1:6,c(1:9, 16:22)]<-100*mybalance[1:6,c(1:9, 16:22)]


pdf("Figures/Fig_C2.pdf", width=6.5, height=6.5)
par(mar = c(2, 8.5, 0.3, .5))
plot(x=NULL,axes=F, xlim=c(-25, 25), ylim=c(1,9),xlab="Differences", ylab="", cex=.8)
  # add the 0, 0.05 vertical lines
axis(side=1,tick=TRUE, las=1, cex.axis=.8)
axis(side=2,at=c(1:9), labels=row.names(mybalance), cex.axis=.8, las=1, tick=F)
abline(v=0, lty=2)
#Writing in state names

for (i in 1:nrow(mybalance)) {
arrows(x0= mybalance[i,16], x1= mybalance[i,17], y0=(i+.2), y1=(i+.2), angle=90, length=.025, code=3)
arrows(x0= mybalance[i,18], x1= mybalance[i,19], y0=(i), y1=(i), angle=90, length=.025, code=3)
arrows(x0= mybalance[i,20], x1= mybalance[i,21], y0=(i-.2), y1=(i-.2), angle=90, length=.025, code=3)

points(x=as.numeric(mybalance[i,3]), y=i+.2, pch=19, cex=.7)
points(x=as.numeric(mybalance[i,6]), y=i, pch=25, cex=.7,  bg="white")
points(x=as.numeric(mybalance[i,9]), y=i-.2, pch=4, cex=.7,  bg="white")

text(x=-20, y=i, paste("Mean=", round(mybalance[i,22],1), sep=""), cex=.8)

}
legend(x=4.5, y=8.5, pch=c(19, 25, 4, NA), lty=c(NA, NA, NA, 1), legend=c("Overall difference", "Matched on location", "Matched with covariates", "95% CI, clustered PC"), cex=.8, merge=T)
dev.off()



pdf("Figures/Fig_C3.pdf", width=9.1, height=4.5)
par(mfrow=c(1,2))
par(mar=c(3,2.5,3, 0))

N_PC<-c(length(unique(LS14vill$PC_no01[complete.cases(LS14vill$matching2) & LS14vill$newPCprop>=.5])),
length(unique(LS14vill$PC_no01[complete.cases(LS14vill$matching2) & LS14vill$newPCprop>=.75])))

LS14vill_s<-LS14vill[LS14vill$newPCprop>=.5,]

m1<-lm(LS14vill_s$anyproject_2004~as.factor(LS14vill_s$matching2)) 
m2<-lm(LS14vill_s$anyproject_middle~as.factor(LS14vill_s$matching2)) 
m3<-lm(LS14vill_s$anyproject_after~as.factor(LS14vill_s$matching2)) 

mydata<-round(100*c(
tapply(LS14vill_s$anyproject_2004, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$anyproject_middle, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$anyproject_after, as.factor(LS14vill_s$matching2), mean, na.rm=T)),1)

myP<-c(clusterSE(m1, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m2, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m3, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]])

myP<-ifelse(myP<0.01, "p<0.01", paste("p=", format(round(myP, 2), nsmall = 2), sep=""))
myP<-ifelse(is.na(myP), "1.00", myP)

myplot<-barplot(mydata,
yaxt="n", xaxt="n", space=c(0.5,0.2, 0.7, 0.2, 0.7, 0.2), col=c("gray60", "gray90"), ylim=c(0, 10), ylab="", main=paste("50% overlap (N PC=", N_PC[1], ")", sep=""))
abline(v= (myplot[2,]+myplot[3,])/2, lty=3, col="gray30")
abline(v= (myplot[4,]+myplot[5,])/2, lty=3, col="gray30")
text(x=(myplot[2,]+myplot[3,])/2, y=8, "First draft\n shared \nMarch 2005", pos=2)
text(x=(myplot[4,]+myplot[5,])/2, y=8, "Final delimitation\norder\nDecember 2006", pos=4)
mgp.axis(1, at=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, labels=c("2004", "2005-2006", "2007-2009"), mgp = c(3, 1.5, 0), tick=FALSE)
mtext(side=2, line=1.5, "Percentage of villages")
mgp.axis(2, mgp = c(3, 0.8, 0), las=1)
mgp.axis(1, at=myplot, labels=rep(c("Stayed", "Moved"), 3), mgp = c(3, 0.2, 0), tick=FALSE, cex.axis=0.85)
text(x=myplot, y=ifelse(mydata==0.3, .1, -(0.2)), paste(format(mydata, nsmall=1), "%", sep=""), xpd = TRUE, col = "black", pos=3, cex=0.9)
text(x=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, y=apply(rbind(mydata[c(1,3, 5)], mydata[c(2,4, 6)]), 2, max, na.rm = TRUE)+.1, pos=3, myP)


LS14vill_s<-LS14vill[LS14vill$newPCprop>=.75,]

m1<-lm(LS14vill_s$anyproject_2004~as.factor(LS14vill_s$matching2)) 
m2<-lm(LS14vill_s$anyproject_middle~as.factor(LS14vill_s$matching2)) 
m3<-lm(LS14vill_s$anyproject_after~as.factor(LS14vill_s$matching2)) 

mydata<-round(100*c(
tapply(LS14vill_s$anyproject_2004, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$anyproject_middle, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$anyproject_after, as.factor(LS14vill_s$matching2), mean, na.rm=T)),1)

myP<-c(clusterSE(m1, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m2, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m3, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]])

myP<-ifelse(myP<0.01, "p<0.01", paste("p=", format(round(myP, 2), nsmall = 2), sep=""))
myP<-ifelse(is.na(myP), "1.00", myP)

myplot<-barplot(mydata,
yaxt="n", xaxt="n", space=c(0.5,0.2, 0.7, 0.2, 0.7, 0.2), col=c("gray60", "gray90"), ylim=c(0, 10), ylab="", main=paste("75% overlap (N PC=", N_PC[2], ")", sep=""))
abline(v= (myplot[2,]+myplot[3,])/2, lty=3, col="gray30")
abline(v= (myplot[4,]+myplot[5,])/2, lty=3, col="gray30")
text(x=(myplot[2,]+myplot[3,])/2, y=8, "First draft\n shared \nMarch 2005", pos=2)
text(x=(myplot[4,]+myplot[5,])/2, y=8, "Final delimitation\norder\nDecember 2006", pos=4)
mgp.axis(1, at=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, labels=c("2004", "2005-2006", "2007-2009"), mgp = c(3, 1.5, 0), tick=FALSE)
#mtext(side=2, line=1.5, "Percentage of villages")
mgp.axis(2, mgp = c(3, 0.8, 0), las=1)
mgp.axis(1, at=myplot, labels=rep(c("Stayed", "Moved"), 3), mgp = c(3, 0.2, 0), tick=FALSE, cex.axis=0.85)
text(x=myplot, y=ifelse(mydata==0.3, .1, -(0.2)), paste(format(mydata, nsmall=1), "%", sep=""), xpd = TRUE, col = "black", pos=3, cex=0.9)
text(x=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, y=apply(rbind(mydata[c(1,3, 5)], mydata[c(2,4, 6)]), 2, max, na.rm = TRUE)+.1, pos=3, myP)
dev.off()

##ALLOCATED FUNDS 

pdf("Figures/Fig_C4.pdf", width=9.1, height=4.5)
par(mfrow=c(1,2))
par(mar=c(3,2.5,3, 0))

N_PC<-c(length(unique(LS14vill$PC_no01[complete.cases(LS14vill$matching2) & LS14vill$newPCprop>=.5])),
length(unique(LS14vill$PC_no01[complete.cases(LS14vill$matching2) & LS14vill$newPCprop>=.75])))

LS14vill_s<-LS14vill[LS14vill$newPCprop>=.5,]

m1<-lm(LS14vill_s$cost_before_log~as.factor(LS14vill_s$matching2)) 
m2<-lm(LS14vill_s$cost_middle_log~as.factor(LS14vill_s$matching2)) 
m3<-lm(LS14vill_s$cost_after_log~as.factor(LS14vill_s$matching2)) 

mydata<-round(c(
tapply(LS14vill_s$cost_before_log, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$cost_middle_log, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$cost_after_log, as.factor(LS14vill_s$matching2), mean, na.rm=T)),2)

myP<-c(clusterSE(m1, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m2, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m3, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]])

myP<-ifelse(myP<0.01, "p<0.01", paste("p=", format(round(myP, 2), nsmall = 2), sep=""))
myP<-ifelse(is.na(myP), "1.00", myP)

myplot<-barplot(mydata,
yaxt="n", xaxt="n", space=c(0.5,0.2, 0.7, 0.2, 0.7, 0.2), col=c("gray60", "gray90"), ylim=c(0, 1), ylab="", main=paste("50% overlap (N PC=", N_PC[1], ")", sep=""))
abline(v= (myplot[2,]+myplot[3,])/2, lty=3, col="gray30")
abline(v= (myplot[4,]+myplot[5,])/2, lty=3, col="gray30")
text(x=(myplot[2,]+myplot[3,])/2, y=.8, "First draft\n shared \nMarch 2005", pos=2)
text(x=(myplot[4,]+myplot[5,])/2, y=.8, "Final delimitation\norder\nDecember 2006", pos=4)
mgp.axis(1, at=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, labels=c("2004", "2005-2006", "2007-2009"), mgp = c(3, 1.5, 0), tick=FALSE)
mtext(side=2, line=1.6, "Funding in INR (logged)")
mgp.axis(2, mgp = c(3, 0.8, 0), las=1)
mgp.axis(1, at=myplot, labels=rep(c("Stayed", "Moved"), 3), mgp = c(3, 0.2, 0), tick=FALSE, cex.axis=0.85)
text(x=myplot, y=-.01, format(mydata, nsmall=1), xpd = TRUE, col = "black", pos=3, cex=0.9)
text(x=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, y=apply(rbind(mydata[c(1,3, 5)], mydata[c(2,4, 6)]), 2, max, na.rm = TRUE), pos=3, myP)

LS14vill_s<-LS14vill[LS14vill$newPCprop>=.75,]

m1<-lm(LS14vill_s$cost_before_log~as.factor(LS14vill_s$matching2)) 
m2<-lm(LS14vill_s$cost_middle_log~as.factor(LS14vill_s$matching2)) 
m3<-lm(LS14vill_s$cost_after_log~as.factor(LS14vill_s$matching2)) 

mydata<-round(c(
tapply(LS14vill_s$cost_before_log, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$cost_middle_log, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$cost_after_log, as.factor(LS14vill_s$matching2), mean, na.rm=T)),2)

myP<-c(clusterSE(m1, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m2, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m3, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]])

myP<-ifelse(myP<0.01, "p<0.01", paste("p=", format(round(myP, 2), nsmall = 2), sep=""))
myP<-ifelse(is.na(myP), "1.00", myP)

myplot<-barplot(mydata,
yaxt="n", xaxt="n", space=c(0.5,0.2, 0.7, 0.2, 0.7, 0.2), col=c("gray60", "gray90"), ylim=c(0, 1), ylab="", main=paste("75% overlap (N PC=", N_PC[2], ")", sep=""))
abline(v= (myplot[2,]+myplot[3,])/2, lty=3, col="gray30")
abline(v= (myplot[4,]+myplot[5,])/2, lty=3, col="gray30")
text(x=(myplot[2,]+myplot[3,])/2, y=.8, "First draft\n shared \nMarch 2005", pos=2)
text(x=(myplot[4,]+myplot[5,])/2, y=.8, "Final delimitation\norder\nDecember 2006", pos=4)
mgp.axis(1, at=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, labels=c("2004", "2005-2006", "2007-2009"), mgp = c(3, 1.5, 0), tick=FALSE)
#mtext(side=2, line=1.5, "Funding in INR (logged)")
mgp.axis(2, mgp = c(3, 0.8, 0), las=1)
mgp.axis(1, at=myplot, labels=rep(c("Stayed", "Moved"), 3), mgp = c(3, 0.2, 0), tick=FALSE, cex.axis=0.85)
text(x=myplot, y=-.01, format(mydata, nsmall=1), xpd = TRUE, col = "black", pos=3, cex=0.9)
text(x=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, y=apply(rbind(mydata[c(1,3, 5)], mydata[c(2,4, 6)]), 2, max, na.rm = TRUE), pos=3, myP)
dev.off()

pdf("Figures/Fig_2.pdf", width=9.1, height=4.5)
par(mfrow=c(1,2))
par(mar=c(3,2.5,3, 0))

N_PC<-c(length(unique(LS14vill$PC_no01[complete.cases(LS14vill$matching2) & LS14vill$newPCprop>=.5])),
length(unique(LS14vill$PC_no01[complete.cases(LS14vill$matching2) & LS14vill$newPCprop>=.75])))

LS14vill_s<-LS14vill[LS14vill$newPCprop>=.5,]

m1<-lm(LS14vill_s$anyproject_2004_complete~as.factor(LS14vill_s$matching2)) 
m2<-lm(LS14vill_s$anyproject_middle_complete~as.factor(LS14vill_s$matching2)) 
m3<-lm(LS14vill_s$anyproject_after_complete~as.factor(LS14vill_s$matching2)) 

mydata<-round(100*c(
tapply(LS14vill_s$anyproject_2004_complete, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$anyproject_middle_complete, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$anyproject_after_complete, as.factor(LS14vill_s$matching2), mean, na.rm=T)),1)

myP<-c(clusterSE(m1, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m2, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m3, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]])

myP<-ifelse(myP<0.01, "p<0.01", paste("p=", format(round(myP, 2), nsmall = 2), sep=""))
myP<-ifelse(is.na(myP), "1.00", myP)

myplot<-barplot(mydata,
yaxt="n", xaxt="n", space=c(0.5,0.2, 0.7, 0.2, 0.7, 0.2), col=c("gray60", "gray90"), ylim=c(0, 8), ylab="", main=paste("50% overlap (N PC=", N_PC[1], ")", sep=""))
abline(v= (myplot[2,]+myplot[3,])/2, lty=3, col="gray30")
abline(v= (myplot[4,]+myplot[5,])/2, lty=3, col="gray30")
text(x=(myplot[2,]+myplot[3,])/2, y=6, "First draft\n shared \nMarch 2005", pos=2)
text(x=(myplot[4,]+myplot[5,])/2, y=6, "Final delimitation\norder\nDecember 2006", pos=4)
mgp.axis(1, at=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, labels=c("2004", "2005-2006", "2007-2009"), mgp = c(3, 1.5, 0), tick=FALSE)
mtext(side=2, line=1.5, "Percentage of villages")
mgp.axis(2, mgp = c(3, 0.8, 0), las=1)
mgp.axis(1, at=myplot, labels=rep(c("Stayed", "Moved"), 3), mgp = c(3, 0.2, 0), tick=FALSE, cex.axis=0.85)
text(x=myplot, y=.15, paste(format(mydata, nsmall=1), "%", sep=""), xpd = TRUE, col = "black", pos=3, cex=0.9)
text(x=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, y=apply(rbind(mydata[c(1,3, 5)], mydata[c(2,4, 6)]), 2, max, na.rm = TRUE)+.3, pos=3, myP)


LS14vill_s<-LS14vill[LS14vill$newPCprop>=.75,]

m1<-lm(LS14vill_s$anyproject_2004_complete~as.factor(LS14vill_s$matching2)) 
m2<-lm(LS14vill_s$anyproject_middle_complete~as.factor(LS14vill_s$matching2)) 
m3<-lm(LS14vill_s$anyproject_after_complete~as.factor(LS14vill_s$matching2)) 

mydata<-round(100*c(
tapply(LS14vill_s$anyproject_2004_complete, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$anyproject_middle_complete, as.factor(LS14vill_s$matching2), mean, na.rm=T),
tapply(LS14vill_s$anyproject_after_complete, as.factor(LS14vill_s$matching2), mean, na.rm=T)),1)

myP<-c(clusterSE(m1, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m2, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]],
clusterSE(m3, LS14vill_s[complete.cases(LS14vill_s$matching2),], "PC_no01", output="mytest")[[8]])

myP<-ifelse(myP<0.01, "p<0.01", paste("p=", format(round(myP, 2), nsmall = 2), sep=""))
myP<-ifelse(is.na(myP), "1.00", myP)

myplot<-barplot(mydata,
yaxt="n", xaxt="n", space=c(0.5,0.2, 0.7, 0.2, 0.7, 0.2), col=c("gray60", "gray90"), ylim=c(0, 8), ylab="", main=paste("75% overlap (N PC=", N_PC[2], ")", sep=""))
abline(v= (myplot[2,]+myplot[3,])/2, lty=3, col="gray30")
abline(v= (myplot[4,]+myplot[5,])/2, lty=3, col="gray30")
text(x=(myplot[2,]+myplot[3,])/2, y=6, "First draft\n shared \nMarch 2005", pos=2)
text(x=(myplot[4,]+myplot[5,])/2, y=6, "Final delimitation\norder\nDecember 2006", pos=4)
mgp.axis(1, at=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, labels=c("2004", "2005-2006", "2007-2009"), mgp = c(3, 1.5, 0), tick=FALSE)
#mtext(side=2, line=1.5, "Percentage of villages")
mgp.axis(2, mgp = c(3, 0.8, 0), las=1)
mgp.axis(1, at=myplot, labels=rep(c("Stayed", "Moved"), 3), mgp = c(3, 0.2, 0), tick=FALSE, cex.axis=0.85)
text(x=myplot, y=.15, paste(format(mydata, nsmall=1), "%", sep=""), xpd = TRUE, col = "black", pos=3, cex=0.9)
text(x=(myplot[c(1,3, 5),]+ myplot[c(2,4, 6),])/2, y=apply(rbind(mydata[c(1,3, 5)], mydata[c(2,4, 6)]), 2, max, na.rm = TRUE)+.3, pos=3, myP)
dev.off()



