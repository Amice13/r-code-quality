# Replication code for Public and Elite Opinion on International Human Rights Law

# Set working directory accordingly
setwd(". . .")

# Load datasets
dfusa <- read.csv("datapublic.csv") 
dfe <- read.csv("dataelite.csv")

# Set path for figures accordingly
figurepath <- ". . ."

# Set number of decimals for output
options(scipen=4)

# Load packages
library(xtable)
library(msm)
library(stats)
library(mvtnorm)
library(car)
library(MASS)
library(stargazer)
library(plyr)
library(Zelig)
library(mvtnorm)
library(graphics)
library(mediation)
library(stm)
library(tm)
library(SnowballC) 
library(ResourceSelection)
library(boot)
library(faraway)
library(nnet)
library(MASS)
library(aod)
library(effects)

# Note that the figures and the tables are not reproduced in the order of appearance.
# Instead, all figures and tables are noted by their appropriately numbered headings.

# The results were created using this version of R
sessionInfo() 

#> sessionInfo()
#R version 3.1.2 (2014-10-31)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#
#locale:
#  [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
#[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
#[5] LC_TIME=English_United States.1252    
#
#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     
#
#loaded via a namespace (and not attached):
#  [1] tools_3.1.2



##############
## FIGURE 1 ##
##############
a <- rep(NA, 3)
lower <- rep(NA, 3)
upper <- rep(NA, 3)
for (i in (1:3)){
  a[i] <- mean(dfusa$t3[dfusa$w3==i], na.rm=T)
  s <- sd(dfusa$t3[dfusa$w3==i], na.rm=T)
  n <- sum(dfusa$w3==i, na.rm=T)
  error <- qt(0.975,df=n-1)*s/sqrt(n)
  lower[i] <- a[i]-error
  upper[i] <- a[i]+error
}
dfusae1 <- matrix( c("Control", "Low obligation","High obligation"),
                   nrow=3, ncol=2) 
dfusae1 <- data.frame(dfusae1)
dfusae1$X2 <- c(a)
dfusae1$lower <- c(lower)
dfusae1$upper <- c(upper)
dfusae1$color <- "blue"
holder <- NA
pointestimate <- NA
lower <- NA
upper <- NA
pointestimate <- c(dfusae1$X2)
lower <- c(dfusae1$lower) 
upper <- c(dfusae1$upper) 
holder<- as.data.frame(pointestimate)
holder$groups <- c("Control","Low obligation",
                   "High obligation")
holder$groups <- factor(holder$groups, c("Control","Low obligation",
                                         "High obligation"))
holder$label <- c("U.S. Public","U.S. Public","U.S. Public")
holder$color[holder$label=="U.S. Public"] <- "blue"

#pdf(file=file.path(figurepath, "f1.pdf"),w=10,h=7)
par(mfrow = c(1, 1), mar = c(1,1,1,1), oma=c(3,1,1,1))
par(lheight=.6)
dotchart(holder$pointestimate[c(1:3)],
         group=holder$groups,
         xlab="Oppose compliance (0) ... Favor compliance (1)",
         pch=15,cex=1.5,
         xlim=c(0.4, 0.8))
arrows(lower[c(1:3)], c(7,4,1), upper[c(1:3)], c(7,4,1),
       lwd=2,length=0.05, angle=90, code=3,
)
abline(v=0,lty=2)
#dev.off()

##############
## FIGURE 2 ##
##############
a <- rep(NA, 3)
lower <- rep(NA, 3)
upper <- rep(NA, 3)
for (i in (1:3)){
  a[i] <- mean(dfe$t1[dfe$w1==i], na.rm=T)
  s <- sd(dfe$t1[dfe$w1==i], na.rm=T)
  n <- sum(dfe$w1==i, na.rm=T)
  error <- qt(0.975,df=n-1)*s/sqrt(n)
  lower[i] <- a[i]-error
  upper[i] <- a[i]+error
}
dfe1 <- matrix( c("Control", "High obligation",
                  "High obligation \n\n + public support\n"),
                   nrow=3, ncol=2) 
dfe1 <- data.frame(dfe1)
dfe1$X2 <- c(a)
dfe1$lower <- c(lower)
dfe1$upper <- c(upper)
dfe1$color <- "red"
holder <- NA
pointestimate <- NA
lower <- NA
upper <- NA
pointestimate <- c(dfe1$X2)
lower <- c(dfe1$lower) 
upper <- c(dfe1$upper) 
holder<- as.data.frame(pointestimate)
holder$groups <- c("Control", "High obligation",
                   "High obligation \n\n + public support\n")
holder$groups <- factor(holder$groups, c("Control", "High obligation",
                                         "High obligation \n\n + public support\n"))
holder$label <- c("U.S. Elite","U.S. Elite","U.S. Elite")
holder$color[holder$label=="U.S. Elite"] <- "red"

#pdf(file=file.path(figurepath, "f2.pdf"),w=10,h=7)
par(mfrow = c(1, 1), mar = c(1,1,1,1), oma=c(3,1,1,1))
par(lheight=.6)
dotchart(holder$pointestimate[c(1:3)],
         group=holder$groups,
         xlab="Oppose compliance (0) ... Favor compliance (1)",
         pch=17,cex=1.5,
         xlim=c(0.3, 0.95))
arrows(lower[c(1:3)], c(7,4,1), upper[c(1:3)], c(7,4,1),
       lwd=2,length=0.05, angle=90, code=3,
)
abline(v=0,lty=2)
#dev.off()

######################
## APPENDIX TABLE 5 ##
######################
m3<-lm(t3 ~ norm3 + il3  + age + sex + education + employed+ 
         income + as.factor(race) + pid+ know + involve  + ci + mi+ iso  , dfusa)
m3a<-lm(t3 ~ norm3 + il3 , dfusa)
summary(m3a)
stargazer(m3a,m3,no.space=T,table.placement = "H")

######################
## APPENDIX TABLE 6 ##
######################
m3e<-lm(t1 ~ as.factor(w1) , dfe[dfe$w1!=1,])
summary(m3e)
m3e<-lm(t1 ~ as.factor(w1) + age + sex + education + 
         as.factor(race) + pid + ci + mi+ iso + 
         state +senate + experience , dfe)
summary(m3e)
m3ae<-lm(t1 ~ as.factor(w1)  , dfe)
summary(m3ae)
stargazer(m3ae,m3e,no.space=T,table.placement = "H")

###############
### TABLE 1 ###
###############
holdermat <- matrix(c(rep(NA, 25)), nrow=5, ncol=5) 
holdermat[2,2] <-  m3a$coeff[1] 
holdermat[3,2] <-  m3a$coeff[1]+m3a$coeff[2] 
holdermat[4,2] <-  m3a$coeff[1]+m3a$coeff[3] 
holdermat[3,3] <- holdermat[3,2]-holdermat[2,2]
holdermat[4,3] <- holdermat[4,2]-holdermat[3,2]
holdermat[2,4] <- m3ae$coeff[1] 
holdermat[4,4] <-  m3ae$coeff[1]+m3ae$coeff[2] 
holdermat[5,4] <-  m3ae$coeff[1]+m3ae$coeff[3] 
holdermat[4,5] <- holdermat[4,4]-holdermat[2,4]
holdermat[5,5] <- holdermat[5,4]-holdermat[4,4]
holdermat<- round(100*holdermat,2)
holdermat[1,1:5] <- c(NA,"U.S. Public", "Difference","U.S. Elite","Difference")
holdermat[1:4,1] <- c(NA,"No information", "Low obligation","High obligation")
print(xtable(holdermat),include.rownames=F,include.colnames=F)

##############
## FIGURE 3 ##
##############
peus1 <- m3a$coeff[2]
moeus1<-sqrt(diag(vcov(m3a)))[2]*1.96
peus2 <- m3a$coeff[3]
moeus2<-sqrt(diag(vcov(m3a)))[3]*1.96
dfusae1 <- matrix( c("Low obligation","High obligation"),
                   nrow=2, ncol=2) 
dfusae1 <- data.frame(dfusae1)
dfusae1$X2 <- c(peus1, peus2 )
dfusae1$lower <- c(peus1-moeus1, peus2-moeus2)
dfusae1$upper <- c(peus1+moeus1, peus2+moeus2)
dfusae1$color <- "blue"
peus1 <- m3e$coeff[2]
moeus1<-sqrt(diag(vcov(m3e)))[2]*qt(.975,70)
peus2 <- m3e$coeff[3]
moeus2<-sqrt(diag(vcov(m3e)))[3]*qt(.975,70)
dfee1 <- matrix( c("Low obligation","High obligation", "High obligation\n\n + public support"),
                 nrow=3, ncol=2) 
dfee1 <- data.frame(dfee1)
dfee1$X2 <- c(NA, peus1, peus2)
dfee1$lower <- c(NA, peus1-moeus1, peus2-moeus2)
dfee1$upper <- c(NA, peus1+moeus1, peus2+moeus2)
dfee1$color <- "red"
holder <- NA
pointestimate <- NA
lower <- NA
upper <- NA
pointestimate[c(5,3,1)] <- c(dfusae1$X2,NA)
lower[c(5,3,1)] <- c(dfusae1$lower,NA) 
upper[c(5,3,1)] <- c(dfusae1$upper,NA) 
pointestimate[c(6,4,2)] <- dfee1$X2
lower[c(6,4,2)] <- dfee1$lower
upper[c(6,4,2)] <- dfee1$upper
holder<- as.data.frame(pointestimate)
holder$groups <- c("High obligation \n\n + public support\n","High obligation \n\n + public support\n",
                   "High obligation","High obligation","Low obligation","Low obligation"                 
                   )
holder$groups <- factor(holder$groups, c("Low obligation","High obligation",
                                         "High obligation \n\n + public support\n"                                         
                                         ))
holder$label <- c("Public","Elite","Public","Elite","Public","Elite")
holder$color[holder$label=="Elite"] <- "red"
holder$color[holder$label=="Public"] <- "blue"
holder$label <- c("Elite"," ","Elite","Public"," ","Public")
holder$color[holder$label=="Elite"] <- "red"

#pdf(file=file.path(figurepath, "f3.pdf"),w=10,h=7)
par(mfrow = c(1, 1), mar = c(1,1,1,1), oma=c(3,1,1,1))
par(lheight=.6)
dotchart(holder$pointestimate[c(2,1,4,3,6,5)],
         group=holder$groups,
         xlab="Oppose compliance (0) ... Favor compliance (1)",
         xlim=c(-.05,0.8),labels=holder$label,
         pch = c(17,NA,17,15,NA,15), cex=1.5)
arrows(lower[c(1,3,5)], c(2,6,10), upper[c(1,3,5)], c(2,6,10),
       lwd=2,length=0.05, angle=90, code=3,
)
arrows(lower[c(2,4,6)], c(1,5,9), upper[c(2,4,6)], c(1,5,9),
       lwd=2,length=0.05, angle=90, code=3, 
)
#dev.off()

##############
## TABLE 10 ##
##############
dfe$black<-0
dfe$white<-0
dfe$asian<-0
dfe$hispanic<-0
dfe$black[dfe$race=="black"]<-1
dfe$white[dfe$race=="white"]<-1
dfe$asian[dfe$race=="asian"]<-1
dfe$hispanic[dfe$race=="hispanic"]<-1
mn1 <- multinom(t1text ~ w1 + age + sex + education + 
                  black + asian + hispanic + pid + ci + mi+ iso + 
                  state +senate + experience, 
                data=dfe[dfe$t1text!="Other",])
summary(mn1)
stargazer(mn1,no.space=T,table.placement = "H")
mndf <- data.frame(w1=c(0,1,2), 
                   age=median(dfe$age,na.rm=T),
                   sex= median(dfe$sex,na.rm=T), 
                   education= median(dfe$education,na.rm=T),
                   black=0,
                   hispanic=0,
                   asian=0, 
                   pid=median(dfe$pid,na.rm=T),
                   ci= median(dfe$ci,na.rm=T),
                   mi= median(dfe$mi,na.rm=T),
                   iso=  median(dfe$iso,na.rm=T),
                   state= median(dfe$state),
                   senate= median(dfe$senate),
                   experience=mean(dfe$experience)
)
multi <- predict(mn1, newdata=mndf,type="probs", se.fit=TRUE)

######################
## APPENDIX TABLE 9 ##
######################
mn1 <- multinom(t1text ~ w1 , 
                data=dfe[dfe$t1text!="Other",])
summary(mn1)
stargazer(mn1,no.space=T,table.placement = "H")

##############
## FIGURE 4 ##
##############
m <- mn1
fit.eff <- Effect("w1", m)
results<-data.frame(fit.eff$model.matrix, fit.eff$prob, fit.eff$lower.prob, 
                    fit.eff$upper.prob)[c(1,3,5),]

#pdf(file=file.path(figurepath, "f4.pdf"),w=11,h=11)
par(mfrow = c(2,2), mar = c(14,2,2,7), oma=c(1,1,1,1),
    mai = c(1, 1,0.3, 0.1)
    )
par(lheight=.6)
plot(c(1,2,3), results[,3], col="black",
     ylim=c(0,1), type="o",pch=16,lwd=2,
     xlim=c(0.9,3.14),
     main="Information",
     xlab=" ",
     xaxt = "n",
     ylab="Proportion of Mechanism")
arrows(c(1,2,3), results[,7], c(1,2,3),results[,11], 
       lwd=2,length=0.05, angle=90, code=3,
       col="black")
axis(1, at=c(1,2,2.97), 
     labels=c("Control","High obligation"," High oblig.+public support"))
plot(c(1,2,3), results[,4], col="black",
     ylim=c(0,1), type="o",pch=16,lwd=2,
     xlim=c(0.9,3.19),main="Market",
     xlab=" ",
     xaxt = "n",
     ylab="Proportion of Mechanism")
arrows(c(1,2,3), results[,8], c(1,2,3), results[,12], 
       lwd=2,length=0.05, angle=90, code=3,
       col="black")
axis(1, at=c(1,2,2.97), 
     labels=c("Control","High obligation"," High oblig.+public support"))
plot(c(1,2,3), results[,5], col="black",
     ylim=c(0,1), type="o",pch=16,lwd=2,
     xlim=c(0.9,3.14),main="Morality",
     xlab=" ",
     xaxt = "n",
     ylab="Proportion of Mechanism")
arrows(c(1,2,3), results[,9], c(1,2,3),results[,13], 
       lwd=2,length=0.05, angle=90, code=3,
       col="black")
axis(1, at=c(1,2,2.97), 
     labels=c("Control","High obligation"," High oblig.+public support"))
plot(c(1,2,3), results[,6], col="black",
     ylim=c(0,1), type="o",pch=16,lwd=2,
     xlim=c(0.9,3.19),main="Reputation",
     xlab=" ",
     xaxt = "n",
     ylab="Proportion of Mechanism")
arrows(c(1,2,3), results[,10], c(1,2,3),results[,14], 
       lwd=2,length=0.05, angle=90, code=3,
       col="black")
axis(1, at=c(1,2,2.97), 
     labels=c("Control","High obligation"," High oblig.+public support"))
#dev.off()

######################
## APPENDIX TABLE 1 ##
######################
df <- read.csv("datapublic.csv") 
mean(df$sex)
n<-length(df$sex)
ha<- sum(df$age<=29)/n
hb<- sum(df$age<=44)/n-ha
hc<- sum(df$age<=64)/n-hb-ha
hd<- sum(df$age<=100)/n-hb-ha-hc
c(ha,hb,hc,hd)
sum(df$race=="White")/n
sum(df$race=="Hispanic/Latino")/n
sum(df$race=="African American")/n
sum(df$education==0 | df$education==1)/n
sum(df$education==2)/n
sum(df$education==3 | df$education==4)/n
sum(df$education==5 | df$education==6)/n
sum(df$pid==0 | df$pid==1 | df$pid==2)/n
sum(df$pid==3)/n
sum(df$pid==4 | df$pid==5 | df$pid==6)/n

######################
## APPENDIX TABLE 3 ##
######################
ha<-NA
hb<-NA
hc<-NA
hd<-NA
for(i in 1:3){
  ha[i]<-round(sum(df[df$w3==i,]$sex==1, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
  hb[i]<-1-ha[i]  
}
ha
hb
for(i in 1:3){
  ha[i]<- round(sum(df[df$w3==i,]$age<=29, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
  hb[i]<- round(sum(df[df$w3==i,]$age<=44, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex))-ha[i],3)
  hc[i]<- round(sum(df[df$w3==i,]$age<=64, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex))-hb[i]-ha[i],3)
  hd[i]<- round(sum(df[df$w3==i,]$age<=100, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex))-hb[i]-ha[i]-hc[i],3)
}
ha
hb
hc
hd
for(i in 1:3){
  ha[i]<-round(sum(df[df$w3==i,]$education==0 | df[df$w3==i,]$education==1, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
  hb[i]<-round(sum(df[df$w3==i,]$education==2, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
  hc[i]<-round(sum(df[df$w3==i,]$education==3 | df[df$w3==i,]$education==4, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
  hd[i]<-round(sum(df[df$w3==i,]$education==5 | df[df$w3==i,]$education==6, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
}
ha
hb
hc
hd
for(i in 1:3){
  ha[i]<-round(sum(df[df$w3==i,]$race=="White", na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
  hb[i]<-round(sum(df[df$w3==i,]$race=="Hispanic/Latino", na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
  hc[i]<-round(sum(df[df$w3==i,]$race=="African American", na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
}
ha
hb
hc
for(i in 1:3){
  ha[i]<-round(sum(df[df$w3==i,]$pid==0 | df[df$w3==i,]$pid==1 | df[df$w3==i,]$pid==2, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
  hb[i]<-round(sum(df[df$w3==i,]$pid==3, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
  hc[i]<-round(sum(df[df$w3==i,]$pid==4 | df[df$w3==i,]$pid==5 | df[df$w3==i,]$pid==6, na.rm=T)/sum(!is.na(df[df$w3==i,]$sex)),3)
}
ha
hb
hc

######################
## APPENDIX TABLE 2 ##
######################
df <- read.csv("dataelite.csv")
mean(df$sex,na.rm=T)
n<-length(which(!is.na(df$w1)))
ha<- sum(df$age<=29,na.rm=T)/n
hb<- sum(df$age<=44,na.rm=T)/n-ha
hc<- sum(df$age<=64,na.rm=T)/n-hb-ha
hd<- sum(df$age<=100,na.rm=T)/n-hb-hc-ha
c(ha,hb,hc,hd)
sum(df$education==0,na.rm=T)/n
sum(df$education==1,na.rm=T)/n
sum(df$education==2,na.rm=T)/n
sum(df$education==3,na.rm=T)/n
sum(df$race=="white")/n
sum(df$race=="hispanic")/n
sum(df$race=="black")/n
table(df$pid)

######################
## APPENDIX TABLE 4 ##
######################
ha<-NA
hb<-NA
hc<-NA
hd<-NA
for(i in 1:3){
  ha[i]<-round(sum(df[df$w1==i,]$sex==1)/length(df[df$w1==i,]$sex),3)
  hb[i]<-1-ha[i]  
}
ha
hb
for(i in 1:3){
  ha[i]<- round(sum(df[df$w1==i,]$age<=29)/length(df[df$w1==i,]$sex),3)
  hb[i]<- round(sum(df[df$w1==i,]$age<=44)/length(df[df$w1==i,]$sex)-ha[i],3)
  hc[i]<- round(sum(df[df$w1==i,]$age<=64)/length(df[df$w1==i,]$sex)-ha[i]-hb[i],3)
  hd[i]<- round(sum(df[df$w1==i,]$age<=100)/length(df[df$w1==i,]$sex)-hc[i]-hb[i]-ha[i],3)
}
ha
hb
hc
hd
for(i in 1:3){
  ha[i]<-round(sum(df[df$w1==i,]$education==0)/length(df[df$w1==i,]$sex),3)
  hb[i]<-round(sum(df[df$w1==i,]$education==1)/length(df[df$w1==i,]$sex),3)
  hc[i]<-round(sum(df[df$w1==i,]$education==2)/length(df[df$w1==i,]$sex),3)
  hd[i]<-round(sum(df[df$w1==i,]$education==3)/length(df[df$w1==i,]$sex),3)
}
ha
hb
hc
hd
for(i in 1:3){
  ha[i]<-round(sum(df[df$w1==i,]$race=="white")/length(df[df$w1==i,]$sex),3)
  hb[i]<-round(sum(df[df$w1==i,]$race=="hispanic")/length(df[df$w1==i,]$sex),3)
  hc[i]<-round(sum(df[df$w1==i,]$race=="black")/length(df[df$w1==i,]$sex),3)
}
ha
hb
hc
for(i in 1:3){
  ha[i]<-round(sum(df[df$w1==i,]$pid==0)/length(df[df$w1==i,]$sex),3)
  hb[i]<-round(sum(df[df$w1==i,]$pid==1)/length(df[df$w1==i,]$sex),3)
  hc[i]<-round(sum(df[df$w1==i,]$pid==0.5)/length(df[df$w1==i,]$sex),3)
}
ha
hb
hc

######################
## APPENDIX TABLE 7 ##
######################
m3a<-polr(as.factor(t3) ~  norm3 + il3  + age + sex + education + employed+ 
            income + as.factor(race) + pid+ know + involve  + ci + mi+ iso  , dfusa)
m3<-polr(as.factor(t3) ~  norm3 + il3   , dfusa)
stargazer(m3,m3a,no.space=T,table.placement = "H")

######################
## APPENDIX TABLE 8 ##
######################
df <- read.csv("dataelite.csv")
m3<-polr(as.factor(t1likert) ~ as.factor(w1) + age + sex + education + 
           as.factor(race) + pid + ci + mi+ iso + state +senate , df)
summary(m3)
m3a<-polr(as.factor(t1likert) ~ as.factor(w1)  , df)
summary(m3a)
stargazer(m3a,m3,no.space=T,table.placement = "H")