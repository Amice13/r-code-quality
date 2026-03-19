######################################################################################
######################################################################################
#Replication code for:

#Mark T. Buntaine & Lauren Prather
#Preferences for Domestic Action Over International Transfers in Global Climate Policy
#Journal of Experimental Political Science, forthcoming.

#Mark Buntaine contact (as of October 2017): buntaine@bren.ucsb.edu
#Lauren Prather contact (as of October 2017): lprather@ucsd.edu

#Compiled using R Version 3.4.1 (version "Single Candle") on Mac running OS X 10.12.6
######################################################################################
######################################################################################


######################################################################################
###Packages
######################################################################################
library(nnet) #version 7.3-12
library(ri) #version 0.9
library(ggplot2) #version 2.2.1
library(MASS) #version 7.3-47
library(pscl) #version 1.5.1
library(boot) #version 1.3-20
library(nnet) #version 7.3-12
library(stargazer) #version 5.2
library(AER) #version 1.2-5
library(reshape2) #version 1.4.2
library(plyr) #version 1.8.4
library(cowplot) #version 0.8.0


######################################################################################
###Setting up data from Experiment 1
######################################################################################

#data <- read.csv("~/Google Drive/Climate Transfers project/ClimateTransfer_MTurkData_coded.csv", stringsAsFactors=FALSE)
#data <- read.csv("~/Google Drive/Climate Transfers project/JEPS replication/Experiment1_Data.csv", stringsAsFactors=FALSE)
data <- read.csv("~/Google Drive/Climate Transfers project/JEPS replication/Experiment1_Data_new.csv", stringsAsFactors=FALSE)
data <- subset(data,consent==2 | is.na(consent)) #Removing subjects who decline to participate after reading consent

data$effect1 <- ifelse(data$effect1=="effect",1,data$effect1)
data$effect1 <- ifelse(data$effect1=="no effect",0,data$effect1) #These two lines make 0 the no cost-effectness and 1 the cost-effectness treatments
data$effect1 <- as.numeric(data$effect1)

data$country <- ifelse(data$country=="the United States","US",data$country)
data$country <- factor(data$country, levels=c("US","India","China"))

data$wri.stated <- ifelse(data$wri.stated==2,0,data$wri.stated) #This transforms binary variable to expected values
data$wri.bonus.W <- ifelse(data$wri.stated==1,1,10) #This is the reverse probability of sampling, which re-weights the sample because only 10% of disapproving subjects selected into the bonus portion

data$wri.bonus <- ifelse(data$wri.bonus==2,0,data$wri.bonus) #This transforms binary variable to expected values
data$wri.bonus.confirm <- ifelse(data$wri.bonus.confirm==2,0,data$wri.bonus.confirm) #This transforms binary variable to expected values
data$wri.bonus.confirm.NArm <- ifelse(is.na(data$wri.bonus.confirm),0,data$wri.bonus.confirm)
data$wri.fullbonus.final <- ifelse(is.na(data$wri.bonus),NA,data$wri.bonus.confirm.NArm) #This adds in the people who stated "no" to the final confirmed step for those who said "yes"

data$wri.bonus.amount <- ifelse(data$wri.bonus>=0,0,0)
data$wri.bonus.amount <- ifelse(!is.na(data$wri.fullbonus.final) & data$wri.fullbonus.final==1,50,data$wri.bonus.amount)
data$wri.bonus.amount <- ifelse(!is.na(data$wri.bonus.portion.confirm) & data$wri.bonus.portion.confirm==1 & data$wri.bonus.portion==2,10,data$wri.bonus.amount)
data$wri.bonus.amount <- ifelse(!is.na(data$wri.bonus.portion.confirm) & data$wri.bonus.portion.confirm==1 & data$wri.bonus.portion==3,20,data$wri.bonus.amount)
data$wri.bonus.amount <- ifelse(!is.na(data$wri.bonus.portion.confirm) & data$wri.bonus.portion.confirm==1 & data$wri.bonus.portion==4,30,data$wri.bonus.amount)
data$wri.bonus.amount <- ifelse(!is.na(data$wri.bonus.portion.confirm) & data$wri.bonus.portion.confirm==1 & data$wri.bonus.portion==5,40,data$wri.bonus.amount)

data$wri.bonus.any <- ifelse(data$wri.bonus.amount>0,1,0)

data$govt.spending.support <- ifelse(data$govt.spending.support==2,0,data$govt.spending.support) #This transforms binary variable to expected values, 1-approval; 0-disapprove
data$govt.encourage <- ifelse(data$govt.encourage==2,0,1) #This transforms binary variable to expected values, 1-yes; 0-no
data$govt.discourage <- ifelse(data$govt.discourage==2,0,1) #This transforms binary variable to expected values, 1-yes; 0-no

data$govt.message <- NA #Combining govt.encourage and govt.discourage into one variable
data$govt.message <- ifelse(data$govt.encourage==1 & !is.na(data$statement),"encourage",data$govt.message)
data$govt.message <- ifelse(data$govt.discourage==1 & !is.na(data$govt.discourage)& !is.na(data$statement),"discourage",data$govt.message)
data$govt.message <- ifelse((data$govt.encourage==0 & !is.na(data$govt.encourage)) | (data$govt.discourage==0 & !is.na(data$govt.discourage)) | (data$statement.nonmessage==1 & !is.na(data$statement.nonmessage)),"none",data$govt.message)
data$govt.message <- as.factor(data$govt.message)
data$govt.message <- relevel(data$govt.message,ref="none")

data$representative[is.na(data$representative) & !is.na(data$govt.message)] <- 0
data$rep.message <- ifelse(data$representative==1,as.character(data$govt.message),"none")
data$rep.message <- as.factor(data$rep.message)
data$rep.message <- relevel(data$rep.message,ref="none")

data$govt.message.repclicked <- ifelse(data$repclicked==1 & !is.na(data$repclicked), as.character(data$govt.message), "none")

data$govt.encourage.rep.final <- ifelse(data$govt.message.repclicked=="encourage",1,0)
data$govt.discourage.rep.final <- ifelse(data$govt.message.repclicked=="discourage",1,0)

##Covariate recoding/generation
data$bin.cc.serious.US <- ifelse(data$cc.serious.US==1 | data$cc.serious.US==2,1,0)
data$bin.cc.serious.foreign <- ifelse(data$cc.serious.foreign==1 | data$cc.serious.foreign==2,1,0)
data$bin.cc.notserious.US <- ifelse(data$cc.serious.US==3 | data$cc.serious.US==4,1,0)
data$bin.cc.notserious.foreign <- ifelse(data$cc.serious.foreign==3 | data$cc.serious.foreign==4,1,0)

data$cc.not.willing <- ifelse(data$cc.serious.foreign==4 & data$cc.serious.US==4,1,0)

data$US.favorable <- ifelse(data$US.favorability==1 | data$US.favorability==2,1,0)
data$india.favorable <- ifelse(data$india.favorability==1 | data$india.favorability==2,1,0)
data$china.favorable <- ifelse(data$china.favorability==1 | data$china.favorability==2,1,0)
data$US.unfavorable <- ifelse(data$US.favorability==3 | data$US.favorability==4,1,0)
data$india.unfavorable <- ifelse(data$india.favorability==3 | data$india.favorability==4,1,0)
data$china.unfavorable <- ifelse(data$china.favorability==3 | data$china.favorability==4,1,0)

data$recall.ce[data$recall.ce==1]<-NA
data$recall.ce[data$recall.ce==2]<-"somewhat"
data$recall.ce[data$recall.ce==4]<-"not sure"
data$recall.ce[data$recall.ce==5]<-"most"
data$recall.ce[data$recall.ce==6]<-"least"

data$bin.us.active <- ifelse(data$us.active==1,1,0)

data$received.bonus <- ifelse(!is.na(data$wri.bonus),1,0)

##Data subsets
data.us <- subset(data,country=="US")
data.india <- subset(data,country=="India")
data.china <- subset(data,country=="China")
data.no.repub <- subset(data,party!=1)
data.no.colgrad <- subset(data,education<5)
data.colgrad <- subset(data,education>=5)

data.us.ce0 <- subset(data,country=="US" & effect1==0)
data.india.ce0 <- subset(data,country=="India" & effect1==0)
data.china.ce0 <- subset(data,country=="China" & effect1==0)
data.us.ce1 <- subset(data,country=="US" & effect1==1)
data.india.ce1 <- subset(data,country=="India" & effect1==1)
data.china.ce1 <- subset(data,country=="China" & effect1==1)

data.click <- data[-(1:224),] #Discarding observations with unknown click tracking
data.click.no.repub <- subset(data.click, party!=1) #Excluding Republicans for subgroup analysis
data.click.no.colgrad <- subset(data.click, education<5)
data.click.colgrad <- subset(data.click, education>=5)
data.click.willing <- subset(data.click, cc.not.willing==0) #To match our sampling strategy of "hypothetically willing"

data.willing <- subset(data,cc.not.willing==0) #To match our sampling strategy of "hypothetically willing"
data.click.willing.ce0 <- subset(data.click.willing, effect1==0)
data.willing.us <- subset(data,cc.not.willing==0 & country=="US")
data.willing.india <- subset(data,cc.not.willing==0 & country=="India")
data.willing.china <- subset(data,cc.not.willing==0 & country=="China")

data.willing.us.ce0 <- subset(data.willing,country=="US" & effect1==0)
data.willing.india.ce0 <- subset(data.willing,country=="India" & effect1==0)
data.willing.china.ce0 <- subset(data.willing,country=="China" & effect1==0)
data.willing.us.ce1 <- subset(data.willing,country=="US" & effect1==1)
data.willing.india.ce1 <- subset(data.willing,country=="India" & effect1==1)
data.willing.china.ce1 <- subset(data.willing,country=="China" & effect1==1)

data.willing.no.colgrad <- subset(data.willing, education<5)


######################################################################################
###Setting up data from Experiment 2
######################################################################################

data2.old <- read.csv("~/Google Drive/Climate Transfers project/MTurk_Experiment2_Export_coded.csv", stringsAsFactors=FALSE)
data2 <- read.csv("~/Google Drive/Climate Transfers project/JEPS replication/Experiment2_Data.csv", stringsAsFactors=FALSE)

data2 <- data2[-1,]

#Treatments
data2$pub <- ifelse(data2$control11==1 | data2$control21==1,0,1)
data2$ce <- ifelse(data2$all11==1 | data2$pubcost11==1 | data2$all21==1 | data2$pubcost21==1,1,0)
data2$priv <- ifelse(data2$all11==1 | data2$pubpriv11==1 | data2$all21==1 | data2$pubpriv21==1,1,0)
data2$donate.first <- ifelse(data2$control11==1 | data2$pub11==1 | data2$pubcost11==1 | data2$all11==1 | data2$pubpriv11==1,1,0)

#Outcomes
data2$donate.us.amount <- as.numeric(data2$donate_1)
data2$donate.foreign.amount <- as.numeric(data2$donate_2)
data2$donate.diff <- data2$donate.us.amount - data2$donate.foreign.amount
data2$keep <- as.numeric(data2$donate_3)

data2$donate.multi <- NA
data2$donate.multi <- factor(data2$donate.multi, levels=c("both","us.only","foreign.only","neither"))
data2$donate.multi[(data2$donate.us.amount>0 & data2$donate.foreign.amount>0)] <- "both"
data2$donate.multi[data2$donate.us.amount>0 & data2$donate.foreign.amount==0] <- "us.only"
data2$donate.multi[data2$donate.us.amount==0 & data2$donate.foreign.amount>0] <- "foreign.only"
data2$donate.multi[data2$donate.us.amount==0 & data2$donate.foreign.amount==0] <- "neither"
table(data2$donate.multi, useNA = "always")

data2$donate.any <- ifelse(data2$keep==20, 0, 1)
data2$donate.total <- data2$donate.us.amount + data2$donate.foreign.amount
data2$donate.prop.us <- data2$donate.us.amount / data2$donate.total

data2$fundus <- as.numeric(data2$fundus)
data2$fundus <- ifelse(data2$fundus==2,0,1)
data2$fundfor <- as.numeric(data2$fundfor)
data2$fundfor <- ifelse(data2$fundfor==2,0,1)

#Written message & click
data2$govt.us.encourage.rep.final <- ifelse(data2$fundus==1 & nchar(data2$text, allowNA = FALSE)>0 & is.na(data2$statement.nonmessage) & data2$repclick==1, 1, 0)
data2$govt.us.discourage.rep.final <- ifelse(data2$fundus==0 & nchar(data2$text, allowNA = FALSE)>0 & is.na(data2$statement.nonmessage) & data2$repclick==1, 1, 0)
data2$govt.foreign.encourage.rep.final <- ifelse(data2$fundfor==1 & nchar(data2$text, allowNA = FALSE)>0 & is.na(data2$statement.nonmessage) & data2$repclick==1, 1, 0)
data2$govt.foreign.discourage.rep.final <- ifelse(data2$fundfor==0 & nchar(data2$text, allowNA = FALSE)>0 & is.na(data2$statement.nonmessage) & data2$repclick==1, 1, 0)

#Covariates
for (i in 22:24){
  data2[,i] <- as.numeric(data2[,i])}

data2$local.benefits.score <- rowMeans(data2[,22:24],na.rm=T) #This measures the extent to which subjects report local benefits to be very important

#Effective Data
data2 <- subset(data2,Q70==2 | Q70=="") #Removing subjects who do not consent to participate
data2.no.repub <- subset(data2, Q142!=1) #excluding subjects who identify as Republicans
data2.no.colgrad <- subset(data2, Q115<5) #excluding subjects who graduated from college

#Data Subsets
data2.privT <- subset(data2,pub==1 & priv==1)
data2.NOprivT <- subset(data2,pub==1 & priv==0)

data2.willing <- subset(data2, Q202!=4 & Q203!=4)
data2.w.pure.control <- subset(data2.willing, pub==0)
data2.w.noT <- subset(data2.willing, ce==0 & priv==0)
data2.w.pub <- subset(data2.willing, pub==1)
data2.w.pb.ceiling <- subset(data2.willing, local.benefits.score<=3)
data2.w.pb.prior <- subset(data2.willing, Q132_1!=2 & Q132_2!=4)
data2.w.no.colgrad <- subset(data2.willing, Q115<5)

data2.w.any <- subset(data2.w.pub, donate.us.amount>0 | donate.foreign.amount>0)


######################################################################################
######################################################################################
###Main manuscript
######################################################################################
######################################################################################

######################################################################################
###Figure 1: Location
######################################################################################

### (A) Exp 1: Private Donation
#Note: this runs the analysis for both the location and cost-effectiveness arms (Figure 2)

#Analysis
no.vector.us.ce0<-data.willing.us.ce0$wri.fullbonus.final[data.willing.us.ce0$wri.stated==0 & data.willing.us.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.china.ce0<-data.willing.china.ce0$wri.fullbonus.final[data.willing.china.ce0$wri.stated==0 & data.willing.china.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.india.ce0<-data.willing.india.ce0$wri.fullbonus.final[data.willing.india.ce0$wri.stated==0 & data.willing.india.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.us.ce1<-data.willing.us.ce1$wri.fullbonus.final[data.willing.us.ce1$wri.stated==0 & data.willing.us.ce1$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.china.ce1<-data.willing.china.ce1$wri.fullbonus.final[data.willing.china.ce1$wri.stated==0 & data.willing.china.ce1$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.india.ce1<-data.willing.india.ce1$wri.fullbonus.final[data.willing.india.ce1$wri.stated==0 & data.willing.india.ce1$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part

prob.switch.us.ce0 <- sum(no.vector.us.ce0)/length(no.vector.us.ce0) #This is the probability of donating after stating "no"
prob.switch.china.ce0 <- sum(no.vector.china.ce0)/length(no.vector.china.ce0) #This is the probability of donating after stating "no"
prob.switch.india.ce0 <- sum(no.vector.india.ce0)/length(no.vector.india.ce0) #This is the probability of donating after stating "no"
prob.switch.us.ce1 <- sum(no.vector.us.ce1)/length(no.vector.us.ce1) #This is the probability of donating after stating "no"
prob.switch.china.ce1 <- sum(no.vector.china.ce1)/length(no.vector.china.ce1) #This is the probability of donating after stating "no"
prob.switch.india.ce1 <- sum(no.vector.india.ce1)/length(no.vector.india.ce1) #This is the probability of donating after stating "no"

#Setting up for the simulation
imp <- data.willing$wri.fullbonus.final

treat.country <- unique(data.willing$country)
treat.ce <- c(0,1)
sims <- 10000

us.ob.ce0 <- rep(NA,sims)
china.ob.ce0 <- rep(NA,sims)
india.ob.ce0 <- rep(NA,sims)
us.ob.ce1 <- rep(NA,sims)
china.ob.ce1 <- rep(NA,sims)
india.ob.ce1 <- rep(NA,sims)

ate.china <- rep(NA,sims)
ate.india <- rep(NA,sims)

ate.china.ce1 <- rep(NA,sims)
ate.india.ce1 <- rep(NA,sims)

ate.us.ce <- rep(NA,sims)
ate.china.ce <- rep(NA,sims)
ate.india.ce <- rep(NA,sims)

#The sampling distribution for the ATE under the sharp null is formed over repeated imputations
#This imputes a full data.willing dataset, sampling from the observed values for donating after stating "no"
set.seed(201)
for (j in 1:sims){
  
  for (i in 1:nrow(data.willing)){
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="US" & data.willing$effect1[i]==0){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.us.ce0)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="China" & data.willing$effect1[i]==0){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.china.ce0)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="India" & data.willing$effect1[i]==0){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.india.ce0)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="US" & data.willing$effect1[i]==1){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.us.ce1)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="China" & data.willing$effect1[i]==1){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.china.ce1)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="India" & data.willing$effect1[i]==1){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.india.ce1)}
  }
  
  treatment.country <- sample(treat.country, size=nrow(data.willing), replace=TRUE)
  treatment.ce <- sample(treat.ce, size=nrow(data.willing), replace=TRUE)
  
  us.ob.ce0[j] <- mean(imp[data.willing$country=="US" & data.willing$effect1==0])
  china.ob.ce0[j] <- mean(imp[data.willing$country=="China" & data.willing$effect1==0])
  india.ob.ce0[j]  <- mean(imp[data.willing$country=="India" & data.willing$effect1==0])
  us.ob.ce1[j] <- mean(imp[data.willing$country=="US" & data.willing$effect1==1])
  china.ob.ce1[j] <- mean(imp[data.willing$country=="China" & data.willing$effect1==1])
  india.ob.ce1[j]  <- mean(imp[data.willing$country=="India" & data.willing$effect1==1])
  
  ate.china[j] <- mean(imp[treatment.country=="China" & data.willing$effect1==0]) - mean(imp[treatment.country=="US" & data.willing$effect1==0])
  ate.india[j] <- mean(imp[treatment.country=="India" & data.willing$effect1==0]) - mean(imp[treatment.country=="US" & data.willing$effect1==0])
  
  ate.china.ce1[j] <- mean(imp[treatment.country=="China" & data.willing$effect1==1]) - mean(imp[treatment.country=="US" & data.willing$effect1==1])
  ate.india.ce1[j] <- mean(imp[treatment.country=="India" & data.willing$effect1==1]) - mean(imp[treatment.country=="US" & data.willing$effect1==1])
  
  ate.us.ce[j] <- mean(imp[treatment.ce==1 & data.willing$country=="US"]) - mean(imp[treatment.ce==0 & data.willing$country=="US"])
  ate.china.ce[j] <- mean(imp[treatment.ce==1 & data.willing$country=="China"]) - mean(imp[treatment.ce==0 & data.willing$country=="China"])
  ate.india.ce[j] <- mean(imp[treatment.ce==1 & data.willing$country=="India"]) - mean(imp[treatment.ce==0 & data.willing$country=="India"])
}

#Means after imputation:
mean(us.ob.ce0) #0.2174562
mean(india.ob.ce0) #0.2397313
mean(china.ob.ce0) #0.1281301
#Note: these are the values for Figure 1, Panel A

mean(us.ob.ce1) #0.2411839
mean(india.ob.ce1) #0.2354218
mean(china.ob.ce1) #0.2049109

#SEs of imputation:
sd(us.ob.ce0) #0.01770294
sd(india.ob.ce0) #0.01956023
sd(china.ob.ce0) #0.01453143
sd(us.ob.ce1) #0.02022568
sd(india.ob.ce1) #0.01877704
sd(china.ob.ce1) #0.01833503

#Displaying the sampling distributions after imputation
hist(ate.china)
hist(ate.india)

hist(ate.china.ce1)
hist(ate.india.ce1)

hist(ate.us.ce)
hist(ate.china.ce)
hist(ate.india.ce)

###Evaluating and then averaging the p-value of different imputations
china.p <- rep(NA,sims)
india.p <- rep(NA,sims)

china.ce1.p <- rep(NA,sims)
india.ce1.p <- rep(NA,sims)

us.ce.p <- rep(NA,sims)
china.ce.p <- rep(NA,sims)
india.ce.p <- rep(NA,sims)

for (k in 1:sims){
  china.test <- (ate.china < (china.ob.ce0[k] - us.ob.ce0[k]))
  india.test <- (ate.india < (india.ob.ce0[k] - us.ob.ce0[k]))
  
  china.ce1.test <- (ate.china.ce1 < (china.ob.ce1[k] - us.ob.ce1[k]))
  india.ce1.test <- (ate.india.ce1 < (india.ob.ce1[k] - us.ob.ce1[k]))
  
  us.ce.test <- (ate.us.ce < (us.ob.ce1[k] - us.ob.ce0[k])) #One-tailed test about negative effect of poor CE information
  china.ce.test <- (ate.china.ce > (china.ob.ce1[k] - china.ob.ce0[k])) #One-tailed test about positive effect of good CE information
  india.ce.test <- (abs(ate.india.ce) > abs(india.ob.ce1[k] - india.ob.ce0[k])) #Two-tailed test about moderate CE information
  
  china.p[k] <- sum(china.test)/length(china.test)
  india.p[k] <- sum(india.test)/length(india.test)
  
  china.ce1.p[k] <- sum(china.ce1.test)/length(china.ce1.test)
  india.ce1.p[k] <- sum(india.ce1.test)/length(india.ce1.test)
  
  us.ce.p[k] <- sum(us.ce.test)/length(us.ce.test)
  china.ce.p[k] <- sum(china.ce.test)/length(china.ce.test)
  india.ce.p[k] <- sum(india.ce.test)/length(india.ce.test)
}

mean(china.p) #0.02991913
mean(india.p) #0.6853572

mean(china.ce1.p) #0.2372263
mean(india.ce1.p) #0.457938

mean(us.ce.p) #0.6769077
mean(china.ce.p) #0.04402671
mean(india.ce.p) #0.6353137

set.seed(201)
se <- c(sd(rbinom(size=nrow(data.willing.us.ce0), n=10000, prob=0.2174562)/nrow(data.willing.us.ce0)),
        sd(rbinom(size=nrow(data.willing.india.ce0), n=10000, prob=0.2397313)/nrow(data.willing.india.ce0)),   
        sd(rbinom(size=nrow(data.willing.china.ce0), n=10000, prob=0.1281301)/nrow(data.willing.china.ce0)),
        sd(rbinom(size=nrow(data.willing.us.ce1), n=10000, prob=0.2411839)/nrow(data.willing.us.ce1)),
        sd(rbinom(size=nrow(data.willing.india.ce1), n=10000, prob=0.2354218)/nrow(data.willing.india.ce1)),   
        sd(rbinom(size=nrow(data.willing.china.ce1), n=10000, prob=0.2049109)/nrow(data.willing.china.ce1))
)
#Note: the first three elements of "se" are the SEs for Figure 1, Panel A

se <- c(0.04877672,0.05132853,0.04450108,0.03126550,0.02910411,0.02944495) #Based exactly on above

#Plotting
country <- rev(c("US","India","China"))
prop.donate<-rev(c(0.2174562,0.2397313,0.1281301)) #From proportions calculated above
se <- rev(c(0.04877672,0.05132853,0.04450108)) #From proportions calculated above
prop.dta <- data.frame(prop.donate,country,se)
se.bars <- aes(ymax = prop.donate + se, ymin = prop.donate - se)

fig1.a <-ggplot(data=prop.dta, aes(x=country, y=prop.donate)) + theme_grey() + scale_x_discrete(limits = country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","khaki","indianred2")), colour="black") + geom_errorbar(se.bars, width=0.3) +
  ylab("Proportion Donating") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(A) Exp 1: Private Donation") + theme(plot.title = element_text(lineheight=1, face="bold"))


### (B) Exp 2: Private Donation

#Analysis
diff.ob <- mean(data2.w.pure.control$donate.foreign.amount,na.rm=T) - mean(data2.w.pure.control$donate.us.amount, na.rm=T)
diff.ob
donate.vector <- c(data2.w.pure.control$donate.us.amount,data2.w.pure.control$donate.foreign.amount)
diff <- rep(NA,10000)
set.seed(202)
for (i in 1:10000){
  sample <- sample(1:1068,534)
  diff[i] <- mean(donate.vector[sample], na.rm=T) - mean(donate.vector[-sample], na.rm=T)
}
location.p <- sum((diff < diff.ob))/10000 #0.018

#Plotting
dta2 <- melt(data2.w.pure.control, measure.vars = c('donate.foreign.amount','donate.us.amount')) #Make the object as we want it, selecting final two columns
dta2 <- dta2[,(ncol(dta2)-1):ncol(dta2)]
names(dta2) <- c("Country","Amount")
dta2$Country <- ifelse(dta2$Country=="donate.foreign.amount","Foreign","US")
mean.info <- ddply(dta2, "Country", summarise, donate.mean=mean(Amount))

foreign.store <- rep(NA, 10000)
US.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  foreign.store[i] <- mean(sample(dta2$Amount[1:534], size=534, replace=T))
  US.store[i] <- mean(sample(dta2$Amount[535:1068], size=534, replace=T))
}
mean.info$se[1] <- sd(foreign.store)
mean.info$se[2] <- sd(US.store)
se.bars2 <- aes(ymax = mean.info$donate.mean + mean.info$se, ymin = mean.info$donate.mean - mean.info$se)

fig1.b<-ggplot(mean.info, aes(x=Country, y=donate.mean)) + theme_grey() + scale_x_discrete(limits = mean.info$Country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","darkslategrey")), colour="black") + geom_errorbar(se.bars2, width=0.3) +
  ylab("Amount Donated ($)") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(B) Exp 2: Private Donation") + theme(plot.title = element_text(lineheight=1, face="bold"))


### (C) Exp 1: Public Spending

#Analysis
data.china.test <- subset(data.click.willing, country!="India" & effect1==0)
data.china.test$treat.china <- ifelse(data.china.test$country=="China",1,0)
perms <- genperms(data.china.test$treat.china) # all possible permutations of treatment
ate <- estate(data.china.test$govt.encourage.rep.final, data.china.test$treat.china, HT=TRUE) #point estimate of the ATE (simple in this case)
Ys <- genouts(data.china.test$govt.encourage.rep.final, data.china.test$treat.china, ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys,perms) # generate sampling distribution under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

data.india.test <- subset(data.click.willing, country!="China" & effect1==0)
data.india.test$treat.india <- ifelse(data.india.test$country=="India",1,0)
perms <- genperms(data.india.test$treat.india) # all possible permutations of treatment
ate <- estate(data.india.test$govt.encourage.rep.final, data.india.test$treat.india) #point estimate of the ATE (simple in this case)
Ys <- genouts(data.india.test$govt.encourage.rep.final, data.india.test$treat.india, ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys,perms) # generate sampling distribution under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

#Plotting
tab <- as.data.frame.matrix(table(data.click.willing.ce0$country,data.click.willing.ce0$govt.encourage.rep.final))
names(tab) <- c("X0","X1")
tab$n <- tab$X1 + tab$X0
tab$prop.govt.encourage <- tab$X1/(tab$X0+tab$X1)

us.store <- rep(NA, 10000)
india.store <- rep(NA, 10000)
china.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  us.store[i] <- mean(rbinom(n=tab$n[1], size=1, prob=tab$prop.govt.encourage[1]))
  india.store[i] <- mean(rbinom(n=tab$n[2], size=1, prob=tab$prop.govt.encourage[2]))
  china.store[i] <- mean(rbinom(n=tab$n[3], size=1, prob=tab$prop.govt.encourage[3]))
}
tab$se[1] <- sd(us.store)
tab$se[2] <- sd(india.store)
tab$se[3] <- sd(china.store)
tab$country <- row.names(tab)
tab <- tab[order(tab$country),]

se.bars.ps1 <- aes(ymax = tab$prop.govt.encourage + tab$se, ymin = tab$prop.govt.encourage - tab$se)
fig1.c<-ggplot(tab, aes(x=country, y=prop.govt.encourage)) + theme_grey() + scale_x_discrete(limits = tab$country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","khaki","indianred2")), colour="black") + geom_errorbar(se.bars.ps1, width=0.3) +
  ylab("Proportion Writing to Support") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(C) Exp 1: Public Spending") + theme(plot.title = element_text(lineheight=1, face="bold"))


### (D) Exp 2: Public Spending

#Analysis
diff.ob <- mean(data2.w.pure.control$fundfor,na.rm=T) - mean(data2.w.pure.control$fundus, na.rm=T)
diff.ob
support.vector <- c(data2.w.pure.control$fundus, data2.w.pure.control$fundfor)
diff <- rep(NA,10000)
set.seed(202)
for (i in 1:10000){
  sample <- sample(1:1068,534)
  diff[i] <- mean(support.vector[sample], na.rm=T) - mean(support.vector[-sample], na.rm=T)
}
location.p <- sum((diff < diff.ob))/10000

#Plotting
dta4 <- melt(data2.w.pure.control, measure.vars = c("fundus","fundfor")) #Make the object as we want it, selecting final two columns
dta4 <- dta4[,(ncol(dta4)-1):ncol(dta4)]
names(dta4) <- c("Country","Write")

dta4$Country <- ifelse(dta4$Country=="fundfor","Foreign","US")
mean.info4 <- ddply(dta4, "Country", summarise, write.mean=mean(Write, na.rm=T))

foreign.store <- rep(NA, 10000)
US.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  foreign.store[i] <- mean(sample(dta4$Write[1:534], size=534, replace=T), na.rm=T)
  US.store[i] <- mean(sample(dta4$Write[535:1068], size=534, replace=T), na.rm=T)
}
mean.info4$se[1] <- sd(foreign.store)
mean.info4$se[2] <- sd(US.store)
se.bars4 <- aes(ymax = mean.info4$write.mean + mean.info4$se, ymin = mean.info4$write.mean - mean.info4$se)

fig1.d<-ggplot(mean.info4, aes(x=Country, y=write.mean)) + theme_grey() + scale_x_discrete(limits = mean.info4$Country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","darkslategrey")), colour="black") + geom_errorbar(se.bars4, width=0.3) +
  ylab("Proportion Support") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(D) Exp 2: Public Spending") + theme(plot.title = element_text(lineheight=1, face="bold"))

F1<-plot_grid(fig1.a, fig1.b, fig1.c, fig1.d)

  
######################################################################################
###Figure 2: Cost-Effectiveness
######################################################################################

par(mfrow=c(2,2),mar=c(2.1,4.1,2.1,1.1),mgp=c(2.5,1,0)) #This will plot the four panels together, skip if examining individually

### (A) Exp 1: Private Donation
#Note: this runs the analysis for both the location and cost-effectiveness arms (Figure 1)
#Note: this is an exact copy of the analysis in Figure 1, Panel A pasted here for convenience

#Analysis
no.vector.us.ce0<-data.willing.us.ce0$wri.fullbonus.final[data.willing.us.ce0$wri.stated==0 & data.willing.us.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.china.ce0<-data.willing.china.ce0$wri.fullbonus.final[data.willing.china.ce0$wri.stated==0 & data.willing.china.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.india.ce0<-data.willing.india.ce0$wri.fullbonus.final[data.willing.india.ce0$wri.stated==0 & data.willing.india.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.us.ce1<-data.willing.us.ce1$wri.fullbonus.final[data.willing.us.ce1$wri.stated==0 & data.willing.us.ce1$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.china.ce1<-data.willing.china.ce1$wri.fullbonus.final[data.willing.china.ce1$wri.stated==0 & data.willing.china.ce1$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.india.ce1<-data.willing.india.ce1$wri.fullbonus.final[data.willing.india.ce1$wri.stated==0 & data.willing.india.ce1$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part

prob.switch.us.ce0 <- sum(no.vector.us.ce0)/length(no.vector.us.ce0) #This is the probability of donating after stating "no"
prob.switch.china.ce0 <- sum(no.vector.china.ce0)/length(no.vector.china.ce0) #This is the probability of donating after stating "no"
prob.switch.india.ce0 <- sum(no.vector.india.ce0)/length(no.vector.india.ce0) #This is the probability of donating after stating "no"
prob.switch.us.ce1 <- sum(no.vector.us.ce1)/length(no.vector.us.ce1) #This is the probability of donating after stating "no"
prob.switch.china.ce1 <- sum(no.vector.china.ce1)/length(no.vector.china.ce1) #This is the probability of donating after stating "no"
prob.switch.india.ce1 <- sum(no.vector.india.ce1)/length(no.vector.india.ce1) #This is the probability of donating after stating "no"

#Setting up for the simulation
imp <- data.willing$wri.fullbonus.final

treat.country <- unique(data.willing$country)
treat.ce <- c(0,1)
sims <- 10000

us.ob.ce0 <- rep(NA,sims)
china.ob.ce0 <- rep(NA,sims)
india.ob.ce0 <- rep(NA,sims)
us.ob.ce1 <- rep(NA,sims)
china.ob.ce1 <- rep(NA,sims)
india.ob.ce1 <- rep(NA,sims)

ate.china <- rep(NA,sims)
ate.india <- rep(NA,sims)

ate.china.ce1 <- rep(NA,sims)
ate.india.ce1 <- rep(NA,sims)

ate.us.ce <- rep(NA,sims)
ate.china.ce <- rep(NA,sims)
ate.india.ce <- rep(NA,sims)

#The sampling distribution for the ATE under the sharp null is formed over repeated imputations
#This imputes a full data.willing dataset, sampling from the observed values for donating after stating "no"
set.seed(201)
for (j in 1:sims){
  
  for (i in 1:nrow(data.willing)){
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="US" & data.willing$effect1[i]==0){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.us.ce0)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="China" & data.willing$effect1[i]==0){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.china.ce0)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="India" & data.willing$effect1[i]==0){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.india.ce0)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="US" & data.willing$effect1[i]==1){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.us.ce1)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="China" & data.willing$effect1[i]==1){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.china.ce1)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="India" & data.willing$effect1[i]==1){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.india.ce1)}
  }
  
  treatment.country <- sample(treat.country, size=nrow(data.willing), replace=TRUE)
  treatment.ce <- sample(treat.ce, size=nrow(data.willing), replace=TRUE)
  
  us.ob.ce0[j] <- mean(imp[data.willing$country=="US" & data.willing$effect1==0])
  china.ob.ce0[j] <- mean(imp[data.willing$country=="China" & data.willing$effect1==0])
  india.ob.ce0[j]  <- mean(imp[data.willing$country=="India" & data.willing$effect1==0])
  us.ob.ce1[j] <- mean(imp[data.willing$country=="US" & data.willing$effect1==1])
  china.ob.ce1[j] <- mean(imp[data.willing$country=="China" & data.willing$effect1==1])
  india.ob.ce1[j]  <- mean(imp[data.willing$country=="India" & data.willing$effect1==1])
  
  ate.china[j] <- mean(imp[treatment.country=="China" & data.willing$effect1==0]) - mean(imp[treatment.country=="US" & data.willing$effect1==0])
  ate.india[j] <- mean(imp[treatment.country=="India" & data.willing$effect1==0]) - mean(imp[treatment.country=="US" & data.willing$effect1==0])
  
  ate.china.ce1[j] <- mean(imp[treatment.country=="China" & data.willing$effect1==1]) - mean(imp[treatment.country=="US" & data.willing$effect1==1])
  ate.india.ce1[j] <- mean(imp[treatment.country=="India" & data.willing$effect1==1]) - mean(imp[treatment.country=="US" & data.willing$effect1==1])
  
  ate.us.ce[j] <- mean(imp[treatment.ce==1 & data.willing$country=="US"]) - mean(imp[treatment.ce==0 & data.willing$country=="US"])
  ate.china.ce[j] <- mean(imp[treatment.ce==1 & data.willing$country=="China"]) - mean(imp[treatment.ce==0 & data.willing$country=="China"])
  ate.india.ce[j] <- mean(imp[treatment.ce==1 & data.willing$country=="India"]) - mean(imp[treatment.ce==0 & data.willing$country=="India"])
}

#Means after imputation:
mean(us.ob.ce0) #0.2174562
mean(india.ob.ce0) #0.2397313
mean(china.ob.ce0) #0.1281301
mean(us.ob.ce1) #0.2411839
mean(india.ob.ce1) #0.2354218
mean(china.ob.ce1) #0.2049109
#Note: these are the values for Figure 2, Panel A

mean(us.ob.ce1) #0.2411839
mean(india.ob.ce1) #0.2354218
mean(china.ob.ce1) #0.2049109

#SEs of imputation:
sd(us.ob.ce0) #0.01770294
sd(india.ob.ce0) #0.01956023
sd(china.ob.ce0) #0.01453143
sd(us.ob.ce1) #0.02022568
sd(india.ob.ce1) #0.01877704
sd(china.ob.ce1) #0.01833503

#Displaying the sampling distributions after imputation
hist(ate.china)
hist(ate.india)

hist(ate.china.ce1)
hist(ate.india.ce1)

hist(ate.us.ce)
hist(ate.china.ce)
hist(ate.india.ce)

###Evaluating and then averaging the p-value of different imputations
china.p <- rep(NA,sims)
india.p <- rep(NA,sims)

china.ce1.p <- rep(NA,sims)
india.ce1.p <- rep(NA,sims)

us.ce.p <- rep(NA,sims)
china.ce.p <- rep(NA,sims)
india.ce.p <- rep(NA,sims)

for (k in 1:sims){
  china.test <- (ate.china < (china.ob.ce0[k] - us.ob.ce0[k]))
  india.test <- (ate.india < (india.ob.ce0[k] - us.ob.ce0[k]))
  
  china.ce1.test <- (ate.china.ce1 < (china.ob.ce1[k] - us.ob.ce1[k]))
  india.ce1.test <- (ate.india.ce1 < (india.ob.ce1[k] - us.ob.ce1[k]))
  
  us.ce.test <- (ate.us.ce < (us.ob.ce1[k] - us.ob.ce0[k])) #One-tailed test about negative effect of poor CE information
  china.ce.test <- (ate.china.ce > (china.ob.ce1[k] - china.ob.ce0[k])) #One-tailed test about positive effect of good CE information
  india.ce.test <- (abs(ate.india.ce) > abs(india.ob.ce1[k] - india.ob.ce0[k])) #Two-tailed test about moderate CE information
  
  china.p[k] <- sum(china.test)/length(china.test)
  india.p[k] <- sum(india.test)/length(india.test)
  
  china.ce1.p[k] <- sum(china.ce1.test)/length(china.ce1.test)
  india.ce1.p[k] <- sum(india.ce1.test)/length(india.ce1.test)
  
  us.ce.p[k] <- sum(us.ce.test)/length(us.ce.test)
  china.ce.p[k] <- sum(china.ce.test)/length(china.ce.test)
  india.ce.p[k] <- sum(india.ce.test)/length(india.ce.test)
}

mean(china.p) #0.02991913
mean(india.p) #0.6853572

mean(china.ce1.p) #0.2372263
mean(india.ce1.p) #0.457938

mean(us.ce.p) #0.6769077
mean(china.ce.p) #0.04402671
mean(india.ce.p) #0.6353137

#Plotting
set.seed(201)
se.e1 <- c(sd(rbinom(size=nrow(data.willing.us.ce0), n=10000, prob=0.2174562)/nrow(data.willing.us.ce0)),
        sd(rbinom(size=nrow(data.willing.china.ce0), n=10000, prob=0.1281301)/nrow(data.willing.china.ce0)),
        sd(rbinom(size=nrow(data.willing.india.ce0), n=10000, prob=0.2397313)/nrow(data.willing.india.ce0)),
        sd(rbinom(size=nrow(data.willing.us.ce1), n=10000, prob=0.2411839)/nrow(data.willing.us.ce1)),
        sd(rbinom(size=nrow(data.willing.china.ce1), n=10000, prob=0.2049109)/nrow(data.willing.china.ce1)),
        sd(rbinom(size=nrow(data.willing.india.ce1), n=10000, prob=0.2354218)/nrow(data.willing.india.ce1))   
)
se.e1 <- c(0.02958062,0.02430956,0.03149871,0.03139279,0.02972925,0.02913393) #Based exactly on block immediately above

ce = c(0,-0.05,0.05,1,0.95,1.05)
prop.donate=c(0.2174562,
              0.1281301,
              0.2397313,
              0.2411839,
              0.2049109,
              0.2354218) #From Analysis block; Ordering: US.ce0,China.ce0,India.ce0,US.ce1,China.ce1,India.ce1

plot(x=ce,y=prop.donate,xaxt="n",xlim=c(-0.3,1.3),ylim=c(0.05,0.3),ylab="Proportion Donating Full Bonus",xlab="",
     main="(A) Exp 1: Private Donation", col=c("steelblue1","indianred2","khaki","steelblue1","indianred2","khaki"), pch=19,cex=1.5, cex.main=1.4)
lines(x=c(0,1), y=c(prop.donate[1],prop.donate[4]), col="steelblue1", lwd=2)
lines(x=c(-0.05,0.95), y=c(prop.donate[2],prop.donate[5]), col="indianred2", lwd=2)
lines(x=c(0.05,1.05), y=c(prop.donate[3],prop.donate[6]), col="khaki", lwd=2)
arrows(ce, prop.donate-se.e1, ce, prop.donate+se.e1, length=0.05, angle=90, code=3)
axis(1,at=c(0,1),labels=c("Control","CE Treatment"), cex.axis=1.2)
legend(x=0.9,y=0.11, legend=c("India","US","China"), cex=1, 
       fill=c("khaki","steelblue1","indianred2"), bty = "n")


### (B) Exp 2: Private Donation

#Analysis
data2.h2 <- subset(data2.willing,pub==1)
mean(data2.h2$donate.us.amount[data2.h2$ce==1],na.rm=T) #2.90
mean(data2.h2$donate.us.amount[data2.h2$ce==0],na.rm=T) #3.32
diff.ob.us = mean(data2.h2$donate.us.amount[data2.h2$ce==1],na.rm=T) - mean(data2.h2$donate.us.amount[data2.h2$ce==0],na.rm=T)
diff.ob.us
sims <- 5000
y1.mean.us <- rep(NA,sims)
y0.mean.us <- rep(NA,sims)
set.seed(201)
for (i in 1:sims){
  treat.vector <- rbinom(n=nrow(data2.h2),size=1,prob=0.5)
  y1 <- data2.h2$donate.us.amount[treat.vector==1]
  y0 <- data2.h2$donate.us.amount[treat.vector==0]
  y1.mean.us[i] <- mean(y1)
  y0.mean.us[i] <- mean(y0)
}
h2.diff.us <- y1.mean.us - y0.mean.us
hist(h2.diff.us)
ce.us.p <- sum((diff.ob.us > h2.diff.us))/sims #p = 0.0026

mean(data2.h2$donate.foreign.amount[data2.h2$ce==1],na.rm=T) #3.50
mean(data2.h2$donate.foreign.amount[data2.h2$ce==0],na.rm=T) #3.00
diff.ob.foreign = mean(data2.h2$donate.foreign.amount[data2.h2$ce==1],na.rm=T) - mean(data2.h2$donate.foreign.amount[data2.h2$ce==0],na.rm=T)
diff.ob.foreign #0.50
sims <- 5000
y1.mean.foreign <- rep(NA,sims)
y0.mean.foreign <- rep(NA,sims)
set.seed(201)
for (i in 1:sims){
  treat.vector <- rbinom(n=nrow(data2.h2),size=1,prob=0.5)
  y1 <- data2.h2$donate.foreign.amount[treat.vector==1]
  y0 <- data2.h2$donate.foreign.amount[treat.vector==0]
  y1.mean.foreign[i] <- mean(y1)
  y0.mean.foreign[i] <- mean(y0)
}
h2.diff.foreign <- y1.mean.foreign - y0.mean.foreign
hist(h2.diff.foreign)
ce.foreign.p <- sum((diff.ob.foreign < h2.diff.foreign))/sims #p = 0.0006

#Plotting
treat.donate2=c(0,0,1,1)
value.donate2=c(mean(data2.w.pub$donate.us.amount[data2.w.pub$ce==0],na.rm=T),
                mean(data2.w.pub$donate.foreign.amount[data2.w.pub$ce==0],na.rm=T),
                mean(data2.w.pub$donate.us.amount[data2.w.pub$ce==1],na.rm=T),
                mean(data2.w.pub$donate.foreign.amount[data2.w.pub$ce==1],na.rm=T)
)
n.donate2=c(length(data2.w.pub$donate.us.amount[data2.w.pub$ce==0]),
            length(data2.w.pub$donate.foreign.amount[data2.w.pub$ce==0]),
            length(data2.w.pub$donate.us.amount[data2.w.pub$ce==1]),
            length(data2.w.pub$donate.foreign.amount[data2.w.pub$ce==1])
)

d <- data.frame(matrix(nrow=10000))[,-1] #Creating empty dataframe to fill with bootstrapped means
set.seed(201)
for (i in 1:10000){
  d$y0.mean.us[i] <- mean(sample(data2.w.pub$donate.us.amount[data2.w.pub$ce==0], replace=T, size=n.donate2[1]))
  d$y0.mean.foreign[i] <- mean(sample(data2.w.pub$donate.foreign.amount[data2.w.pub$ce==0], replace=T, size=n.donate2[2]))
  d$y1.mean.us[i] <- mean(sample(data2.w.pub$donate.us.amount[data2.w.pub$ce==1], replace=T, size=n.donate2[3]))
  d$y1.mean.foreign[i] <- mean(sample(data2.w.pub$donate.foreign.amount[data2.w.pub$ce==1], replace=T, size=n.donate2[4]))
}

se.donate2=c(sd(d$y0.mean.us),sd(d$y0.mean.foreign),sd(d$y1.mean.us),sd(d$y1.mean.foreign))

plot(x=treat.donate2,y=value.donate2,xaxt="n",xlim=c(-0.5,1.5),ylim=c(2.6,3.7),ylab="Amount Donated ($)",xlab="",
     main="(B) Exp 2: Private Donation", col=c("steelblue1","black","steelblue1","black"), pch=19,cex=1.5, cex.main=1.4)
axis(1,at=c(0,1),labels=c("Control","CE Treatment"), cex.axis=1.2)
arrows(treat.donate2, value.donate2-se.donate2, treat.donate2, value.donate2+se.donate2, length=0.05, angle=90, code=3)
lines(x=c(0,1), y=c(value.donate2[1], value.donate2[3]), col="steelblue1", lwd=2)
lines(x=c(0,1), y=c(value.donate2[2], value.donate2[4]), col="black", lwd=2)
legend("bottomleft", legend=c("US","Foreign"), cex=1, 
       fill=c("steelblue1","black"), bty = "n")


### (C) Exp 1: Public Spending

#Analysis
data.us.ce.test <- subset(data.click.willing, country=="US")
perms <- genperms(data.us.ce.test$effect1) # all possible permutations of treatment
ate <- estate(data.us.ce.test$govt.encourage.rep.final, data.us.ce.test$effect1) #point estimate of the ATE (simple in this case)
Ys <- genouts(data.us.ce.test$govt.encourage.rep.final, data.us.ce.test$effect1, ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys,perms) # generate sampling distribution under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

data.china.ce.test <- subset(data.click.willing, country=="China")
perms <- genperms(data.china.ce.test$effect1) # all possible permutations of treatment
ate <- estate(data.china.ce.test$govt.encourage.rep.final, data.china.ce.test$effect1) #point estimate of the ATE (simple in this case)
Ys <- genouts(data.china.ce.test$govt.encourage.rep.final, data.china.ce.test$effect1, ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys,perms) # generate sampling distribution under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

data.india.ce.test <- subset(data.click.willing, country=="India")
perms <- genperms(data.india.ce.test$effect1) # all possible permutations of treatment
ate <- estate(data.india.ce.test$govt.encourage.rep.final, data.india.ce.test$effect1) #point estimate of the ATE (simple in this case)
Ys <- genouts(data.india.ce.test$govt.encourage.rep.final, data.india.ce.test$effect1, ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys,perms) # generate sampling distribution under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

#Plotting
ce = c(0.05,-0.05,0,1.05,0.95,1)

us.e.ce0 <- data.click.willing$govt.encourage.rep.final[data.click.willing$country=="US" & data.click.willing$effect1==0]
china.e.ce0 <- data.click.willing$govt.encourage.rep.final[data.click.willing$country=="China" & data.click.willing$effect1==0]
india.e.ce0 <- data.click.willing$govt.encourage.rep.final[data.click.willing$country=="India" & data.click.willing$effect1==0]
us.e.ce1 <- data.click.willing$govt.encourage.rep.final[data.click.willing$country=="US" & data.click.willing$effect1==1]
china.e.ce1 <- data.click.willing$govt.encourage.rep.final[data.click.willing$country=="China" & data.click.willing$effect1==1]
india.e.ce1 <- data.click.willing$govt.encourage.rep.final[data.click.willing$country=="India" & data.click.willing$effect1==1]

prop.encourage=c(sum(us.e.ce0) / length(us.e.ce0),
                 sum(china.e.ce0) / length(china.e.ce0),
                 sum(india.e.ce0) / length(india.e.ce0),
                 sum(us.e.ce1) / length(us.e.ce1),
                 sum(china.e.ce1) / length(china.e.ce1),
                 sum(india.e.ce1) / length(india.e.ce1)
)

set.seed(201)
se.pub <- c(sd(rbinom(size=length(us.e.ce0), n=10000, prob=mean(us.e.ce0)) / length(us.e.ce0)),
            sd(rbinom(size=length(china.e.ce0), n=10000, prob=mean(china.e.ce0)) / length(china.e.ce0)),
            sd(rbinom(size=length(india.e.ce0), n=10000, prob=mean(india.e.ce0)) / length(india.e.ce0)),
            sd(rbinom(size=length(us.e.ce1), n=10000, prob=mean(us.e.ce1)) / length(us.e.ce1)),
            sd(rbinom(size=length(china.e.ce1), n=10000, prob=mean(china.e.ce1)) / length(china.e.ce1)),
            sd(rbinom(size=length(india.e.ce1), n=10000, prob=mean(india.e.ce1)) / length(india.e.ce1))
)
#Note: this is actually a simplification of the bootstrapped CIs, which might not be symetric

plot(x=ce,y=prop.encourage,xaxt="n",xlim=c(-0.3,1.3),ylim=c(0.00,0.20),ylab="Proportion Writing to Encourage",xlab="",
     main="(C) Exp 1: Public Spending", col=c("steelblue1","indianred2","khaki","steelblue1","indianred2","khaki"), pch=19,cex=1.5, cex.main=1.4)
lines(x=c(0.05,1.05), y=c(sum(us.e.ce0) / length(us.e.ce0), sum(us.e.ce1) / length(us.e.ce1)), col="steelblue1", lwd=2)
lines(x=c(-0.05,0.95), y=c(sum(china.e.ce0) / length(china.e.ce0), sum(china.e.ce1) / length(china.e.ce1)), col="indianred2", lwd=2)
lines(x=c(0,1), y=c(sum(india.e.ce0) / length(india.e.ce0), sum(india.e.ce1) / length(india.e.ce1)), col="khaki", lwd=2)
arrows(ce, prop.encourage-se.pub, ce, prop.encourage+se.pub, length=0.05, angle=90, code=3)
axis(1,at=c(0,1),labels=c("Control","CE Treatment"), cex.axis=1.2)
legend("bottomleft", legend=c("US","India","China"), cex=1, 
       fill=c("steelblue1","khaki","indianred2"), bty = "n")


### (D) Exp 2: Public Spending

#Analysis
diff.ob <- mean(data2.w.pub$fundus[data2.w.pub$ce==1],na.rm=T) - mean(data2.w.pub$fundus[data2.w.pub$ce==0], na.rm=T)
diff.ob #-0.03144224
sims <- 5000
treat.vector <- c(0,1)
y1.mean <- rep(NA,sims)
y0.mean <- rep(NA,sims)
set.seed(201)
for (i in 1:sims){
  rand.treat <- sample(treat.vector, size=length(data2.w.pub$fundus), replace=T)
  y1 <- data2.w.pub$fundus[rand.treat==1]
  y0 <- data2.w.pub$fundus[rand.treat==0]
  y1.mean[i] <- mean(y1, na.rm=T)
  y0.mean[i] <- mean(y0, na.rm=T)
}
te <- y1.mean - y0.mean
p <- sum((te < diff.ob))/sims #0.005

diff.ob <- mean(data2.w.pub$fundfor[data2.w.pub$ce==1],na.rm=T) - mean(data2.w.pub$fundfor[data2.w.pub$ce==0], na.rm=T)
diff.ob #0.05716352
sims <- 5000
treat.vector <- c(0,1)
y1.mean <- rep(NA,sims)
y0.mean <- rep(NA,sims)
set.seed(201)
for (i in 1:sims){
  rand.treat <- sample(treat.vector, size=length(data2.w.pub$fundfor), replace=T)
  y1 <- data2.w.pub$fundfor[rand.treat==1]
  y0 <- data2.w.pub$fundfor[rand.treat==0]
  y1.mean[i] <- mean(y1, na.rm=T)
  y0.mean[i] <- mean(y0, na.rm=T)
}
te <- y1.mean - y0.mean
p <- sum((te > diff.ob))/sims #0.0028

#Plotting
treat.public2=c(1,0,1,0)
value.public2=c(mean(data2.w.pub$fundus[data2.w.pub$ce==1],na.rm=T),
                mean(data2.w.pub$fundus[data2.w.pub$ce==0],na.rm=T),
                mean(data2.w.pub$fundfor[data2.w.pub$ce==1],na.rm=T),
                mean(data2.w.pub$fundfor[data2.w.pub$ce==0],na.rm=T))
n.public2=c(length(!is.na(data2.w.pub$fundus[data2.w.pub$ce==1])),
            length(!is.na(data2.w.pub$fundus[data2.w.pub$ce==0])),
            length(!is.na(data2.w.pub$fundfor[data2.w.pub$ce==1])),
            length(!is.na(data2.w.pub$fundfor[data2.w.pub$ce==0])))

d <- data.frame(matrix(nrow=10000))[,-1] #Creating empty dataframe to fill with bootstrapped means
set.seed(201)
for (i in 1:10000){
  d$y1.mean.us[i] <- mean(sample(data2.w.pub$fundus[data2.w.pub$ce==1], replace=T, size=n.donate2[1]),na.rm=T)
  d$y0.mean.us[i] <- mean(sample(data2.w.pub$fundus[data2.w.pub$ce==0], replace=T, size=n.donate2[2]),na.rm=T)
  d$y1.mean.foreign[i] <- mean(sample(data2.w.pub$fundfor[data2.w.pub$ce==1], replace=T, size=n.donate2[3]),na.rm=T)
  d$y0.mean.foreign[i] <- mean(sample(data2.w.pub$fundfor[data2.w.pub$ce==0], replace=T, size=n.donate2[4]),na.rm=T)
}
se.public2=c(sd(d$y1.mean.us),sd(d$y0.mean.us),sd(d$y1.mean.foreign),sd(d$y0.mean.foreign))

plot(x=treat.public2,y=value.public2,xaxt="n",xlim=c(-0.5,1.5),ylim=c(0.6,1),ylab="Proportion Stated Support",xlab="",
     main="(D) Exp 2: Public Spending", col=c("steelblue1","steelblue1","black","black"), pch=19,cex=1.5, cex.main=1.4)
lines(x=c(0,1), y=c(value.public2[2], value.public2[1]), col="steelblue1", lwd=2)
lines(x=c(0,1), y=c(value.public2[4], value.public2[3]), col="black", lwd=2)
axis(1,at=c(0,1),labels=c("Control","CE Treatment"), cex.axis=1,2)
arrows(treat.public2, value.public2-se.public2, treat.public2, value.public2+se.public2, length=0.05, angle=90, code=3)
legend("bottomleft", legend=c("US","Foreign"), cex=1, 
       fill=c("steelblue1","black"), bty = "n")


######################################################################################
###Figure 3: Provisioning Benefits
######################################################################################

### (A) Exp 2: Private Donation

#Analysis (Domestic/Public Goods Prompt)
data2.h1 <- subset(data2.willing,ce==0 & priv==0)
mean(data2.h1$donate.us.amount[data2.h1$pub==1],na.rm=T) #3.44
mean(data2.h1$donate.us.amount[data2.h1$pub==0],na.rm=T) #3.28
diff.ob = mean(data2.h1$donate.us.amount[data2.h1$pub==1],na.rm=T) - mean(data2.h1$donate.us.amount[data2.h1$pub==0],na.rm=T)
sims <- 5000
y1.mean.us <- rep(NA,sims)
y0.mean.us <- rep(NA,sims)
set.seed(202)
for (i in 1:sims){
  treat.vector <- rbinom(n=nrow(data2.h1),size=1,prob=0.5)
  y1 <- data2.h1$donate.us.amount[treat.vector==1]
  y0 <- data2.h1$donate.us.amount[treat.vector==0]
  y1.mean.us[i] <- mean(y1)
  y0.mean.us[i] <- mean(y0)
}
h1.diff.us <- y1.mean.us - y0.mean.us
hist(h1.diff.us)
pub.p <- sum((abs(h1.diff.us) > abs(diff.ob)))/sims #0.4954

#Analysis (Domestic/Provisioning Benefits)
data2.h2 <- subset(data2.willing,pub==1)
mean(data2.h2$donate.us.amount[data2.h2$priv==1],na.rm=T) #3.05
mean(data2.h2$donate.us.amount[data2.h2$priv==0],na.rm=T) #3.17
diff.ob.us = mean(data2.h2$donate.us.amount[data2.h2$priv==1],na.rm=T) - mean(data2.h2$donate.us.amount[data2.h2$priv==0],na.rm=T)
diff.ob.us #-0.12
sims <- 5000
y1.mean.us <- rep(NA,sims)
y0.mean.us <- rep(NA,sims)
set.seed(201)
for (i in 1:sims){
  treat.vector <- rbinom(n=nrow(data2.h2),size=1,prob=0.5)
  y1 <- data2.h2$donate.us.amount[treat.vector==1]
  y0 <- data2.h2$donate.us.amount[treat.vector==0]
  y1.mean.us[i] <- mean(y1)
  y0.mean.us[i] <- mean(y0)
}
h2.diff.us <- y1.mean.us - y0.mean.us
hist(h2.diff.us)
priv.us.p <- sum((diff.ob.us < h2.diff.us))/sims #p = 0.777

#Analysis (Foreign/Public Goods Prompt)
mean(data2.h1$donate.foreign.amount[data2.h1$pub==1],na.rm=T) #3.19
mean(data2.h1$donate.foreign.amount[data2.h1$pub==0],na.rm=T) #2.83
diff.ob = mean(data2.h1$donate.foreign.amount[data2.h1$pub==1],na.rm=T) - mean(data2.h1$donate.foreign.amount[data2.h1$pub==0],na.rm=T) #0.36
sims <- 5000
y1.mean.foreign <- rep(NA,sims)
y0.mean.foreign <- rep(NA,sims)
set.seed(202)
for (i in 1:sims){
  treat.vector <- rbinom(n=nrow(data2.h1),size=1,prob=0.5)
  y1 <- data2.h1$donate.us.amount[treat.vector==1]
  y0 <- data2.h1$donate.us.amount[treat.vector==0]
  y1.mean.foreign[i] <- mean(y1)
  y0.mean.foreign[i] <- mean(y0)
}
h1.diff.foreign <- y1.mean.foreign - y0.mean.foreign
hist(h1.diff.foreign)
pub.p.for <- sum(h1.diff.foreign > diff.ob)/sims #0.0556

#Analysis (Foreign/Provision Benefits)
mean(data2.h2$donate.foreign.amount[data2.h2$priv==1],na.rm=T) #3.10
mean(data2.h2$donate.foreign.amount[data2.h2$priv==0],na.rm=T) #3.40
diff.ob.foreign = mean(data2.h2$donate.foreign.amount[data2.h2$priv==1],na.rm=T) - mean(data2.h2$donate.foreign.amount[data2.h2$priv==0],na.rm=T)
diff.ob.foreign #-0.31
sims <- 5000
y1.mean.foreign <- rep(NA,sims)
y0.mean.foreign <- rep(NA,sims)
set.seed(201)
for (i in 1:sims){
  treat.vector <- rbinom(n=nrow(data2.h2),size=1,prob=0.5)
  y1 <- data2.h2$donate.foreign.amount[treat.vector==1]
  y0 <- data2.h2$donate.foreign.amount[treat.vector==0]
  y1.mean.foreign[i] <- mean(y1)
  y0.mean.foreign[i] <- mean(y0)
}
h2.diff.foreign <- y1.mean.foreign - y0.mean.foreign
hist(h2.diff.foreign)
priv.foreign.p <- sum((diff.ob.foreign > h2.diff.foreign))/sims #p = 0.0236

#Plotting
country <- rep(c("US","Foreign"),3)
country <- ordered(country, levels=c("US","Foreign"))
prop.donate=c(mean(data2.willing$donate.us.amount[data2.willing$pub==0],na.rm=T),
              mean(data2.willing$donate.foreign.amount[data2.willing$pub==0],na.rm=T),
              mean(data2.willing$donate.us.amount[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0],na.rm=T),
              mean(data2.willing$donate.foreign.amount[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0],na.rm=T),
              mean(data2.willing$donate.us.amount[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0],na.rm=T),
              mean(data2.willing$donate.foreign.amount[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0],na.rm=T))

n.donate=c(length(data2.willing$donate.us.amount[data2.willing$pub==0]),
           length(data2.willing$donate.foreign.amount[data2.willing$pub==0]),
           length(data2.willing$donate.us.amount[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0]),
           length(data2.willing$donate.foreign.amount[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0]),
           length(data2.willing$donate.us.amount[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0]),
           length(data2.willing$donate.foreign.amount[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0]))

d <- data.frame(matrix(nrow=10000))[,-1] #Creating empty dataframe to fill with bootstrapped means
set.seed(201)
for (i in 1:10000){
  d$yc.mean.us[i] <- mean(sample(data2.willing$donate.us.amount[data2.willing$pub==0], replace=T, size=n.donate[1]), na.rm=T)
  d$yc.mean.foreign[i] <- mean(sample(data2.willing$donate.foreign.amount[data2.willing$pub==0], replace=T, size=n.donate[2]), na.rm=T)
  d$y0.mean.us[i] <- mean(sample(data2.willing$donate.us.amount[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0], replace=T, size=n.donate[3]), na.rm=T)
  d$y0.mean.foreign[i] <- mean(sample(data2.willing$donate.foreign.amount[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0], replace=T, size=n.donate[4]), na.rm=T)
  d$y1.mean.us[i] <- mean(sample(data2.willing$donate.us.amount[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0], replace=T, size=n.donate[5]), na.rm=T)
  d$y1.mean.foreign[i] <- mean(sample(data2.willing$donate.foreign.amount[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0], replace=T, size=n.donate[6]), na.rm=T)
}
se.donate=c(sd(d$yc.mean.us),sd(d$yc.mean.foreign),sd(d$y0.mean.us),sd(d$y0.mean.foreign),sd(d$y1.mean.us),sd(d$y1.mean.foreign))

cond <- c("Pure Control","Pure Control","Global Good","Global Good","Global Good + Local Co-Benefit","Global Good + Local Co-Benefit")
cond <- ordered(cond, levels=c("Pure Control","Global Good","Global Good + Local Co-Benefit"))
prop.dta <- data.frame(prop.donate,se.donate,country,cond)
se.bars <- aes(ymax = prop.donate + se.donate, ymin = prop.donate - se.donate)
facet_names <- list(
  'US'="US",
  'Foreign'="Foreign"
)
facet_labeller <- function(variable,value){
  return(facet_names[value])
}

fig3.a <- ggplot(data=prop.dta, aes(x=cond, y=prop.donate)) + 
  geom_point(stat="identity", colour=rev(c("black","black","black","steelblue1","steelblue1","steelblue1")), size=4) + 
  geom_errorbar(se.bars, width=0.2) + facet_wrap(~country, labeller=facet_labeller) +
  xlab("Treatment Condition") + ylab("Amount Donated ($)") +  ggtitle("(A) Exp 2: Private Donation") +
  theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=11)) +
  scale_x_discrete(labels=c("Pure Control" = "Pure Control", "Global Good" = "Global Good", "Global Good + Local Co-Benefit" = "Global Good +\nLocal Co-Benefit"))


### (B) Exp 2: Public Spending

#Analysis (Domestic\Public Goods Prompt)
data.test <- subset(data2.willing, ce==0 & priv==0)
diff.ob <- mean(data.test$fundus[data.test$pub==1],na.rm=T) - mean(data.test$fundus[data.test$pub==0], na.rm=T)
diff.ob #0.01139055
sims <- 5000
treat.vector <- c(0,1)
y1.mean <- rep(NA,sims)
y0.mean <- rep(NA,sims)
set.seed(201)
for (i in 1:sims){
  rand.treat <- sample(treat.vector, size=length(data.test$fundus), replace=T)
  y1 <- data.test$fundus[rand.treat==1]
  y0 <- data.test$fundus[rand.treat==0]
  y1.mean[i] <- mean(y1, na.rm=T)
  y0.mean[i] <- mean(y0, na.rm=T)
}
te <- y1.mean - y0.mean
p <- sum((te > diff.ob))/sims #0.2436

#Analysis (Foreign\Public Goods Prompt)
data.test <- subset(data2.willing, ce==0 & priv==0)
diff.ob <- mean(data.test$fundfor[data.test$pub==1],na.rm=T) - mean(data.test$fundfor[data.test$pub==0], na.rm=T)
diff.ob #0.05448668
sims <- 5000
treat.vector <- c(0,1)
y1.mean <- rep(NA,sims)
y0.mean <- rep(NA,sims)
set.seed(202)
for (i in 1:sims){
  rand.treat <- sample(treat.vector, size=length(data.test$fundfor), replace=T)
  y1 <- data.test$fundfor[rand.treat==1]
  y0 <- data.test$fundfor[rand.treat==0]
  y1.mean[i] <- mean(y1, na.rm=T)
  y0.mean[i] <- mean(y0, na.rm=T)
}
te <- y1.mean - y0.mean
p <- sum((te > diff.ob))/sims #0.0294

#Analysis (Domestic/Provisioning Benefits)
diff.ob <- mean(data2.w.pub$fundus[data2.w.pub$priv==1],na.rm=T) - mean(data2.w.pub$fundus[data2.w.pub$priv==0], na.rm=T)
diff.ob #0.01037057
sims <- 5000
treat.vector <- c(0,1)
y1.mean <- rep(NA,sims)
y0.mean <- rep(NA,sims)
set.seed(201)
for (i in 1:sims){
  rand.treat <- sample(treat.vector, size=length(data2.w.pub$fundus), replace=T)
  y1 <- data2.w.pub$fundus[rand.treat==1]
  y0 <- data2.w.pub$fundus[rand.treat==0]
  y1.mean[i] <- mean(y1, na.rm=T)
  y0.mean[i] <- mean(y0, na.rm=T)
}
te <- y1.mean - y0.mean
p <- sum((te > diff.ob))/sims #0.2058

#Analysis (Foreign/Provisioning Benefits)
diff.ob <- mean(data2.w.pub$fundfor[data2.w.pub$priv==1],na.rm=T) - mean(data2.w.pub$fundfor[data2.w.pub$priv==0], na.rm=T)
diff.ob #-0.01988511
sims <- 5000
treat.vector <- c(0,1)
y1.mean <- rep(NA,sims)
y0.mean <- rep(NA,sims)
for (i in 1:sims){
  rand.treat <- sample(treat.vector, size=length(data2.w.pub$fundfor), replace=T)
  y1 <- data2.w.pub$fundfor[rand.treat==1]
  y0 <- data2.w.pub$fundfor[rand.treat==0]
  y1.mean[i] <- mean(y1, na.rm=T)
  y0.mean[i] <- mean(y0, na.rm=T)
}
te <- y1.mean - y0.mean
p <- sum((te < diff.ob))/sims #0.161

#Plotting
country2 <- rep(c("US","Foreign"),3)
country2 <- ordered(country2, levels=c("US","Foreign"))
prop.ps=c(mean(data2.willing$fundus[data2.willing$pub==0],na.rm=T),
          mean(data2.willing$fundfor[data2.willing$pub==0],na.rm=T),
          mean(data2.willing$fundus[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0],na.rm=T),
          mean(data2.willing$fundfor[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0],na.rm=T),
          mean(data2.willing$fundus[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0],na.rm=T),
          mean(data2.willing$fundfor[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0],na.rm=T))

n.ps=c(length(data2.willing$fundus[data2.willing$pub==0]),
       length(data2.willing$fundfor[data2.willing$pub==0]),
       length(data2.willing$fundus[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0]),
       length(data2.willing$fundfor[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0]),
       length(data2.willing$fundus[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0]),
       length(data2.willing$fundfor[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0]))

d <- data.frame(matrix(nrow=10000))[,-1] #Creating empty dataframe to fill with bootstrapped means
set.seed(201)
for (i in 1:10000){
  d$yc.mean.us[i] <- mean(sample(data2.willing$fundus[data2.willing$pub==0], replace=T, size=n.donate[1]), na.rm=T)
  d$yc.mean.foreign[i] <- mean(sample(data2.willing$fundfor[data2.willing$pub==0], replace=T, size=n.donate[2]), na.rm=T)
  d$y0.mean.us[i] <- mean(sample(data2.willing$fundus[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0], replace=T, size=n.donate[3]), na.rm=T)
  d$y0.mean.foreign[i] <- mean(sample(data2.willing$fundfor[data2.willing$pub==1 & data2.willing$priv==0 & data2.willing$ce==0], replace=T, size=n.donate[4]), na.rm=T)
  d$y1.mean.us[i] <- mean(sample(data2.willing$fundus[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0], replace=T, size=n.donate[5]), na.rm=T)
  d$y1.mean.foreign[i] <- mean(sample(data2.willing$fundfor[data2.willing$pub==1 & data2.willing$priv==1 & data2.willing$ce==0], replace=T, size=n.donate[6]), na.rm=T)
}
se.ps=c(sd(d$yc.mean.us),sd(d$yc.mean.foreign),sd(d$y0.mean.us),sd(d$y0.mean.foreign),sd(d$y1.mean.us),sd(d$y1.mean.foreign))

cond2 <- c("Pure Control","Pure Control","Global Good","Global Good","Global Good + Local Co-Benefit","Global Good + Local Co-Benefit")
cond2 <- ordered(cond, levels=c("Pure Control","Global Good","Global Good + Local Co-Benefit"))
prop.dta2 <- data.frame(prop.ps,se.ps,country2,cond2)
se.bars2 <- aes(ymax = prop.ps + se.ps, ymin = prop.ps - se.ps)
facet_names <- list(
  'US'="US",
  'Foreign'="Foreign"
)
facet_labeller <- function(variable,value){
  return(facet_names[value])
}

fig3.b <- ggplot(data=prop.dta2, aes(x=cond2, y=prop.ps)) + 
  geom_point(stat="identity", colour=rev(c("black","black","black","steelblue1","steelblue1","steelblue1")), size=4) + 
  geom_errorbar(se.bars2, width=0.2) + facet_wrap(~country2, labeller=facet_labeller, scales = "free") +
  xlab("Treatment Condition") + ylab("Proportion Stated Support") + ggtitle("(B) Exp 2: Public Spending") +
  theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=11)) + 
  scale_x_discrete(labels=c("Pure Control" = "Pure Control", "Global Good" = "Global Good", "Global Good + Local Co-Benefit" = "Global Good +\nLocal Co-Benefit"))


######################################################################################
######################################################################################
###Supporting Information
######################################################################################
######################################################################################

######################################################################################
###Table S1: Characteristics of Experimental Sample
######################################################################################

##Study 1
sum(data.willing$gender==2, na.rm=T)/length(data.willing$gender[!is.na(data.willing$gender)]) #Female
sum(data.willing$education>=5, na.rm=T)/length(data.willing$education[!is.na(data.willing$education)]) #College Graduates
sum(data.willing$employed==4, na.rm=T)/length(data.willing$employed[!is.na(data.willing$employed)]) #Unemployed
sum(data.willing$race==1, na.rm=T)/length(data.willing$race[!is.na(data.willing$race)]) #White
sum(data.willing$party==2, na.rm=T)/length(data.willing$party[!is.na(data.willing$party)]) #Democrat
sum(data.willing$party==1, na.rm=T)/length(data.willing$party[!is.na(data.willing$party)]) #Republican
sum(data.willing$party==3, na.rm=T)/length(data.willing$party[!is.na(data.willing$party)]) #Independent

##Study 2
sum(data2.willing$Q55==2, na.rm=T)/length(data2.willing$Q55[!is.na(data2.willing$Q55)]) #Female
sum(data2.willing$Q115>=5, na.rm=T)/length(data2.willing$Q115[!is.na(data2.willing$Q115)]) #College Graduates
sum(data2.willing$Q70.1==4, na.rm=T)/length(data2.willing$Q70.1[!is.na(data2.willing$Q70.1)]) #Unemployed
sum(data2.willing$Q141==1, na.rm=T)/length(data2.willing$Q141[!is.na(data2.willing$Q141)]) #White
sum(data2.willing$Q142==2, na.rm=T)/length(data2.willing$Q142[!is.na(data2.willing$Q142)]) #Democrat
sum(data2.willing$Q142==1, na.rm=T)/length(data2.willing$Q142[!is.na(data2.willing$Q142)]) #Republican
sum(data2.willing$Q142==3, na.rm=T)/length(data2.willing$Q142[!is.na(data2.willing$Q142)]) #Independent


######################################################################################
###Table S3: Climate opinions of national samples regarding climate harm
######################################################################################

#Ex1
prop.table(table(data.willing$bin.cc.serious.US))
prop.table(table(data.willing$bin.cc.serious.foreign))

#Ex2
#Q202 is perception of seriousness of climate change for US
#Q203 is perception of seriousness of climate change for foreign countries
#Response categories: 1=very serious problem; 2=somewhat serious problem; 3=less serious problem; 4=not a problem; 5=not sure;
prop.table(table(data2.willing$Q202))
prop.table(table(data2.willing$Q203))


######################################################################################
###Table S4: Treatment Assignments in Experiment 1
######################################################################################

#Donation outcome (d)
table(data.willing$country, data.willing$effect1)

#Letter-writing outcome (l)
table(data.click.willing$country, data.click.willing$effect1)


######################################################################################
###Table S5: Treatment Assignments in Experiment 2
######################################################################################

nrow(data2.willing[data2.willing$pub==0,]) #Pure control
nrow(data2.willing[data2.willing$ce==0 & data2.willing$priv==0 & data2.willing$pub==1,]) #Provisioning=0 & CE=0
nrow(data2.willing[data2.willing$ce==1 & data2.willing$priv==0,]) #Provisioning=0 & CE=1
nrow(data2.willing[data2.willing$ce==0 & data2.willing$priv==1,]) #Provisioning=1 & CE=0
nrow(data2.willing[data2.willing$ce==1 & data2.willing$priv==1,]) #Provisioning=1 & CE=1


######################################################################################
###Table S6: Treatment Assignments in Ex1 for subjects unwilling to donate
######################################################################################

#Total
table(data.willing$effect1, data.willing$country)

#"no" stated
data.no <- subset(data.willing, wri.stated==0)
table(data.no$effect1, data.no$country)

#bonus
data.bonus <- subset(data.no, randForce==1)
table(data.bonus$effect1, data.bonus$country)

#donated
data.donated <- subset(data.bonus, wri.fullbonus.final==1)
table(data.donated$effect1, data.donated$country)


######################################################################################
###Table S7: Manipulation checks for cost-effectiveness treatments
######################################################################################

#Study 1
sub <- subset(data.willing, country=="US")
prop.table(table(sub$effect1,sub$recall.ce),1)

sub <- subset(data.willing, country=="India")
prop.table(table(sub$effect1,sub$recall.ce),1)

sub <- subset(data.willing, country=="China")
prop.table(table(sub$effect1,sub$recall.ce),1)

#Study 2
tab <- table(data2.willing$ce,data2.willing$manipcheck) #1:Neither; 4:Rapidly Developing more; 3:Not sure; 5:US more; 6:both CE 
prop.table(tab, 1)


######################################################################################
###Table S8: Manipulation checks for cost-effectiveness treatments among non-college graduates
######################################################################################

#Study 1
sub <- subset(data.willing.no.colgrad, country=="US")
prop.table(table(sub$effect1,sub$recall.ce),1)

sub <- subset(data.willing.no.colgrad, country=="India")
prop.table(table(sub$effect1,sub$recall.ce),1)

sub <- subset(data.willing.no.colgrad, country=="China")
prop.table(table(sub$effect1,sub$recall.ce),1)

#Study 2
tab <- table(data2.w.no.colgrad$ce, data2.w.no.colgrad$manipcheck) #1:Neither; 4:Rapidly Developing more; 3:Not sure; 5:US more; 6:both CE 
prop.table(tab, 1)


######################################################################################
###Figure S1: Discouraging Public Spending in Experiment 1
######################################################################################

#Analysis
data.china.test <- subset(data.click.willing, country!="India" & effect1==0)
data.china.test$treat.china <- ifelse(data.china.test$country=="China",1,0)
mean(data.china.test$govt.discourage.rep.final[data.china.test$treat.china==0]) #0.01265823
mean(data.china.test$govt.discourage.rep.final[data.china.test$treat.china==1]) #0.05031447
perms <- genperms(data.china.test$treat.china) # all possible permutations of treatment
ate <- estate(data.china.test$govt.discourage.rep.final, data.china.test$treat.china) #point estimate of the ATE (simple in this case)
Ys <- genouts(data.china.test$govt.discourage.rep.final, data.china.test$treat.china, ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys,perms) # generate sampling distribution under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

data.india.test <- subset(data.click.willing, country!="China" & effect1==0)
data.india.test$treat.india <- ifelse(data.india.test$country=="India",1,0)
mean(data.india.test$govt.discourage.rep.final[data.india.test$treat.india==0]) #0.01265823
mean(data.india.test$govt.discourage.rep.final[data.india.test$treat.india==1]) #0.02777778
perms <- genperms(data.india.test$treat.india) # all possible permutations of treatment
ate <- estate(data.india.test$govt.discourage.rep.final, data.india.test$treat.india) #point estimate of the ATE (simple in this case)
Ys <- genouts(data.india.test$govt.discourage.rep.final, data.india.test$treat.india, ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys,perms) # generate sampling distribution under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

data.us.ce.test <- subset(data.click.willing, country=="US")
mean(data.us.ce.test$govt.discourage.rep.final[data.us.ce.test$effect1==0]) #0.01265823
mean(data.us.ce.test$govt.discourage.rep.final[data.us.ce.test$effect1==1]) #0.006802721
perms <- genperms(data.us.ce.test$effect1) # all possible permutations of treatment
ate <- estate(data.us.ce.test$govt.discourage.rep.final, data.us.ce.test$effect1) #point estimate of the ATE (simple in this case)
Ys <- genouts(data.us.ce.test$govt.discourage.rep.final, data.us.ce.test$effect1, ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys,perms) # generate sampling distribution under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

data.china.ce.test <- subset(data.click.willing, country=="China")
mean(data.china.ce.test$govt.discourage.rep.final[data.china.ce.test$effect1==0]) #0.05031447
mean(data.china.ce.test$govt.discourage.rep.final[data.china.ce.test$effect1==1]) #0.03921569
perms <- genperms(data.china.ce.test$effect1) # all possible permutations of treatment
ate <- estate(data.china.ce.test$govt.discourage.rep.final, data.china.ce.test$effect1) #point estimate of the ATE (simple in this case)
Ys <- genouts(data.china.ce.test$govt.discourage.rep.final, data.china.ce.test$effect1, ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys,perms) # generate sampling distribution under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

data.india.ce.test <- subset(data.click.willing, country=="India")
mean(data.india.ce.test$govt.discourage.rep.final[data.india.ce.test$effect1==0]) #0.02777778
mean(data.india.ce.test$govt.discourage.rep.final[data.india.ce.test$effect1==1]) #0.04705882
perms <- genperms(data.india.ce.test$effect1) # all possible permutations of treatment
ate <- estate(data.india.ce.test$govt.discourage.rep.final, data.india.ce.test$effect1) #point estimate of the ATE (simple in this case)
Ys <- genouts(data.india.ce.test$govt.discourage.rep.final, data.india.ce.test$effect1, ate=0) # generate potential outcomes under sharp null
distout <- gendist(Ys,perms) # generate sampling distribution under sharp null
dispdist(distout, ate) # display characteristics of sampling dist. for inference

#Plotting
ce = c(0.05,-0.05,0,1.05,0.95,1)

us.e.ce0 <- data.click.willing$govt.discourage.rep.final[data.click.willing$country=="US" & data.click.willing$effect1==0]
china.e.ce0 <- data.click.willing$govt.discourage.rep.final[data.click.willing$country=="China" & data.click.willing$effect1==0]
india.e.ce0 <- data.click.willing$govt.discourage.rep.final[data.click.willing$country=="India" & data.click.willing$effect1==0]
us.e.ce1 <- data.click.willing$govt.discourage.rep.final[data.click.willing$country=="US" & data.click.willing$effect1==1]
china.e.ce1 <- data.click.willing$govt.discourage.rep.final[data.click.willing$country=="China" & data.click.willing$effect1==1]
india.e.ce1 <- data.click.willing$govt.discourage.rep.final[data.click.willing$country=="India" & data.click.willing$effect1==1]

prop.encourage=c(sum(us.e.ce0) / length(us.e.ce0),
                 sum(china.e.ce0) / length(china.e.ce0),
                 sum(india.e.ce0) / length(india.e.ce0),
                 sum(us.e.ce1) / length(us.e.ce1),
                 sum(china.e.ce1) / length(china.e.ce1),
                 sum(india.e.ce1) / length(india.e.ce1)
)

se.pub <- c(sd(rbinom(size=length(us.e.ce0), n=10000, prob=mean(us.e.ce0)) / length(us.e.ce0)),
            sd(rbinom(size=length(china.e.ce0), n=10000, prob=mean(china.e.ce0)) / length(china.e.ce0)),
            sd(rbinom(size=length(india.e.ce0), n=10000, prob=mean(india.e.ce0)) / length(india.e.ce0)),
            sd(rbinom(size=length(us.e.ce1), n=10000, prob=mean(us.e.ce1)) / length(us.e.ce1)),
            sd(rbinom(size=length(china.e.ce1), n=10000, prob=mean(china.e.ce1)) / length(china.e.ce1)),
            sd(rbinom(size=length(india.e.ce1), n=10000, prob=mean(india.e.ce1)) / length(india.e.ce1))
)

#pdf("FigS1_JEPS-Final.pdf", width=4, height=4)
par(mfrow=c(1,1),mar=c(2.1,4.1,2.1,1.1),mgp=c(2,1,0))
plot(x=ce,y=prop.encourage,xaxt="n",xlim=c(-0.3,1.3),ylim=c(0.00,0.08),ylab="Proportion Writing to Discourage Public Spending \n and Clicking Representative Link",xlab="",
     main="Discourage Public Spending", col=c("steelblue1","indianred2","khaki","steelblue1","indianred2","khaki"), pch=19,cex=1.5)

lines(x=c(0.05,1.05), y=c(sum(us.e.ce0) / length(us.e.ce0), sum(us.e.ce1) / length(us.e.ce1)), col="steelblue1", lwd=2)
lines(x=c(-0.05,0.95), y=c(sum(china.e.ce0) / length(china.e.ce0), sum(china.e.ce1) / length(china.e.ce1)), col="indianred2", lwd=2)
lines(x=c(0,1), y=c(sum(india.e.ce0) / length(india.e.ce0), sum(india.e.ce1) / length(india.e.ce1)), col="khaki", lwd=2)
arrows(ce, prop.encourage-se.pub, ce, prop.encourage+se.pub, length=0.05, angle=90, code=3)
axis(1,at=c(0,1),labels=c("No Info","CE Info"))
legend("top", legend=c("China","India","US"), cex=1, 
       fill=c("indianred2","khaki","steelblue1"), bty = "n")
#dev.off()


######################################################################################
###Table S9: Binary and Donation Amount in Experiment 2
######################################################################################

any <- lm(donate.any ~ ce*priv, data=data2.w.pub)
summary(any)

total <- lm(donate.total ~ ce*priv, data=data2.w.pub)
summary(total)

stargazer(any, total, type = "html", 
          dep.var.labels = c("Donate Any (0|1)  ", "Donate Amount ($)"),
          covariate.labels = c("Cost-Effectiveness","Provisioning","Cost-Effectiveness * Provisioning","Intercept"),
          df = FALSE, omit.stat = c("rsq","ser")
)


######################################################################################
###Table S10: Multinomial regression of donation type in Experiment 2
######################################################################################

multi <- multinom(donate.multi ~ ce*priv, data=data2.w.pub)
summary(multi)
coeftest(multi)

stargazer(multi, type = "html", 
          dep.var.labels = c("US Only", "Foreign Only", "Neither"),
          covariate.labels = c("Cost-Effectiveness","Provisioning","Cost-Effectiveness * Provisioning","Intercept"),
          df = FALSE, omit.stat = c("rsq","ser")
)


######################################################################################
###Table S11: Proportion of donation directed to US in Experiment 2f
######################################################################################

prop <- lm(donate.prop.us ~ ce*priv, data=data2.w.any)
summary(prop)

stargazer(prop, type = "html", 
          dep.var.labels = c("Proportion to US"),
          covariate.labels = c("Cost-Effectiveness","Provisioning","Cost-Effectiveness * Provisioning","Intercept"),
          df = FALSE, omit.stat = c("rsq","ser")
)

######################################################################################
###Figure S2: Home preference among self-described Democrats
######################################################################################

##Donation 1
#Note: Overwriting objects from main to avoid recoding plotting code
#Note: Rerun setup blocks if going back to any of the analyses/plots above

data.willing <- subset(data,cc.not.willing==0 & effect1==0 & party==2)
data.willing.us.ce0 <- subset(data.willing,country=="US")
data.willing.india.ce0 <- subset(data.willing,country=="India")
data.willing.china.ce0 <- subset(data.willing,country=="China")

no.vector.us.ce0<-data.willing.us.ce0$wri.fullbonus.final[data.willing.us.ce0$wri.stated==0 & data.willing.us.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.china.ce0<-data.willing.china.ce0$wri.fullbonus.final[data.willing.china.ce0$wri.stated==0 & data.willing.china.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.india.ce0<-data.willing.india.ce0$wri.fullbonus.final[data.willing.india.ce0$wri.stated==0 & data.willing.india.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part

prob.switch.us.ce0 <- sum(no.vector.us.ce0)/length(no.vector.us.ce0) #This is the probability of donating after stating "no"
prob.switch.china.ce0 <- sum(no.vector.china.ce0)/length(no.vector.china.ce0) #This is the probability of donating after stating "no"
prob.switch.india.ce0 <- sum(no.vector.india.ce0)/length(no.vector.india.ce0) #This is the probability of donating after stating "no"

#Setting up for the simulation
imp <- data.willing$wri.fullbonus.final
treat.country <- unique(data.willing$country)
sims <- 10000

us.ob.ce0 <- rep(NA,sims)
china.ob.ce0 <- rep(NA,sims)
india.ob.ce0 <- rep(NA,sims)

ate.china <- rep(NA,sims)
ate.india <- rep(NA,sims)

#The sampling distribution for the ATE under the sharp null is formed over repeated imputations
#This imputes a full data.willing set, sampling from the observed values for donating after stating "no"
set.seed(201)
for (j in 1:sims){
  
  for (i in 1:nrow(data.willing)){
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="US"){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.us.ce0)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="China"){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.china.ce0)}
    if(is.na(data.willing$wri.fullbonus.final)[i] & data.willing$country[i]=="India"){
      imp[i] <- rbinom(n=1,size=1,prob=prob.switch.india.ce0)}
  }
  
  treatment.country <- sample(treat.country, size=nrow(data.willing), replace=TRUE)
  
  us.ob.ce0[j] <- mean(imp[data.willing$country=="US" & data.willing$effect1==0])
  china.ob.ce0[j] <- mean(imp[data.willing$country=="China" & data.willing$effect1==0])
  india.ob.ce0[j]  <- mean(imp[data.willing$country=="India" & data.willing$effect1==0])
  
  ate.china[j] <- mean(imp[treatment.country=="China" & data.willing$effect1==0]) - mean(imp[treatment.country=="US" & data.willing$effect1==0])
  ate.india[j] <- mean(imp[treatment.country=="India" & data.willing$effect1==0]) - mean(imp[treatment.country=="US" & data.willing$effect1==0])
}

#Means after imputation:
mean(us.ob.ce0) #0.3014809
mean(india.ob.ce0) #0.3039405
mean(china.ob.ce0) #0.1948053

#SEs of mean value (this is different than SEs of imputation)
set.seed(201)
se <- c(sd(rbinom(size=nrow(data.willing.us.ce0), n=10000, prob=0.3014809)/nrow(data.willing.us.ce0)),
        sd(rbinom(size=nrow(data.willing.india.ce0), n=10000, prob=0.3039405)/nrow(data.willing.india.ce0)),   
        sd(rbinom(size=nrow(data.willing.china.ce0), n=10000, prob=0.1948053)/nrow(data.willing.china.ce0))
)
se <- c(0.04856768,0.05338192,0.04081898) #Based exactly on above

country <- rev(c("US","India","China"))
prop.donate<-rev(c(0.3014809,0.3039405,0.1948053)) #From "Means after imputation" directly above
se <- rev(se)
prop.dta <- data.frame(prop.donate,country,se)
se.bars <- aes(ymax = prop.donate + se, ymin = prop.donate - se)

figS2.a<-ggplot(data=prop.dta, aes(x=country, y=prop.donate)) + theme_grey() + scale_x_discrete(limits = country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","khaki","indianred2")), colour="black") + geom_errorbar(se.bars, width=0.3) +
  ylab("Proportion Donating") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(A) Exp 1: Private Donation") + theme(plot.title = element_text(lineheight=1, face="bold"))

##Donation 2
data2.w.pure.control.dem <- subset(data2.w.pure.control, Q142==2)
dta2 <- melt(data2.w.pure.control.dem, measure.vars = c('donate.foreign.amount','donate.us.amount')) #Make the object as we want it, selecting final two columns
dta2 <- dta2[,(ncol(dta2)-1):ncol(dta2)]

names(dta2) <- c("Country","Amount")
dta2$Country <- ifelse(dta2$Country=="donate.foreign.amount","Foreign","US")
mean.info <- ddply(dta2, "Country", summarise, donate.mean=mean(Amount))

foreign.store <- rep(NA, 10000)
US.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  foreign.store[i] <- mean(sample(dta2$Amount[1:243], size=243, replace=T))
  US.store[i] <- mean(sample(dta2$Amount[244:486], size=243, replace=T))
}
mean.info$se[1] <- sd(foreign.store)
mean.info$se[2] <- sd(US.store)
se.bars2 <- aes(ymax = mean.info$donate.mean + mean.info$se, ymin = mean.info$donate.mean - mean.info$se)

figS2.b<-ggplot(mean.info, aes(x=Country, y=donate.mean)) + theme_grey() + scale_x_discrete(limits = mean.info$Country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","darkslategrey")), colour="black") + geom_errorbar(se.bars2, width=0.3) +
  ylab("Amount Donated ($)") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(B) Exp 2: Private Donation") + theme(plot.title = element_text(lineheight=1, face="bold"))

##Public Spending 1
data.click.willing.dem <- subset(data.click.willing, party==2)
tab <- as.data.frame.matrix(table(data.click.willing.dem$country, data.click.willing.dem$govt.encourage.rep.final))
names(tab) <- c("X0","X1")
tab$n <- tab$X1 + tab$X0
tab$prop.govt.encourage <- tab$X1/(tab$X0+tab$X1)

us.store <- rep(NA, 10000)
india.store <- rep(NA, 10000)
china.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  us.store[i] <- mean(rbinom(n=tab$n[1], size=1, prob=tab$prop.govt.encourage[1]))
  india.store[i] <- mean(rbinom(n=tab$n[2], size=1, prob=tab$prop.govt.encourage[2]))
  china.store[i] <- mean(rbinom(n=tab$n[3], size=1, prob=tab$prop.govt.encourage[3]))
}
tab$se[1] <- sd(us.store)
tab$se[2] <- sd(india.store)
tab$se[3] <- sd(china.store)
tab$country <- row.names(tab)
tab <- tab[order(tab$country),]
se.bars.ps1 <- aes(ymax = tab$prop.govt.encourage + tab$se, ymin = tab$prop.govt.encourage - tab$se)

figS2.c<-ggplot(tab, aes(x=country, y=prop.govt.encourage)) + theme_grey() + scale_x_discrete(limits = tab$country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","khaki","indianred2")), colour="black") + geom_errorbar(se.bars.ps1, width=0.3) +
  ylab("Proportion Writing to Support") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(C) Exp 1: Public Spending") + theme(plot.title = element_text(lineheight=1, face="bold"))

##Public Spending 2
data2.w.pure.control.dem <- subset(data2.w.pure.control, Q142==2)
dta4 <- melt(data2.w.pure.control.dem, measure.vars = c("fundus","fundfor")) #Make the object as we want it, selecting final two columns
dta4 <- dta4[,(ncol(dta4)-1):ncol(dta4)]
names(dta4) <- c("Country","Write")
dta4$Country <- ifelse(dta4$Country=="fundfor","Foreign","US")
mean.info4 <- ddply(dta4, "Country", summarise, write.mean=mean(Write, na.rm=T))

foreign.store <- rep(NA, 10000)
US.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  foreign.store[i] <- mean(sample(dta4$Write[1:243], size=243, replace=T), na.rm=T)
  US.store[i] <- mean(sample(dta4$Write[244:486], size=243, replace=T), na.rm=T)
}
mean.info4$se[1] <- sd(foreign.store)
mean.info4$se[2] <- sd(US.store)
se.bars4 <- aes(ymax = mean.info4$write.mean + mean.info4$se, ymin = mean.info4$write.mean - mean.info4$se)

figS2.d<-ggplot(mean.info4, aes(x=Country, y=write.mean)) + theme_grey() + scale_x_discrete(limits = mean.info4$Country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","darkslategrey")), colour="black") + geom_errorbar(se.bars4, width=0.3) +
  ylab("Proportion Support") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(D) Exp 2: Public Spending") + theme(plot.title = element_text(lineheight=1, face="bold"))

S2 <- plot_grid(figS2.a, figS2.b, figS2.c, figS2.d)


######################################################################################
###Figure S3: Home preference among self-described Republicans
######################################################################################

##Donation 1
#Note: Overwriting objects from main to avoid recoding plotting code
#Note: Rerun setup blocks if going back to any of the analyses/plots above
data.willing <- subset(data,cc.not.willing==0 & effect1==0 & party==1)
data.willing.us.ce0 <- subset(data.willing,country=="US")
data.willing.india.ce0 <- subset(data.willing,country=="India")
data.willing.china.ce0 <- subset(data.willing,country=="China")

no.vector.us.ce0<-data.willing.us.ce0$wri.fullbonus.final[data.willing.us.ce0$wri.stated==0 & data.willing.us.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.china.ce0<-data.willing.china.ce0$wri.fullbonus.final[data.willing.china.ce0$wri.stated==0 & data.willing.china.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.india.ce0<-data.willing.india.ce0$wri.fullbonus.final[data.willing.india.ce0$wri.stated==0 & data.willing.india.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part

prob.switch.us.ce0 <- sum(no.vector.us.ce0)/length(no.vector.us.ce0) #This is the probability of donating after stating "no"
prob.switch.china.ce0 <- sum(no.vector.china.ce0)/length(no.vector.china.ce0) #This is the probability of donating after stating "no"
prob.switch.india.ce0 <- sum(no.vector.india.ce0)/length(no.vector.india.ce0) #This is the probability of donating after stating "no"

#Setting up for the simulation
imp <- data.willing$wri.fullbonus.final
table(imp)
#Note: insufficient number of observations (11) for the imputation to run behavioral result
#Note: switching to stated intention to vote

prop.donate <- c(sum(data.willing.us.ce0$wri.stated)/length(data.willing.us.ce0$wri.stated),
                 sum(data.willing.india.ce0$wri.stated)/length(data.willing.india.ce0$wri.stated),
                 sum(data.willing.china.ce0$wri.stated)/length(data.willing.china.ce0$wri.stated))

#SEs of prop.donate
set.seed(201)
se <- c(sd(rbinom(size=nrow(data.willing.us.ce0), n=10000, prob=0.03846154)/nrow(data.willing.us.ce0)),
        sd(rbinom(size=nrow(data.willing.india.ce0), n=10000, prob=0.14814815)/nrow(data.willing.india.ce0)),   
        sd(rbinom(size=nrow(data.willing.china.ce0), n=10000, prob=0.08333333)/nrow(data.willing.china.ce0))
) #Note: probabilities come from prop.donate

se <- c(0.03757992,0.06833109,0.05643995) #Based exactly on above

country <- rev(c("US","India","China"))
prop.donate<-rev(c(0.03846154,0.14814815,0.08333333)) #From "Means after imputation" directly above
se <- rev(c(0.03757992,0.06833109,0.05643995)) #From "se" directly above
prop.dta <- data.frame(prop.donate,country,se)
se.bars <- aes(ymax = prop.donate + se, ymin = prop.donate - se)

figS3.a<-ggplot(data=prop.dta, aes(x=country, y=prop.donate)) + theme_grey() + scale_x_discrete(limits = country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","khaki","indianred2")), colour="black") + geom_errorbar(se.bars, width=0.3) +
  ylab("Proportion Willing to Donate") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(A) Exp 1: Private Donation") + theme(plot.title = element_text(lineheight=1, face="bold"))

##Donation 2
data2.w.pure.control.rep <- subset(data2.w.pure.control, Q142==1)
dta2 <- melt(data2.w.pure.control.rep, measure.vars = c('donate.foreign.amount','donate.us.amount'))
dta2 <- dta2[,(ncol(dta2)-1):ncol(dta2)]
names(dta2) <- c("Country","Amount")
dta2$Country <- ifelse(dta2$Country=="donate.foreign.amount","Foreign","US")
mean.info <- ddply(dta2, "Country", summarise, donate.mean=mean(Amount))

foreign.store <- rep(NA, 10000)
US.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  foreign.store[i] <- mean(sample(dta2$Amount[1:74], size=74, replace=T))
  US.store[i] <- mean(sample(dta2$Amount[75:148], size=74, replace=T))
}
mean.info$se[1] <- sd(foreign.store)
mean.info$se[2] <- sd(US.store)
se.bars2 <- aes(ymax = mean.info$donate.mean + mean.info$se, ymin = mean.info$donate.mean - mean.info$se)

figS3.b<-ggplot(mean.info, aes(x=Country, y=donate.mean)) + theme_grey() + scale_x_discrete(limits = mean.info$Country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","darkslategrey")), colour="black") + geom_errorbar(se.bars2, width=0.3) +
  ylab("Amount Donated ($)") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(B) Exp 2: Private Donation") + theme(plot.title = element_text(lineheight=1, face="bold"))

##Public Spending 1
data.click.willing.rep <- subset(data.click.willing, party==1)
tab <- as.data.frame.matrix(table(data.click.willing.rep$country, data.click.willing.rep$govt.encourage.rep.final))
names(tab) <- c("X0","X1")
tab$n <- tab$X1 + tab$X0
tab$prop.govt.encourage <- tab$X1/(tab$X0+tab$X1)

us.store <- rep(NA, 10000)
india.store <- rep(NA, 10000)
china.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  us.store[i] <- mean(rbinom(n=tab$n[1], size=1, prob=tab$prop.govt.encourage[1]))
  india.store[i] <- mean(rbinom(n=tab$n[2], size=1, prob=tab$prop.govt.encourage[2]))
  china.store[i] <- mean(rbinom(n=tab$n[3], size=1, prob=tab$prop.govt.encourage[3]))
}
tab$se[1] <- sd(us.store)
tab$se[2] <- sd(india.store)
tab$se[3] <- sd(china.store)
tab$country <- row.names(tab)
tab <- tab[order(tab$country),]
se.bars.ps1 <- aes(ymax = tab$prop.govt.encourage + tab$se, ymin = tab$prop.govt.encourage - tab$se)

figS3.c<-ggplot(tab, aes(x=country, y=prop.govt.encourage)) + theme_grey() + scale_x_discrete(limits = tab$country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","khaki","indianred2")), colour="black") + geom_errorbar(se.bars.ps1, width=0.3) +
  ylab("Proportion Writing to Support") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(C) Exp 1: Public Spending") + theme(plot.title = element_text(lineheight=1, face="bold"))

##Public Spending 2
data2.w.pure.control.rep <- subset(data2.w.pure.control, Q142==1)
dta4 <- melt(data2.w.pure.control.rep, measure.vars = c("fundus","fundfor"))
dta4 <- dta4[,(ncol(dta4)-1):ncol(dta4)]
names(dta4) <- c("Country","Write")
dta4$Country <- ifelse(dta4$Country=="fundfor","Foreign","US")
mean.info4 <- ddply(dta4, "Country", summarise, write.mean=mean(Write, na.rm=T))

foreign.store <- rep(NA, 10000)
US.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  foreign.store[i] <- mean(sample(dta4$Write[1:74], size=74, replace=T), na.rm=T)
  US.store[i] <- mean(sample(dta4$Write[75:148], size=74, replace=T), na.rm=T)
}
mean.info4$se[1] <- sd(foreign.store)
mean.info4$se[2] <- sd(US.store)
se.bars4 <- aes(ymax = mean.info4$write.mean + mean.info4$se, ymin = mean.info4$write.mean - mean.info4$se)

figS3.d<-ggplot(mean.info4, aes(x=Country, y=write.mean)) + theme_grey() + scale_x_discrete(limits = mean.info4$Country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","darkslategrey")), colour="black") + geom_errorbar(se.bars4, width=0.3) +
  ylab("Proportion Support") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(D) Exp 2: Public Spending") + theme(plot.title = element_text(lineheight=1, face="bold"))

S3 <- plot_grid(figS3.a,figS3.b,figS3.c,figS3.d)


######################################################################################
###Figure S4: Home preference among self-described Independents
######################################################################################

##Donation 1
#Overwriting objects from main ClimateTransfers.R analysis to keep from recoding
data.willing <- subset(data,cc.not.willing==0 & effect1==0 & party==3)
data.willing.us.ce0 <- subset(data.willing,country=="US")
data.willing.india.ce0 <- subset(data.willing,country=="India")
data.willing.china.ce0 <- subset(data.willing,country=="China")

no.vector.us.ce0<-data.willing.us.ce0$wri.fullbonus.final[data.willing.us.ce0$wri.stated==0 & data.willing.us.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.china.ce0<-data.willing.china.ce0$wri.fullbonus.final[data.willing.china.ce0$wri.stated==0 & data.willing.china.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part
no.vector.india.ce0<-data.willing.india.ce0$wri.fullbonus.final[data.willing.india.ce0$wri.stated==0 & data.willing.india.ce0$randForce==1] #These are the observations of the "no" respondents who were forced to the bonus part

#Note: probability of switching for independents is always zero in our sample

data.willing$wri.fullbonus.final[is.na(data.willing$wri.fullbonus.final)] <- 0
data.willing.us.ce0 <- subset(data.willing,country=="US")
data.willing.india.ce0 <- subset(data.willing,country=="India")
data.willing.china.ce0 <- subset(data.willing,country=="China")

prop.donate <- c(sum(data.willing.us.ce0$wri.fullbonus.final)/length(data.willing.us.ce0$wri.fullbonus.final),
                 sum(data.willing.india.ce0$wri.fullbonus.final)/length(data.willing.india.ce0$wri.fullbonus.final),
                 sum(data.willing.china.ce0$wri.fullbonus.final)/length(data.willing.china.ce0$wri.fullbonus.final))

#SEs of prop.donate
set.seed(201)
se <- c(sd(rbinom(size=nrow(data.willing.us.ce0), n=10000, prob=0.18309859)/nrow(data.willing.us.ce0)),
        sd(rbinom(size=nrow(data.willing.india.ce0), n=10000, prob=0.14492754)/nrow(data.willing.india.ce0)),   
        sd(rbinom(size=nrow(data.willing.china.ce0), n=10000, prob=0.07017544)/nrow(data.willing.china.ce0))
) #Note: probabilities come from prop.donate

se <- c(0.04574577,0.04237056,0.03401052) #Based exactly on above

country <- rev(c("US","India","China"))
prop.donate<-rev(prop.donate) #From "prop.donate" directly above
se <- rev(se) #From "se" directly above
prop.dta <- data.frame(prop.donate,country,se)
se.bars <- aes(ymax = prop.donate + se, ymin = prop.donate - se)

figS4.a<-ggplot(data=prop.dta, aes(x=country, y=prop.donate)) + theme_grey() + scale_x_discrete(limits = country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","khaki","indianred2")), colour="black") + geom_errorbar(se.bars, width=0.3) +
  ylab("Proportion Donating") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(A) Exp 1: Private Donation") + theme(plot.title = element_text(lineheight=1, face="bold"))

##Donation 2
data2.w.pure.control.ind <- subset(data2.w.pure.control, Q142==3)
dta2 <- melt(data2.w.pure.control.ind, measure.vars = c('donate.foreign.amount','donate.us.amount'))
dta2 <- dta2[,(ncol(dta2)-1):ncol(dta2)]
names(dta2) <- c("Country","Amount")
dta2$Country <- ifelse(dta2$Country=="donate.foreign.amount","Foreign","US")
mean.info <- ddply(dta2, "Country", summarise, donate.mean=mean(Amount))

foreign.store <- rep(NA, 10000)
US.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  foreign.store[i] <- mean(sample(dta2$Amount[1:196], size=196, replace=T))
  US.store[i] <- mean(sample(dta2$Amount[197:392], size=196, replace=T))
}
mean.info$se[1] <- sd(foreign.store)
mean.info$se[2] <- sd(US.store)
se.bars2 <- aes(ymax = mean.info$donate.mean + mean.info$se, ymin = mean.info$donate.mean - mean.info$se)

figS4.b<-ggplot(mean.info, aes(x=Country, y=donate.mean)) + theme_grey() + scale_x_discrete(limits = mean.info$Country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","darkslategrey")), colour="black") + geom_errorbar(se.bars2, width=0.3) +
  ylab("Amount Donated ($)") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(B) Exp 2: Private Donation") + theme(plot.title = element_text(lineheight=1, face="bold"))

##Public Spending 1
data.click.willing.ind <- subset(data.click.willing, party==3)
tab <- as.data.frame.matrix(table(data.click.willing.ind$country, data.click.willing.ind$govt.encourage.rep.final))
names(tab) <- c("X0","X1")
tab$n <- tab$X1 + tab$X0
tab$prop.govt.encourage <- tab$X1/(tab$X0+tab$X1)

us.store <- rep(NA, 10000)
india.store <- rep(NA, 10000)
china.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  us.store[i] <- mean(rbinom(n=tab$n[1], size=1, prob=tab$prop.govt.encourage[1]))
  india.store[i] <- mean(rbinom(n=tab$n[2], size=1, prob=tab$prop.govt.encourage[2]))
  china.store[i] <- mean(rbinom(n=tab$n[3], size=1, prob=tab$prop.govt.encourage[3]))
}
tab$se[1] <- sd(us.store)
tab$se[2] <- sd(india.store)
tab$se[3] <- sd(china.store)
tab$country <- row.names(tab)
tab <- tab[order(tab$country),]
se.bars.ps1 <- aes(ymax = tab$prop.govt.encourage + tab$se, ymin = tab$prop.govt.encourage - tab$se)

figS4.c<-ggplot(tab, aes(x=country, y=prop.govt.encourage)) + theme_grey() + scale_x_discrete(limits = tab$country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","khaki","indianred2")), colour="black") + geom_errorbar(se.bars.ps1, width=0.3) +
  ylab("Proportion Writing to Support") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(C) Exp 1: Public Spending") + theme(plot.title = element_text(lineheight=1, face="bold"))

##Public Spending 2
data2.w.pure.control.ind <- subset(data2.w.pure.control, Q142==3)
dta4 <- melt(data2.w.pure.control.ind, measure.vars = c("fundus","fundfor"))
dta4 <- dta4[,(ncol(dta4)-1):ncol(dta4)]
names(dta4) <- c("Country","Write")
dta4$Country <- ifelse(dta4$Country=="fundfor","Foreign","US")
mean.info4 <- ddply(dta4, "Country", summarise, write.mean=mean(Write, na.rm=T))

foreign.store <- rep(NA, 10000)
US.store <- rep(NA, 10000)
set.seed(201)
for (i in 1:10000){
  foreign.store[i] <- mean(sample(dta4$Write[1:196], size=196, replace=T), na.rm=T)
  US.store[i] <- mean(sample(dta4$Write[197:392], size=196, replace=T), na.rm=T)
}
mean.info4$se[1] <- sd(foreign.store)
mean.info4$se[2] <- sd(US.store)
se.bars4 <- aes(ymax = mean.info4$write.mean + mean.info4$se, ymin = mean.info4$write.mean - mean.info4$se)

figS4.d<-ggplot(mean.info4, aes(x=Country, y=write.mean)) + theme_grey() + scale_x_discrete(limits = mean.info4$Country) +
  geom_bar(stat="identity", fill=rev(c("steelblue1","darkslategrey")), colour="black") + geom_errorbar(se.bars4, width=0.3) +
  ylab("Proportion Support") + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12, colour="black")) + xlab("") +
  coord_flip() +
  ggtitle("(D) Exp 2: Public Spending") + theme(plot.title = element_text(lineheight=1, face="bold"))

S4 <- plot_grid(figS4.a,figS4.b,figS4.c,figS4.d)

