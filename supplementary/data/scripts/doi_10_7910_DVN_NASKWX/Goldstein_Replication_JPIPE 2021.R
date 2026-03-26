
#Clear global environment
rm(list = ls())

#Load Packages 
require(foreign)
require(Hmisc)
require(ggplot2)
require(gplots)
require(xtable)
require(plyr)
require(haven)
require(stargazer) 
require(visreg)
require(sandwich)
require(lmtest)
require(gridExtra)

#Set Working Directory 
setwd("~/Desktop/Replication File Data")


#Figure 1, Figure 2, Table 1, Table 2, Appendix A

##Load Data Figure 1, Figure 2, Table 1, Table 2, Appendix A
data <- read.csv("CCES_Data.csv")
attach(data)

##Define Common Variables Figure 1, Figure 2, Table 1, Table 2, Appendix A 
age <- 2016-data$birthyr
under30 <- ifelse(age<30,1,0)
age30to44 <- ifelse(age>=30&age<45,1,0)
age45to64 <- ifelse(age>=45&age<65,1,0)
over65 <- ifelse(age>=65,1,0)

agecat <- c()
agecat[under30==1] <- 1
agecat[age30to44==1] <- 2
agecat[age45to64==1] <- 3
agecat[over65==1] <- 4
agecat <- as.factor(agecat)

comwt <- data$commonweight

tvnews <- data$CC16_300b
tvnews <- ifelse(data$CC16_300b=="Local Newscast"|data$CC16_300b=="Both",1,0)

probcrime <- data$CC16_301j
crimehighimp <- ifelse(probcrime=="Very High Importance" | probcrime=="Somewhat High Importance",1,0)
crimehighimp[probcrime==""] <- NA

vic <- data$CC16_305_9

victim <- ifelse(vic=="Yes",1,0)
victim[vic==""] <- NA

white <- ifelse(data$race=="White",1,0)
black <- ifelse(data$race=="Black",1,0)
hisp <- ifelse(data$race=="Hispanic",1,0)
asi <- ifelse(data$race=="Asian",1,0)
racegrp <- c()
racegrp[white==1] <- 1
racegrp[black==1] <- 2
racegrp[hisp==1] <- 3
racegrp[asi==1] <- 4

male <- ifelse(data$gender=="Male",1,0)

child18 <- ifelse(data$child18=="Yes",1,0)

incunder30 <- ifelse(data$faminc=="Less than $10,000"|data$faminc=="$10,000 - $19,999" | data$faminc=="$20,000 - $29,999",1,0)

inc30to60 <- ifelse(data$faminc=="$30,000 - $39,999" | data$faminc=="$40,000 - $49,999" | data$faminc=="$50,000 - $59,999",1,0)

inc60to100 <- ifelse(data$faminc=="$60,000 - $69,999" | data$faminc=="$70,000 - $79,999" | data$faminc=="$80,000 - $99,999",1,0)

incover100 <- ifelse(data$faminc=="$100,000 - $119,999" | data$faminc=="$120,000 - $149,999" | data$faminc=="$150,000 - $199,999" | data$faminc=="$200,000 - $249,999" | data$faminc=="$250,000 - $349,999" | data$faminc=="$350,000 - $499,999" | data$faminc=="$500,000 or more",1,0)

incnosay <- ifelse(data$faminc=="Prefer not to say",1,0)
inc <- c()
inc[incunder30==1] <- 1
inc[inc30to60==1] <- 2
inc[inc60to100==1] <- 3
inc[incover100==1] <- 4
inc[incnosay==1] <- NA

own <- ifelse(data$ownhome=="Own",1,0)
rent <- ifelse(data$ownhome=="Rent",1,0)

urban <- ifelse(data$class==1|data$class==2,1,0)
smallmet <- ifelse(data$class==3|data$class==4,1,0)
rural <- ifelse(data$class==5|data$class==6,1,0)

urban.3 <- c()
urban.3[urban==1] <- 3
urban.3[smallmet==1] <- 2
urban.3[rural==1] <- 1
urban.3[is.na(data$class)] <- NA

nohs <- ifelse(data$educ=="No HS",1,0)
hs <- ifelse(data$educ=="High school graduate",1,0)
somecoll <- ifelse(data$educ=="2-year" | data$educ=="Some college",1,0)
coll <- ifelse(data$educ=="4-year",1,0)
postgrad <- ifelse(data$educ=="Post-grad",1,0)

ed <- c()
ed[nohs==1] <- 1
ed[hs==1] <- 2
ed[somecoll==1] <- 3
ed[coll==1] <- 4
ed[postgrad==1] <- 5

incpolice <- ifelse(data$CC16_334c=="Support",1,0)
incpolice[data$CC16_334c==""] <- NA

incpolice <- as.integer(incpolice)

policefeel <- ifelse(data$CC16_307=="Mostly safe" | data$CC16_307=="Somewhat safe",1,0)
policefeel[data$CC16_307==""] <- NA

ch18 <- child18

racegrp <- c()
racegrp[white==1] <- "1 White"
racegrp[black==1] <- "2 Black"
racegrp[hisp==1] <- "3 Hispanic"
racegrp[asi==1] <- "4 Asian"
racegrp <- as.factor(racegrp)

part <- ifelse(data$CC16_417a_1=="Yes" | data$CC16_417a_2=="Yes" | data$CC16_417a_3=="Yes" | data$CC16_417a_4=="Yes",1,0)
part[data$CC16_417a_1==""&data$CC16_417a_2==""&data$CC16_417a_3==""&data$CC16_417a_4==""] <- NA

state <- data$inputstate

dem <- ifelse(data$pid7=="Strong Democrat" | data$pid7=="Not very strong Democrat" | data$pid7=="Lean Democrat", 1,0)
repub <- ifelse(data$pid7=="Strong Republican" | data$pid7=="Not very strong Republican" | data$pid7=="Lean Republican", 1,0)

vicrime <- data$viol_rate
propcrime <- data$prop_rate

agecat1 <- weighted.mean(incpolice[agecat==1],w=comwt[agecat==1],na.rm=T)
agecat2 <- weighted.mean(incpolice[agecat==2],w=comwt[agecat==2],na.rm=T)
agecat3 <- weighted.mean(incpolice[agecat==3],w=comwt[agecat==3],na.rm=T)
agecat4 <- weighted.mean(incpolice[agecat==4],w=comwt[agecat==4],na.rm=T)
blackmean <- weighted.mean(incpolice[black==1],w=comwt[black==1],na.rm=T)
whitemean <- weighted.mean(incpolice[white==1],w=comwt[white==1],na.rm=T)
hispmean <- weighted.mean(incpolice[hisp==1],w=comwt[hisp==1],na.rm=T)
asimean <- weighted.mean(incpolice[asi==1],w=comwt[asi==1],na.rm=T)
femalemean <- weighted.mean(incpolice[male==0],w=comwt[male==0],na.rm=T)
malemean <- weighted.mean(incpolice[male==1],w=comwt[male==1],na.rm=T)
nohs.mean <- weighted.mean(incpolice[nohs==1],w=comwt[nohs==1],na.rm=T)
hs.mean <- weighted.mean(incpolice[hs==1],w=comwt[hs==1],na.rm=T)
somecoll.mean <- weighted.mean(incpolice[somecoll==1],w=comwt[somecoll==1],na.rm=T)
coll.mean <- weighted.mean(incpolice[coll==1],w=comwt[coll==1],na.rm=T)
postgrad.mean <- weighted.mean(incpolice[postgrad==1],w=comwt[postgrad==1],na.rm=T)
dem.mean <- weighted.mean(incpolice[dem==1],w=comwt[dem==1],na.rm=T)
rep.mean <- weighted.mean(incpolice[repub==1],w=comwt[repub==1],na.rm=T)
indep.mean <- weighted.mean(incpolice[dem==0&repub==0],w=comwt[dem==0&repub==0],na.rm=T)
incunder30.mean <- weighted.mean(incpolice[incunder30==1],w=comwt[incunder30==1],na.rm=T)
inc30to60.mean <- weighted.mean(incpolice[inc30to60==1],w=comwt[inc30to60==1],na.rm=T)
inc60to100.mean <- weighted.mean(incpolice[inc60to100==1],w=comwt[inc60to100==1],na.rm=T)
incover100.mean <- weighted.mean(incpolice[incover100==1],w=comwt[incover100==1],na.rm=T)
urban.mean <- weighted.mean(incpolice[urban==1],w=comwt[urban==1],na.rm=T)
smallmet.mean <- weighted.mean(incpolice[smallmet==1],w=comwt[smallmet==1],na.rm=T)
rural.mean <- weighted.mean(incpolice[rural==1],w=comwt[rural==1],na.rm=T)
ch18.mean <- weighted.mean(incpolice[ch18==1],w=comwt[ch18==1],na.rm=T)
noch18.mean <- weighted.mean(incpolice[ch18==0],w=comwt[ch18==0],na.rm=T)
own.mean <- weighted.mean(incpolice[own==1],w=comwt[own==1],na.rm=T)
no.own.mean <- weighted.mean(incpolice[own==0],w=comwt[own==0],na.rm=T)

black.agecat1 <- weighted.mean(incpolice[black==1&agecat==1],w=comwt[black==1&agecat==1],na.rm=T)
black.agecat2 <- weighted.mean(incpolice[black==1&agecat==2],w=comwt[black==1&agecat==2],na.rm=T)
black.agecat3 <- weighted.mean(incpolice[black==1&agecat==3],w=comwt[black==1&agecat==3],na.rm=T)
black.agecat4 <- weighted.mean(incpolice[black==1&agecat==4],w=comwt[black==1&agecat==4],na.rm=T)

black.age1.sd <- sqrt((black.agecat1*(1-black.agecat1))/length(incpolice[black==1&agecat==1]))
black.age2.sd <- sqrt((black.agecat2*(1-black.agecat2))/length(incpolice[black==1&agecat==2]))
black.age3.sd <- sqrt((black.agecat3*(1-black.agecat3))/length(incpolice[black==1&agecat==3]))
black.age4.sd <- sqrt((black.agecat4*(1-black.agecat4))/length(incpolice[black==1&agecat==4]))

white.agecat1 <- weighted.mean(incpolice[white==1&agecat==1],w=comwt[white==1&agecat==1],na.rm=T)
white.agecat2 <- weighted.mean(incpolice[white==1&agecat==2],w=comwt[white==1&agecat==2],na.rm=T)
white.agecat3 <- weighted.mean(incpolice[white==1&agecat==3],w=comwt[white==1&agecat==3],na.rm=T)
white.agecat4 <- weighted.mean(incpolice[white==1&agecat==4],w=comwt[white==1&agecat==4],na.rm=T)

white.age1.sd <- sqrt((white.agecat1*(1-white.agecat1))/length(incpolice[white==1&agecat==1]))
white.age2.sd <- sqrt((white.agecat2*(1-white.agecat2))/length(incpolice[white==1&agecat==2]))
white.age3.sd <- sqrt((white.agecat3*(1-white.agecat3))/length(incpolice[white==1&agecat==3]))
white.age4.sd <- sqrt((white.agecat4*(1-white.agecat4))/length(incpolice[white==1&agecat==4]))

hisp.agecat1 <- weighted.mean(incpolice[hisp==1&agecat==1],w=comwt[hisp==1&agecat==1],na.rm=T)
hisp.agecat2 <- weighted.mean(incpolice[hisp==1&agecat==2],w=comwt[hisp==1&agecat==2],na.rm=T)
hisp.agecat3 <- weighted.mean(incpolice[hisp==1&agecat==3],w=comwt[hisp==1&agecat==3],na.rm=T)
hisp.agecat4 <- weighted.mean(incpolice[hisp==1&agecat==4],w=comwt[hisp==1&agecat==4],na.rm=T)

hisp.age1.sd <- sqrt((hisp.agecat1*(1-hisp.agecat1))/length(incpolice[hisp==1&agecat==1]))
hisp.age2.sd <- sqrt((hisp.agecat2*(1-hisp.agecat2))/length(incpolice[hisp==1&agecat==2]))
hisp.age3.sd <- sqrt((hisp.agecat3*(1-hisp.agecat3))/length(incpolice[hisp==1&agecat==3]))
hisp.age4.sd <- sqrt((hisp.agecat4*(1-hisp.agecat4))/length(incpolice[hisp==1&agecat==4]))

asi.agecat1 <- weighted.mean(incpolice[asi==1&agecat==1],w=comwt[asi==1&agecat==1],na.rm=T)
asi.agecat2 <- weighted.mean(incpolice[asi==1&agecat==2],w=comwt[asi==1&agecat==2],na.rm=T)
asi.agecat3 <- weighted.mean(incpolice[asi==1&agecat==3],w=comwt[asi==1&agecat==3],na.rm=T)
asi.agecat4 <- weighted.mean(incpolice[asi==1&agecat==4],w=comwt[asi==1&agecat==4],na.rm=T)

asi.age1.sd <- sqrt((asi.agecat1*(1-asi.agecat1))/length(incpolice[asi==1&agecat==1]))
asi.age2.sd <- sqrt((asi.agecat2*(1-asi.agecat2))/length(incpolice[asi==1&agecat==2]))
asi.age3.sd <- sqrt((asi.agecat3*(1-asi.agecat3))/length(incpolice[asi==1&agecat==3]))
asi.age4.sd <- sqrt((asi.agecat4*(1-asi.agecat4))/length(incpolice[asi==1&agecat==4]))

black.opinion <- round(c(black.agecat1,black.agecat2,black.agecat3,black.agecat4),2)
white.opinion <- round(c(white.agecat1,white.agecat2,white.agecat3,white.agecat4),2)
hisp.opinion <- round(c(hisp.agecat1,hisp.agecat2,hisp.agecat3,hisp.agecat4),2)
asi.opinion <- round(c(asi.agecat1,asi.agecat2,asi.agecat3,asi.agecat4),2)

black.age.sds <- c(black.age1.sd,black.age2.sd,black.age3.sd,black.age4.sd)
white.age.sds <- c(white.age1.sd,white.age2.sd,white.age3.sd,white.age4.sd)
hisp.age.sds <- c(hisp.age1.sd,hisp.age2.sd,hisp.age3.sd,hisp.age4.sd)
asi.age.sds <- c(asi.age1.sd,asi.age2.sd,asi.age3.sd,asi.age4.sd)

black.agecat1.var <- (black.agecat1*(1-black.agecat1))/length(incpolice[black==1&agecat==1])
black.agecat2.var <- (black.agecat2*(1-black.agecat2))/length(incpolice[black==1&agecat==2])
black.agecat3.var <- (black.agecat3*(1-black.agecat3))/length(incpolice[black==1&agecat==3])
black.agecat4.var <- (black.agecat4*(1-black.agecat4))/length(incpolice[black==1&agecat==4])

white.agecat1.var <- (white.agecat1*(1-white.agecat1))/length(incpolice[white==1&agecat==1])
white.agecat2.var <- (white.agecat2*(1-white.agecat2))/length(incpolice[white==1&agecat==2])
white.agecat3.var <- (white.agecat3*(1-white.agecat3))/length(incpolice[white==1&agecat==3])
white.agecat4.var <- (white.agecat4*(1-white.agecat4))/length(incpolice[white==1&agecat==4])

hisp.agecat1.var <- (hisp.agecat1*(1-hisp.agecat1))/length(incpolice[hisp==1&agecat==1])
hisp.agecat2.var <- (hisp.agecat2*(1-hisp.agecat2))/length(incpolice[hisp==1&agecat==2])
hisp.agecat3.var <- (hisp.agecat3*(1-hisp.agecat3))/length(incpolice[hisp==1&agecat==3])
hisp.agecat4.var <- (hisp.agecat4*(1-hisp.agecat4))/length(incpolice[hisp==1&agecat==4])

asi.agecat1.var <- (asi.agecat1*(1-asi.agecat1))/length(incpolice[asi==1&agecat==1])
asi.agecat2.var <- (asi.agecat2*(1-asi.agecat2))/length(incpolice[asi==1&agecat==2])
asi.agecat3.var <- (asi.agecat3*(1-asi.agecat3))/length(incpolice[asi==1&agecat==3])
asi.agecat4.var <- (asi.agecat4*(1-asi.agecat4))/length(incpolice[asi==1&agecat==4])

opinion <- c(white.opinion,black.opinion,hisp.opinion,asi.opinion)
races.sds <- c(white.age.sds,black.age.sds,hisp.age.sds,asi.age.sds)

Races <-c(rep("White", 4), rep("Black", 4), rep("Hispanic",4), rep("Asian",4))
agecats <- c("<30","30-44","45-64","65+","<30","30-44","45-64","65+","<30","30-44","45-64","65+","<30","30-44","45-64","65+")

shagecat1.white <- count(white[agecat==1],wt_var="comwt[agecat==1]")[2,2]/count(white,wt_var="comwt")[2,2]
shagecat2.white <- count(white[agecat==2],wt_var="comwt[agecat==2]")[2,2]/count(white,wt_var="comwt")[2,2]
shagecat3.white <- count(white[agecat==3],wt_var="comwt[agecat==3]")[2,2]/count(white,wt_var="comwt")[2,2]
shagecat4.white <- count(white[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(white,wt_var="comwt")[2,2]

shagecat1.black <- count(black[agecat==1],wt_var="comwt[agecat==1]")[2,2]/count(black,wt_var="comwt")[2,2]
shagecat2.black<- count(black[agecat==2],wt_var="comwt[agecat==2]")[2,2]/count(black,wt_var="comwt")[2,2]
shagecat3.black<- count(black[agecat==3],wt_var="comwt[agecat==3]")[2,2]/count(black,wt_var="comwt")[2,2]
shagecat4.black<- count(black[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(black,wt_var="comwt")[2,2]

shagecat1.hisp <- count(hisp[agecat==1],wt_var="comwt[agecat==1]")[2,2]/count(hisp,wt_var="comwt")[2,2]
shagecat2.hisp<- count(hisp[agecat==2],wt_var="comwt[agecat==2]")[2,2]/count(hisp,wt_var="comwt")[2,2]
shagecat3.hisp<- count(hisp[agecat==3],wt_var="comwt[agecat==3]")[2,2]/count(hisp,wt_var="comwt")[2,2]
shagecat4.hisp<- count(hisp[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(hisp,wt_var="comwt")[2,2]

shagecat1.asi <- count(asi[agecat==1],wt_var="comwt[agecat==1]")[2,2]/count(asi,wt_var="comwt")[2,2]
shagecat2.asi<- count(asi[agecat==2],wt_var="comwt[agecat==2]")[2,2]/count(asi,wt_var="comwt")[2,2]
shagecat3.asi<- count(asi[agecat==3],wt_var="comwt[agecat==3]")[2,2]/count(asi,wt_var="comwt")[2,2]
shagecat4.asi<- count(asi[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(asi,wt_var="comwt")[2,2]

shagecats.white <- c(shagecat1.white,shagecat2.white,shagecat3.white,shagecat4.white)
shagecats.black <- c(shagecat1.black,shagecat2.black,shagecat3.black,shagecat4.black)
shagecats.hisp<- c(shagecat1.hisp,shagecat2.hisp,shagecat3.hisp,shagecat4.hisp)
shagecats.asi<- c(shagecat1.asi,shagecat2.asi,shagecat3.asi,shagecat4.asi)

shagecats.races <- c(shagecats.white,shagecats.black,shagecats.hisp,shagecats.asi)

white.meanop <- sum((shagecats.races*opinion)[1:4])
black.meanop <- sum((shagecats.races*opinion)[5:8])
hisp.meanop <- sum((shagecats.races*opinion)[9:12])
asi.meanop <- sum((shagecats.races*opinion)[13:16])

black.meanop.adj <- sum(shagecats.races[1:4]*opinion[5:8])
black.meanop.adj.vec <- shagecats.races[1:4]*opinion[5:8]
hisp.meanop.adj <- sum(shagecats.races[1:4]*opinion[9:12])
hisp.meanop.adj.vec <- shagecats.races[1:4]*opinion[9:12]
asi.meanop.adj <- sum(shagecats.races[1:4]*opinion[13:16])
asi.meanop.adj.vec <- (shagecats.races[1:4]*opinion[13:16])

black.var <- c(black.agecat1.var,black.agecat2.var,black.agecat3.var,black.agecat4.var)
hisp.var <- c(hisp.agecat1.var,hisp.agecat2.var,hisp.agecat3.var,hisp.agecat4.var)
asi.var <- c(asi.agecat1.var,asi.agecat2.var,asi.agecat3.var,asi.agecat4.var)

black.sd.adj <- sqrt(sum((shagecats.races[1:4]^2)*black.var))
hisp.sd.adj <- sqrt(sum((shagecats.races[1:4]^2)*hisp.var))
asi.sd.adj <- sqrt(sum((shagecats.races[1:4]^2)*asi.var))

black.sd <- sqrt((black.meanop*(1-black.meanop))/length(incpolice[black==1]))
white.sd <- sqrt((white.meanop*(1-white.meanop))/length(incpolice[white==1]))
hisp.sd <- sqrt((hisp.meanop*(1-hisp.meanop))/length(incpolice[hisp==1]))
asi.sd <- sqrt((asi.meanop*(1-asi.meanop))/length(incpolice[asi==1]))

meanop.adj <- c(black.meanop,black.meanop.adj,hisp.meanop,hisp.meanop.adj,asi.meanop,asi.meanop.adj)
races.foradj <- c("Black","Black (adjusted)","Hispanic","Hispanic (adjusted)","Asian","Asian (adjusted)")
sds.adj <- c(black.sd,black.sd.adj,hisp.sd,hisp.sd.adj,asi.sd,asi.sd.adj)

white <- as.factor(white)
black <- as.factor(black)
hisp <- as.factor(hisp)
asi <- as.factor(asi)
urban.3 <- as.factor(urban.3) 
inc <- as.factor(inc) 
ch18 <- as.factor(ch18)
own <- as.factor(own)
dem <- as.factor(dem)
ed <- as.factor(ed)
log.vicrime <- log(vicrime+1) 
log.propcrime <- log(propcrime+1) 

mainreg.col3 <- lm(incpolice~agecat+racegrp+dem+male+urban.3+(inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)
mainreg.col2 <- lm(incpolice~agecat+racegrp+as.factor(state),weights=comwt)
mainreg.col1 <- lm(incpolice~agecat+as.factor(state),weights=comwt)

shasian.agecat1 <- count(under30[asi==1],wt_var="comwt[asi==1]")[2,2]/count(under30,wt_var="comwt")[2,2]
shasian.agecat2 <- count(age30to44[asi==1],wt_var="comwt[asi==1]")[2,2]/count(age30to44,wt_var="comwt")[2,2]
shasian.agecat3 <- count(age45to64[asi==1],wt_var="comwt[asi==1]")[2,2]/count(age45to64,wt_var="comwt")[2,2]
shasian.agecat4 <- count(over65[asi==1],wt_var="comwt[asi==1]")[2,2]/count(over65,wt_var="comwt")[2,2]

shwhite.agecat1 <- count(under30[white==1],wt_var="comwt[white==1]")[2,2]/count(under30,wt_var="comwt")[2,2]
shwhite.agecat2 <- count(age30to44[white==1],wt_var="comwt[white==1]")[2,2]/count(age30to44,wt_var="comwt")[2,2]
shwhite.agecat3 <- count(age45to64[white==1],wt_var="comwt[white==1]")[2,2]/count(age45to64,wt_var="comwt")[2,2]
shwhite.agecat4 <- count(over65[white==1],wt_var="comwt[white==1]")[2,2]/count(over65,wt_var="comwt")[2,2]

shblack.agecat1 <- count(under30[black==1],wt_var="comwt[black==1]")[2,2]/count(under30,wt_var="comwt")[2,2]
shblack.agecat2 <- count(age30to44[black==1],wt_var="comwt[black==1]")[2,2]/count(age30to44,wt_var="comwt")[2,2]
shblack.agecat3 <- count(age45to64[black==1],wt_var="comwt[black==1]")[2,2]/count(age45to64,wt_var="comwt")[2,2]
shblack.agecat4 <- count(over65[black==1],wt_var="comwt[black==1]")[2,2]/count(over65,wt_var="comwt")[2,2]

shhisp.agecat1 <- count(under30[hisp==1],wt_var="comwt[hisp==1]")[2,2]/count(under30,wt_var="comwt")[2,2]
shhisp.agecat2 <- count(age30to44[hisp==1],wt_var="comwt[hisp==1]")[2,2]/count(age30to44,wt_var="comwt")[2,2]
shhisp.agecat3 <- count(age45to64[hisp==1],wt_var="comwt[hisp==1]")[2,2]/count(age45to64,wt_var="comwt")[2,2]
shhisp.agecat4 <- count(over65[hisp==1],wt_var="comwt[hisp==1]")[2,2]/count(over65,wt_var="comwt")[2,2]

races.under30 <- c(shwhite.agecat1,shblack.agecat1,shhisp.agecat1,shasian.agecat1)
races.age30to44 <- c(shwhite.agecat2,shblack.agecat2,shhisp.agecat2,shasian.agecat2)
races.age45to64 <- c(shwhite.agecat3,shblack.agecat3,shhisp.agecat3,shasian.agecat3)
races.65plus <- c(shwhite.agecat4,shblack.agecat4,shhisp.agecat4,shasian.agecat4)

age2 <- age^2

mainreg.continuous <- lm(incpolice~age+age2+racegrp+dem+male+urban.3+(inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)

robust.ses.1 <- coeftest(mainreg.col1, vcov = vcovHC(mainreg.col1, type="HC1"))

robust.ses.2 <- coeftest(mainreg.col2, vcov = vcovHC(mainreg.col2, type="HC1"))

robust.ses.3 <- coeftest(mainreg.col3, vcov = vcovHC(mainreg.col3, type="HC1"))

mainreg.interaction <- lm(incpolice~agecat*racegrp+dem+male+urban.3+
                            (inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)

mainreg.col3.interaction <- lm(incpolice~agecat*racegrp+dem+male+urban.3+
                                 (inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)

mainreg.col2.interaction <- lm(incpolice~agecat*racegrp+as.factor(state),weights=comwt)

mainreg.col1.interaction <- lm(incpolice~agecat+as.factor(state),weights=comwt)

pred.policefeel <- lm(policefeel~agecat+racegrp+dem+male+urban.3+(inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)

pred.crimehighimp <- lm(crimehighimp~agecat+racegrp+dem+male+urban.3+(inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)

mainreg.wcrimehighimp <- lm(incpolice~crimehighimp+agecat+racegrp+dem+male+urban.3+(inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)

overall.mean <- weighted.mean(tvnews,w=comwt,na.rm=T)

under30.mean <- weighted.mean(tvnews[under30==1],w=comwt[under30==1],na.rm=T)
age30to44.mean <- weighted.mean(tvnews[age30to44==1],w=comwt[age30to44==1],na.rm=T)
age45to64.mean <- weighted.mean(tvnews[age45to64==1],w=comwt[age45to64==1],na.rm=T)

over65.mean <- weighted.mean(tvnews[over65==1],w=comwt[over65==1],na.rm=T)

under30.sd <- sqrt((under30.mean*(1-under30.mean))/length(tvnews[under30==1]))
age30to44.sd <- sqrt((age30to44.mean*(1-age30to44.mean))/length(tvnews[age30to44==1]))
age45to64.sd <- sqrt((age45to64.mean*(1-age45to64.mean))/length(tvnews[age45to64==1]))
over65.sd <- sqrt((over65.mean*(1-over65.mean))/length(tvnews[over65==1]))

means4 <- c(under30.mean,age30to44.mean,age45to64.mean,over65.mean)
ages4 <- c("18-29","30-44","45-64","65+")
sd4 <- c(under30.sd,age30to44.sd,age45to64.sd,over65.sd)

pred.tvnews <- lm(tvnews~agecat+racegrp+dem+male+urban.3+(inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)
mainreg.wtvnews <- lm(incpolice~tvnews+agecat+racegrp+dem+male+urban.3+(inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)

#Table 1
sumstats.labs <- c("Age under 30", "Age 30-44", "Age 45-64", "Age 65+",
                   "Black", "White", "Hispanic", "Asian", "Female", "Male", "No HS", "HS grad",
                   "Some college","College grad","Postgrad","Income under $30k","Income $30k-$60k",
                   "Income $60k-$100k","Income over $100k","Democrat","Republican","Independent",
                   "Child under 18", "No child under 18", "Homeowner", "Not a homeowner")

sumstats.vector <- c(agecat1,agecat2,agecat3,agecat4,blackmean,whitemean,hispmean,asimean,
                     femalemean,malemean,nohs.mean,hs.mean,somecoll.mean,coll.mean,postgrad.mean,
                     incunder30.mean,inc30to60.mean,inc60to100.mean,incover100.mean,dem.mean,rep.mean,indep.mean,
                     ch18.mean,noch18.mean,own.mean,no.own.mean) 

##Print Table 1
table1<-xtable(as.data.frame(cbind(sumstats.labs,round(sumstats.vector,2))))
table1

#Figure 1 
dat <- data.frame(agecats,Races,opinion,races.sds)

##Print Figure 1
figure1 <- ggplot(dat,aes(agecats,opinion,fill=Races))
figure1 <- figure1 +guides(fill=guide_legend(title=""))+xlab("Age groups") + ylab("")+ylim(0,1)+
  geom_bar(stat = "identity",position="dodge")+geom_text(aes(label=round(opinion,2)), 
                                                         vjust=7,position=position_dodge(width=.95),size=5) +
  geom_errorbar(aes(ymin=opinion-qnorm(.975)*races.sds, ymax=opinion+qnorm(.975)*races.sds), width=.2,
                position=position_dodge(.9))+ theme(
                  panel.background = element_rect(fill = "white"), 
                  plot.background = element_rect(fill = "white", color = "white"), 
                  panel.grid.major = element_blank(), 
                  panel.border = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  axis.line.x = element_line(color = "gray40"),
                  axis.text.x = element_text(size = 15), 
                  axis.title.y = element_text(size = 15), 
                  axis.text.y = element_text(size = 15),
                  axis.line.y = element_line(color = "gray40"),
                  #   legend.position = c(0.9, 0.2),
                  plot.title = element_text(hjust = 0.2))
figure1

#Figure 2
dat <- data.frame(meanop.adj,races.foradj,sds.adj)

##Print Figure 2
figure2 <- ggplot(dat,aes(races.foradj,meanop.adj,fill=races.foradj))
figure2 <- figure2 +guides(fill=guide_legend(title="")) + xlab("")+ylab("")+ylim(0,.9)+
  geom_hline(yintercept = white.meanop, col = "navy", size = 1, linetype = "dashed") +
  geom_bar(stat = "identity",position="dodge")+
  geom_errorbar(aes(ymin=meanop.adj-qnorm(.975)*sds.adj,ymax=meanop.adj+qnorm(.975)*sds.adj), width=.2,
                position=position_dodge(.9))+ 
  scale_fill_manual(values=c("#E69F00","#E69F00","#56B4E9","#56B4E9","#009E73","#009E73","#F0E442"))+
  theme(
    panel.background = element_rect(fill = "white"), 
    #  plot.background = element_rect(fill = "white", color = "white"), 
    # panel.grid.major = element_blank(), 
    panel.border = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line.x = element_line(color = "gray40"),
    axis.text.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15), 
    axis.text.y = element_text(size = 15))
figure2

#Table 2
labs <- c("Age 30-44", "Age 45-64", "Age 65+", "Black", "Hispanic",
          "Asian","Democrat","Male","Medium metro county", "Urban county",
          "Income $30k-$60k","Income $60k-$100k","Income over $100k",
          "HS grad","Some college", "College grad","Postgrad", "Child under 18",
          "Homeowner","Violent crime rate (log)")

##Print Table 2 
stargazer(mainreg.col1,mainreg.col2,mainreg.col3,omit="state",no.space=T,covariate.labels=labs)

#Appendix Table A1
agelabs <- c("Age under 30", "Age 30-44", "Age 45-64", "Age 65+")

##Print Appendix Table A1
tablea1 <- xtable(rbind(races.under30,races.age30to44,races.age45to64,races.65plus),caption = "Table A1: Racial makeup of four age groups (VAP only)")
rownames(tablea1) <- agelabs
colnames(tablea1) <- c("White","Black","Hispanic","Asian")
tablea1

#Appendix Table A2
agelabs <- c("Age under 30", "Age 30-44", "Age 45-64", "Age 65+")

##Print Appendix Table A2
tablea2 <- xtable(rbind(shagecats.white,shagecats.black,shagecats.hisp,shagecats.asi),caption="Table A2: VAP breakdown in four race groups")
colnames(tablea2) <- agelabs
rownames(tablea2) <- c("White","Black","Hispanic","Asian")
tablea2

#Appendix Table A3
labs.continuous <- c("Age", "Age squared", "Black", "Hispanic",
                     "Asian","Democrat","Male","Medium metro county", "Urban county",
                     "Income $30k-$60k","Income $60k-$100k","Income over $100k",
                     "HS grad","Some college", "College grad","Postgrad", "Child under 18",
                     "Homeowner","Violent crime rate (log)")

##Print Appendix Table A3
stargazer(mainreg.continuous,omit="state",no.space=T,covariate.labels=labs.continuous)

#Appendix Table A4

##Print Appendix Table A4
stargazer(mainreg.col1,mainreg.col2,mainreg.col3,omit="state",no.space=T,
                     covariate.labels=labs,se=list(robust.ses.1[,2],robust.ses.2[,2],robust.ses.3[,2]))

#Appendix Table A5
int.labs <- c("Age 30-44", "Age 45-64", "Age 65+", "Black", "Hispanic",
              "Asian","Democrat","Male","Medium metro county", "Urban county",
              "Income $30k-$60k","Income $60k-$100k","Income over $100k",
              "HS grad","Some college", "College grad","Postgrad", "Child under 18",
              "Homeowner","Violent crime rate (log)","Age 30-44:Black","Age 45-64:Black","Age 65+:Black",
              "Age 30-44:Hispanic","Age 45-64:Hispanic","Age 65+:Hispanic",
              "Age 30-44:Asian","Age 45-64:Asian","Age 65+:Asian")

##Print Appendix Table A5
stargazer(mainreg.col1.interaction, mainreg.col2.interaction, mainreg.col3.interaction, omit="state",no.space=T,covariate.labels=int.labs)

#Appendix Figure A1
black.agecat1 <- weighted.mean(policefeel[black==1&agecat==1],w=comwt[black==1&agecat==1],na.rm=T)
black.agecat2 <- weighted.mean(policefeel[black==1&agecat==2],w=comwt[black==1&agecat==2],na.rm=T)
black.agecat3 <- weighted.mean(policefeel[black==1&agecat==3],w=comwt[black==1&agecat==3],na.rm=T)
black.agecat4 <- weighted.mean(policefeel[black==1&agecat==4],w=comwt[black==1&agecat==4],na.rm=T)

black.age1.sd <- sqrt((black.agecat1*(1-black.agecat1))/length(policefeel[black==1&agecat==1]))
black.age2.sd <- sqrt((black.agecat2*(1-black.agecat2))/length(policefeel[black==1&agecat==2]))
black.age3.sd <- sqrt((black.agecat3*(1-black.agecat3))/length(policefeel[black==1&agecat==3]))
black.age4.sd <- sqrt((black.agecat4*(1-black.agecat4))/length(policefeel[black==1&agecat==4]))

white.agecat1 <- weighted.mean(policefeel[white==1&agecat==1],w=comwt[white==1&agecat==1],na.rm=T)
white.agecat2 <- weighted.mean(policefeel[white==1&agecat==2],w=comwt[white==1&agecat==2],na.rm=T)
white.agecat3 <- weighted.mean(policefeel[white==1&agecat==3],w=comwt[white==1&agecat==3],na.rm=T)
white.agecat4 <- weighted.mean(policefeel[white==1&agecat==4],w=comwt[white==1&agecat==4],na.rm=T)

white.age1.sd <- sqrt((white.agecat1*(1-white.agecat1))/length(policefeel[white==1&agecat==1]))
white.age2.sd <- sqrt((white.agecat2*(1-white.agecat2))/length(policefeel[white==1&agecat==2]))
white.age3.sd <- sqrt((white.agecat3*(1-white.agecat3))/length(policefeel[white==1&agecat==3]))
white.age4.sd <- sqrt((white.agecat4*(1-white.agecat4))/length(policefeel[white==1&agecat==4]))

hisp.agecat1 <- weighted.mean(policefeel[hisp==1&agecat==1],w=comwt[hisp==1&agecat==1],na.rm=T)
hisp.agecat2 <- weighted.mean(policefeel[hisp==1&agecat==2],w=comwt[hisp==1&agecat==2],na.rm=T)
hisp.agecat3 <- weighted.mean(policefeel[hisp==1&agecat==3],w=comwt[hisp==1&agecat==3],na.rm=T)
hisp.agecat4 <- weighted.mean(policefeel[hisp==1&agecat==4],w=comwt[hisp==1&agecat==4],na.rm=T)

hisp.age1.sd <- sqrt((hisp.agecat1*(1-hisp.agecat1))/length(policefeel[hisp==1&agecat==1]))
hisp.age2.sd <- sqrt((hisp.agecat2*(1-hisp.agecat2))/length(policefeel[hisp==1&agecat==2]))
hisp.age3.sd <- sqrt((hisp.agecat3*(1-hisp.agecat3))/length(policefeel[hisp==1&agecat==3]))
hisp.age4.sd <- sqrt((hisp.agecat4*(1-hisp.agecat4))/length(policefeel[hisp==1&agecat==4]))

asi.agecat1 <- weighted.mean(policefeel[asi==1&agecat==1],w=comwt[asi==1&agecat==1],na.rm=T)
asi.agecat2 <- weighted.mean(policefeel[asi==1&agecat==2],w=comwt[asi==1&agecat==2],na.rm=T)
asi.agecat3 <- weighted.mean(policefeel[asi==1&agecat==3],w=comwt[asi==1&agecat==3],na.rm=T)
asi.agecat4 <- weighted.mean(policefeel[asi==1&agecat==4],w=comwt[asi==1&agecat==4],na.rm=T)

asi.age1.sd <- sqrt((asi.agecat1*(1-asi.agecat1))/length(policefeel[asi==1&agecat==1]))
asi.age2.sd <- sqrt((asi.agecat2*(1-asi.agecat2))/length(policefeel[asi==1&agecat==2]))
asi.age3.sd <- sqrt((asi.agecat3*(1-asi.agecat3))/length(policefeel[asi==1&agecat==3]))
asi.age4.sd <- sqrt((asi.agecat4*(1-asi.agecat4))/length(policefeel[asi==1&agecat==4]))

black.opinion <- round(c(black.agecat1,black.agecat2,black.agecat3,black.agecat4),2)
white.opinion <- round(c(white.agecat1,white.agecat2,white.agecat3,white.agecat4),2)
hisp.opinion <- round(c(hisp.agecat1,hisp.agecat2,hisp.agecat3,hisp.agecat4),2)
asi.opinion <- round(c(asi.agecat1,asi.agecat2,asi.agecat3,asi.agecat4),2)

black.age.sds <- c(black.age1.sd,black.age2.sd,black.age3.sd,black.age4.sd)
white.age.sds <- c(white.age1.sd,white.age2.sd,white.age3.sd,white.age4.sd)
hisp.age.sds <- c(hisp.age1.sd,hisp.age2.sd,hisp.age3.sd,hisp.age4.sd)
asi.age.sds <- c(asi.age1.sd,asi.age2.sd,asi.age3.sd,asi.age4.sd)

opinion <- c(white.opinion,black.opinion,hisp.opinion,asi.opinion)
races.sds <- c(white.age.sds,black.age.sds,hisp.age.sds,asi.age.sds)

dat <- data.frame(agecats,Races,opinion,races.sds)

##Print Appendix Figure A1
figurea1 <- ggplot(dat,aes(agecats,opinion,fill=Races))
figurea1 <- figurea1 +guides(fill=guide_legend(title=""))+xlab("Age groups") + ylab("")+ylim(0,1)+
  geom_bar(stat = "identity",position="dodge")+geom_text(aes(label=round(opinion,2)), 
                                                         vjust=7,position=position_dodge(width=.95),size=5) +
  geom_errorbar(aes(ymin=opinion-qnorm(.975)*races.sds, ymax=opinion+qnorm(.975)*races.sds), width=.2,
                position=position_dodge(.9))+ theme(
                  panel.background = element_rect(fill = "white"), 
                  plot.background = element_rect(fill = "white", color = "white"), 
                  panel.grid.major = element_blank(), 
                  panel.border = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  axis.line.x = element_line(color = "gray40"),
                  axis.text.x = element_text(size = 15), 
                  axis.title.y = element_text(size = 15), 
                  axis.text.y = element_text(size = 15),
                  axis.line.y = element_line(color = "gray40"),
                  #   legend.position = c(0.9, 0.2),
                  plot.title = element_text(hjust = 0.2))
figurea1

#Appendix Table A6
policefeel.labs <- c("Age 30-44", "Age 45-64", "Age 65+", "Black", "Hispanic",
                     "Asian","Democrat","Male","Medium metro county", "Urban county",
                     "Income $30k-$60k","Income $60k-$100k","Income over $100k",
                     "HS grad","Some college", "College grad","Postgrad", "Child under 18",
                     "Homeowner","Violent crime rate (log)")

##Print Appendix Table A6
stargazer(pred.policefeel,omit="state",no.space=T,covariate.labels=policefeel.labs)

#Appendix Figure A2
black.agecat1 <- weighted.mean(crimehighimp[black==1&agecat==1],w=comwt[black==1&agecat==1],na.rm=T)
black.agecat2 <- weighted.mean(crimehighimp[black==1&agecat==2],w=comwt[black==1&agecat==2],na.rm=T)
black.agecat3 <- weighted.mean(crimehighimp[black==1&agecat==3],w=comwt[black==1&agecat==3],na.rm=T)
black.agecat4 <- weighted.mean(crimehighimp[black==1&agecat==4],w=comwt[black==1&agecat==4],na.rm=T)

black.age1.sd <- sqrt((black.agecat1*(1-black.agecat1))/length(crimehighimp[black==1&agecat==1]))
black.age2.sd <- sqrt((black.agecat2*(1-black.agecat2))/length(crimehighimp[black==1&agecat==2]))
black.age3.sd <- sqrt((black.agecat3*(1-black.agecat3))/length(crimehighimp[black==1&agecat==3]))
black.age4.sd <- sqrt((black.agecat4*(1-black.agecat4))/length(crimehighimp[black==1&agecat==4]))

white.agecat1 <- weighted.mean(crimehighimp[white==1&agecat==1],w=comwt[white==1&agecat==1],na.rm=T)
white.agecat2 <- weighted.mean(crimehighimp[white==1&agecat==2],w=comwt[white==1&agecat==2],na.rm=T)
white.agecat3 <- weighted.mean(crimehighimp[white==1&agecat==3],w=comwt[white==1&agecat==3],na.rm=T)
white.agecat4 <- weighted.mean(crimehighimp[white==1&agecat==4],w=comwt[white==1&agecat==4],na.rm=T)

white.age1.sd <- sqrt((white.agecat1*(1-white.agecat1))/length(crimehighimp[white==1&agecat==1]))
white.age2.sd <- sqrt((white.agecat2*(1-white.agecat2))/length(crimehighimp[white==1&agecat==2]))
white.age3.sd <- sqrt((white.agecat3*(1-white.agecat3))/length(crimehighimp[white==1&agecat==3]))
white.age4.sd <- sqrt((white.agecat4*(1-white.agecat4))/length(crimehighimp[white==1&agecat==4]))

hisp.agecat1 <- weighted.mean(crimehighimp[hisp==1&agecat==1],w=comwt[hisp==1&agecat==1],na.rm=T)
hisp.agecat2 <- weighted.mean(crimehighimp[hisp==1&agecat==2],w=comwt[hisp==1&agecat==2],na.rm=T)
hisp.agecat3 <- weighted.mean(crimehighimp[hisp==1&agecat==3],w=comwt[hisp==1&agecat==3],na.rm=T)
hisp.agecat4 <- weighted.mean(crimehighimp[hisp==1&agecat==4],w=comwt[hisp==1&agecat==4],na.rm=T)

hisp.age1.sd <- sqrt((hisp.agecat1*(1-hisp.agecat1))/length(crimehighimp[hisp==1&agecat==1]))
hisp.age2.sd <- sqrt((hisp.agecat2*(1-hisp.agecat2))/length(crimehighimp[hisp==1&agecat==2]))
hisp.age3.sd <- sqrt((hisp.agecat3*(1-hisp.agecat3))/length(crimehighimp[hisp==1&agecat==3]))
hisp.age4.sd <- sqrt((hisp.agecat4*(1-hisp.agecat4))/length(crimehighimp[hisp==1&agecat==4]))

asi.agecat1 <- weighted.mean(crimehighimp[asi==1&agecat==1],w=comwt[asi==1&agecat==1],na.rm=T)
asi.agecat2 <- weighted.mean(crimehighimp[asi==1&agecat==2],w=comwt[asi==1&agecat==2],na.rm=T)
asi.agecat3 <- weighted.mean(crimehighimp[asi==1&agecat==3],w=comwt[asi==1&agecat==3],na.rm=T)
asi.agecat4 <- weighted.mean(crimehighimp[asi==1&agecat==4],w=comwt[asi==1&agecat==4],na.rm=T)

asi.age1.sd <- sqrt((asi.agecat1*(1-asi.agecat1))/length(crimehighimp[asi==1&agecat==1]))
asi.age2.sd <- sqrt((asi.agecat2*(1-asi.agecat2))/length(crimehighimp[asi==1&agecat==2]))
asi.age3.sd <- sqrt((asi.agecat3*(1-asi.agecat3))/length(crimehighimp[asi==1&agecat==3]))
asi.age4.sd <- sqrt((asi.agecat4*(1-asi.agecat4))/length(crimehighimp[asi==1&agecat==4]))

black.opinion <- round(c(black.agecat1,black.agecat2,black.agecat3,black.agecat4),2)
white.opinion <- round(c(white.agecat1,white.agecat2,white.agecat3,white.agecat4),2)
hisp.opinion <- round(c(hisp.agecat1,hisp.agecat2,hisp.agecat3,hisp.agecat4),2)
asi.opinion <- round(c(asi.agecat1,asi.agecat2,asi.agecat3,asi.agecat4),2)

black.age.sds <- c(black.age1.sd,black.age2.sd,black.age3.sd,black.age4.sd)
white.age.sds <- c(white.age1.sd,white.age2.sd,white.age3.sd,white.age4.sd)
hisp.age.sds <- c(hisp.age1.sd,hisp.age2.sd,hisp.age3.sd,hisp.age4.sd)
asi.age.sds <- c(asi.age1.sd,asi.age2.sd,asi.age3.sd,asi.age4.sd)

opinion <- c(white.opinion,black.opinion,hisp.opinion,asi.opinion)
races.sds <- c(white.age.sds,black.age.sds,hisp.age.sds,asi.age.sds)

Races <-c(rep("White", 4), rep("Black", 4), rep("Hispanic",4), rep("Asian",4))
agecats <- c("<30","30-44","45-64","65+","<30","30-44","45-64","65+","<30","30-44","45-64","65+","<30","30-44","45-64","65+")

dat <- data.frame(agecats,Races,opinion,races.sds)

##Print Appendix Figure A2
figurea2 <- ggplot(dat,aes(agecats,opinion,fill=Races))
figurea2 <- figurea2 +guides(fill=guide_legend(title=""))+xlab("Age groups") + ylab("")+ylim(0,1)+
  geom_bar(stat = "identity",position="dodge")+geom_text(aes(label=round(opinion,2)), 
                                                         vjust=7,position=position_dodge(width=.95),size=5) +
  geom_errorbar(aes(ymin=opinion-qnorm(.975)*races.sds, ymax=opinion+qnorm(.975)*races.sds), width=.2,
                position=position_dodge(.9))+ theme(
                  panel.background = element_rect(fill = "white"), 
                  plot.background = element_rect(fill = "white", color = "white"), 
                  panel.grid.major = element_blank(), 
                  panel.border = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  axis.line.x = element_line(color = "gray40"),
                  axis.text.x = element_text(size = 15), 
                  axis.title.y = element_text(size = 15), 
                  axis.text.y = element_text(size = 15),
                  axis.line.y = element_line(color = "gray40"),
                  #   legend.position = c(0.9, 0.2),
                  plot.title = element_text(hjust = 0.2))
figurea2

#Appendix Table A7
crimehighimp.labs <- c("Crime high importance","Age 30-44", "Age 45-64", "Age 65+", "Black", "Hispanic",
                       "Asian","Democrat","Male","Medium metro county", "Urban county",
                       "Income $30k-$60k","Income $60k-$100k","Income over $100k",
                       "HS grad","Some college", "College grad","Postgrad", "Child under 18",
                       "Homeowner","Violent crime rate (log)")

##Print Table A7
stargazer(pred.crimehighimp,mainreg.wcrimehighimp,omit="state",no.space=T,covariate.labels=crimehighimp.labs)

#Appendix Figure A3
dat <- data.frame(means4,ages4)

##Print Appendix Figure A3
figurea3 <- ggplot(dat,aes(ages4,means4,fill=ages4))
figurea3 <- figurea3 +guides(fill=guide_legend(title=""))+xlab("Age groups") + ylab("")+
  ylim(0,.9)+geom_bar(stat = "identity")+
  #geom_text(aes(label=round(means4,2)), 
  #vjust=10,size=5) +
  geom_errorbar(aes(ymin=means4-qnorm(.975)*sd4, ymax=means4+qnorm(.975)*sd4), width=.2,
                position=position_dodge(.9))+ theme(
                  panel.background = element_rect(fill = "white"), 
                  plot.background = element_rect(fill = " white ", color = " white"), 
                  panel.grid.major = element_blank(), 
                  panel.border = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  axis.line.x = element_line(color = "gray40"),
                  axis.text.x = element_text(size = 15), 
                  axis.title.y = element_text(size = 15), 
                  axis.text.y = element_text(size = 15),
                  axis.line.y = element_line(color = "gray40"),
                  #   legend.position = c(0.9, 0.2),
                  plot.title = element_text(hjust = 0.2))
figurea3

#Appendix Table A8
tvnews.labs <- c("Watched local TV news in the last 24 hours","Age 30-44", "Age 45-64", "Age 65+", "Black", "Hispanic",
                 "Asian","Democrat","Male","Medium metro county", "Urban county",
                 "Income $30k-$60k","Income $60k-$100k","Income over $100k",
                 "HS grad","Some college", "College grad","Postgrad", "Child under 18",
                 "Homeowner","Violent crime rate (log)")

##Print Appendix Table A8
stargazer(pred.tvnews,mainreg.wtvnews,omit="state",no.space=T,covariate.labels=tvnews.labs)

#Appendix B 

##Load Data Appendix B 
data <- read.csv("CCES_VV_Data.csv")
attach(data)

##Define Common Variables Appendix B 
age <- 2016-data$birthyr
under30 <- ifelse(age<30,1,0)
age30to44 <- ifelse(age>=30&age<45,1,0)
age45to64 <- ifelse(age>=45&age<65,1,0)
over65 <- ifelse(age>=65,1,0)

agecat <- c()
agecat[under30==1] <- 1
agecat[age30to44==1] <- 2
agecat[age45to64==1] <- 3
agecat[over65==1] <- 4
agecat <- as.factor(agecat)

male <- ifelse(data$gender=="Male",1,0)

own <- ifelse(data$ownhome=="Own",1,0)

state <- data$inputstate

white <- ifelse(data$race=="White",1,0)
black <- ifelse(data$race=="Black" | data$multrace_2=="Yes",1,0)
hisp <- ifelse(data$race=="Hispanic"| data$multrace_3=="Yes",1,0)
asi <- ifelse(data$race=="Asian"| data$multrace_4=="Yes",1,0)
racegrp <- c()
racegrp[white==1] <- 1
racegrp[black==1] <- 2
racegrp[hisp==1] <- 3
racegrp[asi==1] <- 4

nohs <- ifelse(data$educ=="No HS",1,0)
hs <- ifelse(data$educ=="High school graduate",1,0)
somecoll <- ifelse(data$educ=="2-year" | data$educ=="Some college",1,0)
coll <- ifelse(data$educ=="4-year",1,0)
postgrad <- ifelse(data$educ=="Post-grad",1,0)

ed <- c()
ed[nohs==1] <- 1
ed[hs==1] <- 2
ed[somecoll==1] <- 3
ed[coll==1] <- 4
ed[postgrad==1] <- 5

child18 <- ifelse(data$child18=="Yes",1,0)
ch18 <- child18

matched <- ifelse(data$CL_matched=="Y",1,0)
active <- ifelse(data$CL_voterstatus=="active ",1,0)
mactive <- ifelse(matched==1&active==1,1,0)

dem <- ifelse(data$pid7=="Strong Democrat" | data$pid7=="Not very strong Democrat" | data$pid7=="Lean Democrat", 1,0)
repub <- ifelse(data$pid7=="Strong Republican" | data$pid7=="Not very strong Republican" | data$pid7=="Lean Republican", 1,0)

comwt <- data$commonweight

incpolice <- ifelse(data$CC16_334c=="Support",1,0)
incpolice[data$CC16_334c==""] <- NA

part <- ifelse(data$CC16_417a_1=="Yes" | data$CC16_417a_2=="Yes" | data$CC16_417a_3=="Yes" | data$CC16_417a_4=="Yes",1,0)
part[data$CC16_417a_1==""&data$CC16_417a_2==""&data$CC16_417a_3==""&data$CC16_417a_4==""] <- NA

voted <- ifelse(data$CL_E2016GVM=="",0,1)

own <- ifelse(data$ownhome=="Own",1,0)
rent <- ifelse(data$ownhome=="Rent",1,0)

urban <- ifelse(data$class==1|data$class==2,1,0)
smallmet <- ifelse(data$class==3|data$class==4,1,0)
rural <- ifelse(data$class==5|data$class==6,1,0)

urban.3 <- c()
urban.3[urban==1] <- 3
urban.3[smallmet==1] <- 2
urban.3[rural==1] <- 1
urban.3[is.na(data$class)] <- NA

incunder30 <- ifelse(data$faminc=="Less than $10,000"|data$faminc=="$10,000 - $19,999" | data$faminc=="$20,000 - $29,999",1,0)   
inc30to60 <- ifelse(data$faminc=="$30,000 - $39,999" | data$faminc=="$40,000 - $49,999" | data$faminc=="$50,000 - $59,999",1,0)
inc60to100 <- ifelse(data$faminc=="$60,000 - $69,999" | data$faminc=="$70,000 - $79,999" | data$faminc=="$80,000 - $99,999",1,0)
incover100 <- ifelse(data$faminc=="$100,000 - $119,999" | data$faminc=="$120,000 - $149,999" | data$faminc=="$150,000 - $199,999" | data$faminc=="$200,000 - $249,999" | data$faminc=="$250,000 - $349,999" | data$faminc=="$350,000 - $499,999" | data$faminc=="$500,000 or more",1,0)
incnosay <- ifelse(data$faminc=="Prefer not to say",1,0)
inc <- c()
inc[incunder30==1] <- 1
inc[inc30to60==1] <- 2
inc[inc60to100==1] <- 3
inc[incover100==1] <- 4
inc[incnosay==1] <- NA

racegrp <- as.factor(racegrp)
white <- as.factor(white)
black <- as.factor(black)
hisp <- as.factor(hisp)
asi <- as.factor(asi)
urban.3 <- as.factor(urban.3) 
inc <- as.factor(inc) 
ch18 <- as.factor(ch18)
own <- as.factor(own)
dem <- as.factor(dem)
ed <- as.factor(ed)

vicrime <- data$viol_rate
propcrime <- data$prop_rate
log.vicrime <- log(vicrime+1) 
log.propcrime <- log(propcrime+1) 

pred.mactive <- lm(mactive~agecat+racegrp+dem+male+urban.3+(inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)

pred.part <- lm(part~agecat+racegrp+dem+male+urban.3+(inc)+(ed)+ch18+own+log.vicrime+as.factor(state),weights=comwt)

shreg.age1 <- count(mactive[agecat==1],wt_var="comwt[agecat==1]")[2,2]/count(mactive,wt_var="comwt")[2,2]
shreg.age2 <- count(mactive[agecat==2],wt_var="comwt[agecat==2]")[2,2]/count(mactive,wt_var="comwt")[2,2]
shreg.age3 <- count(mactive[agecat==3],wt_var="comwt[agecat==3]")[2,2]/count(mactive,wt_var="comwt")[2,2]
shreg.age4 <- count(mactive[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(mactive,wt_var="comwt")[2,2]

shpart.age1 <- count(part[agecat==1],wt_var="comwt[agecat==1]")[2,2]/count(part,wt_var="comwt")[2,2]
shpart.age2 <- count(part[agecat==2],wt_var="comwt[agecat==2]")[2,2]/count(part,wt_var="comwt")[2,2]
shpart.age3 <- count(part[agecat==3],wt_var="comwt[agecat==3]")[2,2]/count(part,wt_var="comwt")[2,2]
shpart.age4 <- count(part[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(part,wt_var="comwt")[2,2]

shagecat1 <- weighted.mean(agecat==1,w=comwt,na.rm=T)
shagecat2 <- weighted.mean(agecat==2,w=comwt,na.rm=T)
shagecat3 <- weighted.mean(agecat==3,w=comwt,na.rm=T)
shagecat4 <- weighted.mean(agecat==4,w=comwt,na.rm=T)

shreg.agecats <- c(shreg.age1,shreg.age2,shreg.age3,shreg.age4)
shagecats <- c(shagecat1,shagecat2,shagecat3,shagecat4)

shreg.age1.white <- count(mactive[agecat==1&white==1],wt_var="comwt[agecat==1&white==1]")[2,2]/count(mactive[white==1],wt_var="comwt[white==1]")[2,2]
shreg.age2.white <- count(mactive[agecat==2&white==1],wt_var="comwt[agecat==2&white==1]")[2,2]/count(mactive[white==1],wt_var="comwt[white==1]")[2,2]
shreg.age3.white <- count(mactive[agecat==3&white==1],wt_var="comwt[agecat==3&white==1]")[2,2]/count(mactive[white==1],wt_var="comwt[white==1]")[2,2]
shreg.age4.white <- count(mactive[agecat==4&white==1],wt_var="comwt[agecat==4&white==1]")[2,2]/count(mactive[white==1],wt_var="comwt[white==1]")[2,2]

shreg.agecats.white <- c(shreg.age1.white,shreg.age2.white,shreg.age3.white,shreg.age4.white)

shreg.age1.black <- count(mactive[agecat==1&black==1],wt_var="comwt[agecat==1&black==1]")[2,2]/count(mactive[black==1],wt_var="comwt[black==1]")[2,2]
shreg.age2.black <- count(mactive[agecat==2&black==1],wt_var="comwt[agecat==2&black==1]")[2,2]/count(mactive[black==1],wt_var="comwt[black==1]")[2,2]
shreg.age3.black <- count(mactive[agecat==3&black==1],wt_var="comwt[agecat==3&black==1]")[2,2]/count(mactive[black==1],wt_var="comwt[black==1]")[2,2]
shreg.age4.black <- count(mactive[agecat==4&black==1],wt_var="comwt[agecat==4&black==1]")[2,2]/count(mactive[black==1],wt_var="comwt[black==1]")[2,2]

shreg.age1.hisp <- count(mactive[agecat==1&hisp==1],wt_var="comwt[agecat==1&hisp==1]")[2,2]/count(mactive[hisp==1],wt_var="comwt[hisp==1]")[2,2]
shreg.age2.hisp<- count(mactive[agecat==2&hisp==1],wt_var="comwt[agecat==2&hisp==1]")[2,2]/count(mactive[hisp==1],wt_var="comwt[hisp==1]")[2,2]
shreg.age3.hisp<- count(mactive[agecat==3&hisp==1],wt_var="comwt[agecat==3&hisp==1]")[2,2]/count(mactive[hisp==1],wt_var="comwt[hisp==1]")[2,2]
shreg.age4.hisp<- count(mactive[agecat==4&hisp==1],wt_var="comwt[agecat==4&hisp==1]")[2,2]/count(mactive[hisp==1],wt_var="comwt[hisp==1]")[2,2]

shreg.age1.asi <- count(mactive[agecat==1&asi==1],wt_var="comwt[agecat==1&asi==1]")[2,2]/count(mactive[asi==1],wt_var="comwt[asi==1]")[2,2]
shreg.age2.asi<- count(mactive[agecat==2&asi==1],wt_var="comwt[agecat==2&asi==1]")[2,2]/count(mactive[asi==1],wt_var="comwt[asi==1]")[2,2]
shreg.age3.asi<- count(mactive[agecat==3&asi==1],wt_var="comwt[agecat==3&asi==1]")[2,2]/count(mactive[asi==1],wt_var="comwt[asi==1]")[2,2]
shreg.age4.asi<- count(mactive[agecat==4&asi==1],wt_var="comwt[agecat==4&asi==1]")[2,2]/count(mactive[asi==1],wt_var="comwt[asi==1]")[2,2]

shreg.agecats.black <- c(shreg.age1.black,shreg.age2.black,shreg.age3.black,shreg.age4.black)
shreg.agecats.hisp <- c(shreg.age1.hisp,shreg.age2.hisp,shreg.age3.hisp,shreg.age4.hisp)
shreg.agecats.asi <- c(shreg.age1.asi,shreg.age2.asi,shreg.age3.asi,shreg.age4.asi)

shagecat1.white <- count(white[agecat==1],wt_var="comwt[agecat==1]")[2,2]/count(white,wt_var="comwt")[2,2]
shagecat2.white <- count(white[agecat==2],wt_var="comwt[agecat==2]")[2,2]/count(white,wt_var="comwt")[2,2]
shagecat3.white <- count(white[agecat==3],wt_var="comwt[agecat==3]")[2,2]/count(white,wt_var="comwt")[2,2]
shagecat4.white <- count(white[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(white,wt_var="comwt")[2,2]

shagecat1.black <- count(black[agecat==1],wt_var="comwt[agecat==1]")[2,2]/count(black,wt_var="comwt")[2,2]
shagecat2.black<- count(black[agecat==2],wt_var="comwt[agecat==2]")[2,2]/count(black,wt_var="comwt")[2,2]
shagecat3.black<- count(black[agecat==3],wt_var="comwt[agecat==3]")[2,2]/count(black,wt_var="comwt")[2,2]
shagecat4.black<- count(black[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(black,wt_var="comwt")[2,2]

shagecat1.hisp <- count(hisp[agecat==1],wt_var="comwt[agecat==1]")[2,2]/count(hisp,wt_var="comwt")[2,2]
shagecat2.hisp<- count(hisp[agecat==2],wt_var="comwt[agecat==2]")[2,2]/count(hisp,wt_var="comwt")[2,2]
shagecat3.hisp<- count(hisp[agecat==3],wt_var="comwt[agecat==3]")[2,2]/count(hisp,wt_var="comwt")[2,2]
shagecat4.hisp<- count(hisp[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(hisp,wt_var="comwt")[2,2]

shagecat1.asi <- count(asi[agecat==1],wt_var="comwt[agecat==1]")[2,2]/count(asi,wt_var="comwt")[2,2]
shagecat2.asi<- count(asi[agecat==2],wt_var="comwt[agecat==2]")[2,2]/count(asi,wt_var="comwt")[2,2]
shagecat3.asi<- count(asi[agecat==3],wt_var="comwt[agecat==3]")[2,2]/count(asi,wt_var="comwt")[2,2]
shagecat4.asi<- count(asi[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(asi,wt_var="comwt")[2,2]

shpart.age4.overall <- count(part[agecat==4],wt_var="comwt[agecat==4]")[2,2]/count(part,wt_var="comwt")[2,2]

shpart.age1.white <- count(part[agecat==1&white==1],wt_var="comwt[agecat==1&white==1]")[2,2]/count(part[white==1],wt_var="comwt[white==1]")[2,2]
shpart.age2.white <- count(part[agecat==2&white==1],wt_var="comwt[agecat==2&white==1]")[2,2]/count(part[white==1],wt_var="comwt[white==1]")[2,2]
shpart.age3.white <- count(part[agecat==3&white==1],wt_var="comwt[agecat==3&white==1]")[2,2]/count(part[white==1],wt_var="comwt[white==1]")[2,2]
shpart.age4.white <- count(part[agecat==4&white==1],wt_var="comwt[agecat==4&white==1]")[2,2]/count(part[white==1],wt_var="comwt[white==1]")[2,2]

shpart.age1.black <- count(part[agecat==1&black==1],wt_var="comwt[agecat==1&black==1]")[2,2]/count(part[black==1],wt_var="comwt[black==1]")[2,2]
shpart.age2.black <- count(part[agecat==2&black==1],wt_var="comwt[agecat==2&black==1]")[2,2]/count(part[black==1],wt_var="comwt[black==1]")[2,2]
shpart.age3.black <- count(part[agecat==3&black==1],wt_var="comwt[agecat==3&black==1]")[2,2]/count(part[black==1],wt_var="comwt[black==1]")[2,2]
shpart.age4.black <- count(part[agecat==4&black==1],wt_var="comwt[agecat==4&black==1]")[2,2]/count(part[black==1],wt_var="comwt[black==1]")[2,2]

shpart.age1.hisp <- count(part[agecat==1&hisp==1],wt_var="comwt[agecat==1&hisp==1]")[2,2]/count(part[hisp==1],wt_var="comwt[hisp==1]")[2,2]
shpart.age2.hisp<- count(part[agecat==2&hisp==1],wt_var="comwt[agecat==2&hisp==1]")[2,2]/count(part[hisp==1],wt_var="comwt[hisp==1]")[2,2]
shpart.age3.hisp<- count(part[agecat==3&hisp==1],wt_var="comwt[agecat==3&hisp==1]")[2,2]/count(part[hisp==1],wt_var="comwt[hisp==1]")[2,2]
shpart.age4.hisp<- count(part[agecat==4&hisp==1],wt_var="comwt[agecat==4&hisp==1]")[2,2]/count(part[hisp==1],wt_var="comwt[hisp==1]")[2,2]

shpart.age1.asi <- count(part[agecat==1&asi==1],wt_var="comwt[agecat==1&asi==1]")[2,2]/count(part[asi==1],wt_var="comwt[asi==1]")[2,2]
shpart.age2.asi<- count(part[agecat==2&asi==1],wt_var="comwt[agecat==2&asi==1]")[2,2]/count(part[asi==1],wt_var="comwt[asi==1]")[2,2]
shpart.age3.asi<- count(part[agecat==3&asi==1],wt_var="comwt[agecat==3&asi==1]")[2,2]/count(part[asi==1],wt_var="comwt[asi==1]")[2,2]
shpart.age4.asi<- count(part[agecat==4&asi==1],wt_var="comwt[agecat==4&asi==1]")[2,2]/count(part[asi==1],wt_var="comwt[asi==1]")[2,2]

shagecats.white <- c(shagecat1.white,shagecat2.white,shagecat3.white,shagecat4.white)
shagecats.black <- c(shagecat1.black,shagecat2.black,shagecat3.black,shagecat4.black)
shagecats.hisp<- c(shagecat1.hisp,shagecat2.hisp,shagecat3.hisp,shagecat4.hisp)
shagecats.asi<- c(shagecat1.asi,shagecat2.asi,shagecat3.asi,shagecat4.asi)

shagecats.races <- c(shagecats.white,shagecats.black,shagecats.hisp,shagecats.asi)

shpart.agecats.white <- c(shpart.age1.white,shpart.age2.white,shpart.age3.white,shpart.age4.white)
shpart.agecats.black <- c(shpart.age1.black,shpart.age2.black,shpart.age3.black,shpart.age4.black)
shpart.agecats.hisp <- c(shpart.age1.hisp,shpart.age2.hisp,shpart.age3.hisp,shpart.age4.hisp)
shpart.agecats.asi <- c(shpart.age1.asi,shpart.age2.asi,shpart.age3.asi,shpart.age4.asi)

shpart.agecats.races <- c(shpart.agecats.white,shpart.agecats.black,shpart.agecats.hisp,shpart.agecats.asi)
shreg.agecats.races <- c(shreg.agecats.white,shreg.agecats.black,shreg.agecats.hisp,shreg.agecats.asi)

Races <-c(rep("White", 4), rep("Black", 4), rep("Hispanic",4), rep("Asian",4))
agecats <- c("<30","30-44","45-64","65+","<30","30-44","45-64","65+","<30","30-44","45-64","65+","<30","30-44","45-64","65+")
agecats <- as.factor(agecats)

partcomp <- data.frame(Races,shagecats.races,shreg.agecats.races,shpart.agecats.races)

partgraph.write <- write.csv(partcomp, "partcomp.csv")

regratio.white <- shreg.agecats.white/shagecats.white
regratio.black<- shreg.agecats.black/shagecats.black
regratio.hisp<- shreg.agecats.hisp/shagecats.hisp
regratio.asi<- shreg.agecats.asi/shagecats.asi

partratio.white <- shpart.agecats.white/shagecats.white
partratio.black <- shpart.agecats.black/shagecats.black
partratio.hisp <- shpart.agecats.hisp/shagecats.hisp
partratio.asi <- shpart.agecats.asi/shagecats.asi

regratios <- c(regratio.white,regratio.black,regratio.hisp,regratio.asi)
partratios <- c(partratio.white,partratio.black,partratio.hisp,partratio.asi)

dat <- data.frame(regratios,Races,agecats)

regratios.write <- write.csv(dat,"regratios.csv")

colnames(dat)[colnames(dat)=="races"] <- "Race groups"

dat2 <- data.frame(partratios,Races,agecats)

#Appendix B Table 
labs <- c("Age 30-44", "Age 45-64", "Age 65+", "Black", "Hispanic",
          "Asian","Democrat","Male","Medium metro county", "Urban county",
          "Income $30k-$60k","Income $60k-$100k","Income over $100k",
          "HS grad","Some college", "College grad","Postgrad", "Child under 18",
          "Homeowner","Violent crime rate (log)")

##Print Appendix B Table
stargazer(pred.mactive,pred.part,omit="state",no.space=T,covariate.labels=labs)

#Appendix B figure
p <-ggplot(dat, aes(agecats,regratios))

q <- ggplot(dat2,aes(agecats,partratios))

##Print Appendix B Figure
grid.arrange(p +ggtitle("Registration ratios")+guides(fill=guide_legend(title=""))+xlab("") + ylab("Registration ratio")+ylim(0,2)+
                                 geom_bar(stat = "identity", aes(fill = Races),position="dodge")+geom_hline(yintercept = 1, col = "darkblue", size = 1, linetype = "solid")+theme(
                                   panel.background = element_rect(fill = "white"), 
                                   #  plot.background = element_rect(fill = "white", color = "white"), 
                                   # panel.grid.major = element_blank(), 
                                   panel.border = element_blank(), 
                                   panel.grid.minor = element_blank(), 
                                   axis.line.x = element_line(color = "gray40"),
                                   axis.text.x = element_text(size = 15), 
                                   axis.title.y = element_blank(), 
                                   axis.text.y = element_text(size = 15)),
                               q+ ggtitle("Other participation ratios")+guides(fill=guide_legend(title=""))+xlab("Age groups") + ylab("Other participation ratio")+ylim(0,2)+
                                 geom_bar(stat = "identity", aes(fill = Races),position="dodge")+geom_hline(yintercept = 1, col = "darkblue", size = 1, linetype = "solid")+theme(
                                   panel.background = element_rect(fill = "white"), 
                                   #  plot.background = element_rect(fill = "white", color = "white"), 
                                   # panel.grid.major = element_blank(), 
                                   panel.border = element_blank(), 
                                   panel.grid.minor = element_blank(), 
                                   axis.line.x = element_line(color = "gray40"),
                                   axis.text.x = element_text(size = 15), 
                                   axis.title.y = element_blank(), 
                                   axis.text.y = element_text(size = 15)))

#Table 3

##Load Data Table 3 
gss <- read.csv("GSS_Data.csv")
attach(gss)

##Define Variables Table 3 
wts <- gss$wtssall

ba <- ifelse(gss$degree=="bachelor",1,0)

year <- gss$year

dem <- ifelse(gss$partyid=="ind,near dem" | gss$partyid=="not str democrat" | gss$partyid=="strong democrat",1,0)

fearq <- gss$fear

fear <- ifelse(fearq=="yes",1,0)
fear[fearq==".a"|fearq==".b"|fearq==".d"|fearq==".i"|fearq=="na"] <- NA

splaw <- c()
splaw[gss$natcrimy=="about right" | gss$natcrimy=="too much"] <- 0
splaw[gss$natcrimy=="too little"] <- 1

inc <- gss$realinc
inc <- as.numeric(as.character(inc))
loginc <- log(inc+1)

white <- c()
white[gss$race=="white"] <- 1
white[gss$race=="black"] <- 0
white[gss$race=="other"] <- 0

white <- ifelse(gss$race=="white",1,0)
black <- ifelse(gss$race=="black",1,0)
other <- ifelse(gss$race=="other",1,0)

hisp <- ifelse(gss$hispanic==".d" | gss$hispanic==".i" | gss$hispanic=="NOT HISPANIC" | gss$hispanic=="not hispanic",0,1)

nohs <- ifelse(gss$degree=="lt high school",1,0)
hs <- ifelse(gss$degree=="high school",1,0)
somecoll <- ifelse(gss$degree=="junior college",1,0)
ba <- ifelse(gss$degree=="bachelor",1,0)
postgrad <- ifelse(gss$degree=="graduate",1,0)

male <- ifelse(gss$sex=="male",1,0)
ch18 <- as.numeric(as.character(gss$childs))

ch18[gss$childs=="eight or more" | gss$childs=="EIGHT OR MORE"] <- 8

e.nor.central <- ifelse(gss$region=="e. nor. central"|gss$region=="E. NOR. CENTRAL",1,0)
e.sou.central <- ifelse(gss$region=="e. sou. central"|gss$region=="E. SOU. CENTRAL",1,0)
w.sou.central <- ifelse(gss$region=="w. sou. central"|gss$region=="W. SOU. CENTRAL",1,0)
w.nor.central <- ifelse(gss$region=="w. nor. central"|gss$region=="W. NOR. CENTRAL",1,0)
midatlantic <- ifelse(gss$region=="middle atlantic" | gss$region=="MIDDLE ATLANTIC",1,0)
southatlantic <- ifelse(gss$region=="south atlantic" | gss$region=="SOUTH ATLANTIC",1,0)
pacific <- ifelse(gss$region=="pacific" | gss$region=="PACIFIC",1,0)
new.england <- ifelse(gss$region=="new england" | gss$region=="NEW ENGLAND",1,0)
mountain <- ifelse(gss$region=="mountain" | gss$region=="MOUNTAIN",1,0)

region <- c()
region[e.nor.central==1] <- 1
region[e.sou.central==1] <- 2
region[w.sou.central==1] <- 3
region[w.nor.central==1] <- 4
region[midatlantic==1] <- 5
region[southatlantic==1] <- 6
region[pacific==1] <- 7
region[new.england] <- 8
region[mountain==1] <- 9
region <- as.factor(region)

age <- as.numeric(as.character(gss$age))
under30 <- ifelse(age<=30,1,0)
age30to44 <- ifelse(age>30&age<=44,1,0)
age45to64 <- ifelse(age>=45&age<65,1,0)
over65 <- ifelse(age>=65,1,0)

agecat <- c()
agecat[under30==1] <- 1
agecat[age30to44==1] <- 2
agecat[age45to64==1] <- 3
agecat[over65==1] <- 4
agecat <- as.factor(agecat)

own <- ifelse(gss$dwelown=="own or is buying" | gss$dwelown=="OWN OR IS BUYING",1,0)
own[gss$dwelown=="iap"|gss$dwelown=="dk"|gss$dwelown==".n"|gss$dwelown==".c"] <- NA
own <- as.factor(own)

birthyr <- year-age

birth.1898to1910 <- as.factor(ifelse(birthyr>=1898&birthyr<1910,1,0))
birth.1910to1922 <- as.factor(ifelse(birthyr>=1910&birthyr<1922,1,0))
birth.1922to1934 <- as.factor(ifelse(birthyr>=1922&birthyr<1934,1,0))
birth.1934to1946 <- as.factor(ifelse(birthyr>=1934&birthyr<1946,1,0))
birth.1946to1958 <- as.factor(ifelse(birthyr>=1946&birthyr<1958,1,0))
birth.1958to1970 <- as.factor(ifelse(birthyr>=1958&birthyr<1970,1,0))
birth.1970to1982 <- as.factor(ifelse(birthyr>=1970&birthyr<1982,1,0))
birth.1982to1994 <- as.factor(ifelse(birthyr>=1982&birthyr<1994,1,0))

gssdata <- as.data.frame(cbind(splaw,birth.1898to1910,birth.1910to1922,birth.1922to1934,
                               birth.1934to1946,birth.1946to1958,birth.1958to1970,birth.1970to1982,birth.1982to1994,ba,
                               dem,loginc,black,male,region,ch18,own,year,age,wts))

gss.master <- write.csv(gssdata,"masterdata_gss.csv")

pred.splaw.age <- lm(splaw~age+birth.1898to1910+birth.1910to1922+birth.1922to1934+
                       birth.1934to1946+birth.1946to1958+birth.1958to1970+birth.1970to1982+ba+
                       dem+loginc+black+male+as.factor(region)+ch18+own+as.factor(year),weights=wts)

table3.labs <- c("Age", "Born 1898 to 1910", "Born 1910 to 1922", "Born 1922 to 1934",
                "Born 1934 to 1946", "Born 1946 to 1958", "Born 1958 to 1970", "Born 1970 to 1982",
                "College grad", "Democrat", "Income (log)", "Black", "Male", "Child under 18", "Homeowner")

##Print Table 3 
stargazer(pred.splaw.age,no.space=T,omit=c("region","year"), covariate.labels=table3.labs)

#Table 4

##Load Data Table 4
data <- read_dta("acs_lemas_ucr_2007_13_panel.dta")
attach(data)

##Define Variables Table 4 
hhi <- sh_white^2+sh_black^2+sh_hisp^2+sh_asian^2

logpop2 <- logpop^2
logpop3 <- logpop^3

panel.offcrs <- lm(log_ftsworn_pc~log(sh_over65)+log(sh_black+.01)+
                     hhi+log_pcincome+log_vicrime_rate+as.factor(state_code)+as.factor(lemasyr))

panel.budget <- lm(log_budget_pc~log(sh_over65)+log(sh_black+.01)+hhi+log_pcincome+
                     log_vicrime_rate+as.factor(state_code)+as.factor(lemasyr))

varnames <- c("Share over 65 (log)","Share Black (log)","Herfindahl index","PC income (log)","Violent crime rate (log)","LEMAS 2013")
depvarnames <- c("Officers per capita (log)","Budget per capita (log)")

##Print Table 4
stargazer(panel.offcrs,panel.budget,omit="state_code",
                    dep.var.labels=depvarnames,covariate.labels=varnames,omit.stat=c("f","ser","adj.rsq"),add.lines = list(c("State fixed effects?", "Yes", "Yes")))

