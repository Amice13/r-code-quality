#Gender (im)balance in the Russian cinema: on the screen and behind the camera // Journal of Cultural Analytics

#Set your working directory
setwd("...")

library(dplyr)
library(ggplot2)
library(stargazer)
library(car)
library(sjPlot)


#dataset for the whole market
Gdata <- read.csv("Russian gender dataset.csv", header=T, sep=";", dec = ",", encoding	= "UTF-8") #
glimpse(Gdata)
#dataset for BWL test
data <- read.csv("BWL dataset.csv", header=T, sep=";", dec = ",", encoding	= "UTF-8")
glimpse(data)


#DESCRIPTION OF THE DATA
#ID - Identification number of the film
#Title - film title in English
#Original title	- Russian title
#Y - year of film production
#Country - countries of production
#Release_date - release date in Russia
#FilmType - Animation, Feature film or Documentary
#Type - film types used for the BWL sample formation: films with average attendance, top 10 Russian films of the year, films for festival and cinephiles, undefined
#DirMaleShare - the proportion of men among all people in the position of director
#ProdMaleShare - the proportion of men among all people in the position of producer
#ScrMaleShare - the proportion of men among all people in the position of screenwriter
#maleshare - an accumulated proportion of men in all three positions together
#Director - film director name
#BWL - test score of Bechdel-Wallace modified by Leontyeva (in percent of qualified dialogues)
#GDR - gender degree ratio of characters based on participation in dialogues
#GDRT - gender degree ratio total based on characters participation in the same scenes
#Attendance - number of cinema tickets sold
#BudgetALL - film budget amount in rubles, discounted as of the end of December 2019
#StateSup - amount of the state financial support for the film, discounted as of the end of December 2019
#AgeRateRu - age audiences’ restrictions in Russia (0/6+ (basic), 12+, 14/16+, 18+)
#Drama, Comedy, Family, Suspense, Dynamic, Prestige - genres categories (in percent of genres enlisted on Kinopoisk.ru)
#threshold - 2015 year of release 
#DistrType – film distributors ranging by the strength: 1 – self-distributed films (basic), 2 – independent distributors, 3 – Russian leading producers representatives, 4 – Hollywood majors representatives 
#ScrRU - average number of screens in Russia for the month of release
#VacDays - number of days off in a film release week (during long New Year holyday the vacation week could last 10 days in a row). 
#Competitors - number of films released during the same week
#female.directors - number of women in the position of director
#male.directors - number of men in the position of director
#NA.directors - number of persons with unknown sex in the position of director
#total.directors - number of persons in the position of director
#male.producers - number of women in the position of producer
#female.producers - number of men in the position of producer
#NA.producers - number of persons with unknown sex in the position of producer
#total.producers - number of men in the position of producer
#male.screenwriters - number of women in the position of screenwriter
#female.screenwriters - number of men in the position of screenwriter
#NA.screenwriters - number of persons with unknown sex in the position of screenwriter
#total.screenwriters - number of men in the position of screenwriter
#male - number of men in all three positions together
#fwmale - number of women in all three positions together

#Make log-transformations in BWL dataset
data$BWLLog <- log(data$BWL + 0.0000001)
data$GDRLog <- log(data$GDR + 0.0000001)
data$GDRTLog <- log(data$GDRT + 0.0000001)
data$AttendanceLog <- log(data$Attendance + 0.0000001)

#Make factors in datasets
data$DistrType <- as.factor(data$DistrType)
Gdata$DistrType <- as.factor(Gdata$DistrType)

#Relevel the threshold
Gdata$threshold <- relevel(Gdata$threshold, ref = "Before")
data$threshold <- relevel(data$threshold, ref = "Before")

#Make datasets with budgets only
Gdata_1 <- Gdata[which(Gdata$BudgetALL != "NA"),]
data_1 <- data[which(data$BudgetALL != "NA"),]


#descriptive statistics of the datasets

#plot 2 for all films 2
hist(Gdata$Y, labels = TRUE, cex = 0.5, col="lightblue", ylab = "Number of films", xlab="Production year",
     main = "a) N=1285", ylim = c(1, 500))
#plot for data with budgets
hist(Gdata_1$Y, labels = TRUE, cex = 0.5, col="lightblue", ylab = "Number of films", xlab="Production year",
     main = "b) N=644", ylim = c(1, 500))

#plot 2 data with BWL 
hist(data$Y, labels = TRUE, cex = 0.5, col="lightblue", ylab = "Number of films", xlab="Production year",
     main = "c) N=243", ylim = c(1, 500))
#plot for data with budgets
hist(data_1$Y, labels = TRUE, cex = 0.5, col="lightblue", ylab = "Number of films", xlab="Production year",
     main = "d) N=193", ylim = c(1, 500))

#Table 1
stargazer(Gdata[c("DirMaleShare", "ProdMaleShare", "ScrMaleShare", "maleshare", "BudgetALL")], 
          type='html', out = 'statgender.html', 
          title = 'Proportions of males in key film crew positions and film budgets; full dataset (N=1285)')

#Table 2
stargazer(data[c("Attendance", "DirMaleShare", "ProdMaleShare", "ScrMaleShare", "maleshare", "BudgetALL", "VacDays",
                 "Competitors", "ScrRU", "BWL", "GDR", "GDRT")], type='html', out = 'statBec.html', 
          title = 'Proportion of males in key film crew positions and major film features; BWL dataset (N=243)')

#Table 3
sum(Gdata$male.producers)
sum(Gdata$female.producers)
sum(Gdata$male.producers) / sum(Gdata$female.producers, Gdata$male.producers)
sum(Gdata$female.producers) / sum(Gdata$female.producers, Gdata$male.producers)
sum(Gdata$male.screenwriters)
sum(Gdata$female.screenwriters)
sum(Gdata$male.screenwriters) / sum(Gdata$female.screenwriters, Gdata$male.screenwriters)
sum(Gdata$female.screenwriters) / sum(Gdata$female.screenwriters, Gdata$male.screenwriters)
sum(Gdata$male.directors)
sum(Gdata$female.directors)
sum(Gdata$male.directors) / sum(Gdata$female.directors, Gdata$male.directors)
sum(Gdata$female.directors) / sum(Gdata$female.directors, Gdata$male.directors)
sum(Gdata$male.producers, Gdata$male.screenwriters, Gdata$male.directors)
sum(Gdata$female.producers, Gdata$female.screenwriters, Gdata$female.directors)
sum(Gdata$male.producers, Gdata$male.screenwriters, Gdata$male.directors) / sum(Gdata$male.producers, Gdata$female.producers, Gdata$female.screenwriters, Gdata$male.screenwriters, Gdata$female.directors, Gdata$male.directors)
sum(Gdata$female.producers, Gdata$female.screenwriters, Gdata$female.directors) / sum(Gdata$male.producers, Gdata$female.producers, Gdata$female.screenwriters, Gdata$male.screenwriters, Gdata$female.directors, Gdata$male.directors)


#Table 6
sum(data$male.producers)
sum(data$female.producers)
sum(data$male.producers) / sum(data$female.producers, data$male.producers)
sum(data$female.producers) / sum(data$female.producers, data$male.producers)
sum(data$male.screenwriters)
sum(data$female.screenwriters)
sum(data$male.screenwriters) / sum(data$female.screenwriters, data$male.screenwriters)
sum(data$female.screenwriters) / sum(data$female.screenwriters, data$male.screenwriters)
sum(data$male.directors)
sum(data$female.directors)
sum(data$male.directors) / sum(data$female.directors, data$male.directors)
sum(data$female.directors) / sum(data$female.directors, data$male.directors)
sum(data$male.producers, data$male.screenwriters, data$male.directors)
sum(data$female.producers, data$female.screenwriters, data$female.directors)
sum(data$male.producers, data$male.screenwriters, data$male.directors) / sum(data$male.producers, data$female.producers, data$female.screenwriters, data$male.screenwriters, data$female.directors, data$male.directors)
sum(data$female.producers, data$female.screenwriters, data$female.directors) / sum(data$male.producers, data$female.producers, data$female.screenwriters, data$male.screenwriters, data$female.directors, data$male.directors)


# plot 3
hist(Gdata$maleshare, col="lightgreen", labels = TRUE, ylim = c(0, 1050), 
     xlab="Share of males in whole team", ylab="Number of films", main = "a)")
hist(Gdata$ProdMaleShare, col="red", labels = TRUE, ylim = c(0, 1050), 
     xlab="Share of males in the producer's team", ylab="Number of films", main = "b)")
hist(Gdata$ScrMaleShare, col="yellow", labels = TRUE, ylim = c(0, 1050), 
     xlab="Share of males in the writer's team", ylab="Number of films", main = "c)")
hist(Gdata$DirMaleShare, col="lightpink", labels = TRUE, ylim = c(0, 1050),
     xlab="Share of males in the director's team", ylab="Number of films", main = "d)")


# plot 4
hist(data$BWL, col="coral", labels = TRUE, border="coral",  breaks=c(0, seq(0.05,1, 0.05)), ylim = c(0, 170),
     main = "BWL test results with score for the Russian films", xlab="Qualified female characters dialogues share")
#piechart
data$Bechdelresen [data$BWL>0] <- "passed"
data$Bechdelresen [data$BWL==0] <- "not passed"
Bechbaren <- as.data.frame(table(data$Bechdelresen))
Bechbaren$percent <- Bechbaren$Freq/sum(Bechbaren$Freq)
lblsen <- paste(Bechbaren$Var1, ": ",round(Bechbaren$percent,2)*100, " %", sep="")
pie(Bechbaren$percent, labels = lblsen, col= c("lightblue", "coral "), border = c("lightblue", "coral "),
    main = "BWL test results for the Russian films")

#plot 5
hist(data$GDR, col="aquamarine", border="aquamarine",  breaks=c(0, seq(0.05,8, 0.05)),
     main = "Gender degree ratio of the Russian films", xlab="GDR")
hist(data$GDRT, col="cyan2", border="cyan2",  breaks=c(0, seq(0.05,8, 0.05)),
     main = "Gender degree ratio total of the Russian films", xlab="GDRT")



#Testing hypothesis 

#Behind the screen
#Н1.	The higher the proportion of women among filmmakers in key creative positions, the smaller the film budget is.
#Н2.	The higher the proportion of women among filmmakers in key creative positions, the lower a film distributor’s power is.
gendermall <- lm(maleshare ~ log(BudgetALL) + 
                     log(StateSup) + DistrType, data = Gdata)
summary(gendermall)
vif(gendermall) #no multicollinearity
ncvTest(gendermall) #no heteroscedasticity
#producers
gendermprod <- lm(ProdMaleShare ~ log(BudgetALL) +  
                      log(StateSup) + DistrType, data = Gdata)
summary(gendermprod)
vif(gendermprod) #no multicollinearity
ncvTest(gendermprod) #no heteroscedasticity
#directors
gendermdir <- lm(DirMaleShare ~ log(BudgetALL) +  
                     log(StateSup) + DistrType, data = Gdata)
summary(gendermdir)
vif(gendermdir) #no multicollinearity
ncvTest(gendermdir) #no heteroscedasticity
#writers
gendermscr <- lm(ScrMaleShare ~ log(BudgetALL) + 
                     log(StateSup) + DistrType, data = Gdata)
summary(gendermscr)
vif(gendermscr) #no multicollinearity
ncvTest(gendermscr) #no heteroscedasticity
(exp(coef(gendermscr))-1)*100
stargazer(gendermall, gendermprod, gendermdir, gendermscr,
          type='html', out = 'modgender.html', 
          title = 'Results of the regression analysis of the relationship between access to resources and the share of men in key film crew positions')
#Male teams have access to the bigger budgets and stronger distributors


#Н3.	The higher the proportion of women among filmmakers in key creative positions, the lower the attendance of Russian films in the CIS is.
genderattall <- lm(log(Attendance) ~ ProdMaleShare + DirMaleShare +
                       ScrMaleShare + DistrType + AgeRateRu + ScrRU + VacDays + Competitors,
                   data=Gdata)
summary(genderattall)
vif(genderattall) #we have deleted all genres and maleshare due to the multicolinearity
ncvTest(genderattall) #no heteroscedasticity
genderattall1 <- update(genderattall, .~. + log(BudgetALL))
summary(genderattall1)
vif(genderattall1) #no multicollinearity
ncvTest(genderattall1) #no heteroscedasticity

#Н4.	 After 2015 the (positive) effect of the proportion of men among filmmakers on theatrical attendance is smaller than before. 
genderattallint <- lm(log(Attendance) ~ ProdMaleShare * threshold + DirMaleShare * threshold +
                          ScrMaleShare  * threshold + DistrType + AgeRateRu + 
                          ScrRU + VacDays + Competitors, data=Gdata)
summary(genderattallint)
vif(genderattallint) #no multicollinearity
ncvTest(genderattallint) #no heteroscedasticity
genderattallint1 <- update(genderattallint, .~. + log(BudgetALL))
summary(genderattallint1)
vif(genderattallint1) #no multicollinearity
ncvTest(genderattallint1) #no heteroscedasticity

#table 5
stargazer(genderattall, genderattall1, genderattallint, genderattallint1,
          type='html', out = 'modgenderadm.html', 
          title = "Linear regression models of Russian films attendance and key crew members' gender composition")



#off/on screen 

#Н5.	The higher the proportion of women among filmmakers, 
#(a) the higher the centrality of female characters, 
#and (b) the less stereotypical their representation is.

#with BWL
model<-lm(BWLLog~maleshare, data=data)
modeld<-lm(BWLLog~DirMaleShare, data=data)
modelp<-lm(BWLLog~ProdMaleShare, data=data)
models<-lm(BWLLog~ScrMaleShare, data=data)
#with GDR
model1<-lm(GDRLog~maleshare, data=data)
modeld1<-lm(GDRLog~DirMaleShare, data=data)
modelp1<-lm(GDRLog~ProdMaleShare, data=data)
models1<-lm(GDRLog~ScrMaleShare, data=data)
#with GDRT
model12<-lm(GDRTLog~maleshare, data=data)
modeld12<-lm(GDRTLog~DirMaleShare, data=data)
modelp12<-lm(GDRTLog~ProdMaleShare, data=data)
models12<-lm(GDRTLog~ScrMaleShare, data=data)

#table 7
stargazer(model, modeld, modelp, models,
          model1, modeld1, modelp1, models1,
          model12, modeld12, modelp12, models12,
          type='html', out = 'modplotgendereng.html', report = "vc*",
          title = 'Linear regression models of the key crew members gender composition and female representation indicies')


#KEY HYPOTHESIS: 
#Н6.	(a) The higher the degree centrality of female characters 
#and (b) the lower their stereotyping in dialogues is, the higher the attendance of Russian films in the CIS is.

#BWL as independent 
#without budget
H3model1 <- lm(log(Attendance) ~ BWLLog + DistrType + 
                 AgeRateRu + ScrRU + VacDays + Competitors +
                 Comedy + Family + Suspence + Dynamic + Prestige +
                 Type, data=data) 
summary(H3model1)
vif(H3model1) #due to multicollinearity with drama and comedy remove drama
ncvTest(H3model1) #no heteroscedasticity: p < 0.05 
#with budget
H3model2 <- update(H3model1, .~. + log(BudgetALL))
summary(H3model2)
vif(H3model2) #no multicollinearity
ncvTest(H3model2) #no heteroscedasticity: p < 0.05


#GDR 
#without budget
H3model3 <- lm(log(Attendance) ~ GDRLog + DistrType + 
                 AgeRateRu + ScrRU + VacDays + Competitors +
                 Comedy + Family + Suspence + Dynamic + Prestige +
                 Type, data=data) 
summary(H3model3)
vif(H3model3) #no multicollinearity
ncvTest(H3model3) #no heteroscedasticity: p < 0.05
#with budget
H3model4 <- update(H3model3, .~. + log(BudgetALL))
summary(H3model4)
vif(H3model4) #no multicollinearity
ncvTest(H3model4) #no heteroscedasticity: p < 0.05


#GDRT 
#without budget
H3model5 <- lm(log(Attendance) ~ GDRTLog + DistrType + 
                 AgeRateRu + ScrRU + VacDays + Competitors +
                 Comedy + Family + Suspence + Dynamic + Prestige +
                 Type, data=data) 
summary(H3model5)
vif(H3model5) #no multicollinearity
ncvTest(H3model5) #no heteroscedasticity: p < 0.05

#with budget
H3model6 <- update(H3model5, .~. + log(BudgetALL))
summary(H3model6)
vif(H3model6) #no multicollinearity
ncvTest(H3model6) #no heteroscedasticity: p < 0.05


#Н7.	After 2015, the (positive) effect of (a) female centrality 
#and (b) female character non-stereotyping on attendance is higher than before 2015.

#Threshold 2015 year interaction with BWL and GDR(T)

#without budget
H3modelint1 <- lm(log(Attendance) ~ BWLLog * threshold + DistrType + 
                    AgeRateRu + ScrRU + VacDays + Competitors + Type +
                    Comedy + Family + Suspence + Dynamic + Prestige, 
                  data=data) 
summary(H3modelint1)
vif(H3modelint1) #no multicollinearity without drama
ncvTest(H3modelint1) #no heteroscedasticity: p < 0.05
#with budget
H3modelint2 <- lm(log(Attendance) ~ BWLLog * threshold + DistrType + 
                    AgeRateRu + VacDays + Competitors + Type +
                    Comedy + Family + Suspence + Dynamic + Prestige + log(BudgetALL),
                  data=data)
summary(H3modelint2)
vif(H3modelint2) #no multicollinearity  without screens
ncvTest(H3modelint2) #no heteroscedasticity: p < 0.05

#GDR 
#without budget
H3modelint3 <- lm(log(Attendance) ~ GDRLog * threshold + DistrType + 
                    AgeRateRu + ScrRU + VacDays + Competitors + Type +
                    Comedy + Family + Suspence + Dynamic + Prestige,
                  data=data) 
summary(H3modelint3)
vif(H3modelint3) #no multicollinearity 
ncvTest(H3modelint3) #no heteroscedasticity: p < 0.05
#with budget
H3modelint4 <- lm(AttendanceLog ~ GDRLog * threshold + DistrType + 
                    AgeRateRu + VacDays + Competitors + Type +
                    Comedy + Family + Suspence + Dynamic + Prestige + log(BudgetALL), 
                  data=data) 
summary(H3modelint4)
vif(H3modelint4) #no multicollinearity without screens
ncvTest(H3modelint4) #no heteroscedasticity: p < 0.05

#GDRT 
#without budget
H3modelint5 <- lm(log(Attendance) ~ GDRTLog * threshold + DistrType + 
                    AgeRateRu + ScrRU + VacDays + Competitors + Type +
                    Comedy + Family + Suspence + Dynamic + Prestige,
                  data=data) 
summary(H3modelint5)
vif(H3modelint5) #no multicollinearity
ncvTest(H3modelint5) #no heteroscedasticity: p < 0.05
#with budget
H3modelint6 <- lm(AttendanceLog ~ GDRTLog * threshold + DistrType + 
                    AgeRateRu + VacDays + Competitors + Type +
                    Comedy + Family + Suspence + Dynamic + Prestige + log(BudgetALL),
                  data=data)
summary(H3modelint6)
vif(H3modelint6) #no multicollinearity without screens
ncvTest(H3modelint6) #no heteroscedasticity: p < 0.05

#Table 8
stargazer(H3model1, H3model2, H3model3, H3model4, H3model5, H3model6,
          H3modelint1, H3modelint2, H3modelint3, H3modelint4, H3modelint5, H3modelint6,
          type='html', out = 'modplotadmeng12.html', report = "vc*",
          title = 'Linear regression models of the film attendance and female representation indicies')

#Plot 6 "Effect of threshold on the relationship between BWL/GDRT/GDR and attendance"

plot_model(H3modelint1, type = "pred", terms = c("BWLLog", "threshold"),
           title="a)")
plot_model(H3modelint2, type = "pred", terms = c("BWLLog", "threshold"),
           title="b)")

plot_model(H3modelint3, type = "pred", terms = c("GDRLog", "threshold"),
           title="a)")
#we take this
plot_model(H3modelint4, type = "pred", terms = c("GDRLog", "threshold"),
           title="b)")
plot_model(H3modelint5, type = "pred", terms = c("GDRTLog", "threshold"),
           title="a)")
#we take this
plot_model(H3modelint6, type = "pred", terms = c("GDRTLog", "threshold"),
           title="b)")

