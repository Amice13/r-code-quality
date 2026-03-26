####REPLICATION CODE ####
#for "Containing Nationalism: Culture, Economics and Indirect Rule in Corsica"
#July 2020; for questions on this code or the data please contact sean.muller@unil.ch 

#set working directory 
setwd("~/6.pending/2.articles/Corsica paper 2020/FINAL CPS/replication")
system('defaults write org.R-project.R force.LANG en_US.UTF-8')

#load libraries
library(foreign)
library(RColorBrewer)    
library(stargazer)
library(sjPlot) 
library(sjmisc)
library(sjlabelled)
library(ggplot2)
library(ggpubr)
library(readxl)
library(car)
library(xlsx)
library(systemfit)
library(jtools)
library(interflex)
library(xtable)
library(corrplot)
library(effects)
library(ggpubr)
library(rgdal)
library(sp)
library(maptools)
library(maps)
library(expss)


#load the 2 datasets
Data <- read_excel("SurveyData.xlsx", 1) #aggregate data/360 communes
Communes <- read_excel("CorsicaData.xlsx") #expert survey replies

###Figure 1a: the map####
#load map file
map <- readOGR(dsn="shape")  

#add electoral results: check that merging variable of same type
str(map$INSEE_COM)
str(Communes$INSEE_COM)

#...and same order
map <- map[order(map$INSEE_COM),]
Communes <- Communes[order(Communes$INSEE_COM),]
identical(map$INSEE_COM,Communes$INSEE_COM ) #should be TRUE

#merge
merged <- merge(map, Communes, by="INSEE_COM")
str(merged)

#plot 2015 regional election results of nationalist parties, 1st round
natlistvotechng.palette <- colorRampPalette(c("white", "black"), space = "rgb")
spplot(merged,"Nat2015st",col.regions=natlistvotechng.palette(20), main="Nationalist Vote (2015 RE, 1st round)") 

####Figure 1b: the barplot####
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}
p<-ggplot(data=Communes, aes(x=reorder(name, -Nat2015st), Nat2015st)) +
  geom_bar(stat="identity") + scale_x_discrete(breaks = every_nth(n = 5))
p + coord_flip()+ggtitle("nationalist vote shares in 2015 \n (regional elections, first round)") +
  xlab("commune") + ylab("% nationalists")



####SUR ANALYSIS (towards Table 1)#####

##1. recode SURVEY data; new variables at EXPERT level
attach(Data)

#Corsican culture/language: recoding of Lang: merge categories 4 and 5, since 5 is very small --> Lang2
Lang2 <- NA
Lang2 <- Lang
Lang2[Lang==5] <- 4
table(Lang,Lang2)

#Indirect Rule/IR: 1 = Domination by the same party over the entire period
IR <- NA
IR <- Power
IR[Power==2] <- 1 #Domination by the same party over the entire period
IR[Power==1] <- 0 #Frequent turnover in power
IR[Power==3] <- 0 #Domination by a party that came to power during that period
IR[Power==4] <- 0 #Recent defeat of a well anchored party
table(Power,IR)

#CDL/cultural division of labor variable 1: Recent newcomers to Corsica more easily find LOWER-level jobs?
CDLlow <- Jobs1

#CDL variable 2: Recent newcomers to Corsica more easily find TOP-level jobs?
CDLtop <- Jobs2

##2. avg up to the commune level
##step 0: merge all relevant variables (old and new plus INSEE) into new dataframe

attach(Data)
Experts <- data.frame(Autonomy2,Lang2,CDLlow,CDLtop,IR,Left,Right,Nationalist,Tourism,INSEE_COM)
str(Experts)

#step 1: aggregate everything up to commune level 

Experts2 <- aggregate(Experts[,c(1:9)], by = list(Experts$INSEE_COM), mean,na.rm = TRUE)
names(Experts2)[1] <- "INSEE_COM"
str(Experts2)

#step 2: load observational data file
Communes <- read_excel("CorsicaData.xlsx") 
attach(Communes)
str(Communes)

#step 3: merge the two on INSEE

averaged <- merge(Experts2,Communes,by="INSEE_COM")
str(averaged)
attach(averaged)

##3. new variables AFTER averaging up

#redummy IR after merging by commune  -> create "IRcomm"
averaged$IRcomm <- NA
averaged$IRcomm <- averaged$IR
averaged$IRcomm[averaged$IR>0.5] <- 1 #above or equal to .5
averaged$IRcomm[averaged$IRcomm==0.5] <- 1 #above .5
averaged$IRcomm[averaged$IRcomm<0.5] <- 0 #below .5

table(averaged$IRcomm,averaged$IR)

#alternative: dummy Lang2 after merging by commune  -> create "Langcomm" (>2.4, where 3 = 40%+)
averaged$Langcomm <- NA
averaged$Langcomm <- averaged$Lang2
averaged$Langcomm[averaged$Lang2>2.4] <- 1 #above 2
averaged$Langcomm[averaged$Lang2<2.4] <- 0 #below 2

table(averaged$Langcomm,averaged$Lang2)

table(averaged$Langcomm)
table(averaged$IRcomm)

addmargins(table(averaged$Langcomm,averaged$IRcomm))
round(prop.table(table(averaged$Langcomm,averaged$IRcomm),1),2)
cor(averaged$Langcomm,averaged$IRcomm,use = "pairwise.complete.obs")

summary(averaged$Langcomm)
summary(averaged$IRcomm)

summary(averaged$Nat2015st)
hist(averaged$Nat2015st)

summary(averaged$Autonomy2)
hist(averaged$Autonomy2)

#standardize tourism (Q: 1-9), autonomy demands and language

averaged$TourismSt <- standardize(averaged$Tourism)
averaged$AutonomySt <- standardize(averaged$Autonomy2)
averaged$LangSt <- standardize(averaged$Lang2)

#partyID

averaged$party <- 0 #default = all experts are from non-nationalist parties
averaged$party[averaged$Nationalist<1 & averaged$Nationalist>0] <- 1 #nationalist and non-nationalist experts mixed
averaged$party[averaged$Nationalist==1] <- 2 #all experts are nationalist

table(averaged$party)


#new, re-dummied CDL variables
averaged$CDLlow2[averaged$CDLlow < 2.6] <- 0
averaged$CDLlow2[averaged$CDLlow > 2.59] <- 1
table(averaged$CDLlow,averaged$CDLlow2)
table(averaged$CDLlow2)

averaged$CDLtop2[averaged$CDLtop < 2.6] <- 0
averaged$CDLtop2[averaged$CDLtop > 2.59] <- 1
table(averaged$CDLtop,averaged$CDLtop2)
table(averaged$CDLtop2)



#####TABLE 1####
#M1
r1 <- Autonomy2 ~ Lang2+IRcomm+TourismSt
r2 <- Nat2015st ~ Autonomy2+Lang2+IRcomm+TourismSt
fitsur <- systemfit(list(demandreg = r1, votereg = r2), data=averaged)
summary(fitsur, residCov = FALSE, equations = FALSE)

#M2
r1 <- Autonomy2 ~ Lang2*IRcomm+TourismSt
r2 <- Nat2015st ~ Autonomy2*IRcomm+Lang2+TourismSt
fitsur <- systemfit(list(demandreg = r1, votereg = r2), data=averaged)
summary(fitsur, residCov = FALSE, equations = FALSE)


#M3
r1 <- Autonomy2 ~ Lang2*IRcomm+TourismSt+party+CDLlow2+CDLtop2+PcUnempl
r2 <- Nat2015st ~ Autonomy2*IRcomm+Lang2+TourismSt+party+CDLlow2+CDLtop2+PcUnempl
fitsur <- systemfit(list(demandreg = r1, votereg = r2), data=averaged)
summary(fitsur, residCov = FALSE, equations = FALSE)


#####ME plots for SUR models (Figures 2 and 3)####
averaged = apply_labels(averaged,IRcomm = "Indirect rule at local level")
attach(averaged)
val_lab(IRcomm) = num_lab("
            0 absent
             1 present    
")

#Figure 2, left
m1 <- lm(Autonomy2 ~ Lang2*IRcomm+TourismSt, data=averaged)
plot_model(m1, type = "pred", terms = c("Lang2","IRcomm"),
           ci.lvl = .95,colors="bw",
           axis.title =c("Corsican Culture","predicted values of autonomy demand"))

#Figure 2, right
plot_model(m1, type = "pred", terms = "TourismSt",
           axis.lim=c(2,7),colors="bw",
           axis.title =c("Local importance of  tourism (standardized)",
                         "predicted values of autonomy demand"))

#Figure 3
m4 <- lm(Nat2015st ~ AutonomySt*IRcomm+Lang2+TourismSt, data=averaged)
plot_model(m4, type = "pred", terms = c("AutonomySt","IRcomm"),
           colors="bw",ci.lvl = .95,
           axis.title =c("Autonomy demands","predicted values for nationalist vote"))


#####Appendix 2: Descriptive info on variables, sample and population#####

summary(Communes) #all 360 communes
sapply(Communes, sd, na.rm=TRUE)
summary(averaged) #only the communes with expert info on them
sapply(averaged, sd, na.rm=TRUE)
round(prop.table(table(Communes$coast)),2)
round(prop.table(table(averaged$coast)),2)
round(prop.table(table(Communes$agglo)),2)
round(prop.table(table(averaged$agglo)),2)
round(prop.table(table(averaged$IRcomm)),2)



####Appendix 4: correlation plots between main IVs and DVs####
# correlation among two DVs (Figure 4a)
sp <- ggscatter(averaged, x = "AutonomySt", y = "Nat2015st",
                add = "reg.line",
                ylab="Nationalist vote share, 2015 regional elections, first round",
                xlab="local demand for more regional autonomy (standardized)",
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE)
sp + stat_cor(method = "pearson")
round(cor(averaged$Autonomy2,averaged$Nat2015st,use = "pairwise.complete.obs"),3)


#correlation among two IVs: tourism & language (Figure 4b)
sp <- ggscatter(averaged, x = "TourismSt", y = "Lang2",
                add = "reg.line",
                add.params = list(color = "blue", fill = "lightgray"),
                conf.int = TRUE,
                ylab="prevalence of Corsican speakers in commune",
                xlab="local importance of tourism (standardized)")
sp + stat_cor(method = "pearson")
round(cor(averaged$TourismSt,averaged$Lang2,use = "pairwise.complete.obs"),3)


####Appendix 5: Full SUR models####
#m4
r1 <- Autonomy2 ~ Lang2*IRcomm+TourismSt+pcWorkers+pcFarmers+PcAbove60
r2 <- Nat2015st ~ Autonomy2*IRcomm+Lang2+TourismSt+pcWorkers+pcFarmers+PcAbove60
fitsur <- systemfit(list(demandreg = r1, votereg = r2), data=averaged)
summary(fitsur, residCov = FALSE, equations = FALSE)

#m5
r1 <- Autonomy2 ~ Lang2*IRcomm+TourismSt+Area+pop13+agglo+coast
r2 <- Nat2015st ~ Autonomy2*IRcomm+Lang2+TourismSt+Area+pop13+agglo+coast
fitsur <- systemfit(list(demandreg = r1, votereg = r2), data=averaged)
summary(fitsur, residCov = FALSE, equations = FALSE)


#####Appendix 6: Interaction Diagnostics#####

#Stage 1: pred. demand
inter.raw(Y = "Autonomy2", D = "IRcomm", X = "Lang2", data = averaged)
inter.binning(Y = "Autonomy2", D = "IRcomm", X = "Lang2",
              data = averaged, na.rm=T)

#Stage 2: pred. voting
inter.raw(Y = "Nat2015st", D = "IRcomm", X = "AutonomySt", data = averaged,
          Ylabel = "Nationaist voting", Dlabel = "IRcomm", Xlabel="Demand")
inter.binning(Y = "Nat2015st", D = "IRcomm", X = "AutonomySt",
              data = averaged, na.rm=T)


####Appendix 7: language/culture as dummy####
averaged$Langcomm <- as.factor(averaged$Langcomm)
m3 <- lm(Autonomy2 ~ Langcomm*IRcomm+TourismSt, data=averaged)
plot_model(m3, type = "pred", terms = c("Langcomm","IRcomm"),
           colors="bw",
           axis.title =c("Corsican Culture","predicted values of autonomy demand"))


####Other Diagnostics: descriptives for survey data only, by commune####
Langmean <- aggregate(Lang, by = list(INSEE_COM), mean,na.rm = TRUE) #sample SD, not population SD...
LangSD <- aggregate(Lang, by = list(INSEE_COM), sd,na.rm = TRUE) 
Autonmean<- aggregate(Autonomy2, by = list(INSEE_COM), mean,na.rm = TRUE)
AutonSD<- aggregate(Autonomy2, by = list(INSEE_COM), sd,na.rm = TRUE)
IRmean<- aggregate(IR, by = list(INSEE_COM), mean,na.rm = TRUE)
IRSD<- aggregate(IR, by = list(INSEE_COM), sd,na.rm = TRUE)


library(dplyr)
Nexperts <- Data %>%
  group_by(INSEE_COM) %>%
  summarise(Nexeprts = n())

ExpertsSummary <- cbind(Nexperts[c(1:105),],round(Langmean$x,2),
                        round(LangSD$x,2),round(Autonmean$x,2),round(AutonSD$x,2),
                        round(IRmean$x,2),round(IRSD$x,2))
colnames(ExpertsSummary) <-  c("INSEE","Nexperts","LanguageMean","LanguageSD",
                               "AutonomyMean","AutonomySD","IR_Mean","IR_SD")
write.xlsx(ExpertsSummary, "ExpertsSummary.xlsx")

round(prop.table(table(Lang,Nationalist),1),2)
round(prop.table(table(Lang,Nationalist),2),2)
round(prop.table(table(IR,Nationalist),2),2)


cor(Autonomy2,Nationalist,use = "pairwise.complete.obs")
cor(Lang,Nationalist,use = "pairwise.complete.obs")
cor(IR,Nationalist,use = "pairwise.complete.obs")

#autonomy demands

group_by(Data, Nationalist) %>%
  summarise(
    count = n(),
    mean = mean(Autonomy2, na.rm = TRUE),
    median= median(Autonomy2, na.rm = TRUE),
    sd = sd(Autonomy2, na.rm = TRUE))

ggboxplot(Data, x = "Nationalist", y = "Autonomy2", 
          color = "Nationalist", palette = c("#00AFBB", "#E7B800"),
          ylab = "Autonomy Demands", xlab = "Nationalist Y/N")

with(Data, shapiro.test(Autonomy2[Nationalist == 0]))# p = 0.1
with(Data, shapiro.test(Autonomy2[Nationalist == 1]))# p = 0.1

wilcox.test(Autonomy2 ~ Nationalist, data = Data,
            exact = FALSE)
t.test(Autonomy2 ~ Nationalist, alternative = "two.sided", var.equal = FALSE)

#language

group_by(Data, Nationalist) %>%
  summarise(
    count = n(),
    mean = mean(Lang, na.rm = TRUE),
    median= median(Lang, na.rm = TRUE),
    sd = sd(Lang, na.rm = TRUE))

ggboxplot(Data, x = "Nationalist", y = "Lang", 
          color = "Nationalist", palette = c("#00AFBB", "#E7B800"),
          ylab = "Language spoken", xlab = "Nationalist Y/N")

with(Data, shapiro.test(Lang[Nationalist == 0]))# p = 0.1
with(Data, shapiro.test(Lang[Nationalist == 1]))# p = 0.1

wilcox.test(Lang ~ Nationalist, data = Data,
            exact = FALSE)
t.test(Lang ~ Nationalist, alternative = "two.sided", var.equal = FALSE)

#IR

group_by(Data, Nationalist) %>%
  summarise(
    count = n(),
    mean = mean(IR, na.rm = TRUE),
    median= median(IR, na.rm = TRUE),
    sd = sd(IR, na.rm = TRUE))

ggboxplot(Data, x = "Nationalist", y = "IR", 
          color = "Nationalist", palette = c("#00AFBB", "#E7B800"),
          ylab = "Indirect rule", xlab = "Nationalist Y/N")

with(Data, shapiro.test(IR[Nationalist == 0]))# p = 0.1
with(Data, shapiro.test(IR[Nationalist == 1]))# p = 0.1

wilcox.test(IR ~ Nationalist, data = Data,
            exact = FALSE)
t.test(IR ~ Nationalist, alternative = "two.sided", var.equal = FALSE)

