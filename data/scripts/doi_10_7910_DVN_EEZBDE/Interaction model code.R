#Load data
rm(list=ls())
setwd("/Users/luyuema/Dropbox/method")
load("WV6_Data_R_v_2016_01_01.rdata")
WV<-WV6_Data_R
library(dplyr)

# Germany dataset
gmds<-WV[WV$V2 == 276,]
#US subest
usds<-WV[WV$V2 == 840,]

#Merge two subsets
ds<- rbind(gmds, usds)

#Dichotomize a variable indicating German = 1 and non-German (US) = 0
table(ds$V2)
ds$german <- ifelse((ds$V2 == 276),1,0)
table(ds$german)
G <- ds$german


#Recode missing value function
recode.missing.col <- function (col.name) {
  column <- ds[,col.name]
  column[column %in% c(-2,-3,-4,-5)] <- NA
  column[column == -1] <- mean(column, na.rm = TRUE)
  return(column)
}


##Recode variables
#Recode indi.trust 
indi.trust.cols <- c("V102", "V103", "V104", "V105", "V106","V107")
ds[,indi.trust.cols] <- do.call("cbind", lapply(indi.trust.cols, recode.missing.col))
for (indi.trust.col in indi.trust.cols) {
  ds[,indi.trust.col] <- recode(as.character(ds[,indi.trust.col]), '4'='0', '3'='1','2'='2', '1'='3')
  ds[,indi.trust.col] <- as.numeric(ds[,indi.trust.col])
}
ds$indi.trust <- apply(ds[,indi.trust.cols], 1, mean)
hist(ds$indi.trust)
summary(ds$indi.trust)
sd(ds$indi.trust, na.rm = TRUE)

#Recode poli.trust 
poli.insti.trust.cols<-c("V115","V116","V117","V118")
ds[,poli.insti.trust.cols] <- do.call("cbind", lapply(poli.insti.trust.cols, recode.missing.col))
for (poli.insti.trust.col in poli.insti.trust.cols) {
  ds[,poli.insti.trust.col] <- recode(as.character(ds[,poli.insti.trust.col]), '4'='0', '3'='1','2'='2', '1'='3')
  ds[,poli.insti.trust.col] <- as.numeric(ds[,poli.insti.trust.col])
}
ds$poli.insti.trust <- apply(ds[,poli.insti.trust.cols], 1, mean)
hist(ds$poli.insti.trust)
summary(ds$poli.insti.trust)
sd(ds$poli.insti.trust, na.rm = TRUE)

#Recode mass.media 
mass.media.cols <- c("V217", "V218","V219","V220")
ds[,mass.media.cols] <- do.call("cbind", lapply(mass.media.cols, recode.missing.col))
for (mass.media.col in mass.media.cols) {
  ds[,mass.media.col] <- recode(as.character(ds[,mass.media.col]), '5'='0', '4'='1','3'='2', '2'='3','1'='4')
  ds[,mass.media.col] <- as.numeric(ds[,mass.media.col])
}
ds$mass.media<- apply(ds[,mass.media.cols], 1, mean)
hist(ds$mass.media)
summary(ds$mass.media)
sd(ds$mass.media,na.rm = TRUE)

#Recode internet.use
internet.use.cols <- c("V223")
ds[,internet.use.cols] <- do.call("cbind", lapply(internet.use.cols, recode.missing.col))
ds$internet.use<- apply(ds[,internet.use.cols], 1, mean)
for (internet.use.col in internet.use.cols) {
  ds[,internet.use.col] <- recode(as.character(ds[,internet.use.col]), '5'='0', '4'='1','3'='2', '2'='3','1'='4')
  ds[,internet.use.col] <- as.numeric(ds[,internet.use.col])
}
hist(ds$internet.use)
summary(ds$internet.use)
sd(ds$internet.use, na.rm = TRUE)

##Recode poli.part 
poli.part.cols <- c("V85", "V86", "V87", "V88", "V89")
ds[,poli.part.cols] <- do.call("cbind", lapply(poli.part.cols, recode.missing.col))
for (poli.part.col in poli.part.cols) {
  ds[,poli.part.col] <- recode(as.character(ds[,poli.part.col]), '3'='0','2'='1','1'='2')
  ds[,poli.part.col] <- as.numeric(ds[,poli.part.col])
}
ds$poli.part<- apply(ds[,poli.part.cols], 1, sum)
hist(ds$poli.part)
summary(ds$poli.part)
sd(ds$poli.part,na.rm = TRUE)

#Recode poli.interest 
poli.interest.cols<-c("V84")
ds[,poli.interest.cols] <- do.call("cbind", lapply(poli.interest.cols, recode.missing.col))
ds$poli.interest<-apply(ds[,poli.interest.cols], 1, mean)
for (poli.interest.col in poli.interest.cols) {
  ds[,poli.interest.col] <- recode(as.character(ds[,poli.interest.col]), '4'='0', '3'='1','2'='2', '1'='3')
  ds[,poli.interest.col] <- as.numeric(ds[,poli.interest.col])
}
hist(ds$poli.interest)
summary(ds$poli.interest)
sd(ds$poli.interest,na.rm = TRUE)

#Recode edu 
edu.level.cols<-c("V248")
ds[,edu.level.cols] <- do.call("cbind", lapply(edu.level.cols, recode.missing.col))
ds$edu.level<-apply(ds[,edu.level.cols], 1, mean)
hist(ds$edu.level)
summary(ds$edu.level)
sd(ds$edu.level, na.rm = TRUE)

#Recode income 
income.cols<-c("V239")
ds[,income.cols] <- do.call("cbind", lapply(income.cols, recode.missing.col))
ds$income<-apply(ds[,income.cols],1,mean)
hist(ds$income)
summary(ds$income)
sd(ds$income, na.rm = TRUE)

#Recode age 
age.cols<-c("V242")
ds[,age.cols] <- do.call("cbind", lapply(age.cols, recode.missing.col))
ds$age<-apply(ds[,age.cols],1,mean)
hist(ds$age)
summary(ds$age)
sd(ds$age)

#Interaction model
f1<-lm(formula = poli.part~indi.trust+poli.insti.trust+mass.media+internet.use+poli.interest+edu.level+income+age,data = ds)
summary(f1)


f2<-lm(formula = poli.part~indi.trust+poli.insti.trust+mass.media+internet.use+poli.interest+edu.level+income+age+indi.trust*G+poli.insti.trust*G+mass.media*G+internet.use*G+poli.interest*G+edu.level*G+income*G+age*G,data = ds)
summary(f2)

library(stargazer)
tablef<- stargazer(f1,f2, title="Multiple Linear Regression Models with Interaction Effects",align=TRUE, dep.var.labels=c("Non-Institutional Political Participation"), 
                   column.labels = c("Main Effects", "Interaction Effects"), omit.stat=c("LL","ser","f"), type = "text")

write.table(tablef, file = "tablef.txt", sep=",",quote = FALSE, row.names = F)


#Visualizing results 
library(ggplot2)
library(effects)

#Run interaction of poli.insti.trust 
inter.poli.insti.trust<- effect('poli.insti.trust*G', f2,
                         se=TRUE)

#Put data in data frame 
inter.poli.insti.trust.df <- as.data.frame(inter.poli.insti.trust)

# Relable them to put them back in order
inter.poli.insti.trust.df$Country<-factor(inter.poli.insti.trust.df$G,
                                          levels=c(0, 1), 
                                          labels=c("United States", "Germany"))

#Create plot

Plot.poli.insti.trust<-ggplot(inter.poli.insti.trust.df[complete.cases(inter.poli.insti.trust.df),], 
  aes(x=poli.insti.trust, y=fit, group=Country))+
  geom_line(size=2, aes(color=Country))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill= Country),alpha=.2)+
  ylab("Non-Institutional Political Participation")+
  xlab("Trust in political Institutions")+
  ggtitle("Trust in Political Institutions as a Predictor of Non-Institutional Political Participation in US and Germany")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")
  
Plot.poli.insti.trust

#Run interaction of internet.use

inter.internet.use<- effect('internet.use*G', f2,
                                se=TRUE)

#Put data in data frame 
inter.internet.use.df <- as.data.frame(inter.internet.use)

# Relable them to put them back in order
inter.internet.use.df$Country<-factor(inter.internet.use.df$G,
                                          levels=c(0, 1), 
                                          labels=c("United States", "Germany"))

#Create plot

Plot.internet.use<-ggplot(inter.internet.use.df[complete.cases(inter.internet.use.df),], 
                              aes(x=internet.use, y=fit, group=Country))+
  geom_line(size=2, aes(color=Country))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,fill= Country),alpha=.2)+
  ylab("Non-Institutional Political Participation")+
  xlab("Internet Use")+
  ggtitle("Internet Use as a Predictor of Non-Institutional Political Participation in US and Germany")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")

Plot.internet.use



?effect




