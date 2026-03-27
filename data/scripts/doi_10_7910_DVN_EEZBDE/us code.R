library(dplyr)
library(foreign)
getwd()
setwd("/Users/luyuema/Downloads/")

#WV6_Data_R = read.csv("....")
#save(WV6_Data_R, file ='WV6_Data_R_v_2016_01_01.rdata')

rm(list=ls())
#Load data
load("WV6_Data_R_v_2016_01_01.rdata")
WV<-WV6_Data_R
head(WV)
head(WV$V88)
WV$V2
table(WV$V2)

#US subest
usds<-WV[WV$V2 == 840,]
table(is.na(WV$V104))

table(is.na(WV))

#Recode missing value function
recode.missing.col <- function (col.name) {
  column <- usds[,col.name]
  column[column == -2] <- NA
  return(column)
}

#Recode variables
indi.trust.cols <- c("V102", "V103", "V104", "V105", "V106","V107")
usds[,indi.trust.cols] <- do.call("cbind", lapply(indi.trust.cols, recode.missing.col))
for (indi.trust.col in indi.trust.cols) {
  usds[,indi.trust.col] <- recode(as.character(usds[,indi.trust.col]), '4'='0', '3'='1','2'='2', '1'='3')
  usds[,indi.trust.col] <- as.numeric(usds[,indi.trust.col])
}
usds$indi.trust <- apply(usds[,indi.trust.cols], 1, mean)
hist(usds$indi.trust)
summary(usds$indi.trust)
sd(usds$indi.trust, na.rm = TRUE)


poli.insti.trust.cols<-c("V115","V116","V117","V118")
usds[,poli.insti.trust.cols] <- do.call("cbind", lapply(poli.insti.trust.cols, recode.missing.col))
for (poli.insti.trust.col in poli.insti.trust.cols) {
  usds[,poli.insti.trust.col] <- recode(as.character(usds[,poli.insti.trust.col]), '4'='0', '3'='1','2'='2', '1'='3')
  usds[,poli.insti.trust.col] <- as.numeric(usds[,poli.insti.trust.col])
}
usds$poli.insti.trust <- apply(usds[,poli.insti.trust.cols], 1, mean)
hist(usds$poli.insti.trust)
summary(usds$poli.insti.trust)
sd(usds$poli.insti.trust, na.rm = TRUE)


mass.media.cols <- c("V217", "V218","V219","V220")
usds[,mass.media.cols] <- do.call("cbind", lapply(mass.media.cols, recode.missing.col))
for (mass.media.col in mass.media.cols) {
  usds[,mass.media.col] <- recode(as.character(usds[,mass.media.col]), '5'='0', '4'='1','3'='2', '2'='3','1'='4')
  usds[,mass.media.col] <- as.numeric(usds[,mass.media.col])
}
usds$mass.media<- apply(usds[,mass.media.cols], 1, mean)

hist(usds$mass.media)
summary(usds$mass.media)
sd(usds$mass.media,na.rm = TRUE)


internet.use.cols <- c("V223")
usds[,internet.use.cols] <- do.call("cbind", lapply(internet.use.cols, recode.missing.col))
usds$internet.use<- apply(usds[,internet.use.cols], 1, mean)
for (internet.use.col in internet.use.cols) {
  usds[,internet.use.col] <- recode(as.character(usds[,internet.use.col]), '5'='0', '4'='1','3'='2', '2'='3','1'='4')
  usds[,internet.use.col] <- as.numeric(usds[,internet.use.col])
}
hist(usds$internet.use)
summary(usds$internet.use)
sd(usds$internet.use, na.rm = TRUE)


demo.supp.cols <- c("V130")
usds[,demo.supp.cols] <- do.call("cbind", lapply(demo.supp.cols, recode.missing.col))
usds$demo.supp<- apply(usds[,demo.supp.cols], 1, mean)
for (demo.supp.col in demo.supp.cols) {
  usds[,demo.supp.col] <- recode(as.character(usds[,demo.supp.col]), '4'='1', '3'='2','2'='3', '1'='4')
  usds[,demo.supp.col] <- as.numeric(usds[,demo.supp.col])
}
hist(usds$demo.supp)

demo.value.cols <- c("V127","V128", "V129", "demo.supp")
usds[,demo.value.cols] <- do.call("cbind", lapply(demo.value.cols, recode.missing.col))
usds$demo.value<- apply(usds[,demo.value.cols], 1, mean)
summary(usds$demo.value)
hist(usds$demo.value)
sd(usds$demo.value, na.rm = TRUE)


poli.part.cols <- c("V85", "V86", "V87", "V88", "V89")
usds[,poli.part.cols] <- do.call("cbind", lapply(poli.part.cols, recode.missing.col))
for (poli.part.col in poli.part.cols) {
  usds[,poli.part.col] <- recode(as.character(usds[,poli.part.col]), '3'='0','2'='1','1'='2')
  usds[,poli.part.col] <- as.numeric(usds[,poli.part.col])
}
usds$poli.part<- apply(usds[,poli.part.cols], 1, sum)

hist(usds$poli.part)
summary(usds$poli.part)
sd(usds$poli.part,na.rm = TRUE)



poli.interest.cols<-c("V84")
usds[,poli.interest.cols] <- do.call("cbind", lapply(poli.interest.cols, recode.missing.col))
usds$poli.interest<-apply(usds[,poli.interest.cols], 1, mean)
for (poli.interest.col in poli.interest.cols) {
  usds[,poli.interest.col] <- recode(as.character(usds[,poli.interest.col]), '4'='0', '3'='1','2'='2', '1'='3')
  usds[,poli.interest.col] <- as.numeric(usds[,poli.interest.col])
}
hist(usds$poli.interest)
summary(usds$poli.interest)


edu.level.cols<-c("V248")
usds[,edu.level.cols] <- do.call("cbind", lapply(edu.level.cols, recode.missing.col))
usds$edu.level<-apply(usds[,edu.level.cols], 1, mean)
hist(usds$edu.level)

income.cols<-c("V239")
usds[,income.cols] <- do.call("cbind", lapply(income.cols, recode.missing.col))
usds$income<-apply(usds[,income.cols],1,mean)
hist(usds$income)
summary(usds$income)


#Correlation 
cor(usds[,c("poli.part", "internet.use")],  use="complete.obs")
cor(usds[,c("poli.part", "mass.media")],  use="complete.obs")
cor(usds[,c("poli.part", "mass.media","internet.use")],  use="complete.obs")
cor(usds[,c("poli.part", "poli.interest")],  use="complete.obs")
cor(usds[,c("poli.part", "indi.trust","poli.insti.trust")],  use="complete.obs")
cor(usds[,c("poli.part", "income","edu.level")],  use="complete.obs")


install.packages("xtable")
library(xtable)
cor.xtable<-xtable(cor(usds[,c("poli.part", "mass.media", "internet.use","indi.trust","poli.insti.trust","demo.value", "poli.interest","edu.level","income")],  use="complete.obs"))
print.xtable(cor.xtable, type="html", file="cor.xtable.html")



#Run regression 
f1<-lm(formula = poli.part~indi.trust+poli.insti.trust,data = usds)
summary(f1)

f2<-lm(formula = poli.part~indi.trust+poli.insti.trust+mass.media+internet.use,data = usds)
summary(f2)

f3<-lm(formula = poli.part~indi.trust+poli.insti.trust+mass.media+internet.use+demo.value+poli.interest,data = usds)
summary(f3)

f4<-lm(formula = poli.part~indi.trust+poli.insti.trust+mass.media+internet.use+demo.value+poli.interest+income+edu.level,data = usds)
summary(f4)


#Write regression table
library(stargazer)
table1<-stargazer(f1, f2, f3, f4, title="Multiple Regression Results",align=TRUE, dep.var.labels=c("Political Participation"), 
                  covariate.labels=c("Trust in Individuals","Trust in Political Institutions","Mass Media Use", "Internet Use","Democratic Values","Political Interest","Income","Education Level"),
                  omit.stat=c("LL","ser","f"), type = "text")

write.table(table1, file = "table1.txt", sep=",",quote = FALSE, row.names = F)

#residuals
hist(residuals(f1))
hist(residuals(f2))
hist(residuals(f3))
hist(residuals(f4))

qqnorm(residuals(f1))
qqnorm(residuals(f2))
qqnorm(residuals(f3))
qqnorm(residuals(f4))












rm(list=ls())





