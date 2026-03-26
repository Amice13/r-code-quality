#Load data
rm(list=ls())
setwd("/Users/luyuema/Dropbox/method")
load("WV6_Data_R_v_2016_01_01.rdata")
WV<-WV6_Data_R

# Germany dataset
gmds<-WV[WV$V2 == 276,]
#US subest
usds<-WV[WV$V2 == 840,]

#Recode GM missing value function
recode.missing.col <- function (col.name) {
  column <- gmds[,col.name]
  column[column %in% c(-2,-3,-4,-5)] <- NA
  column[column == -1] <- mean(column, na.rm = TRUE)
  return(column)
}

#Recode US missing value function
recode.missing.col.us <- function (col.name) {
  column <- usds[,col.name]
  column[column %in% c(-2,-3,-4,-5)] <- NA
  column[column == -1] <- mean(column, na.rm = TRUE)
  return(column)
}



#Recode variables
#Recode indi.trust in gmds
indi.trust.cols <- c("V102", "V103", "V104", "V105", "V106","V107")
gmds[,indi.trust.cols] <- do.call("cbind", lapply(indi.trust.cols, recode.missing.col))

for (indi.trust.col in indi.trust.cols) {
  gmds[,indi.trust.col] <- recode(as.character(gmds[,indi.trust.col]), '4'='0', '3'='1','2'='2', '1'='3')
  gmds[,indi.trust.col] <- as.numeric(gmds[,indi.trust.col])
}
gmds$indi.trust <- apply(gmds[,indi.trust.cols], 1, mean)
hist(gmds$indi.trust)
summary(gmds$indi.trust)
sd(gmds$indi.trust, na.rm = TRUE)

#Recode indi.trust in usds
indi.trust.cols <- c("V102", "V103", "V104", "V105", "V106","V107")
usds[,indi.trust.cols] <- do.call("cbind", lapply(indi.trust.cols, recode.missing.col.us))
for (indi.trust.col in indi.trust.cols) {
  usds[,indi.trust.col] <- recode(as.character(usds[,indi.trust.col]), '4'='0', '3'='1','2'='2', '1'='3')
  usds[,indi.trust.col] <- as.numeric(usds[,indi.trust.col])
}
usds$indi.trust <- apply(usds[,indi.trust.cols], 1, mean)
hist(usds$indi.trust)
summary(usds$indi.trust)
sd(usds$indi.trust, na.rm = TRUE)

#t.test of indi.trust
t.test(usds$indi.trust,gmds$indi.trust, var.equal=TRUE)


#Recode poli.trust in gmds
poli.insti.trust.cols<-c("V115","V116","V117","V118")
gmds[,poli.insti.trust.cols] <- do.call("cbind", lapply(poli.insti.trust.cols, recode.missing.col))
for (poli.insti.trust.col in poli.insti.trust.cols) {
  gmds[,poli.insti.trust.col] <- recode(as.character(gmds[,poli.insti.trust.col]), '4'='0', '3'='1','2'='2', '1'='3')
  gmds[,poli.insti.trust.col] <- as.numeric(gmds[,poli.insti.trust.col])
}
gmds$poli.insti.trust <- apply(gmds[,poli.insti.trust.cols], 1, mean)
hist(gmds$poli.insti.trust)
summary(gmds$poli.insti.trust)
sd(gmds$poli.insti.trust, na.rm = TRUE)

#Recode poli.trust in usds
poli.insti.trust.cols<-c("V115","V116","V117","V118")
usds[,poli.insti.trust.cols] <- do.call("cbind", lapply(poli.insti.trust.cols, recode.missing.col.us))
for (poli.insti.trust.col in poli.insti.trust.cols) {
  usds[,poli.insti.trust.col] <- recode(as.character(usds[,poli.insti.trust.col]), '4'='0', '3'='1','2'='2', '1'='3')
  usds[,poli.insti.trust.col] <- as.numeric(usds[,poli.insti.trust.col])
}
usds$poli.insti.trust <- apply(usds[,poli.insti.trust.cols], 1, mean)
hist(usds$poli.insti.trust)
summary(usds$poli.insti.trust)
sd(usds$poli.insti.trust, na.rm = TRUE)

#t.test of poli.trust
t.test(usds$poli.insti.trust,gmds$poli.insti.trust, var.equal=TRUE)


#Recode mass.media in gmds
mass.media.cols <- c("V217", "V218","V219","V220")
gmds[,mass.media.cols] <- do.call("cbind", lapply(mass.media.cols, recode.missing.col))
for (mass.media.col in mass.media.cols) {
  gmds[,mass.media.col] <- recode(as.character(gmds[,mass.media.col]), '5'='0', '4'='1','3'='2', '2'='3','1'='4')
  gmds[,mass.media.col] <- as.numeric(gmds[,mass.media.col])
}
gmds$mass.media<- apply(gmds[,mass.media.cols], 1, mean)
hist(gmds$mass.media)
summary(gmds$mass.media)
sd(gmds$mass.media,na.rm = TRUE)

#Recode mass.media in usds
mass.media.cols <- c("V217", "V218","V219","V220")
usds[,mass.media.cols] <- do.call("cbind", lapply(mass.media.cols, recode.missing.col.us))
for (mass.media.col in mass.media.cols) {
  usds[,mass.media.col] <- recode(as.character(usds[,mass.media.col]), '5'='0', '4'='1','3'='2', '2'='3','1'='4')
  usds[,mass.media.col] <- as.numeric(usds[,mass.media.col])
}
usds$mass.media<- apply(usds[,mass.media.cols], 1, mean)
hist(usds$mass.media)
summary(usds$mass.media)
sd(usds$mass.media,na.rm = TRUE)

#t.test of mass media use
t.test(usds$mass.media,gmds$mass.media, var.equal=TRUE)


#Recode internet.use in gmds
internet.use.cols <- c("V223")
gmds[,internet.use.cols] <- do.call("cbind", lapply(internet.use.cols, recode.missing.col))
gmds$internet.use<- apply(gmds[,internet.use.cols], 1, mean)
for (internet.use.col in internet.use.cols) {
  gmds[,internet.use.col] <- recode(as.character(gmds[,internet.use.col]), '5'='0', '4'='1','3'='2', '2'='3','1'='4')
  gmds[,internet.use.col] <- as.numeric(gmds[,internet.use.col])
}

hist(gmds$internet.use)
summary(gmds$internet.use)
sd(gmds$internet.use, na.rm = TRUE)

#Recode internet.use in usds
internet.use.cols <- c("V223")
usds[,internet.use.cols] <- do.call("cbind", lapply(internet.use.cols, recode.missing.col.us))
usds$internet.use<- apply(usds[,internet.use.cols], 1, mean)
for (internet.use.col in internet.use.cols) {
  usds[,internet.use.col] <- recode(as.character(usds[,internet.use.col]), '5'='0', '4'='1','3'='2', '2'='3','1'='4')
  usds[,internet.use.col] <- as.numeric(usds[,internet.use.col])
}
hist(usds$internet.use)
summary(usds$internet.use)
sd(usds$internet.use, na.rm = TRUE)

#t.test of Internet use
t.test(usds$internet.use,gmds$internet.use, var.equal=TRUE)


##Recode poli.part in gmds
poli.part.cols <- c("V85", "V86", "V87", "V88", "V89")
gmds[,poli.part.cols] <- do.call("cbind", lapply(poli.part.cols, recode.missing.col))
for (poli.part.col in poli.part.cols) {
  gmds[,poli.part.col] <- recode(as.character(gmds[,poli.part.col]), '3'='0','2'='1','1'='2')
  gmds[,poli.part.col] <- as.numeric(gmds[,poli.part.col])
}
gmds$poli.part<- apply(gmds[,poli.part.cols], 1, sum)
hist(gmds$poli.part)
summary(gmds$poli.part)
sd(gmds$poli.part,na.rm = TRUE)

#Recode poli.part in usds
poli.part.cols <- c("V85", "V86", "V87", "V88", "V89")
usds[,poli.part.cols] <- do.call("cbind", lapply(poli.part.cols, recode.missing.col.us))
for (poli.part.col in poli.part.cols) {
  usds[,poli.part.col] <- recode(as.character(usds[,poli.part.col]), '3'='0','2'='1','1'='2')
  usds[,poli.part.col] <- as.numeric(usds[,poli.part.col])
}
usds$poli.part<- apply(usds[,poli.part.cols], 1, sum)
hist(usds$poli.part)
summary(usds$poli.part)
sd(usds$poli.part,na.rm = TRUE)

#t.test of political participation
t.test(usds$poli.part,gmds$poli.part, var.equal=TRUE)


#Recode poli.interest in gmds
poli.interest.cols<-c("V84")
gmds[,poli.interest.cols] <- do.call("cbind", lapply(poli.interest.cols, recode.missing.col))
gmds$poli.interest<-apply(gmds[,poli.interest.cols], 1, mean)
for (poli.interest.col in poli.interest.cols) {
  gmds[,poli.interest.col] <- recode(as.character(gmds[,poli.interest.col]), '4'='0', '3'='1','2'='2', '1'='3')
  gmds[,poli.interest.col] <- as.numeric(gmds[,poli.interest.col])
}
hist(gmds$poli.interest)
summary(gmds$poli.interest)
sd(gmds$poli.interest,na.rm = TRUE)

#Recode poli.interest in usds
poli.interest.cols<-c("V84")
usds[,poli.interest.cols] <- do.call("cbind", lapply(poli.interest.cols, recode.missing.col.us))
usds$poli.interest<-apply(usds[,poli.interest.cols], 1, mean)
for (poli.interest.col in poli.interest.cols) {
  usds[,poli.interest.col] <- recode(as.character(usds[,poli.interest.col]), '4'='0', '3'='1','2'='2', '1'='3')
  usds[,poli.interest.col] <- as.numeric(usds[,poli.interest.col])
}
hist(usds$poli.interest)
summary(usds$poli.interest)

#Recode edu in gmds
edu.level.cols<-c("V248")
gmds[,edu.level.cols] <- do.call("cbind", lapply(edu.level.cols, recode.missing.col))
gmds$edu.level<-apply(gmds[,edu.level.cols], 1, mean)
hist(gmds$edu.level)
summary(gmds$edu.level)
sd(gmds$edu.level, na.rm = TRUE)

#Recode edu in usds
edu.level.cols<-c("V248")
usds[,edu.level.cols] <- do.call("cbind", lapply(edu.level.cols, recode.missing.col.us))
usds$edu.level<-apply(usds[,edu.level.cols], 1, mean)
hist(usds$edu.level)

#Recode income in gmds
income.cols<-c("V239")
gmds[,income.cols] <- do.call("cbind", lapply(income.cols, recode.missing.col))
gmds$income<-apply(gmds[,income.cols],1,mean)
hist(gmds$income)
summary(gmds$income)
sd(gmds$income, na.rm = TRUE)

#Recode income in usds
income.cols<-c("V239")
usds[,income.cols] <- do.call("cbind", lapply(income.cols, recode.missing.col.us))
usds$income<-apply(usds[,income.cols],1,mean)
hist(usds$income)
summary(usds$income)

#Recode age in gmds
age.cols<-c("V242")
gmds[,age.cols] <- do.call("cbind", lapply(age.cols, recode.missing.col))
gmds$age<-apply(gmds[,age.cols],1,mean)
hist(gmds$age)
summary(gmds$age)
sd(gmds$age)

#Recode age in usds
age.cols<-c("V242")
usds[,age.cols] <- do.call("cbind", lapply(age.cols, recode.missing.col.us))
usds$age<-apply(usds[,age.cols],1,mean)
hist(usds$age)
summary(usds$age)
sd(usds$age)


#Correlations

cor.test(gmds[,'poli.part'], gmds[,'indi.trust'])
cor.test(gmds[,'poli.part'], gmds[,'poli.insti.trust'])

cor.test(usds[,'poli.part'], usds[,'indi.trust'])
cor.test(usds[,'poli.part'], usds[,'poli.insti.trust'])

cor.test(gmds[,'poli.part'], gmds[,'mass.media'])
cor.test(gmds[,'poli.part'], gmds[,'internet.use'])

cor.test(usds[,'poli.part'], usds[,'mass.media'])
cor.test(usds[,'poli.part'], usds[,'internet.use'])

#correlation tables 
install.packages("xtable")
library(xtable)

#GM cor table
cor.xtable.gm<-xtable(cor(gmds[,c("poli.part", "indi.trust","poli.insti.trust","mass.media", "internet.use","poli.interest","income","edu.level", "age")],  use="complete.obs"))
print.xtable(cor.xtable.gm, type="html", file="cor.xtable.gm.html")

#US cor table
cor.xtable.us<-xtable(cor(usds[,c("poli.part", "indi.trust","poli.insti.trust","mass.media", "internet.use","poli.interest","income","edu.level", "age")],  use="complete.obs"))
print.xtable(cor.xtable.us, type="html", file="cor.xtable.us.html")


#regression GM
gmf<-lm(formula = poli.part~indi.trust+poli.insti.trust+mass.media+internet.use+poli.interest+income+edu.level+age,data = gmds)
summary(gmf)
hist(residuals(gmf))


library(stargazer)
tablegm<-stargazer(gmf, title="Linear Regression Results",align=TRUE, dep.var.labels=c("Non-Institutional Political Participation"), 
                    covariate.labels=c("Trust in Individuals","Trust in Political Institutions","Mass Media Use", "Internet Use","Political Interest","Income","Education Level","Age"),
                    omit.stat=c("LL","ser","f"), type = "text")
write.table(tablegm, file = "tablegm.txt", sep=",",quote = FALSE, row.names = F)


#regression US
usf<-lm(formula = poli.part~indi.trust+poli.insti.trust+mass.media+internet.use+poli.interest+income+edu.level+age,data = usds)
summary(usf)
hist(residuals(usf))

tableus<-stargazer(usf, title="Linear Regression Results",align=TRUE, dep.var.labels=c("Non-Institutional Political Participation"), 
                   covariate.labels=c("Trust in Individuals","Trust in Political Institutions","Mass Media Use", "Internet Use","Political Interest","Income","Education Level","Age"),
                   omit.stat=c("LL","ser","f"), type = "text")
write.table(tableus, file = "tableus.txt", sep=",",quote = FALSE, row.names = F)



hist(residuals(usf))
hist(residuals(gmf))
