load("./data/individual_diff.rdata")
source("tools/toolbox.R")
library(stargazer)
names(full)
library(ggplot2)
fu<-strsplit(full$district_pvi,"+")
p<-sapply(fu,"[[",1)
num<-as.numeric(sapply(fu,"[[",3))
num<-ifelse(p=="D",-1*num,num)
full$num<-num
full$num<-ifelse(is.na(full$num),0,full$num)


R<-full[full$party=="R",]
D<-full[full$party=="D",]

mover<-lm(move~winner+party+party*winner,data=full)

### Reviewer Comment : We should exclude single party districts from the analysis

spd<-c("CA-12", "CA-18", "CA-29", "CA-34", "CA-38", "CA-44", "CA-53", "WA-10")
full2<-full[!full$District%in%spd,]
full2$loser<-full2$winner*-1
full$loser<-full$winner*-1

mover2<-lm(move~loser+party+party*loser,data=full2)
mover<-lm(move~loser+party+party*loser,data=full)

# Creating Table 6
stargazer(mover,mover2,type="latex",out ="./appendix/tables/table6.tex",column.labels = c("Original Analysis","Without Same Party"),covariate.labels = c("Loser","Republican","Republican Loser"),dep.var.labels = c("Movement Right"),style = "apsr",label = "sameparty",title  = "Original Analysis and Removal of Same Party")


## Controlling for PVI: We create Figure 4 for the Main Paper

mover<-lm(move~winner+party+party*winner,data=full)
summary(mover)

m0<-lm(move~winner+party+party*winner,data=full)
summary(m0)

m3<-lm(move3~winner+party+party*winner,data=full)
summary(m3)



full$Incumbent<-ifelse(full$Incumbent=="","no",full$Incumbent)
full$Incumbent<-ifelse(full$Incumbent=="not","no",full$Incumbent)


full$pvi<-full$num

rep<-full[full$party=="R",]
dem<-full[full$party=="D",]

dem$loser<-dem$winner*-1
rep$loser<-rep$winner*-1



model1<-lm(move~loser+pvi+Incumbent,data=dem)
model2<-lm(move3~loser+pvi+Incumbent,data=dem)

model3<-lm(move~loser+pvi+Incumbent,data=rep)
model4<-lm(move3~loser+pvi+Incumbent,data=rep)


interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier


# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          Model = "Democrats Absolut")

model1Frame$Variable<-c('Intercept',"Loser","Lean Conservative","Incumbent")

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          Model = "Democrats 3 Errors")

# Combine these data.frames

# Combine these data.frames
model2Frame$Variable<-c('Intercept',"Loser","Lean Conservative","Incumbent")



model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          Model = "Republicans Absolute")

model3Frame$Variable<-c('Intercept',"Loser","Lean Conservative","Incumbent")

model4Frame <- data.frame(Variable = rownames(summary(model4)$coef),
                          Coefficient = summary(model4)$coef[, 1],
                          SE = summary(model4)$coef[, 2],
                          Model = "Republicans 3 Errors")

# Combine these data.frames
model4Frame$Variable<-c('Intercept',"Loser","Lean Conservative","Incumbent")


level_order1<-c('Intercept',"Loser","Lean Conservative","Incumbent")

allModelFrame <- data.frame(rbind(model1Frame, model2Frame,model3Frame,model4Frame))  # etc.

colscheme<-c("light blue","blue","red","dark red")

zp1<-mult_plot(allModelFrame,level_order=level_order1,sz=14,l=1.5,legend = T)
zp1
pdf('figure4.pdf',width = 12, height = 10)
zp1
dev.off()


# The corresponding Table (7)
stargazer(model1,model2,model3,model4,type="latex",out ="./appendix/tables/table7.tex",covariate.labels = level_order1,dep.var.labels = c("Movement Right","Movement 3 Errors","Movement Right","Movement 3 Errors"),label = "indeffects",title = "Individual Level Results (as Coefficient Plot)",column.labels = c("Democrats Absolut","Democrats 3 Errors","Republicans Absolut","Republicans 3 Errors"),style = "apsr")



full$loser<-full$winner*-1

### with lean loser interactions: Table 8
model1<-lm(move~loser*party,data=full)
model2<-lm(move3~loser*party,data=full)
model3<-lm(move~loser*party+pvi+Incumbent,data=full)
model4<-lm(move3~loser*party+pvi+Incumbent,data=full)



stargazer(model1,model2,model3,model4,type="latex",out ="./appendix/tables/table8.tex",covariate.labels = c("Loser","Party Republican","PVI","Incumbent","Loser*Party Republican"),dep.var.labels = c("Movement Right","Movement 3 Errors","Movement Right","Movement 3 Errors"),label = "indeffects_nocontrols",title = "Individual Robustness without Controls",column.labels = c("Absolut","3 Errors","Absolut + Controls","3 Errors + Controls"),style = "apsr")

## Overview dataset (not in paper)


full_s<-full[,c("nominate","move","theta","theta_after","uncontested")]
names(full_s)<-c("NOMINATE","Movement","Position before","Position after","Uncontested")
stargazer(full_s,out ="./appendix/tables/table3.tex",title = "Descriptive Statistics")
