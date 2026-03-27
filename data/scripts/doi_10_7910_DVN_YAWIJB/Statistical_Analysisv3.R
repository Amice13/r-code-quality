library(foreign) #stata file
library(MASS) #ordinal logistic regression
library(countrycode) #ID variable 
library(ordinal) #clmm ologit regression
library(lme4)  #random effects logits
library(stargazer) #LaTeX tables 
library(Zelig)

#setwd("C:/Users/Josh Jackson/Dropbox/Democracy and Personal Integrity/ITT Data")

#Import ITT file 
itt.final= read.csv("itt_v2_final.csv")
itt.final <- subset(itt.final, select = c("idvar", 
                                        "year",
                                        "countryname",
                                        "gdppercap",
                                        "population",
                                        "RstrctAccess",
                                        "xconst",
                                        "parcomp",
                                        "democracy",
                                        "lotPolCri",
                                        "lotPolDis",
                                        "lotPolMar",
                                        "v2x_polyarchy",
                                        "v2x_api",
                                        "v2x_mpi",
                                        "v2x_jucon",
                                        "INJUD"))


head(itt.final)  
summary(itt.final)   #we transform levels of torture variables to lotPolCri (etc) with negative values coded as NAs and use these as DVs in our analyses


# create a binary version of our originally ordinal variable
itt.final$crim_bin <- as.numeric(itt.final$lotPolCri > 0)
itt.final$marg_bin <- as.numeric(itt.final$lotPolMar > 0)
itt.final$diss_bin <- as.numeric(itt.final$lotPolDis > 0)


### Binary Logits, police agency, random effects on country

## Competition Indicators
    
#Vreeland's indicator for democracy (ACLP)

summary(crim.bin1<-glmer(crim_bin ~ democracy + log(gdppercap) + log(population ) + RstrctAccess + (1|idvar), data = itt.final, family=binomial(link="logit"))
)
summary(diss.bin1<-glmer(diss_bin ~ democracy + log(gdppercap) + log(population ) + RstrctAccess + (1|idvar), data = itt.final, family=binomial(link="logit"))
)
summary(marg.bin1<-glmer(marg_bin ~ democracy + log(gdppercap) + log(population ) + RstrctAccess + (1|idvar), data = itt.final, family=binomial(link="logit"))
)

    #Parcomp

summary(crim.bin2<-glmer(crim_bin ~ parcomp + log(gdppercap) + log(population ) + RstrctAccess + (1|idvar), data = itt.final, family=binomial(link="logit"))
)
summary(diss.bin2<-glmer(diss_bin ~ parcomp + log(gdppercap) + log(population ) + RstrctAccess + (1|idvar), data = itt.final, family=binomial(link="logit"))
)
summary(marg.bin2<-glmer(marg_bin ~ parcomp + log(gdppercap) + log(population ) + RstrctAccess + (1|idvar), data = itt.final, family=binomial(link="logit"))
)

    #Polyarchy

summary(crim.bin3<-glmer(crim_bin ~ v2x_polyarchy + log(gdppercap) + log(population ) + RstrctAccess + (1|idvar), data = itt.final, family=binomial(link="logit"))
)
summary(diss.bin3<-glmer(diss_bin ~ v2x_polyarchy + log(gdppercap) + log(population ) + RstrctAccess + (1|idvar), data = itt.final, family=binomial(link="logit"))
)
summary(marg.bin3<-glmer(marg_bin ~ v2x_polyarchy + log(gdppercap) + log(population ) + RstrctAccess + (1|idvar), data = itt.final, family=binomial(link="logit"))
)



### Coefficient Plot

##Creating coefficient plot 

coef(summary(crim.bin1))  #ACLP
coef(summary(crim.bin2))  #Parcomp
coef(summary(crim.bin3))  #polyarchy

coef(summary(diss.bin1))
coef(summary(diss.bin2))
coef(summary(diss.bin3))

coef(summary(marg.bin1))
coef(summary(marg.bin2))
coef(summary(marg.bin3))


##Competition Dependent Variables 

first.matrix.competition<- rbind(coef(summary(diss.bin1))[2,1:2],
					   coef(summary(diss.bin2))[2,1:2],
		                     coef(summary(diss.bin3))[2,1:2],
					   coef(summary(crim.bin1))[2,1:2],
					   coef(summary(crim.bin2))[2,1:2],
				         coef(summary(crim.bin3))[2,1:2],
					   coef(summary(marg.bin1))[2,1:2],
					   coef(summary(marg.bin2))[2,1:2],
		                     coef(summary(marg.bin3))[2,1:2])

plot.coef.ests.comp<- cbind(first.matrix.competition[,1], first.matrix.competition[,1]-1.96*first.matrix.competition[,2], first.matrix.competition[,1]+1.96*first.matrix.competition[,2])

#create another new matrix that has some space between each set
dev.off()

coefs.comp<-matrix(nrow=12,ncol=3)
coefs.comp[2:4,]<- plot.coef.ests.comp[7:9,]
coefs.comp[6:8,]<- plot.coef.ests.comp[4:6,]
coefs.comp[10:12,]<- plot.coef.ests.comp[1:3,]

#makin' some labels
dem.labs.comp<- c("ACLP","Parcomp","Polyarchy")
police.labs.comp<- c("Dissident","Criminal","Marginalized")

#making the plot
pdf(width=8, height=8, file="police_coefs_comp.pdf")

dev.new(width=8,height=8)
par(mar=c(3, 7.5, 2, 2))
plot(coefs.comp[,1], seq(1:dim(coefs.comp)[1]), type="n", ylab="", ylim=c(1,13.5), xlab="", axes=F, xlim=c(-5,2), main="Political Competition Coefficients")
abline(v=0, lty=3, lwd=2)
abline(v=c(-5,-4,-3,-2,-1,1,2), lty=3, lwd=2, col="grey60")
points(coefs.comp[,1], seq(1:dim(coefs.comp)[1]), pch=16, cex=1.5)
for (i in 1:dim(coefs.comp)[1]){
  lines(coefs.comp[i,2:3], c(i, i), lwd=1.5)
}
axis(2, labels=dem.labs.comp, at=2:4, las=1)
axis(2, labels=dem.labs.comp, at=6:8, las=1)
axis(2, labels=dem.labs.comp, at=10:12, las=1)

axis(2, labels=police.labs.comp, at=c(13, 9, 5), las=1, font=2, tick=F)
axis(1, at=seq(from=-5, to=2, by=1))
box()


dev.copy(pdf, "police_plot_competition.pdf")

dev.off()



### Making a Stargazer table for logits
#political competition models: ACLP, Parcomp, Polyarchy
stargazer(diss.bin1, crim.bin1, marg.bin1, diss.bin2, crim.bin2, marg.bin2, diss.bin3, crim.bin3, marg.bin3,
          title="Logit Results, Political Competition",
          covariate.labels=c("ACLP","Parcomp","Polyarchy","GDP/capita(log)","Population(log)","Restricted Access"),
          no.space=TRUE, style="APSR", dep.var.labels.include = FALSE,
          column.labels=c("Dissident", "Criminal", "Marginalized", 
          "Dissident", "Criminal", "Marginalized","Dissident", "Criminal", "Marginalized"), float.env = "sidewaystable")




###Alternate analyses: OLS

##Competition indices

#ACLP
ols.pol.cri= lm(as.numeric(lotPolCri)~ democracy + log(gdppercap) + log(population) + RstrctAccess, data=itt.final)
summary(ols.pol.cri)

ols.pol.dis= lm(as.numeric(lotPolDis)~ democracy + log(gdppercap) +log(population) + RstrctAccess, data=itt.final)
summary(ols.pol.dis)

ols.pol.mar= lm(as.numeric(lotPolMar)~ democracy + log(gdppercap) + log(population) + RstrctAccess, data=itt.final)
summary(ols.pol.mar)

#Parcomp
ols.pol.cri2= lm(as.numeric(lotPolCri)~ parcomp + log(gdppercap) + log(population) + RstrctAccess, data=itt.final)
summary(ols.pol.cri2)

ols.pol.dis2= lm(as.numeric(lotPolDis)~ parcomp + log(gdppercap) +log(population) + RstrctAccess, data=itt.final)
summary(ols.pol.dis2)

ols.pol.mar2= lm(as.numeric(lotPolMar)~ parcomp + log(gdppercap) + log(population) + RstrctAccess, data=itt.final)
summary(ols.pol.mar2)

#Polyarchy
ols.pol.cri3= lm(as.numeric(lotPolCri)~ v2x_polyarchy + log(gdppercap) + log(population) + RstrctAccess, data=itt.final)
summary(ols.pol.cri3)

ols.pol.dis3= lm(as.numeric(lotPolDis)~ v2x_polyarchy + log(gdppercap) +log(population) + RstrctAccess, data=itt.final)
summary(ols.pol.dis3)

ols.pol.mar3= lm(as.numeric(lotPolMar)~ v2x_polyarchy + log(gdppercap) + log(population) + RstrctAccess, data=itt.final)
summary(ols.pol.mar3)



### Making a Stargazer table for OLS
#political competition models: ACLP, Parcomp, Polyarchy
stargazer(ols.pol.dis, ols.pol.cri, ols.pol.mar, ols.pol.dis2, ols.pol.cri2, ols.pol.mar2, ols.pol.dis3, ols.pol.cri3, ols.pol.mar3,
          title="OLS Results, Political Competition",
          covariate.labels=c("ACLP","Parcomp","Polyarchy","GDP/capita(log)","Population(log)","Restricted Access"),
          no.space=TRUE, style="APSR", dep.var.labels.include = FALSE,
          column.labels=c("Dissident", "Criminal", "Marginalized", 
          "Dissident", "Criminal", "Marginalized","Dissident", "Criminal", "Marginalized"), float.env = "sidewaystable")



###OLS with random effects 

##Democracy indices

#ACLP
lmer.pol.cri= lmer(as.numeric(lotPolCri)~ democracy + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), data=itt.final)
summary(lmer.pol.cri)

lmer.pol.dis= lmer(as.numeric(lotPolDis)~ democracy + log(gdppercap) +log(population) + RstrctAccess + (1|idvar), data=itt.final)
summary(lmer.pol.dis)

lmer.pol.mar= lmer(as.numeric(lotPolMar)~ democracy + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), data=itt.final)
summary(lmer.pol.mar)

#Parcomp
lmer.pol.cri2= lmer(as.numeric(lotPolCri)~ parcomp + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), data=itt.final)
summary(lmer.pol.cri2)

lmer.pol.dis2= lmer(as.numeric(lotPolDis)~ parcomp + log(gdppercap) +log(population) + RstrctAccess + (1|idvar), data=itt.final)
summary(lmer.pol.dis2)

lmer.pol.mar2= lmer(as.numeric(lotPolMar)~ parcomp + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), data=itt.final)
summary(lmer.pol.mar2)

#Polyarchy
lmer.pol.cri3= lmer(as.numeric(lotPolCri)~ v2x_polyarchy + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), data=itt.final)
summary(lmer.pol.cri3)

lmer.pol.dis3= lmer(as.numeric(lotPolDis)~ v2x_polyarchy + log(gdppercap) +log(population) + RstrctAccess + (1|idvar), data=itt.final)
summary(lmer.pol.dis3)

lmer.pol.mar3= lmer(as.numeric(lotPolMar)~ v2x_polyarchy + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), data=itt.final)
summary(lmer.pol.mar3)



### Making a Stargazer table for OLS with random effects 
#political competition models: ACLP, Parcomp, Polyarchy
stargazer(lmer.pol.dis, lmer.pol.cri, lmer.pol.mar, lmer.pol.dis2, lmer.pol.cri2, lmer.pol.mar2, lmer.pol.dis3, lmer.pol.cri3, lmer.pol.mar3,
          title="OLS (Random Effects) Results, Political Competition",
          covariate.labels=c("ACLP","Parcomp","Polyarchy","GDP/capita(log)","Population(log)","Restricted Access"),
          no.space=TRUE, style="APSR", dep.var.labels.include = FALSE,
          column.labels=c("Dissident", "Criminal", "Marginalized", 
          "Dissident", "Criminal", "Marginalized","Dissident", "Criminal", "Marginalized"), float.env = "sidewaystable")




### Ordered logits, Police agency, random effects on country

### Competition Indicators
#Vreeland's indicator for democracy (ACLP)

ologit.pol.cri1= clmm(as.factor(lotPolCri)~ democracy + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), link="logit", data=itt.final)
summary(ologit.pol.cri1)
#democracy insig; pop positive sig

ologit.pol.dis1= clmm(as.factor(lotPolDis)~ democracy + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), link= "logit", data=itt.final)
summary(ologit.pol.dis1)
#democracy neg sig p<.10; pop pos sig 

ologit.pol.mar1= clmm(as.factor(lotPolMar)~ democracy + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), link= "logit", data=itt.final)
summary(ologit.pol.mar1)
#democracy insig; gdp pos sig; pop pos sig; RestrctAccess pos sig


# parcomp 

ologit.pol.cri2= clmm(as.factor(lotPolCri)~ parcomp + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), link="logit", data=itt.final)
summary(ologit.pol.cri2)
#parcomp insig; population pos sig

ologit.pol.dis2= clmm(as.factor(lotPolDis)~ parcomp + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), link= "logit", data=itt.final)
summary(ologit.pol.dis2)
#parcomp neg sig; pop pos sig

ologit.pol.mar2= clmm(as.factor(lotPolMar)~ parcomp + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), link= "logit", data=itt.final)
summary(ologit.pol.mar2)
#parcomp neg sig; gdp pos sig; pop pos sig; RstAcc pos sig p<.10


# v2x_polycarchy from new Vdem data

ologit.pol.cri3= clmm(as.factor(lotPolCri)~ v2x_polyarchy + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), link="logit", data=itt.final)
summary(ologit.pol.cri3)
#polyarchy - sig; population pos sig; gdp pos sig

ologit.pol.dis3= clmm(as.factor(lotPolDis)~ v2x_polyarchy + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), link= "logit", data=itt.final)
summary(ologit.pol.dis3)
#polyarchy neg sig; pop pos sig

ologit.pol.mar3= clmm(as.factor(lotPolMar)~ v2x_polyarchy + log(gdppercap) + log(population) + RstrctAccess + (1|idvar), link= "logit", data=itt.final)
summary(ologit.pol.mar3)
#polyarchy pos sig; gdp pos sig; pop pos sig; RstAcc pos sig p<.10



### Sink summary of model results 

sink("C:/Users/Josh Jackson/Dropbox/Democracy and Personal Integrity/R Script/ITT_summary_output_ologits.txt", split=T)
summary(ologit.pol.dis1)
summary(ologit.pol.cri1)
summary(ologit.pol.mar1)
summary(ologit.pol.dis2)
summary(ologit.pol.cri2)
summary(ologit.pol.mar2)
summary(ologit.pol.dis3)
summary(ologit.pol.cri3)
summary(ologit.pol.mar3)
sink()



##Ologit stargazer tables: clmm incompatible. Get logit output and change output direcly in LaTeX

#political competition models: ACLP, Parcomp, Polyarchy
stargazer(diss.bin1, crim.bin1, marg.bin1, diss.bin2, crim.bin2, marg.bin2, diss.bin3, crim.bin3, marg.bin3,
          title="Ordered Logit Results, Political Competition",
          covariate.labels=c("ACLP","Parcomp","Polyarchy","GDP/capita(log)","Population(log)","Restricted Access"),
          no.space=TRUE, style="APSR", dep.var.labels.include = FALSE,
          column.labels=c("Dissident", "Criminal", "Marginalized", 
          "Dissident", "Criminal", "Marginalized","Dissident", "Criminal", "Marginalized"), float.env = "sidewaystable")


