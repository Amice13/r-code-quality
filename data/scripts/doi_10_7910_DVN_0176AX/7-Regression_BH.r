###############################################
#model used for linear regression
###############################################
#clear all variables in workspace
rm(list=ls()) 

#read DB in a dataset named 'bd'

#csv from Macbook
bd<- read.csv(header = TRUE, sep = ",", "/Users/fred/Desktop/BH/6-Data_for_regression.csv")

#csv from Windows
#bd<- read.csv("C:/Users/Frederico/Desktop/BH/6-Data_for_regression.csv")


options(scipen = 100, digits = 4) #to prevent use of exponential notation (e.g. e+10) 



###############################################
# Compute correlations
##############################################

#kolmogorov-Smirnov (n>50)
correl<-data.frame("Roubo"="0.05","Furto"="0.05","Homicidio"="0.05","Roubo a Transeunte"="0.05","Roubo a Veiculo"="0.05","Roubo a Resid?ncia"="0.05","Furto a Transeunte"="0.05","Furto a Veiculo"="0.05","Furto a Residencia"="0.05")
correl[2,1]<- signif(ks.test(bd[,2],"pnorm",mean(bd[,2]),sd(bd[,2]))$p.value, digits = 4)
correl[2,2]<- signif(ks.test(bd[,3],"pnorm",mean(bd[,3]),sd(bd[,3]))$p.value, digits = 4)
correl[2,3]<- signif(ks.test(bd[,4],"pnorm",mean(bd[,4]),sd(bd[,4]))$p.value, digits = 4)
correl[2,4]<- signif(ks.test(bd[,5],"pnorm",mean(bd[,5]),sd(bd[,5]))$p.value, digits = 4)
correl[2,5]<- signif(ks.test(bd[,6],"pnorm",mean(bd[,6]),sd(bd[,6]))$p.value, digits = 4)
correl[2,6]<- signif(ks.test(bd[,7],"pnorm",mean(bd[,7]),sd(bd[,7]))$p.value, digits = 4)
correl[2,7]<- signif(ks.test(bd[,8],"pnorm",mean(bd[,6]),sd(bd[,6]))$p.value, digits = 4)
correl[2,8]<- signif(ks.test(bd[,9],"pnorm",mean(bd[,9]),sd(bd[,9]))$p.value, digits = 4)
correl[2,9]<- signif(ks.test(bd[,10],"pnorm",mean(bd[,10]),sd(bd[,10]))$p.value, digits = 4)

rownames(correl) [1] <- "Alpha"
rownames(correl) [2] <- "Kolmogorov"
correl<-as.data.frame(t.data.frame(correl))

#Perform Pearson or Spearman correlation, based on normality test result

if(correl[1,2]>0.05) {correl[1,3]<- signif(cor(bd$DIFF_Roubo, bd$Time, method=c("pearson")), digits = 4)} else {correl[1,3]<- signif(cor(bd$DIFF_Roubo, bd$Time, method=c("spearman")), digits = 4)}
if(correl[2,2]>0.05) {correl[2,3]<- signif(cor(bd$DIFF_Furto, bd$Time, method=c("pearson")), digits = 4)} else {correl[2,3]<- signif(cor(bd$DIFF_Furto, bd$Time, method=c("spearman")), digits = 4)}
if(correl[3,2]>0.05) {correl[3,3]<- signif(cor(bd$DIFF_Homicidio, bd$Time, method=c("pearson")), digits = 4)} else {correl[3,3]<- signif(cor(bd$DIFF_Homicidio, bd$Time, method=c("spearman")), digits = 4)}
if(correl[4,2]>0.05) {correl[4,3]<- signif(cor(bd$DIFF_RouboT, bd$Time, method=c("pearson")), digits = 4)} else {correl[4,3]<- signif(cor(bd$DIFF_RouboT, bd$Time, method=c("spearman")), digits = 4)}
if(correl[5,2]>0.05) {correl[5,3]<- signif(cor(bd$DIFF_RouboV, bd$Time, method=c("pearson")), digits = 4)} else {correl[5,3]<- signif(cor(bd$DIFF_RouboV, bd$Time, method=c("spearman")), digits = 4)}
if(correl[6,2]>0.05) {correl[6,3]<- signif(cor(bd$DIFF_RouboR, bd$Time, method=c("pearson")), digits = 4)} else {correl[6,3]<- signif(cor(bd$DIFF_RouboR, bd$Time, method=c("spearman")), digits = 4)}
if(correl[7,2]>0.05) {correl[7,3]<- signif(cor(bd$DIFF_FurtoT, bd$Time, method=c("pearson")), digits = 4)} else {correl[7,3]<- signif(cor(bd$DIFF_FurtoT, bd$Time, method=c("spearman")), digits = 4)}
if(correl[8,2]>0.05) {correl[8,3]<- signif(cor(bd$DIFF_FurtoV, bd$Time, method=c("pearson")), digits = 4)} else {correl[8,3]<- signif(cor(bd$DIFF_FurtoV, bd$Time, method=c("spearman")), digits = 4)}
if(correl[9,2]>0.05) {correl[9,3]<- signif(cor(bd$DIFF_FurtoR, bd$Time, method=c("pearson")), digits = 4)} else {correl[9,3]<- signif(cor(bd$DIFF_FurtoR, bd$Time, method=c("spearman")), digits = 4)}

colnames(correl) [3] <- "Time"


if(correl[1,2]>0.05) {correl[1,4]<- signif(cor(bd$DIFF_Roubo, bd$Pandemics, method=c("pearson")), digits = 4)} else {correl[1,4]<- signif(cor(bd$DIFF_Roubo, bd$Pandemics, method=c("spearman")), digits = 4)}
if(correl[2,2]>0.05) {correl[2,4]<- signif(cor(bd$DIFF_Furto, bd$Pandemics, method=c("pearson")), digits = 4)} else {correl[2,4]<- signif(cor(bd$DIFF_Furto, bd$Pandemics, method=c("spearman")), digits = 4)}
if(correl[3,2]>0.05) {correl[3,4]<- signif(cor(bd$DIFF_Homicidio, bd$Pandemics, method=c("pearson")), digits = 4)} else {correl[3,4]<- signif(cor(bd$DIFF_Homicidio, bd$Pandemics, method=c("spearman")), digits = 4)}
if(correl[4,2]>0.05) {correl[4,4]<- signif(cor(bd$DIFF_RouboT, bd$Pandemics, method=c("pearson")), digits = 4)} else {correl[4,4]<- signif(cor(bd$DIFF_RouboT, bd$Pandemics, method=c("spearman")), digits = 4)}
if(correl[5,2]>0.05) {correl[5,4]<- signif(cor(bd$DIFF_RouboV, bd$Pandemics, method=c("pearson")), digits = 4)} else {correl[5,4]<- signif(cor(bd$DIFF_RouboV, bd$Pandemics, method=c("spearman")), digits = 4)}
if(correl[6,2]>0.05) {correl[6,4]<- signif(cor(bd$DIFF_RouboR, bd$Pandemics, method=c("pearson")), digits = 4)} else {correl[6,4]<- signif(cor(bd$DIFF_RouboR, bd$Pandemics, method=c("spearman")), digits = 4)}
if(correl[7,2]>0.05) {correl[7,4]<- signif(cor(bd$DIFF_FurtoT, bd$Pandemics, method=c("pearson")), digits = 4)} else {correl[7,4]<- signif(cor(bd$DIFF_FurtoT, bd$Pandemics, method=c("spearman")), digits = 4)}
if(correl[8,2]>0.05) {correl[8,4]<- signif(cor(bd$DIFF_FurtoV, bd$Pandemics, method=c("pearson")), digits = 4)} else {correl[8,4]<- signif(cor(bd$DIFF_FurtoV, bd$Pandemics, method=c("spearman")), digits = 4)}
if(correl[9,2]>0.05) {correl[9,4]<- signif(cor(bd$DIFF_FurtoR, bd$Pandemics, method=c("pearson")), digits = 4)} else {correl[9,4]<- signif(cor(bd$DIFF_FurtoR, bd$Pandemics, method=c("spearman")), digits = 4)}

colnames(correl) [4] <- "Pandemics"

if(correl[1,2]>0.05) {correl[1,5]<- signif(cor(bd$DIFF_Roubo, bd$Work, method=c("pearson")), digits = 4)} else {correl[1,5]<- signif(cor(bd$DIFF_Roubo, bd$Work, method=c("spearman")), digits = 4)}
if(correl[2,2]>0.05) {correl[2,5]<- signif(cor(bd$DIFF_Furto, bd$Work, method=c("pearson")), digits = 4)} else {correl[2,5]<- signif(cor(bd$DIFF_Furto, bd$Work, method=c("spearman")), digits = 4)}
if(correl[3,2]>0.05) {correl[3,5]<- signif(cor(bd$DIFF_Homicidio, bd$Work, method=c("pearson")), digits = 4)} else {correl[3,5]<- signif(cor(bd$DIFF_Homicidio, bd$Work, method=c("spearman")), digits = 4)}
if(correl[4,2]>0.05) {correl[4,5]<- signif(cor(bd$DIFF_RouboT, bd$Work, method=c("pearson")), digits = 4)} else {correl[4,5]<- signif(cor(bd$DIFF_RouboT, bd$Work, method=c("spearman")), digits = 4)}
if(correl[5,2]>0.05) {correl[5,5]<- signif(cor(bd$DIFF_RouboV, bd$Work, method=c("pearson")), digits = 4)} else {correl[5,5]<- signif(cor(bd$DIFF_RouboV, bd$Work, method=c("spearman")), digits = 4)}
if(correl[6,2]>0.05) {correl[6,5]<- signif(cor(bd$DIFF_RouboR, bd$Work, method=c("pearson")), digits = 4)} else {correl[6,5]<- signif(cor(bd$DIFF_RouboR, bd$Work, method=c("spearman")), digits = 4)}
if(correl[7,2]>0.05) {correl[7,5]<- signif(cor(bd$DIFF_FurtoT, bd$Work, method=c("pearson")), digits = 4)} else {correl[7,5]<- signif(cor(bd$DIFF_FurtoT, bd$Work, method=c("spearman")), digits = 4)}
if(correl[8,2]>0.05) {correl[8,5]<- signif(cor(bd$DIFF_FurtoV, bd$Work, method=c("pearson")), digits = 4)} else {correl[8,5]<- signif(cor(bd$DIFF_FurtoV, bd$Work, method=c("spearman")), digits = 4)}
if(correl[9,2]>0.05) {correl[9,5]<- signif(cor(bd$DIFF_FurtoR, bd$Work, method=c("pearson")), digits = 4)} else {correl[9,5]<- signif(cor(bd$DIFF_FurtoR, bd$Work, method=c("spearman")), digits = 4)}

colnames(correl) [5] <- "Work"

if(correl[1,2]>0.05) {correl[1,6]<- signif(cor(bd$DIFF_Roubo, bd$Residential, method=c("pearson")), digits = 4)} else {correl[1,6]<- signif(cor(bd$DIFF_Roubo, bd$Residential, method=c("spearman")), digits = 4)}
if(correl[2,2]>0.05) {correl[2,6]<- signif(cor(bd$DIFF_Furto, bd$Residential, method=c("pearson")), digits = 4)} else {correl[2,6]<- signif(cor(bd$DIFF_Furto, bd$Residential, method=c("spearman")), digits = 4)}
if(correl[3,2]>0.05) {correl[3,6]<- signif(cor(bd$DIFF_Homicidio, bd$Residential, method=c("pearson")), digits = 4)} else {correl[3,6]<- signif(cor(bd$DIFF_Homicidio, bd$Residential, method=c("spearman")), digits = 4)}
if(correl[4,2]>0.05) {correl[4,6]<- signif(cor(bd$DIFF_RouboT, bd$Residential, method=c("pearson")), digits = 4)} else {correl[4,6]<- signif(cor(bd$DIFF_RouboT, bd$Residential, method=c("spearman")), digits = 4)}
if(correl[5,2]>0.05) {correl[5,6]<- signif(cor(bd$DIFF_RouboV, bd$Residential, method=c("pearson")), digits = 4)} else {correl[5,6]<- signif(cor(bd$DIFF_RouboV, bd$Residential, method=c("spearman")), digits = 4)}
if(correl[6,2]>0.05) {correl[6,6]<- signif(cor(bd$DIFF_RouboR, bd$Residential, method=c("pearson")), digits = 4)} else {correl[6,6]<- signif(cor(bd$DIFF_RouboR, bd$Residential, method=c("spearman")), digits = 4)}
if(correl[7,2]>0.05) {correl[7,6]<- signif(cor(bd$DIFF_FurtoT, bd$Residential, method=c("pearson")), digits = 4)} else {correl[7,6]<- signif(cor(bd$DIFF_FurtoT, bd$Residential, method=c("spearman")), digits = 4)}
if(correl[8,2]>0.05) {correl[8,6]<- signif(cor(bd$DIFF_FurtoV, bd$Residential, method=c("pearson")), digits = 4)} else {correl[8,6]<- signif(cor(bd$DIFF_FurtoV, bd$Residential, method=c("spearman")), digits = 4)}
if(correl[9,2]>0.05) {correl[9,6]<- signif(cor(bd$DIFF_FurtoR, bd$Residential, method=c("pearson")), digits = 4)} else {correl[9,6]<- signif(cor(bd$DIFF_FurtoR, bd$Residential, method=c("spearman")), digits = 4)}

colnames(correl) [6] <- "Residential"



write.csv(correl, file = "/Users/fred/Desktop/BH/8.1-Correl.csv")
write.csv(correl, file = "C:/Users/Frederico/Desktop/BH/8.1-Correl.csv")




###############################################
#Linear Regression
###############################################
#using DIFF as dependent variable
modelR <- lm(DIFF_Roubo ~ Time + Pandemics
            + Work + Residential,
            data = bd)
modelF <- lm(DIFF_Furto ~ Time + Pandemics
             + Work + Residential,
             data = bd)
modelH <- lm(DIFF_Homicidio ~ Time + Pandemics
             + Work + Residential,
             data = bd)
modelRT <- lm(DIFF_RouboT ~ Time + Pandemics
             + Work + Residential,
             data = bd)
modelRV <- lm(DIFF_RouboV ~ Time + Pandemics
              + Work + Residential,
              data = bd)
modelRR <- lm(DIFF_RouboR ~ Time + Pandemics
              + Work + Residential,
              data = bd)
modelFT <- lm(DIFF_FurtoT ~ Time + Pandemics
              + Work + Residential,
              data = bd)
modelFV <- lm(DIFF_FurtoV ~ Time + Pandemics
              + Work + Residential,
              data = bd)
modelFR <- lm(DIFF_FurtoR ~ Time + Pandemics
               + Work + Residential,
               data = bd)




###########Print model summary
#print(summary(modelR))
#library(broom)
#tidy_modelR <- tidy(modelR)
#summary(tidy_modelR)


###############################################
#Perform various checks for multicolinearity
###############################################
#library(mctest)
#eigprop(modelR)
#omcdiag(modelR)
#imcdiag(modelR)
#mc.plot(modelR)




###############################################
#Global Validation of Linear Models Assumptions:
#Pena and Slate (2006)
#Global Stat- Are the relationships between your X predictors and Y roughly linear?
#Rejection of the null (p < .05) indicates a non-linear relationship between one or more of your X’s and Y
#Skewness -  Rejection of the null (p < .05) indicates non-normal data
#Kurtosis- Rejection of the null (p < .05) indicates non-normal data
#Link Function- Is your dependent variable truly continuous, or categorical? 
#Rejection of the null (p < .05) indicates that you should use an alternative form of the generalized linear model (e.g. logistic or binomial regression).
#Heteroscedasticity- Rejection of the null (p < .05) indicates that your residuals are heteroscedastic, model is worse at predicting for certain ranges of your X scales.
library(gvlma)
d<-data.frame("R"=1,"T"=1,"H"=1,"RW"=1,"RV"=1,"RH"=1,"TW"=1,"TV"=1,"TH"=1)
d[1,1]<-as.data.frame(gvlma(modelR)$GlobalTest$GlobalStat4$Decision)
d[1,2]<-as.data.frame(gvlma(modelF)$GlobalTest$GlobalStat4$Decision)
d[1,3]<-as.data.frame(gvlma(modelH)$GlobalTest$GlobalStat4$Decision)
d[1,4]<-as.data.frame(gvlma(modelRT)$GlobalTest$GlobalStat4$Decision)
d[1,5]<-as.data.frame(gvlma(modelRV)$GlobalTest$GlobalStat4$Decision)
d[1,6]<-as.data.frame(gvlma(modelRR)$GlobalTest$GlobalStat4$Decision)
d[1,7]<-as.data.frame(gvlma(modelFT)$GlobalTest$GlobalStat4$Decision)
d[1,8]<-as.data.frame(gvlma(modelFV)$GlobalTest$GlobalStat4$Decision)
d[1,9]<-as.data.frame(gvlma(modelFR)$GlobalTest$GlobalStat4$Decision)

d[2,1]<-as.data.frame(gvlma(modelR)$GlobalTest$DirectionalStat1$Decision)
d[2,2]<-as.data.frame(gvlma(modelF)$GlobalTest$DirectionalStat1$Decision)
d[2,3]<-as.data.frame(gvlma(modelH)$GlobalTest$DirectionalStat1$Decision)
d[2,4]<-as.data.frame(gvlma(modelRT)$GlobalTest$DirectionalStat1$Decision)
d[2,5]<-as.data.frame(gvlma(modelRV)$GlobalTest$DirectionalStat1$Decision)
d[2,6]<-as.data.frame(gvlma(modelRR)$GlobalTest$DirectionalStat1$Decision)
d[2,7]<-as.data.frame(gvlma(modelFT)$GlobalTest$DirectionalStat1$Decision)
d[2,8]<-as.data.frame(gvlma(modelFV)$GlobalTest$DirectionalStat1$Decision)
d[2,9]<-as.data.frame(gvlma(modelFR)$GlobalTest$DirectionalStat1$Decision)

d[3,1]<-as.data.frame(gvlma(modelR)$GlobalTest$DirectionalStat2$Decision)
d[3,2]<-as.data.frame(gvlma(modelF)$GlobalTest$DirectionalStat2$Decision)
d[3,3]<-as.data.frame(gvlma(modelH)$GlobalTest$DirectionalStat2$Decision)
d[3,4]<-as.data.frame(gvlma(modelRT)$GlobalTest$DirectionalStat2$Decision)
d[3,5]<-as.data.frame(gvlma(modelRV)$GlobalTest$DirectionalStat2$Decision)
d[3,6]<-as.data.frame(gvlma(modelRR)$GlobalTest$DirectionalStat2$Decision)
d[3,7]<-as.data.frame(gvlma(modelFT)$GlobalTest$DirectionalStat2$Decision)
d[3,8]<-as.data.frame(gvlma(modelFV)$GlobalTest$DirectionalStat2$Decision)
d[3,9]<-as.data.frame(gvlma(modelFR)$GlobalTest$DirectionalStat2$Decision)

d[4,1]<-as.data.frame(gvlma(modelR)$GlobalTest$DirectionalStat3$Decision)
d[4,2]<-as.data.frame(gvlma(modelF)$GlobalTest$DirectionalStat3$Decision)
d[4,3]<-as.data.frame(gvlma(modelH)$GlobalTest$DirectionalStat3$Decision)
d[4,4]<-as.data.frame(gvlma(modelRT)$GlobalTest$DirectionalStat3$Decision)
d[4,5]<-as.data.frame(gvlma(modelRV)$GlobalTest$DirectionalStat3$Decision)
d[4,6]<-as.data.frame(gvlma(modelRR)$GlobalTest$DirectionalStat3$Decision)
d[4,7]<-as.data.frame(gvlma(modelFT)$GlobalTest$DirectionalStat3$Decision)
d[4,8]<-as.data.frame(gvlma(modelFV)$GlobalTest$DirectionalStat3$Decision)
d[4,9]<-as.data.frame(gvlma(modelFR)$GlobalTest$DirectionalStat3$Decision)

d[5,1]<-as.data.frame(gvlma(modelR)$GlobalTest$DirectionalStat4$Decision)
d[5,2]<-as.data.frame(gvlma(modelF)$GlobalTest$DirectionalStat4$Decision)
d[5,3]<-as.data.frame(gvlma(modelH)$GlobalTest$DirectionalStat4$Decision)
d[5,4]<-as.data.frame(gvlma(modelRT)$GlobalTest$DirectionalStat4$Decision)
d[5,5]<-as.data.frame(gvlma(modelRV)$GlobalTest$DirectionalStat4$Decision)
d[5,6]<-as.data.frame(gvlma(modelRR)$GlobalTest$DirectionalStat4$Decision)
d[5,7]<-as.data.frame(gvlma(modelFT)$GlobalTest$DirectionalStat4$Decision)
d[5,8]<-as.data.frame(gvlma(modelFV)$GlobalTest$DirectionalStat4$Decision)
d[5,9]<-as.data.frame(gvlma(modelFR)$GlobalTest$DirectionalStat4$Decision)


rownames(d) [1] <- "Global Validation"
rownames(d) [2] <- "Linearity"
rownames(d) [3] <- "Normality"
rownames(d) [4] <- "Uncorrelatedness"
rownames(d) [5] <- "Homoskedasticity"

#Replace 0 for "TRUE" & 1 for "FALSE" (https://statisticsglobe.com/replace-particular-value-in-data-frame-in-r)
d[d == 0] <- "TRUE"
d[d == 1] <- "FALSE" 

p<-data.frame("R"=1,"T"=1,"H"=1,"RW"=1,"RV"=1,"RH"=1,"TW"=1,"TV"=1,"TH"=1)
p[1,1]<-as.data.frame(gvlma(modelR)$GlobalTest$GlobalStat4$pvalue)
p[1,2]<-as.data.frame(gvlma(modelF)$GlobalTest$GlobalStat4$pvalue)
p[1,3]<-as.data.frame(gvlma(modelH)$GlobalTest$GlobalStat4$pvalue)
p[1,4]<-as.data.frame(gvlma(modelRT)$GlobalTest$GlobalStat4$pvalue)
p[1,5]<-as.data.frame(gvlma(modelRV)$GlobalTest$GlobalStat4$pvalue)
p[1,6]<-as.data.frame(gvlma(modelRR)$GlobalTest$GlobalStat4$pvalue)
p[1,7]<-as.data.frame(gvlma(modelFT)$GlobalTest$GlobalStat4$pvalue)
p[1,8]<-as.data.frame(gvlma(modelFV)$GlobalTest$GlobalStat4$pvalue)
p[1,9]<-as.data.frame(gvlma(modelFR)$GlobalTest$GlobalStat4$pvalue)

p[2,1]<-as.data.frame(gvlma(modelR)$GlobalTest$DirectionalStat1$pvalue)
p[2,2]<-as.data.frame(gvlma(modelF)$GlobalTest$DirectionalStat1$pvalue)
p[2,3]<-as.data.frame(gvlma(modelH)$GlobalTest$DirectionalStat1$pvalue)
p[2,4]<-as.data.frame(gvlma(modelRT)$GlobalTest$DirectionalStat1$pvalue)
p[2,5]<-as.data.frame(gvlma(modelRV)$GlobalTest$DirectionalStat1$pvalue)
p[2,6]<-as.data.frame(gvlma(modelRR)$GlobalTest$DirectionalStat1$pvalue)
p[2,7]<-as.data.frame(gvlma(modelFT)$GlobalTest$DirectionalStat1$pvalue)
p[2,8]<-as.data.frame(gvlma(modelFV)$GlobalTest$DirectionalStat1$pvalue)
p[2,9]<-as.data.frame(gvlma(modelFR)$GlobalTest$DirectionalStat1$pvalue)

p[3,1]<-as.data.frame(gvlma(modelR)$GlobalTest$DirectionalStat2$pvalue)
p[3,2]<-as.data.frame(gvlma(modelF)$GlobalTest$DirectionalStat2$pvalue)
p[3,3]<-as.data.frame(gvlma(modelH)$GlobalTest$DirectionalStat2$pvalue)
p[3,4]<-as.data.frame(gvlma(modelRT)$GlobalTest$DirectionalStat2$pvalue)
p[3,5]<-as.data.frame(gvlma(modelRV)$GlobalTest$DirectionalStat2$pvalue)
p[3,6]<-as.data.frame(gvlma(modelRR)$GlobalTest$DirectionalStat2$pvalue)
p[3,7]<-as.data.frame(gvlma(modelFT)$GlobalTest$DirectionalStat2$pvalue)
p[3,8]<-as.data.frame(gvlma(modelFV)$GlobalTest$DirectionalStat2$pvalue)
p[3,9]<-as.data.frame(gvlma(modelFR)$GlobalTest$DirectionalStat2$pvalue)

p[4,1]<-as.data.frame(gvlma(modelR)$GlobalTest$DirectionalStat3$pvalue)
p[4,2]<-as.data.frame(gvlma(modelF)$GlobalTest$DirectionalStat3$pvalue)
p[4,3]<-as.data.frame(gvlma(modelH)$GlobalTest$DirectionalStat3$pvalue)
p[4,4]<-as.data.frame(gvlma(modelRT)$GlobalTest$DirectionalStat3$pvalue)
p[4,5]<-as.data.frame(gvlma(modelRV)$GlobalTest$DirectionalStat3$pvalue)
p[4,6]<-as.data.frame(gvlma(modelRR)$GlobalTest$DirectionalStat3$pvalue)
p[4,7]<-as.data.frame(gvlma(modelFT)$GlobalTest$DirectionalStat3$pvalue)
p[4,8]<-as.data.frame(gvlma(modelFV)$GlobalTest$DirectionalStat3$pvalue)
p[4,9]<-as.data.frame(gvlma(modelFR)$GlobalTest$DirectionalStat3$pvalue)

p[5,1]<-as.data.frame(gvlma(modelR)$GlobalTest$DirectionalStat4$pvalue)
p[5,2]<-as.data.frame(gvlma(modelF)$GlobalTest$DirectionalStat4$pvalue)
p[5,3]<-as.data.frame(gvlma(modelH)$GlobalTest$DirectionalStat4$pvalue)
p[5,4]<-as.data.frame(gvlma(modelRT)$GlobalTest$DirectionalStat4$pvalue)
p[5,5]<-as.data.frame(gvlma(modelRV)$GlobalTest$DirectionalStat4$pvalue)
p[5,6]<-as.data.frame(gvlma(modelRR)$GlobalTest$DirectionalStat4$pvalue)
p[5,7]<-as.data.frame(gvlma(modelFT)$GlobalTest$DirectionalStat4$pvalue)
p[5,8]<-as.data.frame(gvlma(modelFV)$GlobalTest$DirectionalStat4$pvalue)
p[5,9]<-as.data.frame(gvlma(modelFR)$GlobalTest$DirectionalStat4$pvalue)


rownames(p) [1] <- "GVLMA p-value"
rownames(p) [2] <- "Linearity p-value"
rownames(p) [3] <- "Normality p-value"
rownames(p) [4] <- "Uncorrelatedness p-value"
rownames(p) [5] <- "Homoskedasticity p-value"

model<-rbind(d[1,],signif(p[1,], digits = 4))
model<-rbind(model,d[2,])
model<-rbind(model,signif(p[2,], digits = 4))
model<-rbind(model,d[3,])
model<-rbind(model,signif(p[3,], digits = 4))
model<-rbind(model,d[4,])
model<-rbind(model,signif(p[4,], digits = 4))
model<-rbind(model,d[5,])
model<-rbind(model,signif(p[5,], digits = 4))
rm(d,p)



#Shapiro-Wilk the residuals for normality check
#The test REJECTS the hypothesis of normality when the p-value is less than or equal to 0.05
sw<-data.frame("R"=0.05,"T"=0.05,"H"=0.05,"RW"=0.05,"RV"=0.05,"RH"=0.05,"TW"=0.05,"TV"=0.05,"TH"=0.05)
sw[2,1]<-(shapiro.test(modelR$residuals)$p.value)
sw[2,2]<-(shapiro.test(modelF$residuals)$p.value)
sw[2,3]<-(shapiro.test(modelH$residuals)$p.value)
sw[2,4]<-(shapiro.test(modelRT$residuals)$p.value)
sw[2,5]<-(shapiro.test(modelRV$residuals)$p.value)
sw[2,6]<-(shapiro.test(modelRR$residuals)$p.value)
sw[2,7]<-(shapiro.test(modelFT$residuals)$p.value)
sw[2,8]<-(shapiro.test(modelFV$residuals)$p.value)
sw[2,9]<-(shapiro.test(modelFR$residuals)$p.value)
rownames(sw) [1] <- "Alpha"
rownames(sw) [2] <- "Shapiro-Wilk"

sw <- signif(sw, digits = 4) #use only the first 4 digits 

#Create line to make easy to interpret
if(sw[2,1]>0.05) {sw[3,1]<-"Normal"} else {sw[3,1]<-"Non-Normal"}
if(sw[2,2]>0.05) {sw[3,2]<-"Normal"} else {sw[3,2]<-"Non-Normal"}
if(sw[2,3]>0.05) {sw[3,3]<-"Normal"} else {sw[3,3]<-"Non-Normal"}
if(sw[2,4]>0.05) {sw[3,4]<-"Normal"} else {sw[3,4]<-"Non-Normal"}
if(sw[2,5]>0.05) {sw[3,5]<-"Normal"} else {sw[3,5]<-"Non-Normal"}
if(sw[2,6]>0.05) {sw[3,6]<-"Normal"} else {sw[3,6]<-"Non-Normal"}
if(sw[2,7]>0.05) {sw[3,7]<-"Normal"} else {sw[3,7]<-"Non-Normal"}
if(sw[2,8]>0.05) {sw[3,8]<-"Normal"} else {sw[3,8]<-"Non-Normal"}
if(sw[2,9]>0.05) {sw[3,9]<-"Normal"} else {sw[3,9]<-"Non-Normal"}
rownames(sw) [3] <- "SW Normality"

model <- rbind(model, sw)
rm(sw)



#using VIF to check for auto-correlation and quality of the regression
library(car)
v<-data.frame("R"=0.05,"T"=0.05,"H"=0.05,"RW"=0.05,"RV"=0.05,"RH"=0.05,"TW"=0.05,"TV"=0.05,"TH"=0.05)
v[1,1]<-max(as.data.frame(vif(modelR)))
v[1,2]<-max(as.data.frame(vif(modelF)))
v[1,3]<-max(as.data.frame(vif(modelH)))
v[1,4]<-max(as.data.frame(vif(modelRT)))
v[1,5]<-max(as.data.frame(vif(modelRV)))
v[1,6]<-max(as.data.frame(vif(modelRR)))
v[1,7]<-max(as.data.frame(vif(modelFT)))
v[1,8]<-max(as.data.frame(vif(modelFV)))
v[1,9]<-max(as.data.frame(vif(modelFR)))
v <- signif(v, digits = 4) #use only the first 4 digits 
rownames(v) [1] <- "Maximum VIF found"

if(v[1,1]<10) {v[2,1]<-"Tolerable"} else {v[2,1]<-"Fail"}
if(v[1,2]<10) {v[2,2]<-"Tolerable"} else {v[2,2]<-"Fail"}
if(v[1,3]<10) {v[2,3]<-"Tolerable"} else {v[2,3]<-"Fail"}
if(v[1,4]<10) {v[2,4]<-"Tolerable"} else {v[2,4]<-"Fail"}
if(v[1,5]<10) {v[2,5]<-"Tolerable"} else {v[2,5]<-"Fail"}
if(v[1,6]<10) {v[2,6]<-"Tolerable"} else {v[2,6]<-"Fail"}
if(v[1,7]<10) {v[2,7]<-"Tolerable"} else {v[2,7]<-"Fail"}
if(v[1,8]<10) {v[2,8]<-"Tolerable"} else {v[2,8]<-"Fail"}
if(v[1,9]<10) {v[2,9]<-"Tolerable"} else {v[2,9]<-"Fail"}

rownames(v) [2] <- "Multicollinearity"

model <- rbind(model, v)
rm(v)

###############################################
#Perform Breusch-Pagan test to rule out heteroskedasticity
###############################################
#test the null hypothesis that heteroskedasticity is not present 
#which means homoskedastic
#p-value < 0.05 indicates the null should rejected.
bp<-data.frame("R"=0.05,"T"=0.05,"H"=0.05,"RW"=0.05,"RV"=0.05,"RH"=0.05,"TW"=0.05,"TV"=0.05,"TH"=0.05)

library(lmtest) 
bp1<-bptest(modelR, studentize = TRUE)
bp2<-bptest(modelF, studentize = TRUE)
bp3<-bptest(modelH, studentize = TRUE)
bp4<-bptest(modelRT, studentize = TRUE)
bp5<-bptest(modelRV, studentize = TRUE)
bp6<-bptest(modelRR, studentize = TRUE)
bp7<-bptest(modelFT, studentize = TRUE)
bp8<-bptest(modelFV, studentize = TRUE)
bp9<-bptest(modelFR, studentize = TRUE)
bp[1,1]<-as.data.frame(bp1$p.value)
bp[1,2]<-as.data.frame(bp2$p.value)
bp[1,3]<-as.data.frame(bp3$p.value)
bp[1,4]<-as.data.frame(bp4$p.value)
bp[1,5]<-as.data.frame(bp5$p.value)
bp[1,6]<-as.data.frame(bp6$p.value)
bp[1,7]<-as.data.frame(bp7$p.value)
bp[1,8]<-as.data.frame(bp8$p.value)
bp[1,9]<-as.data.frame(bp9$p.value)

rownames(bp) [1] <- "Breusch-Pagan"
colnames(bp) [1:9] <- colnames(model)
bp <- signif(bp, digits = 4) #use only the first 4 digits 

#Create line to make easy to interpret
if(bp[1,1]>0.05) {bp[2,1]<-"homoskedastic"} else {bp[2,1]<-"heteroskedastic"}
if(bp[1,2]>0.05) {bp[2,2]<-"homoskedastic"} else {bp[2,2]<-"heteroskedastic"}
if(bp[1,3]>0.05) {bp[2,3]<-"homoskedastic"} else {bp[2,3]<-"heteroskedastic"}
if(bp[1,4]>0.05) {bp[2,4]<-"homoskedastic"} else {bp[2,4]<-"heteroskedasticity"}
if(bp[1,5]>0.05) {bp[2,5]<-"homoskedastic"} else {bp[2,5]<-"heteroskedastic"}
if(bp[1,6]>0.05) {bp[2,6]<-"homoskedastic"} else {bp[2,6]<-"heteroskedastic"}
if(bp[1,7]>0.05) {bp[2,7]<-"homoskedastic"} else {bp[2,7]<-"heteroskedastic"}
if(bp[1,8]>0.05) {bp[2,8]<-"homoskedastic"} else {bp[2,8]<-"heteroskedastic"}
if(bp[1,9]>0.05) {bp[2,9]<-"homoskedastic"} else {bp[2,9]<-"heteroskedastic"}
rownames(bp) [2] <- "Robust Homoskedasticity"


model <- rbind(model, bp)
rm(bp,bp1,bp2,bp3,bp4,bp5,bp6,bp7,bp8,bp9)

###############################################
#Durbin-Watson Test for auto-correlation
##############################################
#Null hypothesis first-order autocorrelation DOES NOT EXIST
#p-values <= 0.05 means "autocorrelation exist"
#p-values > 0.05 means "no autocorrelation exist"

library(lmtest)
dw<-data.frame("R"=0.05,"T"=0.05,"H"=0.05,"RW"=0.05,"RV"=0.05,"RH"=0.05,"TW"=0.05,"TV"=0.05,"TH"=0.05)
dw[1,1]<-dwtest(modelR)$statistic$DW
dw[1,2]<-dwtest(modelF)$p.value
dw[1,3]<-dwtest(modelH)$p.value
dw[1,4]<-dwtest(modelRT)$p.value
dw[1,5]<-dwtest(modelRV)$p.value
dw[1,6]<-dwtest(modelRR)$p.value
dw[1,7]<-dwtest(modelFT)$p.value
dw[1,8]<-dwtest(modelFV)$p.value
dw[1,9]<-dwtest(modelFR)$p.value
rownames(dw) [1] <- "Durbin-Watson p-value"

d1<-data.frame(dwtest(modelR)$statistic)
d2<-data.frame(dwtest(modelF)$statistic)
d3<-data.frame(dwtest(modelH)$statistic)
d4<-data.frame(dwtest(modelRT)$statistic)
d5<-data.frame(dwtest(modelRV)$statistic)
d6<-data.frame(dwtest(modelRR)$statistic)
d7<-data.frame(dwtest(modelFT)$statistic)
d8<-data.frame(dwtest(modelFV)$statistic)
d9<-data.frame(dwtest(modelFR)$statistic)

d <- cbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
d <- signif(d, digits = 4) #use only the first 4 digits 

colnames(d) [1:9] <- colnames(dw)
rownames(d) [1] <- "Durbin-Watson test"



#Check autocorrelation way
#2 is no autocorrelation.
#0 to <2 is positive autocorrelation (common in time series data).
#>2 to 4 is negative autocorrelation (less common in time series data).
if(d[1,1]>2) {d[2,1]<-"negative"} else {d[2,1]<-"positive"}
if(d[1,2]>2) {d[2,2]<-"negative"} else {d[2,2]<-"positive"}
if(d[1,3]>2) {d[2,3]<-"negative"} else {d[2,3]<-"positive"}
if(d[1,4]>2) {d[2,4]<-"negative"} else {d[2,4]<-"positive"}
if(d[1,5]>2) {d[2,5]<-"negative"} else {d[2,5]<-"positive"}
if(d[1,6]>2) {d[2,6]<-"negative"} else {d[2,6]<-"positive"}
if(d[1,7]>2) {d[2,7]<-"negative"} else {d[2,7]<-"positive"}
if(d[1,8]>2) {d[2,8]<-"negative"} else {d[2,8]<-"positive"}
if(d[1,9]>2) {d[2,9]<-"negative"} else {d[2,9]<-"positive"}
rownames(d) [2] <- "Correlation polarity"

#Create line to make easy to interpret
if(dw[1,1]>0.05) {d[3,1]<-"independent"} else {d[3,1]<-"autocorrelated"}
if(dw[1,2]>0.05) {d[3,2]<-"independent"} else {d[3,2]<-"autocorrelated"}
if(dw[1,3]>0.05) {d[3,3]<-"independent"} else {d[3,3]<-"autocorrelated"}
if(dw[1,4]>0.05) {d[3,4]<-"independent"} else {d[3,4]<-"autocorrelated"}
if(dw[1,5]>0.05) {d[3,5]<-"independent"} else {d[3,5]<-"autocorrelated"}
if(dw[1,6]>0.05) {d[3,6]<-"independent"} else {d[3,6]<-"autocorrelated"}
if(dw[1,7]>0.05) {d[3,7]<-"independent"} else {d[3,7]<-"autocorrelated"}
if(dw[1,8]>0.05) {d[3,8]<-"independent"} else {d[3,8]<-"autocorrelated"}
if(dw[1,9]>0.05) {d[3,9]<-"independent"} else {d[3,9]<-"autocorrelated"}
rownames(d) [3] <- "Independency"

dw<-signif(dw, digits = 4)

model <- rbind(model,dw)
model <- rbind(model, d)

rm(dw,d,d1,d2,d3,d4,d5,d6,d7,d8,d9)


#Bonferroni p-values for testing presence of outliers
library(car)
library(dplyr)
model[17,1]<-signif(max(outlierTest(modelR)$bonf.p), digits = 4)
model[17,2]<-signif(max(outlierTest(modelF)$bonf.p), digits = 4)
model[17,3]<-signif(max(outlierTest(modelH)$bonf.p), digits = 4)
model[17,4]<-signif(max(outlierTest(modelRT)$bonf.p), digits = 4)
model[17,5]<-signif(max(outlierTest(modelRV)$bonf.p), digits = 4)
model[17,6]<-signif(max(outlierTest(modelRR)$bonf.p), digits = 4)
model[17,7]<-signif(max(outlierTest(modelFT)$bonf.p), digits = 4)
model[17,8]<-signif(max(outlierTest(modelFV)$bonf.p), digits = 4)
model[17,9]<-signif(max(outlierTest(modelFR)$bonf.p), digits = 4)
if(model[17,1]<0.05) {model[18,1]<-"no"} else {model[18,1]<-"yes"}
if(model[17,2]<0.05) {model[18,2]<-"no"} else {model[18,2]<-"yes"}
if(model[17,3]<0.05) {model[18,3]<-"no"} else {model[18,3]<-"yes"}
if(model[17,4]<0.05) {model[18,4]<-"no"} else {model[18,4]<-"yes"}
if(model[17,5]<0.05) {model[18,5]<-"no"} else {model[18,5]<-"yes"}
if(model[17,6]<0.05) {model[18,6]<-"no"} else {model[18,6]<-"yes"}
if(model[17,7]<0.05) {model[18,7]<-"no"} else {model[18,7]<-"yes"}
if(model[17,8]<0.05) {model[18,8]<-"no"} else {model[18,8]<-"yes"}
if(model[17,9]<0.05) {model[18,9]<-"no"} else {model[18,9]<-"yes"}

rownames(model) [17] <- "Bonferroni p"
rownames(model) [18] <- "Outliers"


#accuracy = 100-MAPE (as low, the better)
library(modelr)
m<-data.frame("R"=0.05,"T"=0.05,"H"=0.05,"RW"=0.05,"RV"=0.05,"RH"=0.05,"TW"=0.05,"TV"=0.05,"TH"=0.05)
m[1,1]<-100-mape(modelR, bd)
m[1,2]<-100-mape(modelF, bd)
m[1,3]<-100-mape(modelH, bd)
m[1,4]<-100-mape(modelRT, bd)
m[1,5]<-100-mape(modelRV, bd)
m[1,6]<-100-mape(modelRR, bd)
m[1,7]<-100-mape(modelFT, bd)
m[1,8]<-100-mape(modelFV, bd)
m[1,9]<-100-mape(modelFR, bd)

m <- signif(m, digits = 4) #use only the first 4 digits 

rownames(m) [1] <- "MAPE Accuracy"

model <- rbind(model, m)
rm(m)
#check the 'MAPE' column -> percentage of error - 
#accuracy = 100-MAPE (as low, the better)



write.csv(model, file = "/Users/fred/Desktop/BH/8.2-model.csv")
write.csv(model, file = "C:/Users/Frederico/Desktop/BH/8.2-model.csv")




###############################################
# export Quality of the models Table
##############################################
#insert R2
quality<-data.frame("R2"=0,"R2 adj"=0,"p-valor"=0,"RESET fitted"=0,"MAPE"=0)
quality[1,1]<-signif(summary(modelR)$r.squared, digits = 4)
quality[2,1]<-signif(summary(modelF)$r.squared, digits = 4)
quality[3,1]<-signif(summary(modelH)$r.squared, digits = 4)
quality[4,1]<-signif(summary(modelRT)$r.squared, digits = 4)
quality[5,1]<-signif(summary(modelRV)$r.squared, digits = 4)
quality[6,1]<-signif(summary(modelRR)$r.squared, digits = 4)
quality[7,1]<-signif(summary(modelFT)$r.squared, digits = 4)
quality[8,1]<-signif(summary(modelFV)$r.squared, digits = 4)
quality[9,1]<-signif(summary(modelFR)$r.squared, digits = 4)
#insert R2 adjusted
quality[1,2]<-signif(summary(modelR)$adj.r.squared, digits = 4)
quality[2,2]<-signif(summary(modelF)$adj.r.squared, digits = 4)
quality[3,2]<-signif(summary(modelH)$adj.r.squared, digits = 4)
quality[4,2]<-signif(summary(modelRT)$adj.r.squared, digits = 4)
quality[5,2]<-signif(summary(modelRV)$adj.r.squared, digits = 4)
quality[6,2]<-signif(summary(modelRR)$adj.r.squared, digits = 4)
quality[7,2]<-signif(summary(modelFT)$adj.r.squared, digits = 4)
quality[8,2]<-signif(summary(modelFV)$adj.r.squared, digits = 4)
quality[9,2]<-signif(summary(modelFR)$adj.r.squared, digits = 4)
#insert model overall p-value
library(broom)

quality[1,3]<-signif(glance(modelR)$p.value, digits = 4)
quality[2,3]<-signif(glance(modelF)$p.value, digits = 4)
quality[3,3]<-signif(glance(modelH)$p.value, digits = 4)
quality[4,3]<-signif(glance(modelRT)$p.value, digits = 4)
quality[5,3]<-signif(glance(modelRV)$p.value, digits = 4)
quality[6,3]<-signif(glance(modelRR)$p.value, digits = 4)
quality[7,3]<-signif(glance(modelFT)$p.value, digits = 4)
quality[8,3]<-signif(glance(modelFV)$p.value, digits = 4)
quality[9,3]<-signif(glance(modelFR)$p.value, digits = 4)

#insert RESET test result
###############################################
#Regression Specification Error Test
#p.value > 0.05 means GOOD FIT (no need to use log)
library(lmtest)
quality[1,4]<-resettest(modelR, power = 2, type = "fitted", data = bd)$p.value
quality[2,4]<-resettest(modelF, power = 2, type = "fitted", data = bd)$p.value
quality[3,4]<-resettest(modelH, power = 2, type = "fitted", data = bd)$p.value
quality[4,4]<-resettest(modelRT, power = 2, type = "fitted", data = bd)$p.value
quality[5,4]<-resettest(modelRV, power = 2, type = "fitted", data = bd)$p.value
quality[6,4]<-resettest(modelRR, power = 2, type = "fitted", data = bd)$p.value
quality[7,4]<-resettest(modelFT, power = 2, type = "fitted", data = bd)$p.value
quality[8,4]<-resettest(modelFV, power = 2, type = "fitted", data = bd)$p.value
quality[9,4]<-resettest(modelFR, power = 2, type = "fitted", data = bd)$p.value

#insert the 'MAPE' column -> percentage of error - 
#model <- lm(bd[,2] ~ Time, data = bd)
###############################################
library(modelr)
quality[1,5]<-mape(modelR, bd)
quality[2,5]<-mape(modelF, bd)
quality[3,5]<-mape(modelH, bd)
quality[4,5]<-mape(modelRT, bd)
quality[5,5]<-mape(modelRV, bd)
quality[6,5]<-mape(modelRR, bd)
quality[7,5]<-mape(modelFT, bd)
quality[8,5]<-mape(modelFV, bd)
quality[9,5]<-mape(modelFR, bd)

#change row names to model dependent variables
rownames(quality)  <- rownames(correl)  


write.csv(quality, file = "/Users/fred/Desktop/BH/8.3-quality.csv")
write.csv(quality, file = "C:/Users/Frederico/Desktop/BH/8.3-quality.csv")


rm(print)
###############################################
# export Model result Table
##############################################

###################### Robbery
##############################################
library(QuantPsyc)
library(broom)
#insert basic coef`s from model`
print<-as.data.frame(summary(modelR)$coef)
colnames(print) [1] <- "Coefficients"
colnames(print) [2] <- "Standard Error"
colnames(print) [3] <- "T Statistics"
colnames(print) [4] <- "P-value (sig.)"


#insert Beta
printer1<-data.frame(0)
colnames(printer1) [1] <- "Beta"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(lm.beta(modelR))
colnames(printer2) [1] <- "Beta"
printer1<- rbind(printer1, printer2)
rownames(print) <- rownames(printer1)
printer3<-print[,3:4]
print<- cbind(print[,1:2], printer1,printer3)
rm(printer1,printer2,printer3)

#insert Confidence Interval
cilevel<-0.95  #choose C.I.
print1<-as.data.frame(confint(modelR, level = cilevel))
colnames(print1) <- c("Confidence level at",paste(cilevel*100,"%", sep=""))
print<- cbind(print, print1)
rm(print1, cilevel)

#insert VIF
library(car)
printer1<-data.frame(0)
colnames(printer1) [1] <- "VIF"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(vif(modelR))
colnames(printer2) [1] <- "VIF"
printer1 <- rbind(printer1, printer2)
print<- cbind(print, printer1)
rm(printer1,printer2)

#insert Anova
printer1<-data.frame("Df"=0,"Sum.Sq"=0,"Mean.Sq"=0,"F.value"=0,"Pr..F."=0)
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(anova(modelR))
printer1<- rbind(printer1, printer2)
print[6,1]<-0
rownames(print) <- rownames(printer1)
print<-cbind(print, printer1)
rm(printer1,printer2)
print[6,2]<-sigma(modelR)

#create a last line with Model Quality
library(broom)
printer1<-data.frame("Observations"=0,"Missing Values"=0, "R2"=0,"R2 adjusted"=0,"Model P value"=0)
printer1[1,1]<-nobs(modelR)
rownames(printer1)[1]<-as.character(modelR[["terms"]][[2]])
printer1[1,2]<-length(as.character(modelR$na.action))
printer1[1,3]<-signif(summary(modelR)$r.squared, digits = 4)
printer1[1,4]<-signif(summary(modelR)$adj.r.squared, digits = 4)
printer1[1,5]<-signif(glance(modelR)$p.value, digits = 4)

printer2<-as.data.frame(signif(summary(modelR)$fstatistic, digits = 4))
printer1<-cbind(printer1,t(printer2))
colnames(printer1) [6:8]<- c("F statistic","Number of variables", "Degrees of Freedom")
printer1[9:13]<-""
library(dplyr)
printer1 %>% na_if("")
printer2<-printer1 %>% na_if("")
colnames(printer2)<- colnames(print)
print<-signif(print, digits = 4)
print<-rbind(print,printer2)

rm(printer1,printer2)


#write csv
write.csv(print, file = "/Users/fred/Desktop/BH/8.4.1-Roubo.csv")
write.csv(print, file = "C:/Users/Frederico/Desktop/BH/8.4.1-Roubo.csv")


###################### Theft
##############################################
rm(print)
print<-as.data.frame(summary(modelF)$coef)
colnames(print) [1] <- "Coefficients"
colnames(print) [2] <- "Standard Error"
colnames(print) [3] <- "T Statistics"
colnames(print) [4] <- "P-value (sig.)"
printer1<-data.frame(0)
colnames(printer1) [1] <- "Beta"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(lm.beta(modelF))
colnames(printer2) [1] <- "Beta"
printer1<- rbind(printer1, printer2)
rownames(print) <- rownames(printer1)
printer3<-print[,3:4]
print<- cbind(print[,1:2], printer1,printer3)
rm(printer1,printer2,printer3)
cilevel<-0.95  #choose C.I.
print1<-as.data.frame(confint(modelF, level = cilevel))
colnames(print1) <- c("Confidence level at",paste(cilevel*100,"%", sep=""))
print<- cbind(print, print1)
rm(print1, cilevel)
printer1<-data.frame(0)
colnames(printer1) [1] <- "VIF"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(vif(modelF))
colnames(printer2) [1] <- "VIF"
printer1 <- rbind(printer1, printer2)
print<- cbind(print, printer1)
rm(printer1,printer2)
printer1<-data.frame("Df"=0,"Sum.Sq"=0,"Mean.Sq"=0,"F.value"=0,"Pr..F."=0)
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(anova(modelF))
printer1<- rbind(printer1, printer2)
print[6,1]<-0
rownames(print) <- rownames(printer1)
print<-cbind(print, printer1)
rm(printer1,printer2)
print[6,2]<-sigma(modelF)
printer1<-data.frame("Observations"=0,"Missing Values"=0, "R2"=0,"R2 adjusted"=0,"Model P value"=0)
printer1[1,1]<-nobs(modelF)
rownames(printer1)[1]<-as.character(modelF[["terms"]][[2]])
printer1[1,2]<-length(as.character(modelF$na.action))
printer1[1,3]<-signif(summary(modelF)$r.squared, digits = 4)
printer1[1,4]<-signif(summary(modelF)$adj.r.squared, digits = 4)
printer1[1,5]<-signif(glance(modelF)$p.value, digits = 4)
printer2<-as.data.frame(signif(summary(modelF)$fstatistic, digits = 4))
printer1<-cbind(printer1,t(printer2))
colnames(printer1) [6:8]<- c("F statistic","Number of variables", "Degrees of Freedom")
printer1[9:13]<-""
printer1 %>% na_if("")
printer2<-printer1 %>% na_if("")
colnames(printer2)<- colnames(print)
print<-signif(print, digits = 4)
print<-rbind(print,printer2)
rm(printer1,printer2)


#write csv
write.csv(print, file = "/Users/fred/Desktop/BH/8.4.2-Furto.csv")
write.csv(print, file = "C:/Users/Frederico/Desktop/BH/8.4.2-Furto.csv")


###################### Homicide
##############################################
rm(print)
print<-as.data.frame(summary(modelH)$coef)
colnames(print) [1] <- "Coefficients"
colnames(print) [2] <- "Standard Error"
colnames(print) [3] <- "T Statistics"
colnames(print) [4] <- "P-value (sig.)"
printer1<-data.frame(0)
colnames(printer1) [1] <- "Beta"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(lm.beta(modelH))
colnames(printer2) [1] <- "Beta"
printer1<- rbind(printer1, printer2)
rownames(print) <- rownames(printer1)
printer3<-print[,3:4]
print<- cbind(print[,1:2], printer1,printer3)
rm(printer1,printer2,printer3)
cilevel<-0.95  #choose C.I.
print1<-as.data.frame(confint(modelH, level = cilevel))
colnames(print1) <- c("Confidence level at",paste(cilevel*100,"%", sep=""))
print<- cbind(print, print1)
rm(print1, cilevel)
printer1<-data.frame(0)
colnames(printer1) [1] <- "VIF"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(vif(modelH))
colnames(printer2) [1] <- "VIF"
printer1 <- rbind(printer1, printer2)
print<- cbind(print, printer1)
rm(printer1,printer2)
printer1<-data.frame("Df"=0,"Sum.Sq"=0,"Mean.Sq"=0,"F.value"=0,"Pr..F."=0)
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(anova(modelH))
printer1<- rbind(printer1, printer2)
print[6,1]<-0
rownames(print) <- rownames(printer1)
print<-cbind(print, printer1)
rm(printer1,printer2)
print[6,2]<-sigma(modelH)
printer1<-data.frame("Observations"=0,"Missing Values"=0, "R2"=0,"R2 adjusted"=0,"Model P value"=0)
printer1[1,1]<-nobs(modelH)
rownames(printer1)[1]<-as.character(modelH[["terms"]][[2]])
printer1[1,2]<-length(as.character(modelH$na.action))
printer1[1,3]<-signif(summary(modelH)$r.squared, digits = 4)
printer1[1,4]<-signif(summary(modelH)$adj.r.squared, digits = 4)
printer1[1,5]<-signif(glance(modelH)$p.value, digits = 4)
printer2<-as.data.frame(signif(summary(modelH)$fstatistic, digits = 4))
printer1<-cbind(printer1,t(printer2))
colnames(printer1) [6:8]<- c("F statistic","Number of variables", "Degrees of Freedom")
printer1[9:13]<-""
printer1 %>% na_if("")
printer2<-printer1 %>% na_if("")
colnames(printer2)<- colnames(print)
print<-signif(print, digits = 4)
print<-rbind(print,printer2)
rm(printer1,printer2)


#write csv
write.csv(print, file = "/Users/fred/Desktop/BH/8.4.3-Homicidio.csv")
write.csv(print, file = "C:/Users/Frederico/Desktop/BH/8.4.3-Homicidio.csv")


###################### Robbery Street
##############################################
rm(print)
print<-as.data.frame(summary(modelRT)$coef)
colnames(print) [1] <- "Coefficients"
colnames(print) [2] <- "Standard Error"
colnames(print) [3] <- "T Statistics"
colnames(print) [4] <- "P-value (sig.)"
printer1<-data.frame(0)
colnames(printer1) [1] <- "Beta"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(lm.beta(modelRT))
colnames(printer2) [1] <- "Beta"
printer1<- rbind(printer1, printer2)
rownames(print) <- rownames(printer1)
printer3<-print[,3:4]
print<- cbind(print[,1:2], printer1,printer3)
rm(printer1,printer2,printer3)
cilevel<-0.95  #choose C.I.
print1<-as.data.frame(confint(modelRT, level = cilevel))
colnames(print1) <- c("Confidence level at",paste(cilevel*100,"%", sep=""))
print<- cbind(print, print1)
rm(print1, cilevel)
printer1<-data.frame(0)
colnames(printer1) [1] <- "VIF"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(vif(modelRT))
colnames(printer2) [1] <- "VIF"
printer1 <- rbind(printer1, printer2)
print<- cbind(print, printer1)
rm(printer1,printer2)
printer1<-data.frame("Df"=0,"Sum.Sq"=0,"Mean.Sq"=0,"F.value"=0,"Pr..F."=0)
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(anova(modelRT))
printer1<- rbind(printer1, printer2)
print[6,1]<-0
rownames(print) <- rownames(printer1)
print<-cbind(print, printer1)
rm(printer1,printer2)
print[6,2]<-sigma(modelRT)
printer1<-data.frame("Observations"=0,"Missing Values"=0, "R2"=0,"R2 adjusted"=0,"Model P value"=0)
printer1[1,1]<-nobs(modelRT)
rownames(printer1)[1]<-as.character(modelRT[["terms"]][[2]])
printer1[1,2]<-length(as.character(modelRT$na.action))
printer1[1,3]<-signif(summary(modelRT)$r.squared, digits = 4)
printer1[1,4]<-signif(summary(modelRT)$adj.r.squared, digits = 4)
printer1[1,5]<-signif(glance(modelRT)$p.value, digits = 4)
printer2<-as.data.frame(signif(summary(modelRT)$fstatistic, digits = 4))
printer1<-cbind(printer1,t(printer2))
colnames(printer1) [6:8]<- c("F statistic","Number of variables", "Degrees of Freedom")
printer1[9:13]<-""
printer1 %>% na_if("")
printer2<-printer1 %>% na_if("")
colnames(printer2)<- colnames(print)
print<-signif(print, digits = 4)
print<-rbind(print,printer2)
rm(printer1,printer2)


#write csv
write.csv(print, file = "/Users/fred/Desktop/BH/8.4.4-RouboT.csv")
write.csv(print, file = "C:/Users/Frederico/Desktop/BH/8.4.4-RouboT.csv")


###################### Robbery Vehicles
##############################################
rm(print)
print<-as.data.frame(summary(modelRV)$coef)
colnames(print) [1] <- "Coefficients"
colnames(print) [2] <- "Standard Error"
colnames(print) [3] <- "T Statistics"
colnames(print) [4] <- "P-value (sig.)"
printer1<-data.frame(0)
colnames(printer1) [1] <- "Beta"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(lm.beta(modelRV))
colnames(printer2) [1] <- "Beta"
printer1<- rbind(printer1, printer2)
rownames(print) <- rownames(printer1)
printer3<-print[,3:4]
print<- cbind(print[,1:2], printer1,printer3)
rm(printer1,printer2,printer3)
cilevel<-0.95  #choose C.I.
print1<-as.data.frame(confint(modelRV, level = cilevel))
colnames(print1) <- c("Confidence level at",paste(cilevel*100,"%", sep=""))
print<- cbind(print, print1)
rm(print1, cilevel)
printer1<-data.frame(0)
colnames(printer1) [1] <- "VIF"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(vif(modelRV))
colnames(printer2) [1] <- "VIF"
printer1 <- rbind(printer1, printer2)
print<- cbind(print, printer1)
rm(printer1,printer2)
printer1<-data.frame("Df"=0,"Sum.Sq"=0,"Mean.Sq"=0,"F.value"=0,"Pr..F."=0)
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(anova(modelRV))
printer1<- rbind(printer1, printer2)
print[6,1]<-0
rownames(print) <- rownames(printer1)
print<-cbind(print, printer1)
rm(printer1,printer2)
print[6,2]<-sigma(modelRV)
printer1<-data.frame("Observations"=0,"Missing Values"=0, "R2"=0,"R2 adjusted"=0,"Model P value"=0)
printer1[1,1]<-nobs(modelRV)
rownames(printer1)[1]<-as.character(modelRV[["terms"]][[2]])
printer1[1,2]<-length(as.character(modelRV$na.action))
printer1[1,3]<-signif(summary(modelRV)$r.squared, digits = 4)
printer1[1,4]<-signif(summary(modelRV)$adj.r.squared, digits = 4)
printer1[1,5]<-signif(glance(modelRV)$p.value, digits = 4)
printer2<-as.data.frame(signif(summary(modelRV)$fstatistic, digits = 4))
printer1<-cbind(printer1,t(printer2))
colnames(printer1) [6:8]<- c("F statistic","Number of variables", "Degrees of Freedom")
printer1[9:13]<-""
printer1 %>% na_if("")
printer2<-printer1 %>% na_if("")
colnames(printer2)<- colnames(print)
print<-signif(print, digits = 4)
print<-rbind(print,printer2)
rm(printer1,printer2)


#write csv
write.csv(print, file = "/Users/fred/Desktop/BH/8.4.5-RouboV.csv")
write.csv(print, file = "C:/Users/Frederico/Desktop/BH/8.4.5-RouboV.csv")

###################### Robbery Residential
##############################################
rm(print)
print<-as.data.frame(summary(modelRR)$coef)
colnames(print) [1] <- "Coefficients"
colnames(print) [2] <- "Standard Error"
colnames(print) [3] <- "T Statistics"
colnames(print) [4] <- "P-value (sig.)"
printer1<-data.frame(0)
colnames(printer1) [1] <- "Beta"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(lm.beta(modelRR))
colnames(printer2) [1] <- "Beta"
printer1<- rbind(printer1, printer2)
rownames(print) <- rownames(printer1)
printer3<-print[,3:4]
print<- cbind(print[,1:2], printer1,printer3)
rm(printer1,printer2,printer3)
cilevel<-0.95  #choose C.I.
print1<-as.data.frame(confint(modelRR, level = cilevel))
colnames(print1) <- c("Confidence level at",paste(cilevel*100,"%", sep=""))
print<- cbind(print, print1)
rm(print1, cilevel)
printer1<-data.frame(0)
colnames(printer1) [1] <- "VIF"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(vif(modelRR))
colnames(printer2) [1] <- "VIF"
printer1 <- rbind(printer1, printer2)
print<- cbind(print, printer1)
rm(printer1,printer2)
printer1<-data.frame("Df"=0,"Sum.Sq"=0,"Mean.Sq"=0,"F.value"=0,"Pr..F."=0)
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(anova(modelRR))
printer1<- rbind(printer1, printer2)
print[6,1]<-0
rownames(print) <- rownames(printer1)
print<-cbind(print, printer1)
rm(printer1,printer2)
print[6,2]<-sigma(modelRR)
printer1<-data.frame("Observations"=0,"Missing Values"=0, "R2"=0,"R2 adjusted"=0,"Model P value"=0)
printer1[1,1]<-nobs(modelRR)
rownames(printer1)[1]<-as.character(modelRR[["terms"]][[2]])
printer1[1,2]<-length(as.character(modelRR$na.action))
printer1[1,3]<-signif(summary(modelRR)$r.squared, digits = 4)
printer1[1,4]<-signif(summary(modelRR)$adj.r.squared, digits = 4)
printer1[1,5]<-signif(glance(modelRR)$p.value, digits = 4)
printer2<-as.data.frame(signif(summary(modelRR)$fstatistic, digits = 4))
printer1<-cbind(printer1,t(printer2))
colnames(printer1) [6:8]<- c("F statistic","Number of variables", "Degrees of Freedom")
printer1[9:13]<-""
printer1 %>% na_if("")
printer2<-printer1 %>% na_if("")
colnames(printer2)<- colnames(print)
print<-signif(print, digits = 4)
print<-rbind(print,printer2)
rm(printer1,printer2)


#write csv
write.csv(print, file = "/Users/fred/Desktop/BH/8.4.6-RouboR.csv")
write.csv(print, file = "C:/Users/Frederico/Desktop/BH/8.4.6-RouboR.csv")

###################### Theft Street
##############################################
rm(print)
print<-as.data.frame(summary(modelFT)$coef)
colnames(print) [1] <- "Coefficients"
colnames(print) [2] <- "Standard Error"
colnames(print) [3] <- "T Statistics"
colnames(print) [4] <- "P-value (sig.)"
printer1<-data.frame(0)
colnames(printer1) [1] <- "Beta"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(lm.beta(modelFT))
colnames(printer2) [1] <- "Beta"
printer1<- rbind(printer1, printer2)
rownames(print) <- rownames(printer1)
printer3<-print[,3:4]
print<- cbind(print[,1:2], printer1,printer3)
rm(printer1,printer2,printer3)
cilevel<-0.95  #choose C.I.
print1<-as.data.frame(confint(modelFT, level = cilevel))
colnames(print1) <- c("Confidence level at",paste(cilevel*100,"%", sep=""))
print<- cbind(print, print1)
rm(print1, cilevel)
printer1<-data.frame(0)
colnames(printer1) [1] <- "VIF"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(vif(modelFT))
colnames(printer2) [1] <- "VIF"
printer1 <- rbind(printer1, printer2)
print<- cbind(print, printer1)
rm(printer1,printer2)
printer1<-data.frame("Df"=0,"Sum.Sq"=0,"Mean.Sq"=0,"F.value"=0,"Pr..F."=0)
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(anova(modelFT))
printer1<- rbind(printer1, printer2)
print[6,1]<-0
rownames(print) <- rownames(printer1)
print<-cbind(print, printer1)
rm(printer1,printer2)
print[6,2]<-sigma(modelFT)
printer1<-data.frame("Observations"=0,"Missing Values"=0, "R2"=0,"R2 adjusted"=0,"Model P value"=0)
printer1[1,1]<-nobs(modelFT)
rownames(printer1)[1]<-as.character(modelFT[["terms"]][[2]])
printer1[1,2]<-length(as.character(modelFT$na.action))
printer1[1,3]<-signif(summary(modelFT)$r.squared, digits = 4)
printer1[1,4]<-signif(summary(modelFT)$adj.r.squared, digits = 4)
printer1[1,5]<-signif(glance(modelFT)$p.value, digits = 4)
printer2<-as.data.frame(signif(summary(modelFT)$fstatistic, digits = 4))
printer1<-cbind(printer1,t(printer2))
colnames(printer1) [6:8]<- c("F statistic","Number of variables", "Degrees of Freedom")
printer1[9:13]<-""
printer1 %>% na_if("")
printer2<-printer1 %>% na_if("")
colnames(printer2)<- colnames(print)
print<-signif(print, digits = 4)
print<-rbind(print,printer2)
rm(printer1,printer2)


#write csv
write.csv(print, file = "/Users/fred/Desktop/BH/8.4.7-FurtoT.csv")
write.csv(print, file = "C:/Users/Frederico/Desktop/BH/8.4.7-FurtoT.csv")

###################### Theft Vehicle
##############################################
rm(print)
print<-as.data.frame(summary(modelFV)$coef)
colnames(print) [1] <- "Coefficients"
colnames(print) [2] <- "Standard Error"
colnames(print) [3] <- "T Statistics"
colnames(print) [4] <- "P-value (sig.)"
printer1<-data.frame(0)
colnames(printer1) [1] <- "Beta"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(lm.beta(modelFV))
colnames(printer2) [1] <- "Beta"
printer1<- rbind(printer1, printer2)
rownames(print) <- rownames(printer1)
printer3<-print[,3:4]
print<- cbind(print[,1:2], printer1,printer3)
rm(printer1,printer2,printer3)
cilevel<-0.95  #choose C.I.
print1<-as.data.frame(confint(modelFV, level = cilevel))
colnames(print1) <- c("Confidence level at",paste(cilevel*100,"%", sep=""))
print<- cbind(print, print1)
rm(print1, cilevel)
printer1<-data.frame(0)
colnames(printer1) [1] <- "VIF"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(vif(modelFV))
colnames(printer2) [1] <- "VIF"
printer1 <- rbind(printer1, printer2)
print<- cbind(print, printer1)
rm(printer1,printer2)
printer1<-data.frame("Df"=0,"Sum.Sq"=0,"Mean.Sq"=0,"F.value"=0,"Pr..F."=0)
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(anova(modelFV))
printer1<- rbind(printer1, printer2)
print[6,1]<-0
rownames(print) <- rownames(printer1)
print<-cbind(print, printer1)
rm(printer1,printer2)
print[6,2]<-sigma(modelFV)
printer1<-data.frame("Observations"=0,"Missing Values"=0, "R2"=0,"R2 adjusted"=0,"Model P value"=0)
printer1[1,1]<-nobs(modelFV)
rownames(printer1)[1]<-as.character(modelFV[["terms"]][[2]])
printer1[1,2]<-length(as.character(modelFV$na.action))
printer1[1,3]<-signif(summary(modelFV)$r.squared, digits = 4)
printer1[1,4]<-signif(summary(modelFV)$adj.r.squared, digits = 4)
printer1[1,5]<-signif(glance(modelFV)$p.value, digits = 4)
printer2<-as.data.frame(signif(summary(modelFV)$fstatistic, digits = 4))
printer1<-cbind(printer1,t(printer2))
colnames(printer1) [6:8]<- c("F statistic","Number of variables", "Degrees of Freedom")
printer1[9:13]<-""
printer1 %>% na_if("")
printer2<-printer1 %>% na_if("")
colnames(printer2)<- colnames(print)
print<-signif(print, digits = 4)
print<-rbind(print,printer2)
rm(printer1,printer2)


#write csv
write.csv(print, file = "/Users/fred/Desktop/BH/8.4.8-FurtoV.csv")
write.csv(print, file = "C:/Users/Frederico/Desktop/BH/8.4.8-FurtoV.csv")

###################### Theft Residence
##############################################
rm(print)
print<-as.data.frame(summary(modelFR)$coef)
colnames(print) [1] <- "Coefficients"
colnames(print) [2] <- "Standard Error"
colnames(print) [3] <- "T Statistics"
colnames(print) [4] <- "P-value (sig.)"
printer1<-data.frame(0)
colnames(printer1) [1] <- "Beta"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(lm.beta(modelFR))
colnames(printer2) [1] <- "Beta"
printer1<- rbind(printer1, printer2)
rownames(print) <- rownames(printer1)
printer3<-print[,3:4]
print<- cbind(print[,1:2], printer1,printer3)
rm(printer1,printer2,printer3)
cilevel<-0.95  #choose C.I.
print1<-as.data.frame(confint(modelFR, level = cilevel))
colnames(print1) <- c("Confidence level at",paste(cilevel*100,"%", sep=""))
print<- cbind(print, print1)
rm(print1, cilevel)
printer1<-data.frame(0)
colnames(printer1) [1] <- "VIF"
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(vif(modelFR))
colnames(printer2) [1] <- "VIF"
printer1 <- rbind(printer1, printer2)
print<- cbind(print, printer1)
rm(printer1,printer2)
printer1<-data.frame("Df"=0,"Sum.Sq"=0,"Mean.Sq"=0,"F.value"=0,"Pr..F."=0)
rownames(printer1) [1] <- "(Intercept)"
printer2<-data.frame(anova(modelFR))
printer1<- rbind(printer1, printer2)
print[6,1]<-0
rownames(print) <- rownames(printer1)
print<-cbind(print, printer1)
rm(printer1,printer2)
print[6,2]<-sigma(modelFR)
printer1<-data.frame("Observations"=0,"Missing Values"=0, "R2"=0,"R2 adjusted"=0,"Model P value"=0)
printer1[1,1]<-nobs(modelFR)
rownames(printer1)[1]<-as.character(modelFR[["terms"]][[2]])
printer1[1,2]<-length(as.character(modelFR$na.action))
printer1[1,3]<-signif(summary(modelFR)$r.squared, digits = 4)
printer1[1,4]<-signif(summary(modelFR)$adj.r.squared, digits = 4)
printer1[1,5]<-signif(glance(modelFR)$p.value, digits = 4)
printer2<-as.data.frame(signif(summary(modelFR)$fstatistic, digits = 4))
printer1<-cbind(printer1,t(printer2))
colnames(printer1) [6:8]<- c("F statistic","Number of variables", "Degrees of Freedom")
printer1[9:13]<-""
printer1 %>% na_if("")
printer2<-printer1 %>% na_if("")
colnames(printer2)<- colnames(print)
print<-signif(print, digits = 4)
print<-rbind(print,printer2)
rm(printer1,printer2)


#write csv
write.csv(print, file = "/Users/fred/Desktop/BH/8.4.9-FurtoR.csv")
write.csv(print, file = "C:/Users/Frederico/Desktop/BH/8.4.9-FurtoR.csv")

rm(print)





#create HTML output table
#https://youtu.be/Sn0pD-BIz6M
library(stargazer)
stargazer(modelR, modelF, modelH, modelRT, modelRV, modelRR, modelFT, modelFV, modelFR,
          type="text",
          title="Regression Results with Confidence Interval at 95%", 
          style = "all", 
          decimal.mark = ",", digit.separate = 0,
          single.row=FALSE, summary = FALSE, df = TRUE,
          intercept.bottom = FALSE, model.names = TRUE, initial.zero = TRUE,
          ci=TRUE, ci.level=0.95, #EVERY TIME CHANGE THE CI.LVL, MUST CHANGE TITLE TO MAKE COMPATIBLE
          iqr = TRUE, mean.sd=TRUE,
          report = ("vcstp*"),
          out="/Users/fred/Desktop/BH/9-Results.html")
#         out="C:/Users/p3-chefe/Desktop/BH/9-Results.html")





###############################################
#CREATE ONE COMBINED PLOT FOR EACH MODEL
###############################################
#this four image chart will show charts for a visual check of the linear regression
#assumptions:
#normal distribution of residuals; 
#QQ-plots;
#homoscedasticity (constant variance of residuals) 
#multicollinearity-check (Variance Inflation Factors)

library(sjmisc)
library(sjPlot)
p1<-plot_model(modelR,type = "diag")[3]
png(file = "/Users/fred/Desktop/BH/Charts/p1.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p1
dev.off()

p2<-plot_model(modelR,type = "diag")[2]
png(file = "/Users/fred/Desktop/BH/Charts/p2.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p2
dev.off()

p3<-plot_model(modelR,type = "diag")[4]
png(file = "/Users/fred/Desktop/BH/Charts/p3.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p3
dev.off()

p4<-plot_model(modelR,type = "diag")[1]
png(file = "/Users/fred/Desktop/BH/Charts/p4.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p4
dev.off()


#combines 4 plots in 1 single image:
library(magick)
#https://cran.r-project.org/web/packages/magick/vignettes/intro.html
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
p3 <- image_read('/Users/fred/Desktop/BH/Charts/p3.png')
p4 <- image_read('/Users/fred/Desktop/BH/Charts/p4.png')
p1 <- c(p1, p2)
image_write(image_append(image_scale(p1, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p1.png", format = "png")
p2 <- c(p3, p4)
image_write(image_append(image_scale(p2, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p2.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
mainchart <- c(p1, p2)

image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
                path = "/Users/fred/Desktop/BH/Charts/ROUBO.png", format = "png")

p1 <- image_read('/Users/fred/Desktop/BH/Charts/mROUBO.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/ROUBO.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/ROUBO.png", format = "png")

rm(p1,p2,p3,p4,mainchart)



p1<-plot_model(modelF,type = "diag")[3]
png(file = "/Users/fred/Desktop/BH/Charts/p1.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p1
dev.off()
p2<-plot_model(modelF,type = "diag")[2]
png(file = "/Users/fred/Desktop/BH/Charts/p2.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p2
dev.off()
p3<-plot_model(modelF,type = "diag")[4]
png(file = "/Users/fred/Desktop/BH/Charts/p3.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p3
dev.off()
p4<-plot_model(modelF,type = "diag")[1]
png(file = "/Users/fred/Desktop/BH/Charts/p4.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p4
dev.off()
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
p3 <- image_read('/Users/fred/Desktop/BH/Charts/p3.png')
p4 <- image_read('/Users/fred/Desktop/BH/Charts/p4.png')
p1 <- c(p1, p2)
image_write(image_append(image_scale(p1, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p1.png", format = "png")
p2 <- c(p3, p4)
image_write(image_append(image_scale(p2, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p2.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/FURTO.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/mFURTO.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/FURTO.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/FURTO.png", format = "png")
rm(p1,p2,p3,p4,mainchart)



p1<-plot_model(modelH,type = "diag")[3]
png(file = "/Users/fred/Desktop/BH/Charts/p1.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p1
dev.off()
p2<-plot_model(modelH,type = "diag")[2]
png(file = "/Users/fred/Desktop/BH/Charts/p2.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p2
dev.off()
p3<-plot_model(modelH,type = "diag")[4]
png(file = "/Users/fred/Desktop/BH/Charts/p3.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p3
dev.off()
p4<-plot_model(modelH,type = "diag")[1]
png(file = "/Users/fred/Desktop/BH/Charts/p4.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p4
dev.off()
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
p3 <- image_read('/Users/fred/Desktop/BH/Charts/p3.png')
p4 <- image_read('/Users/fred/Desktop/BH/Charts/p4.png')
p1 <- c(p1, p2)
image_write(image_append(image_scale(p1, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p1.png", format = "png")
p2 <- c(p3, p4)
image_write(image_append(image_scale(p2, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p2.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/HOMICIDIO.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/mHOMICIDIO.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/HOMICIDIO.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/HOMICIDIO.png", format = "png")
rm(p1,p2,p3,p4,mainchart)



p1<-plot_model(modelRT,type = "diag")[3]
png(file = "/Users/fred/Desktop/BH/Charts/p1.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p1
dev.off()
p2<-plot_model(modelRT,type = "diag")[2]
png(file = "/Users/fred/Desktop/BH/Charts/p2.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p2
dev.off()
p3<-plot_model(modelRT,type = "diag")[4]
png(file = "/Users/fred/Desktop/BH/Charts/p3.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p3
dev.off()
p4<-plot_model(modelRT,type = "diag")[1]
png(file = "/Users/fred/Desktop/BH/Charts/p4.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p4
dev.off()
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
p3 <- image_read('/Users/fred/Desktop/BH/Charts/p3.png')
p4 <- image_read('/Users/fred/Desktop/BH/Charts/p4.png')
p1 <- c(p1, p2)
image_write(image_append(image_scale(p1, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p1.png", format = "png")
p2 <- c(p3, p4)
image_write(image_append(image_scale(p2, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p2.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/ROUBOT.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/mROUBOT.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/ROUBOT.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/ROUBOT.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/mROUBOT.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/ROUBOT.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/ROUBOT.png", format = "png")
rm(p1,p2,p3,p4,mainchart)



p1<-plot_model(modelRV,type = "diag")[3]
png(file = "/Users/fred/Desktop/BH/Charts/p1.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p1
dev.off()
p2<-plot_model(modelRV,type = "diag")[2]
png(file = "/Users/fred/Desktop/BH/Charts/p2.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p2
dev.off()
p3<-plot_model(modelRV,type = "diag")[4]
png(file = "/Users/fred/Desktop/BH/Charts/p3.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p3
dev.off()
p4<-plot_model(modelRV,type = "diag")[1]
png(file = "/Users/fred/Desktop/BH/Charts/p4.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p4
dev.off()
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
p3 <- image_read('/Users/fred/Desktop/BH/Charts/p3.png')
p4 <- image_read('/Users/fred/Desktop/BH/Charts/p4.png')
p1 <- c(p1, p2)
image_write(image_append(image_scale(p1, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p1.png", format = "png")
p2 <- c(p3, p4)
image_write(image_append(image_scale(p2, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p2.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/ROUBOV.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/mROUBOV.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/ROUBOV.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/ROUBOV.png", format = "png")
rm(p1,p2,p3,p4,mainchart)

p1<-plot_model(modelRR,type = "diag")[3]
png(file = "/Users/fred/Desktop/BH/Charts/p1.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p1
dev.off()
p2<-plot_model(modelRR,type = "diag")[2]
png(file = "/Users/fred/Desktop/BH/Charts/p2.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p2
dev.off()
p3<-plot_model(modelRR,type = "diag")[4]
png(file = "/Users/fred/Desktop/BH/Charts/p3.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p3
dev.off()
p4<-plot_model(modelRR,type = "diag")[1]
png(file = "/Users/fred/Desktop/BH/Charts/p4.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p4
dev.off()
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
p3 <- image_read('/Users/fred/Desktop/BH/Charts/p3.png')
p4 <- image_read('/Users/fred/Desktop/BH/Charts/p4.png')
p1 <- c(p1, p2)
image_write(image_append(image_scale(p1, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p1.png", format = "png")
p2 <- c(p3, p4)
image_write(image_append(image_scale(p2, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p2.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/ROUBOR.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/mROUBOR.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/ROUBOR.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/ROUBOR.png", format = "png")
rm(p1,p2,p3,p4,mainchart)

p1<-plot_model(modelFT,type = "diag")[3]
png(file = "/Users/fred/Desktop/BH/Charts/p1.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p1
dev.off()
p2<-plot_model(modelFT,type = "diag")[2]
png(file = "/Users/fred/Desktop/BH/Charts/p2.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p2
dev.off()
p3<-plot_model(modelFT,type = "diag")[4]
png(file = "/Users/fred/Desktop/BH/Charts/p3.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p3
dev.off()
p4<-plot_model(modelFT,type = "diag")[1]
png(file = "/Users/fred/Desktop/BH/Charts/p4.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p4
dev.off()
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
p3 <- image_read('/Users/fred/Desktop/BH/Charts/p3.png')
p4 <- image_read('/Users/fred/Desktop/BH/Charts/p4.png')
p1 <- c(p1, p2)
image_write(image_append(image_scale(p1, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p1.png", format = "png")
p2 <- c(p3, p4)
image_write(image_append(image_scale(p2, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p2.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/FURTOT.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/mFURTOT.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/FURTOT.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/FURTOT.png", format = "png")
rm(p1,p2,p3,p4,mainchart)


p1<-plot_model(modelFV,type = "diag")[3]
png(file = "/Users/fred/Desktop/BH/Charts/p1.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p1
dev.off()
p2<-plot_model(modelFV,type = "diag")[2]
png(file = "/Users/fred/Desktop/BH/Charts/p2.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p2
dev.off()
p3<-plot_model(modelFV,type = "diag")[4]
png(file = "/Users/fred/Desktop/BH/Charts/p3.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p3
dev.off()
p4<-plot_model(modelFV,type = "diag")[1]
png(file = "/Users/fred/Desktop/BH/Charts/p4.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p4
dev.off()
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
p3 <- image_read('/Users/fred/Desktop/BH/Charts/p3.png')
p4 <- image_read('/Users/fred/Desktop/BH/Charts/p4.png')
p1 <- c(p1, p2)
image_write(image_append(image_scale(p1, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p1.png", format = "png")
p2 <- c(p3, p4)
image_write(image_append(image_scale(p2, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p2.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/FURTOV.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/mFURTOV.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/FURTOV.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/FURTOV.png", format = "png")
rm(p1,p2,p3,p4,mainchart)


p1<-plot_model(modelFR,type = "diag")[3]
png(file = "/Users/fred/Desktop/BH/Charts/p1.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p1
dev.off()
p2<-plot_model(modelFR,type = "diag")[2]
png(file = "/Users/fred/Desktop/BH/Charts/p2.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p2
dev.off()
p3<-plot_model(modelFR,type = "diag")[4]
png(file = "/Users/fred/Desktop/BH/Charts/p3.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p3
dev.off()
p4<-plot_model(modelFR,type = "diag")[1]
png(file = "/Users/fred/Desktop/BH/Charts/p4.png", bg = "transparent", width = 1592, height = 1140, res = 200)
p4
dev.off()
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
p3 <- image_read('/Users/fred/Desktop/BH/Charts/p3.png')
p4 <- image_read('/Users/fred/Desktop/BH/Charts/p4.png')
p1 <- c(p1, p2)
image_write(image_append(image_scale(p1, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p1.png", format = "png")
p2 <- c(p3, p4)
image_write(image_append(image_scale(p2, "3184x1140"), stack = FALSE), path = "/Users/fred/Desktop/BH/Charts/p2.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/p1.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/p2.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/FURTOR.png", format = "png")
p1 <- image_read('/Users/fred/Desktop/BH/Charts/mFURTOR.png')
p2 <- image_read('/Users/fred/Desktop/BH/Charts/FURTOR.png')
mainchart <- c(p1, p2)
image_write(image_append(image_scale(mainchart, "3184x2280"), stack = TRUE), 
            path = "/Users/fred/Desktop/BH/Charts/FURTOR.png", format = "png")
rm(p1,p2,p3,p4,mainchart)


# Remove 1.png ; 2.png, etc that were generated during the process
file.remove('/Users/fred/Desktop/BH/Charts/p1.png')
file.remove('/Users/fred/Desktop/BH/Charts/p2.png')
file.remove('/Users/fred/Desktop/BH/Charts/p3.png')
file.remove('/Users/fred/Desktop/BH/Charts/p4.png')



###############################################
#Visual check of heterocedascity
###############################################
#as flatter the central line and as close the dots, 
#less chance of heteroskedasticity








