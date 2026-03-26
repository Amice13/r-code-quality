################################################################################
###########################|| COBBLE RESTORATION  || ###########################
###################################|| BRUVS || #################################
################################################################################
#################|| - UNIVARIATE SPECIES ABUNDANCE || ##########################
################################################################################

###     1.Install all the necessary packages
if(!require("MASS")) {
  install.packages("MASS")
  library("MASS")
}
if(!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}
if(!require("FSA")){
  install.packages("FSA")
  library("FSA")}
if(!require("psych")){
  install.packages("psych")
  library("psych")
}
if(!require("lmerTest")){
  install.packages("lmerTest")
  library("lmerTest")
}
if(!require("nlme")){
  install.packages("nlme")
  library("nlme")
}
if(!require("lme4")) {
  install.packages("lme4")
  library("lme4")
}
if(!require("GLMMadaptive")) {
  install.packages("GLMMadaptive")
  library("GLMMadaptive")
}
if(!require("arm")) {
  install.packages("arm")
  library("arm")
}
if(!require("glmmTMB")) {
  install.packages("glmmTMB")
  library("glmmTMB")#has the function dispformula!
}
if(!require("DLL")) {
  install.packages("DLL")
  library("DLL")
}
if(!require("pscl")) {
  install.packages("pscl")
  library("pscl")
}
if(!require("DHARMa")) {
  install.packages("DHARMa")
  library("DHARMa")
}
if(!require("emmeans")) {
  install.packages("emmeans")
  library("emmeans")
}
if(!require("HMMpa")) {
  install.packages("HMMpa")
  library("HMMpa")
}
if(!require(".GlobalEnv")) {
  install.packages(".GlobalEnv")
  library(".GlobalEnv")
}
if(!require("Rcpp")) {
  install.packages("Rcpp")
  library("Rcpp")
}
if(!require("insight")) {
  install.packages("insight")
  library("insight")
}
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}
if(!require("yarrr")) {
  install.packages("yarrr")
  library("yarrr")
}
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}


###     2. Modelling
###########################//ATLANTIC COD MODELS //#############################
#Poisson 
C10<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
            data=BRUVS_cod,
            family=poisson)
summary(C10)
#Poisson + Random effects
C11<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
            data=BRUVS_cod,
            family=poisson())
summary(C11)
#Quasi poisson 
C12<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
            data=BRUVS_cod,
            family=nbinom1)
summary(C12)
#Quasi poisson + Random effects
C13<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
            data=BRUVS_cod,
            family=nbinom1)
summary(C13)
#Negative Binomial2
C14<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
            data=BRUVS_cod,
            family=nbinom2)
summary(C14)
#Negative binomial2 + Random effects
C15<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
            data=BRUVS_cod,
            family=nbinom2)
summary(C15)
#Quasi-poisson 
C16<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Herring,
            data=BRUVS_cod,
            ziformula = ~1,
            family=nbinom2)
summary(C16)
#Negative binomial + herring as a variable
C17<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Herring,
             data=BRUVS_cod,
             family=nbinom2)
summary(C17)
#Zero-inflated model + adding wrasse and goby as covariates
C18<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+Herring+Wrasse+Goby+(1|Site),
              data=BRUVS_cod,
              ziformula = ~1,
              family=nbinom2)
summary(C18)
#Negative binomial + adding wrasse and goby as variables 
C20<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+Herring+Wrasse+Goby+(1|Site),
             data=BRUVS_cod,
         family=nbinom2)
summary(C20)
#Poission
C21<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+Herring+Wrasse+Goby+(1|Site),
             data=BRUVS_cod,
             family=poisson())
summary(C21)
#Adding macroalgae as a variable
C22<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+Herring+Wrasse+Goby+Macroalgae+(1|Site),
             data=BRUVS_cod,
             family=nbinom2)
summary(C22)
#Adding macroalgae as a variable
C23<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
             data=BRUVS_cod,
             family=nbinom2)
summary(C23)
### 2.1 AIC comparison
CODCOMP<-AIC(C10,C11,C12,C13,C14,C15,C16,C17,C18,C20,C21,C22,C23)
CODCOMP
#Plotting variables potential correlations. 
#variables with a correlation coefficient of 0.7 or higher are eliminated
var_cod<-c("Year","Treatment","Temperature","Soaktime_hour_log","Visibility_log","FOV_log","Herring","Goby","Wrasse","Macroalgae")
var_cod2<-c("Cod")
Mydotplot(BRUVS_cod[,var_cod2])
par(mar = c(2, 2, 2, 2)) 
par(cex = 0.5)  
Mypairs(BRUVS_cod[,var_cod])
### 2.2 Model validation
library(lme4)
library(Matrix)
library(emmeans)
sim_res_C23<-simulateResiduals(C23,n=10000)
testZeroInflation(sim_res_C23)
testOverdispersion(sim_res_C23) 
testDispersion(sim_res_C23)
testUniformity(sim_res_C23) 
###########################//ATLANTIC HERRING MODELS //#########################
#Poisson 
H10<-glmmTMB(Herring~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_her,
             family=poisson)
summary(H10)
#Poisson + Random effects
H11<-glmmTMB(Herring~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_her,
             family=poisson())
summary(H11)
#Quasi poisson 
H12<-glmmTMB(Herring~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_her,
             family=nbinom1)
summary(H12)
#Quasi poisson + Random effects
H13<-glmmTMB(Herring~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_her,
             family=nbinom1)
summary(H13)
#Negative Binomial2
H14<-glmmTMB(Herring~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_her,
             family=nbinom2)
summary(H14)
#Negative binomial2 + Random effects
H15<-glmmTMB(Herring~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_her,
             family=nbinom2)
summary(H15)
#Zero inflated formula + cod as a variable
H16<-glmmTMB(Herring~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_her,
             ziformula = ~1,
             family=nbinom2)
summary(H16)
#Begative binomial + cod as a variable
H17<-glmmTMB(Herring~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_her,
             family=nbinom2)
summary(H17)
#Zero inflated formula 
H18<-glmmTMB(Herrings~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_her,
             ziformula = ~1,
             family=nbinom2)
summary(H18)
#Poisson
H19<-glmmTMB(Herrings~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_her,
             family=poisson())
summary(H19)
#Quasi poisson + macroalgae as variable
H20<-glmmTMB(Herrings~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+BC+Macroalgae+(1|Site),
             data=BRUVS_her,
             family=nbinom1)
summary(H20)
#Negative binomial, backwards cleaned
H21<-glmmTMB(Herrings~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
             data=BRUVS_her,
             family=nbinom1)
summary(H21)
#Negative binomial, backwards cleaned
H22<-update(H21, .~. -Macroalgae) 
summary(H22)
tab_model(H22, transform = NULL, show.se = TRUE, auto.label = TRUE, show.stat = TRUE, collapse.ci = FALSE, digits = 6, digits.p = 6, p.style = "numeric_stars", file = "herringS_results.xls")

### 2.1 AIC comparison
HERRINGCOMP<-AIC(H18,H19,H20,H21,H22)
HERRINGCOMP

#Plotting variables potential correlations (for backwards cleaning)
#variables with a correlation coefficient of 0.7 or higher are eliminated
var_her<-c("Year","Treatment","Temperature","Soaktime_hour_log","Visibility_log","FOV_log","BC","Cod")
var_her2<-c("Herring")
Mydotplot(BRUVS_her[,var_her2])
Mypairs(BRUVS_her[,var_her])

### 2.2 Model validation
sim_res_H22<-simulateResiduals(H22,n=10000)
testZeroInflation(sim_res_H22) 
testOverdispersion(sim_res_H22) 
testOutliers(sim_res_H21) 
testUniformity(sim_res_H22) 

###########################//GOLDSINNY WRASSE MODELS //#########################
#Poisson 
W10<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_lab,
             family=poisson)
summary(W10)
#Poisson + Random effects
W11<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_lab,
             family=poisson())
summary(W11)
#Quasi poisson 
W12<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_lab,
             family=nbinom1)
summary(W12)
#Quasi poisson + Random effects
W13<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_lab,
             family=nbinom1)
summary(W13)
#Negative Binomial2
W14<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_lab,
             family=nbinom2)
summary(W14)
#Negative binomial2 + Random effects
W15<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_lab,
             family=nbinom2)
summary(W15)
#Zero inflated formula + Cod as a variable
W16<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_lab,
             ziformula = ~1,
             family=nbinom2)
summary(W16)
#Negative binomial + Cod as a variable
W17<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_lab,
             family=nbinom2)
summary(W17)
#Negative binomial (Backwards cleaned)
W18<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_lab,
             ziformula = ~1,
             family=nbinom2)
summary(W18)

#Poisson + Boulder coverage + cod
W19<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_lab,
             family=poisson())
summary(W19)

#Quasi poisson + Macroalgae + cod 
W20<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+Macroalgae+Cod+(1|Site),
             data=BRUVS_lab,
             family=nbinom1)
summary(W20)

#Quasi poisson + Macroalgae + cod (backwards cleaned)
W21<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+Macroalgae+Cod+(1|Site),
             data=BRUVS_lab,
             family=nbinom1)
summary(W21)

#Negative binomial + Macroalgae + cod
W22<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+Macroalgae+Cod+(1|Site),
             data=BRUVS_lab,
             family=nbinom2)
summary(W22)

#Negative binomial + Macroalgae + cod
W23<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+Macroalgae+Cod+(1|Site),
             data=BRUVS_lab,
             dispformula= ~Treatment,
             family=nbinom1)
summary(W23)

#Negative binomial + Macroalgae 
W24<-glmmTMB(Wrasse~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
             data=BRUVS_lab,
             family=nbinom2)
summary(W24)
### 2.1 AIC comparison
WRASSECOMP<-AIC(W10,W11,W12,W13,W14,W15,W16,W17,W18,W19,W20,W21,W22,W23,W24)
WRASSECOMP

#Plotting variables potential correlations (for backwards cleaning)
#variables with a correlation coefficient of 0.7 or higher are eliminated
var_wra<-c("Year","Treatment","Temperature","Soaktime_hour_log","Visibility_log","FOV_log","BC","Cod","Macroalgae")
var_wra2<-c("Wrasse")
Mypairs(BRUVS_lab[,var_wra])
Mydotplot(BRUVS_lab[,var_wra2])
par(mar=c(0.5,0.5,0.5,0.5))
plot(BRUVS_lab$Wrasse)
colnames(BRUVS_lab)

### 2.2 Model validation
sim_res_W24<-simulateResiduals(W24,n=10000)
testZeroInflation(sim_res_W24) 
testOverdispersion(sim_res_W24) 
testOutliers(sim_res_W24,type="bootstrap")
testUniformity(sim_res_W24)#D = 0.035434, p-value = 0.4106

#plotting the residuals against the predictors
residuals_W24<- residuals(W24)
wrasse_treatment<-plot(BRUVS_lab$Treatment, residuals_W24, xlab = "treatment", ylab = "Residuals",
     main = "Residuals vs. Treatment")

wrasse_year<-plot(BRUVS_lab$Year, residuals_W24, xlab = "treatment", ylab = "Residuals",
     main = "Residuals vs. Year")
ggsave(wrasse_year, filename = "wrasse_year.png", width = 10, height = 12, dpi = 300) 

#p-value test
lm_treatment <- lm(residuals_W24 ~ BRUVS_lab$Treatment)
summary(lm_treatment)
lm_year <- lm(residuals_W24 ~ BRUVS_lab$Year)
summary(lm_year)

################################// GOBY MODELS. //##############################
#Poisson 
G10<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_gob,
             family=poisson)
summary(G10)
#Poisson + Random effects
G11<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_gob,
             family=poisson())
summary(G11)
#Quasi poisson 
G12<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_gob,
             family=nbinom1)
summary(G12)
#Quasi poisson + Random effects
G13<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_gob,
             family=nbinom1)
summary(G13)
#Negative Binomial2 (no random effects)
G14<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_gob,
             family=nbinom2)
summary(G14)
#Negative binomial2 + Random effects
library(glmmTMB)
G15<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_gob,
             family=nbinom2)
summary(G15)
#Quasi-poisson + Herring as a variable
G16<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_gob,
             ziformula = ~1,
             family=nbinom2)
summary(G16)
#Negative binomial
G17<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_gob,
             family=nbinom2)
summary(G17)
#Zero inflated formula 
G18<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_gob,
             ziformula = ~1,
             family=nbinom2)
summary(G18)
#Poisson 
G19<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_gob,
             family=poisson())
summary(G19)
#Negative binomial+BC
G20<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_gob,
             family=nbinom2)
G20
#Negative binomial (bakwards cleaned)
library(glmmTMB)
G21<-glmmTMB(Goby~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
             data=BRUVS_gob,
             family=nbinom2)
summary(G21)
### 2.1 AIC comparison
GOBYCOMP<-AIC(G10,G11,G12,G13,G14,G15,G16,G17,G18,G19,G20,G21,G22)
GOBYCOMP
#Plotting variables potential correlations. 
#variables with a correlation coefficient of 0.7 or higher are eliminated
var_goby<-c("Year","Treatment","Temperature","Visibility_log","FOV_log","Soaktime_hour_log","BC","Macroalgae")
Mypairs(BRUVS_gob[,var_goby])
var_goby2<-c("Goby")
Mydotplot(BRUVS_gob[,var_goby2])
library(lattice)
# Create the dot plot
dotplot(BRUVS_gob[, var_goby2])
### 2.2 Model validation
sim_res_G21<-simulateResiduals(G21,n=10000)
testZeroInflation(sim_res_G21) 
testDispersion(sim_res_G21) 
testOutliers(sim_res_G21)
testUniformity(sim_res_G21)
###########################// SHORE CRAB MODELS //##############################
#Poisson 
SC10<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_crab,
             family=poisson)
summary(SC10)
#Poisson + Random effects
SC11<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_crab,
             family=poisson())
summary(SC11)
#Quasi poisson 
SC12<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_crab,
             family=nbinom1)
summary(SC12)
#Quasi poisson + Random effects
SC13<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_crab,
             family=nbinom1)
summary(SC13)
#Negative Binomial2 (no ranodm effects)
SC14<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_crab,
             family=nbinom2)
summary(SC14)
#Negative binomial2 + Random effects
SC15<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_crab,
             family=nbinom2)
summary(SC15)
#Quasi-poisson + Cod as variable + 0 inflated formula
SC16<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_crab,
             ziformula = ~1,
             family=nbinom2)
summary(SC16)
#Negative binomial + cod
SC17<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_crab,
             family=nbinom2)
summary(SC17)
#Negative binomial + zero inflated formula + backwards cleaned (no cod)
SC18<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_crab,
             ziformula = ~1,
             family=nbinom2)
summary(SC18)
#Poisson + backwards cleaned (no macroalgae)
SC19<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Cod,
             data=BRUVS_crab,
             family=poisson())
summary(SC19)
#Negative binomial bakwards cleaned (no macroalgae/cod)
SC20<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_crab,
             family=nbinom2)
SC20
#Quasi-poisson + Macroalgae 
SC21<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+Macroalgae+(1|Site)+Cod,
              data=BRUVS_crab,
              ziformula = ~1,
              family=nbinom2)
summary(SC21)
#Quasi-poisson + Macroalgae (backwards cleaned no cod)
SC22<-glmmTMB(Shore_crab~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
              data=BRUVS_crab,
              family=nbinom1)
summary(SC22)
### 2.1 AIC comparison
SHORE_CRAB_COMP<-AIC(SC10,SC11,SC12,SC13,SC14,SC15,SC16,SC17,SC18,SC19,SC20,SC21,SC22)
SHORE_CRAB_COMP
##Plotting variables potential reciprocal correlations. 
#variables with a correlation coefficient of 0.7 or higher are eliminated
var_crab<-c("Year","Treatment","Temperature","Visibility_log","FOV_log","Soaktime_hour_log","BC","Macroalgae")
var_crab2<-c("Shore_crab")
Mydotplot(BRUVS_crab[,var_crab2])
Mypairs(BRUVS_crab[,var_crab])
## 2.2 Model Validation
sim_res_SC22<-simulateResiduals(SC22,n=10000)
testZeroInflation(sim_res_SC22) #ratioObsSim = 0.92276, p-value = 0.5844
testOverdispersion(sim_res_SC22) #dispersion = 1.1529, p-value = 0.3506
testOutliers(sim_res_SC22)#outliers at both margin(s) = 0, observations = 629, p-value = 1
testUniformity(sim_res_SC22)#D = 0.037451, p-value = 0.3409

####################################// FLATFISH //##############################
#Poisson 
F10<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_flat,
             family=poisson)
summary(F10)
#Poisson + Random effects
F11<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_flat,
             family=poisson())
summary(F11)
#Quasi poisson 
F12<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_flat,
             family=nbinom1)
summary(F12)
#Quasi poisson + Random effects
F13<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_flat,
             family=nbinom1)
summary(F13)
#Negative Binomial
F14<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC,
             data=BRUVS_flat,
             family=nbinom2)
summary(F14)
#Negative binomial + Random effects
F15<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_flat,
             family=nbinom2)
summary(F15)
#Quasi-poisson + zero inflated formula + Herring as a variable
F16<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Herring,
             data=BRUVS_flat,
             ziformula = ~1,
             family=nbinom2)
summary(F16)
#Negative binomial + random effects + herring
F17<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site)+Herring,
             data=BRUVS_flat,
             family=nbinom2)
summary(F17)
#Negative binomial + zero inflated formula (backwards cleaned)
F18<-glmmTMB(Cod~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_flat,
             ziformula = ~1,
             family=nbinom2)
summary(F18)
#Negative binomial (backwards cleaned)
F20<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_flat,
             family=nbinom2)
summary(F20)
#Poisson (backwards cleaned)
F21<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+(1|Site),
             data=BRUVS_flat,
             family=poisson())
summary(F21)
#Negative binomial + Macroalgae (backwards cleaned)
F22<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+BC+Macroalgae+(1|Site),
             data=BRUVS_flat,
             family=nbinom2)
summary(F22)
#Negative binomial + Macroalgae (backwards cleaned) (- BC)
F23<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_hour_log+Macroalgae+(1|Site),
             data=BRUVS_flat,
             family=nbinom2)
summary(F23)
#Quassi poisson + Macroalgae (backwards cleaned) (- BC)
F24<-glmmTMB(Flatfish~Year*Treatment+Temperature+Visibility_log+FOV_log+Soaktime_group_log+Macroalgae+(1|Site),
             data=BRUVS_flat,
             family=nbinom1)
summary(F24)
#### 2.1 AIC comparison
FLATCOMP<-AIC(F10,F11,F12,F13,F14,F15,F16,F17,F18,F20,F21,F22,F23,F24)
FLATCOMP
##Plotting variables potential reciprocal correlations. 
#variables with a correlation coefficient of 0.7 or higher are eliminated
var_flat<-c("Year","Treatment","Temperature","Visibility_log","FOV_log","Soaktime_hour_log","BC","Macroalgae")
var_flat2<-c("Flatfish")
Mydotplot(BRUVS_flat[,var_flat2])
Mypairs(BRUVS_flat[,var_flat])
#### 2.2 Model validation
sim_res_F24<-simulateResiduals(F24,n=10000)
testZeroInflation(sim_res_F24) 
testDispersion(sim_res_F24) 
testOutliers(sim_res_F24) 
testUniformity(sim_res_F24)
################################################################################
#### 3. Save models as RDS
saveRDS(C23,file="M_cod_final",ascii=TRUE)
saveRDS(H22,file="M_her_final",ascii=TRUE)
saveRDS(W24,file="M_lab_final",ascii=TRUE)
saveRDS(G21,file="M_gob_final",ascii=TRUE)
saveRDS(SC22,file="M_crab_final",ascii=TRUE)
saveRDS(F24,file="M_flat_final",ascii=TRUE)
################################################################################
############################ || END OF SCRIPT || ###############################