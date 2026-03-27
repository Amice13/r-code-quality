################################################################################
###########################|| KORTFISK REPORT STATS || #########################
################################################################################
#################|| Script 3.1 - UNIVARIATE SPECIES ABUNDANCE || ###############
################################################################################

# Load the required packages
options(java.parameters = "-Xmx1024m")
Sys.setenv(LANG = "en")

install.package("rJava")
library("rJava")
if(!require("xlsx")) {
  install.packages("xlsx")
  library("xlsx")
}
if(!require("vegan")) {
  install.packages("vegan")
  library("vegan")
}
if(!require("glmmTMB")) {
  install.packages("glmmTMB")
  library("glmmTMB")
}
if(!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}
if(!require("DHARMa")) {
  install.packages("DHARMa")
  library("DHARMa")
}
if(!require("tidyverse")) {
  install.packages("tidyverse")
  library("tidyverse")
}
if(!require("lattice")) {
  install.packages("lattice")
  library("lattice")
}
if(!require("pracma")) {
  install.packages("pracma")
  library("pracma")
}
if(!require("geoR")) {
  install.packages("geoR")
  library("geoR")
}
if(!require("spdep")) {
  install.packages("spdep")
  library("spdep")
}
if(!require("gstat")) {
  install.packages("gstat")
  library("gstat")
}
if(!require("emmeans")) {
  install.packages("emmeans")
  library("emmeans")
}
if(!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}
if(!require("gridExtra")) {
  install.packages("gridExtra")
  library("gridExtra")
}
if(!require("gap")) {
  install.packages("gap")
  library("gap")
}
source("HighstatLibV13.R")

update.packages()
update.packages("gap")
install.packages("DHARMa")
library(DHARMa)
library(gap)
library(dplyr)
######################################|| 1. Data handling || ########################################

##open datasets
UBRUVS_cod <- readRDS(file = "UBRUVS_cod")
UBRUVS_her <- readRDS(file = "UBRUVS_her")
UBRUVS_lab <- readRDS(file = "UBRUVS_lab")
UBRUVS_gob <- readRDS(file = "UBRUVS_gob")
UBRUVS_crab <- readRDS(file = "UBRUVS_crab")
UBRUVS_flat <- readRDS(file = "UBRUVS_flat")

#####################################|| 2. Modelling || ############################################

M1a_cod <- glmmTMB(MaxN ~ Year * Treatment + Macroalgae + Eelgrass + logVis + logFOV +  
                     Temperature + (1 | Site/Deployment_No.), data = UBRUVS_cod, family = "poisson")
summary(M1a_cod)


M2a_cod <- glmmTMB(MaxN ~ Year * Treatment + Macroalgae + Eelgrass + logVis + logFOV + 
                     Temperature + (1 | Site/Deployment_No.), data = UBRUVS_cod, family = "nbinom2")
summary(M2a_cod)

AIC(M1a_cod, M2a_cod) #nb preferred as expected
# Exclude eelgrass

M2b_cod <- update(M2a_cod, .~. -Eelgrass)
summary(M2b_cod)

M2c_cod <- update(M2b_cod, .~. -Macroalgae)
summary(M2c_cod)  # <-- final model

res_M2c_cod <- simulateResiduals(M2c_cod, n = 10000, integerResponse = T)
testDispersion(res_M2c_cod) #dispersion = 0.48076, p-value = 0.952
testZeroInflation(res_M2c_cod) #ratioObsSim = 0.99678, p-value = 0.586
testUniformity(res_M2c_cod) # Good

### Modelling herring
M1a_her <- glmmTMB(MaxN ~ Year * Treatment + Macroalgae + Eelgrass + logVis + logFOV + 
                     Temperature + (1 | Site/Deployment_No.), data = UBRUVS_her, family = "poisson")
summary(M1a_her)
library(DHARMa)
res_M1a_her <- simulateResiduals(M1a_her, n = 1000, integerResponse = T)
testDispersion(res_M1a_her)
library(glmmTMB)
M2a_her <- glmmTMB(MaxN ~ Year * Treatment + Macroalgae + Eelgrass + logVis + logFOV + 
                     Temperature + (1 | Site/Deployment_No.), 
                   data = UBRUVS_her, 
                   family = "nbinom2")
summary(M2a_her)

AIC(M1a_her, M2a_her) # nbinom provides much better fit
summary(M2a_her)

M2b_her <- update(M2a_her, .~. -Eelgrass)
summary(M2b_her)

M2c_her <- update(M2b_her, .~. -Macroalgae)
summary(M2c_her)
library(DHARMa)
res_M2c_her <- simulateResiduals(M2c_her, n = 10000, integerResponse = T)
testDispersion(res_M2c_her)
testOverdispersion(res_M2c_her)# Widely dispersed, but at least no overdispersion
testZeroInflation(res_M2c_her) # Fine
testUniformity(res_M2c_her)

### Modelling wrasse
M1a_lab <- glmmTMB(MaxN ~ Year * Treatment + Macroalgae + Eelgrass + logVis + logFOV + 
                     Temperature + (1 | Site/Deployment_No.), data = UBRUVS_lab, family = "poisson")
summary(M1a_lab)

res_M1a_lab <- simulateResiduals(M1a_lab, n = 1000, integerResponse = T)
testDispersion(res_M1a_lab) # Poisson is fine here.

M1b_lab <- update(M1a_lab, .~. -Macroalgae) # Bit surprising wrasse is not affected by macroalgae
summary(M1b_lab)

M1c_lab <- update(M1b_lab, .~. -Eelgrass) 
summary(M1c_lab)

res_M1c_lab <- simulateResiduals(M1c_lab, n = 1000, integerResponse = T)
testZeroInflation(res_M1c_lab) # Fine
testDispersion(res_M1c_lab) 
testUniformity(res_M1c_lab)

### Modelling goby
str(d_gob)
M1a_gob <- glmmTMB(MaxN ~ Year * Treatment + Macroalgae + Eelgrass + logVis + logFOV + 
                     Temperature + (1 | Site/Deployment_No.), data = UBRUVS_gob, family = "poisson")
summary(M1a_gob)
library(DHARMa)
res_M1a_gob <- simulateResiduals(M1a_gob, n = 1000, integerResponse = T)
testDispersion(res_M1a_gob) # Almost underdispersed but should be okay, still fit NB for comparison

M2a_gob <- glmmTMB(MaxN ~ Year * Treatment + Macroalgae + Eelgrass + logVis + logFOV + 
                     Temperature + (1 | Site/Deployment_No.), data = UBRUVS_gob, family = "nbinom2")
summary(M2a_gob)
AIC(M1a_gob, M2a_gob) # still big preference for NB

M2b_gob <- update(M2a_gob, .~. -Eelgrass) 
summary(M2b_gob)


### Modelling shore crab
M1a_crab <- glmmTMB(MaxN ~ Year * Treatment + Macroalgae + Eelgrass + logVis + logFOV + 
                     Temperature + (1 | Site/Deployment_No.), data = UBRUVS_crab, family = "poisson")
summary(M1a_crab)

res_M1a_crab <- simulateResiduals(M1a_crab, n = 1000, integerResponse = T)
testDispersion(res_M1a_crab) # Almost underdispersed but should be okay, still fit NB for comparison

M2a_crab <- glmmTMB(MaxN ~ Year * Treatment + Macroalgae + Eelgrass + logVis + logFOV + 
                      Temperature + (1 | Site/Deployment_No.), data = UBRUVS_crab, family = "nbinom2")
summary(M2a_crab)# Did not converge somehow...

AIC(M1a_crab, M2a_crab)
plot(x = d_crab$Year, d_crab$MaxN)

testZeroInflation(res_M1a_crab) #fine

M1b_crab <- update(M1a_crab, .~. -Eelgrass) 
summary(M1b_crab) # Quite a weird one, need to inspect the plots carefully later on.

res_M1b_crab <- simulateResiduals(M1b_crab, n = 1000, integerResponse = T)
testZeroInflation(M1b_crab)
testDispersion(M1b_crab)
testUniformity(M1b_crab)
### MOdelling flatfish
M1a_flat <- glmmTMB(MaxN ~ Year * Treatment + Macroalgae + Eelgrass + logVis + logFOV + 
                      Temperature + (1 | Site/Deployment_No.), data = UBRUVS_flat, family = "poisson")
summary(M1a_flat)

res_M1a_flat <- simulateResiduals(M1a_flat, n = 1000, integerResponse = T)
testDispersion(res_M1a_flat) # Fine

M1b_flat <- update(M1a_flat, .~. -Eelgrass) 
summary(M1b_flat) # Nice to see that flatfish are no longer negatively impacted by the construction
# of the new reefs on a sandy bottom.

#save models as RDS
saveRDS(M2c_cod,file="BRUVS_M_cod_final",ascii=TRUE)
saveRDS(M2c_her,file="BRUVS_M_her_final",ascii=TRUE)
saveRDS(M1c_lab,file="BRUVS_M_lab_final",ascii=TRUE)
saveRDS(M2b_gob,file="BRUVS_M_gob_final",ascii=TRUE)
saveRDS(M1b_crab,file="BRUVS_M_crab_final",ascii=TRUE)
saveRDS(M1b_flat,file="BRUVS_M_flat_final",ascii=TRUE)

# Change models names for an integrated analysis with BRUVS
UN_COD<-M2c_cod
UN_HER<-M2c_her
UN_LAB<-M1c_lab
UN_GOB<-M2b_gob
UN_CRAB<-M1b_crab
UN_FLAT<-M1b_flat
library(glmmTMB)

#save models as a spreadsheet
install.packages("sjPlot")
library(sjPlot)
tab_model(UN_COD, transform = NULL, show.se = TRUE, auto.label = TRUE, show.stat = TRUE, 
          collapse.ci = FALSE, digits = 6, digits.p = 6, p.style = "numeric_stars", file = "UN_COD_results.xls")
tab_model(UN_HER, transform = NULL, show.se = TRUE, auto.label = TRUE, show.stat = TRUE, 
          collapse.ci = FALSE, digits = 6, digits.p = 6, p.style = "numeric_stars", file = "UN_HER_results.xls")
tab_model(UN_LAB, transform = NULL, show.se = TRUE, auto.label = TRUE, show.stat = TRUE, 
          collapse.ci = FALSE, digits = 6, digits.p = 6, p.style = "numeric_stars", file = "UN_LAB_results.xls")
tab_model(UN_GOB, transform = NULL, show.se = TRUE, auto.label = TRUE, show.stat = TRUE, 
          collapse.ci = FALSE, digits = 6, digits.p = 6, p.style = "numeric_stars", file = "UN_GOB_results.xls")
tab_model(UN_CRAB, transform = NULL, show.se = TRUE, auto.label = TRUE, show.stat = TRUE, 
          collapse.ci = FALSE, digits = 6, digits.p = 6, p.style = "numeric_stars", file = "UN_CRAB_results.xls")
tab_model(UN_FLAT, transform = NULL, show.se = TRUE, auto.label = TRUE, show.stat = TRUE, 
          collapse.ci = FALSE, digits = 6, digits.p = 6, p.style = "numeric_stars", file = "UN_FLAT_results.xls")


#open models
M2c_cod <- readRDS(file = "M_cod_final")
M2c_her <- readRDS(file = "M_her_final")
M1c_lab <- readRDS(file = "M_lab_final")
M2b_gob <- readRDS(file = "M_gob_final")
M1b_crab <- readRDS(file = "M_crab_final")
M1b_flat <- readRDS(file = "M_flat_final")

##################################### || END OF SCRIPT || ##########################################
