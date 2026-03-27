#This R Script contains the codes nececessary to reproduce the causal mediation analyses and associated sensitivity
#analyses in the study "Food-sourcing from on-farm trees mediates positive relationships between tree cover 
#and dietary quality in Malawi"

library(openxlsx)
library(readxl)
library(dplyr)
library(corrplot)
library(scales)
library(regclass)
library(forcats)
library(mediation)
library(sensemakr)

####DRY SEASON CAUSAL MEDIATION ANALYSIS####

#upload dry season data (note: must extract from Excel workbook and save as separate file)
DryData <- read_excel('NAME OF DATASET')

#cleaning dataset
#convert variable types
DryData$north.south <- as.factor(DryData$north.south)
DryData$treeuse.food <- as.numeric(DryData$treeuse.food)

#order education variable, collapse categories with too few observations
DryData$edu.lvl <- factor(DryData$edu.lvl, levels = c("None", "Primary", "Secondary", "Diploma"))
DryData$edu.lvl <- fct_collapse(DryData$edu.lvl, Secondary = c("Secondary", "Diploma"))

#rescale continuous variables
DryData$hh.size_s <- rescale(DryData$hh.size)
DryData$crop.count_s <- rescale(DryData$crop.count)
DryData$no.acres.cult_s<- rescale(DryData$no.acres.cult)
DryData$TLU_s<- rescale(DryData$TLU)
DryData$hh.mpi_s <- rescale(DryData$hh.mpi)
DryData$FC.1km_s <- rescale(DryData$FC.1km)
DryData$FC.500m_s <- rescale(DryData$FC.500m)

#causal mediation analysis
##code can be re-run by swapping out nutrient outcome variable (ex. here it is Vitamin A)

#the total effect of tree cover percentage on micronutrient adequacy (does not include mediator)
fit.totaleffect.d <- lm(VitA.MSM.NAR ~ FC.1km_s + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = DryData)
s.fit.totaleffect.d <- summary(fit.totaleffect.d)
s.fit.totaleffect.d

#the effect of the tree cover on the mediator (use of trees on farms for food)
fit.mediator.d <- glm(treeuse.food ~ FC.1km_s + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = DryData, family = binomial(link ="probit"))
s.fit.mediator.d <- summary(fit.mediator.d)
s.fit.mediator.d

#effect of mediator on dependent variable (while controlling for predictor variable)
fit.dv.d <- lm(VitA.MSM.NAR ~  FC.1km_s + treeuse.food + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = DryData)
s.fit.dv.d <- summary(fit.dv.d)
s.fit.dv.d

#run bootstrapping to calculate average causal mediation effects
results.d = mediate(fit.mediator.d, fit.dv.d, treat='FC.1km_s', mediator='treeuse.food', boot=T)
summary(results.d)


####WET SEASON CAUSAL MEDIATION ANALYSIS####

#upload dry season data (note: must extract from Excel workbook and save as separate file)
DryData <- read_excel('NAME OF DATASET')

#cleaning dataset
#convert variable types
WetData$north.south <- as.factor(WetData$north.south)
WetData$treeuse.food <- as.numeric(WetData$treeuse.food)

#order education variable, collapse categories with too few observations
WetData$edu.lvl <- factor(WetData$edu.lvl, levels = c("None", "Primary", "Secondary", "Diploma"))
WetData$edu.lvl <- fct_collapse(WetData$edu.lvl, Secondary = c("Secondary", "Diploma"))

#rescale continuous variables
WetData$hh.size_s <- rescale(WetData$hh.size)
WetData$crop.count_s <- rescale(WetData$crop.count)
WetData$no.acres.cult_s<- rescale(WetData$no.acres.cult)
WetData$TLU_s<- rescale(WetData$TLU)
WetData$FC.1km_s<- rescale(WetData$FC.1km)
WetData$FC.500m_s <- rescale(WetData$FC.500m)
WetData$hh.mpi_s<- rescale(WetData$hh.mpi)

#causal mediation analysis
##code can be re-run by swapping out nutrient outcome variable (ex. here it is Vitamin A)

#the total effect of tree cover percentage on micronutrient adequacy (does not include mediator)
fit.totaleffect.w <- lm(VitA.MSM.NAR ~ FC.1km_s + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = WetData)
s.fit.totaleffect.w <- summary(fit.totaleffect.w)
s.fit.totaleffect.w

#the effect of the tree cover on the mediator (use of trees on farms for food)
fit.mediator.w <- glm(treeuse.food ~ FC.1km_s + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = WetData, family = binomial(link="probit"))
s.fit.mediator.w <- summary(fit.mediator.w)
s.fit.mediator.w

#run bootstrapping to calculate average causal mediation effects
fit.dv.w <- lm(VitA.MSM.NAR ~ FC.1km_s + treeuse.food + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = WetData)
s.fit.dv.w <- summary(fit.dv.w)
s.fit.dv.w

results.w = mediate(fit.mediator.w, fit.dv.w, treat='FC.1km_s', mediator='treeuse.food', boot=T)
summary(results.w)

####SENSITIVITY ANALYSES + ROBUSTNESS CHECKS####

##test with 500m tree cover variable##

#it is possible to conduct a robustness check by replacing the treatment variable (1km tree cover buffer) 
#with a 500m tree cover buffer in the mediation analysis. This can be done by using the above code and replacing
#"FC.1km_s" with "FC.500m_s"

##sensmakr analysis to test for the impact of omitted variables##

#generates sensitivity contour plots of t-values to illustrate the effect of unobserved confounding variables on the results based on
#the observed observed benchmark covariate: crop count (selected as a benchmark as it most frequently emerged as significant in the model results)

#the following is an example testing the total effect model (e.g. effect of tree cover on VitA adequacy) for the dry season
sens.totaleffect.d <- sensemakr(model = totaleffect.d, 
                             treatment = "FC.1km_s",
                             benchmark_covariates = "crop.count_s",
                             kd = 1:3)
sens.totaleffect.d
summary(sens.totaleffect.d)

plot(sens.totaleffect.d, sensitivity.of = "t-value")

##testing for the effect of food tree species diversity##

#hurdle model with households that use on-farm trees for food (n=360)

DryData_hurdle <- subset(tof.d, treeuse.food == 1)
WetData_hurdle <- subset(tof.w, treeuse.food == 1)

##causal mediation analysis with data sub-samples, using food tree species count as mediator
#examples here are provided for vitamin A

#DRY
fit.totaleffect.d <- lm(VitA.MSM.NAR ~ FC.1km_s + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = DryData_hurdle)
s.fit.totaleffect.d <- summary(fit.totaleffect.d)
s.fit.totaleffect.d

fit.mediator.d <- glm(food.trees ~ FC.1km_s + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = DryData_hurdle, family ="poisson")
s.fit.mediator.d <- summary(fit.mediator.d)
s.fit.mediator.d

fit.dv.d <- lm(VitA.MSM.NAR ~  FC.1km_s + food.trees + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = DryData_hurdle)
s.fit.dv.d <- summary(fit.dv.d)
s.fit.dv.d

results.d = mediate(fit.mediator.d, fit.dv.d, treat='FC.1km_s', mediator='food.trees', boot=T)
summary(results.d)

#WET
fit.totaleffect.w <- lm(VitA.MSM.NAR ~ FC.1km_s + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = WetData_hurdle)
s.fit.totaleffect.w <- summary(fit.totaleffect.w.va)
s.fit.totaleffect.w

fit.mediator.w <- glm(food.trees ~ FC.1km_s + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = WetData_hurdle, family ="poisson")
s.fit.mediator.w <- summary(fit.mediator.w)
s.fit.mediator.w

fit.dv.w <- lm(VitA.MSM.NAR ~  FC.1km_s + food.trees + north.south + hh.size_s + hh.mpi_s + edu.lvl + no.acres.cult_s + crop.count_s + TLU_s, data = WetData_hurdle)
s.fit.dv.w <- summary(fit.dv.w)
s.fit.dv.w

results.w = mediate(fit.mediator.w, fit.dv.w, treat='FC.1km_s', mediator='food.trees', boot=T)
summary(results.w)
