####PACKAGES####
library(sjPlot)
library(r2glmm)#Nakagawa R^2
library(Matrix)
library(lme4) 
library(parameters)#Kenward-Roger df
library(reghelper)#ICC
library(metafor) #Egger and Begg test
library(glmmTMB)
library(dplyr)
library(numDeriv)#numerical derivatives and Hessian matrix 
library(ggeffects)#marginal effect in interactions plots

####Dataset####

MLM <- read.csv("Data2_MLM_Input.csv")
DRI_EB <- read.csv("Data4_Egger_Begg_DRI.csv", dec = ",")

#analysis without Stakeholder cases reported in tables C.5.2. and Table C.5.2.2
MLM_Stakeholder <- MLM %>% filter(Stakeholder == 0)

##### MLM #####

#####MODELS (TABLE 4)#####
#Model 1#
model1 <- lmer(DRI ~ 1+ (1 | CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))
summary(model1)
### the model does not present singularity issues

ICC(model1)#ICC 0.313 which is over 0.05 therefore we need to use MLM 

dof_kenward(model1)# given small sample size we use Kenward-Roger degree of freedom
p_value_kenward(model1)# Kenward-Roger p values
ci_kenward(model1)# Kenward-Roger confidence intervals
se_kenward(model1)# Kenward-Roger standard error
VarCorr(model1)# RE Standard deviation 

#Model 2# 
model2 <- lmer(DRI~1+Deliberation+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model2)
p_value_kenward(model2)
ci_kenward(model2)
se_kenward(model2)
dof_kenward(model2)
r2beta(model2, method = 'nsj')#Pseudo R^2
VarCorr(model2)

#Model 3#
model3 <- lmer(DRI~1+Deliberation+Age+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model3)
p_value_kenward(model3)
ci_kenward(model3)
se_kenward(model3)
dof_kenward(model3)
r2beta(model3, method = 'nsj')
VarCorr(model3)

#Model 4#
model4 <- lmer(DRI~1+Deliberation+Age+Gender+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model4)
p_value_kenward(model4)
ci_kenward(model4)
se_kenward(model4)
dof_kenward(model4)
r2beta(model4, method = 'nsj')
VarCorr(model4)

#Model 5#
model5 <- lmer(DRI~1+Deliberation+Age+Gender+Education+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model5)
p_value_kenward(model5)
ci_kenward(model5)
se_kenward(model5)
dof_kenward(model5)
r2beta(model5, method = 'nsj')
VarCorr(model5)

#Model 6#
model6 <- lmer(DRI~1+Deliberation+Age+Gender+Education+Group_building+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model6)
p_value_kenward(model6)
ci_kenward(model6)
se_kenward(model6)
dof_kenward(model6)
r2beta(model6, method = 'nsj')
VarCorr(model6)

#Model 7#
model7 <- lmer(DRI~1+Deliberation+Age+Gender+Education+Group_building+Complexity+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-6)))

summary(model7)
p_value_kenward(model7)
ci_kenward(model7)
se_kenward(model7)
dof_kenward(model7)
r2beta(model7, method = 'nsj')
VarCorr(model7)

#Model 8#
model8 <- lmer(DRI~1+Deliberation+Age+Gender+Education+Group_building+Complexity+Duration+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model8)
p_value_kenward(model8)
ci_kenward(model8)
se_kenward(model8)
dof_kenward(model8)
r2beta(model8, method = 'nsj')
VarCorr(model8)

#Model 9#
model9 <- lmer(DRI~1+Deliberation+Age+Gender+Education+Group_building+Complexity+Duration+Decision_impact+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model9)
ci_kenward(model9)
se_kenward(model9)
dof_kenward(model9)
p_value_kenward(model9)
VarCorr(model9)
r2beta(model9, method = 'nsj')

######INTERACTION MODELS (TABLE 5) ######
###Interaction Model I1###
modelI1 <- lmer(DRI~Deliberation+Education+Gender+Age+Group_building+Decision_impact+Complexity+Duration+Decision_impact*Complexity+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                 control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))
summary(modelI1)
p_value_kenward(modelI1)
ci_kenward(modelI1)
se_kenward(modelI1)
dof_kenward(modelI1)
VarCorr(modelI1)
r2beta(modelI1, method = 'nsj')

#Model I2###
modelI2 <- lmer(DRI~Deliberation+Education+Gender+Age+Group_building+Decision_impact+Complexity+Duration+Decision_impact*Complexity+Group_building*Complexity+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                 control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))
summary(modelI2)
p_value_kenward(modelI2)
ci_kenward(modelI2)
se_kenward(modelI2)
dof_kenward(modelI2)
VarCorr(modelI2)
r2beta(modelI2, method = 'nsj')

###Tables exported into xls###
tab_model(model1, model2, model3, model4, model5, model6, model7, model8, model9,
          digits = 4, p.val="kr", show.df =TRUE, show.se = T, show.r2 = T, show.ci = F,
          file ="Output/Tables/Table_4_tabular.xls")

tab_model(modelI1,modelI2, 
          digits = 4, p.val="kr", show.df =TRUE, show.se = T, show.r2 = T, show.ci = F,
          file ="Output/Tables/Table_5_tabular.xls")

#####APPENDIX#####
###MODEL WITH SIZE (TABLE D.5.1.1)####

model10_size <- lmer(DRI~1+Deliberation+Gender+Age+Education+Group_building+Decision_impact+Complexity+Duration+Size+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                 control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model10_size)
p_value_kenward(model10_size)
ci_kenward(model10_size)
se_kenward(model10_size)
dof_kenward(model10_size)
VarCorr(model10_size)
r2beta(model10_size, method = 'nsj')

###Interaction Model I3 (TABLE D.5.1.1 appendix)###

modelI3_size <- lmer(DRI~Deliberation+Education+Gender+Age+Group_building+Decision_impact+Complexity+Duration+Decision_impact*Complexity+Size+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                 control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))
summary(modelI3_size)
p_value_kenward(modelI3_size)
ci_kenward(modelI3_size)
se_kenward(modelI3_size)
dof_kenward(modelI3_size)
VarCorr(modelI3_size)
r2beta(modelI3_size, method = 'nsj')

#Model I4 (TABLE D.5.1.1 appendix) ###
modelI4_size <- lmer(DRI~Deliberation+Education+Gender+Age+Group_building+Decision_impact+Complexity+Duration+Decision_impact*Complexity+Group_building*Complexity+Size+(1|CaseID), data = MLM, REML = TRUE, na.action = na.exclude,
                 control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))
summary(modelI4_size)
p_value_kenward(modelI4_size)
ci_kenward(modelI4_size)
se_kenward(modelI4_size)
dof_kenward(modelI4_size)
VarCorr(modelI4_size)
r2beta(modelI4_size, method = 'nsj')

#table exported into xls file###
tab_model(model10_size,modelI3_size,modelI4_size, 
          digits = 4, p.val="kr", show.df =TRUE, show.se = T, show.r2 = T, show.ci = F,
          file ="Output/Tables/Table_D_5_1_1_tabular.xls")


#####TABLE D_5_2.1: Model without stakeholder groups####
#Model basic#
model1_Stakeholder <- lmer(DRI~1+ (1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))
summary(model1_Stakeholder)
p_value_kenward(model1_Stakeholder)
ci_kenward(model1_Stakeholder)
se_kenward(model1_Stakeholder)
dof_kenward(model1_Stakeholder)
VarCorr(model1_Stakeholder)


#Model 2 NO_Stakeholder# 
model2_Stakeholder <- lmer(DRI~1+Deliberation+(1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model2_Stakeholder)
p_value_kenward(model2_Stakeholder)
ci_kenward(model2_Stakeholder)
se_kenward(model2_Stakeholder)
dof_kenward(model2_Stakeholder)
r2beta(model2_Stakeholder, method = 'nsj')
VarCorr(model2_Stakeholder)

#Model 3 NO_Stakeholder#
model3_Stakeholder <- lmer(DRI~1+Deliberation+Age+(1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model3_Stakeholder)
p_value_kenward(model3_Stakeholder)
ci_kenward(model3_Stakeholder)
se_kenward(model3_Stakeholder)
dof_kenward(model3_Stakeholder)
r2beta(model3_Stakeholder, method = 'nsj')
VarCorr(model3_Stakeholder)

#Model 4 No_stakeholder#
model4_Stakeholder <- lmer(DRI~1+Deliberation+Age+Gender+(1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model4_Stakeholder)
p_value_kenward(model4_Stakeholder)
ci_kenward(model4_Stakeholder)
se_kenward(model4_Stakeholder)
dof_kenward(model4_Stakeholder)
r2beta(model4_Stakeholder, method = 'nsj')
VarCorr(model4_Stakeholder)

#Model 5 NO-Stakeholder#
model5_Stakeholder <- lmer(DRI~1+Deliberation+Age+Gender+Education+(1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model5_Stakeholder)
p_value_kenward(model5_Stakeholder)
ci_kenward(model5_Stakeholder)
se_kenward(model5_Stakeholder)
dof_kenward(model5_Stakeholder)
r2beta(model5_Stakeholder, method = 'nsj')
VarCorr(model5_Stakeholder)

#Model 6 NO-Stakeholder#
model6_Stakeholder <- lmer(DRI~1+Deliberation+Age+Gender+Education+Group_building+(1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model6_Stakeholder)
p_value_kenward(model6_Stakeholder)
ci_kenward(model6_Stakeholder)
se_kenward(model6_Stakeholder)
dof_kenward(model6_Stakeholder)
r2beta(model6_Stakeholder, method = 'nsj')
VarCorr(model6_Stakeholder)

#Model 7 NO-Stakeholder#
model7_Stakeholder <- lmer(DRI~1+Deliberation+Age+Gender+Education+Group_building+Complexity+(1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-6)))

summary(model7_Stakeholder)
p_value_kenward(model7_Stakeholder)
ci_kenward(model7_Stakeholder)
se_kenward(model7_Stakeholder)
dof_kenward(model7_Stakeholder)
r2beta(model7_Stakeholder, method = 'nsj')
VarCorr(model7_Stakeholder)

#Model 8 NO-Stakeholder#
model8_Stakeholder <- lmer(DRI~1+Deliberation+Age+Gender+Education+Group_building+Complexity+Duration+(1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model8_Stakeholder)
p_value_kenward(model8_Stakeholder)
ci_kenward(model8_Stakeholder)
se_kenward(model8_Stakeholder)
dof_kenward(model8_Stakeholder)
r2beta(model8_Stakeholder, method = 'nsj')
VarCorr(model8_Stakeholder)

#Model 9 NO-stakeholder#
model9_Stakeholder <- lmer(DRI~1+Deliberation+Age+Gender+Education+Group_building+Complexity+Duration+Decision_impact+(1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))

summary(model9_Stakeholder)
ci_kenward(model9_Stakeholder)
se_kenward(model9_Stakeholder)
dof_kenward(model9_Stakeholder)
p_value_kenward(model9_Stakeholder)
r2beta(model9_Stakeholder, method = 'nsj')
VarCorr(model9_Stakeholder)

#export table D.5.2.1 into xls format
tab_model(model1_Stakeholder,
          model2_Stakeholder,
          model3_Stakeholder, 
          model4_Stakeholder, 
          model5_Stakeholder, 
          model6_Stakeholder,
          model7_Stakeholder,
          model8_Stakeholder, 
          model9_Stakeholder, 
          digits = 4, p.val="kr", show.df =TRUE, show.se = T, show.r2 = T, show.ci = F,
          file ="Output/Tables/Table_D_5_2_1_tabular.xls")


####TABLE D.5.2.2: Interaction Models NO stakeholder groups####
#Model I1 NO stakeholder###
modelI1_Stakeholder <- lmer(DRI~Deliberation+Education+Gender+Age+Group_building+Decision_impact+Complexity+Duration+Decision_impact*Complexity+(1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                 control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))
summary(modelI1_Stakeholder)
p_value_kenward(modelI1_Stakeholder)
ci_kenward(modelI1_Stakeholder)
se_kenward(modelI1_Stakeholder)
dof_kenward(modelI1_Stakeholder)
r2beta(modelI1_Stakeholder, method = 'nsj')
VarCorr(modelI1_Stakeholder)

#Model I2s###
modelI2_Stakeholder <- lmer(DRI~Deliberation+Education+Gender+Age+Group_building+Decision_impact+Complexity+Duration+Decision_impact*Complexity+Group_building*Complexity+(1|CaseID), data = MLM_Stakeholder, REML = TRUE, na.action = na.exclude,
                 control=lmerControl(check.conv.singular = .makeCC(action = "warning",  tol = 1e-4)))
summary(modelI2_Stakeholder)
p_value_kenward(modelI2_Stakeholder)
ci_kenward(modelI2_Stakeholder)
se_kenward(modelI2_Stakeholder)
dof_kenward(modelI2_Stakeholder)
r2beta(modelI2_Stakeholder, method = 'nsj')
VarCorr(modelI2_Stakeholder)

#export C.5.2.2 into xls format##
tab_model(modelI1_Stakeholder, modelI2_Stakeholder, 
          digits = 4, p.val="kr", show.df =TRUE, show.se = T, show.r2 = T, show.ci = F,
          file ="Output/Tables/Table_D_5_2_2_tabular.xls")

####TABLE F.5.1 VIF Values####
#VIF function for lme not available in the package
vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }


vif_model_2<-vif.lme(model2)
vif_model_2
vif_model_3<-vif.lme(model3)
vif_model_3
vif_model_4<-vif.lme(model4)
vif_model_4
vif_model_5<-vif.lme(model5)
vif_model_5
vif_model_6<-vif.lme(model6)
vif_model_6
vif_model_7<-vif.lme(model7)
vif_model_7
vif_model_8<-vif.lme(model8)
vif_model_8
vif_model_9<-vif.lme(model9)
vif_model_9


####TABLE F.6.1: EGGER REGRESSION AND BEGG RANK TEST####

N_PRE_cases<-as.numeric(DRI_EB$NumberPRE)
N_POST_cases<-as.numeric(DRI_EB$NumberPost)

DRI_PRE<-as.numeric(DRI_EB$PRE.DRI)
DRI_POST<-as.numeric(DRI_EB$POST.DRI)
sd_PRE<-as.numeric(DRI_EB$SD.PRE)
sd_POST<-as.numeric(DRI_EB$SD.POST)
effectsize <- escalc(n1i = N_POST_cases, n2i = N_PRE_cases, m1i = DRI_POST, m2i = DRI_PRE, 
                     sd1i = sd_POST, sd2i = sd_PRE, measure = "SMD", data = DRI_EB, append=FALSE)
summary(effectsize)

ma_model_1 <- rma(yi, vi, data = effectsize)

ranktest(ma_model_1)#BEGG RANK TEST - p value not significant
reg_eg<-regtest(ma_model_1,model="rma",predictor="ninv",ret.fit=TRUE)# EGGER REGRESSION on inverse sample size / not significant
print(reg_eg)

#####INTERACTION PLOT FIGURE G.1.1 ######
Decision_Complexity <- plot_model(modelI2, type = "pred", terms = c("Complexity","Decision_impact[1,2,3,4,5]"))

save_plot(
  "Output/Figures/FigG_1_1_Decision_Complexity.png",
  fig = Decision_Complexity,
  width = 12,
  height = 9,
  dpi = 300,
  label.color = "black",
  label.size = 2.4,
  axis.textsize = 0.8,
  axis.titlesize = 0.75,
  legend.textsize = 0.6,
  legend.titlesize = 0.65,
  legend.itemsize = 0.5
)


####INTERACTION PLOTS FIGURES G.2.1 #####
Complexity_group <- plot_model(modelI2, type = "pred", terms = c("Complexity","Group_building"))
Complexity_group
save_plot(
  "Output/Figures/FigG_2_1_Complexity_group.png",
  fig = Complexity_group,
  width = 12,
  height = 9,
  dpi = 300,
  label.color = "black",
  label.size = 2.4,
  axis.textsize = 0.8,
  axis.titlesize = 0.75,
  legend.textsize = 0.6,
  legend.titlesize = 0.65,
  legend.itemsize = 0.5
)


####Table G.1.1 & G.2.1 Marginal effect interaction plots####
effect_pred<-ggpredict(modelI2,terms = c("Complexity","Decision_impact[1,2,3,4,5]"))
effect_pred

effect_pred2<-ggpredict(modelI2,terms = c("Complexity","Group_building"))
effect_pred2#conditional effect

####Table G.1.2 & G.2.2 Marginal effect interaction plots####
library(stargazer)
stargazer(effect_pred, type = "text")
stargazer(effect_pred2, type = "text")

#####FURTHER ANALYSIS NOT REPORTED####
####STANDARDIZED MEAN DIFFERENCE DRI EFFECT SIZE FOREST PLOT (NOT REPORTED)####
forest(ma_model_1)#not reported

#####MODELS ICC VALUES (NOT REPORTED)####
ICC(model1)
ICC(model2)
ICC(model3)
ICC(model4)#dataset smaller
ICC(model5)#dataset smaller
ICC(model6)
ICC(model7)
ICC(model8)
ICC(model9)
ICC(modelI1)
ICC(modelI2)#best ICC

###Other plots (NOT REPORTED)####
#slopes e.g., model 9a
plot_model(model9,
           type = "slope")
#slopes e.g., model I2a
plot_model(model9,
           type = "slope")
#estimates e.g., model 9
plot_model(model9,
           type = "pred")

#diagnostic plots
plot_model(model1,
           type = "diag")