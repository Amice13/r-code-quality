############################################################################################################################
########The wrong place at the wrong time? Territorial autonomy and conflict during regime transitions########
#####CEE-FSU regional analyses (main and robustness checks)#####
#####Andreas Juon & Daniel Bochsler 2023 (Comparative Political Studies)#####
############################################################################################################################

####################################################################################
#########1. Libraries, data import, definitions#########
####################################################################################

#######1.1 Libraries#######
library(plyr)
library(dplyr)
library(zoo)
library(gridExtra)
library(grid)
library(glm.predict)
library(boot)
library(fastDummies)
library(stargazer)
library(car)
library(stargazer)

#######1.2 Functions#######
get_confint<-function(model, vcovCL){
  t<-qt(.975, model$df.residual)
  ct<-coeftest(model, vcovCL)
  est<-cbind(ct[,1], ct[,1]-t*ct[,2], ct[,1]+t*ct[,2])
  colnames(est)<-c("Estimate","LowerCI","UpperCI")
  return(est)
}
cluster.se<-function(model, cluster)
{
  require(multiwayvcov)
  require(lmtest)
  vcovCL<-cluster.vcov(model, cluster)
  coef<-coeftest(model, vcovCL)
  return(coef)
}

#######1.3 Reading Data#######
setwd("[!!!Set working directory containing the data file!!!]")
df_global <- read.csv("./juon_bochsler_cps_autonomy.csv",sep=",")
df_global <- subset(df_global, status_no < 3)#excluding groups with dominant or monopoly status
df_global$yearf <- as.factor(df_global$year)
df_global$region <- as.factor(df_global$region)
df_ceefsu <- subset(df_global, (cowcode == 290 | cowcode == 310 | cowcode == 315 | cowcode == 316 | cowcode == 317 | cowcode == 339 | cowcode == 341 | cowcode == 343 | cowcode == 344 | cowcode == 345 | cowcode == 346 | cowcode == 347 | cowcode == 349 | cowcode == 355 | cowcode == 359 | cowcode == 360 | cowcode == 365 | cowcode == 366 | cowcode == 367 | cowcode == 368 | cowcode == 369 | cowcode == 370 | cowcode == 371 | cowcode == 372 | cowcode == 373 | cowcode == 701 | cowcode == 702 | cowcode == 703 | cowcode == 704 | cowcode == 705))
df_ceefsu_before <- subset(df_ceefsu, year < 1989)
df_ceefsu_before <- subset(df_ceefsu_before,gwgroupid != "36502000"  & gwgroupid != "36504000"  & gwgroupid != "36505000"  & gwgroupid !=  "36508000"  & gwgroupid != "36509000"  & gwgroupid != "36510000"  & gwgroupid != "36512000"  & gwgroupid !="36513000"  & gwgroupid !="36514000"  & gwgroupid !="36519000"  & gwgroupid !="36521000"  & gwgroupid !="36522000"  & gwgroupid !="36524000"  & gwgroupid !="36525000")#for pre-1989 analysis: subset to groups who are minorities in the post-1989 period, who enter our sample.
df_ceefsu_1989 <- subset(df_ceefsu, year == 1989)
df_ceefsu <- subset(df_ceefsu, year > 1989)

#######1.4 Defining variable lists#######
group_controls <- c("state_control","included","size","log(distance_border+0.00001)","tek_sup_irre_any","tek_sdm","oil_area_pct")
country_controls <- c("log(gdppc_l1)","log(pop_l1)","sd_sum_l1","polityca")
sdm_years <- c("years_no_sd_l1", "I(years_no_sd_l1^2)", "I(years_no_sd_l1^3)")
violsdm_years <- c("sd_l1","years_no_violsd_l1", "I(years_no_violsd_l1^2)", "I(years_no_violsd_l1^3)")
additional_controls <- c("cleavage_mean","low_ratio","high_ratio")
alternative_controls2 <- c("russian_base_true_l1","hegemony_group","accession_perspective_l1")
alternative_controls3 <- c("terraut_loss_l1")
alternative_controls4 <- c("terraut_up_shock","nogroups4_epr")
group_controls_limited <- c("state_control","included","size","log(distance_border+0.00001)","tek_sdm","oil_area_pct")
country_controls_limited <- c("polityca")

#######1.5 Defining variable reporting order for stargazer#######
vars.order_main1 <- c("autonomy","terraut_1989","terraut_1936","terraut_l1","year_since89","sd_onsets_sum_after1989","sd_escalation_sum_after1989","sd_l1","recent_concession5")
vars.order_main2 <- c(gsub("log\\(|\\)","",group_controls),gsub("log\\(|\\)","",country_controls),additional_controls,alternative_controls2,alternative_controls3,alternative_controls4)


####################################################################################
#########2. Main models#########
####################################################################################

###a) models
sd_onset_1989_main <- glm(as.formula(paste("sd_onset", paste(c("terraut_1989", "state_control", group_controls, country_controls, "years_no_sd_l1", "I(years_no_sd_l1^2)", "I(years_no_sd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0), family=binomial(link="logit"),control = list(maxit = 50),na.action = na.exclude)
cse_sd_onset_1989_main <- data.frame(cluster.se(sd_onset_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))[, 2])
terraut_up_1989_main <- glm(as.formula(paste("terraut_up", paste(c("terraut_1989", group_controls, country_controls), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0), family=binomial(link="logit"),control = list(maxit = 50))
cse_terraut_up_1989_main <- data.frame(cluster.se(terraut_up_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))[, 2])
sd_escalation_1989_main <- glm(as.formula(paste("violsd_onset", paste(c("terraut_1989", "recent_concession5", group_controls, country_controls, "years_no_violsd_l1", "I(years_no_violsd_l1^2)", "I(years_no_violsd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1), family=binomial(link="logit"),control = list(maxit = 50))
cse_sd_escalation_1989_main <- data.frame(cluster.se(sd_escalation_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))[, 2])
stargazer(type="text",sd_onset_1989_main,terraut_up_1989_main, sd_escalation_1989_main,se=c(cse_sd_onset_1989_main, cse_terraut_up_1989_main, cse_sd_escalation_1989_main), column.labels = c("Model 1", "Model 2", "Model 3", "Model 4","Model 5"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("SDM onset","Concession","Escalation during SDM","Escalation during SDM"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses. Cubic term for time dependence of dependent variable included but not reported.", covariate.labels = c("Autonomy (1989)","Recent concession","Most powerful","Included","Group size","TEK irredentism","TEK SDMs", "Petroleum % in settlement area","Previous SDMs","Normalized polity score","Distance border (log)","GDP p.c. (log)","Population (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))


####################################################################################
#########3. Robustness checks (appendix E)#########
####################################################################################

##########3.1 Different time windows I - all years between 1990 and 2017##########
r1a_sd_onset_1989_main <- glm(as.formula(paste("sd_onset", paste(c("terraut_1989", "state_control", group_controls, country_controls, "years_no_sd_l1", "I(years_no_sd_l1^2)", "I(years_no_sd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, sd_l1 == 0), family=binomial(link="logit"),control = list(maxit = 50),na.action = na.exclude)
cse_r1a_sd_onset_1989_main <- data.frame(cluster.se(r1a_sd_onset_1989_main,as.factor(subset(df_ceefsu, sd_l1 == 0)$cowcode))[, 2])
r1a_terraut_up_1989_main <- glm(as.formula(paste("terraut_up", paste(c("terraut_1989", group_controls, country_controls), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, sd_l1 == 1 & sd_period_terraut_up_l1==0), family=binomial(link="logit"),control = list(maxit = 50))
cse_r1a_terraut_up_1989_main <- data.frame(cluster.se(r1a_terraut_up_1989_main,as.factor(subset(df_ceefsu, sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))[, 2])
r1a_sd_escalation_1989_main <- glm(as.formula(paste("violsd_onset", paste(c("terraut_1989", "recent_concession5", group_controls, country_controls, "years_no_violsd_l1", "I(years_no_violsd_l1^2)", "I(years_no_violsd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, violsd_l1 == 0 & sd_l1 == 1), family=binomial(link="logit"),control = list(maxit = 50))
cse_r1a_sd_escalation_1989_main <- data.frame(cluster.se(r1a_sd_escalation_1989_main,as.factor(subset(df_ceefsu, violsd_l1 == 0 & sd_l1 == 1)$cowcode))[, 2])
stargazer(type="text",r1a_sd_onset_1989_main,r1a_terraut_up_1989_main, r1a_sd_escalation_1989_main,se=c(cse_r1a_sd_onset_1989_main, cse_r1a_terraut_up_1989_main, cse_r1a_sd_escalation_1989_main), column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("SDM onset","Concession","Escalation during SDM","Escalation during SDM"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses. Cubic term for time dependence of dependent variable included but not reported.", covariate.labels = c("Autonomy (1989)","Recent concession","Most powerful","Included","Group size","TEK irredentism","TEK SDMs", "Petroleum % in settlement area","Previous SDMs","Normalized polity score","Distance border (log)","GDP p.c. (log)","Population (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))

##########3.2 Different time windows II - immediate post-transition phase##########
r1b_sd_onset_1989_main <- glm(as.formula(paste("sd_onset", paste(c("terraut_1989", "state_control", group_controls, country_controls, "years_no_sd_l1", "I(years_no_sd_l1^2)", "I(years_no_sd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, year <= 1994 & sd_l1 == 0), family=binomial(link="logit"),control = list(maxit = 50),na.action = na.exclude)
cse_r1b_sd_onset_1989_main <- data.frame(cluster.se(r1b_sd_onset_1989_main,as.factor(subset(df_ceefsu, year <= 1994 & sd_l1 == 0)$cowcode))[, 2])
r1b_terraut_up_1989_main <- glm(as.formula(paste("terraut_up", paste(c("terraut_1989", group_controls, country_controls), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, year <= 1994 & sd_l1 == 1 & sd_period_terraut_up_l1==0), family=binomial(link="logit"),control = list(maxit = 50))
cse_r1b_terraut_up_1989_main <- data.frame(cluster.se(r1b_terraut_up_1989_main,as.factor(subset(df_ceefsu, year <= 1994 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))[, 2])
r1b_sd_escalation_1989_main <- glm(as.formula(paste("violsd_onset", paste(c("terraut_1989", "recent_concession5", group_controls, country_controls, "years_no_violsd_l1", "I(years_no_violsd_l1^2)", "I(years_no_violsd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, year <= 1994 & violsd_l1 == 0 & sd_l1 == 1), family=binomial(link="logit"),control = list(maxit = 50))
cse_r1b_sd_escalation_1989_main <- data.frame(cluster.se(r1b_sd_escalation_1989_main,as.factor(subset(df_ceefsu, year <= 1994 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))[, 2])
stargazer(type="text",r1b_sd_onset_1989_main,r1b_terraut_up_1989_main, r1b_sd_escalation_1989_main,se=c(cse_r1b_sd_onset_1989_main, cse_r1b_terraut_up_1989_main, cse_r1b_sd_escalation_1989_main), column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("SDM onset","Concession","Escalation during SDM","Escalation during SDM"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses. Cubic term for time dependence of dependent variable included but not reported.", covariate.labels = c("Autonomy (1989)","Recent concession","Most powerful","Included","Group size","TEK irredentism","TEK SDMs", "Petroleum % in settlement area","Previous SDMs","Normalized polity score","Distance border (log)","GDP p.c. (log)","Population (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))

##########3.3 Only multi-ethnic states##########
r3a_sd_onset_1989_main <- glm(as.formula(paste("sd_onset", paste(c("terraut_1989", "state_control", group_controls, country_controls, "years_no_sd_l1", "I(years_no_sd_l1^2)", "I(years_no_sd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, nopluralitysum_l1 >= 0.1 & cee_fsu_transition == 1 & sd_l1 == 0), family=binomial(link="logit"),control = list(maxit = 50),na.action = na.exclude)
cse_r3a_sd_onset_1989_main <- data.frame(cluster.se(r3a_sd_onset_1989_main,as.factor(subset(df_ceefsu,  nopluralitysum_l1 >= 0.1 & cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))[, 2])
r3a_terraut_up_1989_main <- glm(as.formula(paste("terraut_up", paste(c("terraut_1989", group_controls, country_controls), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, nopluralitysum_l1 >= 0.1 & cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0), family=binomial(link="logit"),control = list(maxit = 50))
cse_r3a_terraut_up_1989_main <- data.frame(cluster.se(r3a_terraut_up_1989_main,as.factor(subset(df_ceefsu, nopluralitysum_l1 >= 0.1 & cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))[, 2])
r3a_sd_escalation_1989_main <- glm(as.formula(paste("violsd_onset", paste(c("terraut_1989", "recent_concession5", group_controls, country_controls, "years_no_violsd_l1", "I(years_no_violsd_l1^2)", "I(years_no_violsd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, nopluralitysum_l1 >= 0.1 & cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1), family=binomial(link="logit"),control = list(maxit = 50))
cse_r3a_sd_escalation_1989_main <- data.frame(cluster.se(r3a_sd_escalation_1989_main,as.factor(subset(df_ceefsu, nopluralitysum_l1 >= 0.1 & cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))[, 2])
stargazer(type="text",r3a_sd_onset_1989_main,r3a_terraut_up_1989_main, r3a_sd_escalation_1989_main,se=c(cse_r3a_sd_onset_1989_main, cse_r3a_terraut_up_1989_main, cse_r3a_sd_escalation_1989_main), column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("SDM onset","Concession","Escalation during SDM","Escalation during SDM"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses. Cubic term for time dependence of dependent variable included but not reported.", covariate.labels = c("Autonomy (1989)","Recent concession","Most powerful","Included","Group size","TEK irredentism","TEK SDMs", "Petroleum % in settlement area","Previous SDMs","Normalized polity score","Distance border (log)","GDP p.c. (log)","Population (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))

##########3.4 Only FSU##########
r3b_sd_onset_1989_main <- glm(as.formula(paste("sd_onset", paste(c("terraut_1989", "state_control", group_controls, country_controls_limited, "years_no_sd_l1", "I(years_no_sd_l1^2)", "I(years_no_sd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 0), family=binomial(link="logit"),control = list(maxit = 50),na.action = na.exclude)
cse_r3b_sd_onset_1989_main <- data.frame(cluster.se(r3b_sd_onset_1989_main,as.factor(subset(df_ceefsu,  FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))[, 2])
r3b_terraut_up_1989_main <- glm(as.formula(paste("terraut_up", paste(c("terraut_1989", group_controls, country_controls_limited), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0), family=binomial(link="logit"),control = list(maxit = 50))
cse_r3b_terraut_up_1989_main <- data.frame(cluster.se(r3b_terraut_up_1989_main,as.factor(subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))[, 2])
r3b_sd_escalation_1989_main <- glm(as.formula(paste("violsd_onset", paste(c("terraut_1989", "recent_concession5", group_controls, country_controls_limited, "years_no_violsd_l1", "I(years_no_violsd_l1^2)", "I(years_no_violsd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1), family=binomial(link="logit"),control = list(maxit = 50))
cse_r3b_sd_escalation_1989_main <- data.frame(cluster.se(r3b_sd_escalation_1989_main,as.factor(subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))[, 2])
stargazer(type="text",r3b_sd_onset_1989_main,r3b_terraut_up_1989_main, r3b_sd_escalation_1989_main,se=c(cse_r3b_sd_onset_1989_main, cse_r3b_terraut_up_1989_main, cse_r3b_sd_escalation_1989_main), column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("SDM onset","Concession","Escalation during SDM","Escalation during SDM"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses. Cubic term for time dependence of dependent variable included but not reported.", covariate.labels = c("Autonomy (1989)","Recent concession","Most powerful","Included","Group size","TEK irredentism","TEK SDMs", "Petroleum % in settlement area","Normalized polity score","Distance border (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))

##########3.5 Only FSU + autonomy measured in 1936##########
r3b_sd_onset_1936_main <- glm(as.formula(paste("sd_onset", paste(c("terraut_1936", "state_control", group_controls, country_controls_limited, "years_no_sd_l1", "I(years_no_sd_l1^2)", "I(years_no_sd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 0), family=binomial(link="logit"),control = list(maxit = 50),na.action = na.exclude)
cse_r3b_sd_onset_1936_main <- data.frame(cluster.se(r3b_sd_onset_1936_main,as.factor(subset(df_ceefsu,  FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))[, 2])
r3b_terraut_up_1936_main <- glm(as.formula(paste("terraut_up", paste(c("terraut_1936", group_controls, country_controls_limited), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0), family=binomial(link="logit"),control = list(maxit = 50))
cse_r3b_terraut_up_1936_main <- data.frame(cluster.se(r3b_terraut_up_1936_main,as.factor(subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))[, 2])
r3b_sd_escalation_1936_main <- glm(as.formula(paste("violsd_onset", paste(c("terraut_1936", "recent_concession5", group_controls, country_controls_limited, "years_no_violsd_l1", "I(years_no_violsd_l1^2)", "I(years_no_violsd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1), family=binomial(link="logit"),control = list(maxit = 50))
cse_r3b_sd_escalation_1936_main <- data.frame(cluster.se(r3b_sd_escalation_1936_main,as.factor(subset(df_ceefsu, FSU == 1 & cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))[, 2])
stargazer(type="text",r3b_sd_onset_1936_main,r3b_terraut_up_1936_main, r3b_sd_escalation_1936_main,se=c(cse_r3b_sd_onset_1936_main, cse_r3b_terraut_up_1936_main, cse_r3b_sd_escalation_1936_main), column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("SDM onset","Concession","Escalation during SDM","Escalation during SDM"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses. Cubic term for time dependence of dependent variable included but not reported.", covariate.labels = c("Autonomy (1936)","Recent concession","Most powerful","Included","Group size","TEK irredentism","TEK SDMs", "Petroleum % in settlement area","Normalized polity score","Distance border (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))

##########3.6 Controlling for autonomy loss##########
r2a_sd_onset_1989_main <- glm(as.formula(paste("sd_onset", paste(c("terraut_1989", "state_control", group_controls, country_controls, alternative_controls3, "years_no_sd_l1", "I(years_no_sd_l1^2)", "I(years_no_sd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0), family=binomial(link="logit"),control = list(maxit = 50),na.action = na.exclude)
cse_r2a_sd_onset_1989_main <- data.frame(cluster.se(r2a_sd_onset_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))[, 2])
r2a_terraut_up_1989_main <- glm(as.formula(paste("terraut_up", paste(c("terraut_1989", group_controls, country_controls, alternative_controls3), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0), family=binomial(link="logit"),control = list(maxit = 50))
cse_r2a_terraut_up_1989_main <- data.frame(cluster.se(r2a_terraut_up_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))[, 2])
r2a_sd_escalation_1989_main <- glm(as.formula(paste("violsd_onset", paste(c("terraut_1989", "recent_concession5", group_controls, country_controls, alternative_controls3, "years_no_violsd_l1", "I(years_no_violsd_l1^2)", "I(years_no_violsd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1), family=binomial(link="logit"),control = list(maxit = 50))
cse_r2a_sd_escalation_1989_main <- data.frame(cluster.se(r2a_sd_escalation_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))[, 2])
stargazer(type="text",r2a_sd_onset_1989_main,r2a_terraut_up_1989_main, r2a_sd_escalation_1989_main,se=c(cse_r2a_sd_onset_1989_main, cse_r2a_terraut_up_1989_main, cse_r2a_sd_escalation_1989_main), column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("SDM onset","Concession","Escalation during SDM","Escalation during SDM"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses. Cubic term for time dependence of dependent variable included but not reported.", covariate.labels = c("Autonomy (1989)","Recent concession","Most powerful","Included","Group size","TEK irredentism","TEK SDMs", "Petroleum % in settlement area","Previous SDMs","Normalized polity score","Autonomy loss since 1989","Distance border (log)","GDP p.c. (log)","Population (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))

##########3.7 Controlling for bargaining environment##########
r2b_sd_onset_1989_main <- glm(as.formula(paste("sd_onset", paste(c("terraut_1989", "state_control", group_controls, country_controls, alternative_controls4, "years_no_sd_l1", "I(years_no_sd_l1^2)", "I(years_no_sd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0), family=binomial(link="logit"),control = list(maxit = 50),na.action = na.exclude)
cse_r2b_sd_onset_1989_main <- data.frame(cluster.se(r2b_sd_onset_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))[, 2])
r2b_terraut_up_1989_main <- glm(as.formula(paste("terraut_up", paste(c("terraut_1989", group_controls, country_controls, alternative_controls4), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0), family=binomial(link="logit"),control = list(maxit = 50))
cse_r2b_terraut_up_1989_main <- data.frame(cluster.se(r2b_terraut_up_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))[, 2])
r2b_sd_escalation_1989_main <- glm(as.formula(paste("violsd_onset", paste(c("terraut_1989", "recent_concession5", group_controls, country_controls, alternative_controls4, "years_no_violsd_l1", "I(years_no_violsd_l1^2)", "I(years_no_violsd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1), family=binomial(link="logit"),control = list(maxit = 50))
cse_r2b_sd_escalation_1989_main <- data.frame(cluster.se(r2b_sd_escalation_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))[, 2])
stargazer(type="text",r2b_sd_onset_1989_main,r2b_terraut_up_1989_main, r2b_sd_escalation_1989_main,se=c(cse_r2b_sd_onset_1989_main, cse_r2b_terraut_up_1989_main, cse_r2b_sd_escalation_1989_main), column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("SDM onset","Concession","Escalation during SDM","Escalation during SDM"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses. Cubic term for time dependence of dependent variable included but not reported.", covariate.labels = c("Autonomy (1989)","Recent concession","Most powerful","Included","Group size","TEK irredentism","TEK SDMs", "Petroleum % in settlement area","Previous SDMs","Normalized polity score","Autonomy concession (other group)","Number of groups","Distance border (log)","GDP p.c. (log)","Population (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))

##########3.8 Controlling for SDM years before 1989##########
r2c_sd_onset_1989_main <- glm(as.formula(paste("sd_onset", paste(c("terraut_1989", "state_control", group_controls, country_controls, "lsd_b1989_count", "years_no_sd_l1", "I(years_no_sd_l1^2)", "I(years_no_sd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0), family=binomial(link="logit"),control = list(maxit = 50),na.action = na.exclude)
cse_r2c_sd_onset_1989_main <- data.frame(cluster.se(r2c_sd_onset_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 0)$cowcode))[, 2])
r2c_terraut_up_1989_main <- glm(as.formula(paste("terraut_up", paste(c("terraut_1989", group_controls, country_controls, "lsd_b1989_count"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0), family=binomial(link="logit"),control = list(maxit = 50))
cse_r2c_terraut_up_1989_main <- data.frame(cluster.se(r2c_terraut_up_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & sd_l1 == 1 & sd_period_terraut_up_l1==0)$cowcode))[, 2])
r2c_sd_escalation_1989_main <- glm(as.formula(paste("violsd_onset", paste(c("terraut_1989", "recent_concession5", group_controls, country_controls, "lsd_b1989_count", "years_no_violsd_l1", "I(years_no_violsd_l1^2)", "I(years_no_violsd_l1^3)"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1), family=binomial(link="logit"),control = list(maxit = 50))
cse_r2c_sd_escalation_1989_main <- data.frame(cluster.se(r2c_sd_escalation_1989_main,as.factor(subset(df_ceefsu, cee_fsu_transition == 1 & violsd_l1 == 0 & sd_l1 == 1)$cowcode))[, 2])
stargazer(type="text",r2c_sd_onset_1989_main,r2c_terraut_up_1989_main, r2c_sd_escalation_1989_main,se=c(cse_r2c_sd_onset_1989_main, cse_r2c_terraut_up_1989_main, cse_r2c_sd_escalation_1989_main), column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("SDM onset","Concession","Escalation during SDM","Escalation during SDM"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses. Cubic term for time dependence of dependent variable included but not reported.", covariate.labels = c("Autonomy (1989)","Recent concession","Most powerful","Included","Group size","TEK irredentism","TEK SDMs", "Petroleum % in settlement area","Previous SDMs","Normalized polity score","SDM years before 1989 (log)","Distance border (log)","GDP p.c. (log)","Population (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))

##########3.9 Instrumenting for autonomy with autonomy in 1989##########
#see Stata script

##########3.10 Probing the impact of SDMs on changes in autonomy in pre-transition period##########
concession_pre_cont <- lm(as.formula(paste("terraut - terraut_l1", paste(c("terraut_l1","sd_l1", group_controls, country_controls), collapse = " + "), sep = " ~ ")), data=df_ceefsu_before)
cluster.se(concession_pre_cont,as.factor(df_ceefsu_before$cowcode))
cse_concession_pre_cont <- data.frame(cluster.se(concession_pre_cont,as.factor(df_ceefsu_before$cowcode))[, 2])
#concession continuous, all CEE-FSU
concession_pre_cont_fsu <- lm(as.formula(paste("terraut - terraut_l1", paste(c("terraut_l1","sd_l1", "state_control","size","log(distance_border+0.00001)","tek_sdm","oil_area_pct", country_controls), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu_before, FSU == 1))
se_concession_pre_cont_fsu <- data.frame(summary(concession_pre_cont_fsu)$coefficients[, 2])#excluding included and tek_sup_irre_any due to multicollinearity
stargazer(type="text",concession_pre_cont,concession_pre_cont_fsu,se=c(cse_concession_pre_cont,se_concession_pre_cont_fsu), column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("Autonomy (t) - Autonomy (t-1)","Autonomy (t) - Autonomy (t-1)"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses. Cubic term for time dependence of dependent variable included but not reported.", covariate.labels = c("Autonomy (t-1)","Ongoing SDM (t-1)","Most powerful","Included","Group size","TEK irredentism","TEK SDMs", "Petroleum % in settlement area","Previous SDMs","Normalized polity score","Distance border (log)","GDP p.c. (log)","Population (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))

##########3.11 Reverse estimation##########
df_ceefsu <- df_ceefsu %>% group_by(gwgroupid) %>% arrange(gwgroupid,year) %>% mutate(sd_onsets_sum_after1989 = sum(sd_onset, na.rm=T))
df_ceefsu <- df_ceefsu %>% group_by(gwgroupid) %>% arrange(gwgroupid,year) %>% mutate(sd_escalation_sum_after1989 = sum(violsd_onset, na.rm=T))
df_ceefsu <- df_ceefsu %>% group_by(gwgroupid) %>% arrange(gwgroupid,year) %>% mutate(year_group_start = min(year, na.rm=T))
sd_onset_1989_placebo <- lm(as.formula(paste("terraut_1989", paste(c("sd_onsets_sum_after1989", "size", "log(distance_border+0.00001)", "oil_area_pct"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, year == year_group_start))
cse_sd_onset_1989_placebo <- data.frame(cluster.se(sd_onset_1989_placebo,as.factor(subset(df_ceefsu, year == year_group_start)$cowcode))[, 2])
sd_escalation_1989_placebo <- lm(as.formula(paste("terraut_1989", paste(c("sd_escalation_sum_after1989", "size", "log(distance_border+0.00001)", "oil_area_pct"), collapse = " + "), sep = " ~ ")), data=subset(df_ceefsu, year == year_group_start))
cse_sd_escalation_1989_placebo <- data.frame(cluster.se(sd_escalation_1989_placebo,as.factor(subset(df_ceefsu, year == year_group_start)$cowcode))[, 2])
stargazer(type="text",sd_onset_1989_placebo, sd_escalation_1989_placebo,se=c(cse_sd_onset_1989_placebo, cse_sd_escalation_1989_placebo), column.labels = c("Model 1 placebo", "Model 2 placebo", "Model 4 placebo"),omit=c("years_no"),  dep.var.labels.include=TRUE, dep.var.labels = c("Autonomy (1989)","Autonomy (1989)","Autonomy (1989)"), style = "ajps", notes.append = TRUE, notes.align = "l", model.numbers = F, notes = "Country-clustered errors in parentheses.", covariate.labels = c("SDM onsets after 1989","SDM escalations after 1989","Size in successor state", "Petroleum % in settlement area","Distance border successor state (log)"), order=c(paste0("^", vars.order_main1, "$"), ":", paste0("^", vars.order_main2, "$")))




