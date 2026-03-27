####################
#  Analysis, tables, and graphics
#  for WERN survey experiment
####################
# Created By: John Ahlquist 020923, Jake Grumbach 030623
# JSA edited to produce likert plots

library(car)
library(estimatr)
library(margins)
library(MASS)
library(broom)
library(ggplot2)
library(texreg)
library(sjPlot)
#library(likert)
library(broom)
library(sjmisc)
library(sjlabelled)


setwd("../data")

data <- readRDS("wern_experiment_analysis_data.rds"); gc()

outcome_vars <- c("dv_petition_num", "dv_call_rep_num", "dv_donate_num", "dv_social_med_num", "dv_union_num", "dv_strike_num", "dv_index")
outcome_vars_name <- c("Petition", "Call Rep.", "Donate", "Social Media", "Union", "Strike","Index")

data$x_dem_id <- NA
data$x_gop_id <- NA

data$x_dem_id[data$pol_pid3=="Independent"|data$pol_pid3=="Other"|data$pol_pid3=="Republican"] <- 0
data$x_gop_id[data$pol_pid3=="Independent"|data$pol_pid3=="Other"|data$pol_pid3=="Democrat"] <- 0

data$x_dem_id[data$pol_pid3=="Democrat"] <- 1
data$x_gop_id[data$pol_pid3=="Republican"] <- 1

data$x_age <- 2022 - data$age_num

data$x_age_group <- NA

data$x_age_group[data$age_num <= 1967] <- "55+"
data$x_age_group[data$age_num >= 1968 & data$age_num <= 1987] <- "35-54" 
data$x_age_group[data$age_num >= 1988 & data$age_num <= 2004] <- "18-34"

data$x_industry <- NA
data$x_industry[data$industry_screen_num==1] <- "Health Care"
data$x_industry[data$industry_screen_num==2] <- "Hospitality"
data$x_industry[data$industry_screen_num==3] <- "Retail"
data$x_industry[data$industry_screen_num==4] <- "Telecom"
data$x_industry[data$industry_screen_num==5] <- "Warehousing"

data$x_gender <- data$gender
data$x_gender[data$gender=="Non-Binary/Gender Non-conforming"] <- NA


spec_bv <- c("vignette_coworkertreat", "vignette_neighbortreat") #basic specification
spec_cov <- c("vignette_coworkertreat", "vignette_neighbortreat",
            "x_age", "x_race", "highest_degree", "pol_pid3", "female",
            "industry_screen", "rtw_live", "hh_income_num" ,"urban") #covariate specification from PAP

spec_hte_white <- c('vignette_coworkertreat*x_white',
                  'vignette_neighbortreat*x_white') # HTE by white/nonwhite
spec_hte_fem <- c("vignette_coworkertreat*female",
                "vignette_neighbortreat*female") # HTE by gender
spec_hte_rtw <- c("vignette_coworkertreat*rtw_live",
                "vignette_neighbortreat*rtw_live") # HTE by RtW status of R's state
spec_hte_pid3 <- c("vignette_coworkertreat*x_dem_id",
                  "vignette_neighbortreat*x_dem_id", 
                  "vignette_coworkertreat*x_gop_id",
                  "vignette_neighbortreat*x_gop_id") # HTE by Party ID pid3 status
spec_hte_race_conflict <- c("vignette_coworkertreat*race_create_conflict_num",
                          "vignette_neighbortreat*race_create_conflict_num") # HTE by racial conflict @ work
spec_hte_qir <- c("vignette_coworkertreat*qir_index",
                "vignette_neighbortreat*qir_index") # HTE by QIR index
spec_hte_age <- c("vignette_coworkertreat*x_age",
                  "vignette_neighbortreat*x_age") # HTE by age
spec_hte_wagetheft <- c("vignette_coworkertreat*x_wage_theft_index",
                        "vignette_neighbortreat*x_wage_theft_index") # HTE by wage theft index


###Wage theft descriptives

data$late_nopay_both <- 0
data$late_nopay_both[data$problem_exp_late_pay==1 & data$problem_exp_no_pay==1] <- 1

data$late_nopay_either <- 0
data$late_nopay_either[data$problem_exp_late_pay==1 | data$problem_exp_no_pay==1] <- 1

t.test(data$late_nopay_either)$conf.int[1:2]


plotdata <- data.frame(cbind(
  colMeans(data[,c("problem_exp_late_pay", "problem_exp_no_pay", "late_nopay_either", "late_nopay_both")]),
  c("Paid Late", "Underpaid", "Either", "Both")))
plotdata$estimate <- 100*as.numeric(plotdata$X1)

tiff("../output/wage_theft_descriptives.tiff", w=550, h=550, res=120)
ggplot(plotdata, aes(x=X2, y=estimate)) +
  geom_bar(stat="identity") +
  xlab("") +
  ylab("%") +
  scale_x_discrete(limits = rev(levels(factor(plotdata$X2)))) +
  #ylim(0, 20) +
  theme_bw()
dev.off()



##Wage theft by demographics

#age, race, gender, industry

age <- aggregate(cbind(problem_exp_late_pay, problem_exp_no_pay, late_nopay_either) ~ x_age_group, data, mean, na.rm=T)
race <- aggregate(cbind(problem_exp_late_pay, problem_exp_no_pay, late_nopay_either) ~ x_race, data, mean, na.rm=T)
gender <- aggregate(cbind(problem_exp_late_pay, problem_exp_no_pay, late_nopay_either) ~ x_gender, data, mean, na.rm=T)
industry <- aggregate(cbind(problem_exp_late_pay, problem_exp_no_pay, late_nopay_either) ~ x_industry, data, mean, na.rm=T)

names(age)[1] <- "group"
names(race)[1] <- "group"
names(gender)[1] <- "group"
names(industry)[1] <- "group"

age$category <- "Age"
race$category <- "Race"
gender$category <- "Gender"
industry$category <- "Industry"

plotdata <- rbind(age, race, gender, industry)
plotdata <- rbind(plotdata, plotdata, plotdata)
plotdata$estimate <- NA
plotdata$estimate[1:15] <- plotdata$problem_exp_late_pay[1:15]
plotdata$estimate[16:30] <- plotdata$problem_exp_no_pay[16:30]
plotdata$estimate[31:45] <- plotdata$late_nopay_either[31:45]

plotdata$outcome[1:15] <- "Paid Late"
plotdata$outcome[16:30] <- "Underpaid"
plotdata$outcome[31:45] <- "Either"

plotdata$estimate <- 100*plotdata$estimate

plotdata$group <- factor(plotdata$group, levels=rev(levels(factor(plotdata$group))))
plotdata$outcome <- factor(plotdata$outcome, levels=c("Paid Late","Underpaid","Either"))

pd <- position_dodge(.2)

tiff("../output/wage_theft_demographics.tiff", w=700, h=800, res=120)
ggplot(plotdata, aes(x=group, y=estimate, group=outcome, shape=outcome)) +
  geom_point(position=pd) +
  facet_wrap(~category, ncol=1, scales="free_y") +
  scale_shape_manual(values=c(19,17,0), name="") +
  #scale_y_continuous(limits=c(0,15), breaks=c(0,5,10,15)) +
  ylab("% Experienced in Past 6 Months") +
  xlab("") +
  coord_flip() +
  theme_bw()
dev.off()


data$factor_race <- relevel(as.factor(data$x_race), ref = 5)
data$factor_gender <- relevel(as.factor(data$x_gender), ref = 2)
data$x_nonenglish <- NA
data$x_nonenglish[grepl("english", data$hh_lang, ignore.case = T)] <- 0
data$x_nonenglish[grepl("english", data$hh_lang, ignore.case = T)==F] <- 1
data$x_nonenglish[data$hh_lang==""] <- 0


lm1 <- lm_robust(problem_exp_late_pay ~ x_age_group + factor_race + factor_gender + x_nonenglish + x_industry, se_type = "HC3", data)
lm2 <- lm_robust(problem_exp_no_pay ~ x_age_group + factor_race + factor_gender + x_nonenglish + x_industry, se_type = "HC3", data)
lm3 <- lm_robust(late_nopay_either ~ x_age_group + factor_race + factor_gender + x_nonenglish + x_industry, se_type = "HC3", data)

htmlreg(list(lm1, lm2, lm3), include.ci = FALSE, digits=3, custom.model.names=c("Late Pay", "No Pay", "Either"),
        file="../output/wage_theft_demographics_regs.doc")




###Wage theft predictors

data$x_union_support <- data$union_support_num
data$x_union_support[data$union_support_num==-77] <- 0.5

lm1 <- lm_robust(x_union_support ~ problem_exp_late_pay + problem_exp_no_pay, se_type = "HC3", data)
lm2 <- lm_robust(x_union_support ~ problem_exp_late_pay + problem_exp_no_pay +
                   x_age + x_race + highest_degree + pol_pid3 + female + industry_screen + 
                   rtw_live + hh_income_num + urban, se_type = "HC3", data)


htmlreg(list(lm1, lm2), include.ci = FALSE, digits=3, file="../output/wage_theft_predictors.doc")

plotdata <- rbind(tidy(lm1)[2:3,], tidy(lm2)[2:3,])
plotdata$spec <- NA
plotdata$spec[1:2] <- "Bivariate"
plotdata$spec[3:4] <- "Covariates"

plotdata$term_full <- rep(c("Paid Late", "Underpaid"), 2)

pd <- position_dodge(.35)

tiff("../output/wage_theft_predictors.tiff", w=500, h=500, res=120)
ggplot(plotdata, aes(x=term_full, y=estimate, shape=spec)) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0, position=pd) +
  scale_shape_manual(values=c(16, 17), name="Specification") +
  geom_hline(yintercept = 0, linetype=2) +
  xlab("") +
  ylab("") +
  theme_bw()
dev.off()



data$x_union_support <- data$union_support_num
data$x_union_support[data$union_support_num==-77] <- 0.5

lm1 <- lm_robust(x_union_support ~ problem_exp_late_pay + problem_exp_no_pay, se_type = "HC3", data[data$union_mem=="No",])
lm2 <- lm_robust(x_union_support ~ problem_exp_late_pay + problem_exp_no_pay +
                   x_age + x_race + highest_degree + pol_pid3 + female + industry_screen + 
                   rtw_live + hh_income_num + urban, se_type = "HC3", data[data$union_mem=="No",])


htmlreg(list(lm1, lm2), include.ci = FALSE, digits=3, file="../output/wage_theft_predictors_nonunion.doc")

plotdata <- rbind(tidy(lm1)[2:3,], tidy(lm2)[2:3,])
plotdata$spec <- NA
plotdata$spec[1:2] <- "Bivariate"
plotdata$spec[3:4] <- "Covariates"

plotdata$term_full <- rep(c("Paid Late", "Underpaid"), 2)

pd <- position_dodge(.35)


tiff("../output/wage_theft_predictors_nonunion.tiff", w=500, h=500, res=120)
ggplot(plotdata, aes(x=term_full, y=estimate, shape=spec)) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0, position=pd) +
  scale_shape_manual(values=c(16, 17), name="Specification") +
  geom_hline(yintercept = 0, linetype=2) +
  xlab("") +
  ylab("") +
  theme_bw()
dev.off()

#Late pay ONLY

data$problem_exp_late_pay_only <- NA
data$problem_exp_late_pay_only[data$problem_exp_late_pay==1 & data$problem_exp_no_pay==0] <- 1
data$problem_exp_late_pay_only[data$problem_exp_late_pay==0] <- 0

lm1 <- lm_robust(x_union_support ~ problem_exp_late_pay_only + problem_exp_no_pay, se_type = "HC3", data[data$union_mem=="No",])
lm2 <- lm_robust(x_union_support ~ problem_exp_late_pay_only + problem_exp_no_pay +
                   x_age + x_race + highest_degree + pol_pid3 + female + industry_screen + 
                   rtw_live + hh_income_num + urban, se_type = "HC3", data[data$union_mem=="No",])


plotdata <- rbind(tidy(lm1)[2:3,], tidy(lm2)[2:3,])
plotdata$spec <- NA
plotdata$spec[1:2] <- "Bivariate"
plotdata$spec[3:4] <- "Covariates"

plotdata$term_full <- rep(c("Paid Late (exclusively)", "Underpaid"), 2)

pd <- position_dodge(.35)


tiff("../output/wage_theft_predictors_nonunion_latepayONLY.tiff", w=500, h=500, res=120)
ggplot(plotdata, aes(x=term_full, y=estimate, shape=spec)) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0, position=pd) +
  scale_shape_manual(values=c(16, 17), name="Specification") +
  geom_hline(yintercept = 0, linetype=2) +
  xlab("") +
  ylab("") +
  theme_bw()
dev.off()

#excluding DKs
data$x_union_support <- data$union_support_num
data$x_union_support[data$union_support_num==-77] <- NA

lm1 <- lm_robust(x_union_support ~ problem_exp_late_pay + problem_exp_no_pay, se_type = "HC3", data)
lm2 <- lm_robust(x_union_support ~ problem_exp_late_pay + problem_exp_no_pay +
                   x_age + x_race + highest_degree + pol_pid3 + female + industry_screen + 
                   rtw_live + hh_income_num + urban, se_type = "HC3", data)


htmlreg(list(lm1, lm2), include.ci = FALSE, digits=3, file="../output/wage_theft_predictors_noDKs.doc")

plotdata <- rbind(tidy(lm1)[2:3,], tidy(lm2)[2:3,])
plotdata$spec <- NA
plotdata$spec[1:2] <- "Bivariate"
plotdata$spec[3:4] <- "Covariates"

plotdata$term_full <- rep(c("Paid Late", "Underpaid"), 2)

pd <- position_dodge(.35)

tiff("../output/wage_theft_predictors_noDKs.tiff", w=500, h=500, res=120)
ggplot(plotdata, aes(x=term_full, y=estimate, shape=spec)) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0, position=pd) +
  scale_shape_manual(values=c(16, 17), name="Specification") +
  geom_hline(yintercept = 0, linetype=2) +
  xlab("") +
  ylab("") +
  theme_bw()
dev.off()

data$x_union_support <- data$union_support_num
data$x_union_support[data$union_support_num==-77] <- 0

mean(data$x_union_support[data$late_nopay_either==1], na.rm=T)
mean(data$x_union_support[data$late_nopay_either==0], na.rm=T)

data$x_union_support[data$union_support_num==-77] <- NA
mean(data$x_union_support[data$late_nopay_either==1], na.rm=T)
mean(data$x_union_support[data$late_nopay_either==0], na.rm=T)



#wage theft and quit intention
data$x_find_job <- data$find_job_num
data$x_find_job[data$x_find_job==-77] <- NA
data$x_find_job <- (data$x_find_job-min(data$x_find_job, na.rm=T))/(max(data$x_find_job, na.rm=T)-min(data$x_find_job, na.rm=T))

lm1 <- lm_robust(x_find_job ~ problem_exp_late_pay + problem_exp_no_pay, se_type = "HC3", data)
lm2 <- lm_robust(x_find_job ~ problem_exp_late_pay + problem_exp_no_pay +
                   x_age + x_race + highest_degree + pol_pid3 + female + industry_screen + 
                   rtw_live + hh_income_num + urban, se_type = "HC3", data)


plotdata <- rbind(tidy(lm1)[2:3,], tidy(lm2)[2:3,])
plotdata$spec <- NA
plotdata$spec[1:2] <- "Bivariate"
plotdata$spec[3:4] <- "Covariates"

plotdata$term_full <- rep(c("Paid Late", "Underpaid"), 2)

pd <- position_dodge(.35)

tiff("../output/wage_theft_quit_intention.tiff", w=500, h=500, res=120)
ggplot(plotdata, aes(x=term_full, y=estimate, shape=spec)) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0, position=pd) +
  scale_shape_manual(values=c(16, 17), name="Specification") +
  geom_hline(yintercept = 0, linetype=2) +
  xlab("") +
  ylab("Intent to Quit") +
  theme_bw()
dev.off()


data$x_find_job_high <- NA
data$x_find_job_high[data$x_find_job>=0.75] <- 1
data$x_find_job_high[data$x_find_job<0.75] <- 0

mean(data$x_find_job_high[data$late_nopay_either==1], na.rm=T)
mean(data$x_find_job_high[data$late_nopay_either==0], na.rm=T)


summary(lm(late_nopay_either ~ x_union_support*x_find_job, data))
cor(data$x_union_support, data$x_find_job, use = "pairwise.complete.obs")


#late pay ONLY

lm1 <- lm_robust(x_find_job ~ problem_exp_late_pay_only + problem_exp_no_pay, se_type = "HC3", data)
lm2 <- lm_robust(x_find_job ~ problem_exp_late_pay_only + problem_exp_no_pay +
                   x_age + x_race + highest_degree + pol_pid3 + female + industry_screen + 
                   rtw_live + hh_income_num + urban, se_type = "HC3", data)


plotdata <- rbind(tidy(lm1)[2:3,], tidy(lm2)[2:3,])
plotdata$spec <- NA
plotdata$spec[1:2] <- "Bivariate"
plotdata$spec[3:4] <- "Covariates"

plotdata$term_full <- rep(c("Paid Late (exclusively)", "Underpaid"), 2)

pd <- position_dodge(.35)

tiff("../output/wage_theft_quit_intention_latepayONLY.tiff", w=500, h=500, res=120)
ggplot(plotdata, aes(x=term_full, y=estimate, shape=spec)) +
  geom_point(position=pd) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0, position=pd) +
  scale_shape_manual(values=c(16, 17), name="Specification") +
  geom_hline(yintercept = 0, linetype=2) +
  xlab("") +
  ylab("Intent to Quit") +
  theme_bw()
dev.off()






###Descriptives of DVs

outlist <- list()

for(i in outcome_vars){
  
  dv <- i
  
  temp <- data.frame(mean(data[data$vignette_coworkertreat==0 & data$vignette_neighbortreat==0,i], na.rm=T), 
                     median(data[data$vignette_coworkertreat==0 & data$vignette_neighbortreat==0,i], na.rm=T),
                     sd(data[data$vignette_coworkertreat==0 & data$vignette_neighbortreat==0,i], na.rm=T), 
                     i)
  
  names(temp) <- c("mean", "median", "sd", "outcome")
  
  outlist[[i]] <- temp
  
} 

plotdata <- do.call(rbind, outlist)

plotdata$outcome_var[grepl("petition", plotdata$outcome)] <- "Petition"
plotdata$outcome_var[grepl("strike", plotdata$outcome)] <- "Strike"
plotdata$outcome_var[grepl("union", plotdata$outcome)] <- "Union"
plotdata$outcome_var[grepl("call", plotdata$outcome)] <- "Call Rep."
plotdata$outcome_var[grepl("donate", plotdata$outcome)] <- "Donate"
plotdata$outcome_var[grepl("social", plotdata$outcome)] <- "Social Media"
plotdata$outcome_var[grepl("index", plotdata$outcome)] <- "Index"



plotdata$outcome_var <- factor(plotdata$outcome_var, levels = rev(c("Call Rep.", "Donate", "Petition", "Social Media", "Strike", "Union", "Index")))

tiff("../output/control_means_medians_sd.tiff", w=500, h=500, res=120)
ggplot(plotdata, aes(x=outcome_var, y=mean)) +
  geom_bar(stat="identity", fill="grey80", color="black") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd),
                width=0, linetype=2) +
  geom_point(aes(x=outcome_var, y=median), size=2) +
  xlab("Variable") +
  ylab("Mean (Bar) and Median (Point) Responses\nfor Control Group") +
  ylim(0,1) +
  coord_flip() +
  theme_bw()
dev.off()

### Likert version of plot

cntrldta<-3*data[data$vignette_coworkertreat==0 & data$vignette_neighbortreat==0,
outcome_vars]+1

cntrldta[is.na(cntrldta)]<- 5
cntrldta<-na.omit(cntrldta)
cntrldta <- cntrldta |> 
dplyr::select(-c("dv_index")) |>
dplyr::mutate_all( ~ ordered(.))

pdf("../output/DV_dist_plot.pdf")
p <- plot_likert(cntrldta, 
                 axis.labels = outcome_vars_name[1:length(outcome_vars_name)-1],
                 #rel_heights = c(5, 3),
                 cat.neutral = 5,
                 catcount = 4,
                 sort.frq = "neg.desc",
                 legend.labels = c(
                  "extremely unlikley",
                  "somewhat unlikely",
                  "somewhat likely",
                  "extremely likely", "DK"),
                 #grid.range = c(1,.5),
                 reverse.scale = T,
                 values = "sum.inside", 
                 digits = 0,
                 show.prc.sign=T,
                 title = "Willingness to take action (control group)"
)
dev.off()

########Main Analysis

outlist_main <- list()
outlist_models_main <- list()
outlist_models_supp <- list()

for(i in outcome_vars){
  dv <- i
  #formula objects; no need to alter; change specifications above or, if new models, add.
  f_base <-  as.formula( 
    paste(dv,
          paste(spec_bv, collapse=" + "),
          sep=" ~ "))
  f_cov <- as.formula(
    paste(dv,
          paste(spec_cov, collapse=" + "),
          sep=" ~ "))
  
  
  # models with HC3 SEs
  
  m_base <- lm_robust(f_base, data, se_type = "HC3")
  m_cov <- lm_robust(f_cov, data, se_type = "HC3")
  
  m_base_wtd <- lm_robust(f_base, data, weights=rk_wgt_final, se_type = "HC3")
  m_cov_wtd <- lm_robust(f_cov, data, weights=rk_wgt_final, se_type = "HC3")
  
  
  outlist_models_main[[i]] <- list(m_base, m_cov, m_base_wtd, m_cov_wtd)
  
  m_base <- data.frame(tidy(m_base))[2:3,]
  m_cov <- data.frame(tidy(m_cov))[2:3,]
  m_base_wtd <- data.frame(tidy(m_base_wtd))[2:3,]
  m_cov_wtd <- data.frame(tidy(m_cov_wtd))[2:3,]
  
  m_base$weighted <- "Unweighted"
  m_cov$weighted <- "Unweighted"
  
  m_base_wtd$weighted <- "Weighted"
  m_cov_wtd$weighted <- "Weighted"
  
  m_base$covariates <- "Bivariate"
  m_cov$covariates <- "Covariates"
  m_base_wtd$covariates <- "Bivariate"
  m_cov_wtd$covariates <- "Covariates"
  
  outlist_main[[i]] <- rbind(m_base, m_cov, m_base_wtd, m_cov_wtd)
  
} 

#output main covariates models to .doc
htmlreg(list(outlist_models_main[[1]][[2]], outlist_models_main[[2]][[2]], outlist_models_main[[3]][[2]], 
             outlist_models_main[[4]][[2]], outlist_models_main[[5]][[2]], outlist_models_main[[6]][[2]], 
             outlist_models_main[[7]][[2]]), include.ci = FALSE, digits=3, file="../output/main_results_unweighted_cov.doc")

htmlreg(list(outlist_models_main[[1]][[1]], outlist_models_main[[2]][[1]], outlist_models_main[[3]][[1]], 
             outlist_models_main[[4]][[1]], outlist_models_main[[5]][[1]], outlist_models_main[[6]][[1]], 
             outlist_models_main[[7]][[1]]), include.ci = FALSE, digits=3, file="../output/main_results_unweighted_bv.doc")

htmlreg(list(outlist_models_main[[1]][[4]], outlist_models_main[[2]][[4]], outlist_models_main[[3]][[4]], 
             outlist_models_main[[4]][[4]], outlist_models_main[[5]][[4]], outlist_models_main[[6]][[4]], 
             outlist_models_main[[7]][[4]]), include.ci = FALSE, digits=3, file="../output/main_results_weighted_cov.doc")

htmlreg(list(outlist_models_main[[1]][[3]], outlist_models_main[[2]][[3]], outlist_models_main[[3]][[3]], 
             outlist_models_main[[4]][[3]], outlist_models_main[[5]][[3]], outlist_models_main[[6]][[3]], 
             outlist_models_main[[7]][[3]]), include.ci = FALSE, digits=3, file="../output/main_results_weighted_bv.doc")


#main results figure

plotdata <- do.call(rbind, outlist_main)

plotdata$variable <- NA
plotdata$variable[grepl("coworker", plotdata$term)] <- "Coworker\nTreatment"
plotdata$variable[grepl("neighbor", plotdata$term)] <- "Neighbor\nTreatment"


plotdata$outcome_var[grepl("petition", plotdata$outcome)] <- "Petition"
plotdata$outcome_var[grepl("strike", plotdata$outcome)] <- "Strike"
plotdata$outcome_var[grepl("union", plotdata$outcome)] <- "Union"
plotdata$outcome_var[grepl("call", plotdata$outcome)] <- "Call Rep."
plotdata$outcome_var[grepl("donate", plotdata$outcome)] <- "Donate"
plotdata$outcome_var[grepl("social", plotdata$outcome)] <- "Social Media"
plotdata$outcome_var[grepl("index", plotdata$outcome)] <- "Index"

plotdata$outcome_var <- factor(plotdata$outcome_var, levels = c("Call Rep.", "Donate", "Petition", "Social Media", "Strike", "Union", "Index"))


plotdata$covariates_weighted <- paste(plotdata$covariates, plotdata$weighted, sep=" ")

plotdata$covariates_weighted <- factor(plotdata$covariates_weighted, levels = c("Bivariate Unweighted", "Covariates Unweighted",
                                                                                "Bivariate Weighted", "Covariates Weighted"))


pd <- position_dodge(.35)

tiff("../output/main_results.tiff", w=1000, h=720)
ggplot(plotdata[plotdata$weighted=="Unweighted",], aes(x=variable, shape=covariates, y=estimate)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error),
                position=pd, width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_manual(values=c(19,15,17,18), name="Specification") +
  #scale_shape_manual(values=c(19,15), name="Specification") +
  facet_wrap("outcome_var") +
  xlab("") +
  ylab("Treatment Effect") +
  #coord_flip() +
  theme_bw()
dev.off()


tiff("../output/main_results_weighted.tiff", w=1000, h=720)
ggplot(plotdata[plotdata$weighted=="Weighted",], aes(x=variable, shape=covariates, y=estimate)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error),
                position=pd, width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_manual(values=c(19,15,17,18), name="Specification") +
  #scale_shape_manual(values=c(19,15), name="Specification") +
  facet_wrap("outcome_var") +
  xlab("") +
  ylab("Treatment Effect") +
  #coord_flip() +
  theme_bw()
dev.off()





####HTE analysis

outlist_models_supp <- list()

outlist_hte_white <- list()
outlist_hte_fem  <- list()
outlist_hte_rtw <- list()
outlist_hte_race_conflict  <- list()
outlist_hte_qir <- list()
outlist_hte_pid3 <- list()
outlist_hte_age <- list()
outlist_hte_wagetheft <- list()


for(i in outcome_vars){
  dv <- i
  
  f_hte_white <- as.formula(
    paste(dv,
          paste(spec_hte_white, collapse=" + "),
          sep=" ~ "))
  f_hte_fem <- as.formula(
    paste(dv,
          paste(spec_hte_fem, collapse=" + "),
          sep=" ~ "))
  f_hte_rtw <- as.formula(
    paste(dv,
          paste(spec_hte_rtw, collapse=" + "),
          sep=" ~ "))
  f_hte_race_conflict <- as.formula(
    paste(dv,
          paste(spec_hte_race_conflict, collapse=" + "),
          sep=" ~ "))
  f_hte_qir <- as.formula(
    paste(dv,
          paste(spec_hte_qir, collapse=" + "),
          sep=" ~ "))
  f_hte_pid3 <- as.formula(
    paste(dv,
          paste(spec_hte_pid3, collapse=" + "),
          sep=" ~ "))
  f_hte_age <- as.formula(
    paste(dv,
          paste(spec_hte_age, collapse=" + "),
          sep=" ~ "))
  f_hte_wagetheft <- as.formula(
    paste(dv,
          paste(spec_hte_wagetheft, collapse=" + "),
          sep=" ~ "))
  
  m_hte_white <- lm_robust(f_hte_white, data, se_type = "HC3")
  m_hte_fem <- lm_robust(f_hte_fem, data, se_type = "HC3")
  m_hte_rtw <- lm_robust(f_hte_rtw, data, se_type = "HC3")
  m_hte_race_conflict <- lm_robust(f_hte_race_conflict, data, se_type = "HC3")
  m_hte_qir <- lm_robust(f_hte_qir, data, se_type = "HC3")
  m_hte_pid3 <- lm_robust(f_hte_pid3, data, se_type = "HC3")
  m_hte_age <- lm_robust(f_hte_age, data, se_type = "HC3")
  m_hte_wagetheft <- lm_robust(f_hte_wagetheft, data, se_type = "HC3")
  
  m_hte_white_wtd <- lm_robust(f_hte_white, data, weights=rk_wgt_final, se_type = "HC3")
  m_hte_fem_wtd <- lm_robust(f_hte_fem, data, weights=rk_wgt_final, se_type = "HC3")
  m_hte_rtw_wtd <- lm_robust(f_hte_rtw, data, weights=rk_wgt_final, se_type = "HC3")
  m_hte_race_conflict_wtd <- lm_robust(f_hte_race_conflict, data, weights=rk_wgt_final, se_type = "HC3")
  m_hte_qir_wtd <- lm_robust(f_hte_qir, data, weights=rk_wgt_final, se_type = "HC3")
  m_hte_pid3_wtd <- lm_robust(f_hte_pid3, data, weights=rk_wgt_final, se_type = "HC3")
  m_hte_age_wtd <- lm_robust(f_hte_age, data, weights=rk_wgt_final, se_type = "HC3")
  m_hte_wagetheft_wtd <- lm_robust(f_hte_wagetheft, data, weights=rk_wgt_final, se_type = "HC3")
  
  #all HTE model objects
  outlist_models_supp[[i]] <- list(m_hte_white, m_hte_fem, m_hte_rtw, m_hte_race_conflict, m_hte_qir, m_hte_pid3, m_hte_age, m_hte_wagetheft,
                                   m_hte_white_wtd, m_hte_fem_wtd, m_hte_rtw_wtd, m_hte_race_conflict_wtd, m_hte_qir_wtd, m_hte_pid3_wtd, m_hte_age_wtd, m_hte_wagetheft_wtd)
  
  #tidying/cleaning
  m_hte_white <- data.frame(tidy(m_hte_white))
  m_hte_fem <- data.frame(tidy(m_hte_fem))
  m_hte_rtw <- data.frame(tidy(m_hte_rtw))
  m_hte_race_conflict <- data.frame(tidy(m_hte_race_conflict))
  m_hte_qir <- data.frame(tidy(m_hte_qir))
  m_hte_pid3 <- data.frame(tidy(m_hte_pid3))
  m_hte_age <- data.frame(tidy(m_hte_age))
  m_hte_wagetheft <- data.frame(tidy(m_hte_wagetheft))
  
  m_hte_white_wtd <- data.frame(tidy(m_hte_white_wtd))
  m_hte_fem_wtd <- data.frame(tidy(m_hte_fem_wtd))
  m_hte_rtw_wtd <- data.frame(tidy(m_hte_rtw_wtd))
  m_hte_race_conflict_wtd <- data.frame(tidy(m_hte_race_conflict_wtd))
  m_hte_qir_wtd <- data.frame(tidy(m_hte_qir_wtd))
  m_hte_pid3_wtd <- data.frame(tidy(m_hte_pid3_wtd))
  m_hte_age_wtd <- data.frame(tidy(m_hte_age_wtd))
  m_hte_wagetheft_wtd <- data.frame(tidy(m_hte_wagetheft_wtd))
  
  m_hte_white$weighted <- "Unweighted"
  m_hte_fem$weighted <- "Unweighted"
  m_hte_rtw$weighted <- "Unweighted"
  m_hte_race_conflict$weighted <- "Unweighted"
  m_hte_qir$weighted <- "Unweighted"
  m_hte_pid3$weighted <- "Unweighted"
  m_hte_age$weighted <- "Unweighted"
  m_hte_wagetheft$weighted <- "Unweighted"
  
  m_hte_white_wtd$weighted <- "Weighted"
  m_hte_fem_wtd$weighted <- "Weighted"
  m_hte_rtw_wtd$weighted <- "Weighted"
  m_hte_race_conflict_wtd$weighted <- "Weighted"
  m_hte_qir_wtd$weighted <- "Weighted"
  m_hte_pid3_wtd$weighted <- "Weighted"
  m_hte_age_wtd$weighted <- "Weighted"
  m_hte_wagetheft_wtd$weighted <- "Weighted"
  
  outlist_hte_white[[i]] <- rbind(m_hte_white, m_hte_white_wtd)
  outlist_hte_fem[[i]] <- rbind(m_hte_fem, m_hte_fem_wtd)
  outlist_hte_rtw[[i]] <- rbind(m_hte_rtw, m_hte_rtw_wtd)
  outlist_hte_race_conflict[[i]] <- rbind(m_hte_race_conflict, m_hte_race_conflict_wtd)
  outlist_hte_qir[[i]] <- rbind(m_hte_qir, m_hte_qir_wtd)
  outlist_hte_pid3[[i]] <- rbind(m_hte_pid3, m_hte_pid3_wtd)
  outlist_hte_age[[i]] <- rbind(m_hte_age, m_hte_age_wtd)
  outlist_hte_wagetheft[[i]] <- rbind(m_hte_wagetheft, m_hte_wagetheft_wtd)
  
}


###white

outlist <- list()

for(i in 1:7){
  
  cat(i, "\n")
  
  temp <- summary(margins(outlist_models_supp[[i]][[1]], at=list(x_white=0:1)))
  temp$outcome_var <- outcome_vars_name[i]
  temp$weighted <- "Unweighted"
  
  temp2 <- summary(margins(outlist_models_supp[[i]][[9]], at=list(x_white=0:1)))
  temp2$outcome_var <- outcome_vars_name[i]
  temp2$weighted <- "Weighted"
  
  outlist[[i]] <- rbind(temp[1:4,], temp2[1:4,])
  
}

plotdata <- do.call(rbind, outlist)

plotdata$variable <- NA
plotdata$variable[grepl("coworker", plotdata$factor)] <- "Coworker\nTreatment"
plotdata$variable[grepl("neighbor", plotdata$factor)] <- "Neighbor\nTreatment"

plotdata$race <- NA
plotdata$race[plotdata$x_white==1] <- "White"
plotdata$race[plotdata$x_white==0] <- "Non-White"

pd <- position_dodge(.35)

tiff("../output/hte_white_results.tiff", w=1000, h=720)
ggplot(plotdata[plotdata$weighted=="Unweighted",], aes(x=outcome_var, shape=weighted, y=AME)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE),
                position=pd, width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_manual(values=c(19,15,17,18), name="Specification") +
  #scale_shape_manual(values=c(19,15), name="Specification") +
  facet_wrap(variable~race) +
  xlab("") +
  ylab("Treatment Effect") +
  coord_flip() +
  theme_bw()
dev.off()


tiff("../output/hte_white_results_weighted.tiff", w=1000, h=720)
ggplot(plotdata[plotdata$weighted=="Weighted",], aes(x=outcome_var, y=AME)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE),
                position=pd, width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_manual(values=c(19,15,17,18), name="Specification") +
  #scale_shape_manual(values=c(19,15), name="Specification") +
  facet_wrap(variable~race) +
  xlab("") +
  ylab("Treatment Effect") +
  coord_flip() +
  theme_bw()
dev.off()







###Female

#formal test of equal effects by gender

outlist_hte_fem[[7]]

data$treat_any <- NA
data$treat_any[data$vignette_coworkertreat==1|data$vignette_neighbortreat==1] <- 1
data$treat_any[data$vignette_coworkertreat==0 & data$vignette_neighbortreat==0] <- 0

lm_robust(dv_index ~ treat_any*female, data, se_type="HC3")


outlist <- list()

for(i in 1:7){
  
  cat(i, "\n")
  
  temp <- summary(margins(outlist_models_supp[[i]][[2]], at=list(female=0:1)))
  temp$outcome_var <- outcome_vars_name[i]
  temp$weighted <- "Unweighted"
  
  temp2 <- summary(margins(outlist_models_supp[[i]][[10]], at=list(female=0:1)))
  temp2$outcome_var <- outcome_vars_name[i]
  temp2$weighted <- "Weighted"
  
  outlist[[i]] <- rbind(temp[3:6,], temp2[3:6,])
  
}

plotdata <- do.call(rbind, outlist)

plotdata$variable <- NA
plotdata$variable[grepl("coworker", plotdata$factor)] <- "Coworker\nTreatment"
plotdata$variable[grepl("neighbor", plotdata$factor)] <- "Neighbor\nTreatment"

plotdata$race <- NA
plotdata$race[plotdata$female==1] <- "Women"
plotdata$race[plotdata$female==0] <- "Men"

pd <- position_dodge(.35)

tiff("../output/hte_fem_results.tiff", w=1000, h=720)
ggplot(plotdata[plotdata$weighted=="Unweighted",], aes(x=outcome_var, y=AME)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE),
                position=pd, width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_manual(values=c(19,15,17,18), name="Specification") +
  #scale_shape_manual(values=c(19,15), name="Specification") +
  facet_wrap(variable~race) +
  xlab("") +
  ylab("Treatment Effect") +
  coord_flip() +
  theme_bw()
dev.off()


tiff("../output/hte_fem_results_weighted.tiff", w=1000, h=720)
ggplot(plotdata[plotdata$weighted=="Weighted",], aes(x=outcome_var, y=AME)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE),
                position=pd, width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_manual(values=c(19,15,17,18), name="Specification") +
  #scale_shape_manual(values=c(19,15), name="Specification") +
  facet_wrap(variable~race) +
  xlab("") +
  ylab("Treatment Effect") +
  coord_flip() +
  theme_bw()
dev.off()



###RTW

#formal test of RTW interaction
outlist_hte_rtw[[7]]
lm_robust(dv_index ~ treat_any*rtw_live, data, se_type="HC3")


outlist <- list()

for(i in 1:7){
  
  cat(i, "\n")
  
  temp <- summary(margins(outlist_models_supp[[i]][[3]], at=list(rtw_live=0:1)))
  temp$outcome_var <- outcome_vars_name[i]
  temp$weighted <- "Unweighted"
  
  temp2 <- summary(margins(outlist_models_supp[[i]][[11]], at=list(rtw_live=0:1)))
  temp2$outcome_var <- outcome_vars_name[i]
  temp2$weighted <- "Weighted"
  
  outlist[[i]] <- rbind(temp[3:6,], temp2[3:6,])
  
}

plotdata <- do.call(rbind, outlist)

plotdata$variable <- NA
plotdata$variable[grepl("coworker", plotdata$factor)] <- "Coworker\nTreatment"
plotdata$variable[grepl("neighbor", plotdata$factor)] <- "Neighbor\nTreatment"

plotdata$group <- NA
plotdata$group[plotdata$rtw_live==1] <- "RTW State"
plotdata$group[plotdata$rtw_live==0] <- "Non-RTW State"

pd <- position_dodge(.35)

tiff("../output/hte_rtw_results.tiff", w=1000, h=720)
ggplot(plotdata[plotdata$weighted=="Unweighted",], aes(x=outcome_var, y=AME)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE),
                position=pd, width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_manual(values=c(19,15,17,18), name="Specification") +
  #scale_shape_manual(values=c(19,15), name="Specification") +
  facet_wrap(variable~group) +
  xlab("") +
  ylab("Treatment Effect") +
  coord_flip() +
  theme_bw()
dev.off()


tiff("../output/hte_rtw_results_weighted.tiff", w=1000, h=720)
ggplot(plotdata[plotdata$weighted=="Weighted",], aes(x=outcome_var, y=AME)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE),
                position=pd, width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_manual(values=c(19,15,17,18), name="Specification") +
  #scale_shape_manual(values=c(19,15), name="Specification") +
  facet_wrap(variable~group) +
  xlab("") +
  ylab("Treatment Effect") +
  coord_flip() +
  theme_bw()
dev.off()






###Party ID pid3

htmlreg(list(outlist_models_supp[[1]][[6]], outlist_models_supp[[2]][[6]], outlist_models_supp[[3]][[6]], outlist_models_supp[[4]][[6]], 
             outlist_models_supp[[5]][[6]], outlist_models_supp[[6]][[6]], outlist_models_supp[[7]][[6]]), 
        include.ci = FALSE, digits=3, file="../output/hte_pid3_results.doc")


htmlreg(list(outlist_models_supp[[1]][[14]], outlist_models_supp[[2]][[14]], outlist_models_supp[[3]][[14]], outlist_models_supp[[4]][[14]], 
             outlist_models_supp[[5]][[14]], outlist_models_supp[[6]][[14]], outlist_models_supp[[7]][[14]]), 
        include.ci = FALSE, digits=3, file="../output/hte_pid3_results_weighted.doc")




###Racial Conflict

htmlreg(list(outlist_models_supp[[1]][[4]], outlist_models_supp[[2]][[4]], outlist_models_supp[[3]][[4]], outlist_models_supp[[4]][[4]], 
             outlist_models_supp[[5]][[4]], outlist_models_supp[[6]][[4]], outlist_models_supp[[7]][[4]]), 
        include.ci = FALSE, digits=3, file="../output/hte_racial_conflict_results.doc")


htmlreg(list(outlist_models_supp[[1]][[12]], outlist_models_supp[[2]][[12]], outlist_models_supp[[3]][[12]], outlist_models_supp[[4]][[12]], 
             outlist_models_supp[[5]][[12]], outlist_models_supp[[6]][[12]], outlist_models_supp[[7]][[12]]), 
        include.ci = FALSE, digits=3, file="../output/hte_racial_conflict_results_weighted.doc")



###QIR

htmlreg(list(outlist_models_supp[[1]][[5]], outlist_models_supp[[2]][[5]], outlist_models_supp[[3]][[5]], outlist_models_supp[[4]][[5]], 
             outlist_models_supp[[5]][[5]], outlist_models_supp[[6]][[5]], outlist_models_supp[[7]][[5]]), 
        include.ci = FALSE, digits=3, file="../output/hte_qir_results.doc")


htmlreg(list(outlist_models_supp[[1]][[13]], outlist_models_supp[[2]][[13]], outlist_models_supp[[3]][[13]], outlist_models_supp[[4]][[13]], 
             outlist_models_supp[[5]][[13]], outlist_models_supp[[6]][[13]], outlist_models_supp[[7]][[13]]), 
        include.ci = FALSE, digits=3, file="../output/hte_qir_results_weighted.doc")





###Age

htmlreg(list(outlist_models_supp[[1]][[7]], outlist_models_supp[[2]][[7]], outlist_models_supp[[3]][[7]], outlist_models_supp[[4]][[7]], 
             outlist_models_supp[[5]][[7]], outlist_models_supp[[6]][[7]], outlist_models_supp[[7]][[7]]), 
        include.ci = FALSE, digits=3, file="../output/hte_age_results.doc")


htmlreg(list(outlist_models_supp[[1]][[15]], outlist_models_supp[[2]][[15]], outlist_models_supp[[3]][[15]], outlist_models_supp[[4]][[15]], 
             outlist_models_supp[[5]][[15]], outlist_models_supp[[6]][[15]], outlist_models_supp[[7]][[15]]), 
        include.ci = FALSE, digits=3, file="../output/hte_age_results_weighted.doc")




###Wage Theft Index

htmlreg(list(outlist_models_supp[[1]][[8]], outlist_models_supp[[2]][[8]], outlist_models_supp[[3]][[8]], outlist_models_supp[[4]][[8]], 
             outlist_models_supp[[5]][[8]], outlist_models_supp[[6]][[8]], outlist_models_supp[[7]][[8]]), 
        include.ci = FALSE, digits=3, file="../output/hte_wagetheft_results.doc")


htmlreg(list(outlist_models_supp[[1]][[16]], outlist_models_supp[[2]][[16]], outlist_models_supp[[3]][[16]], outlist_models_supp[[4]][[16]], 
             outlist_models_supp[[5]][[16]], outlist_models_supp[[6]][[16]], outlist_models_supp[[7]][[16]]), 
        include.ci = FALSE, digits=3, file="../output/hte_wagetheft_results_weighted.doc")






#########Female controlling for QIR and wage theft index

aggregate(qir_index ~ female, data, mean, na.rm=T)

summary(lm(qir_index ~ female, data))
summary(lm(wage_theft_index ~ female, data))
summary(lm(worker_support_num01 ~ female, data))


outlist <- list()

for(i in 1:7){
  
  cat(i, "\n")
  
  data$temp_dv <- data[,outcome_vars[i]]
  
  temp_lm <- lm_robust(temp_dv ~ vignette_coworkertreat*female + vignette_neighbortreat*female + qir_index + wage_theft_index, data, se_type = "HC3")
  
  temp <- data.frame(summary(margins(temp_lm, at=list(female=0:1))))
  temp$outcome_var <- outcome_vars_name[i]
  temp$weighted <- "Unweighted"
  
  outlist[[i]] <- temp[3:6,]
  
}

plotdata <- do.call(rbind, outlist)

plotdata$variable <- NA
plotdata$variable[grepl("coworker", plotdata$factor)] <- "Coworker\nTreatment"
plotdata$variable[grepl("neighbor", plotdata$factor)] <- "Neighbor\nTreatment"

plotdata$race <- NA
plotdata$race[plotdata$female==1] <- "Women"
plotdata$race[plotdata$female==0] <- "Men"

pd <- position_dodge(.35)

tiff("../output/hte_fem_results_qir_wagetheft.tiff", w=1000, h=720)
ggplot(plotdata[plotdata$weighted=="Unweighted",], aes(x=outcome_var, y=AME)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE),
                position=pd, width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_manual(values=c(19,15,17,18), name="Specification") +
  #scale_shape_manual(values=c(19,15), name="Specification") +
  facet_wrap(variable~race) +
  xlab("") +
  ylab("Treatment Effect") +
  coord_flip() +
  theme_bw()
dev.off()




############HTE caretaking caretakers#############

data$caretaker <- NA
data$caretaker[(data$marital_status_num %in% c(1,2) & data$partner_emp=="No") | 
                 (data$hh_05_num!=1|data$hh_618_num!=1)] <- 1
data$caretaker[(data$marital_status_num %in% c(1,2) & data$partner_emp=="No")==F & 
                 data$hh_05_num==1 & data$hh_618_num==1] <- 0

cor(data$caretaker, data$female, use="pairwise.complete.obs")

outlist <- list()


for(i in 1:7){
  
  cat(i, "\n")
  
  data$temp_dv <- data[,outcome_vars[i]]
  
  temp_lm <- lm_robust(temp_dv ~ vignette_coworkertreat*caretaker + vignette_neighbortreat*caretaker, data, se_type = "HC3")
  
  temp <- data.frame(summary(margins(temp_lm, at=list(caretaker=0:1))))
  temp$outcome_var <- outcome_vars_name[i]
  temp$weighted <- "Unweighted"
  
  outlist[[i]] <- temp[3:6,]
  
}

plotdata <- do.call(rbind, outlist)

plotdata$variable <- NA
plotdata$variable[grepl("coworker", plotdata$factor)] <- "Coworker\nTreatment"
plotdata$variable[grepl("neighbor", plotdata$factor)] <- "Neighbor\nTreatment"

plotdata$race <- NA
plotdata$race[plotdata$caretaker==1] <- "Caretaker"
plotdata$race[plotdata$caretaker==0] <- "Not Caretaker"

pd <- position_dodge(.35)

tiff("../output/hte_results_caretaking.tiff", w=1000, h=720)
ggplot(plotdata[plotdata$weighted=="Unweighted",], aes(x=outcome_var, y=AME)) +
  geom_point(size=3, position=pd) +
  geom_errorbar(aes(ymin=AME-1.96*SE, ymax=AME+1.96*SE),
                position=pd, width=0) +
  geom_hline(yintercept=0, linetype=2) +
  scale_shape_manual(values=c(19,15,17,18), name="Specification") +
  #scale_shape_manual(values=c(19,15), name="Specification") +
  facet_wrap(variable~race) +
  xlab("") +
  ylab("Treatment Effect") +
  coord_flip() +
  theme_bw()
dev.off()



############HTE who do you turn to#############



write.csv(data.frame(cor(data[,c("female", names(data[grepl("problem_help_", names(data))]))],
            use="pairwise.complete.obs"))[1], "../output/female_problem_help_corrs.csv")














####################Robustness Checks on Main Results



########Ordered Logit

for(i in outcome_vars){
  
  data[,i] <- factor(data[,i])
  
}

outlist_main <- list()
outlist_models_main <- list()
outlist_models_supp <- list()

for(i in outcome_vars){
  dv <- i
  #formula objects; no need to alter; change specifications above or, if new models, add.
  f_base <-  as.formula( 
    paste(dv,
          paste(spec_bv, collapse=" + "),
          sep=" ~ "))
  f_cov <- as.formula(
    paste(dv,
          paste(spec_cov, collapse=" + "),
          sep=" ~ "))
  
  
  # ordered logit
  
  m_base <- polr(f_base, data, Hess = T)
  m_cov <- polr(f_cov, data, Hess = T)
  
  m_base_wtd <- polr(f_base, data, weights=rk_wgt_final, Hess = T)
  m_cov_wtd <- polr(f_cov, data, weights=rk_wgt_final, Hess = T)
  
  
  outlist_models_main[[i]] <- list(m_base, m_cov, m_base_wtd, m_cov_wtd)
  
  m_base <- data.frame(tidy(m_base))[2:3,]
  m_cov <- data.frame(tidy(m_cov))[2:3,]
  m_base_wtd <- data.frame(tidy(m_base_wtd))[2:3,]
  m_cov_wtd <- data.frame(tidy(m_cov_wtd))[2:3,]
  
  m_base$weighted <- "Unweighted"
  m_cov$weighted <- "Unweighted"
  
  m_base_wtd$weighted <- "Weighted"
  m_cov_wtd$weighted <- "Weighted"
  
  m_base$covariates <- "Bivariate"
  m_cov$covariates <- "Covariates"
  m_base_wtd$covariates <- "Bivariate"
  m_cov_wtd$covariates <- "Covariates"
  
  outlist_main[[i]] <- rbind(m_base, m_cov, m_base_wtd, m_cov_wtd)
  
} 

#output main covariates models to .doc
htmlreg(list(outlist_models_main[[1]][[2]], outlist_models_main[[2]][[2]], outlist_models_main[[3]][[2]], 
             outlist_models_main[[4]][[2]], outlist_models_main[[5]][[2]], outlist_models_main[[6]][[2]]), 
        include.ci = FALSE, digits=3, file="../output/main_results_ologit_unweighted_cov.doc")

htmlreg(list(outlist_models_main[[1]][[1]], outlist_models_main[[2]][[1]], outlist_models_main[[3]][[1]], 
             outlist_models_main[[4]][[1]], outlist_models_main[[5]][[1]], outlist_models_main[[6]][[1]]), 
        include.ci = FALSE, digits=3, file="../output/main_results_ologit_unweighted_bv.doc")

htmlreg(list(outlist_models_main[[1]][[4]], outlist_models_main[[2]][[4]], outlist_models_main[[3]][[4]], 
             outlist_models_main[[4]][[4]], outlist_models_main[[5]][[4]], outlist_models_main[[6]][[4]]), 
        include.ci = FALSE, digits=3, file="../output/main_results_ologit_weighted_cov.doc")

htmlreg(list(outlist_models_main[[1]][[3]], outlist_models_main[[2]][[3]], outlist_models_main[[3]][[3]], 
             outlist_models_main[[4]][[3]], outlist_models_main[[5]][[3]], outlist_models_main[[6]][[3]]), 
        include.ci = FALSE, digits=3, file="../output/main_results_ologit_weighted_bv.doc")





