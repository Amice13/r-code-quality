# Install required packages 
# install.packages("remotes")
# library(remotes)
# install_version("reshape2", version = "1.4.4");install_version("dplyr", version = "1.1.4")
# install_version("haven", version = "2.5.4");install_version("stargazer", version = "5.2.3")
# install_version("AER", version = "1.2-12");install_version("sandwich", version = "3.1-0")
# install_version("parallel", version = "4.3.3");install_version("randomizr", version = "1.0.0")
# install_version("ggplot2", version = "3.4.4");install_version("xtable", version = "1.8-4")
# install_version("lfe", version = "2.9-0");install_version("estimatr", version = "1.0.2")
# install_version("texreg", version = "1.39.3");install_version("car", version = "3.1-2")
# install_version("sjmisc", version = "2.8.9");install_version("MASS", version = "7.3-60.0.1")
# install_version("doSNOW", version = "1.0.20");install_version("doRNG", version = "1.8.6")
# install_version("data.table", version = "1.15.0"); install_version("tibble", version = "3.2.1")


# Load required packages
library(reshape2)
library(dplyr)
library(haven)
library(stargazer)
library(AER)
library(sandwich)
library(tibble)
library(parallel)
library(randomizr)
library(ggplot2)
library(xtable)
library(lfe)
library(estimatr)
library(texreg)
library(car)
library(sjmisc)
library(MASS)
library(doSNOW)
library(doRNG)
library(data.table)
library(tidyr)



# List all objects in the environment
all_objects <- ls()

# Specify the objects to keep
objects_to_keep <- c("lib", 'wd')

# Identify objects to remove
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)

##### Set working directory ########
setwd(wd)




#### Set the location of libraries for the parallel_ivclust_function #####



###### PREPARE FUNCTIONS FOR MAKING TABLES AND ESTIMATION #####


count_nonna = function(x){
  count = sum(!is.na(x))
  return(count)
}

mean_nona = function(x){
  count = mean(x, na.rm = T)
  return(count)
}

sd_nona = function(x){
  count = sd(x, na.rm = T)
  return(count)
}

get_coef = function(lm, names){
  coef = lm$coefficients[names(lm$coefficients) %in% names]
  return(coef)
}

get_se_mean = function(se, names){
  se = apply(se[rownames(se) %in% names,], MARGIN = 1, FUN = mean)
  
  return(se)
}

get_p_values = function(se, lm, names){
  coefs = get_coef(lm, names)
  se = get_se_mean(se, names)
  
  t = coefs/se
  
  df = lm$df.residual
  
  p_val = 2*pt(-abs(t),df=df)
  p_val = p_val %>% round(3)
  return(p_val)
}

parallel_ivclust_func = function(form, id_vp, treat_tally, dep.var.str, data, cl.no, fem=0){
  cl <- makeCluster(cl.no, type = "FORK")
  clusterEvalQ(cl, {
    library(estimatr, lib.loc = lib)
    library(randomizr, lib.loc = lib)
    
  })
  treat_tally <- treat_tally
  id_vp <- id_vp
  dep.var.str <- dep.var.str
  form <- form
  my.data <- data
  fem <- fem
  clusterExport(cl, varlist = c("form", "my.data", "id_vp", "treat_tally", "dep.var.str", "fem"), envir = environment())
  if (fem==0){
    
    
    se1 <- parSapply(cl, X = 1:5000,
                     FUN = function(x){
                       cluster = unlist(sapply(treat_tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat_tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat_tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat_tally$treatment_assigned[x]])}))
                       se = iv_robust(as.formula(paste0(dep.var.str, "~", form)), 
                                      fixed_effects = ~ assembly_id, 
                                      clusters = cluster,
                                      data = my.data, se_type = "stata")$std.error
                       #class(x)
                     }) 
  }else {
    
    se1 <- parSapply(cl, X = 1:5000, 
                     FUN = function(x){
                       se = iv_robust(as.formula(paste0(dep.var.str, "~", form)), 
                                      clusters = unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = treat_tally$items[x], prob_each = id_vp$prob[id_vp$fem_treat_assigned==treat_tally$fem_treat_assigned[x]], conditions = id_vp$assembly_id[id_vp$fem_treat_assigned==treat_tally$fem_treat_assigned[x]])})), 
                                      data = my.data, se_type = "stata")$std.error
                       #class(x)
                     }) 
  }
  stopCluster(cl)
  return(se1)
}



###### PREPARE MAIN ANALYSIS DATASET #####
dta = read.csv("aap_pamphlets_replication.csv", stringsAsFactors = F)

# Create dataset with status quo recruitment
control = dta %>% filter(treatment_assigned==5)

# create status quo dataset for female treatment
control.fem = dta %>% filter(fem_treat_assigned==0)
control.messages = dta %>% filter(t_base == 1)

# Create output folders
dir.create("output")
dir.create("output/tables")
dir.create("output/figures")
tab.out = "output/tables/"
fig.out = "output/figures/"


####### CREATE MATRICES FOR BOOTSTRAPPING #######

# First generate probability of assignment for each VP cluster
id_vp = dta %>% mutate(check = 1) %>%
  group_by(treatment_assigned, id_vp2) %>%
  summarise_at(vars(check), funs(count_nonna)) %>%
  rename(items = check) %>% group_by(treatment_assigned) %>%
  mutate(total = sum(items), prob = items/total)


# Generate probably of assignment to each assembly
id_assembly = dta %>%
  mutate(check = 1) %>%
  group_by(fem_treat_assigned, assembly_id) %>%
  summarise_at(vars(check), funs(count_nonna)) %>%
  rename(items = check) %>% group_by(fem_treat_assigned) %>%
  mutate(total = sum(items), prob = items/total)

# Tally number of pamphlets and VPs per treatment
treat.tally = id_vp %>% group_by(treatment_assigned) %>%
  tally() %>% left_join(id_vp %>% group_by(treatment_assigned) %>%
                          summarize_at(vars(items), funs(sum)))

# Tally number of pamphlets and assemblies per treatment arm for
# female treatment
fem.treat.tally = id_assembly %>% group_by(fem_treat_assigned) %>%
  tally() %>% left_join(id_assembly %>% group_by(fem_treat_assigned) %>%
                          summarize_at(vars(items), funs(sum)))

####### CREATE CORES FOR STANDARD ERROR FUNCTION #######
cl = detectCores()
cluster = snow::makeCluster(cl)
registerDoSNOW(cluster)
n = 5000


seed = 20191101


######## TABLE 2:  SUMMARY STATISTICS ######
control.stat = control %>%
  ungroup() %>%
  summarize_at(vars(missedcall,consented_21, excluded_group,
                    female, excluded_caste,sc_caste, st_group,
                    obc_caste,excluded_religion,
                    muslim, christian, other_relig, hindu,
                    skill_index,
                    any_employ, educ_hi,
                    prior_vote, prior_volunteer), funs(mean_nona)) %>%
  reshape2::melt() %>%
  rename(mean = value) %>%
  bind_cols(control %>% ungroup() %>%
              summarize_at(vars(missedcall,consented_21, excluded_group,
                                female, excluded_caste,sc_caste, st_group,
                                obc_caste,excluded_religion,
                                muslim, christian, other_relig, hindu,
                                skill_index,
                                any_employ, educ_hi,
                                prior_vote, prior_volunteer), funs(sd_nona)) %>%
              reshape2::melt() %>% rename(sd = value) %>% dplyr::select(-variable)) %>%
  bind_cols(control %>% ungroup() %>%
              summarize_at(vars(missedcall,consented_21, excluded_group,
                                female, excluded_caste,sc_caste, st_group,
                                obc_caste,excluded_religion,
                                muslim, christian, other_relig, hindu,
                                skill_index,
                                any_employ, educ_hi,
                                prior_vote, prior_volunteer), funs(count_nonna)) %>%
              reshape2::melt() %>% rename(n = value) %>% dplyr::select(-variable)) %>%
  mutate(se = sd/sqrt(n), cilo = mean-1.96*se, cihi = mean+1.96*se,
         variable = c("New members (Onboarding Survey)", "New members (Long-term Retention Survey)",
                      "Excluded group", "Female",
                      "Excluded caste/tribe", "SC", "ST", "OBC",
                      "Excluded religion", "Muslim", "Christian", "Other", "Hindu",
                      "Skilled Member",
                      "Any employment", "High education",
                      "Prior vote", "Prior volunteer"))

control.out = control.stat %>% dplyr::select(variable, mean, sd, n)
control.out[,2:4] = round(control.out[,2:4], 3)

stargazer(control.out, summary = FALSE, out = paste0(tab.out, "table2_baseline_summary.tex"),
          title = "Summary Statistics on the Group Recruited by Baseline Pamphlet", 
          notes = "Note: The table shows, for each variable, the mean value, standard deviation, and number of non-missing observations in the status-quo recruitment group along the dimensions of Size, Type, and Skill. Each variable has been multiplied by 1,000 so that each reflects the number of individuals recruited per 1,000 pamphlets. Italics denote an index.")


######## TABLE E.2: Balance Table for Treatment Groups ######
# PANEL A
# Aggregate data to the assembly level
assembly = dta %>%
  group_by(assembly_id, fem_treat_assigned) %>%
  mutate(count = 1) %>%
  summarise_at(vars(count), .funs = sum, na.rm = T) %>%
  mutate(treat_fem = ifelse(fem_treat_assigned==1, 1, 0)) %>%
  mutate(treat_male = ifelse(fem_treat_assigned==0, 1, 0))



# Male - female
mod.ols =  lm(count ~ treat_male + treat_fem-1, data = assembly)
se.ols = vcovHC(mod.ols, type = "HC1") %>% diag %>% sqrt()
rmod.ols = lm(count ~  I(treat_male + treat_fem)-1, data = assembly)

jointp.ols = anova(mod.ols, rmod.ols)$`Pr(>F)`[2]


# All messages - Logged(Count)
mod.lols =  lm(log(count) ~ treat_male + treat_fem-1, data = assembly)
se.lols = vcovHC(mod.lols, type = "HC1") %>% diag %>% sqrt()
rmod.lols = lm(log(count) ~ I(treat_male + treat_fem)-1, data = assembly)

jointp.lols = anova(mod.lols, rmod.lols)$`Pr(>F)`[2]



# All messages - Neg.Binom
mod.nb = glm.nb(count ~ treat_male + treat_fem-1, data = assembly)
se.nb= sqrt(diag(vcov(mod.nb)))
rmod.nb = glm.nb(count ~ I(treat_male + treat_fem)-1, data = assembly)

jointp.nb = anova(mod.nb, rmod.nb)$`Pr(Chi)`[2]

joint_pvalues = c(jointp.ols, jointp.lols, jointp.nb)




texreg(l = list(mod.ols, mod.lols, mod.nb),
       caption = "Balance Table for Treatment Groups (Panel A)",
       override.se = list(se.ols, se.lols, se.nb),
       caption.above = T, digits = 3, naive = T, include.ci = F,
       booktabs = T, stars = numeric(0),
       include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE,
       include.rmse = F, include.nclusts = F,
       include.aic = F,
       include.bic = F, include.loglik = F,
       include.deviance = F,
       omit.coef = "as.factor", custom.model.names = c("Count", "Log(Count)", "Count"),
       custom.coef.names = c("Male", "Female"),
       custom.header = list("OLS" = 1:2, "Neg.Binom" = 3),
       custom.gof.rows = list("Joint orthogonality p-value" = joint_pvalues),
       file = paste0(tab.out, "tableE2_panelA_count_balance_female.tex"))





# PANEL B
# Aggregate the data to the VP level
vp = dta %>%
  group_by(assembly_id, id_vp2, treatment_assigned) %>%
  mutate(count = 1) %>%
  summarise_at(vars(count), .funs = sum, na.rm = T) %>%
  group_by(assembly_id) %>%
  mutate(num_treat = length(unique(treatment_assigned))) %>%
  mutate(message_treat = case_when(
    treatment_assigned == 1 | treatment_assigned == 6 ~ "Candidacy",
    treatment_assigned == 2 | treatment_assigned == 7 ~ "Career",
    treatment_assigned == 3 | treatment_assigned == 8 ~ "Ideology",
    treatment_assigned == 4 | treatment_assigned == 9 ~ "Policy",
    treatment_assigned == 5 | treatment_assigned == 10 ~ "Baseline"))

vp = vp %>%
  bind_cols(to_dummy(vp$message_treat, var.name = "treat")) %>%
  rename(treat_pol = treat_5, treat_car = treat_3,
         treat_base = treat_1, treat_ideo = treat_4,
         treat_cand = treat_2)

# All messages - OLS
mod.ols =  lm(count ~ treat_base + treat_ideo + treat_cand + treat_pol + treat_car +
                as.factor(assembly_id)-1, data = vp)
se.ols = vcovHC(mod.ols, type = "HC1") %>% diag %>% sqrt()
rmod.ols = lm(count ~ I(treat_base + treat_ideo + treat_cand + treat_pol + treat_car) +
                as.factor(assembly_id)-1, data = vp)

jointp.ols = anova(mod.ols, rmod.ols)$`Pr(>F)`[2]
base_pol.ols = linearHypothesis(mod.ols, c("treat_base-treat_pol"), singular.ok = T)$`Pr(>F)`[2]
base_cand.ols = linearHypothesis(mod.ols, c("treat_base-treat_cand"), singular.ok = T)$`Pr(>F)`[2]
base_car.ols = linearHypothesis(mod.ols, c("treat_base-treat_car"), singular.ok = T)$`Pr(>F)`[2]
base_ideo.ols = linearHypothesis(mod.ols, c("treat_base-treat_ideo"), singular.ok = T)$`Pr(>F)`[2]


# All messages - Logged(Count)
mod.lols =  lm(log(count) ~ treat_base + treat_ideo + treat_cand + treat_pol + treat_car +
                 as.factor(assembly_id)-1, data = vp)
se.lols = vcovHC(mod.lols, type = "HC1") %>% diag %>% sqrt()
rmod.lols = lm(log(count) ~ I(treat_base + treat_ideo + treat_cand + treat_pol + treat_car) +
                 as.factor(assembly_id)-1, data = vp)

jointp.lols = anova(mod.lols, rmod.lols)$`Pr(>F)`[2]
base_pol.lols = linearHypothesis(mod.lols, c("treat_base-treat_pol"), singular.ok = T)$`Pr(>F)`[2]
base_cand.lols = linearHypothesis(mod.lols, c("treat_base-treat_cand"), singular.ok = T)$`Pr(>F)`[2]
base_car.lols = linearHypothesis(mod.lols, c("treat_base-treat_car"), singular.ok = T)$`Pr(>F)`[2]
base_ideo.lols = linearHypothesis(mod.lols, c("treat_base-treat_ideo"), singular.ok = T)$`Pr(>F)`[2]



# All messages - Neg.Binom
mod.nb = glm.nb(count ~ treat_base + treat_ideo + treat_cand + treat_pol + treat_car +
                  as.factor(assembly_id)-1, data = vp)
se.nb= sqrt(diag(vcov(mod.nb)))
rmod.nb = glm.nb(count ~ I(treat_base + treat_ideo + treat_cand + treat_pol + treat_car) +
                   as.factor(assembly_id)-1, data = vp)
anova.nb = anova(mod.nb, rmod.nb)

jointp.nb = anova(mod.nb, rmod.nb)$`Pr(Chi)`[2]
base_pol.nb = linearHypothesis(mod.nb, c("treat_base-treat_pol"), singular.ok = T)$`Pr(>Chisq)`[2]
base_cand.nb = linearHypothesis(mod.nb, c("treat_base-treat_cand"), singular.ok = T)$`Pr(>Chisq)`[2]
base_car.nb = linearHypothesis(mod.nb, c("treat_base-treat_car"), singular.ok = T)$`Pr(>Chisq)`[2]
base_ideo.nb = linearHypothesis(mod.nb, c("treat_base-treat_ideo"), singular.ok = T)$`Pr(>Chisq)`[2]

joint_pvalues = c(jointp.ols, jointp.lols, jointp.nb)
base_pol = c(base_pol.ols, base_pol.lols, base_pol.nb)
base_cand = c(base_cand.ols, base_cand.lols, base_cand.nb)
base_car = c(base_car.ols, base_car.lols, base_car.nb)
base_ideo = c(base_ideo.ols, base_ideo.lols, base_ideo.nb)


texreg(l = list(mod.ols, mod.lols, mod.nb),
       override.se = list(se.ols, se.lols, se.nb),
       caption = "Balance Table for Recruitment Messages",
       caption.above = T, digits = 3, naive = T, include.ci = F,
       booktabs = T, stars = numeric(0),
       include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE,
       include.rmse = F, include.nclusts = F,
       include.aic = F,
       include.bic = F, include.loglik = F,
       include.deviance = F,
       omit.coef = "as.factor", custom.model.names = c("Count", "Log(Count)", "Count"),
       custom.coef.names = c("Baseline", "Ideology", "Candidacy", "Policy", "Career"),
       custom.header = list("OLS" = 1:2, "Neg.Binom" = 3),
       custom.gof.rows = list("Joint orthogonality p-value" = joint_pvalues,
                              "Base - Ideology = 0" = base_ideo,
                              "Base - Candidacy = 0" = base_cand,
                              "Base - Policy = 0" = base_pol,
                              "Base - Career = 0" = base_car),
       file = paste0(tab.out, "tableE2_panelB_count_balance_messages.tex"))


####### TABLE 4: Do Party Leader Efforts Affect the Rank and File? #######



## COLUMNS 1-3: Onboarding survey
## Missed call outcome
means = iv_robust(missedcall ~ t_base_male_takeup + t_ideo_male_takeup +
                    t_cand_male_takeup + t_poli_male_takeup +  
                    t_care_male_takeup +  t_base_female_takeup +
                    t_ideo_female_takeup + t_cand_female_takeup +
                    t_poli_female_takeup + t_care_female_takeup -1 |
                    t_base_male + t_ideo_male + t_cand_male + t_poli_male +
                    t_care_male + t_base_female + t_ideo_female + t_cand_female +
                    t_poli_female + t_care_female -1, clusters = id_vp2,
                  data = dta, se_type = "stata")

m1.ur = felm(missedcall ~ 1 | 0 |
               (t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup ~ t_ideo_male+t_cand_male+t_poli_male+t_care_male+t_base_female+t_ideo_female+t_poli_female+t_cand_female+t_care_female)| id_vp2, data = dta)
joint.f.stat1 = lfe::waldtest(m1.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[5] %>% round(3)
joint.p1 = lfe::waldtest(m1.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[4] %>% round(3)

## Excluded groups
means_excluded = iv_robust(excluded_group ~ t_base_male_takeup + t_ideo_male_takeup +
                             t_cand_male_takeup + t_poli_male_takeup +  
                             t_care_male_takeup +  t_base_female_takeup +
                             t_ideo_female_takeup + t_cand_female_takeup +
                             t_poli_female_takeup + t_care_female_takeup -1 |
                             t_base_male + t_ideo_male + t_cand_male + t_poli_male +
                             t_care_male + t_base_female + t_ideo_female + t_cand_female +
                             t_poli_female + t_care_female -1, clusters = id_vp2,
                           data = dta, se_type = "stata")

m2.ur = felm(excluded_group ~ 1 | 0 |
               (t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup ~ t_ideo_male+t_cand_male+t_poli_male+t_care_male+t_base_female+t_ideo_female+t_poli_female+t_cand_female+t_care_female)| id_vp2, data = dta)
joint.f.stat2 = lfe::waldtest(m2.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[5] %>% round(3)
joint.p2 = lfe::waldtest(m2.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[4] %>% round(3)

## Skill index
means_skill = iv_robust(skill_index ~ t_base_male_takeup + t_ideo_male_takeup +
                          t_cand_male_takeup + t_poli_male_takeup +  
                          t_care_male_takeup +  t_base_female_takeup +
                          t_ideo_female_takeup + t_cand_female_takeup +
                          t_poli_female_takeup + t_care_female_takeup -1 |
                          t_base_male + t_ideo_male + t_cand_male + t_poli_male +
                          t_care_male + t_base_female + t_ideo_female + t_cand_female +
                          t_poli_female + t_care_female -1, clusters = id_vp2,
                        data = dta, se_type = "stata")

m3.ur = felm(skill_index ~ 1 | 0 |
               (t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup ~ t_ideo_male+t_cand_male+t_poli_male+t_care_male+t_base_female+t_ideo_female+t_poli_female+t_cand_female+t_care_female)| id_vp2, data = dta)
joint.f.stat3 = lfe::waldtest(m3.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[5] %>% round(3)
joint.p3 = lfe::waldtest(m3.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[4] %>% round(3)



## COLUMNS 4-6: Recall survey
## Consentend to 21 survey
means_consented_21 = iv_robust(consented_21 ~ t_base_male_takeup + t_ideo_male_takeup +
                                 t_cand_male_takeup + t_poli_male_takeup +  
                                 t_care_male_takeup +  t_base_female_takeup +
                                 t_ideo_female_takeup + t_cand_female_takeup +
                                 t_poli_female_takeup + t_care_female_takeup -1 |
                                 t_base_male + t_ideo_male + t_cand_male + t_poli_male +
                                 t_care_male + t_base_female + t_ideo_female + t_cand_female +
                                 t_poli_female + t_care_female -1, clusters = id_vp2,
                               data = dta, se_type = "stata")

m1.ur_21 = felm(consented_21 ~ 1 | 0 |
                  (t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup ~ t_ideo_male+t_cand_male+t_poli_male+t_care_male+t_base_female+t_ideo_female+t_poli_female+t_cand_female+t_care_female)| id_vp2, data = dta)
joint.f.stat1_21 = lfe::waldtest(m1.ur_21, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[5] %>% round(3)
joint.p1_21 = lfe::waldtest(m1.ur_21, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[4] %>% round(3)

## Excluded group in 21
means_excluded_21 = iv_robust(excluded_group_21 ~ t_base_male_takeup + t_ideo_male_takeup +
                                t_cand_male_takeup + t_poli_male_takeup +  
                                t_care_male_takeup +  t_base_female_takeup +
                                t_ideo_female_takeup + t_cand_female_takeup +
                                t_poli_female_takeup + t_care_female_takeup -1 |
                                t_base_male + t_ideo_male + t_cand_male + t_poli_male +
                                t_care_male + t_base_female + t_ideo_female + t_cand_female +
                                t_poli_female + t_care_female -1, clusters = id_vp2,
                              data = dta, se_type = "stata")



m2.ur_21 = felm(excluded_group_21 ~ 1 | 0 |
                  (t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup ~ t_ideo_male+t_cand_male+t_poli_male+t_care_male+t_base_female+t_ideo_female+t_poli_female+t_cand_female+t_care_female)| id_vp2, data = dta)
joint.f.stat2_21 = lfe::waldtest(m2.ur_21, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[5] %>% round(3)
joint.p2_21 = lfe::waldtest(m2.ur_21, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[4] %>% round(3)

## Skill index in 21
means_skill_21 = iv_robust(skill_index_21 ~ t_base_male_takeup + t_ideo_male_takeup +
                             t_cand_male_takeup + t_poli_male_takeup +  
                             t_care_male_takeup +  t_base_female_takeup +
                             t_ideo_female_takeup + t_cand_female_takeup +
                             t_poli_female_takeup + t_care_female_takeup -1 |
                             t_base_male + t_ideo_male + t_cand_male + t_poli_male +
                             t_care_male + t_base_female + t_ideo_female + t_cand_female +
                             t_poli_female + t_care_female -1, clusters = id_vp2,
                           data = dta, se_type = "stata")

m3.ur_21 = felm(skill_index_21 ~ 1 | 0 |
                  (t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup ~ t_ideo_male+t_cand_male+t_poli_male+t_care_male+t_base_female+t_ideo_female+t_poli_female+t_cand_female+t_care_female)| id_vp2, data = dta)
joint.f.stat3_21 = lfe::waldtest(m3.ur_21, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[5] %>% round(3)
joint.p3_21 = lfe::waldtest(m3.ur_21, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[4] %>% round(3)


bind_cols(treatments = c("Baseline (Male)", "Policy (Male)", "Candidacy (Male)", "Career (Male)", "Ideology (Male)",
                         "Baseline (Female)", "Policy (Female)", "Candidacy (Female)", "Career (Female)", "Ideology (Female)",
                         "Can leaders manipulate rank-and-file hypothesis test", "Joint F statistic", "Joint p-value", "Observations"),
          missedcall= c(round(tidy(means)$estimate, 3), NA_integer_, joint.f.stat1, joint.p1, means$nobs),
          excluded_group = c(round(tidy(means_excluded)$estimate, 3), NA_integer_, joint.f.stat2, joint.p2, means_excluded$nobs),
          skill_index = c(round(tidy(means_skill)$estimate, 3), NA_integer_, joint.f.stat3, joint.p3, means_skill$nobs),
          consented_21= c(round(tidy(means_consented_21)$estimate, 3), NA_integer_, joint.f.stat1_21, joint.p1_21, means_consented_21$nobs),
          excluded_group_21 = c(round(tidy(means_excluded_21)$estimate, 3), NA_integer_, joint.f.stat2_21, joint.p2_21, means_excluded_21$nobs),
          skilled_21 = c(round(tidy(means_skill_21)$estimate, 3), NA_integer_, joint.f.stat3_21, joint.p3_21, means_skill_21$nobs)) %>%
  rename(`New Members` = missedcall, `Excluded Group Members` = excluded_group, `Skilled Members` = skill_index, 
         `New Members (Retention)` = consented_21, `Excluded Group Members (Retention)` = excluded_group_21, 
         `Skilled Members (Retention)` = skilled_21) %>%
  xtable(digits = 3) %>% print(file = paste0(tab.out, "table4_joint_hypothesis.tex"),
                               booktabs = T, include.rownames = F)






####### TABLE D.1: Sensitivity Analysis for Inference #######

# Modifying main probs for ACs - new range 100 200 400 800
id_assembly <- as.data.table(id_assembly)

# Code to identify rows with max prob and perform initial modifications
result_ac_max_prob <- id_assembly[id_assembly[, .I[prob == max(prob)], by = fem_treat_assigned]$V1]
result_ac_max_prob <- result_ac_max_prob[, .(fem_treat_assigned, assembly_id, prob)]
setnames(result_ac_max_prob, c("assembly_id", 'prob'), c("assembly_id_max_prob", "max_prob"))

# Merging to add the max_prob to the original data.table
new_id_assembly <- merge(id_assembly, result_ac_max_prob, by = "fem_treat_assigned")
new_id_assembly$rest_prob <- 1 - new_id_assembly$max_prob
new_id_assembly$prob_var <- -new_id_assembly$prob / new_id_assembly$rest_prob
new_id_assembly[assembly_id == assembly_id_max_prob, prob_var := 1]

# Loop to create pro100, prob200, prob400, prob800 columns
for (i in c(100, 200, 400, 800 )) {
  percentage <- i / 100  # Convert percentage to a decimal
  new_column_name <- paste0("prob", i)  # Construct the new column name
  # Adjust prob values by the specified percentage and create the new column
  new_id_assembly[[new_column_name]] <- new_id_assembly$prob + 
    ( new_id_assembly$prob_var * 
        percentage * 
        new_id_assembly$max_prob )
}


# Calculate the count of each assembly_id within each fem_treat_assigned group
new_id_assembly[, count := .N, by = .(fem_treat_assigned, assembly_id)]

# Calculate the total count of fem_treat_assigned
new_id_assembly[, total := .N, by = fem_treat_assigned]

# Calculate the probability as the count of each assembly_id over the total in each fem_treat_assigned
# and add it as a new column named prob_equal
new_id_assembly[, prob_equal := count / total]
new_id_assembly[, c("count", "total") := NULL]


# for 8X we have negative probs, but also a prob greater than 1
new_id_assembly$prob800 <- ifelse(new_id_assembly$prob800 < 0, 0, new_id_assembly$prob800)
new_id_assembly$prob800 <- ifelse(new_id_assembly$prob800 > 1, 1, new_id_assembly$prob800)

# checking specification
# Specify the new prob columns of interest
new_prob_cols <- c( "prob100", "prob200", "prob400", 'prob800', 'prob_equal' )

# Calculate the sum of the new prob columns by fem_treat_assigned
sums_by_fem_treat_assigned <- new_id_assembly[, lapply(.SD, sum), by = fem_treat_assigned, .SDcols = new_prob_cols]

# Check the result
print(sums_by_fem_treat_assigned)

# Save this dataset
new_id_assembly <- as.data.table(new_id_assembly)


# Modifying main probs for VPs  100, 200, 400, 800 
id_vp <- as.data.table(id_vp)

# Code to identify rows with max prob and perform initial modifications
result_vp_max_prob <- id_vp[id_vp[, .I[prob == max(prob)], by = treatment_assigned]$V1]
result_vp_max_prob <- result_vp_max_prob[, .(treatment_assigned, id_vp2, prob)]
setnames(result_vp_max_prob, c("id_vp2", 'prob'), c("id_vp2_max_prob", "max_prob"))

# Step 1: Randomly select one id_vp2_max_prob per treatment_assigned group
# Set seed for reproducibility
# Perform the random selection 5000 for the next step
bootstraps <- lapply(1:5000, function(boot_num) {
  # Randomly select one row per treatment_assigned group
  selected <- result_vp_max_prob[, .SD[sample(.N, 1)], by = treatment_assigned]
  
  # Add the boot_num column to indicate the iteration
  selected[, boot_num := boot_num]
  
  return(selected)
})

# Step 2: Replicate each selection 5000 times and add the boot_num column
bootstrapped <- rbindlist(bootstraps)


# Merging to add the max_prob to the original data.table
new_id_vp <- merge(id_vp, bootstrapped, by = "treatment_assigned", allow.cartesian=TRUE)
setorder(new_id_vp, boot_num, treatment_assigned)


new_id_vp$rest_prob <- 1 - new_id_vp$max_prob
new_id_vp$prob_var <- -new_id_vp$prob / new_id_vp$rest_prob
new_id_vp[id_vp2 == id_vp2_max_prob, prob_var := 1]


# Loop to create prob100, prob200, ..., prob800 columns
for (i in c(100, 200, 400, 800 ) ) {
  percentage <- i / 100  # Convert percentage to a decimal
  new_column_name <- paste0("prob", i)  # Construct the new column name
  # Adjust prob values by the specified percentage and create the new column
  new_id_vp[[new_column_name]] <- new_id_vp$prob + (new_id_vp$prob_var * percentage * new_id_vp$max_prob)
}

# Calculate the count of each id_vp2 within each treatment_assigned group
new_id_vp[, count := .N, by = .(treatment_assigned, id_vp2)]

# Calculate the total count of treatment_assigned
new_id_vp[, total := .N, by = treatment_assigned]

# Calculate the probability as the count of each id_vp2 over the total in each treatment_assigned
# and add it as a new column named prob_equal
new_id_vp[, prob_equal := count / total]
new_id_vp[, c("count", "total") := NULL]


# checking specification
# Specify the new prob columns of interest
new_prob_cols <- c( "prob100", "prob200", "prob400", 'prob800', 'prob_equal' )

# Calculate the sum of the new prob columns by treatment_assigned
sums_by_treatment_assigned <- new_id_vp[, lapply(.SD, sum), 
                                        by = .(treatment_assigned, boot_num), .SDcols = new_prob_cols]

sums_by_treatment_assigned
# Save this dataset
new_id_vp <- as.data.table(new_id_vp)



# Estimation for female
f.m1 = iv_robust(missedcall ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")

# Function for standard errors with simulated clusters
N = 5000
f.se.ori = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", "data.table")) %dorng%
  {
    set.seed(i)
    
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # selecting the number of arms
    new_id_assembly2 <- as.data.table(new_id_assembly)
    n_arm <- sample(1:2, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:2
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_assembly2[fem_treat_assigned %nin% sel.arms, 
                       (columns_to_modify) := lapply(.SD, function(x) prob), 
                       .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_assembly2 <- as.data.table(new_id_assembly2)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_assembly2[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(1:2, 
                            FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x],
                                                                     prob_each = new_id_assembly2$prob[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]],
                                                                     conditions = new_id_assembly2$assembly_id[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(missedcall ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }
f.se.100 = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    set.seed(i)
    
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # selecting the number of arms
    new_id_assembly2 <- as.data.table(new_id_assembly)
    n_arm <- sample(1:2, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:2
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_assembly2[fem_treat_assigned %nin% sel.arms, 
                       (columns_to_modify) := lapply(.SD, function(x) prob), 
                       .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_assembly2 <- as.data.table(new_id_assembly2)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_assembly2[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(1:2, 
                            FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x],
                                                                     prob_each = new_id_assembly2$prob100[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]],
                                                                     conditions = new_id_assembly2$assembly_id[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(missedcall ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }
f.se.200 = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    set.seed(i)
    
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # selecting the number of arms
    new_id_assembly2 <- as.data.table(new_id_assembly)
    n_arm <- sample(1:2, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:2
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_assembly2[fem_treat_assigned %nin% sel.arms, 
                       (columns_to_modify) := lapply(.SD, function(x) prob), 
                       .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_assembly2 <- as.data.table(new_id_assembly2)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_assembly2[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(1:2, 
                            FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x],
                                                                     prob_each = new_id_assembly2$prob200[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]],
                                                                     conditions = new_id_assembly2$assembly_id[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(missedcall ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }
f.se.400 = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    set.seed(i)
    
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # selecting the number of arms
    new_id_assembly2 <- as.data.table(new_id_assembly)
    n_arm <- sample(1:2, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:2
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_assembly2[fem_treat_assigned %nin% sel.arms, 
                       (columns_to_modify) := lapply(.SD, function(x) prob), 
                       .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_assembly2 <- as.data.table(new_id_assembly2)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_assembly2[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(1:2, 
                            FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x],
                                                                     prob_each = new_id_assembly2$prob400[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]],
                                                                     conditions = new_id_assembly2$assembly_id[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(missedcall ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }
f.se.800 = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    set.seed(i)
    
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # selecting the number of arms
    new_id_assembly2 <- as.data.table(new_id_assembly)
    n_arm <- sample(1:2, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:2
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_assembly2[fem_treat_assigned %nin% sel.arms, 
                       (columns_to_modify) := lapply(.SD, function(x) prob), 
                       .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_assembly2 <- as.data.table(new_id_assembly2)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_assembly2[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(1:2, 
                            FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x],
                                                                     prob_each = new_id_assembly2$prob800[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]],
                                                                     conditions = new_id_assembly2$assembly_id[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(missedcall ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }
f.se.equal = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    set.seed(i)
    
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # selecting the number of arms
    new_id_assembly2 <- as.data.table(new_id_assembly)
    n_arm <- sample(1:2, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:2
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_assembly2[fem_treat_assigned %nin% sel.arms, 
                       (columns_to_modify) := lapply(.SD, function(x) prob), 
                       .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_assembly2 <- as.data.table(new_id_assembly2)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_assembly2[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(1:2, 
                            FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x],
                                                                     prob_each = new_id_assembly2$prob_equal[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]],
                                                                     conditions = new_id_assembly2$assembly_id[new_id_assembly2$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(missedcall ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }

# Getting th pvalues
f.m.ori.p = get_p_values(se = f.se.ori, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))
f.m.100.p = get_p_values(se = f.se.100, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))
f.m.200.p = get_p_values(se = f.se.200, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))
f.m.400.p = get_p_values(se = f.se.400, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))
f.m.800.p = get_p_values(se = f.se.800, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))
f.m.equal.p = get_p_values(se = f.se.equal, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))

pvals.f <- list( 
  "prob" = f.m.ori.p, 
  "prob100" = f.m.100.p, 
  "prob200" = f.m.200.p, 
  "prob400" = f.m.400.p, 
  "prob800" = f.m.800.p, 
  "prob_equal" = f.m.equal.p
  )




# Main treatment effectiveness
m1 = iv_robust(missedcall ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")


# Function for standard errors with simulated clusters
se.ori = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # Setting the seed
    set.seed(i)
    b_sample <- sample(1:5000, 1)
    new_id_vp_bsel <- new_id_vp[ new_id_vp$boot_num == b_sample ]
    
    # Negation of Nin for the next steps     `%nin%` = Negate(`%in%`)
    # Choose a random number from 0 to 2, 
    # 0 : No arm increases the prob of its highest prob VP
    # 1 : One arm increases the prob of its highest prob VP
    # 2 : two arms increase the prob of their highest prob VP
    # ...
    # 10 : Ten arms increase the prob of their highest prob VP
    set.seed(i)
    n_arm <- sample(1:10, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:10
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      set.seed(i)
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      print( paste( "this is the selected arm to increase probs:", sel.arms ) )
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_vp_bsel[treatment_assigned %nin% sel.arms, 
                     (columns_to_modify) := lapply(.SD, function(x) prob), 
                     .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_vp_bsel <- id_vp
      new_id_vp_bsel <- as.data.table(new_id_vp_bsel)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_vp_bsel[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], 
                                         prob_each = new_id_vp_bsel$prob[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]], 
                                         conditions = new_id_vp_bsel$new_id_vp_bsel2[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(missedcall ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
              clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }
se.100 = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # Setting the seed
    set.seed(i)
    b_sample <- sample(1:5000, 1)
    new_id_vp_bsel <- new_id_vp[ new_id_vp$boot_num == b_sample ]
    
    # We have to select randomly the number of arms
    # Choose a random number from 0 to 2, 
    # 0 : No arm increases the prob of its highest prob VP
    # 1 : One arm increases the prob of its highest prob VP
    # 2 : two arms increase the prob of their highest prob VP
    # ...
    # 10 : Ten arms increase the prob of their highest prob VP
    set.seed(i)
    n_arm <- sample(1:10, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:10
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      set.seed(i)
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      print( paste( "this is the selected arm to increase probs:", sel.arms ) )
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_vp_bsel[treatment_assigned %nin% sel.arms, 
                     (columns_to_modify) := lapply(.SD, function(x) prob), 
                     .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_vp_bsel <- id_vp
      new_id_vp_bsel <- as.data.table(new_id_vp_bsel)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_vp_bsel[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], 
                                           prob_each = new_id_vp_bsel$prob100[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]], 
                                           conditions = new_id_vp_bsel$new_id_vp_bsel2[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(missedcall ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
              clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }
se.200 = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # Setting the seed
    set.seed(i)
    b_sample <- sample(1:5000, 1)
    new_id_vp_bsel <- new_id_vp[ new_id_vp$boot_num == b_sample ]
    
    # We have to select randomly the number of arms
    # Choose a random number from 0 to 2, 
    # 0 : No arm increases the prob of its highest prob VP
    # 1 : One arm increases the prob of its highest prob VP
    # 2 : two arms increase the prob of their highest prob VP
    # ...
    # 10 : Ten arms increase the prob of their highest prob VP
    set.seed(i)
    n_arm <- sample(1:10, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:10
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      set.seed(i)
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      print( paste( "this is the selected arm to increase probs:", sel.arms ) )
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_vp_bsel[treatment_assigned %nin% sel.arms, 
                     (columns_to_modify) := lapply(.SD, function(x) prob), 
                     .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_vp_bsel <- id_vp
      new_id_vp_bsel <- as.data.table(new_id_vp_bsel)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_vp_bsel[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], 
                                                                                                     prob_each = new_id_vp_bsel$prob200[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]], 
                                                                                                     conditions = new_id_vp_bsel$new_id_vp_bsel2[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(missedcall ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
              clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }
se.400 = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # Setting the seed
    set.seed(i)
    b_sample <- sample(1:5000, 1)
    new_id_vp_bsel <- new_id_vp[ new_id_vp$boot_num == b_sample ]
    
    
    # We have to select randomly the number of arms
    # Choose a random number from 0 to 2, 
    # 0 : No arm increases the prob of its highest prob VP
    # 1 : One arm increases the prob of its highest prob VP
    # 2 : two arms increase the prob of their highest prob VP
    # ...
    # 10 : Ten arms increase the prob of their highest prob VP
    set.seed(i)
    n_arm <- sample(1:10, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:10
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      set.seed(i)
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      print( paste( "this is the selected arm to increase probs:", sel.arms ) )
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_vp_bsel[treatment_assigned %nin% sel.arms, 
                     (columns_to_modify) := lapply(.SD, function(x) prob), 
                     .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_vp_bsel <- id_vp
      new_id_vp_bsel <- as.data.table(new_id_vp_bsel)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_vp_bsel[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], 
                                                                                                     prob_each = new_id_vp_bsel$prob400[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]], 
                                                                                                     conditions = new_id_vp_bsel$new_id_vp_bsel2[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(missedcall ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
              clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }
se.800 = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # Setting the seed
    set.seed(i)
    b_sample <- sample(1:5000, 1)
    new_id_vp_bsel <- new_id_vp[ new_id_vp$boot_num == b_sample ]
    
    # We have to select randomly the number of arms
    # Choose a random number from 0 to 2, 
    # 0 : No arm increases the prob of its highest prob VP
    # 1 : One arm increases the prob of its highest prob VP
    # 2 : two arms increase the prob of their highest prob VP
    # ...
    # 10 : Ten arms increase the prob of their highest prob VP
    set.seed(i)
    n_arm <- sample(1:10, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:10
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      set.seed(i)
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      print( paste( "this is the selected arm to increase probs:", sel.arms ) )
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_vp_bsel[treatment_assigned %nin% sel.arms, 
                     (columns_to_modify) := lapply(.SD, function(x) prob), 
                     .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_vp_bsel <- id_vp
      new_id_vp_bsel <- as.data.table(new_id_vp_bsel)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_vp_bsel[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], 
                                                                                                     prob_each = new_id_vp_bsel$prob800[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]], 
                                                                                                     conditions = new_id_vp_bsel$new_id_vp_bsel2[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(missedcall ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
              clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }
se.equal = foreach(i = 1:N, .combine = "cbind", .packages = c("estimatr", "randomizr", 'data.table')) %dorng%
  {
    # Negation of Nin for the next steps
    `%nin%` = Negate(`%in%`)
    
    # Setting the seed
    set.seed(i)
    b_sample <- sample(1:5000, 1)
    new_id_vp_bsel <- new_id_vp[ new_id_vp$boot_num == b_sample ]
    
    # We have to select randomly the number of arms
    # Choose a random number from 0 to 2, 
    # 0 : No arm increases the prob of its highest prob VP
    # 1 : One arm increases the prob of its highest prob VP
    # 2 : two arms increase the prob of their highest prob VP
    # ...
    # 10 : Ten arms increase the prob of their highest prob VP
    set.seed(i)
    n_arm <- sample(1:10, 1)
    
    
    # Checking the number of arms
    if ( n_arm > 0 ){
      
      # generation of combination with all the selected arms
      numbers <- 1:10
      comb_arms <- combn(numbers, n_arm)
      
      # Choose one combination randomly with uniform distribution
      set.seed(i)
      comb_sel <- sample( ncol( comb_arms ), 1 )
      sel.arms <- comb_arms[, comb_sel]
      print( paste( "this is the selected arm to increase probs:", sel.arms ) )
      
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      new_id_vp_bsel[treatment_assigned %nin% sel.arms, 
                     (columns_to_modify) := lapply(.SD, function(x) prob), 
                     .SDcols = columns_to_modify]
      
    } else{
      
      # If we are not going to increase any arm we have to change the probabilities
      new_id_vp_bsel <- id_vp
      new_id_vp_bsel <- as.data.table(new_id_vp_bsel)
      # Define the columns you want to modify
      columns_to_modify <- c("prob100", "prob200", "prob400", "prob800", "prob_equal")
      
      # Filter rows based on 'fem_treat_assigned' and modify the specified columns
      # Generate the columns and set their values equal to the 'prob' column
      new_id_vp_bsel[, (columns_to_modify) := .(prob, prob, prob, prob, prob)]
      
    }
    
    set.seed(i)
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], 
                                                                                                     prob_each = new_id_vp_bsel$prob_equal[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]], 
                                                                                                     conditions = new_id_vp_bsel$new_id_vp_bsel2[new_id_vp_bsel$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(missedcall ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
              clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }



# get p-values
m.ori.p = get_p_values(se = se.ori, lm = m1, names = c("t_ideo_takeup", "t_cand_takeup", "t_poli_takeup", "t_care_takeup"))
m.100.p = get_p_values(se = se.100, lm = m1, names = c("t_ideo_takeup", "t_cand_takeup", "t_poli_takeup", "t_care_takeup"))
m.200.p = get_p_values(se = se.200, lm = m1, names = c("t_ideo_takeup", "t_cand_takeup", "t_poli_takeup", "t_care_takeup"))
m.400.p = get_p_values(se = se.400, lm = m1, names = c("t_ideo_takeup", "t_cand_takeup", "t_poli_takeup", "t_care_takeup"))
m.800.p = get_p_values(se = se.800, lm = m1, names = c("t_ideo_takeup", "t_cand_takeup", "t_poli_takeup", "t_care_takeup"))
m.equal.p = get_p_values(se = se.equal, lm = m1, names = c("t_ideo_takeup", "t_cand_takeup", "t_poli_takeup", "t_care_takeup"))

pvals.m <- list( 
  "prob" = m.ori.p, 
  "prob100" = m.100.p, 
  "prob200" = m.200.p, 
  "prob400" = m.400.p, 
  "prob800" = m.800.p, 
  "prob_equal" = m.equal.p
)

prob_names <- c("prob", "prob100", "prob200", "prob400", "prob800", "prob_equal")
df.pvals <- data.frame()
for (col in prob_names){
  # AC
  pval <- pvals.f[[col]]
  df_aux <- as.data.frame(pval)
  df_aux$reg <- "AC"
  df_aux$prob <- col
  df_aux$id <- 1:2
  df_with_rownames <- rownames_to_column(df_aux, var = "Coefficient")
  df.pvals <- rbind(df.pvals, df_with_rownames)
  
  # VP
  pval <- pvals.m[[col]]
  df_aux <- as.data.frame(pval)
  df_aux$reg <- "VP"
  df_aux$prob <- col
  df_aux$id <- 3:6
  df_with_rownames <- rownames_to_column(df_aux, var = "Coefficient")
  df.pvals <- rbind(df.pvals, df_with_rownames)
  
}

# keep only selected rows
df.pvals.clean <- df.pvals[ df.pvals$Coefficient != "(Intercept)", ]

# Pivot the table from Long to Wide
wide_df <- df.pvals.clean %>%
  pivot_wider(
    names_from = c(reg, prob),    # Use 'reg' values to create new column names
    values_from = pval,  # Fill the new columns with values from 'prob'
    id_cols = id         # Keep 'id' as the identifier
  )

# Assign the right names of the coefficients
wide_df$id <- c("Female, LATE", "Ideology, LATE", "Candidacy, LATE", "Policy, LATE", "Career, LATE")
latex_table <- xtable(wide_df, digits = 3 )  # Adjust the number of 5s based on the number of columns in wide_df
# Define the path and name of your .tex file
file_path <- "output/tables/tableD1_sensitivity_analysis.tex"  

# Save the LaTeX table to a .tex file
print(latex_table, type = "latex", file = file_path, comment = FALSE, include.rownames = FALSE)






####### Table G.9: Campaign Messages and the Number of New Recruits #######
# Female treatment effectiveness
f.m1 = iv_robust(missedcall ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")
# Function for standard errors with simulated clusters
set.seed(seed)
f.se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(1:2, 
                            FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x],
                                                                     prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]],
                                                                     conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(missedcall ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }



# Main treatment effectiveness
m1 = iv_robust(missedcall ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")


# Function for standard errors with simulated clusters
set.seed(seed)
se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(missedcall ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
              clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }


# Extract lm_robust equation
m1.tab = estimatr::extract.iv_robust(m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
f.m1.tab = estimatr::extract.iv_robust(f.m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                       include.rmse = F, include.nclusts = T, include.ci = F)
# Get correct p-values
m1.p = get_p_values(se = se1, lm = m1, names = c("t_ideo_takeup", "t_cand_takeup", "t_poli_takeup", "t_care_takeup"))
f.m1.p = get_p_values(se = f.se1, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))




texreg(l = list(f.m1.tab, m1.tab),
       override.se = list(apply(f.se1, 1, FUN = mean),
                          apply(se1, 1, FUN = mean)),
       caption = "", caption.above = T, digits = 3,
       override.pvalues = list(f.m1.p,m1.p),
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("New Member", "New Member"),
       custom.coef.names = c("Female, LATE", "Ideology, LATE", "Candidacy, LATE", "Policy, LATE", "Career, LATE"),
       custom.gof.rows = list(
         "Control Mean" = c(mean(control.fem$missedcall, na.rm = T),
                            mean(control.messages$missedcall, na.rm = T)),
         "Constituency Fixed Effects" = c("No", "Yes")),
       stars = c(0.01, 0.05, 0.1), file = paste0(tab.out, "tableG9_newmembers_late_simple.tex"))

# Save models and standard errors for plots (Figure 5)
models.table = tidy(m1) %>%
  bind_rows(tidy(f.m1)) %>%
  mutate(se_bootstr = c(apply(se1, 1, FUN = mean), apply(f.se1, 1, FUN = mean)))

write.csv(models.table, file = paste0(tab.out, "newmembers_coefficients.csv"))







######### Table F.6: Campaign Messages and Callbacks from Located Members and Motivated Members #####################
# Female treatment effectiveness
# Located member
f.m1 = iv_robust(newmemb ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")
set.seed(seed)
f.se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x], prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]], conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(newmemb ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }
# Motivated member
f.m2 = iv_robust(motivmemb ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")
set.seed(seed)
f.se2 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x], prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]], conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(motivmemb ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }



# Treatment message effectiveness
# Located member
m1 = iv_robust(newmemb ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")

set.seed(seed)
se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(newmemb ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
              clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }
# Motivated member
m2 = iv_robust(motivmemb ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
set.seed(seed)
se2 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(motivmemb ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
              clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }




# Get correct p-values based on the simulated SEs
names = c("t_poli_takeup", "t_cand_takeup", "t_care_takeup", "t_ideo_takeup")
f.m1.p = get_p_values(se = f.se1, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))
f.m2.p = get_p_values(se = f.se2, lm = f.m2, names = c("(Intercept)", "t_fem_takeup"))
m1.p = get_p_values(se = se1, lm = m1, names = names)
m2.p = get_p_values(se = se2, lm = m2, names = names)






texreg(l = list(f.m1, f.m2,  m1, m2),
       override.se = list(
         apply(f.se1, 1, FUN = mean), apply(f.se2, 1, FUN = mean),
         apply(se1, 1, FUN = mean), apply(se2, 1, FUN = mean)),
       caption = "", caption.above = T, digits = 3,
       override.pvalues = list(f.m1.p, f.m2.p, m1.p, m2.p),
       include.adjrs = FALSE, include.rsquared = FALSE, include.rmse = F, 
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("Located Member", "Motivated Member", "Located Member", "Motivated Member"),
       custom.coef.names = c("Female, LATE", "Ideology, LATE", "Candidacy, LATE",
                             "Policy, LATE", "Career, LATE"),
       custom.gof.rows = list(
         "Control Mean" = c(mean(control.fem$newmemb, na.rm = T),
                            mean(control.fem$motivmemb, na.rm = T),
                            mean(control.messages$newmemb, na.rm = T),
                            mean(control.messages$motivmemb, na.rm = T)),
         "Constituency Fixed Effects" = c("No", "No", "Yes", "Yes")),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "tableF6_locatedmembers_late_simple.tex"))


# Stop cluster
registerDoSEQ()
stopCluster(cluster)
