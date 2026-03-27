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



# Load required packages
library(reshape2)
library(dplyr)
library(haven)
library(stargazer)
library(AER)
library(sandwich)
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




############# Table G.10: Effect of Recruitment Messages Together with Female Encouragement Treatment ################
## Number of new members and all messages-female treatments
form = c("~ t_ideo_male_takeup + t_cand_male_takeup + t_poli_male_takeup + t_care_male_takeup + t_base_female_takeup + t_ideo_female_takeup + t_cand_female_takeup + t_poli_female_takeup + t_care_female_takeup | t_ideo_male + t_cand_male + t_poli_male + t_care_male + t_base_female + t_ideo_female + t_cand_female + t_poli_female + t_care_female")
m1 = (iv_robust(paste0("missedcall", form) %>% as.formula, clusters = id_vp2, data = dta, se_type = "stata"))
set.seed(seed)
se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(as.formula(paste0("missedcall", form)), clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }

baseline_f_m1 = m1$p.value[m1$term=="t_base_female_takeup"]

# Extract lm_robust equation
m1.tab = estimatr::extract.iv_robust(m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
# Get correct p-values
names = c("(Intercept)", "t_ideo_male_takeup", "t_cand_male_takeup", "t_poli_male_takeup", "t_care_male_takeup", 
          "t_base_female_takeup", "t_ideo_female_takeup", "t_cand_female_takeup", "t_poli_female_takeup", "t_care_female_takeup")
m1.p = get_p_values(se = se1, lm = m1, names = names)

# Female treatment effectiveness alone
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

f.m1.p = get_p_values(se = f.se1, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))


# Joint orthogonality tests
# Test joint significance of the treatments for male pamphlets
mm.ur1 = felm(missedcall ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==0))
joint.f.statm1 = lfe::waldtest(mm.ur1, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pm1 = lfe::waldtest(mm.ur1, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)

# Test joint significance of the treatments for female pamphlets
mf.ur1 = felm(missedcall ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==1))
joint.f.statf1 = lfe::waldtest(mf.ur1, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pf1 = lfe::waldtest(mf.ur1, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)


## Excluded group members 
form = c("~  t_poli_male_takeup + t_cand_male_takeup + t_care_male_takeup + t_ideo_male_takeup + t_base_female_takeup + t_poli_female_takeup + t_cand_female_takeup + t_care_female_takeup + t_ideo_female_takeup  |  t_poli_male + t_cand_male + t_care_male + t_ideo_male + t_base_female + t_poli_female + t_cand_female + t_care_female + t_ideo_female")
m2 = (iv_robust(paste0("excluded_group", form) %>% as.formula, clusters = id_vp2, data = dta, se_type = "stata"))
set.seed(seed)
se2 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(as.formula(paste0("excluded_group", form)), clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }

baseline_f_m2 = m2$p.value[m2$term=="t_base_female_takeup"]


# Extract lm_robust equation
m2.tab = estimatr::extract.iv_robust(m2, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
# Get correct p-values
m2.p = get_p_values(se = se2, lm = m2, names = names)


# Test joint significance of the treatments for male pamphlets
mm.ur2 = felm(excluded_group ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==0))
joint.f.statm2 = lfe::waldtest(mm.ur2, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pm2 = lfe::waldtest(mm.ur2, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)


# Test joint significance of the treatments for female pamphlets
mf.ur2 = felm(excluded_group ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==1))
joint.f.statf2 = lfe::waldtest(mf.ur2, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pf2 = lfe::waldtest(mf.ur2, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)

# Effectiveness of female treatment
f.m2 = iv_robust(excluded_group ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")
set.seed(seed)
f.se2 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x], prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]], conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(excluded_group ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }

f.m2.p = get_p_values(se = f.se2, lm = f.m2, names = c("(Intercept)", "t_fem_takeup"))






### Skilled members
form = c("~  t_poli_male_takeup + t_cand_male_takeup + t_care_male_takeup + t_ideo_male_takeup + t_base_female_takeup + t_poli_female_takeup + t_cand_female_takeup + t_care_female_takeup + t_ideo_female_takeup  |  t_poli_male + t_cand_male + t_care_male + t_ideo_male + t_base_female + t_poli_female + t_cand_female + t_care_female + t_ideo_female")
m3 = (iv_robust(paste0("skill_index", form) %>% as.formula, clusters = id_vp2, data = dta, se_type = "stata"))
set.seed(seed)
se3 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(as.formula(paste0("skill_index", form)), clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }

baseline_f_m3 = m3$p.value[m3$term=="t_base_female_takeup"]


# Extract lm_robust equation
m3.tab = estimatr::extract.iv_robust(m3, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
# Get correct p-values
names = c("(Intercept)", "t_poli_male_takeup", "t_cand_male_takeup", "t_care_male_takeup", "t_ideo_male_takeup", 
          "t_base_female_takeup", "t_poli_female_takeup", "t_cand_female_takeup", "t_care_female_takeup", "t_ideo_female_takeup")
m3.p = get_p_values(se = se3, lm = m3, names = names)


# Test joint significance of the treatments for male pamphlets
mm.ur3 = felm(skill_index ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==0))
joint.f.statm3 = lfe::waldtest(mm.ur3, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pm3 = lfe::waldtest(mm.ur3, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)



# Test joint significance of the treatments for female pamphlets
mf.ur3 = felm(skill_index ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==1))
joint.f.statf3 = lfe::waldtest(mf.ur3, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pf3 = lfe::waldtest(mf.ur3, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)

# Effectiveness of female treatement
f.m3 = iv_robust(skill_index ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")
set.seed(seed)
f.se3 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x], prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]], conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(skill_index ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }

f.m3.p = get_p_values(se = f.se3, lm = f.m3, names = c("(Intercept)", "t_fem_takeup"))






texreg(l = list(m1.tab, m2.tab, m3.tab),
       override.se = list(apply(se1, 1, FUN = mean), apply(se2, 1, FUN = mean),
                          apply(se3, 1, FUN = mean)),
       caption = "", caption.above = T, digits = 3,
       override.pvalues = list(m1.p, m2.p, m3.p),
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("New Member", "Excluded Member", "Skilled Member"),
       custom.coef.names = c("Ideology (M), LATE", "Candidacy (M), LATE", 
                             "Policy (M), LATE", "Career (M), LATE", 
                             "Baseline (F), LATE", "Ideology (F), LATE",
                             "Candidacy (F), LATE", "Policy (F), LATE",
                             "Career (F), LATE"),
       custom.gof.rows = list("Control Mean" = c(mean(control.messages$missedcall[control.messages$t_fem==0], na.rm = T),
                                                 mean(control.messages$excluded_group[control.messages$t_fem==0], na.rm = T), 
                                                 mean(control.messages$skill_index[control.messages$t_fem==0], na.rm = T)),
                              "Constituency Fixed Effects" = c("No", "No", "No"),
                              "p-values" = c("", "", ""), 
                              "All F vs. All M p-value" = c(f.m1.p[2], f.m2.p[2], f.m3.p[2]), 
                              "M Benefits vs Baseline (M) F-stat" = c(joint.f.statm1, joint.f.statm2, joint.f.statm3),
                              "M Benefits vs Baseline (M) p-value" = c(joint.pm1, joint.pm2, joint.pm3),
                              "Baseline (F) vs Baseline (M) p-value" = c(baseline_f_m1, baseline_f_m2, baseline_f_m3),
                              "F Benefits vs Baseline (F) F-stat" = c(joint.f.statf1, joint.f.statf2, joint.f.statf3),
                              "F Benefits vs Baseline (F) p-value" = c(joint.pf1, joint.pf2, joint.pf3)),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "tableG10_interaction_late_simple.tex"))




###### Figure 6: The Impact of Decomposed Treatments on Number of Recruits ########
form = c("~ t_ideo_male_takeup + t_cand_male_takeup + t_poli_male_takeup + t_care_male_takeup + t_base_female_takeup + t_ideo_female_takeup + t_cand_female_takeup + t_poli_female_takeup + t_care_female_takeup | t_ideo_male + t_cand_male + t_poli_male + t_care_male + t_base_female + t_ideo_female + t_cand_female + t_poli_female + t_care_female")
m1 = (iv_robust(paste0("missedcall", form) %>% as.formula, clusters = id_vp2, data = dta, se_type = "stata"))
set.seed(seed)
se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(as.formula(paste0("missedcall", form)), clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }

# Test joint significance of the treatments for male pamphlets
mm.ur = felm(missedcall ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==0))
joint.f.statm = lfe::waldtest(mm.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pm = lfe::waldtest(mm.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)

# Test joint significance of the treatments for female pamphlets
mf.ur = felm(missedcall ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==1))
joint.f.statf = lfe::waldtest(mf.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pf = lfe::waldtest(mf.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)


# Save models and standard errors for figure
models.table = tidy(m1) %>%
  mutate(se_bootstr = c(apply(se1, 1, FUN = mean))) %>%
  bind_rows(data.frame(
    p.value = c(joint.pm, joint.pf),
    statistic = c(joint.f.statm, joint.f.statf),
    term = c("Joint Sign. Male Pamph.", "Joint Sign. Female Pamp."),
    sample = c("male", "female")
  ))


write.csv(models.table, file = paste0(tab.out, "newmembers_interaction_coefficients.csv"))




######## FIGURE G.9: EXCLUDED GROUP ########
form = c("~  t_poli_male_takeup + t_cand_male_takeup + t_care_male_takeup + t_ideo_male_takeup + t_base_female_takeup + t_poli_female_takeup + t_cand_female_takeup + t_care_female_takeup + t_ideo_female_takeup  |  t_poli_male + t_cand_male + t_care_male + t_ideo_male + t_base_female + t_poli_female + t_cand_female + t_care_female + t_ideo_female")
m1 = (iv_robust(paste0("excluded_group", form) %>% as.formula, clusters = id_vp2, data = dta, se_type = "stata"))
set.seed(seed)
se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(as.formula(paste0("excluded_group", form)), clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }


# Test joint significance of the treatments for male pamphlets
mm.ur = felm(excluded_group ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==0))
joint.f.statm = lfe::waldtest(mm.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pm = lfe::waldtest(mm.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)


# Test joint significance of the treatments for female pamphlets
mf.ur = felm(excluded_group ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==1))
joint.f.statf = lfe::waldtest(mf.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pf = lfe::waldtest(mf.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)



# Save models and standard errors
models.table = tidy(m1) %>%
  mutate(se_bootstr = c(apply(se1, 1, FUN = mean))) %>%
  bind_rows(data.frame(
    p.value = c(joint.pm, joint.pf),
    statistic = c(joint.f.statm, joint.f.statf),
    term = c("Joint Sign. Male Pamph.", "Joint Sign. Female Pamp."),
    sample = c("male", "female")
  ))


write.csv(models.table, file = paste0(tab.out, "excluded_group_interaction_coefficients.csv"))





####### FIGURE G.9: SKILLED MEMBERS #######
form = c("~  t_poli_male_takeup + t_cand_male_takeup + t_care_male_takeup + t_ideo_male_takeup + t_base_female_takeup + t_poli_female_takeup + t_cand_female_takeup + t_care_female_takeup + t_ideo_female_takeup  |  t_poli_male + t_cand_male + t_care_male + t_ideo_male + t_base_female + t_poli_female + t_cand_female + t_care_female + t_ideo_female")
m1 = (iv_robust(paste0("skill_index", form) %>% as.formula, clusters = id_vp2, data = dta, se_type = "stata"))
set.seed(seed)
se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(as.formula(paste0("skill_index", form)), clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }



# Test joint significance of the treatments for male pamphlets
mm.ur = felm(skill_index ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==0))
joint.f.statm = lfe::waldtest(mm.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pm = lfe::waldtest(mm.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)



# Test joint significance of the treatments for female pamphlets
mf.ur = felm(skill_index ~ 1 | 0 |
               (t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup ~ t_poli+t_cand+t_care+t_ideo) | id_vp2, data = dta %>% filter(t_fem==1))
joint.f.statf = lfe::waldtest(mf.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[5] %>% round(3)
joint.pf = lfe::waldtest(mf.ur, ~t_poli_takeup|t_cand_takeup|t_care_takeup|t_ideo_takeup)[4] %>% round(3)


# Save models and standard errors
models.table = tidy(m1) %>%
  mutate(se_bootstr = c(apply(se1, 1, FUN = mean))) %>%
  bind_rows(data.frame(
    p.value = c(joint.pm, joint.pf),
    statistic = c(joint.f.statm, joint.f.statf),
    term = c("Joint Sign. Male Pamph.", "Joint Sign. Female Pamp."),
    sample = c("male", "female")
  ))


write.csv(models.table, file = paste0(tab.out, "skill_index_interaction_coefficients.csv"))

# Stop cluster
registerDoSEQ()
stopCluster(cluster)
