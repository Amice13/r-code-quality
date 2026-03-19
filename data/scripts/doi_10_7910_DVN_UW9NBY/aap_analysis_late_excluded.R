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



####### Table G.7: Treatment Means and Joint Orthogonality Tests #######

## Excluded group
means_excl_gr = iv_robust(excluded_group ~ t_base_male_takeup + t_ideo_male_takeup +
                            t_cand_male_takeup + t_poli_male_takeup +  
                            t_care_male_takeup +  t_base_female_takeup +
                            t_ideo_female_takeup + t_cand_female_takeup +
                            t_poli_female_takeup + t_care_female_takeup -1 |
                            t_base_male + t_ideo_male + t_cand_male + t_poli_male +
                            t_care_male + t_base_female + t_ideo_female + t_cand_female +
                            t_poli_female + t_care_female -1, clusters = id_vp2,
                          data = dta, se_type = "stata")

m1.ur = felm(excluded_group ~ 1 | 0 |
               (t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup ~ t_ideo_male+t_cand_male+t_poli_male+t_care_male+t_base_female+t_ideo_female+t_poli_female+t_cand_female+t_care_female)| id_vp2, data = dta)
joint.f.stat1 = lfe::waldtest(m1.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[5] %>% round(3)
joint.p1 = lfe::waldtest(m1.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[4] %>% round(3)

## Female
means_female = iv_robust(female ~ t_base_male_takeup + t_ideo_male_takeup +
                           t_cand_male_takeup + t_poli_male_takeup +  
                           t_care_male_takeup +  t_base_female_takeup +
                           t_ideo_female_takeup + t_cand_female_takeup +
                           t_poli_female_takeup + t_care_female_takeup -1 |
                           t_base_male + t_ideo_male + t_cand_male + t_poli_male +
                           t_care_male + t_base_female + t_ideo_female + t_cand_female +
                           t_poli_female + t_care_female -1, clusters = id_vp2,
                         data = dta, se_type = "stata")



m2.ur = felm(female ~ 1 | 0 |
               (t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup ~ t_ideo_male+t_cand_male+t_poli_male+t_care_male+t_base_female+t_ideo_female+t_poli_female+t_cand_female+t_care_female)| id_vp2, data = dta)
joint.f.stat2 = lfe::waldtest(m2.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[5] %>% round(3)
joint.p2 = lfe::waldtest(m2.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[4] %>% round(3)

## Excluded Caste
means_excl_caste = iv_robust(excluded_caste ~  t_base_male_takeup + t_ideo_male_takeup +
                               t_cand_male_takeup + t_poli_male_takeup +  
                               t_care_male_takeup +  t_base_female_takeup +
                               t_ideo_female_takeup + t_cand_female_takeup +
                               t_poli_female_takeup + t_care_female_takeup -1 |
                               t_base_male + t_ideo_male + t_cand_male + t_poli_male +
                               t_care_male + t_base_female + t_ideo_female + t_cand_female +
                               t_poli_female + t_care_female -1, clusters = id_vp2,
                             data = dta, se_type = "stata")

m3.ur = felm(excluded_caste ~ 1 | 0 |
               (t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup ~ t_ideo_male+t_cand_male+t_poli_male+t_care_male+t_base_female+t_ideo_female+t_poli_female+t_cand_female+t_care_female)| id_vp2, data = dta)
joint.f.stat3 = lfe::waldtest(m3.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[5] %>% round(3)
joint.p3 = lfe::waldtest(m3.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[4] %>% round(3)

## Excluded Caste
means_excl_religion = iv_robust(excluded_religion ~  t_base_male_takeup + t_ideo_male_takeup +
                                  t_cand_male_takeup + t_poli_male_takeup +  
                                  t_care_male_takeup +  t_base_female_takeup +
                                  t_ideo_female_takeup + t_cand_female_takeup +
                                  t_poli_female_takeup + t_care_female_takeup -1 |
                                  t_base_male + t_ideo_male + t_cand_male + t_poli_male +
                                  t_care_male + t_base_female + t_ideo_female + t_cand_female +
                                  t_poli_female + t_care_female -1, clusters = id_vp2,
                                data = dta, se_type = "stata")

m4.ur = felm(excluded_religion ~ 1 | 0 |
               (t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup ~ t_ideo_male+t_cand_male+t_poli_male+t_care_male+t_base_female+t_ideo_female+t_poli_female+t_cand_female+t_care_female)| id_vp2, data = dta)
joint.f.stat4 = lfe::waldtest(m4.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[5] %>% round(3)
joint.p4 = lfe::waldtest(m4.ur, ~t_ideo_male_takeup|t_cand_male_takeup|t_poli_male_takeup|t_care_male_takeup|t_base_female_takeup|t_ideo_female_takeup|t_cand_female_takeup|t_poli_female_takeup|t_care_female_takeup)[4] %>% round(3)


bind_cols(treatments = c("Baseline (Male)", "Policy (Male)", "Candidacy (Male)", "Career (Male)", "Ideology (Male)",
                         "Baseline (Female)", "Policy (Female)", "Candidacy (Female)", "Career (Female)", "Ideology (Female)",
                         "Joint Orthogonality Hypothesis Test", "Joint F statistic", "Joint p-value", "Observations"),
          excluded_group= c(round(tidy(means_excl_gr)$estimate, 3), NA_integer_, joint.f.stat1, joint.p1, means_excl_gr$nobs),
          female = c(round(tidy(means_female)$estimate, 3), NA_integer_, joint.f.stat2, joint.p2, means_female$nobs),
          excluded_caste = c(round(tidy(means_excl_caste)$estimate, 3), NA_integer_, joint.f.stat3, joint.p3, means_excl_caste$nobs),
          excluded_religion = c(round(tidy(means_excl_religion)$estimate, 3), NA_integer_, joint.f.stat4, joint.p4, means_excl_religion$nobs)) %>%
  xtable(digits = 3) %>% 
  rename(`Excluded Group` = excluded_group, `Female` = female, `Excluded Caste/Tribe` = excluded_caste, 
         `Excluded Religion` = excluded_religion) %>%
  print(file = paste0(tab.out, "tableG7_joint_hypothesis_excluded.tex"), booktabs = T,
        include.rownames = F)



####### TABLE 5: PANEL A: CHARACTERISTICS OF NEW RECRUITS, FEMALE TREATMENT #######
# LATE estimation
f.m1 = iv_robust(excluded_group ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")
set.seed(seed)
f.se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x], prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]], conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(excluded_group ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }

f.m2 = iv_robust(female ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")
set.seed(seed)
f.se2 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x], prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]], conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(female ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }

f.m3 = iv_robust(excluded_caste ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")
set.seed(seed)
f.se3 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x], prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]], conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(excluded_caste ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }

f.m4 = iv_robust(excluded_religion ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")
set.seed(seed)
f.se4 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x], prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]], conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(excluded_religion ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }



# Extract lm_robust equation
f.m1.tab = estimatr::extract.iv_robust(f.m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                       include.rmse = F, include.nclusts = T, include.ci = F)
f.m2.tab = estimatr::extract.iv_robust(f.m2, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                       include.rmse = F, include.nclusts = T, include.ci = F)
f.m3.tab = estimatr::extract.iv_robust(f.m3, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                       include.rmse = F, include.nclusts = T, include.ci = F)
f.m4.tab = estimatr::extract.iv_robust(f.m4, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                       include.rmse = F, include.nclusts = T, include.ci = F)

# Get correct p-values
f.m1.p = get_p_values(se = f.se1, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))
f.m2.p = get_p_values(se = f.se2, lm = f.m2, names = c("(Intercept)", "t_fem_takeup"))
f.m3.p = get_p_values(se = f.se3, lm = f.m3, names = c("(Intercept)", "t_fem_takeup"))
f.m4.p = get_p_values(se = f.se4, lm = f.m4, names = c("(Intercept)", "t_fem_takeup"))



texreg(l = list(f.m1.tab, f.m2.tab, f.m3.tab, f.m4.tab),
       override.se = list(apply(f.se1, 1, FUN = mean),
                          apply(f.se2, 1, FUN = mean), apply(f.se3, 1, FUN = mean),
                          apply(f.se4, 1, FUN = mean)),
       caption = "", caption.above = T, digits = 3,
       override.pvalues = list(f.m1.p, f.m2.p, f.m3.p, f.m4.p),
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("Excluded Group", "Female", "Excluded Caste/Tribe",
                              "Excluded Religion"),
       custom.coef.names = c("Female Encouragement, LATE"),
       custom.gof.rows = list("Control Mean" = c(mean(control.fem$excluded_group, na.rm = T),
                                                 mean(control.fem$female, na.rm = T),
                                                 mean(control.fem$excluded_caste, na.rm = T),
                                                 mean(control.fem$excluded_religion, na.rm = T)),
                              "Constituency Fixed Effects" = c("No", "No", "No", "No")),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "table5A_excluded_fem_late_simple.tex"))

# Save models and standard errors
models.table = tidy(f.m1) %>%
  bind_rows(tidy(f.m2), tidy(f.m3), tidy(f.m4)) %>%
  mutate(se_bootstr = c(apply(f.se1, 1, FUN = mean), apply(f.se2, 1, FUN = mean),
                        apply(f.se3, 1, FUN = mean), apply(f.se4, 1, FUN = mean)))

write.csv(models.table, file = paste0(tab.out, "excluded_fem_coefficients.csv"))




####### TABLE 5: PANEL B: CHARACTERISTICS OF NEW RECRUITS, MESSAGES #######

# LATE estimation
m1 = iv_robust(excluded_group ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup |
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
set.seed(seed)
se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
{
  cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
  iv_robust(excluded_group ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup |
              t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
            clusters = cluster, data = dta, se_type = "stata")$std.error

}

m2 = iv_robust(female ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup |
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
set.seed(seed)
se2 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
{
  cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
  iv_robust(female ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup |
              t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
            clusters = cluster, data = dta, se_type = "stata")$std.error

}

m3 = iv_robust(excluded_caste ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup |
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
set.seed(seed)
se3 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
{
  cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
  iv_robust(excluded_caste ~t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup |
              t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
            clusters = cluster, data = dta, se_type = "stata")$std.error

}

m4 = iv_robust(excluded_religion~t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup |
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
set.seed(seed)
se4 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
{
  cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
  iv_robust(excluded_religion~t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup |
              t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
            clusters = cluster, data = dta, se_type = "stata")$std.error

}


names = c("t_poli_takeup", "t_cand_takeup", "t_care_takeup", "t_ideo_takeup")


# Extract lm_robust equation
m1.tab = estimatr::extract.iv_robust(m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m2.tab = estimatr::extract.iv_robust(m2, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m3.tab = estimatr::extract.iv_robust(m3, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m4.tab = estimatr::extract.iv_robust(m4, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)

# Get correct p-values
m1.p = get_p_values(se = se1, lm = m1, names = names)
m2.p = get_p_values(se = se2, lm = m2, names = names)
m3.p = get_p_values(se = se3, lm = m3, names = names)
m4.p = get_p_values(se = se4, lm = m4, names = names)



texreg(l = list(m1.tab, m2.tab, m3.tab, m4.tab),
       override.se = list(apply(se1, 1, FUN = mean),
                          apply(se2, 1, FUN = mean), apply(se3, 1, FUN = mean),
                          apply(se4, 1, FUN = mean)),
       caption = "", caption.above = T, digits = 3,
       override.pvalues = list(m1.p, m2.p, m3.p, m4.p),
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("Excluded Group", "Female", "Excluded Caste/Tribe",
                              "Excluded Religion"),
       custom.coef.names = c("Ideology, LATE", "Candidacy, LATE", "Policy, LATE","Career, LATE"),
       custom.gof.rows = list(
         "Control Mean" = c(mean(control.messages$excluded_group, na.rm = T),
                            mean(control.messages$female, na.rm = T),
                            mean(control.messages$excluded_caste, na.rm = T),
                            mean(control.messages$excluded_religion, na.rm = T)),
         "Constituency Fixed Effects" = c("Yes", "Yes", "Yes", "Yes")),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "table5B_updated_excluded_late_simple.tex"))

# Save models and standard errors
models.table = tidy(m1) %>%
  bind_rows(tidy(m2), tidy(m3), tidy(m4)) %>%
  mutate(se_bootstr = c(apply(se1, 1, FUN = mean), apply(se2, 1, FUN = mean),
                        apply(se3, 1, FUN = mean), apply(se4, 1, FUN = mean)))

write.csv(models.table, file = paste0(tab.out, "excluded_coefficients.csv"))


####### "Table G.11: Gender Inclusiveness, Benefits, and the Recruitment of Included Groups #######
f.m1 = iv_robust(included_groups ~ t_fem_takeup|t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id,
                 se_type = "stata")
set.seed(seed)
f.se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x], prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]], conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))
    iv_robust(included_groups ~ t_fem_takeup|t_fem, data = dta,
              clusters = cluster,
              se_type = "stata")$std.error
    
  }





# Extract lm_robust equation
f.m1.tab = estimatr::extract.iv_robust(f.m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                       include.rmse = F, include.nclusts = T, include.ci = F)

# Get correct p-values
f.m1.p = get_p_values(se = f.se1, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))

### Messages analysis  ####
m1 = iv_robust(included_groups ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup |
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
set.seed(seed)
se1 = foreach(i = 1:n, .combine = "cbind", .packages = c("estimatr", "randomizr")) %dorng%
  {
    cluster = unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))
    iv_robust(included_groups ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup |
                t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
              clusters = cluster, data = dta, se_type = "stata")$std.error
    
  }




# Extract lm_robust equation
m1.tab = estimatr::extract.iv_robust(m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)

# Get correct p-values
names = c("t_ideo_takeup","t_cand_takeup", "t_poli_takeup", "t_care_takeup")
m1.p = get_p_values(se = se1, lm = m1, names = names)


texreg(l = list(f.m1.tab, m1.tab),
       override.se = list(apply(f.se1, 1, FUN = mean),
                          apply(se1, 1, FUN = mean)),
       caption = "", caption.above = T, digits = 3,
       override.pvalues = list(f.m1.p, m1.p),
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("Included Group", "Included Group"),
       custom.coef.names = c("Female, LATE", "Ideology, LATE", "Candidacy, LATE",
                             "Policy, LATE", "Career, LATE"),
       custom.gof.rows = list("Control Mean" = c(mean(control.fem$included_groups, na.rm = T),
                                                 mean(control.messages$included_groups, na.rm = T)),
                              "Constituency Fixed Effects" = c("No", "Yes")),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "tableG11_included_group_late_simple.tex"))

# Stop cluster
registerDoSEQ()
stopCluster(cluster)
