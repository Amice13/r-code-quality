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

#### Set the location of libraries for the parallel_ivclust_function on line 91 #####



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




parallel_ivclust_func = function(form, cluster.mat, dep.var.str, data, cl.no, fem=0){
  
  cl <- makeCluster(cl.no, varlist = "lib", envir = environment())
  parallel::clusterExport(cl, varlist = c("lib"), envir = environment())
  clusterEvalQ(cl, {
    library(estimatr, lib.loc = lib) # set library location here!
   
    
  })
  cluster.mat <- cluster.mat
  dep.var.str <- dep.var.str
  form <- form
  my.data <- data
  fem <- fem
  parallel::clusterExport(cl, varlist = c("form", "my.data", "cluster.mat", "dep.var.str", "fem"), envir = environment())
  if (fem==0){
    se1 <- parApply(cl, X = cluster.mat, MARGIN = 2,
                    FUN = function(x){
                      num <- x
                      formula = as.formula(paste0(dep.var.str, "~", form))
                      se = iv_robust(formula, 
                                     fixed_effects = ~ assembly_id, 
                                     clusters = num, 
                                     data = my.data, se_type = "stata")$std.error
                      #class(x)
                    })
  }else {
    se1 <- parApply(cl, X = cluster.mat, MARGIN = 2,
                    FUN = function(x){
                      num <- x
                      formula = as.formula(paste0(dep.var.str, "~", form))
                      se = iv_robust(formula, 
                                     clusters = num, 
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

######### SET UP FOR RUNNING STANDARD ERRORS ##########3




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


seed = 20191101
n = 5000

treatments = treat.tally[rep(rownames(treat.tally), treat.tally$items), c(1,3)] %>%
  cbind.data.frame(matrix(NA, ncol = n, nrow = 144975))

treatments.fem = fem.treat.tally[rep(rownames(fem.treat.tally), fem.treat.tally$items), c(1,3)] %>%
  cbind.data.frame(matrix(NA, ncol = n, nrow = 144975))


set.seed(seed)
prob.mat = sapply(treatments[,3:ncol(treatments)], FUN = function(y) {unlist(sapply(treat.tally$treatment_assigned, FUN = function(x){randomizr::complete_ra(N = treat.tally$items[x], prob_each = id_vp$prob[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]], conditions = id_vp$id_vp2[id_vp$treatment_assigned==treat.tally$treatment_assigned[x]])}))})
prob.mat.fem = sapply(treatments.fem[,3:ncol(treatments.fem)], FUN = function(y) {unlist(sapply(1:2, FUN = function(x){randomizr::complete_ra(N = fem.treat.tally$items[x], prob_each = id_assembly$prob[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]], conditions = id_assembly$assembly_id[id_assembly$fem_treat_assigned==fem.treat.tally$fem_treat_assigned[x]])}))})

cl = detectCores()
cluster = snow::makeCluster(cl)
registerDoSNOW(cluster)



###### Table G.14: Gender Inclusiveness, Benefits of Joining, and the Retention of Rankand-File: Panel A #########

# LATE estimation
f.m1 = iv_robust(consented_21  ~ t_fem_takeup |
                   t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id, se_type = "stata")
f.m2 = iv_robust(excluded_group_21  ~ t_fem_takeup |
                   t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id, se_type = "stata")
f.m3 = iv_robust(skill_index_21 ~ t_fem_takeup |
                   t_fem, data = dta %>% arrange(fem_treat_assigned),
                 clusters = assembly_id, se_type = "stata")

f.lmlist = list(f.m1, f.m2, f.m3)
# Get clustered standard errors
form = "t_fem_takeup | t_fem"
f.se1 = parallel_ivclust_func(form = form, dep.var.str = "consented_21", data = dta, fem = 1, cl.no = 8, cluster.mat = prob.mat.fem[which(!is.na(dta$consented_21)),])
f.se2 = parallel_ivclust_func(form = form,  dep.var.str = "excluded_group_21", data = dta, fem = 1, cl.no = 5, cluster.mat = prob.mat.fem[which(!is.na(dta$excluded_group_21)),])
f.se3 = parallel_ivclust_func(form = form,  dep.var.str = "skill_index_21", data = dta, fem = 1, cl.no = 8, cluster.mat = prob.mat.fem[which(!is.na(dta$skill_index_21)),])


f.selist = list(f.se1, f.se2, f.se3)
names = c("t_fem_takeup")
colnames = c("consented_21", "excluded_group_21", "skill_index_21")
f.m1.p = get_p_values(se = f.se1, lm = f.m1, names = c("(Intercept)", "t_fem_takeup"))
f.m2.p = get_p_values(se = f.se2, lm = f.m2, names = c("(Intercept)", "t_fem_takeup"))
f.m3.p = get_p_values(se = f.se3, lm = f.m3, names = c("(Intercept)", "t_fem_takeup"))


f.m1.tab = estimatr::extract.iv_robust(f.m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
f.m2.tab = estimatr::extract.iv_robust(f.m2, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
f.m3.tab = estimatr::extract.iv_robust(f.m3, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)



## Simple table with mean of the standard errors
texreg(l = list(f.m1.tab, f.m2.tab,f.m3.tab),
       override.se = list(apply(f.se1, 1, FUN = mean), apply(f.se2, 1, FUN = mean),
                          apply(f.se3, 1, FUN = mean)),
       override.pvalues = list(f.m1.p, f.m2.p, f.m3.p),
       caption = "", caption.above = T, digits = 3,
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("Consented (2021)", "Excluded Group (2021)",
                              "Skilled (2021)"),
       custom.coef.names = c("Female, LATE"),
       custom.gof.rows = list("Control Mean" = c(mean(control.fem$consented_21, na.rm = T),
                                                 mean(control.fem$excluded_group_21, na.rm = T),
                                                 mean(control.fem$skill_index_21, na.rm = T)), 
                              "Constituency Fixed Effects" = c("No", "No", "No")),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "tableG14_panelA_recall_fem_late_simple.tex"))

# Save models and standard errors
models.table = tidy(f.m1) %>%
  bind_rows(tidy(f.m2), tidy(f.m3)) %>%
  mutate(se_bootstr = c(apply(f.se1, 1, FUN = mean),
                        apply(f.se2, 1, FUN = mean),
                        apply(f.se3, 1, FUN = mean)))

write.csv(models.table, file = paste0(tab.out, "recall_fem_coefficients.csv"))




###### Table G.14: Gender Inclusiveness, Benefits of Joining, and the Retention of Rankand-File, PANEL B: Effectiveness of messages long-term #######

#LATE estimation
m1 = iv_robust(consented_21 ~t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
m2 = iv_robust(excluded_group_21 ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
m3 = iv_robust(skill_index_21 ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")



lmlist = list(m1, m2,  m3)
# Get clustered standard errors
form = "t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup | t_ideo+t_cand+t_poli+t_care"
se1 = parallel_ivclust_func(form = form, dep.var.str = "consented_21", data = dta, fem = 0, cl.no = 4, cluster.mat = prob.mat[which(!is.na(dta$consented_21)),])
se2 = parallel_ivclust_func(form = form, dep.var.str = "excluded_group_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$excluded_group_21)),])
se3 = parallel_ivclust_func(form = form, dep.var.str = "skill_index_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$skill_index_21)),])

selist = list(se1, se2, se3)
# Get p-values
names = c("t_ideo_takeup","t_cand_takeup", "t_poli_takeup",  "t_care_takeup")
m1.p = get_p_values(se = se1, lm = m1, names = names)
m2.p = get_p_values(se = se2, lm = m2, names = names)
m3.p = get_p_values(se = se3, lm = m3, names = names)

# Produce first table
m1.tab = estimatr::extract.iv_robust(m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                       include.rmse = F, include.nclusts = T, include.ci = F)
m2.tab = estimatr::extract.iv_robust(m2, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                       include.rmse = F, include.nclusts = T, include.ci = F)
m3.tab = estimatr::extract.iv_robust(m3, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                       include.rmse = F, include.nclusts = T, include.ci = F)


texreg(l = list(m1.tab, m2.tab, m3.tab),
       override.se = list(apply(se1, 1, FUN = mean),
                          apply(se2, 1, FUN = mean),
                          apply(se3, 1, FUN = mean)),
       caption = "", caption.above = T, digits = 3,
       override.pvalues = list(m1.p, m2.p, m3.p),
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("Consented (2021)", "Excluded Group (2021)",
                              "Skilled (2021)"),
       custom.coef.names = c("Ideology, LATE", "Candidacy, LATE", "Policy, LATE", "Career, LATE"),
       custom.gof.rows = list("Control Mean" = c(mean(control.messages$consented_21, na.rm = T),
                                                 mean(control.messages$excluded_group_21, na.rm = T),
                                                 mean(control.messages$skill_index_21, na.rm = T)), 
                              "Constituency Fixed Effects" = c("Yes", "Yes", "Yes")),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "tableG14_panelB_recall_late_simple.tex"))




# Save models and standard errors
models.table = tidy(m1) %>%
  bind_rows(tidy(m2)) %>%
  bind_rows(tidy(m3)) %>%
  mutate(se_bootstr = c(apply(se1, 1, FUN = mean), apply(se2, 1, FUN = mean),
                        apply(se3, 1, FUN = mean)))

write.csv(models.table, file = paste0(tab.out, "recall_coefficients.csv"))




######## Table G.15: Gender Inclusiveness, Benefits of Joining, and the Retention of Excluded Groups, PANEL A: Recruit characteristics - female treat ###########

m1 = iv_robust(excluded_group_21~ t_fem_takeup |
                 t_fem, data = dta %>% arrange(fem_treat_assigned),
               clusters = assembly_id, se_type = "stata")
m2 = iv_robust(female_21 ~ t_fem_takeup |
                 t_fem, data = dta %>% arrange(fem_treat_assigned),
               clusters = assembly_id, se_type = "stata")
m3 = iv_robust(excluded_caste_21 ~ t_fem_takeup |
                 t_fem, data = dta %>% arrange(fem_treat_assigned),
               clusters = assembly_id, se_type = "stata")
m4 = iv_robust(excluded_religion_21 ~ t_fem_takeup |
                 t_fem, data = dta %>% arrange(fem_treat_assigned),
               clusters = assembly_id, se_type = "stata")


lmlist.excluded = list(m1, m2,  m3, m4)
# # Get clustered standard errors
form = "t_fem_takeup | t_fem"
se1 = parallel_ivclust_func(form = form, dep.var.str = "excluded_group_21", data = dta, fem = 1, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$excluded_group_21)),])
se2 = parallel_ivclust_func(form = form, dep.var.str = "female_21", data = dta, fem = 1, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$female_21)),])
se3 = parallel_ivclust_func(form = form, dep.var.str = "excluded_caste_21", data = dta, fem = 1, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$excluded_caste_21)),])
se4 = parallel_ivclust_func(form = form, dep.var.str = "excluded_religion_21", data = dta, fem = 1, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$excluded_religion_21)),])


selist.excluded = list(se1, se2, se3, se4)
names = c("(Intercept)", "t_fem_takeup")
# Get p-values
m1.p = get_p_values(se = se1, lm = m1, names = names)
m2.p = get_p_values(se = se2, lm = m2, names = names)
m3.p = get_p_values(se = se3, lm = m3, names = names)
m4.p = get_p_values(se = se4, lm = m4, names = names)

# Produce first table
m1.tab = estimatr::extract.iv_robust(m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m2.tab = estimatr::extract.iv_robust(m2, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m3.tab = estimatr::extract.iv_robust(m3, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m4.tab = estimatr::extract.iv_robust(m4, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)

cont.mean.excluded = c(mean(control.fem$excluded_group_21), mean(control.fem$female_21), mean(control.fem$excluded_caste_21), mean(control.fem$excluded_religion_21))


# Print out the table
texreg(l = list(m1.tab, m2.tab, m3.tab, m4.tab),
       override.se = list(apply(se1, 1, FUN = mean),
                          apply(se2, 1, FUN = mean),
                          apply(se3, 1, FUN = mean),
                          apply(se4, 1, FUN = mean)),
       override.pvalues = list(m1.p, m2.p, m3.p, m4.p),
       caption = "", caption.above = T, digits = 3,
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("Excluded Group (2021)", "Woman (2021)",
                              "Excluded Caste (2021)", "Excluded Religion (2021)"),
       custom.coef.names = c("Female Encouragement, LATE"),
       custom.gof.rows = list("Control Mean" = cont.mean.excluded,
                              "Constituency Fixed Effects" = c("No", "No", "No", "No")),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "tableG15_panelA_recall_fem_late_excluded.tex"))


# Save models and standard errors
models.table = tidy(m1) %>%
  bind_rows(tidy(m2)) %>%
  bind_rows(tidy(m3)) %>%
  bind_rows(tidy(m4)) %>%
  mutate(se_bootstr = c(apply(se1, 1, FUN = mean), apply(se2, 1, FUN = mean),
                        apply(se3, 1, FUN = mean),  apply(se4, 1, FUN = mean)))

write.csv(models.table, file = paste0(tab.out, "recall_excluded_fem_coefficients.csv"))


###### "Table G.15: Gender Inclusiveness, Benefits of Joining, and the Retention of Excluded Groups", PANEL B: Recruit characteristics, messages treatment #######

m1 = iv_robust(excluded_group_21~  t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
m2 = iv_robust(female_21 ~ t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
m3 = iv_robust(excluded_caste_21 ~  t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
m4 = iv_robust(excluded_religion_21 ~  t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")



lmlist.excluded = list(m1, m2,  m3, m4)
# Get clustered standard errors
form = "t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup | t_ideo+t_cand+t_poli+t_care"
se1 = parallel_ivclust_func(form = form,  dep.var.str = "excluded_group_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$excluded_group_21)),])
se2 = parallel_ivclust_func(form = form,  dep.var.str = "female_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$female_21)),])
se3 = parallel_ivclust_func(form = form,  dep.var.str = "excluded_caste_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$excluded_caste_21)),])
se4 = parallel_ivclust_func(form = form,  dep.var.str = "excluded_religion_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$excluded_religion_21)),])


selist.excluded = list(se1, se2, se3, se4)
names = c("t_ideo_takeup","t_cand_takeup", "t_poli_takeup", "t_care_takeup")

# Produce first table
cont.mean.excluded = c(mean(control.messages$excluded_group_21),
                       mean(control.messages$female_21),
                       mean(control.messages$excluded_caste_21),
                       mean(control.messages$excluded_religion_21))

# Get p-values
m1.p = get_p_values(se = se1, lm = m1, names = names)
m2.p = get_p_values(se = se2, lm = m2, names = names)
m3.p = get_p_values(se = se3, lm = m3, names = names)
m4.p = get_p_values(se = se4, lm = m4, names = names)


# Produce first table
m1.tab = estimatr::extract.iv_robust(m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m2.tab = estimatr::extract.iv_robust(m2, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m3.tab = estimatr::extract.iv_robust(m3, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m4.tab = estimatr::extract.iv_robust(m4, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)


# Excluded table
texreg(l = list(m1.tab, m2.tab, m3.tab, m4.tab),
       override.se = list(apply(se1, 1, FUN = mean),
                          apply(se2, 1, FUN = mean),
                          apply(se3, 1, FUN = mean),
                          apply(se4, 1, FUN = mean)),
       override.pvalues = list(m1.p, m2.p, m3.p, m4.p),
       caption = "", caption.above = T, digits = 3,
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("Excluded Group (2021)", "Woman (2021)",
                              "Excluded Caste/Tribe (2021)", "Excluded Religion (2021)"),
       custom.coef.names = c("Ideology, LATE","Candidacy, LATE", "Policy, LATE",  "Career, LATE"),
       custom.gof.rows = list("Control Mean" = cont.mean.excluded, 
                              "Constituency Fixed Effects" = c("Yes", "Yes", "Yes", "Yes")),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "tableG15_panelB_recall_late_excluded.tex"))

# Save models and standard errors
models.table = tidy(m1) %>%
  bind_rows(tidy(m2)) %>%
  bind_rows(tidy(m3)) %>%
  bind_rows(tidy(m4)) %>%
    mutate(se_bootstr = c(apply(se1, 1, FUN = mean), apply(se2, 1, FUN = mean),
                        apply(se3, 1, FUN = mean),  apply(se4, 1, FUN = mean)))

write.csv(models.table, file = paste0(tab.out, "recall_excluded_coefficients.csv"))







############# "Table G.16: Gender Inclusiveness, Benefits of Joining, and the Retention of Skilled Recruits", PANEL A: Long-term effect, female treatment #########
m1 = iv_robust(skill_index_21 ~ t_fem_takeup |
                 t_fem, data = dta %>% arrange(fem_treat_assigned),
               clusters = assembly_id, se_type = "stata")
m2 = iv_robust(any_employ_21 ~t_fem_takeup |
                 t_fem, data = dta %>% arrange(fem_treat_assigned),
               clusters = assembly_id, se_type = "stata")
m3 = iv_robust(educ_hi_21 ~ t_fem_takeup |
                 t_fem, data = dta %>% arrange(fem_treat_assigned),
               clusters = assembly_id, se_type = "stata")
m4 = iv_robust(prior_vote_21 ~ t_fem_takeup |
                 t_fem, data = dta %>% arrange(fem_treat_assigned),
               clusters = assembly_id, se_type = "stata")
m5 = iv_robust(prior_volunteer_21 ~ t_fem_takeup |
                 t_fem, data = dta %>% arrange(fem_treat_assigned),
               clusters = assembly_id, se_type = "stata")

lmlist.skills = list(m1, m2, m3, m4, m5)
form = "t_fem_takeup | t_fem"
se1 = parallel_ivclust_func(form = form,  dep.var.str = "skill_index_21", data = dta, fem = 1, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$skill_index_21)),])
se2 = parallel_ivclust_func(form = form,  dep.var.str = "any_employ_21", data = dta, fem = 1, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$any_employ_21)),])
se3 = parallel_ivclust_func(form = form,  dep.var.str = "educ_hi_21", data = dta, fem = 1, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$educ_hi_21)),])
se4 = parallel_ivclust_func(form = form,  dep.var.str = "prior_vote_21", data = dta, fem = 1, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$prior_vote_21)),])
se5 = parallel_ivclust_func(form = form,  dep.var.str = "prior_volunteer_21", data = dta, fem = 1, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$prior_volunteer_21)),])

selist.skills = list(se1, se2, se3, se4, se5)
# Get p-values 
names = c("(Intercept)", "t_fem_takeup")

# Get p-values
m1.p = get_p_values(se = se1, lm = m1, names = names)
m2.p = get_p_values(se = se2, lm = m2, names = names)
m3.p = get_p_values(se = se3, lm = m3, names = names)
m4.p = get_p_values(se = se4, lm = m4, names = names)
m5.p = get_p_values(se = se5, lm = m5, names = names)

# Produce first table
m1.tab = estimatr::extract.iv_robust(m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m2.tab = estimatr::extract.iv_robust(m2, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m3.tab = estimatr::extract.iv_robust(m3, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m4.tab = estimatr::extract.iv_robust(m4, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m5.tab = estimatr::extract.iv_robust(m5, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)


cont.mean.skills = c(mean(control.fem$skill_index_21),
                     mean(control.fem$any_employ_21),
                     mean(control.fem$educ_hi_21),
                     mean(control.fem$prior_vote_21),
                     mean(control.fem$prior_volunteer_21))

# Skills table
texreg(l = list(m1.tab, m2.tab, m3.tab, m4.tab, m5.tab),
       override.se = list(apply(se1, 1, FUN = mean),
                          apply(se2, 1, FUN = mean),
                          apply(se3, 1, FUN = mean),
                          apply(se4, 1, FUN = mean),
                          apply(se5, 1, FUN = mean)),
       override.pvalues = list(m1.p, m2.p, m3.p, m4.p, m5.p),
       caption = "", caption.above = T, digits = 3,
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c( "Skilled (2021)", "Employed (2021)", "High Educ. (2021)",
                               "Prior Vote (2021)",
                               "Prior Volunteer (2021)"),
       custom.coef.names = c("Female, LATE"),
       custom.gof.rows = list("Control Mean" = cont.mean.skills, 
                              "Constituency Fixed Effects" = c("No", "No", "No", "No", "No")),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "tableG16_panelA_recall_fem_late_skills.tex"))




# Save models and standard errors
models.table = tidy(m1) %>%
  bind_rows(tidy(m2), tidy(m3), tidy(m4), tidy(m5)) %>%
  mutate(se_bootstr = c(apply(se1, 1, FUN = mean), apply(se2, 1, FUN = mean),
                        apply(se3, 1, FUN = mean),  apply(se4, 1, FUN = mean),
                        apply(se5, 1, FUN = mean)))

write.csv(models.table, file = paste0(tab.out, "recall_skills_fem_coefficients.csv"))








###### "Table G.16: Gender Inclusiveness, Benefits of Joining, and the Retention of Skilled Recruits", PANEL B: Long-term effect on skills of recruits #########

# # Skills
m1 = iv_robust(skill_index_21 ~  t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
m2 = iv_robust(any_employ_21 ~  t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
m3 = iv_robust(educ_hi_21 ~  t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
m4 = iv_robust(prior_vote_21 ~  t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")
m5 = iv_robust(prior_volunteer_21 ~  t_ideo_takeup+t_cand_takeup+t_poli_takeup+t_care_takeup|
                 t_ideo+t_cand+t_poli+t_care, fixed_effects = ~ assembly_id,
               clusters = id_vp2, data = dta, se_type = "stata")

lmlist.skills = list(m1, m2, m3, m4, m5)
form = "t_ideo_takeup+t_cand_takeup+t_care_takeup+t_poli_takeup | t_ideo+t_cand+t_poli+t_care"
se1 = parallel_ivclust_func(form = form,  dep.var.str = "skill_index_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$skill_index_21)),])
se2 = parallel_ivclust_func(form = form,  dep.var.str = "any_employ_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$any_employ_21)),])
se3 = parallel_ivclust_func(form = form,  dep.var.str = "educ_hi_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$educ_hi_21)),])
se4 = parallel_ivclust_func(form = form,  dep.var.str = "prior_vote_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$prior_vote_21)),])
se5 = parallel_ivclust_func(form = form,  dep.var.str = "prior_volunteer_21", data = dta, fem = 0, cl.no = 5, cluster.mat = prob.mat[which(!is.na(dta$prior_volunteer_21)),])

selist.skills = list(se1, se2, se3, se4, se5)
names = c("t_poli_takeup", "t_cand_takeup", "t_care_takeup", "t_ideo_takeup")



# Get p-values
m1.p = get_p_values(se = se1, lm = m1, names = names)
m2.p = get_p_values(se = se2, lm = m2, names = names)
m3.p = get_p_values(se = se3, lm = m3, names = names)
m4.p = get_p_values(se = se4, lm = m4, names = names)
m5.p = get_p_values(se = se5, lm = m5, names = names)

# Produce first table
m1.tab = estimatr::extract.iv_robust(m1, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m2.tab = estimatr::extract.iv_robust(m2, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m3.tab = estimatr::extract.iv_robust(m3, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m4.tab = estimatr::extract.iv_robust(m4, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)
m5.tab = estimatr::extract.iv_robust(m5, include.adjrs = FALSE, include.rsquared = FALSE, include.df = FALSE, include.nobs = T,
                                     include.rmse = F, include.nclusts = T, include.ci = F)




cont.mean.skills = c(mean(control.messages$skill_index_21), mean(control.messages$any_employ_21), mean(control.messages$educ_hi_21), mean(control.messages$prior_vote_21), mean(control.messages$prior_volunteer_21))



# Skills table

texreg(l = list(m1.tab, m2.tab, m3.tab, m4.tab, m5.tab),
       override.se = list(apply(se1, 1, FUN = mean),
                          apply(se2, 1, FUN = mean),
                          apply(se3, 1, FUN = mean),
                          apply(se4, 1, FUN = mean),
                          apply(se5, 1, FUN = mean)),
       override.pvalues = list(m1.p, m2.p, m3.p, m4.p, m5.p),
       caption = "", caption.above = T, digits = 3,
       naive = T, include.ci = F, omit.coef = c("as.factor|Intercept"), out.header = F,
       custom.model.names = c("Skilled (2021)", "Employed (2021)", "High Educ. (2021)",
                               "Prior Vote (2021)","Prior Volunteer (2021)"),
       custom.coef.names = c("Ideology, LATE", "Candidacy, LATE", "Policy, LATE", "Career, LATE"),
       custom.gof.rows = list("Control Mean" = cont.mean.skills, 
                              "Constituency Fixed Effects" = c("Yes", "Yes", "Yes", "Yes", "Yes")),
       stars = c(0.01, 0.05, 0.1), 
       file = paste0(tab.out, "tableG16_panelB_recall_late_skills.tex"))


# Save models and standard errors
models.table = tidy(m1) %>%
  bind_rows(tidy(m2), tidy(m3), tidy(m4), tidy(m5)) %>%
  mutate(se_bootstr = c(apply(se1, 1, FUN = mean), apply(se2, 1, FUN = mean),
                        apply(se3, 1, FUN = mean),  apply(se4, 1, FUN = mean),
                        apply(se5, 1, FUN = mean)))

write.csv(models.table, file = paste0(tab.out, "recall_skills_coefficients.csv"))




# Stop cluster
registerDoSEQ()
stopCluster(cluster)







