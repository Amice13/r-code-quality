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


############### SIMULATION EXERCISE V2 - 90:10 ################################

set.seed(20191101)
### First, take the demographic information from matched pamphlets
simu <- dta
simu$rowid = 1:nrow(simu)
simu <- simu %>%
  mutate(gender_sim = case_when(
    missedcall==1000 & female==1000 ~ "woman", 
    missedcall==1000 & female==0 ~ "man", 
    missedcall==0 ~ NA_character_
  ))



simu_f_pamph <- simu[simu$t_fem==1, ]
simu_m_pamph <- simu[simu$t_fem==0, ]

simulation_function <- function(ratio, w_prop_m_p){
  
  
  
  ### Now, let's create different scenarios for the distribution of potential volunteers
  missedcall_w_index_f_pamph <- which(simu_f_pamph$gender_sim=="woman")
  missedcall_m_index_f_pamph <- which(simu_f_pamph$gender_sim=="man")
  missedcall_w_index_m_pamph <- which(simu_m_pamph$gender_sim=="woman")
  missedcall_m_index_m_pamph <- which(simu_m_pamph$gender_sim=="man")
  
  
  
  # set the prop of women under both pamphlet types
  w_prop_m_pamph <- w_prop_m_p
  w_prop_f_pamph <- w_prop_m_p*(1+ratio)
  
  
  m_pamph <- nrow(simu_m_pamph)
  f_pamph <- nrow(simu_f_pamph)
  
  # number of potential volunteers
  total_w_number_f_pamph <- floor(w_prop_f_pamph*f_pamph)
  total_w_number_m_pamph <- floor(w_prop_m_pamph*m_pamph)
  
  
  
  # remaining number of potential felame respondents who did not give a missedcall
  remaining_w_number_f_pamph <- total_w_number_f_pamph-length(missedcall_w_index_f_pamph)
  remaining_w_number_m_pamph <- total_w_number_m_pamph-length(missedcall_w_index_m_pamph)
  
  # get the indeces for potential female respondents for female pamphlets
  #rownames_f_pamph <- rownames(simu_f_pamph) %>% as.numeric()
  rownames_f_pamph <- 1:nrow(simu_f_pamph)
  remaining_rows_f_pamph <- rownames_f_pamph[-c(missedcall_w_index_f_pamph, missedcall_m_index_f_pamph)]
  woman_index_f_pamph <- sample(remaining_rows_f_pamph, size = remaining_w_number_f_pamph, replace= F)
  simu_f_pamph[woman_index_f_pamph,"gender_sim"] <- "woman"
  simu_f_pamph <- simu_f_pamph %>%
    mutate(gender_sim = ifelse(is.na(gender_sim), "man", gender_sim))
  table(simu_f_pamph$gender_sim)
  
  
  # get the indeces for potential female respondents for female pamphlets
  rownames_m_pamph <- 1:nrow(simu_m_pamph)
  remaining_rows_m_pamph <- rownames_m_pamph[-c(missedcall_w_index_m_pamph, missedcall_m_index_m_pamph)]
  woman_index_m_pamph <- sample(remaining_rows_m_pamph, size = remaining_w_number_m_pamph, replace= F)
  simu_m_pamph[woman_index_m_pamph,"gender_sim"] <- "woman"
  simu_m_pamph <- simu_m_pamph %>%
    mutate(gender_sim = ifelse(is.na(gender_sim), "man", gender_sim))
  table(simu_m_pamph$gender_sim)
  
  simu_fin <- bind_rows(simu_f_pamph, simu_m_pamph)
  
  
  
  # Now analyze the impact of different treatments
  f.m = lh_robust(missedcall ~ t_fem*as.factor(gender_sim),
                  data = simu_fin %>% arrange(fem_treat_assigned),
                  clusters = assembly_id, se_type = "stata", 
                  linear_hypothesis = "t_fem + t_fem:as.factor(gender_sim)woman = 0")
  
  # define ratio of men to women
  mw_ratio_f_pamph <- (1-w_prop_f_pamph)/w_prop_f_pamph
  mw_ratio_m_pamph <- (1-w_prop_m_pamph)/w_prop_m_pamph
  women_prop_f_m_pamp <- ratio
  
  res = tidy(f.m$lm_robust) %>%
    bind_rows(tidy(f.m$lh)) %>%
    mutate(mw_ratio_f_pamph = mw_ratio_f_pamph) %>%
    mutate(mw_ration_m_pamph = mw_ratio_m_pamph) %>%
    mutate(women_prop_f_m_pamp = women_prop_f_m_pamp) 
  
  return(res)
  
}

ratios <- seq(-0.5, 0.75, by = 0.05)

# simulation function 
sim_res = lapply(X = ratios, FUN = function(x){simulation_function(ratio = x, w_prop_m_p = 0.1)})
sim_res = bind_rows(sim_res[[1]], sim_res[[2]], sim_res[[3]],
                    sim_res[[4]], sim_res[[5]], sim_res[[6]], 
                    sim_res[[7]], sim_res[[8]], sim_res[[9]], 
                    sim_res[[10]], sim_res[[11]], sim_res[[12]],
                    sim_res[[13]], sim_res[[14]], sim_res[[15]],
                    sim_res[[16]], sim_res[[17]], sim_res[[18]], 
                    sim_res[[19]], sim_res[[20]], sim_res[[21]], 
                    sim_res[[22]], sim_res[[23]], sim_res[[24]],
                    sim_res[[25]], sim_res[[26]]) %>% as.data.frame()



# Now ggplot t_fem results
library(ggpubr)
library(scales)
sim_res %>%
  filter(women_prop_f_m_pamp >=-0.25 & women_prop_f_m_pamp <= 0.5) %>%
  filter(term=="t_fem"|term=="t_fem + t_fem:as.factor(gender_sim)woman = 0") %>%
  mutate(effect_type = ifelse(term=="t_fem", "Men", "Women")) %>%
  ggplot(., aes(x = women_prop_f_m_pamp, y = estimate))+
  geom_hline(yintercept = 1.581,  color = "navyblue")+
  geom_point(aes(color = effect_type), 
             size = 2.5, position = position_dodge(width = 0.05), alpha = 0.7)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, 
                    color = effect_type), alpha = 0.7,
                size = 1.5, width = 0, position = position_dodge(width = 0.05))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray48")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray48")+
  labs(x = "% More Women Approached Under Female Pamphlet ", y = "")+
  guides(color = guide_legend(title = "Gender-Inclusive Heterogeneous Treatment Effect For:"))+
  scale_x_continuous(labels = scales::percent, breaks = seq(-0.5, 0.75, 0.25))+
  scale_color_manual(values = c("#56B4E9", "#009E73"))+
  theme_pubr()+
  theme(axis.title =  element_text(size = 17), 
        axis.text = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14))
ggsave(paste0(fig.out, "figureE8_simulation_women_90_10.pdf"),
       width = 11.4, height = 5.65, dpi = 1000)



############### SIMULATION EXERCISE V2 - 80:20 ################################

set.seed(20191101)
### First, take the demographic information from matched pamphlets
simu <- dta
simu$rowid = 1:nrow(simu)
simu <- simu %>%
  mutate(gender_sim = case_when(
    missedcall==1000 & female==1000 ~ "woman", 
    missedcall==1000 & female==0 ~ "man", 
    missedcall==0 ~ NA_character_
  ))


set.seed(20191101)
simu_f_pamph <- simu[simu$t_fem==1, ]
simu_m_pamph <- simu[simu$t_fem==0, ]

ratios <- seq(-0.5, 0.75, by = 0.05)

# simulation function 
sim_res = lapply(X = ratios, FUN = function(x){simulation_function(ratio = x, w_prop_m_p = 0.2)})
sim_res = bind_rows(sim_res[[1]], sim_res[[2]], sim_res[[3]],
                    sim_res[[4]], sim_res[[5]], sim_res[[6]], 
                    sim_res[[7]], sim_res[[8]], sim_res[[9]], 
                    sim_res[[10]], sim_res[[11]], sim_res[[12]],
                    sim_res[[13]], sim_res[[14]], sim_res[[15]],
                    sim_res[[16]], sim_res[[17]], sim_res[[18]], 
                    sim_res[[19]], sim_res[[20]], sim_res[[21]], 
                    sim_res[[22]], sim_res[[23]], sim_res[[24]],
                    sim_res[[25]], sim_res[[26]]) %>% as.data.frame()


# Now ggplot t_fem results
library(ggpubr)
library(scales)
sim_res %>%
  filter(women_prop_f_m_pamp >=-0.25 & women_prop_f_m_pamp <= 0.5) %>%
  filter(term=="t_fem"|term=="t_fem + t_fem:as.factor(gender_sim)woman = 0") %>%
  mutate(effect_type = ifelse(term=="t_fem", "Men", "Women")) %>%
  ggplot(., aes(x = women_prop_f_m_pamp, y = estimate))+
  geom_hline(yintercept = 1.581,  color = "navyblue")+
  geom_point(aes(color = effect_type), 
             size = 2.5, position = position_dodge(width = 0.05), alpha = 0.7)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, 
                    color = effect_type), alpha = 0.7,
                size = 1.5, width = 0, position = position_dodge(width = 0.05))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray48")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray48")+
  labs(x = "% More Women Approached Under Female Pamphlet ", y = "")+
  guides(color = guide_legend(title = "Gender-Inclusive Heterogeneous Treatment Effect For:"))+
  scale_x_continuous(labels = scales::percent, breaks = seq(-0.5, 0.75, 0.25))+
  scale_color_manual(values = c("#56B4E9", "#009E73"))+
  theme_pubr()+
  theme(axis.title =  element_text(size = 17), 
        axis.text = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14))
ggsave(paste0(fig.out, "figureE8_simulation_women_80_20.pdf"),
       width = 11.4, height = 5.65, dpi = 1000)



############### SIMULATION EXERCISE V2 - 70:30 ################################

set.seed(20191101)
### First, take the demographic information from matched pamphlets
simu <- dta
simu$rowid = 1:nrow(simu)
simu <- simu %>%
  mutate(gender_sim = case_when(
    missedcall==1000 & female==1000 ~ "woman", 
    missedcall==1000 & female==0 ~ "man", 
    missedcall==0 ~ NA_character_
  ))


set.seed(20191101)
simu_f_pamph <- simu[simu$t_fem==1, ]
simu_m_pamph <- simu[simu$t_fem==0, ]

ratios <- seq(-0.5, 0.75, by = 0.05)

# simulation function 
sim_res = lapply(X = ratios, FUN = function(x){simulation_function(ratio = x, w_prop_m_p = 0.3)})
sim_res = bind_rows(sim_res[[1]], sim_res[[2]], sim_res[[3]],
                    sim_res[[4]], sim_res[[5]], sim_res[[6]], 
                    sim_res[[7]], sim_res[[8]], sim_res[[9]], 
                    sim_res[[10]], sim_res[[11]], sim_res[[12]],
                    sim_res[[13]], sim_res[[14]], sim_res[[15]],
                    sim_res[[16]], sim_res[[17]], sim_res[[18]], 
                    sim_res[[19]], sim_res[[20]], sim_res[[21]], 
                    sim_res[[22]], sim_res[[23]], sim_res[[24]],
                    sim_res[[25]], sim_res[[26]]) %>% as.data.frame()


# Now ggplot t_fem results
library(ggpubr)
library(scales)
sim_res %>%
  filter(women_prop_f_m_pamp >=-0.25 & women_prop_f_m_pamp <= 0.5) %>%
  filter(term=="t_fem"|term=="t_fem + t_fem:as.factor(gender_sim)woman = 0") %>%
  mutate(effect_type = ifelse(term=="t_fem", "Men", "Women")) %>%
  ggplot(., aes(x = women_prop_f_m_pamp, y = estimate))+
  geom_hline(yintercept = 1.581,  color = "navyblue")+
  geom_point(aes(color = effect_type), 
             size = 2.5, position = position_dodge(width = 0.05), alpha = 0.7)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, 
                    color = effect_type), alpha = 0.7,
                size = 1.5, width = 0, position = position_dodge(width = 0.05))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray48")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray48")+
  labs(x = "% More Women Approached Under Female Pamphlet ", y = "")+
  guides(color = guide_legend(title = "Gender-Inclusive Heterogeneous Treatment Effect For:"))+
  scale_x_continuous(labels = scales::percent, breaks = seq(-0.5, 0.75, 0.25))+
  scale_color_manual(values = c("#56B4E9", "#009E73"))+
  theme_pubr()+
  theme(axis.title =  element_text(size = 17), 
        axis.text = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14))
ggsave(paste0(fig.out, "figureE8_simulation_women_70_30.pdf"),
       width = 11.4, height = 5.65, dpi = 1000)





############### SIMULATION EXERCISE V2 - 60:40 ################################

set.seed(20191101)
### First, take the demographic information from matched pamphlets
simu <- dta
simu$rowid = 1:nrow(simu)
simu <- simu %>%
  mutate(gender_sim = case_when(
    missedcall==1000 & female==1000 ~ "woman", 
    missedcall==1000 & female==0 ~ "man", 
    missedcall==0 ~ NA_character_
  ))


set.seed(20191101)
simu_f_pamph <- simu[simu$t_fem==1, ]
simu_m_pamph <- simu[simu$t_fem==0, ]

ratios <- seq(-0.5, 0.75, by = 0.05)

# simulation function 
sim_res = lapply(X = ratios, FUN = function(x){simulation_function(ratio = x, w_prop_m_p = 0.4)})
sim_res = bind_rows(sim_res[[1]], sim_res[[2]], sim_res[[3]],
                    sim_res[[4]], sim_res[[5]], sim_res[[6]], 
                    sim_res[[7]], sim_res[[8]], sim_res[[9]], 
                    sim_res[[10]], sim_res[[11]], sim_res[[12]],
                    sim_res[[13]], sim_res[[14]], sim_res[[15]],
                    sim_res[[16]], sim_res[[17]], sim_res[[18]], 
                    sim_res[[19]], sim_res[[20]], sim_res[[21]], 
                    sim_res[[22]], sim_res[[23]], sim_res[[24]],
                    sim_res[[25]], sim_res[[26]]) %>% as.data.frame()


# Now ggplot t_fem results
library(ggpubr)
library(scales)
sim_res %>%
  filter(women_prop_f_m_pamp >=-0.25 & women_prop_f_m_pamp <= 0.5) %>%
  filter(term=="t_fem"|term=="t_fem + t_fem:as.factor(gender_sim)woman = 0") %>%
  mutate(effect_type = ifelse(term=="t_fem", "Men", "Women")) %>%
  ggplot(., aes(x = women_prop_f_m_pamp, y = estimate))+
  geom_hline(yintercept = 1.581,  color = "navyblue")+
  geom_point(aes(color = effect_type), 
             size = 2.5, position = position_dodge(width = 0.05), alpha = 0.7)+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, 
                    color = effect_type), alpha = 0.7,
                size = 1.5, width = 0, position = position_dodge(width = 0.05))+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray48")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray48")+
  labs(x = "% More Women Approached Under Female Pamphlet ", y = "")+
  guides(color = guide_legend(title = "Gender-Inclusive Heterogeneous Treatment Effect For:"))+
  scale_x_continuous(labels = scales::percent, breaks = seq(-0.5, 0.75, 0.25))+
  scale_color_manual(values = c("#56B4E9", "#009E73"))+
  theme_pubr()+
  theme(axis.title =  element_text(size = 17), 
        axis.text = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14))
ggsave(paste0(fig.out, "figureE8_simulation_women_60_40.pdf"),
       width = 11.4, height = 5.65, dpi = 1000)


# Stop cluster
registerDoSEQ()
stopCluster(cluster)