library(tidyverse)
library(bayesplot)
library(brms)
library(bayestestR)
library(loo)
library(knitr)
library(patchwork)
library(countrycode)
library(tidybayes)
library(mice)
library(gt)
library(scales)
library(sjPlot)

df <- readRDS("data/societal_determinants_flood-induced_displacement.rds")

df$continent <- countrycode(df$gwcode, origin = "gwn", destination = "continent")
df$continent <- if_else(df$gwcode == 816, "Asia", df$continent)
df$continent <- if_else(df$gwcode == 345, "Europe", df$continent)
df$continent <- if_else(df$gwcode == 678, "Asia", df$continent)

df$continent <- if_else(df$continent == "Oceania", "Asia and Oceania", df$continent)
df$continent <- if_else(df$continent == "Asia", "Asia and Oceania", df$continent)

#### 1) Dependent variable: displaced_w, dead_w ###
DEPVAR <- "displaced_w"
#### 2) Data selection: all data, leave out where dead_w >10 & displaced_w == 0 ###
DATA_SELECTION <- "alldata" # "dnd" or "alldata" (dead_no_displaced)
#### 3) Add random effects on continents?
RANDOM_EFFECTS <- "re"

#### Log-transform predictors ####
# This is not really an option, as the models do not converge if we do not
log_vars <- c("population_affected", "duration", "displaced_sum10", "dead_sum10", "nevents_sum10",
              "brd_during", "brd_12mb", "brd_6ma", "brd_10yb", "brd_6mb", 
              "ne_during", "ne_12mb", "ne_6ma", "ne_10yb", "ne_6mb",
              "casualties_brd_l1", "casualties_brd_sum10",
              "wdi_gdppc", "nlightsmean", "perc_post_secondary")
df <- df %>% mutate(across(all_of(log_vars), .fns = function(x) log(x+1)))

missing_df <- df %>% filter(displaced_w == 0)

df <- df %>% filter(displaced_w > 0)

df %>% group_by(continent) %>% summarise(mean(displaced_w, na.rm = T)) # Continent seems relevant...

#### Split train, test ####
train <- df %>% dplyr::filter(began < as.Date("2015-01-01"))
test <- df %>% dplyr::filter(began >= as.Date("2015-01-01")) 

train %>% mutate(displaced_lt_affected = (exp(population_affected)-1)*10 < displaced_w) %>% pull(displaced_lt_affected) %>% mean(na.rm = T)

# if(DATA_SELECTION == "dnd"){
#   train <- train %>% dplyr::filter(!(population_affected == 0 & displaced_w > 0)) # If there are zero affected, there should be zero displaced. (assume these are erroneously coded)
#   train <- train %>% dplyr::filter(!((exp(population_affected)-1) * 10 < displaced_w)) # If there are 10 times fewer affected than displaced, we believe they are dubious observations 
# }

# m1 <- lm(log(depvar+1) ~ log(population_affected+1) + log(duration+1) + log(brd_6mb+1) + log(wdi_gdppc) + log(nlightsmean+1), data = train)
# p1 <- glm(depvar ~ log(population_affected+1) + log(duration+1) + log(brd_6mb+1) + log(wdi_gdppc) + log(nlightsmean+1), data = train, family = poisson(link = "log"))
# z1 <- pscl::zeroinfl(depvar ~ population_affected + duration + brd_6mb + wdi_gdppc + nlightsmean, data = train, dist = "poisson")

#### Set up formulas we want to use ####

f_base <- formula(depvar ~ population_affected + duration + nevents_sum10)
f_conflict <- update(f_base, . ~ . + brd_6mb + casualties_brd_sum10)
f_economy <- update(f_base, . ~ . + nlightsmean + wdi_gdppc)
f_politics <- update(f_base, . ~ . + I(excluded_share>0) + regime_type)
f_conflict_economy <- update(f_base, . ~ . + brd_6mb + casualties_brd_sum10 + 
                               nlightsmean + wdi_gdppc)
f_conflict_politics <- update(f_base, . ~ . + brd_6mb + casualties_brd_sum10 + I(excluded_share>0) + regime_type)
f_economy_politics <- update(f_base, . ~ . + nlightsmean + wdi_gdppc + I(excluded_share>0) + regime_type)
f_conflict_politics_economy <- update(f_base, . ~ . + brd_6mb + casualties_brd_sum10 + nlightsmean + wdi_gdppc + I(excluded_share>0) + regime_type)
f_all <- update(f_base, . ~ . + brd_6mb + casualties_brd_sum10 + nlightsmean + wdi_gdppc + I(excluded_share>0) + regime_type)


# f_base <- formula(depvar ~ population_affected + duration + nevents_sum10)
# f_conflict <- update(f_base, . ~ . + casualties_brd_sum10)
# f_economy <- update(f_base, . ~ . + wdi_gdppc)
# f_politics <- update(f_base, . ~ . + regime_type)
# f_conflict_economy <- update(f_base, . ~ . + casualties_brd_sum10 + 
#                                wdi_gdppc)
# f_conflict_politics <- update(f_base, . ~ . + casualties_brd_sum10 + regime_type)
# f_economy_politics <- update(f_base, . ~ . + wdi_gdppc + regime_type)
# f_conflict_politics_economy <- update(f_base, . ~ . + casualties_brd_sum10 + wdi_gdppc + regime_type)
# f_all <- update(f_base, . ~ . + casualties_brd_sum10 + wdi_gdppc + regime_type)


all_formulas <- list(f_base, f_conflict, f_economy, f_politics, f_conflict_economy, f_conflict_politics, f_economy_politics, f_conflict_politics_economy, f_all)
names(all_formulas) <- c("f_base", "f_conflict", "f_economy", "f_politics", "f_conflict_economy", "f_conflict_politics", "f_economy_politics", "f_conflict_politics_economy", "f_all")

# Change dependent variable based on DEPVAR selection
all_formulas <- lapply(all_formulas, function(x) update(x, paste0(DEPVAR," ~ .")))

# Model missing values on dependent variable
#all_formulas <- lapply(all_formulas, function(x) update(x, paste0(DEPVAR,"| mi()  ~ .")))

if(RANDOM_EFFECTS == "re"){
  all_formulas <- lapply(all_formulas, function(x) update(x, . ~ . + (1|continent)))  
}


SEEDNUM <- 42
options("mc.cores" = 8)

TRAIN_MODELS <- TRUE

if(TRAIN_MODELS){
  f_nb <- brm(displaced_w ~ population_affected + duration + nevents_sum10, 
              data = train, family = negbinomial(), chains = 8, iter = 3000, 
              control = list(adapt_delta = 0.99, max_treedepth = 15), 
              cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb2 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + (1|continent), 
               data = train, family = negbinomial(), chains = 8, iter = 3000, 
               control = list(adapt_delta = 0.99, max_treedepth = 15), 
               cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb3 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + (1 + population_affected + duration + nevents_sum10|continent), 
               data = train, family = negbinomial(), chains = 8, iter = 3000, 
               control = list(adapt_delta = 0.99, max_treedepth = 15), 
               cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb4 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + brd_6mb + 
                 casualties_brd_sum10 + nlightsmean + wdi_gdppc + I(excluded_share > 0) + regime_type + (1 + population_affected + duration + nevents_sum10|continent), 
               data = train, family = negbinomial(), chains = 8, iter = 3000, 
               control = list(adapt_delta = 0.99, max_treedepth = 15), 
               cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb5 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + 
                 casualties_brd_sum10 + wdi_gdppc+ regime_type + (1 + population_affected + duration + nevents_sum10|continent), 
               data = train, family = negbinomial(), chains = 8, iter = 3000, 
               control = list(adapt_delta = 0.99, max_treedepth = 15), 
               cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb6 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + brd_6mb + 
                 nlightsmean + I(excluded_share > 0) + (1 + population_affected + duration + nevents_sum10|continent), 
               data = train, family = negbinomial(), chains = 8, iter = 3000, 
               control = list(adapt_delta = 0.99, max_treedepth = 15), 
               cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb7 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + brd_6mb + 
                 casualties_brd_sum10 + (1 + population_affected + duration + nevents_sum10|continent), 
               data = train, family = negbinomial(), chains = 8, iter = 3000, 
               control = list(adapt_delta = 0.99, max_treedepth = 15), 
               cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb8 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + 
                 nlightsmean + wdi_gdppc + (1 + population_affected + duration + nevents_sum10|continent), 
               data = train, family = negbinomial(), chains = 8, iter = 3000, 
               control = list(adapt_delta = 0.99, max_treedepth = 15), 
               cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb9 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + I(excluded_share > 0) + regime_type + (1 + population_affected + duration + nevents_sum10|continent), 
               data = train, family = negbinomial(), chains = 8, iter = 3000, 
               control = list(adapt_delta = 0.99, max_treedepth = 15), 
               cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb10 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + brd_6mb + 
                  casualties_brd_sum10 + nlightsmean + wdi_gdppc + (1 + population_affected + duration + nevents_sum10|continent), 
                data = train, family = negbinomial(), chains = 8, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 15), 
                cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb11 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + 
                  nlightsmean + wdi_gdppc + I(excluded_share > 0) + regime_type + (1 + population_affected + duration + nevents_sum10|continent), 
                data = train, family = negbinomial(), chains = 8, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 15), 
                cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb12 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + brd_6mb + 
                  casualties_brd_sum10 + I(excluded_share > 0) + regime_type + (1 + population_affected + duration + nevents_sum10|continent), 
                data = train, family = negbinomial(), chains = 8, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 15), 
                cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  
  f_nb13 <- brm(displaced_w ~ t2(population_affected, duration) + nevents_sum10 + brd_6mb + 
                  casualties_brd_sum10 + nlightsmean + wdi_gdppc + I(excluded_share > 0) + regime_type + (1|continent), 
                data = train, family = negbinomial(), chains = 8, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 15), 
                cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb14 <- brm(displaced_w ~ t2(population_affected, duration) + nevents_sum10 + 
                  casualties_brd_sum10 + wdi_gdppc + regime_type + (1|continent), 
                data = train, family = negbinomial(), chains = 8, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 15), 
                cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb15 <- brm(displaced_w ~ t2(population_affected, duration) + nevents_sum10 + brd_6mb + 
                  nlightsmean + I(excluded_share > 0) + (1|continent), 
                data = train, family = negbinomial(), chains = 8, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 15), 
                cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  
  f_nb16 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + brd_6mb + 
                  casualties_brd_sum10 + nlightsmean + wdi_gdppc + I(excluded_share > 0) + regime_type, 
                data = train, family = negbinomial(), chains = 8, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 15), 
                cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb17 <- brm(displaced_w ~ population_affected + duration + nevents_sum10  + 
                  casualties_brd_sum10 + wdi_gdppc + regime_type, 
                data = train, family = negbinomial(), chains = 8, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 15), 
                cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  f_nb18 <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + brd_6mb + 
                  nlightsmean + I(excluded_share > 0), 
                data = train, family = negbinomial(), chains = 8, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 15), 
                cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  
  f_nb19 <- brm(displaced_w ~ t2(population_affected, duration) + nevents_sum10 + (1|continent), 
                data = train, family = negbinomial(), chains = 8, iter = 3000, 
                control = list(adapt_delta = 0.99, max_treedepth = 15), 
                cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = SEEDNUM)
  
  
  saveRDS(list("f_nb" = f_nb,
               "f_nb2" = f_nb2,
               "f_nb3" = f_nb3,
               "f_nb4" = f_nb4,
               "f_nb5" = f_nb5,
               "f_nb6" = f_nb6,
               "f_nb7" = f_nb7,
               "f_nb8" = f_nb8,
               "f_nb9" = f_nb9,
               "f_nb10" = f_nb10,
               "f_nb11" = f_nb11,
               "f_nb12" = f_nb12,
               "f_nb13" = f_nb13,
               "f_nb14" = f_nb14,
               "f_nb15" = f_nb15,
               "f_nb16" = f_nb16,
               "f_nb17" = f_nb17,
               "f_nb18" = f_nb18,
               "f_nb19" = f_nb19), file = paste0("brm_results/brmfits-", DEPVAR, "-", DATA_SELECTION, "-", RANDOM_EFFECTS, ".rds"))
}


all_models <- readRDS("brm_results/brmfits-displaced_w-alldata-re.rds")

fit_df <- tibble("fits" = all_models)
fit_df$mname <- names(all_models)
fit_df$depvar <- "displaced_w"
fit_df$selection <- "alldata"
fit_df$formula <- c("base", "base+ri", "base+ri+rs", "full+ri+rs", "c-lvl+ri+rs", "l-lvl+ri+rs", "conflict+ri+rs", "economy+ri+rs", "politics+ri+rs", 
                    "c-e+ri+rs", "e-p+ri+rs", "c-p+ri+rs", "full+ri+t2", "c-lvl+ri+t2", "l-lvl+ri+t2", "full", "c-lvl", "l-lvl", "base+ri+t2")

#### Adding a year trend improves the general fit, but the same pattern for base and full set of structural predictors for in-sample and out-of-sample can be found

f_nb3t <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + year + (1 + population_affected + duration + nevents_sum10|continent), 
             data = train, family = negbinomial(), chains = 8, iter = 3000, 
             control = list(adapt_delta = 0.99, max_treedepth = 15), 
             cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = 442)

f_nb4t <- brm(displaced_w ~ population_affected + duration + nevents_sum10 + brd_6mb + 
               casualties_brd_sum10 + nlightsmean + wdi_gdppc + I(excluded_share > 0) + regime_type + year + (1 + population_affected + duration + nevents_sum10|continent), 
             data = train, family = negbinomial(), chains = 8, iter = 3000, 
             control = list(adapt_delta = 0.99, max_treedepth = 15), 
             cores = getOption("mc.cores"), save_pars = save_pars(all  = TRUE), seed = 42)


model_weights(fit_df$fits$f_nb4,
                     fit_df$fits$f_nb3,
                     f_nb3t,
                     f_nb4t)
loo(fit_df$fits$f_nb3,
            fit_df$fits$f_nb4,
            f_nb3t,
            f_nb4t)

model_weights(
  fit_df$fits$f_nb3,
  fit_df$fits$f_nb4,
  f_nb3t,
  f_nb4t,  newdata = test, allow_new_levels = TRUE)

loo(
  fit_df$fits$f_nb3,
  fit_df$fits$f_nb4,
  f_nb3t,
  f_nb4t,  newdata = test, allow_new_levels = TRUE)


#### Calculate predictive performance, model selection ####
mw_res <- model_weights(fit_df$fits$f_nb,
                        fit_df$fits$f_nb2,
                        fit_df$fits$f_nb3,
                        fit_df$fits$f_nb4,
                        fit_df$fits$f_nb5,
                        fit_df$fits$f_nb6,
                        fit_df$fits$f_nb7,
                        fit_df$fits$f_nb8,
                        fit_df$fits$f_nb9,
                        fit_df$fits$f_nb10,
                        fit_df$fits$f_nb11,
                        fit_df$fits$f_nb12,
                        fit_df$fits$f_nb13,
                        fit_df$fits$f_nb14,
                        fit_df$fits$f_nb15,
                        fit_df$fits$f_nb16,
                        fit_df$fits$f_nb17,
                        fit_df$fits$f_nb18,
                        fit_df$fits$f_nb19)
names(mw_res) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res)) %>% match(., fit_df$mname)]

mw_res_oos <- model_weights(fit_df$fits$f_nb,
                        fit_df$fits$f_nb2,
                        fit_df$fits$f_nb3,
                        fit_df$fits$f_nb4,
                        fit_df$fits$f_nb5,
                        fit_df$fits$f_nb6,
                        fit_df$fits$f_nb7,
                        fit_df$fits$f_nb8,
                        fit_df$fits$f_nb9,
                        fit_df$fits$f_nb10,
                        fit_df$fits$f_nb11,
                        fit_df$fits$f_nb12,
                        fit_df$fits$f_nb13,
                        fit_df$fits$f_nb14,
                        fit_df$fits$f_nb15,
                        fit_df$fits$f_nb16,
                        fit_df$fits$f_nb17,
                        fit_df$fits$f_nb18,
                        fit_df$fits$f_nb19, newdata = test, allow_new_levels = TRUE)
names(mw_res_oos) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res_oos)) %>% match(., fit_df$mname)]

loo_res <- loo(fit_df$fits$f_nb,
              fit_df$fits$f_nb2,
              fit_df$fits$f_nb3,
              fit_df$fits$f_nb4,
              fit_df$fits$f_nb5,
              fit_df$fits$f_nb6,
              fit_df$fits$f_nb7,
              fit_df$fits$f_nb8,
              fit_df$fits$f_nb9,
              fit_df$fits$f_nb10,
              fit_df$fits$f_nb11,
              fit_df$fits$f_nb12,
              fit_df$fits$f_nb13,
              fit_df$fits$f_nb14,
              fit_df$fits$f_nb15,
              fit_df$fits$f_nb16,
              fit_df$fits$f_nb17,
              fit_df$fits$f_nb18,
              fit_df$fits$f_nb19)

rownames(loo_res$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res$diffs)) %>% match(., fit_df$mname)]

loo_res_oos <- loo(fit_df$fits$f_nb,
               fit_df$fits$f_nb2,
               fit_df$fits$f_nb3,
               fit_df$fits$f_nb4,
               fit_df$fits$f_nb5,
               fit_df$fits$f_nb6,
               fit_df$fits$f_nb7,
               fit_df$fits$f_nb8,
               fit_df$fits$f_nb9,
               fit_df$fits$f_nb10,
               fit_df$fits$f_nb11,
               fit_df$fits$f_nb12,
               fit_df$fits$f_nb13,
               fit_df$fits$f_nb14,
               fit_df$fits$f_nb15,
               fit_df$fits$f_nb16,
               fit_df$fits$f_nb17,
               fit_df$fits$f_nb18,
               fit_df$fits$f_nb19, newdata = test, allow_new_levels = TRUE)

rownames(loo_res_oos$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res_oos$diffs)) %>% match(., fit_df$mname)]

predictive_performance_table <- loo_res$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo, elpd_diff) %>%
  left_join(mw_res %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight" = "value"), by = "mname")

predictive_performance_table_oos <- loo_res_oos$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo_oos = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff_oos = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo_oos, elpd_diff_oos) %>%
  left_join(mw_res_oos %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight_oos" = "value"), by = "mname")

# Minus t2
mw_res <- model_weights(fit_df$fits$f_nb,
                        fit_df$fits$f_nb2,
                        fit_df$fits$f_nb3,
                        fit_df$fits$f_nb4,
                        fit_df$fits$f_nb5,
                        fit_df$fits$f_nb6,
                        fit_df$fits$f_nb7,
                        fit_df$fits$f_nb8,
                        fit_df$fits$f_nb9,
                        fit_df$fits$f_nb10,
                        fit_df$fits$f_nb11,
                        fit_df$fits$f_nb12,
                        fit_df$fits$f_nb16,
                        fit_df$fits$f_nb17,
                        fit_df$fits$f_nb18)
names(mw_res) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res)) %>% match(., fit_df$mname)]

mw_res_oos <- model_weights(fit_df$fits$f_nb,
                            fit_df$fits$f_nb2,
                            fit_df$fits$f_nb3,
                            fit_df$fits$f_nb4,
                            fit_df$fits$f_nb5,
                            fit_df$fits$f_nb6,
                            fit_df$fits$f_nb7,
                            fit_df$fits$f_nb8,
                            fit_df$fits$f_nb9,
                            fit_df$fits$f_nb10,
                            fit_df$fits$f_nb11,
                            fit_df$fits$f_nb12,
                            fit_df$fits$f_nb16,
                            fit_df$fits$f_nb17,
                            fit_df$fits$f_nb18, newdata = test, allow_new_levels = TRUE)
names(mw_res_oos) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res_oos)) %>% match(., fit_df$mname)]

loo_res <- loo(fit_df$fits$f_nb,
               fit_df$fits$f_nb2,
               fit_df$fits$f_nb3,
               fit_df$fits$f_nb4,
               fit_df$fits$f_nb5,
               fit_df$fits$f_nb6,
               fit_df$fits$f_nb7,
               fit_df$fits$f_nb8,
               fit_df$fits$f_nb9,
               fit_df$fits$f_nb10,
               fit_df$fits$f_nb11,
               fit_df$fits$f_nb12,
               fit_df$fits$f_nb16,
               fit_df$fits$f_nb17,
               fit_df$fits$f_nb18)

rownames(loo_res$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res$diffs)) %>% match(., fit_df$mname)]

loo_res_oos <- loo(fit_df$fits$f_nb,
                   fit_df$fits$f_nb2,
                   fit_df$fits$f_nb3,
                   fit_df$fits$f_nb4,
                   fit_df$fits$f_nb5,
                   fit_df$fits$f_nb6,
                   fit_df$fits$f_nb7,
                   fit_df$fits$f_nb8,
                   fit_df$fits$f_nb9,
                   fit_df$fits$f_nb10,
                   fit_df$fits$f_nb11,
                   fit_df$fits$f_nb12,
                   fit_df$fits$f_nb16,
                   fit_df$fits$f_nb17,
                   fit_df$fits$f_nb18, newdata = test, allow_new_levels = TRUE)

rownames(loo_res_oos$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res_oos$diffs)) %>% match(., fit_df$mname)]

predictive_performance_table_not2 <- loo_res$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo, elpd_diff) %>%
  left_join(mw_res %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight" = "value"), by = "mname")

predictive_performance_table_oos_not2 <- loo_res_oos$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo_oos = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff_oos = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo_oos, elpd_diff_oos) %>%
  left_join(mw_res_oos %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight_oos" = "value"), by = "mname")

# Base components only (+ri+rs)

mw_res <- model_weights(fit_df$fits$f_nb3,
                        fit_df$fits$f_nb4,
                        fit_df$fits$f_nb7,
                        fit_df$fits$f_nb8,
                        fit_df$fits$f_nb9,
                        fit_df$fits$f_nb10,
                        fit_df$fits$f_nb11,
                        fit_df$fits$f_nb12)
names(mw_res) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res)) %>% match(., fit_df$mname)]

mw_res_oos <- model_weights(fit_df$fits$f_nb3,
                            fit_df$fits$f_nb4,
                            fit_df$fits$f_nb7,
                            fit_df$fits$f_nb8,
                            fit_df$fits$f_nb9,
                            fit_df$fits$f_nb10,
                            fit_df$fits$f_nb11,
                            fit_df$fits$f_nb12, newdata = test, allow_new_levels = TRUE)
names(mw_res_oos) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res_oos)) %>% match(., fit_df$mname)]

loo_res <- loo(fit_df$fits$f_nb3,
               fit_df$fits$f_nb4,
               fit_df$fits$f_nb7,
               fit_df$fits$f_nb8,
               fit_df$fits$f_nb9,
               fit_df$fits$f_nb10,
               fit_df$fits$f_nb11,
               fit_df$fits$f_nb12)

rownames(loo_res$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res$diffs)) %>% match(., fit_df$mname)]

loo_res_oos <- loo(fit_df$fits$f_nb3,
                   fit_df$fits$f_nb4,
                   fit_df$fits$f_nb7,
                   fit_df$fits$f_nb8,
                   fit_df$fits$f_nb9,
                   fit_df$fits$f_nb10,
                   fit_df$fits$f_nb11,
                   fit_df$fits$f_nb12, newdata = test, allow_new_levels = TRUE)

rownames(loo_res_oos$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res_oos$diffs)) %>% match(., fit_df$mname)]

predictive_performance_table_small <- loo_res$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo, elpd_diff) %>%
  left_join(mw_res %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight" = "value"), by = "mname")

predictive_performance_table_oos_small <- loo_res_oos$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo_oos = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff_oos = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo_oos, elpd_diff_oos) %>%
  left_join(mw_res_oos %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight_oos" = "value"), by = "mname")





gt(left_join(predictive_performance_table, predictive_performance_table_oos, by = "mname")) %>% tab_header("Predictive performance") %>%
  tab_spanner(label = "In-sample (2000-2014)",
              columns = c("elpd_loo", "elpd_diff", "stacking_weight")) %>%
  tab_spanner(label = "Out-of-sample (2015-2018)",
              columns = c("elpd_loo_oos", "elpd_diff_oos", "stacking_weight_oos")) %>%
  cols_label(mname = "Model",
             elpd_loo = "elpd LOO",
             elpd_diff = html("&Delta;elpd"),
             stacking_weight = "Stacking Weight",
             elpd_loo_oos = "elpd LOO",
             elpd_diff_oos = html("&Delta;elpd"),
             stacking_weight_oos = "Stacking Weight") %>%
  gtsave("brm_results/tables/predictive_performance.html")


gt(left_join(predictive_performance_table_not2, predictive_performance_table_oos_not2, by = "mname")) %>% tab_header("Predictive performance") %>%
  tab_spanner(label = "In-sample (2000-2014)",
              columns = c("elpd_loo", "elpd_diff", "stacking_weight")) %>%
  tab_spanner(label = "Out-of-sample (2015-2018)",
              columns = c("elpd_loo_oos", "elpd_diff_oos", "stacking_weight_oos")) %>%
  cols_label(mname = "Model",
             elpd_loo = "elpd LOO",
             elpd_diff = html("&Delta;elpd"),
             stacking_weight = "Stacking Weight",
             elpd_loo_oos = "elpd LOO",
             elpd_diff_oos = html("&Delta;elpd"),
             stacking_weight_oos = "Stacking Weight") %>%
  gtsave("brm_results/tables/predictive_performance_not2.html")
  

gt(left_join(predictive_performance_table_small, predictive_performance_table_oos_small, by = "mname")) %>% tab_header("Predictive performance") %>%
  tab_spanner(label = "In-sample (2000-2014)",
              columns = c("elpd_loo", "elpd_diff", "stacking_weight")) %>%
  tab_spanner(label = "Out-of-sample (2015-2018)",
              columns = c("elpd_loo_oos", "elpd_diff_oos", "stacking_weight_oos")) %>%
  cols_label(mname = "Model",
             elpd_loo = "elpd LOO",
             elpd_diff = html("&Delta;elpd"),
             stacking_weight = "Stacking Weight",
             elpd_loo_oos = "elpd LOO",
             elpd_diff_oos = html("&Delta;elpd"),
             stacking_weight_oos = "Stacking Weight") %>%
  gtsave("brm_results/tables/predictive_performance_small.html")

#### Single components only ###

mw_res <- model_weights(fit_df$fits$f_nb7,
                        fit_df$fits$f_nb8,
                        fit_df$fits$f_nb9)
names(mw_res) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res)) %>% match(., fit_df$mname)]

mw_res_oos <- model_weights(fit_df$fits$f_nb7,
                            fit_df$fits$f_nb8,
                            fit_df$fits$f_nb9, newdata = test, allow_new_levels = TRUE)
names(mw_res_oos) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res_oos)) %>% match(., fit_df$mname)]

loo_res <- loo(fit_df$fits$f_nb7,
               fit_df$fits$f_nb8,
               fit_df$fits$f_nb9)

rownames(loo_res$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res$diffs)) %>% match(., fit_df$mname)]

loo_res_oos <- loo(fit_df$fits$f_nb7,
                   fit_df$fits$f_nb8,
                   fit_df$fits$f_nb9, newdata = test, allow_new_levels = TRUE)

rownames(loo_res_oos$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res_oos$diffs)) %>% match(., fit_df$mname)]

predictive_performance_table_small <- loo_res$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo, elpd_diff) %>%
  left_join(mw_res %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight" = "value"), by = "mname")

predictive_performance_table_oos_small <- loo_res_oos$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo_oos = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff_oos = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo_oos, elpd_diff_oos) %>%
  left_join(mw_res_oos %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight_oos" = "value"), by = "mname")


gt(left_join(predictive_performance_table_small, predictive_performance_table_oos_small, by = "mname")) %>% tab_header("Predictive performance") %>%
  tab_spanner(label = "In-sample (2000-2014)",
              columns = c("elpd_loo", "elpd_diff", "stacking_weight")) %>%
  tab_spanner(label = "Out-of-sample (2015-2018)",
              columns = c("elpd_loo_oos", "elpd_diff_oos", "stacking_weight_oos")) %>%
  cols_label(mname = "Model",
             elpd_loo = "elpd LOO",
             elpd_diff = html("&Delta;elpd"),
             stacking_weight = "Stacking Weight",
             elpd_loo_oos = "elpd LOO",
             elpd_diff_oos = html("&Delta;elpd"),
             stacking_weight_oos = "Stacking Weight") %>%
  gtsave("brm_results/tables/predictive_performance_single.html")
####

#### Double components only ###

mw_res <- model_weights(fit_df$fits$f_nb10,
                        fit_df$fits$f_nb11,
                        fit_df$fits$f_nb12)
names(mw_res) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res)) %>% match(., fit_df$mname)]

mw_res_oos <- model_weights(fit_df$fits$f_nb10,
                            fit_df$fits$f_nb11,
                            fit_df$fits$f_nb12, newdata = test, allow_new_levels = TRUE)
names(mw_res_oos) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res_oos)) %>% match(., fit_df$mname)]

loo_res <- loo(fit_df$fits$f_nb10,
               fit_df$fits$f_nb11,
               fit_df$fits$f_nb12)

rownames(loo_res$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res$diffs)) %>% match(., fit_df$mname)]

loo_res_oos <- loo(fit_df$fits$f_nb10,
                   fit_df$fits$f_nb11,
                   fit_df$fits$f_nb12, newdata = test, allow_new_levels = TRUE)

rownames(loo_res_oos$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res_oos$diffs)) %>% match(., fit_df$mname)]

predictive_performance_table_small <- loo_res$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo, elpd_diff) %>%
  left_join(mw_res %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight" = "value"), by = "mname")

predictive_performance_table_oos_small <- loo_res_oos$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo_oos = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff_oos = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo_oos, elpd_diff_oos) %>%
  left_join(mw_res_oos %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight_oos" = "value"), by = "mname")


gt(left_join(predictive_performance_table_small, predictive_performance_table_oos_small, by = "mname")) %>% tab_header("Predictive performance") %>%
  tab_spanner(label = "In-sample (2000-2014)",
              columns = c("elpd_loo", "elpd_diff", "stacking_weight")) %>%
  tab_spanner(label = "Out-of-sample (2015-2018)",
              columns = c("elpd_loo_oos", "elpd_diff_oos", "stacking_weight_oos")) %>%
  cols_label(mname = "Model",
             elpd_loo = "elpd LOO",
             elpd_diff = html("&Delta;elpd"),
             stacking_weight = "Stacking Weight",
             elpd_loo_oos = "elpd LOO",
             elpd_diff_oos = html("&Delta;elpd"),
             stacking_weight_oos = "Stacking Weight") %>%
  gtsave("brm_results/tables/predictive_performance_double.html")
####

#### Base vs full ###

mw_res <- model_weights(fit_df$fits$f_nb3,
                        fit_df$fits$f_nb4)
names(mw_res) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res)) %>% match(., fit_df$mname)]

mw_res_oos <- model_weights(fit_df$fits$f_nb3,
                            fit_df$fits$f_nb4, newdata = test, allow_new_levels = TRUE)
names(mw_res_oos) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", names(mw_res_oos)) %>% match(., fit_df$mname)]

loo_res <- loo(fit_df$fits$f_nb3,
               fit_df$fits$f_nb4)

rownames(loo_res$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res$diffs)) %>% match(., fit_df$mname)]

loo_res_oos <- loo(fit_df$fits$f_nb3,
                   fit_df$fits$f_nb4, newdata = test, allow_new_levels = TRUE)

rownames(loo_res_oos$diffs) <- fit_df$formula[gsub("fit_df\\$fits\\$", "", rownames(loo_res_oos$diffs)) %>% match(., fit_df$mname)]

predictive_performance_table_small <- loo_res$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo, elpd_diff) %>%
  left_join(mw_res %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight" = "value"), by = "mname")

predictive_performance_table_oos_small <- loo_res_oos$diffs %>% as_tibble(rownames = "mname") %>% 
  mutate(across(!all_of("mname"), ~ as.numeric(.x))) %>% 
  mutate(across(!all_of("mname"), ~ round(.x, digits = 0)))  %>% 
  mutate(elpd_loo_oos = paste0(elpd_loo, " (", se_elpd_loo, ")"),
         elpd_diff_oos = paste0(elpd_diff, " (", se_diff, ")")) %>%
  select(mname, elpd_loo_oos, elpd_diff_oos) %>%
  left_join(mw_res_oos %>% round(digits = 2) %>% as_tibble(rownames = "mname") %>% rename("stacking_weight_oos" = "value"), by = "mname")


gt(left_join(predictive_performance_table_small, predictive_performance_table_oos_small, by = "mname")) %>% tab_header("Predictive performance") %>%
  tab_spanner(label = "In-sample (2000-2014)",
              columns = c("elpd_loo", "elpd_diff", "stacking_weight")) %>%
  tab_spanner(label = "Out-of-sample (2015-2018)",
              columns = c("elpd_loo_oos", "elpd_diff_oos", "stacking_weight_oos")) %>%
  cols_label(mname = "Model",
             elpd_loo = "elpd LOO",
             elpd_diff = html("&Delta;elpd"),
             stacking_weight = "Stacking Weight",
             elpd_loo_oos = "elpd LOO",
             elpd_diff_oos = html("&Delta;elpd"),
             stacking_weight_oos = "Stacking Weight") %>%
  gtsave("brm_results/tables/predictive_performance_base_vs_full.html")
####


#### Figure 3: Predictive fit by continent ####

q10 <- function(y) quantile(y, 0.1)
q90 <- function(y) quantile(y, 0.9)

my_breaks <- function(x) {
  halfbillion <- 500000000
  billion <- 1000000000
  million <- 1000000
  thousand <- 1000
  if(max(x, na.rm = T) > (halfbillion)){
    paste0(x/billion, "B")
  }  else if (max(x, na.rm = T) > million) {
    paste0(x/million, "M")
  } else if (max(x, na.rm = T) > thousand) {
    paste0(x/thousand, "K")
  } else{x}
}

median_to_plot <- train %>% group_by(continent) %>% summarize(stat = median(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q10_to_plot <- train %>% group_by(continent) %>% summarize(stat = q10(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q90_to_plot <- train %>% group_by(continent) %>% summarize(stat = q90(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")

ppc_skew_50 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb4), stat = "median", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = median_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_10 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb4), stat = "q10", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q10_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_90 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb4), stat = "q90", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q90_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")


median_to_plot <- test %>% group_by(continent) %>% summarize(stat = median(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q10_to_plot <- test %>% group_by(continent) %>% summarize(stat = q10(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q90_to_plot <- test %>% group_by(continent) %>% summarize(stat = q90(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")

ppc_skew_50_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb4, newdata = test, allow_new_levels = TRUE), stat = "median", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = median_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_10_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb4, newdata = test, allow_new_levels = TRUE), stat = "q10", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q10_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_90_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb4, newdata = test, allow_new_levels = TRUE), stat = "q90", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q90_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")

ppc_skew_10 + ppc_skew_50 + ppc_skew_90 + plot_layout(ncol = 3) &
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 6),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        plot.tag = element_text(size = 14),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
my_tiff <- function(...) ragg::agg_tiff(..., units = "in", res = 300)

ggsave("brm_results/figures/figure3_predictive_fit_by_continent_insample-full+ri+rs_medium_300dpi.tiff", device = my_tiff, width = 3.6, height = 3.6, scale = 2)
ppc_skew_10_oos + ppc_skew_50_oos + ppc_skew_90_oos + plot_layout(ncol = 3)
ggsave("brm_results/figures/predictive_fit_by_continent_oos-full+ri+rs.png", device = ragg::agg_png(), scale = 2, dpi = 300)

#### Test with other models ####

# nb14: Varying intercept, country-level covariates, tensor interactions for population_affected and duration

median_to_plot <- train %>% group_by(continent) %>% summarize(stat = median(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q10_to_plot <- train %>% group_by(continent) %>% summarize(stat = q10(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q90_to_plot <- train %>% group_by(continent) %>% summarize(stat = q90(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")

ppc_skew_50 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb14), stat = "median", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = median_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_10 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb14), stat = "q10", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q10_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_90 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb14), stat = "q90", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q90_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")


median_to_plot <- test %>% group_by(continent) %>% summarize(stat = median(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q10_to_plot <- test %>% group_by(continent) %>% summarize(stat = q10(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q90_to_plot <- test %>% group_by(continent) %>% summarize(stat = q90(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")

ppc_skew_50_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb14, newdata = test, allow_new_levels = TRUE), stat = "median", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = median_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_10_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb14, newdata = test, allow_new_levels = TRUE), stat = "q10", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q10_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_90_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb14, newdata = test, allow_new_levels = TRUE), stat = "q90", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q90_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")

ppc_skew_10 + ppc_skew_50 + ppc_skew_90 + plot_layout(ncol = 3)
ggsave("brm_results/figures/predictive_fit_by_continent_insample-c-lvl+ri+t2.png", device = ragg::agg_png(), scale = 2, dpi = 300)
ppc_skew_10_oos + ppc_skew_50_oos + ppc_skew_90_oos + plot_layout(ncol = 3)
ggsave("brm_results/figures/predictive_fit_by_continent_oos-c-lvl+ri+t2.png", device = ragg::agg_png(), scale = 2, dpi = 300)


# nb19: Varying intercept, only baseline, tensor interactions for population_affected and duration

median_to_plot <- train %>% group_by(continent) %>% summarize(stat = median(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q10_to_plot <- train %>% group_by(continent) %>% summarize(stat = q10(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q90_to_plot <- train %>% group_by(continent) %>% summarize(stat = q90(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")

ppc_skew_50 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb19), stat = "median", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = median_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_10 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb19), stat = "q10", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q10_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_90 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb19), stat = "q90", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q90_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")


median_to_plot <- test %>% group_by(continent) %>% summarize(stat = median(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q10_to_plot <- test %>% group_by(continent) %>% summarize(stat = q10(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q90_to_plot <- test %>% group_by(continent) %>% summarize(stat = q90(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")

ppc_skew_50_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb19, newdata = test, allow_new_levels = TRUE), stat = "median", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = median_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_10_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb19, newdata = test, allow_new_levels = TRUE), stat = "q10", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q10_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_90_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb19, newdata = test, allow_new_levels = TRUE), stat = "q90", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q90_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")

ppc_skew_10 + ppc_skew_50 + ppc_skew_90 + plot_layout(ncol = 3)
ggsave("brm_results/figures/predictive_fit_by_continent_insample-base+ri+t2.png", device = ragg::agg_png(), scale = 2, dpi = 300)
ppc_skew_10_oos + ppc_skew_50_oos + ppc_skew_90_oos + plot_layout(ncol = 3)
ggsave("brm_results/figures/predictive_fit_by_continent_oos-base+ri+t2.png", device = ragg::agg_png(), scale = 2, dpi = 300)

# nb3: Varying intercept and slope, only baseline.

median_to_plot <- train %>% group_by(continent) %>% summarize(stat = median(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q10_to_plot <- train %>% group_by(continent) %>% summarize(stat = q10(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q90_to_plot <- train %>% group_by(continent) %>% summarize(stat = q90(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")

ppc_skew_50 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb3), stat = "median", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = median_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_10 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb3), stat = "q10", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q10_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_90 <- ppc_stat_grouped(y = train[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb3), stat = "q90", group = train$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q90_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")

median_to_plot <- test %>% group_by(continent) %>% summarize(stat = median(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q10_to_plot <- test %>% group_by(continent) %>% summarize(stat = q10(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")
q90_to_plot <- test %>% group_by(continent) %>% summarize(stat = q90(displaced_w) %>% round(digits = 0)) %>% rename("group" = "continent")

ppc_skew_50_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb3, newdata = test, allow_new_levels = TRUE), stat = "median", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = median_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_10_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb3, newdata = test, allow_new_levels = TRUE), stat = "q10", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q10_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")
ppc_skew_90_oos <- ppc_stat_grouped(y = test[[DEPVAR]], yrep = posterior_predict(fit_df$fits$f_nb3, newdata = test, allow_new_levels = TRUE), stat = "q90", group = test$continent, facet_args = list("ncol" = 1, "scales" = "free_x")) + 
  geom_text(data = q90_to_plot, aes(x = stat, y = 2, label = scales::comma(stat)), size = 3, vjust = -3, hjust = -0.5) +
  scale_x_continuous(labels = my_breaks, breaks = scales::pretty_breaks(5, min.n = 2)) + 
  theme(legend.position = "bottom")

ppc_skew_10 + ppc_skew_50 + ppc_skew_90 + plot_layout(ncol = 3)
ggsave("brm_results/figures/predictive_fit_by_continent_insample-base+ri+rs.png", device = ragg::agg_png(), scale = 2, dpi = 300)
ppc_skew_10_oos + ppc_skew_50_oos + ppc_skew_90_oos + plot_layout(ncol = 3)
ggsave("brm_results/figures/predictive_fit_by_continent_oos-base+ri+rs.png", device = ragg::agg_png(), scale = 2, dpi = 300)


####

cfx_pred <- conditional_effects(fit_df$fits$f_nb4, method = "posterior_predict", plot = FALSE, prob = 0.8) %>% plot(plot = FALSE) # This includes the error term, and is what we use when we want to predict new cases.
cfx_pred$population_affected <- cfx_pred$population_affected + xlab("directly_exposed")
cfx_pred <- lapply(cfx_pred, function(x) x + scale_y_continuous(labels = my_breaks, breaks = scales::pretty_breaks(3, min.n = 2)))
wrap_plots(cfx_pred)  &
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 6),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        plot.tag = element_text(size = 14),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))
ggsave(filename = paste0("brm_results/figures/figure2-cfx-posterior-predict-80predint-full+ri+rs_medium_300dpi.tiff"), device = my_tiff, width = 3.6, height = 3.6, scale = 2)
cfx_pred <- conditional_effects(fit_df$fits$f_nb4, method = "posterior_epred", plot = FALSE, prob = 0.8) %>% plot(plot = FALSE) # This includes the error term, and is what we use when we want to predict new cases.
cfx_pred$population_affected <- cfx_pred$population_affected + xlab("directly_exposed")
cfx_pred <- lapply(cfx_pred, function(x) x + scale_y_continuous(labels = my_breaks, breaks = scales::pretty_breaks(3, min.n = 2)))
wrap_plots(cfx_pred)
ggsave(filename = paste0("brm_results/figures/cfx-posterior-epred-80predint-full+ri+rs.png"), device = ragg::agg_png(), scale = 2, dpi = 300)



# Varying intercept and slope
g1 <- fit_df$fits$f_nb4 %>%
  spread_draws(b_Intercept, r_continent[continent,]) %>%
  mutate(condition_mean = b_Intercept + r_continent) %>%
  ggplot(aes(y = continent, x = condition_mean)) +
  stat_halfeye() +
  theme_bw() +
  ggtitle("full+ri+rs") +
  labs(x = "Random Intercept", y = "Continent") + theme(axis.title.y = element_blank())

g2 <- fit_df$fits$f_nb4 %>%
  spread_draws(r_continent[continent,term], b_population_affected) %>%
  filter(term == "population_affected") %>%
  mutate(population_affected = r_continent + b_population_affected) %>%
  ggplot(aes(y = continent, x = population_affected)) +
  stat_halfeye() +
  theme_bw() +
  ggtitle("full+ri+rs") +
  labs(x = "RS: Directly Exposed", y = "Continent") + theme(axis.title.y = element_blank())

g3 <- fit_df$fits$f_nb4 %>%
  spread_draws(r_continent[continent,term], b_duration) %>%
  filter(term == "duration") %>%
  mutate(duration = r_continent + b_duration) %>%
  ggplot(aes(y = continent, x = duration)) +
  stat_halfeye() +
  theme_bw() +
  ggtitle("full+ri+rs") +
  labs(x = "RS: Duration", y = "Continent") + theme(axis.title.y = element_blank())

# Varying intercept only
g4 <- fit_df$fits$f_nb13 %>%
  spread_draws(b_Intercept, r_continent[continent,]) %>%
  mutate(condition_mean = b_Intercept + r_continent) %>%
  ggplot(aes(y = continent, x = condition_mean)) +
  stat_halfeye() +
  theme_bw() +
  ggtitle("full+ri+t2 (no random slope)") +
  labs(x = "Random Intercept", y = "Continent") + theme(axis.title.y = element_blank())

pwrk <- g4 + (g1 + g2 + g3) + plot_layout(ncol = 1) + plot_annotation(tag_levels = "A") 
pwrk[[2]] <- pwrk[[2]] + plot_layout(tag_level = "new")
pwrk + plot_annotation(tag_levels = c("A", "1"))
ggsave("brm_results/figures/random_effects_for_selected_models.png", device = ragg::agg_png(), scale = 2, dpi = 300)

#### Regression Tables ####
library(sjPlot)

for(i in 1:length(fit_df$fits)){
  fname <- paste0("brm_results/tables/regression_table-", fit_df$formula[i], ".html")
  tab_model(fit_df$fits[[i]], file = fname, seed = 42) %>% print()
}

#### Spline conditional smooth plot ####

fit_df$fits$f_nb19 %>% conditional_smooths() %>% plot(stype = "raster")
fit_df$fits$f_nb14 %>% conditional_smooths()


#### MCMC convergence ####

plot(fit_df$fits$f_nb4, ask = FALSE)
get_variables(fit_df$fits$f_nb4)
pairs(fit_df$fits$f_nb4, variable = c("cor_continent__Intercept__population_affected", 
                                      "cor_continent__population_affected__duration",
                                      "cor_continent__population_affected__nevents_sum10",
                                      "cor_continent__Intercept__duration",
                                      "cor_continent__Intercept__duration"))
fit_df$fits$f_nb2 # 1 divergent transition
fit_df$fits$f_nb4 # 54 divergent transitions
fit_df$fits$f_nb13 # No divergent transitions

#Outlier plot (influence on posterior distribution), full+ri+rs
png(paste0("brm_results/figures/k-diag-full+ri+rs.png"), width = 800, height = 400)
plot(loo(fit_df$fits$f_nb4), label_points = T, main = "")
dev.off()

#Outlier plot (influence on posterior distribution), full+ri+t2
png(paste0("brm_results/figures/k-diag-full+ri+t2.png"), width = 800, height = 400)
plot(loo(fit_df$fits$f_nb13), label_points = T, main = "")
dev.off()