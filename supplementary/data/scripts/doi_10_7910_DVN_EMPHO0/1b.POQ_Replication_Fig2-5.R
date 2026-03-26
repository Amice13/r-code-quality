
#====================================================================================#
#
# Set up StepMix Rwrapper 
#
#====================================================================================#

## ONE-TIME SETUP
## DO THIS ONLY ONCE IN THE CONSOLE

install.packages(c("reticulate", "stepmixr"))

# install Miniconda and Python StepMix
reticulate::install_miniconda()
reticulate::conda_create("stepmix_env")
reticulate::conda_install(
  envname  = "stepmix_env",
  packages = "stepmix",
  pip = TRUE
)

## BEFORE EACH SESSION

library(reticulate)
use_condaenv("stepmix_env", required = TRUE)

# optional: control threading
Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1"
)

#====================================================================================#
#
# Prepare Data for StepMix 
#
#====================================================================================#

# load libraries
library(dplyr)
library(fastDummies)
library(ggplot2)
library(tidyr)
library(rio)
library(stepmixr)
library(ggpubr)
library(egg)
library(nnet)
library(marginaleffects)
library(MASS)
library(knitr)
library(kableExtra)
library(boot)
library(stringr)
library(xtable)
library(survey)
library(purrr)

# import LCA data
data <- rio::import("POQ_Replication/lca_dat.csv")

#only columns for LCA
lca_dat <- data[ ,1:37] 

#====================================================================================#
#
# LCA Model
#
#====================================================================================#

# LCA model
Y <- as.matrix(lca_dat[, 1:8]) # indicators
Z <- as.matrix(lca_dat[, 9:25]) # covariates

# specify model for covariates
structural_descriptor <- list(
  covariate = list(
    model = "covariate",
    n_columns = ncol(Z), # num of covariates
    method = "newton-raphson",
    max_iter = as.integer(100)
  )
)

# build model object
model = stepmix(
  n_components = 4,
  measurement = "categorical", # i.e. indicators
  structural = structural_descriptor, 
  n_steps = 3,
  n_init = 15,
  assignment = "soft", 
  random_state = 42,
  correction = "BCH"
)

# fit lca with covariates 
fit1 <- stepmixr::fit(model, X = Y, Y = Z)

#====================================================================================#
#
# Figure 2: Class Membership 
#
#====================================================================================#

# extract conditional response probabilities 
mus <-  fit1$get_parameters()
pis <- mus$measurement$pis 

# pivot pis to long format
long_dat <- pis %>% 
  reshape2::melt(
  ) 

# get class weights from posterior probs
class_weights <- colMeans(
  stepmixr::predict_proba(fit1, X = Y, Y = Z)
) 

# class shares
long_dat$post_popshare <- round(
  class_weights,
  digits = 3
) 

# add response categories
long_dat$category <- rep(
  c("Don't Know", "Decrease", "No Change", "Increase"),
  each  = 4,
  times = 8 
)

# add indicator names
long_dat$variable <- rep(
  c("Quality", "Price", "Profits", "Demand",
    "Complement", "Inequality", "Wages", "Hiring"),
  each = 16
)

# add class names
long_dat$lca_class_names <- rep(
  c("Uncertains", "Substituters", "Complementers", "Skeptics"),
  times = 32
)

# re-order factors for plotting
long_dat <- long_dat %>%
  mutate(
    category = factor(category, 
                      levels=c("Increase", "No Change", "Decrease", "Don't Know")),
    variable = factor(variable, 
                      levels=c("Wages", "Hiring", "Inequality", "Complement",
                               "Price", "Quality", "Demand", "Profits")
    )
  )

# plot class proportions
lca_props <- long_dat %>%
  slice(1:4) %>% # class shares (post_popshare) repeat through long_dat
  mutate(
    lca_class_names = 
      factor(lca_class_names, 
             levels = c("Complementers", "Skeptics", "Substituters", "Uncertains")
      )) %>%
  arrange(lca_class_names) %>%  
  ggplot(aes(x = lca_class_names, y = post_popshare, fill = lca_class_names)) + 
  geom_col(show.legend = FALSE) + 
  scale_fill_manual(values = c("Complementers" = "#FFCC4D", 
                               "Skeptics" = "#66C1A3", 
                               "Substituters" = "#66A3CC", 
                               "Uncertains" = "#E897C2")) + 
  theme_minimal() + 
  ylab("Proportion") + 
  xlab("") + 
  ylim(0, 0.4) + 
  ggtitle("") + 
  theme(text = element_text(size = 10))

# re-order for plotting
long_dat <- long_dat %>%
  mutate(
    lca_class_names = 
      factor(lca_class_names, 
             levels = c("Complementers", "Skeptics", "Substituters", "Uncertains")
      ) 
  )

# plot item response profiles
lca_vars <- long_dat %>%
  mutate(
    lca_class_names = 
      factor(lca_class_names, 
             levels = c("Complementers", "Skeptics", "Substituters", "Uncertains")
      )) %>%
  ggplot(aes(x = variable, y = value, color = lca_class_names)) +
  geom_point(size = 2) +  
  scale_color_manual(values = c("Complementers" = "#FFCC4D", 
                                "Skeptics" = "#66C1A3", 
                                "Substituters" = "#66A3CC", 
                                "Uncertains" = "#E897C2")) +  
  facet_grid(rows = vars(category), 
             cols = vars(lca_class_names), 
             scales = "free_y", 
             switch = "x") + 
  theme_minimal() +
  coord_flip() +
  geom_hline(yintercept = 0.5, linetype = "solid", color="grey") +
  scale_y_continuous(breaks = c(0.2, 0.5, 0.8), 
                     limits = c(0, 1)) +
  labs(x = "", y = "Proportion", color = "Response") +
  theme(legend.position = "none",
        strip.text.x = element_blank(), 
        legend.title = element_blank(),
        text = element_text(size = 8))

# combined plot
combined_plot <- ggarrange(
  lca_props + rremove("x.grid"), 
  lca_vars + rremove("x.grid"),
  nrow=2,  
  heights=c(1.55, 5)
) 
ggsave(combined_plot, filename = "POQ_Replication/Figure2.png",  width = 6.5, height = 7)

#====================================================================================#
# bind predictions to dataset

# drop missing rows to align with lca
data <- data %>%
  drop_na(1:27)

# add predicted latent class to data
predicted_class <- data.frame(lca_class = predict(fit1, X = Y, Y = Z))
data <- cbind(data, predicted_class)

# rename classes
data <- data %>%
  mutate(class_names = case_when(
    lca_class==0 ~ "Uncertains", 
    lca_class==1 ~ "Substituters",
    lca_class==2 ~ "Complementers",
    lca_class==3 ~ "Skeptics"
  )) 

# add posterior probabilities
predicted_probs <- data.frame(posterior = predict_proba(fit1, X = Y, Y = Z))
data <- cbind(data, predicted_probs)

# rename posterior probabilities
data <- data %>% 
  dplyr::rename(
    prob_uncertains = posterior.1,
    prob_substituters = posterior.2,
    prob_complementers = posterior.3,
    prob_skeptics = posterior.4
  )


#====================================================================================#
#
# Figure 3
#
#====================================================================================#

# bootstrap model params
set.seed(42)
fit1_bs <- bootstrap(
  fit1, Y, Z, 
  n_repetitions = 1000,
  progress_bar = TRUE)

samples  <- fit1_bs$samples

# class labels and colors 
classes <- c("Uncertains", "Substituters", "Complementers", "Skeptics")
my_colors <- c(
  "Uncertains" = "#E897C2",
  "Substituters" = "#66A3CC",
  "Complementers" = "#FFCC4D",
  "Skeptics" = "#66C1A3"
)

# intercepts and betas from covariate model
wide_dat <- samples %>%
  filter(model_name == "covariate", param == "beta") %>%
  dplyr::select(class_no, rep, variable, value) %>%
  tidyr::pivot_wider(names_from = variable,
                     values_from = value) %>%
  dplyr::mutate(
    class = factor(class_no,
                   levels = 0:3,
                   labels = classes)
  )

# function to compute marginal effects via softmax for a feature
effect_fun <- function(feature,
                       label_low, # for x=0
                       label_high, # for x=1
                       plot_title,
                       y_limits = c(-0.27, 0.2)) {
  
  df <- wide_dat %>%
    dplyr::group_by(rep) %>% # each bootstrap rep
    dplyr::mutate(
      eta_low  = intercept, 
      eta_high = intercept + .data[[feature]], 
      # softmax denominators
      denom_low  = sum(exp(eta_low)),
      denom_high = sum(exp(eta_high)),
      # 4-class probs
      p_low  = exp(eta_low)  / denom_low, #P(class when x=0)
      p_high = exp(eta_high) / denom_high, #P(class when X =1)
      marginal_effect = p_high - p_low
    ) %>%
    dplyr::ungroup()
  
  sum_dat <- df %>%
    dplyr::group_by(class) %>%
    dplyr::summarize(
      estimate = mean(marginal_effect),
      SE = sd(marginal_effect),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      lower_ci = estimate - 1.96 * SE,
      upper_ci = estimate + 1.96 * SE,
      class = factor(class,
                     levels = c("Uncertains","Substituters","Skeptics","Complementers"))
    )
  
  ggplot(sum_dat, aes(x = class, y = estimate, fill = class)) +
    geom_col(width = 0.7) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
    coord_flip() +
    scale_fill_manual(values = my_colors) +
    labs(
      title = plot_title,
      x = "",
      y = "Difference in predicted probability"
    ) +
    ylim(y_limits) +
    theme_minimal(base_size = 6.5) +
    theme(legend.position = "none")
}

# plots for select covariates (note: first feature is feature_0)
a <- effect_fun("feature_3", "Man", "Woman",
                plot_title = "Gender: p(Woman) − p(Man)",
                y_limits = c(-0.27, 0.2))

b <- effect_fun("feature_1", "Under 30", "45–64",
                plot_title = "Age: p(45–64) − p(Under 30)",
                y_limits = c(-0.27, 0.2))

c <- effect_fun("feature_5", "HS or less", "University",
                plot_title = "Education: p(University) − p(HS)",
                y_limits = c(-0.27, 0.2))

d <- effect_fun("feature_12", "Mid-income", "High-income",
                plot_title = "Income: p(High) − p(Mid)",
                y_limits = c(-0.27, 0.2))

e <- effect_fun("feature_7", "White", "Black",
                plot_title = "Race: p(Black) − p(White)",
                y_limits = c(-0.27, 0.2))

f <- effect_fun("feature_14", "Urban", "Rural",
                plot_title = "Place Type: p(Rural) − p(Urban)",
                y_limits = c(-0.27, 0.2))

g <- effect_fun("feature_13", "US", "Canada",
                plot_title = "Country: p(Canada) − p(US)",
                y_limits = c(-0.27, 0.2))

# combined plot
blank <- ggplot() + theme_void()

combined_plot <- ggpubr::ggarrange(
  a + rremove("xlab"),
  b + rremove("xlab"),
  c + rremove("xlab"),
  d + rremove("xlab"),
  e + rremove("xlab"),
  f + rremove("xlab"),
  blank, 
  g,
  ncol = 3, nrow = 3,
  font.label = list(size = 6.5)
)

ggsave(combined_plot, filename = "POQ_Replication/Figure3.png", width = 6.75, height = 7)


#====================================================================================#
#
# Figure 4: Policy DVs
#
#====================================================================================#

# policy vars and labels
policies <- c(
  social_spending = "Increase Social Spending",
  basic_income = "Basic Income",
  reskill = "Reskill Workers",
  invest_uni = "Invest in Education",
  reduce_immigHS = "Reduce High-Skill Immig",
  reduce_immigLS = "Reduce Low-Skill Immig",
  restrict_trade = "Restrict Trade",
  tax_AI = "Tax AI",
  subsidies = "Subsidize Company Jobs",
  regulation = "Regulate AI"
)

# fit mod with distal outcome, bootstrap contrasts (relative to complementers)
policy_fun <- function(policy_name, policy_label) {
  
  # extract indicators, covariates, and distal outcome
  Y <- as.matrix(lca_dat[, 1:8]) # indicators           
  Z <- as.matrix(lca_dat[, 9:25]) # covariates           
  distal_outcome <- as.matrix(lca_dat[[policy_name]]) # each distal policy outcome
  
  # combine covariates and distal outcome
  Z_with_distal <- cbind(Z, distal_outcome)              
  
  # model 
  structural_descriptor <- list(
    covariate = list(
      model = "covariate",
      n_columns = ncol(Z),
      method = "newton-raphson",
      max_iter = as.integer(100)
    ),
    response = list(
      model = "binary_nan", # binary outcome (policy) with missing values 
      n_columns = as.integer(1)
    )
  )
  
  # fit model
  model <- stepmix(
    n_components = 4, # classes
    measurement = "categorical",
    structural = structural_descriptor, 
    n_steps = 3,
    n_init = 15,
    assignment = "soft",  
    random_state = 42,
    correction = "BCH"
  )
  
  fit1 <- stepmixr::fit(model, X = Y, Y = Z_with_distal)
  
  # bootstrap class-specific policy support
  set.seed(42)
  fit1_bs <- bootstrap(
    fit1, Y, Z_with_distal,
    n_repetitions = 1000,
    progress_bar = TRUE
  )
  
  samples <- fit1_bs$samples
  
  # class-specific params for the policy outcome
  pis_wide <- samples %>%
    filter(model_name == "response", param == "pis") %>%
    pivot_wider(
      id_cols = rep, # replicate
      names_from = class_no, # classes
      values_from = value,
      names_prefix = "class_"
    )
  
  # within‐replicate contrasts vs. complementers (class 2) 
  marginal_effects <- pis_wide %>%
    transmute(
      Uncertains = class_0 - class_2,
      Substituters = class_1 - class_2,
      Skeptics = class_3 - class_2
    )
  
  # estimate and SEs for each contrast
  marginal_effects %>%
    dplyr::summarize(
      estimate_Uncertains = mean(Uncertains),
      SE_Uncertains = sd(Uncertains),
      estimate_Substituters = mean(Substituters),
      SE_Substituters = sd(Substituters),
      estimate_Skeptics = mean(Skeptics),
      SE_Skeptics = sd(Skeptics)
    ) %>%
    pivot_longer(
      everything(),
      names_to  = c(".value", "class_name"),
      names_sep = "_"
    ) %>%
    mutate(
      policy = policy_name,
      policy_labs = policy_label
    )
}

# apply function across policies
policies_df <- map2_dfr(names(policies), policies, policy_fun) %>%
  mutate(
    class_name  = factor(class_name, levels = c("Skeptics","Substituters","Uncertains")),
    policy_labs = factor(policy_labs, levels = policies)
  )

# plot
ggplot(policies_df, aes(
  x = class_name,
  y  = estimate,
  ymin = estimate - 1.96 * SE,
  ymax = estimate + 1.96 * SE,
  color = class_name
)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(width = 0.1, position = position_dodge(0.5)) +
  geom_point(size = 1.5,    position = position_dodge(0.5)) +
  coord_flip() +
  ylim(-0.33, 0.33) +
  facet_wrap(~ policy_labs, ncol = 1) +
  scale_color_manual(values = c("#66C1A3", "#66A3CC", "#E897C2")) +
  labs(x = NULL, y = "Difference in predicted probability") +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9.5)
  )
ggsave(filename = "POQ_Replication/Figure4.png", width = 6, height = 6)

#====================================================================================#
#
# Figure 5: Vote Choice
#
#====================================================================================#

#====================================================================================#
# US vote choice

Y <- as.matrix(lca_dat[, 1:8]) # indicators
lca_dat$vote_us <- as.numeric(lca_dat$vote_us)
Z <- as.matrix(lca_dat[, c(9:25,36)]) # covariates + US vote outcome

# specify structural mod and outcome
structural_descriptor <- list(
  covariate = list(
    model = "covariate",
    n_columns = as.integer(17),
    method = "newton-raphson",
    max_iter = as.integer(100)
  ),
  response = list(
    model = "categorical_nan", 
    n_columns = as.integer(1)
  )
)

model = stepmix(
  n_components = 4,
  measurement = "categorical", # indicators
  structural = structural_descriptor, 
  n_steps = 3,
  n_init = 15,
  assignment = "soft", 
  random_state = 42,
  correction = "BCH"
)

# fit mod, get bootstraps on US voters
fit1 <- stepmixr::fit(model, X = Y, Y = Z)

# bootstrap on US data
lca_datUS <- lca_dat %>% drop_na(vote_us) # only respondents who voted in the US
Y_US <- as.matrix(lca_datUS[, 1:8]) # indicators
Z_US <- as.matrix(lca_datUS[, c(9:25,36)]) # covariates (9-25, and distal outcome 36)

set.seed(42)
fit1_bs <- bootstrap(
  fit1, Y_US, Z_US, 
  n_repetitions = 1000, 
  progress_bar = T)

samples <- fit1_bs$samples

# class-by-party probs in wide format
pis_wide <- samples %>%
  filter(model_name == "response", param == "pis") %>%
  pivot_wider(
    id_cols = rep, # replicate
    names_from = c(class_no, variable), # classes, and party
    values_from = value,
    names_glue  = "class{class_no}_{variable}"
  )

# get contrasts relative to complementers (class 2)
marginal_effects <- pis_wide %>%
  transmute(
    Democrat_Uncertains = class0_feature_17_0 - class2_feature_17_0, 
    Republican_Uncertains = class0_feature_17_1 - class2_feature_17_1,
    Other_Uncertains = class0_feature_17_2 - class2_feature_17_2,
    Democrat_Substituters = class1_feature_17_0 - class2_feature_17_0,
    Republican_Substituters   = class1_feature_17_1 - class2_feature_17_1,
    Other_Substituters = class1_feature_17_2 - class2_feature_17_2,
    Democrat_Skeptics = class3_feature_17_0 - class2_feature_17_0,
    Republican_Skeptics = class3_feature_17_1 - class2_feature_17_1,
    Other_Skeptics = class3_feature_17_2 - class2_feature_17_2
  )

# summary stats
party_diffs <- marginal_effects %>%
  pivot_longer(
    cols = everything(),
    names_to = "contrast",
    values_to = "marginal_effect"
  ) %>%
  # mean and sd for each contrast
  group_by(contrast) %>%
  dplyr::summarize(
    estimate = mean(marginal_effect),
    SE = sd(marginal_effect),
    .groups  = "drop"
  ) %>%
  # split names of party and class
  separate(
    contrast,
    into = c("party", "class_name"),
    sep  = "_"
  ) %>%
  mutate(
    party = factor(party, levels = c("Democrat", "Republican", "Other"))
  )

# plot
us <- ggplot(party_diffs, aes(
  x = class_name, 
  y = estimate, 
  ymin = estimate - 1.96*SE, 
  ymax = estimate + 1.96*SE, 
  color = class_name)
  ) +
  geom_pointrange(size=0.2) +  
  geom_hline(yintercept = 0, linetype = "dotted") +  
  coord_flip() +  
  ggtitle("US") +
  facet_wrap(~ party, nrow = 1) +  
  ylab("") + xlab("") + 
  scale_color_manual(values = c("#66C1A3", "#66A3CC", "#E897C2")) +
  theme_minimal(base_size = 9.5) + 
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    strip.text = element_text(size = 9.5, face = "bold")
  )

#====================================================================================#
# Canadian vote choice

lca_datCA <- lca_dat %>% drop_na(vote_can)

Y <- as.matrix(lca_datCA[, 1:8]) # indicators
lca_datCA$vote_can <- as.numeric(lca_datCA$vote_can)
Z <- as.matrix(lca_datCA[, c(9:25,37)]) # covariates + Canadian vote

# specify structural mod and outcome
structural_descriptor <- list(
  covariate = list(
    model = "covariate",
    n_columns = as.integer(17),
    method = "newton-raphson",
    max_iter = as.integer(100)
  ),
  response = list(
    model = "categorical_nan", 
    n_columns = as.integer(1)
  )
)

model = stepmix(
  n_components = 4,
  measurement = "categorical", 
  structural = structural_descriptor, 
  n_steps = 3,
  n_init = 15,
  assignment = "soft", 
  random_state = 42,
  correction = "BCH"
)

# fit model, get bootstraps
fit1 <- stepmixr::fit(model, X = Y, Y = Z)

set.seed(42)
fit1_bs <- bootstrap(
  fit1, Y, Z, 
  n_repetitions = 1000, 
  progress_bar = T)

samples <- fit1_bs$samples

# class-by-party probs in wide format
pis_wide <- samples %>%
  filter(model_name == "response", param == "pis") %>%
  pivot_wider(
    id_cols = rep, # replicate
    names_from = c(class_no, variable), # classes, and party
    values_from = value,
    names_glue  = "class{class_no}_{variable}"
  )

# get contrasts relative to complementers (class0)
marginal_effects <- pis_wide %>%
  transmute(
    LPC_Skeptics = class2_feature_17_0 - class0_feature_17_0,
    CPC_Skeptics = class2_feature_17_1 - class0_feature_17_1,
    NDP_Skeptics = class2_feature_17_2 - class0_feature_17_2,
    Other_Skeptics = class2_feature_17_3 - class0_feature_17_3,
    LPC_Uncertains = class3_feature_17_0 - class0_feature_17_0,
    CPC_Uncertains = class3_feature_17_1 - class0_feature_17_1,
    NDP_Uncertains = class3_feature_17_2 - class0_feature_17_2,
    Other_Uncertains = class3_feature_17_3 - class0_feature_17_3,
    LPC_Substituters = class1_feature_17_0 - class0_feature_17_0, 
    CPC_Substituters = class1_feature_17_1 - class0_feature_17_1,
    NDP_Substituters = class1_feature_17_2 - class0_feature_17_2,
    Other_Substituters = class1_feature_17_3 - class0_feature_17_3
  )

# summary stats
party_diffs <- marginal_effects %>%
  pivot_longer(
    cols = everything(),
    names_to = "contrast",
    values_to = "marginal_effect"
  ) %>%
  # mean and sd for each contrast
  group_by(contrast) %>%
  dplyr::summarize(
    estimate = mean(marginal_effect),
    SE = sd(marginal_effect),
    .groups  = "drop"
  ) %>%
  # split names of party and class
  separate(
    contrast,
    into = c("party", "class_name"),
    sep  = "_"
  ) %>%
  mutate(
    party = factor(party, levels = c("NDP", "LPC", "CPC", "Other"))
  )

# plot
can <- ggplot(party_diffs, aes(
  x = class_name, 
  y = estimate, 
  ymin = estimate - 1.96*SE, 
  ymax = estimate + 1.96*SE, 
  color = class_name)
) +
  geom_pointrange(size=0.2) + 
  geom_hline(yintercept = 0, linetype = "dotted") +  
  coord_flip() +  
  ggtitle("Canada") +
  facet_wrap(~ party, nrow = 1) +  
  ylab("") + xlab("") + 
  scale_color_manual(values = c("#66C1A3", "#66A3CC", "#E897C2")) +
  theme_minimal(base_size = 9.5) + 
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    strip.text = element_text(size = 9.5, face = "bold")
  )

# combined plot
combined_plot <- ggpubr::ggarrange(
  us, can, 
  ncol = 1, 
  nrow = 2,
  font.label = list(size = 6))

ggsave(combined_plot, filename = "POQ_Replication/Figure5.png", width = 6, height = 4)


