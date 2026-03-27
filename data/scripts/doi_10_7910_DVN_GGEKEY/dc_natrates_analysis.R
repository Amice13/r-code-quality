# A global panel dataset of dyadic dual citizenship acceptance
# Maarten Vink, Luuk van der Baaren, David Reichel
# International Migration Review, 2025
# Replication script for plots and tables in paper and supplementary materials
# Motivating application: Naturalization rates analysis
# Code prepared by David Reichel and Maarten Vink

# For analysis and visualisation we replicate and adjust parts of the code from:
# https://www.andrewheiss.com/blog/2021/11/08/beta-regression-guide/#zero-inflated-beta-regression-bayesian-style

#start with clean workspace
rm(list=ls(all=TRUE))

#load packages
library(tidyverse)        # ggplot, dplyr, %>%, and friends
library(brms)             # Bayesian modeling through Stan
library(tidybayes)        # Manipulate Stan objects in a tidy way
library(broom)            # Convert model objects to data frames
library(broom.mixed)      # Convert brms model objects to data frames
library(betareg)          # for betaregression in robustness check
library(extraDistr)       # Use extra distributions like dprop()
library(ggdist)           # Special geoms for posterior distributions
library(gghalves)         # Special half geoms
library(ggbeeswarm)       # Special distribution-shaped point jittering
library(ggrepel)          # Automatically position labels
library(patchwork)        # Combine ggplot objects
library(scales)           # Format numbers in nice ways
library(marginaleffects)  # Calculate marginal effects for regression models
library(scales)           # Use to scale to 0 to 1
library(tinytex)          #
library(flextable)        # 
library(vtable)
library(xtable)
library(gridExtra)
library(sjPlot)
library(see)
library(emmeans)
library(performance)        
library(modelsummary)
library(kableExtra)
library(gt)

set.seed(1234)  # Make everything reproducible

# Custom ggplot theme to make pretty plots
# Get the font at https://fonts.google.com/specimen/Barlow+Semi+Condensed
theme_clean <- function() {
  theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(),
          axis.title = element_text(),
          strip.text = element_text(size = rel(1), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA))
}

# Format things as percentage points
label_pp <- label_number(accuracy = 1, scale = 100, 
                         suffix = " pp.", style_negative = "minus")
label_pp_tiny <- label_number(accuracy = 0.01, scale = 100, 
                              suffix = " pp.", style_negative = "minus")

#Import dataset with naturalisation rates from Eurostat and dyadic dual citizenship acceptance from GLOBALCIT
  d <- read.csv("dc_natrates_dyadic_data_2023-06-24.csv") %>% # replication file based on Eurostat data from 2023-06-24
    # replace by updated date reference when running the replication script "dc_natrates_dataprep.R"
  mutate(EU_status = factor(EU_status, 
                            levels = c("EU_none", "EU_orig", "EU_dest", "EU_both"),
                            labels = c("None", "Only origin", "Only destination", "Both"))) %>%
  mutate(dc = dc_dy_bin_c > 0) %>%
  mutate(dc2 = dc2_dy_bin_c > 0) %>%
  # We recode zeros and ones very slightly, so that these can be modelled in beta regressions. 
  mutate(nat_rate2 = ifelse(nat_rate == 1, 0.9999,
                            ifelse(nat_rate == 0, 0.0001,
                                   nat_rate))) %>%
  # we create another one keeping zeros, as we will run a zero inflated regression model
  mutate(nat_rate3 = ifelse(nat_rate == 1, 0.9999,
                            nat_rate)) %>%
  mutate(yearc = year - 2008) %>% # normalize year
  mutate(gdp_o_log = log(gdp_o)) %>% #first do a log transform
  mutate(gdp_o_rs = rescale(gdp_o_log)) %>% #then scale values in 'gdp_o' column to be between 0 and 1
  mutate(income_o = factor(income_o)) |>
  select(nat_rate, nat_rate2, nat_rate3, iso2_o, iso2_d, dyad, dc, dc2, dc_dy_bin_c, 
         dc1_dy_bin_c, dc2_dy_bin_c, EU_status, year, yearc, EU_member_o, income_o, income_d, gdp_o_rs,
         EU_member_d, cit_acq, citizenship) %>%
  drop_na(dc_dy_bin_c, nat_rate3, income_o, gdp_o_rs, EU_status, yearc, iso2_o, iso2_d, dyad) 
  # note: we restrict also by no NA's for GDP, even if we do not use gdp_o_rs in final analyses
  # included here only for replication purpose

# prepare sub-samples for heterogeneity analysis by origin country income
table(d$income_o)
# High income          Low income Lower middle income      Not classified Upper middle income 
# 15048                4913               11004                   0               11270 

# Low and Lower middle income 
low_mid <- c("Low income", "Lower middle income")
mid_high <- c("Upper middle income", "High income")
d2a <- d |>
  filter(income_o %in% low_mid)
nrow(d2a) #15917
d2b <- d |>
  filter(income_o %in% mid_high)
nrow(d2b) #26318

#DESCRIPTIVES

# Prepare table with summary statistics for SM4
# Calculate summary statistics with xtable command st() to easily combine numeric and factor variables 
levels(d$income_o)
d |>
  mutate(income_o=droplevels(income_o)) |>
  select(nat_rate3,
         dc_dy_bin_c, dc2_dy_bin_c, 
         year, income_o, EU_status) |>
  st(out = "latex", file = "TableS3_sum_stat_natrates.tex",
  labels = c("Naturalization rate",
  "Dyadic dc acceptance (any restriction)", "Dyadic dc acceptance (consistent restriction)",
     "Year", "Origin country income group",
     "EU status"),
  title = "Summary statistics of variables in naturalization rate analysis")

# Description
d2 <- d %>% 
  group_by(dyad) %>% 
  mutate(yearb = year-min(year),
         yearc = year-2008) %>% 
  ungroup()

n_obs <- nrow(d)
n_obs

n_dest <- length(unique(d$iso2_d))
n_dest
#[1] 30
dest <- paste(unique(d$iso2_d), collapse = ", ")
dest
#[1] "AT, BE, CH, CZ, DE, DK, FI, HU, NL, NO, PL, SE, PT, SI, ES, LV, IS, LU, IT, GB, SK, IE, BG, LI, RO, EE, LT, FR, GR, HR"
n_orig <- length(unique(d$iso2_o))
n_orig
#[1] 186
orig <- paste(unique(d$iso2_o), collapse = ", ")

n_dyads <- length(unique(d$dyad))
n_dyads
#[1] 3128
mn_dyads <- count(d, dyad) %>% pull(n) %>% mean() %>% round()
mn_dyads
#[1] 14
ma_dyads <- count(d, dyad) %>% pull(n) %>% max() %>% round()
ma_dyads
#[1] 20
mi_dyads <- count(d, dyad) %>% pull(n) %>% min() %>% round()
mi_dyads
#[1] 1
mi_year = min(d$year)
mi_year # 2002
ma_year = max(d$year)
ma_year # 2021

#average nat rate
mn_natrate <- mean(d$nat_rate)
mn_natrate
#[1] 0.035542

# rates by year
d %>%
  group_by(year) %>%
  summarise(nat_rate = mean(nat_rate),
            n = n())
# year nat_rate     n
# <int>    <dbl> <int>
# 1  2002   0.0514  1597
# 2  2003   0.0387  1759
# 3  2004   0.0464  1423
# 4  2005   0.0443  1461
# 5  2006   0.0430  1409
# 6  2007   0.0378  1589
# 7  2008   0.0423  2087
# 8  2009   0.0373  2057
# 9  2010   0.0377  1956
# 10  2011   0.0393  1791
# 11  2012   0.0385  2040
# 12  2013   0.0357  2197
# 13  2014   0.0356  2304
# 14  2015   0.0328  2433
# 15  2016   0.0344  2607
# 16  2017   0.0336  2680
# 17  2018   0.0278  2724
# 18  2019   0.0251  2712
# 19  2020   0.0249  2805
# 20  2021   0.0299  2604

# rate by destination - highest
d %>%
  group_by(iso2_d) %>%
  summarise(nat_rate = mean(nat_rate),
            n = n()) %>% 
  arrange(-nat_rate)
# iso2_d nat_rate     n
# 1 SE       0.0806  2732
# 2 NL       0.0698  2802
# 3 NO       0.0600  2517
# 4 BE       0.0548  2287
# 5 IS       0.0438   725

# rates by destination - lowest
d %>%
  group_by(iso2_d) %>%
  summarise(nat_rate = mean(nat_rate),
            n = n()) %>% 
  arrange(nat_rate)
# 1 EE      0.00225   450
# 2 GR      0.00460   156
# 3 HR      0.00524   135
# 4 LT      0.00581   355
# 5 RO      0.00993   448

#trend plot in nat rates by year, averaged across iso3_d
summary(d)
mn_natrate <- mean(d$nat_rate)
d1 <- d |>
  group_by(dc) %>%
  summarise(nat_rate_dc = mean(nat_rate),
            n = n()) 
d1
labels <- c("FALSE" = "Dual citizenship restricted", "TRUE" = "Dual citizenship unrestricted")

p3a <- d |>
  group_by(dc, year) %>%
  summarise(nat_rate = mean(nat_rate),
            n = n()) |>
  ggplot(aes(x = year, y = nat_rate, color = dc)) + 
  geom_bar(aes(fill = dc), colour = "black", stat='identity') +
  facet_wrap(~dc, labeller=labeller(dc = labels))+
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("darkred", "#238B45")) +
  scale_colour_manual(values = c("darkred", "#238B45")) +
  geom_hline(data=d1, mapping = aes(yintercept = nat_rate_dc), linetype = 2) +
  geom_text(data=d1, aes(x = 2020, y = nat_rate_dc), label = round(d1$nat_rate_dc*100, 1), size = 5, vjust = -0.3) +
  geom_text(data=d1, aes(x = 2017, y = nat_rate_dc), label = "mean:", size = 5, vjust = -0.3) +
  labs(y = "", 
       x = "")+
  ggtitle("a. Trend in naturalization rates across 29 European destinations, 2002-2021\n") +
  theme_clean()+
  theme(legend.position="none")
p3a
# ggsave(p3a, file = "figures/plot3a_trend.png", width = 7, height = 5)

# same plot with alternative dc measure for Figure S6
d1b <- d |>
  group_by(dc2) %>%
  summarise(nat_rate_dc2 = mean(nat_rate),
            n = n()) 
p3a_2 <- d |>
  group_by(dc2, year) %>%
  summarise(nat_rate = mean(nat_rate),
            n = n()) |>
  ggplot(aes(x = year, y = nat_rate, color = dc2)) + 
  geom_bar(aes(fill = dc2), colour = "black", stat='identity') +
  facet_wrap(~dc2, labeller=labeller(dc2 = labels))+
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("darkred", "#238B45")) +
  scale_colour_manual(values = c("darkred", "#238B45")) +
  geom_hline(data=d1b, mapping = aes(yintercept = nat_rate_dc2), linetype = 2) +
  geom_text(data=d1b, aes(x = 2020, y = nat_rate_dc2), label = round(d1b$nat_rate_dc2*100, 1), size = 3, vjust = -0.3) +
  geom_text(data=d1b, aes(x = 2018, y = nat_rate_dc2), label = "mean:", size = 3, vjust = -0.3) +
  labs(y = "", 
       x = "")+
  ggtitle("a. Trend in naturalization rates across 29 European destinations, 2002-2021\n(consistent dual citizenship restrictions)\n") +
  theme_clean()+
  theme(legend.position="none")
p3a_2
# ggsave(p3a_2, file = "plot3a_trend2.png", width = 7, height = 5)



###MODELS

#main model with dc
# uncheck these lines to run the main model
# m1a <- bf(
#   # mu (mean) part
#   nat_rate3 ~ dc + income_o + EU_status + yearc +
#     (1 + yearc | iso2_o) + (1 + yearc | iso2_d) + (1 | dyad),
#   # phi (precision) part
#   phi ~ dc + (1 + yearc | iso2_o) + (1 + yearc | iso2_d) + (1 | dyad),
#   # alpha (zero-inflation) part
#   zi ~ dc + income_o + (1 + yearc | iso2_o) + (1 + yearc | iso2_d) + (1 | dyad)
# )
# 
# get_prior(
#   m1a,
#   data = d,
#   family = zero_inflated_beta()
# )
# 
# priors1a <- c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
#               set_prior("normal(0, 1)", class = "b"))
# 
# m1a <- brm(
#   model1a,
#   data = d,
#   family = zero_inflated_beta(),
#   prior = priors1a,
#   init = 0,
#   control = list(adapt_delta = 0.97,
#                  max_treedepth = 12),
#   chains = 4, iter = 2000, warmup = 1000,
#   cores = 4, seed = 1234, 
#   threads = threading(2),
#   backend = "cmdstanr",
#   file = "model1a"
# )

# read in m1a
m1a <- readRDS(file = "model1a.rds")
# check model outpu
summary(m1a)

#robustness check 1: main model with dc2
# uncheck these lines to run the robustness check1 model
# m1b <- bf(
#   # mu (mean) part
#   nat_rate3 ~ dc2 + income_o + EU_status + yearc +
#     (1 + yearc | iso2_o) + (1 + yearc | iso2_d) + (1 | dyad),
#   # phi (precision) part
#   phi ~ dc2 + (1 + yearc | iso2_o) + (1 + yearc | iso2_d) + (1 | dyad),
#   # alpha (zero-inflation) part
#   zi ~ dc2 + income_o + (1 + yearc | iso2_o) + (1 + yearc | iso2_d) + (1 | dyad)
# )
# 
# get_prior(
#   m1b,
#   data = d,
#   family = zero_inflated_beta()
# )
# 
# priors1b <- c(set_prior("student_t(3, 0, 2.5)", class = "Intercept"),
#                set_prior("normal(0, 1)", class = "b"))
# 
# m1b <- brm(
#   model1b,
#   data = d,
#   family = zero_inflated_beta(),
#   prior = priors1b,
#   init = 0,
#   control = list(adapt_delta = 0.97,
#                  max_treedepth = 12),
#   chains = 4, iter = 2000, warmup = 1000,
#   cores = 4, seed = 1234, 
#   threads = threading(2),
#   backend = "cmdstanr",
#   file = "model1b"
# )

# read in m1b
m1b <- readRDS(file = "model1b.rds")
# check model output
summary(m1b)

# save the table for two main models dc and dc2
# tab_model(m1a,m1b, 
#           show.r2 = TRUE,
#           show.icc = TRUE,
#           show.re.var = FALSE,
#           p.style = "scientific",
#           terms = c("Intercept", "dcTRUE", 
#                     "income_oLowincome", "income_oLowermiddleincome","income_oUppermiddleincome",
#                     "EU_statusOnlyorigin", "EU_statusOnlydestination","EU_statusBoth",
#                     "yearc", "dc2TRUE"),
#           pred.labels = c("Intercept", "Dual citizenship (dyadic, corrected)", 
#                           "Origin: low income", "Origin: lower-middle income", "Origin: upper-middle income",
#                           "EU: only origin", "EU: only destination", "EU: both", 
#                           "Year (centered)","Dual citizenship (dyadic, corrected)"), 
#           emph.p = TRUE,show.reflvl = FALSE, 
#           dv.labels = c("m1a: any dual citizenship restriction", "m1b: consistent dual citizenship restriction"),
#           file = "tables/m1a_m1b.html")

##############################
# Model checks m1a and m1b

# Model convergence checks
plot(m1a) # hit enter to see the convergence of the sampling for all paramenters
plot(m1b) # hit enter to see the convergence of the sampling for all paramenters

# Model check based on distribution and accuracy of predictions
p1 <- pp_check(m1a, ndraws = 30) + scale_x_continuous(limits = c(0,0.1)) + labs(title = "m1a")
p2 <- pp_check(m1b, ndraws = 30) + scale_x_continuous(limits = c(0,0.1)) + labs(title = "m1b")

# compare them
p1 / p2 

# Figure S5
jpeg('figures/FigS5.accuracy.pred.jpeg',  width = 8, height = 10, units = 'in', res = 400)
grid.arrange(arrangeGrob(p1, p2,nrow=2, ncol=1))
dev.off()

#################################

### Robustness check 
# Fixed effects betaregression
# https://www.rdocumentation.org/packages/betareg/versions/3.2-1/topics/betareg

# uncheck to run betaregression model 2a
# filter out dyads with only 1 observation
dyads1 <- count(d, dyad) |> dplyr::filter(n == 1) |> pull(dyad)
d <- dplyr::filter(d, !dyad %in% dyads1) # 42151 obs of 21 vars

# run betaregression, same model as m1a but with origin and destination country-year fixed-effects (so without income_o + EU_status) and dyad FE
# m2a <- betareg(nat_rate2 ~ dc + iso2_o:yearc + iso2_d:yearc + dyad, data = d, link = "logit")
# summary(m2a)
# saveRDS(m2a, file = "m2a_beta_fixed_full.rds")
m2a <- readRDS(file = "m2a_beta_fixed_full.rds")

# make a Latex table with all three models
models1_2 <- list("Model 1a" = m1a, "Model 1b" = m1b, "Model fixed" = m2a)
ms1_2 <- modelsummary(models1_2,
                      output = "model_results1_2.tex",
                      statistic = NULL,
                      estimate = "{estimate} [{conf.low}, {conf.high}]",
                      coef_omit = "^dyad|^iso2..") 
# coef_omit = "^(?!.*dc)") 
kableExtra::save_kable(ms1_2, file = 'model_results1_2.tex')

# neat output table with stargazer
library(stargazer)
stargazer(m1a, m1b, m2a,
          type = "text", omit = c("iso2_o", "iso2_d", "dyad", "yearc"),
          dep.var.caption = "Dyadic naturalisation rate",
          dep.var.labels.include = FALSE,
          header = F,
          title = "",
          add.lines = list(c("Origin-Year", "RE", "RE", "FE"), c("Destination-Year", "RE", "RE", "FE"), 
                           c("Dyad", "RE", "RE", "FE")),
          covariate.labels = c("Dual citizenship acceptance"),
          notes = "", 
          out = c("models_results_sg.txt", "models_results_sg.tex"))


######PLOTS

# Prepare 3 additional plots for combi plot Figure 3, next to Fig 3a (descriptive trend naturalization rates)
# model m1a

# Figure 3b: plot predicted naturalization rate by DC
# set values of other variables at mean
table(d$income_o)
# High income          Low income Lower middle income Upper middle income 
# 15048                4913               11004               11270 


# define newdata for posterior prediction
# set year 2008 (middle of observations), Upper middle income origin income, only EU destinations
newd2 <- expand_grid(dc = c("FALSE", "TRUE"), 
                     yearc = 0, 
                     income_o = "Upper middle income",
                     EU_status = 'Only destination')

m1a_epred <- m1a %>% 
  epred_draws(newdata = newd2, re_formula = NA) %>% 
  mutate(dc = dc == 'TRUE')

# estimated rates and hdi
m1a_epred_point_estimates <- m1a_epred %>%
  mutate(dc = ifelse(dc == TRUE,
                              "Dual citizenship accepted",
                              "Dual citizenship restricted")) %>% 
  group_by(dc) %>%
  summarise(med_exp_pred = median(.epred),
            lo_exp_pred = quantile(.epred, 0.05),
            hi_exp_pred = quantile(.epred, 0.95))

m1a_epred_point_estimates
# with income_o at Upper middle income 
# dc                          med_exp_pred lo_exp_pred hi_exp_pred
# 1 Dual citizenship accepted         0.0465      0.0311      0.0612
# 2 Dual citizenship restricted       0.0314      0.0189      0.0426

nr_dc_yes <- 100*m1a_epred_point_estimates$med_exp_pred[1] |> round(4)
nr_dc_yes #4.65
nr_dc_no <- 100*m1a_epred_point_estimates$med_exp_pred[2] |> round(4)
nr_dc_no #3.14

#plot
p3b <- m1a_epred %>% 
  mutate(dc = ifelse(dc == TRUE,
                              "Dual citizenship accepted",
                              "Dual citizenship restricted")) %>% 
  ggplot(aes(x = .epred, fill = dc)) +
  stat_halfeye(.width = c(0.8, 0.95), point_interval = "median_hdi") +
  scale_fill_manual(values = c("#238B45", "darkred")) +
  guides(fill = "none") +
  ggtitle("b. Predicted naturalisation rate, by dyadic dual citizenship policy\n")+
  labs(x = "Predicted naturalisation rate (in percentage)", y = NULL,
       caption = "80% and 95% credible intervals shown in black") +
  theme_clean() +
  theme(legend.position = "none")+
  scale_x_continuous(labels = label_percent()) +
  facet_wrap(~dc, ncol = 1) +
  geom_vline(data = m1a_epred_point_estimates, aes(xintercept = med_exp_pred),
             linetype = 2) +
  geom_text(data = m1a_epred_point_estimates, aes(x = med_exp_pred, y = 0.95,
                                                 label = round(med_exp_pred*100, 1),
                                                 hjust = -0.2,
                                                 size = 3))
p3b
# ggsave(p3b, file = "Figure3b_pred_m1a-repl.png", width = 5, height = 6)


# Figure 3c: Average Marginal Effects plot main model - for a dyad that is based on random draws from model 1a
# set values of other variables at mean
ame_m1a <- m1a %>% 
  emmeans(~ dc + dyad,
          at = list(yearc = 0, 
                    income_o = "Lower middle income", 
                    EU_status = 'Only destination',
                    dyad = "random dyad"),
          epred = TRUE, re_formula = NULL, 
          allow_new_levels = TRUE, 
          sample_new_levels = "gaussian",
          rg.limit = 12000) %>% 
  contrast(method = "revpairwise") %>% 
  gather_emmeans_draws()

ame_m1a %>% median_hdi() # Note that not all numbers will be reproduced exactly due to the use of random sampling processes at this point in the analysis.
# contrast                             .value  .lower .upper .width .point .interval
# <chr>                                 <dbl>   <dbl>  <dbl>  <dbl> <chr>  <chr>    
#1 TRUE random dyad - FALSE random dyad 0.0151 0.00434 0.0318   0.95 median hdi      


# calculate median
m1a_median <- ame_m1a |>
  summarise(med_exp_pred = median(.value),
            lo_exp_pred = quantile(.value, 0.05),
            hi_exp_pred = quantile(.value, 0.95))
m1a_median
# contrast                             med_exp_pred lo_exp_pred hi_exp_pred
# 1 TRUE random dyad - FALSE random dyad       0.0149     0.00618      0.0310

#plot
p3c <- ggplot(ame_m1a, aes(x = .value)) +
  stat_halfeye(fill = '#238B45') +
  labs(x = "\nAverage marginal effect (in percentage points increase)\n", y = NULL,
       title = "c. Marginal effect of dyadic dual citizenship acceptance on naturalization rates\n(average predicted outcome for a dyad that is based on random draws from model 1a)",
       caption = "80% and 95% credible intervals shown in horizontal black lines") +
  theme_clean() +
  theme(legend.position = "none")+
  scale_x_continuous(labels = label_pp) +
  geom_vline(data = m1a_median, aes(xintercept = med_exp_pred),
             linetype = 2) +
  geom_text(data = m1a_median, aes(x = med_exp_pred, y = 0.95,
                                   label = round(med_exp_pred*100, 1)), hjust = -0.2, size = 5)
p3c
#ggsave(p3c, file = "Fig3c_ame_m1a.png", width = 9, height = 5)

### Interpretation substantive effects Fig 3c ###

# How many more new citizens could have been expected if dual citizenship
# would be allowed everywhere?

newd3a <- d %>% 
  select(year, yearc, dc, iso2_d, iso2_o, dyad, nat_rate3, cit_acq, citizenship, EU_status, income_o) %>% 
  filter(year == 2020) |>
  mutate(income_o=droplevels(income_o))
levels(newd3a$income_o)
# [1] "High income"         "Low income"          "Lower middle income" "Upper middle income"

m5_epred_re_exp_actfull <- m1a %>% 
  epred_draws(newdata = newd3a) %>% 
  group_by(dyad) %>% 
  summarise(nat_rate_exp_med = median(.epred),
            nat_rate_exp_low = quantile(.epred, 0.1),
            nat_rate_exp_hig = quantile(.epred, 0.9),
            nat_rate3 = mean(nat_rate3),
            dc_dy_bin_c = mean(dc),
            cit_acq = mean(cit_acq),
            income_o = "Lower middle income",
            EU_status = "Only destination",
            citizenship = mean(citizenship)) %>% 
  ungroup() 

m5_epred_re_exp_act <- m5_epred_re_exp_actfull %>% 
  mutate(cit_acq_exp = nat_rate_exp_med*citizenship,
         cit_acq_exp_low = nat_rate_exp_low*citizenship,
         cit_acq_exp_hig = nat_rate_exp_hig*citizenship) %>% 
  summarise(sum_cit_acq_exp = sum(cit_acq_exp),
            low_cit_acq_exp = sum(cit_acq_exp_low),
            hig_cit_acq_exp = sum(cit_acq_exp_hig),
            sum_cit_acq_act = sum(cit_acq))

newd3b <- d %>% 
  select(year, yearc, dc, iso2_d, iso2_o, dyad, nat_rate3, cit_acq, citizenship, EU_status, income_o) %>% 
  filter(year == 2020) |>
  mutate(income_o=droplevels(income_o)) |>
  mutate(dc = TRUE)

m5_epred_re_exp_dual1 <- m1a %>% 
  epred_draws(newdata = newd3b) %>% 
  group_by(dyad) %>% 
  summarise(nat_rate_exp_med = median(.epred),
            nat_rate_exp_low = quantile(.epred, 0.1),
            nat_rate_exp_hig = quantile(.epred, 0.9),
            nat_rate3 = mean(nat_rate3),
            dc = mean(dc),
            cit_acq = mean(cit_acq),
            citizenship = mean(citizenship)) %>% 
  ungroup() %>% 
  mutate(cit_acq_exp = nat_rate_exp_med*citizenship) %>% 
  summarise(sum_cit_acq_exp = sum(cit_acq_exp),
            min_cit_acq_exp = min(cit_acq_exp),
            max_cit_acq_exp = max(cit_acq_exp),
            sum_cit_acq_act = sum(cit_acq))

more_citizens_if_all_dual <- (m5_epred_re_exp_dual1$sum_cit_acq_exp - m5_epred_re_exp_act$sum_cit_acq_exp) |> round()
more_citizens_if_all_dual # Note that not all numbers will be reproduced exactly due to the use of random sampling processes at this point in the analysis.
# 104450

act_num_cit_aq <- m5_epred_re_exp_dual1$sum_cit_acq_act
pct_more <- round(100*(more_citizens_if_all_dual/act_num_cit_aq),2)
pct_more
# [1] 14.87

# Interpretation: This effect size implies a theoretical increase of naturalizations with 14.9 percent in the year 2020 if all
# countries of origin and destination would have allowed for dual citizenship, roughly corresponding
# to 105 thousand more naturalizations in one year in these 30 European destinations.



# Fig 3d: posterior predictions and marginal effects plot split by origin income group

# Plot with predicted naturalization rate by DC and by income group
# set values of other variables at mean
newd2a <- expand_grid(dc = c("FALSE", "TRUE"), 
                     yearc = 0, 
                     income_o = c('High income', 'Upper middle income', 'Lower middle income', 'Low income'),
                     EU_status = 'Only destination')

m1a_epred <- m1a %>% 
  epred_draws(newdata = newd2a, re_formula = NA) %>% #NOTE: SETTING AT NA MEANS ignoring any country and dyad-specific deviations of the intercept or slope
  mutate(dc = dc == 'TRUE') # so this is the 'global grand mean' ('an average that transcends country/dyad differences') in terms of Heiss' blog

#estimate rates by dc
m1a_epred_point_estimates_raw <- m1a_epred %>%
  mutate(dc = ifelse(dc == TRUE,
                     "Dual citizenship accepted",
                     "Dual citizenship restricted"))

m1a_epred_point_estimates <- m1a_epred_point_estimates_raw %>%
  group_by(dc) %>%
  summarise(med_exp_pred = median(.epred),
            lo_exp_pred = quantile(.epred, 0.05),
            hi_exp_pred = quantile(.epred, 0.95))
m1a_epred_point_estimates
# dc                          med_exp_pred lo_exp_pred hi_exp_pred
# 1 Dual citizenship accepted         0.0488     0.0122       0.0715
# 2 Dual citizenship restricted       0.0327     0.00723      0.0501

# now by income
m1a_epred_point_estimates <- m1a_epred %>%
mutate(dc = ifelse(dc == TRUE,
                   "Dual citizenship accepted",
                   "Dual citizenship restricted")) %>% 
  group_by(income_o) %>%
  summarise(med_exp_pred = median(.epred),
            lo_exp_pred = quantile(.epred, 0.05),
            hi_exp_pred = quantile(.epred, 0.95))
m1a_epred_point_estimates
# income_o            med_exp_pred lo_exp_pred hi_exp_pred
# 1 High income               0.0129     0.00559      0.0225
# 2 Low income                0.0488     0.0291       0.0729
# 3 Lower middle income       0.0460     0.0273       0.0693
# 4 Upper middle income       0.0380     0.0217       0.0577

# Interpretation: We find that naturalization rates are indeed substantially lower among migrants
# from high-income economies (on average 1.3 percent), compared to those from upper-middle-income
# countries (3.8 percent), lower-middle income countries (4.6 percent) and low-income countries (4.9 percent).

# estimated rates and hdi
m1a_epred_point_estimates <- m1a_epred %>%
  mutate(dc = ifelse(dc == TRUE,
                     "Dual citizenship accepted",
                     "Dual citizenship restricted")) %>% 
  group_by(dc, income_o) %>%
  summarise(med_exp_pred = median(.epred),
            lo_exp_pred = quantile(.epred, 0.05),
            hi_exp_pred = quantile(.epred, 0.95))

m1a_epred_point_estimates
# dc                          income_o            med_exp_pred lo_exp_pred hi_exp_pred
# 1 Dual citizenship accepted   High income               0.0164     0.00850      0.0242
# 2 Dual citizenship accepted   Low income                0.0594     0.0407       0.0773
# 3 Dual citizenship accepted   Lower middle income       0.0564     0.0388       0.0739
# 4 Dual citizenship accepted   Upper middle income       0.0465     0.0311       0.0612
# 5 Dual citizenship restricted High income               0.0102     0.00484      0.0162
# 6 Dual citizenship restricted Low income                0.0410     0.0257       0.0545
# 7 Dual citizenship restricted Lower middle income       0.0384     0.0243       0.0518
# 8 Dual citizenship restricted Upper middle income       0.0314     0.0189       0.0426

# Interpretation: Due to variation in naturalization levels, the difference in means of naturalization rates
# between dual citizenship restricting and accepting dyads is also lowest among high-income origin
# migrants (0.6 percentage points), compared to lower income origin (1.5-1.8 percentage points).

nr_dc_yes_high <- 100*m1a_epred_point_estimates$med_exp_pred[1] |> round(4)
nr_dc_yes_low <- 100*m1a_epred_point_estimates$med_exp_pred[2] |> round(4)
nr_dc_yes_low_middle <- 100*m1a_epred_point_estimates$med_exp_pred[3] |> round(4)
nr_dc_yes_upper_middle <- 100*m1a_epred_point_estimates$med_exp_pred[4] |> round(4)
nr_dc_no_high <- 100*m1a_epred_point_estimates$med_exp_pred[5] |> round(4)
nr_dc_no_low <- 100*m1a_epred_point_estimates$med_exp_pred[6] |> round(4)
nr_dc_no_low_middle <- 100*m1a_epred_point_estimates$med_exp_pred[7] |> round(4)
nr_dc_no_upper_middle <- 100*m1a_epred_point_estimates$med_exp_pred[8] |> round(4)

# visualisation a la Heiss
# https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/#binary-effect-1
m1a_epred$income_o = with(m1a_epred, 
                           factor(income_o,
                                  levels = (c("High income", "Upper middle income", "Lower middle income", "Low income"))))
p3d1 <- m1a_epred |>
  ggplot(aes(x = .epred, y = income_o, fill = dc)) +
  stat_halfeye() +
  labs(x = "\nPredicted naturalization rate", y = NULL,
       fill = "Dual citizenship allowed",
       subtitle = "Posterior predictions") +
  theme_clean() +
  scale_fill_manual(values = c("darkred", "#238B45")) +
  scale_x_continuous(labels = label_percent()) +
  theme(legend.position = "bottom")
p3d1

# now calculate the marginal effect (relative increase)
m1a_epred_wide <- m1a_epred %>%
  ungroup() %>% 
  select(-.row) %>%
  pivot_wider(names_from = dc, values_from = .epred) %>%
  mutate(rel_dif = 100*((`TRUE`/`FALSE`)-1))
m1a_epred_wide

m1a_epred_wide |>
  group_by(income_o) |>
  summarize(mean = mean(rel_dif),
            sum = sum(rel_dif))
# income_o             mean     sum
# 1 High income          61.7 246815.
# 2 Upper middle income  49.7 198782.
# 3 Lower middle income  48.8 195083.
# 4 Low income           46.7 186968

# When comparing the relative impact of dual citizenship acceptance, we find that this is larger
# among migrants from high-income countries as among this group those who can naturalize with dual
# citizenship are 62 percent more likely to do so. Among migrants from low-income countries, 
# naturalization rates are 47 percent higher if dual citizenship is accepted.

p3d2 <- m1a_epred_wide |>
  ggplot(aes(x = rel_dif, y = income_o)) +
  stat_halfeye(fill = "#238B45") +
  labs(x = "\nEstimated percentage increase in predicted naturalization rate for accepted dual citizenship vs restricted dual citizenship", y = NULL,
       subtitle = "Marginal effect (relative increase)") +
  theme_clean() +
  # scale_x_continuous(labels = label_percent()) +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 100))
p3d2


# Combined plot
p3d <- grid.arrange(arrangeGrob(p3d1, p3d2,nrow=1, ncol=2, 
                                  top = "d. Heterogeneous dual citizenship effects by origin country income group\n"))
p3d
# ggsave(p3d, file = "plot3d_effect_income.png", width = 10, height = 8)

# combi plot Fig 3
jpeg('Fig3.natrates.combi.jpeg',  width = 20, height = 14, units = 'in', res = 400)
grid.arrange(arrangeGrob(p3a, p3b, p3c, p3d,nrow=2, ncol=2))
dev.off()

#Save combined as .eps file
setEPS()
postscript("Fig3.natrates.combi.eps", width = 20, height = 14)
grid.arrange(arrangeGrob(p3a, p3b, p3c,p3d,nrow=2, ncol=2))
dev.off()



# Fig S6: Prepare 3 additional plots for combi plot Figure S6, next to Fig 3b (descriptive trend naturalization rates)
# model m1b

# Fig S6b posterior predictions
newd22 <- expand_grid(dc2 = c("FALSE", "TRUE"), 
                      yearc = 0, 
                      income_o = "Upper middle income",
                      EU_status = 'Only destination')

m1a_epred <- m1b %>% 
  epred_draws(newdata = newd22, re_formula = NA) %>% 
  mutate(dc2 = dc2 == 'TRUE')

# estimated rates and hdi
m1a_epred_point_estimates <- m1a_epred %>%
  mutate(dc2 = ifelse(dc2 == TRUE,
                      "Dual citizenship accepted",
                      "Dual citizenship restricted")) %>% 
  group_by(dc2) %>%
  summarise(med_exp_pred = median(.epred),
            lo_exp_pred = quantile(.epred, 0.05),
            hi_exp_pred = quantile(.epred, 0.95))

m1a_epred_point_estimates
# with income_o at Upper middle income 
# dc2                         med_exp_pred lo_exp_pred hi_exp_pred
# 1 Dual citizenship accepted         0.0462      0.0307      0.0609
# 2 Dual citizenship restricted       0.0297      0.0184      0.0405

nr_dc2_yes <- 100*m1a_epred_point_estimates$med_exp_pred[1] |> round(4)
nr_dc2_yes
nr_dc2_no <- 100*m1a_epred_point_estimates$med_exp_pred[2] |> round(4)
nr_dc2_no
pS6b_2 <- m1a_epred %>% 
  mutate(dc2 = ifelse(dc2 == TRUE,
                      "Dual citizenship accepted",
                      "Dual citizenship restricted")) %>% 
  ggplot(aes(x = .epred, fill = dc2)) +
  stat_halfeye(.width = c(0.8, 0.95), point_interval = "median_hdi") +
  scale_fill_manual(values = c("#238B45", "darkred")) +
  guides(fill = "none") +
  ggtitle("b. Predicted naturalisation rate, by dyadic dual citizenship policy\n")+
  labs(x = "Predicted naturalisation rate (in percentage)", y = NULL,
       caption = "80% and 95% credible intervals shown in black") +
  theme_clean() +
  theme(legend.position = "none")+
  scale_x_continuous(labels = label_percent()) +
  facet_wrap(~dc2, ncol = 1) +
  geom_vline(data = m1a_epred_point_estimates, aes(xintercept = med_exp_pred),
             linetype = 2) +
  geom_text(data = m1a_epred_point_estimates, aes(x = med_exp_pred, y = 0.95,
                                                  label = round(med_exp_pred*100, 1),
                                                  hjust = -0.2,
                                                  size = 3))
pS6b_2
#ggsave(pS6b_2, file = "FigureS6b_pred_m1b.png", width = 5, height = 6)

# Fig S6c with AME plot model 
ame_m1b <- m1b %>% 
  emmeans(~ dc2 + dyad,
          at = list(yearc = 0, 
                    income_o = "Lower middle income", 
                    EU_status = 'Only destination',
                    dyad = "random dyad"),
          epred = TRUE, re_formula = NULL, 
          allow_new_levels = TRUE, 
          sample_new_levels = "gaussian",
          rg.limit = 12000) %>% 
  contrast(method = "revpairwise") %>% 
  gather_emmeans_draws()

#plot
m1b_median <- ame_m1b |>
  summarise(med_exp_pred = median(.value)*100,
            lo_exp_pred = quantile(.value, 0.05)*100,
            hi_exp_pred = quantile(.value, 0.95)*100)
m1b_median # Note that not all numbers will be reproduced exactly due to the use of random sampling processes at this point in the analysis.
# contrast                             med_exp_pred lo_exp_pred hi_exp_pred
# <chr>                                       <dbl>       <dbl>       <dbl>
#TRUE random dyad - FALSE random dyad         1.67       0.713        3.35

p3c_2 <- ggplot(ame_m1b, 
                aes(x = .value*100)) +
  stat_halfeye(fill = '#238B45') +
  labs(x = "Average marginal effect (in percentage points)", y = NULL,
       title = "Marginal effect of dyadic dual citizenship acceptance (consistent restrictions) on naturalization rates\n(for a hypothetical dyad that is based on random draws from model 1b)") +
  theme_clean() +
  theme(legend.position = "bottom") +
  geom_vline(data = m1b_median, aes(xintercept = med_exp_pred),
             linetype = 2) +
  geom_text(data = m1b_median, aes(x = med_exp_pred, y = 0.9,
                                   label = round(med_exp_pred, 1)), hjust = -0.2)
p3c_2
#ggsave(p3c_2, file = "Figure_ame_m1b.png", width = 9, height = 5)


# Fig S6d. Plot with predicted naturalization rate by DC2 and by income group
# set values of other variables at mean
newd2a2 <- expand_grid(dc2 = c("FALSE", "TRUE"), 
                      yearc = 0, 
                      income_o = c('High income', 'Upper middle income', 'Lower middle income', 'Low income'),
                      EU_status = 'Only destination')

m1b_epred <- m1b %>% 
  epred_draws(newdata = newd2a2, re_formula = NA) %>% #NOTE: SETTING AT NA MEANS ignoring any country and dyad-specific deviations of the intercept or slope
  mutate(dc2 = dc2 == 'TRUE') # so this is the 'global grand mean' ('an average that transcends country/dyad differences') in terms of Heiss' blog

#estimate rates by dc
m1b_epred_point_estimates <- m1b_epred %>%
  mutate(dc2 = ifelse(dc2 == TRUE,
                     "Dual citizenship accepted",
                     "Dual citizenship restricted")) %>% 
  group_by(dc2) %>%
  summarise(med_exp_pred = median(.epred),
            lo_exp_pred = quantile(.epred, 0.05),
            hi_exp_pred = quantile(.epred, 0.95))
m1b_epred_point_estimates
# dc                          med_exp_pred lo_exp_pred hi_exp_pred
# 1 Dual citizenship accepted         0.0493     0.0125       0.0736
# 2 Dual citizenship restricted       0.0316     0.00722      0.0491

# now by income
m1b_epred_point_estimates <- m1b_epred %>%
  mutate(dc2 = ifelse(dc2 == TRUE,
                     "Dual citizenship accepted",
                     "Dual citizenship restricted")) %>% 
  group_by(income_o) %>%
  summarise(med_exp_pred = median(.epred),
            lo_exp_pred = quantile(.epred, 0.05),
            hi_exp_pred = quantile(.epred, 0.95))
m1b_epred_point_estimates
# income_o            med_exp_pred lo_exp_pred hi_exp_pred
# 1 High income               0.0127     0.00556      0.0226
# 2 Low income                0.0493     0.0292       0.0757
# 3 Lower middle income       0.0454     0.0264       0.0707
# 4 Upper middle income       0.0365     0.0208       0.0577

# estimated rates and hdi
m1b_epred_point_estimates <- m1b_epred %>%
  mutate(dc2 = ifelse(dc2 == TRUE,
                     "Dual citizenship accepted",
                     "Dual citizenship restricted")) %>% 
  group_by(dc2, income_o) %>%
  summarise(med_exp_pred = median(.epred),
            lo_exp_pred = quantile(.epred, 0.05),
            hi_exp_pred = quantile(.epred, 0.95))

m1b_epred_point_estimates
# dc                          income_o            med_exp_pred lo_exp_pred hi_exp_pred
# 1 Dual citizenship accepted   High income              0.0166      0.00878      0.0239
# 2 Dual citizenship accepted   Low income               0.0617      0.0426       0.0799
# 3 Dual citizenship accepted   Lower middle income      0.0574      0.0388       0.0745
# 4 Dual citizenship accepted   Upper middle income      0.0462      0.0307       0.0609
# 5 Dual citizenship restricted High income              0.00996     0.00475      0.0153
# 6 Dual citizenship restricted Low income               0.0405      0.0261       0.0537
# 7 Dual citizenship restricted Lower middle income      0.0372      0.0234       0.0495
# 8 Dual citizenship restricted Upper middle income      0.0297      0.0184       0.0405

nr_dc2_yes_high <- 100*m1b_epred_point_estimates$med_exp_pred[1] |> round(4)
nr_dc2_yes_low <- 100*m1b_epred_point_estimates$med_exp_pred[2] |> round(4)
nr_dc2_yes_low_middle <- 100*m1b_epred_point_estimates$med_exp_pred[3] |> round(4)
nr_dc2_yes_upper_middle <- 100*m1b_epred_point_estimates$med_exp_pred[4] |> round(4)
nr_dc2_no_high <- 100*m1b_epred_point_estimates$med_exp_pred[5] |> round(4)
nr_dc2_no_low <- 100*m1b_epred_point_estimates$med_exp_pred[6] |> round(4)
nr_dc2_no_low_middle <- 100*m1b_epred_point_estimates$med_exp_pred[7] |> round(4)
nr_dc2_no_upper_middle <- 100*m1b_epred_point_estimates$med_exp_pred[8] |> round(4)

# visualisation a la Heis
# https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/#binary-effect-1
m1b_epred$income_o = with(m1b_epred, 
                           factor(income_o,
                                  levels = (c("High income", "Upper middle income", "Lower middle income", "Low income"))))
p3d1_2 <- m1b_epred |>
  ggplot(aes(x = .epred, y = income_o, fill = dc2)) +
  stat_halfeye() +
  labs(x = "\nPredicted naturalization rate", y = NULL,
       fill = "Dual citizenship allowed",
       subtitle = "Posterior predictions") +
  theme_clean() +
  scale_fill_manual(values = c("darkred", "#238B45")) +
  scale_x_continuous(labels = label_percent()) +
  theme(legend.position = "bottom")
p3d1_2

# now calculate the marginal effect (relative increase)
m1b_epred_wide <- m1b_epred %>%
  ungroup() %>% 
  select(-.row) %>%
  pivot_wider(names_from = dc2, values_from = .epred) %>%
  mutate(rel_dif = 100*((`TRUE`/`FALSE`)-1))
m1b_epred_wide

m1b_epred_wide |>
  group_by(income_o) |>
  summarize(mean = mean(rel_dif),
            sum = sum(rel_dif))
# income_o             mean     sum
# 1 High income          68.0 271830.
# 2 Upper middle income  56.9 227421.
# 3 Lower middle income  55.6 222296.
# 4 Low income           53.3 213201.

# When comparing the relative impact of dual citizenship acceptance, we find that this is larger
# among migrants from high-income countries as among this group those who can naturalize with dual
# citizenship are 68 percent more likely to do so. Among migrants from low-income countries, 
# naturalization rates are 53 percent higher if dual citizenship is accepted.

p3d2_2 <- m1b_epred_wide |>
  ggplot(aes(x = rel_dif, y = income_o)) +
  stat_halfeye(fill = "#238B45") +
  labs(x = "\nEstimated percentage increase in predicted naturalization rate for accepted dual citizenship vs restricted dual citizenship", y = NULL,
       subtitle = "Marginal effect (relative increase)") +
  theme_clean() +
  # scale_x_continuous(labels = label_percent()) +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(0, 100))
p3d2_2

# Combined plot
p3d_2 <- grid.arrange(arrangeGrob(p3d1_2, p3d2_2,nrow=1, ncol=2, 
                                top = "d. Heterogeneous dual citizenship effects by origin country income group\n"))
p3d_2
#ggsave(p3d_2, file = "plot3d_2_effect_income.png", width = 10, height = 8)

# combi plot Fig S6
jpeg('FigS6.natrates.dc2.combi.jpeg',  width = 20, height = 14, units = 'in', res = 400)
grid.arrange(arrangeGrob(p3a_2, p3b_2, p3c_2,p3d_2,nrow=2, ncol=2))
dev.off()

#Save combined as .eps file
setEPS()
postscript("FigS6.natrates.dc2.combi.eps", width = 20, height = 14)
grid.arrange(arrangeGrob(p3a_2, p3b_2, p3c_2,p3d_2,nrow=2, ncol=2))
dev.off()

##########END#########