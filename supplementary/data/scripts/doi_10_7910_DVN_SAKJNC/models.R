#----------------------------------------------
# Project: Statistical models for
# "Polarization, Political Turnover, and the Stability of Democratic 
# Foreign Policy"
# Accepted for publication in Foreign Policy Analysis
# Author: Matthew DiLorenzo
# Date last updated: 2025-12-17
# Description: This script generates the statistical models and figures in
# both the main text and appendix of the paper.
#-----------------------------------------------

# Load required packages
library(foreign)
library(tidyverse)
library(readxl)
library(stargazer)


## Set working directory to folder containing replication materials
setwd("~/Dropbox/working-papers/polarization-sols/polarization-sols-fpa-replication/")

## These two lines delete the figures and tables folders if they already exist
unlink("figures/", recursive = TRUE)
unlink("tables/", recursive = TRUE)

## Make empty folders to store tables and figures
dir.create("tables")
dir.create("figures")

## Load data, filter, rename, and generate dependent variable
## Note that data should be placed in a folder called "data" within working directory
dat <- read.csv("data/polarization-sols-analysis-data.csv",
                stringsAsFactors = FALSE) %>%
  filter(polity2 >= 6) |>
  rename(lm_solschange = solsch_unga_version) |>
  mutate(idealpoint_change = log(idealpoint_change))

## Load in data with affective polarization measures
dat_aff <- read.csv("data/polarization-affective-sols-analysis-data.csv",
                    stringsAsFactors = FALSE) |>
  distinct() %>%
  filter(polity2 >= 6) |>
  rename(lm_solschange = solsch_unga_version) |>
  mutate(idealpoint_change = log(idealpoint_change))


## Make interaction variables
dat <- dat |>
  mutate(lm_solschange_polar_most_recent = lm_solschange * polar_most_recent) 

dat_aff <- dat_aff |>
  mutate(lm_solschange_polar_aff_most_recent = lm_solschange * polar_aff_most_recent)


## Tidy data for summary statistics
summary_data <-  dat %>%
  select(lm_solschange,
         polar_linear_int,
         polity2,
         gini_most_recent,
         gdp_pc_log,
         kof_globalization_index)


summary_aff_data <-  dat_aff %>%
  select(lm_solschange,
         polar_aff_linear_int,
         polity2,
         gini_most_recent,
         gdp_pc_log,
         kof_globalization_index)


## Make table of descriptive statistics
stargazer(summary_data,
          title = "Descriptive statistics (party polarization)",
          covariate.labels = c(
            "SOLS Change",
            "Party polarization (linear interpolation)",
            "Polity score",
            "GINI index",
            "GDP per capita (log)",
            "KOF globalization index"
          ),
          out = c("tables/desc-table-1.tex",
                  "tables/desc-table-1.html"))

stargazer(summary_aff_data,
          title = "Descriptive statistics (affective polarization)",
          covariate.labels = c(
            "SOLS Change",
            "Affective polarization (linear interpolation)",
            "Polity score",
            "GINI index",
            "GDP per capita (log)",
            "KOF globalization index"
          ),
          out = c("tables/desc-table-2.tex",
                  "tables/desc-table-2.html"))

#---------------------------------------------------
# Figures of polarization by country
#---------------------------------------------------

polar_small <- dat |>
  filter(!is.na(polarization)) |>
  group_by(country) |>
  summarise(
    average_pol = mean(polarization, na.rm = T),
    min_pol = min(polarization, na.rm = T),
    max_pol = max(polarization, na.rm = T)
  )


polar_aff_small <- dat_aff |>
  filter(!is.na(partisan_affect_polarization)) |>
  group_by(country) |>
  summarise(
    average_pol = mean(partisan_affect_polarization, na.rm = T),
    min_pol = min(partisan_affect_polarization, na.rm = T),
    max_pol = max(partisan_affect_polarization, na.rm = T)
  ) |>
  mutate(country = gsub("_", " ", country)) |>
  mutate(country = str_to_title(country))

## Get median level of polarization to mark on figure
median_pol <- median(dat$polarization, na.rm = T)
median_aff_pol <- median(dat_aff$partisan_affect_polarization, na.rm = T)


ggplot(polar_small,
       aes(x = reorder(country, average_pol),
           y = average_pol,
           ymin = min_pol,
           ymax = max_pol)) +
  xlab("") +
  ylab("\nParty polarization\n") +
  ggtitle("Party polarization by country (PSPI)") +
  labs(caption = 
         paste0("Note: Points mark mean level of polarization in sample, \n",
                "bands mark range of polarization in sample. Vertical \n",
                "dashed line marks median value in sample.")) +
  geom_point(stat = "identity", fill = "salmon") +
  geom_hline(yintercept = median_pol, lty = "dashed") +
  geom_errorbar(width = 0) +
  theme_bw() +
  coord_flip()


ggsave("figures/party-polarization-by-country.pdf",
       height = 10,
       width = 5,
       scale = 1.35)


ggplot(polar_aff_small,
       aes(x = reorder(country, average_pol),
           y = average_pol,
           ymin = min_pol,
           ymax = max_pol)) +
  xlab("") +
  ylab("\nAffective polarization") +
  ggtitle("Affective polarization by country") +
  labs(caption = 
         paste0("Note: Points mark mean level of polarization in sample, \n",
                "bands mark range of polarization in sample. Vertical \n",
                "dashed line marks median value in sample.")) +
  geom_point(stat = "identity", fill = "salmon") +
  geom_hline(yintercept = median_aff_pol, lty = "dashed") +
  geom_errorbar(width = 0) +
  theme_bw() +
  coord_flip()

ggsave("figures/affective-polarization-by-country.pdf",
       height = 5,
       width = 4,
       scale = 1.35)



#---------------------------------------------------
# Scatter plot of polarization against vote change
#---------------------------------------------------

measure_1 <- dat |>
  dplyr::select(polar_linear_int, idealpoint_change) |>
  mutate(measure = "Party polarization") |>
  rename(polarization = polar_linear_int)

measure_2 <- dat_aff |>
  dplyr::select(polar_aff_linear_int, idealpoint_change) |>
  mutate(measure = "Affective polarization") |>
  rename(polarization = polar_aff_linear_int)


for_scatter <- rbind(measure_1, measure_2) |>
  mutate(measure = factor(measure,
                          levels = c("Party polarization",
                                     "Affective polarization")))

## Make figure
ggplot(data = for_scatter,
       aes(x = polarization,
           y = idealpoint_change)) +
  facet_grid(. ~ measure, scales = "free_x") +
  geom_point() +
  labs(x = "\nDegree of polarization",
       y = "log(Absolute change in ideal point from previous year)\n",
       title = "")

ggsave("figures/pol-unga-scatter.pdf",
       scale = 0.95)





#-------------------------------------
# MAIN MODELS
#-------------------------------------

library(plm)

## Make panel data 
pdat <- pdata.frame(dat, index = c("iso3c", "year"))

## Make post-Cold War variable
dat_aff$post89 <- ifelse(dat_aff$year > 1989, 1, 0)

## Convert to panel data for affective
pdat_aff <- pdata.frame(dat_aff, index = c("iso3c", "year"))



#-------------------------------------
# Main models with linear interpolation
#-------------------------------------

pdat$lm_solschange_polar_linear_int <- pdat$solschange_t1 * pdat$polar_linear_int

Lp1 <- plm(idealpoint_change ~ lm_solschange +
             polar_linear_int +
             lm_solschange_polar_linear_int,
           model = "pooling",
           data = pdat)

## Controls
Lp2 <- plm(idealpoint_change ~ lm_solschange +
             polar_linear_int +
             lm_solschange_polar_linear_int +
             polity2 +
             gini_most_recent +
             gdp_pc_log +
             kof_globalization_index,
           model = "pooling",
           data = pdat)

## Country-fixed effects
Lp3 <- update(Lp2, . ~ .,
              effect = "individual",
              model = "within",
              data = pdat)

## Country- and year-fixed effects
Lp4 <- update(Lp3, . ~ .,
              effect = "twoways",
              model = "within",
              data = pdat)


## Put models in list
L_fits <- list(Lp1, Lp2, Lp3, Lp4)

## Get country clustered standard errors
for(i in 1:length(L_fits)){
  L_fits[[i]]$vcov <- vcovHC(L_fits[[i]], type = 'sss', cluster = 'group')
}


stargazer(L_fits,
          dep.var.labels = c("Change in UNGA Ideal Point"),
          omit = c("iso3c", "year"),
          covariate.labels = c(
            "SOLS change",
            "Party polarization (linear interpolation)",
            "SOLS change $\\times$ Polarization",
            "Democracy",
            "GINI index",
            "GDP per capita (logged)",
            "KOF Globalization index"
          ),
          label = "pol-sols-t1-linear-interpolation",
          title = c(
            paste0("SOLS changes, polarization, and foreign policy change (linear interpolation)")),
          single.row = FALSE,
          no.space = TRUE,
          notes.align = "l",
          notes.label = "",
          df = FALSE,
          add.lines = list(c("Country-fixed effects", "No", "No", "Yes", "Yes"),
                           c("Year-fixed effects", "No", "No", "No", "Yes")),
          notes = c(
            "Two-tailed tests. Country-clustered estimated standard errors in parentheses.", 
            "OLS estimates."),
          out = c("tables/linear-interpolation-reg-table-1.tex",
                  "tables/linear-interpolation-reg-table-1.html"))



## Affective polarization models

## No controls
pdat_aff$lm_solschange_polar_aff_linear_int <- pdat_aff$lm_solschange * pdat_aff$polar_aff_linear_int

Lap1 <- plm(idealpoint_change ~ lm_solschange +
              polar_aff_linear_int +
              lm_solschange_polar_aff_linear_int,
            model = "pooling",
            data = pdat_aff)



## Controls
Lap2 <- plm(idealpoint_change ~ lm_solschange +
              polar_aff_linear_int +
              lm_solschange_polar_aff_linear_int +
              polity2 +
              gini_most_recent +
              gdp_pc_log +
              kof_globalization_index +
              post89,
            model = "pooling",
            data = pdat_aff)

## Country-fixed effects
Lap3 <- update(Lap2, . ~ .,
               effect = "individual",
               model = "within",
               data = pdat_aff)

## Country- and year-fixed effects
Lap4 <- update(Lap3, . ~ .,
               effect = "twoways",
               model = "within",
               data = pdat_aff)


## Put models in list
L_fits_aff <- list(Lap1, Lap2, Lap3, Lap4)

## Get country clustered standard errors
for(i in 1:length(L_fits_aff)){
  L_fits_aff[[i]]$vcov <- vcovHC(L_fits_aff[[i]], type = 'sss', cluster = 'group')
}


stargazer(L_fits_aff,
          dep.var.labels = c("Change in UNGA Ideal Point"),
          omit = c("iso3c", "year"),
          covariate.labels = c(
            "SOLS change",
            "Affective polarization (linear interpolation)",
            "SOLS change $\\times$ Affective polarization",
            "Democracy",
            "GINI index",
            "GDP per capita (logged)",
            "KOF Globalization index",
            "Post-1989"
          ),
          label = "pol-aff-sols-t1-linear-interpolation",
          title = c(
            paste0("SOLS changes, affective polarization, and foreign policy change (linear interpolation)")),
          single.row = FALSE,
          no.space = TRUE,
          notes.align = "l",
          notes.label = "",
          df = FALSE,
          add.lines = list(c("Country-fixed effects", "No", "No", "Yes", "Yes"),
                           c("Year-fixed effects", "No", "No", "No", "Yes")),
          notes = c(
            "Two-tailed tests. Country-clustered estimated standard errors in parentheses.", 
            "OLS estimates."),
          out = c("tables/linear-interpolation-reg-table-2.tex",
                  "tables/linear-interpolation-reg-table-2.html"))



#-------------------------------------
# Interaction effect figures for linear interpolation measure
#-------------------------------------


## A function to calculate significance of interaction term
mcce_ci_calc <- function(var1, var2, var2.value, 
                         model,
                         model_vcov = vcov(model)){
  
  b1_var <- model_vcov[var1, var1]
  
  int_var <- names(coef(model))[
    grepl(var1, names(coef(model))) & grepl(var2, names(coef(model)))
  ]
  
  b3_var <- model_vcov[int_var, int_var]
  b1b3_covar <- model_vcov[var1, int_var] 
  
  se <- sqrt(b1_var + (var2.value^2)*b3_var + 2*var2.value*b1b3_covar)
  MCCE <- summary(model)$coefficients[var1, 1] + 
    var2.value*summary(model)$coefficients[int_var, 1]
  
  return(c(var2.value = var2.value, 
           beta = MCCE, 
           std.error = se, 
           ci.95.lower = MCCE - 1.96 * se, 
           ci.95.upper = MCCE + 1.96 * se,
           ci.90.lower = MCCE - 1.645 * se, 
           ci.90.upper = MCCE + 1.645 * se))
  
}



## Party polarization

lower_bound <- mean(dat$polar_linear_int) - 2*sd(dat$polar_linear_int)
upper_bound <- mean(dat$polar_linear_int) + 2*sd(dat$polar_linear_int)


lower_bound
upper_bound


## Set values for range of interaction effect
interaction_values <- seq(lower_bound, upper_bound, length.out = 10)


pol_holder <- list()

for(i in 1:length(interaction_values)){
  
  pol_holder[[i]] <- mcce_ci_calc("lm_solschange", "polar_linear_int", 
                                  interaction_values[i],
                                  model = Lp4,
                                  model_vcov = vcov(Lp4))
  
  
}


pol_ests <- do.call(rbind, pol_holder) |> as.data.frame()


pol_ests$polar_type <- "Party"





## Affective polarization
lower_bound_aff <- mean(dat_aff$polar_aff_linear_int) - 2*sd(dat_aff$polar_aff_linear_int)
upper_bound_aff <- mean(dat_aff$polar_aff_linear_int) + 2*sd(dat_aff$polar_aff_linear_int)


lower_bound_aff > min(dat_aff$polar_aff_linear_int)
upper_bound_aff < max(dat_aff$polar_aff_linear_int)

## Set values for range of interaction effect
interaction_aff_values <- seq(lower_bound_aff, upper_bound_aff, length.out = 10)


aff_holder <- list()

for(i in 1:length(interaction_aff_values)){
  
  aff_holder[[i]] <- mcce_ci_calc("lm_solschange", "polar_aff_linear_int", 
                                  interaction_aff_values[i],
                                  model = Lap4,
                                  model_vcov = vcov(Lap4))
  
  
}


aff_ests <- do.call(rbind, aff_holder) |> as.data.frame()

aff_ests$polar_type <- "Affective"



## Bind two sets of estimates

all_polar <- rbind(pol_ests, aff_ests) |>
  data.frame() |>
  mutate(polar_type = factor(polar_type,
                             levels = c("Party", "Affective")))




## Make figure
ggplot(data = all_polar,
       aes(x = var2.value,
           y = beta,
           ymin = ci.95.lower,
           ymax = ci.95.upper)) +
  facet_grid(. ~ polar_type, scales = "free_x") +
  geom_line() +
  geom_ribbon(alpha = 0.25) +
  geom_hline(yintercept = 0, lty = "dashed") +
  labs(x = "\nMeasure of polarization (linear interpolation, +/- 2 standard deviations around sample mean)",
       y = paste0("Marginal effect of a SOLS change"),
       title = "")

ggsave("figures/sols-margin-linear-interpolation.pdf",
       scale = .9)







###############################################################################
###############################################################################
###############################################################################


#-------------------------------------
# Alliance abrogation models
#-------------------------------------

## Loading and restricting data set to cases where member is democracy

alliance_data <- read.csv("data/alliance-reliability-polarization-data.csv",
                          stringsAsFactors = FALSE) |>
  filter(dembothNA >= 1) |>
  mutate(solschdum = ifelse(solschangeNA_3 == "SOLS Change", 1, 0))


## Make interaction variables

alliance_data <- alliance_data |>
  mutate(solschdum_polar_linear_int = solschdum * polar_linear_int,
         solschdum_polar_aff_linear_int = solschdum * polar_aff_linear_int) 

## Logit models
lpm_1 <- lm(violate ~ solschdum + 
              polar_linear_int + 
              solschdum_polar_linear_int +
              regtransNA +
              abscincperch1_10 +
              decthreat1_10 +
              time +
              timesquared +
              timecubed,
            data = alliance_data)


lpm_2 <- lm(violate ~ solschdum + 
              polar_aff_linear_int + 
              solschdum_polar_aff_linear_int +
              regtransNA +
              abscincperch1_10 +
              decthreat1_10 +
              time +
              timesquared +
              timecubed,
            data = alliance_data)


alliance_fits <- list(lpm_1, lpm_2)

stargazer(alliance_fits,
          dep.var.labels = c("Alliance violated?"),
          column.labels = c("Party polarization", "Affective polarization"),
          omit = c("iso3c", "year", "time"),
          covariate.labels = c(
            "SOLS change",
            "Polarization (linear interpolation)",
            "SOLS change $\\times$ Polarization",
            "Affective polarization (linear interpolation)",
            "SOLS change $\\times$ Affective polarization",
            "Regime Transition",
            "$\\Delta$ Military Capabilities",
            "Threat Decrease"
          ),
          label = "alliance-models",
          title = c(
            paste0("SOLS changes, polarization, and alliance abrogation")),
          single.row = FALSE,
          no.space = TRUE,
          notes.align = "l",
          notes.label = "",
          #omit.stat = c("rsq", "adj.rsq"),
          df = FALSE,
          # add.lines = list(c("Country-fixed effects", "No", "No", "Yes", "Yes"),
          #                  c("Year-fixed effects", "No", "No", "No", "Yes")),
          notes = c(
            "Two-tailed tests. Estimated standard errors in parentheses.", 
            "OLS estimates. Cubic time polynomial included but omitted from table."),
          out = c("tables/alliance-reg-table-1.tex",
                  "tables/alliance-reg-table-1.html"))






################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


#-------------------------------------
# APPENDIX MODELS
#-------------------------------------

## Four primary models with "most recent" measure of polarization

## No controls
p1 <- plm(idealpoint_change ~ lm_solschange +
            polar_most_recent +
            lm_solschange_polar_most_recent,
          model = "pooling",
          data = pdat)

## Controls
p2 <- plm(idealpoint_change ~ lm_solschange +
            polar_most_recent +
            lm_solschange_polar_most_recent +
            polity2 +
            gini_most_recent +
            gdp_pc_log +
            kof_globalization_index,
          model = "pooling",
          data = pdat)

## Country-fixed effects
p3 <- update(p2, . ~ .,
             effect = "individual",
             model = "within",
             data = pdat)

## Country- and year-fixed effects
p4 <- update(p3, . ~ .,
             effect = "twoways",
             model = "within",
             data = pdat)


## Put models in list
fits <- list(p1, p2, p3, p4)

## Get country clustered standard errors
for(i in 1:length(fits)){
  fits[[i]]$vcov <- vcovHC(fits[[i]], type = 'sss', cluster = 'group')
}


stargazer(fits,
          dep.var.labels = c("Change in UNGA Ideal Point"),
          omit = c("iso3c", "year"),
          covariate.labels = c(
            "SOLS change",
            "Polarization (most recent)",
            "SOLS change $\\times$ Polarization",
            "Democracy",
            "GINI index",
            "GDP per capita (logged)",
            "KOF Globalization index"
          ),
          label = "pol-sols-t1",
          title = c(
            paste0("SOLS changes, polarization, and foreign policy change")),
          single.row = FALSE,
          no.space = TRUE,
          notes.align = "l",
          notes.label = "",
          df = FALSE,
          add.lines = list(c("Country-fixed effects", "No", "No", "Yes", "Yes"),
                           c("Year-fixed effects", "No", "No", "No", "Yes")),
          notes = c(
            "Two-tailed tests. Country-clustered estimated standard errors in parentheses.", 
            "OLS estimates."),
          out = c("tables/most-recent-reg-table-1.tex",
                  "tables/most-recent-reg-table-1.html"))




#----------------------------------------------
# Affective polarization models
#----------------------------------------------


## Four primary models

## No controls
ap1 <- plm(idealpoint_change ~ lm_solschange +
             polar_aff_most_recent +
             lm_solschange_polar_aff_most_recent,
           model = "pooling",
           data = pdat_aff)

## Controls
ap2 <- plm(idealpoint_change ~ lm_solschange +
             polar_aff_most_recent +
             lm_solschange_polar_aff_most_recent +
             polity2 +
             gini_most_recent +
             gdp_pc_log +
             kof_globalization_index +
             post89,
           model = "pooling",
           data = pdat_aff)

## Country-fixed effects
ap3 <- update(ap2, . ~ .,
              effect = "individual",
              model = "within",
              data = pdat_aff)

## Country- and year-fixed effects
ap4 <- update(ap3, . ~ .,
              effect = "twoways",
              model = "within",
              data = pdat_aff)


## Put models in list
fits_aff <- list(ap1, ap2, ap3, ap4)

## Get country clustered standard errors
for(i in 1:length(fits_aff)){
  fits_aff[[i]]$vcov <- vcovHC(fits_aff[[i]], type = 'sss', cluster = 'group')
}





stargazer(fits_aff,
          dep.var.labels = c("Change in UNGA Ideal Point"),
          omit = c("iso3c", "year"),
          covariate.labels = c(
            "SOLS change",
            "Affective polarization (most recent)",
            "SOLS change $\\times$ Affective polarization",
            "Democracy",
            "GINI index",
            "GDP per capita (logged)",
            "KOF Globalization index",
            "Post-1989"
          ),
          label = "pol-aff-sols-t1",
          title = c(
            paste0("SOLS changes, affective polarization, and foreign policy change")),
          single.row = FALSE,
          no.space = TRUE,
          notes.align = "l",
          notes.label = "",
          df = FALSE,
          add.lines = list(c("Country-fixed effects", "No", "No", "Yes", "Yes"),
                           c("Year-fixed effects", "No", "No", "No", "Yes")),
          notes = c(
            "Two-tailed tests. Country-clustered estimated standard errors in parentheses.", 
            "OLS estimates."),
          out = c("tables/most-recent-reg-table-2.tex",
                  "tables/most-recent-reg-table-2.html"))





#-------------------------------------
# Supplementary analysis:  No imputation of missing values
#-------------------------------------


pdat$lm_solschange_polarization <- pdat$solschange_t1 * pdat$polarization

sp1 <- plm(idealpoint_change ~ lm_solschange +
             polarization +
             lm_solschange_polarization,
           model = "pooling",
           data = pdat)

## Controls
sp2 <- plm(idealpoint_change ~ lm_solschange +
             polarization +
             lm_solschange_polarization +
             polity2 +
             gini_most_recent +
             gdp_pc_log +
             kof_globalization_index,
           model = "pooling",
           data = pdat)

## Country-fixed effects
sp3 <- update(sp2, . ~ .,
              effect = "individual",
              model = "within",
              data = pdat)

## Country- and year-fixed effects
sp4 <- update(sp3, . ~ .,
              effect = "twoways",
              model = "within",
              data = pdat)


## Put models in list
s_fits <- list(sp1, sp2, sp3, sp4)

## Get country clustered standard errors
for(i in 1:length(s_fits)){
  s_fits[[i]]$vcov <- vcovHC(s_fits[[i]], type = 'sss', cluster = 'group')
}


stargazer(s_fits,
          dep.var.labels = c("Change in UNGA Ideal Point"),
          omit = c("iso3c", "year"),
          covariate.labels = c(
            "SOLS change",
            "Polarization (most recent)",
            "SOLS change $\\times$ Polarization",
            "Democracy",
            "GINI index",
            "GDP per capita (logged)",
            "KOF Globalization index"
          ),
          label = "pol-sols-t1-appx-observed",
          title = c(
            paste0("SOLS changes, polarization, and foreign policy change (observed values only)")),
          single.row = FALSE,
          no.space = TRUE,
          notes.align = "l",
          notes.label = "",
          df = FALSE,
          add.lines = list(c("Country-fixed effects", "No", "No", "Yes", "Yes"),
                           c("Year-fixed effects", "No", "No", "No", "Yes")),
          notes = c(
            "Two-tailed tests. Country-clustered estimated standard errors in parentheses.", 
            "OLS estimates."),
          out = c("tables/appx-observed-reg-table-1.tex",
                  "tables/appx-observed-reg-table-1.html"))



## Affective polarization models
## No controls

pdat_aff$lm_solschange_affective_polarization <- pdat_aff$lm_solschange * pdat_aff$partisan_affect_polarization

sap1 <- plm(idealpoint_change ~ lm_solschange +
              partisan_affect_polarization +
              lm_solschange_affective_polarization,
            model = "pooling",
            data = pdat_aff)

## Controls
sap2 <- plm(idealpoint_change ~ lm_solschange +
              partisan_affect_polarization +
              lm_solschange_affective_polarization +
              polity2 +
              gini_most_recent +
              gdp_pc_log +
              kof_globalization_index +
              post89,
            model = "pooling",
            data = pdat_aff)

## Country-fixed effects
sap3 <- update(sap2, . ~ .,
               effect = "individual",
               model = "within",
               data = pdat_aff)

## Country- and year-fixed effects
sap4 <- update(sap3, . ~ .,
               effect = "twoways",
               model = "within",
               data = pdat_aff)


## Put models in list
s_fits_aff <- list(sap1, sap2, sap3, sap4)

## Get country clustered standard errors
for(i in 1:length(fits_aff)){
  s_fits_aff[[i]]$vcov <- vcovHC(s_fits_aff[[i]], type = 'sss', cluster = 'group')
}


stargazer(s_fits_aff,
          dep.var.labels = c("Change in UNGA Ideal Point"),
          omit = c("iso3c", "year"),
          covariate.labels = c(
            "SOLS change",
            "Affective polarization (most recent)",
            "SOLS change $\\times$ Affective polarization",
            "Democracy",
            "GINI index",
            "GDP per capita (logged)",
            "KOF Globalization index",
            "Post-1989"
          ),
          label = "pol-aff-sols-t1-appx-observed",
          title = c(
            paste0("SOLS changes, affective polarization, and foreign policy change (observed cases only)")),
          single.row = FALSE,
          no.space = TRUE,
          notes.align = "l",
          notes.label = "",
          df = FALSE,
          add.lines = list(c("Country-fixed effects", "No", "No", "Yes", "Yes"),
                           c("Year-fixed effects", "No", "No", "No", "Yes")),
          notes = c(
            "Two-tailed tests. Country-clustered estimated standard errors in parentheses.", 
            "OLS estimates."),
          out = c("tables/appx-observed-reg-table-2.tex",
                  "tables/appx-observed-reg-table-2.html"))


