# ====
# Appendix Tables C3 and C4 Replication
# Heckman Model Results
# input: extensive_margin_paneldata_Export_WDI.RData
# R version 4.4.1 (2024-06-14)
# ====

################################################################################
## setup -----
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(sampleSelection)
library(sandwich)
library(stargazer)


# set main directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <- "output/"


################################################################################
## 0. load data  ------
################################################################################
load(paste(REPLICATION_DATA_DIR, "extensive_margin_paneldata_Export_WDI.RData", sep = ""))


################################################################################
## 1. fit models  ------
################################################################################
# Table C.3, columns 1-2 (Baseline)
m1.heckman.base <- selection(total.fdi_lag1_bin ~
                               lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                               polity2_P4_lag + lnexport_lag,
                             product.len ~ total.fdi_lag1_bin +
                               lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                               lnexport_lag + product.len_lag1,
                             data = data.plot.panel.used.aug,
                             method = "ml",
                             type = "treatment")
summary(m1.heckman.base)

# extract model dataframe
model.df.m1.heckman.base <- na.omit(data.plot.panel.used.aug[ , unique(c("iso3c",
                                                                         all.vars(formula(m1.heckman.base$termsS)),
                                                                         all.vars(formula(m1.heckman.base$termsO))))])

# compute clustered SEs
estimate.df.m1.heckman.base.cl <- coefTable(coef(m1.heckman.base, part = "full"),
                                            sqrt(diag(vcovCL(m1.heckman.base, model.df.m1.heckman.base$iso3c))),
                                            m1.heckman.base$param$df) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "term") %>%
  mutate(equation = c(rep("Selection", length(m1.heckman.base$param$index$betaS)),
                      rep("Outcome", length(m1.heckman.base$param$index$betaO)),
                      rep("Stats", 2)),
         term = ifelse(term == "X.Intercept.", "Intercept_Selection", term),
         term = ifelse(term == "X.Intercept..1", "Intercept_Outcome", term))

estimate.df.m1.heckman.base.cl


# Table C.3, columns 3-4 (Augmented)
m1.heckman.aug <- selection(total.fdi_lag1_bin ~
                              lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                              lnexport_lag + lnimport_lag +
                              gdp_growth_lag1 +
                              tot_natural_resource_rents_lag1 +
                              polity2_P4_lag + political_stability_lag1 + rule_of_law_lag1,
                            product.len ~ total.fdi_lag1_bin +
                              lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                              lnexport_lag + lnimport_lag +
                              product.len_lag1 +
                              gdp_growth_lag1 +
                              tot_natural_resource_rents_lag1,
                            data = data.plot.panel.used.aug,
                            method = "ml",
                            type = "treatment")
summary(m1.heckman.aug)

# extract model dataframe
model.df.m1.heckman.aug <- na.omit(data.plot.panel.used.aug[ , unique(c("iso3c",
                                                                        all.vars(formula(m1.heckman.aug$termsS)),
                                                                        all.vars(formula(m1.heckman.aug$termsO))))])

# compute clustered SEs
estimate.df.m1.heckman.aug.cl <- coefTable(coef(m1.heckman.aug, part = "full"),
                                           sqrt(diag(vcovCL(m1.heckman.aug, model.df.m1.heckman.aug$iso3c))),
                                           m1.heckman.aug$param$df) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "term") %>%
  mutate(equation = c(rep("Selection", length(m1.heckman.aug$param$index$betaS)),
                      rep("Outcome", length(m1.heckman.aug$param$index$betaO)),
                      rep("Stats", 2)),
         term = ifelse(term == "X.Intercept.", "Intercept_Selection", term),
         term = ifelse(term == "X.Intercept..1", "Intercept_Outcome", term))

estimate.df.m1.heckman.aug.cl


# Table C.3, columns 5-6 (Augmented + Year FE)
m2.heckman.aug <- selection(total.fdi_lag1_bin ~
                              lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                              lnexport_lag + lnimport_lag +
                              gdp_growth_lag1 +
                              tot_natural_resource_rents_lag1 +
                              polity2_P4_lag + political_stability_lag1 + rule_of_law_lag1,
                            product.len ~ total.fdi_lag1_bin +
                              lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                              lnexport_lag + lnimport_lag +
                              product.len_lag1 +
                              gdp_growth_lag1 +
                              tot_natural_resource_rents_lag1 +
                              factor(year),
                            data = data.plot.panel.used.aug,
                            method = "ml",
                            type = "treatment")
summary(m2.heckman.aug)

# extract model dataframe
model.df.m2.heckman.aug <- na.omit(data.plot.panel.used.aug[ , unique(c("iso3c",
                                                                        all.vars(formula(m2.heckman.aug$termsS)),
                                                                        all.vars(formula(m2.heckman.aug$termsO))))])
# compute clustered SEs
estimate.df.m2.heckman.aug.cl <- coefTable(coef(m2.heckman.aug, part = "full"),
                                           sqrt(diag(vcovCL(m2.heckman.aug, model.df.m2.heckman.aug$iso3c))),
                                           m2.heckman.aug$param$df) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "term") %>%
  mutate(equation = c(rep("Selection", length(m2.heckman.aug$param$index$betaS)),
                      rep("Outcome", length(m2.heckman.aug$param$index$betaO)),
                      rep("Stats", 2)),
         term = ifelse(term == "X.Intercept.", "Intercept_Selection", term),
         term = ifelse(term == "X.Intercept..1", "Intercept_Outcome", term))

estimate.df.m2.heckman.aug.cl


# Table C.4, columns 1-2 (Baseline)
m5.heckman.base <- selection(total.fdi_lag1_bin ~
                               lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                               polity2_P4_lag + lnexport_lag,
                             product.len_delta ~ total.fdi_lag1_bin +
                               lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                               lnexport_lag + product.len_lag1,
                             data = data.plot.panel.used.aug,
                             method = "ml",
                             type = "treatment")
summary(m5.heckman.base)

# extract model dataframe
model.df.m5.heckman.base <- na.omit(data.plot.panel.used.aug[ , unique(c("iso3c",
                                                                         all.vars(formula(m5.heckman.base$termsS)),
                                                                         all.vars(formula(m5.heckman.base$termsO))))])
# compute clustered SEs
estimate.df.m5.heckman.base.cl <- coefTable(coef(m5.heckman.base, part = "full"),
                                            sqrt(diag(vcovCL(m5.heckman.base, model.df.m5.heckman.base$iso3c))),
                                            m5.heckman.base$param$df) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "term") %>%
  mutate(equation = c(rep("Selection", length(m5.heckman.base$param$index$betaS)),
                      rep("Outcome", length(m5.heckman.base$param$index$betaO)),
                      rep("Stats", 2)),
         term = ifelse(term == "X.Intercept.", "Intercept_Selection", term),
         term = ifelse(term == "X.Intercept..1", "Intercept_Outcome", term))

estimate.df.m5.heckman.base.cl

# Table C.4, columns 3-4 (Augmented)
m5.heckman.aug <- selection(total.fdi_lag1_bin ~
                              lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                              lnexport_lag + lnimport_lag +
                              gdp_growth_lag1 +
                              tot_natural_resource_rents_lag1 +
                              polity2_P4_lag + political_stability_lag1 + rule_of_law_lag1,
                            product.len_delta ~ total.fdi_lag1_bin +
                              lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                              lnexport_lag + lnimport_lag +
                              product.len_lag1 +
                              gdp_growth_lag1 +
                              tot_natural_resource_rents_lag1,
                            data = data.plot.panel.used.aug,
                            method = "ml",
                            type = "treatment")
summary(m5.heckman.aug)

# extract model dataframe
model.df.m5.heckman.aug <- na.omit(data.plot.panel.used.aug[ , unique(c("iso3c",
                                                                        all.vars(formula(m5.heckman.aug$termsS)),
                                                                        all.vars(formula(m5.heckman.aug$termsO))))])

# compute clustered SEs
estimate.df.m5.heckman.aug.cl <- coefTable(coef(m5.heckman.aug, part = "full"),
                                           sqrt(diag(vcovCL(m5.heckman.aug, model.df.m5.heckman.aug$iso3c))),
                                           m5.heckman.aug$param$df) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "term") %>%
  mutate(equation = c(rep("Selection", length(m5.heckman.aug$param$index$betaS)),
                      rep("Outcome", length(m5.heckman.aug$param$index$betaO)),
                      rep("Stats", 2)),
         term = ifelse(term == "X.Intercept.", "Intercept_Selection", term),
         term = ifelse(term == "X.Intercept..1", "Intercept_Outcome", term))

estimate.df.m5.heckman.aug.cl


# Table C.4, columns 5-6 (Augmented + Year FE)
m6.heckman.aug <- selection(total.fdi_lag1_bin ~
                              lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                              lnexport_lag + lnimport_lag +
                              gdp_growth_lag1 +
                              tot_natural_resource_rents_lag1 +
                              polity2_P4_lag + political_stability_lag1 + rule_of_law_lag1,
                            product.len_delta ~ total.fdi_lag1_bin +
                              lngdp_WDI_PW_lag + lnpop_WDI_PW_lag +
                              lnexport_lag + lnimport_lag +
                              product.len_lag1 +
                              gdp_growth_lag1 +
                              tot_natural_resource_rents_lag1 +
                              factor(year),
                            data = data.plot.panel.used.aug,
                            method = "ml",
                            type = "treatment")
summary(m6.heckman.aug)

# extract model dataframe
model.df.m6.heckman.aug <- na.omit(data.plot.panel.used.aug[ , unique(c("iso3c",
                                                                        all.vars(formula(m6.heckman.aug$termsS)),
                                                                        all.vars(formula(m6.heckman.aug$termsO))))])

# compute clustered SEs
estimate.df.m6.heckman.aug.cl <- coefTable(coef(m6.heckman.aug, part = "full"),
                                           sqrt(diag(vcovCL(m6.heckman.aug, model.df.m6.heckman.aug$iso3c))),
                                           m6.heckman.aug$param$df) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "term") %>%
  mutate(equation = c(rep("Selection", length(m6.heckman.aug$param$index$betaS)),
                      rep("Outcome", length(m6.heckman.aug$param$index$betaO)),
                      rep("Stats", 2)),
         term = ifelse(term == "X.Intercept.", "Intercept_Selection", term),
         term = ifelse(term == "X.Intercept..1", "Intercept_Outcome", term))

estimate.df.m6.heckman.aug.cl


################################################################################
## 2. generate tables  ------
################################################################################
# function to replace variable names
replaceVarName <- function(var.vec, var.df){
  # Prepare output vector
  out.vec <- rep(NA, length(var.vec))
  matches <- match(var.vec, var.df$var)
  out.vec <- var.df[matches,]$var_name

  if(any(is.na(out.vec))){
    warning(paste("Variable concordence missing: ",
                  paste(var.vec[is.na(out.vec)], collapse = ", "),
                  sep = ""))
  } else{

  }

  return(out.vec)
}

# var dictionary
var.df <- tibble(var = c("total.fdi_lag1_bin",
                         "lngdp_WDI_PW_lag",
                         "lnpop_WDI_PW_lag",
                         "polity2_P4_lag",
                         "political_stability_lag1",
                         "rule_of_law_lag1",
                         "lnexport_lag",
                         "product.len_lag1",
                         "lnimport_lag",
                         "gdp_growth_lag1",
                         "tot_natural_resource_rents_lag1"),
                 var_name = c("FDI (t-1, binary)",
                              "GDP (t-1, logged)",
                              "Population (t-1, logged)",
                              "Polity 2 (t-1)",
                              "Political stability (t-1)",
                              "Rule of law (t-1)",
                              "Export value (t-1, logged)",
                              "Extensive margin (t-1)",
                              "Import value (t-1, logged)",
                              "GDP growth (t-1, annual \\%)",
                              "Total natural resources rents (t-1, \\% of GDP)"
                              ))

# solution to get stargazer to show both selection and outcome equation results
m1.heckman.base.1 <- m1.heckman.base
m1.heckman.base.2 <- m1.heckman.base
m1.heckman.base.2$param$index$betaO <- m1.heckman.base.1$param$index$betaS
m1.heckman.base.2$param$index$betaS <- m1.heckman.base.1$param$index$betaO

m1.heckman.aug.1 <- m1.heckman.aug
m1.heckman.aug.2 <- m1.heckman.aug
m1.heckman.aug.2$param$index$betaO <- m1.heckman.aug.1$param$index$betaS
m1.heckman.aug.2$param$index$betaS <- m1.heckman.aug.1$param$index$betaO

m2.heckman.aug.1 <- m2.heckman.aug
m2.heckman.aug.2 <- m2.heckman.aug
m2.heckman.aug.2$param$index$betaO <- m2.heckman.aug.1$param$index$betaS
m2.heckman.aug.2$param$index$betaS <- m2.heckman.aug.1$param$index$betaO

m5.heckman.base.1 <- m5.heckman.base
m5.heckman.base.2 <- m5.heckman.base
m5.heckman.base.2$param$index$betaO <- m5.heckman.base.1$param$index$betaS
m5.heckman.base.2$param$index$betaS <- m5.heckman.base.1$param$index$betaO

m5.heckman.aug.1 <- m5.heckman.aug
m5.heckman.aug.2 <- m5.heckman.aug
m5.heckman.aug.2$param$index$betaO <- m5.heckman.aug.1$param$index$betaS
m5.heckman.aug.2$param$index$betaS <- m5.heckman.aug.1$param$index$betaO

m6.heckman.aug.1 <- m6.heckman.aug
m6.heckman.aug.2 <- m6.heckman.aug
m6.heckman.aug.2$param$index$betaO <- m6.heckman.aug.1$param$index$betaS
m6.heckman.aug.2$param$index$betaS <- m6.heckman.aug.1$param$index$betaO


## create Table C.3: Heckman Model Results: Export Extensive Margin (t) as the Outcome
# compile models
models.level.list <- list(m1.heckman.base.1, # selection results
                          m1.heckman.base.2, # outcome results
                          m1.heckman.aug.1, # selection results
                          m1.heckman.aug.2, # outcome results
                          m2.heckman.aug.1, # selection results
                          m2.heckman.aug.2 # outcome results
)

# extract clustered SEs
cse.level.list <- list(estimate.df.m1.heckman.base.cl %>%
                         filter(equation == "Selection") %>%
                         pull(`Std. Error`),
                       estimate.df.m1.heckman.base.cl %>%
                         filter(equation == "Outcome") %>%
                         pull(`Std. Error`),
                       estimate.df.m1.heckman.aug.cl %>%
                         filter(equation == "Selection") %>%
                         pull(`Std. Error`),
                       estimate.df.m1.heckman.aug.cl %>%
                         filter(equation == "Outcome") %>%
                         pull(`Std. Error`),
                       estimate.df.m2.heckman.aug.cl %>%
                         filter(equation == "Selection") %>%
                         pull(`Std. Error`),
                       estimate.df.m2.heckman.aug.cl %>%
                         filter(equation == "Outcome") %>%
                         pull(`Std. Error`))

# set var order
var.order <- c("^total.fdi_lag1_bin$",
               "^lngdp_WDI_PW_lag$",
               "^lnpop_WDI_PW_lag$",
               "^lnexport_lag$",
               "^polity2_P4_lag$",
               "^product.len_lag1$",
               "^political_stability_lag1$",
               "^rule_of_law_lag1$",
               "^lnimport_lag$",
               "^gdp_growth_lag1$",
               "^tot_natural_resource_rents_lag1$")

# set var label
var.label <- str_replace_all(var.order, "\\^", "")
var.label <- str_replace_all(var.label, "\\$", "")

# save LaTeX table
sink(file.path(REPLICATION_FIG_DIR, "Tab_C3.tex"))
stargazer(models.level.list,
          se = cse.level.list,
          selection.equation = TRUE,
          type = "latex",
          label = "tb:heckman-level",
          dep.var.labels.include = FALSE,
          column.labels = c("Baseline", "Augmented", " + Year FE"),
          column.separate = c(2, 2, 2),
          font.size = "small",
          single.row = FALSE,
          title = "Heckman Treatment Model Results: Extensive Margin (t) as Outcome",
          omit = "year",
          model.numbers = TRUE,
          order = var.order,
          covariate.labels = replaceVarName(var.vec = var.label,
                                            var.df = var.df),
          add.lines = list(c("Fixed Effects: Year",
                             rep(c("", "",
                                   "", "",
                                   "Yes", "Yes"
                                   ))),
                           c("Years", rep(14, 6)),
                           c("rho",
                             rep(paste(estimate.df.m1.heckman.base.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m1.heckman.base.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2),
                             rep(paste(estimate.df.m1.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m1.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2),
                           rep(paste(estimate.df.m2.heckman.aug.cl %>%
                                       filter(term == "rho") %>%
                                       pull(Estimate) %>%
                                       round(3),
                                     " (",
                                     estimate.df.m2.heckman.aug.cl %>%
                                       filter(term == "rho") %>%
                                       pull(`Std. Error`) %>%
                                       round(3),
                                     ")", sep = ""), 2))),
          keep.stat = c("n", "ll"),
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"),
          notes = c("standard errors clustered by country in parentheses. $^{+}$p$<$0.1, $^{*}$p$<$0.05, $^{**}$p$<$0.01."),
          notes.align = "l",
          notes.append = FALSE)
sink()

# save HTML table
sink(file.path(REPLICATION_FIG_DIR, "Tab_C3.html"))
stargazer(models.level.list,
          se = cse.level.list,
          selection.equation = TRUE,
          type = "html",
          label = "tb:heckman-level",
          dep.var.labels.include = FALSE,
          column.labels = c("Baseline", "Augmented", " + Year FE"),
          column.separate = c(2, 2, 2),
          font.size = "small",
          single.row = FALSE,
          title = "Heckman Treatment Model Results: Extensive Margin (t) as Outcome",
          omit = "year",
          model.numbers = TRUE,
          order = var.order,
          covariate.labels = replaceVarName(var.vec = var.label,
                                            var.df = var.df),
          add.lines = list(c("Fixed Effects: Year",
                             rep(c("", "",
                                   "", "",
                                   "Yes", "Yes"
                             ))),
                           c("Years", rep(14, 6)),
                           c("rho",
                             rep(paste(estimate.df.m1.heckman.base.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m1.heckman.base.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2),
                             rep(paste(estimate.df.m1.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m1.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2),
                             rep(paste(estimate.df.m2.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m2.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2))),
          keep.stat = c("n", "ll"),
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"),
          notes = c("standard errors clustered by country in parentheses. $^{+}$p$<$0.1, $^{*}$p$<$0.05, $^{**}$p$<$0.01."),
          notes.align = "l",
          notes.append = FALSE)
sink()


## create Table C.4: Heckman Model Results: Change in Export Extensive Margin (%) as Outcome
# compile models
models.change.list <- list(m5.heckman.base.1, # selection results
                           m5.heckman.base.2, # outcome results
                           m5.heckman.aug.1, # selection results
                           m5.heckman.aug.2, # outcome results
                           m6.heckman.aug.1, # selection results
                           m6.heckman.aug.2 # outcome results
)

# extract clustered SEs
cse.change.list <- list(estimate.df.m5.heckman.base.cl %>%
                         filter(equation == "Selection") %>%
                         pull(`Std. Error`),
                       estimate.df.m5.heckman.base.cl %>%
                         filter(equation == "Outcome") %>%
                         pull(`Std. Error`),
                       estimate.df.m5.heckman.aug.cl %>%
                         filter(equation == "Selection") %>%
                         pull(`Std. Error`),
                       estimate.df.m5.heckman.aug.cl %>%
                         filter(equation == "Outcome") %>%
                         pull(`Std. Error`),
                       estimate.df.m6.heckman.aug.cl %>%
                         filter(equation == "Selection") %>%
                         pull(`Std. Error`),
                       estimate.df.m6.heckman.aug.cl %>%
                         filter(equation == "Outcome") %>%
                         pull(`Std. Error`))

# set var order
var.order <- c("^total.fdi_lag1_bin$",
               "^lngdp_WDI_PW_lag$",
               "^lnpop_WDI_PW_lag$",
               "^lnexport_lag$",
               "^polity2_P4_lag$",
               "^product.len_lag1$",
               "^political_stability_lag1$",
               "^rule_of_law_lag1$",
               "^lnimport_lag$",
               "^gdp_growth_lag1$",
               "^tot_natural_resource_rents_lag1$")

# set var label
var.label <- str_replace_all(var.order, "\\^", "")
var.label <- str_replace_all(var.label, "\\$", "")

# save LaTeX table
sink(file.path(REPLICATION_FIG_DIR, "Tab_C4.tex"))
stargazer(models.change.list,
          se = cse.change.list,
          selection.equation = TRUE,
          type = "latex",
          label = "tb:heckman-change",
          dep.var.labels.include = FALSE,
          column.labels = c("Baseline", "Augmented", " + Year FE"),
          column.separate = c(2, 2, 2),
          font.size = "small",
          single.row = FALSE,
          title = "Heckman Treatment Model Results: Change in Extensive Margin (%) as Outcome",
          omit = "year",
          model.numbers = TRUE,
          order = var.order,
          covariate.labels = replaceVarName(var.vec = var.label,
                                            var.df = var.df),
          add.lines = list(c("Fixed Effects: Year",
                             rep(c("", "",
                                   "", "",
                                   "Yes", "Yes"
                             ))),
                           c("Years", rep(14, 6)),
                           c("rho",
                             rep(paste(estimate.df.m5.heckman.base.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m5.heckman.base.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2),
                             rep(paste(estimate.df.m5.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m5.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2),
                             rep(paste(estimate.df.m6.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m6.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2))),
          keep.stat = c("n", "ll"),
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"),
          notes = c("standard errors clustered by country in parentheses. $^{+}$p$<$0.1, $^{*}$p$<$0.05, $^{**}$p$<$0.01."),
          notes.align = "l",
          notes.append = FALSE)
sink()

# save HTML table
sink(file.path(REPLICATION_FIG_DIR, "Tab_C4.html"))
stargazer(models.change.list,
          se = cse.change.list,
          selection.equation = TRUE,
          type = "html",
          label = "tb:heckman-change",
          dep.var.labels.include = FALSE,
          column.labels = c("Baseline", "Augmented", " + Year FE"),
          column.separate = c(2, 2, 2),
          font.size = "small",
          single.row = FALSE,
          title = "Heckman Treatment Model Results: Change in Extensive Margin (%) as Outcome",
          omit = "year",
          model.numbers = TRUE,
          order = var.order,
          covariate.labels = replaceVarName(var.vec = var.label,
                                            var.df = var.df),
          add.lines = list(c("Fixed Effects: Year",
                             rep(c("", "",
                                   "", "",
                                   "Yes", "Yes"
                             ))),
                           c("Years", rep(14, 6)),
                           c("rho",
                             rep(paste(estimate.df.m5.heckman.base.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m5.heckman.base.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2),
                             rep(paste(estimate.df.m5.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m5.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2),
                             rep(paste(estimate.df.m6.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(Estimate) %>%
                                         round(3),
                                       " (",
                                       estimate.df.m6.heckman.aug.cl %>%
                                         filter(term == "rho") %>%
                                         pull(`Std. Error`) %>%
                                         round(3),
                                       ")", sep = ""), 2))),
          keep.stat = c("n", "ll"),
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"),
          notes = c("standard errors clustered by country in parentheses. $^{+}$p$<$0.1, $^{*}$p$<$0.05, $^{**}$p$<$0.01."),
          notes.align = "l",
          notes.append = FALSE)
sink()

