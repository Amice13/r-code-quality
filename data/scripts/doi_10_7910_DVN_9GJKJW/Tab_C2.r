# ====
# Table C2 Replication:
# Extensive margin results with two year lags
# input: extensive_margin_paneldata_Export.csv
# R version 4.4.2 (2024-10-31)
# ====


# clean slate
rm(list = ls())
date()

library(tidyverse)
library(countrycode)
library(plm)
library(fixest)
library(modelsummary)
library(broom)
library(kableExtra)


# set main directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"


################################################################################
## 0. load data  ------
################################################################################
# read data
data.plot.panel.used <- read_csv(file = paste0(REPLICATION_DATA_DIR,
                            "extensive_margin_paneldata_Export.csv"))


# subset to non-missing years used in the analyses (due to two year lags)
data.plot.panel.used <- data.plot.panel.used %>%
  filter(year %in% c(2005:2017))

################################################################################
## 1. Run models ------
################################################################################

# m1. Base model ------
summary(m1.r <- feols(product.len ~ total.fdi_lag2_bin
                                + lngdp_WDI_PW_lag2
                                + lnpop_WDI_PW_lag2
                                + polity2_P4_lag2
                                + lnexport_lag2
                                + product.len_lag2,
                                data = data.plot.panel.used),
        cluster = "iso3c")

# m2. Year FE ------
summary(m2.r <- feols(product.len ~ total.fdi_lag2_bin
                              + lngdp_WDI_PW_lag2
                              + lnpop_WDI_PW_lag2
                              + polity2_P4_lag2
                              + lnexport_lag2
                              + product.len_lag2
                              | year,
                              data = data.plot.panel.used),
        cluster = "iso3c")

# m3. Country FE, without lagged DV---------
summary(m3.r <- feols(product.len ~ total.fdi_lag2_bin
                             + lngdp_WDI_PW_lag2
                             + lnpop_WDI_PW_lag2
                             + polity2_P4_lag2
                             + lnexport_lag2
                             | iso3c,
                             data = data.plot.panel.used),
        cluster = "iso3c") 


# m4. Both Year and Country FE, without lagged DV---------
summary(m4.r <- feols(product.len ~ total.fdi_lag2_bin
                             + lngdp_WDI_PW_lag2
                             + lnpop_WDI_PW_lag2
                             + polity2_P4_lag2
                             + lnexport_lag2
                             | iso3c + year,
                             data = data.plot.panel.used),
        cluster = "iso3c")


# m5. Change as DV, without FE---------
summary(m5.r <- feols(product.len_deltal2 ~ total.fdi_lag2_bin
                             +  lngdp_WDI_PW_lag2
                             + lnpop_WDI_PW_lag2
                             + polity2_P4_lag2
                             + lnexport_lag2 +
                             + product.len_lag2,
                             data = data.plot.panel.used),
        cluster = "iso3c")　

# m6. Change as DV, with Year FE---------
summary(m6.r <- feols(product.len_deltal2 ~ total.fdi_lag2_bin
                             + lngdp_WDI_PW_lag2
                             + lnpop_WDI_PW_lag2
                             + polity2_P4_lag2
                             + lnexport_lag2 
                             + product.len_lag2
                             | year,
                             data = data.plot.panel.used),
        cluster = "iso3c") 


################################################################################
## 2. Table ------
################################################################################

coef.labels  <- c("total.fdi_lag1_bin" = "FDI (t-1, binary)",
                "total.fdi_lag2_bin" = "FDI (t-2, binary)",
                "product.len_lag1" = "Extensive margin (t-1)",
                "product.len_lag2" = "Extensive margin (t-2)",
                "lngdp_WDI_PW_lag2" = "GDP (t-2, logged)",
                "lnpop_WDI_PW_lag2" = "Population (t-2, logged)",
                "polity2_P4_lag2" = "Polity 2 (t-2)",
                "lnexport_lag2" = "Export value (t-2, logged)",
                "lnimport_lag" = "Import value (t-2, logged)",
                "(Intercept)" = "Constant")


# report standard errors clustered by country
fixest_robust_se <- function(x){
  return(summary(x, cluster = "iso3c")$se)
}

# report pvalues from standard errors clustered by country
fixest_robust_se_pval <- function(x){
  return(summary(x, cluster = "iso3c")$coeftable[,4])
}

# table customization
tidy_custom.fixest <- function(x, ...) {
  s <- summary(x)
  out <- data.frame(
    term = names(s$coefficients),
    std.error = fixest_robust_se(x),
    p.value = fixest_robust_se_pval(x)
  )
  return(out)
}


model_list.r <- list(m1.r, m2.r, m3.r, m4.r, m5.r, m6.r)

length(model_list.r)


options(modelsummary_factory_latex = "kableExtra")
options(modelsummary_factory_latex_tabular = "kableExtra")
options(modelsummary_format_numeric_latex = "plain")

# latex output ----------------
ncountry <- length(unique(data.plot.panel.used$iso3c))
nyear <- length(unique(data.plot.panel.used$year))

glance_custom.fixest <- function(x, ...) {
  s <- glance(x)
  out <- data.frame(
    "N.new" = paste("mc", round(s["nobs"], digits = 0), "}"),
    "Countries" =  paste("mc", ncountry, "}"),
    "Years" =  paste("mc", nyear, "}"),
    `R2.new` = paste("mc", formatC(round(s[["r.squared"]],3), digits = 3, format = "f"), "}"),
    `R2.Adj.new` = paste("mc", formatC(round(s[["adj.r.squared"]],3), digits = 3, format = "f"), "}"),
    "BIC.new" = paste("mc", formatC(round(s[["BIC"]],3), digits = 3, format = "f"), "}"),
    "Log.Lik.new" = paste("mc", formatC(round(s[["logLik"]],3), digits = 3, format = "f"), "}")
  )
  return(out)
}

tab.appendix.r <- modelsummary(model_list.r,
                               stars = c('+' = 0.1, '*' = 0.05, '**' = 0.01),
                               coef_map = coef.labels,
                               output = "latex_tabular",
                               gof_map = c("N.new", "Countries", "Years", "FE: year", "FE: iso3c", "R2.new", "R2.Adj.new", "BIC.new", "Log.Lik.new")) %>%
                               add_header_above(c("DV: " = 1, "Extensive margin (t)" = 4,
                               " $\\\\Delta$ Extensive margin ($t-2$ to $t$, \\\\%) " = 2), escape = FALSE)

# Latex output format
tab.appendix.r <- tab.appendix.r %>%
  gsub("lcccccc", "lqqqqqq", .) %>%
  gsub(" X ", "\\\\mc{\\\\checkmark}", .) %>%
  gsub(" X\\\\", "\\\\mc{\\\\checkmark}\\\\", .) %>%
  gsub("TRUE", "\\\\mc{\\\\checkmark}", .) %>%
  gsub("FALSE", "", .) %>%
  gsub("N.new", "N", .) %>%
  gsub("R2.new", "R2", .) %>%
  gsub("R2.Adj.new", "Adj. R2", .) %>%
  gsub("BIC.new", "BIC", .) %>%
  gsub("Log.Lik.new", "Log likelihood", .) %>%
  gsub("\\+", "$+$", .) %>%
  gsub("mc ", "\\\\mc{", .) %>%
  gsub(" \\\\}", "}", .) %>%
  gsub("R2", "$R^2$", .) %>%
  gsub("\\\\bottomrule", "\\\\bottomrule \\\\multicolumn{7}{r}{Note: standard errors clustered by country in parentheses. + p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01}", .)

tab.appendix.r <- capture.output(tab.appendix.r)
tab.appendix.r

# save
writeLines(tab.appendix.r, paste0(REPLICATION_FIG_DIR,
                         "TC2_Extensive_results_Exports_table.tex"))

# html output ----------------
glance_custom.fixest <- function(x, ...) {
  s <- glance(x)
  out <- data.frame(
    "N.new" =  s[["nobs"]],
    "Countries" = ncountry,
    "Years" = nyear,
    `R2.new` =  formatC(round(s[["r.squared"]],3), digits = 3, format = "f"),
    `R2.Adj.new` =  formatC(round(s[["adj.r.squared"]],3), digits = 3, format = "f"),
    "BIC.new" =  formatC(round(s[["BIC"]],3), digits = 3, format = "f"),
    "Log.Lik.new" =  formatC(round(s[["logLik"]],3), digits = 3, format = "f")
  )
  return(out)
}


tab.appendix.r.html <- modelsummary(model_list.r,
                             stars = c('+' = 0.1, '*' = 0.05, '**' = 0.01),
                             coef_map = coef.labels,
                             output = "html",
                             gof_map = c("N.new", "Countries", "Years", "FE: year", "FE: iso3c", "R2.new", "R2.Adj.new", "BIC.new", "Log.Lik.new")
                             ) %>% 
                             add_header_above(c("DV: " = 1, "Extensive margin (t)" = 4,
                             "Δ Extensive margin (t-2 to t, %) " = 2), escape = FALSE) %>% 
  kable_styling() %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = "standard errors clustered by country in parentheses.", threeparttable = TRUE) %>%
  htmltools::HTML() %>%
  gsub("X", "✔", .)

tab.appendix.r.html <- tab.appendix.r.html %>%
  gsub("N.new", "N", .) %>%
  gsub("R2.new", "R2", .) %>%
  gsub("R2.Adj.new", "Adj. R2", .) %>%
  gsub("BIC.new", "BIC", .) %>%
  gsub("Log.Lik.new", "Log likelihood", .) 

tab.appendix.r.html <- capture.output(tab.appendix.r.html)
tab.appendix.r.html

writeLines(tab.appendix.r.html, paste0(REPLICATION_FIG_DIR,
                         "TC2_Extensive_results_Exports_table.html"))
