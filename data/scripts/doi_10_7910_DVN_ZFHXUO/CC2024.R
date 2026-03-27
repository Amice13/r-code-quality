## The code is drawn from Callaway's Github Page: https://github.com/bcallaway11. All R packages
## invovled can also be downloaded there.
 
library(did)
library(BMisc)
library(twfeweights)
library(fixest)
library(modelsummary)
library(ggplot2)
library(data.table)

# load date
#load("data2.RData")
#data2$region <- droplevels(data2$region)
df <- haven::read_dta("data.dta") #load data from Wei et al. (2024)

##
## ------------------------------------------------------------------------------
#  adjustment regression, CC (2024)
## ------------------------------------------------------------------------------

## covariate balance weights
ra_wts <- implicit_aipw_weights(
  yname = "CEI",
  tname = "year",
  idname = "id",
  gname = "G",
  xformula = ~ PGDP + PGDP_2 + PDEN + INDS + CONS + GOV + Lnrain + Lnsun,
  d_covs_formula = ~1,
  pscore_formula = ~1,
  data = df
)

## Figure 3
ra_cov_bal <- aipw_cov_bal(ra_wts, ~ PGDP + PGDP_2 + PDEN + INDS + CONS + GOV + Lnrain + Lnsun + -1)
ggtwfeweights(ra_cov_bal,
              absolute_value = FALSE,
              standardize = TRUE,
              plot_relative_to_target = FALSE
) +
  xlim(c(-2, 2)) 

#Callaway and Sant'Anna, regression adjustment
cs_x <- att_gt(
  yname = "CEI",
  tname = "year",
  idname = "id",
  gname = "G",
  xformla = ~ PGDP + PGDP_2 + PDEN + INDS + CONS + GOV + Lnrain + Lnsun,
  control_group = "nevertreated",
  base_period = "varying",
  est_method = "reg",
  clustervars = "id",
  #allow_unbalanced_panel = "TRUE",
  data = df
)
cs_x_res <- aggte(cs_x, type = "simple")
summary(cs_x_res)

## Figure 4
# event study
cs_x_dyn <- aggte(cs_x, type = "dynamic") 
ggdid(cs_x_dyn)+ ylim(c(-0.2, 0.2))

##################################################################################
## ------------------------------------------------------------------------------
#  further accounting for selection criteria, CC (2024)
## ------------------------------------------------------------------------------

## covariate balance weights
ra_wts <- implicit_aipw_weights(
  yname = "CEI",
  tname = "year",
  idname = "id",
  gname = "G",
  xformula = ~ PGDP + PGDP_2 + PDEN + INDS + CONS + GOV + Lnrain + Lnsun + plan87 + EMC + SC + FCI + RUS + ORC,
  d_covs_formula = ~1,
  pscore_formula = ~1,
  data = df
)

## Figure 3B
ra_cov_bal <- aipw_cov_bal(ra_wts, ~ PGDP + PGDP_2 + PDEN + INDS + CONS + GOV + Lnrain + Lnsun + plan87 + EMC + SC + FCI + RUS + ORC + -1)
ggtwfeweights(ra_cov_bal,
              absolute_value = FALSE,
              standardize = TRUE,
              plot_relative_to_target = FALSE
) +
  xlim(c(-2, 2))

#Callaway and Sant'Anna, regression adjustment
cs_x <- att_gt(
  yname = "CEI",
  tname = "year",
  idname = "id",
  gname = "G",
  xformla = ~ PGDP + PGDP_2 + PDEN + INDS + CONS + GOV + Lnrain + Lnsun + plan87 + EMC + SC + FCI + RUS + ORC,
  control_group = "nevertreated",
  base_period = "varying",
  est_method = "reg",
  clustervars = "id",
  #allow_unbalanced_panel = "TRUE",
  data = df
)
cs_x_res <- aggte(cs_x, type = "simple")
summary(cs_x_res)

## Figure 4B
# event study
cs_x_dyn <- aggte(cs_x, type = "dynamic") 
ggdid(cs_x_dyn)+ ylim(c(-0.2, 0.2))







