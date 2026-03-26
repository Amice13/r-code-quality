# load data (saved in stata after running all data preparation commands there)
data <- haven::read_dta("replicationdata_prepped.dta")

## set variables

## controls
# gl ictrls_2l_varying iwdi_pop_2l iwdi_oda_2l iwdi_gdppc_2l iunhcr_ref_idp_2l iwdi_literacy_2l iwdi_fuel_2l
ictrls <- c("ictrls_2l_varying", "iwdi_pop_2l", "iwdi_oda_2l", "iwdi_gdppc_2l", "iunhcr_ref_idp_2l", "iwdi_literacy_2l", "iwdi_fuel_2l")
# gl ictrls_3l_varying iwdi_pop_3l iwdi_oda_3l iwdi_gdppc_3l iunhcr_ref_idp_3l iwdi_literacy_3l iwdi_fuel_3l
ictrls_3l_varying <- c("iwdi_pop_3l", "iwdi_oda_3l", "iwdi_gdppc_3l", "iunhcr_ref_idp_3l", "iwdi_literacy_3l", "iwdi_fuel_3l")
# gl ictrls_4l_varying iwdi_pop_4l iwdi_oda_4l iwdi_gdppc_4l iunhcr_ref_idp_4l iwdi_literacy_4l iwdi_fuel_4l
ictrls_4l_varying <- c("iwdi_pop_4l", "iwdi_oda_4l", "iwdi_gdppc_4l", "iunhcr_ref_idp_4l", "iwdi_literacy_4l", "iwdi_fuel_4l")
# gl ictrls_5l_varying iwdi_pop_5l iwdi_oda_5l iwdi_gdppc_5l iunhcr_ref_idp_5l iwdi_literacy_5l iwdi_fuel_5l
ictrls_5l_varying <- c("iwdi_pop_5l", "iwdi_oda_5l", "iwdi_gdppc_5l", "iunhcr_ref_idp_5l", "iwdi_literacy_5l", "iwdi_fuel_5l")

## dependent variable
dv <- "v2x_polyarchy"
## fe
xtset <- c("country")


## replicate table 3
# iv_ctrls_simple v2x_polyarchy, indvar(ipema_any_demo_assist_dum_2l) iv(ipema_any_demo_assdiv_2l) title(table3)
# xtreg `indvar' `iv' $ictrls_3l_varying, fe
# 			local F_stat_round_ctrls = round(e(F),.001)
#
# 		xtivreg `depvar' (`indvar' = `iv') $ictrls_3l_varying , fe

ictrls_3l_varying <- c("iwdi_pop_3l", "iwdi_oda_3l", "iwdi_gdppc_3l", "iunhcr_ref_idp_3l", "iwdi_literacy_3l", "iwdi_fuel_3l")
dv <- "v2x_polyarchy"
iv <- "ipema_any_demo_assdiv_2l"
endogenous_var <- "ipema_any_demo_assist_dum_2l"
index_vars <- "country"

mod3 <- as.formula(paste(dv, "~", paste(c(ictrls_3l_varying), collapse = " + "), "|", index_vars, "|", endogenous_var, "~", iv))

tab3 <- fixest::feols(mod3, data = data)

summary(tab3)
summary(tab3, vcov = "iid")

mod3.1 <- as.formula(paste(dv, "~", paste(c(ictrls_3l_varying), collapse = " + "), "|", index_vars, "| (", endogenous_var, "~", iv, ")"))
tab3.1 <- lfe::felm(mod3.1, data = data)
summary(tab3.1)


texreg::screenreg(list(tab3, tab3.1))


## get stats from first model
f_stat <- as.numeric(fixest::fitstat(tab3, type = "ivf")$ivf)
r_squared <- as.numeric(fixest::fitstat(tab3, "r2")$r2)
adj_r_squared <- as.numeric(fixest::fitstat(tab3, "ar2")$ar2)


## get stats from second model
f_stat_tab3.1 <- summary(tab3.1)$E.fstat[2]
r_squared_tab3.1 <- summary(tab3.1)$r.squared
adj_r_squared_tab3.1 <- summary(tab3.1)$adj.r.squared
nobs_tab3.1 <- summary(tab3.1)$N


custom_texreg_fixest <- function(model, f_stat, r_squared, adj_r_squared) {
  s <- summary(model, vcov = "iid")
  
  coefnames <- rownames(s$coeftable)
  coefs <- s$coeftable[, 1]
  # se <- s$coeftable[, 2]
  ## analogous to the function in 00-main, replace se with pvalue
  se <- s$coeftable[, 4]
  pvalues <- s$coeftable[, 4]
  
  gof <- c(f_stat, r_squared, adj_r_squared, nobs(model))
  gof.names <- c("F-statistic (first stage)", "R^2 (full model)", "Adj. R^2 (full model)", "Number of observations")
  
  texreg::createTexreg(
    coef.names = coefnames,
    coef = coefs,
    se = se,
    pvalues = pvalues,
    gof.names = gof.names,
    gof = gof
  )
}

custom_tab3 <- custom_texreg_fixest(tab3, f_stat[1], r_squared[1], adj_r_squared[1])

custom_texreg_felm <- function(model, f_stat, r_squared, adj_r_squared, nobs) {
  s <- summary(model)
  
  coefnames <- rownames(s$coefficients)
  coefs <- s$coefficients[, 1]
  # se <- s$coefficients[, 2]
  ## analogous to the function in 00-main, replace se with pvalue
  se <- s$coefficients[, 4]
  pvalues <- s$coefficients[, 4]
  
  gof <- c(f_stat[1], r_squared[1], adj_r_squared[1], nobs(model))
  gof.names <- c("F-statistic (first stage)", "R^2 (full model)", "Adj. R^2 (full model)", "Number of observations")
  
  texreg::createTexreg(
    coef.names = coefnames,
    coef = coefs,
    se = se,
    pvalues = pvalues,
    gof.names = gof.names,
    gof = as.numeric(gof)
  )
}

custom_tab3.1 <- custom_texreg_felm(tab3.1, f_stat_tab3.1[1], r_squared_tab3.1[1], adj_r_squared_tab3.1[1], nobs_tab3.1[1])


texreg::texreg(
  list(custom_tab3, custom_tab3.1),
  omit.coef = "(iwdi)|(iunhcr)",
  custom.coef.names = c("Democracy mandate", "Democracy mandate"),
  custom.model.names = c("Elec. dem. (V-Dem) - feols", "Elec. dem. (V-Dem) - felm"),
  label = "tab:tab3.og",
  stars = c(0.01, 0.05, 0.1),
  include.ci = FALSE,
  digits = 3,
  caption = "Table 3 replication (original)",
  custom.note = "%stars.",
  file = "tables/table3.og.tex"
)



## replicate table 7
# Define variables
ictrls_3l_varying <- c("iwdi_pop_3l", "iwdi_oda_3l", "iwdi_gdppc_3l", "iunhcr_ref_idp_3l", "iwdi_literacy_3l", "iwdi_fuel_3l")
dv <- "v2x_polyarchy"
iv <- "iany_demo_all_maxdiv_2l"
endogenous_var <- "iany_demo_all_max_dum_2l"
index_vars <- "country"

mod7 <- as.formula(paste(dv, "~", paste(c(ictrls_3l_varying), collapse = " + "), "|", index_vars, "|", endogenous_var, "~", iv))

tab7 <- fixest::feols(mod7, data = data)

summary(tab7)
summary(tab7, vcov = "iid")


mod7.1 <- as.formula(paste(dv, "~", paste(c(ictrls_3l_varying), collapse = " + "), "|", index_vars, "| (", endogenous_var, "~", iv, ")"))

tab7.1 <- lfe::felm(mod7.1, data = data)
summary(tab7.1)

texreg::screenreg(list(tab7, tab7.1))

## get stats from first model
f_stat <- as.numeric(fixest::fitstat(tab7, type = "ivf")$ivf)
r_squared <- as.numeric(fixest::fitstat(tab7, "r2")$r2)
adj_r_squared <- as.numeric(fixest::fitstat(tab7, "ar2")$ar2)


## get stats from second model
f_stat_tab7.1 <- summary(tab7.1)$E.fstat[2]
r_squared_tab7.1 <- summary(tab7.1)$r.squared
adj_r_squared_tab7.1 <- summary(tab7.1)$adj.r.squared
nobs_tab7.1 <- summary(tab7.1)$N


custom_texreg_fixest <- function(model, f_stat, r_squared, adj_r_squared) {
  s <- summary(model, vcov = "iid")
  
  coefnames <- rownames(s$coeftable)
  coefs <- s$coeftable[, 1]
  # se <- s$coeftable[, 2]
  se <- s$coeftable[, 4]
  pvalues <- s$coeftable[, 4]
  
  gof <- c(f_stat, r_squared, adj_r_squared, nobs(model))
  gof.names <- c("F-statistic (first stage)", "R^2 (full model)", "Adj. R^2 (full model)", "Number of observations")
  
  texreg::createTexreg(
    coef.names = coefnames,
    coef = coefs,
    se = se,
    pvalues = pvalues,
    gof.names = gof.names,
    gof = gof
  )
}

custom_tab7 <- custom_texreg_fixest(tab7, f_stat[1], r_squared[1], adj_r_squared[1])

custom_texreg_felm <- function(model, f_stat, r_squared, adj_r_squared, nobs) {
  s <- summary(model)
  
  coefnames <- rownames(s$coefficients)
  coefs <- s$coefficients[, 1]
  # se <- s$coefficients[, 2]
  se <- s$coefficients[, 4]
  pvalues <- s$coefficients[, 4]
  
  gof <- c(f_stat[1], r_squared[1], adj_r_squared[1], nobs(model))
  gof.names <- c("F-statistic (first stage)", "R^2 (full model)", "Adj. R^2 (full model)", "Number of observations")
  
  texreg::createTexreg(
    coef.names = coefnames,
    coef = coefs,
    se = se,
    pvalues = pvalues,
    gof.names = gof.names,
    gof = as.numeric(gof)
  )
}

custom_tab7.1 <- custom_texreg_felm(tab7.1, f_stat_tab7.1[1], r_squared_tab7.1[1], adj_r_squared_tab7.1[1], nobs_tab7.1[1])


texreg::texreg(
  list(custom_tab7, custom_tab7.1),
  omit.coef = "(iwdi)|(iunhcr)",
  custom.coef.names = c("Democracy mandate", "Democracy mandate"),
  custom.model.names = c("Elec. dem. (V-Dem) - feols", "Elec. dem. (V-Dem) - felm"),
  label = "tab:tab7.og",
  stars = c(0.01, 0.05, 0.1),
  include.ci = FALSE,
  digits = 3,
  caption = "Table 7 replication (original)",
  custom.note = "%stars.",
  file = "tables/table7.og.tex"
)
