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
xtset <- c("country", "year")


## replicate table 2
# indvar_separate_ctrls v2x_polyarchy, indvar(ipema_any_demo_assist_dum_2l) title(table2)
iv <- "ipema_any_demo_assist_dum_2l"
mod2.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab2.1 <- plm::plm(mod2.1, index = c("country","year"),
                   effect = "twoway",
                   data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab2.1)

data2.2 <- data %>% filter(ucdp_0yrs == 1)
tab2.2 <- plm::plm(mod2.1, index = c("country","year"),
                   effect = "twoway",
                   data = na.omit(data2.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab2.2)

data2.3 <- data %>% filter(ucdp_1yrs == 1)
tab2.3 <- plm::plm(mod2.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data2.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab2.3)

data2.4 <- data %>% filter(ucdp_2yrs == 1)
tab2.4 <- plm::plm(mod2.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data2.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab2.4)

data2.5 <- data %>% filter(ucdp_3yrs == 1)
tab2.5 <- plm::plm(mod2.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data2.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab2.5)

models2 <- list(tab2.1, tab2.2, tab2.3, tab2.4, tab2.5)
## comment this out if you want to have standard errors in brackets instead of p-values
models2 <- replace_se_with_p_values(models2)

texreg::texreg(models2,
               omit.coef = "(Intercept)|(iwdi)|(iunhcr)",
               custom.coef.names = "Democracy mandate",
               caption = "Table 2 replication (panel regression country random + year fixed effects - scaled variables)",
               label = "tab:tab2plmrescaled",
               file = "tables/table2.plm.re.scaled.tex",
               stars = c(0.01, 0.05, 0.1),
               digits = 3)



## replicate table 4
# indvar_separate_ctrls v2x_polyarchy, indvar(itotal_compound_K_2l) title(table4)
iv <- "itotal_compound_K_2l"
mod4.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab4.1 <- plm::plm(mod4.1, index = c("country","year"),
                   effect = "twoway",
                   data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab4.1)

data4.2 <- data %>% filter(ucdp_0yrs == 1)
tab4.2 <- plm::plm(mod4.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data4.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab4.2)

data4.3 <- data %>% filter(ucdp_1yrs == 1)
tab4.3 <- plm::plm(mod4.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data4.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab4.3)

data4.4 <- data %>% filter(ucdp_2yrs == 1)
tab4.4 <- plm::plm(mod4.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data4.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab4.4)

data4.5 <- data %>% filter(ucdp_3yrs == 1)
tab4.5 <- plm::plm(mod4.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data4.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab4.5)

models4 <- list(tab4.1, tab4.2, tab4.3, tab4.4, tab4.5)
## comment this out if you want to have standard errors in brackets instead of p-values
models4 <- replace_se_with_p_values(models4)

texreg::texreg(models4,
               omit.coef = "(Intercept)|(iwdi)|(iunhcr)",
               custom.coef.names = "\\# of uniformed personnel",
               caption = "Table 4 replication (panel regression country random + year fixed effects - scaled variables)",
               label = "tab:tab4plmrescaled",
               file = "tables/table4.plm.re.scaled.tex",
               stars = c(0.01, 0.05, 0.1),
               digits = 3)



## replicate table 5
# indvar_separate_ctrls v2x_polyarchy, indvar(iactual_civilian_total_K_2l) title(table5)
iv <- "iactual_civilian_total_K_2l"
mod5.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab5.1 <- plm::plm(mod5.1, index = c("country","year"),
                   effect = "twoway",
                   data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab5.1)

data5.2 <- data %>% filter(ucdp_0yrs == 1)
tab5.2 <- plm::plm(mod5.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data5.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab5.2)

data5.3 <- data %>% filter(ucdp_1yrs == 1)
tab5.3 <- plm::plm(mod5.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data5.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab5.3)

data5.4 <- data %>% filter(ucdp_2yrs == 1)
tab5.4 <- plm::plm(mod5.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data5.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab5.4)

data5.5 <- data %>% filter(ucdp_3yrs == 1)
tab5.5 <- plm::plm(mod5.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data5.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab5.5)

models5 <- list(tab5.1, tab5.2, tab5.3, tab5.4, tab5.5)
## comment this out if you want to have standard errors in brackets instead of p-values
models5 <- replace_se_with_p_values(models5)

texreg::texreg(models5,
               omit.coef = "(Intercept)|(iwdi)|(iunhcr)",
               custom.coef.names = "\\# of civilian personnel",
               caption = "Table 5 replication (panel regression country random + year fixed effects - scaled variables)",
               label = "tab:tab5plmrescaled",
               file = "tables/table5.plm.re.scaled.tex",
               stars = c(0.01, 0.05, 0.1),
               digits = 3)



## replicate table 6
# indvar_separate_ctrls v2x_polyarchy, indvar(iany_demo_all_max_dum_2l) title(table6)
iv <- "iany_demo_all_max_dum_2l"
mod6.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab6.1 <- plm::plm(mod6.1, index = c("country","year"),
                   effect = "twoway",
                   data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab6.1)

data6.2 <- data %>% filter(ucdp_0yrs == 1)
tab6.2 <- plm::plm(mod6.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data6.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab6.2)

data6.3 <- data %>% filter(ucdp_1yrs == 1)
tab6.3 <- plm::plm(mod6.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data6.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab6.3)

data6.4 <- data %>% filter(ucdp_2yrs == 1)
tab6.4 <- plm::plm(mod6.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data6.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab6.4)

data6.5 <- data %>% filter(ucdp_3yrs == 1)
tab6.5 <- plm::plm(mod6.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data6.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab6.5)

models6 <- list(tab6.1, tab6.2, tab6.3, tab6.4, tab6.5)
## comment this out if you want to have standard errors in brackets instead of p-values
models6 <- replace_se_with_p_values(models6)

texreg::texreg(models6,
               omit.coef = "(Intercept)|(iwdi)|(iunhcr)",
               custom.coef.names = "Any dem. activities",
               caption = "Table 6 replication (panel regression country random + year fixed effects - scaled variables)",
               label = "tab:tab6plmrescaled",
               file = "tables/table6.plm.re.scaled.tex",
               stars = c(0.01, 0.05, 0.1),
               digits = 3)



## replicate table 8
# indvar_together_ctrls v2x_polyarchy, indvar(iany_demo_engage_max_dum_2l iany_demo_bypass_max_dum_2l) title(table8)
iv <- c("iany_demo_engage_max_dum_2l", "iany_demo_bypass_max_dum_2l")
mod8.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab8.1 <- plm::plm(mod8.1, index = c("country","year"),
                   effect = "twoway",
                   data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab8.1)

data8.2 <- data %>% filter(ucdp_0yrs == 1)
tab8.2 <- plm::plm(mod8.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data8.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab8.2)

data8.3 <- data %>% filter(ucdp_1yrs == 1)
tab8.3 <- plm::plm(mod8.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data8.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab8.3)

data8.4 <- data %>% filter(ucdp_2yrs == 1)
tab8.4 <- plm::plm(mod8.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data8.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab8.4)

data8.5 <- data %>% filter(ucdp_3yrs == 1)
tab8.5 <- plm::plm(mod8.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data8.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab8.5)

models8 <- list(tab8.1, tab8.2, tab8.3, tab8.4, tab8.5)
## comment this out if you want to have standard errors in brackets instead of p-values
models8 <- replace_se_with_p_values(models8)

texreg::texreg(models8,
               omit.coef = "(Intercept)|(iwdi)|(iunhcr)",
               custom.coef.names = c("Any dem. eng. with host state",
                                     "Any dem. byp. of host state"),
               caption = "Table 8 replication (panel regression country random + year fixed effects - scaled variables)",
               label = "tab:tab8plmrescaled",
               file = "tables/table8.plm.re.scaled.tex",
               stars = c(0.01, 0.05, 0.1),
               digits = 3)



## replicate table 9
# indvar_together_ctrls v2x_polyarchy, indvar(idemo_all_max_dum_2l ielections_all_max_dum_2l iparties_all_max_dum_2l ivoters_all_max_dum_2l) title(table9)
iv <- c("idemo_all_max_dum_2l", "ielections_all_max_dum_2l", "iparties_all_max_dum_2l", "ivoters_all_max_dum_2l")
mod9.1 <- paste(dv, paste0(c(iv, ictrls_3l_varying), collapse = " + "), sep = " ~ ")
datafilter <- c(dv, xtset, iv, ictrls_3l_varying)

tab9.1 <- plm::plm(mod9.1, index = c("country","year"),
                   effect = "twoway",
                   data = na.omit(data[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)),
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab9.1)

data9.2 <- data %>% filter(ucdp_0yrs == 1)
tab9.2 <- plm::plm(mod9.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data9.2[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab9.2)

data9.3 <- data %>% filter(ucdp_1yrs == 1)
tab9.3 <- plm::plm(mod9.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data9.3[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab9.3)

data9.4 <- data %>% filter(ucdp_2yrs == 1)
tab9.4 <- plm::plm(mod9.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data9.4[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
#summary(tab9.4)

data9.5 <- data %>% filter(ucdp_3yrs == 1)
tab9.5 <- plm::plm(mod9.1, index = c("country","year"), 
                   effect = "twoway",
                   data = na.omit(data9.5[, datafilter]) %>% mutate(across(all_of(ictrls_3l_varying), scale)), 
                   model = "random",
                   random.dfcor = 3,
                   vcov = function(x) vcovHC(x, cluster="country"))
# summary(tab9.5)

models9 <- list(tab9.1, tab9.2, tab9.3, tab9.4, tab9.5)
## comment this out if you want to have standard errors in brackets instead of p-values
models9 <- replace_se_with_p_values(models9)

texreg::texreg(models9,
               omit.coef = "(Intercept)|(iwdi)|(iunhcr)",
               custom.coef.names = c("Any dem. institution act's",
                                     "Any election act's",
                                     "Any pol. party act's",
                                     "Any voter act's"),
               caption = "Table 9 replication (panel regression country random + year fixed effects - scaled variables)",
               label = "tab:tab9plmrescaled",
               file = "tables/table9.plm.re.scaled.tex",
               stars = c(0.01, 0.05, 0.1),
               digits = 3)

