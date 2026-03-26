#######Replication data for "Opting for opt-outs"#######
#Recommended citation: Moland, M. (forthcoming) ‘Opting for opt outs? National identities and support for a differentiated EU’, JCMS: Journal of Common Market Studies, doi:10.1111/jcms.13478
#Note: Due to probabilistic nature of imputation procedure, some results may vary slightly from those in the paper

#####Load data#######
EUI_imputed_10 <- readRDS("data/EUI_imputed_10.Rds")
EUI_imputed_10 <- readRDS("data/datlist_EUI.Rds")
EUI_merged_new <- readRDS("data/EUI_merged_new.Rds")
EUI_impute_nonordic <- readRDS("data/EUI_impute_nonordic.Rds")
EUI_impute_noCEE <- readRDS("data/EUI_impute_noCEE.Rds")

####Table 1####
#####Function for extracting DF########
optout_mod_noeu <- with(datlist_EUI, lm(opt_out_supp ~ market_europe +
                                          alternative_econ + sat_nat_dem + left_right +
                                          left_right_sqr + gender + age + income_scale + 
                                          excl_identity + factor(country), weights = weight))

library(sandwich)
library(texreg)
betas_optout_noeu <- lapply(optout_mod_noeu, coef)
vars_optout_noeu <- lapply(optout_mod_noeu, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)}) #vcovCL is in the sandwich package

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) #number of imputations
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){ vcovCL(x, cluster = cl)})
  # conduct statistical inference and save results into a data.frame
  model1 <- summary(pool_mi(betas, vars))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  #technically, should use R to Z-- but close for now when R is low
  ns <- nobs(tt[[1]])
  
  #creates what is used by texreg
  tr <- createTexreg(
    coef.names = row.names(model1), 
    coef = model1$results, 
    se = model1$se, 
    pvalues = model1$p,
    gof.names = c("R2", "Nobs"), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
}

texreg_1 <- extract.df(optout_mod_noeu, cl = datlist_EUI[[1]]$country)

###Test with EU
optout_mod_eu <- with(datlist_EUI, lm(opt_out_supp ~ market_europe +
                                        alternative_econ + sat_nat_dem + left_right + eurosceptic +
                                        left_right_sqr + gender + age + income_scale + 
                                        excl_identity + factor(country), weights = weight))


betas_optout_eu <- lapply( optout_mod_eu, coef)
vars_optout_eu <- lapply(optout_mod_eu, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_2 <- extract.df(optout_mod_eu, cl = datlist_EUI[[1]]$country)

######Run imputed two speed models########
twospeed_mod_noeu <- with(datlist_EUI, lm(two_speed_supp ~ market_europe +
                                            alternative_econ + sat_nat_dem + left_right +
                                            left_right_sqr + gender + age + income_scale + 
                                            excl_identity + factor(country), weights = weight))

betas_twospeed_noeu <- lapply( twospeed_mod_noeu, coef)
vars_twospeed_noeu <- lapply( twospeed_mod_noeu, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_3 <- extract.df(twospeed_mod_noeu, cl = datlist_EUI[[1]]$country)


#Do same for full model
twospeed_mod_eu <- with(datlist_EUI, lm(two_speed_supp ~ market_europe +
                                          alternative_econ + sat_nat_dem + left_right +
                                          left_right_sqr + eurosceptic + gender + age + income_scale + 
                                          excl_identity + factor(country), weights = weight))

betas_twospeed_eu <- lapply( twospeed_mod_eu, coef)
vars_twospeed_eu <- lapply( twospeed_mod_eu, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_4 <- extract.df(twospeed_mod_eu, cl = datlist_EUI[[1]]$country)



#####Create named vector for variables######
coef_map <- list("excl_identity1" = "Exclusively national identity",
                 'market_europe' = 'Liberal economic values',
                 'alternative_econ' = "Perception of economy", 
                 "left_right" = "Left-right",
                 "age" = "Age",
                 "euro_supp" = "Support for eurozone integration",
                 "sat_nat_dem" = "Satisfaction w/ national democracy", 
                 "eu_democ_satisf" = "Satisfaction with EU democracy",
                 "eurosceptic1" = "Eurosceptic", 
                 "left_right_sqr" = "Left-right (sqr.)", 
                 "gender" = "Gender",
                 "income_scale" = "Income",
                 "eu_democ_dissatisf" = "Dissatisfaction with EU democracy",
                 "eurosceptic1:excl_identity1" = "Exclusively national identity X Euroscepticism")

#Texreg table
htmlreg(list(texreg_1, texreg_2, texreg_3, texreg_4), custom.coef.map = coef_map, 
        caption.above = TRUE, single.row = TRUE,
        caption = "Table 2: Model showing correlation between national identity and support for differentiated integration.
        Cluster-robust SEs (country-level) and country fixed effects", 
        custom.model.names = c("Constitutional DI <br> (no EU variable)",
                               "Constitutional DI <br> (full model)",
                               "Instrumental DI <br> (no EU variable)",
                               "Instrumental DI <br> (full model)"))

######Figure 1########
#Run necessary model
library(lme4)
library(emmeans)
EUI_imputed_10$region_factor <- factor(EUI_imputed_10$region_dummy, 
                                       levels = c(0,1))

#Run models
optout_interact_imputecomplete <- lmer(opt_out_supp ~ market_europe +
                                         alternative_econ + sat_nat_dem + left_right +
                                         left_right_sqr + eurosceptic + gender + age + income_scale + 
                                         excl_identity + region_factor +
                                         excl_identity*region_factor + (1|country),
                                       weights = weight, data = EUI_imputed_10)

twospeed_interact_imputecomplete <- lmer(two_speed_supp ~ market_europe +
                                           alternative_econ + sat_nat_dem + left_right +
                                           left_right_sqr + eurosceptic + gender + age + income_scale + 
                                           excl_identity + region_factor +
                                           excl_identity*region_factor + (1|country),
                                         weights = weight, data = EUI_imputed_10)

#Create estimated marginal means to estimate changes in marginal means
region_optout_emm = emmeans(optout_interact_imputecomplete, 
                            specs = pairwise ~  excl_identity:region_factor)

#Panel A
region_optout_emm

optout_excl_region0 =c(0,1,0,0)
optout_excl_region1 = c(0,0,0,1)

region_contrast <- contrast(region_optout_emm, 
                            method = list("Exclusive (Nordic) - Exclusive (CEE)" = optout_excl_region1 - optout_excl_region0))

region_plot_optout <- plot(region_contrast) + scale_y_discrete(breaks = "Exclusive (Nordics) - Exclusive (CEE)",
                                                               labels = "Exclusive (Nordics) -\n Exclusive (CEE)") +
  labs(title = "Constitutional DI",
       x = "Change marginal means exclusive (Nordics) vs.\n exclusive (CEE)", y = "Comparisons") + geom_vline(xintercept = 0, 
                                                                                                              linetype = "dashed")

#Panel B
region_twospeed_emm = emmeans(twospeed_interact_imputecomplete, 
                              specs = pairwise ~  excl_identity:region_factor)

region_twospeed_emm

#Create plot
optout_excl_region0 =c(0,1,0,0)
optout_excl_region1 = c(0,0,0,1)

region_contrast_twospeed <- contrast(region_twospeed_emm, 
                                     method = list("Exclusive (Nordics) - Exclusive (CEE)" = optout_excl_region1 - optout_excl_region0))

region_plot_twospeed <- plot(region_contrast_twospeed) + scale_y_discrete(breaks = "Exclusive (Nordics) - Exclusive (CEE)",
                                                                          labels = "Exclusive (Nordics) -\n Exclusive (CEE)") +
  labs(title = "Instrumental DI",
       x = "Change marginal means exclusive (Nordics) vs.\n exclusive (CEE)", y = "Comparisons") + scale_x_continuous(limits = c(-0.35, 0)) +
  geom_vline(xintercept = 0, linetype = "dashed")

#Compile figure
library(patchwork)
patch_margmeans_region = region_plot_optout / region_plot_twospeed + plot_annotation(title = "Changes in marginal means for interaction exclusively national identity and region", 
                                                                                     tag_levels = "A")
########Figure 2########
#Start by imputing data without Nordics
library(mice)
EUI_no_nordic <- EUI_merged_new %>%
  dplyr::filter(! nordic_dummy == 1)

EUI_impute_nonordic <- mice(EUI_no_nordic, m = 25)
datlist_EUI_nonordic <- miceadds::mids2datlist(EUI_impute_nonordic)

#Create models
set.seed(465)
optout_CEE_support <- with(datlist_EUI_nonordic, lmer(opt_out_supp ~ market_europe +
                                                        alternative_econ + sat_nat_dem + left_right +
                                                        left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                        excl_identity + CEE_dummy +
                                                        excl_identity*CEE_dummy + 
                                                        (1|country),
                                                      weights = weight))

pool_CEE_new <- mice::pool(optout_CEE_support)

set.seed(469)
twospeed_CEE_support <- with(datlist_EUI_nonordic, lmer(two_speed_supp ~ market_europe +
                                                          alternative_econ + sat_nat_dem + left_right +
                                                          left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                          excl_identity + CEE_dummy +
                                                          excl_identity*CEE_dummy + 
                                                          (1|country),
                                                        weights = weight))

pool_CEE_twospeed <- mice::pool(twospeed_CEE_support)

#Run Nordic models
EUI_no_CEE <- EUI_merged_new %>%
  dplyr::filter(! CEE_dummy == 1)

#Impute
datlist_EUI_noCEE <- miceadds::mids2datlist(EUI_impute_noCEE)

#Start with opt-outs
set.seed(987)
optout_nordic_support <- with(datlist_EUI_noCEE, lmer(opt_out_supp ~ market_europe +
                                                        alternative_econ + sat_nat_dem + left_right +
                                                        left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                        excl_identity + nordic_dummy +
                                                        excl_identity*nordic_dummy + 
                                                        (1|country),
                                                      weights = weight))

pool_nordic_new <- mice::pool(optout_nordic_support)

twospeed_nordic_support <- with(datlist_EUI_noCEE, lmer(two_speed_supp ~ market_europe +
                                                          alternative_econ + sat_nat_dem + left_right +
                                                          left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                          excl_identity + nordic_dummy +
                                                          excl_identity*nordic_dummy + 
                                                          (1|country),
                                                        weights = weight))

pool_nordic_twospeed <- mice::pool(twospeed_nordic_support)

#Produce contrasts
#Start by creating relevant data
EUI_imputed_nonordic10 <- mice::complete(EUI_impute_nonordic, 10)

EUI_imputed_nonordic10$CEE_dummy <- factor(EUI_imputed_nonordic10$CEE_dummy, 
                                           levels = c(0,1))

#Run CEE model
optout_interact_nonordic_imputecomplete <- lmer(opt_out_supp ~ market_europe +
                                                  alternative_econ + sat_nat_dem + left_right +
                                                  left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                  excl_identity + CEE_dummy +
                                                  excl_identity*CEE_dummy + (1|country),
                                                weights = weight, data = EUI_imputed_nonordic10)

#Create Fig. 2 panel A
emm_CEE_optout = emmeans(optout_interact_nonordic_imputecomplete, 
                         specs = pairwise ~  excl_identity:CEE_dummy)


#Create subset
optout_excl_nonCEE =c(0,1,0,0)
optout_excl_CEE = c(0,0,0,1)

CEE_contrast <- contrast(emm_CEE_optout, 
                         method = list("Exclusive (CEE) - Exclusive (non-CEE)" = optout_excl_CEE - optout_excl_nonCEE))

CEE_optout_contrast <- plot(CEE_contrast) + scale_y_discrete(breaks = "Exclusive (CEE) - Exclusive (non-CEE)",
                                                             labels = "Exclusive (CEE) -\n Exclusive (non-CEE)") +
  labs(title = "Constitutional DI", subtitle = "CEE vs. non-CEE",
       x = "Marginal mean change", y = "Comparisons") +
  scale_x_continuous(limits = c(0,0.40)) + geom_vline(xintercept = 0, linetype = "dashed")

#Create Fig. 2 panel B
emm_CEE_twospeed = emmeans(twospeed_interact_nonordic_imputecomplete, 
                           specs = pairwise ~  excl_identity:CEE_dummy)

emm_CEE_twospeed

##Create custom contrast and plot
CEE_contrast_twospeed <- contrast(emm_CEE_twospeed, 
                                  method = list("Exclusive (CEE) - Exclusive (non-CEE)" = optout_excl_CEE - optout_excl_nonCEE))


#Create plot
CEE_twospeed_contrast <- plot(CEE_contrast_twospeed) + scale_y_discrete(breaks = "Exclusive (CEE) - Exclusive (non-CEE)",
                                                                        labels = "Exclusive (CEE) -\n Exclusive (non-CEE)") +
  labs(title = "Instrumental DI",
       x = "Marginal mean change", subtitle = "CEE vs. non-CEE",
       y = "Comparisons") +
  scale_x_continuous(limits = c(0,0.40)) + geom_vline(xintercept = 0, linetype = "dashed")

#Create Nordic data
EUI_imputed_noCEE10 <- mice::complete(EUI_impute_noCEE, 10)

EUI_imputed_noCEE10$nordic_dummy <- factor(EUI_imputed_noCEE10$nordic_dummy, 
                                           levels = c(0,1))

#Create Nordic model
optout_interact_noCEE_imputecomplete <- lmer(opt_out_supp ~ market_europe +
                                               alternative_econ + sat_nat_dem + left_right +
                                               left_right_sqr + eurosceptic + gender + age + income_scale + 
                                               excl_identity + nordic_dummy +
                                               excl_identity*nordic_dummy + (1|country),
                                             weights = weight, data = EUI_imputed_noCEE10)

#Create marginal means
optout_excl_nonNordic =c(0,1,0,0)
optout_excl_Nordic = c(0,0,0,1)

emm_nordic_optout = emmeans(optout_interact_noCEE_imputecomplete, 
                            specs = pairwise ~  excl_identity:nordic_dummy)

#Plot Fig. 2 panel C
nordic_contrast_optout <- contrast(emm_nordic_optout, 
                                   method = list("Exclusive (Nordics) - Exclusive (non-Nordics)" = optout_excl_Nordic - optout_excl_nonNordic))

plot_nordic_optout <- plot(nordic_contrast_optout) + scale_y_discrete(breaks = "Exclusive (Nordics) - Exclusive (non-Nordics)",
                                                                      labels = "Exclusive (Nordics) -\n Exclusive (non-Nordics)") +
  labs(title = "Constitutional DI", subtitle = "Nordics vs. non-Nordics",
       x = "Marginal mean change", y = "Comparisons") + geom_vline(xintercept = 0, linetype = "dashed")

#Create model for panel D
instrumental_interact_noCEE_imputecomplete <- lmer(two_speed_supp ~ market_europe +
                                                     alternative_econ + sat_nat_dem + left_right +
                                                     left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                     excl_identity + nordic_dummy +
                                                     excl_identity*nordic_dummy + (1|country),
                                                   weights = weight, data = EUI_imputed_noCEE10)

#Create marginal means
emm_nordic_twospeed = emmeans(instrumental_interact_noCEE_imputecomplete, 
                              specs = pairwise ~  excl_identity:nordic_dummy)

nordic_contrast_twospeed <- contrast(emm_nordic_twospeed, 
                                     method = list("Exclusive (Nordics) - Exclusive (non-Nordics)" = optout_excl_Nordic - optout_excl_nonNordic))

#Plot Fig 2. panel D
nordic_twospeed_contrast <- plot(nordic_contrast_twospeed) + scale_y_discrete(breaks = "Exclusive (Nordics) - Exclusive (non-Nordics)",
                                                                              labels = "Exclusive (Nordics) -\n Exclusive (non-Nordics)") +
  labs(title = "Instrumental DI", subtitle = "Nordics vs. non-Nordics",
       x = "Marginal mean change", y = "Comparisons") + geom_vline(xintercept = 0, linetype = "dashed")

#Create full fig. 2
margmeans_patch_region2 = CEE_optout_contrast + CEE_twospeed_contrast + plot_nordic_optout + nordic_twospeed_contrast +
  plot_annotation(title = "Changes in marginal means", tag_levels = "A")

##########Online supplementary information########
##S1
library(MASS)

#Constitutional DI
optout_mod_noeu_polr <- with(datlist_EUI, polr(factor(opt_out_supp) ~ market_europe +
                                                 alternative_econ + sat_nat_dem + left_right +
                                                 left_right_sqr + gender + age + income_scale + 
                                                 excl_identity + factor(country), weights = weight, 
                                               Hess = TRUE),
                             (clval <- sandwich::vcovCL(optout_mod_noeu_polr, datlist_EUI[[1]]$country)))

pooled_polr_optoutnoeu <-  mice::pool(optout_mod_noeu_polr)


optout_mod_polr <- with(datlist_EUI, polr(factor(opt_out_supp) ~ market_europe +
                                            alternative_econ + sat_nat_dem + left_right +
                                            left_right_sqr + gender + age + income_scale + eurosceptic + 
                                            excl_identity + factor(country), weights = weight, 
                                          Hess = TRUE),
                        (clval <- sandwich::vcovCL(optout_mod_polr, datlist_EUI[[1]]$country)))

pooled_polr_optout <-  mice::pool(optout_mod_polr)

#Instrumental DI
twospeed_mod_noeu_polr <- with(datlist_EUI, polr(factor(two_speed_supp) ~ market_europe +
                                                   alternative_econ + sat_nat_dem + left_right +
                                                   left_right_sqr + gender + age + income_scale + 
                                                   excl_identity + factor(country), weights = weight, 
                                                 Hess = TRUE),
                               (clval <- sandwich::vcovCL(twospeed_mod_noeu_polr, datlist_EUI[[1]]$country)))

pooled_polr_twospeednoeu <-  mice::pool(twospeed_mod_noeu_polr)

#Instrumental DI
twospeed_mod_polr <- with(datlist_EUI, polr(factor(two_speed_supp) ~ market_europe +
                                              alternative_econ + sat_nat_dem + left_right + eurosceptic +
                                              left_right_sqr + gender + age + income_scale + 
                                              excl_identity + factor(country), weights = weight, 
                                            Hess = TRUE),
                          (clval <- sandwich::vcovCL(twospeed_mod_polr, datlist_EUI[[1]]$country)))

pooled_polr_twospeed <-  mice::pool(twospeed_mod_polr)

#Create modelsummary
library(modelsummary)
modelsummary::modelsummary(list(pooled_polr_optoutnoeu, pooled_polr_optout,
                                pooled_polr_twospeednoeu, pooled_polr_twospeed), coef_map = coef_map_interact,
                           coef_omit = "country", output = "html", stars = TRUE, fmt = 2)

##S2
optout_mod_alteu <- with(datlist_EUI, lm(opt_out_supp ~ market_europe + sat_nat_dem +
                                           alternative_econ + eu_democ_dissatisf + left_right +
                                           left_right_sqr + gender + age + income_scale + 
                                           excl_identity + factor(country), weights = weight))


betas_optout_alteu <- lapply( optout_mod_alteu, coef)
vars_optout_alteu <- lapply(optout_mod_alteu, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_alteu_optout <- extract.df(optout_mod_alteu, cl = datlist_EUI[[1]]$country)

##Do same for two-speed
twospeed_mod_alteu <- with(datlist_EUI, lm(two_speed_supp ~ market_europe + sat_nat_dem +
                                             alternative_econ + eu_democ_dissatisf + left_right +
                                             left_right_sqr + gender + age + income_scale + 
                                             excl_identity + factor(country), weights = weight))


betas_twospeed_alteu <- lapply( twospeed_mod_alteu, coef)
vars_twospeed_alteu <- lapply(twospeed_mod_alteu, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_alteu_twospeed <- extract.df(twospeed_mod_alteu, cl = datlist_EUI[[1]]$country)

####Test texreg##
htmlreg(list(texreg_alteu_optout, texreg_alteu_twospeed), custom.coef.map = coef_map, 
        caption.above = TRUE, 
        custom.model.names = c("Constitutional DI <br> (full model)",
                               "Instrumental DI <br> (full model)"))

##S3
#Table created via Word's table function, using raw code output from sensemakr's summary function
library(sensemakr)

#Instrumental DI
fixeff_twospeed_lm <- lm(two_speed_supp ~ market_europe +
                           alternative_econ + sat_nat_dem + left_right + eurosceptic +
                           left_right_sqr + gender + age + income_scale + 
                           excl_identity + factor(country), weights = weight, 
                         data = EUI_imputed_10)

sensemakr_twospeed_new <- sensemakr(fixeff_twospeed_lm, 
                                    treatment = "excl_identity1", 
                                    kd = 1:3)

##S4
#Constitutional DI
optout_mod_eu_fixeff <- lm(opt_out_supp ~ market_europe +
                             alternative_econ + sat_nat_dem + left_right + eurosceptic +
                             left_right_sqr + gender + age + income_scale + 
                             excl_identity + factor(country), weights = weight, 
                           data = EUI_imputed_10)

sensemakr_optout_new <- sensemakr::sensemakr(optout_mod_eu_fixeff, 
                                             treatment = "excl_identity1", 
                                             kd = 1:3)

##S5
#As for above, table is created via Word's table function
economy_index_EUI <- EUI_merged_new %>%
  dplyr::select(personal_financial, economic_situation_nation,
                employment_area, income_scale) %>% na.omit()

psych::fa(economy_index_EUI, nfactors = 2)

##S6
eurozone_mod_noeu_polr <- with(datlist_EUI, polr(factor(euro_supp) ~ market_europe +
                                                   alternative_econ + sat_nat_dem + left_right +
                                                   left_right_sqr + gender + age + income_scale + 
                                                   excl_identity + factor(country), weights = weight, 
                                                 Hess = TRUE),
                               (clval <- sandwich::vcovCL(optout_mod_noeu_polr, datlist_EUI[[1]]$country)))

pooled_polr_euronoeu <-  mice::pool(eurozone_mod_noeu_polr)


eurozone_mod_eu_polr <- with(datlist_EUI, polr(factor(euro_supp) ~ market_europe +
                                                 alternative_econ + sat_nat_dem + left_right +
                                                 left_right_sqr + gender + age + income_scale + eurosceptic + 
                                                 excl_identity + factor(country), weights = weight, 
                                               Hess = TRUE),
                             (clval <- sandwich::vcovCL(optout_mod_polr, datlist_EUI[[1]]$country)))

pooled_polr_euroeu <-  mice::pool(eurozone_mod_eu_polr)

##Create modelsummary
modelsummary::modelsummary(list(pooled_polr_euronoeu, pooled_polr_euroeu), coef_map = coef_map_interact,
                           coef_omit = "country", output = "html", stars = TRUE, fmt = 2)


##S7
eurosupp_noeu <- with(datlist_EUI, lm(euro_supp ~ market_europe +
                                        alternative_econ + sat_nat_dem + left_right +
                                        left_right_sqr + gender + age + income_scale + 
                                        excl_identity + factor(country), weights = weight))

betas_eurosupp_noeu <- lapply(eurosupp_noeu, coef)
vars_eurosupp_noeu <- lapply(eurosupp_noeu, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)}) #vcovCL is in the sandwich package

texreg_eurosuppnoeu <- extract.df(eurosupp_noeu, cl = datlist_EUI[[1]]$country)

eurosupp_eu <- with(datlist_EUI, lm(euro_supp ~ market_europe + eurosceptic +
                                      alternative_econ + sat_nat_dem + left_right +
                                      left_right_sqr + gender + age + income_scale + 
                                      excl_identity + factor(country), weights = weight))

betas_eurosupp <- lapply(eurosupp_eu, coef)
vars_eurosupp <- lapply(eurosupp_eu, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)}) #vcovCL is in the sandwich package

texreg_eurosuppeu <- extract.df(eurosupp_eu, cl = datlist_EUI[[1]]$country)

#Create table S7
htmlreg(list(texreg_eurosuppnoeu, texreg_eurosuppeu), custom.coef.map = coef_map, 
        caption.above = TRUE, single.row = TRUE,
        caption = "Table 2: Model showing correlation between national identity and support for Eurozone integration.
        Cluster-robust SEs (country-level) and country fixed effects", 
        custom.model.names = c("Eurozone integration <br> (no EU variable)",
                               "Eurozone integration <br> (full model)"))


##S8
library(naniar)
missing_data <- EUI_merged_new %>%
  dplyr::select(opt_out_supp, two_speed_supp, market_europe, alternative_econ, sat_nat_dem, left_right,
                eurosceptic, gender, age, income_scale, eu_democ_satisf, nordic_dummy, CEE_dummy,
                excl_identity, country)

missing_data <- missing_data %>%
  rename(`Support constitutional DI` = opt_out_supp) %>%
  rename(`Support instrumental DI` = two_speed_supp) %>%
  rename(`Liberal economic values` = market_europe) %>%
  rename(`Perception of economy` = alternative_econ) %>%
  rename(`Satisfaction with national democracy` = sat_nat_dem) %>%
  rename(`Satisfaction with EU democracy` = eu_democ_satisf) %>%
  rename(`Age` = age) %>%
  rename(`Income` = income_scale) %>%
  rename(`Left-right` = left_right) %>%
  rename(`Eurosceptic` = eurosceptic) %>%
  rename(`Gender` = gender) %>%
  rename(`Exclusively national identity` = excl_identity) %>%
  rename(`Nordics` = nordic_dummy) %>%
  rename(`CEE` = CEE_dummy) %>%
  rename(`Country` = country)

missing_data_country <- missing_data %>%
  dplyr::filter(Country >= 2)

missing_data_country$Country_fct <- factor(missing_data_country$Country)

#Create figure S8
miss_var_cntry <- gg_miss_fct(missing_data_country, fct = Country_fct) +
  scale_x_discrete(breaks = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14), 
                   labels = c("DK", "FI", "FR", "DE", "SE", "GR", "HU", 
                              "IT", "LT", "NL", "PL", "RO", "ES")) + 
  labs(x = "Country", title = "Missing data analysis", 
       subtitle = "Missingness as function of country")

##S9
#Opt-outs
optout_bivariate <- with(datlist_EUI, lm(opt_out_supp ~ 
                                           excl_identity + factor(country), weights = weight))


betas_optout_bivariate <- lapply( optout_mod_eu, coef)
vars_optout_bivariate <- lapply(optout_mod_eu, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_optout_bivariate <- extract.df(optout_bivariate, cl = datlist_EUI[[1]]$country)

#Two-speed
twospeed_bivariate <- with(datlist_EUI, lm(two_speed_supp ~ 
                                             excl_identity + factor(country), weights = weight))


betas_twospeed_bivariate <- lapply( twospeed_bivariate, coef)
vars_twospeed_bivariate <- lapply(twospeed_bivariate, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_twospeed_bivariate <- extract.df(twospeed_bivariate, cl = datlist_EUI[[1]]$country)

#Create table S9
htmlreg(list(texreg_optout_bivariate, texreg_2, texreg_twospeed_bivariate, texreg_4), custom.coef.map = coef_map, 
        caption.above = TRUE, 
        caption = NULL,
        custom.model.names = c("Constitutional DI <br> (bivariate model)",
                               "Constitutional DI <br> (full model)",
                               "Instrumental DI <br> (bivariate model)",
                               "Instrumental DI <br> (full model)"), 
        single.row = TRUE)


##S10
#Table created via Word's table function
fixeff_twospeed_lm_noeu <- lm(two_speed_supp ~ market_europe +
                                alternative_econ + sat_nat_dem + left_right +
                                left_right_sqr + gender + age + income_scale + 
                                excl_identity + factor(country), weights = weight, 
                              data = EUI_imputed_10)

summary(fixeff_twospeed_lm_noeu)

#Test sensemakr
sensemakr_twospeed_noeu_new <- sensemakr(fixeff_twospeed_lm_noeu, 
                                         treatment = "excl_identity1", 
                                         kd = 1:3)


##S11
#Table created via Word's table function
#Opt-outs
optout_mod_eu_fixeff <- lm(opt_out_supp ~ market_europe +
                             alternative_econ + sat_nat_dem + left_right + eurosceptic +
                             left_right_sqr + gender + age + income_scale + 
                             excl_identity + factor(country), weights = weight, 
                           data = EUI_imputed_10)

sensemakr_optout_new <- sensemakr::sensemakr(optout_mod_eu_fixeff, 
                                             treatment = "excl_identity1", 
                                             kd = 1:3)

##S12
#Opt-outs
optout_noatt <- with(datlist_EUI, lm(opt_out_supp ~ age + gender + income_scale +
                                       excl_identity + factor(country), weights = weight))


betas_optout_noatt <- lapply( optout_noatt, coef)
vars_optout_noatt <- lapply(optout_noatt, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_optout_noatt <- extract.df(optout_noatt, cl = datlist_EUI[[1]]$country)

#Two-speed
twospeed_noatt <- with(datlist_EUI, lm(two_speed_supp ~ age + gender + income_scale +
                                         excl_identity + factor(country), weights = weight))


betas_twospeed_noatt <- lapply(twospeed_noatt, coef)
vars_twospeed_noatt <- lapply(twospeed_noatt, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_twospeed_noatt <- extract.df(twospeed_noatt, cl = datlist_EUI[[1]]$country)

htmlreg(list(texreg_optout_noatt, texreg_2, texreg_twospeed_noatt, texreg_4), custom.coef.map = coef_map, 
        caption.above = TRUE, 
        caption = "Table 2: Model showing correlation between national identity and support for differentiated integration.
        Cluster-robust SEs (country-level) and country fixed effects", 
        custom.model.names = c("Constitutional DI <br> (no attitudinal variables)",
                               "Constitutional DI <br> (full model)",
                               "Instrumental DI <br> (no attitudinal variables)",
                               "Instrumental DI <br> (full model)"), 
        single.row = TRUE)

##S13
#Test both together
optout_mod_euroscepticextra <- with(datlist_EUI, lm(opt_out_supp ~ market_europe +
                                                      alternative_econ + sat_nat_dem + left_right + eu_democ_satisf +
                                                      eurosceptic + left_right_sqr + gender + age + income_scale + 
                                                      excl_identity + factor(country), weights = weight))


betas_optout_eualtadd <- lapply( optout_mod_euroscepticextra, coef)
vars_optout_eualtadd <- lapply(optout_mod_euroscepticextra, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_alternativeEUadd <- extract.df(optout_mod_euroscepticextra, cl = datlist_EUI[[1]]$country)

#####Repeat same process for two speed#####
twospeed_mod_alternativeEU <- with(datlist_EUI, lm(two_speed_supp ~ market_europe +
                                                     alternative_econ + sat_nat_dem + left_right + eu_democ_satisf +
                                                     left_right_sqr + gender + age + income_scale + 
                                                     excl_identity + factor(country), weights = weight))


betas_twospeed_eualt <- lapply( twospeed_mod_alternativeEU, coef)
vars_twospeed_eualt <- lapply(twospeed_mod_alternativeEU, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_alternativeEU_twospeed <- extract.df(twospeed_mod_alternativeEU, cl = datlist_EUI[[1]]$country)

#Test both together
twospeed_mod_euroscepticextra <- with(datlist_EUI, lm(two_speed_supp ~ market_europe +
                                                        alternative_econ + sat_nat_dem + left_right + eurosceptic +
                                                        eu_democ_satisf +
                                                        left_right_sqr + gender + age + income_scale + 
                                                        excl_identity + factor(country), weights = weight))


betas_twospeed_eualtadd <- lapply( twospeed_mod_euroscepticextra, coef)
vars_twospeed_eualtadd <- lapply(twospeed_mod_euroscepticextra, FUN = function(x){vcovCL(x, cluster = datlist_EUI[[1]]$country)})

texreg_alternativeEUadd_twospeed <- extract.df(twospeed_mod_euroscepticextra, cl = datlist_EUI[[1]]$country)

htmlreg(list(texreg_2, texreg_alternativeEUadd, texreg_4, texreg_alternativeEUadd_twospeed), custom.coef.map = coef_map, 
        caption.above = TRUE, 
        caption = NULL,
        custom.model.names = c("Constitutional DI <br> (original model)",
                               "Constitutional DI <br> (model with EU democracy satisfaction)",
                               "Instrumental DI <br> (original model)",
                               "Instrumental DI <br> (model with EU democracy satisfaction)"), 
        single.row = TRUE)

##S14
data_descriptives <- EUI_merged_new %>%
  dplyr::select(opt_out_supp, two_speed_supp, market_europe, alternative_econ, 
                sat_nat_dem, left_right, age, income_scale,
                eurosceptic_num, left_right_sqr, gender_num, 
                excl_num, nordic_num, CEE_num)

data_descriptives <- data_descriptives %>%
  rename(`Support constitutional DI` = opt_out_supp) %>%
  rename(`Support instrumental DI` = two_speed_supp) %>%
  rename(`Nordics` = nordic_num) %>%
  rename(`Central and Eastern Europe` = CEE_num) %>%
  rename(`Liberal economic values` = market_europe) %>%
  rename(`Perception of economy` = alternative_econ) %>%
  rename(`Trust national democracy` = sat_nat_dem) %>%
  rename(`Left-right` = left_right) %>%
  rename(`Left-right (sqr.)` = left_right_sqr) %>%
  rename(`Eurosceptic` = eurosceptic_num) %>%
  rename(`Gender` = gender_num) %>%
  rename(`Age` = age) %>%
  rename(`Income` = income_scale) %>%
  rename(`Exclusive identity` = excl_num)

modelsummary::datasummary_skim(data = data_descriptives, output = "html", histogram = FALSE,
                               fmt = 2)

##S15
gg_miss_var(missing_data, show_pct = TRUE) + labs(title = "Missing data analysis",
                                                  subtitle = "% of missing per variable")

##S16
#First run model
set.seed(4948)
optout_interact_new <- with(datlist_EUI_new, lmer(opt_out_supp ~ market_europe +
                                                    alternative_econ + sat_nat_dem + left_right +
                                                    left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                    excl_identity + factor(region_dummy) +
                                                    excl_identity*factor(region_dummy) + 
                                                    (1|country),
                                                  weights = weight))
pool_interact_new <- mice::pool(optout_interact_new)

twospeed_interact_new <- with(datlist_EUI_new, lmer(two_speed_supp ~ market_europe +
                                                      alternative_econ + sat_nat_dem + left_right +
                                                      left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                      excl_identity + factor(region_dummy) +
                                                      excl_identity*factor(region_dummy) + 
                                                      (1|country),
                                                    weights = weight))
pool_interact_twospeed_new <- mice::pool(twospeed_interact_new)

##Create table S16
coef_map_interact_new <- c("excl_identity1:factor(region_dummy)1" = "Region dummy X Exclusively national identity",
                           "factor(region_dummy)1" = "Region dummy <br> (ref.category: CEE)",
                           "excl_identity1" = "Exclusively national identity",
                           'market_europe' = 'Liberal economic values',
                           'alternative_econ' = "Perception of economy", 
                           "left_right" = "Left-right",
                           "age" = "Age",
                           "sat_nat_dem" = "Satisfaction w/ national democracy", 
                           "eu_democ_satisf" = "Satisfaction with EU democracy",
                           "eurosceptic1" = "Eurosceptic", 
                           "left_right_sqr" = "Left-right (sqr.)", 
                           "gender" = "Gender",
                           "income_scale" = "Income",
                           "eurosceptic1:excl_identity1" = "Exclusively national identity X Euroscepticism")

modelsummary::modelsummary(list(pool_interact_new, pool_interact_twospeed_new), coef_map = coef_map_interact_new, 
                           output = "html", stars = TRUE, fmt = 2)

EUI_no_nordic <- EUI_merged_new %>%
  dplyr::filter(! nordic_dummy == 1)

#Impute
EUI_impute_nonordic <- mice(EUI_no_nordic, m = 25)
datlist_EUI_nonordic <- miceadds::mids2datlist(EUI_impute_nonordic)

##S17
#Create CEE models
set.seed(465)
optout_CEE_support <- with(datlist_EUI_nonordic, lmer(opt_out_supp ~ market_europe +
                                                        alternative_econ + sat_nat_dem + left_right +
                                                        left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                        excl_identity + CEE_dummy +
                                                        excl_identity*CEE_dummy + 
                                                        (1|country),
                                                      weights = weight))
pool_CEE_new <- mice::pool(optout_CEE_support)

#Repeat for two-speed
set.seed(469)
twospeed_CEE_support <- with(datlist_EUI_nonordic, lmer(two_speed_supp ~ market_europe +
                                                          alternative_econ + sat_nat_dem + left_right +
                                                          left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                          excl_identity + CEE_dummy +
                                                          excl_identity*CEE_dummy + 
                                                          (1|country),
                                                        weights = weight))
pool_CEE_twospeed <- mice::pool(twospeed_CEE_support)

#Repeat for Nordic data
EUI_no_CEE <- EUI_merged_new %>%
  dplyr::filter(! CEE_dummy == 1)

#Impute
EUI_impute_noCEE<- mice(EUI_no_CEE, m = 25)
datlist_EUI_noCEE <- miceadds::mids2datlist(EUI_impute_noCEE)

#Run models
#Start with opt-outs
set.seed(987)
optout_nordic_support <- with(datlist_EUI_noCEE, lmer(opt_out_supp ~ market_europe +
                                                        alternative_econ + sat_nat_dem + left_right +
                                                        left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                        excl_identity + nordic_dummy +
                                                        excl_identity*nordic_dummy + 
                                                        (1|country),
                                                      weights = weight))

pool_nordic_new <- mice::pool(optout_nordic_support)

#Repeat for two-speed
set.seed(489)
twospeed_nordic_support <- with(datlist_EUI_noCEE, lmer(two_speed_supp ~ market_europe +
                                                          alternative_econ + sat_nat_dem + left_right +
                                                          left_right_sqr + eurosceptic + gender + age + income_scale + 
                                                          excl_identity + nordic_dummy +
                                                          excl_identity*nordic_dummy + 
                                                          (1|country),
                                                        weights = weight))
summary(pool(twospeed_nordic_support))

pool_nordic_twospeed <- mice::pool(twospeed_nordic_support)

#Coefmap and table
coef_map_interact_impute <- c("excl_identity1:nordic_dummy" = "Nordic dummy X Exclusively national identity",
                              "excl_identity1:CEE_dummy" = "CEE dummy X Exclusively national identity",
                              "nordic_dummy" = "Nordic",
                              "CEE_dummy" = "CEE",
                              "excl_identity1" = "Exclusively national identity",
                              'market_europe' = 'Liberal economic values',
                              'alternative_econ' = "Perception of economy", 
                              "left_right" = "Left-right",
                              "age" = "Age",
                              "sat_nat_dem" = "Satisfaction w/ national democracy", 
                              "eu_democ_satisf" = "Satisfaction with EU democracy",
                              "eurosceptic1" = "Eurosceptic", 
                              "left_right_sqr" = "Left-right (sqr.)", 
                              "gender" = "Gender",
                              "income_scale" = "Income",
                              "eurosceptic1:excl_identity1" = "Exclusively national identity X Euroscepticism")

modelsummary::modelsummary(list(pool_CEE_new, pool_CEE_twospeed, 
                                pool_nordic_new, pool_nordic_twospeed), 
                           coef_map = coef_map_interact_impute, 
                           output = "html", stars = TRUE, fmt = 2)


