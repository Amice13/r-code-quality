## The Representational Consequences
## of Electronic Voting Reform

## Replication Data for: 
## ch 6 :: Effects on Parties

## Santiago Alles
## Tiffany D. Barnes
## Carolina Tchintian

## Script by: Santiago Alles
## updated: 04/06/2023



rm(list = ls())



## Load
## data

rep_data <- haven::read_dta()                        ## load "ABT 2023-Replication Data. Ch5.dta"



## Model --------------------------
## Estimation


for (i in unique(rep_data$year)) {
  
  
  ## Loop ------------
  ## Data
  
  # filter data
  rep_data %>% 
    filter( match_level %in% 'strict') %>% 
    filter(year %in% i) %>% 
    filter(!is.na(weights)) -> loop_data
  
  # Add dummies
  tmp <- with(loop_data, fastDummies::dummy_columns(gov_party_n, ignore_na = T) ) %>% as_tibble() %>% unique()
  colnames(tmp) <- c('gov_party_n', gsub( pattern = '.data', replacement = 'gov_party', x = colnames(tmp)[2:ncol(tmp)] ))
  
  loop_data <- loop_data %>% left_join(tmp, by = "gov_party_n")
  
  # Add interaction terms
  tmp_int <- loop_data %>% select(all_of(colnames(tmp))[-1]) * loop_data$treatment
  tmp_int <- as_tibble(tmp_int)
  
  colnames(tmp_int) <- paste(colnames(tmp)[-1], 'treatment', sep = '_')
  
  loop_data %>%
    data.frame(., tmp_int) %>% 
    as_tibble() -> loop_data
  
  
  ## Model ------------
  ## Equations
  
  # (1) DVs
  dv_1 <- 'provH_perf'
  
  # (2) IVs
  iv_1 <- 'treatment'
  iv_1 <- paste(iv_1, paste(colnames(tmp)[-c(1:2)], collapse = ' + '), sep = ' + ')
  
  # (3) interactions
  int_1 <- paste(colnames(tmp_int)[-1], collapse = ' + ')
  
  # (4) equations
  mod_eq_1 <- paste(dv_1, paste(iv_1, sep = ' + '), sep = ' ~ ')
  mod_eq_2 <- paste(dv_1, paste(iv_1, int_1, sep = ' + '), sep = ' ~ ')
  
  mod_eq_1 <- as.formula(mod_eq_1)
  mod_eq_2 <- as.formula(mod_eq_2)
  
  
  ## Model ------------
  ## Estimation
  
  mod_1 <- lm(mod_eq_1, data = loop_data)
  mod_2 <- lm(mod_eq_2, data = loop_data)
  
  mod_1_cl.se <- with(loop_data, lmtest::coeftest(mod_1, multiwayvcov::cluster.vcov(mod_1, precinct ) ))
  mod_2_cl.se <- with(loop_data, lmtest::coeftest(mod_2, multiwayvcov::cluster.vcov(mod_2, precinct ) ))
  
  
  ## Model ------------
  ## Prediction
  
  # model
  pred_mod <- mod_2
  
  # Posterior simulation
  arm::sim(pred_mod, n.sims = 50000 ) -> mod_sim
  
  # model vars
  all.vars(formula(pred_mod))[-1] -> mod_vars
  
  # frame
  loop_data %>% 
    select( all_of(c('gov_party', 'gov_party_n', 
                     all.vars(mod_eq_2)[-1])) ) %>% 
    unique() -> pred

  # sim coefficients
  pred %>%
    select(all_of(mod_vars)) %>% 
    data.frame(1 ,.) %>% 
    as_tibble() -> X
  
  b <- as.matrix(mod_sim@coef)
  Xb <- t(as.matrix(X) %*% t(b))
  
  ## predictions
  data.frame( apply(Xb, 2, mean), 
              t(apply( Xb, 2, quantile, 
                       probs=c(.025, .05, .08, .92, .95, .975))) 
              ) -> store
  colnames(store) <- c("fit", "ci.lo.95", "ci.lo.90", "ci.lo.84", "ci.up.84", "ci.up.90", "ci.up.95")
  
  pred %>% 
    select(gov_party, gov_party_n, treatment) %>% 
    mutate( year = i) %>% 
    mutate( match_level = 'strict') %>% 
    data.frame(., store) %>% 
    as_tibble() -> pred
  
  
  # store prediction
  if('pred_store' %in% ls()) pred_store <- as_tibble(rbind(pred_store, pred)) else pred -> pred_store
  
  
  rm( X, b, Xb, store,
      mod_vars, pred,
      mod_sim)
  
  rm(pred_mod)
  
  
  ## Print ------------
  ## Results
  
  ## table title/number
  tab_title <- "Relative Performance of Provincial House's Tickets: Capital Department, 2007-2015"
  tab_n <- "Table II-6-1"
  
  stargazer::stargazer( mod_1_cl.se, mod_2_cl.se, 
                        title = paste( paste(tab_n, '.', sep = ''),
                                       paste(tab_title, '.', sep = ''), 
                                       "OLS with standard errors clustered by precinct"),
                        dep.var.labels = rep('Prov H', 2),
                        type = "text",
                        no.space = T,
                        digits = 4, star.cutoffs = c(.05, .01, .001),
                        column.labels = rep(as.character(i), 2), 
                        model.numbers = T, model.names = F, object.names = F,
                        omit.stat=c("f", "ser") )
  
  
  
  rm( dv_1, iv_1, int_1, 
      mod_eq_1, mod_eq_2,
      tmp, tmp_int,
      loop_data,
      mod_1, mod_2,
      mod_1_cl.se, mod_2_cl.se, 
      tab_title, tab_n)
  
  
}

rm(i )
