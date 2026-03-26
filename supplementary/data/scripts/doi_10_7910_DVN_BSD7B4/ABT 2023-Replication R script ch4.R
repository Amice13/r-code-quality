## The Representational Consequences
## of Electronic Voting Reform

## Replication Data for: 
## ch 4 :: Effects on Candidates

## Santiago Alles
## Tiffany D. Barnes
## Carolina Tchintian

## Script by: Santiago Alles
## updated: 04/06/2023



rm(list = ls())



## Load
## data

rep_data_1 <- haven::read_dta()                        ## load "ABT 2023-Replication Data. Ch4-1.dta"
rep_data_2 <- haven::read_dta()                        ## load "ABT 2023-Replication Data. Ch4-2.dta"



## Model --------------------------
## Data

# municipality data
rep_data_1 %>%
  mutate( incumbent_electronic = incumbent * electronic ) %>% 
  mutate( provLegis_electronic = prov_legislator * electronic ) %>% 
  mutate( gender_electronic = gender * electronic ) %>% 
  filter( votes_pct > 0) -> mod_dat

# precinct data
rep_data_2 %>% 
  filter( match_level %in% 'strict') %>% 
  filter( year %in% 2007:2011) %>% 
  filter(!is.na(incumbent)) %>%
  filter( incumbent == 1) %>% 
  filter(!is.na(weights)) -> pct_mod_dat

tmp <- with(pct_mod_dat, fastDummies::dummy_columns(year, ignore_na = T) )
tmp <- tmp[2:ncol(tmp)]
colnames(tmp) <- c('no_imp', 'partial')

data.frame( pct_mod_dat, tmp) %>% 
  mutate( partial_treatment = partial * treatment ) %>% 
  mutate( mayor_perf = votes_pct - gov_votes) %>%
  as_tibble() -> pct_mod_dat

rm(tmp)


# model store
mod_1 <- mod_2 <- mod_3 <- mod_4 <- mod_5 <- list()



## Incumbency --------------------------
## Models

mod_1[['lm_1']] <- lm( votes_pct ~ incumbent + prov_legislator + incumbent_gov + mayor_count, 
                       data = mod_dat %>% 
                         filter(!is.na(electronic)) )

mod_1[['lm_2']] <- lm( votes_pct ~ incumbent + prov_legislator + incumbent_gov + mayor_count, 
                        data = mod_dat %>% 
                          filter(!is.na(electronic)) %>% 
                          filter( candidates_n == 1) )

mod_3[['lm_1']] <- lm( mayor_perf ~ incumbent + prov_legislator + electronic + incumbent_gov + 
                         mayor_count + incumbent_electronic + provLegis_electronic, 
                       data = mod_dat %>% 
                         filter(!is.na(electronic)) )

mod_3[['lm_2']] <- lm( mayor_perf ~ incumbent + prov_legislator + electronic + incumbent_gov + 
                         mayor_count + incumbent_electronic + provLegis_electronic, 
                       data = mod_dat %>% 
                         filter(!is.na(electronic)) %>% 
                         filter( candidates_n == 1) )



## Women --------------------------
## Models

mod_2[['lm_1']] <- lm( votes_pct ~ gender + incumbent_gov + mayor_count + gender_count, 
                       data = mod_dat %>% 
                         filter(!is.na(electronic)) )

mod_2[['lm_2']] <- lm( votes_pct ~ gender + incumbent_gov + mayor_count + gender_count, 
                       data = mod_dat %>% 
                         filter(!is.na(electronic)) %>% 
                         filter( candidates_n == 1) )

mod_4[['lm_1']] <- lm( mayor_perf ~ gender + electronic + incumbent_gov + mayor_count + 
                         gender_count + gender_electronic, 
                       data = mod_dat %>% 
                         filter(!is.na(electronic)) )

mod_4[['lm_2']] <- lm( mayor_perf ~ gender + electronic + incumbent_gov + mayor_count + 
                         gender_count + gender_electronic, 
                       data = mod_dat %>% 
                         filter(!is.na(electronic)) %>% 
                         filter( candidates_n == 1) )


## Precinct-level --------------------------
## Models

mod_5[['lm_1']] <- lm( mayor_perf ~ partial + treatment + 
                         partial_treatment, 
                       data = pct_mod_dat, 
                       weight = weights )

mod_5[['lm_2']] <- lm( mayor_perf ~ partial + treatment + 
                         urtubey_votes + complete_HS_or_higher + NBI + 
                         partial_treatment, 
                       data = pct_mod_dat, 
                       weight = weights )



## Model --------------------------
## Results

stargazer::stargazer( mod_1, 
                      title = "Table II-4-1. The Influence of Experience: Electoral Support of Mayoral Candidates (Province of Salta, 2007-2019)",
                      dep.var.labels = "Candidate's Votes (%)",
                      type = "text",
                      no.space = T,
                      digits = 4, star.cutoffs = c( .05, .01, .001),
                      column.labels = c('All', 'Single') , 
                      model.numbers = T,
                      model.names = F,
                      object.names = F,
                      omit.stat=c("f", "ser")
                      )

stargazer::stargazer( mod_2, 
                      title = "Table II-4-2. The Influence of Gender: Electoral Support of Mayoral Candidates (Province of Salta, 2007-2019)",
                      dep.var.labels = "Candidate's Votes (%)",
                      type = "text",
                      no.space = T,
                      digits = 4, star.cutoffs = c( .05, .01, .001),
                      column.labels = c('All', 'Single') , 
                      model.numbers = T,
                      model.names = F,
                      object.names = F,
                      omit.stat=c("f", "ser")
                      )

stargazer::stargazer( mod_3, 
                      title = "Table II-4-3. The Influence of Experience: Mayor v. Governor (%) (Province of Salta, 2007-2019)",
                      dep.var.labels = "Mayor Performance (%)",
                      type = "text",
                      no.space = T,
                      digits = 4, star.cutoffs = c( .05, .01, .001),
                      column.labels = c('All', 'Single') , 
                      model.numbers = T,
                      model.names = F,
                      object.names = F,
                      omit.stat=c("f", "ser")
                      )

stargazer::stargazer( mod_4, 
                      title = "Table II-4-5. The Influence of Gender: Mayor v. Governor (%) (Province of Salta, 2007-2019)",
                      dep.var.labels = "Mayor Performance (%)",
                      type = "text",
                      no.space = T,
                      digits = 4, star.cutoffs = c( .05, .01, .001),
                      column.labels = c('All', 'Single') , 
                      model.numbers = T,
                      model.names = F,
                      object.names = F,
                      omit.stat=c("f", "ser")
                      )

stargazer::stargazer( mod_5, 
                      title = "Table II-4-4. A Micro-level Examination of an Incumbent Candidate: Miguel Isa (City of Salta, 2007 and 2011)",
                      dep.var.labels = "Mayor Performance (%)",
                      type = "text",
                      no.space = T,
                      digits = 4, star.cutoffs = c( .05, .01, .001),
                      column.labels = c('Model #1',	'Model #2') , 
                      model.numbers = T,
                      model.names = F,
                      object.names = F,
                      omit.stat=c("f", "ser")
                      )


## Model --------------------------
## Prediction (i)

pred_mod <- mod_3[['lm_1']]

# Posterior simulation
pred_mod %>% arm::sim(., n.sims = 50000 ) -> mod_sim

# frame
data.frame( incumbent = c(1, 0, 0), prov_legislator = c(0, 1, 0)) %>%
  bind_rows( data.frame( ., electronic = 0),
             data.frame( ., electronic = 1)) %>%
  filter(!is.na(electronic) ) %>%
  mutate( incumbent_electronic = incumbent * electronic ) %>%
  mutate( provLegis_electronic = prov_legislator * electronic ) %>%
  mutate( incumbent_gov = 0 ) %>%
  mutate( mayor_count = with( pred_mod %>% model.frame(),
                              median(mayor_count)) ) %>% 
  select(all_of(all.vars(formula(pred_mod))[-1])) %>% 
  data.frame( 1, .) %>% 
  as_tibble() -> X

# labels
treat_lab = with(X, ifelse(electronic == 1, "Electronic Voting", 'Paper Ballot'))
experience_lab = with(X, ifelse(incumbent == 1, "Incumbent Mayor", ifelse(prov_legislator == 1, "Prov. Legislator", 'Other')))

# sim coefficients
b <- as.matrix(mod_sim@coef)
Xb <- t(as.matrix(X) %*% t(b))

# predictions
data.frame( apply(Xb, 2, mean), t(apply(Xb, 2, quantile, probs=c(.025,.05,.95,.975))) ) -> pred
colnames(pred) <- c("fit", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")

data.frame( experience_lab,
            treat_lab,
            X %>% select(incumbent, prov_legislator, electronic),
            pred ) %>%
  rename( experience = experience_lab) %>% 
  rename( treatment = treat_lab) %>% 
  as_tibble() -> pred


# save table
cat( paste('Figure 12: Expected candidate performance, relative to the gubernatorial candidate (p.48)',
           "--------------", 
           'Model Predictions based on Table II-4-3 (in Appendix)', 
           'Ordinary Least Squares (OLS). DV: Mayoral Candidate Performance (%)', 
           'Based on Data from Province of Salta, 2007–2019', 
           sep = '\n'),
     '',
     rep('', 2),
     knitr::kable( pred , digits = 4, ), 
     sep = "\n")



rm( experience_lab, treat_lab, 
    Xb, X, b,
    mod_vars,
    mod_sim, pred_mod,
    pred )




## Model --------------------------
## Prediction (ii)

pred_mod <- mod_4[['lm_1']]

# Posterior simulation
pred_mod %>% arm::sim(., n.sims = 50000 ) -> mod_sim


# frame
data.frame( gender = c(0, 0, 1, 1), electronic = c(0, 1, 0, 1)) %>%
  mutate( gender_electronic = gender * electronic ) %>%
  mutate( incumbent_gov = 0 ) %>%
  mutate( gender_count = 1 ) %>%
  mutate( mayor_count = with( pred_mod %>% model.frame(),
                               median(mayor_count)) ) %>% 
  select(all_of(all.vars(formula(pred_mod))[-1])) %>% 
  data.frame( 1, .) %>% 
  as_tibble() -> X

# labels
treat_lab = with(X, ifelse(electronic == 1, "Electronic Voting", 'Paper Ballot'))
gender_lab = with(X, ifelse(gender == 1, "Women", 'Men'))

# sim coefficients
b <- as.matrix(mod_sim@coef)
Xb <- t(as.matrix(X) %*% t(b))

# predictions
data.frame( apply(Xb, 2, mean), t(apply(Xb, 2, quantile, probs=c(.025,.05,.95,.975))) ) -> pred
colnames(pred) <- c("fit", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")

data.frame( gender_lab,
            treat_lab,
            X %>% select(gender, electronic),
            pred) %>%
  rename( candidate = gender_lab) %>% 
  rename( treatment = treat_lab) %>% 
  arrange(electronic, gender) %>% 
  as_tibble() -> pred


# save table
cat( paste('Figure 14: Expected candidate performance, relative to the gubernatorial candidate (p.50)',
           "--------------", 
           'Model Predictions based on Table II-4-5 (in Appendix)', 
           'Ordinary Least Squares (OLS). DV: Mayoral Candidate Performance (%)', 
           'Based on Data from Province of Salta, 2007–2019', 
           sep = '\n'),
     '',
     rep('', 2),
     knitr::kable( pred , digits = 4, ), 
     sep = "\n")



rm( gender_lab, treat_lab, 
    Xb, X, b,
    mod_vars,
    mod_sim, pred_mod,
    pred )



## Model --------------------------
## Prediction (iii)

pred_mod <- mod_5[['lm_1']]

# Posterior simulation
arm::sim(pred_mod, n.sims = 50000 ) -> mod_sim

# frame
data.frame( partial = 0, treatment = 1, partial_treatment = c(0, 1)) %>% 
  select(all_of(all.vars(formula(pred_mod))[-1])) %>% 
  data.frame( 0, .) %>% 
  as_tibble() -> X

# labels
implementation = with(X, ifelse(partial_treatment == 1, "Partial Implementation", 'No Implementation'))
year_lab = with(X, ifelse(partial_treatment == 1, 2011, 2007))

# sim coefficients
b <- as.matrix(mod_sim@coef)
Xb <- t(as.matrix(X) %*% t(b))

# predictions
data.frame( apply(Xb, 2, mean), t(apply(Xb, 2, quantile, probs=c(.025,.05,.95,.975))) ) -> pred
colnames(pred) <- c("fit", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")

data.frame( implementation,
            matching = 'strict' ,
            year = year_lab,
            pred) %>% 
  mutate( procedure = ifelse(year == 2011, 'Partial', 'All Paper')) %>% 
  arrange(year) %>% 
  as_tibble() -> pred


# save table
cat( paste('Figure 13: Expected difference in incumbent mayor’s performance in treated precincts, relative to control precincts (p.49)',
           "--------------", 
           'Model Predictions based on Table II-4-4 (in Appendix)', 
           'Ordinary Least Squares (OLS). DV: Mayoral Candidate Performance (%)', 
           'Based on Data from City of Salta, 2007 & 2011', 
           sep = '\n'),
     '',
     rep('', 2),
     knitr::kable( pred , digits = 4, ), 
     sep = "\n")





rm( implementation, year_lab,
    Xb, X, b,
    mod_sim, pred_mod,
    pred  )

