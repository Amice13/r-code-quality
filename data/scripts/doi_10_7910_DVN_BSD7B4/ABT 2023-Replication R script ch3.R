## The Representational Consequences
## of Electronic Voting Reform

## Replication Data for: 
## ch 3 :: Effects on Voters

## Santiago Alles
## Tiffany D. Barnes
## Carolina Tchintian

## Script by: Santiago Alles
## updated: 04/06/2023


## Loading
## Packages

require(lmtest)
require(multiwayvcov)
require(sandwich)



rm(list = ls())



## Load
## data

rep_data <- haven::read_dta()                        ## load "ABT 2023-Replication Data. Ch3.dta"



### Split-Ticket --------------------------
### Voting

## Model data

rep_data %>% 
  filter( match_level %in% 'strict') %>% 
  filter(!is.na(weights)) -> mod_dat

# year dummies
tmp <- with(mod_dat, fastDummies::dummy_columns(year, ignore_na = T) )
tmp <- tmp[2:ncol(tmp)]
colnames(tmp) <- c('no_imp', 'partial', 'full')

data.frame( mod_dat, tmp) %>% as_tibble() -> mod_dat

# interaction terms
with(mod_dat, partial * treatment) -> mod_dat$partial_treatment
with(mod_dat, full * treatment) -> mod_dat$full_treatment

rm(tmp)


## Interaction model

interact_mod = lm( vote_split ~ 
                     partial + full + 
                     treatment + partial_treatment + full_treatment, 
                   data = mod_dat,  
                   weight = weights)

summary(interact_mod) -> mod


## Difference-in-Differences

# 2007 v. 2011
data.frame( control = c( mod[['coefficients']]['(Intercept)',1],
                         mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['partial',1]),
            treated = c( mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1],
                         mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1] +
                           mod[['coefficients']]['partial',1] + mod[['coefficients']]['partial_treatment',1]) ) %>% 
  as_tibble() -> tab_1

# calculate totals
tab_1[3, ] <- tab_1[2, ] - tab_1[1, ]
tab_1[ ,3] <- tab_1[ ,2] - tab_1[ ,1]

# clean table
tab_1 <- data.frame( c('No Implementation', 'Partial Implementation', 'Difference'), 
                     c(2007, 2011, '--'), round(tab_1, 6))
colnames(tab_1) <- c('device', 'year', 'control', 'treated', 'difference')

# 2011 v. 2015
data.frame( control = c( mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['partial',1], 
                         mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['full',1] ),
            treated = c( mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1] + 
                           mod[['coefficients']]['partial',1] + mod[['coefficients']]['partial_treatment',1],
                         mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1] +
                           mod[['coefficients']]['full',1] + mod[['coefficients']]['full_treatment',1] ) ) %>% 
  as_tibble() -> tab_2

# calculate totals
tab_2[3, ] <- tab_2[2, ] - tab_2[1, ]
tab_2[ ,3] <- tab_2[ ,2] - tab_2[ ,1]

# clean table
tab_2 <- data.frame( c('Partial Implementation', 'Full Implementation', 'Difference'), 
                     c(2011, 2015, '--'), round(tab_2, 6))
colnames(tab_2) <- c('device', 'year', 'control', 'treated', 'difference')


## Weighted T-Test

t.Test_FUN <- function(treat, control) {
  
  weights::wtd.t.test( x = treat$vote_split, y = control$vote_split, 
                       weight = treat$weights, weighty = control$weights,
                       samedata = F) -> test
  
  return(test)
  
}

mod_dat$group <- with( mod_dat, 
                       paste( ifelse(year == 2011, "p", ifelse(year == 2015, "f", "n")), ifelse(treatment == 1, "t", "c"), sep = ""))

data.frame( group = c(rep('Device: Control', 2), rep('Device: Treatment', 2), 'Year: 2007', 'Year: 2011', 'Year: 2015' ),
            pair  = c(rep(c('Year: 2007 vs. 2011', 'Year: 2011 vs. 2015'), 2), rep('Device: Treatment vs. Control', 3) ),
            control = c('nc', 'pc', 'nt', 'pt', 'nc', 'pc', 'fc'),
            treated = c('pc', 'fc', 'pt', 'ft', 'nt', 'pt', 'ft') ) -> t.Test_groups

for (m in 1:nrow(t.Test_groups)) {
  
  loop_test <- t.Test_FUN( mod_dat %>% filter( group %in% t.Test_groups$treated[m] ),
                           mod_dat %>% filter( group %in% t.Test_groups$control[m] ) )
  
  data.frame( group = t.Test_groups$group[m], pair = t.Test_groups$pair[m], 
              Difference = round(loop_test$additional[1], 6), 
              round(data.frame(t(loop_test$coefficients)), 6) ) -> loop_test
  
  
  if('t.test' %in% ls()) t.test <- as_tibble(rbind(t.test, loop_test)) else loop_test -> t.test
  
  rm(loop_test)
  
  
}


rm(t.Test_FUN, t.Test_groups, m)


## Model Prediction

# Model simulations
arm::sim(interact_mod, n.sims = 50000 ) -> mod_sim


# frame
X <- unique( mod_dat %>% select(all_of(all.vars(formula(interact_mod))[-1])))
X <- as_tibble(data.frame( 0, X)) %>% filter(treatment == 1)

# labels
treat_lab = with(X, ifelse(partial == 1, "Partial Implementation", ifelse(full == 1, 'Full Implementation', 'No Implementation')))
year_lab = with(X, ifelse(partial == 1, 2011, ifelse(full == 1, 2015, 2007)))
year_lab <- data.frame(year = year_lab, X %>% select(partial, full))

# corrections
X$partial <- 0
X$full <- 0

# sim coefficients
b <- as.matrix(mod_sim@coef)
Xb <- t(as.matrix(X)%*% t(b))

# predictions
data.frame( apply(Xb, 2, mean), 
            t(apply(Xb, 2, quantile, probs=c(.025,.05,.95,.975))) 
            ) -> pred

colnames(pred) <- c("fit", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")

data.frame( treat_lab,
            set = 'DDD',
            match_level = 'strict' ,
            year_lab,
            pred) %>% 
  arrange(year) %>% 
  as_tibble() -> pred


rm( treat_lab, year_lab,
    Xb, X, b,
    mod_sim )




## Print results

cat("Table II-3-3. Difference-in-Differences (DDD)",
    "===============================",
    'Dependent Variable: Split-Ticket Voting',
    'Matching Method: Coarsened Exact Matching',
    'Matching level: Strict',
    'Years: 2007 v. 2011 v. 2015',
    'Province: Salta',
    'Department: Capital',
    "\n",
    knitr::kable(tab_1, row.names = F), '',
    knitr::kable(tab_2, row.names = F), 
    "\n",
    "Weighted Student's t-Test",
    "-------------------------", '',
    knitr::kable(t.test, row.names = F),
    '\n',
    "Interaction model",
    "-----------------", '',
    capture.output( arm::display(interact_mod, digits = 4, detail = T)),
    sep = "\n",
    append = F)


cat( paste('Figure 7: Predicted vote-splitting difference by treatment group (p.35)',
           "--------------", 
           'Model Predictions based on Table II-3-3 (in Appendix)', 
           'Difference-in-Differences (DDD). DV: Split-Ticket Voting (%)', 
           'Based on Data from the Capital Department, 2007–2015', 
           sep = '\n'), '',
     rep('', 2),
     knitr::kable( pred, digits = 4 ), 
     sep = "\n" )


rm( tab_1, tab_2, t.test, 
    pred, 
    interact_mod, mod,
    mod_dat)





## Residual --------------------------
## Votes

## Model data

rep_data %>% 
  filter( match_level %in% 'strict') %>% 
  filter(!is.na(weights)) -> mod_dat

# year dummies
tmp <- with(mod_dat, fastDummies::dummy_columns(year, ignore_na = T) )
tmp <- tmp[2:ncol(tmp)]
colnames(tmp) <- c('no_imp', 'partial', 'full')

data.frame( mod_dat, tmp) %>% as_tibble() -> mod_dat

# interaction terms
with(mod_dat, partial * treatment) -> mod_dat$partial_treatment
with(mod_dat, full * treatment) -> mod_dat$full_treatment

rm(tmp)



## Interaction Model

interact_mod = lm( gubernatorial_resVote ~ 
                     partial + full +
                     treatment + partial_treatment + full_treatment, 
                   data = mod_dat,  
                   weight = weights)

summary(interact_mod) -> mod


## Difference-in-Differences

## 2007 v. 2011
data.frame( control = c( mod[['coefficients']]['(Intercept)',1],
                         mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['partial',1]),
            treated = c( mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1],
                         mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1] +
                           mod[['coefficients']]['partial',1] + mod[['coefficients']]['partial_treatment',1]) ) %>% 
  as_tibble() -> tab_1

# calculate totals
tab_1[3, ] <- tab_1[2, ] - tab_1[1, ]
tab_1[ ,3] <- tab_1[ ,2] - tab_1[ ,1]

# clean table
tab_1 <- data.frame( c('No Implementation', 'Partial Implementation', 'Difference'), 
                     c(2007, 2011, '--'), round(tab_1, 6))
colnames(tab_1) <- c('device', 'year', 'control', 'treated', 'difference')


## 2011 v. 2015
data.frame( control = c( mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['partial',1], 
                         mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['full',1] ),
            treated = c( mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1] + 
                           mod[['coefficients']]['partial',1] + mod[['coefficients']]['partial_treatment',1],
                         mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1] +
                           mod[['coefficients']]['full',1] + mod[['coefficients']]['full_treatment',1] ) ) %>% 
  as_tibble() -> tab_2

# calculate totals
tab_2[3, ] <- tab_2[2, ] - tab_2[1, ]
tab_2[ ,3] <- tab_2[ ,2] - tab_2[ ,1]

# clean table
tab_2 <- data.frame( c('Partial Implementation', 'Full Implementation', 'Difference'), 
                     c(2011, 2015, '--'), round(tab_2, 6))
colnames(tab_2) <- c('device', 'year', 'control', 'treated', 'difference')


## Weighted T-Test

t.Test_FUN <- function(treat, control, dv) {
  
  weights::wtd.t.test( x = t(treat[, dv]), y = t(control[, dv]), 
                       weight = treat$weights, weighty = control$weights,
                       samedata = F) -> test
  
  return(test)
  
}

mod_dat$group <- with(mod_dat, paste(ifelse(year == 2011, "p", ifelse(year == 2015, "f", "n")), ifelse(treatment == 1, "t", "c"), sep = ""))

data.frame( group = c(rep('Device: Control', 2), rep('Device: Treatment', 2), 'Year: 2007', 'Year: 2011', 'Year: 2015' ),
            pair  = c(rep(c('Year: 2007 vs. 2011', 'Year: 2011 vs. 2015'), 2), rep('Device: Treatment vs. Control', 3) ),
            control = c('nc', 'pc', 'nt', 'pt', 'nc', 'pc', 'fc'),
            treated = c('pc', 'fc', 'pt', 'ft', 'nt', 'pt', 'ft') ) -> t.Test_groups

for (m in 1:nrow(t.Test_groups)) {
  
  loop_test <- t.Test_FUN( mod_dat %>% filter( group %in% t.Test_groups$treated[m] ),
                           mod_dat %>% filter( group %in% t.Test_groups$control[m] ),
                           'gubernatorial_resVote' )
  
  data.frame( group = t.Test_groups$group[m], pair = t.Test_groups$pair[m], 
              Difference = round(loop_test$additional[1], 6), 
              round(data.frame(t(loop_test$coefficients)), 6) ) -> loop_test
  
  if('t.test' %in% ls()) t.test <- as_tibble(rbind(t.test, loop_test)) else loop_test -> t.test
  
  
  rm(loop_test)
  
}


rm(t.Test_FUN, t.Test_groups, m)


## Print Results

cat("Table II-3-4. Difference-in-Differences (DDD)",
    "===============================",
    'Dependent Variable: Residual Votes',
    'Gubernatorial election',
    'Matching Method: Coarsened Exact Matching',
    'Matching level: Strict',
    'Years: 2007 v. 2011 v. 2015',
    'Province: Salta',
    'Department: Capital',
    "\n",
    knitr::kable(tab_1, row.names = F), '',
    knitr::kable(tab_2, row.names = F), 
    "\n",
    "Weighted Student's t-Test",
    "-------------------------", '',
    knitr::kable(t.test, row.names = F),
    '\n',
    "Interaction model",
    "-----------------", '',
    capture.output( arm::display(interact_mod, digits = 4, detail = T)),
    sep = "\n",
    append = F)



rm( tab_1, tab_2, t.test, 
    interact_mod, mod,
    mod_dat )


## Descriptive figure :: Average residual votes, by office and year

cat( paste('Figure 8 (p.37)', "--------------", 
           'Average residual votes, by office and year', 
           'Capital Department (2007-2015)', sep = '\n'), '',
     rep('', 2),
     knitr::kable( rep_data %>%
                     filter(match_level %in% 'strict') %>% 
                     group_by(year) %>% 
                     summarise( Governor = mean(gubernatorial_resVote, na.rm = T), 
                                House = mean(provHouse_resVote, na.rm = T), 
                                Mayor = mean(mayor_resVote, na.rm = T) ) ,
                   digits = 4 ), 
     sep = "\n" )






## Ballot --------------------------
## Roll-off

## Model data

rep_data %>% 
  filter( match_level %in% 'strict') %>% 
  filter(!is.na(weights)) -> mod_dat

# year dummies
tmp <- with(mod_dat, fastDummies::dummy_columns(year, ignore_na = T) )
tmp <- tmp[2:ncol(tmp)]
colnames(tmp) <- c('no_imp', 'partial', 'full')

data.frame( mod_dat, tmp) %>% as_tibble() -> mod_dat

# interaction terms
with(mod_dat, partial * treatment) -> mod_dat$partial_treatment
with(mod_dat, full * treatment) -> mod_dat$full_treatment

rm(tmp)



## Difference-in-Differences

# note: loop produces results estimates of Ballot Roll-Off
# for (1) House Elections and (2) Mayoral Elections

for (i in c('House_rolloff', 'mayor_rolloff')) {
  
  # i = sample(c('House_rolloff', 'mayor_rolloff'), 1)
  
  ## Interaction Model
  
  interact_mod = lm( as.formula(paste(i, 'partial + full + treatment + partial_treatment + full_treatment', sep = ' ~ ')), 
                     data = mod_dat,  
                     weight = weights)
  
  summary(interact_mod) -> mod
  
  
  ## Difference-in-Differences

  ## 2007 v. 2011
  data.frame( control = c( mod[['coefficients']]['(Intercept)',1],
                           mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['partial',1]),
              treated = c( mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1],
                           mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1] +
                             mod[['coefficients']]['partial',1] + mod[['coefficients']]['partial_treatment',1]) ) %>% 
    as_tibble() -> tab_1
  
  # calculate totals
  tab_1[3, ] <- tab_1[2, ] - tab_1[1, ]
  tab_1[ ,3] <- tab_1[ ,2] - tab_1[ ,1]
  
  # clean table
  tab_1 <- data.frame( c('No Implementation', 'Partial Implementation', 'Difference'), 
                       c(2007, 2011, '--'), round(tab_1, 6))
  colnames(tab_1) <- c('device', 'year', 'control', 'treated', 'difference')
  
  
  ## 2011 v. 2015
  data.frame( control = c( mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['partial',1], 
                           mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['full',1] ),
              treated = c( mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1] + 
                             mod[['coefficients']]['partial',1] + mod[['coefficients']]['partial_treatment',1],
                           mod[['coefficients']]['(Intercept)',1] + mod[['coefficients']]['treatment',1] +
                             mod[['coefficients']]['full',1] + mod[['coefficients']]['full_treatment',1] ) ) %>% 
    as_tibble() -> tab_2
  
  # calculate totals
  tab_2[3, ] <- tab_2[2, ] - tab_2[1, ]
  tab_2[ ,3] <- tab_2[ ,2] - tab_2[ ,1]
  
  # clean table
  tab_2 <- data.frame( c('Partial Implementation', 'Full Implementation', 'Difference'), 
                       c(2011, 2015, '--'), round(tab_2, 6))
  colnames(tab_2) <- c('device', 'year', 'control', 'treated', 'difference')
  
  
  ## Weighted T-Test
  
  t.Test_FUN <- function(treat, control, dv) {
    
    weights::wtd.t.test( x = t(treat[, dv]), y = t(control[, dv]), 
                         weight = treat$weights, weighty = control$weights,
                         samedata = F) -> test
    
    return(test)
    
  }
  
  mod_dat$group <- with(mod_dat, paste(ifelse(year == 2011, "p", ifelse(year == 2015, "f", "n")), ifelse(treatment == 1, "t", "c"), sep = ""))
  
  data.frame( group = c(rep('Device: Control', 2), rep('Device: Treatment', 2), 'Year: 2007', 'Year: 2011', 'Year: 2015' ),
              pair  = c(rep(c('Year: 2007 vs. 2011', 'Year: 2011 vs. 2015'), 2), rep('Device: Treatment vs. Control', 3) ),
              control = c('nc', 'pc', 'nt', 'pt', 'nc', 'pc', 'fc'),
              treated = c('pc', 'fc', 'pt', 'ft', 'nt', 'pt', 'ft') ) -> t.Test_groups
  
  for (m in 1:nrow(t.Test_groups)) {
    
    loop_test <- t.Test_FUN( mod_dat %>% filter( group %in% t.Test_groups$treated[m] ),
                             mod_dat %>% filter( group %in% t.Test_groups$control[m] ),
                             i)
    
    data.frame( group = t.Test_groups$group[m], pair = t.Test_groups$pair[m], 
                Difference = round(loop_test$additional[1], 6), 
                round(data.frame(t(loop_test$coefficients)), 6) ) -> loop_test
    
    
    if('t.test' %in% ls()) t.test <- as_tibble(rbind(t.test, loop_test)) else loop_test -> t.test
    
    rm(loop_test)
    
    
  }
  
  
  rm(t.Test_FUN, t.Test_groups, m)
  
  
  ## Predictions
  
  # Posterior simulation
  arm::sim(interact_mod, n.sims = 50000 ) -> mod_sim
  
  # Prediction frame
  X <- unique( mod_dat %>% select(all_of(all.vars(formula(interact_mod))[-1])))
  X <- as_tibble(data.frame( 0, X)) %>% filter(treatment == 1)
  
  # labels
  treat_lab = with(X, ifelse(partial == 1, "Partial Implementation", ifelse(full == 1, 'Full Implementation', 'No Implementation')))
  year_lab = with(X, ifelse(partial == 1, 2011, ifelse(full == 1, 2015, 2007)))
  year_lab <- data.frame(year = year_lab, X %>% select(partial, full))
  
  # corrections
  X$partial <- 0
  X$full <- 0
  
  # sim coefficients
  b <- as.matrix(mod_sim@coef)
  Xb <- t(as.matrix(X)%*% t(b))
  
  # predictions
  data.frame( apply(Xb, 2, mean), t(apply(Xb, 2, quantile, probs=c(.025,.05,.95,.975))) ) -> pred
  colnames(pred) <- c("fit", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")
  
  data.frame( treat_lab,
              office_lab = stringr::str_to_title(gsub( pattern = '_d', replacement = '', x = gsub( pattern = '_rolloff', replacement = '', x = i))),
              set = 'DDD',
              match_level = 'strict' ,
              year_lab,
              pred) %>% 
    arrange(year) %>% 
    as_tibble() -> pred
  
  
  # store Predictions
  if('pred_store' %in% ls()) pred_store <- as_tibble(rbind(pred_store, pred)) else pred -> pred_store
  
  
  rm( treat_lab, year_lab,
      Xb, X, b,
      mod_sim, 
      pred )
  
  
  ## Print Results
  
  cat("Table II-3-5. Difference-in-Differences (DDD)",
      "===============================",
      'Dependent Variable: Roll-Off',
      paste('Election:', stringr::str_to_title(gsub( pattern = '_d', replacement = '', x = gsub( pattern = '_rolloff', replacement = '', x = i)))),
      'Matching Method: Coarsened Exact Matching',
      'Matching level: Strict',
      'Years: 2007 v. 2011 v. 2015',
      'Province: Salta',
      'Department: Capital',
      "\n",
      knitr::kable(tab_1, row.names = F), '',
      knitr::kable(tab_2, row.names = F), 
      "\n",
      "Weighted Student's t-Test",
      "-------------------------", '',
      knitr::kable(t.test, row.names = F),
      '\n',
      "Interaction model",
      "-----------------", '',
      capture.output( arm::display(interact_mod, digits = 4, detail = T)),
      sep = "\n",
      append = F)
  
  
  rm( tab_1, tab_2, t.test, 
      interact_mod, mod)
  
  
}



cat( paste('Figure 10: Predicted ballot roll-off difference by treatment group (p.38)',
           "--------------", 
           'Model Predictions based on Table II-3-5 (in Appendix)', 
           'Difference-in-Differences (DDD). DV: Ballot Rol-Off (%)', 
           'Based on Data from the Capital Department, 2007–2015', 
           sep = '\n'), '',
     rep('', 2),
     knitr::kable( pred_store, digits = 4 ), 
     sep = "\n" )



rm( pred_store, i)
rm( mod_dat)
