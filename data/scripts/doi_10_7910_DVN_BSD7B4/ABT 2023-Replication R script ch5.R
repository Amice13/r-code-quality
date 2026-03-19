## The Representational Consequences
## of Electronic Voting Reform

## Replication Data for: 
## ch 5 :: Vote Concentration

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
## Data

## data
rep_data %>% 
  filter(!is.na(vote_gini)) %>%
  mutate( votes_electronic = votes_pct * electronic) %>%
  mutate( votes_sq = votes_pct ^2) %>%
  mutate( pop_density_log = log(pop_density)) %>%
  mutate( area_k = area / 1000) -> mod_data



## Concentration --------------------------
## Models

mod <- list()

# (1) Provincial House
mod[['H_beta_1']] <- betareg::betareg( vote_gini ~ electronic + votes_electronic + 
                                         votes_pct + votes_sq + 
                                         magnitude,
                                       link = "logit" , 
                                       method = 'Nelder-Mead' , 
                                       data = mod_data %>% 
                                         filter(office %in% "PROV HOUSE") %>%
                                         filter(mixed == 0) )

mod[['H_beta_2']] <- betareg::betareg( vote_gini ~ electronic + votes_electronic + 
                                         votes_pct + votes_sq + 
                                         n_box + gov_concurrence + area_k + pop_density_log + 
                                         complete_HS_or_higher + NBI + urban_housing + 
                                         magnitude,
                                       link = "logit" , 
                                       method = 'Nelder-Mead' , 
                                       data = mod_data %>% 
                                         filter(office %in% "PROV HOUSE") %>%
                                         filter(mixed == 0) )



# (2) Provincial Senate
mod[['S_beta_1']] <- betareg::betareg( vote_gini ~ electronic + votes_electronic + 
                                         votes_pct + votes_sq,
                                       link = "logit" ,
                                       method = 'Nelder-Mead' ,
                                       data = mod_data %>% 
                                         filter(office %in% "PROV SENATE") %>% 
                                         filter(mixed == 0) )

mod[['S_beta_2']] <- betareg::betareg( vote_gini ~ electronic + votes_electronic + 
                                         votes_pct + votes_sq + 
                                         n_box + gov_concurrence + area_k + pop_density_log + 
                                         complete_HS_or_higher + NBI + urban_housing,
                                       link = "logit" ,
                                       method = 'Nelder-Mead' ,
                                       data = mod_data %>% 
                                         filter(office %in% "PROV SENATE") %>% 
                                         filter(mixed == 0) )



## Model --------------------------
## Results

stargazer::stargazer( mod, 
                      title = "Table II-5-1. Vote Concentration in Provincial Elections: Salta's House and Senate, 2009-2019. Beta Regression Models",
                      dep.var.labels = rep('Gini Idx', 4),
                      type = "text",
                      no.space = T,
                      digits = 4, star.cutoffs = c(.05, .01, .001),
                      column.labels = c('House', 'House', 'Senate', 'Senate'), 
                      model.numbers = T, model.names = F, object.names = F,
                      omit.stat=c("f", "ser")
                      )




## Figure 15 --------------------------
## Vote concentration of two (selected) parties

cat( paste('Figure 15: Vote concentration of two (selected) parties (p.58)',
           "--------------", 
           'Frente para la Victoria and Frente Salteño',
           'Province House (Province of Salta, 2009)',
           sep = '\n'),
     rep('', 2),
     knitr::kable( rep_data %>%
                     filter( year == 2009) %>% 
                     filter( office %in% 'PROV HOUSE') %>%
                     filter( party_number %in% c(519, 554)) %>% 
                     select( prov_name, dept_name, prov_code, dept_code, 
                             year, office, 
                             party_number, party, vote_gini ) %>% 
                     arrange( party_number, party, dept_name) , 
                   digits = 4 ), 
     sep = "\n")



## Model --------------------------
## Prediction

pred_FUN <- function( model, group) {
  
  # model vars
  (model %>% formula() %>% all.vars())[-1] -> mod_vars
  
  # frame
  pred <- data.frame( votes_pct = seq( 0, 30, length.out = 121))
  
  rbind( data.frame(electronic = 1, pred),
         data.frame(electronic = 0, pred)) %>%
    as_tibble() -> pred
  
  # Control vars: median value
  data.frame( pred, data.frame(t(apply( as.matrix(model.frame(model)),
                                        2, 
                                        quantile, probs=.5, na.rm=T)) )[mod_vars[!mod_vars %in% colnames(pred)]] ) %>%
    
    # interaction terms
    mutate( votes_electronic = electronic * votes_pct ) %>% 
    mutate( votes_sq = votes_pct^2 ) %>%
    
    # select vars
    dplyr::select(all_of(mod_vars)) %>% 
    as_tibble() -> X
  
  # pred store
  data.frame(matrix(nrow = nrow(pred), ncol = 5)) %>%
    as_tibble() -> store
  
  colnames(store) <- c('fit', "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")
  
  # prediction
  for (i in 1:nrow(store)) {
    
    # set level = .90
    emmeans::emm_options(ref_grid = list(level = .90))
    
    # preditions :: c.i. 90%
    loop_pred <- emmeans::emmeans( model, 
                                   specs = ~ electronic , 
                                   nesting = NULL, 
                                   data = X[i, ] )
    
    loop_pred <- data.frame(loop_pred)
    
    store[i, "ci.lo.90"] <- loop_pred$asymp.LCL
    store[i, "ci.up.90"] <- loop_pred$asymp.UCL
    
    
    # set level = .95
    emmeans::emm_options(ref_grid = list(level = .95))
    
    # preditions :: c.i. 95%
    loop_pred <- emmeans::emmeans( model, 
                                   specs = ~ electronic , 
                                   nesting = NULL, 
                                   data = X[i, ] )
    
    loop_pred <- data.frame(loop_pred)
    
    store[i, "ci.lo.95"] <- loop_pred$asymp.LCL
    store[i, "ci.up.95"] <- loop_pred$asymp.UCL
    
    
    # preditions :: mean
    if('response' %in% colnames(loop_pred)) store[i, 'fit'] <- loop_pred$response else loop_pred$emmean -> store[i, 'fit']
    
    rm(loop_pred)
    
    
  }
  
  # data labels
  pred %>% 
    mutate( ev_lab = ifelse(electronic == 1, 'Electronic Voting', 'Paper Ballot')) %>%
    mutate( office_lab = group ) %>% 
    dplyr::select(office_lab, ev_lab, votes_pct) %>%
    data.frame( ., store ) %>%
    as_tibble() -> pred_store
  
  
  return(pred_store)
  
  
}

rbind( pred_FUN( model = mod[['H_beta_2']], group = 'Province House'),
       pred_FUN( model = mod[['S_beta_2']], group = 'Province Senate') ) %>%
  as_tibble() -> mod_pred






## Prediction --------------------------
## Plot

par( mfrow=c(2,2))

# House
plot( fit ~ votes_pct,
      type = 'l',
      xlim = c(0, 30), ylim = c(0, .6), 
      main = "House Elections: Electronic Voting",
      xlab = "Party's Vote (%)", 
      ylab = "Expected Vote Concentration",
      col = 'red',
      data = mod_pred %>%
        filter( office_lab %in% 'Province House') %>%
        filter( ev_lab %in% 'Electronic Voting')
      )

lines( ci.lo.95 ~ votes_pct, 
       lty = 2,
       data = mod_pred %>%
         filter( office_lab %in% 'Province House') %>%
         filter( ev_lab %in% 'Electronic Voting')
       )

lines( ci.up.95 ~ votes_pct, 
       lty = 2,
       data = mod_pred %>%
         filter( office_lab %in% 'Province House') %>%
         filter( ev_lab %in% 'Electronic Voting')
       )


plot( fit ~ votes_pct,
      type = 'l',
      xlim = c(0, 30), ylim = c(0, .6), 
      main = "House Elections: Paper Ballot",
      xlab = "Party's Vote (%)", 
      ylab = "Expected Vote Concentration",
      col = 'blue',
      data = mod_pred %>%
        filter( office_lab %in% 'Province House') %>%
        filter(!ev_lab %in% 'Electronic Voting')
      )

lines( ci.lo.95 ~ votes_pct, 
       lty = 2,
       data = mod_pred %>%
         filter( office_lab %in% 'Province House') %>%
         filter(!ev_lab %in% 'Electronic Voting')
       )

lines( ci.up.95 ~ votes_pct, 
       lty = 2,
       data = mod_pred %>%
         filter( office_lab %in% 'Province House') %>%
         filter(!ev_lab %in% 'Electronic Voting')
       )

# Senate
plot( fit ~ votes_pct,
      type = 'l',
      xlim = c(0, 30), ylim = c(0, .6), 
      main = "Senate Elections: Electronic Voting",
      xlab = "Party's Vote (%)", 
      ylab = "Expected Vote Concentration",
      col = 'red',
      data = mod_pred %>%
        filter( office_lab %in% 'Province Senate') %>%
        filter( ev_lab %in% 'Electronic Voting')
      )

lines( ci.lo.95 ~ votes_pct, 
       lty = 2,
       data = mod_pred %>%
         filter( office_lab %in% 'Province Senate') %>%
         filter( ev_lab %in% 'Electronic Voting')
       )

lines( ci.up.95 ~ votes_pct, 
       lty = 2,
       data = mod_pred %>%
         filter( office_lab %in% 'Province Senate') %>%
         filter( ev_lab %in% 'Electronic Voting')
       )


plot( fit ~ votes_pct,
      type = 'l',
      xlim = c(0, 30), ylim = c(0, .6), 
      main = "Senate Elections: Paper Ballot",
      xlab = "Party's Vote (%)", 
      ylab = "Expected Vote Concentration",
      col = 'blue',
      data = mod_pred %>%
        filter( office_lab %in% 'Province Senate') %>%
        filter(!ev_lab %in% 'Electronic Voting')
      )

lines( ci.lo.95 ~ votes_pct, 
       lty = 2,
       data = mod_pred %>%
         filter( office_lab %in% 'Province Senate') %>%
         filter(!ev_lab %in% 'Electronic Voting')
       )

lines( ci.up.95 ~ votes_pct, 
       lty = 2,
       data = mod_pred %>%
         filter( office_lab %in% 'Province Senate') %>%
         filter(!ev_lab %in% 'Electronic Voting')
       )

