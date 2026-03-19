## Adaptive Experimental Design: Prospects and Applications in Political Science
## Conducts simulations of many experiments across parameter values
## Produces iterated simulations for Table 1, and all figures and tables for 
## Appendix D: Tables D6-D9, and Figures D7-D11. 

version

set.seed(95126)


# Set options ----
source('utils.R')
iter <- 1e+04
arms <- 9

over <- c(2, 4, 5, 8, 20, 50, 100) # number of periods
range <- c(.10, seq(.12, .19, .01)) # potential values of best arm
first <- c(190, 280, 370, 460, 550, 640, 730, 820, 910) # size of first period

if(file.exists('simulations_supplement1.rds')){
  simulations_supplement1 <- read_rds('simulations_supplement1.rds')
}else{# Run iterated simulations over frequency of updating, first batch ----
  
  # Case 1: clear winner ----
  
  best_prob <- .2
  other_prob <- .1 
  probs <- c(best_prob, rep(other_prob, arms-1))
  
  # Note: mysims may produce vcov errors for estimates of ATE, where there is 
  # no augmented control condition, as there may be no variance in control 
  # observations
  out1 <- mysims(probs = probs, iter = iter, static = FALSE)
  out1_stat <- mysims(probs = probs, iter = iter, static = TRUE)
  out1_te <- mysims(probs = probs, iter = iter, static = FALSE, control = 3)
  
  # iterate over frequency of updating
  for (o in over){
    out <- mysims(probs = probs, iter = iter, static = FALSE, n = (1000/o), 
                  periods = o )
    out_te <- mysims(probs = probs, iter = iter, static = FALSE, 
                     control = 3, n = (1000/o), periods = o)
    
    assign(paste0('out1_', o), out)
    assign(paste0('out1_te_', o), out_te)
  }
  
  # iterate over size of first batch
  for (f in first){
    out <- mysims(probs = probs, iter = iter, static = FALSE, n = 100, 
                  periods = 10, first = f )
    
    out_te <- mysims(probs = probs, iter = iter, static = FALSE, 
                     control = 3, n = 100, periods = 10, first = f)
    
    assign(paste0('out1_b_', f), out)
    assign(paste0('out1_te_b_', f), out_te)
  }
  
  # Case 2: no clear winner ----
  
  best_prob <- .11
  other_prob <- .1
  probs <- c(best_prob, rep(other_prob, arms-1))
  
  out2 <- mysims(probs = probs, iter = iter, static = FALSE)
  out2_stat <- mysims(probs = probs, iter = iter, static = TRUE)
  out2_te <- mysims(probs = probs, iter = iter, static = FALSE, control = 3)
  
  # iterate over frequency of updating
  for (o in over){
    out <- mysims(probs = probs, iter = iter, static = FALSE, n = (1000/o), 
                  periods = o )
    out_te <- mysims(probs = probs, iter = iter, static = FALSE, 
                     control = 3, n = (1000/o), periods = o)
    
    assign(paste0('out2_', o), out)
    assign(paste0('out2_te_', o), out_te)
  }
  
  for (f in first){
    out <- mysims(probs = probs, iter = iter, static = FALSE, n = 100, 
                  periods = 10, first = f )
    
    out_te <- mysims(probs = probs, iter = iter, static = FALSE, 
                     control = 3, n = 100, periods = 10, first = f)
    
    assign(paste0('out2_b_', f), out)
    assign(paste0('out2_te_b_', f), out_te)
  }
  
  # Case 3: competing second best ----
  
  best_prob <- .2
  second_best <- .18
  other_prob <- .1
  probs <- c(best_prob, second_best, rep(other_prob, arms-2))
  
  out3 <- mysims(probs = probs, iter = iter, static = FALSE)
  out3_stat <- mysims(probs = probs, iter = iter, static = TRUE)
  out3_te <- mysims(probs = probs, iter = iter, static = FALSE, control = 3)
  
  # iterate over frequency of updating
  for (o in over){
    out <- mysims(probs = probs, iter = iter, static = FALSE, n = (1000/o), 
                  periods = o )
    out_te <- mysims(probs = probs, iter = iter, static = FALSE, 
                     control = 3, n = (1000/o), periods = o)
    
    assign(paste0('out3_', o), out)
    assign(paste0('out3_te_', o), out_te)
  }
  
  # iterate over size of first batch
  for (f in first){
    out <- mysims(probs = probs, iter = iter, static = FALSE, n = 100, 
                  periods = 10, first = f )
    
    out_te <- mysims(probs = probs, iter = iter, static = FALSE, 
                     control = 3, n = 100, periods = 10, first = f)
    
    assign(paste0('out3_b_', f), out)
    assign(paste0('out3_te_b_', f), out_te)
  }
  
  names <- paste0('out', 1:3) %>% 
    outer(c('', '_te'), paste0) %>% 
    outer(c('', paste0('_', over)), paste0)%>% 
    as.vector()
  
  names2 <- paste0('out', 1:3) %>% 
    outer(c('_stat'), paste0) %>%
    as.vector()  
  
  names3 <- paste0('out', 1:3) %>% 
    outer(c('_b', '_te_b'), paste0) %>% 
    outer(c(paste0('_', first)), paste0)%>% 
    as.vector()
  
  l1 <- lapply(c(names, names2, names3), function(x) as.data.frame(get(x))[,1:10])
  names(l1) <- c(names, names2, names3)
  
  simulations_supplement1 <- bind_rows(l1, .id = 'simulation')
  write_rds(simulations_supplement1, 'simulations_supplement1.rds')
}
  
list2env(split(simulations_supplement1, simulations_supplement1['simulation']), 
         envir=.GlobalEnv)

# Simulate for the best arm over the range .1 to .2
if(file.exists('simulations_supplement2.rds')){
  simulations_supplement2 <- read_rds('simulations_supplement2.rds')
}else{# Run iterated simulations over range ----
  
  probs <- c(NA, rep(.1, arms-1))
  for(j in  range){
    probs[1] <- j
    out <- mysims(probs = probs, iter = iter, static = FALSE)
    out_stat <- mysims(probs = probs, iter = iter, static = TRUE)
    out_te <- mysims(probs = probs, iter = iter, static = FALSE, control = 3)
    
    assign(paste0('out_', 100*j), out)
    assign(paste0('out_stat_', 100*j), out_stat)
    assign(paste0('out_te_', 100*j), out_te)
  }
  
  names <- outer('out', c('', '_stat', '_te'), paste0) %>% 
    outer(paste0('_', range*100), paste0)%>% 
    as.vector()
  
  l2 <- lapply(names, function(x) as.data.frame(get(x))[,1:10])
  names(l2) <- names
  
  simulations_supplement2 <- bind_rows(l2, .id = 'simulation')
  write_rds(simulations_supplement2, 'simulations_supplement2.rds')
}

list2env(split(simulations_supplement2, simulations_supplement2['simulation']), 
         envir=.GlobalEnv)


# Results tables ----
# * Main results ----
# ** [Table 1] ----
df <- tibble(
  algorithm = rep(c('TS', 'Static', 'CA'), each = 3),
  case = rep(1:3, times = 3),
  correct = c(mean(out1[,'correct']), 
              mean(out2[,'correct']), 
              mean(out3[,'correct']),
              mean(out1_stat[,'correct']), 
              mean(out2_stat[,'correct']), 
              mean(out3_stat[,'correct']),
              mean(out1_te[,'correct']), 
              mean(out2_te[,'correct']), 
              mean(out3_te[,'correct'])),
  rmse_best = c(mean(out1[,'rmse_best']), 
                mean(out2[,'rmse_best']), 
                mean(out3[,'rmse_best']),
                mean(out1_stat[,'rmse_best']), 
                mean(out2_stat[,'rmse_best']), 
                mean(out3_stat[,'rmse_best']),
                mean(out1_te[,'rmse_best']), 
                mean(out2_te[,'rmse_best']), 
                mean(out3_te[,'rmse_best'])),
  rmse_te = c(NA, # mean(out1[,'rmse_te']), 
              NA, # mean(out2[,'rmse_te']), 
              NA, # mean(out3[,'rmse_te']),
              mean(out1_stat[,'rmse_te']), 
              mean(out2_stat[,'rmse_te']), 
              mean(out3_stat[,'rmse_te']),
              mean(out1_te[,'rmse_te']), 
              mean(out2_te[,'rmse_te']), 
              mean(out3_te[,'rmse_te'])),
  cov_best = c(mean(out1[,'cov_best']), 
               mean(out2[,'cov_best']), 
               mean(out3[,'cov_best']),
               mean(out1_stat[,'cov_best']), 
               mean(out2_stat[,'cov_best']), 
               mean(out3_stat[,'cov_best']),
               mean(out1_te[,'cov_best']), 
               mean(out2_te[,'cov_best']), 
               mean(out3_te[,'cov_best'])),
  cov_te = c(NA, # mean(out1[,'cov_te']), 
             NA, # mean(out2[,'cov_te']), 
             NA, # mean(out3[,'cov_te']),
             mean(out1_stat[,'cov_te']), 
             mean(out2_stat[,'cov_te']), 
             mean(out3_stat[,'cov_te']),
             mean(out1_te[,'cov_te']), 
             mean(out2_te[,'cov_te']), 
             mean(out3_te[,'cov_te']))
)

head(df)

# While RMSE and coverage *can* be computed for TS for the treatment effect 
# estimate using an arbitrary sub-optimal arm as the control condition, we 
# exclude these metrics from table 1 (replaced with NAs) as there is no explicit 
# control condition  in the design.  

# write_csv(df, 'table1.csv')

# * Frequency of updating results ----
# ** [Table D6] ----
df <- tibble(
  algorithm = rep(c('TS', 'CA'), each = 3*9),
  case = rep(rep(1:3, each = 9), times = 2),
  batches = rep(c(1, 2, 4, 5, 8, 10, 20, 50, 100), times = 3*2),
  correct = c(mean(out1_stat[,'correct']), 
              mean(out1_2[,'correct']),
              mean(out1_4[,'correct']),
              mean(out1_5[,'correct']),
              mean(out1_8[,'correct']),
              mean(out1[,'correct']),
              mean(out1_20[,'correct']),
              mean(out1_50[,'correct']),
              mean(out1_100[,'correct']),
              #
              mean(out2_stat[,'correct']), 
              mean(out2_2[,'correct']),
              mean(out2_4[,'correct']),
              mean(out2_5[,'correct']),
              mean(out2_8[,'correct']),
              mean(out2[,'correct']),
              mean(out2_20[,'correct']),
              mean(out2_50[,'correct']),
              mean(out2_100[,'correct']),
              #
              mean(out3_stat[,'correct']), 
              mean(out3_2[,'correct']),
              mean(out3_4[,'correct']),
              mean(out3_5[,'correct']),
              mean(out3_8[,'correct']),
              mean(out3[,'correct']),
              mean(out3_20[,'correct']),
              mean(out3_50[,'correct']),
              mean(out3_100[,'correct']),
              #
              NA, 
              mean(out1_te_2[,'correct']),
              mean(out1_te_4[,'correct']),
              mean(out1_te_5[,'correct']),
              mean(out1_te_8[,'correct']),
              mean(out1_te[,'correct']),
              mean(out1_te_20[,'correct']),
              mean(out1_te_50[,'correct']),
              mean(out1_te_100[,'correct']),
              #
              NA, 
              mean(out2_te_2[,'correct']),
              mean(out2_te_4[,'correct']),
              mean(out2_te_5[,'correct']),
              mean(out2_te_8[,'correct']),
              mean(out2_te[,'correct']),
              mean(out2_te_20[,'correct']),
              mean(out2_te_50[,'correct']),
              mean(out2_te_100[,'correct']),
              #
              NA, 
              mean(out3_te_2[,'correct']),
              mean(out3_te_4[,'correct']),
              mean(out3_te_5[,'correct']),
              mean(out3_te_8[,'correct']),
              mean(out3_te[,'correct']),
              mean(out3_te_20[,'correct']),
              mean(out3_te_50[,'correct']),
              mean(out3_te_100[,'correct'])
  ),
  rmse_best = c(mean(out1_stat[,'rmse_best']), 
                mean(out1_2[,'rmse_best']),
                mean(out1_4[,'rmse_best']),
                mean(out1_5[,'rmse_best']),
                mean(out1_8[,'rmse_best']),
                mean(out1[,'rmse_best']),
                mean(out1_20[,'rmse_best']),
                mean(out1_50[,'rmse_best']),
                mean(out1_100[,'rmse_best']),
                #
                mean(out2_stat[,'rmse_best']), 
                mean(out2_2[,'rmse_best']),
                mean(out2_4[,'rmse_best']),
                mean(out2_5[,'rmse_best']),
                mean(out2_8[,'rmse_best']),
                mean(out2[,'rmse_best']),
                mean(out2_20[,'rmse_best']),
                mean(out2_50[,'rmse_best']),
                mean(out2_100[,'rmse_best']),
                #
                mean(out3_stat[,'rmse_best']), 
                mean(out3_2[,'rmse_best']),
                mean(out3_4[,'rmse_best']),
                mean(out3_5[,'rmse_best']),
                mean(out3_8[,'rmse_best']),
                mean(out3[,'rmse_best']),
                mean(out3_20[,'rmse_best']),
                mean(out3_50[,'rmse_best']),
                mean(out3_100[,'rmse_best']),
                #
                NA, 
                mean(out1_te_2[,'rmse_best']),
                mean(out1_te_4[,'rmse_best']),
                mean(out1_te_5[,'rmse_best']),
                mean(out1_te_8[,'rmse_best']),
                mean(out1_te[,'rmse_best']),
                mean(out1_te_20[,'rmse_best']),
                mean(out1_te_50[,'rmse_best']),
                mean(out1_te_100[,'rmse_best']),
                #
                NA, 
                mean(out2_te_2[,'rmse_best']),
                mean(out2_te_4[,'rmse_best']),
                mean(out2_te_5[,'rmse_best']),
                mean(out2_te_8[,'rmse_best']),
                mean(out2_te[,'rmse_best']),
                mean(out2_te_20[,'rmse_best']),
                mean(out2_te_50[,'rmse_best']),
                mean(out2_te_100[,'rmse_best']),
                #
                NA, 
                mean(out3_te_2[,'rmse_best']),
                mean(out3_te_4[,'rmse_best']),
                mean(out3_te_5[,'rmse_best']),
                mean(out3_te_8[,'rmse_best']),
                mean(out3_te[,'rmse_best']),
                mean(out3_te_20[,'rmse_best']),
                mean(out3_te_50[,'rmse_best']),
                mean(out3_te_100[,'rmse_best'])),
  rmse_te = c(mean(out1_stat[,'rmse_te']), 
              mean(out1_2[,'rmse_te']),
              mean(out1_4[,'rmse_te']),
              mean(out1_5[,'rmse_te']),
              mean(out1_8[,'rmse_te']),
              mean(out1[,'rmse_te']),
              mean(out1_20[,'rmse_te']),
              mean(out1_50[,'rmse_te']),
              mean(out1_100[,'rmse_te']),
              #
              mean(out2_stat[,'rmse_te']), 
              mean(out2_2[,'rmse_te']),
              mean(out2_4[,'rmse_te']),
              mean(out2_5[,'rmse_te']),
              mean(out2_8[,'rmse_te']),
              mean(out2[,'rmse_te']),
              mean(out2_20[,'rmse_te']),
              mean(out2_50[,'rmse_te']),
              mean(out2_100[,'rmse_te']),
              #
              mean(out3_stat[,'rmse_te']), 
              mean(out3_2[,'rmse_te']),
              mean(out3_4[,'rmse_te']),
              mean(out3_5[,'rmse_te']),
              mean(out3_8[,'rmse_te']),
              mean(out3[,'rmse_te']),
              mean(out3_20[,'rmse_te']),
              mean(out3_50[,'rmse_te']),
              mean(out3_100[,'rmse_te']),
              #
              NA, 
              mean(out1_te_2[,'rmse_te']),
              mean(out1_te_4[,'rmse_te']),
              mean(out1_te_5[,'rmse_te']),
              mean(out1_te_8[,'rmse_te']),
              mean(out1_te[,'rmse_te']),
              mean(out1_te_20[,'rmse_te']),
              mean(out1_te_50[,'rmse_te']),
              mean(out1_te_100[,'rmse_te']),
              #
              NA, 
              mean(out2_te_2[,'rmse_te']),
              mean(out2_te_4[,'rmse_te']),
              mean(out2_te_5[,'rmse_te']),
              mean(out2_te_8[,'rmse_te']),
              mean(out2_te[,'rmse_te']),
              mean(out2_te_20[,'rmse_te']),
              mean(out2_te_50[,'rmse_te']),
              mean(out2_te_100[,'rmse_te']),
              #
              NA, 
              mean(out3_te_2[,'rmse_te']),
              mean(out3_te_4[,'rmse_te']),
              mean(out3_te_5[,'rmse_te']),
              mean(out3_te_8[,'rmse_te']),
              mean(out3_te[,'rmse_te']),
              mean(out3_te_20[,'rmse_te']),
              mean(out3_te_50[,'rmse_te']),
              mean(out3_te_100[,'rmse_te'])),
  cov_best = c(mean(out1_stat[,'cov_best']), 
               mean(out1_2[,'cov_best']),
               mean(out1_4[,'cov_best']),
               mean(out1_5[,'cov_best']),
               mean(out1_8[,'cov_best']),
               mean(out1[,'cov_best']),
               mean(out1_20[,'cov_best']),
               mean(out1_50[,'cov_best']),
               mean(out1_100[,'cov_best']),
               #
               mean(out2_stat[,'cov_best']), 
               mean(out2_2[,'cov_best']),
               mean(out2_4[,'cov_best']),
               mean(out2_5[,'cov_best']),
               mean(out2_8[,'cov_best']),
               mean(out2[,'cov_best']),
               mean(out2_20[,'cov_best']),
               mean(out2_50[,'cov_best']),
               mean(out2_100[,'cov_best']),
               #
               mean(out3_stat[,'cov_best']), 
               mean(out3_2[,'cov_best']),
               mean(out3_4[,'cov_best']),
               mean(out3_5[,'cov_best']),
               mean(out3_8[,'cov_best']),
               mean(out3[,'cov_best']),
               mean(out3_20[,'cov_best']),
               mean(out3_50[,'cov_best']),
               mean(out3_100[,'cov_best']),
               #
               NA, 
               mean(out1_te_2[,'cov_best']),
               mean(out1_te_4[,'cov_best']),
               mean(out1_te_5[,'cov_best']),
               mean(out1_te_8[,'cov_best']),
               mean(out1_te[,'cov_best']),
               mean(out1_te_20[,'cov_best']),
               mean(out1_te_50[,'cov_best']),
               mean(out1_te_100[,'cov_best']),
               #
               NA, 
               mean(out2_te_2[,'cov_best']),
               mean(out2_te_4[,'cov_best']),
               mean(out2_te_5[,'cov_best']),
               mean(out2_te_8[,'cov_best']),
               mean(out2_te[,'cov_best']),
               mean(out2_te_20[,'cov_best']),
               mean(out2_te_50[,'cov_best']),
               mean(out2_te_100[,'cov_best']),
               #
               NA, 
               mean(out3_te_2[,'cov_best']),
               mean(out3_te_4[,'cov_best']),
               mean(out3_te_5[,'cov_best']),
               mean(out3_te_8[,'cov_best']),
               mean(out3_te[,'cov_best']),
               mean(out3_te_20[,'cov_best']),
               mean(out3_te_50[,'cov_best']),
               mean(out3_te_100[,'cov_best'])),
  cov_te = c(mean(out1_stat[,'cov_te']), 
             mean(out1_2[,'cov_te']),
             mean(out1_4[,'cov_te']),
             mean(out1_5[,'cov_te']),
             mean(out1_8[,'cov_te']),
             mean(out1[,'cov_te']),
             mean(out1_20[,'cov_te']),
             mean(out1_50[,'cov_te']),
             mean(out1_100[,'cov_te']),
             #
             mean(out2_stat[,'cov_te']), 
             mean(out2_2[,'cov_te']),
             mean(out2_4[,'cov_te']),
             mean(out2_5[,'cov_te']),
             mean(out2_8[,'cov_te']),
             mean(out2[,'cov_te']),
             mean(out2_20[,'cov_te']),
             mean(out2_50[,'cov_te']),
             mean(out2_100[,'cov_te']),
             #
             mean(out3_stat[,'cov_te']), 
             mean(out3_2[,'cov_te']),
             mean(out3_4[,'cov_te']),
             mean(out3_5[,'cov_te']),
             mean(out3_8[,'cov_te']),
             mean(out3[,'cov_te']),
             mean(out3_20[,'cov_te']),
             mean(out3_50[,'cov_te']),
             mean(out3_100[,'cov_te']),
             #
             NA, 
             mean(out1_te_2[,'cov_te']),
             mean(out1_te_4[,'cov_te']),
             mean(out1_te_5[,'cov_te']),
             mean(out1_te_8[,'cov_te']),
             mean(out1_te[,'cov_te']),
             mean(out1_te_20[,'cov_te']),
             mean(out1_te_50[,'cov_te']),
             mean(out1_te_100[,'cov_te']),
             #
             NA, 
             mean(out2_te_2[,'cov_te']),
             mean(out2_te_4[,'cov_te']),
             mean(out2_te_5[,'cov_te']),
             mean(out2_te_8[,'cov_te']),
             mean(out2_te[,'cov_te']),
             mean(out2_te_20[,'cov_te']),
             mean(out2_te_50[,'cov_te']),
             mean(out2_te_100[,'cov_te']),
             #
             NA, 
             mean(out3_te_2[,'cov_te']),
             mean(out3_te_4[,'cov_te']),
             mean(out3_te_5[,'cov_te']),
             mean(out3_te_8[,'cov_te']),
             mean(out3_te[,'cov_te']),
             mean(out3_te_20[,'cov_te']),
             mean(out3_te_50[,'cov_te']),
             mean(out3_te_100[,'cov_te']))
)

head(df)

# write_csv(df, 'tableD6.csv')


# * First batch results ----
# ** [Tables D8, D9] ----
df <- tibble(
  algorithm = rep(c('TS', 'CA'), each = 3*(length(first)+1) ),
  case = rep(rep(1:3, each = (length(first)+1)), times = 2),
  first_batch = rep(c(100, first), times = 2*3),
  correct = c(mean(out1[,'correct']),
              mean(out1_b_190[,'correct']), 
              mean(out1_b_280[,'correct']),
              mean(out1_b_370[,'correct']),
              mean(out1_b_460[,'correct']),
              mean(out1_b_550[,'correct']),
              mean(out1_b_640[,'correct']),
              mean(out1_b_730[,'correct']),
              mean(out1_b_820[,'correct']),
              mean(out1_b_910[,'correct']),
              #
              mean(out2[,'correct']),
              mean(out2_b_190[,'correct']), 
              mean(out2_b_280[,'correct']),
              mean(out2_b_370[,'correct']),
              mean(out2_b_460[,'correct']),
              mean(out2_b_550[,'correct']),
              mean(out2_b_640[,'correct']),
              mean(out2_b_730[,'correct']),
              mean(out2_b_820[,'correct']),
              mean(out2_b_910[,'correct']),
              #
              mean(out3[,'correct']),
              mean(out3_b_190[,'correct']), 
              mean(out3_b_280[,'correct']),
              mean(out3_b_370[,'correct']),
              mean(out3_b_460[,'correct']),
              mean(out3_b_550[,'correct']),
              mean(out3_b_640[,'correct']),
              mean(out3_b_730[,'correct']),
              mean(out3_b_820[,'correct']),
              mean(out3_b_910[,'correct']),
              #
              mean(out1_te[,'correct']),
              mean(out1_te_b_190[,'correct']), 
              mean(out1_te_b_280[,'correct']),
              mean(out1_te_b_370[,'correct']),
              mean(out1_te_b_460[,'correct']),
              mean(out1_te_b_550[,'correct']),
              mean(out1_te_b_640[,'correct']),
              mean(out1_te_b_730[,'correct']),
              mean(out1_te_b_820[,'correct']),
              mean(out1_te_b_910[,'correct']),
              #
              mean(out2_te[,'correct']),
              mean(out2_te_b_190[,'correct']), 
              mean(out2_te_b_280[,'correct']),
              mean(out2_te_b_370[,'correct']),
              mean(out2_te_b_460[,'correct']),
              mean(out2_te_b_550[,'correct']),
              mean(out2_te_b_640[,'correct']),
              mean(out2_te_b_730[,'correct']),
              mean(out2_te_b_820[,'correct']),
              mean(out2_te_b_910[,'correct']),
              #
              mean(out3_te[,'correct']),
              mean(out3_te_b_190[,'correct']), 
              mean(out3_te_b_280[,'correct']),
              mean(out3_te_b_370[,'correct']),
              mean(out3_te_b_460[,'correct']),
              mean(out3_te_b_550[,'correct']),
              mean(out3_te_b_640[,'correct']),
              mean(out3_te_b_730[,'correct']),
              mean(out3_te_b_820[,'correct']),
              mean(out3_te_b_910[,'correct'])),
  rmse_best = c(mean(out1[,'rmse_best']),
                mean(out1_b_190[,'rmse_best']), 
                mean(out1_b_280[,'rmse_best']),
                mean(out1_b_370[,'rmse_best']),
                mean(out1_b_460[,'rmse_best']),
                mean(out1_b_550[,'rmse_best']),
                mean(out1_b_640[,'rmse_best']),
                mean(out1_b_730[,'rmse_best']),
                mean(out1_b_820[,'rmse_best']),
                mean(out1_b_910[,'rmse_best']),
                #
                mean(out2[,'rmse_best']),
                mean(out2_b_190[,'rmse_best']), 
                mean(out2_b_280[,'rmse_best']),
                mean(out2_b_370[,'rmse_best']),
                mean(out2_b_460[,'rmse_best']),
                mean(out2_b_550[,'rmse_best']),
                mean(out2_b_640[,'rmse_best']),
                mean(out2_b_730[,'rmse_best']),
                mean(out2_b_820[,'rmse_best']),
                mean(out2_b_910[,'rmse_best']),
                #
                mean(out3[,'rmse_best']),
                mean(out3_b_190[,'rmse_best']), 
                mean(out3_b_280[,'rmse_best']),
                mean(out3_b_370[,'rmse_best']),
                mean(out3_b_460[,'rmse_best']),
                mean(out3_b_550[,'rmse_best']),
                mean(out3_b_640[,'rmse_best']),
                mean(out3_b_730[,'rmse_best']),
                mean(out3_b_820[,'rmse_best']),
                mean(out3_b_910[,'rmse_best']),
                #
                mean(out1_te[,'rmse_best']),
                mean(out1_te_b_190[,'rmse_best']), 
                mean(out1_te_b_280[,'rmse_best']),
                mean(out1_te_b_370[,'rmse_best']),
                mean(out1_te_b_460[,'rmse_best']),
                mean(out1_te_b_550[,'rmse_best']),
                mean(out1_te_b_640[,'rmse_best']),
                mean(out1_te_b_730[,'rmse_best']),
                mean(out1_te_b_820[,'rmse_best']),
                mean(out1_te_b_910[,'rmse_best']),
                #
                mean(out2_te[,'rmse_best']),
                mean(out2_te_b_190[,'rmse_best']), 
                mean(out2_te_b_280[,'rmse_best']),
                mean(out2_te_b_370[,'rmse_best']),
                mean(out2_te_b_460[,'rmse_best']),
                mean(out2_te_b_550[,'rmse_best']),
                mean(out2_te_b_640[,'rmse_best']),
                mean(out2_te_b_730[,'rmse_best']),
                mean(out2_te_b_820[,'rmse_best']),
                mean(out2_te_b_910[,'rmse_best']),
                #
                mean(out3_te[,'rmse_best']),
                mean(out3_te_b_190[,'rmse_best']), 
                mean(out3_te_b_280[,'rmse_best']),
                mean(out3_te_b_370[,'rmse_best']),
                mean(out3_te_b_460[,'rmse_best']),
                mean(out3_te_b_550[,'rmse_best']),
                mean(out3_te_b_640[,'rmse_best']),
                mean(out3_te_b_730[,'rmse_best']),
                mean(out3_te_b_820[,'rmse_best']),
                mean(out3_te_b_910[,'rmse_best'])),
  rmse_te = c(mean(out1[,'rmse_te']),
              mean(out1_b_190[,'rmse_te']), 
              mean(out1_b_280[,'rmse_te']),
              mean(out1_b_370[,'rmse_te']),
              mean(out1_b_460[,'rmse_te']),
              mean(out1_b_550[,'rmse_te']),
              mean(out1_b_640[,'rmse_te']),
              mean(out1_b_730[,'rmse_te']),
              mean(out1_b_820[,'rmse_te']),
              mean(out1_b_910[,'rmse_te']),
              #
              mean(out2[,'rmse_te']),
              mean(out2_b_190[,'rmse_te']), 
              mean(out2_b_280[,'rmse_te']),
              mean(out2_b_370[,'rmse_te']),
              mean(out2_b_460[,'rmse_te']),
              mean(out2_b_550[,'rmse_te']),
              mean(out2_b_640[,'rmse_te']),
              mean(out2_b_730[,'rmse_te']),
              mean(out2_b_820[,'rmse_te']),
              mean(out2_b_910[,'rmse_te']),
              #
              mean(out3[,'rmse_te']),
              mean(out3_b_190[,'rmse_te']), 
              mean(out3_b_280[,'rmse_te']),
              mean(out3_b_370[,'rmse_te']),
              mean(out3_b_460[,'rmse_te']),
              mean(out3_b_550[,'rmse_te']),
              mean(out3_b_640[,'rmse_te']),
              mean(out3_b_730[,'rmse_te']),
              mean(out3_b_820[,'rmse_te']),
              mean(out3_b_910[,'rmse_te']),
              #
              mean(out1_te[,'rmse_te']),
              mean(out1_te_b_190[,'rmse_te']), 
              mean(out1_te_b_280[,'rmse_te']),
              mean(out1_te_b_370[,'rmse_te']),
              mean(out1_te_b_460[,'rmse_te']),
              mean(out1_te_b_550[,'rmse_te']),
              mean(out1_te_b_640[,'rmse_te']),
              mean(out1_te_b_730[,'rmse_te']),
              mean(out1_te_b_820[,'rmse_te']),
              mean(out1_te_b_910[,'rmse_te']),
              #
              mean(out2_te[,'rmse_te']),
              mean(out2_te_b_190[,'rmse_te']), 
              mean(out2_te_b_280[,'rmse_te']),
              mean(out2_te_b_370[,'rmse_te']),
              mean(out2_te_b_460[,'rmse_te']),
              mean(out2_te_b_550[,'rmse_te']),
              mean(out2_te_b_640[,'rmse_te']),
              mean(out2_te_b_730[,'rmse_te']),
              mean(out2_te_b_820[,'rmse_te']),
              mean(out2_te_b_910[,'rmse_te']),
              #
              mean(out3_te[,'rmse_te']),
              mean(out3_te_b_190[,'rmse_te']), 
              mean(out3_te_b_280[,'rmse_te']),
              mean(out3_te_b_370[,'rmse_te']),
              mean(out3_te_b_460[,'rmse_te']),
              mean(out3_te_b_550[,'rmse_te']),
              mean(out3_te_b_640[,'rmse_te']),
              mean(out3_te_b_730[,'rmse_te']),
              mean(out3_te_b_820[,'rmse_te']),
              mean(out3_te_b_910[,'rmse_te'])),
  cov_best = c(mean(out1[,'cov_best']),
               mean(out1_b_190[,'cov_best']), 
               mean(out1_b_280[,'cov_best']),
               mean(out1_b_370[,'cov_best']),
               mean(out1_b_460[,'cov_best']),
               mean(out1_b_550[,'cov_best']),
               mean(out1_b_640[,'cov_best']),
               mean(out1_b_730[,'cov_best']),
               mean(out1_b_820[,'cov_best']),
               mean(out1_b_910[,'cov_best']),
               #
               mean(out2[,'cov_best']),
               mean(out2_b_190[,'cov_best']), 
               mean(out2_b_280[,'cov_best']),
               mean(out2_b_370[,'cov_best']),
               mean(out2_b_460[,'cov_best']),
               mean(out2_b_550[,'cov_best']),
               mean(out2_b_640[,'cov_best']),
               mean(out2_b_730[,'cov_best']),
               mean(out2_b_820[,'cov_best']),
               mean(out2_b_910[,'cov_best']),
               #
               mean(out3[,'cov_best']),
               mean(out3_b_190[,'cov_best']), 
               mean(out3_b_280[,'cov_best']),
               mean(out3_b_370[,'cov_best']),
               mean(out3_b_460[,'cov_best']),
               mean(out3_b_550[,'cov_best']),
               mean(out3_b_640[,'cov_best']),
               mean(out3_b_730[,'cov_best']),
               mean(out3_b_820[,'cov_best']),
               mean(out3_b_910[,'cov_best']),
               #
               mean(out1_te[,'cov_best']),
               mean(out1_te_b_190[,'cov_best']), 
               mean(out1_te_b_280[,'cov_best']),
               mean(out1_te_b_370[,'cov_best']),
               mean(out1_te_b_460[,'cov_best']),
               mean(out1_te_b_550[,'cov_best']),
               mean(out1_te_b_640[,'cov_best']),
               mean(out1_te_b_730[,'cov_best']),
               mean(out1_te_b_820[,'cov_best']),
               mean(out1_te_b_910[,'cov_best']),
               #
               mean(out2_te[,'cov_best']),
               mean(out2_te_b_190[,'cov_best']), 
               mean(out2_te_b_280[,'cov_best']),
               mean(out2_te_b_370[,'cov_best']),
               mean(out2_te_b_460[,'cov_best']),
               mean(out2_te_b_550[,'cov_best']),
               mean(out2_te_b_640[,'cov_best']),
               mean(out2_te_b_730[,'cov_best']),
               mean(out2_te_b_820[,'cov_best']),
               mean(out2_te_b_910[,'cov_best']),
               #
               mean(out3_te[,'cov_best']),
               mean(out3_te_b_190[,'cov_best']), 
               mean(out3_te_b_280[,'cov_best']),
               mean(out3_te_b_370[,'cov_best']),
               mean(out3_te_b_460[,'cov_best']),
               mean(out3_te_b_550[,'cov_best']),
               mean(out3_te_b_640[,'cov_best']),
               mean(out3_te_b_730[,'cov_best']),
               mean(out3_te_b_820[,'cov_best']),
               mean(out3_te_b_910[,'cov_best'])),
  cov_te = c(mean(out1[,'cov_te']),
             mean(out1_b_190[,'cov_te']), 
             mean(out1_b_280[,'cov_te']),
             mean(out1_b_370[,'cov_te']),
             mean(out1_b_460[,'cov_te']),
             mean(out1_b_550[,'cov_te']),
             mean(out1_b_640[,'cov_te']),
             mean(out1_b_730[,'cov_te']),
             mean(out1_b_820[,'cov_te']),
             mean(out1_b_910[,'cov_te']),
             #
             mean(out2[,'cov_te']),
             mean(out2_b_190[,'cov_te']), 
             mean(out2_b_280[,'cov_te']),
             mean(out2_b_370[,'cov_te']),
             mean(out2_b_460[,'cov_te']),
             mean(out2_b_550[,'cov_te']),
             mean(out2_b_640[,'cov_te']),
             mean(out2_b_730[,'cov_te']),
             mean(out2_b_820[,'cov_te']),
             mean(out2_b_910[,'cov_te']),
             #
             mean(out3[,'cov_te']),
             mean(out3_b_190[,'cov_te']), 
             mean(out3_b_280[,'cov_te']),
             mean(out3_b_370[,'cov_te']),
             mean(out3_b_460[,'cov_te']),
             mean(out3_b_550[,'cov_te']),
             mean(out3_b_640[,'cov_te']),
             mean(out3_b_730[,'cov_te']),
             mean(out3_b_820[,'cov_te']),
             mean(out3_b_910[,'cov_te']),
             #
             mean(out1_te[,'cov_te']),
             mean(out1_te_b_190[,'cov_te']), 
             mean(out1_te_b_280[,'cov_te']),
             mean(out1_te_b_370[,'cov_te']),
             mean(out1_te_b_460[,'cov_te']),
             mean(out1_te_b_550[,'cov_te']),
             mean(out1_te_b_640[,'cov_te']),
             mean(out1_te_b_730[,'cov_te']),
             mean(out1_te_b_820[,'cov_te']),
             mean(out1_te_b_910[,'cov_te']),
             #
             mean(out2_te[,'cov_te']),
             mean(out2_te_b_190[,'cov_te']), 
             mean(out2_te_b_280[,'cov_te']),
             mean(out2_te_b_370[,'cov_te']),
             mean(out2_te_b_460[,'cov_te']),
             mean(out2_te_b_550[,'cov_te']),
             mean(out2_te_b_640[,'cov_te']),
             mean(out2_te_b_730[,'cov_te']),
             mean(out2_te_b_820[,'cov_te']),
             mean(out2_te_b_910[,'cov_te']),
             #
             mean(out3_te[,'cov_te']),
             mean(out3_te_b_190[,'cov_te']), 
             mean(out3_te_b_280[,'cov_te']),
             mean(out3_te_b_370[,'cov_te']),
             mean(out3_te_b_460[,'cov_te']),
             mean(out3_te_b_550[,'cov_te']),
             mean(out3_te_b_640[,'cov_te']),
             mean(out3_te_b_730[,'cov_te']),
             mean(out3_te_b_820[,'cov_te']),
             mean(out3_te_b_910[,'cov_te']))
)

head(df)

# write_csv(df, 'tableD8-D9.csv')


# * Range results ----
# ** [Table D7] ----
df <- tibble(
  algorithm = rep(c('TS', 'Static', 'CA'), each = 11),
  case = rep(10:20, times = 3),
  correct = c(mean(out_10[,'correct']),
              mean(out2[,'correct']),
              mean(out_12[,'correct']),
              mean(out_13[,'correct']),
              mean(out_14[,'correct']),
              mean(out_15[,'correct']),
              mean(out_16[,'correct']),
              mean(out_17[,'correct']),
              mean(out_18[,'correct']),
              mean(out_19[,'correct']),
              mean(out1[,'correct']),
              #
              mean(out_stat_10[,'correct']),
              mean(out2_stat[,'correct']),
              mean(out_stat_12[,'correct']),
              mean(out_stat_13[,'correct']),
              mean(out_stat_14[,'correct']),
              mean(out_stat_15[,'correct']),
              mean(out_stat_16[,'correct']),
              mean(out_stat_17[,'correct']),
              mean(out_stat_18[,'correct']),
              mean(out_stat_19[,'correct']),
              mean(out1_stat[,'correct']),
              #
              mean(out_te_10[,'correct']),
              mean(out2_te[,'correct']),
              mean(out_te_12[,'correct']),
              mean(out_te_13[,'correct']),
              mean(out_te_14[,'correct']),
              mean(out_te_15[,'correct']),
              mean(out_te_16[,'correct']),
              mean(out_te_17[,'correct']),
              mean(out_te_18[,'correct']),
              mean(out_te_19[,'correct']),
              mean(out1_te[,'correct'])),
  #
  rmse_best = c(mean(out_10[,'rmse_best']),
                mean(out2[,'rmse_best']),
                mean(out_12[,'rmse_best']),
                mean(out_13[,'rmse_best']),
                mean(out_14[,'rmse_best']),
                mean(out_15[,'rmse_best']),
                mean(out_16[,'rmse_best']),
                mean(out_17[,'rmse_best']),
                mean(out_18[,'rmse_best']),
                mean(out_19[,'rmse_best']),
                mean(out1[,'rmse_best']),
                #
                mean(out_stat_10[,'rmse_best']),
                mean(out2_stat[,'rmse_best']),
                mean(out_stat_12[,'rmse_best']),
                mean(out_stat_13[,'rmse_best']),
                mean(out_stat_14[,'rmse_best']),
                mean(out_stat_15[,'rmse_best']),
                mean(out_stat_16[,'rmse_best']),
                mean(out_stat_17[,'rmse_best']),
                mean(out_stat_18[,'rmse_best']),
                mean(out_stat_19[,'rmse_best']),
                mean(out1_stat[,'rmse_best']),
                #
                mean(out_te_10[,'rmse_best']),
                mean(out2_te[,'rmse_best']),
                mean(out_te_12[,'rmse_best']),
                mean(out_te_13[,'rmse_best']),
                mean(out_te_14[,'rmse_best']),
                mean(out_te_15[,'rmse_best']),
                mean(out_te_16[,'rmse_best']),
                mean(out_te_17[,'rmse_best']),
                mean(out_te_18[,'rmse_best']),
                mean(out_te_19[,'rmse_best']),
                mean(out1_te[,'rmse_best'])),
  rmse_te = c(mean(out_10[,'rmse_te']),
              mean(out2[,'rmse_te']),
              mean(out_12[,'rmse_te']),
              mean(out_13[,'rmse_te']),
              mean(out_14[,'rmse_te']),
              mean(out_15[,'rmse_te']),
              mean(out_16[,'rmse_te']),
              mean(out_17[,'rmse_te']),
              mean(out_18[,'rmse_te']),
              mean(out_19[,'rmse_te']),
              mean(out1[,'rmse_te']),
              #
              mean(out_stat_10[,'rmse_te']),
              mean(out2_stat[,'rmse_te']),
              mean(out_stat_12[,'rmse_te']),
              mean(out_stat_13[,'rmse_te']),
              mean(out_stat_14[,'rmse_te']),
              mean(out_stat_15[,'rmse_te']),
              mean(out_stat_16[,'rmse_te']),
              mean(out_stat_17[,'rmse_te']),
              mean(out_stat_18[,'rmse_te']),
              mean(out_stat_19[,'rmse_te']),
              mean(out1_stat[,'rmse_te']),
              #
              mean(out_te_10[,'rmse_te']),
              mean(out2_te[,'rmse_te']),
              mean(out_te_12[,'rmse_te']),
              mean(out_te_13[,'rmse_te']),
              mean(out_te_14[,'rmse_te']),
              mean(out_te_15[,'rmse_te']),
              mean(out_te_16[,'rmse_te']),
              mean(out_te_17[,'rmse_te']),
              mean(out_te_18[,'rmse_te']),
              mean(out_te_19[,'rmse_te']),
              mean(out1_te[,'rmse_te'])),
  #
  cov_best = c(mean(out_10[,'cov_best']),
               mean(out2[,'cov_best']),
               mean(out_12[,'cov_best']),
               mean(out_13[,'cov_best']),
               mean(out_14[,'cov_best']),
               mean(out_15[,'cov_best']),
               mean(out_16[,'cov_best']),
               mean(out_17[,'cov_best']),
               mean(out_18[,'cov_best']),
               mean(out_19[,'cov_best']),
               mean(out1[,'cov_best']),
               #
               mean(out_stat_10[,'cov_best']),
               mean(out2_stat[,'cov_best']),
               mean(out_stat_12[,'cov_best']),
               mean(out_stat_13[,'cov_best']),
               mean(out_stat_14[,'cov_best']),
               mean(out_stat_15[,'cov_best']),
               mean(out_stat_16[,'cov_best']),
               mean(out_stat_17[,'cov_best']),
               mean(out_stat_18[,'cov_best']),
               mean(out_stat_19[,'cov_best']),
               mean(out1_stat[,'cov_best']),
               #
               mean(out_te_10[,'cov_best']),
               mean(out2_te[,'cov_best']),
               mean(out_te_12[,'cov_best']),
               mean(out_te_13[,'cov_best']),
               mean(out_te_14[,'cov_best']),
               mean(out_te_15[,'cov_best']),
               mean(out_te_16[,'cov_best']),
               mean(out_te_17[,'cov_best']),
               mean(out_te_18[,'cov_best']),
               mean(out_te_19[,'cov_best']),
               mean(out1_te[,'cov_best'])),
  #
  cov_te = c(mean(out_10[,'cov_te']),
             mean(out2[,'cov_te']),
             mean(out_12[,'cov_te']),
             mean(out_13[,'cov_te']),
             mean(out_14[,'cov_te']),
             mean(out_15[,'cov_te']),
             mean(out_16[,'cov_te']),
             mean(out_17[,'cov_te']),
             mean(out_18[,'cov_te']),
             mean(out_19[,'cov_te']),
             mean(out1[,'cov_te']),
             #
             mean(out_stat_10[,'cov_te']),
             mean(out2_stat[,'cov_te']),
             mean(out_stat_12[,'cov_te']),
             mean(out_stat_13[,'cov_te']),
             mean(out_stat_14[,'cov_te']),
             mean(out_stat_15[,'cov_te']),
             mean(out_stat_16[,'cov_te']),
             mean(out_stat_17[,'cov_te']),
             mean(out_stat_18[,'cov_te']),
             mean(out_stat_19[,'cov_te']),
             mean(out1_stat[,'cov_te']),
             #
             mean(out_te_10[,'cov_te']),
             mean(out2_te[,'cov_te']),
             mean(out_te_12[,'cov_te']),
             mean(out_te_13[,'cov_te']),
             mean(out_te_14[,'cov_te']),
             mean(out_te_15[,'cov_te']),
             mean(out_te_16[,'cov_te']),
             mean(out_te_17[,'cov_te']),
             mean(out_te_18[,'cov_te']),
             mean(out_te_19[,'cov_te']),
             mean(out1_te[,'cov_te']))
)

head(df)

# write_csv(df, 'tableD7.csv')

# Figures ----
# * Frequency of updating ----
# ** [Figures D7, D8] ----

out1_te_df <-
  bind_rows(
    `1` = data.frame(out1_stat),
    `2` = data.frame(out1_te_2),
    `4` = data.frame(out1_te_4),
    `5` = data.frame(out1_te_5),
    `8` = data.frame(out1_te_8),
    `10` = data.frame(out1_te),
    `20` = data.frame(out1_te_20),
    `50` = data.frame(out1_te_50),
    `100` = data.frame(out1_te_100),
    .id = 'num_batches'
  )

out2_te_df <-
  bind_rows(
    `1` = data.frame(out2_stat),
    `2` = data.frame(out2_te_2),
    `4` = data.frame(out2_te_4),
    `5` = data.frame(out2_te_5),
    `8` = data.frame(out2_te_8),
    `10` = data.frame(out2_te),
    `20` = data.frame(out2_te_20),
    `50` = data.frame(out2_te_50),
    `100` = data.frame(out2_te_100),
    .id = 'num_batches'
  )


out3_te_df <-
  bind_rows(
    `1` = data.frame(out3_stat),
    `2` = data.frame(out3_te_2),
    `4` = data.frame(out3_te_4),
    `5` = data.frame(out3_te_5),
    `8` = data.frame(out3_te_8),
    `10` = data.frame(out3_te),
    `20` = data.frame(out3_te_20),
    `50` = data.frame(out3_te_50),
    `100` = data.frame(out3_te_100),
    .id = 'num_batches'
  )

out1_df <-
  bind_rows(
    `1` = data.frame(out1_stat),
    `2` = data.frame(out1_2),
    `4` = data.frame(out1_4),
    `5` = data.frame(out1_5),
    `8` = data.frame(out1_8),
    `10` = data.frame(out1),
    `20` = data.frame(out1_20),
    `50` = data.frame(out1_50),
    `100` = data.frame(out1_100),
    .id = 'num_batches'
  )

out2_df <-
  bind_rows(
    `1` = data.frame(out2_stat),
    `2` = data.frame(out2_2),
    `4` = data.frame(out2_4),
    `5` = data.frame(out2_5),
    `8` = data.frame(out2_8),
    `10` = data.frame(out2),
    `20` = data.frame(out2_20),
    `50` = data.frame(out2_50),
    `100` = data.frame(out2_100),
    .id = 'num_batches'
  )

out3_df <-
  bind_rows(
    `1` = data.frame(out3_stat),
    `2` = data.frame(out3_2),
    `4` = data.frame(out3_4),
    `5` = data.frame(out3_5),
    `8` = data.frame(out3_8),
    `10` = data.frame(out3),
    `20` = data.frame(out3_20),
    `50` = data.frame(out3_50),
    `100` = data.frame(out3_100),
    .id = 'num_batches'
  )


mega_df <-
  bind_rows(
    `1: Clear winner; Control-Augmented` = out1_te_df,
    `2: No clear winner; Control-Augmented` = out2_te_df,
    `3: Competing second-best; Control-Augmented` = out3_te_df,
    `1: Clear winner; Thompson Sampling` = out1_df,
    `2: No clear winner; Thompson Sampling` = out2_df,
    `3: Competing second-best; Thompson Sampling` = out3_df,
    .id = 'sim'
  )




gg_df <-
  mega_df %>%
  mutate(num_batches = as.numeric(num_batches)) %>%
  separate(sim, c('Scenario', 'Algorithm'), sep = '; ') %>%
  group_by(num_batches, Scenario, Algorithm) %>%
  summarize(`ATE of best arm` = mean(rmse_te),
            `Success rate of best arm` = mean(rmse_best)) %>%
  pivot_longer(c('ATE of best arm', 'Success rate of best arm'), 
               names_to = 'variable', values_to = 'value')

g <- 
  ggplot(gg_df,aes(num_batches, value, group = Scenario, color = Scenario, shape = Scenario,
                   linetype = Scenario)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_grey() +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100)) +
  expand_limits(y = c(.02, .06)) +
  facet_grid(variable ~ Algorithm, scales = 'free_y') +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(3, 'lines'),
        legend.title = element_blank(),
        strip.background = element_blank()) +
  labs(x = 'Number of batches (log scale)',
       y = 'Root mean squared error')

g

# ggsave('figureD8.pdf', g, width = 6.5, height = 6.5)


gg_df_stats <-
  mega_df %>%
  mutate(num_batches = as.numeric(num_batches)) %>%
  separate(sim, c('Scenario', 'Algorithm'), sep = '; ') %>%
  group_by(num_batches, Algorithm, Scenario) %>%
  summarize(`Coverage of ATE` = mean(cov_te),
            `Coverage of best arm` = mean(cov_best),
            `Best arm selection` = mean(correct)) %>%
  pivot_longer(c('Coverage of ATE',
                 'Coverage of best arm',
                 'Best arm selection'), 
               names_to = 'variable', values_to = 'value')

dummy <- filter(gg_df_stats, (num_batches == 1|num_batches == 100) & 
                  Algorithm == 'Thompson Sampling', 
                variable == 'Best arm selection') %>% 
  ungroup() %>% 
  mutate(value = as.vector(outer(c(.875, .15, .6), c(0, .12), '+')) )

g <- 
  ggplot(filter(gg_df_stats, variable == 'Best arm selection'), 
         aes(num_batches, value, group = Algorithm, color = Algorithm, 
             linetype = Algorithm, shape = Algorithm)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_grey() +
  facet_wrap(facets = vars(Scenario) , nrow = 1, scales = 'free_y') +
  geom_blank(data = dummy) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100)) +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = 'none',
    legend.title = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(.5, "lines")) +
  labs(x = '',
       y = 'Best arm selection')

g

# ggsave('figureD7p1.pdf', g, width = 6.5, height = 3)


gg_lines <- data.frame(variable = c('Coverage of ATE', 'Coverage of best arm'), 
                       Y = c(0.95, 0.95))

g <- 
  ggplot(filter(gg_df_stats, variable !='Best arm selection'), 
         aes(num_batches, value, group = Algorithm, color = Algorithm, 
                          linetype = Algorithm, shape = Algorithm)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_grey() +
  facet_grid(facets = variable ~ Scenario) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100)) +
  geom_hline(data = gg_lines, aes(yintercept = Y), 
             lty = 'dashed') +
  expand_limits(y = c(0.5, 1.0))+
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(3, 'lines'),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines")) +
  labs(x = 'Number of batches (log scale)',
       y = 'Arm coverage')

g

# ggsave('figureD7p2.pdf', g, width = 6.5, height = 4)


# * First batch size ----
# ** [Figures D10, D11] ----

out1_te_df <-
  bind_rows(
    `100` = data.frame(out1_te),
    `190` = data.frame(out1_te_b_190),
    `280` = data.frame(out1_te_b_280),
    `370` = data.frame(out1_te_b_370),
    `460` = data.frame(out1_te_b_460),
    `550` = data.frame(out1_te_b_550),
    `640` = data.frame(out1_te_b_640),
    `730` = data.frame(out1_te_b_730),
    `820` = data.frame(out1_te_b_820),
    `910` = data.frame(out1_te_b_910),
    `1000` = data.frame(out1_stat),
    .id = 'first_batch'
  )

out2_te_df <-
  bind_rows(
    `100` = data.frame(out2_te),
    `190` = data.frame(out2_te_b_190),
    `280` = data.frame(out2_te_b_280),
    `370` = data.frame(out2_te_b_370),
    `460` = data.frame(out2_te_b_460),
    `550` = data.frame(out2_te_b_550),
    `640` = data.frame(out2_te_b_640),
    `730` = data.frame(out2_te_b_730),
    `820` = data.frame(out2_te_b_820),
    `910` = data.frame(out2_te_b_910),
    `1000` = data.frame(out2_stat),
    .id = 'first_batch'
  )


out3_te_df <-
  bind_rows(
    `100` = data.frame(out3_te),
    `190` = data.frame(out3_te_b_190),
    `280` = data.frame(out3_te_b_280),
    `370` = data.frame(out3_te_b_370),
    `460` = data.frame(out3_te_b_460),
    `550` = data.frame(out3_te_b_550),
    `640` = data.frame(out3_te_b_640),
    `730` = data.frame(out3_te_b_730),
    `820` = data.frame(out3_te_b_820),
    `910` = data.frame(out3_te_b_910),
    `1000` = data.frame(out3_stat),
    .id = 'first_batch'
  )

out1_df <-
  bind_rows(
    `100` = data.frame(out1),
    `190` = data.frame(out1_b_190),
    `280` = data.frame(out1_b_280),
    `370` = data.frame(out1_b_370),
    `460` = data.frame(out1_b_460),
    `550` = data.frame(out1_b_550),
    `640` = data.frame(out1_b_640),
    `730` = data.frame(out1_b_730),
    `820` = data.frame(out1_b_820),
    `910` = data.frame(out1_b_910),
    `1000` = data.frame(out1_stat),
    .id = 'first_batch'
  )

out2_df <-
  bind_rows(
    `100` = data.frame(out2),
    `190` = data.frame(out2_b_190),
    `280` = data.frame(out2_b_280),
    `370` = data.frame(out2_b_370),
    `460` = data.frame(out2_b_460),
    `550` = data.frame(out2_b_550),
    `640` = data.frame(out2_b_640),
    `730` = data.frame(out2_b_730),
    `820` = data.frame(out2_b_820),
    `910` = data.frame(out2_b_910),
    `1000` = data.frame(out2_stat),
    .id = 'first_batch'
  )


out3_df <-
  bind_rows(
    `100` = data.frame(out3),
    `190` = data.frame(out3_b_190),
    `280` = data.frame(out3_b_280),
    `370` = data.frame(out3_b_370),
    `460` = data.frame(out3_b_460),
    `550` = data.frame(out3_b_550),
    `640` = data.frame(out3_b_640),
    `730` = data.frame(out3_b_730),
    `820` = data.frame(out3_b_820),
    `910` = data.frame(out3_b_910),
    `1000` = data.frame(out3_stat),
    .id = 'first_batch'
  )


mega_df <-
  bind_rows(
    `1: Clear winner; Control-Augmented` = out1_te_df,
    `2: No clear winner; Control-Augmented` = out2_te_df,
    `3: Competing second-best; Control-Augmented` = out3_te_df,
    `1: Clear winner; Thompson Sampling` = out1_df,
    `2: No clear winner; Thompson Sampling` = out2_df,
    `3: Competing second-best; Thompson Sampling` = out3_df,
    .id = 'sim'
  )



gg_df <-
  mega_df %>%
  mutate(first_batch = as.numeric(first_batch)) %>%
  separate(sim, c('Scenario', 'Algorithm'), sep = '; ') %>%
  group_by(first_batch, Scenario, Algorithm) %>%
  summarize(`ATE of best arm` = mean(rmse_te),
            `Success rate of best arm` = mean(rmse_best)) %>%
  pivot_longer(c('ATE of best arm', 'Success rate of best arm'), 
               names_to = 'variable', values_to = 'value')

g <- 
  ggplot(gg_df,aes(first_batch, value, group = Scenario, 
                   color = Scenario, shape = Scenario,
                   linetype = Scenario)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_grey() +
  expand_limits(y = c(.02, .06)) +
  facet_grid(variable ~ Algorithm, scales = 'free_y') +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(3, 'lines'),
        legend.title = element_blank(),
        strip.background = element_blank()) +
  labs(x = 'First batch size',
       y = 'Root mean squared error')

g

# ggsave('figureD11.pdf', g, width = 6.5, height = 6.5)


gg_df_stats <-
  mega_df %>%
  mutate(first_batch = as.numeric(first_batch)) %>%
  separate(sim, c('Scenario', 'Algorithm'), sep = '; ') %>%
  group_by(first_batch, Algorithm, Scenario) %>%
  summarize(`Coverage of ATE` = mean(cov_te),
            `Coverage of best arm` = mean(cov_best),
            `Best arm selection` = mean(correct)) %>%
  pivot_longer(c('Coverage of ATE',
                 'Coverage of best arm',
                 'Best arm selection'), 
               names_to = 'variable', values_to = 'value')

dummy <- filter(gg_df_stats, (first_batch == 100|first_batch == 1000) & 
                  Algorithm == 'Thompson Sampling', 
                variable == 'Best arm selection') %>% 
  ungroup() %>% 
  mutate(value = as.vector(outer(c(.875, .15, .6), c(0, .12), '+')) )


g <- 
  ggplot(filter(gg_df_stats, variable == 'Best arm selection'), 
         aes(first_batch, value, group = Algorithm, color = Algorithm, 
             linetype = Algorithm, shape = Algorithm)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_grey() +
  facet_wrap(facets = vars(Scenario), nrow = 1, scales = 'free_y') +
  geom_blank(data = dummy) +
  theme_bw() +
  theme(
    legend.position = 'none',
    strip.background = element_blank(),
    panel.spacing = unit(.5, "lines")) +
  labs(x = '',
       y = 'Best arm selection')

g

# ggsave('figureD10p1.pdf', g, width = 6.5, height = 3)


gg_lines <- data.frame(variable = c('Coverage of ATE', 'Coverage of best arm'), 
                       Y = c(0.95, 0.95))

g <- 
  ggplot(filter(gg_df_stats, variable !='Best arm selection'), 
         aes(first_batch, value, group = Algorithm, color = Algorithm, 
             linetype = Algorithm, shape = Algorithm)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_grey() +
  facet_grid(facets = variable ~ Scenario) +
  geom_hline(data = gg_lines, aes(yintercept = Y), 
             lty = 'dashed') +
  expand_limits(y = c(0.5, 1.0))+
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(3, 'lines'),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines")) +
  labs(x = 'First batch',
       y = 'Arm coverage')

g

# ggsave('figureD10p2.pdf', g, width = 6.5, height = 4)


# * Range ----
# ** [Figure D9] ----
out_df <-
  bind_rows(
    `.1` = data.frame(out_10),
    `.11` = data.frame(out2),
    `.12` = data.frame(out_12),
    `.13` = data.frame(out_13),
    `.14` = data.frame(out_14),
    `.15` = data.frame(out_15),
    `.16` = data.frame(out_16),
    `.17` = data.frame(out_17),
    `.18` = data.frame(out_18),
    `.19` = data.frame(out_19),
    `.2` = data.frame(out1),
    .id = 'best_arm'
  )

out_stat_df <-
  bind_rows(
    `.1` = data.frame(out_stat_10),
    `.11` = data.frame(out2_stat),
    `.12` = data.frame(out_stat_12),
    `.13` = data.frame(out_stat_13),
    `.14` = data.frame(out_stat_14),
    `.15` = data.frame(out_stat_15),
    `.16` = data.frame(out_stat_16),
    `.17` = data.frame(out_stat_17),
    `.18` = data.frame(out_stat_18),
    `.19` = data.frame(out_stat_19),
    `.2` = data.frame(out1_stat),
    .id = 'best_arm'
  )

out_te_df <-
  bind_rows(
    `.1` = data.frame(out_te_10),
    `.11` = data.frame(out2_te),
    `.12` = data.frame(out_te_12),
    `.13` = data.frame(out_te_13),
    `.14` = data.frame(out_te_14),
    `.15` = data.frame(out_te_15),
    `.16` = data.frame(out_te_16),
    `.17` = data.frame(out_te_17),
    `.18` = data.frame(out_te_18),
    `.19` = data.frame(out_te_19),
    `.2` = data.frame(out1_te),
    .id = 'best_arm'
  )


mega_df <-
  bind_rows(
    `Control-Augmented` = out_te_df,
    `Thompson Sampling` = out_df,
    `Static` = out_stat_df,
    .id = 'Algorithm'
  )




gg_df_stats <-
  mega_df %>%
  mutate(best_arm = as.numeric(best_arm)) %>%
  group_by(best_arm, Algorithm) %>%
  summarize(`Coverage of ATE` = mean(cov_te),
            `Coverage of best arm` = mean(cov_best),
            `Best arm selection` = mean(correct)) %>%
  pivot_longer(c('Coverage of ATE',
                 'Coverage of best arm',
                 'Best arm selection'), 
               names_to = 'variable', values_to = 'value')

gg_df_rmse <-
  mega_df %>%
  mutate(best_arm = as.numeric(best_arm)) %>%
  group_by(best_arm, Algorithm) %>%
  summarize(`ATE of best arm` = mean(rmse_te),
            `Success rate of best arm` = mean(rmse_best)) %>%
  pivot_longer(c('ATE of best arm', 
                 'Success rate of best arm'), 
               names_to = 'variable', values_to = 'value')

gg_lines <- data.frame(variable = unique(gg_df_stats$variable), 
                       Y = c(0.95, 0.95, NA))

g <- 
  ggplot(gg_df_stats, aes(best_arm, value, group = Algorithm, color = Algorithm, 
                          linetype = Algorithm, shape = Algorithm)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_grey() +
  facet_wrap(facets = vars(variable), nrow = 1) +
  scale_x_continuous(breaks = seq(.1, .2, .02)) + 
  geom_hline(data = gg_lines, aes(yintercept = Y), 
             lty = 'dashed') +
  expand_limits(y = c(0.8, 1.0))+
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = 'none',
        legend.key.width = unit(3, 'lines'),
        legend.title = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines")) +
  labs(x = '',
       y = 'Arm selection and coverage')

g

# ggsave('figureD9p1.pdf', g, width = 6.5, height = 3.5)

g <- 
  ggplot(gg_df_rmse, aes(best_arm, value, group = Algorithm, color = Algorithm, 
                         linetype = Algorithm, shape = Algorithm)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_grey() +
  facet_wrap(facets = vars(variable), nrow = 1) +
  scale_x_continuous(breaks = seq(.1, .2, .02)) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(3, 'lines'),
        legend.title = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines")) +
  labs(x = 'Value of best arm',
       y = 'Root mean squared error')

g

# ggsave('figureD9p2.pdf', g, width = 6.5, height = 4)
