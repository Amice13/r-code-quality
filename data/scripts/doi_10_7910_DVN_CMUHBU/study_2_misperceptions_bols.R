## Adaptive Experimental Design: Prospects and Applications in Political Science
## Code for Study 2: misperceptions, batched OLS
## Produces table of batched OLS estimates in Footnote 15

version


set.seed(95126)
iter <- 1e7 # iterations

# Set options ----
source('utils.R')

mturk_bandit <- read_rds('study_2_clean.rds')

mturk_long <- mturk_bandit %>% 
  gather(key = 'out', value = 'Y', c('Y_deficit', 'Y_unemp', 'Y_nfi'))%>% 
  mutate(Y_complete = case_when(
    (out == 'Y_deficit' & Y_deficit_no_response == 1) ~ NA_real_,
    (out == 'Y_unemp' & Y_unemp_no_response == 1) ~ NA_real_,
    (out == 'Y_nfi' & Y_nfi_no_response == 1) ~ NA_real_,
    TRUE ~ Y))


# Group means for each batch 
# ** [study_2_bols.csv] ----
bols_results <- list()

for(i in 1:10){
  fit_d <- lm_robust(Y ~ Z, 
                     data = filter(mturk_long, pid == 'democrat', batch == i), 
                     cluster = id)
  
  bols_results[[paste('d', i, sep = '_')]] <- fit_d
  
  fit_r <- lm_robust(Y ~ Z, 
                     data = filter(mturk_long, pid == 'republican', batch == i), 
                     cluster = id)
  
  bols_results[[paste('r', i, sep = '_')]] <- fit_r
  
}

delta_t <- sigma_t <- df_t <- matrix(NA, ncol = 2, nrow = 10)
colnames(delta_t) <- colnames(sigma_t) <- colnames(df_t) <- c('democrat', 
                                                              'republican')


for(ps in c('democrat', 'republican')){
  for(i in 1:10){
    regid <- paste0(substr(ps, 1, 1), '_', i)
    delta_t[i, ps] <- bols_results[[regid]]$coefficients['Zlottery']
    sigma_t[i, ps] <- bols_results[[regid]]$std.error['Zlottery']
    df_t[i, ps] <- bols_results[[regid]]$df['Zlottery']
  }
}

T_stats <- 1/sqrt(nrow(delta_t))*colSums(delta_t/sigma_t)


null_d <- sort(rowSums(sapply(df_t[,'democrat'], 
                              function(x) rt(iter, x)))/sqrt(10))
p_d <- (1-sum(null_d<abs(T_stats['democrat']))/iter)*2

null_r <- sort(rowSums(sapply(df_t[,'republican'], 
                              function(x) rt(iter, x)))/sqrt(10))
p_r <- (1-sum(null_r>-abs(T_stats['republican']))/iter)*2

p_values <- c(p_d, p_r)

df <- data.frame(rbind(T_stats, p_values))

head(df)

# write_csv(df, 'study_2_bols.csv')
