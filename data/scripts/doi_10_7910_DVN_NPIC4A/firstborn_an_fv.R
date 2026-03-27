library(dplyr)

load("firstborn.rdata")

# subset to women and men born 1995 or earlier who are DK citizen
women <- 
  first_born %>%
  filter(KOEN == 2 & year_of_birth < 1996 & STATSB == 5100) %>%
  filter((in_year_fv_1990 + in_year_fv_1994 + in_year_fv_1998 +
            in_year_fv_2001 + in_year_fv_2005 + in_year_fv_2007 +
            in_year_fv_2011 + in_year_fv_2015) > 0) %>%
  mutate(female_second = female_second == 2,
         fv_ever_run     = fv_n_run     > 0,
         fv_ever_elected = fv_n_elected > 0)

men <- 
  first_born %>%
  filter(KOEN == 1 & year_of_birth < 1996 & STATSB == 5100) %>%
  filter((in_year_fv_1990 + in_year_fv_1994 + in_year_fv_1998 +
            in_year_fv_2001 + in_year_fv_2005 + in_year_fv_2007 +
            in_year_fv_2011 + in_year_fv_2015) > 0) %>%
  mutate(female_second = female_second == 2,
         fv_ever_run     = fv_n_run     > 0,
         fv_ever_elected = fv_n_elected > 0)


# run models for ever running 

simple_model_everFV_run_women <- 
  lm(100*fv_ever_run ~ female_second, 
     data = women)


# run models for ever elected 
simple_model_everFV_elected_women <-
  lm(100*fv_ever_elected ~ female_second, 
     data = women)


# For men

# run models for ever running 

simple_model_everFV_run_men <- 
  lm(100*fv_ever_run ~ female_second, 
     data = men)


# run models for ever elected 
simple_model_everFV_elected_men <-
  lm(100*fv_ever_elected ~ female_second, 
     data = men)

# save result in table 

simple_results_FV <- 
  data_frame(est  = rep(NA, 4),
             se   = rep(NA, 4),
             base = rep(NA, 4),
             N    = rep(NA, 4),
             mod  = c("women_run",
                      "men_run",
                      "women_elect",
                      "men_elect"))

simple_results_FV[1, -5] <- c(summary(simple_model_everFV_run_women)$coefficients[2, 1:2],
                              summary(simple_model_everFV_run_women)$coefficients[1, 1],
                              length(simple_model_everFV_run_women$residuals))

simple_results_FV[2, -5] <- c(summary(simple_model_everFV_run_men)$coefficients[2, 1:2],
                              summary(simple_model_everFV_run_men)$coefficients[1, 1],
                              length(simple_model_everFV_run_men$residuals))

simple_results_FV[3, -5] <- c(summary(simple_model_everFV_elected_women)$coefficients[2, 1:2],
                              summary(simple_model_everFV_elected_women)$coefficients[1, 1],
                              length(simple_model_everFV_elected_women$residuals))

simple_results_FV[4, -5] <- c(summary(simple_model_everFV_elected_men)$coefficients[2, 1:2],
                              summary(simple_model_everFV_elected_men)$coefficients[1, 1],
                              length(simple_model_everFV_elected_men$residuals))

write.table(simple_results_FV, file = "simple_results_FV.txt")

predicted_means <- 
  data_frame(mean = 100*c(tapply(women$fv_ever_run    , women$female_second, mean),
                          tapply(  men$fv_ever_run    ,   men$female_second, mean),
                          tapply(women$fv_ever_elected, women$female_second, mean),
                          tapply(  men$fv_ever_elected,   men$female_second, mean)),
             se   = 100*c(tapply(women$fv_ever_run    , women$female_second, std.error),
                          tapply(  men$fv_ever_run    ,   men$female_second, std.error),
                          tapply(women$fv_ever_elected, women$female_second, std.error),
                          tapply(  men$fv_ever_elected,   men$female_second, std.error)),
             mod  = rep(c("women_run",
                          "men_run",
                          "women_elect",
                          "men_elect"), each = 2),
             target  = rep(rep(c("Women", "Men"), each = 2), 2),
             outcome = rep(c("Ever run for office", "Ever elected for office"), each = 4),
             sibling = rep(c("Brother", "Sister"), 4))

write.table(predicted_means, file = "predicted_means_FV.txt")

# Analyses for share of runs  ---------------------------------------------

share_model_everFV_run_women <- 
  lm(100*share_FV_run ~ female_second, 
     data = women)

share_model_everFV_run_men <- 
  lm(100*share_FV_run ~ female_second, 
     data = men)

share_model_everFV_elected_women <-
  lm(100*share_FV_elected ~ female_second, 
     data = women)

share_model_everFV_elected_men <-
  lm(100*share_FV_elected ~ female_second, 
     data = men)

# save result in table 

share_results_FV <- 
  data_frame(est  = rep(NA, 4),
             se   = rep(NA, 4),
             base = rep(NA, 4),
             N    = rep(NA, 4),
             mod  = c("women_run",
                      "men_run",
                      "women_elect",
                      "men_elect"))

share_results_FV[1, -5] <- c(summary(share_model_everFV_run_women)$coefficients[2, 1:2],
                             summary(share_model_everFV_run_women)$coefficients[1, 1],
                             length(share_model_everFV_run_women$residuals))

share_results_FV[2, -5] <- c(summary(share_model_everFV_run_men)$coefficients[2, 1:2],
                             summary(share_model_everFV_run_men)$coefficients[1, 1],
                             length(share_model_everFV_run_men$residuals))

share_results_FV[3, -5] <- c(summary(share_model_everFV_elected_women)$coefficients[2, 1:2],
                             summary(share_model_everFV_elected_women)$coefficients[1, 1],
                             length(share_model_everFV_elected_women$residuals))

share_results_FV[4, -5] <- c(summary(share_model_everFV_elected_men)$coefficients[2, 1:2],
                             summary(share_model_everFV_elected_men)$coefficients[1, 1],
                             length(share_model_everFV_elected_men$residuals))

save(share_results_FV, file = "share_results_FV.rdata")


# run analyses for running by election 

simple_results_FV_year_temp <- 
  data_frame(est  = rep(NA, 4),
             se   = rep(NA, 4),
             base = rep(NA, 4),
             N    = rep(NA, 4),
             mod  = c("women_run",
                      "men_run",
                      "women_elect",
                      "men_elect"),
             year = rep(NA, 4))

simple_results_FV_year <- 
  data_frame()

for(k in c(1990, 1994, 1998, 2001, 2005, 2007, 2011, 2015)){
  
  women_year <- 
    women %>% 
    filter(year_of_birth < (k - 18)) %>%
    filter(.[, paste("in_year_fv_", k, sep = "")] == 1) %>% 
    rename(run = paste("run_fv_", k, "_FV", sep = ""),
           elected = paste("elected_fv_", k, sep = ""))
  
  men_year <- 
    men %>% 
    filter(year_of_birth < (k - 18)) %>%
    filter(.[, paste("in_year_fv_", k, sep = "")] == 1) %>% 
    rename(run = paste("run_fv_", k, "_FV", sep = ""),
           elected = paste("elected_fv_", k, sep = ""))
  
  simple_model_year_run_women <- 
    lm(100*run ~ female_second, 
       data = women_year)
  
  simple_model_year_run_men <- 
    lm(100*run ~ female_second, 
       data = men_year)
  
  # run models for elected 
  simple_model_year_elected_women <-
    lm(100*elected ~ female_second, 
       data = women_year)
  
  simple_model_year_elected_men <-
    lm(100*elected ~ female_second, 
       data = men_year)  
  
  simple_results_FV_year_temp[1, -c(5, 6)] <- 
    c(summary(simple_model_year_run_women)$coefficients[2, 1:2],
      summary(simple_model_year_run_women)$coefficients[1, 1],
      length( simple_model_year_run_women$residuals))
  
  simple_results_FV_year_temp[2, -c(5, 6)] <- 
    c(summary(simple_model_year_run_men)$coefficients[2, 1:2],
      summary(simple_model_year_run_men)$coefficients[1, 1],
      length( simple_model_year_run_men$residuals))
  
  simple_results_FV_year_temp[3, -c(5, 6)] <- 
    c(summary(simple_model_year_elected_women)$coefficients[2, 1:2],
      summary(simple_model_year_elected_women)$coefficients[1, 1],
      length( simple_model_year_elected_women$residuals))
  
  simple_results_FV_year_temp[4, -c(5, 6)] <- 
    c(summary(simple_model_year_elected_men)$coefficients[2, 1:2],
      summary(simple_model_year_elected_men)$coefficients[1, 1],
      length( simple_model_year_elected_men$residuals))  
  
  simple_results_FV_year_temp$year <- k
  
  simple_results_FV_year <- 
    bind_rows(simple_results_FV_year, simple_results_FV_year_temp)
}

write.table(simple_results_FV_year, file = "simple_results_FV_year.txt")

# Year-by-year analysis with middle 90% of candidate age distribut --------

simple_results_FV_year_90 <- 
  data_frame()

for(k in c(1990, 1994, 1998, 2001, 2005, 2007, 2011, 2015)){
  women_year <- 
    women %>% 
    filter(year_of_birth < (k - 18)) %>%
    filter(.[, paste("in_year_fv_", k, sep = "")] == 1) %>% 
    rename(run = paste("run_fv_", k, "_FV", sep = ""),
           elected = paste("elected_fv_", k, sep = ""))
  
  men_year <- 
    men %>% 
    filter(year_of_birth < (k - 18)) %>%
    filter(.[, paste("in_year_fv_", k, sep = "")] == 1) %>% 
    rename(run = paste("run_fv_", k, "_FV", sep = ""),
           elected = paste("elected_fv_", k, sep = ""))
  
  # find candidates 
  candidates <- 
    bind_rows(women_year %>% 
                filter(run == 1),
              men_year %>% 
                filter(run == 1))
  
  women_year <-
    women_year %>% 
    filter(year_of_birth <= quantile(candidates$year_of_birth, p = 0.95) &
             year_of_birth >= quantile(candidates$year_of_birth, p = 0.05))
  
  men_year <-
    men_year %>% 
    filter(year_of_birth <= quantile(candidates$year_of_birth, p = 0.95) &
             year_of_birth >= quantile(candidates$year_of_birth, p = 0.05))
  simple_model_year_run_women <- 
    lm(100*run ~ female_second, 
       data = women_year)
  
  simple_model_year_run_men <- 
    lm(100*run ~ female_second, 
       data = men_year)
  
  # run models for ever elected 
  simple_model_year_elected_women <-
    lm(100*elected ~ female_second, 
       data = women_year)
  
  simple_model_year_elected_men <-
    lm(100*elected ~ female_second, 
       data = men_year)  
  
  simple_results_FV_year_temp[1, -c(5, 6)] <- 
    c(summary(simple_model_year_run_women)$coefficients[2, 1:2],
      summary(simple_model_year_run_women)$coefficients[1, 1],
      length( simple_model_year_run_women$residuals))
  
  simple_results_FV_year_temp[2, -c(5, 6)] <- 
    c(summary(simple_model_year_run_men)$coefficients[2, 1:2],
      summary(simple_model_year_run_men)$coefficients[1, 1],
      length( simple_model_year_run_men$residuals))
  
  simple_results_FV_year_temp[3, -c(5, 6)] <- 
    c(summary(simple_model_year_elected_women)$coefficients[2, 1:2],
      summary(simple_model_year_elected_women)$coefficients[1, 1],
      length( simple_model_year_elected_women$residuals))
  
  simple_results_FV_year_temp[4, -c(5, 6)] <- 
    c(summary(simple_model_year_elected_men)$coefficients[2, 1:2],
      summary(simple_model_year_elected_men)$coefficients[1, 1],
      length( simple_model_year_elected_men$residuals))  
  
  simple_results_FV_year_temp$year <- k
  
  simple_results_FV_year_90 <- 
    bind_rows(simple_results_FV_year_90, simple_results_FV_year_temp)
}

write.table(simple_results_FV_year_90, file = "res/simple_results_FV_year_90.txt")