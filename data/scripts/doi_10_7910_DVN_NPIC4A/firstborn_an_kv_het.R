library(dplyr)

load("firstborn.rdata")

# subset to women and men born 1995 or earlier who are DK citizen
women <- 
  first_born %>%
  filter(KOEN == 2 & year_of_birth < 1996 & STATSB == 5100) %>%
  mutate(female_second = female_second == 2,
         age_dif       = year_of_birth_second - year_of_birth,
         age_dif_med   = age_dif <= median(age_dif))

men <- 
  first_born %>%
  filter(KOEN == 1 & year_of_birth < 1996 & STATSB == 5100) %>%
  mutate(female_second = female_second == 2,
         age_dif       = year_of_birth_second - year_of_birth,
         age_dif_med   = age_dif <= median(age_dif))

# Find ppl in at least one election year

women <- 
  women %>% 
  filter((in_year_1993 + in_year_1997 + in_year_2001 +
            in_year_2005 + in_year_2009 + in_year_2013) > 0)

men <- 
  men %>% 
  filter((in_year_1993 + in_year_1997 + in_year_2001 +
            in_year_2005 + in_year_2009 + in_year_2013) > 0)

# run models for ever running 

women_close <- 
  lm(100*kv_ever_run ~ female_second, 
     data = women %>% filter(age_dif_med == 1))

women_dist <- 
  lm(100*kv_ever_run ~ female_second, 
     data = women %>% filter(age_dif_med == 0))

men_close <- 
  lm(100*kv_ever_run ~ female_second, 
     data = men %>% filter(age_dif_med == 1))

men_dist <- 
  lm(100*kv_ever_run ~ female_second, 
     data = men %>% filter(age_dif_med == 0))


# save result in table 

res_mat <- matrix(NA, nrow = 4, ncol = 4)

res_mat[1, ] <- c(summary(women_close)$coefficients[2, 1:2],
                            summary(women_close)$coefficients[1, 1:1],
                            length(women_close$residuals))

res_mat[2, ] <- c(summary(women_dist)$coefficients[2, 1:2],
                            summary(women_dist)$coefficients[1, 1:1],
                            length(women_dist$residuals))

res_mat[3, ] <- c(summary(men_close)$coefficients[2, 1:2],
                            summary(men_close)$coefficients[1, 1:1],
                            length(men_close$residuals))

res_mat[4, ] <- c(summary(men_dist)$coefficients[2, 1:2],
                            summary(men_dist)$coefficients[1, 1:1],
                            length(men_dist$residuals))

age_med_results <- 
  tibble(est  = res_mat[1,],
         se   = res_mat[2,],
         base = res_mat[3,],
         N    = res_mat[4,],
         mod  = c("women_close",
                  "women_dist",
                  "men_close",
                  "men_dist"))

# loop over 1-6 years 

res_mat <- matrix(NA, nrow = 12, ncol = 4)

for(i in 1:6){
  women_temp <- 
    lm(100*kv_ever_run ~ female_second, 
       data = women %>% filter(age_dif == i))
  
  men_temp <- 
    lm(100*kv_ever_run ~ female_second, 
       data = men %>% filter(age_dif == i))
  
  res_mat[i, 1:4] <- 
    c(summary(women_temp)$coefficients[2, 1:2],
      summary(women_temp)$coefficients[1, 1:1],
      length(women_temp$residuals))
  
  res_mat[i + 6, 1:4] <- 
    c(summary(men_temp)$coefficients[2, 1:2],
      summary(men_temp)$coefficients[1, 1:1],
      length(men_temp$residuals))
  
}

results_year <- 
  tibble(est  = res_mat[ , 1],
         se   = res_mat[ , 2],
         base = res_mat[ , 3],
         N    = res_mat[ , 4],
         mod  = rep(c("women",
                      "men"), each = 6),
         year = rep(1:6, 2))

write.table(age_med_results, file = "age_med_results_kv.txt")
write.table(results_year, file = "results_year_kv.txt")
