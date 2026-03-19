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

# create descriptive table for all women, female candidates
## OBS: Too few winners

desc_women <- 
  women %>% 
  summarise(yob       = mean(year_of_birth),
            native    = mean(IE_TYPE == 1),
            n_sibship = mean(n_sibship) - 1,
            n_girls   = mean(n_girls) - 1) %>% 
  mutate(stat = "mean") %>% 
  bind_rows(.,   women %>% 
              summarise(yob       = sd(year_of_birth),
                        native    = sd(IE_TYPE == 1),
                        n_sibship = sd(n_sibship),
                        n_girls   = sd(n_girls)) %>% 
              mutate(stat = "sd")) %>%
  bind_rows(.,   women %>% 
              summarise(yob       = quantile(year_of_birth, p = 0.05),
                        native    = quantile(IE_TYPE == 1, p = 0.05),
                        n_sibship = quantile(n_sibship, p = 0.05) - 1,
                        n_girls   = quantile(n_girls, p = 0.05) - 1) %>% 
              mutate(stat = "q05")) %>% 
  bind_rows(.,   women %>% 
              summarise(yob       = quantile(year_of_birth, p = 0.95),
                        native    = quantile(IE_TYPE == 1, p = 0.95),
                        n_sibship = quantile(n_sibship, p = 0.95) - 1,
                        n_girls   = quantile(n_girls, p = 0.95) - 1) %>% 
              mutate(stat = "q95")) %>% 
  bind_rows(.,   women %>% 
              summarise(yob       = length(year_of_birth),
                        native    = length(IE_TYPE),
                        n_sibship = length(n_sibship),
                        n_girls   = length(n_girls)) %>% 
              mutate(stat = "n")) %>% 
  mutate(sex = "women",
         pop = "all") 

# election candidates
desc_women_can <-
  women %>% 
  filter(fv_ever_run == 1) %>% 
  summarise(yob       = mean(year_of_birth),
            native    = mean(IE_TYPE == 1),
            n_sibship = mean(n_sibship) - 1,
            n_girls   = mean(n_girls) - 1) %>% 
  mutate(stat = "mean") %>% 
  bind_rows(.,     women %>% 
              filter(fv_ever_run == 1) %>% 
              summarise(yob       = sd(year_of_birth),
                        native    = sd(IE_TYPE == 1),
                        n_sibship = sd(n_sibship),
                        n_girls   = sd(n_girls)) %>% 
              mutate(stat = "sd")) %>%
  bind_rows(.,   women %>% 
              filter(fv_ever_run == 1) %>% 
              summarise(yob       = quantile(year_of_birth, p = 0.05),
                        native    = quantile(IE_TYPE == 1, p = 0.05),
                        n_sibship = quantile(n_sibship, p = 0.05) - 1,
                        n_girls   = quantile(n_girls, p = 0.05) - 1) %>% 
              mutate(stat = "q05")) %>% 
  bind_rows(.,   women %>% 
              filter(fv_ever_run == 1) %>% 
              summarise(yob       = quantile(year_of_birth, p = 0.95),
                        native    = quantile(IE_TYPE == 1, p = 0.95),
                        n_sibship = quantile(n_sibship, p = 0.95) - 1,
                        n_girls   = quantile(n_girls, p = 0.95) - 1) %>% 
              mutate(stat = "q95")) %>% 
  bind_rows(.,   women %>% 
              filter(fv_ever_run == 1) %>% 
              summarise(yob       = length(year_of_birth),
                        native    = length(IE_TYPE),
                        n_sibship = length(n_sibship),
                        n_girls   = length(n_girls)) %>% 
              mutate(stat = "n")) %>% 
  mutate(sex = "women",
         pop = "can")




# Men ---------------------------------------------------------------------



# create descriptive table for all men, male candidates
## OBS: Too few winners

desc_men <- 
  men %>% 
  summarise(yob       = mean(year_of_birth),
            native    = mean(IE_TYPE == 1),
            n_sibship = mean(n_sibship) - 1,
            n_girls   = mean(n_girls)) %>% 
  mutate(stat = "mean") %>% 
  bind_rows(.,   men %>% 
              summarise(yob       = sd(year_of_birth),
                        native    = sd(IE_TYPE == 1),
                        n_sibship = sd(n_sibship),
                        n_girls   = sd(n_girls)) %>% 
              mutate(stat = "sd")) %>%
  bind_rows(.,   men %>% 
              summarise(yob       = quantile(year_of_birth, p = 0.05),
                        native    = quantile(IE_TYPE == 1, p = 0.05),
                        n_sibship = quantile(n_sibship, p = 0.05) - 1,
                        n_girls   = quantile(n_girls, p = 0.05)) %>% 
              mutate(stat = "q05")) %>% 
  bind_rows(.,   men %>% 
              summarise(yob       = quantile(year_of_birth, p = 0.95),
                        native    = quantile(IE_TYPE == 1, p = 0.95),
                        n_sibship = quantile(n_sibship, p = 0.95) - 1,
                        n_girls   = quantile(n_girls, p = 0.95)) %>% 
              mutate(stat = "q95")) %>% 
  bind_rows(.,   men %>% 
              summarise(yob       = length(year_of_birth),
                        native    = length(IE_TYPE),
                        n_sibship = length(n_sibship),
                        n_girls   = length(n_girls)) %>% 
              mutate(stat = "n")) %>% 
  mutate(sex = "men",
         pop = "all") 

# election candidates
desc_men_can <-
  men %>% 
  filter(fv_ever_run == 1) %>% 
  summarise(yob       = mean(year_of_birth),
            native    = mean(IE_TYPE == 1),
            n_sibship = mean(n_sibship) - 1,
            n_girls   = mean(n_girls)) %>% 
  mutate(stat = "mean") %>% 
  bind_rows(.,     men %>% 
              filter(fv_ever_run == 1) %>% 
              summarise(yob       = sd(year_of_birth),
                        native    = sd(IE_TYPE == 1),
                        n_sibship = sd(n_sibship),
                        n_girls   = sd(n_girls)) %>% 
              mutate(stat = "sd")) %>%
  bind_rows(.,   men %>% 
              filter(fv_ever_run == 1) %>% 
              summarise(yob       = quantile(year_of_birth, p = 0.05),
                        native    = quantile(IE_TYPE == 1, p = 0.05),
                        n_sibship = quantile(n_sibship, p = 0.05) - 1,
                        n_girls   = quantile(n_girls, p = 0.05)) %>% 
              mutate(stat = "q05")) %>% 
  bind_rows(.,   men %>% 
              filter(fv_ever_run == 1) %>% 
              summarise(yob       = quantile(year_of_birth, p = 0.95),
                        native    = quantile(IE_TYPE == 1, p = 0.95),
                        n_sibship = quantile(n_sibship, p = 0.95) - 1,
                        n_girls   = quantile(n_girls, p = 0.95)) %>% 
              mutate(stat = "q95")) %>% 
  bind_rows(.,   men %>% 
              filter(fv_ever_run == 1) %>% 
              summarise(yob       = length(year_of_birth),
                        native    = length(IE_TYPE),
                        n_sibship = length(n_sibship),
                        n_girls   = length(n_girls)) %>% 
              mutate(stat = "n")) %>% 
  mutate(sex = "men",
         pop = "can")


# Bind tables

desc_table <- 
  bind_rows(desc_women, 
            desc_women_can,
            desc_men, 
            desc_men_can)

write.table(desc_table, file = "desc_stat_fv.txt")
