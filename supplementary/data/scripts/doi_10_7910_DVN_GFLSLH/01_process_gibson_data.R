#### 1. Read in Gibson's data ####
gibson_dat <- import("data/raw/gibson_replication/ABAJPS CHANGE.SAV")

#### 2. Cleaning ####
gibson_dat <- gibson_dat %>% 
  select(SCSUP1, WAVE, WEIGHT) %>% 
  ## make Wave a factor
  ## recode the agreement variable
  mutate(WAVE = factorize(WAVE), 
         agree1 = case_when(SCSUP1 %in% 1:2 ~ 1, 
                            SCSUP1 %in% 3:5 ~ 0, 
                            TRUE ~ NA_real_)) %>% 
  ## remove missings
  drop_na()

#### 3. Save data to analysis folder ####
save(gibson_dat, file="data/analysis/gibson_replication/gibson_dat.rda")
rio::export(gibson_dat, "data/analysis/gibson_replication/gibson_dat.dta")

