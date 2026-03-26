library(dplyr)

rm(list = ls())

#setwd() # set working directory

ihds <- read.csv("ihds_replication.csv", stringsAsFactors = F)


# create indicator for overseas migrants
ihds <- ihds %>%
  mutate(migrants = ifelse(MGYEAR5==1, 1, 0)) %>%
  mutate(mig_os = ifelse(MGYEAR5==1 & MG4==3, 1, 0))

# Filter for migrants only
migrants <- ihds %>% filter(MGYEAR5==1)

# Look at overseas migrants only
mig_os <- migrants %>% filter(MG4==3)

# Length of migration
mean(mig_os$MG7, na.rm = T) # 20.32 months

# Demographics: Gender
table(mig_os$RO3)
ihds = ihds %>%
  mutate(male = ifelse(RO3==1, 1, 0))
mig_os = mig_os %>%
  mutate(male = ifelse(RO3==1, 1, 0))

# Demographics: Age
ihds = ihds %>%
  mutate(age = RO5) %>%
  mutate(age31 = ifelse(age<31, 1, 0)) %>%
  mutate(age31_50 = ifelse(age>30 & age< 51, 1, 0)) %>%
  mutate(age51 = ifelse(age > 50, 1, 0))

mean(mig_os$RO5)
mig_os = mig_os %>%
  mutate(age = RO5) %>%
  mutate(age31 = ifelse(age<31, 1, 0)) %>%
  mutate(age31_50 = ifelse(age>30 & age< 51, 1, 0)) %>%
  mutate(age51 = ifelse(age > 50, 1, 0))

# Demographics: Education 
table(mig_os$ED6) # Education - in years
mean(mig_os$ED6)

ihds = ihds %>%
  mutate(yrs_school = ED6) %>%
  mutate(comp10st = ifelse(yrs_school>=10, 1, 0)) %>%
  mutate(comp12st = ifelse(yrs_school>=12, 1, 0))

mig_os = mig_os %>%
  mutate(yrs_school = ED6) %>%
  mutate(comp10st = ifelse(yrs_school>=10, 1, 0)) %>%
  mutate(comp12st = ifelse(yrs_school>=12, 1, 0))


# Demographics: Religion
table(mig_os$ID11)

ihds = ihds %>%
  mutate(hindu = ifelse(ID11==1, 1, 0)) %>%
  mutate(muslim = ifelse(ID11==2, 1, 0)) %>%
  mutate(christian = ifelse(ID11==3, 1, 0)) %>%
  mutate(sikh = ifelse(ID11==4, 1, 0)) %>%
  mutate(min_religion = ifelse(muslim==1|christian==1, 1, 0))


mig_os = mig_os %>%
  mutate(hindu = ifelse(ID11==1, 1, 0)) %>%
  mutate(muslim = ifelse(ID11==2, 1, 0)) %>%
  mutate(christian = ifelse(ID11==3, 1, 0)) %>%
  mutate(sikh = ifelse(ID11==4, 1, 0))%>%
  mutate(min_religion = ifelse(muslim==1|christian==1, 1, 0))

#mig_os <- as.data.frame(mig_os)
#ihds <- as.data.frame(ihds)

mig_os_means <- 
  mig_os %>%
  summarise_at(vars("age31", "age31_50", "age51", "male", "comp10st", 
                    "min_religion"), .funs = mean, na.rm = T) %>%
  as.data.frame() %>% t()

ihds_means <- 
  ihds %>% 
  summarise_at(vars("age31", "age31_50", "age51", "male", "comp10st", 
                    "min_religion"), .funs = mean, na.rm = T)  %>%
  as.data.frame() %>% t()

# Combine columns
means_ihds <- 
  bind_cols("general_pop" = round(ihds_means, 2), "overseas_migrants" = round(mig_os_means, 2))
means_ihds

