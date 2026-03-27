
##
## augsynth on Paglayan data
##

## load packages
require(augsynth)
require(DIDdesign)
require(tidyverse)

## load data
data(paglayan2019)

paglayan2019 <- paglayan2019 %>%
  filter(!(state %in% c("WI", "DC"))) %>%
  mutate(log_pupil_expenditure = log(pupil_expenditure + 1),
         log_teacher_salary = log(teacher_salary + 1))

## fit
set.seed(1234)
pupil_augsyn <- multisynth(log_pupil_expenditure ~ treatment, state, year,
                        paglayan2019 %>% data.frame(), n_leads = 10)
pupil_summary <- summary(pupil_augsyn)

## salary
salary_augsyn <- multisynth(log_teacher_salary ~ treatment, state, year,
                        paglayan2019 %>% data.frame(), n_leads = 10)
salary_summary <- summary(salary_augsyn)


## save
saveRDS(pupil_summary, file = '../results/application/12-augsynth_paglayan_expenditure.rds')
saveRDS(salary_summary, file = '../results/application/13-augsynth_paglayan_salary.rds')
