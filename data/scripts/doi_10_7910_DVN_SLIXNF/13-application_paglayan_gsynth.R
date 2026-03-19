
##
## gsynth on Paglayan
##

## load packages
require(gsynth)
require(DIDdesign)
require(tidyverse)
data(paglayan2019)


##
## Gsynth
##
models <- list(
  log_pupil_expenditure ~ treatment,
  log_teacher_salary ~ treatment
)

## fit the model
## min.T0 = 7 follows the error message
set.seed(1234)
fit <- map(models, ~ gsynth(formula = .x,
          data = paglayan2019 %>%
            filter(!(state %in% c("WI", "DC"))) %>%
            mutate(log_pupil_expenditure = log(pupil_expenditure + 1),
                   log_teacher_salary = log(teacher_salary + 1)),
          index = c("state", "year"), min.T0 = 7, se = TRUE))

## summarise results
gsynth_att <- map(fit, function(x) {
  names <- rownames(x$est.att)
  dat   <- as_tibble(x$est.att) %>%
  mutate(Time = as.numeric(names)) %>%
  filter(Time >= -3 & Time <= 9)
  return(dat)
})

## save
saveRDS(gsynth_att, file = '../results/application/14-gsynth_paglayan.rds')
