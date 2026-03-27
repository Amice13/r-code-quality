

## load packages
require(tidyverse)
require(DIDdesign)

## load packages
data(paglayan2019)


## ------------------------------------ ##
## Prepare data
## ------------------------------------ ##
paglayan2019 <- paglayan2019 %>%
  filter(!(state %in% c("WI", "DC"))) %>%
  mutate(id_time = year,
         id_subject = as.numeric(as.factor(state)),
         log_expenditure = log(pupil_expenditure + 1),
         log_salary      = log(teacher_salary + 1)
       )


## formula
fm <- list(
  log_expenditure ~ treatment,
  log_salary ~ treatment
)

## ------------------------------------ ##
## Check pre-treatment trends
## ------------------------------------ ##
set.seed(1234)
cat("Checking pre-treatment trends.\n")
check_sa <- map(fm, ~did_check(
  formula = .x,
  data    = paglayan2019,
  id_unit = "id_subject",
  id_time = "id_time",
  design  = "sa",
  option  = list(n_boot = 2000, parallel = TRUE,
                 lag = 1:5,
                 thres = 1)
))


## ------------------------------------ ##
## Estimate SA-ATT
## ------------------------------------ ##
cat("Estimating treatment effects.\n")
fit_sa <-  map(fm, ~did(
  formula = .x,
  data    = paglayan2019,
  id_unit = "id_subject",
  id_time = "id_time",
  design  = "sa",
  option  = list(n_boot = 2000, parallel = TRUE,
                 lead = 0:9,
                 thres = 1)
))


## ------------------------------------ ##
## save
## ------------------------------------ ##

res <- list(check = check_sa, est = fit_sa)
saveRDS(res, file = "../results/application/11-double_did_paglayan_final.rds")
