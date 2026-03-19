##
##

## load packages
require(tidyverse)
require(DIDdesign)
require(Formula)

## load data
data(malesky2014)
malesky2014 <- drop_na(malesky2014, lnarea, lnpopden, city)

## -------------------------------------
## prepare input
## -------------------------------------
outcome_pt      <- c("pro4")
outcome_ptt     <- c("tapwater")
outcome_violate <- c("agrext")
outcome_list <- c(outcome_pt, outcome_ptt, outcome_violate)
fm <- map(outcome_list,
  ~ as.Formula(paste(.x, "~ treatment + post_treat | lnarea + lnpopden + city + as.factor(reg8)")))

## -------------------------------------
## assess the assumption
## -------------------------------------
set.seed(1234)
check_out <- map(fm, ~did_check(
  formula  = .x,
  data     = malesky2014,
  id_time  = "year",
  is_panel = FALSE,
  option   = list(n_boot = 2000, parallel = TRUE, id_cluster = "id_district", lag = 1))
)

## name obj and summarize
names(check_out) <- outcome_list
check_summary <- map_dfr(check_out, ~.x$estimate, .id = "var") %>%
  mutate(statistic = estimate / std.error,
         p_value = 2 * pnorm(abs(statistic), lower.tail = FALSE))

## save
saveRDS(list(out = check_out, summary = check_summary), file = "../results/application/01-check_main_output.rds")

## -------------------------------------
## estimate ATT
## -------------------------------------
set.seed(1234)
est_out <- map(fm, ~did(
  formula  = .x,
  data     = malesky2014,
  id_time  = "year",
  is_panel = FALSE,
  option   = list(n_boot = 2000, parallel = TRUE, id_cluster = "id_district", lead = 0))
)

## name object and summarize
names(est_out) <- outcome_list
est_summary <- map_dfr(est_out, ~as_tibble(summary(.x)), .id = "var")


## save
saveRDS(list(out = est_out, summary = est_summary), file = "../results/application/02-est_main_output.rds")
