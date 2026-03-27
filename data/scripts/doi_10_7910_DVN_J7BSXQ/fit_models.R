###############################################################
## Fit response and outcome models
###############################################################
library(tidyverse)
library(glmnet)
library(ranger)
library(mgcv)
library(gbm)

set.seed(10112118)

# TODO: don't hardcode this :)
pew_loc <- "~/Dropbox/DR P/data/pew_example/pew.rds"
cces_loc <- "~/Dropbox/DR P/data/pew_example/cces.rds"

# read data
pew2016_full <- read_rds(pew_loc) %>%
  mutate(recode_attndch = recode(recode_attndch, 
              "Don't know/Refused (VOL.)" = "Don't know"))

cces2016_full <- read_rds(cces_loc) %>%
  filter((CC16_401 == "I definitely voted in the General Election.") &
           !is.na(commonweight_vv_post)) %>%
  mutate(recode_relig = replace_na(recode_relig, "Something else"))

keep_cols <- c("age_bucket", "female", "race", 
              "region", "pid_3way", "educ", "inputstate",
              "relig", "born", "attndch", "income")

elect2016 <- bind_rows(pew2016_full %>% mutate(cces = 0, commonweight_vv_post = 0),
                    cces2016_full %>% mutate(cces = 1)) %>%
          mutate(rvote_2016 = recode_vote_2016 == "Republican") %>%
          mutate(across(paste0("recode_", keep_cols), as.factor)) %>%
          select(rvote_2016, cces, paste0("recode_", keep_cols), commonweight_vv_post) %>%
          rename_with(~str_replace_all(., "recode_", "")) %>%
          na.omit() %>%
          mutate(across(where(is.factor), droplevels))


bal_cols <- c("educ", "income", "region", "race", "female", "age_bucket",
              "pid_3way","born")

# create design matrix
X <- Matrix::sparse.model.matrix(~ ., elect2016[bal_cols])
X4 <- Matrix::sparse.model.matrix(~ . ^ 4, elect2016[bal_cols])
resp <- 1 - elect2016$cces
rvote <- elect2016$rvote_2016

# fit ridge pscore models
fit4_lowreg <- glmnet(X4, resp, family = "binomial", trace.it = 1, alpha = 0)
# fit random forest pscore model
rf <- ranger(x = X, y = as.factor(resp),
             class.weights = c(1, 12), probability = T)


# add predictions to data
sim_data <- elect2016[bal_cols]
sim_data$responded <- 1 - elect2016$cces
sim_data$pscore_4_lowreg <- c(predict(fit4_lowreg, X4, type = "response")[,100])
sim_data$pscore_rf <- predict(rf, X)$predictions[,2]


# fit ridge outcome models
fit4_out_lowreg <- glmnet(X4, rvote, family = "binomial", trace.it = 1, alpha = 0)

# fit random forest outcome model
rf_out <- ranger(x = X, y = as.factor(rvote), probability = T)


sim_data$rvote <- rvote
sim_data$out4_lowreg <- c(predict(fit4_out_lowreg, X4, type = "response")[,100])

write_csv(sim_data, "sim_data.csv")
