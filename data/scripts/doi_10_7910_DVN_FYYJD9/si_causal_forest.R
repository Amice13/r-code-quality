### ---- Setup ----------

covariates <- 
  c("x_ideo_7n", "x_pid_7n", "x_edu", "x_hhi_short", "x_female", "x_age_cat", 
    "x_region", "x_race", "x_employ_short", "x_altruism_n", "x_vaxnat_n", 
    "x_patnat1_n", "x_patnat2_n")


## Estimate ATEs on index of all outcomes. Create index using inverse covariance
## weighting, then standardize w/ glass delta. 
tmp <-
  norc_survey %>%
  select(
    y_donate,
    y_petition,
    y_prop_cost,
    covariates,
    Z_frame
  ) %>%
  filter(!is.na(y_donate), !is.na(y_petition), !is.na(y_prop_cost)) 

## Pairwise application of function to each treatment/control group:
tmp_ineq <-
  tmp %>%
  filter(Z_frame %in% c("Control", "Inequality")) %>%
  mutate(
    Z = as.numeric(Z_frame != "Control"),
    y_idx = invcov(
      Z = Z,
      outcome_mat = cbind(y_donate, y_petition, y_prop_cost),
      reorient = FALSE
    )
  )

tmp_past <-
  tmp %>%
  filter(Z_frame %in% c("Control", "Past Success")) %>%
  mutate(
    Z = as.numeric(Z_frame != "Control"),
    y_idx = invcov(
      Z = Z,
      outcome_mat = cbind(y_donate, y_petition, y_prop_cost),
      reorient = FALSE
    )
  )

tmp_mutate <-
  tmp %>%
  filter(Z_frame %in% c("Control", "Mutation Risk")) %>%
  mutate(
    Z = as.numeric(Z_frame != "Control"),
    y_idx = invcov(
      Z = Z,
      outcome_mat = cbind(y_donate, y_petition, y_prop_cost),
      reorient = FALSE
    )
  )

tmp_econ <-
  tmp %>%
  filter(Z_frame %in% c("Control", "Economic Benefits")) %>%
  mutate(
    Z = as.numeric(Z_frame != "Control"),
    y_idx = invcov(
      Z = Z,
      outcome_mat = cbind(y_donate, y_petition, y_prop_cost),
      reorient = FALSE
    )
  )


tmp_diplo <-
  tmp %>%
  filter(Z_frame %in% c("Control", "Vaccine Diplomacy")) %>%
  mutate(
    Z = as.numeric(Z_frame != "Control"),
    y_idx = invcov(
      Z = Z,
      outcome_mat = cbind(y_donate, y_petition, y_prop_cost),
      reorient = FALSE
    )
  ) 

## Now stack scaled datasets, standardize using Glass Delta
est_dat <- 
  bind_rows(tmp_diplo, tmp_econ, tmp_ineq, tmp_mutate, tmp_past) %>% 
  filter(Z == 1) %>% 
  bind_rows(., tmp_diplo %>% filter(Z_frame == "Control")) %>% 
  mutate(y_idx_s = glass_delta(Y = y_idx, Z = Z_frame, reference = "Control"))


# NB: GRF only accepts rows with complete cases on DVs and all  covariates. 
index <- complete.cases(est_dat)
X <- est_dat[index, c(covariates, "Z_frame")]

## NB: only takes binary treatments, so we need to run the process 5
## times, once for each treatment arm
W_diplo <- X %>%
  mutate(W_diplo = case_when(Z_frame == "Vaccine Diplomacy" ~ 1,
                             Z_frame == "Control" ~ 0)) %>%
  pull(W_diplo)

W_econ <- X %>%
  mutate(W_econ = case_when(Z_frame == "Economic Benefits" ~ 1,
                            Z_frame == "Control" ~ 0)) %>%
  pull(W_econ)

W_ineq <- X %>%
  mutate(W_ineq = case_when(Z_frame == "Inequality" ~ 1,
                            Z_frame == "Control" ~ 0)) %>%
  pull(W_ineq)

W_mutate <- X %>%
  mutate(W_mutate = case_when(Z_frame == "Mutation Risk" ~ 1,
                              Z_frame == "Control" ~ 0)) %>%
  pull(W_mutate)

W_past <- X %>%
  mutate(W_past = case_when(Z_frame == "Past Success" ~ 1,
                            Z_frame == "Control" ~ 0)) %>%
  pull(W_past)

## Recode covariates to numeric
X$x_white <- as.numeric(X$x_race == "White") 
X$x_black <- as.numeric(X$x_race == "Black") 
X$x_hispanic <- as.numeric(X$x_race == "Hispanic") 
## Combine rarely sampled sub-groups
X$x_other <- as.numeric(X$x_race %in% c("AAPI", "Other")) 
X$x_west <- as.numeric(X$x_region == "West")
X$x_midwest <- as.numeric(X$x_region == "Midwest")
X$x_south <- as.numeric(X$x_region == "South")
X$x_northeast <- as.numeric(X$x_region == "Northeast")
X$x_employ <- as.numeric(X$x_employ_short == "Working")
X$x_age <- as.integer(X$x_age_cat)
X$x_hhi <- as.integer(X$x_hhi_short)
X$x_edu <- as.integer(X$x_edu)

nums <- unlist(lapply(X, is.numeric))  
X <- as.matrix(X[, nums])

## Pull out outcomes and treatment vectors
Y_Obs <- est_dat$y_idx[index]

### ---- Estimation ----------

# Step 1. Train a causal forest using the observed outcome (Y_Obs), the matrix
# of pre-treatment covariates (X), and reatment assignment vector (W).
set.seed(123)
tau_econ <-
  causal_forest(
    Y = Y_Obs[!is.na(W_econ)],
    X = X[!is.na(W_econ),],
    W = W_econ[!is.na(W_econ)],
    honesty = TRUE,
    num.trees = 4000,
    seed = 123
  )

tau_diplo <-
  causal_forest(
    Y = Y_Obs[!is.na(W_diplo)],
    X = X[!is.na(W_diplo),],
    W = W_diplo[!is.na(W_diplo)],
    honesty = TRUE,
    num.trees = 4000,
    seed = 123
  )


tau_ineq <-
  causal_forest(
    Y = Y_Obs[!is.na(W_ineq)],
    X = X[!is.na(W_ineq),],
    W = W_ineq[!is.na(W_ineq)],
    honesty = TRUE,
    num.trees = 4000,
    seed = 123
  )


tau_mutate <-
  causal_forest(
    Y = Y_Obs[!is.na(W_mutate)],
    X = X[!is.na(W_mutate),],
    W = W_mutate[!is.na(W_mutate)],
    honesty = TRUE,
    num.trees = 4000,
    seed = 123
  )


tau_past <-
  causal_forest(
    Y = Y_Obs[!is.na(W_past)],
    X = X[!is.na(W_past),],
    W = W_past[!is.na(W_past)],
    honesty = TRUE,
    num.trees = 4000,
    seed = 123
  )


# Step 2. Estimate the treatment effects for all units using
# out-of-bag prediction.
tau_hat_econ <- predict(tau_econ, estimate.variance = TRUE)
tau_hat_ineq <- predict(tau_ineq, estimate.variance = TRUE)
tau_hat_diplo <- predict(tau_diplo, estimate.variance = TRUE)
tau_hat_mutate <- predict(tau_mutate, estimate.variance = TRUE)
tau_hat_past <- predict(tau_past, estimate.variance = TRUE)

# Step 3. Compute estimated SEs treatment effects for predictions. 
sigma_hat_econ <- sqrt(tau_hat_econ$variance.estimates)
sigma_hat_ineq <- sqrt(tau_hat_ineq$variance.estimates)
sigma_hat_diplo <- sqrt(tau_hat_diplo$variance.estimates)
sigma_hat_mutate <- sqrt(tau_hat_mutate$variance.estimates)
sigma_hat_past <- sqrt(tau_hat_past$variance.estimates)

# Step 4. Store the estimated treatment effects and CIs in a data frame.
grf_econ <- 
  cbind(data.frame(
    tau_hat = tau_hat_econ$predictions,
    ci_upper = tau_hat_econ$predictions + 1.96 * sigma_hat_econ,
    ci_lower = tau_hat_econ$predictions - 1.96 * sigma_hat_econ),
    X[!is.na(W_econ),]
  ) %>% 
  mutate(W = "Economic Benefits")

grf_ineq <- 
  cbind(data.frame(
    tau_hat = tau_hat_ineq$predictions,
    ci_upper = tau_hat_ineq$predictions + 1.96 * sigma_hat_ineq,
    ci_lower = tau_hat_ineq$predictions - 1.96 * sigma_hat_ineq),
    X[!is.na(W_ineq),]
  ) %>% 
  mutate(W = "Global Inequality")

grf_diplo <- 
  cbind(data.frame(
    tau_hat = tau_hat_diplo$predictions,
    ci_upper = tau_hat_diplo$predictions + 1.96 * sigma_hat_diplo,
    ci_lower = tau_hat_diplo$predictions - 1.96 * sigma_hat_diplo),
    X[!is.na(W_diplo),]
  ) %>% 
  mutate(W = "Vaccine Diplomacy")

grf_mutate <- 
  cbind(data.frame(
    tau_hat = tau_hat_mutate$predictions,
    ci_upper = tau_hat_mutate$predictions + 1.96 * sigma_hat_mutate,
    ci_lower = tau_hat_mutate$predictions - 1.96 * sigma_hat_mutate),
    X[!is.na(W_mutate),]
  )  %>% 
  mutate(W = "Mutation Risk")

grf_past <- 
  cbind(data.frame(
    tau_hat = tau_hat_past$predictions,
    ci_upper = tau_hat_past$predictions + 1.96 * sigma_hat_past,
    ci_lower = tau_hat_past$predictions - 1.96 * sigma_hat_past),
    X[!is.na(W_past),]
  ) %>% 
  mutate(W = "Past Success")

grf_df <-
  bind_rows(grf_econ, grf_past, grf_diplo, grf_mutate, grf_ineq) %>%
  mutate(W = factor(
    W,
    levels = c(
      "Economic Benefits",
      "Mutation Risk",
      "Global Inequality",
      "Vaccine Diplomacy",
      "Past Success"
    )
  ))

## Export estimates 
write_rds(grf_df, "grf_estimates.rds")

