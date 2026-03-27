rm(list = ls())

library("ltm")
library("pryr")
library("dplyr")
library("splines")
library("ggplot2")
library("hIRT")
library("rms")
load("nes7216.RData")
load("GLpoints.RData")
source("hgrm2.R")

start_time <- Sys.time()

nes_v1 <- nes %>% dplyr::select(one_of(c(var_id, item_id))) %>%
  `names<-`(c(var_names, item_names)) %>%
  filter(year>=1972, party>0, educ>0) %>%
  mutate(party = factor(findInterval(party, c(1, 3, 6))),
         educ = factor(findInterval(educ, c(1, 3)))) %>%
  mutate_at(item_names, tmp_fun_a) %>%
  mutate_at(vars(contains("2"), contains("3"), contains("4"), contains("5")), tmp_fun_b)

nes_v1 <- nes_v1 %>% data.frame(nvalid_econ = rowSums(!is.na(dplyr::select(., health_ins7:FS_assistblacks3))),
                                nvalid_civil = rowSums(!is.na(dplyr::select(., negro_chan3:blacks_deserve_more5))),
                                nvalid_moral = rowSums(!is.na(dplyr::select(., women_role7:abort4))),
                                nvalid_foreign = rowSums(!is.na(dplyr::select(., urss_coop7:FS_space3)))) %>%
  group_by(year) %>% mutate(nvalid_econ_yr_level = max(nvalid_econ),
                            nvalid_civil_yr_level = max(nvalid_civil),
                            nvalid_moral_yr_level = max(nvalid_moral),
                            nvalid_foreign_yr_level = max(nvalid_foreign)) %>%  ungroup()

nes_econ <- nes_v1 %>%
  filter(nvalid_econ >=1, nvalid_econ_yr_level>=3) %>%
  dplyr::select(year:income, health_ins7:FS_assistblacks3)
hist(nes_econ$year)

nes_civil <- nes_v1 %>%
  filter(nvalid_civil >=1, nvalid_civil_yr_level>=3) %>%
  dplyr::select(year:income, negro_chan3:blacks_deserve_more5)
hist(nes_civil$year)

nes_moral <- nes_v1 %>%
  filter(nvalid_moral >=1, nvalid_moral_yr_level>=3) %>%
  dplyr::select(year:income, women_role7:abort4)
hist(nes_moral$year)

nes_foreign <- nes_v1 %>%
  filter(nvalid_foreign >=1, nvalid_foreign_yr_level>=3) %>%
  dplyr::select(year:income, urss_coop7:FS_space3)
hist(nes_foreign$year)

number_years <- c(by(nes_econ, nes_econ$year, function(x) sapply(x, function(x) any(!is.na(x)))) %>%
                    simplify2array() %>% rowSums() %>% `[`(names(.) %in% item_names),
                  by(nes_civil, nes_civil$year, function(x) sapply(x, function(x) any(!is.na(x)))) %>%
                    simplify2array() %>% rowSums() %>% `[`(names(.) %in% item_names),
                  by(nes_moral, nes_moral$year, function(x) sapply(x, function(x) any(!is.na(x)))) %>%
                    simplify2array() %>% rowSums() %>% `[`(names(.) %in% item_names),
                  by(nes_foreign, nes_foreign$year, function(x) sapply(x, function(x) any(!is.na(x)))) %>%
                    simplify2array() %>% rowSums() %>% `[`(names(.) %in% item_names))

table1 <- data.frame(item_id, item_names, item_wording, number_cat) %>%
  filter(item_names %in% names(number_years)) %>%
  cbind(number_years)

item_names <- setdiff(item_names, c("urb_unrest7", "immi5"))

# economics

year_bs <- bs(nes_econ$year, df = 4, degree = 2)
x_econ <- model.matrix( ~ year_bs * party * educ, nes_econ)
z_econ <- model.matrix( ~ 1, nes_econ)
y_econ <- nes_econ %>% dplyr::select(intersect(item_names, names(.)))

hgrm_econ <- hgrm(y_econ, x_econ, z_econ)

data_new <- expand.grid(party = factor(1:3), year = range(nes_econ$year)[1]:range(nes_econ$year)[2], educ = factor(1:2))
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp * party * educ, data_new)
z_new <- model.matrix( ~ 1, data_new)

gamma <- coef_mean(hgrm_econ)$Est
gamma_index <- with(hgrm_econ, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
cov_gamma <- hgrm_econ$vcov[gamma_index, gamma_index]

econ_plot <- data_new %>%
  mutate(fitted =  x_new %*% gamma,
         fitted_se = apply(x_new[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         `Party ID` = factor(party, labels = c("Democrat", "Independent", "Republican")),
         educ = factor(educ, labels = c("No College", "College")))

x_econ2 <- model.matrix( ~ year_bs, nes_econ)
z_econ2 <- model.matrix( ~ year_bs, nes_econ)
hgrm_econ2 <- hgrm2(y_econ, x_econ2, z_econ2)

data_new <- expand.grid(year = range(nes_econ$year)[1]:range(nes_econ$year)[2])
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp, data_new)
z_new <- model.matrix( ~ year_bs_simp, data_new)

gamma <- coef_mean(hgrm_econ2)$Est
lambda <- coef_var(hgrm_econ2)$Est
gamma_index <- with(hgrm_econ2, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
lambda_index <- with(hgrm_econ2, if(q>1) seq(sum(H)+p, sum(H)+p+q-2) else NULL)
cov_gamma <- hgrm_econ2$vcov[gamma_index, gamma_index]
cov_lambda <- hgrm_econ2$vcov[lambda_index, lambda_index]

econ2_plot <- data_new %>%
  mutate(mean_est =  x_new %*% gamma,
         mean_se = apply(x_new[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se,
         var_est = exp(z_new %*% lambda),
         var_upper =  exp(z_new %*% lambda + 1.96*apply(z_new[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda %*% z))),
         var_lower =  exp(z_new %*% lambda - 1.96*apply(z_new[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda %*% z))))

# civil rights

year_bs <- bs(nes_civil$year, df = 4, degree = 2)
x_civil <- model.matrix( ~ year_bs * party * educ, nes_civil)
z_civil <- model.matrix( ~ 1, nes_civil)
y_civil <- nes_civil %>% dplyr::select(intersect(item_names, names(.)))
hgrm_civil <- hgrm(y_civil, x_civil, z_civil)

data_new <- expand.grid(party = factor(1:3), year = range(nes_civil$year)[1]:range(nes_civil$year)[2], educ = factor(1:2))
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp * party * educ, data_new)
z_new <- model.matrix( ~ 1, data_new)

gamma <- coef_mean(hgrm_civil)$Est
gamma_index <- with(hgrm_civil, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
cov_gamma <- hgrm_civil$vcov[gamma_index, gamma_index]

civil_plot <- data_new %>%
  mutate(fitted =  x_new %*% gamma,
         fitted_se = apply(x_new[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         `Party ID` = factor(party, labels = c("Democrat", "Independent", "Republican")),
         educ = factor(educ, labels = c("No College", "College")))

x_civil2 <- model.matrix( ~ year_bs, nes_civil)
z_civil2 <- model.matrix( ~ year_bs, nes_civil)
hgrm_civil2 <- hgrm2(y_civil, x_civil2, z_civil2)

data_new <- expand.grid(year = range(nes_civil$year)[1]:range(nes_civil$year)[2])
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp, data_new)
z_new <- model.matrix( ~ year_bs_simp, data_new)

gamma <- coef_mean(hgrm_civil2)$Est
lambda <- coef_var(hgrm_civil2)$Est
gamma_index <- with(hgrm_civil2, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
lambda_index <- with(hgrm_civil2, if(q>1) seq(sum(H)+p, sum(H)+p+q-2) else NULL)
cov_gamma <- hgrm_civil2$vcov[gamma_index, gamma_index]
cov_lambda <- hgrm_civil2$vcov[lambda_index, lambda_index]

civil2_plot <- data_new %>%
  mutate(mean_est =  x_new %*% gamma,
         mean_se = apply(x_new[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se,
         var_est = exp(z_new %*% lambda),
         var_upper =  exp(z_new %*% lambda + 1.96*apply(z_new[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda %*% z))),
         var_lower =  exp(z_new %*% lambda - 1.96*apply(z_new[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda %*% z))))

# morality

year_bs <- bs(nes_moral$year, df = 4, degree = 2)
x_moral <- model.matrix( ~ year_bs * party * educ, nes_moral)
z_moral <- model.matrix( ~ 1, nes_moral)
y_moral <- nes_moral %>% dplyr::select(intersect(item_names, names(.)))
hgrm_moral <- hgrm(y_moral, x_moral, z_moral)

data_new <- expand.grid(party = factor(1:3), year = range(nes_moral$year)[1]:range(nes_moral$year)[2], educ = factor(1:2))
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp * party * educ, data_new)
z_new <- model.matrix( ~ 1, data_new)

gamma <- coef_mean(hgrm_moral)$Est
gamma_index <- with(hgrm_moral, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
cov_gamma <- hgrm_moral$vcov[gamma_index, gamma_index]

moral_plot <- data_new %>%
  mutate(fitted =  x_new %*% gamma,
         fitted_se = apply(x_new[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         `Party ID` = factor(party, labels = c("Democrat", "Independent", "Republican")),
         educ = factor(educ, labels = c("No College", "College")))

x_moral2 <- model.matrix( ~ year_bs, nes_moral)
z_moral2 <- model.matrix( ~ year_bs, nes_moral)
y_moral2 <- nes_moral %>% dplyr::select(intersect(item_names, names(.)))
hgrm_moral2 <- hgrm2(y_moral, x_moral2, z_moral2)

data_new <- expand.grid(year = range(nes_moral$year)[1]:range(nes_moral$year)[2])
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp, data_new)
z_new <- model.matrix( ~ year_bs_simp, data_new)

gamma <- coef_mean(hgrm_moral2)$Est
lambda <- coef_var(hgrm_moral2)$Est
gamma_index <- with(hgrm_moral2, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
lambda_index <- with(hgrm_moral2, if(q>1) seq(sum(H)+p, sum(H)+p+q-2) else NULL)
cov_gamma <- hgrm_moral2$vcov[gamma_index, gamma_index]
cov_lambda <- hgrm_moral2$vcov[lambda_index, lambda_index]

moral2_plot <- data_new %>%
  mutate(mean_est =  x_new %*% gamma,
         mean_se = apply(x_new[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se,
         var_est = exp(z_new %*% lambda),
         var_upper =  exp(z_new %*% lambda + 1.96*apply(z_new[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda %*% z))),
         var_lower =  exp(z_new %*% lambda - 1.96*apply(z_new[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda %*% z))))

# foreign policy

year_bs <- bs(nes_foreign$year, df = 3, degree = 2)
x_foreign <- model.matrix( ~ year_bs * party * educ, nes_foreign)
z_foreign <- model.matrix( ~ 1, nes_foreign)
y_foreign <- nes_foreign %>% dplyr::select(intersect(item_names, names(.)))
hgrm_foreign <- hgrm(y_foreign, x_foreign, z_foreign, control = list(max_iter = 500))

data_new <- expand.grid(party = factor(1:3), year = range(nes_foreign$year)[1]:range(nes_foreign$year)[2], educ = factor(1:2))
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp * party * educ, data_new)
z_new <- model.matrix( ~ 1, data_new)

gamma <- coef_mean(hgrm_foreign)$Est
lambda <- coef_var(hgrm_foreign)$Est
gamma_index <- with(hgrm_foreign, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
lambda_index <- with(hgrm_foreign, if(q>1) seq(sum(H)+p, sum(H)+p+q-2) else NULL)
cov_gamma <- hgrm_foreign$vcov[gamma_index, gamma_index]
cov_lambda <- hgrm_foreign$vcov[lambda_index, lambda_index]

foreign_plot <- data_new %>%
  mutate(fitted =  x_new %*% gamma,
         fitted_se = apply(x_new[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
        `Party ID` = factor(party, labels = c("Democrat", "Independent", "Republican")),
         educ = factor(educ, labels = c("No College", "College")))

x_foreign2 <- model.matrix( ~ year_bs, nes_foreign)
z_foreign2 <- model.matrix( ~ year_bs, nes_foreign)
hgrm_foreign2 <- hgrm2(y_foreign, x_foreign2, z_foreign2, control = list(max_iter = 500))

data_new <- expand.grid(year = range(nes_foreign$year)[1]:range(nes_foreign$year)[2])
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp, data_new)
z_new <- model.matrix( ~ year_bs_simp, data_new)

gamma <- coef_mean(hgrm_foreign2)$Est
lambda <- coef_var(hgrm_foreign2)$Est
gamma_index <- with(hgrm_foreign2, if(p>1) seq(sum(H)+1, sum(H)+p-1) else NULL)
lambda_index <- with(hgrm_foreign2, if(q>1) seq(sum(H)+p, sum(H)+p+q-2) else NULL)
cov_gamma <- hgrm_foreign2$vcov[gamma_index, gamma_index]
cov_lambda <- hgrm_foreign2$vcov[lambda_index, lambda_index]

foreign2_plot <- data_new %>%
  mutate(mean_est =  x_new %*% gamma,
         mean_se = apply(x_new[, -1], 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se,
         var_est = exp(z_new %*% lambda),
         var_upper =  exp(z_new %*% lambda + 1.96*apply(z_new[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda %*% z))),
         var_lower =  exp(z_new %*% lambda - 1.96*apply(z_new[, -1], 1, function(z) sqrt(t(z) %*% cov_lambda %*% z))))

# ideological constraint

nes_align <- nes_v1 %>% filter(nvalid_econ >=1, nvalid_econ_yr_level>=3,
                               nvalid_civil >=1, nvalid_civil_yr_level>=3,
                               nvalid_moral >=1, nvalid_moral_yr_level>=3) %>% dplyr::select(-school_busing7)

y_econ3 <- nes_align %>% dplyr::select(health_ins7:FS_assistblacks3)
y_civil3 <- nes_align %>% dplyr::select(negro_chan3:blacks_deserve_more5)
y_moral3 <- nes_align %>% dplyr::select(women_role7:abort4)

year_bs <- bs(nes_align$year, df = 4, degree = 2)
x3 <- model.matrix( ~ year_bs, nes_align)
z3 <- model.matrix( ~ year_bs, nes_align)

hgrm_econ3 <- hgrm(y_econ3, x3, z3)
hgrm_civil3 <- hgrm(y_civil3, x3, z3)
hgrm_moral3 <- hgrm(y_moral3, x3, z3)

nes_align_out <- nes_align %>% mutate(econ3_score = hgrm_econ3$scores$est,
                                      civil3_score = hgrm_civil3$scores$est,
                                      moral3_score = hgrm_moral3$scores$est)

end_time <- Sys.time()

nes_models_time <- end_time - start_time

save.image(file = "nes_out.RData")