rm(list = ls())

library("ltm")
library("pryr")
library("dplyr")
library("splines")
library("ggplot2")
library("hIRT")
library("rms")
library("statmod")

load("nes_out.RData")
load("nes7216.RData")

start_time <- Sys.time()

econ_var <- as.vector(var(x_econ %*% coef_mean(hgrm_econ)$Est) + 1)
civil_var <- as.vector(var(x_civil %*% coef_mean(hgrm_civil)$Est) + 1)
moral_var <- as.vector(var(x_moral %*% coef_mean(hgrm_moral)$Est) + 1)
foreign_var <- as.vector(var(x_foreign %*% coef_mean(hgrm_foreign)$Est) + 1)

K <- 500

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

keep_pca <- union(names(which(number_years>=11)), c("defense_spend7", "FS_space3"))

nes_econ2 <- nes_econ %>% dplyr::select(year:income, one_of(keep_pca)) %>% na.omit()
nes_civil2 <- nes_civil %>% dplyr::select(year:income, one_of(keep_pca)) %>% na.omit()
nes_moral2 <- nes_moral %>% dplyr::select(year:income, one_of(keep_pca)) %>% na.omit()
nes_foreign2 <- nes_foreign %>% dplyr::select(year:income, one_of(keep_pca)) %>% na.omit()

## economics ##

n_econ <- nrow(nes_econ2)

# year splines
year_bs <- bs(nes_econ2$year, knots = attr(bs(nes_econ$year, df = 4, degree = 2), "knots"), degree = 2)
  
# pca scores
y_econ <- dplyr::select(nes_econ2, one_of(keep_pca))
nes_econ2$econ_scores <- princomp(y_econ, cor = TRUE)$scores[, 1] %>% scale() %>% `*`(sqrt(econ_var))
nes_econ2$econ_scores <- with(nes_econ2, econ_scores * sign(mean(econ_scores[party==3])-mean(econ_scores[party==1])))

# regression
econ_lm <- lm(econ_scores ~ year_bs * party * educ, data = nes_econ2)
p <- length(econ_lm$coefficients)
gamma <- econ_lm$coefficients

# new data
data_new <- expand.grid(party = factor(1:3),
                        year = range(nes_econ2$year)[1]:range(nes_econ2$year)[2],
                        educ = factor(1:2))
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp * party * educ, data_new)

# bootstrap

gamma_holder <- matrix(NA, nrow = K, ncol = length(gamma))
fitted_holder <- matrix(NA, nrow = K, ncol = nrow(x_new))

for (k in 1:K){
  
  # bt sample
  index_bt <- sample.int(nrow(nes_econ2), replace = TRUE)
  nes_econ2bt <- nes_econ2[index_bt, ]
  
  # pca scores
  y_econ <- dplyr::select(nes_econ2bt, one_of(keep_pca))
  nes_econ2bt$econ_scores <- princomp(y_econ, cor = TRUE)$scores[, 1] %>% as.vector() %>% scale() %>% `*`(sqrt(econ_var))
  nes_econ2bt$econ_scores <- with(nes_econ2bt, econ_scores * sign(mean(econ_scores[party==3])-mean(econ_scores[party==1])))
  
  # year splines
  year_bs_bt <- bs(nes_econ2bt$year, knots = attr(year_bs, "knots"), degree = 2)
  
  # regression
  econ_lm <- lm(econ_scores ~ year_bs_bt * party * educ, data = nes_econ2bt)
  econ_coef <- econ_lm$coefficients 
  gamma_holder[k, ] <- econ_coef
  fitted_holder[k, ] <- x_new %*% econ_coef

}

cov_gamma <- cov(gamma_holder)

econ_pca1 <- data_new %>%
  mutate(fitted =  x_new %*% gamma,
         fitted_se = apply(x_new, 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         fitted_upper = apply(fitted_holder, 2, quantile, 0.975),
         fitted_lower = apply(fitted_holder, 2, quantile, 0.025),
         `Party ID` = factor(party, labels = c("Democrat", "Independent", "Republican")),
         educ = factor(educ, labels = c("No College", "College")))

## civil rights issues ##

n_civil <- nrow(nes_civil2)

# pca scores
y_civil <- dplyr::select(nes_civil2, one_of(keep_pca))
nes_civil2$civil_scores <- princomp(y_civil, cor = TRUE)$scores[, 1] %>% as.vector() %>% scale() %>% `*`(sqrt(civil_var))
nes_civil2$civil_scores <- with(nes_civil2, civil_scores * sign(mean(civil_scores[party==3])-mean(civil_scores[party==1])))

# year splines
year_bs <- bs(nes_civil2$year, knots = attr(bs(nes_civil$year, df = 4, degree = 2), "knots"), degree = 2)

# regression
civil_lm <- lm(civil_scores ~ year_bs * party * educ, data = nes_civil2)
gamma <- civil_lm$coefficients

# new data
data_new <- expand.grid(party = factor(1:3),
                        year = range(nes_civil2$year)[1]:range(nes_civil2$year)[2],
                        educ = factor(1:2))
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp * party * educ, data_new)

# bootstrap

gamma_holder <- matrix(NA, nrow = K, ncol = length(gamma))
fitted_holder <- matrix(NA, nrow = K, ncol = nrow(x_new))

for (k in 1:K){
  
  # bt sample
  index_bt <- sample.int(nrow(nes_civil2), replace = TRUE)
  nes_civil2bt <- nes_civil2[index_bt, ]
  
  # pca scores
  y_civil <- dplyr::select(nes_civil2bt, one_of(keep_pca))
  nes_civil2bt$civil_scores <- princomp(y_civil, cor = TRUE)$scores[, 1] %>% as.vector() %>% scale() %>% `*`(sqrt(civil_var))
  nes_civil2bt$civil_scores <- with(nes_civil2bt, civil_scores * sign(mean(civil_scores[party==3])-mean(civil_scores[party==1])))
  
  # year splines
  year_bs_bt <- bs(nes_civil2bt$year, knots = attr(year_bs, "knots"), degree = 2)
  
  # regression
  civil_lm <- lm(civil_scores ~ year_bs_bt * party * educ, data = nes_civil2bt)
  civil_coef <- civil_lm$coefficients
  gamma_holder[k, ] <- civil_coef
  fitted_holder[k, ] <- x_new %*% civil_coef
}

cov_gamma <- cov(gamma_holder)

civil_pca1 <- data_new %>%
  mutate(fitted =  x_new %*% gamma,
         fitted_se = apply(x_new, 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         fitted_upper = apply(fitted_holder, 2, quantile, 0.975),
         fitted_lower = apply(fitted_holder, 2, quantile, 0.025),
         `Party ID` = factor(party, labels = c("Democrat", "Independent", "Republican")),
         educ = factor(educ, labels = c("No College", "College")))

## moral issues ##

n_moral <- nrow(nes_moral2)

# pca scores
y_moral <- dplyr::select(nes_moral2, one_of(keep_pca))
nes_moral2$moral_scores <- princomp(y_moral, cor = TRUE)$scores[, 1] %>% as.vector() %>% scale() %>% `*`(sqrt(moral_var))
nes_moral2$moral_scores <- with(nes_moral2, moral_scores * sign(mean(moral_scores[party==3])-mean(moral_scores[party==1])))

# year splines
year_bs <- bs(nes_moral2$year, knots = attr(bs(nes_moral$year, df = 4, degree = 2), "knots"), degree = 2)

# regression
moral_lm <- lm(moral_scores ~ year_bs * party * educ, data = nes_moral2)
gamma <- moral_lm$coefficients

# new data
data_new <- expand.grid(party = factor(1:3),
                        year = range(nes_moral2$year)[1]:range(nes_moral2$year)[2],
                        educ = factor(1:2))
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp * party * educ, data_new)

# bootstrap

gamma_holder <- matrix(NA, nrow = K, ncol = length(gamma))
fitted_holder <- matrix(NA, nrow = K, ncol = nrow(x_new))

for (k in 1:K){

  # bt sample
  index_bt <- sample.int(nrow(nes_moral2), replace = TRUE)
  nes_moral2bt <- nes_moral2[index_bt, ]
  
  # pca scores
  y_moral <- dplyr::select(nes_moral2bt, one_of(keep_pca)) 
  nes_moral2bt$moral_scores <- princomp(y_moral, cor = TRUE)$scores[, 1] %>% as.vector() %>% scale() %>% `*`(sqrt(moral_var))
  nes_moral2bt$moral_scores <- with(nes_moral2bt, moral_scores * sign(mean(moral_scores[party==3])-mean(moral_scores[party==1])))
  
  # year splines
  year_bs_bt <- bs(nes_moral2bt$year, knots = attr(year_bs, "knots"), degree = 2)
  
  # regression
  moral_lm <- lm(moral_scores ~ year_bs_bt * party * educ, data = nes_moral2bt)
  moral_coef <- moral_lm$coefficients
  gamma_holder[k, ] <- moral_coef
  fitted_holder[k, ] <- x_new %*% moral_coef
}

cov_gamma <- cov(gamma_holder)

moral_pca1 <- data_new %>%
  mutate(fitted =  x_new %*% gamma,
         fitted_se = apply(x_new, 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         fitted_upper = apply(fitted_holder, 2, quantile, 0.975),
         fitted_lower = apply(fitted_holder, 2, quantile, 0.025),
         `Party ID` = factor(party, labels = c("Democrat", "Independent", "Republican")),
         educ = factor(educ, labels = c("No College", "College")))

## foreign policy issues ##

n_foreign <- nrow(nes_foreign2)

# pca scores
y_foreign <- dplyr::select(nes_foreign2, one_of(keep_pca))
nes_foreign2$foreign_scores <- princomp(y_foreign, cor = TRUE)$scores[, 1] %>% as.vector() %>% scale() %>% `*`(sqrt(foreign_var))
nes_foreign2$foreign_scores <- with(nes_foreign2, foreign_scores * sign(mean(foreign_scores[party==3])-mean(foreign_scores[party==1])))

# year splines
year_bs <- bs(nes_foreign2$year, knots = attr(bs(nes_foreign$year, df = 3, degree = 2), "knots"), degree = 2)

# regression
foreign_lm <- lm(foreign_scores ~ year_bs * party * educ, data = nes_foreign2)
gamma <- foreign_lm$coefficients

# new data
data_new <- expand.grid(party = factor(1:3),
                        year = range(nes_foreign2$year)[1]:range(nes_foreign2$year)[2],
                        educ = factor(1:2))
year_bs_simp <- bs(data_new$year, knots = attr(year_bs, "knots"), degree = 2)
x_new <- model.matrix( ~ year_bs_simp * party * educ, data_new)

# bootstrap

gamma_holder <- matrix(NA, nrow = K, ncol = length(gamma))
fitted_holder <- matrix(NA, nrow = K, ncol = nrow(x_new))

for (k in 1:K){
  
  # bt sample
  index_bt <- sample.int(nrow(nes_foreign2), replace = TRUE)
  nes_foreign2bt <- nes_foreign2[index_bt, ]
  
  # pca scores
  y_foreign <- dplyr::select(nes_foreign2bt, one_of(keep_pca))
  nes_foreign2bt$foreign_scores <- princomp(y_foreign, cor = TRUE)$scores[, 1]  %>% scale() %>% `*`(sqrt(foreign_var))
  nes_foreign2bt$foreign_scores <- with(nes_foreign2bt, foreign_scores * sign(mean(foreign_scores[party==3])-mean(foreign_scores[party==1])))
  
  # year splines
  year_bs_bt <- bs(nes_foreign2bt$year, knots = attr(year_bs, "knots"), degree = 2)
  
  # regression
  foreign_lm <- lm(foreign_scores ~ year_bs_bt * party * educ, data = nes_foreign2bt)
  foreign_coef <- foreign_lm$coefficients
  gamma_holder[k, ] <- foreign_coef
  fitted_holder[k, ] <- x_new %*% foreign_coef
}

cov_gamma <- cov(gamma_holder)

foreign_pca1 <- data_new %>%
  mutate(fitted =  x_new %*% gamma,
         fitted_se = apply(x_new, 1, function(x) sqrt(t(x) %*% cov_gamma %*% x)),
         fitted_upper = apply(fitted_holder, 2, quantile, 0.975),
         fitted_lower = apply(fitted_holder, 2, quantile, 0.025),
         `Party ID` = factor(party, labels = c("Democrat", "Independent", "Republican")),
         educ = factor(educ, labels = c("No College", "College")))

end_time <- Sys.time()

nes_pca_time <- end_time - start_time

save.image(file = "nes_out_pca.RData")