#### 1. Creating Stan model from JAGS code ####
## Stan model code translated from JAGS code in Gill book
stanmod <- "
data {
  int<lower=1> VALUE; // Number of groups (or individuals)
  int<lower=1> TIME;  // Number of time points
  vector[TIME] x1;    // Covariate x1
  matrix[VALUE, TIME] y; // Response variable matrix
  
}

parameters {
  real mu_beta0;           // Mean of beta0
  real<lower=0> tau_beta0; // Precision of beta0
  real mu_beta1;           // Mean of beta1
  real<lower=0> tau_beta1; // Precision of beta1
  real<lower=0> tau;       // Precision of the observation error
  vector[VALUE] beta0;     // Intercepts
  vector[VALUE] beta1;     // Slopes
}

transformed parameters {
  real<lower=0> sigma_beta0 = sqrt(1 / tau_beta0); // Convert precision to standard deviation
  real<lower=0> sigma_beta1 = sqrt(1 / tau_beta1); // Convert precision to standard deviation
  real<lower=0> sigma = sqrt(1 / tau);             // Convert precision to standard deviation
  vector[TIME] x1_centered = x1 - mean(x1);        // Centered x1
}

model {
  // Priors
  tau ~ gamma(1e-2, 1e-2);
  mu_beta0 ~ normal(0, sqrt(1 / 1e-2));
  tau_beta0 ~ gamma(1e-2, 1e-2);
  mu_beta1 ~ normal(0, sqrt(1 / 1e-2));
  tau_beta1 ~ gamma(1e-2, 1e-2);
  
  beta0 ~ normal(mu_beta0, sigma_beta0);
  beta1 ~ normal(mu_beta1, sigma_beta1);
  
  // Likelihood
  for (i in 1:VALUE) {
    for (j in 1:TIME) {
      y[i, j] ~ normal(beta0[i] + beta1[i] * x1_centered[j], sigma);
    }
  }
}"

#### 2. Load data from BaM package and make list ####
## Load retail sales 
data(retail.sales, package="BaM")

## Reorganize data as list for model
rs.dat <- list(
  TIME = nrow(retail.sales), 
  VALUE = 6, 
  y = t(as.matrix(retail.sales[,-1]))/1000, 
  x1 = c(retail.sales[,1])
)

#### 3. Estimate model ####
## This may take some time
stanest <- stan(model_code=stanmod, 
                data = rs.dat, 
                chains=4, 
                iter=10000, 
                seed=519)

#### 4. Prepare data for Table A1 ####
## extract beta1 posterior draws
b1_post <- rstan::extract(stanest, "beta1")$beta1

## name the columns of the beta1 posterior
colnames(b1_post) <- rownames(rs.dat$y)

## summarise the beta1 posteriors
post_sum <- stanest %>% 
  tidy_draws() %>% 
  select(1:3, starts_with("beta1")) %>% 
  setNames(c(".chain", ".iteration", ".draw", rownames(rs.dat$y))) %>% 
  gather_variables() %>% 
  group_by(.variable) %>% 
  mutate(.variable = factor(.variable, levels=c("DSB", "EMP", "BDG", "CAR", "FRN", "GMR"))) %>% 
  summarise_draws(median, mad, Rhat) %>% 
  ungroup() %>% 
  arrange(.variable) %>% 
  select(-2) %>% 
  dplyr::rename(indicator = .variable)  %>% 
  left_join(as_tibble(t(HDInterval::hdi(b1_post))) %>% 
  mutate(indicator = colnames(b1_post)))

#### 5. Make Table A1 ####
xtable:::print.xtable(xtable(post_sum, digits=3), 
      file="results/TableA1.tex", 
      include.rownames=FALSE, 
      include.colnames=FALSE, 
      hline.after = NULL, 
      only.contents = TRUE
      )


#### 6. Prepare data for Table A2 ####
## Build a contrast matrix to help in doing the pairwise 
## comparison of chains. 
combs <- combn(6,2)
D <- matrix(0, nrow=6, ncol=ncol(combs))
D[cbind(combs[1,], 1:ncol(combs))] <- -1
D[cbind(combs[2,], 1:ncol(combs))] <- 1

## Reorder the columns of beta1 posterior to be 
## increasing in average value
b1_post <- b1_post[, order(colMeans(b1_post))]

## Calculate the pairwise differences for each pair of 
## chain values. 
diff_1 <- b1_post %*% D

## Calculate posterior probability of the difference 
## being bigger than zero. 
p1 <- apply(diff_1, 2, \(x)mean(x > 0))

## Organize the data into a data frame. 
diffs <- tibble(
  smaller = colnames(b1_post)[combs[1,]], 
  larger = colnames(b1_post)[combs[2,]], 
  difference = colMeans(diff_1), 
  post_pr_diff = p1, 
  credible = ifelse(post_pr_diff > .95, "Yes", "No"))

## Calculate the 95% HDIs
hdis <- t(HDInterval::hdi(b1_post, .width=.95))

## Identify overlaps in the HDIs
olaps <- sapply(1:ncol(combs), \(i){
  ol <- hdis[combs[1,i], 2] > hdis[combs[2,i], 1]
  ifelse(ol, "Yes", "No")
})

## add overlaps to data
diffs$overlap <- olaps

#### 7. Make Table A2 ####
## Show all values
cmd <- vector(mode="character", length=2)
cmd[1] <- cmd[2] <- "\\rowcolor{white}"
print(xtable(diffs, digits=3), 
      file="results/TableA2.tex", 
      include.rownames=FALSE, 
      include.colnames=FALSE, 
      hline.after = NULL, 
      only.contents = TRUE, 
      add.to.row = list(pos = list(6, 10), command = cmd))


#### 8. Calling viztest ####
s1 <- structure(.Data = list(est=b1_post), 
                class="vtsim")
v1 <- viztest(s1, test_level = .05, include_zero=TRUE, level_increment = .001, 
              range_levels = c(.7, .9), cifun = "hdi")
v1

#### 9. Prepare HDIs for plot ####
## Calculate HDIs for 81.6% optimal intervals
hdii <- t(HDInterval::hdi(b1_post, credMass = .816)) %>% 
  as_tibble(rownames="indicator") %>% 
  rename(lwr_i = lower, upr_i = upper)

## Add HDIs to posterior summary
post_sum <- left_join(post_sum, hdii)

#### 10. Plot ####
ggplot(post_sum, 
       aes(y=reorder(indicator, median, mean))) + 
  geom_segment(aes(x=lower, xend=upper), linetype=1, color="gray50", linewidth=1.5) + 
  geom_segment(aes(x=lwr_i, xend=upr_i), linetype=1, color="black", linewidth=3) + 
  geom_point(aes(x=median), color="white") + 
  theme_classic() + 
  labs(x="Coefficient for Time", y="")
ggsave("results/FigureA6.png", height=4, width=7, units="in", dpi=300)
