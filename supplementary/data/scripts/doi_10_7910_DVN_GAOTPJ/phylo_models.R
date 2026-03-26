################################################################################
#### Phylogenetic models for mammals
#### Written by Hannah Correia | University of North Dakota | hec0003@auburn.edu
#### Adapted by Stephen Dobson | IPHC - CNRS Strasbourg | fsdobson@msn.com
#### Last updated 15 September 2025
################################################################################

#### Load libraries
library(TreeTools)
library(phytools)

library(tidyverse)
library(brms)
library(MCMCglmm) # for phylo relationship matrices only

library(ggpubr)
library(wesanderson)

library(bayesplot) # plotting brms
library(tidybayes)
library(ggdist)


#### Capture session info (for reproducibility checks)
# writeLines(capture.output(sessionInfo()), "sessionInfo_07022025.txt")


#### Read in phylogenetic tree ####
mamm_phylo <- read.tree("phylo_tree.tre")

## Check the tree
mamm_phylo
str(mamm_phylo)
names(mamm_phylo)
summary(mamm_phylo)
is.ultrametric(mamm_phylo)
is.rooted(mamm_phylo)
plot(mamm_phylo,no.margin=TRUE,edge.width=1.5,cex=0.7)

## Invert the phylogeny matrix
inv.phylo <- MCMCglmm::inverseA(mamm_phylo, nodes = "TIPS", scale = TRUE)
AM <- solve(inv.phylo$Ainv)
rownames(AM) <- rownames(inv.phylo$Ainv)


#### Read in MP data set with 98 populations and 60 species ####
mamm_data <- read.csv("mp_data.csv", header=TRUE)
mamm_data$animal <- mamm_data$phylo_name
mamm_data$phylo_name %in% mamm_phylo$tip.label 
dim(mamm_data)
names(mamm_data)

## Add extra column for models accounting for multiple populations per species
mamm_data$phylo_name2 <- mamm_data$phylo_name
str(mamm_data)

## Check all data species are now represented in phylo
phylo.names <- sort(unique(mamm_phylo$tip.label))
data.names <- sort(unique(mamm_data$phylo_name))
identical(data.names, phylo.names) ## Do they match?
# ## If FALSE, which species in the data file are not in the phylogeny?
# data.names[which(!data.names %in% phylo.names)]  ## Species in the data file that are not on the phylo
# phylo.names[which(!phylo.names %in% data.names)]  ## Species on the phylo that are not in the data file


## Need reference levels to be meaningful "base" factor of each
mamm_data$troph3 <- factor(mamm_data$troph3, 
                           levels = c("herbivore","omnivore","carnivore"))
mamm_data$habitat <- factor(mamm_data$habitat, 
                            levels = c("terrestrial", "arboreal", "aerial"))
mamm_data$SO <- factor(mamm_data$SO, levels = c("M1", "M0", "M+"))



###############################################################################
#### Summary statistics ####
###############################################################################

## Calculate bootstrapped mean and CI (non-parametric)
library(boot)

mean_fun <- function(vector, indices){
  v <- vector[indices] # allows boot to select sample
  stat <- mean(v, na.rm = TRUE) # calculate mean
  return(stat) # return mean value
}


#### MP 
set.seed(687)
boot_mean <- boot(mamm_data$pmult, statistic = mean_fun, R = 2000)
boot_mean  ## bootstrapped mean
boot.ci(boot_mean, type = "bca")  ## bootstrapped CI


#### avgbrood
set.seed(687)
boot_mean <- boot(mamm_data$avgbrood, statistic = mean_fun, R = 2000)
boot_mean  ## bootstrapped mean
boot.ci(boot_mean, type = "bca")  ## bootstrapped CI


#### sexual size dimorphism
set.seed(687)
boot_mean <- boot(mamm_data$rbm, statistic = mean_fun, R = 2000)
boot_mean  ## bootstrapped mean
boot.ci(boot_mean, type = "bca")  ## bootstrapped CI



#### habitat, diet, and mate co-habitation

## look at only unique species, since habitat repeated across all pop within species
species_summ <- mamm_data %>%  
  distinct(phylo_name, .keep_all = TRUE)

## number of spp per habitat
table(species_summ$habitat)  

## number of spp per diet
table(species_summ$troph3)  

## number of spp per mate co-habitation
table(species_summ$SO)

## number of observations per unique 3-some groups
mamm_data %>%
  group_by(habitat, troph3, SO) %>%
  count()



#### Transformations of avgbrood and rbm 

## density plots of avgbrood and log10(avgbrood) compared to Gaussian distribution
ggdensity(mamm_data$avgbrood, fill = "lightgray") +
  stat_overlay_normal_density(color = "red", linetype = "dashed") 
ggdensity(log10(mamm_data$avgbrood), fill = "lightgray") +
  stat_overlay_normal_density(color = "red", linetype = "dashed") 


## density plots of avgbrood and log10(avgbrood) compared to Gaussian distribution
ggdensity(mamm_data$rbm, fill = "lightgray") +
  stat_overlay_normal_density(color = "red", linetype = "dashed") 
ggdensity(log10(mamm_data$rbm), fill = "lightgray") +
  stat_overlay_normal_density(color = "red", linetype = "dashed") 



###############################################################################
#### Phylogenetic Mixed Models - Estimate trait effects on MP ####
###############################################################################

## Full model with all covariates 
mp.ntree.brm <- brm(round(pmult*nbrood) | trials(nbrood) ~ (1 | gr(phylo_name, cov = AM)) + 
                      (1 | phylo_name2) +  ## term accounts for multiple pops per spp
                      I(log10(avgbrood)) + 
                      I(log10(rbm)) + habitat + troph3 + SO,
                    data = mamm_data, 
                    family = "binomial",
                    data2 = list(AM = AM), 
                    sample_prior = "yes",
                    iter = 4000,
                    save_pars = save_pars(all = TRUE),
                    control = list(adapt_delta = 0.98),
                    silent = 2, refresh = 0, 
                    seed = 6765)


## Diagnostic plots
plot(mp.ntree.brm, nvariables = 3, ask = FALSE)
pp_check(mp.ntree.brm)  # posterior predictive check


#### Calculate ICC values ####

## Extract random variances from posterior draws
tau2_phy_post <- as_draws_matrix(mp.ntree.brm, "sd_phylo_name__Intercept")^2
tau2_spp_post <- as_draws_matrix(mp.ntree.brm, "sd_phylo_name2__Intercept")^2

## variance approximation for logistic regression; see Browne et al (2005) and Nakagawa & Schielzeth (2010)
var_dist_const <- (pi^2)/3  
sig2_logistic  <- rep(var_dist_const, length(tau2_phy_post))


#### Phylogenetic ICC (both calculations should match)

## Calculate a mean and CI for spp ICC using posterior draws from model:
## (uses same objects calculated for posterior phylo ICC above)
lambda_phylo <- tau2_phy_post/(tau2_phy_post + tau2_spp_post + sig2_logistic)
mean(lambda_phylo)  
quantile(lambda_phylo, c(0.025, 0.975))  

## Calculate a mean and CI for phylo ICC using hypothesis test from brms package:
## sig2 = (pi^2)/3 = 3.289868 approximation for logistic regression
hyp_phy <- "sd_phylo_name__Intercept^2 / (sd_phylo_name__Intercept^2 + sd_phylo_name2__Intercept^2 + 3.289868) = 0"
(hyp_phylo_mamm <- hypothesis(mp.ntree.brm, hyp_phy, class = NULL))  ## note same mean and CI as posterior draws
hyp_phylo_mamm$hypothesis


#### Species ICC (both calculations should match)
#### i.e., proportion of variance attributed to repeated spp (multiple populations)

## Calculate a mean and CI for spp ICC using posterior draws from model:
## (uses same objects calculated for posterior phylo ICC above)
lambda_spp <- tau2_spp_post/(tau2_phy_post + tau2_spp_post + sig2_logistic)
mean(lambda_spp)  
quantile(lambda_spp, c(0.025, 0.975)) 

## Calculate a mean and CI for spp ICC using hypothesis test from brms package:
## sig2 = (pi^2)/3 = 3.289868 approximation for logistic regression
hyp_spp <- "sd_phylo_name2__Intercept^2 / (sd_phylo_name__Intercept^2 + sd_phylo_name2__Intercept^2 + 3.289868) = 0"
(hyp_spp_mamm <- hypothesis(mp.ntree.brm, hyp_spp, class = NULL))  ## note same mean and CI as posterior draws
hyp_spp_mamm$hypothesis



#### Proportions of TOTAL variance due to fixed, random effects

## Extract fixed‐effect estimates (draws):
betas <- as_draws_matrix(mp.ntree.brm, c("b_Intercept", "b_Ilog10avgbrood","b_Ilog10rbm", 
                                         "b_habitatarboreal","b_habitataerial", 
                                         "b_troph3omnivore", "b_troph3carnivore", 
                                         "b_SOM0","b_SOMP"))
## Build the (unweighted) design matrix for the fixed effects:
X <- model.matrix(~ I(log10(avgbrood)) + I(log10(rbm)) + habitat + troph3 + SO, data = mamm_data)

## Get the weights (number of trials per observation):
w <- mamm_data$nbrood

## Weight the design matrix by sqrt(n_trials):
X_w <- X * sqrt(w)

## Compute weighted linear predictor and its variance (with credible intervals)
eta_w <- X_w %*% t(betas)
sigma2_fixed <- as.vector(apply(eta_w, 2, var))


## total variance
var_total <- (sigma2_fixed + tau2_phy_post + tau2_spp_post + sig2_logistic)

## Calculate R^2 values
r2_conditional <- (sigma2_fixed + tau2_phy_post + tau2_spp_post) / var_total
r2_fixed <- sigma2_fixed / var_total
r2_random <- (tau2_phy_post + tau2_spp_post)/var_total
r2_phylo <- tau2_phy_post / var_total
r2_spp <- tau2_spp_post / var_total
r2_resid <- sig2_logistic / var_total

## summarize variance components
summaries_logit_6 <- rbind(
  R2_conditional = c(mean(r2_conditional), quantile(r2_conditional, c(0.025,0.975))),
  R2_fixed = c(mean(r2_fixed), quantile(r2_fixed, c(0.025,0.975))),
  R2_random = c(mean(r2_phylo + r2_spp), quantile(r2_phylo + r2_spp, c(0.025,0.975))),
  R2_phylo = c(mean(r2_phylo), quantile(r2_phylo, c(0.025,0.975))),
  R2_spp = c(mean(r2_spp), quantile(r2_spp, c(0.025,0.975)))
)
colnames(summaries_logit_6) <- c("mean","CI2.5","CI97.5")
print(summaries_logit_6, digits = 3)



###############################################################################
#### Coefficients and One-sided tests using Bayesian probability statements ####
###############################################################################


#### Model coefficient estimates for fixed effects ####

## Mean and 90% credible intervals for parameters
summary(mp.ntree.brm, prob = 0.9)  # Table 2

## Convert coefficients from log-odds to odds (exponentiate)
parameters::model_parameters(mp.ntree.brm, centrality = "mean", ci = 0.9, exponentiate = TRUE)



#### Two-sided tests for fixed effects ####


## Two-sided test for avgbrood

## Extract posterior samples for avgbrood
post_avgbrood <- as_draws_df(mp.ntree.brm, variable = "b_Ilog10avgbrood")

## Compute proportion of the posterior distribution that supports two-sided hypotheses:
2*min(mean(post_avgbrood < 0), mean(post_avgbrood > 0))  
## i.e., 50% probability that avgbrood has non-zero effect on MP



## One-sided tests for rbm

## Extract posterior samples for rbm
post_rbm <- as_draws_df(mp.ntree.brm, variable = "b_Ilog10rbm")

## Compute proportion of the posterior distribution that supports one-sided hypotheses:
# Proportion of posterior where log10(rbm) - log10(1) > 0
mean(post_rbm < 0)



## One-sided tests for habitat

## Extract posterior samples for the habitat levels
post_arb <- as_draws_df(mp.ntree.brm, variable = "b_habitatarboreal")
post_aer <- as_draws_df(mp.ntree.brm, variable = "b_habitataerial")

## Compute proportion of the posterior distribution that supports one-sided hypotheses:

# Proportion of posterior where arboreal > terrestrial (ref level)
mean(post_arb > 0)  # i.e., 98% probability that arboreal has larger log-odds MP than terrestrial

# Proportion of posterior where aerial > terrestrial
mean(post_aer > 0)  # i.e., 97% probability that aerial has larger log-odds MP than terrestrial

# Proportion of posterior where aerial > arboreal
mean(post_aer > post_arb)  # i.e., 16% probability that aerial has larger log-odds MP than arboreal



## One-sided tests for troph3

## Extract posterior samples for the trophic levels
post_omni <- as_draws_df(mp.ntree.brm, variable = "b_troph3omnivore")
post_carn <- as_draws_df(mp.ntree.brm, variable = "b_troph3carnivore")

## Compute proportion of the posterior distribution that supports one-sided hypotheses:

# Proportion of posterior where omnivore < herbivore (ref level)
mean(post_omni < 0)  # i.e., 22% probability that omnivore has smaller log-odds MP than herbivore

# Proportion of posterior where carnivore < herbivore
mean(post_carn < 0)  # i.e., 17% probability that carnivore has smaller log-odds MP than herbivore

# Proportion of posterior where carnivore < omnivore
mean(post_carn < post_omni)  # i.e., 4% probability that carnivore has smaller log-odds MP than omnivore



## One-sided tests for SO

## Extract posterior samples for the SO levels
post_som0 <- as_draws_df(mp.ntree.brm, variable = "b_SOM0")
post_somp <- as_draws_df(mp.ntree.brm, variable = "b_SOMP")

## Compute proportion of the posterior distribution that supports one-sided hypotheses:

# Proportion of posterior where M0 > M1 (ref level)
mean(post_som0 > 0)  # i.e., 88% probability that M0 has larger log-odds MP than M1

# Proportion of posterior where M+ > M1 (ref level)
mean(post_somp > 0)  # i.e., 99% probability that M+ has larger log-odds MP than M1

# Proportion of posterior where M+ > M0
mean(post_somp > post_som0)  # i.e., 24% probability that M+ has larger log-odds MP than M0



###############################################################################
#### Plot coefficient estimates for full model ####
###############################################################################

## Customized plot using `bayesplot`
color_scheme_set("blue")
# color_scheme_set("red")
# color_scheme_set("green")  ## try different colors!

post <- as_draws_df(mp.ntree.brm)

p_area <- post %>% 
  select(starts_with("b_")) %>% 
  mcmc_areas(prob = 0.9, point_est = "mean", border_size = 0.1) +
  vline_0(color = "firebrick4", linetype = 2)

p_interval <- post %>% 
  select(starts_with("b_")) %>% 
  mcmc_intervals(prob = 0.5, prob_outer = 0.9, point_est = "mean") +
  vline_0(color = "firebrick4", linetype = 2)

figure3 <- p_area +  ## this can either be "p_area" or "p_interval" depending on preference
  scale_x_continuous(breaks = seq(-12, 16, by = 2)) +
  ## renaming the y-axis labels
  scale_y_discrete(limits = c("b_SOMP", "b_SOM0", 
                              "b_troph3carnivore", "b_troph3omnivore", 
                              "b_habitataerial", "b_habitatarboreal",
                              "b_Ilog10rbm", "b_Ilog10avgbrood", "b_Intercept"), 
                   labels = c("b_SOMP" = "Social org: M+",
                              "b_SOM0" = "Social org: M0",
                              "b_troph3carnivore" = "Trophic: carnivore",
                              "b_troph3omnivore" = "Trophic: omnivore",
                              "b_habitataerial" = "Habitat: aerial",
                              "b_habitatarboreal" = "Habitat: arboreal",
                              "b_Ilog10rbm" = bquote(log[10]*"(rbm)"), 
                              "b_Ilog10avgbrood" = bquote(log[10]*"(avgbrood)"),
                              "b_Intercept" = "Intercept")) +                         
  theme_bw() +
  labs(x = "Expected log-odds of MP (90% CrI)", y = "") +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())

figure3
# ggexport(figure3, filename = "figure3.pdf", width = 10, height = 7, units = "in")
# ggsave(filename = "figure3.jpeg", plot = figure3, width = 10, height = 7, units = "in")
# ggsave(filename = "figure3.eps", plot = figure3, device = cairo_ps, dpi = 1200, width = 10, height = 7, units = "in")



###############################################################################
#### Plot conditional effects for full model separately with defaults ####
###############################################################################
## Default uses the reference level for factors and the mean for continuous variables

## attractive colors for line plots
wes_red <- wes_palette("Darjeeling1", 5, type = "discrete")[1]
wes_teal <- wes_palette("Darjeeling1", 5, type = "discrete")[2]
## attractive colors for boxplots
wes3 <- wes_palette("Darjeeling1", 5, type = "discrete")[c(1,3,5)]
wes3alt <- wes_palette("Darjeeling1", 5, type = "discrete")[c(1,3,2)]


## Conditional effect of avgbrood (terrestrial herbivore M0)
ce_avgbrood <- conditional_effects(mp.ntree.brm, effects = "avgbrood")
# spaghetti = T, ndraws = 200) 
# conditions = data.frame(nbrood = c(20), 
#                         pmult = c(0.3662527, 0.75)), 
# re_formula = NULL)
## get and modify line plot
ugly_avgbrood <- plot(ce_avgbrood, plot = FALSE, rug = FALSE, 
                      ## default rug has incorrect transformation; turn off here and add manually below
                      line_args = list(color = wes_red, fill = "grey70"),
                      rug_args = list(color = wes_teal))[[1]]
## add points and change axis labels manually with ggplot2
g1 <- ugly_avgbrood + 
  geom_point(
    aes(x = avgbrood, y = round(pmult*nbrood)/nbrood), 
    data = mp.ntree.brm$data,  # this is the key!
    color = "black", size = 2, alpha = 0.5,
    inherit.aes = FALSE  # ignore the ymin and ymax settings used elsewhere
  ) +
  # geom_rug(
  #   aes(x = avgbrood),
  #   data = mp.ntree.brm$data,
  #   sides = "b", color = wes_teal,
  #   inherit.aes = FALSE
  # ) +
  labs(x = "Average litter size", y = "Proportion of\nmultiple paternity") +
  theme_bw() + theme(plot.margin = unit(c(1,2,1,1), "lines"))
g1
# ggsave("model_ce_avgbrood.jpeg", plot = last_plot(), 
#        width = 8, height = 5, units = "in")


## Conditional effect of sexual size dimorphism 
ce_rbm <- conditional_effects(mp.ntree.brm, effects = "rbm")
## get and modify line plot
ugly_rbm <- plot(ce_rbm, plot = FALSE, rug = FALSE, 
                 ## default rug has incorrect transformation; turn off here and add manually below
                 line_args = list(color = wes_red, fill = "grey70"),
                 rug_args = list(color = wes_teal))[[1]]
## add points and change axis labels manually with ggplot2
g2 <- ugly_rbm + 
  geom_point(
    aes(x = rbm, y = round(pmult*nbrood)/nbrood), 
    data = mp.ntree.brm$data,  # this is the key!
    color = "black", size = 2, alpha = 0.5,
    inherit.aes = FALSE  # ignore the ymin and ymax settings used elsewhere
  ) +
  # geom_rug(
  #   aes(x = rbm),
  #   data = mp.ntree.brm$data,
  #   sides = "b", color = wes_teal,
  #   inherit.aes = FALSE
  # ) +
  labs(x = "Sexual size dimorphism", y = "Proportion of\nmultiple paternity") +
  theme_bw() + theme(plot.margin = unit(c(1,1,1,2), "lines"))
g2
# ggsave("model_ce_rbm.jpeg", plot = last_plot(), 
#        width = 8, height = 5, units = "in")


## Conditional effect of habitat 
ce_habitat <- conditional_effects(mp.ntree.brm, effects = "habitat")
## get and modify line plot
ugly_habitat <- plot(ce_habitat, plot = FALSE, rug = FALSE, 
                     cat_args = list(color = wes3, shape = 15, size = 5),
                     errorbar_args = list(color = wes3, linewidth = 1, width = 0.1),
                     rug_args = list(color = wes_teal))[[1]]
## add points and change axis labels manually with ggplot2
g3 <- ugly_habitat + 
  geom_jitter(
    aes(x = habitat, y = round(pmult*nbrood)/nbrood), 
    data = mp.ntree.brm$data,  # this is the key!
    color = "black", size = 1, width = 0.1, alpha = 0.5,
    inherit.aes = FALSE  # ignore the ymin and ymax settings used elsewhere
  ) +
  labs(x = "Habitat", y = "Proportion of\nmultiple paternity") +
  theme_bw() + theme(plot.margin = unit(c(1,1,1,1), "lines"))
g3
# ggsave("model_ce_habitat.jpeg", plot = last_plot(), 
#        width = 8, height = 5, units = "in")


## Conditional effect of trophic level 
ce_trophic <- conditional_effects(mp.ntree.brm, effects = "troph3")
## get and modify line plot
ugly_trophic <- plot(ce_trophic, plot = FALSE, rug = FALSE, 
                     cat_args = list(color = wes3, shape = 15, size = 5),
                     errorbar_args = list(color = wes3, linewidth = 1, width = 0.1),
                     rug_args = list(color = wes_teal))[[1]]
## add points and change axis labels manually with ggplot2
g4 <- ugly_trophic + 
  geom_jitter(
    aes(x = troph3, y = round(pmult*nbrood)/nbrood), 
    data = mp.ntree.brm$data,  # this is the key!
    color = "black", size = 1, width = 0.1, alpha = 0.5,
    inherit.aes = FALSE  # ignore the ymin and ymax settings used elsewhere
  ) +
  labs(x = "Trophic level", y = "Proportion of\nmultiple paternity") +
  theme_bw() + theme(plot.margin = unit(c(1,1,1,1), "lines"))
g4
# ggsave("model_ce_trophic.jpeg", plot = last_plot(), 
#        width = 8, height = 5, units = "in")


## Conditional effect of social organization
ce_social <- conditional_effects(mp.ntree.brm, effects = "SO")
## get and modify line plot
ugly_social <- plot(ce_social, plot = FALSE, rug = FALSE, 
                    cat_args = list(color = wes3, shape = 15, size = 5),
                    errorbar_args = list(color = wes3, linewidth = 1, width = 0.1),
                    rug_args = list(color = wes_teal))[[1]]
## add points and change axis labels manually with ggplot2
g5 <- ugly_social + 
  geom_jitter(
    aes(x = SO, y = round(pmult*nbrood)/nbrood), 
    data = mp.ntree.brm$data,  # this is the key!
    color = "black", size = 1, width = 0.1, alpha = 0.5,
    inherit.aes = FALSE  # ignore the ymin and ymax settings used elsewhere
  ) +
  labs(x = "Social organization", y = "Proportion of\nmultiple paternity") +
  theme_bw() + theme(plot.margin = unit(c(1,1,1,1), "lines"))
g5
# ggsave("model_ce_so.jpeg", plot = last_plot(), 
#        width = 8, height = 5, units = "in")


#### Plot all 5 conditional plots together
figure4 <- ggarrange(ggarrange(g1, g2, ncol = 2, labels = c("a","b")),
                     ggarrange(g3, g4, g5, ncol = 3, labels = c("c", "d", "e")),
                     nrow = 2)
figure4
# ggexport(figure4, filename = "figure4.pdf", width = 10, height = 7, units = "in")
# ggsave(filename = "figure4.jpeg", plot = figure4, width = 10, height = 7, units = "in")
# ggsave(filename = "figure4.eps", plot = figure4, device = cairo_ps, dpi = 1200, width = 10, height = 7, units = "in")

