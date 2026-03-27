#import dataset_petandcovid.csv and convert it as d

#libraries####
library(tidyverse)
library(brms)
library(coda)
library(rstan)

#guide####
#A.descriptive: L44
#B.priors
##1.QOL: L49
##2.overall health: L62
##3.loneliness: L76
##4.mental health: L90
##5.stress: L100
##6.anxiety: L110
#C.univariable models
##1.QOL: L121
##2.overall health: L134
##3.loneliness: L147
##4.mental health: L160
##5.stress: L173
##6.anxiety: L185
#D.multivariable models
##1.QOL: L198
##2.overall health: L212
##3.loneliness: L226
##4.mental health: L240
##5.stress: L254
##6.anxiety: L268
#E.diagnostics
##1.QOL: L283
##2.overall health: L298
##3.loneliness: L313
##4.mental health: L328
##5.stress: L357
##6.anxiety: L 386
#F.plots
##4.mental health: L416
##5.stress: L457
##6.anxiety: L495

#A.descriptive####
describe(d)
describe.By(d, group = O1)

#B.priors####
##1.QOL####
priors_mQOL.0 <- c(set_prior("normal(0.76,0.14)", class = "Intercept"),
                   set_prior("cauchy(0,2)", class = "sigma"),
                   set_prior("normal(0,1)", class = "b"),
                   set_prior("cauchy(0,2)", class = "sd")
)

priors_mQOL.full <- c(set_prior("normal(0.76,0.14)", class = "Intercept"),
                      set_prior("cauchy(0,2)", class = "sigma"),
                      set_prior("normal(0,1)", class = "b"),
                      set_prior("cauchy(0,2)", class = "sd")
)

##2.overall health####
priors_mGH.0 <- c(set_prior("normal(83,14)", class = "Intercept"),#LM // modify intercept prior
                  set_prior("cauchy(0,2)", class = "sigma"), #LM
                  set_prior("normal(0,1)", class = "b"),
                  set_prior("cauchy(0,2)", class = "sd")
)

priors_mGH.full <- c(set_prior("normal(83,14)", class = "Intercept"),#LM // modify intercept prior
                     set_prior("cauchy(0,2)", class = "sigma"), #LM
                     set_prior("normal(0,1)", class = "b"),
                     set_prior("cauchy(0,2)", class = "sd")
)


##3.loneliness####
priors_mL.0 <- c(set_prior("normal(6.1, 2.1)", class = "Intercept"),#LM // modify intercept prior
                 set_prior("cauchy(0,2)", class = "sigma"), #LM
                 set_prior("normal(0,1)", class = "b"),
                 set_prior("cauchy(0,2)", class = "sd")
)

priors_mL.full <- c(set_prior("normal(6.1, 2.1)", class = "Intercept"),#LM // modify intercept prior
                    set_prior("cauchy(0,2)", class = "sigma"), #LM
                    set_prior("normal(0,1)", class = "b"),
                    set_prior("cauchy(0,2)", class = "sd")
)


##4.mental health####
priors_mMH.0 <- c(set_prior("normal(0,1)", class = "Intercept"),#LM // modify intercept prior
                  set_prior("normal(0,1)", class = "b"),
                  set_prior("cauchy(0,2)", class = "sd")
)

priors_mMH.full <- c(set_prior("normal(0,1)", class = "Intercept"),#LM // modify intercept prior
                     set_prior("normal(0,1)", class = "b"),
                     set_prior("cauchy(0,2)", class = "sd")
)
##5.stress####
priors_mSS.0 <- c(set_prior("normal(0,1)", class = "Intercept"),#LM // modify intercept prior
                  set_prior("normal(0,1)", class = "b"),
                  set_prior("cauchy(0,2)", class = "sd")
)
priors_mSS.full <- c(set_prior("normal(0,1)", class = "Intercept"),#LM // modify intercept prior
                     set_prior("normal(0,1)", class = "b"),
                     set_prior("cauchy(0,2)", class = "sd")
)

##6.anxiety####
priors_mAx.0 <- c(set_prior("normal(0,1)", class = "Intercept"),#LM // modify intercept prior
                  set_prior("normal(0,1)", class = "b"),
                  set_prior("cauchy(0,2)", class = "sd")
)
priors_mAx.full <- c(set_prior("normal(0,1)", class = "Intercept"),#LM // modify intercept prior
                     set_prior("normal(0,1)", class = "b"),
                     set_prior("cauchy(0,2)", class = "sd")
)

#C.univariable models####
##1.QOL####
mQOL.0 <- brm(QOL ~ O1 + (1|region), 
              data = d, 
              family = gaussian,
              iter  = 3000, chains = 3, 
              warmup = 1000, 
              seed  = 123, 
              prior = priors_mQOL.0,
              sample_prior = TRUE, 
              control = list(adapt_delta = 0.99, max_treedepth = 15) 
)
summary(mQOL.0)

##2.overall health####
mGH.0 <- brm(GH ~ O1 + (1|region), 
             data = d, 
             family = gaussian,
             iter  = 3000, chains = 3, 
             warmup = 1000, 
             seed  = 123, 
             prior = priors_mGH.0,
             sample_prior = TRUE, 
             control = list(adapt_delta = 0.99, max_treedepth = 15) # change to avoid divergent transitions as the amount of numerical error was below the median over the iterations (assessed using pairs(); https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup) 
)
summary(mGH.0)

##3.loneliness####
mL.0 <- brm(L1 ~ O1 + (1|region), 
            data = d, 
            family = gaussian,
            iter  = 3000, chains = 3, 
            warmup = 1000, 
            seed  = 123, 
            prior = priors_mL.0,
            sample_prior = TRUE, 
            control = list(adapt_delta = 0.99, max_treedepth = 15) # change to avoid divergent transitions as the amount of numerical error was below the median over the iterations (assessed using pairs(); https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup) 
)
summary(mL.0)

##4.mental health####
mMH.0 <- brm(H3 ~ O1 + (1|region), 
             data = d, 
             family = cumulative("probit"),
             iter  = 3000, chains = 3, 
             warmup = 1000, 
             seed  = 123, 
             prior = priors_mMH.0,
             sample_prior = TRUE, 
             control = list(adapt_delta = 0.99, max_treedepth = 15) 
)
summary(mMH.0)

##5.stress####
mSS.0 <- brm(Q2 ~ O1 + (1|region), 
             data = d, 
             family = cumulative("probit"),
             iter  = 3000, chains = 3, 
             warmup = 1000, 
             seed  = 123, 
             prior = priors_mSS.0,
             sample_prior = TRUE, 
             control = list(adapt_delta = 0.99, max_treedepth = 15) # change to avoid divergent transitions as the amount of numerical error was below the median over the iterations (assessed using pairs(); https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup) 
)
summary(mSS.0)
##6.anxiety####
mAx.0 <- brm(Q1_cat ~ O1 + (1|region), 
             data = d, 
             family = cumulative("probit"),
             iter  = 3000, chains = 3, 
             warmup = 1000, 
             seed  = 123, 
             prior = priors_mAx.0,
             sample_prior = TRUE, 
             control = list(adapt_delta = 0.99, max_treedepth = 15) 
)
summary(mAx.0)
#D.multivariable models####
##1.QOL####
mQOL.full <- brm(QOL ~ O1 + D1 + H2 + D5 + D67 + D4 + D11 + D8 + H4  + O8b + P1 + S + (1|region), 
                 data = d, 
                 family = gaussian,
                 iter  = 3000, chains = 3, 
                 warmup = 1000, 
                 seed  = 123, 
                 prior = priors_mQOL.full,
                 sample_prior = TRUE,
                 init_r = 0.05, 
                 control = list(adapt_delta = 0.99, max_treedepth = 15)
)
summary(mQOL.full)

##2.overall health####
mGH.full <- brm(GH ~ O1 + D1 + H2 + D5 + D67 + D4 + D11 + D8 + H4  + O8b + P1 + S + (1|region), 
                data = d, 
                family = gaussian,
                iter  = 3000, chains = 3, 
                warmup = 1000, 
                seed  = 123, 
                prior = priors_mGH.full,
                sample_prior = TRUE,
                init_r = 0.05, 
                control = list(adapt_delta = 0.99, max_treedepth = 15)
)
summary(mGH.full)

##3.loneliness####
mL.full <- brm(L1 ~ O1 + D1 + H2 + D5 + D67 + D4 + D11 + D8 + H4  + O8b + P1 + S + (1|region), 
               data = d, 
               family = gaussian,
               iter  = 3000, chains = 3, 
               warmup = 1000, 
               seed  = 123, 
               prior = priors_mL.full,
               sample_prior = TRUE,
               init_r = 0.05, 
               control = list(adapt_delta = 0.99, max_treedepth = 15)
)
summary(mL.full)

##4.mental health####
mMH.full <- brm(H3 ~ O1 + D1 + H2 + D5 + D67 + D4 + D11 + D8 + H4  + O8b + P1 + S + (1|region), 
                data = d, 
                family = cumulative("probit"),
                iter  = 3000, chains = 3, 
                warmup = 1000, 
                seed  = 123, 
                prior = priors_mMH.full,
                sample_prior = TRUE, 
                init_r = 0.05,
                control = list(adapt_delta = 0.99, max_treedepth = 15) # change to avoid divergent transitions as the amount of numerical error was below the median over the iterations (assessed using pairs(); https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup) 
)
summary(mMH.full)

##5.stress####
mSS.full <- brm(Q2 ~ O1 + D1 + H2 + D5 + D67 + D4 + D11 + D8 + H4 + O8b + P1 + S + (1|region), 
                data = d, 
                family = cumulative("probit"),
                iter  = 3000, chains = 3, 
                warmup = 1000, 
                seed  = 123, 
                prior = priors_mSS.full,
                sample_prior = TRUE, 
                init_r = 0.05,
                control = list(adapt_delta = 0.99, max_treedepth = 15) # change to avoid divergent transitions as the amount of numerical error was below the median over the iterations (assessed using pairs(); https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup) 
)
summary(mSS.full)

##6.anxiety####
mAx.full <- brm(Q1_cat ~ O1 + D1 + H2 + D5 + D67 + D4 + D11 + D8 + H4 + O8b + P1 + S + (1|region), 
                data = d, 
                family = cumulative("probit"),
                iter  = 3000, chains = 3, 
                warmup = 1000, 
                seed  = 123, 
                prior = priors_mAx.full,
                sample_prior = TRUE, 
                init_r = 0.05,
                control = list(adapt_delta = 0.99, max_treedepth = 15) # change to avoid divergent transitions as the amount of numerical error was below the median over the iterations (assessed using pairs(); https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup) 
)
summary(mAx.full)

#E.diagnostics####
##1.QOL####
plot(mQOL.full)
###Gelman
modelposterior <- as.mcmc(mQOL.full) 
gelman.diag(modelposterior)
##residuals
resid = resid(mQOL.full)[, "Estimate"]
fit = fitted(mQOL.full)[, "Estimate"]
ggplot() + geom_point(data = NULL, aes(y = resid, x = fit))
##residual vs predictor
ggplot() + geom_point(data = NULL, aes(y = resid, x = d$O1))
##studentized residuals
sresid = resid/sd(resid)
ggplot() + geom_point(data = NULL, aes(y = sresid, x = fit))

##2.overall health####
plot(mGH.full)
###Gelman
modelposterior <- as.mcmc(mGH.full) 
gelman.diag(modelposterior)
##residuals
resid = resid(mGH.full)[, "Estimate"]
fit = fitted(mGH.full)[, "Estimate"]
ggplot() + geom_point(data = NULL, aes(y = resid, x = fit))
##residual vs predictor
ggplot() + geom_point(data = NULL, aes(y = resid, x = d$O1))
##studentized residuals
sresid = resid/sd(resid)
ggplot() + geom_point(data = NULL, aes(y = sresid, x = fit))                   
                   
##3.loneliness####
plot(mL.full)
###Gelman
modelposterior <- as.mcmc(mL.full) 
gelman.diag(modelposterior)
##residuals
resid = resid(mL.full)[, "Estimate"]
fit = fitted(mL.full)[, "Estimate"]
ggplot() + geom_point(data = NULL, aes(y = resid, x = fit))
##residual vs predictor
ggplot() + geom_point(data = NULL, aes(y = resid, x = d$O1))
##studentized residuals
sresid = resid/sd(resid)
ggplot() + geom_point(data = NULL, aes(y = sresid, x = fit))                   

##4.mental health####
plot(mMH.full)
#Gelman
modelposterior <- as.mcmc(mMH.full) 
gelman.diag(modelposterior[,0:38])
#category-specific effects
mMH.cs <- brm(H3 ~ cs(O1) + (1|region), 
              data = d, 
              family = acat("probit"),
              iter  = 3000, chains = 3, 
              warmup = 1000, 
              seed  = 123, 
              prior = priors_mMH.0,
              control = list(adapt_delta = 0.90, max_treedepth = 15) 
)
# unequal variance
mMH.v <- brm(bf(H3 ~ O1 + (1|region))+ 
               lf(disc ~ 0 + O1 + (1|region), cmc = FALSE), 
             data = d, 
             family = cumulative("probit"),
             iter  = 3000, chains = 3, 
             warmup = 1000, 
             seed  = 123, 
             prior = priors_mMH.0,
             control = list(adapt_delta = 0.90, max_treedepth = 15) 
)
#comparison
loo(mMH.0, mMH.cs, mMH.v) #elpd_diff was not different, m.0 was kept

##5.stress####
plot(mSS.full)
#Gelman
modelposterior <- as.mcmc(mSS.full) 
gelman.diag(modelposterior[,0:38])
#category-specific effects
mSS.cs <- brm(Q2 ~ cs(O1) + (1|region), 
              data = d, 
              family = acat("probit"),
              iter  = 3000, chains = 3, 
              warmup = 1000, 
              seed  = 123, 
              prior = priors_mSS.0,
              control = list(adapt_delta = 0.90, max_treedepth = 15) 
)
# unequal variance
mSS.v <- brm(bf(Q2 ~ O1 + (1|region))+ 
               lf(disc ~ 0 + O1 + (1|region), cmc = FALSE), 
             data = d, 
             family = cumulative("probit"),
             iter  = 3000, chains = 3, 
             warmup = 1000, 
             seed  = 123, 
             prior = priors_mSS.0,
             control = list(adapt_delta = 0.90, max_treedepth = 15) 
)
#comparison
loo(mSS.0, mSS.cs, mSS.v) #elpd_diff was not different, m.0 was kept

##6.anxiety####
plot(mAx.full)
#Gelman
modelposterior <- as.mcmc(mAx.full) 
gelman.diag(modelposterior[,0:38])
#category-specific effects
mAx.cs <- brm(Q1_cat ~ cs(O1) + (1|region), 
              data = d, 
              family = acat("probit"),
              iter  = 3000, chains = 3, 
              warmup = 1000, 
              seed  = 123, 
              prior = priors_mAx.0,
              control = list(adapt_delta = 0.90, max_treedepth = 15) 
)
# unequal variance
mAx.v <- brm(bf(Q1_cat ~ O1 + (1|region))+ 
               lf(disc ~ 0 + O1 + (1|region), cmc = FALSE), 
             data = d, 
             family = cumulative("probit"),
             iter  = 3000, chains = 3, 
             warmup = 1000, 
             seed  = 123, 
             prior = priors_mAx.0,
             control = list(adapt_delta = 0.90, max_treedepth = 15) 
)
#comparison
loo(mAx.0, mAx.cs, mAx.v) #elpd_diff was not different, m.0 was kept

#F.plots####
##4.mental health####
p1 <- plot(conditional_effects(mMH.0,"O1", categorical = TRUE), ask = FALSE)
p1.1 <- p1[["O1:cats__"]] +
  scale_fill_brewer(palette = "Set2", 
                    name = "Mental health", 
                    labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))+
  scale_color_brewer(palette = "Set2", 
                     name = "Mental health", 
                     labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))+
  theme_light(base_family = "Times") +
  xlab("Pet owners") +
  ylim(0,0.5) +
  #guides(fill=guide_legend(nrow=3))+
  theme(legend.margin=margin()) 

jpeg("MH_0.jpeg", width = 10, height = 5, units = 'cm',res = 300)
p1.1
dev.off()

p2 <- plot(conditional_effects(mMH.full,"O1", 
                               categorical = TRUE, 
                               re_formula =  NULL,
                               method = "posterior_epred"), ask = FALSE)


p2.1 <- p2[["O1:cats__"]] +
  scale_fill_brewer(palette = "Set2", 
                    name = "Mental health", 
                    labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))+
  scale_color_brewer(palette = "Set2", 
                     name = "Mental health", 
                     labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))+
  theme_light(base_family = "Times") +
  xlab("Pet owners") +
  ylim(0,0.5) +
  #guides(fill=guide_legend(nrow=3))+
  theme(legend.margin=margin()) 

jpeg("MH_full.jpeg", width = 10, height = 5, units = 'cm',res = 300)
p2.1
dev.off()
##5.stress####
p3 <- plot(conditional_effects(mSS.0,"O1", categorical = TRUE), ask = FALSE)
p3.1 <- p3[["O1:cats__"]] +
  scale_fill_brewer(palette = "Set2", 
                    name = "Stress", 
                    labels = c("Not at all", "Net very", "A bit", "Quite a bit", "Extremely"))+
  scale_color_brewer(palette = "Set2", 
                     name = "Stress", 
                     labels = c("Not at all", "Net very", "A bit", "Quite a bit", "Extremely"))+
  theme_light(base_family = "Times") +
  xlab("Pet owners") +
  ylim(0,0.5) +
  #guides(fill=guide_legend(nrow=3))+
  theme(legend.margin=margin()) 

jpeg("SS_0.jpeg", width = 10, height = 5, units = 'cm',res = 300)
p3.1
dev.off()

p4 <- plot(conditional_effects(mSS.full,"O1", categorical = TRUE), ask = FALSE)
p4.1 <- p4[["O1:cats__"]] +
  scale_fill_brewer(palette = "Set2", 
                    name = "Stress", 
                    labels = c("Not at all", "Net very", "A bit", "Quite a bit", "Extremely"))+
  scale_color_brewer(palette = "Set2", 
                     name = "Stress", 
                     labels = c("Not at all", "Net very", "A bit", "Quite a bit", "Extremely"))+
  theme_light(base_family = "Times") +
  xlab("Pet owners") +
  ylim(0,0.5) +
  #guides(fill=guide_legend(nrow=3))+
  theme(legend.margin=margin()) 

jpeg("SS_full.jpeg", width = 10, height = 5, units = 'cm',res = 300)
p4.1
dev.off()


##6.anxiety####
p5 <- plot(conditional_effects(mAx.0,"O1", categorical = TRUE), ask = FALSE)
p5.1 <- p5[["O1:cats__"]] +
  scale_fill_brewer(palette = "Set2", 
                    name = "Anxiety", 
                    labels = c("Minimal", "Mild", "Moderate", "Severe"))+
  scale_color_brewer(palette = "Set2", 
                     name = "Anxiety", 
                     labels = c("Minimal", "Mild", "Moderate", "Severe"))+
  theme_light(base_family = "Times") +
  xlab("Pet owners") +
  ylim(0,0.85) +
  #guides(fill=guide_legend(nrow=2))+
  theme(legend.margin=margin()) 

jpeg("Ax_0.jpeg", width = 10, height = 5, units = 'cm',res = 300)
p5.1
dev.off()

p6 <- plot(conditional_effects(mAx.full,"O1", categorical = TRUE), ask = FALSE)
p6.1 <- p6[["O1:cats__"]] +
  scale_fill_brewer(palette = "Set2", 
                    name = "Anxiety", 
                    labels = c("Minimal", "Mild", "Moderate", "Severe"))+
  scale_color_brewer(palette = "Set2", 
                     name = "Anxiety", 
                     labels = c("Minimal", "Mild", "Moderate", "Severe"))+
  theme_light(base_family = "Times") +
  xlab("Pet owners") +
  ylim(0,0.85) +
  #guides(fill=guide_legend(nrow=2))+
  theme(legend.margin=margin()) 

jpeg("Ax_full.jpeg", width = 10, height = 5, units = 'cm',res = 300)
p6.1
dev.off()