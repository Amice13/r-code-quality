setwd("/Users/ruixingcao/Library/Documents/rebel support")

library(tidyverse)
library(forcats)
library(dplyr)
library(foreign)
library(plotly)
library(survival)
library(flexsurv)  
library(aod)       
library(haven)
library(magrittr)
library(survminer)
library(lmtest)
library(sandwich)
library(stargazer)
library(MASS)
library(countrycode)
library(plm)
library(knitr)
library(pscl)
library(nnet)
library(mediation)

## read data

dyad4 = read_csv("exsupport_general_sept24.csv")

## Table 1 in the main text

model1.111 = multinom(support2~gwf_personal1
                      +tar_gdppc
                      +rival_0+num_reb+pcw
                      +rebstrength
                      +tran_kin+EFindex+defense
                      +oilrentpc
                      +spell_time+support_lag
                      +factor(region1)+factor(decade),
                      data=dyad4,Hess=TRUE)

model1.211 = multinom(support2~latent_personalism
                      +tar_gdppc
                      +rival_0+num_reb+pcw
                      +rebstrength
                      +tran_kin+EFindex+defense
                      +oilrentpc
                      +spell_time+support_lag
                      +factor(region1)+factor(decade),
                      data=dyad4,Hess=TRUE)
summary(model1.111)

stargazer(model1.111, model1.211, covariate.labels = 
            c("Personalism", "Personalism", "GDP percapita", "Rivalry",
              "Number of rebel groups", "Post-Cold War", "Rebel strength", "Transborder kin", "Ethnic fractionalization",
              "Defense pact", "Oil rent per capita", "Rebel group duration", "Support last year"))

## extract number of obeservations

nrow(residuals(model1.111))
nrow(residuals(model1.211))

## Figure 1 in the main text 

pervalue = c(0,1)

B <- c(t(coef(model1.111)))
V <- vcov(model1.111)
simcoefs <- mvrnorm(1000, mu=B, Sigma=V)

simwts <- apply(simcoefs, 1, FUN=function(x){
  C <- t(matrix(x, ncol(coef(model1.111)), nrow(coef(model1.111))))
  C <- rbind(0, C)
  C <- cbind(0, C)
  C <- c(t(C))
  return(C)
})
simwts <- t(simwts)

model1.1112 <- model1.111
qi.se <- apply(simwts, 1, FUN=function(s){
  model1.1112$wts <- s
  qi <- sapply(pervalue, FUN=function(x){
    colMeans(predict(model1.1112, type="probs", newdata = mutate(dyad4, gwf_personal1=x)), na.rm=TRUE)
  })
  qi <- data.frame(gwf_personal1=pervalue, t(qi))
  qi <- gather(qi, `X0`:`X2`, key="Rebel_Sponsorship", value="prob")
  return(qi)
})


qi.se <- plyr::rbind.fill(qi.se)

qi.se <- group_by(qi.se, gwf_personal1, Rebel_Sponsorship)
qi <- summarize(qi.se, mean = mean(prob, na.rm=TRUE),
                se = sd(prob, na.rm=TRUE),
                lb = quantile(prob, .025, na.rm=TRUE),
                ub = quantile(prob, .975, na.rm=TRUE) 
)



qi <- mutate(qi, Rebel_Sponsorship = factor(Rebel_Sponsorship,
                                            levels = c("X0", "X1", "X2"),
                                            labels = c("No Sponsorship", "Other Support",
                                                       "Troop Support")))

g <- ggplot(qi, aes(x=factor(gwf_personal1), y=mean)) + 
  geom_point() +
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.2,
                position=position_dodge(0.05)) +
  xlab("Personalism") +
  ylab("Predicted Probability") +
  facet_wrap(~ Rebel_Sponsorship)
g

## Figure 2 in the main text

pervalue = quantile(dyad4$latent_personalism, probs = c(0.10, 0.25, 0.5,0.75, 0.90), na.rm=TRUE)
B <- c(t(coef(model1.211)))
V <- vcov(model1.211)
simcoefs <- mvrnorm(1000, mu=B, Sigma=V)

simwts <- apply(simcoefs, 1, FUN=function(x){
  C <- t(matrix(x, ncol(coef(model1.211)), nrow(coef(model1.211))))
  C <- rbind(0, C)
  C <- cbind(0, C)
  C <- c(t(C))
  return(C)
})
simwts <- t(simwts)

model1.22 <- model1.211
qi.se <- apply(simwts, 1, FUN=function(s){
  model1.22$wts <- s
  qi <- sapply(pervalue, FUN=function(x){
    colMeans(predict(model1.22, type="probs", newdata = mutate(dyad4, latent_personalism=x)), na.rm=TRUE)
  })
  qi <- data.frame(latent_personalism=pervalue, t(qi))
  qi <- gather(qi, `X0`:`X2`, key="Rebel_Sponsorship", value="prob")
  return(qi)
})


qi.se <- plyr::rbind.fill(qi.se)

qi.se <- group_by(qi.se, latent_personalism, Rebel_Sponsorship)
qi <- summarize(qi.se, mean = mean(prob, na.rm=TRUE),
                se = sd(prob, na.rm=TRUE),
                lb = quantile(prob, .025, na.rm=TRUE),
                ub = quantile(prob, .975, na.rm=TRUE) 
)



qi <- mutate(qi, Rebel_Sponsorship = factor(Rebel_Sponsorship,
                                            levels = c("X0", "X1", "X2"),
                                            labels = c("No Sponsorship", "Other Support",
                                                       "Troop Support")))

g <- ggplot(qi, aes(x=latent_personalism, y=mean)) + 
  geom_line() +
  geom_ribbon(aes(ymin=lb, ymax=ub), fill="blue", alpha=.5) +
  xlab("Personalism") +
  ylab("Predicted Probability") +
  facet_wrap(~ Rebel_Sponsorship, scales="free")
g

## Table 2 in the main text

model1.311 = glm(support1~latent_personalism
                 +factor(region1)+factor(decade),
                 data=dyad4,family=binomial(link=logit))
result1.311 = coeftest(model1.311, vcov. = vcovCL(model1.311, cluster = ~dyad_id, type = "HC0"))

model1.11 = glm(support1~latent_personalism+tar_gdppc
                +rival_0+num_reb+pcw
                +rebstrength
                +tran_kin+EFindex+defense
                +oilrentpc
                +spell_time+support_lag
                +factor(region1)+factor(decade),
                data=dyad4, family=binomial(link=logit))

result1.11 = coeftest(model1.11, vcov. = vcovCL(model1.11, cluster = ~dyad_id, type = "HC0"))


model1.32 = glm(support1~gwf_personal1
                +factor(region1)+factor(decade),
                data=dyad4,family=binomial(link=logit))
result1.32=coeftest(model1.32, vcov. = vcovCL(model1.32, cluster = ~dyad_id, type = "HC0"))

model1.31 = glm(support1~gwf_personal1
                +tar_gdppc
                +rival_0+num_reb+pcw
                +rebstrength
                +tran_kin+EFindex+defense
                +oilrentpc
                +spell_time+support_lag
                +factor(region1)+factor(decade),
                data=dyad4,family=binomial(link=logit))

result1.31 = coeftest(model1.31, vcov. = vcovCL(model1.31, cluster = ~dyad_id, type = "HC0"))


stargazer(result1.311, result1.11, result1.32, result1.31, 
          covariate.labels = c("Personalism", "GDP per capita", "Rivalry",
                               "Number of rebel groups", "Post-Cold War", "Rebel strength", "Transborder kin", "Ethnic fractionalization",
                               "Defense pact", "Oil rent per capita", "Rebel group duration", "Support last year", "Personalism"))
stargazer(model1.311, model1.11, model1.32, model1.31)

## Table A1 and Table A2 in the online appendix

exsupport1 = read_csv("exsupport_mediation_jan24.csv")

## authoritarian sample
set.seed(2220)
outcome1 = glm(S_Troop~gwf_personal+hacb_75
               +tar_gdppc
               +num_reb+pcw
               +rebstrength
               +EFindex+defense
               +resource_rent
               +factor(region1)+factor(decade),
               data=exsupport1,family=binomial(link=logit))
summary(outcome1)
mediator1 = glm(hacb_75~gwf_personal
                +tar_gdppc
                +num_reb+pcw
                +rebstrength
                +EFindex+defense
                +resource_rent
                +factor(region1)+factor(decade), 
                data=exsupport1,family=binomial(link=logit))
summary(mediator1)
mediation1 <- mediate(mediator1, outcome1, sims=1000, 
                      treat="gwf_personal", mediator="hacb_75")
summary(mediation1)

## Including democracies
set.seed(2220)
outcome2 = glm(S_Troop~gwf_personal1+hacb_75
               +tar_gdppc
               +num_reb+pcw
               +rebstrength
               +EFindex+defense
               +resource_rent
               +factor(region1)+factor(decade),
               data=exsupport1,family=binomial(link=logit))
summary(outcome2)
mediator2 = glm(hacb_75~gwf_personal1
                +tar_gdppc
                +num_reb+pcw
                +rebstrength
                +EFindex+defense
                +resource_rent
                +factor(region1)+factor(decade), 
                data=exsupport1,family=binomial(link=logit))
summary(mediator2)
mediation2 <- mediate(mediator2, outcome2, sims=1000, 
                      treat="gwf_personal1", mediator="hacb_75")
summary(mediation2)

stargazer(mediator1, outcome1, mediator2, outcome2)

## Table A3 in the online appendix

dyad3 = subset(dyad4, support1==1)

model1.33 = glm(S_Troop~latent_personalism
                +factor(region1)+factor(decade),
                data=dyad3,family=binomial(link=logit))
result1.33=coeftest(model1.33, vcov. = vcovCL(model1.33, cluster = ~dyad_id, type = "HC0"))

model1.41 = glm(S_Troop~latent_personalism+tar_gdppc
                +rival_0+spon_xpolity
                +num_reb+pcw
                +rebstrength
                +tran_kin+EFindex+defense
                +oilrentpc
                +spell_time+troop_lag
                +factor(region1)+factor(decade),
                data=dyad3,family=binomial(link=logit))
result1.41 = coeftest(model1.41, vcov. = vcovCL(model1.41, cluster = ~dyad_id, type = "HC0"))



model1.34 = glm(S_Troop~gwf_personal1
                +factor(region1)+factor(decade),
                data=dyad3,family=binomial(link=logit))
result1.34=coeftest(model1.34, vcov. = vcovCL(model1.34, cluster = ~dyad_id, type = "HC0"))

model1.61 = glm(S_Troop~gwf_personal1+tar_gdppc
                +rival_0+spon_xpolity
                +num_reb+pcw
                +rebstrength
                +tran_kin+EFindex+defense
                +oilrentpc
                +spell_time+troop_lag
                +factor(region1)+factor(decade),
                data=dyad3,family=binomial(link=logit))
result1.61 = coeftest(model1.61, vcov. = vcovCL(model1.61, cluster = ~dyad_id, type = "HC0"))

## ordered logit
dyad3$support_del1 = as.factor(dyad3$support_del1)
dyad3$support_del1 = as.ordered(dyad3$support_del1)
model1.7 = polr(support_del1~latent_personalism+tar_gdppc
                +rival_0+spon_xpolity
                +num_reb+pcw
                +rebstrength
                +tran_kin+EFindex+defense
                +oilrentpc
                +spell_time+support_lag,
                data=dyad3,Hess=TRUE)
result1.7 = coeftest(model1.7, vcov. = vcovCL(model1.7, cluster = ~dyad_id, type = "HC0"))
summary(model1.7)


stargazer(result1.33, result1.41, result1.34, result1.61, result1.7,
          covariate.labels = c("Personalism", "GDP per capita", "Rivalry", "Sponsor Xpolity",
                               "Number of rebel groups", "Post-Cold War", "Rebel strength", "Transborder kin", "Ethnic fractionalization",
                               "Defense pact", "Oil rent per capita", "Rebel group duration", "Troop support last year", "Personalism"))

stargazer(model1.33, model1.41, model1.34, model1.61, model1.7)

## Figure A1 in the appendix

model.data <- augment(model1.41) %>% 
  mutate(index = 1:n()) 

model.data = rename(model.data, region1 = 'factor(region1)', decade='factor(decade)')

model1.4 = glm(S_Troop~latent_personalism+tar_gdppc
               +rival_0+spon_xpolity
               +num_reb+pcw
               +rebstrength
               +EFindex+defense
               +oilrentpc
               +spell_time+troop_lag
               +region1+decade,
               data=model.data,family=binomial(link=logit))

pervalue = quantile(dyad3$latent_personalism, probs = c(0.10, 0.25, 0.5,0.75, 0.90), na.rm=TRUE)
set.seed(2220)

B <- coef(model1.4)
V <- vcov(model1.4)
sim.coefs <- mvrnorm(1000, mu=B, Sigma=V)

model.sim <- model1.4
sim.qi1 <- apply(sim.coefs, 1, FUN=function(x){
  model.sim$coefficients <- x
  logit.prob <- sapply(pervalue, FUN=function(y){
    mean(predict(model.sim, type = "response", 
                 newdata = mutate(model.data, latent_personalism=y)), na.rm=TRUE)
  })
})

sim.qi1 <- t(sim.qi1)

Predicted.prod1 <- apply(sim.qi1, 2, mean)
Predicted.prod1 <- data.frame(Predicted.prod1)
model.per.prob.se1 <- apply(sim.qi1, 2, sd)
model.per.prob.lb1 <- apply(sim.qi1, 2, FUN=function(x){
  quantile(x, .025)
})
model.per.prob.ub1 <- apply(sim.qi1, 2, FUN=function(x){
  quantile(x, .975)
})

output1 <- mutate(Predicted.prod1, 
                  model.per.prob.se1 = model.per.prob.se1,
                  model.per.prob.lb1 = model.per.prob.lb1,
                  model.per.prob.ub1 = model.per.prob.ub1)
output1 = cbind(output1, pervalue)

output1["quantile"] = c(0.1, 0.25, 0.5, 0.75, 0.9)

g4 <- ggplot(output1, aes(x=factor(quantile), y=Predicted.prod1, group=1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=model.per.prob.lb1, ymax=model.per.prob.ub1), width=.2,
                position=position_dodge(0.05)) +
  geom_hline(yintercept = 0, lty=2) +
  xlab("Personalism") +
  ylab("Predicted Probabilities")
g4

sim.qi1 = data.frame(sim.qi1)
t.test(sim.qi1$X10., sim.qi1$X90.)
t.test(sim.qi1$X10., sim.qi1$X25.)
t.test(sim.qi1$X10., sim.qi1$X50.)
t.test(sim.qi1$X10., sim.qi1$X75.)
t.test(sim.qi1$X25., sim.qi1$X50.)
t.test(sim.qi1$X25., sim.qi1$X75.)
t.test(sim.qi1$X25., sim.qi1$X90.)
t.test(sim.qi1$X50., sim.qi1$X75.)
t.test(sim.qi1$X50., sim.qi1$X90.)
t.test(sim.qi1$X75., sim.qi1$X90.)

## Figure A2 in the online appendix

model.data <- augment(model1.61) %>% 
  mutate(index = 1:n()) 

model.data = rename(model.data, region1 = 'factor(region1)', decade='factor(decade)')

model1.6 = glm(S_Troop~gwf_personal1+tar_gdppc
               +rival_0+spon_xpolity
               +num_reb+pcw
               +rebstrength
               +EFindex+defense
               +oilrentpc
               +troop_lag+spell_time
               +region1+decade,
               data=model.data,family=binomial(link=logit))

pervalue = c(0,1)
set.seed(2220)

B <- coef(model1.6)
V <- vcov(model1.6)
sim.coefs <- mvrnorm(1000, mu=B, Sigma=V)

model.sim <- model1.6
sim.qi1 <- apply(sim.coefs, 1, FUN=function(x){
  model.sim$coefficients <- x
  logit.prob <- sapply(pervalue, FUN=function(y){
    mean(predict(model.sim, type = "response", 
                 newdata = mutate(model.data, gwf_personal1=y)), na.rm=TRUE)
  })
})

sim.qi1 <- t(sim.qi1)

Predicted.prod1 <- apply(sim.qi1, 2, mean)
Predicted.prod1 <- data.frame(Predicted.prod1)
model.per.prob.se1 <- apply(sim.qi1, 2, sd)
model.per.prob.lb1 <- apply(sim.qi1, 2, FUN=function(x){
  quantile(x, .025)
})
model.per.prob.ub1 <- apply(sim.qi1, 2, FUN=function(x){
  quantile(x, .975)
})

output1 <- mutate(Predicted.prod1, 
                  model.per.prob.se1 = model.per.prob.se1,
                  model.per.prob.lb1 = model.per.prob.lb1,
                  model.per.prob.ub1 = model.per.prob.ub1)
output1 = cbind(output1, pervalue)

output1["quantile"] = c("Non-personalist", "Personalist")

g4 <- ggplot(output1, aes(x=factor(quantile), y=Predicted.prod1, group=1)) +
  
  geom_point() +
  geom_errorbar(aes(ymin=model.per.prob.lb1, ymax=model.per.prob.ub1), width=.2,
                position=position_dodge(0.05)) +
  geom_hline(yintercept = 0, lty=2) +
  xlab("Personalism") +
  ylab("Predicted Probabilities")
g4

sim.qi1 = data.frame(sim.qi1)
t.test(sim.qi1$X1, sim.qi1$X2)

## Figure A3 in the online appendix

model.data <- augment(model1.7) %>% 
  mutate(index = 1:n()) 

model1.7 = polr(support_del1~latent_personalism+tar_gdppc
                +rival_0+spon_xpolity
                +num_reb+pcw
                +rebstrength
                +EFindex+defense
                +oilrentpc
                +spell_time+support_lag,
                data=model.data,Hess=TRUE)


B <- c(coef(model1.7), model1.7$zeta)
V <- vcov(model1.7)
simcoefs <- mvrnorm(1000, mu=B, Sigma=V)

model31 <- model1.7
pervalue = quantile(dyad3$latent_personalism, probs = c(0.10, 0.25, 0.5,0.75, 0.90), na.rm=TRUE)

qi.df.se <- apply(simcoefs, 1, FUN=function(c){
  newcoef <- c[1:12]
  newcutpoints <- c[13:14]
  model31$coefficients <- newcoef
  model31$zeta <- newcutpoints
  
  probmat <- sapply(pervalue, FUN=function(x){
    colMeans(predict(model31, type="probs",
                     newdata=mutate(model.data, latent_personalism=x)), na.rm=TRUE)
  }) 
  qi.df <- data.frame(t(probmat)) 
  names(qi.df) <- c("p1", "p2", "p3") 
  qi.df <- mutate(qi.df, personalism=pervalue)
  qi.df <- mutate(qi.df, 
                  personalism = pervalue)
  
  
  qi.df <- dplyr::select(qi.df, personalism, everything())
  qi.df <- bind_rows(qi.df)
  
  qi.df <- gather(qi.df, p1:p3, key="QI", value="point.estimate")
  return(qi.df)
})
qi.df.se <- plyr::rbind.fill(qi.df.se)
qi.df.se <- group_by(qi.df.se, personalism, QI)
qi.df.se1 <- dplyr::summarize(qi.df.se, 
                              prod = mean(point.estimate),
                              se = sd(point.estimate),
                              lb = quantile(point.estimate, .025),
                              ub = quantile(point.estimate, .975))
qi.df.se1 <- mutate(qi.df.se1,
                    QI = factor(QI, levels=c("p1", "p2", "p3"), 
                                labels=c("Indirect Support", 
                                         "Training/Safe Haven", 
                                         "Troop" 
                                )))
qi.df.se1["quantile"] = c(0.1, 0.1, 0.1, 0.25, 0.25,0.25,0.5,0.5, 0.5,
                          0.75,0.75,0.75, 0.9,0.9,0.9)
qi.df.se1$quantile = as.factor(qi.df.se1$quantile)

g <- ggplot(qi.df.se1, aes(x=QI, y=prod, color=quantile, shape=quantile)) +
  geom_point(size=4, position=position_dodge(width=0.3)) +
  geom_linerange(aes(ymin=lb, ymax=ub), position=position_dodge(width=0.3)) +
  xlab("") +
  ylab("Predicted probability")
g

## Table A4 in the online appendix

dyad5 = subset(dyad4, pcw==1)
dyad6 = subset(dyad4, pcw==0)



model1.1111 = multinom(support2~gwf_personal1
                       +tar_gdppc
                       +rival_0+num_reb
                       +rebstrength
                       +tran_kin+EFindex+defense
                       +oilrentpc
                       +spell_time+support_lag
                       +factor(region1)+factor(decade),
                       data=dyad5,Hess=TRUE)

model1.2111 = multinom(support2~gwf_personal1
                       +tar_gdppc
                       +rival_0+num_reb
                       +rebstrength
                       +tran_kin+EFindex+defense
                       +oilrentpc
                       +spell_time+support_lag
                       +factor(region1)+factor(decade),
                       data=dyad6,Hess=TRUE)
nrow(residuals(model1.1111))
nrow(residuals(model1.2111))

stargazer(model1.1111, model1.2111, covariate.labels = 
            c("Personalism", "GDP percapita", "Rivalry",
              "Number of rebel groups", "Rebel strength", "Transborder kin", "Ethnic fractionalization",
              "Defense pact", "Oil rent per capita", "Rebel group duration", "Support last year"))

## Table A5 in the online appendix

dyad4$latent_personalism1 = dyad4$latent_personalism
dyad4$latent_personalism1[dyad4$xpolity==-66] =NA
dyad4$latent_personalism1[dyad4$xpolity==-77] =NA

dyad4$gwf_personal2 = dyad4$gwf_personal1
dyad4$gwf_personal2[dyad4$xpolity==-66] =NA
dyad4$gwf_personal2[dyad4$xpolity==-77] =NA

model1.5 = multinom(support2~gwf_personal2
                    +tar_gdppc
                    +rival_0+num_reb+pcw
                    +rebstrength
                    +tran_kin+EFindex+defense
                    +oilrentpc
                    +spell_time+support_lag
                    +factor(region1)+factor(decade),
                    data=dyad4,Hess=TRUE)

model1.51 = multinom(support2~latent_personalism1
                     +tar_gdppc
                     +rival_0+num_reb+pcw
                     +rebstrength
                     +tran_kin+EFindex+defense
                     +oilrentpc
                     +spell_time+support_lag
                     +factor(region1)+factor(decade),
                     data=dyad4,Hess=TRUE)

nrow(residuals(model1.5))
nrow(residuals(model1.51))

stargazer(model1.5, model1.51,
          covariate.labels = 
            c("Personalism", "Personalism", "GDP percapita", "Rivalry",
              "Number of rebel groups", "Post-Cold War", "Rebel strength", "Transborder kin", "Ethnic fractionalization",
              "Defense pact", "Oil rent per capita", "Rebel group duration", "Support last year"))

## Table A6 in the online appendix

exsup7 = read_csv("ucdp_exsup_sept24.csv")

model1 = glm(ext_x~latent_personalism,
             data=exsup7,family=binomial(link=logit))
result1 = coeftest(model1, vcov. = vcovCL(model1, cluster = ~dyad_id, type = "HC0"))


model1.1 = glm(ext_x~latent_personalism
               +tar_gdppc+rival_0_25
               +spon_xpolity
               +num_reb+pcw
               +rebstrength
               +EFindex+defense
               +oilrentpc+extx_lag,
               data=exsup7,family=binomial(link=logit))
result1.1 = coeftest(model1.1, vcov. = vcovCL(model1.1, cluster = ~dyad_id, type = "HC0"))

model1.11 = glm(ext_x~gwf_personal1
                ,
                data=exsup7,family=binomial(link=logit))
result1.11 = coeftest(model1.11, vcov. = vcovCL(model1.11, cluster = ~dyad_id, type = "HC0"))

model1.111 = glm(ext_x~gwf_personal1
                 +tar_gdppc+rival_0_25
                 +spon_xpolity
                 +num_reb+pcw
                 +rebstrength
                 +EFindex+defense
                 +oilrentpc+extx_lag,
                 data=exsup7,family=binomial(link=logit))
result1.111 = coeftest(model1.111, vcov. = vcovCL(model1.111, cluster = ~dyad_id, type = "HC0"))

stargazer(result1, result1.1, result1.11, result1.111,
          covariate.labels = 
            c("Personalism",  "GDP per capita", "Rivalry", "Sponsor Xpolity",
              "Number of rebel groups", "Post-Cold War", "Rebel strength", "Ethnic fractionalization",
              "Defense pact", "Oil rent per capita", "Troop support last year", "Personalism"))
stargazer(model1, model1.1, model1.11, model1.111)

## Figure A4 in the online appendix
LeaveOneResults <- data.frame('Target' = NA, "Est" = NA, "Se" = NA)
for(i in 1:length(unique(dyad4$TarNum_COW))){
  LeaveOneResults[i,1] <- unique(dyad4$TarNum_COW)[i]
  LeaveOne <- dyad4[dyad4$TarNum_COW != unique(dyad4$TarNum_COW)[i],]
  LeaveOneMod <- multinom(support2~gwf_personal1
                          +tar_gdppc
                          +rival_0+num_reb+pcw
                          +rebstrength
                          +tran_kin+EFindex+defense
                          +oilrentpc
                          +spell_time+support_lag
                          +factor(region1)+factor(decade),
                          data=LeaveOne,Hess=TRUE)
  LeaveOneResults[i,2] <- summary(LeaveOneMod)$coefficients[2,2] 
  LeaveOneResults[i,3] <- summary(LeaveOneMod)$standard.errors[2,2]
}
LeaveOneResults$Lb <- LeaveOneResults$Est-1.96*LeaveOneResults$Se
LeaveOneResults$Ub <- LeaveOneResults$Est+1.96*LeaveOneResults$Se
LeaveOneResults <- with(LeaveOneResults,  LeaveOneResults[order(Target), ])
LeaveOneResults1 <- LeaveOneResults[seq(1, nrow(LeaveOneResults), 2),]
LeaveOneResults2 <- LeaveOneResults[seq(2, nrow(LeaveOneResults), 2),]
LeaveOneResults$Target = factor(LeaveOneResults$Target)
g4 <- ggplot(LeaveOneResults, aes(x=Target, y=Est)) +
  geom_point(aes(fct_reorder(Target, Est))) +
  geom_linerange(aes(ymin = Lb, ymax = Ub)) +
  geom_hline(aes(yintercept = 0,colour="darkblue" )) +
  scale_color_discrete(name = "baselines", labels = c("0")) +
  coord_flip() +
  xlab("Target") +
  ylab("Coefficients on Combat Support") +
  ggtitle("Leave One Out Results: Target")
g4

## Figure A5 in the appendix
LeaveOneResults <- data.frame('Target' = NA, "Est" = NA, "Se" = NA)
for(i in 1:length(unique(dyad4$TarNum_COW))){
  LeaveOneResults[i,1] <- unique(dyad4$TarNum_COW)[i]
  LeaveOne <- dyad4[dyad4$TarNum_COW != unique(dyad4$TarNum_COW)[i],]
  LeaveOneMod <- multinom(support2~latent_personalism
                          +tar_gdppc
                          +rival_0+num_reb+pcw
                          +rebstrength
                          +tran_kin+EFindex+defense
                          +oilrentpc
                          +spell_time+support_lag
                          +factor(region1)+factor(decade),
                          data=LeaveOne,Hess=TRUE)
  LeaveOneResults[i,2] <- summary(LeaveOneMod)$coefficients[2,2] 
  LeaveOneResults[i,3] <- summary(LeaveOneMod)$standard.errors[2,2]
}
LeaveOneResults$Lb <- LeaveOneResults$Est-1.96*LeaveOneResults$Se
LeaveOneResults$Ub <- LeaveOneResults$Est+1.96*LeaveOneResults$Se
LeaveOneResults <- with(LeaveOneResults,  LeaveOneResults[order(Target), ])
LeaveOneResults1 <- LeaveOneResults[seq(1, nrow(LeaveOneResults), 2),]
LeaveOneResults2 <- LeaveOneResults[seq(2, nrow(LeaveOneResults), 2),]
LeaveOneResults$Target = factor(LeaveOneResults$Target)
g4 <- ggplot(LeaveOneResults, aes(x=Target, y=Est)) +
  geom_point(aes(fct_reorder(Target, Est))) +
  geom_linerange(aes(ymin = Lb, ymax = Ub)) +
  geom_hline(aes(yintercept = 0,colour="darkblue" )) +
  scale_color_discrete(name = "baselines", labels = c("0")) +
  coord_flip() +
  xlab("Target") +
  ylab("Coefficients on Combat Support") +
  ggtitle("Leave One Out Results: Target")
g4


