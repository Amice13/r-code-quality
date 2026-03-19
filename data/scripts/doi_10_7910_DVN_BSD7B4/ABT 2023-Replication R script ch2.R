## The Representational Consequences
## of Electronic Voting Reform

## Replication Data for: 
## ch 2 :: Reform Adoption

## Santiago Alles
## Tiffany D. Barnes
## Carolina Tchintian

## Script by: Santiago Alles
## updated: 04/06/2023



rm(list = ls())


## Load
## data

rep_data <- haven::read_dta()                        ## load "ABT 2023-Replication Data. Ch2.dta"



## Location  --------------------------
## Models

## Model estimation
mod_1 <- glm( electronic ~ urtubey_votes + complete_HS_or_higher + NBI ,                           # logit :: entire province
              family = binomial(link = "logit"),
              data = rep_data)

mod_2 <- glm( electronic ~ urtubey_votes + complete_HS_or_higher + NBI ,                           # logit :: Capital
              family = binomial(link = "logit"),
              data = rep_data  %>% filter(dept_code == 28))

mod_3 <- glm( electronic ~ urtubey_votes + complete_HS_or_higher + NBI ,                           # logit :: interior
              family = binomial(link = "logit"),
              data = rep_data %>% filter(dept_code != 28) )

mod_4 <- glm( electronic ~ urtubey_votes + complete_HS_or_higher + NBI + urban_housing ,           # logit :: entire province
              family = binomial(link = "logit"),
              data = rep_data)

mod_5 <- glm( electronic ~ urtubey_votes + complete_HS_or_higher + NBI + urban_housing ,           # logit :: Capital
              family = binomial(link = "logit"),
              data = rep_data %>% filter(dept_code == 28) )

mod_6 <- glm( electronic ~ urtubey_votes + complete_HS_or_higher + NBI + urban_housing ,           # logit :: interior
              family = binomial(link = "logit"),
              data = rep_data %>% filter(dept_code != 28) )



## print Model results
stargazer::stargazer( mod_1, mod_2, mod_3, mod_4, mod_5, mod_6, 
                      title = "Table 2-1. Predicting the Assignment of Electronic Devices to an Election Precinct in Salta (2011). Logit Regression models",
                      dep.var.labels = 'Assignment of EV to Precinct',
                      type = "text",
                      no.space = T,
                      digits = 4, star.cutoffs = c(.05, .01, .001),
                      column.labels = rep(c('Province', 'Capital', 'Interior'), 2), 
                      model.numbers = T, model.names = F, object.names = F,
                      omit.stat=c("f", "ser") 
                      )


## Model --------------------------
## Prediction

plot_mod <- mod_1
n_sims <- 50000

## model simulation
mod_sim <- arm::sim(plot_mod, n.sims = n_sims)

## variable to predict: Educational attainment
with( plot_mod$data, quantile(complete_HS_or_higher, probs = seq(.1, .9, .1), na.rm = T))   # entire province

pred_var <- 'complete_HS_or_higher'
plot_IV <- seq(0, 40, length.out = 101)

## control IVs
control_IVs <- all.vars(formula(plot_mod))[-1]
control_IVs <- control_IVs[!control_IVs %in% pred_var]

## prediction frame
X <- data.frame(plot_IV)
colnames(X) <- pred_var

X <- data.frame( 1, data.frame( X , rep_data %>% summarise_at(vars(all_of(control_IVs)), median, na.rm = T) ) %>% 
                   select(all_of(all.vars(formula(plot_mod))[-1])) )

X <- X[!names(X) %in% 'dept_name']

## sim coefficients
b <- as.matrix(mod_sim@coef)
Xb <- t(as.matrix(X)%*% t(b))

## prediction
as_tibble(data.frame( X %>% select(all_of(pred_var)), 
                      apply(exp(Xb) / (1+exp(Xb)), 2, mean),
                      t(apply(exp(Xb) / (1+exp(Xb)), 2, quantile, probs = c(.025, .05, .95, .975))) )
          ) -> mod_pred

colnames(mod_pred) <- c(pred_var, "fit", "ci.lo.95", "ci.lo.90", "ci.up.90", "ci.up.95")


rm( Xb, X,b)

rm( mod_sim, 
    pred_var, plot_IV, control_IVs)

rm( plot_mod, n_sims)



## Prediction --------------------------
## Plot

plot( fit ~ complete_HS_or_higher,
      type = 'l',
      xlim = c(0, 40), ylim = c(0, .8), 
      main = "Predicted probability of piloting electronic voting,\nby level of education\n90% and 95% c.i. (Province of Salta, 2011)",
      sub = "Figure 5 (p.25)", 
      xlab = "Complete high-sch. or higher (%)", 
      ylab = "Predicted probability",
      col = 'red',
      data = mod_pred)
 
lines( ci.lo.95 ~ complete_HS_or_higher, 
       lty = 2,
       data = mod_pred)

lines( ci.up.95 ~ complete_HS_or_higher, 
       lty = 2,
       data = mod_pred)

