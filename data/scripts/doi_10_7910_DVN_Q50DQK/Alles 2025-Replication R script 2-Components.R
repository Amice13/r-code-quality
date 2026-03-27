### Who Supports Gender Equality?
### Age, Education, and Sexism in the Shadow of the Radical Right in Argentina

## European Journal of Politics and Gender
## DOI: 10.1332/25151088Y2025D000000104

## Santiago Alles
## Departamento de Ciencias Sociales
## Universidad de San Andrés
## <salles@udesa.edu.ar>


## Replication Data for: 
## Section 2 :: Index's Components


## updated: 2025-08-25


## Loading
## Packages

require(dplyr)
require(ggplot2)



rm(list = ls())





## Article   -----------------------------------------------
## data

# load data
rep_data <- haven::read_dta()                                  ## load: "Alles 2025-EJPG 10.133_225151088Y2025D000000104-Replication data.dta"


# model data
rep_data %>% 
  
  mutate( age_sq = age^2) %>%
  
  # interactions: gender x age
  mutate( fem_age = female * age) %>%
  mutate( fem_age_sq = female * age_sq) %>% 
  
  # interactions: gender x education
  mutate( fem_medLo = female * medLo_educ) %>%
  mutate( fem_medUp = female * medUp_educ) %>%
  mutate( fem_hi = female * hi_educ) %>%
  
  as_tibble() -> mod_data




## Model estimation (2)  -----------------------------------------------
## Idx components

# store
mod_1 <- list()
mod_2 <- list()


# Age models
mod_1[['01']] <- lme4::lmer( gender_att_dem ~ 
                               female + age + age_sq + 
                               medLo_educ + medUp_educ + hi_educ + 
                               pub_employee + unemp + retired + inactive + 
                               fem_age + fem_age_sq + 
                               (1 | sample) + (1 | region), 
                             data = mod_data )

mod_1[['02']] <- lme4::lmer( gender_att_prv ~ 
                               female + age + age_sq + 
                               medLo_educ + medUp_educ + hi_educ + 
                               pub_employee + unemp + retired + inactive + 
                               fem_age + fem_age_sq + 
                               (1 | sample) + (1 | region), 
                             data = mod_data )

mod_1[['03']] <- lme4::lmer( gender_att_men ~ 
                               female + age + age_sq + 
                               medLo_educ + medUp_educ + hi_educ + 
                               pub_employee + unemp + retired + inactive + 
                               fem_age + fem_age_sq + 
                               (1 | sample) + (1 | region), 
                             data = mod_data )


# Education models
mod_2[['01']] <- lme4::lmer( gender_att_dem ~ 
                               female + age + age_sq + 
                               medLo_educ + medUp_educ + hi_educ + 
                               pub_employee + unemp + retired + inactive + 
                               fem_medLo + fem_medUp + fem_hi + 
                               (1 | sample) + (1 | region), 
                             data = mod_data )

mod_2[['02']] <- lme4::lmer( gender_att_prv ~ 
                               female + age + age_sq + 
                               medLo_educ + medUp_educ + hi_educ + 
                               pub_employee + unemp + retired + inactive + 
                               fem_medLo + fem_medUp + fem_hi + 
                               (1 | sample) + (1 | region), 
                             data = mod_data )

mod_2[['03']] <- lme4::lmer( gender_att_men ~ 
                               female + age + age_sq + 
                               medLo_educ + medUp_educ + hi_educ + 
                               pub_employee + unemp + retired + inactive + 
                               fem_medLo + fem_medUp + fem_hi + 
                               (1 | sample) + (1 | region), 
                             data = mod_data )





## Model   -----------------------------------------------
## results


# Age models
stargazer::stargazer( mod_1,
                      type = 'text', 
                      title = "Table C-3: Effect of Age over Modern Sexism Index's Components (linear fixed-effects models, non-weighted data)",
                      no.space = T,
                      digits = 4, 
                      star.cutoffs = c(.05, .01, .001),
                      dep.var.labels = c('Men have more rights', 'Quotas create privileges', 'Parity is more democratic'), 
                      model.numbers = T, 
                      model.names = F, 
                      object.names = F,
                      omit.stat = c("f", "ser")
                      )


# Education models
stargazer::stargazer( mod_2,
                      type = 'text', 
                      title = "Table C-4: Effect of Education over Modern Sexism Index's Components (linear fixed-effects models, non-weighted data)",
                      no.space = T,
                      digits = 4, 
                      star.cutoffs = c(.05, .01, .001),
                      dep.var.labels = c('Men have more rights', 'Quotas create privileges', 'Parity is more democratic'), 
                      model.numbers = T, 
                      model.names = F, 
                      object.names = F,
                      omit.stat = c("f", "ser")
                      )





## Prediction   -----------------------------------------------
## Figure (2)

## Expected value of the Index's Components

# store
plot_store <- list()

# n simulations
n_sims = 50000


## Upper Panel-1
pred_mod <- mod_1[['01']]                                     # prediction model
source("Alles 2025-Replication Figure 2-A.R")


## Upper Panel-2
pred_mod <- mod_1[['03']]                                     # prediction model
source("Alles 2025-Replication Figure 2-A.R")


## Upper Panel-X (only in Appendix)
pred_mod <- mod_1[['02']]                                     # prediction model
source("Alles 2025-Replication Figure 2-A.R")


## Lower Panel-1
pred_mod <- mod_2[['01']]                                     # prediction model
source("Alles 2025-Replication Figure 2-B.R")


## Lower Panel-2
pred_mod <- mod_2[['03']]                                     # prediction model
source("Alles 2025-Replication Figure 2-B.R")


## Lower Panel-X (only in Appendix)
pred_mod <- mod_2[['02']]                                     # prediction model
source("Alles 2025-Replication Figure 2-B.R")




rm( n_sims, pred_mod,
    mod_data)




