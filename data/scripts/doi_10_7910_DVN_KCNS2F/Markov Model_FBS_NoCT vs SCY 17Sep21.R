## CEA ANALYSIS COMPARING NON-CT OSCAR Households (NO cash transfer) to street-based self-care
## Sep 17, 2021

# remove any variables in R's memory 
rm(list = ls())  

#install packages needed for markov model heemod package 
library("heemod")
library("diagram")
library("flexsurv")
library("mstate")
library("hesim")
library("BCEA")
library("ggplot2")
library("scales")
library("data.table")

#transition matrix that describes the 3 possible health states and 3 possible transitions. 
#The matrix is a square-matrix where the (i,j) element is a positive integer if a transition from i to j is possible and NA otherwise.
tmat <- rbind(c(NA, 1, 2),
              c(NA, NA, 3),
              c(NA, NA, NA))
print(tmat)

diagram::plotmat(t(tmat), name = c("Healthy", "HIV", "Death"),
                 pos = c(1, 2))


###Define key model parameters###

n_sample <- 1000                        #number of simulated individuals

par_mod <- define_parameters(
  age_base = 10.37,                      #mean age of OSCA at baseline
  age_cycle = model_time + age_base)     #age of individuals for a given cycle age_cycle is the age at the beginning of the model plus the time the model has run 


par_mod <- modify(
  par_mod,
  
  ## Transition probabilities (per cycle) and per care environment from JAMA PEDS physical health paper 
  #Street-connected youth/ self-care 
  p.HS.S    = 0.108,            	   # probability to become HIV positive if HIV negative at baseline 
  p.HD.S    = 0.100,                 # probability to die if HIV negative at baseline 
  p.HH.S    = 1-(p.HS.S + p.HD.S),   # probability to stay HIV negative if HIV negative at baseline 
  p.SH.S    = 0,           	         # probability to become HIV negative when HIV positive at baseline
  p.SD.S    = 0.000,                # probability to die regardless of HIV status at baseline
  p.SS.S    = 1-(p.SD.S),            # probability to stay HIV positive after becoming HIV positive 
  
  #Family-based settings (non-cash transfer)
  p.HS.f    = 0.009,          	     # probability to become HIV positive if HIV negative at baseline
  p.HD.f    = 0.008,                # probability to die if HIV negative at baseline 
  p.HH.f    = 1-(p.HS.f + p.HD.f),   # probability to stay HIV negative if HIV negative at baseline
  p.SH.f    = 0,           	         # probability to become HIV negative when HIV positive at baseline
  p.SD.f    = 0.25,                 # probability to if HIV positive at baseline
  p.SS.f    = 1-(p.SD.f),            # probability to stay HIV positive after becoming HIV positive 
  
  #cost and utility inputs (per cycle)
  cost_scy_recurrent = 0,                #mean recurrent costs in street-based self care
  cost_scy_capital = 0,                  #mean capital costs in street-based self care
  
  cost_fbs_recurrent = 664,            #mean recurrent costs in all family-based settings
  cost_fbs_capital = 36,               #mean capital costs (sum of capital and personal items) in all family-based settings
  
  utility_healthy = 0.98,                 # utility HIV-negative OSCA, age 10-18
  utility_hiv  = 0.7,                    # utility when HIV positive, symptomatic, pre-AIDS
  utility_dead = 0,       
  
  dw_healthy = 0,
  dw_hiv  = 0.274,                       # disability weight applied to the HIV symptomatic, pre-AIDS disease state
  dr = 0                             #equal discounting of costs and outcomes, 3% discount rate  
)


###Define the transition matrix for each care environment
##Street-based
mat_trans_scy <- define_transition( 
  p.HH.S, p.HS.S, p.HD.S, 
  p.SH.S,        p.SS.S ,         p.SD.S , 
  0,       0,         1
)

mat_trans_scy

##Family-based care
mat_trans_fbs <- define_transition( #FBS
  p.HH.f, p.HS.f, p.HD.f, 
  p.SH.f,         p.SS.f,          p.SD.f, 
  0,       0,         1
)

mat_trans_fbs



###FOR PRIMARY OUTCOME ICER: $/QALY
#Define the costs and UTILITIES associated with each health state for each care environment 

##SCY
state_healthyscy <- define_state( #State A, scy
  cost_health =  cost_scy_recurrent,
  cost_drugs = dispatch_strategy(
    scy =  cost_scy_capital
  ),
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  utility = discount(utility_healthy, dr)
)#healthy

state_HIVscy <- define_state( #State B, scy
  cost_health =  cost_scy_recurrent,
  cost_drugs = dispatch_strategy(
    scy =  cost_scy_capital
  ),
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  utility = discount(utility_hiv, dr)
) #HIV

state_deathscy <- define_state( #State C, scy
  cost_health = 0,
  cost_drugs = 0,
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  utility = 0
) #dead


##FBS
state_healthyFbs <- define_state( #State A, FBS
  cost_health = cost_fbs_recurrent ,
  cost_drugs = dispatch_strategy(
    fbs = cost_fbs_capital
  ),
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  utility = discount(utility_healthy, dr)
)#healthy

state_HIVFbs <- define_state( #State B, FBS
  cost_health = cost_fbs_recurrent,
  cost_drugs = dispatch_strategy(
    fbs = cost_fbs_capital
  ),
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  utility = discount(utility_hiv, dr)
) #HIV

state_deathFBS <- define_state( #State C, FBS
  cost_health = 0,
  cost_drugs = 0,
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  utility = 0
) #dead



###Define the intervention strategies for each care environment
#SCY
strat_scy <- define_strategy(
  transition = mat_trans_scy,
  state_healthyscy,
  state_HIVscy,
  state_deathscy
)


#FBS
strat_fbs <- define_strategy( 
  transition = mat_trans_fbs,
  state_healthyFbs,
  state_HIVFbs,
  state_deathFBS
)



###Run the health-state transition model for both care environments over 5-year forecast
res_mod <- run_model(
  scy = strat_scy,
  fbs = strat_fbs,
  parameters = par_mod,
  cycles = 5,           #5 cycles of 1 year each
  cost = cost_total, 
  effect = utility,     #effect outcome = QALY
  method = "end",
  init = c(1000, 0, 0)  #start with 1000 individuals in healthy state
)
summary(res_mod)

###Run the health-state transition model for both care environments over 10-year forecast
res_mod <- run_model(
  scy = strat_scy,
  fbs = strat_fbs,
  parameters = par_mod,
  cycles = 10,          #10 cycles of 1 year each
  cost = cost_total, 
  effect = utility,     #effect outcome = QALY
  method = "end",
  init = c(1000, 0, 0)  #start with 1000 individuals in healthy state
)
summary(res_mod)

###Run the health-state transition model for both care environments over 15-year forecast
res_mod <- run_model(
  scy = strat_scy,
  fbs = strat_fbs,
  parameters = par_mod,
  cycles = 15,          #15 cycles of 1 year each
  cost = cost_total, 
  effect = utility,     #effect outcome = QALY
  method = "end",
  init = c(1000, 0, 0)  #start with 1000 individuals in healthy state
)
summary(res_mod) 

dev.off()
plot(res_mod)
plot(res_mod, type = "counts", panel = "by_state")
plot(res_mod, type = "values", panel = "by_value")


#how many individuals are in each health state at the start and end of the forecast 
library(dplyr)
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 1 & state_names == "A")
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 5 & state_names == "A")
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 10 & state_names == "A")
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 15 & state_names == "A")

get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 1 & state_names == "B")
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 5 & state_names == "B")
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 10 & state_names == "B")
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 15 & state_names == "B")

get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 1 & state_names == "C")
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 5 & state_names == "C")
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 10 & state_names == "C")
get_counts(res_mod) %>% 
  dplyr::filter(markov_cycle == 15 & state_names == "C")






### FOR PRIMARY OUTCOME ICER: $/DALY
###Define the costs and DISABILITY WEIGHTS associated with each health state for each care environment 

##SCY
state_healthyscy <- define_state( #State A, scy
  cost_health =  cost_scy_recurrent,
  cost_drugs = dispatch_strategy(
    scy =  cost_scy_capital
  ),
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  life_year = 0
)#healthy

state_HIVscy <- define_state( #State B, scy
  cost_health =  cost_scy_recurrent,
  cost_drugs = dispatch_strategy(
    scy =  cost_scy_capital
  ),
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  life_year = discount( (utility_hiv*dw_hiv), dr) ##To estimate DALYs, the 2013 Global Burden of Disease (GBD) disability weight was applied to the HIV disease state as in previous studies [Mafirakureva et al. 2021]
) #HIV

state_deathscy <- define_state( #State C, scy
  cost_health = 0,
  cost_drugs = 0,
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  life_year = discount(1,dr)
) #dead


##FBS
state_healthyFbs <- define_state( #State A, FBS
  cost_health = cost_fbs_recurrent ,
  cost_drugs = dispatch_strategy(
    fbs = cost_fbs_capital
  ),
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  life_year = 0
)#healthy

state_HIVFbs <- define_state( #State B, FBS
  cost_health = cost_fbs_recurrent,
  cost_drugs = dispatch_strategy(
    fbs = cost_fbs_capital
  ),
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  life_year = discount( (utility_hiv*dw_hiv), dr)
) #HIV

state_deathFBS <- define_state( #State C, FBS
  cost_health = 0,
  cost_drugs = 0,
  cost_total = discount(cost_health + cost_drugs, dr, first = T),
  life_year = discount(1,dr)
) #dead



###Define the intervention strategies for each care environment
scy <- strat_scy <- define_strategy( #scy
  transition = mat_trans_scy,
  state_healthyscy,
  state_HIVscy,
  state_deathscy
)


strat_fbs <- define_strategy( #FBS
  transition = mat_trans_fbs,
  state_healthyFbs,
  state_HIVFbs,
  state_deathFBS
)



###Run the health-state transition model for both care environments over 5-year forecast
res_mod <- run_model(
  scy = strat_scy, 
  fbs = strat_fbs,
  parameters = par_mod,
  cycles = 5,          #5 cycles of 1 year each
  cost = cost_total, 
  effect = life_year*(-1),     #effect outcome = DALY
  method = "end",
  init = c(1000, 0, 0)  #start with 1000 individuals in healthy state
)
summary(res_mod)
print(res_mod)
head(get_counts(res_mod))



###Run the health-state transition model for both care environments over 10-year forecast
res_mod <- run_model(
  scy = strat_scy, 
  fbs = strat_fbs,
  parameters = par_mod,
  cycles = 10,          #10 cycles of 1 year each
  cost = cost_total, 
  effect = life_year*(-1),     #effect outcome = DALY
  method = "end",
  init = c(1000, 0, 0)  #start with 1000 individuals in healthy state
)
summary(res_mod)

###Run the health-state transition model for both care environments over 15-year forecast
res_mod <- run_model(
  scy = strat_scy, 
  fbs = strat_fbs,
  parameters = par_mod,
  cycles = 15,          #15 cycles of 1 year each
  cost = cost_total, 
  effect = life_year*(-1),     #effect outcome = DALY
  method = "end",
  init = c(1000, 0, 0)  #start with 1000 individuals in healthy state
)
summary(res_mod)








###Deterministic Sensitivity Analysis for 15-year time horizon 
#https://mran.microsoft.com/snapshot/2016-11-23/web/packages/heemod/vignettes/f-sensitivity.html

se <- define_dsa(
  
  age_base, 8.9, 11.9,
  
  dr, 0.0, 0.05, 
  
  p.HS.S, 0.049, 0.167,     
  p.HD.S, 0.041, 0.159,
  
  p.HS.f, 0.005, 0.014, 
  p.HD.f, 0.006, 0.017,     
  p.SD.f, 0.245, 0.265,   
  
  cost_fbs_recurrent, 497, 829,  
  cost_fbs_capital, 26.9, 44.9,       
  
  utility_healthy, 0.775, 1.0, 
  utility_hiv, 0.567, 0.837,   
  
  dw_hiv, 0.184, 0.377  
  
)

res_dsa <- run_dsa(
  model = res_mod,
  dsa = se
)
summary(res_dsa) 

plot(res_dsa,
     strategy = "fbs",
     result = "effect",
     type = "simple")

plot(res_dsa,
     strategy = "fbs",
     result = "effect",
     type = "simple")


plot(res_dsa,
     strategy = "fbs",
     result = "cost",
     type = "difference")


plot(res_dsa,
     strategy = "fbs",
     result = "icer",
     type = "difference",
     limits_by_bars = FALSE)



##probabilistic sensitivity analysis for 15-year time horizon
#varying each parameter simultaneously 
rsp <- define_psa(
  age_base ~ normal(mean = 10.37, sd = 2),
  
  p.HH.s + p.HS.s + p.HD.s ~ multinomial(792, 108, 100),
  p.HH.f + p.HS.f + p.HD.f ~ multinomial(980, 12, 8),
  p.SH.s + p.SS.s + p.SD.s ~ multinomial(0, 1000, 0),
  p.SH.f + p.SS.f + p.SD.f ~ multinomial(0, 750, 250),
  
  cost_fbs_recurrent  ~ gamma(mean = 664, sd = sqrt(664)),
  cost_fbs_capital ~ gamma(mean = 36, sd = sqrt(36)),
  
  dr ~ binomial(prob = 0.03, size = 100),
  utility_healthy ~ normal(mean = .98, sd = 0.1),               
  utility_hiv ~ normal(mean = 0.7, sd = 0.2),                   
  dw_hiv  ~ normal(mean = 0.274, sd = 0.1)
  
)


summary(rsp)

###Run the health-state transition model for both care environments over 15-year forecast
res_mod <- run_model(
  scy = strat_scy,   
  fbs = strat_fbs,
  parameters = par_mod,
  cycles = 15,          #15 cycles of 1 year each
  cost = cost_total, 
  effect = life_year*(-1),     #effect outcome = DALY
  method = "end",
  init = c(1000, 0, 0)  #start with 1000 individuals in healthy state
)
summary(res_mod)

pm_all <- run_psa(
  model = res_mod,
  psa = rsp,
  N = 1000
)

summary(
  pm_all, 
  threshold = c(500, 1000, 5000))

##export the results from the PSA in order to combine the CE planes and CEACs for all 3
##family-based settings in STATA 
export_savi(pm_all, folder = "PSA Results")

##cost-effectiveness plane based on DALYs
plot(pm_all,
     type = "ce")

##cost-effectiveness acceptability plane based on DALYs 
plot(pm_all, type = "ac", max_wtp = 5000, log_scale = FALSE)
