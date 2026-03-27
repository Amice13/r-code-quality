###########################################
## Replication File for Davis and Pratt, ##
## "The Forces of Attraction" #############
## July 2020  #############################
###########################################


## Read in State-Year-IGO dataset 
library(data.table)
SIGO <- as.data.frame(fread("SIGO.csv"))

## Subset to economic organizations 
SIGO.E <- SIGO[which(SIGO$Econ_IGO==1),] 

## Restrict sample to observations with membership eligibility
SIGO.ED <- SIGO.E[which(SIGO.E$eligible==1),]

## Descriptive statistics
length(unique(SIGO.ED$IGO)) # 233 IGOs
mean(SIGO.ED$Member) # 0.349 (overall membership rate in the sample)
mean(SIGO.ED$Member[SIGO.ED$alliances_avgmembers_lag_form == 0], na.rm=T) # 0.197 (membership rate with no alliances)
mean(SIGO.ED$Member[SIGO.ED$alliances_avgmembers_lag_form > 0], na.rm=T) # 0.514 (membership rate with at least one alliance)


## function for clustering SEs
cl <- function(dat,fm, cluster){ 
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }



##############################
## Code to Generate Table 1 ##
##############################

## Full logit model (Table 1, Col 2)
full_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + 
                       stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat <- full_ally_avg$data[-full_ally_avg$na.action,] 
cf <- cl(dat, full_ally_avg, as.character(dat$state))


dim(dat) # 570,695 observations
length(unique(dat$IGO)) # 231 IGOs in the sample
length(unique(dat$state)) # 164 states

## substantive effect 
df2 <- df1 <- dat
df2$alliances_avgmembers_lag_form <- mean(dat$alliances_avgmembers_lag_form) + sd(dat$alliances_avgmembers_lag_form)
df1$alliances_avgmembers_lag_form <- mean(dat$alliances_avgmembers_lag_form)
mean(predict(full_ally_avg, df2, type="response") - predict(full_ally_avg, df1, type="response")) # 0.059

df2 <- df1 <- dat
df2$trade_avgmembers.l_lag_form <- mean(dat$trade_avgmembers.l_lag_form) + sd(dat$trade_avgmembers.l_lag_form)
df1$trade_avgmembers.l_lag_form <- mean(dat$trade_avgmembers.l_lag_form)
mean(predict(full_ally_avg, df2, type="response") - predict(full_ally_avg, df1, type="response")) # 0.15

## Baseline model (Table 1, Col 1)
bl_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                     Polity_lag + GDP_lag, family = "binomial", data = dat)
cb <- cl(dat, bl_ally_avg, as.character(dat$state)) 


## Entry only model (Table 1, Col 3)
SIGO.ED.entry <- SIGO.ED[which(SIGO.ED$afterjoin==0),] # exclude years after a state joins an IGO
mean(SIGO.ED.entry$Member) # membership rate is low, we use rare events logit
library(Zelig)
entry_ally_avg <- zelig(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                            Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                            regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                            state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                            CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry, model="relogit")
summary(entry_ally_avg)

# stargazer won't take zelig output, so estimate a glm and replace with the zelig coefficients 
entry_ally_avg.l <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                          CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry)


## Year of formation only (Table 1, Col 4)
Form <- SIGO.ED[which(SIGO.ED$formation==1),]
mean(Form$Member)
form_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + #region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + #fatalMIDs_count_lag_form + 
                       comcol + formercol + 
                       CW1 + t + t2 + t3, family = "binomial", data = Form)
dat <- form_ally_avg$data[-form_ally_avg$na.action,]
cfo <- cl(dat, form_ally_avg, as.character(dat$state)) 


## Enlargement years only (Table 1, Col 5)
Expan <- SIGO.ED[which(SIGO.ED$formation==0 & SIGO.ED$afterjoin==0),]
mean(Expan$Member) # use rare events logit
expan_ally_avg <- zelig(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3, family = "binomial", data = Expan, model="relogit")
summary(expan_ally_avg)

expan_ally_avg.l <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                          CW1 + t + t2 + t3, family = "binomial", data = Expan)


## IGO Exit (Table 1, Col 6)
SIGO.E.Exit <- SIGO.ED[which(SIGO.ED$Member==1 | SIGO.ED$Exit==1),] # subset to years when a state is either a member or exits that year
mean(SIGO.E.Exit$Exit) # very rare
exit_ally_avg <- zelig(Exit ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                           Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                           regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                           state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                           CW1 + t + t2 + t3, data=SIGO.E.Exit, model="relogit") 
summary(exit_ally_avg)

exit_ally_avg.l <- glm(Exit ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3, family = "binomial", data=SIGO.E.Exit) 


## State and IGO Fixed Effects (Table 1, Col 7) -- sometimes have to increase memory to run
dat <- entry_ally_avg.l$data[-entry_ally_avg.l$na.action,]
FE_ally_avg <- zelig(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + #stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3 + state + IGO, family = "binomial", data = dat, model="relogit")
summary(FE_ally_avg)

FE_ally_avg.l <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + #stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3 + state + IGO, family = "binomial", data = dat)


## Diff-in-Diff (Table 1, Col 8)
DID_ally_avg <- lm(Member ~ alliances_avgmembers_lag_form + as.factor(t) + alliances_avgmembers_lag_form_treatgroup + 
                     trade_avgmembers.l_lag_form + trade_avgmembers_lag_form_treatgroup + 
                     alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                     Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval + 
                     regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                     state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol,
                   data = SIGO.ED)
dat <- SIGO.ED[-DID_ally_avg$na.action,]
cdid <- cl(dat, DID_ally_avg, as.character(dat$state)) 

df2 <- df1 <- dat
df2$alliances_avgmembers_lag_form <- mean(dat$alliances_avgmembers_lag_form) + sd(dat$alliances_avgmembers_lag_form)
df1$alliances_avgmembers_lag_form <- mean(dat$alliances_avgmembers_lag_form)
mean(predict(DID_ally_avg, df2) - predict(DID_ally_avg, df1)) # 0.090

df2 <- df1 <- dat
df2$trade_avgmembers.l_lag_form <- mean(dat$trade_avgmembers.l_lag_form) + sd(dat$trade_avgmembers.l_lag_form)
df1$trade_avgmembers.l_lag_form <- mean(dat$trade_avgmembers.l_lag_form)
mean(predict(full_ally_avg, df2, type="response") - predict(full_ally_avg, df1, type="response")) # 0.145


## Table 1
entrycoef <- entry_ally_avg.l
entrycoef$coefficients <- from_zelig_model(entry_ally_avg)[[1]]

expancoef <- expan_ally_avg.l
expancoef$coefficients <- from_zelig_model(expan_ally_avg)[[1]]

exitcoef <- exit_ally_avg.l
exitcoef$coefficients <- from_zelig_model(exit_ally_avgRE)[[1]] # stargazer won't take zelig output, so use glm with the zelig coefficients 

FEcoef <- FE_ally_avg.l
FEcoef$coefficients <- from_zelig_model(FE_ally_avg)[[1]]

names(DID_ally_avg$coefficients)[68] <- "alliances_treatgroup"

library(stargazer)
stargazer(bl_ally_avg, full_ally_avg, entrycoef, form_ally_avg, 
          expancoef, exitcoef, FEcoef, DID_ally_avg,
          se=list(cb[,2], cf[,2], unlist(get_se(entry_ally_avg)), 
                  cfo[,2], unlist(get_se(expan_ally_avg)), unlist(get_se(exit_ally_avg)), 
                  unlist(get_se(FE_ally_avg)),cdid[,2]),
          keep = c("alliances_avgmembers_lag_form", 
                   "trade_avgmembers.l_lag_form",  
                   "Polity_lag", "GDP_lag", "GDPpc_lag",  "trade.openness2_lag", 
                   "stringent_approval", "regional_org",
                   "CW1"),
          covariate.labels = c("Average Alliances", "Trade with Members", 
                               "Polity", "GDP", "GDP per capita", 
                               "Trade Openness", "Stringent Accession","Regional IGO",  
                               "Cold War"), 
          column.labels = c("Baseline", "Full", "Entry", "Formation", "Expansion", "Exit", "State-IGO FE", "Diff-in-Diff"),
          no.space = TRUE, keep.stat = c("n"), column.sep.width = "-9 pt",
          add.lines = list(c("# IGOs", length(unique(bl_ally_avg$data$IGO)), 
                             length(unique(full_ally_avg$data$IGO[-full_ally_avg$na.action])),
                             length(unique(entry_ally_avg.l$data$IGO[-entry_ally_avg.l$na.action])),
                             length(unique(form_ally_avg$data$IGO[-form_ally_avg$na.action])),
                             length(unique(expan_ally_avg.l$data$IGO[-expan_ally_avg.l$na.action])),
                             length(unique(exit_ally_avg.l$data$IGO[-exit_ally_avg.l$na.action])),
                             length(unique(FE_ally_avg.l$data$IGO)),
                             length(unique(SIGO.ED$IGO[-DID_ally_avg$na.action]))),
                           c("# States", length(unique(bl_ally_avg$data$state)), 
                             length(unique(full_ally_avg$data$state[-full_ally_avg$na.action])),
                             length(unique(entry_ally_avg.l$data$state[-entry_ally_avg.l$na.action])),
                             length(unique(form_ally_avg$data$state[-form_ally_avg$na.action])),
                             length(unique(expan_ally_avg.l$data$state[-expan_ally_avg.l$na.action])),
                             length(unique(exit_ally_avg.l$data$state[-exit_ally_avg.l$na.action])),
                             length(unique(FE_ally_avg.l$data$state)),
                             length(unique(SIGO.ED$state[-DID_ally_avg$na.action])))),
          align=TRUE, title="Effect of Alliances on IGO Membership") 


#############
##Figure 1 ##
#############

## We use Monte Carlo simulations to get standard errors for change in predicted probability
library(MASS)
vcov.cl   <- function(dat,fm, cluster){ # function to get clustered variance-covariance matrix
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)}

## effect of average alliances
dat <- SIGO.ED[-full_ally_avg$na.action,]
beta.sim <- mvrnorm(1000, mu = coef(full_ally_avg), # simulate 1,000 coefficients using model estimates and variance
                    Sigma = vcov.cl(dat, full_ally_avg, dat$state))
cov.sim <- cbind(rep(1, nrow(dat)), # get covariate data to use with simulated coefficients
                 dat[, names(coef(full_ally_avg))[-1]])
df1 <- df2 <- cov.sim
df1$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) 
df2$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) + 
  sd(cov.sim$alliances_avgmembers_lag_form) # increase by 1 sd
full.ally.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})
mean(full.ally.results) 
sd(full.ally.results) 


## effect of alliance with lead state
full_ally_LS <- glm(Member ~ ally_leadstate.dynamic_lag_form + trade_leadstate.dynamic.l_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                      CW1 + t + t2 + t3,
                    family = "binomial", data = SIGO.ED)
dat <- SIGO.ED[-full_ally_LS$na.action,]
beta.sim <- mvrnorm(1000, mu = coef(full_ally_LS), Sigma = vcov.cl(dat, full_ally_LS, dat$state))
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(full_ally_LS))[-1]])
df1 <- df2 <- cov.sim
df1$ally_leadstate.dynamic_lag_form <- 0 
df2$ally_leadstate.dynamic_lag_form <- 1 # increase from 0 to 1
full.allyLS.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})
mean(full.allyLS.results) 
sd(full.allyLS.results) 


## effect of average trade
dat <- SIGO.ED[-full_ally_avg$na.action,]
beta.sim <- mvrnorm(1000, mu = coef(full_ally_avg), Sigma = vcov.cl(dat, full_ally_avg, dat$state))
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(full_ally_avg))[-1]])
df1 <- df2 <- cov.sim
df1$trade_avgmembers.l_lag_form <- mean(cov.sim$trade_avgmembers.l_lag_form) 
df2$trade_avgmembers.l_lag_form <- mean(cov.sim$trade_avgmembers.l_lag_form) + 
  sd(cov.sim$trade_avgmembers.l_lag_form) 
full.trade.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})
mean(full.trade.results) 
sd(full.trade.results) 


## effect of lead state trade
dat <- SIGO.ED[-full_ally_LS$na.action,]
beta.sim <- mvrnorm(1000, mu = coef(full_ally_LS), Sigma = vcov.cl(dat, full_ally_LS, dat$state))
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(full_ally_LS))[-1]])
df1 <- df2 <- cov.sim
df1$trade_leadstate.dynamic.l_lag_form <- mean(cov.sim$trade_leadstate.dynamic.l_lag_form) 
df2$trade_leadstate.dynamic.l_lag_form <- mean(cov.sim$trade_leadstate.dynamic.l_lag_form) + 
  sd(cov.sim$trade_leadstate.dynamic.l_lag_form) 
full.tradeLS.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})
mean(full.tradeLS.results) 
sd(full.tradeLS.results) 


## effect of average S-scores
full_S_avg <- glm(Member ~ S_score_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                    Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                    regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                    state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                    CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat <- SIGO.ED[-full_S_avg$na.action,]
beta.sim <- mvrnorm(1000, mu = coef(full_S_avg), Sigma = vcov.cl(dat, full_S_avg, dat$state))
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(full_S_avg))[-1]])
df1 <- df2 <- cov.sim
df1$S_score_avgmembers_lag_form <- mean(cov.sim$S_score_avgmembers_lag_form) 
df2$S_score_avgmembers_lag_form <- mean(cov.sim$S_score_avgmembers_lag_form) + 
  sd(cov.sim$S_score_avgmembers_lag_form) 
full.S.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})
mean(full.S.results) 
sd(full.S.results) 


## effect of lead state S-scores
full_S_LS <- glm(Member ~ S_score_leadstate.dynamic_lag_form + trade_leadstate.dynamic.l_lag_form + 
                   Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                   regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                   state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                   CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat <- SIGO.ED[-full_S_LS$na.action,]
beta.sim <- mvrnorm(1000, mu = coef(full_S_LS), Sigma = vcov.cl(dat, full_S_LS, dat$state))
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(full_S_LS))[-1]])
df1 <- df2 <- cov.sim
df1$S_score_leadstate.dynamic_lag_form <- mean(cov.sim$S_score_leadstate.dynamic_lag_form) 
df2$S_score_leadstate.dynamic_lag_form <- mean(cov.sim$S_score_leadstate.dynamic_lag_form) + 
  sd(cov.sim$S_score_leadstate.dynamic_lag_form) 
full.SLS.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})
mean(full.SLS.results) 
sd(full.SLS.results) 


## effect of average UN ideal point distance
full_UNIP_avg <- glm(Member ~ UN_IP_sim_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat <- SIGO.ED[-full_UNIP_avg$na.action,]
beta.sim <- mvrnorm(1000, mu = coef(full_UNIP_avg), Sigma = vcov.cl(dat, full_UNIP_avg, dat$state))
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(full_UNIP_avg))[-1]])
df1 <- df2 <- cov.sim
df1$UN_IP_sim_avgmembers_lag_form <- mean(cov.sim$UN_IP_sim_avgmembers_lag_form) 
df2$UN_IP_sim_avgmembers_lag_form <- mean(cov.sim$UN_IP_sim_avgmembers_lag_form) + 
  sd(cov.sim$UN_IP_sim_avgmembers_lag_form) 
full.UNIP.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})
mean(full.UNIP.results) 
sd(full.UNIP.results) 


## effect of lead state UN ideal point distance
full_UNIP_LS <- glm(Member ~ UN_IP_sim_leadstate.dynamic_lag_form + trade_avgmembers.l_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                      CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat <- SIGO.ED[-full_UNIP_LS$na.action,]
beta.sim <- mvrnorm(1000, mu = coef(full_UNIP_LS), Sigma = vcov.cl(dat, full_UNIP_LS, dat$state))
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(full_UNIP_LS))[-1]])
df1 <- df2 <- cov.sim
df1$UN_IP_sim_leadstate.dynamic_lag_form <- mean(cov.sim$UN_IP_sim_leadstate.dynamic_lag_form) 
df2$UN_IP_sim_leadstate.dynamic_lag_form <- mean(cov.sim$UN_IP_sim_leadstate.dynamic_lag_form) + 
  sd(cov.sim$UN_IP_sim_leadstate.dynamic_lag_form) 
full.UNIPLS.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})
mean(full.UNIPLS.results) 
sd(full.UNIPLS.results) 

## Create Figure: Substantive Effect of Geopolitical Alignment 
par(mfrow=c(1,1))
plot(1:13, 1:13, cex=0, xlab="", ylab="Change in Pr(Membership)",
     ylim=c(-0.013, 0.2), xlim=c(1.5, 12.5), xaxt="n",
     main="Effect of Geopolitical Alignment on Probability of IGO Membership")
points(2.25, mean(full.ally.results), pch=19) # ally avg
lines(x=c(2.25,2.25), y=c(mean(full.ally.results) - sd(full.ally.results)*qnorm(0.975),
                          mean(full.ally.results) + sd(full.ally.results)*qnorm(0.975)))
points(2.75, mean(full.allyLS.results)) # ally ls
lines(x=c(2.75,2.75), y=c(mean(full.allyLS.results) - sd(full.allyLS.results)*qnorm(0.975),
                          mean(full.allyLS.results) + sd(full.allyLS.results)*qnorm(0.975)))
points(5.25, mean(full.S.results), pch=19) # S-score avg
lines(x=c(5.25,5.25), y=c(mean(full.S.results) - sd(full.S.results)*qnorm(0.975),
                          mean(full.S.results) + sd(full.S.results)*qnorm(0.975)))
points(5.75, mean(full.SLS.results)) # S-score ls
lines(x=c(5.75,5.75), y=c( mean(full.SLS.results) -  sd(full.SLS.results)*qnorm(0.975),
                           mean(full.SLS.results) +  sd(full.SLS.results)*qnorm(0.975)))
points(8.25, mean(full.UNIP.results), pch=19) # UNIP avg
lines(x=c(8.25,8.25), y=c(mean(full.UNIP.results) - sd(full.UNIP.results)*qnorm(0.975),
                          mean(full.UNIP.results) + sd(full.UNIP.results)*qnorm(0.975)))
points(8.75, mean(full.UNIPLS.results)) # UNIP ls
lines(x=c(8.75,8.75), y=c(mean(full.UNIPLS.results) - sd(full.UNIPLS.results)*qnorm(0.975),
                          mean(full.UNIPLS.results) + sd(full.UNIPLS.results)*qnorm(0.975)))
points(11.25, mean(full.trade.results), pch=19) # trade avg
lines(x=c(11.25,11.25), y=c(mean(full.trade.results) - sd(full.trade.results)*qnorm(0.975),
                            mean(full.trade.results) + sd(full.trade.results)*qnorm(0.975)))
points(11.75, mean(full.tradeLS.results)) # trade ls
lines(x=c(11.75,11.75), y=c(mean(full.tradeLS.results) - sd(full.tradeLS.results)*qnorm(0.975),
                            mean(full.tradeLS.results) + sd(full.tradeLS.results)*qnorm(0.975)))
abline(h=0, lty=2)
axis(side=1, at=c(2.5, 5.5, 8.5, 11.5),
     labels=c("Allianfces", "S-score", "Ideal Point Similarity", "Trade"))
legend(pch=c(19, 1), legend=c("Average Measure", "Lead State Measure"),
       x="topleft")





#############
## Table 2 ##
#############

SIGO.ED.break <- SIGO.ED[which(SIGO.ED$afterjoin==0 & SIGO.ED$sharpbreak_next5==1),]
mean(SIGO.ED.break$Member) # rare
shift_ally_avg <- zelig(Member ~ sharpbreak_add_IGO_next5 + sharpbreak_loss_IGO_next5 +
                          trade_avgmembers.l_lag_form + 
                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                          CW1 + t + t2 + t3,
                        family="binomial", data = SIGO.ED.break, model="relogit")
summary(shift_ally_avg)

shift_ally_avg.l <- glm(Member ~ sharpbreak_add_IGO_next5 + sharpbreak_loss_IGO_next5 +
                          trade_avgmembers.l_lag_form + 
                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                          CW1 + t + t2 + t3,
                        family="binomial", data = SIGO.ED.break)


## Adjust exit sample so that once exit occurs, you remove them from the sample
SIGO.ED.break.exit <- SIGO.ED[which((SIGO.ED$Member==1 | SIGO.ED$Exit==1) & SIGO.ED$sharpbreak_next5==1),]
mean(SIGO.ED.break.exit$Exit) # rare
shift_ally_avg.exit <- zelig(Exit ~ sharpbreak_add_IGO_next5 + sharpbreak_loss_IGO_next5 +
                               trade_avgmembers.l_lag_form + 
                               Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                               regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                               state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                               CW1 + t + t2 + t3,
                           family="binomial", data = SIGO.ED.break.exit, model="relogit")
summary(shift_ally_avg.exit)

shift_ally_avg.exit.l <- glm(Exit ~ sharpbreak_add_IGO_next5 + sharpbreak_loss_IGO_next5 +
                               trade_avgmembers.l_lag_form + 
                               Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                               regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                               state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                               CW1 + t + t2 + t3,
                             family="binomial", data = SIGO.ED.break.exit)

shift_entry <- shift_ally_avg.l
shift_entry$coefficients <- from_zelig_model(shift_ally_avg)[[1]]

shift_exit <- shift_ally_avg.exit.l
shift_exit$coefficients <- from_zelig_model(shift_ally_avg.exit)[[1]]

stargazer(shift_entry, shift_exit,
          se=list(unlist(get_se(shift_ally_avg)), unlist(get_se(shift_ally_avg.exit))),
          keep = c("sharpbreak_add_IGO_next5", 
                   "sharpbreak_loss_IGO_next5",  "trade_avgmembers.l_lag_form", 
                   "Polity_lag", "GDP_lag", "GDPpc_lag",  "trade.openness2_lag", 
                   "stringent_approval"),
          covariate.labels = c("New Alliance with Members", "Dropped Alliance with Members",
                               "Trade with Members", 
                               "Polity", "GDP", "GDP per capita", 
                               "Trade Openness", "Stringent Accession"), 
          column.labels = c("Entry", "Exit"),
          no.space = TRUE, keep.stat = c("n"), column.sep.width = "-9 pt",
          add.lines = list(c("# IGOs", length(unique(shift_ally_avg.l$data$IGO[-shift_ally_avg.l$na.action])), 
                             length(unique(shift_ally_avg.exit.l$data$IGO[-shift_ally_avg.exit.l$na.action]))),
                           c("# States", length(unique(shift_ally_avg.l$data$state[-shift_ally_avg.l$na.action])), 
                             length(unique(shift_ally_avg.exit.l$data$state[-shift_ally_avg.exit.l$na.action])))),
          align=TRUE, title="Effect of Alliances on IGO Membership: Sudden Reversals in Geopolitical Orientation") 



###################################
## Table 3: Finite Mixture Model ##
###################################

## See Forces_Replication_MM.R for estimation procedure.  
# Estimation takes a long time (8-10 hours), so fitted models have been saved for loading.

library(flexmix)
load("MM_SIGOED_2019.Rdata")

## Results for main (average) model
attributes(result.theory)$components # model 1 is alliances, model 2 is trade
model.assignments <- attributes(result.theory)$posterior[[1]]
colMeans(model.assignments) # 44% alliances, 56% trade

# refit to get clustered SEs
dat <- full_ally_avg$data[-full_ally_avg$na.action,] 
model1 <- glm(Member ~ alliances_avgmembers_lag_form + 
                Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                CW1 + t + t2 + t3, 
              family = "binomial", data = dat, weights = model.assignments[,1])
m1.se <- cl(dat, model1, as.character(dat$state))

model2 <- glm(Member ~ trade_avgmembers.l_lag_form + 
                Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                CW1 + t + t2 + t3, 
              family = "binomial", data = dat, weights = model.assignments[,2])
m2.se <- cl(dat, model2, as.character(dat$state))

stargazer(model1, model2, full_ally_avg,
          se=list(m1.se[,2], m2.se[,2], cf[,2]),
          keep = c("alliances_avgmembers_lag_form", "trade_avgmembers.l_lag_form",
                   "Polity_lag", "GDP_lag", "GDPpc_lag",  "trade.openness2_lag", 
                   "stringent_approval", "regional_org", "fatalMIDs_count_lag_form",
                   "region_mems_lag_form", "CW1"),
          covariate.labels = c("Average Alliances", "Trade with Members",
                               "Polity", "GDP", "GDP per capita", 
                               "Trade Openness", "Stringent Accession","Regional IGO",
                               "Existing Members from Region","Fatal MIDs with Members", 
                               "Cold War"), 
          column.labels = c("Geopolitical Model", "Economic Model", "Pooled Model"),
          no.space = TRUE, keep.stat = c("n"), column.sep.width = "-9 pt",
          align=TRUE, title="IGO Membership: Geopolitical vs. Economic Models") 

## Calculate change in Pr(membership) due to 1 sd shift in alliances
dat1 <- dat2 <- dat
dat1$alliances_avgmembers_lag_form <- mean(dat$alliances_avgmembers_lag_form)
dat2$alliances_avgmembers_lag_form <- mean(dat$alliances_avgmembers_lag_form) + sd(dat$alliances_avgmembers_lag_form)
pred1 <- predict(model1, newdata = dat1, type="response")
pred2 <- predict(model1, newdata = dat2, type="response")
sum(pred2 * model.assignments[,1]) / sum(model.assignments[,1]) - 
  sum(pred1 * model.assignments[,1]) / sum(model.assignments[,1]) # 15.0% shift in Pr(Membership)

## Calculate change in Pr(membership) due to 1 sd shift in trade
dat1 <- dat2 <- dat
dat1$trade_avgmembers.l_lag_form <- mean(dat$trade_avgmembers.l_lag_form)
dat2$trade_avgmembers.l_lag_form <- mean(dat$trade_avgmembers.l_lag_form) + sd(dat$trade_avgmembers.l_lag_form)
pred1 <- predict(model2, newdata = dat1, type="response")
pred2 <- predict(model2, newdata = dat2, type="response")
sum(pred2 * model.assignments[,2]) / sum(model.assignments[,2]) - 
  sum(pred1 * model.assignments[,2]) / sum(model.assignments[,2]) # 31.23% shift in Pr(Membership)


##############
## Figure 2 ##
##############

yr.geop <- tapply(model.assignments[,1], dat$year, mean)
plot(unique(dat$year), yr.geop, type="o", ylim=c(0.35, 0.6),
     main="Share of Observations Assigned to Geopolitical Model by Year",
     ylab="% Observations Assigned to Geopolitical Model", xlab="")



###############
## Figure 3: ##
###############
dat <- full_ally_avg$data[-full_ally_avg$na.action,] 
datLS <- full_ally_LS$data[-full_ally_LS$na.action,]
  
dat$democstate <- ifelse(dat$Polity_lag >= 7, 1, 0)
avg.b <- cbind(prop.table(apply(model.assignments, 2, function(x){sum(x[dat$CW1==1])})),
               prop.table(apply(model.assignments, 2, function(x){sum(x[dat$stringent_approval==1])})),
               prop.table(apply(model.assignments, 2, function(x){sum(x[dat$regional_org==1])})),
               prop.table(apply(model.assignments, 2, function(x){sum(x[dat$democstate==1])})))

prop.table(apply(model.assignments, 2, function(x){sum(x[dat$leadstate=="USA"])}))
prop.table(apply(model.assignments, 2, function(x){sum(x[dat$leadstate!="USA"])}))

model.assignments.ls <- attributes(resultLS.theory)$posterior[[1]]
datLS$democstate <- ifelse(datLS$Polity_lag >= 7, 1, 0)
LS.b <- cbind(prop.table(apply(model.assignments.ls, 2, function(x){sum(x[datLS$CW1==1])})),
              prop.table(apply(model.assignments.ls, 2, function(x){sum(x[datLS$stringent_approval==1])})),
              prop.table(apply(model.assignments.ls, 2, function(x){sum(x[datLS$regional_org==1])})),
              prop.table(apply(model.assignments.ls, 2, function(x){sum(x[datLS$democstate==1])})))

par(mfrow=c(1,2))
barplot(as.matrix(avg.b), 
        main = "Ties with Average Member",
        ylim=c(0, 1.15), ylab = "% Observations Assigned to Models",
        legend.text=c("Geopolitical Model", "Economic Model"),
        axisnames=TRUE, names.arg = c("Cold \n War", "Stringent \n Accession", "Regional \n IGO", "Democratic \n State"))
barplot(as.matrix(LS.b),
        main = "Ties with Lead State",
        ylim=c(0, 1.15), ylab = "% Observations Assigned to Models",
        legend.text=c("Geopolitical Model", "Economic Model"),
        axisnames=TRUE, names.arg = c("Cold \n War", "Stringent \n Accession", "Regional \n IGO", "Democratic \n State"))



#####################
## Appendix Tables ##
#####################

## Table A1

full_S_avg <- glm(Member ~ S_score_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                    Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + 
                    stringent_approval +
                    regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                    state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                    CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat <- full_S_avg$data[-full_S_avg$na.action,] 
cfs <- cl(dat, full_S_avg, as.character(dat$state))


bl_S_avg <- glm(Member ~ S_score_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                  Polity_lag + GDP_lag, family = "binomial", data = dat)
cbs <- cl(dat, bl_S_avg, as.character(dat$state)) 


full_UNIP_avg <- glm(Member ~ UN_IP_sim_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat <- SIGO.ED[-full_UNIP_avg$na.action,]
cfu <- cl(dat, full_UNIP_avg, as.character(dat$state))

bl_UNIP_avg <- glm(Member ~ UN_IP_sim_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                     Polity_lag + GDP_lag, family = "binomial", data = dat)
cbu <- cl(dat, bl_UNIP_avg, as.character(dat$state)) 

stargazer(bl_S_avg, full_S_avg, bl_UNIP_avg, full_UNIP_avg, 
          se=list(cbs[,2], cfs[,2], cbu[,2], cfu[,2]),
          keep = c("S_score_avgmembers_lag_form", "UN_IP_sim_avgmembers_lag_form",
                   "trade_avgmembers.l_lag_form",  
                   "Polity_lag", "GDP_lag", "GDPpc_lag",  "trade.openness2_lag", 
                   "stringent_approval", "regional_org",
                   "CW1"),
          covariate.labels = c("S score", "UN Ideal Point Similarity", 
                               "Trade with Members", 
                               "Polity", "GDP", "GDP per capita", 
                               "Trade Openness", "Stringent Accession","Regional IGO",  
                               "Cold War"), 
          column.labels = c("Baseline S-score", "Full S-score", "Baseline UN Ideal Point", "Full UN Ideal Point"),
          no.space = TRUE, keep.stat = c("n"), column.sep.width = "-9 pt",
          add.lines = list(c("# IGOs", length(unique(bl_S_avg$data$IGO)), 
                             length(unique(full_S_avg$data$IGO[-full_S_avg$na.action])),
                             length(unique(bl_UNIP_avg$data$IGO)), 
                             length(unique(full_UNIP_avg$data$IGO[-full_S_avg$na.action]))),
                           c("# States", length(unique(bl_S_avg$data$state)), 
                             length(unique(full_S_avg$data$state[-full_S_avg$na.action])),
                             length(unique(bl_UNIP_avg$data$state)), 
                             length(unique(full_UNIP_avg$data$state[-full_UNIP_avg$na.action])))),
          align=TRUE, title="Effect of Geopolitical Alignment on IGO Membership")


## Table A2

FDI <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
             FDI_inflow_state_log + FDI_outflow_state_log + 
             Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
             regional_org + region_mems_lag_form + IGO_mems_lag_form + 
             state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
             CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
datf <- FDI$data[-FDI$na.action,] 
cfdi <- cl(datf, FDI, as.character(datf$state))


lagDV_ally_avg <- glm(Member ~ Member_lag + alliances_avgmembers_lag_form + 
                        trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3,
                      family = "binomial", data = SIGO.ED)
datD <- lagDV_ally_avg$data[-lagDV_ally_avg$na.action,] 
cD <- cl(datD, lagDV_ally_avg, as.character(datD$state))


SIGO.ED.exclNATO <- SIGO.ED[-which(SIGO.ED$state %in% c("USA", "UK", "France", "Belgium", "Canada", "Iceland",
                                                        "Italy", "Luxembourg", "Netherlands", "Norway", 
                                                        "Portugal", "Greece", "Turkey", "Germany", "Spain",
                                                        "Czech Rep", "Hungary", "Poland", "Bulgaria", 
                                                        "Estonia", "Latvia", "Lithuania", "Romania", 
                                                        "Denmark", "Germany West",
                                                        "Slovakia", "Slovenia", "Albania", "Croatia")),]
nNATO_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3,
                      family = "binomial", data = SIGO.ED.exclNATO)
datnN <- nNATO_ally_avg$data[-nNATO_ally_avg$na.action,] 
cnN <- cl(datnN, nNATO_ally_avg, as.character(datnN$state))


CW_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + CW1:alliances_avgmembers_lag_form + 
                     trade_avgmembers.l_lag_form + 
                     Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                     regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                     state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                     CW1 + t + t2 + t3,
                   family = "binomial", data = SIGO.ED)
datCW <- CW_ally_avg$data[-CW_ally_avg$na.action,] 
cCW <- cl(datCW, CW_ally_avg, as.character(datCW$state))


defense_ally_avg <- glm(Member ~ alliances_defense_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                          CW1 + t + t2 + t3,
                        family = "binomial", data = SIGO.ED)
datDP <- defense_ally_avg$data[-defense_ally_avg$na.action,] 
cDP <- cl(datDP, defense_ally_avg, as.character(datDP$state))


count_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + alliance_count + zero_alliances + 
                        trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3,
                      family = "binomial", data = SIGO.ED)
datcount <- count_ally_avg$data[-count_ally_avg$na.action,]
ccount <- cl(datcount, count_ally_avg, as.character(datcount$state))

SIGO.ED$avg_dist_log <- log(SIGO.ED$avg_dist)
dist_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + avg_dist_log + t + t2 + t3,
                     family = "binomial", data = SIGO.ED)
datdist <- dist_ally_avg$data[-dist_ally_avg$na.action,]
cdist <- cl(datdist, dist_ally_avg, as.character(datdist$state))

# structure data for Cox PH
library(survival)
orgs <- unique(SIGO.ED$IGO)
SIGO.surv <- SIGO.ED[0,]
for(i in orgs){
  sub <- SIGO.ED[which(SIGO.ED$IGO==i),]
  mem.mean <- tapply(sub$Member, sub$state, mean)
  nonmems <- names(mem.mean)[mem.mean==0]
  keep.nonmems <- which(SIGO.ED$IGO==i & SIGO.ED$state %in% nonmems)
  mems <-  names(mem.mean)[mem.mean>0]
  keeps <- c()
  for(j in mems){
    join.yr <- min(sub$year[which(sub$Member==1 & sub$state==j)])
    rel.obs <- which(SIGO.ED$IGO==i & SIGO.ED$state==j & SIGO.ED$year <= join.yr)
    keeps <- c(keeps, rel.obs)
  }
  SIGO.surv <- rbind(SIGO.surv, SIGO.ED[c(keep.nonmems, keeps),])
  print(which(orgs==i))
}

## For each state-IGO pair, create indicators for begin period and end period
SIGO.surv$begin.period <- SIGO.surv$end.period <- 
  SIGO.surv$begin.period2 <- SIGO.surv$end.period2 <- NA
for(i in orgs){
  sub <- SIGO.surv[which(SIGO.surv$IGO==i),]
  states <- unique(sub$state)
  orgyears <- sort(unique(sub$year))
  for(z in orgyears){
    sub$begin.period2[sub$year==z] <- which(orgyears==z)-1
    sub$end.period2[sub$year==z] <- which(orgyears==z)
  }
  for(j in states){
    sub2 <- sub[which(sub$state==j),]
    sub2$begin.period <- sub2$year - min(sub2$year)
    sub2$end.period <- sub2$begin.period + 1
    sub$begin.period[which(sub$state==j)] <- sub2$begin.period
    sub$end.period[which(sub$state==j)] <- sub2$begin.period + 1
  }
  SIGO.surv$begin.period[which(SIGO.surv$IGO==i)] <- sub$begin.period
  SIGO.surv$end.period[which(SIGO.surv$IGO==i)] <- sub$end.period
  SIGO.surv$begin.period2[which(SIGO.surv$IGO==i)] <- sub$begin.period2
  SIGO.surv$end.period2[which(SIGO.surv$IGO==i)] <- sub$end.period2
  print(which(orgs==i))
}

Cox2 <- coxph(Surv(begin.period, end.period, Member) ~
                alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + 
                region_mems_lag_form + IGO_mems_lag_form + 
                state_mems_lag + 
                fatalMIDs_count_lag_form + comcol + formercol + 
                CW1 + cluster(state) + strata(IGO), 
              data=SIGO.surv)
summary(Cox2)
cox.zph(Cox2)

# Repeat model above, with time interaction for variables that violate PH assumption

Cox2c <- coxph(Surv(begin.period, end.period, Member) ~
                 alliances_avgmembers_lag_form + 
                 trade_avgmembers.l_lag_form + trade_avgmembers.l_lag_form:log(end.period) + 
                 Polity_lag + Polity_lag:log(end.period) + 
                 GDP_lag + GDP_lag:log(end.period) + 
                 GDPpc_lag + GDPpc_lag:log(end.period) + 
                 trade.openness2_lag + #trade.openness2_lag:log(end.period) + 
                 region_mems_lag_form + region_mems_lag_form:log(end.period) + 
                 IGO_mems_lag_form + IGO_mems_lag_form:log(end.period) + 
                 state_mems_lag + state_mems_lag:log(end.period) + 
                 fatalMIDs_count_lag_form + comcol + comcol:log(end.period) + formercol + 
                 CW1 + #CW1:log(end.period) + 
                 cluster(state_IGO) + strata(IGO),
               data=SIGO.surv)
cox.zph(Cox2c) # trade always violates PHA


library(stargazer)
stargazer(FDI, lagDV_ally_avg, nNATO_ally_avg, CW_ally_avg, 
          defense_ally_avg, count_ally_avg, dist_ally_avg, Cox2,  
          se=list(cfdi[,2], cD[,2], cnN[,2], cCW[,2], cDP[,2], 
                  ccount[,2], cdist[,2], NULL),
          keep = c("alliances_avgmembers_lag_form", "alliance_count",
                   "trade_avgmembers.l_lag_form",  "alliances_defense_avgmembers_lag_form",
                   "FDI_inflow_state_log", "FDI_outflow_state_log",
                   "Polity_lag", "CW1",  
                   "avg_dist", "alliances_avgmembers_lag_form:CW1"),
          #covariate.labels = c("Average Alliances", "Incoming FDI", "Outgoing FDI",
          #                   "Cold War * Alliances with Members", "Alliance Count", "Avg. Distance",
          #                   "Trade with Members", 
          #                   "Polity", "Cold War"), 
          column.labels = c("FDI", "Lag DV", "Excl NATO", "CW", "Defense Pacts", 
                            "Alliance Count", "Distance", "Cox PH"),
          no.space = TRUE, keep.stat = c("n"), column.sep.width = "-11 pt",
          add.lines = list(c("# IGOs",  
                             length(unique(datf$IGO)), length(unique(datD$IGO)),
                             length(unique(datnN$IGO)), length(unique(datCW$IGO)),
                             length(unique(datDP$IGO)), length(unique(datcount$IGO)),
                             length(unique(datdist$IGO)), length(unique(datcount$IGO)),
                             length(unique(SIGO.surv$IGO[-Cox2$na.action]))),
                           c("# States", 
                             length(unique(datf$state)), length(unique(datD$state)),
                             length(unique(datnN$state)), length(unique(datCW$state)),
                             length(unique(datDP$state)), length(unique(datcount$state)),
                             length(unique(datdist$state)), length(unique(datcount$state)),
                             length(unique(SIGO.surv$state[-Cox2$na.action])))),
          align=TRUE, title="Effect of Alliances on IGO Membership") 


## Appendix Table A3: remove IGOs that might count as alliances 

IGO.alliances <- c("CIS", "ECOWAS", "LOAS", "WEU", "MRU", "GCC", "OECS", "AMU", "SCO")
SIGO.sub <- SIGO.ED[-which(SIGO.ED$IGO %in% IGO.alliances),]

excl_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3, family = "binomial", data = SIGO.sub)
dat.e <- excl_ally_avg$data[-excl_ally_avg$na.action,] 
c.e <- cl(dat.e, excl_ally_avg, as.character(dat.e$state))

excl_bl_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                          Polity_lag + GDP_lag, family = "binomial", data = dat.e)
cb.e <- cl(dat.e, excl_bl_ally_avg, as.character(dat.e$state)) 

SIGO.ED.entry <- SIGO.sub[which(SIGO.sub$afterjoin==0),] 
excl_entry_ally <- zelig(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                           Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                           regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                           state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                           CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry, model="relogit")
excl_entry_ally.l <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                          CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry)

Form <- SIGO.sub[which(SIGO.sub$formation==1),]
form_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3, family = "binomial", data = Form)
dat <- form_ally_avg$data[-form_ally_avg$na.action,]
cfo.e <- cl(dat, form_ally_avg, as.character(dat$state)) 

Expan <- SIGO.sub[which(SIGO.sub$formation==0 & SIGO.sub$afterjoin==0),]
expan_ally_avg <- zelig(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                          CW1 + t + t2 + t3, family = "binomial", data = Expan, model="relogit")
expan_ally_avg.l <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                          CW1 + t + t2 + t3, family = "binomial", data = Expan)

sub.Exit <- SIGO.sub[which(SIGO.sub$Member==1 | SIGO.sub$Exit==1),]
excl_exit_ally_avg <- zelig(Exit ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                           Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                           regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                           state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                           CW1 + t + t2 + t3, data=sub.Exit, model="relogit")
excl_exit_ally_avg.l <- glm(Exit ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                              Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                              regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                              state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                              CW1 + t + t2 + t3, data=sub.Exit)

dat <- excl_entry_ally$data[-excl_entry_ally$na.action,]
FE_ally_excl <- zelig(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + #stringent_approval +
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                      CW1 + t + t2 + t3 + state + IGO, family = "binomial", data = dat, model="relogit")

FE_ally_excl.l <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + #stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3 + state + IGO, family = "binomial", data = dat)

DID_ally_avg <- lm(Member ~ alliances_avgmembers_lag_form + as.factor(t) + alliances_avgmembers_lag_form_treatgroup + 
                     trade_avgmembers.l_lag_form + trade_avgmembers_lag_form_treatgroup + 
                     alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                     Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval + 
                     regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                     state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol,
                   data = SIGO.sub)
dat <- SIGO.sub[-DID_ally_avg$na.action,]
cdid.e <- cl(dat, DID_ally_avg, as.character(dat$state)) 

excl_entry <- excl_entry_ally.l
excl_entry$coefficients <- from_zelig_model(excl_entry_ally)[[1]] 

excl_expan <- expan_ally_avg.l
excl_expan$coefficients <- from_zelig_model(expan_ally_avg)[[1]] 

excl_exit <- excl_exit_ally_avg.l
excl_exit$coefficients <- from_zelig_model(excl_exit_ally_avg)[[1]] 

excl_FE <- FE_ally_excl.l
excl_FE$coefficients <- from_zelig_model(FE_ally_excl)[[1]] 


stargazer(excl_bl_ally_avg, excl_ally_avg, excl_entry, form_ally_avg, 
          excl_expan, excl_exit, excl_FE, DID_ally_avg,
          se=list(cb.e[,2], c.e[,2], unlist(get_se(excl_entry_ally)), 
                  cfo.e[,2], unlist(get_se(expan_ally_avg)), 
                  unlist(get_se(excl_exit_ally_avg)), 
                  unlist(get_se(FE_ally_excl)), cdid.e[,2]),
          keep = c("alliances_avgmembers_lag_form", 
                   "trade_avgmembers.l_lag_form",  
                   "Polity_lag", "GDP_lag", "GDPpc_lag",  "trade.openness2_lag", 
                   "stringent_approval", "regional_org",
                   "CW1"),
          #covariate.labels = c("Average Alliances", "Trade with Members", 
          #                     "Polity", "GDP", "GDP per capita", 
          #                     "Trade Openness", "Stringent Accession","Regional IGO",  
          #                     "Cold War"), 
          column.labels = c("Baseline", "Full", "Entry", "Formation", "Expansion", "Exit", "State-IGO FE", "Diff-in-Diff"),
          no.space = TRUE, keep.stat = c("n"), column.sep.width = "-9 pt",
          add.lines = list(c("# IGOs", length(unique(excl_bl_ally_avg$data$IGO)), 
                             length(unique(excl_ally_avg$data$IGO[-excl_ally_avg$na.action])),
                             length(unique(excl_entry_ally.l$data$IGO[-excl_entry_ally.l$na.action])),
                             length(unique(form_ally_avg$data$IGO[-form_ally_avg$na.action])),
                             length(unique(expan_ally_avg.l$data$IGO[-expan_ally_avg.l$na.action])),
                             length(unique(excl_exit_ally_avg.l$data$IGO[-excl_exit_ally_avg.l$na.action])),
                             length(unique(FE_ally_excl.l$data$IGO)),
                             length(unique(SIGO.sub$IGO[-DID_ally_avg$na.action]))),
                           c("# States", length(unique(excl_bl_ally_avg$data$state)), 
                             length(unique(excl_ally_avg$data$state[-excl_ally_avg$na.action])),
                             length(unique(excl_entry_ally.l$data$state[-excl_entry_ally.l$na.action])),
                             length(unique(form_ally_avg$data$state[-form_ally_avg$na.action])),
                             length(unique(expan_ally_avg.l$data$state[-expan_ally_avg.l$na.action])),
                             length(unique(excl_exit_ally_avg.l$data$state[-excl_exit_ally_avg.l$na.action])),
                             length(unique(FE_ally_excl.l$data$state)),
                             length(unique(SIGO.sub$state[-DID_ally_avg$na.action])))),
          align=TRUE, title="Effect of Alliances on IGO Membership") 


## Table A4

stringent <- glm(Member ~ alliances_avgmembers_lag_form + 
                   alliances_avgmembers_lag_form*stringent_approval + 
                   trade_avgmembers.l_lag_form + 
                   Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                   regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                   state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                   CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat.str <- stringent$data[-stringent$na.action,] 
c.str <- cl(dat.str, stringent, as.character(dat.str$state))

SIGO.fta <- SIGO.ED[SIGO.ED$fta !=1,]
SIGO.bank <- SIGO.ED[SIGO.ED$bank !=1,]
SIGO.commodity <- SIGO.ED[SIGO.ED$commodity !=1,]

fta_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + 
                      stringent_approval +
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + 
                      fatalMIDs_count_lag_form + 
                      comcol + formercol + 
                      CW1 + t + t2 + t3,
                    family = "binomial", data = SIGO.fta)
dat <- fta_ally_avg$data[-fta_ally_avg$na.action,] 
cfta <- cl(dat, fta_ally_avg, as.character(dat$state)) 

fta_ally_LS <- glm(Member ~ ally_leadstate.dynamic_lag_form + trade_leadstate.dynamic_lag_form + 
                     Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + 
                     stringent_approval +
                     regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                     state_mems_lag + 
                     fatalMIDs_count_lag_form + 
                     comcol + formercol + 
                     CW1 + t + t2 + t3,
                   family = "binomial", data = SIGO.fta)
dat <- fta_ally_LS$data[-fta_ally_LS$na.action,] 
Lfta <- cl(dat, fta_ally_LS, as.character(dat$state)) # pos and insignificant

bank_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3,
                     family = "binomial", data = SIGO.bank)
dat <- bank_ally_avg$data[-bank_ally_avg$na.action,] #
cbank <- cl(dat, bank_ally_avg, as.character(dat$state))

bank_ally_LS <- glm(Member ~ ally_leadstate.dynamic_lag_form + trade_leadstate.dynamic_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                      CW1 + t + t2 + t3,
                    family = "binomial", data = SIGO.bank)
dat <- bank_ally_LS$data[-bank_ally_LS$na.action,] #
Lbank <- cl(dat, bank_ally_LS, as.character(dat$state))

commodity_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                            Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                            regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                            state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                            CW1 + t + t2 + t3,
                          family = "binomial", data = SIGO.commodity)
dat <- commodity_ally_avg$data[-commodity_ally_avg$na.action,] #
ccom <- cl(dat, commodity_ally_avg, as.character(dat$state))

commodity_ally_LS <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                           Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                           regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                           state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                           CW1 + t + t2 + t3,
                         family = "binomial", data = SIGO.commodity)
dat <- commodity_ally_LS$data[-commodity_ally_LS$na.action,] 
Lcom <- cl(dat, commodity_ally_LS, as.character(dat$state))

stargazer(stringent, fta_ally_avg, bank_ally_avg, commodity_ally_avg, 
          se=list(c.str[,2], cfta[,2], cbank[,2], ccom[,2]),
          no.space = TRUE, keep.stat = c("n"), column.sep.width = "-9 pt",
          add.lines = list(c("# IGOs", length(unique(stringent$data$IGO[-stringent$na.action])), 
                             length(unique(fta_ally_avg$data$IGO[-fta_ally_avg$na.action])),
                             length(unique(bank_ally_avg$data$IGO[-bank_ally_avg$na.action])),
                             length(unique(commodity_ally_avg$data$IGO[-commodity_ally_avg$na.action]))),
                           c("# States", length(unique(stringent$data$state[-stringent$na.action])), 
                             length(unique(fta_ally_avg$data$state[-fta_ally_avg$na.action])),
                             length(unique(bank_ally_avg$data$state[-bank_ally_avg$na.action])),
                             length(unique(commodity_ally_avg$data$state[-commodity_ally_avg$na.action])))),
          align=TRUE, title="Effect of Alliances on IGO Membership") 

## Table A5: decade-level interactions 
SIGO.ED$decade <- NA
SIGO.ED$decade[SIGO.ED$year %in% 1940:1949] <- "aa_forty"
SIGO.ED$decade[SIGO.ED$year %in% 1950:1959] <- "ab_fifty"
SIGO.ED$decade[SIGO.ED$year %in% 1960:1969] <- "ac_sixty"
SIGO.ED$decade[SIGO.ED$year %in% 1970:1979] <- "ad_seventy"
SIGO.ED$decade[SIGO.ED$year %in% 1980:1989] <- "ae_eighty"
SIGO.ED$decade[SIGO.ED$year %in% 1990:1999] <- "af_ninety"
SIGO.ED$decade[SIGO.ED$year %in% 2000:2009] <- "ag_aught"
SIGO.ED$decade[SIGO.ED$year %in% 2010:2019] <- "ah_ten"

decade_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + alliances_avgmembers_lag_form:decade +
                         trade_avgmembers.l_lag_form + trade_avgmembers.l_lag_form:decade +
                         Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + 
                         stringent_approval +
                         regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                         state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                         CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat <- decade_ally_avg$data[-decade_ally_avg$na.action,] 
cdec <- cl(dat, decade_ally_avg, as.character(dat$state))

stargazer(decade_ally_avg,
          se=list(cdec[,2]),
          keep = c("alliances_avgmembers_lag_form", 
                   "trade_avgmembers.l_lag_form",  
                   "decade"),
          no.space = F, keep.stat = c("n"), column.sep.width = "-9 pt",
          align=TRUE, title="Effect of Geopolitical Alignment by Decade")


## Table A6: alliance, trade interaction
full_inter_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        alliances_avgmembers_lag_form*trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + 
                        stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat <- full_inter_avg$data[-full_inter_avg$na.action,] 
cint <- cl(dat, full_inter_avg, as.character(dat$state))

stargazer(full_inter_avg,
          se=list(cint[,2]),
          no.space = F, keep.stat = c("n"), column.sep.width = "-9 pt",
          align=TRUE, title="Interactive Effect of Alliances, Trade")


## Table A7: full sample of state-IGO years

full_ally_avg2 <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3, family = "binomial", data = SIGO.E)
dat <- full_ally_avg2$data[-full_ally_avg2$na.action,] 
cf2 <- cl(dat, full_ally_avg2, as.character(dat$state))

bl_ally_avg2 <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                      Polity_lag + GDP_lag, family = "binomial", data = dat)
cb2 <- cl(dat, bl_ally_avg2, as.character(dat$state)) 

SIGO.E.entry <- SIGO.E[which(SIGO.E$afterjoin==0),] 
entry_ally_avg2 <- zelig(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                         Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                         regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                         state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                         CW1 + t + t2 + t3, family = "binomial", data = SIGO.E.entry, model="relogit")
entry_ally_avg2.l <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                           Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                           regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                           state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                           CW1 + t + t2 + t3, family = "binomial", data = SIGO.E.entry)

Form <- SIGO.E[which(SIGO.E$formation==1),]
form_ally_avg2 <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3, family = "binomial", data = Form)
dat <- form_ally_avg2$data[-form_ally_avg2$na.action,]
cfo2 <- cl(dat, form_ally_avg2, as.character(dat$state)) 

Expan <- SIGO.E[which(SIGO.E$formation==0 & SIGO.E$afterjoin==0),]
expan_ally_avg2 <- zelig(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                         Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                         regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                         state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                         CW1 + t + t2 + t3, family = "binomial", data = Expan, model="relogit")
expan_ally_avg2.l <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                           Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                           regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                           state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                           CW1 + t + t2 + t3, family = "binomial", data = Expan)

SIGO.E.Exit <- SIGO.E[which(SIGO.E$Member==1 | SIGO.E$Exit==1),] 
exit_ally_avg2 <- zelig(Exit ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3, family = "binomial", data = SIGO.E.Exit, model="relogit")
exit_ally_avg2.l <- glm(Exit ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                          CW1 + t + t2 + t3, family = "binomial", data = SIGO.E.Exit)

dat <- entry_ally_avg2.l$data[-entry_ally_avg2.l$na.action,]
FE_ally_avg2 <- zelig(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + #stringent_approval +
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                      CW1 + t + t2 + t3 + state + IGO, family = "binomial", data = dat, model="relogit")
FE_ally_avg2.l <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + #stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3 + state + IGO, family = "binomial", data = dat)

DID_ally_avg2 <- lm(Member ~ alliances_avgmembers_lag_form + as.factor(t) + alliances_avgmembers_lag_form_treatgroup + 
                      trade_avgmembers.l_lag_form + trade_avgmembers_lag_form_treatgroup + 
                      alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval + 
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol,
                    data = SIGO.E)
dat <- SIGO.E[-DID_ally_avg2$na.action,]
cdid2 <- cl(dat, DID_ally_avg2, as.character(dat$state)) 

entry2 <- entry_ally_avg2.l
entry2$coefficients <- from_zelig_model(entry_ally_avg2)[[1]] 

expan2 <- expan_ally_avg2.l
expan2$coefficients <- from_zelig_model(expan_ally_avg2)[[1]] 

exit2 <- exit_ally_avg2.l
exit2$coefficients <- from_zelig_model(exit_ally_avg2)[[1]] 

FE2 <- FE_ally_avg2.l
FE2$coefficients <- from_zelig_model(FE_ally_avg2)[[1]] 

names(DID_ally_avg2$coefficients)[68] <- "alliances_treatgroup"

library(stargazer)
stargazer(bl_ally_avg2, full_ally_avg2, entry2, form_ally_avg2, 
          expan2, r2, FE2, DID_ally_avg2,
          se=list(cb2[,2], cf2[,2], unlist(get_se(entry_ally_avg2)), cfo2[,2], 
                  unlist(get_se(expan_ally_avg2)),  unlist(get_se(exit_ally_avg2)), 
                  unlist(get_se(FE_ally_avg2)), cdid2[,2]),
          keep = c("alliances_avgmembers_lag_form", 
                   "trade_avgmembers.l_lag_form",  
                   "Polity_lag", "GDP_lag", "GDPpc_lag",  "trade.openness2_lag", 
                   "stringent_approval", "regional_org",
                   "CW1"),
          covariate.labels = c("Average Alliances", "Trade with Members", 
                               "Polity", "GDP", "GDP per capita", 
                               "Trade Openness", "Stringent Accession","Regional IGO",  
                               "Cold War"), 
          column.labels = c("Baseline", "Full", "Entry", "Formation", "Expansion", "Exit", "State-IGO FE", "Diff-in-Diff"),
          no.space = TRUE, keep.stat = c("n"), column.sep.width = "-9 pt",
          add.lines = list(c("# IGOs", length(unique(bl_ally_avg2$data$IGO)), 
                             length(unique(full_ally_avg2$data$IGO[-full_ally_avg2$na.action])),
                             length(unique(entry_ally_avg2.l$data$IGO[-entry_ally_avg2.l$na.action])),
                             length(unique(form_ally_avg2$data$IGO[-form_ally_avg2$na.action])),
                             length(unique(expan_ally_avg2.l$data$IGO[-expan_ally_avg2.l$na.action])),
                             length(unique(exit_ally_avg2.l$data$IGO[-exit_ally_avg2.l$na.action])),
                             length(unique(FE_ally_avg2.l$data$IGO)),
                             length(unique(SIGO.E$IGO[-DID_ally_avg2$na.action]))),
                           c("# States", length(unique(bl_ally_avg2$data$state)), 
                             length(unique(full_ally_avg2$data$state[-full_ally_avg2$na.action])),
                             length(unique(entry_ally_avg2.l$data$state[-entry_ally_avg2.l$na.action])),
                             length(unique(form_ally_avg2$data$state[-form_ally_avg2$na.action])),
                             length(unique(expan_ally_avg2.l$data$state[-expan_ally_avg2.l$na.action])),
                             length(unique(exit_ally_avg2.l$data$state[-exit_ally_avg2.l$na.action])),
                             length(unique(FE_ally_avg2.l$data$state)),
                             length(unique(SIGO.E$state[-DID_ally_avg2$na.action])))),
          align=TRUE, title="Effect of Alliances on IGO Membership") 


## Table A8: Lead State Mixture Model

load("MM_SIGOED_2019.Rdata")
attributes(resultLS.theory)$components # model 1 is alliances, model 2 is trade
model.assignments <- attributes(resultLS.theory)$posterior[[1]]
colMeans(model.assignments) # 56% alliances, 44% trade

# refit to get clustered SEs
full_ally_LS <- glm(Member ~ ally_leadstate.dynamic_lag_form + trade_leadstate.dynamic.l_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                      CW1 + t + t2 + t3,
                    family = "binomial", data = SIGO.ED)
dat <- full_ally_LS$data[-full_ally_LS$na.action,] 
cls <- cl(dat, full_ally_LS, as.character(dat$state))


model1 <- glm(Member ~ ally_leadstate.dynamic_lag_form + 
                Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                CW1 + t + t2 + t3, 
              family = "binomial", data = dat, weights = model.assignments[,1])
m1.se <- cl(dat, model1, as.character(dat$state))
df1 <- df2 <- dat
df1$ally_leadstate.dynamic_lag_form <- 0
df2$ally_leadstate.dynamic_lag_form <- 1
mean(predict(model1, newdata=df2, type="response") - predict(model1, newdata=df1, type="response"))

model2 <- glm(Member ~ trade_leadstate.dynamic.l_lag_form + 
                Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                CW1 + t + t2 + t3, 
              family = "binomial", data = dat, weights = model.assignments[,2])
m2.se <- cl(dat, model2, as.character(dat$state))

stargazer(model1, model2, full_ally_LS,
          se=list(m1.se[,2], m2.se[,2], cls[,2]),
          keep = c("ally_leadstate.dynamic_lag_form", "trade_leadstate.dynamic.l_lag_form",
                   "Polity_lag", "GDP_lag", "GDPpc_lag",  "trade.openness2_lag", 
                   "stringent_approval", "regional_org", "fatalMIDs_count_lag_form",
                   "region_mems_lag_form", "CW1"),
          covariate.labels = c("Alliance Lead State", "Trade with Lead State",
                               "Polity", "GDP", "GDP per capita", 
                               "Trade Openness", "Stringent Accession","Regional IGO",
                               "Existing Members from Region", "Fatal MIDs with Members", 
                               "Cold War"), 
          column.labels = c("Geopolitical Model", "Economic Model", "Pooled Model"),
          no.space = TRUE, keep.stat = c("n"), column.sep.width = "-9 pt",
          align=TRUE, title="IGO Membership: Geopolitical vs. Economic Models") 


## Table A9: Percent of Observations Consistent with Geopolitical Model
model.assignments <- attributes(result.theory)$posterior[[1]]
dat <- full_ally_avg$data[-full_ally_avg$na.action,] 

tapply(model.assignments[,1], dat$CW1, mean)
tapply(model.assignments[,1], dat$stringent_approval, mean)
tapply(model.assignments[,1], dat$regional_org, mean)
dat$dem <- ifelse(dat$Polity >= 7, 1, 0)
tapply(model.assignments[,1], dat$dem, mean)


## Table A10: formation vs. enlargement mixture model distributions
load("MM_SIGOEDentry_2021.Rdata")
model.assignments_entry <- attributes(result.theory)$posterior[[1]]
dat.entry <- entry_ally_avg.l$data[-entry_ally_avg.l$na.action,]

colMeans(model.assignments_entry[which(dat.entry$afterjoin==0),]) # all entry
colMeans(model.assignments_entry[which(dat.entry$formation==1),]) # formation
colMeans(model.assignments_entry[which(dat.entry$formation==0 & dat.entry$afterjoin==0),]) # enlargement


######################
## Appendix Figures ##
######################

## Figure(s) A1: entry, enlargement, formation samples with alternative measures
library(MASS)
MCsim <- function(fm){ # function for automating monte carlo simulation
  if(class(fm)=="Zelig-relogit"){
    dat <- model.frame(fm$model.call, fm$data)
    Sigma <- fm$get_vcov()[[1]]
  }
  if(class(fm)!="Zelig-relogit"){
    dat <- fm$data[-fm$na.action,]
    Sigma <- vcov.cl(dat, fm, dat$state)
  }
  beta.sim <- mvrnorm(1000, mu = coef(fm), Sigma = Sigma)
  cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(fm))[-1]])
  df1 <- df2 <- cov.sim
  df1[,names(coef(fm))[2]] <- mean(cov.sim[,names(coef(fm))[2]]) 
  df2[,names(coef(fm))[2]] <- mean(cov.sim[,names(coef(fm))[2]]) + 
    sd(cov.sim[,names(coef(fm))[2]]) 
  res <- apply(beta.sim, 1, function(x){
    pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
    pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
    return(mean(pred2 - pred1))
  })
  return(res)
}

full.ally.results <- MCsim(full_ally_avg)
entry.ally.results <- MCsim(entry_ally_avg)
form.ally.results <- MCsim(form_ally_avg)
expan.ally.results <- MCsim(expan_ally_avg)

full.S.results <- MCsim(full_S_avg)

SIGO.ED.entry <- SIGO.ED[which(SIGO.ED$afterjoin==0),] 
entry_S_avg <- zelig(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                     Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                     regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                     state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                     CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry, model="relogit")
entry.S.results <- MCsim(entry_S_avg)

form_S_avg <- glm(Member ~ S_score_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                    Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                    regional_org + #region_mems_lag_form + IGO_mems_lag_form + #fatalMIDs_count_lag_form + 
                    state_mems_lag + comcol + formercol + 
                    CW1 + t + t2 + t3, family = "binomial", data = Form)
form.S.results <- MCsim(form_S_avg)

expan_S_avg <- zelig(Member ~ S_score_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                     Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                     regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                     state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                     CW1 + t + t2 + t3, family = "binomial", data = Expan, model="relogit")
expan.S.results <- MCsim(expan_S_avg)

full.U.results <- MCsim(full_UNIP_avg)

entry_UNIP_avg <- zelig(Member ~ UN_IP_sim_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry, model="relogit")
entry.U.results <- MCsim(entry_UNIP_avg)

form_UNIP_avg <- glm(Member ~ UN_IP_sim_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + #region_mems_lag_form + IGO_mems_lag_form + #fatalMIDs_count_lag_form + 
                       state_mems_lag + comcol + formercol + 
                       CW1 + t + t2 + t3, family = "binomial", data = Form)
form.U.results <- MCsim(form_UNIP_avg)

expan_UNIP_avg <- zelig(Member ~ UN_IP_sim_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3, family = "binomial", data = Expan, model="relogit")
expan.U.results <- MCsim(expan_UNIP_avg)


par(xpd=FALSE)
par(mfrow=c(1,1))
plot(1:13, 1:13, cex=0, xlab="", ylab="Change in Pr(Membership)",
     ylim=c(-0.03, 0.13), xlim=c(1.5, 6.5), xaxt="n")
points(2.3, mean(full.ally.results), pch=19, cex=.75) 
lines(x=c(2.3,2.3), y=c(mean(full.ally.results) - sd(full.ally.results)*qnorm(0.975),
                        mean(full.ally.results) + sd(full.ally.results)*qnorm(0.975)))
points(2.75, mean(full.S.results), pch=19, col="red", cex=.75) 
lines(x=c(2.75,2.75), y=c(mean(full.S.results) - sd(full.S.results)*qnorm(0.975),
                          mean(full.S.results) + sd(full.S.results)*qnorm(0.975)), col="red")
points(3.2, mean(full.U.results), pch=19, col="blue", cex=.75) 
lines(x=c(3.2,3.2), y=c(mean(full.U.results) - sd(full.U.results)*qnorm(0.975),
                        mean(full.U.results) + sd(full.U.results)*qnorm(0.975)), col="blue")

points(4.8, mean(form.ally.results), pch=19, col="black", cex=.75)
lines(x=c(4.8,4.8), y=c(mean(form.ally.results) - sd(form.ally.results)*qnorm(0.975),
                        mean(form.ally.results) + sd(form.ally.results)*qnorm(0.975)), col="black")
points(5.25, mean(form.S.results), pch=19, col="red", cex=.75)
lines(x=c(5.25,5.25), y=c(mean(form.S.results) - sd(form.S.results)*qnorm(0.975),
                          mean(form.S.results) + sd(form.S.results)*qnorm(0.975)), col="red")
points(5.7, mean(form.U.results), pch=19, col="blue", cex=.75)
lines(x=c(5.7,5.7), y=c(mean(form.U.results) - sd(form.U.results)*qnorm(0.975),
                        mean(form.U.results) + sd(form.U.results)*qnorm(0.975)), col="blue")
abline(h=0, lty=2)
axis(side=1, at=c(2.75, 5.25),
     labels=c("Full", "IGO Formation"))
legend(pch=19, legend=c("Alliances", "S-Scores", "UN Ideal Point Distance"),
       x="topleft", col=c("black", "red", "blue"))


par(xpd=FALSE)
par(mfrow=c(1,1))
plot(1:13, 1:13, cex=0, xlab="", ylab="Change in Pr(Membership)",
     ylim=c(-0.001, 0.0127), xlim=c(1.5, 6.5), xaxt="n")
points(2.3, mean(entry.ally.results), pch=19, cex=.75) 
lines(x=c(2.3,2.3), y=c(mean(entry.ally.results) - sd(entry.ally.results)*qnorm(0.975),
                        mean(entry.ally.results) + sd(entry.ally.results)*qnorm(0.975)))
points(2.75, mean(entry.S.results), pch=19, col="red", cex=.75) 
lines(x=c(2.75,2.75), y=c(mean(entry.S.results) - sd(entry.S.results)*qnorm(0.975),
                          mean(entry.S.results) + sd(entry.S.results)*qnorm(0.975)), col="red")
points(3.2, mean(entry.U.results), pch=19, col="blue", cex=.75) 
lines(x=c(3.2,3.2), y=c(mean(entry.U.results) - sd(entry.U.results)*qnorm(0.975),
                        mean(entry.U.results) + sd(entry.U.results)*qnorm(0.975)), col="blue")

points(4.8, mean(expan.ally.results), pch=19, col="black", cex=.75)
lines(x=c(4.8,4.8), y=c(mean(expan.ally.results) - sd(expan.ally.results)*qnorm(0.975),
                        mean(expan.ally.results) + sd(expan.ally.results)*qnorm(0.975)), col="black")
points(5.25, mean(expan.S.results), pch=19, col="red", cex=.75)
lines(x=c(5.25,5.25), y=c(mean(expan.S.results) - sd(expan.S.results)*qnorm(0.975),
                          mean(expan.S.results) + sd(expan.S.results)*qnorm(0.975)), col="red")
points(5.7, mean(expan.U.results), pch=19, col="blue", cex=.75)
lines(x=c(5.7,5.7), y=c(mean(expan.U.results) - sd(expan.U.results)*qnorm(0.975),
                        mean(expan.U.results) + sd(expan.U.results)*qnorm(0.975)), col="blue")
abline(h=0, lty=2)
axis(side=1, at=c(2.75, 5.25),
     labels=c("Entry", "IGO Enlargement"))
legend(pch=19, legend=c("Alliances", "S-Scores", "UN Ideal Point Distance"),
       x="topleft", col=c("black", "red", "blue"))


## Figure A2: Effect of Alliances in Different Samples of Economic IGOs
library(MASS)
SIGO.EDS <- SIGO.ED[SIGO.ED$salient==1,]
full_ally_EconSalient <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                               Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                               regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                               state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                               CW1 + t + t2 + t3, family = "binomial", data = SIGO.EDS)
datEDS <- full_ally_EconSalient$data[-full_ally_EconSalient$na.action,] 
beta.sim <- mvrnorm(1000, mu = coef(full_ally_EconSalient),
                    Sigma = vcov.cl(datEDS, full_ally_EconSalient, datEDS$state))
cov.sim <- cbind(rep(1, nrow(datEDS)), datEDS[, names(coef(full_ally_EconSalient))[-1]])
df1 <- df2 <- cov.sim
df1$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) 
df2$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) + 
  sd(cov.sim$alliances_avgmembers_lag_form) 
ES.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})


SIGO.EDStr <- SIGO.ED[SIGO.ED$stringent_approval==1,]
full_ally_EconStringent <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                                 Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + #stringent_approval +
                                 regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                                 state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                                 CW1 + t + t2 + t3, family = "binomial", data = SIGO.EDStr)
datEDStr <- full_ally_EconStringent$data[-full_ally_EconStringent$na.action,] 
beta.sim <- mvrnorm(1000, mu = coef(full_ally_EconStringent),
                    Sigma = vcov.cl(datEDStr, full_ally_EconStringent, datEDStr$state))
cov.sim <- cbind(rep(1, nrow(datEDStr)), datEDStr[, names(coef(full_ally_EconStringent))[-1]])
df1 <- df2 <- cov.sim
df1$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) 
df2$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) + 
  sd(cov.sim$alliances_avgmembers_lag_form) 
EStr.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})


SIGO.EDreg <- SIGO.ED[SIGO.ED$regional_org==1,]
full_ally_Econreg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                           Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                           region_mems_lag_form + IGO_mems_lag_form + 
                           state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                           CW1 + t + t2 + t3, family = "binomial", data = SIGO.EDreg)
datEDr <- full_ally_Econreg$data[-full_ally_Econreg$na.action,] 
beta.sim <- mvrnorm(1000, mu = coef(full_ally_Econreg),
                    Sigma = vcov.cl(datEDr, full_ally_Econreg, datEDr$state))
cov.sim <- cbind(rep(1, nrow(datEDr)), datEDr[, names(coef(full_ally_Econreg))[-1]])
df1 <- df2 <- cov.sim
df1$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) 
df2$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) + 
  sd(cov.sim$alliances_avgmembers_lag_form) 
EDreg.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})


SIGO.EDnreg <- SIGO.ED[SIGO.ED$regional_org==0,]
full_ally_Econnreg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                            Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                            region_mems_lag_form + IGO_mems_lag_form + 
                            state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                            CW1 + t + t2 + t3, family = "binomial", data = SIGO.EDnreg)
datEDnr <- full_ally_Econnreg$data[-full_ally_Econnreg$na.action,] 
beta.sim <- mvrnorm(1000, mu = coef(full_ally_Econnreg),
                    Sigma = vcov.cl(datEDnr, full_ally_Econnreg, datEDnr$state))
cov.sim <- cbind(rep(1, nrow(datEDnr)), datEDnr[, names(coef(full_ally_Econnreg))[-1]])
df1 <- df2 <- cov.sim
df1$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) 
df2$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) + 
  sd(cov.sim$alliances_avgmembers_lag_form) 
EDnreg.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})

par(mfrow=c(1,1))
plot(1:7, 1:7, cex=0, xlab="", ylab="Change in Pr(Membership)",
     ylim=c(-0.008, 0.13), xlim=c(1.85, 6.65), xaxt="n",
     main="Effect of Geopolitical Alignment on Probability of IGO Membership")
points(2.25, mean(full.ally.results), pch=19) # All Econ
lines(x=c(2.25,2.25), y=c(mean(full.ally.results) - sd(full.ally.results)*qnorm(0.975),
                          mean(full.ally.results) + sd(full.ally.results)*qnorm(0.975)))
points(3.25, mean(ES.results), pch=19) # Econ Salient
lines(x=c(3.25,3.25), y=c(mean(ES.results) - sd(ES.results)*qnorm(0.975),
                          mean(ES.results) + sd(ES.results)*qnorm(0.975)))
points(4.25, mean(EStr.results), pch=19) # Stringent Econ 
lines(x=c(4.25,4.25), y=c(mean(EStr.results) - sd(EStr.results)*qnorm(0.975),
                          mean(EStr.results) + sd(EStr.results)*qnorm(0.975)))
points(5.25, mean(EDreg.results), pch=19) # Regional Econ 
lines(x=c(5.25,5.25), y=c(mean(EDreg.results) - sd(EDreg.results)*qnorm(0.975),
                          mean(EDreg.results) + sd(EDreg.results)*qnorm(0.975)))
points(6.25, mean(EDnreg.results), pch=19) # Non-Regional Econ 
lines(x=c(6.25,6.25), y=c(mean(EDnreg.results) - sd(EDnreg.results)*qnorm(0.975),
                          mean(EDnreg.results) + sd(EDnreg.results)*qnorm(0.975)))
abline(h=0, lty=2)
axis(side=1, at=c(2.25, 3.25, 4.25, 5.25, 6.25),
     labels=c("All Econ \n (231)", "Econ Salient \n (79)", "Stringent \n Accession Econ \n (75)", 
              "Regional \n Econ  \n (154)", "Non-regional \n Econ \n (77)"),
     cex=0.9, tick=FALSE, pos = -0.019)


## Figure A3: Effect of Alliances in Different Issue Areas

SIGO.D <- SIGO[SIGO$eligible==1,]

## Security only IGOs
SIGO.sec <- SIGO.D[which(SIGO.D$Security_IGO==1 & SIGO.D$Econ_IGO==0 & 
                           SIGO.D$Social_IGO==0 & SIGO.D$Environ_IGO==0),]
sec_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                      CW1 + t + t2 + t3,
                    family = "binomial", data = SIGO.sec)
datS <- sec_ally_avg$data[-sec_ally_avg$na.action,] 
length(unique(datS$IGO)) # 8 IGOs in the sample
library(MASS)
beta.sim <- mvrnorm(1000, mu = coef(sec_ally_avg), Sigma = vcov.cl(datS, sec_ally_avg, datS$state))
cov.sim <- cbind(rep(1, nrow(datS)), datS[, names(coef(sec_ally_avg))[-1]])
df1 <- df2 <- cov.sim
df1$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) 
df2$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) + 
  sd(cov.sim$alliances_avgmembers_lag_form)
sec.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})

## Economic only IGOs
SIGO.econonly <- SIGO.D[which(SIGO.D$Security_IGO==0 & SIGO.D$Econ_IGO==1 & 
                                SIGO.D$Social_IGO==0 & SIGO.D$Environ_IGO==0),]
EO_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                     Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                     regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                     state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                     CW1 + t + t2 + t3,
                   family = "binomial", data = SIGO.econonly)
datEO <- EO_ally_avg$data[-EO_ally_avg$na.action,] 
length(unique(datEO$IGO)) # 144 IGOs in the sample
beta.sim <- mvrnorm(1000, mu = coef(EO_ally_avg), Sigma = vcov.cl(datEO, EO_ally_avg, datEO$state))
cov.sim <- cbind(rep(1, nrow(datEO)), datEO[, names(coef(EO_ally_avg))[-1]])
df1 <- df2 <- cov.sim
df1$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) 
df2$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) + 
  sd(cov.sim$alliances_avgmembers_lag_form)
EO.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})


## Environmental IGOs
SIGO.environ <- SIGO.D[which(SIGO.D$Security_IGO==0 & SIGO.D$Econ_IGO==0 & 
                               SIGO.D$Social_IGO==0 & SIGO.D$Environ_IGO==1),]
env_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                      CW1 + t + t2 + t3,
                    family = "binomial", data = SIGO.environ)
datEn <- env_ally_avg$data[-env_ally_avg$na.action,] 
length(unique(datEn$IGO)) # 9 IGOs in the sample
beta.sim <- mvrnorm(1000, mu = coef(env_ally_avg), Sigma = vcov.cl(datEn, env_ally_avg, datEn$state))
cov.sim <- cbind(rep(1, nrow(datEn)), datEn[, names(coef(env_ally_avg))[-1]])
df1 <- df2 <- cov.sim
df1$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) 
df2$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) + 
  sd(cov.sim$alliances_avgmembers_lag_form)
Env.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})


par(mfrow=c(1,1))
plot(1:6, 1:6, cex=0, xlab="", ylab="Change in Pr(Membership)",
     ylim=c(-0.008, 0.17), xlim=c(1.85, 5.65), xaxt="n",
     main="Effect of Geopolitical Alignment on Probability of IGO Membership")
points(2.25, mean(sec.results), pch=19) # security only
lines(x=c(2.25,2.25), y=c(mean(sec.results) - sd(sec.results)*qnorm(0.975),
                          mean(sec.results) + sd(sec.results)*qnorm(0.975)))
points(3.25, mean(full.ally.results), pch=19) # econ broad
lines(x=c(3.25,3.25), y=c(mean(full.ally.results) - sd(full.ally.results)*qnorm(0.975),
                          mean(full.ally.results) + sd(full.ally.results)*qnorm(0.975)))
points(4.25, mean(EO.results), pch=19) # econ narrow
lines(x=c(4.25,4.25), y=c(mean(EO.results) - sd(EO.results)*qnorm(0.975),
                          mean(EO.results) + sd(EO.results)*qnorm(0.975)))
points(5.25, mean(Env.results), pch=19) # environment narrow
lines(x=c(5.25,5.25), y=c(mean(Env.results) - sd(Env.results)*qnorm(0.975),
                          mean(Env.results) + sd(Env.results)*qnorm(0.975)))
abline(h=0, lty=2)
axis(side=1, at=c(2.25, 3.25, 4.25, 5.25),
     labels=c("Security Narrow \n (8)", "Econ Narrow \n (144)", 
              "All Econ \n (231)", "Environment Narrow \n (9)"),
     cex=0.9, tick=FALSE)



## Figure A4: Sorting of Observations into Geopolitical Model 
load("MM_SIGOED_2019.Rdata")
attributes(result.theory)$components # model 1 is alliances, model 2 is trade
model.assignments <- attributes(result.theory)$posterior[[1]]
hist(model.assignments[,1], 
     main="Sorting of Observations into Geopolitical Model",
     xlab="")


## Figures A5-A9: Country-Specific Sorting of Observations 
dat <- full_ally_avg$data[-full_ally_avg$na.action,]

Russia <- tapply(model.assignments[dat$state=="Russia",1], dat$year[dat$state=="Russia"], mean)
plot(as.numeric(names(Russia)), Russia, type="o", ylim=c(0.15, 0.75),
     main="Share of Observations Assigned to Geopolitical Model by Year, Russia",
     ylab="% Geopolitical Observations", xlab="")

USA <- tapply(model.assignments[dat$state=="USA",1], dat$year[dat$state=="USA"], mean)
plot(unique(dat$year), USA, type="o", ylim=c(0.15, 0.75),
     main="Share of Observations Assigned to Geopolitical Model by Year, USA",
     ylab="% Geopolitical Observations", xlab="")

China <- tapply(model.assignments[dat$state=="China",1], dat$year[dat$state=="China"], mean)
plot(unique(dat$year[dat$state=="China"]), China, type="o", ylim=c(0.15, 0.75),
     main="Share of Observations Assigned to Geopolitical Model by Year, China",
     ylab="% Geopolitical Observations", xlab="")

Iran <- tapply(model.assignments[dat$state=="Iran",1], dat$year[dat$state=="Iran"], mean)
plot(unique(dat$year[dat$state=="Iran"]), Iran, type="o", ylim=c(0.15, 0.75),
     main="Share of Observations Assigned to Geopolitical Model by Year, Iran",
     ylab="% Geopolitical Observations", xlab="")

Japan <- tapply(model.assignments[dat$state=="Japan",1], dat$year[dat$state=="Japan"], mean)
plot(unique(dat$year[dat$state=="Japan"]), Japan, type="o", ylim=c(0.15, 0.75),
     main="Share of Observations Assigned to Geopolitical Model by Year, Japan",
     ylab="% Geopolitical Observations", xlab="")


###################################
## Appendix C: IGO Entry Results ##
###################################

SIGO.ED.entry <- SIGO.ED[which(SIGO.ED$afterjoin==0),] 



# Figure A10

dat <- model.frame(entry_ally_avg$model.call, entry_ally_avg$data)
beta.sim <- mvrnorm(1000, mu = coef(entry_ally_avg), Sigma = entry_ally_avg$get_vcov()[[1]])
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(entry_ally_avg))[-1]])
df1 <- df2 <- cov.sim
df1$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) 
df2$alliances_avgmembers_lag_form <- mean(cov.sim$alliances_avgmembers_lag_form) + 
  sd(cov.sim$alliances_avgmembers_lag_form) # increase by 1 sd
entry.ally.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})

entry_ally_LS <- zelig(Member ~ ally_leadstate.dynamic_lag_form + trade_leadstate.dynamic.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3,
                     family = "binomial", data = SIGO.ED.entry, model="relogit")
dat <-  model.frame(entry_ally_LS$model.call, entry_ally_LS$data)
beta.sim <- mvrnorm(1000, mu = coef(entry_ally_LS), Sigma = entry_ally_LS$get_vcov()[[1]])
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(entry_ally_LS))[-1]])
df1 <- df2 <- cov.sim
df1$ally_leadstate.dynamic_lag_form <- 0 
df2$ally_leadstate.dynamic_lag_form <- 1 
entry.allyLS.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})

dat <- model.frame(entry_ally_avg$model.call, entry_ally_avg$data)
beta.sim <- mvrnorm(1000, mu = coef(entry_ally_avg), Sigma = entry_ally_avg$get_vcov()[[1]])
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(entry_ally_avg))[-1]])
df1 <- df2 <- cov.sim
df1$trade_avgmembers.l_lag_form <- mean(cov.sim$trade_avgmembers.l_lag_form) 
df2$trade_avgmembers.l_lag_form <- mean(cov.sim$trade_avgmembers.l_lag_form) + 
  sd(cov.sim$trade_avgmembers.l_lag_form) 
entry.trade.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})

dat <-  model.frame(entry_ally_LS$model.call, entry_ally_LS$data)
beta.sim <- mvrnorm(1000, mu = coef(entry_ally_LS), Sigma = entry_ally_LS$get_vcov()[[1]])
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(entry_ally_LS))[-1]])
df1 <- df2 <- cov.sim
df1$trade_leadstate.dynamic.l_lag_form <- mean(cov.sim$trade_leadstate.dynamic.l_lag_form) 
df2$trade_leadstate.dynamic.l_lag_form <- mean(cov.sim$trade_leadstate.dynamic.l_lag_form) + 
  sd(cov.sim$trade_leadstate.dynamic.l_lag_form) 
entry.tradeLS.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})


entry_S_avg <- zelig(Member ~ S_score_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                     Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                     regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                     state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                     CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry, model="relogit")
dat <-  model.frame(entry_S_avg$model.call, entry_S_avg$data)
beta.sim <- mvrnorm(1000, mu = coef(entry_S_avg), Sigma = entry_S_avg$get_vcov()[[1]])
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(entry_S_avg))[-1]])
df1 <- df2 <- cov.sim
df1$S_score_avgmembers_lag_form <- mean(cov.sim$S_score_avgmembers_lag_form) 
df2$S_score_avgmembers_lag_form <- mean(cov.sim$S_score_avgmembers_lag_form) + 
  sd(cov.sim$S_score_avgmembers_lag_form) 
entry.S.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})


entry_S_LS <- zelig(Member ~ S_score_leadstate.dynamic_lag_form + trade_leadstate.dynamic.l_lag_form + 
                    Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                    regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                    state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                    CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry, model="relogit")
dat <-  model.frame(entry_S_LS$model.call, entry_S_LS$data)
beta.sim <- mvrnorm(1000, mu = coef(entry_S_LS), Sigma = entry_S_LS$get_vcov()[[1]])
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(entry_S_LS))[-1]])
df1 <- df2 <- cov.sim
df1$S_score_leadstate.dynamic_lag_form <- mean(cov.sim$S_score_leadstate.dynamic_lag_form) 
df2$S_score_leadstate.dynamic_lag_form <- mean(cov.sim$S_score_leadstate.dynamic_lag_form) + 
  sd(cov.sim$S_score_leadstate.dynamic_lag_form) 
entry.SLS.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})


entry_UNIP_avg <- zelig(Member ~ UN_IP_sim_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry, model="relogit")
dat <-  model.frame(entry_UNIP_avg$model.call, entry_UNIP_avg$data)
beta.sim <- mvrnorm(1000, mu = coef(entry_UNIP_avg), Sigma = entry_UNIP_avg$get_vcov()[[1]])
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(entry_UNIP_avg))[-1]])
df1 <- df2 <- cov.sim
df1$UN_IP_sim_avgmembers_lag_form <- mean(cov.sim$UN_IP_sim_avgmembers_lag_form) 
df2$UN_IP_sim_avgmembers_lag_form <- mean(cov.sim$UN_IP_sim_avgmembers_lag_form) + 
  sd(cov.sim$UN_IP_sim_avgmembers_lag_form) 
entry.UNIP.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})


entry_UNIP_LS <- zelig(Member ~ UN_IP_sim_leadstate.dynamic_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry, model="relogit")
dat <-  model.frame(entry_UNIP_LS$model.call, entry_UNIP_LS$data)
beta.sim <- mvrnorm(1000, mu = coef(entry_UNIP_LS), Sigma = entry_UNIP_LS$get_vcov()[[1]])
cov.sim <- cbind(rep(1, nrow(dat)), dat[, names(coef(entry_UNIP_LS))[-1]])
df1 <- df2 <- cov.sim
df1$UN_IP_sim_leadstate.dynamic_lag_form <- mean(cov.sim$UN_IP_sim_leadstate.dynamic_lag_form) 
df2$UN_IP_sim_leadstate.dynamic_lag_form <- mean(cov.sim$UN_IP_sim_leadstate.dynamic_lag_form) + 
  sd(cov.sim$UN_IP_sim_leadstate.dynamic_lag_form) 
entry.UNIPLS.results <- apply(beta.sim, 1, function(x){
  pred1 <- exp(as.matrix(df1) %*% as.matrix(x)) / (1 + exp(as.matrix(df1) %*% as.matrix(x)))
  pred2 <- exp(as.matrix(df2) %*% as.matrix(x)) / (1 + exp(as.matrix(df2) %*% as.matrix(x)))
  return(mean(pred2 - pred1))
})

par(mfrow=c(1,1))
plot(1:13, 1:13, cex=0, xlab="", ylab="Change in Pr(Entry)",
     ylim=c(-0.0025, 0.0125), xlim=c(1.5, 12.5), xaxt="n",
     main="Effect of Geopolitical Alignment on Probability of IGO Entry")
points(2.25, mean(entry.ally.results), pch=19) # ally avg
lines(x=c(2.25,2.25), y=c(mean(entry.ally.results) - sd(entry.ally.results)*qnorm(0.975),
                          mean(entry.ally.results) + sd(entry.ally.results)*qnorm(0.975)))
points(2.75, mean(entry.allyLS.results)) # ally ls
lines(x=c(2.75,2.75), y=c(mean(entry.allyLS.results) - sd(entry.allyLS.results)*qnorm(0.975),
                          mean(entry.allyLS.results) + sd(entry.allyLS.results)*qnorm(0.975)))
points(5.25, mean(entry.S.results), pch=19) # S-score avg
lines(x=c(5.25,5.25), y=c(mean(entry.S.results) - sd(entry.S.results)*qnorm(0.975),
                          mean(entry.S.results) + sd(entry.S.results)*qnorm(0.975)))
points(5.75, mean(entry.SLS.results)) # S-score ls
lines(x=c(5.75,5.75), y=c( mean(entry.SLS.results) -  sd(entry.SLS.results)*qnorm(0.975),
                           mean(entry.SLS.results) +  sd(entry.SLS.results)*qnorm(0.975)))
points(8.25, mean(entry.UNIP.results), pch=19) # UNIP avg
lines(x=c(8.25,8.25), y=c(mean(entry.UNIP.results) - sd(entry.UNIP.results)*qnorm(0.975),
                          mean(entry.UNIP.results) + sd(entry.UNIP.results)*qnorm(0.975)))
points(8.75, mean(entry.UNIPLS.results)) # UNIP ls
lines(x=c(8.75,8.75), y=c(mean(entry.UNIPLS.results) - sd(entry.UNIPLS.results)*qnorm(0.975),
                          mean(entry.UNIPLS.results) + sd(entry.UNIPLS.results)*qnorm(0.975)))
points(11.25, mean(entry.trade.results), pch=19) # trade avg
lines(x=c(11.25,11.25), y=c(mean(entry.trade.results) - sd(entry.trade.results)*qnorm(0.975),
                            mean(entry.trade.results) + sd(entry.trade.results)*qnorm(0.975)))
points(11.75, mean(entry.tradeLS.results)) # trade ls
lines(x=c(11.75,11.75), y=c(mean(entry.tradeLS.results) - sd(entry.tradeLS.results)*qnorm(0.975),
                            mean(entry.tradeLS.results) + sd(entry.tradeLS.results)*qnorm(0.975)))
abline(h=0, lty=2)
axis(side=1, at=c(2.5, 5.5, 8.5, 11.5),
     labels=c("Alliances", "S-score", "Ideal Point Similarity", "Trade"))
legend(pch=c(19, 1), legend=c("Average Measure", "Lead State Measure"),
       x="topleft")


## Table A11

load("MM_SIGOEDentry_2021.Rdata") # See Forces_Replication_MM.R for estimation code
load("MM_SIGOEDentryLS_2021.Rdata") 

attributes(result.theory)$components # model 1 is alliances, model 2 is trade
model.assignments <- attributes(result.theory)$posterior[[1]]
colMeans(model.assignments) 

dat <- entry_ally_avg.l$data[-entry_ally_avg.l$na.action,] 
model1 <- glm(Member ~ alliances_avgmembers_lag_form + 
                Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                CW1 + t + t2 + t3, 
              family = "binomial", data = dat, weights = model.assignments[,1])
m1.se <- cl(dat, model1, as.character(dat$state))

model2 <- glm(Member ~ trade_avgmembers.l_lag_form + 
                Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                CW1 + t + t2 + t3, 
              family = "binomial", data = dat, weights = model.assignments[,2], maxit=100)
m2.se <- cl(dat, model2, as.character(dat$state))

entrycoef <- entry_ally_avg.l
entrycoef$coefficients <- from_zelig_model(entry_ally_avg)[[1]]

stargazer(model1, model2, entrycoef,
          se=list(m1.se[,2], m2.se[,2], unlist(get_se(entry_ally_avg))),
          keep = c("alliances_avgmembers_lag_form", "trade_avgmembers.l_lag_form",
                   "Polity_lag", "GDP_lag", "GDPpc_lag",  "trade.openness2_lag", 
                   "stringent_approval", "regional_org", "fatalMIDs_count_lag_form",
                   "region_mems_lag_form", "CW1"),
          covariate.labels = c("Average Alliances", "Trade with Members",
                               "Polity", "GDP", "GDP per capita", 
                               "Trade Openness", "Stringent Accession","Regional IGO",
                               "Existing Members from Region","Fatal MIDs with Members", 
                               "Cold War"), 
          column.labels = c("Geopolitical Model", "Economic Model", "Pooled Model"),
          no.space = TRUE, keep.stat = c("n"), column.sep.width = "-9 pt",
          align=TRUE, title="IGO Membership: Geopolitical vs. Economic Models") 


## Figure A11

yr.geop <- tapply(model.assignments[,1], dat$year, mean)
plot(unique(dat$year), yr.geop, type="o", ylim=c(0.35, 0.6),
     main="Share of IGO Entry Observations Assigned to Geopolitical Model by Year",
     ylab="% Observations Assigned to Geopolitical Model", xlab="")


## Figure A12

dat$democstate <- ifelse(dat$Polity_lag >= 7, 1, 0)
avg.b <- cbind(prop.table(apply(model.assignments, 2, function(x){sum(x[dat$CW1==1])})),
               prop.table(apply(model.assignments, 2, function(x){sum(x[dat$stringent_approval==1])})),
               prop.table(apply(model.assignments, 2, function(x){sum(x[dat$regional_org==1])})),
               prop.table(apply(model.assignments, 2, function(x){sum(x[dat$democstate==1])})))

prop.table(apply(model.assignments, 2, function(x){sum(x[dat$leadstate=="USA"])}))
prop.table(apply(model.assignments, 2, function(x){sum(x[dat$leadstate!="USA"])}))

datLS <- entry_ally_LS$data[-entry_ally_LS$na.action,]
model.assignments.ls <- attributes(resultLS.theory)$posterior[[1]]
datLS$democstate <- ifelse(datLS$Polity_lag >= 7, 1, 0)
LS.b <- cbind(prop.table(apply(model.assignments.ls, 2, function(x){sum(x[datLS$CW1==1])})),
              prop.table(apply(model.assignments.ls, 2, function(x){sum(x[datLS$stringent_approval==1])})),
              prop.table(apply(model.assignments.ls, 2, function(x){sum(x[datLS$regional_org==1])})),
              prop.table(apply(model.assignments.ls, 2, function(x){sum(x[datLS$democstate==1])})))

par(mfrow=c(1,2))
barplot(as.matrix(avg.b), 
        main = "Ties with Average Member",
        ylim=c(0, 1.15), ylab = "% Observations Assigned to Models",
        legend.text=c("Geopolitical Model", "Economic Model"),
        axisnames=TRUE, names.arg = c("Cold \n War", "Stringent \n Accession", "Regional \n IGO", "Democratic \n State"))
barplot(as.matrix(LS.b),
        main = "Ties with Lead State",
        ylim=c(0, 1.15), ylab = "% Observations Assigned to Models",
        legend.text=c("Geopolitical Model", "Economic Model"),
        axisnames=TRUE, names.arg = c("Cold \n War", "Stringent \n Accession", "Regional \n IGO", "Democratic \n State"))
