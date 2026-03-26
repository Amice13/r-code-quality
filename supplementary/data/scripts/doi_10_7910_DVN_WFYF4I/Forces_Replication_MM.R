###########################################
## Replication File for Davis and Pratt, ##
## "The Forces of Attraction" #############
## Finite Mixture Model Estimation ########
## July 2020  #############################
###########################################

## Estimation takes a long time (8-10 hours).  Fitted models have been saved for loading.

library(data.table)
SIGO <- as.data.frame(fread("SIGO.csv"))
SIGO.E <- SIGO[SIGO$Econ_IGO==1,] 
SIGO.ED <- SIGO.E[SIGO.E$eligible==1,]

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

# Average alliance model
full_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + 
                       stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED)
dat <- full_ally_avg$data[-full_ally_avg$na.action,] 

library(flexmix)
MMdat <- dat[, colnames(dat) %in% c("Member", names(coef(full_ally_avg)))]

model.t <- FLXMRglmfix(family = "binomial", nested = list(k = c(1, 1),
                                                          formula = c(~ alliances_avgmembers_lag_form + 
                                                                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                                                                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                                                                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                                                                        CW1 + t + t2 + t3,
                                                                      ~ trade_avgmembers.l_lag_form + 
                                                                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                                                                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                                                                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                                                                        CW1 + t + t2 + t3)))

result.theory <- stepFlexmix(cbind(Member, 1 - Member) ~ 1, k = 2, model = model.t,
                             concomitant = FLXPmultinom(~ CW1 + Polity_lag + stringent_approval), 
                             data = MMdat, nrep = 15)

## Lead State alliance model
full_ally_LS <- glm(Member ~ ally_leadstate.dynamic_lag_form + trade_leadstate.dynamic.l_lag_form + 
                      Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                      regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                      state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                      CW1 + t + t2 + t3,
                    family = "binomial", data = SIGO.ED)
datLS <- full_ally_LS$data[-full_ally_LS$na.action,]
MMdat <- datLS[, colnames(datLS) %in% c("Member", names(coef(full_ally_LS)))]

model.tLS <- FLXMRglmfix(family = "binomial", nested = list(k = c(1, 1),
                                                            formula = c(~ ally_leadstate.dynamic_lag_form + 
                                                                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                                                                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                                                                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                                                                          CW1 + t + t2 + t3,
                                                                        ~ trade_leadstate.dynamic.l_lag_form +  
                                                                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                                                                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                                                                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                                                                          CW1 + t + t2 + t3)))

resultLS.theory <- stepFlexmix(cbind(Member, 1 - Member) ~ 1, k = 2, model = model.tLS,
                               concomitant = FLXPmultinom(~ CW1 + Polity_lag + stringent_approval), 
                               data = MMdat, nrep = 5)

save(result.theory, resultLS.theory, file="MM_SIGOED_2019.Rdata")


## Repeat with entry-only observations

SIGO.ED.entry <- SIGO.ED[which(SIGO.ED$afterjoin==0),] 
entry_ally_avg <- glm(Member ~ alliances_avgmembers_lag_form + trade_avgmembers.l_lag_form + 
                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                        CW1 + t + t2 + t3, family = "binomial", data = SIGO.ED.entry)
entry_ally_LS <- glm(Member ~ ally_leadstate.dynamic_lag_form + trade_leadstate.dynamic.l_lag_form + 
                       Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                       regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                       state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                       CW1 + t + t2 + t3,, family = "binomial", data = SIGO.ED.entry)
datLS <- entry_ally_LS$data[-entry_ally_LS$na.action,]
MMdatLS <- datLS[, colnames(datLS) %in% c("Member", names(coef(entry_ally_LS)))]

dat <- entry_ally_avg$data[-entry_ally_avg$na.action,] 
MMdat <- dat[, colnames(dat) %in% c("Member", names(coef(entry_ally_avg)))]

model.t <- FLXMRglmfix(family = "binomial", nested = list(k = c(1, 1),
                                                          formula = c(~ alliances_avgmembers_lag_form + 
                                                                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                                                                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                                                                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                                                                        CW1 + t + t2 + t3,
                                                                      ~ trade_avgmembers.l_lag_form + 
                                                                        Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                                                                        regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                                                                        state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                                                                        CW1 + t + t2 + t3)))

result.theory <- stepFlexmix(cbind(Member, 1 - Member) ~ 1, k = 2, model = model.t,
                             concomitant = FLXPmultinom(~ CW1 + Polity_lag + stringent_approval), 
                             data = MMdat, nrep = 1,
                             control=list(iter.max=1500))
save(result.theory, file="~/Dropbox/IGO_Membership/Results/MM_SIGOEDentry_2020.Rdata")

model.tLS <- FLXMRglmfix(family = "binomial", nested = list(k = c(1, 1),
                                                            formula = c(~ ally_leadstate.dynamic_lag_form + 
                                                                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                                                                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                                                                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                                                                          CW1 + t + t2 + t3,
                                                                        ~ trade_leadstate.dynamic.l_lag_form +  
                                                                          Polity_lag + GDP_lag + GDPpc_lag + trade.openness2_lag + stringent_approval +
                                                                          regional_org + region_mems_lag_form + IGO_mems_lag_form + 
                                                                          state_mems_lag + fatalMIDs_count_lag_form + comcol + formercol + 
                                                                          CW1 + t + t2 + t3)))

resultLS.theory <- stepFlexmix(cbind(Member, 1 - Member) ~ 1, k = 2, model = model.tLS,
                               concomitant = FLXPmultinom(~ CW1 + Polity_lag + stringent_approval), 
                               data = MMdatLS, nrep = 1,
                               control=list(iter.max=1500))
save(resultLS.theory, file="~/Dropbox/IGO_Membership/Results/MM_SIGOEDentryLS_2020.Rdata")

