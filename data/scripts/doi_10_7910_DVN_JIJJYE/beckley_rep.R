
# --- Libraries --- #
library(readr)
library(MASS)
set.seed(1912)

setwd("YOUR WORKING DIRECTORY")



# --- APPLICATION ONE: AIC RE-ANALYSIS --- #

# --- Table 3 from Appendix:

aic_table <- as.data.frame(read_csv("data/beckley_aic_table.csv")) # read in papers w/ AICs from CSV file



# --- Pairwise AIC differences reported in the paper (pp. 39-40):

aic_table$cinc_diffs <- aic_table$`CINC AIC` - aic_table$`GDPxGDPPC AIC` # positive differences imply GDPxGDPPC better than CINC
aic_table$gdp_diffs <- aic_table$`GDP AIC` - aic_table$`GDPxGDPPC AIC` # positive differences imply GDPxGDPPC better than GDP

length(which(aic_table$cinc_diffs > 0)) # lowest vs. CINC scores 17/24 times, as reported in paper
length(which(aic_table$gdp_diffs > 0))  # lowest vs. GDP 11/24 times, as reported in paper



# --- Number of times GDPxGDPPC lower than both CINC and GDP:

length(which(aic_table$cinc_diffs > 0 & aic_table$gdp_diffs > 0)) # lowest vs. GDP _and_ CINC Scores 10/24 times

# --> implies that over half of the time, one of the two existing variables will suffice



# --- Number of times GDPxGDPPC lower than both CINC and GDP _and_ meets some threshold of significance:

# Difference of delta > 2 is relatively traditional (see Burnham & Anderson 2002: 70-71)

length(which(aic_table$cinc_diffs > 2 & aic_table$gdp_diffs > 2)) # 6 cases where GDPxGDPPC exhibits significant improvements over both existing variables















# --- APPLICATION TWO: MODEL REPLICATIONS--- #

# --- Replications that used first order effect:

papers <- c("Allen and DiGiuseppe (2013)", "Colgan and Weeks (2015)", "Grauer and Horowitz (2012)",
            "Horowitz and Stam (2014)", "Narang and Talmadge (2017)")

rep_sub <- subset(aic_table, Paper %in% papers)



# --- Import Beckley power data 

power2 <- read_delim("data/power2.tab", "\t", escape_double = FALSE, trim_ws = TRUE) # Beckley power measurement



# --- Add a square root term to approximate first order effects

keeps <- c("ccode", "year", "gdp", "y") # NB: "y" = "GDPxGDPPC"

power_sub <- power2[,colnames(power2) %in% keeps] # keep only vars we need 

power_sub$y_root <- sqrt(power_sub$y) # add a square root of this variable to illustrate impact of linear effect inclusion

power_sub$pop <- 1/(power_sub$y/power_sub$gdp^2) # transformation to calculate population

power_sub$gdpsquared <- power_sub$gdp^2

rep_list <- list() # empty list to store replication results




# --- Allen and DiGiuseppe (2013):
set.seed(1912)
Allen_DiGi_Rep <- read_delim("data/Allen_DiGi_Rep.tab", "\t", escape_double = FALSE, trim_ws = TRUE)

power_sub_digi <- subset(power_sub, year %in% unique(Allen_DiGi_Rep$year))

digi_merge <- merge(Allen_DiGi_Rep, power_sub_digi, by = c("ccode", "year"))

rep_1 <- glm(formationB2NNA ~ cntgovtdbtgdp1 + IMR + cinc + sumrival1 + wovers_1 +
               atwar5 + ayr + ayr2 + ayr3, family=binomial(link='probit'),
             data=digi_merge) # cinc

rep_2 <- glm(formationB2NNA ~ cntgovtdbtgdp1 + IMR + y + sumrival1 + wovers_1 +
               atwar5 + ayr + ayr2 + ayr3, family=binomial(link='probit'),
             data=digi_merge) # new measure

rep_3 <- glm(formationB2NNA ~ cntgovtdbtgdp1 + IMR + gdp + sumrival1 + wovers_1 +
               atwar5 + ayr + ayr2 + ayr3, family=binomial(link='probit'),
             data=digi_merge) # gdp

rep_4 <- glm(formationB2NNA ~ cntgovtdbtgdp1 + IMR + y_root + y + sumrival1 + wovers_1 +
               atwar5 + ayr + ayr2 + ayr3, family=binomial(link='probit'),
             data=digi_merge) # sqr root + new measure

rep_5 <- glm(formationB2NNA ~ cntgovtdbtgdp1 + IMR + pop + gdp + gdpsquared + sumrival1 + wovers_1 +
               atwar5 + ayr + ayr2 + ayr3, family=binomial(link='probit'),
             data=digi_merge) # pop + gdp + gdp^2

rep_list[["ad"]] <- list(rep_1, rep_2, rep_3, rep_4, rep_5)




# --- Colgan and Weeks (2015):
Colgan <- read_delim("data/Colgan.tab", "\t", escape_double = FALSE, trim_ws = TRUE)

power_sub_cw <- subset(power_sub, year %in% unique(Colgan$year))

cw_merge <- merge(Colgan, power_sub_cw, by = c("ccode", "year"))

cw_merge$y_lag <- lag(cw_merge$y, 1)
cw_merge$y_root_lag <- lag(cw_merge$y_root, 1)
cw_merge$gdp_lag <- lag(cw_merge$gdp, 1)
cw_merge$pop_lag <- lag(cw_merge$pop, 1)
cw_merge$gdpsquared_lag <- lag(cw_merge$gdpsquared, 1)

rep_1 <- glm.nb(initmid ~ revpers10_lag + rev10_lag + pers10_lag + logopengled_lag + total_lag + cap_lag + 
                  totalally_lag + civilwar_lag + majpow_lag + pcyrsinitmid + pcyrsinitmids1 +
                  pcyrsinitmids2 + pcyrsinitmids3,
                data = cw_merge) # cinc

rep_2 <- glm.nb(initmid ~ revpers10_lag + rev10_lag + pers10_lag + logopengled_lag + total_lag + y_lag + 
                  totalally_lag + civilwar_lag + majpow_lag + pcyrsinitmid + pcyrsinitmids1 +
                  pcyrsinitmids2 + pcyrsinitmids3,
                data = cw_merge) # new measure

rep_3 <- glm.nb(initmid ~ revpers10_lag + rev10_lag + pers10_lag + logopengled_lag + total_lag + gdp_lag + 
                  totalally_lag + civilwar_lag + majpow_lag + pcyrsinitmid + pcyrsinitmids1 +
                  pcyrsinitmids2 + pcyrsinitmids3,
                data = cw_merge) # gdp

rep_4 <- glm.nb(initmid ~ revpers10_lag + rev10_lag + pers10_lag + logopengled_lag + total_lag + y_root_lag + y_lag + 
                  totalally_lag + civilwar_lag + majpow_lag + pcyrsinitmid + pcyrsinitmids1 +
                  pcyrsinitmids2 + pcyrsinitmids3,
                data = cw_merge) # sqr root + new measure

rep_5 <- glm.nb(initmid ~ revpers10_lag + rev10_lag + pers10_lag + logopengled_lag + total_lag + pop_lag + 
                  gdp_lag + gdpsquared_lag + totalally_lag + civilwar_lag + majpow_lag + pcyrsinitmid + pcyrsinitmids1 +
                  pcyrsinitmids2 + pcyrsinitmids3,
                data = cw_merge) # pop + gdp + gdp^2

rep_list[["cw"]] <- list(rep_1, rep_2, rep_3, rep_4, rep_5)




# --- Grauer and Horowitz (2012):

GH_SS_Replication <- read_delim("data/GH_SS_Replication.tab", "\t", escape_double = FALSE, trim_ws = TRUE)

power_sub_gh <- subset(power_sub, year %in% unique(GH_SS_Replication$year))

gh_merge <- merge(GH_SS_Replication, power_sub_gh, by = c("ccode", "year"))


rep_1 <- glm(win ~ msadopt + polity + politysquared + cinc + strat + terrain + troopsengaged + opptroopsengaged + 
               bankseducation + loggdppercapita + bankscoups, 
             family=binomial(link='probit'),
             data=gh_merge) # cinc

rep_2 <- glm(win ~ msadopt + polity + politysquared + y + strat + terrain + troopsengaged + opptroopsengaged + 
               bankseducation + loggdppercapita + bankscoups, 
             family=binomial(link='probit'),
             data=gh_merge) # new measure

rep_3 <- glm(win ~ msadopt + polity + politysquared + gdp + strat + terrain + troopsengaged + opptroopsengaged + 
               bankseducation + loggdppercapita + bankscoups, 
             family=binomial(link='probit'),
             data=gh_merge) # gdp

# NB: this is a lot of overfitting on variables associated with gdp; if anything that should be in the proposed measure's favor, though, since AIC penalizes for number of parameters
rep_4 <- glm(win ~ msadopt + polity + politysquared + y_root + y + strat + terrain + troopsengaged + opptroopsengaged + 
               bankseducation + loggdppercapita + bankscoups, 
             family=binomial(link='probit'),
             data=gh_merge) # sqr root + new measure

rep_5 <- glm(win ~ msadopt + polity + politysquared + pop + gdp + gdpsquared + terrain + troopsengaged + opptroopsengaged + 
               bankseducation + loggdppercapita + bankscoups,  
             family=binomial(link='probit'),
             data=gh_merge) # pop + gdp + gdp^2

rep_list[["gh"]] <- list(rep_1, rep_2, rep_3, rep_4, rep_5)




# --- Horowitz and Stam (2014):

HorowitzStamLeadersIOMIDReplication <- read_delim("data/HorowitzStamLeadersIOMIDReplication.tab", "\t", escape_double = FALSE, trim_ws = TRUE)

power_sub_hs <- subset(power_sub, year %in% unique(HorowitzStamLeadersIOMIDReplication$year))

hs_merge <- merge(HorowitzStamLeadersIOMIDReplication, power_sub_hs, by = c("ccode", "year"))


rep_1 <- glm(cwinit ~ milnoncombat + combat + rebel + warwin + warloss + rebelwin + rebelloss + age + aut + 
               cinc + tau_lead + officetenure1000 + fiveyearchallengelag + cwpceyrs1 + cwpceyrs2 + cwpceyrs3,
             family=binomial(link='logit'),
             data = hs_merge) # cinc

rep_2 <- glm(cwinit ~ milnoncombat + combat + rebel + warwin + warloss + rebelwin + rebelloss + age + aut + 
               y + tau_lead + officetenure1000 + fiveyearchallengelag + cwpceyrs1 + cwpceyrs2 + cwpceyrs3,
             family=binomial(link='logit'),
             data = hs_merge) # new measure

rep_3 <- glm(cwinit ~ milnoncombat + combat + rebel + warwin + warloss + rebelwin + rebelloss + age + aut + 
               gdp + tau_lead + officetenure1000 + fiveyearchallengelag + cwpceyrs1 + cwpceyrs2 + cwpceyrs3,
             family=binomial(link='logit'),
             data = hs_merge) # gdp

rep_4 <- glm(cwinit ~ milnoncombat + combat + rebel + warwin + warloss + rebelwin + rebelloss + age + aut + 
               y_root + y + tau_lead + officetenure1000 + fiveyearchallengelag + cwpceyrs1 + cwpceyrs2 + cwpceyrs3,
             family=binomial(link='logit'),
             data = hs_merge) # sqr root + new measure

rep_5 <- glm(cwinit ~ milnoncombat + combat + rebel + warwin + warloss + rebelwin + rebelloss + age + aut + 
               pop + gdp + gdpsquared + tau_lead + officetenure1000 + fiveyearchallengelag + cwpceyrs1 + cwpceyrs2 + cwpceyrs3,
             family=binomial(link='logit'),
             data = hs_merge) # pop + gdp + gdp^2

rep_list[["hs"]] <- list(rep_1, rep_2, rep_3, rep_4, rep_5)




# --- Narang and Talmadge (2017):

NarangTalmadge <- read_delim("data/NarangTalmadge.tab", "\t", escape_double = FALSE, trim_ws = TRUE)

colnames(NarangTalmadge)[5] <- "year"

NarangTalmadge_sub <- subset(NarangTalmadge, WarOutcome <= 1)

power_sub_nt <- subset(power_sub, year %in% unique(NarangTalmadge_sub$year))

nt_merge <- merge(NarangTalmadge_sub, power_sub_nt, by = c("ccode", "year"))

rep_1 <- glm(WarOutcome ~ Inititator + ConventionalCapability + Defense + PolityScore + civwarCOW + sumBad2,
             family=binomial(link='logit'),
             data = nt_merge) # cinc

rep_2 <- glm(WarOutcome ~ Inititator + y + Defense + PolityScore + civwarCOW + sumBad2,
             family=binomial(link='logit'),
             data = nt_merge) # new measure

rep_3 <- glm(WarOutcome ~ Inititator + gdp + Defense + PolityScore + civwarCOW + sumBad2,
             family=binomial(link='logit'),
             data = nt_merge) # gdp

rep_4 <- glm(WarOutcome ~ Inititator + y_root + y + Defense + PolityScore + civwarCOW + sumBad2,
             family=binomial(link='logit'),
             data = nt_merge) # sqr root + new measure

rep_5 <- glm(WarOutcome ~ Inititator + pop + gdp + gdpsquared + PolityScore + civwarCOW + sumBad2,
             family=binomial(link='logit'),
             data = nt_merge) # pop + gdp + gdp^2

rep_list[["nt"]] <- list(rep_1, rep_2, rep_3, rep_4, rep_5)




# --- AIC results:

rep_results_gof_list <- list()
for(i in 1:length(rep_list)){
  
  list.i <- rep_list[[i]]
  rep_results_gof <- data.frame()
  for(j in 1:length(list.i)){
    bic_i <- BIC(list.i[[j]])
    aic_i <- AIC(list.i[[j]])
    loglik_i <- logLik(list.i[[j]])
    
    row.i <- c(bic_i, aic_i, loglik_i)
    rep_results_gof <- rbind(rep_results_gof, row.i)
  }
  colnames(rep_results_gof) <- c("bic", "aic", "loglik")
  rep_results_gof_list[[i]] <- rep_results_gof
  
}

for(i in 1:length(rep_results_gof_list)){
  rownames(rep_results_gof_list[[i]]) <- c("cinc", "proposed", "gdp", "linear + proposed", "disaggregated")
}

names(rep_results_gof_list) <- names(rep_list)



# --- Traditional dissaggregated specification:

# positive values indicate that pop + gdp + gdp^2 has lower AIC than the proposed GDPxGDPPC variable

rep_results_gof_list$ad$aic[2] - rep_results_gof_list$ad$aic[5] 
rep_results_gof_list$cw$aic[2] - rep_results_gof_list$cw$aic[5]
rep_results_gof_list$gh$aic[2] - rep_results_gof_list$gh$aic[5]
rep_results_gof_list$hs$aic[2] - rep_results_gof_list$hs$aic[5]
rep_results_gof_list$nt$aic[2] - rep_results_gof_list$nt$aic[5]

# traditional specification shows improved AICs in 3/5 cases


# --- Proposed variable w/ first order (i.e. logged) version

# positive values indicate that sqrt(gdpxgdppc) + gdpxgdpcc has lower AIC than the proposed GDPxGDPPC variable alone

rep_results_gof_list$ad$aic[2] - rep_results_gof_list$ad$aic[4] 
rep_results_gof_list$cw$aic[2] - rep_results_gof_list$cw$aic[4]
rep_results_gof_list$gh$aic[2] - rep_results_gof_list$gh$aic[4]
rep_results_gof_list$hs$aic[2] - rep_results_gof_list$hs$aic[4]
rep_results_gof_list$nt$aic[2] - rep_results_gof_list$nt$aic[4]

# specification that includes a first order version of the proposed variable shows improved AICs in 3/5 cases





# --> sample size is too small to draw definitive conclusions, but the results are noteworthy since AIC penalizes 
# models with additional parameters; one nice feature of the traditional specification is that we can better
# isolate the explanatory work done by pop vs. gdp, but of course modeling decisions will depend on a number of 
# study-specific concerns.


