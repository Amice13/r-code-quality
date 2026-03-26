library(car)
library(data.table)
library(lme4)
library(lmtest)
library(multcomp)
library(multiwayvcov)
library(stringr)
library(texreg)
library(tidyr)

# set your working directory to the location of the replication archive
# setwd()

# data for bill enactment and advancement analyses

bill_data <- fread("lobbying_on_advancement.csv", header = TRUE, stringsAsFactors = FALSE)

# data for vote switching analysis

vote_switching_data <- fread("vote_switching.csv", header = TRUE, stringsAsFactors = FALSE)

################################################################################
# Table 1: Lobbying Patterns for All States

table(bill_data$for_against_final)
round(prop.table(table(bill_data$for_against_final)),3)*100

################################################################################
# Table 2: Lobbying Patterns and Probability of Enactment

# set "no lobbying" as the baseline type of lobbying experienced by each bill
bill_data$for_against_final <- relevel(as.factor(bill_data$for_against_final), "none")

# analysis pooling all states, no covariates
enact_model_all <- lm(enacted ~ for_against_final + as.factor(state_session), bill_data)
summary(enact_model_all)

# testing effects among non-baseline levels of lobbying
summary(glht(enact_model_all, linfct = "`for_against_finalonly for` - `for_against_finalonly against` = 0"))
summary(glht(enact_model_all, linfct = "`for_against_finalfor and against` - `for_against_finalonly against` = 0"))

# analysis for CO, no covariates
enact_model_co <- lm(enacted ~ for_against_final + as.factor(state_session), 
                     bill_data[which(bill_data$state=="CO")])
summary(enact_model_co)

summary(glht(enact_model_co, linfct = "`for_against_finalonly for` - `for_against_finalonly against` = 0"))
summary(glht(enact_model_co, linfct = "`for_against_finalfor and against` - `for_against_finalonly against` = 0"))

# analysis for NE, no covariates
enact_model_ne <- lm(enacted ~ for_against_final + as.factor(state_session), 
                     bill_data[which(bill_data$state=="NE"),])
summary(enact_model_ne)

summary(glht(enact_model_ne, linfct = "`for_against_finalonly for` - `for_against_finalonly against` = 0"))
summary(glht(enact_model_ne, linfct = "`for_against_finalfor and against` - `for_against_finalonly against` = 0"))

# analysis for WI, no covariates
enact_model_wi <- lm(enacted ~ for_against_final + as.factor(session), 
                     bill_data[which(bill_data$state=="WI"),])
summary(enact_model_wi)

summary(glht(enact_model_wi, linfct = "`for_against_finalonly for` - `for_against_finalonly against` = 0"))
summary(glht(enact_model_wi, linfct = "`for_against_finalfor and against` - `for_against_finalonly against` = 0"))

# analysis pooling all states, with covariates
enact_model_all_covars <- lm(enacted ~ for_against_final + intro_pid + maj_leader_intro + 
                               min_leader_intro + key_vote + duplicate_bill + 
                               as.factor(state_session), bill_data)
summary(enact_model_all_covars)

summary(glht(enact_model_all_covars, linfct = "`for_against_finalonly for` - `for_against_finalonly against` = 0"))
summary(glht(enact_model_all_covars, linfct = "`for_against_finalfor and against` - `for_against_finalonly against` = 0"))

# analysis for CO, with covariates
enact_model_co_covars <- lm(enacted ~ for_against_final + intro_pid + maj_leader_intro + 
                              min_leader_intro + key_vote + duplicate_bill + 
                              as.factor(state_session), bill_data[which(bill_data$state=="CO"),])
summary(enact_model_co_covars)

summary(glht(enact_model_co_covars, linfct = "`for_against_finalonly for` - `for_against_finalonly against` = 0"))
summary(glht(enact_model_co_covars, linfct = "`for_against_finalfor and against` - `for_against_finalonly against` = 0"))

# analysis for NE, with covariates
enact_model_ne_covars <- lm(enacted ~ for_against_final + intro_pid + maj_leader_intro + 
                              key_vote + duplicate_bill + 
                              as.factor(state_session), bill_data[which(bill_data$state=="NE"),])
summary(enact_model_ne_covars)

summary(glht(enact_model_ne_covars, linfct = "`for_against_finalonly for` - `for_against_finalonly against` = 0"))
summary(glht(enact_model_ne_covars, linfct = "`for_against_finalfor and against` - `for_against_finalonly against` = 0"))

# analysis for WI, with covariates
enact_model_covars_wi <- lm(enacted ~ for_against_final + intro_pid + maj_leader_intro + 
                              min_leader_intro + key_vote + duplicate_bill + 
                              as.factor(state_session), bill_data[which(bill_data$state=="WI"),])
summary(enact_model_covars_wi)

summary(glht(enact_model_covars_wi, linfct = "`for_against_finalonly for` - `for_against_finalonly against` = 0"))
summary(glht(enact_model_covars_wi, linfct = "`for_against_finalfor and against` - `for_against_finalonly against` = 0"))

htmlreg(l=list(enact_model_all, enact_model_all_covars, enact_model_co, enact_model_co_covars,
               enact_model_ne, enact_model_ne_covars, enact_model_wi, enact_model_covars_wi), 
        file="table2.doc",
        stars = 0.05,
        custom.model.names = c("All", "All", "Colorado", "Colorado", "Nebraska",
                               "Nebraska", "Wisconsin", "Wisconsin"),
        custom.coef.map = list("for_against_finalfor and against" = "Lobbying For and Against",
                               "for_against_finalonly against" = "Only Lobbying Against", 
                               "for_against_finalonly for" = "Only Lobbying For"
        ),
        omit.coef = c("state_session"),
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are coded as dichotomous indicators for whether organized interests 
        officially take positions only in support of a proposal, only against a proposal, 
        or both for and against a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The model in the leftmost column pools legislative proposals from
        Colorado, Nebraska, and Wisconsin, while the remaining three models use only data from
        the state indicated.", 
        caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Enactment (OLS)")

################################################################################
# Table 3: Number of Groups Lobbying and Probability of Enactment

# analysis pooling all states, no covariates
enact_model_counts_all <- lm(enacted ~ num_for_before_final + num_against_before_final + 
                               as.factor(state_session), bill_data)
summary(enact_model_counts_all)

summary(glht(enact_model_counts_all, linfct = "`num_for_before_final` + `num_against_before_final` = 0"))

# analysis for CO, no covariates
enact_model_counts_co <- lm(enacted ~ num_for_before_final + num_against_before_final + 
                              as.factor(state_session), bill_data[which(bill_data$state=="CO")])
summary(enact_model_counts_co)

summary(glht(enact_model_counts_co, linfct = "`num_for_before_final` + `num_against_before_final` = 0"))

# analysis for NE, no covariates
enact_model_counts_ne <- lm(enacted ~ num_for_before_final + num_against_before_final + 
                              as.factor(state_session), bill_data[which(bill_data$state=="NE"),])
summary(enact_model_counts_ne)

summary(glht(enact_model_counts_ne, linfct = "`num_for_before_final` + `num_against_before_final` = 0"))

# analysis for WI, no covariates
enact_model_counts_wi <- lm(enacted ~ num_for_before_final + num_against_before_final + 
                              as.factor(state_session), bill_data[which(bill_data$state=="WI"),])
summary(enact_model_counts_wi)

summary(glht(enact_model_counts_wi, linfct = "`num_for_before_final` + `num_against_before_final` = 0"))


# analysis pooling all states, with covariates
enact_counts_model_all_covars <- lm(enacted ~ num_for_before_final + num_against_before_final + intro_pid + maj_leader_intro + 
                                      min_leader_intro + key_vote + duplicate_bill + 
                                      as.factor(state_session), bill_data)
summary(enact_counts_model_all_covars)

summary(glht(enact_counts_model_all_covars, linfct = "`num_for_before_final` + `num_against_before_final` = 0"))

# analysis for CO, with covariates
enact_counts_model_co_covars <- lm(enacted ~ num_for_before_final + num_against_before_final + intro_pid + maj_leader_intro + 
                                     min_leader_intro + key_vote + duplicate_bill + 
                                     as.factor(state_session), bill_data[which(bill_data$state=="CO"),])
summary(enact_counts_model_co_covars)

summary(glht(enact_counts_model_co_covars, linfct = "`num_for_before_final` + `num_against_before_final` = 0"))

# analysis for NE, with covariates
enact_counts_model_ne_covars <- lm(enacted ~ num_for_before_final + num_against_before_final + intro_pid + maj_leader_intro + 
                                     key_vote + duplicate_bill + 
                                     as.factor(state_session), bill_data[which(bill_data$state=="NE"),])
summary(enact_counts_model_ne_covars)

summary(glht(enact_counts_model_ne_covars, linfct = "`num_for_before_final` + `num_against_before_final` = 0"))

# analysis for WI, with covariates
enact_counts_model_covars_wi <- lm(enacted ~ num_for_before_final + num_against_before_final + intro_pid + maj_leader_intro + 
                                     min_leader_intro + key_vote + duplicate_bill + 
                                     as.factor(state_session), bill_data[which(bill_data$state=="WI"),])
summary(enact_counts_model_covars_wi)

summary(glht(enact_counts_model_covars_wi, linfct = "`num_for_before_final` + `num_against_before_final` = 0"))


htmlreg(l=list(enact_model_counts_all, enact_counts_model_all_covars, enact_model_counts_co, enact_counts_model_co_covars,  
               enact_model_counts_ne, enact_counts_model_ne_covars, enact_model_counts_wi, enact_counts_model_covars_wi), 
        file="table3.doc",
        stars = 0.05,
        custom.model.names = c("All", "All", "Colorado", "Colorado", "Nebraska",
                               "Nebraska", "Wisconsin", "Wisconsin"),
        custom.coef.map = list("num_for_before_final" = "Number Lobbying For",
                               "num_against_before_final" = "Number Lobbying Against"
        ),
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are counts of the number of organized interests who take positions in support
        and in opposition to a proposal. The model in the leftmost column pools legislative 
        proposals from Colorado, Nebraska, and Wisconsin, while the remaining three models use
        only data from the state indicated.", 
        caption.above=TRUE,
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        caption = "Number of Groups Lobbying and Probability of Enactment (OLS)")

################################################################################
# Table 4: Lobbying Patterns and Probability of Legislative Advancement (WI only)

# set "no lobbying" as the baseline type of lobbying experienced by each bill
bill_data$for_against_pass <- relevel(as.factor(bill_data$for_against_pass), "none")
bill_data$for_against_floor <- relevel(as.factor(bill_data$for_against_floor), "none")
bill_data$for_against_committee <- relevel(as.factor(bill_data$for_against_committee), "none")

# did the bill pass the chamber of origin? (no covariates)
pass_chamber_model_wi <- lm(passed_chamber ~ for_against_pass + as.factor(state_session), 
                            bill_data[which(bill_data$state=="WI"),])
summary(pass_chamber_model_wi)

# did the bill reach the floor in the chamber of origin? (no covariates)
reach_floor_model_wi <- lm(reached_floor ~ for_against_floor + as.factor(state_session), 
                           bill_data[which(bill_data$state=="WI"),])
summary(reach_floor_model_wi)

# did the bill emerge from committee in the chamber of origin? (no covariates)
pass_committee_model_wi <- lm(passed_committee ~ for_against_committee + as.factor(state_session), 
                              bill_data[which(bill_data$state=="WI"),])
summary(pass_committee_model_wi)

# did the bill pass the chamber of origin? (w/ covariates)
pass_chamber_model_covars_wi <- lm(passed_chamber ~  for_against_pass + intro_pid + maj_leader_intro + 
                                     min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                   bill_data[which(bill_data$state=="WI"),])
summary(pass_chamber_model_covars_wi)

# did the bill reach the floor in the chamber of origin? (w/ covariates)
reach_floor_model_covars_wi <- lm(reached_floor ~  for_against_floor + intro_pid + maj_leader_intro + 
                                    min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                  bill_data[which(bill_data$state=="WI"),])
summary(reach_floor_model_covars_wi)

# did the bill emerge from committee in the chamber of origin? (w/ covariates)
pass_committee_model_covars_wi <- lm(passed_committee ~  for_against_committee + intro_pid + maj_leader_intro + 
                                       min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                     bill_data[which(bill_data$state=="WI"),])
summary(pass_committee_model_covars_wi)

htmlreg(l=list(pass_committee_model_wi, pass_committee_model_covars_wi, 
               reach_floor_model_wi, reach_floor_model_covars_wi, 
               pass_chamber_model_wi, pass_chamber_model_covars_wi), 
        file="table4.doc",
        stars = 0.05,
        custom.model.names = c("Passed Committee", "Passed Committee", 
                               "Reached Floor", "Reached Floor",
                               "Passed Chamber", "Passed Chamber"),
        custom.coef.map = list("for_against_passfor and against"="Lobbying For and Against",
                               "for_against_passonly against"="Only Lobbying Against", 
                               "for_against_passonly for"="Only Lobbying For",
                               "for_against_floorfor and against"="Lobbying For and Against",
                               "for_against_flooronly against"="Only Lobbying Against", 
                               "for_against_flooronly for"="Only Lobbying For",
                               "for_against_committeefor and against"="Lobbying For and Against",
                               "for_against_committeeonly against"="Only Lobbying Against", 
                               "for_against_committeeonly for"="Only Lobbying For"
        ),
        omit.coef = "state_session",
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal reached the stage of the legislative
        process indicated by the column heading. The explanatory 
        variables are coded as dichotomous indicators for whether organized interests 
        officially take positions only in support of a proposal, only against a proposal, 
        or both for and against a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The models include only legislative proposals from Wisconsin.", caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Legislative Advancement (OLS)")

################################################################################
# Table 5: Number of Groups Lobbying and Probability of Legislative Advancement (WI only)

# did the bill pass the chamber of origin? (no covariates)
pass_chamber_counts_model_wi <- lm(passed_chamber ~ num_for_before_pass + num_against_before_pass + as.factor(state_session), 
                                   bill_data[which(bill_data$state=="WI"),])
summary(pass_chamber_counts_model_wi)

# did the bill reach the floor in the chamber of origin? (no covariates)
reach_floor_counts_model_wi <- lm(reached_floor ~ num_for_before_floor + num_against_before_floor + as.factor(state_session), 
                                  bill_data[which(bill_data$state=="WI"),])
summary(reach_floor_counts_model_wi)

# did the bill emerge from committee in the chamber of origin? (no covariates)
pass_committee_counts_model_wi <- lm(passed_committee ~ num_for_before_committee + num_against_before_committee + as.factor(state_session), 
                                     bill_data[which(bill_data$state=="WI"),])
summary(pass_committee_counts_model_wi)

# comparing magnitudes of counts for and against
summary(glht(pass_chamber_counts_model_wi, linfct = "`num_for_before_pass` + `num_against_before_pass` = 0"))
summary(glht(reach_floor_counts_model_wi, linfct = "`num_for_before_floor` + `num_against_before_floor` = 0"))
summary(glht(pass_committee_counts_model_wi, linfct = "`num_for_before_committee` + `num_against_before_committee` = 0"))

# did the bill pass the chamber of origin? (w/ covariates)
pass_chamber_counts_covars_model_wi <- lm(passed_chamber ~ num_for_before_pass + num_against_before_pass + intro_pid + maj_leader_intro + 
                                            min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                          bill_data[which(bill_data$state=="WI"),])
summary(pass_chamber_counts_covars_model_wi)

# did the bill reach the floor in the chamber of origin? (w/ covariates)
reach_floor_counts_covars_model_wi <- lm(reached_floor ~ num_for_before_floor + num_against_before_floor + intro_pid + maj_leader_intro + 
                                           min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                         bill_data[which(bill_data$state=="WI"),])
summary(reach_floor_counts_covars_model_wi)

# did the bill emerge from committee in the chamber of origin? (w/ covariates)
pass_committee_counts_covars_model_wi <- lm(passed_committee ~ num_for_before_committee + num_against_before_committee + intro_pid + maj_leader_intro + 
                                              min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                            bill_data[which(bill_data$state=="WI"),])
summary(pass_committee_counts_covars_model_wi)

htmlreg(l=list(pass_committee_counts_model_wi, pass_committee_counts_covars_model_wi, 
               reach_floor_counts_model_wi, reach_floor_counts_covars_model_wi, 
               pass_chamber_counts_model_wi, pass_chamber_counts_covars_model_wi), 
        file="table5.doc",
        stars = 0.05,
        custom.model.names = c("Passed Committee", "Passed Committee", 
                               "Reached Floor", "Reached Floor",
                               "Passed Chamber", "Passed Chamber"),
        custom.coef.map = list("num_for_before_committee"="Number Lobbying For",
                               "num_against_before_committee"="Number Lobbying Against",
                               "num_for_before_floor"="Number Lobbying For",
                               "num_against_before_floor"="Number Lobbying Against",
                               "num_for_before_pass"="Number Lobbying For",
                               "num_against_before_pass"="Number Lobbying Against"
        ),
        omit.coef = "state_session",
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal reached the stage of the legislative
        process indicated by the column heading. The explanatory 
        variables are counts of the number of organized interests who take positions in support of and in opposition
        to a proposal.  The models include only legislative proposals from Wisconsin.", caption.above=TRUE,
        caption = "Number of Groups Lobbying and Probability of Legislative Advancement (OLS)")

################################################################################
# Table 7: WI Vote Buying 

# set "no lobbying" as the baseline type of lobbying experienced by each bill
vote_switching_data$type_lobbying <- relevel(as.factor(vote_switching_data$type_lobbying), "No Lobbying")

floorvote_outcome <- lm(floor_stage_vote ~ committee_stage_vote + type_lobbying + as.factor(session), 
                        data=vote_switching_data)
summary(floorvote_outcome)
cluster_vcov <- cluster.vcov(floorvote_outcome, cluster=paste0(vote_switching_data$session,vote_switching_data$bill))
coeftest(floorvote_outcome, cluster_vcov)
clustered_ses <- coeftest(floorvote_outcome, cluster_vcov)[,2]
clustered_pvals <- coeftest(floorvote_outcome, cluster_vcov)[,4]

htmlreg(l=list(floorvote_outcome),
        file="table7.doc",
        stars = 0.05,
        custom.model.names = c("Model 1"),
        custom.coef.names = c("Intercept",
                              "Committee Vote-Yes",
                              "Pre-Against, Post-For", "Pre-Against, Post-None",
                              "Pre-Both, Post-None", "Pre-For, Post-Against",
                              "Pre-For, Post-None", "Pre-None, Post-Against",
                              "Pre-None, Post-For"),
        omit.coef = "session",
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        override.se = list(clustered_ses),
        override.pvalues = list(clustered_pvals),
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislator voted for a given legislative proposal on the floor.
        The explanatory variables include a dichotomous indicator for whether a legislator
        voted for a given legislative proposal in committee and a series of dichotomous
        indicators specifying the timing and directionality of lobbying activity on the
        proposal.", caption.above=TRUE,
        caption = "Lobbying Patterns and Vote Switching (OLS)")

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# SUPPLEMENTAL INFORMATION MATERIALS

###############################################################################
# Table SI.1: Descriptive Statistics for ``Lobbying Patterns and Probability of Enactment Analyses"

# enactment (outcome)
summary(bill_data$enacted)
summary(bill_data$enacted[which(bill_data$state=="CO")])
summary(bill_data$enacted[which(bill_data$state=="NE")])
summary(bill_data$enacted[which(bill_data$state=="WI")])

# lobbying for and against
summary(as.numeric(bill_data$for_against_final=="for and against"))
summary(as.numeric(bill_data$for_against_final[which(bill_data$state=="CO")]=="for and against"))
summary(as.numeric(bill_data$for_against_final[which(bill_data$state=="NE")]=="for and against"))
summary(as.numeric(bill_data$for_against_final[which(bill_data$state=="WI")]=="for and against"))

# only lobbying against
summary(as.numeric(bill_data$for_against_final=="only against"))
summary(as.numeric(bill_data$for_against_final[which(bill_data$state=="CO")]=="only against"))
summary(as.numeric(bill_data$for_against_final[which(bill_data$state=="NE")]=="only against"))
summary(as.numeric(bill_data$for_against_final[which(bill_data$state=="WI")]=="only against"))

# only lobbying for
summary(as.numeric(bill_data$for_against_final=="only for"))
summary(as.numeric(bill_data$for_against_final[which(bill_data$state=="CO")]=="only for"))
summary(as.numeric(bill_data$for_against_final[which(bill_data$state=="NE")]=="only for"))
summary(as.numeric(bill_data$for_against_final[which(bill_data$state=="WI")]=="only for"))

# num groups lobbying for
summary(bill_data$num_for_before_final)
summary(bill_data$num_for_before_final[which(bill_data$state=="CO")])
summary(bill_data$num_for_before_final[which(bill_data$state=="NE")])
summary(bill_data$num_for_before_final[which(bill_data$state=="WI")])

# num groups lobbying against
summary(bill_data$num_against_before_final)
summary(bill_data$num_against_before_final[which(bill_data$state=="CO")])
summary(bill_data$num_against_before_final[which(bill_data$state=="NE")])
summary(bill_data$num_against_before_final[which(bill_data$state=="WI")])

# maj. pty. sponsor
summary(as.numeric(bill_data$intro_pid=="Majority"))
summary(as.numeric(bill_data$intro_pid[which(bill_data$state=="CO")]=="Majority"))
summary(as.numeric(bill_data$intro_pid[which(bill_data$state=="NE")]=="Majority"))
summary(as.numeric(bill_data$intro_pid[which(bill_data$state=="WI")]=="Majority"))

# min. pty. sponsor
summary(as.numeric(bill_data$intro_pid=="Minority"))
summary(as.numeric(bill_data$intro_pid[which(bill_data$state=="CO")]=="Minority"))
summary(as.numeric(bill_data$intro_pid[which(bill_data$state=="NE")]=="Minority"))
summary(as.numeric(bill_data$intro_pid[which(bill_data$state=="WI")]=="Minority"))

# maj. ldr. sponsor
summary(bill_data$maj_leader_intro)
summary(bill_data$maj_leader_intro[which(bill_data$state=="CO")])
summary(bill_data$maj_leader_intro[which(bill_data$state=="NE")])
summary(bill_data$maj_leader_intro[which(bill_data$state=="WI")])

# min. ldr. sponsor
summary(bill_data$min_leader_intro)
summary(bill_data$min_leader_intro[which(bill_data$state=="CO")])
summary(bill_data$min_leader_intro[which(bill_data$state=="NE")])
summary(bill_data$min_leader_intro[which(bill_data$state=="WI")])

# PVS key vote
summary(bill_data$key_vote)
summary(bill_data$key_vote[which(bill_data$state=="CO")])
summary(bill_data$key_vote[which(bill_data$state=="NE")])
summary(bill_data$key_vote[which(bill_data$state=="WI")])

# duplicate bill
summary(bill_data$duplicate_bill)
summary(bill_data$duplicate_bill[which(bill_data$state=="CO")])
summary(bill_data$duplicate_bill[which(bill_data$state=="NE")])
summary(bill_data$duplicate_bill[which(bill_data$state=="WI")])

# polarization
summary(bill_data$chamber_pol)
summary(bill_data$chamber_pol[which(bill_data$state=="CO")])
summary(bill_data$chamber_pol[which(bill_data$state=="NE")])
summary(bill_data$chamber_pol[which(bill_data$state=="WI")])

# professionalization
summary(bill_data$squire)
summary(bill_data$squire[which(bill_data$state=="CO")])
summary(bill_data$squire[which(bill_data$state=="NE")])
summary(bill_data$squire[which(bill_data$state=="WI")])

################################################################################
# Table SI.2: Summary of Bill Introductions/Enactments and Lobbying Patterns by State and Session

descriptive_stats_pattern <- table(bill_data$state_session, bill_data$for_against_final, bill_data$enacted)
descriptive_stats_total_introduced <- table(bill_data$state_session)
descriptive_stats_total_enacted <- table(bill_data$state_session[which(bill_data$enacted==1)])

################################################################################
# Table SI.3: Lobbyists Motivations and Perceptions about Bills they Lobbying On

# SEE STATA .DO FILE

################################################################################
# Table SI.4: Lobbying Patterns and Probability of Enactment (OLS w/ Covariates)

htmlreg(l=list(enact_model_all_covars, enact_model_co_covars, enact_model_ne_covars, enact_model_covars_wi), 
        file="tablesi4.doc",
        stars = 0.05,
        custom.model.names = c("All", "Colorado", "Nebraska", "Wisconsin"),
        custom.coef.map = list("(Intercept)" = "Intercept", 
                               "for_against_finalfor and against" = "Lobbying For and Against",
                               "for_against_finalonly against" = "Only Lobbying Against", 
                               "for_against_finalonly for" = "Only Lobbying For",
                               "intro_pidMajority" = "Maj. Pty. Sponsor",
                               "intro_pidMinority" = "Min. Pty. Sponsor",
                               "maj_leader_intro" = "Maj. Ldr. Sponsor",
                               "min_leader_intro" = "Min. Ldr. Sponsor",
                               "key_vote" = "PVS Key Vote",
                               "duplicate_bill" = "Duplicate Bill"
        ),
        omit.coef = "session",
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are coded as dichotomous indicators for whether organized interests 
        officially take positions only in support of a proposal, only against a proposal, 
        or both for and against a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The model in the leftmost column pools legislative proposals from
        Colorado, Nebraska, and Wisconsin, while the remaining three models use only data from
        the state indicated.", 
        caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Enactment (OLS with Covariates)")

################################################################################
# Table SI.5: Lobbying Patterns and Probability of Enactment (Logit)

enact_model_logit_all <- glm(enacted ~ for_against_final + as.factor(state_session), bill_data,
                             family = binomial(link="logit"))
summary(enact_model_logit_all)

enact_model_logit_co <- glm(enacted ~ for_against_final + as.factor(state_session), 
                            bill_data[which(bill_data$state=="CO")], family = binomial(link="logit"))
summary(enact_model_logit_co)

enact_model_logit_ne <- glm(enacted ~ for_against_final + as.factor(state_session), 
                            bill_data[which(bill_data$state=="NE"),], family = binomial(link="logit"))
summary(enact_model_logit_ne)

enact_model_logit_wi <- glm(enacted ~ for_against_final + as.factor(session), 
                            bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(enact_model_logit_wi)

enact_model_logit_covars_all <- glm(enacted ~ for_against_final + intro_pid + maj_leader_intro + min_leader_intro + 
                                      key_vote + duplicate_bill + as.factor(state_session), bill_data,
                                    family = binomial(link="logit"))
summary(enact_model_logit_covars_all)

enact_model_logit_covars_co <- glm(enacted ~ for_against_final + intro_pid + maj_leader_intro + min_leader_intro + 
                                     key_vote + duplicate_bill + as.factor(state_session), 
                                   bill_data[which(bill_data$state=="CO")], family = binomial(link="logit"))
summary(enact_model_logit_covars_co)

enact_model_logit_covars_ne <- glm(enacted ~ for_against_final + intro_pid + maj_leader_intro + min_leader_intro + 
                                     key_vote + duplicate_bill + as.factor(state_session), 
                                   bill_data[which(bill_data$state=="NE"),], family = binomial(link="logit"))
summary(enact_model_logit_covars_ne)

enact_model_logit_covars_wi <- glm(enacted ~ for_against_final + intro_pid + maj_leader_intro + min_leader_intro + 
                                     key_vote + duplicate_bill + as.factor(session), 
                                   bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(enact_model_logit_covars_wi)

htmlreg(l=list(enact_model_logit_all, enact_model_logit_covars_all,
               enact_model_logit_co, enact_model_logit_covars_co,
               enact_model_logit_ne, enact_model_logit_covars_ne,
               enact_model_logit_wi, enact_model_logit_covars_wi), 
        file="tablesi5.doc",
        stars = 0.05,
        custom.model.names = c("All", "All", 
                               "Colorado", "Colorado",
                               "Nebraska", "Nebraska",
                               "Wisconsin", "Wisconsin"),
        custom.coef.map = list("(Intercept)" = "Intercept", 
                               "for_against_finalfor and against" = "Lobbying For and Against",
                               "for_against_finalonly against" = "Only Lobbying Against", 
                               "for_against_finalonly for" = "Only Lobbying For"
        ),
        omit.coef = "session",
        include.aic = FALSE,
        include.bic = FALSE,
        include.loglik = FALSE,
        include.deviance = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with logistic regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are coded as dichotomous indicators for whether organized interests 
        officially take positions only in support of a proposal, only against a proposal, 
        or both for and against a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The model in the leftmost column pools legislative proposals from
        Colorado, Nebraska, and Wisconsin, while the remaining three models use only data from
        the state indicated.", 
        caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Enactment (Logit)")

################################################################################
# Table SI.6: Lobbying Patterns and Probability of Enactment (MLM)

enact_model_all <- lmer(enacted ~ for_against_final + (1|state_session) + (1|state), bill_data)
summary(enact_model_all)

enact_model_co <- lmer(enacted ~ for_against_final + (1|state_session), 
                       bill_data[which(bill_data$state=="CO")])
summary(enact_model_co)

enact_model_ne <- lmer(enacted ~ for_against_final + (1|state_session), 
                       bill_data[which(bill_data$state=="NE"),])
summary(enact_model_ne)

enact_model_wi <- lmer(enacted ~ for_against_final + (1|state_session), 
                       bill_data[which(bill_data$state=="WI"),])
summary(enact_model_wi)

htmlreg(l=list(enact_model_all, enact_model_co, enact_model_ne, enact_model_wi), 
        file="tablesi6.doc",
        stars = 0.05,
        custom.model.names = c("All", "Colorado", "Nebraska", "Wisconsin"),
        custom.coef.names = c("Intercept", "Lobbying for and Against",
                              "Only Lobbying Against", "Only Lobbying For"
        ),
        include.aic = FALSE,
        include.bic = FALSE,
        include.loglik = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        omit.coef = "state_session",
        custom.gof.names = c("Num. obs.", "Num. State-Sessions", "Num. States", 
                             "Var(State-Sessions)", "Var(States)", "Var(Residual)"),
        custom.note = "Models are estimated with multilevel ordinary least squares regression. 
        * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are coded as dichotomous indicators for whether organized interests 
        officially take positions only in support of a proposal, only against a proposal, 
        or both for and against a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The model in the leftmost column pools legislative proposals from
        Colorado, Nebraska, and Wisconsin, while the remaining three models use only data from
        the state indicated.  The first model includes varying intercepts for each legislative
        session and for each state (with legislative sessions nested in states).  The remaining
        models include varying intercepts for each legislative session.", 
        caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Enactment (OLS)")

################################################################################
# Table SI.7: Lobbying Patterns and Probability of Enactment (MLM w/ Covariates)

enact_model_all <- lmer(enacted ~ for_against_final + intro_pid + maj_leader_intro + min_leader_intro + 
                          key_vote + duplicate_bill + chamber_pol + squire + (1|state_session) + (1|state), bill_data)
summary(enact_model_all)

enact_model_co <- lmer(enacted ~ for_against_final + intro_pid + maj_leader_intro + min_leader_intro + 
                         key_vote + duplicate_bill + chamber_pol + squire + (1|state_session), 
                       bill_data[which(bill_data$state=="CO")])
summary(enact_model_co)

enact_model_ne <- lmer(enacted ~ for_against_final + intro_pid + maj_leader_intro + 
                         key_vote + duplicate_bill + chamber_pol + squire + (1|state_session), 
                       bill_data[which(bill_data$state=="NE"),])
summary(enact_model_ne)

enact_model_wi <- lmer(enacted ~ for_against_final + intro_pid + maj_leader_intro + min_leader_intro + 
                         key_vote + duplicate_bill + chamber_pol + squire + (1|state_session), 
                       bill_data[which(bill_data$state=="WI"),])
summary(enact_model_wi)

htmlreg(l=list(enact_model_all, enact_model_co, enact_model_ne, enact_model_wi), 
        file="tablesi7.doc",
        stars = 0.05,
        custom.model.names = c("All", "Colorado", "Nebraska", "Wisconsin"),
        custom.coef.map = list("(Intercept)" = "Intercept", 
                               "for_against_finalfor and against" = "Lobbying For and Against",
                               "for_against_finalonly against" = "Only Lobbying Against", 
                               "for_against_finalonly for" = "Only Lobbying For",
                               "intro_pidMajority" = "Maj. Pty. Sponsor",
                               "intro_pidMinority" = "Min. Pty. Sponsor",
                               "maj_leader_intro" = "Maj. Ldr. Sponsor",
                               "min_leader_intro" = "Min. Ldr. Sponsor",
                               "key_vote" = "PVS Key Vote",
                               "duplicate_bill" = "Duplicate Bill",
                               "chamber_pol" = "Chamber Polarization",
                               "squire" = "Leg. Professionalization"
        ),
        include.aic = FALSE,
        include.bic = FALSE,
        include.loglik = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        omit.coef = "state_session",
        custom.gof.names = c("Num. obs.", "Num. State-Sessions", "Num. States", 
                             "Var(State-Sessions)", "Var(States)", "Var(Residual)"),
        custom.note = "Models are estimated with multilevel ordinary least squares regression. 
        * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are coded as dichotomous indicators for whether organized interests 
        officially take positions only in support of a proposal, only against a proposal, 
        or both for and against a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The model in the leftmost column pools legislative proposals from
        Colorado, Nebraska, and Wisconsin, while the remaining three models use only data from
        the state indicated.  The first model includes varying intercepts for each legislative
        session and for each state (with legislative sessions nested in states).  The remaining
        models include varying intercepts for each legislative session.", 
        caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Enactment (MLM w/ Covariates)")

################################################################################
# Table SI.8: Lobbying Patterns and Probability of Enactment (OLS w/ "Other")

# set "no lobbying" as the baseline type of lobbying experienced by each bill
bill_data$for_against_other_final <- relevel(as.factor(bill_data$for_against_other_final), "none")

enact_model_all_other <- lm(enacted ~ for_against_other_final + as.factor(state_session), bill_data)
summary(enact_model_all_other)

enact_model_co_other <- lm(enacted ~ for_against_other_final + as.factor(state_session), 
                           bill_data[which(bill_data$state=="CO")])
summary(enact_model_co_other)

enact_model_ne_other <- lm(enacted ~ for_against_other_final + as.factor(state_session), 
                           bill_data[which(bill_data$state=="NE"),])
summary(enact_model_ne_other)

enact_model_wi_other <- lm(enacted ~ for_against_other_final + as.factor(state_session), 
                           bill_data[which(bill_data$state=="WI"),])
summary(enact_model_wi_other)

htmlreg(l=list(enact_model_all_other, enact_model_co_other, enact_model_ne_other, enact_model_wi_other), 
        file="tablesi8.doc",
        stars = 0.05,
        custom.model.names = c("All", "Colorado", "Nebraska", "Wisconsin"),
        custom.coef.names = c("Intercept", "Lobbying For, Against, and Other",
                              "Only Lobbying Against",
                              "Only Lobbying Against and Other",
                              "Only Lobbying For",
                              "Only Lobbying For and Against",
                              "Only Lobbying For and Other",
                              "Only Lobbying Other"),
        omit.coef = c("state_session"),
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are dichotomous indicators of the lobbying patterns for each bill, including
        lobbying for or against the bill, as well as those who take positions neither for nor 
        against the proposal. The model in the leftmost column pools legislative 
        proposals from Colorado, Nebraska, and Wisconsin, while the remaining three models use
        only data from the state indicated.", 
        caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Enactment (OLS w/ Other)")

################################################################################
# Table SI.9: Number of Groups Lobbying and Probability of Enactment (OLS w/ Covariates)

enact_counts_model_all_covars <- lm(enacted ~ num_for_before_final + num_against_before_final + intro_pid + maj_leader_intro + 
                                      min_leader_intro + key_vote + duplicate_bill + 
                                      as.factor(state_session), bill_data)
summary(enact_counts_model_all_covars)

enact_counts_model_co_covars <- lm(enacted ~ num_for_before_final + num_against_before_final + intro_pid + maj_leader_intro + 
                                     min_leader_intro + key_vote + duplicate_bill + 
                                     as.factor(state_session), bill_data[which(bill_data$state=="CO"),])
summary(enact_counts_model_co_covars)

enact_counts_model_ne_covars <- lm(enacted ~ num_for_before_final + num_against_before_final + intro_pid + maj_leader_intro + 
                                     key_vote + duplicate_bill + 
                                     as.factor(state_session), bill_data[which(bill_data$state=="NE"),])
summary(enact_counts_model_ne_covars)

enact_counts_model_covars_wi <- lm(enacted ~ num_for_before_final + num_against_before_final + intro_pid + maj_leader_intro + 
                                     min_leader_intro + key_vote + duplicate_bill + 
                                     as.factor(state_session), bill_data[which(bill_data$state=="WI"),])
summary(enact_counts_model_covars_wi)

htmlreg(l=list(enact_counts_model_all_covars, enact_counts_model_co_covars, enact_counts_model_ne_covars, enact_counts_model_covars_wi), 
        file="tablesi9.doc",
        stars = 0.05,
        custom.model.names = c("All", "Colorado", "Nebraska", "Wisconsin"),
        custom.coef.map = list("(Intercept)" = "Intercept", 
                               "num_for_before_final" = "Number Lobbying For",
                               "num_against_before_final" = "Number Lobbying Against", 
                               "intro_pidMajority" = "Maj. Pty. Sponsor",
                               "intro_pidMinority" = "Min. Pty. Sponsor",
                               "maj_leader_intro" = "Maj. Ldr. Sponsor",
                               "min_leader_intro" = "Min. Ldr. Sponsor",
                               "key_vote" = "PVS Key Vote",
                               "duplicate_bill" = "Duplicate Bill"
        ),
        omit.coef = "session",
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are coded as dichotomous indicators for whether organized interests 
        officially take positions only in support of a proposal, only against a proposal, 
        or both for and against a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The model in the leftmost column pools legislative proposals from
        Colorado, Nebraska, and Wisconsin, while the remaining three models use only data from
        the state indicated.", 
        caption.above=TRUE,
        caption = "Number of Groups Lobbying and Probability of Enactment (OLS with Covariates)")

################################################################################
# Table SI.10: Number of Groups Lobbying and Probability of Enactment (Logit)

enact_model_logit_counts_all <- glm(enacted ~ num_for_before_final + num_against_before_final + 
                                      as.factor(state_session), bill_data,
                                    family = binomial(link="logit"))
summary(enact_model_logit_counts_all)

enact_model_logit_counts_co <- glm(enacted ~ num_for_before_final + num_against_before_final + 
                                     as.factor(state_session), bill_data[which(bill_data$state=="CO")],
                                   family = binomial(link="logit"))
summary(enact_model_logit_counts_co)

enact_model_logit_counts_ne <- glm(enacted ~ num_for_before_final + num_against_before_final + 
                                     as.factor(state_session), bill_data[which(bill_data$state=="NE"),],
                                   family = binomial(link="logit"))
summary(enact_model_logit_counts_ne)

enact_model_logit_counts_wi <- glm(enacted ~ num_for_before_final + num_against_before_final + 
                                     as.factor(state_session), bill_data[which(bill_data$state=="WI"),],
                                   family = binomial(link="logit"))
summary(enact_model_logit_counts_wi)

enact_model_logit_covars_counts_all <- glm(enacted ~ num_for_before_final + num_against_before_final + 
                                             intro_pid + maj_leader_intro + 
                                             min_leader_intro + key_vote + duplicate_bill +
                                             as.factor(state_session), bill_data,
                                           family = binomial(link="logit"))
summary(enact_model_logit_covars_counts_all)

enact_model_logit_covars_counts_co <- glm(enacted ~ num_for_before_final + num_against_before_final + 
                                            intro_pid + maj_leader_intro + 
                                            min_leader_intro + key_vote + duplicate_bill +
                                            as.factor(state_session), bill_data[which(bill_data$state=="CO")],
                                          family = binomial(link="logit"))
summary(enact_model_logit_covars_counts_co)

enact_model_logit_covars_counts_ne <- glm(enacted ~ num_for_before_final + num_against_before_final + 
                                            intro_pid + maj_leader_intro + 
                                            min_leader_intro + key_vote + duplicate_bill +
                                            as.factor(state_session), bill_data[which(bill_data$state=="NE"),],
                                          family = binomial(link="logit"))
summary(enact_model_logit_covars_counts_ne)

enact_model_logit_covars_counts_wi <- glm(enacted ~ num_for_before_final + num_against_before_final + 
                                            intro_pid + maj_leader_intro + 
                                            min_leader_intro + key_vote + duplicate_bill +
                                            as.factor(state_session), bill_data[which(bill_data$state=="WI"),],
                                          family = binomial(link="logit"))
summary(enact_model_logit_covars_counts_wi)

htmlreg(l=list(enact_model_logit_counts_all, enact_model_logit_covars_counts_all, 
               enact_model_logit_counts_co, enact_model_logit_covars_counts_co,
               enact_model_logit_counts_ne, enact_model_logit_covars_counts_ne,
               enact_model_logit_counts_wi, enact_model_logit_covars_counts_wi), 
        file="tablesi10.doc",
        stars = 0.05,
        custom.model.names = c("All", "All", 
                               "Colorado", "Colorado",
                               "Nebraska", "Nebraska",
                               "Wisconsin", "Wisconsin"),
        custom.coef.map = list("(Intercept)" = "Intercept", 
                               "num_for_before_final" = "Number Lobbying For",
                               "num_against_before_final" = "Number Lobbying Against"
        ),
        omit.coef = "state_session",
        include.aic = FALSE,
        include.bic = FALSE,
        include.loglik = FALSE,
        include.deviance = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with logistic regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are counts of the number of organized interests who take positions in support
        and in opposition to a proposal. The model in the leftmost column pools legislative 
        proposals from Colorado, Nebraska, and Wisconsin, while the remaining three models use
        only data from the state indicated.", 
        caption.above=TRUE,
        caption = "Number of Groups Lobbying and Probability of Enactment (Logit)")
################################################################################
# Table SI.11: Number of Groups and Probability of Enactment (MLM)

enact_model_counts_all <- lmer(enacted ~ num_for_before_final + num_against_before_final + (1|state_session) + (1|state), bill_data)
summary(enact_model_counts_all)

enact_model_counts_co <- lmer(enacted ~ num_for_before_final + num_against_before_final + (1|state_session), 
                              bill_data[which(bill_data$state=="CO")])
summary(enact_model_counts_co)

enact_model_counts_ne <- lmer(enacted ~ num_for_before_final + num_against_before_final + (1|state_session), 
                              bill_data[which(bill_data$state=="NE"),])
summary(enact_model_counts_ne)

enact_model_counts_wi <- lmer(enacted ~ num_for_before_final + num_against_before_final + (1|state_session), 
                              bill_data[which(bill_data$state=="WI"),])
summary(enact_model_counts_wi)

htmlreg(l=list(enact_model_counts_all, enact_model_counts_co, enact_model_counts_ne, enact_model_counts_wi), 
        file="tablesi11.doc",
        stars = 0.05,
        custom.model.names = c("All", "Colorado", "Nebraska", "Wisconsin"),
        custom.coef.map = list("num_for_before_final" = "Number Lobbying For",
                               "num_against_before_final" = "Number Lobbying Against"
        ),
        include.aic = FALSE,
        include.bic = FALSE,
        include.loglik = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        omit.coef = "state_session",
        custom.gof.names = c("Num. obs.", "Num. State-Sessions", "Num. States", 
                             "Var(State-Sessions)", "Var(States)", "Var(Residual)"),
        custom.note = "Models are estimated with multilevel ordinary least squares regression. 
        * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are counts of the number of organized interests who take positions in support
        and in opposition to a proposal.  The model in the leftmost column pools legislative proposals from
        Colorado, Nebraska, and Wisconsin, while the remaining three models use only data from
        the state indicated.  The first model includes varying intercepts for each legislative
        session and for each state (with legislative sessions nested in states).  The remaining
        models include varying intercepts for each legislative session.", 
        caption.above=TRUE,
        caption = "Number of Groups Lobbying and Probability of Enactment (MLM)")

################################################################################
# Table SI.12: Lobbying Patterns and Probability of Enactment (MLM w/ Covariates)

enact_model_counts_covars_all <- lmer(enacted ~ num_for_before_final + num_against_before_final + 
                                        intro_pid + maj_leader_intro + min_leader_intro + 
                                        key_vote + duplicate_bill + chamber_pol + squire + (1|state_session) + (1|state), bill_data)
summary(enact_model_counts_covars_all)

enact_model_counts_covars_co <- lmer(enacted ~ num_for_before_final + num_against_before_final + 
                                       intro_pid + maj_leader_intro + min_leader_intro + 
                                       key_vote + duplicate_bill + chamber_pol + squire + (1|state_session), 
                                     bill_data[which(bill_data$state=="CO")])
summary(enact_model_counts_covars_co)

enact_model_counts_covars_ne <- lmer(enacted ~ num_for_before_final + num_against_before_final + 
                                       intro_pid + maj_leader_intro + 
                                       key_vote + duplicate_bill + chamber_pol + squire + (1|state_session), 
                                     bill_data[which(bill_data$state=="NE"),])
summary(enact_model_counts_covars_ne)

enact_model_counts_covars_wi <- lmer(enacted ~ num_for_before_final + num_against_before_final + 
                                       intro_pid + maj_leader_intro + min_leader_intro + 
                                       key_vote + duplicate_bill + chamber_pol + squire + (1|state_session), 
                                     bill_data[which(bill_data$state=="WI"),])
summary(enact_model_counts_covars_wi)

htmlreg(l=list(enact_model_counts_covars_all, enact_model_counts_covars_co, enact_model_counts_covars_ne, 
               enact_model_counts_covars_wi), 
        file="tablesi12.doc",
        stars = 0.05,
        custom.model.names = c("All", "Colorado", "Nebraska", "Wisconsin"),
        custom.coef.map = list("num_for_before_final" = "Number Lobbying For",
                               "num_against_before_final" = "Number Lobbying Against",
                               "intro_pidMajority" = "Maj. Pty. Sponsor",
                               "intro_pidMinority" = "Min. Pty. Sponsor",
                               "maj_leader_intro" = "Maj. Ldr. Sponsor",
                               "min_leader_intro" = "Min. Ldr. Sponsor",
                               "key_vote" = "PVS Key Vote",
                               "duplicate_bill" = "Duplicate Bill",
                               "chamber_pol" = "Chamber Polarization",
                               "squire" = "Leg. Professionalization"
        ),
        include.aic = FALSE,
        include.bic = FALSE,
        include.loglik = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        omit.coef = "state_session",
        custom.gof.names = c("Num. obs.", "Num. State-Sessions", "Num. States", 
                             "Var(State-Sessions)", "Var(States)", "Var(Residual)"),
        custom.note = "Models are estimated with multilevel ordinary least squares regression. 
        * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are counts of the number of organized interests who take positions in support
        and in opposition to a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The model in the leftmost column pools legislative proposals from
        Colorado, Nebraska, and Wisconsin, while the remaining three models use only data from
        the state indicated.  The first model includes varying intercepts for each legislative
        session and for each state (with legislative sessions nested in states).  The remaining
        models include varying intercepts for each legislative session.", 
        caption.above=TRUE,
        caption = "Number of Groups Lobbying and Probability of Enactment (MLM w/ Covariates)")

################################################################################
# Table SI.13: Number of Groups Lobbying and Probability of Enactment (OLS w/ "Other")

enact_model_logit_counts_other_all <- lm(enacted ~ num_for_before_final + num_against_before_final + num_other_before_final +
                                           as.factor(state_session), bill_data)
summary(enact_model_logit_counts_other_all)

enact_model_logit_counts_other_co <- lm(enacted ~ num_for_before_final + num_against_before_final + num_other_before_final +
                                          as.factor(state_session), bill_data[which(bill_data$state=="CO")])
summary(enact_model_logit_counts_other_co)

enact_model_logit_counts_other_ne <- lm(enacted ~ num_for_before_final + num_against_before_final + num_other_before_final +
                                          as.factor(state_session), bill_data[which(bill_data$state=="NE"),])
summary(enact_model_logit_counts_other_ne)

enact_model_logit_counts_other_wi <- lm(enacted ~ num_for_before_final + num_against_before_final + num_other_before_final +
                                          as.factor(state_session), bill_data[which(bill_data$state=="WI"),])
summary(enact_model_logit_counts_other_wi)

htmlreg(l=list(enact_model_logit_counts_other_all, enact_model_logit_counts_other_co, enact_model_logit_counts_other_ne, enact_model_logit_counts_other_wi), 
        file="tablesi13.doc",
        stars = 0.05,
        custom.model.names = c("All", "Colorado", "Nebraska", "Wisconsin"),
        custom.coef.names = c("Intercept", "Number Lobbying For", "Number Lobbying Against",
                              "Number Lobbying Other"),
        omit.coef = "state_session",
        include.aic = FALSE,
        include.bic = FALSE,
        include.loglik = FALSE,
        include.deviance = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are counts of the number of organized interests who take positions in support
        and in opposition to a proposal, as well as those who take positions neither for nor 
        against the proposal. The model in the leftmost column pools legislative 
        proposals from Colorado, Nebraska, and Wisconsin, while the remaining three models use
        only data from the state indicated.", 
        caption.above=TRUE,
        caption = "Number of Groups Lobbying and Probability of Enactment (OLS w/ Other)")

################################################################################
# Table SI.14: Balance of Group Lobbying and Probability of Enactment (OLS)

enact_model_balance_all <- lm(enacted ~ for_against_balance_before_final +  num_for_against_total_before_final +
                                as.factor(state_session), bill_data)
summary(enact_model_balance_all)

enact_model_balance_co <- lm(enacted ~ for_against_balance_before_final +  num_for_against_total_before_final +
                               as.factor(state_session), bill_data[which(bill_data$state=="CO")])
summary(enact_model_balance_co)

enact_model_balance_ne <- lm(enacted ~ for_against_balance_before_final +  num_for_against_total_before_final +
                               as.factor(state_session), bill_data[which(bill_data$state=="NE"),])
summary(enact_model_balance_ne)

enact_model_balance_wi <- lm(enacted ~ for_against_balance_before_final +  num_for_against_total_before_final +
                               as.factor(state_session), bill_data[which(bill_data$state=="WI"),])
summary(enact_model_balance_wi)

htmlreg(l=list(enact_model_balance_all, enact_model_balance_co, enact_model_balance_ne, enact_model_balance_wi), 
        file="tablesi14.doc",
        stars = 0.05,
        custom.model.names = c("All", "Colorado", "Nebraska", "Wisconsin"),
        custom.coef.names = c("Intercept", "# Groups For - # Groups Against", 
                              "Total # Groups for and Against"),
        omit.coef = "state_session",
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are the balance of the number of organized interests who take positions in support
        and in opposition to a proposal and the sum of organized interests taking positions in
        support of or in opposition to a proposal. The model in the leftmost column pools legislative 
        proposals from Colorado, Nebraska, and Wisconsin, while the remaining three models use
        only data from the state indicated.", 
        caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Enactment (OLS)")

################################################################################
# Table SI.15: Balance of Group Lobbying and Probability of Enactment (OLS w/ Interaction)

enact_model_balance_inter_all <- lm(enacted ~ for_against_balance_before_final +  num_for_against_total_before_final +
                                      for_against_balance_before_final:num_for_against_total_before_final +
                                      as.factor(state_session), bill_data)
summary(enact_model_balance_inter_all)

enact_model_balance_inter_co <- lm(enacted ~ for_against_balance_before_final +  num_for_against_total_before_final +
                                     for_against_balance_before_final:num_for_against_total_before_final +
                                     as.factor(state_session), bill_data[which(bill_data$state=="CO")])
summary(enact_model_balance_inter_co)

enact_model_balance_inter_ne <- lm(enacted ~ for_against_balance_before_final +  num_for_against_total_before_final +
                                     for_against_balance_before_final:num_for_against_total_before_final +
                                     as.factor(state_session), bill_data[which(bill_data$state=="NE"),])
summary(enact_model_balance_inter_ne)

enact_model_balance_inter_wi <- lm(enacted ~ for_against_balance_before_final +  num_for_against_total_before_final +
                                     for_against_balance_before_final:num_for_against_total_before_final +
                                     as.factor(state_session), bill_data[which(bill_data$state=="WI"),])
summary(enact_model_balance_inter_wi)

htmlreg(l=list(enact_model_balance_inter_all, enact_model_balance_inter_co, enact_model_balance_inter_ne, enact_model_balance_inter_wi), 
        file="tablesi15.doc",
        stars = 0.05,
        custom.model.names = c("All", "Colorado", "Nebraska", "Wisconsin"),
        custom.coef.names = c("Intercept", "# Groups For - # Groups Against", 
                              "Total # Groups for and Against",
                              "# Groups For - # Groups Against:Total # Groups for and Against"),
        omit.coef = "state_session",
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal is enacted into law. The explanatory 
        variables are the balance of the number of organized interests who take positions in support
        and in opposition to a proposal and the sum of organized interests taking positions in
        support of or in opposition to a proposal. The model in the leftmost column pools legislative 
        proposals from Colorado, Nebraska, and Wisconsin, while the remaining three models use
        only data from the state indicated.", 
        caption.above=TRUE,
        caption = "Balance of Group Lobbying and Probability of Enactment (OLS w/Interaction)")

################################################################################
# Table SI.16: Lobbying Patterns and Probability of Legislative Advancement (OLS w/Covars)

htmlreg(l=list(pass_committee_model_covars_wi, reach_floor_model_covars_wi, pass_chamber_model_covars_wi), 
        file="tablesi16.doc",
        stars = 0.05,
        custom.model.names = c("Passed Committee", "Reached Floor", "Passed Chamber"),
        custom.coef.map = list("(Intercept)" = "Intercept", 
                               "for_against_passfor and against" = "Lobbying For and Against",
                               "for_against_passonly against" = "Only Lobbying Against", 
                               "for_against_passonly for" = "Only Lobbying For",
                               "for_against_floorfor and against" = "Lobbying For and Against",
                               "for_against_flooronly against" = "Only Lobbying Against", 
                               "for_against_flooronly for" = "Only Lobbying For",
                               "for_against_committeefor and against" = "Lobbying For and Against",
                               "for_against_committeeonly against" = "Only Lobbying Against", 
                               "for_against_committeeonly for" = "Only Lobbying For",
                               "intro_pidMajority" = "Maj. Pty. Sponsor",
                               "intro_pidMinority" = "Min. Pty. Sponsor",
                               "maj_leader_intro" = "Maj. Ldr. Sponsor",
                               "min_leader_intro" = "Min. Ldr. Sponsor",
                               "key_vote" = "PVS Key Vote",
                               "duplicate_bill" = "Duplicate Bill"
        ),
        omit.coef = "state_session",
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with logistic regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal reached the stage of the legislative
        process indicated by the column heading. The explanatory 
        variables are coded as dichotomous indicators for whether organized interests 
        officially take positions only in support of a proposal, only against a proposal, 
        or both for and against a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The models pool legislative proposals from
        Colorado and Wisconsin; legislative proposals from Nebraska are excluded because lobbying
        reports in that state are only filed once, after the conclusion of the legislative session,
        and contain no information about the timing of lobbying activity.", caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Legislative Advancement (Logit)")

################################################################################
# Table SI.17: Lobbying Patterns and Probability of Legislative Advancement (Logit)

pass_chamber_model_wi <- glm(passed_chamber ~ for_against_pass  +  as.factor(state_session), 
                             bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(pass_chamber_model_wi)

reach_floor_model_wi <- glm(reached_floor ~ for_against_floor + as.factor(state_session), 
                            bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(reach_floor_model_wi)

pass_committee_model_wi <- glm(passed_committee ~ for_against_committee + as.factor(state_session), 
                               bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(pass_committee_model_wi)

pass_chamber_model_covars_wi <- glm(passed_chamber ~ for_against_pass + intro_pid + maj_leader_intro + 
                                      min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                    bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(pass_chamber_model_covars_wi)

reach_floor_model_covars_wi <- glm(reached_floor ~ for_against_floor + intro_pid + maj_leader_intro + 
                                     min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                   bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(reach_floor_model_covars_wi)

pass_committee_model_covars_wi <- glm(passed_committee ~ for_against_committee + intro_pid + maj_leader_intro + 
                                        min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                      bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(pass_committee_model_covars_wi)

htmlreg(l=list(pass_committee_model_wi, pass_committee_model_covars_wi,
               reach_floor_model_wi, reach_floor_model_covars_wi,
               pass_chamber_model_wi, pass_chamber_model_covars_wi), 
        file="tablesi17.doc",
        stars = 0.05,
        custom.model.names = c("Passed Committee", "Passed Committee",
                               "Reached Floor", "Reached Floor",
                               "Passed Chamber", "Passed Chamber"),
        custom.coef.map = list( "for_against_passfor and against" = "Lobbying For and Against",
                                "for_against_passonly against" = "Only Lobbying Against", 
                                "for_against_passonly for" = "Only Lobbying For",
                                "for_against_floorfor and against" = "Lobbying For and Against",
                                "for_against_flooronly against" = "Only Lobbying Against", 
                                "for_against_flooronly for" = "Only Lobbying For",
                                "for_against_committeefor and against" = "Lobbying For and Against",
                                "for_against_committeeonly against" = "Only Lobbying Against", 
                                "for_against_committeeonly for" = "Only Lobbying For"
        ),
        omit.coef = "state_session",
        include.aic = FALSE,
        include.bic = FALSE,
        include.loglik = FALSE,
        include.deviance = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with logistic regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal reached the stage of the legislative
        process indicated by the column heading. The explanatory 
        variables are coded as dichotomous indicators for whether organized interests 
        officially take positions only in support of a proposal, only against a proposal, 
        or both for and against a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The models pool legislative proposals from
        Colorado and Wisconsin; legislative proposals from Nebraska are excluded because lobbying
        reports in that state are only filed once, after the conclusion of the legislative session,
        and contain no information about the timing of lobbying activity.", caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Legislative Advancement (Logit)")

###############################################################################
# Table SI18: Number of Groups Lobbying and Probability of Legislative Advancement (WI only)

htmlreg(l=list(pass_committee_counts_covars_model_wi, 
               reach_floor_counts_covars_model_wi, 
               pass_chamber_counts_covars_model_wi), 
        file="tablesi18.doc",
        stars = 0.05,
        custom.model.names = c("Passed Committee", 
                               "Reached Floor",
                               "Passed Chamber"),
        custom.coef.map = list("num_for_before_committee"="Number Lobbying For",
                               "num_against_before_committee"="Number Lobbying Against",
                               "num_for_before_floor"="Number Lobbying For",
                               "num_against_before_floor"="Number Lobbying Against",
                               "num_for_before_pass"="Number Lobbying For",
                               "num_against_before_pass"="Number Lobbying Against",
                               "intro_pidMajority" = "Maj. Pty. Sponsor",
                               "intro_pidMinority" = "Min. Pty. Sponsor",
                               "maj_leader_intro" = "Maj. Ldr. Sponsor",
                               "min_leader_intro" = "Min. Ldr. Sponsor",
                               "key_vote" = "PVS Key Vote",
                               "duplicate_bill" = "Duplicate Bill"
        ),
        omit.coef = "state_session",
        include.rmse = FALSE,
        include.adjrs = FALSE,
        include.rsquared = FALSE,
        custom.note = "Models are estimated with ordinary least squares regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal reached the stage of the legislative
        process indicated by the column heading. The explanatory 
        variables are counts of the number of organized interests who take positions in support of and in opposition
        to a proposal.  The models include only legislative proposals from Wisconsin.", caption.above=TRUE,
        caption = "Number of Groups Lobbying and Probability of Legislative Advancement (OLS)")

################################################################################
# Table SI.19: Number of Groups and Probability of Legislative Advancement (Logit)

pass_chamber_model_wi <- glm(passed_chamber ~ num_for_before_pass + num_against_before_pass + as.factor(state_session), 
                             bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(pass_chamber_model_wi)

reach_floor_model_wi <- glm(reached_floor ~ num_for_before_floor + num_against_before_floor + as.factor(state_session), 
                            bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(reach_floor_model_wi)

pass_committee_model_wi <- glm(passed_committee ~ num_for_before_committee + num_against_before_committee + as.factor(state_session), 
                               bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(pass_committee_model_wi)

pass_chamber_model_covars_wi <- glm(passed_chamber ~ num_for_before_pass + num_against_before_pass + 
                                      intro_pid + maj_leader_intro + 
                                      min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                    bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(pass_chamber_model_covars_wi)

reach_floor_model_covars_wi <- glm(reached_floor ~ num_for_before_floor + num_against_before_floor + 
                                     intro_pid + maj_leader_intro + 
                                     min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                   bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(reach_floor_model_covars_wi)

pass_committee_model_covars_wi <- glm(passed_committee ~ num_for_before_committee + num_against_before_committee + 
                                        intro_pid + maj_leader_intro + 
                                        min_leader_intro + key_vote + duplicate_bill + as.factor(state_session), 
                                      bill_data[which(bill_data$state=="WI"),], family = binomial(link="logit"))
summary(pass_committee_model_covars_wi)

htmlreg(l=list(pass_committee_model_wi, pass_committee_model_covars_wi,
               reach_floor_model_wi, reach_floor_model_covars_wi,
               pass_chamber_model_wi, pass_chamber_model_covars_wi), 
        file="tablesi19.doc",
        stars = 0.05,
        custom.model.names = c("Passed Committee", "Passed Committee",
                               "Reached Floor", "Reached Floor",
                               "Passed Chamber", "Passed Chamber"),
        custom.coef.map = list("num_for_before_committee"="Number Lobbying For",
                               "num_against_before_committee"="Number Lobbying Against",
                               "num_for_before_floor"="Number Lobbying For",
                               "num_against_before_floor"="Number Lobbying Against",
                               "num_for_before_pass"="Number Lobbying For",
                               "num_against_before_pass"="Number Lobbying Against"
        ),
        omit.coef = "state_session",
        include.aic = FALSE,
        include.bic = FALSE,
        include.loglik = FALSE,
        include.deviance = FALSE,
        inline.css = FALSE,
        doctype = TRUE,
        html.tag = TRUE,
        head.tag = TRUE,
        body.tag = TRUE,
        custom.note = "Models are estimated with logistic regression. * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislative proposal reached the stage of the legislative
        process indicated by the column heading. The explanatory 
        variables are coded as dichotomous indicators for whether organized interests 
        officially take positions only in support of a proposal, only against a proposal, 
        or both for and against a proposal. Proposals in the omitted category are those 
        proposals on which no organized interest takes an official position for or against 
        the proposal.  The models pool legislative proposals from
        Colorado and Wisconsin; legislative proposals from Nebraska are excluded because lobbying
        reports in that state are only filed once, after the conclusion of the legislative session,
        and contain no information about the timing of lobbying activity.", caption.above=TRUE,
        caption = "Lobbying Patterns and Probability of Legislative Advancement (Logit)")

################################################################################
# Table SI.20: Lobbying Patterns and Vote Switching (MLM OLS)

# creating unique identifiers for bills and sessions

vote_switching_data$bill_id <- paste0(vote_switching_data$bill_type, vote_switching_data$bill_number)
vote_switching_data$bill_session_id <- paste0(vote_switching_data$session, vote_switching_data$bill_type, vote_switching_data$bill_number)

floorvote_outcome_mlm <- lmer(floor_stage_vote ~ committee_stage_vote + type_lobbying +
                                (1|legislator) + (1|bill_session_id) + (1|session), 
                              data=vote_switching_data)
summary(floorvote_outcome_mlm)

htmlreg(l=list(floorvote_outcome_mlm), 
        file="tablesi20.doc",
        stars = 0.05,
        custom.model.names = c("Model 1"),
        custom.coef.names = c("Intercept",
                              "Committee Vote-Yes",
                              "Pre-Against, Post-For", "Pre-Against, Post-None",
                              "Pre-Both, Post-None", "Pre-For, Post-Against",
                              "Pre-For, Post-None", "Pre-None, Post-Against",
                              "Pre-None, Post-For"),
        include.aic = FALSE,
        include.bic = FALSE,
        include.loglik = FALSE,
        custom.gof.names = c("Num. obs.", "Num. Legislators", "Num. Bills", "Num. Sessions",
                             "Var(Legislators)", "Var(Bills)", "Var(Sessions)", "Var(Residual)"),
        custom.note = "Models are estimated with multilevel ordinary least squares regression. 
        * denotes
        statistical significance at the p&lt;0.05 level. The dependent variable is a dichotomous 
        indicator for whether a legislator voted for a given legislative proposal on the floor.
        The explanatory variables include a dichotomous indicator for whether a legislator
        voted for a given legislative proposal in committee and a series of dichotomous
        indicators specifying the timing and directionality of lobbying activity on the
        proposal.  The model includes varying intercepts for each legislator, each bill, and
        each legislative session.", 
        caption.above=TRUE,
        caption = "Lobbying Patterns and Vote Switching (MLM OLS)")
