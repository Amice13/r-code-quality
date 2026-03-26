#######################################################
#######################################################
###                                                 ###
### Linking Individual Electoral Performance to the ###
###          Composition of Elected Bodies:         ###
###         A Counterfactual-Based Approach         ###
###                                                 ###
#######################################################
#######################################################

# Author: Oliver Huwyler
# Date: 2024-03-06

# Purpose: This script replicates the figures and tables from
# a) the paper
# b) the appendix



########################
# Getting Started in R #
########################

# Change the language and date formatting to English if it is not already
Sys.setenv(LANG = "EN")
Sys.setlocale("LC_TIME", "English") # key, without this conversion to POSIXct data format does not work
Sys.getlocale(category = "LC_ALL")

# Set working directory
# setwd("")



# Install libraries if they have not been installed yet
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
if("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")}
if("xlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("xlsx")}
if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")}
if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
if("mgsub" %in% rownames(installed.packages()) == FALSE) {install.packages("mgsub")}
if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}
if("miceadds" %in% rownames(installed.packages()) == FALSE) {install.packages("miceadds")}
if("splitstackshape" %in% rownames(installed.packages()) == FALSE) {install.packages("splitstackshape")}
if("vtable" %in% rownames(installed.packages()) == FALSE) {install.packages("vtable")}
if("sjPlot" %in% rownames(installed.packages()) == FALSE) {install.packages("sjPlot")}
if("texreg" %in% rownames(installed.packages()) == FALSE) {install.packages("texreg")}
if("htmlTable" %in% rownames(installed.packages()) == FALSE) {install.packages("htmlTable")}
if("performance" %in% rownames(installed.packages()) == FALSE) {install.packages("performance")}



# Load libraries
library(stringr)
library(gtools)
library(readxl)
library(xlsx)
library(gtools)
library(sqldf)
library(zoo)
library(tidyverse)
library(mgsub)
library(data.table)
library(miceadds)
library(splitstackshape)
library(vtable)
library(sjPlot)
library(texreg)
library(htmlTable)
library(performance)



####################################
####################################
# Load the Data Frame for Analysis #
####################################
####################################


# Load the data on the 870 candidates
LUCANDS <- readRDS("./replication_package/Lucerne_Candidates_2023.rds")

# Note: The unit of analysis in the data are individual candidates.
#       Each candidate occurs only once.


###########################################
###########################################
# Categorical Variables: Define Baselines #
###########################################
###########################################


# district_id
LUCANDS$district_id <- as.factor(LUCANDS$district_id)
LUCANDS <- within(LUCANDS, district_id <- relevel(district_id, ref = "CHE_LUN_REG20_LU_19jun2023__Luzern-Stadt"))

# party_id
LUCANDS$party_id <- as.factor(LUCANDS$party_id)
LUCANDS <- within(LUCANDS, party_id <- relevel(party_id, ref = "CHE_Mitte_NAT"))

# list_type
LUCANDS$list_type <- as.factor(LUCANDS$list_type)
LUCANDS <- within(LUCANDS, list_type <- relevel(list_type, ref = "PTY")) # PTY = party proper

# gender
LUCANDS$gender <- as.factor(LUCANDS$gender)
LUCANDS <- within(LUCANDS, gender <- relevel(gender, ref = "m"))

# gender2
LUCANDS$gender2 <- as.factor(LUCANDS$gender2)
LUCANDS <- within(LUCANDS, gender2 <- relevel(gender2, ref = "not female"))

# prof_org_act
LUCANDS$prof_org_act <- as.factor(LUCANDS$prof_org_act)
LUCANDS <- within(LUCANDS, prof_org_act <- relevel(prof_org_act, ref = "WOR"))

# prof_org_act2
LUCANDS$prof_org_act2 <- as.factor(LUCANDS$prof_org_act2)
LUCANDS <- within(LUCANDS, prof_org_act2 <- relevel(prof_org_act2, ref = "WOR"))

# precumul1
LUCANDS$precumul1 <- as.factor(LUCANDS$precumul1)
LUCANDS <- within(LUCANDS, precumul1 <- relevel(precumul1, ref = "no precumul cands on list"))

# precumul2
LUCANDS$precumul2 <- as.factor(LUCANDS$precumul2)
LUCANDS <- within(LUCANDS, precumul2 <- relevel(precumul2, ref = "not pre-cumulated"))

# ballot_pos_last1
LUCANDS$ballot_pos_last1 <- as.factor(LUCANDS$ballot_pos_last1)
LUCANDS <- within(LUCANDS, ballot_pos_last1 <- relevel(ballot_pos_last1, ref = "not last position"))

# ballot_pos_after_non_precumul_incmb
LUCANDS$ballot_pos_after_non_precumul_incmb <- as.factor(LUCANDS$ballot_pos_after_non_precumul_incmb)
LUCANDS <- within(LUCANDS, ballot_pos_after_non_precumul_incmb <- relevel(ballot_pos_after_non_precumul_incmb, ref = "no"))

# locpuboff_highest_now
LUCANDS$locpuboff_highest_now <- as.factor(LUCANDS$locpuboff_highest_now)
LUCANDS <- within(LUCANDS, locpuboff_highest_now <- relevel(locpuboff_highest_now, ref = "0 - no local public office"))

# locpuboff_highest_now2
LUCANDS$locpuboff_highest_now2 <- as.factor(LUCANDS$locpuboff_highest_now2)
LUCANDS <- within(LUCANDS, locpuboff_highest_now2 <- relevel(locpuboff_highest_now2, ref = "0 - no local public office"))


# locpuboff_highest_now
LUCANDS$locpuboff_highest_now <- as.factor(LUCANDS$locpuboff_highest_now)
LUCANDS <- within(LUCANDS, locpuboff_highest_now <- relevel(locpuboff_highest_now, ref = "0 - no local public office"))

# regparl_incumb
LUCANDS$regparl_incumb <- as.factor(LUCANDS$regparl_incumb)
LUCANDS <- within(LUCANDS, regparl_incumb <- relevel(regparl_incumb, ref = "challenger"))

# locptyoff_highest_now
LUCANDS$locptyoff_highest_now <- as.factor(LUCANDS$locptyoff_highest_now)
LUCANDS <- within(LUCANDS, locptyoff_highest_now <- relevel(locptyoff_highest_now, ref = "0 - no role in party"))

# regptyoff_highest_now
LUCANDS$regptyoff_highest_now <- as.factor(LUCANDS$regptyoff_highest_now)
LUCANDS <- within(LUCANDS, regptyoff_highest_now <- relevel(regptyoff_highest_now, ref = "0 - no role in party"))

# natptyoff_highest_now
LUCANDS$natptyoff_highest_now <- as.factor(LUCANDS$natptyoff_highest_now)
LUCANDS <- within(LUCANDS, natptyoff_highest_now <- relevel(natptyoff_highest_now, ref = "0 - no role in party"))

# locptyoff_any
LUCANDS$locptyoff_any <- as.factor(LUCANDS$locptyoff_any)
LUCANDS <- within(LUCANDS, locptyoff_any <- relevel(locptyoff_any, ref = "0 - no role in party"))

# regptyoff_any
LUCANDS$regptyoff_any <- as.factor(LUCANDS$regptyoff_any)
LUCANDS <- within(LUCANDS, regptyoff_any <- relevel(regptyoff_any, ref = "0 - no role in party"))


###############
###############
### Table 2 ###
###############
###############


# Variable overview and descriptives

st(LUCANDS, 
   vars = c("total_votes_zlist", "total_votes_pct", "elected_status",
            "age","gender2", "non_swiss", "prof_org_act2", "locpuboff_highest_now2", "regparl_incumb",
            "locptyoff_any", "regptyoff_any",
            "ig_formalties_eco_locreg", "ig_formalties_fir_locreg", "ig_formalties_hob_locreg",
            "ig_formalties_ide_locreg", "ig_formalties_pig_locreg", 
            "precumul2", "ballot_pos_rel1"),
   summ = c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
   summ.names = c('N','Mean','Std. Dev.','Min','Max'),
   out = "viewer")



###############
###############
### Table 3 ###
###############
###############


# Full model for total votes 
m1tot <- lm.cluster(total_votes_zlist ~ age + gender2 + non_swiss + prof_org_act2 + # election_budget +
                      locpuboff_highest_now2 + regparl_incumb +
                      locptyoff_any + regptyoff_any +
                      ig_formalties_eco_locreg + ig_formalties_fir_locreg + ig_formalties_hob_locreg + 
                      ig_formalties_ide_locreg + ig_formalties_pig_locreg +
                      precumul2 + ballot_pos_rel1 +
                      list_id,
                    cluster = 'list_id',
                    data = LUCANDS)

m1totcoeffs <- data.frame(summary(m1tot))
m1totcoeffs[!startsWith(row.names(m1totcoeffs), 'list_id'),]


# Full model for share of preference votes
m1share <- lm.cluster(total_votes_pct ~ age + gender2 + non_swiss + prof_org_act2 + # election_budget +
                      locpuboff_highest_now2 + regparl_incumb +
                      locptyoff_any + regptyoff_any +
                      ig_formalties_eco_locreg + ig_formalties_fir_locreg + ig_formalties_hob_locreg + 
                      ig_formalties_ide_locreg + ig_formalties_pig_locreg +
                      precumul2 + ballot_pos_rel1 +
                      list_id,
                    cluster = 'list_id',
                    data = LUCANDS)

m1sharecoeffs <- data.frame(summary(m1share))
m1sharecoeffs[!startsWith(row.names(m1sharecoeffs), 'list_id'),]



# Full model for being elected (0/1)
m1elec <- glm.cluster(elected_status ~ age + gender2 + non_swiss + prof_org_act2 + # election_budget +
                      locpuboff_highest_now2 + regparl_incumb +
                      locptyoff_any + regptyoff_any +
                      ig_formalties_eco_locreg + ig_formalties_fir_locreg + ig_formalties_hob_locreg + 
                      ig_formalties_ide_locreg + ig_formalties_pig_locreg +
                      precumul2 + ballot_pos_rel1 +
                      list_id,
                    cluster = 'list_id',
                    family = 'binomial',
                    data = LUCANDS)

m1eleccoeffs <- data.frame(summary(m1elec))
m1eleccoeffs[!startsWith(row.names(m1eleccoeffs), 'list_id'),]

# Pseudo R2
r2_nagelkerke(m1elec$glm_res)

# Export models to html for easy copying to word document
htmlreg(list(m1tot, m1share, m1elec), 
        single.row = 1, # SEs on same line as coeffs
        digits = 3,
        custom.header = list("Electoral performance" = 1:3),
        custom.model.names = c("Total preference votes", "Percentage of preference votes", "Seat won"),
        custom.coef.map = list("age" = "Age", "gender2female" = "Female", "non_swiss1" = "Non-Swiss family name",
                               "prof_org_act2NOT WOR" = "Not working a regular job",
                               "locpuboff_highest_now21 - appointed local public office" = "Appointed local public office",
                               "locpuboff_highest_now22 - elected local public office" = "Elected local public office",
                               "regparl_incumbincumbent" = "Incumbent in regional parliament",
                               "locptyoff_any1 - role in party" = "Role in local party",
                               "regptyoff_any1 - role in party" = "Role in regional party",
                               "ig_formalties_eco_locreg" = "No. of formal ties: economic interest groups",
                               "ig_formalties_fir_locreg" = "No. of formal ties: firms",
                               "ig_formalties_hob_locreg" = "No. of formal ties: hobby and leisure groups",
                               "ig_formalties_ide_locreg" = "No. of formal ties: identity groups",
                               "ig_formalties_pig_locreg" = "No. of formal ties: public interest groups",
                               "precumul2pre-cumulated" = "Pre-cumulated on list",
                               "ballot_pos_rel1" = "Relative list position"
                               ,"(Intercept)" = "Intercept"),
        custom.gof.rows = list("List fixed effects" = c("yes", "yes", "yes")),
        custom.gof.map = list("R$^2$" = "R$^2$", "Adj. R$^2$" = "Adj. R$^2$"),
        include.nobs = FALSE,
        custom.note = "Notes: %stars. N = 870.",
        file = paste(getwd(),"/Table3_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".html", sep = ""))



################
################
### Figure 1 ###
################
################


# 1) Define the variables for which we want to obtain counterfactuals
cfvars <- c("gender2", "non_swiss", "prof_org_act2", "locpuboff_highest_now2", "regparl_incumb",
            "locptyoff_any", "regptyoff_any",
            "ig_formalties_eco_locreg", "ig_formalties_fir_locreg", "ig_formalties_hob_locreg",
            "ig_formalties_ide_locreg", "ig_formalties_pig_locreg", 
            "precumul2", "ballot_pos_rel1"
)


# 2) Generate the "true" predictions, i.e. based on the original input data
PREDTRUE <- data.frame(predict(m1tot$lm_res, # the lm object
                               vcov = m1tot$vcov, # the variance-covariance matrix
                               newdata = LUCANDS, # the input data for prediction
                               interval = 'confidence')) # also estimate a confidence interval


# 3) Calculate counterfactual predictions and store in a df what the counterfactual condition was
CFPREDS <-list()

CFCONDS <- data.frame(matrix(nrow = length(cfvars), ncol = 2))
colnames(CFCONDS) <- c("variable", "cf_cond")

for (i in 1:length(cfvars))
{
  pb <- txtProgressBar(min = 1, max = length(cfvars), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Find the variable position of the variable to manipulate in LUCANDS
  varpos <- grep(cfvars[i], colnames(LUCANDS))
  
  # 2) Get LUCANDS and store a copy
  LUCANDSMOD <- get("LUCANDS")
  
  # 3) Alter the respective variable
  if(cfvars[i] == "gender2"){
    LUCANDSMOD[,varpos] <- "not female"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "not female"
  }
  
  if(cfvars[i] == "non_swiss"){
    LUCANDSMOD[,varpos] <- "0"
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "prof_org_act2"){
    LUCANDSMOD[,varpos] <- "NOT WOR"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "NOT WOR"
  }
  
  if(cfvars[i] == "locpuboff_highest_now2"){
    LUCANDSMOD[,varpos] <- "0 - no local public office"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0 - no local public office"
  }
  
  if(cfvars[i] == "regparl_incumb"){
    LUCANDSMOD[,varpos] <- "challenger"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "challenger"
  }
  
  if(cfvars[i] == "locptyoff_any"){
    LUCANDSMOD[,varpos] <- "0 - no role in party"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0 - no role in party"
  }
  
  if(cfvars[i] == "regptyoff_any"){
    LUCANDSMOD[,varpos] <- "0 - no role in party"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0 - no role in party"
  }
  
  if(cfvars[i] == "ig_formalties_eco_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_fir_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_hob_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_ide_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_pig_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "precumul2"){
    LUCANDSMOD[,varpos] <- "not pre-cumulated"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "not pre-cumulated"
  }
  
  if(cfvars[i] == "ballot_pos_rel1"){
    LUCANDSMOD[,varpos] <- 1
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "1"
  }
  
  # 4) Calculate the counterfactual prediction
  TEMPPRED <- data.frame(predict(m1tot$lm_res, # the lm object
                                 vcov = m1tot$vcov, # the variance-covariance matrix
                                 newdata = LUCANDSMOD, # the input data for prediction
                                 interval = 'confidence')) # also estimate a confidence interval
  
  
  # 5) Add the predictions from TEMPPRED to the list CFPREDS
  CFPREDS[[toupper(cfvars[i])]] <- TEMPPRED
  rm(TEMPPRED,LUCANDSMOD)
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 4) Calculate the difference between the true and the counterfactual predictions
PREDTRUECFDIFF <-  data.frame(matrix(nrow = nrow(LUCANDS), ncol = 0)) 

for (i in 1:length(CFPREDS))
{
  pb <- txtProgressBar(min = 1, max = length(CFPREDS), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Calculate the difference between the true prediction and the counterfactual prediction
  PREDTRUECFDIFF$newdiff <- PREDTRUE$fit - CFPREDS[[i]]$fit
  
  # 2) Rename that column
  colnames(PREDTRUECFDIFF)[colnames(PREDTRUECFDIFF) == 'newdiff'] <- paste(tolower(names(CFPREDS)[i]), "_trcfdiff", sep="")
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 5) Calculate the final predicted (counterfactual) score
PREDFINAL <-  data.frame(matrix(nrow = nrow(LUCANDS), ncol = 0)) 

for (i in 1:ncol(PREDTRUECFDIFF))
{
  pb <- txtProgressBar(min = 1, max = ncol(PREDTRUECFDIFF), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Calculate the difference between the true prediction and the counterfactual prediction
  PREDFINAL$newfinal <- LUCANDS$total_votes_zlist + (-1)*PREDTRUECFDIFF[,i]
  
  # 2) Rename that column
  colnames(PREDFINAL)[colnames(PREDFINAL) == 'newfinal'] <- paste(cfvars[i], "_predfinal", sep="")
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 6) Calculate whether the candidate would have been elected with the final predicted (counterfactual) number of votes
# a) Store the information in an overview
toselect <- c(c("pers_id","list_id", "total_votes_zlist", "elected_status"), cfvars)
PREDELEC <- subset(LUCANDS, select = toselect)
rm(toselect)

for (i in 1:ncol(PREDFINAL))
{
  pb <- txtProgressBar(min = 1, max = ncol(PREDFINAL), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Create an overview of how many candidates per list were elected.
  # a) An overview:
  LISTELEC <- sqldf("SELECT DISTINCT LUCANDS.list_id, LUCANDS.list_seats_won
                        FROM LUCANDS
                  ")
  
  # b) Remove lists that did not win any seat
  LISTELEC <- LISTELEC[which(LISTELEC$list_seats_won != 0),]
  
  # c) Get all number in a certain range
  LISTELEC$winner_pos <- lapply(LISTELEC$list_seats_won, function(x) paste(seq.int(1,x, by = 1), collapse=";"))
  LISTELEC$winner_pos <- as.character(LISTELEC$winner_pos)
  
  # d) Split winner_pos into multiple columns
  LISTELEC <- cSplit(LISTELEC, "winner_pos", ";")
  
  # e) Move to long format
  LISTELEC <- gather(LISTELEC, variable, winner_pos, winner_pos_1:winner_pos_7, factor_key=TRUE)
  
  # f) Keep only relevant rows (those that are not NA on winner_pos)
  LISTELEC <- LISTELEC[which(!is.na(LISTELEC$winner_pos)),]
  LISTELEC$elected <- 1
  
  # 2) Create another helper df where we generate the rank order on the list based on the values in PREDFINAL
  RANKS <- data.frame(LUCANDS$pers_id, LUCANDS$list_id, PREDFINAL[,i])
  colnames(RANKS) <- c("pers_id", "list_id", "predfinal_votes")
  
  # 3) Create a rank order per list
  # a) Order by list_id and then the number of votes
  RANKS <- RANKS[order(RANKS$list_id,RANKS$predfinal_votes),]
  
  # b) Invert the row order, so we have a descending number of votes
  RANKS<- RANKS[seq(dim(RANKS)[1],1),]
  
  # c) Add a rank order
  RANKS$rank_order <- ave(RANKS$predfinal_votes, RANKS$list_id, FUN = seq_along)
  
  
  # 4) Add information on whether someone would have one a seat to RANKS
  RANKS <- sqldf("SELECT RANKS.*, LISTELEC.elected
                 FROM RANKS LEFT JOIN LISTELEC
                 ON
                 (RANKS.list_id = LISTELEC.list_id)
                 AND
                 (RANKS.rank_order = LISTELEC.winner_pos)
                 ")
  
  # Set NAs to 0
  RANKS[which(is.na(RANKS$elected)),]$elected <- 0
  
  # 5) Add information from PREDFINAL and RANKS to PREDELC
  # a) The predicted number of votes (z-standardised) from PREDFINAL
  PREDELEC$predfinal <- PREDFINAL[,i]
  colnames(PREDELEC)[ncol(PREDELEC)] <- colnames(PREDFINAL)[i]
  
  # b) The elected status from RANK
  PREDELEC <- sqldf("SELECT PREDELEC.*, RANKS.elected
                 FROM PREDELEC LEFT JOIN RANKS
                 ON
                 (PREDELEC.list_id = RANKS.list_id)
                 AND
                 (PREDELEC.pers_id = RANKS.pers_id)
                 ")
  
  colnames(PREDELEC)[ncol(PREDELEC)] <- paste(cfvars[i], "_predelected", sep="")
  
  
  # 6) Remove dfs
  rm(RANKS, LISTELEC)
  
  
  setTxtProgressBar(pb, i)
}
close(pb)



# 7) Count how often different candidates would (not) have been elected.
PREDOVERVIEW <-  data.frame(matrix(nrow = length(cfvars), ncol = 7))
colnames(PREDOVERVIEW) <- c("variable", "real_cond", "cf_cond",
                            "elect_1_predelect_1", "elect_1_predelect_0", "elect_0_predelect_1", "elect_0_predelect_0")

for (i in 1:length(cfvars))
{
  pb <- txtProgressBar(min = 1, max = length(cfvars), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Define the relevant predelected column in PREDELEC
  varname <- paste(cfvars[i], "_predelected", sep="") # varname to search
  varpos <- grep(varname, colnames(PREDELEC))
  
  # 2) Count the number of cases for elect_1_predelect_1 (elected in actuality, predicted to be elected by the counterfactual model)
  PREDOVERVIEW$elect_1_predelect_1[i] <- nrow(PREDELEC[which(PREDELEC$elected_status == 1 & PREDELEC[,varpos] == 1),])
  
  # 3) Count the number of cases for elect_1_predelect_0 (elected in actuality, but not elected in the counterfactual model)
  PREDOVERVIEW$elect_1_predelect_0[i] <- nrow(PREDELEC[which(PREDELEC$elected_status == 1 & PREDELEC[,varpos] == 0),])
  
  # 4) Count the number of cases for elect_0_predelect_1 (not actually elected, predicted to be elected by the counterfactual model)
  PREDOVERVIEW$elect_0_predelect_1[i] <- nrow(PREDELEC[which(PREDELEC$elected_status == 0 & PREDELEC[,varpos] == 1),])
  
  # 5) Count the number of cases for elect_0_predelect_0 (not actually elected, and not elected in the counterfactual model)
  PREDOVERVIEW$elect_0_predelect_0[i] <- nrow(PREDELEC[which(PREDELEC$elected_status == 0 & PREDELEC[,varpos] == 0),])
  
  # 6) The variable in question  
  PREDOVERVIEW$variable[i] <- cfvars[i]
  
  
  setTxtProgressBar(pb, i)
}
close(pb)




# 8) Add information on conditions (real and counterfactual)

for (i in 1:nrow(PREDOVERVIEW))
{
  pb <- txtProgressBar(min = 1, max = nrow(PREDOVERVIEW), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Fill in the counterfactual condition
  PREDOVERVIEW$cf_cond[i] <- paste(CFCONDS$cf_cond[i], ": ", nrow(LUCANDS), sep="")
  
  # 2) For (regular) categorical variables, use tabled information
  if(grepl("^(gender2|non_swiss|prof_org_act2|locpuboff_highest_now2|regparl_incumb|locptyoff_any|regptyoff_any|precumul2)$", PREDOVERVIEW$variable[i])){
    # a) Get variable name
    varname <- PREDOVERVIEW$variable[i]
    
    # b) Find the variable position in LUCANDS
    varpos <- grep(varname, colnames(LUCANDS))
    
    # c) Table the variable
    TABLE <- data.frame(table(LUCANDS[,varpos]))
    
    # d) Obtain a string
    PREDOVERVIEW$real_cond[i] <- paste(paste(TABLE$Var1, ": ", TABLE$Freq, sep=""), collapse="\n")
  }
  
  # 3) For continuous measures on IG ties, calculate the share of zeroes and above-zeroes
  if(grepl("^(ig_formalties_eco_locreg|ig_formalties_fir_locreg|ig_formalties_hob_locreg|ig_formalties_ide_locreg|ig_formalties_pig_locreg)$", PREDOVERVIEW$variable[i])){
    # a) Get variable name
    varname <- PREDOVERVIEW$variable[i]
    
    # b) Find the variable position in LUCANDS
    varpos <- grep(varname, colnames(LUCANDS)) 
    
    # c) Extract how many IG ties are 0 or above
    PREDOVERVIEW$real_cond[i] <- 
      paste("ties = 0: ",
            length(LUCANDS[which(LUCANDS[,varpos] == 0),][,varpos]),
            "\n",
            "ties > 0: ",
            length(LUCANDS[which(LUCANDS[,varpos] > 0),][,varpos]),
            sep="")
    
    # d) Make the text in cf_cond more descriptive
    PREDOVERVIEW$cf_cond[i] <- sub("^0:", "0 ties:", PREDOVERVIEW$cf_cond[i])
  }
  
  # 4) For ballot_pos_rel1, we use a special approach
  if(grepl("^(ballot_pos_rel1)$", PREDOVERVIEW$variable[i])){
    # a) Find out how many candidates have their first list position above or below 0.5
    PREDOVERVIEW$real_cond[i] <- 
      paste("position < 0.6: ",
            length(LUCANDS[which(LUCANDS$ballot_pos_rel1 < 0.6),]$ballot_pos_rel1),
            "\n",
            "position >= 0.6: ",
            length(LUCANDS[which(LUCANDS$ballot_pos_rel1 >= 0.6),]$ballot_pos_rel1),
            sep="")
    # b) Make the text in cf_cond more descriptive
    PREDOVERVIEW$cf_cond[i] <- sub("^1:", "position = 1:", PREDOVERVIEW$cf_cond[i])
  }
  
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 9) Plot PREDOVERVIEW
# a) Move PREDOVERVIEW to long format
PREDOVERVIEWLONG<- gather(PREDOVERVIEW, elected_status, elected_num, elect_1_predelect_1:elect_0_predelect_0, factor_key=TRUE)

# b) Sort by PREDOVERVIEW$variable
PREDOVERVIEWLONG <- PREDOVERVIEWLONG[order(match(PREDOVERVIEWLONG$variable, PREDOVERVIEW$variable)),]
PREDOVERVIEWLONG$variable <- factor(PREDOVERVIEWLONG$variable, levels = unique(PREDOVERVIEWLONG$variable))


# c) Plot
counterfact_elecsucc <- 
  ggplot(PREDOVERVIEWLONG[which(grepl("elect_1_predelect_1|elect_0_predelect_1",PREDOVERVIEWLONG$elected_status) & PREDOVERVIEWLONG$elected_num > 0),],
         aes(fill=elected_status, y=elected_num, x=reorder(variable, desc(variable)))) +
  geom_bar(position="stack", stat="identity", colour="black") +
  geom_text(size=3, aes(color = elected_status, label = elected_num),
            position = position_stack(vjust = 0.5),show.legend = FALSE) +
  scale_fill_manual(name = "elected_status",
                    values =  c("#525252", "#cccccc"),
                    breaks = c("elect_0_predelect_1","elect_1_predelect_1"), # Legend order
                    labels=c('Different electee', 'Same electee')) +
  scale_color_manual(values = c("black", "white"),
                     breaks = c("elect_1_predelect_1","elect_0_predelect_1")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80"),
        axis.title.y = element_text(size = 13),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.25,0.0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous("Number of seats in parliament", position = "left",
                     limits = c(0, 120), breaks=seq(0,120,by = 20),
                     expand = c(0, 0)) + # removes lines between labels, e.g. gender2 and the bars
  scale_x_discrete("Electoral success of candidates if everyone...", position = "bottom",
                   labels=c('gender2'  = 'were non-female', 'non_swiss'  = 'had a Swiss surname', 
                            'prof_org_act2'  = 'worked a regular job', 'locpuboff_highest_now2'  = 'had no local public office', 
                            'regparl_incumb'  = 'were non-incumbent', 'locptyoff_any'  = 'had no local party role', 
                            'regptyoff_any'  = 'had no regional party role', 'ig_formalties_eco_locreg'  = 'had no formal ties to\neconomic interest groups', 
                            'ig_formalties_fir_locreg'  = 'had no formal ties to\nfirms', 'ig_formalties_hob_locreg'  = 'had no formal ties to\nhobby and leisure groups', 
                            'ig_formalties_ide_locreg'  = 'had no formal ties to\nidentity groups', 'ig_formalties_pig_locreg'  = 'had no formal ties to\npublic interest groups', 
                            'precumul2'  = 'were not pre-cumulated\non their list', 'ballot_pos_rel1'  = 'had the first place\non their list')) +
  coord_flip()




counterfact_elecsucc



# 10) Export figure
completefilename <- paste(getwd(),"./Figure1_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".tif", sep = "")
tiff(completefilename,height = 17, width = 19, units = "cm", compression = "lzw", res = 1200)
counterfact_elecsucc
dev.off()


#################
#################
### Figure A4 ###
#################
#################


# In this step, we replicate Figure 1 for the appendix, but account for confidence intervals.

# 1) Calculate a version based on confidence intervals for the final predicted (counterfactual) score
CONFINTVAL <-  data.frame(matrix(nrow = nrow(PREDFINAL), ncol = 0)) 

for (i in 1:ncol(PREDTRUECFDIFF))
{
  pb <- txtProgressBar(min = 1, max = ncol(PREDTRUECFDIFF), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Calculate the confidence interval for all variables
  # Note: var(LUCANDS$total_votes_zlist - PREDTRUE$fit) is the variance of the residual
  
  # a) Lower bound
  CONFINTVAL$lwr <- LUCANDS$total_votes_zlist + (-1)*PREDTRUECFDIFF[,i] - 1.959963984540054*sqrt(
    var(PREDTRUE$fit) + var(CFPREDS[[i]]$fit) - 2*cov(PREDTRUE$fit, CFPREDS[[i]]$fit) + var(LUCANDS$total_votes_zlist - PREDTRUE$fit)
  )
  
  # b) Upper bound
  CONFINTVAL$upr <- LUCANDS$total_votes_zlist + (-1)*PREDTRUECFDIFF[,i] + 1.959963984540054*sqrt(
    var(PREDTRUE$fit) + var(CFPREDS[[i]]$fit) - 2*cov(PREDTRUE$fit, CFPREDS[[i]]$fit) + var(LUCANDS$total_votes_zlist - PREDTRUE$fit)
  )
  
  # 2) Rename that column
  colnames(CONFINTVAL)[colnames(CONFINTVAL) == 'lwr'] <- paste(cfvars[i], "_lwr", sep="")
  colnames(CONFINTVAL)[colnames(CONFINTVAL) == 'upr'] <- paste(cfvars[i], "_upr", sep="")
  
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 2) Calculate whether the candidate would have been elected with the final predicted (counterfactual) number of votes, and
#     whether they would have managed this with significantly more votes than (95 CI) than the lowest true electee obtained.
# Significant differences are indicated with *

# a) Store the information in an overview
toselect <- c(c("pers_id","list_id", "total_votes_zlist", "elected_status"), cfvars)
PREDELEC_CI <- subset(LUCANDS, select = toselect)
rm(toselect)

for (i in 1:ncol(PREDFINAL))
{
  pb <- txtProgressBar(min = 1, max = ncol(PREDFINAL), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Create an overview of how many candidates per list were elected.
  # a) An overview:
  LISTELEC <- sqldf("SELECT DISTINCT LUCANDS.list_id, LUCANDS.list_seats_won
                        FROM LUCANDS
                  ")
  
  # b) Remove lists that did not win any seat
  LISTELEC <- LISTELEC[which(LISTELEC$list_seats_won != 0),]
  
  # c) Get all number in a certain range
  LISTELEC$winner_pos <- lapply(LISTELEC$list_seats_won, function(x) paste(seq.int(1,x, by = 1), collapse=";"))
  LISTELEC$winner_pos <- as.character(LISTELEC$winner_pos)
  
  # d) Split winner_pos into multiple columns
  LISTELEC <- cSplit(LISTELEC, "winner_pos", ";")
  
  # e) Move to long format
  LISTELEC <- gather(LISTELEC, variable, winner_pos, winner_pos_1:winner_pos_7, factor_key=TRUE)
  
  # f) Keep only relevant rows (those that are not NA on winner_pos)
  LISTELEC <- LISTELEC[which(!is.na(LISTELEC$winner_pos)),]
  LISTELEC$elected <- 1
  
  # 2) Create another helper df where we generate the rank order on the list based on the values in PREDFINAL
  RANKS <- data.frame(LUCANDS$pers_id, LUCANDS$list_id, PREDFINAL[,i])
  colnames(RANKS) <- c("pers_id", "list_id", "predfinal_votes")
  
  # 3) Create a rank order per list
  # a) Order by list_id and then the number of votes
  RANKS <- RANKS[order(RANKS$list_id,RANKS$predfinal_votes),]
  
  # b) Invert the row order, so we have a descending number of votes
  RANKS<- RANKS[seq(dim(RANKS)[1],1),]
  
  # c) Add a rank order
  RANKS$rank_order <- ave(RANKS$predfinal_votes, RANKS$list_id, FUN = seq_along)
  
  
  # 4) Add information on whether someone would have one a seat to RANKS
  RANKS <- sqldf("SELECT RANKS.*, LISTELEC.elected
                 FROM RANKS LEFT JOIN LISTELEC
                 ON
                 (RANKS.list_id = LISTELEC.list_id)
                 AND
                 (RANKS.rank_order = LISTELEC.winner_pos)
                 ")
  
  # Set NAs to 0
  RANKS[which(is.na(RANKS$elected)),]$elected <- 0
  
  # 5) Add information from PREDFINAL and RANKS to PREDELC
  # a) The predicted number of votes (z-standardised) from PREDFINAL
  PREDELEC_CI$predfinal <- PREDFINAL[,i]
  colnames(PREDELEC_CI)[ncol(PREDELEC_CI)] <- colnames(PREDFINAL)[i]
  
  # b) Add the confidence interval from CONFINTVAL
  # Define the variable name
  tofind <- gsub("_predfinal", "",colnames(PREDELEC_CI)[ncol(PREDELEC_CI)])
  
  # Extract column names from CONFINTVAL that match the pattern
  matching_columns <- grep(tofind, names(CONFINTVAL), value = TRUE)
  
  # Combine PREDELEC_CI with the matching columns from CONFINTVAL
  PREDELEC_CI <- cbind(PREDELEC_CI, subset(CONFINTVAL, select = matching_columns))
  rm(matching_columns,tofind)
  
  
  # c) The elected status from RANK
  PREDELEC_CI <- sqldf("SELECT PREDELEC_CI.*, RANKS.elected
                 FROM PREDELEC_CI LEFT JOIN RANKS
                 ON
                 (PREDELEC_CI.list_id = RANKS.list_id)
                 AND
                 (PREDELEC_CI.pers_id = RANKS.pers_id)
                 ")
  
  #colnames(PREDELEC_CI)[ncol(PREDELEC_CI)] <- paste(cfvars[i], "_predelected", sep="")
  
  
  # 6) Find the lowest winning total_votes_zlist per list and add the info to PREDELEC_CI
  PREDELEC_CI <- PREDELEC_CI %>%
    group_by(list_id) %>%
    mutate(min_winning_total_votes_zlist = min(total_votes_zlist[elected_status == 1], na.rm = TRUE)) %>%
    ungroup()
  
  
  # 7) Check whether min_winning_total_votes_zlist is smaller than the lower bound of the predicted votes
  # a) Check whether min_winning_total_votes_zlist is equal to or smaller than the lower bound
  PREDELEC_CI$lwr_above_minvotes <- PREDELEC_CI$min_winning_total_votes_zlist <= PREDELEC_CI[,grep(paste(cfvars[i], "_lwr", sep=""), colnames(PREDELEC_CI))]
  colnames(PREDELEC_CI)[ncol(PREDELEC_CI)] <- "lwr_above_minvotes"
  
  # b) Code the cases that are not significantly above the threshold of the min_winning_total_votes_zlist (lwr_above_minvotes = FALSE) as 0.5
  PREDELEC_CI[which(PREDELEC_CI$elected == 1 & PREDELEC_CI$elected_status == 0 & PREDELEC_CI$lwr_above_minvotes == FALSE),]$elected <- 0.5
  
  # c) Delete and rename variables
  PREDELEC_CI$lwr_above_minvotes <- NULL
  PREDELEC_CI$min_winning_total_votes_zlist <- NULL
  
  colnames(PREDELEC_CI)[ncol(PREDELEC_CI)] <- paste(cfvars[i], "_predelected", sep="")
  
  # 8) Remove dfs
  rm(RANKS, LISTELEC)
  
  
  setTxtProgressBar(pb, i)
}
close(pb)





# 3) Count how often different candidates would (not) have been elected.
PREDOVERVIEW_CI <-  data.frame(matrix(nrow = length(cfvars), ncol = 8))
colnames(PREDOVERVIEW_CI) <- c("variable", "real_cond", "cf_cond",
                               "elect_1_predelect_1", "elect_1_predelect_0", "elect_0_predelect_1", "elect_0_predelect_0",
                               "elect_0_predelect_05")

for (i in 1:length(cfvars))
{
  pb <- txtProgressBar(min = 1, max = length(cfvars), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Define the relevant predelected column in PREDELEC_CI
  varname <- paste(cfvars[i], "_predelected", sep="") # varname to search
  varpos <- grep(varname, colnames(PREDELEC_CI))
  
  # 2) Count the number of cases for elect_1_predelect_1 (elected in actuality, predicted to be elected by the counterfactual model)
  PREDOVERVIEW_CI$elect_1_predelect_1[i] <- nrow(PREDELEC_CI[which(PREDELEC_CI$elected_status == 1 & PREDELEC_CI[,varpos] == 1),])
  
  # 3) Count the number of cases for elect_1_predelect_0 (elected in actuality, but not elected in the counterfactual model)
  PREDOVERVIEW_CI$elect_1_predelect_0[i] <- nrow(PREDELEC_CI[which(PREDELEC_CI$elected_status == 1 & PREDELEC_CI[,varpos] == 0),])
  
  # 4) Count the number of cases for elect_0_predelect_1 (not actually elected, predicted to be elected by the counterfactual model)
  PREDOVERVIEW_CI$elect_0_predelect_1[i] <- nrow(PREDELEC_CI[which(PREDELEC_CI$elected_status == 0 & PREDELEC_CI[,varpos] == 1),])
  
  # 5) Count the number of cases for elect_0_predelect_0 (not actually elected, and not elected in the counterfactual model)
  PREDOVERVIEW_CI$elect_0_predelect_0[i] <- nrow(PREDELEC_CI[which(PREDELEC_CI$elected_status == 0 & PREDELEC_CI[,varpos] == 0),])
  
  # 6) Count the number of cases for elect_0_predelect_05 (not actually elected, and not (significantly) predicted to be elected by the counterfactual model)
  PREDOVERVIEW_CI$elect_0_predelect_05[i] <- nrow(PREDELEC_CI[which(PREDELEC_CI$elected_status == 0 & PREDELEC_CI[,varpos] == 0.5),])
  
  # 7) The variable in question  
  PREDOVERVIEW_CI$variable[i] <- cfvars[i]
  
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 4) Add information on conditions (real and counterfactual)

for (i in 1:nrow(PREDOVERVIEW_CI))
{
  pb <- txtProgressBar(min = 1, max = nrow(PREDOVERVIEW_CI), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Fill in the counterfactual condition
  PREDOVERVIEW_CI$cf_cond[i] <- paste(CFCONDS$cf_cond[i], ": ", nrow(LUCANDS), sep="")
  
  # 2) For (regular) categorical variables, use tabled information
  if(grepl("^(gender2|non_swiss|prof_org_act2|locpuboff_highest_now2|regparl_incumb|locptyoff_any|regptyoff_any|precumul2)$", PREDOVERVIEW_CI$variable[i])){
    # a) Get variable name
    varname <- PREDOVERVIEW_CI$variable[i]
    
    # b) Find the variable position in LUCANDS
    varpos <- grep(varname, colnames(LUCANDS))
    
    # c) Table the variable
    TABLE <- data.frame(table(LUCANDS[,varpos]))
    
    # d) Obtain a string
    PREDOVERVIEW_CI$real_cond[i] <- paste(paste(TABLE$Var1, ": ", TABLE$Freq, sep=""), collapse="\n")
  }
  
  # 3) For continuous measures on IG ties, calculate the share of zeroes and above-zeroes
  if(grepl("^(ig_formalties_eco_locreg|ig_formalties_fir_locreg|ig_formalties_hob_locreg|ig_formalties_ide_locreg|ig_formalties_pig_locreg)$", PREDOVERVIEW_CI$variable[i])){
    # a) Get variable name
    varname <- PREDOVERVIEW_CI$variable[i]
    
    # b) Find the variable position in LUCANDS
    varpos <- grep(varname, colnames(LUCANDS)) 
    
    # c) Extract how many IG ties are 0 or above
    PREDOVERVIEW_CI$real_cond[i] <- 
      paste("ties = 0: ",
            length(LUCANDS[which(LUCANDS[,varpos] == 0),][,varpos]),
            "\n",
            "ties > 0: ",
            length(LUCANDS[which(LUCANDS[,varpos] > 0),][,varpos]),
            sep="")
    
    # d) Make the text in cf_cond more descriptive
    PREDOVERVIEW_CI$cf_cond[i] <- sub("^0:", "0 ties:", PREDOVERVIEW_CI$cf_cond[i])
  }
  
  # 4) For ballot_pos_rel1, we use a special approach
  if(grepl("^(ballot_pos_rel1)$", PREDOVERVIEW_CI$variable[i])){
    # a) Find out how many candidates have their first list position above or below 0.5
    PREDOVERVIEW_CI$real_cond[i] <- 
      paste("position < 0.6: ",
            length(LUCANDS[which(LUCANDS$ballot_pos_rel1 < 0.6),]$ballot_pos_rel1),
            "\n",
            "position >= 0.6: ",
            length(LUCANDS[which(LUCANDS$ballot_pos_rel1 >= 0.6),]$ballot_pos_rel1),
            sep="")
    # b) Make the text in cf_cond more descriptive
    PREDOVERVIEW_CI$cf_cond[i] <- sub("^1:", "position = 1:", PREDOVERVIEW_CI$cf_cond[i])
  }
  
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 5) Plot PREDOVERVIEW_CI
# a) Move PREDOVERVIEW_CI to long format
PREDOVERVIEW_CILONG<- gather(PREDOVERVIEW_CI, elected_status, elected_num, elect_1_predelect_1:elect_0_predelect_05, factor_key=TRUE)

# b) Sort by PREDOVERVIEW_CI$variable
PREDOVERVIEW_CILONG <- PREDOVERVIEW_CILONG[order(match(PREDOVERVIEW_CILONG$variable, PREDOVERVIEW_CI$variable)),]
PREDOVERVIEW_CILONG$variable <- factor(PREDOVERVIEW_CILONG$variable, levels = unique(PREDOVERVIEW_CILONG$variable))


# c) Plot
counterfact_elecsucc_ci <- 
  ggplot(PREDOVERVIEW_CILONG[which(grepl("elect_1_predelect_1|elect_0_predelect_1|elect_0_predelect_05",PREDOVERVIEW_CILONG$elected_status) & PREDOVERVIEW_CILONG$elected_num > 0),],
         aes(fill=elected_status, y=elected_num, x=reorder(variable, desc(variable)))) +
  geom_bar(position="stack", stat="identity", colour="black") +
  geom_text(size=3, aes(color = elected_status, label = elected_num),
            position = position_stack(vjust = 0.5),show.legend = FALSE) +
  scale_fill_manual(name = "elected_status",
                    values =  c("#525252", "#cccccc", "#f0f0f0"),,
                    breaks = c("elect_0_predelect_05","elect_0_predelect_1","elect_1_predelect_1"), # Legend order
                    labels=c('Different electee (n.s.)','Different electee (sig.)', 'Same electee')) +
  scale_color_manual(values = c("white", "black", "black"),
                     breaks = c("elect_0_predelect_05", "elect_1_predelect_1","elect_0_predelect_1")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80"),
        axis.title.y = element_text(size = 13),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.25,0.0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous("Number of seats in parliament", position = "left",
                     limits = c(0, 120), breaks=seq(0,120,by = 20),
                     expand = c(0, 0)) + # removes lines between labels, e.g. gender2 and the bars
  scale_x_discrete("Electoral success of candidates if everyone...", position = "bottom",
                   labels=c('gender2'  = 'were non-female', 'non_swiss'  = 'had a Swiss surname', 
                            'prof_org_act2'  = 'worked a regular job', 'locpuboff_highest_now2'  = 'had no local public office', 
                            'regparl_incumb'  = 'were non-incumbent', 'locptyoff_any'  = 'had no local party role', 
                            'regptyoff_any'  = 'had no regional party role', 'ig_formalties_eco_locreg'  = 'had no formal ties to\neconomic interest groups', 
                            'ig_formalties_fir_locreg'  = 'had no formal ties to\nfirms', 'ig_formalties_hob_locreg'  = 'had no formal ties to\nhobby and leisure groups', 
                            'ig_formalties_ide_locreg'  = 'had no formal ties to\nidentity groups', 'ig_formalties_pig_locreg'  = 'had no formal ties to\npublic interest groups', 
                            'precumul2'  = 'were not pre-cumulated\non their list', 'ballot_pos_rel1'  = 'had the first place\non their list')) +
  coord_flip()




counterfact_elecsucc_ci



# 6) Export figure
completefilename <- paste(getwd(),"./FigureA4_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".tif", sep = "")
tiff(completefilename,height = 17, width = 19, units = "cm", compression = "lzw", res = 1200)
counterfact_elecsucc_ci
dev.off()




################
################
### Figure 2 ###
################
################


# 1) We use PREDOVERVIEW2 as the starting point
PREDOVERVIEW2 <- subset(PREDOVERVIEW, select = c("variable"))

# 2) Define the candidate and list characteristics
PREDOVERVIEW2$characteristic <- ""

PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "gender2"),]$characteristic <- "Woman"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "non_swiss"),]$characteristic <- "Non-Swiss surname"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "prof_org_act2"),]$characteristic <- "Working a regular job"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "locpuboff_highest_now2"),]$characteristic <- "Elected local public office"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "regparl_incumb"),]$characteristic <- "Incumbent in regional parliament"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "locptyoff_any"),]$characteristic <- "Role in local party"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "regptyoff_any"),]$characteristic <- "Role in regional party role"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ig_formalties_eco_locreg"),]$characteristic <- "No. of economic interest group ties > 0"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ig_formalties_fir_locreg"),]$characteristic <- "No. of firm ties > 0"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ig_formalties_hob_locreg"),]$characteristic <- "No. of hobby and leisure group ties > 0"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ig_formalties_ide_locreg"),]$characteristic <- "No. of identity group ties > 0"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ig_formalties_pig_locreg"),]$characteristic <- "No. of public interest group ties > 0"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "precumul2"),]$characteristic <- "Pre-cumulated on the list"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ballot_pos_rel1"),]$characteristic <- "First place on the list"

# 3) Add the raw category
PREDOVERVIEW2$raw_characteristic <- ""

PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "gender2"),]$raw_characteristic <- "^female"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "non_swiss"),]$raw_characteristic <- "1"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "prof_org_act2"),]$raw_characteristic <- "^WOR"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "locpuboff_highest_now2"),]$raw_characteristic <- "2 - elected local public office"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "regparl_incumb"),]$raw_characteristic <- "^incumbent"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "locptyoff_any"),]$raw_characteristic <- "1 - role in party"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "regptyoff_any"),]$raw_characteristic <- "1 - role in party"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ig_formalties_eco_locreg"),]$raw_characteristic <- "^[^0]+$"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ig_formalties_fir_locreg"),]$raw_characteristic <- "^[^0]+$"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ig_formalties_hob_locreg"),]$raw_characteristic <- "^[^0]+$"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ig_formalties_ide_locreg"),]$raw_characteristic <- "^[^0]+$"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ig_formalties_pig_locreg"),]$raw_characteristic <- "^[^0]+$"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "precumul2"),]$raw_characteristic <- "^pre-cumulated"
PREDOVERVIEW2[which(PREDOVERVIEW2$variable == "ballot_pos_rel1"),]$raw_characteristic <- "^1"




# 3) Count how often different candidates would (not) have been elected among those on successful lists (i.e. lists that won at least one seat)
# a) Successful lists
succlists <- unique(LUCANDS[which(LUCANDS$list_seats_won > 0),]$list_id)

# b) Count cases per scenario
PREDOVERVIEW2$elect_1_predelect_1 <- ""
PREDOVERVIEW2$elect_1_predelect_0 <- ""
PREDOVERVIEW2$elect_0_predelect_1 <- ""
PREDOVERVIEW2$elect_0_predelect_0 <- ""


for (i in 1:length(cfvars))
{
  pb <- txtProgressBar(min = 1, max = length(cfvars), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Define the relevant predelected column in PREDELEC
  varname <- paste(cfvars[i], "_predelected", sep="") # varname to search
  varpos <- grep(varname, colnames(PREDELEC))
  
  varpos2 <- grep(paste("^",cfvars[i],"$", sep=""), colnames(PREDELEC))
  
  
  # 2) Count the number of cases for elect_1_predelect_1 (elected in actuality, predicted to be elected by the counterfactual model)
  PREDOVERVIEW2$elect_1_predelect_1[i] <- nrow(PREDELEC[which(PREDELEC$elected_status == 1 & PREDELEC[,varpos] == 1 & PREDELEC$list_id %in% succlists & grepl(PREDOVERVIEW2$raw_characteristic[i], PREDELEC[,varpos2])),])
  
  # 3) Count the number of cases for elect_1_predelect_0 (elected in actuality, but not elected in the counterfactual model)
  PREDOVERVIEW2$elect_1_predelect_0[i] <- nrow(PREDELEC[which(PREDELEC$elected_status == 1 & PREDELEC[,varpos] == 0 & PREDELEC$list_id %in% succlists & grepl(PREDOVERVIEW2$raw_characteristic[i], PREDELEC[,varpos2])),])
  
  # 4) Count the number of cases for elect_0_predelect_1 (not actually elected, predicted to be elected by the counterfactual model)
  PREDOVERVIEW2$elect_0_predelect_1[i] <- nrow(PREDELEC[which(PREDELEC$elected_status == 0 & PREDELEC[,varpos] == 1 & PREDELEC$list_id %in% succlists & grepl(PREDOVERVIEW2$raw_characteristic[i], PREDELEC[,varpos2])),])
  
  # 5) Count the number of cases for elect_0_predelect_0 (not actually elected, and not elected in the counterfactual model)
  PREDOVERVIEW2$elect_0_predelect_0[i] <- nrow(PREDELEC[which(PREDELEC$elected_status == 0 & PREDELEC[,varpos] == 0 & PREDELEC$list_id %in% succlists & grepl(PREDOVERVIEW2$raw_characteristic[i], PREDELEC[,varpos2])),])
  
  # 6) The variable in question  
  PREDOVERVIEW2$variable[i] <- cfvars[i]
  
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 4) Create three categories
PREDOVERVIEW2$different_electee <- ""

PREDOVERVIEW2[which(PREDOVERVIEW2$elect_1_predelect_0 > PREDOVERVIEW2$elect_0_predelect_1),]$different_electee <- PREDOVERVIEW2[which(PREDOVERVIEW2$elect_1_predelect_0 > PREDOVERVIEW2$elect_0_predelect_1),]$elect_1_predelect_0
PREDOVERVIEW2[which(PREDOVERVIEW2$elect_1_predelect_0 < PREDOVERVIEW2$elect_0_predelect_1),]$different_electee <- PREDOVERVIEW2[which(PREDOVERVIEW2$elect_1_predelect_0 < PREDOVERVIEW2$elect_0_predelect_1),]$elect_0_predelect_1
PREDOVERVIEW2[which(PREDOVERVIEW2$elect_1_predelect_0 == PREDOVERVIEW2$elect_0_predelect_1),]$different_electee <- PREDOVERVIEW2[which(PREDOVERVIEW2$elect_1_predelect_0 == PREDOVERVIEW2$elect_0_predelect_1),]$elect_0_predelect_1

# 5) Rename variables
names(PREDOVERVIEW2)[names(PREDOVERVIEW2) == 'elect_1_predelect_1'] <- 'same_electee'
names(PREDOVERVIEW2)[names(PREDOVERVIEW2) == 'elect_0_predelect_0'] <- 'same_nonelectee'

PREDOVERVIEW2$elect_1_predelect_0 <- NULL
PREDOVERVIEW2$elect_0_predelect_1 <- NULL

# 6) Reorder variables
PREDOVERVIEW2 %>% relocate(same_nonelectee, .after=different_electee) -> PREDOVERVIEW2

# 7) Create a longformat version of PREDOVERVIEW2
# a) Move PREDOVERVIEW2 to long format
PREDOVERVIEWLONG2<- gather(PREDOVERVIEW2, elected_status, elected_num, same_electee:same_nonelectee, factor_key=TRUE)

# b) Sort by PREDOVERVIEW2$variable
PREDOVERVIEWLONG2 <- PREDOVERVIEWLONG2[order(match(PREDOVERVIEWLONG2$variable, PREDOVERVIEW2$variable)),]
PREDOVERVIEWLONG2$variable <- factor(PREDOVERVIEWLONG2$variable, levels = unique(PREDOVERVIEWLONG2$variable))

# c) Calculate group percentages
PREDOVERVIEWLONG2$elected_num <- as.numeric(PREDOVERVIEWLONG2$elected_num)

PREDOVERVIEWLONG2 <- group_by(PREDOVERVIEWLONG2, variable) %>% mutate(elect_pct = elected_num/sum(elected_num))
PREDOVERVIEWLONG2 <- group_by(PREDOVERVIEWLONG2, variable) %>% mutate(n = sum(elected_num))
#PREDOVERVIEWLONG2$elect_pct <- PREDOVERVIEWLONG2$elect_pct*100

# c) Plot

PREDOVERVIEWLONG2$elected_status <- factor(PREDOVERVIEWLONG2$elected_status, levels = c("same_nonelectee", "same_electee", "different_electee"))

counterfact_elecsucc2 <- 
  ggplot(PREDOVERVIEWLONG2[which(PREDOVERVIEWLONG2$elected_num > 0),],
         aes(fill=elected_status, y=elect_pct, x=reorder(variable, desc(variable)))) +
  geom_bar(position="stack", stat="identity", colour="black") +
  geom_text(size=3, aes(color = elected_status, label = elected_num),
            position = position_stack(vjust = 0.5),show.legend = FALSE) +
  scale_fill_manual(name = "elected_status",
                    values =  c("#525252", "#cccccc", "#f0f0f0"),
                    breaks = c("different_electee","same_electee", "same_nonelectee"), # Legend order
                    labels=c('Different electee', 'Same electee', 'Same non-electee')) +
  scale_color_manual(values = c("black", "black", "white"),
                     breaks = c("same_electee", "same_nonelectee","different_electee")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey80"), 
        panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey80"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_text(size = 13),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,4.75,0.0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous("Percentage of candidates on successful lists", position = "left",
                     labels = scales::percent,
                     limits = c(0, 1.05), breaks=seq(0,1,by = 0.2),
                     expand = c(0, 0)) + # removes lines between labels, e.g. gender2 and the bars
  scale_x_discrete("Electoral success of candidates if everyone...", position = "bottom",
                   labels=c('gender2'  = 'were non-female', 'non_swiss'  = 'had a Swiss surname', 
                            'prof_org_act2'  = 'worked a regular job', 'locpuboff_highest_now2'  = 'had no local public office', 
                            'regparl_incumb'  = 'were non-incumbent', 'locptyoff_any'  = 'had no local party role', 
                            'regptyoff_any'  = 'had no regional party role', 'ig_formalties_eco_locreg'  = 'had no formal ties to\neconomic interest groups', 
                            'ig_formalties_fir_locreg'  = 'had no formal ties to\nfirms', 'ig_formalties_hob_locreg'  = 'had no formal ties to\nhobby and leisure groups', 
                            'ig_formalties_ide_locreg'  = 'had no formal ties to\nidentity groups', 'ig_formalties_pig_locreg'  = 'had no formal ties to\npublic interest groups', 
                            'precumul2'  = 'were not pre-cumulated\non their list', 'ballot_pos_rel1'  = 'had the first place\non their list')) +
  coord_flip(clip = 'off') +
  geom_text(data = PREDOVERVIEWLONG2[which(PREDOVERVIEWLONG2$elected_num > 0 & PREDOVERVIEWLONG2$elected_status == "same_nonelectee"),], aes(y = max(elect_pct) + 0.13, label = paste(characteristic, "\nN = ", n, sep="")), hjust = 0,
            position = position_dodge(.9), size=3) + # hjust = 0 for left allignment of text
  annotate(geom = "text", x = 14.5, y = 1.013, hjust = 0, label = "Group:",
           colour = "black", parse = FALSE) # hjust = 0 for left allignment of text


counterfact_elecsucc2



# 8) Export figure
completefilename <- paste(getwd(),"./Figure2_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".tif", sep = "")
tiff(completefilename,height = 23.8, width = 26.6, units = "cm", compression = "lzw", res = 1200)
counterfact_elecsucc2
dev.off() 



################
################
### Figure 3 ###
################
################

####################################################
### Absolute Preference Vote-Based Non-Anchoring ###
####################################################

# 1) Calculate the final predicted (counterfactual) score
PREDNOANCHFINAL <-  data.frame(matrix(nrow = nrow(LUCANDS), ncol = 0)) 

for (i in 1:ncol(PREDTRUECFDIFF))
{
  pb <- txtProgressBar(min = 1, max = ncol(PREDTRUECFDIFF), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Just use the difference between the true and the counterfactual prediction without anchor
  PREDNOANCHFINAL$newfinal <- CFPREDS[[i]]$fit
  
  # 2) Rename that column
  colnames(PREDNOANCHFINAL)[colnames(PREDNOANCHFINAL) == 'newfinal'] <- paste(cfvars[i], "_predfinal", sep="")
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 2) Calculate whether the candidate would have been elected with the final predicted (counterfactual) number of votes
# a) Store the information in an overview
toselect <- c(c("pers_id","list_id", "total_votes_zlist", "elected_status"), cfvars)
PREDNOANCHELEC <- subset(LUCANDS, select = toselect)
rm(toselect)

for (i in 1:ncol(PREDNOANCHFINAL))
{
  pb <- txtProgressBar(min = 1, max = ncol(PREDNOANCHFINAL), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Create an overview of how many candidates per list were elected.
  # a) An overview:
  LISTELEC <- sqldf("SELECT DISTINCT LUCANDS.list_id, LUCANDS.list_seats_won
                        FROM LUCANDS
                  ")
  
  # b) Remove lists that did not win any seat
  LISTELEC <- LISTELEC[which(LISTELEC$list_seats_won != 0),]
  
  # c) Get all number in a certain range
  LISTELEC$winner_pos <- lapply(LISTELEC$list_seats_won, function(x) paste(seq.int(1,x, by = 1), collapse=";"))
  LISTELEC$winner_pos <- as.character(LISTELEC$winner_pos)
  
  # d) Split winner_pos into multiple columns
  LISTELEC <- cSplit(LISTELEC, "winner_pos", ";")
  
  # e) Move to long format
  LISTELEC <- gather(LISTELEC, variable, winner_pos, winner_pos_1:winner_pos_7, factor_key=TRUE)
  
  # f) Keep only relevant rows (those that are not NA on winner_pos)
  LISTELEC <- LISTELEC[which(!is.na(LISTELEC$winner_pos)),]
  LISTELEC$elected <- 1
  
  # 2) Create another helper df where we generate the rank order on the list based on the values in PREDNOANCHFINAL
  RANKS <- data.frame(LUCANDS$pers_id, LUCANDS$list_id, PREDNOANCHFINAL[,i])
  colnames(RANKS) <- c("pers_id", "list_id", "predfinal_votes")
  
  # 3) Create a rank order per list
  # a) Order by list_id and then the number of votes
  RANKS <- RANKS[order(RANKS$list_id,RANKS$predfinal_votes),]
  
  # b) Invert the row order, so we have a descending number of votes
  RANKS<- RANKS[seq(dim(RANKS)[1],1),]
  
  # c) Add a rank order
  RANKS$rank_order <- ave(RANKS$predfinal_votes, RANKS$list_id, FUN = seq_along)
  
  
  # 4) Add information on whether someone would have one a seat to RANKS
  RANKS <- sqldf("SELECT RANKS.*, LISTELEC.elected
                 FROM RANKS LEFT JOIN LISTELEC
                 ON
                 (RANKS.list_id = LISTELEC.list_id)
                 AND
                 (RANKS.rank_order = LISTELEC.winner_pos)
                 ")
  
  # Set NAs to 0
  RANKS[which(is.na(RANKS$elected)),]$elected <- 0
  
  # 5) Add information from PREDNOANCHFINAL and RANKS to PREDNOANCHELC
  # a) The predicted number of votes (z-standardised) from PREDNOANCHFINAL
  PREDNOANCHELEC$predfinal <- PREDNOANCHFINAL[,i]
  colnames(PREDNOANCHELEC)[ncol(PREDNOANCHELEC)] <- colnames(PREDNOANCHFINAL)[i]
  
  # b) The elected status from RANK
  PREDNOANCHELEC <- sqldf("SELECT PREDNOANCHELEC.*, RANKS.elected
                 FROM PREDNOANCHELEC LEFT JOIN RANKS
                 ON
                 (PREDNOANCHELEC.list_id = RANKS.list_id)
                 AND
                 (PREDNOANCHELEC.pers_id = RANKS.pers_id)
                 ")
  
  colnames(PREDNOANCHELEC)[ncol(PREDNOANCHELEC)] <- paste(cfvars[i], "_predelected", sep="")
  
  
  # 6) Remove dfs
  rm(RANKS, LISTELEC)
  
  
  setTxtProgressBar(pb, i)
}
close(pb)



# 3) Count how often different candidates would (not) have been elected.
PREDNOANCHOVERVIEW <-  data.frame(matrix(nrow = length(cfvars), ncol = 7))
colnames(PREDNOANCHOVERVIEW) <- c("variable", "real_cond", "cf_cond",
                                  "elect_1_predelect_1", "elect_1_predelect_0", "elect_0_predelect_1", "elect_0_predelect_0")

for (i in 1:length(cfvars))
{
  pb <- txtProgressBar(min = 1, max = length(cfvars), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Define the relevant predelected column in PREDNOANCHELEC
  varname <- paste(cfvars[i], "_predelected", sep="") # varname to search
  varpos <- grep(varname, colnames(PREDNOANCHELEC))
  
  # 2) Count the number of cases for elect_1_predelect_1 (elected in actuality, predicted to be elected by the counterfactual model)
  PREDNOANCHOVERVIEW$elect_1_predelect_1[i] <- nrow(PREDNOANCHELEC[which(PREDNOANCHELEC$elected_status == 1 & PREDNOANCHELEC[,varpos] == 1),])
  
  # 3) Count the number of cases for elect_1_predelect_0 (elected in actuality, but not elected in the counterfactual model)
  PREDNOANCHOVERVIEW$elect_1_predelect_0[i] <- nrow(PREDNOANCHELEC[which(PREDNOANCHELEC$elected_status == 1 & PREDNOANCHELEC[,varpos] == 0),])
  
  # 4) Count the number of cases for elect_0_predelect_1 (not actually elected, predicted to be elected by the counterfactual model)
  PREDNOANCHOVERVIEW$elect_0_predelect_1[i] <- nrow(PREDNOANCHELEC[which(PREDNOANCHELEC$elected_status == 0 & PREDNOANCHELEC[,varpos] == 1),])
  
  # 5) Count the number of cases for elect_0_predelect_0 (not actually elected, and not elected in the counterfactual model)
  PREDNOANCHOVERVIEW$elect_0_predelect_0[i] <- nrow(PREDNOANCHELEC[which(PREDNOANCHELEC$elected_status == 0 & PREDNOANCHELEC[,varpos] == 0),])
  
  # 6) The variable in question  
  PREDNOANCHOVERVIEW$variable[i] <- cfvars[i]
  
  
  setTxtProgressBar(pb, i)
}
close(pb)




# 4) Add information on conditions (real and counterfactual)

for (i in 1:nrow(PREDNOANCHOVERVIEW))
{
  pb <- txtProgressBar(min = 1, max = nrow(PREDNOANCHOVERVIEW), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Fill in the counterfactual condition
  PREDNOANCHOVERVIEW$cf_cond[i] <- paste(CFCONDS$cf_cond[i], ": ", nrow(LUCANDS), sep="")
  
  # 2) For (regular) categorical variables, use tabled information
  if(grepl("^(gender2|non_swiss|prof_org_act2|locpuboff_highest_now2|regparl_incumb|locptyoff_any|regptyoff_any|precumul2)$", PREDNOANCHOVERVIEW$variable[i])){
    # a) Get variable name
    varname <- PREDNOANCHOVERVIEW$variable[i]
    
    # b) Find the variable position in LUCANDS
    varpos <- grep(varname, colnames(LUCANDS))
    
    # c) Table the variable
    TABLE <- data.frame(table(LUCANDS[,varpos]))
    
    # d) Obtain a string
    PREDNOANCHOVERVIEW$real_cond[i] <- paste(paste(TABLE$Var1, ": ", TABLE$Freq, sep=""), collapse="\n")
  }
  
  # 3) For continuous measures on IG ties, calculate the share of zeroes and above-zeroes
  if(grepl("^(ig_formalties_eco_locreg|ig_formalties_fir_locreg|ig_formalties_hob_locreg|ig_formalties_ide_locreg|ig_formalties_pig_locreg)$", PREDNOANCHOVERVIEW$variable[i])){
    # a) Get variable name
    varname <- PREDNOANCHOVERVIEW$variable[i]
    
    # b) Find the variable position in LUCANDS
    varpos <- grep(varname, colnames(LUCANDS)) 
    
    # c) Extract how many IG ties are 0 or above
    PREDNOANCHOVERVIEW$real_cond[i] <- 
      paste("ties = 0: ",
            length(LUCANDS[which(LUCANDS[,varpos] == 0),][,varpos]),
            "\n",
            "ties > 0: ",
            length(LUCANDS[which(LUCANDS[,varpos] > 0),][,varpos]),
            sep="")
    
    # d) Make the text in cf_cond more descriptive
    PREDNOANCHOVERVIEW$cf_cond[i] <- sub("^0:", "0 ties:", PREDNOANCHOVERVIEW$cf_cond[i])
  }
  
  # 4) For ballot_pos_rel1, we use a special approach
  if(grepl("^(ballot_pos_rel1)$", PREDNOANCHOVERVIEW$variable[i])){
    # a) Find out how many candidates have their first list position above or below 0.5
    PREDNOANCHOVERVIEW$real_cond[i] <- 
      paste("position < 0.6: ",
            length(LUCANDS[which(LUCANDS$ballot_pos_rel1 < 0.6),]$ballot_pos_rel1),
            "\n",
            "position >= 0.6: ",
            length(LUCANDS[which(LUCANDS$ballot_pos_rel1 >= 0.6),]$ballot_pos_rel1),
            sep="")
    # b) Make the text in cf_cond more descriptive
    PREDNOANCHOVERVIEW$cf_cond[i] <- sub("^1:", "position = 1:", PREDNOANCHOVERVIEW$cf_cond[i])
  }
  
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 5) Plot PREDNOANCHOVERVIEW
# a) Move PREDNOANCHOVERVIEW to long format
PREDNOANCHOVERVIEWLONG<- gather(PREDNOANCHOVERVIEW, elected_status, elected_num, elect_1_predelect_1:elect_0_predelect_0, factor_key=TRUE)

# b) Sort by PREDNOANCHOVERVIEW$variable
PREDNOANCHOVERVIEWLONG <- PREDNOANCHOVERVIEWLONG[order(match(PREDNOANCHOVERVIEWLONG$variable, PREDNOANCHOVERVIEW$variable)),]
PREDNOANCHOVERVIEWLONG$variable <- factor(PREDNOANCHOVERVIEWLONG$variable, levels = unique(PREDNOANCHOVERVIEWLONG$variable))


# c) Plot
counterfact_elecsucc_noanchor <- 
  ggplot(PREDNOANCHOVERVIEWLONG[which(grepl("elect_1_predelect_1|elect_0_predelect_1",PREDNOANCHOVERVIEWLONG$elected_status) & PREDNOANCHOVERVIEWLONG$elected_num > 0),],
         aes(fill=elected_status, y=elected_num, x=reorder(variable, desc(variable)))) +
  geom_bar(position="stack", stat="identity", colour="black") +
  geom_text(size=3, aes(color = elected_status, label = elected_num),
            position = position_stack(vjust = 0.5),show.legend = FALSE) +
  scale_fill_manual(name = "elected_status",
                    values =  c("#525252", "#cccccc"),
                    breaks = c("elect_0_predelect_1","elect_1_predelect_1"), # Legend order
                    labels=c('Different electee', 'Same electee')) +
  scale_color_manual(values = c("black", "white"),
                     breaks = c("elect_1_predelect_1","elect_0_predelect_1")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80"),
        axis.title.y = element_text(size = 13),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.25,0.0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous("Number of seats in parliament", position = "left",
                     limits = c(0, 120), breaks=seq(0,120,by = 20),
                     expand = c(0, 0)) + # removes lines between labels, e.g. gender2 and the bars
  scale_x_discrete("Electoral success of candidates if everyone...", position = "bottom",
                   labels=c('gender2'  = 'were non-female', 'non_swiss'  = 'had a Swiss surname', 
                            'prof_org_act2'  = 'worked a regular job', 'locpuboff_highest_now2'  = 'had no local public office', 
                            'regparl_incumb'  = 'were non-incumbent', 'locptyoff_any'  = 'had no local party role', 
                            'regptyoff_any'  = 'had no regional party role', 'ig_formalties_eco_locreg'  = 'had no formal ties to\neconomic interest groups', 
                            'ig_formalties_fir_locreg'  = 'had no formal ties to\nfirms', 'ig_formalties_hob_locreg'  = 'had no formal ties to\nhobby and leisure groups', 
                            'ig_formalties_ide_locreg'  = 'had no formal ties to\nidentity groups', 'ig_formalties_pig_locreg'  = 'had no formal ties to\npublic interest groups', 
                            'precumul2'  = 'were not pre-cumulated\non their list', 'ballot_pos_rel1'  = 'had the first place\non their list')) +
  coord_flip()




counterfact_elecsucc_noanchor


# 6) Obtain information on the alternative ranking based on the true predicted number of preference votes
# Create an overview of how many candidates per list were elected.
# a) An overview:
LISTELEC <- sqldf("SELECT DISTINCT LUCANDS.list_id, LUCANDS.list_seats_won
                      FROM LUCANDS
                ")

# b) Remove lists that did not win any seat
LISTELEC <- LISTELEC[which(LISTELEC$list_seats_won != 0),]

# c) Get all number in a certain range
LISTELEC$winner_pos <- lapply(LISTELEC$list_seats_won, function(x) paste(seq.int(1,x, by = 1), collapse=";"))
LISTELEC$winner_pos <- as.character(LISTELEC$winner_pos)

# d) Split winner_pos into multiple columns
LISTELEC <- cSplit(LISTELEC, "winner_pos", ";")

# e) Move to long format
LISTELEC <- gather(LISTELEC, variable, winner_pos, winner_pos_1:winner_pos_7, factor_key=TRUE)

# f) Keep only relevant rows (those that are not NA on winner_pos)
LISTELEC <- LISTELEC[which(!is.na(LISTELEC$winner_pos)),]
LISTELEC$elected <- 1

# 2) Create another helper df where we generate the rank order on the list based on the values in PREDFINAL
RANKS <- data.frame(LUCANDS$pers_id, LUCANDS$list_id, PREDTRUE$fit)
colnames(RANKS) <- c("pers_id", "list_id", "PREDFINAL_votes")

# 7) Create a rank order per list
# a) Order by list_id and then the number of votes
RANKS <- RANKS[order(RANKS$list_id,RANKS$PREDFINAL_votes),]

# b) Invert the row order, so we have a descending number of votes
RANKS<- RANKS[seq(dim(RANKS)[1],1),]

# c) Add a rank order
RANKS$rank_order <- ave(RANKS$PREDFINAL_votes, RANKS$list_id, FUN = seq_along)


# 8) Add information on whether someone would have one a seat to RANKS
RANKS <- sqldf("SELECT RANKS.*, LISTELEC.elected
               FROM RANKS LEFT JOIN LISTELEC
               ON
               (RANKS.list_id = LISTELEC.list_id)
               AND
               (RANKS.rank_order = LISTELEC.winner_pos)
               ")

# Set NAs to 0
RANKS[which(is.na(RANKS$elected)),]$elected <- 0

# 9) Add information from PREDFINAL and RANKS to PREDELC
# a) The predicted number of votes (z-standardised) from PREDFINAL
PREDELEC$true_pred_prob <- PREDTRUE$fit

# b) The elected status from RANK
PREDELEC <- sqldf("SELECT PREDELEC.*, RANKS.elected
               FROM PREDELEC LEFT JOIN RANKS
               ON
               (PREDELEC.list_id = RANKS.list_id)
               AND
               (PREDELEC.pers_id = RANKS.pers_id)
               ")

colnames(PREDELEC)[ncol(PREDELEC)] <- "true_pred_noanch"


# 10) Remove dfs
rm(RANKS, LISTELEC)




# 11) Count how often different candidates would (not) have been elected.
PREDNOANCHTRUEOVERVIEW <-  data.frame(matrix(nrow = 1, ncol = 7))
colnames(PREDNOANCHTRUEOVERVIEW) <- c("variable", "real_cond", "cf_cond",
                                      "elect_1_predelect_1", "elect_1_predelect_0", "elect_0_predelect_1", "elect_0_predelect_0")


# a) Count the number of cases for elect_1_predelect_1 (elected in actuality, predicted to be elected by the counterfactual model)
PREDNOANCHTRUEOVERVIEW$elect_1_predelect_1 <- nrow(PREDELEC[which(PREDELEC$elected_status == 1 & PREDELEC[length(PREDELEC)] == 1),])

# b) Count the number of cases for elect_1_predelect_0 (elected in actuality, but not elected in the counterfactual model)
PREDNOANCHTRUEOVERVIEW$elect_1_predelect_0 <- nrow(PREDELEC[which(PREDELEC$elected_status == 1 & PREDELEC[length(PREDELEC)] == 0),])

# c) Count the number of cases for elect_0_predelect_1 (not actually elected, predicted to be elected by the counterfactual model)
PREDNOANCHTRUEOVERVIEW$elect_0_predelect_1 <- nrow(PREDELEC[which(PREDELEC$elected_status == 0 & PREDELEC[length(PREDELEC)] == 1),])

# d) Count the number of cases for elect_0_predelect_0 (not actually elected, and not elected in the counterfactual model)
PREDNOANCHTRUEOVERVIEW$elect_0_predelect_0 <- nrow(PREDELEC[which(PREDELEC$elected_status == 0 & PREDELEC[length(PREDELEC)] == 0),])

# e) The variable in question  
PREDNOANCHTRUEOVERVIEW$variable <- "true_pred_noanch"



####################################################
### Relative Preference Vote-Based Non-Anchoring ###
####################################################


# 1) Define the variables for which we want to obtain counterfactuals
cfvars <- c("gender2", "non_swiss", "prof_org_act2", "locpuboff_highest_now2", "regparl_incumb",
            "locptyoff_any", "regptyoff_any",
            "ig_formalties_eco_locreg", "ig_formalties_fir_locreg", "ig_formalties_hob_locreg",
            "ig_formalties_ide_locreg", "ig_formalties_pig_locreg", 
            "precumul2", "ballot_pos_rel1"
)


# 2) Generate the "true" predictions, i.e. based on the original input data
PREDRELTRUE <- data.frame(predict(m1share$lm_res, # the lm object
                                  vcov = m1share$vcov, # the variance-covariance matrix
                                  newdata = LUCANDS, # the input data for prediction
                                  interval = 'confidence')) # also estimate a confidence interval


# 3) Calculate counterfactual predictions and store in a df what the counterfactual condition was
CFPREDRELS <-list()

CFCONDS <- data.frame(matrix(nrow = length(cfvars), ncol = 2))
colnames(CFCONDS) <- c("variable", "cf_cond")

for (i in 1:length(cfvars))
{
  pb <- txtProgressBar(min = 1, max = length(cfvars), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Find the variable position of the variable to manipulate in LUCANDS
  varpos <- grep(cfvars[i], colnames(LUCANDS))
  
  # 2) Get LUCANDS and store a copy
  LUCANDSMOD <- get("LUCANDS")
  
  # 3) Alter the respective variable
  if(cfvars[i] == "gender2"){
    LUCANDSMOD[,varpos] <- "not female"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "not female"
  }
  
  if(cfvars[i] == "non_swiss"){
    LUCANDSMOD[,varpos] <- "0"
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "prof_org_act2"){
    LUCANDSMOD[,varpos] <- "NOT WOR"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "NOT WOR"
  }
  
  if(cfvars[i] == "locpuboff_highest_now2"){
    LUCANDSMOD[,varpos] <- "0 - no local public office"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0 - no local public office"
  }
  
  if(cfvars[i] == "regparl_incumb"){
    LUCANDSMOD[,varpos] <- "challenger"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "challenger"
  }
  
  if(cfvars[i] == "locptyoff_any"){
    LUCANDSMOD[,varpos] <- "0 - no role in party"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0 - no role in party"
  }
  
  if(cfvars[i] == "regptyoff_any"){
    LUCANDSMOD[,varpos] <- "0 - no role in party"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0 - no role in party"
  }
  
  if(cfvars[i] == "ig_formalties_eco_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_fir_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_hob_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_ide_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_pig_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "precumul2"){
    LUCANDSMOD[,varpos] <- "not pre-cumulated"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "not pre-cumulated"
  }
  
  if(cfvars[i] == "ballot_pos_rel1"){
    LUCANDSMOD[,varpos] <- 1
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "1"
  }
  
  # 4) Calculate the counterfactual prediction
  TEMPPREDREL <- data.frame(predict(m1share$lm_res, # the lm object
                                    vcov = m1share$vcov, # the variance-covariance matrix
                                    newdata = LUCANDSMOD, # the input data for prediction
                                    interval = 'confidence')) # also estimate a confidence interval
  
  
  # 5) Add the predictions from TEMPPREDREL to the list CFPREDRELS
  CFPREDRELS[[toupper(cfvars[i])]] <- TEMPPREDREL
  rm(TEMPPREDREL,LUCANDSMOD)
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 4) Extract the counterfactual predictions
PREDRELFINAL <-  data.frame(matrix(nrow = nrow(LUCANDS), ncol = 0)) 

for (i in 1:length(CFPREDRELS))
{
  pb <- txtProgressBar(min = 1, max = length(CFPREDRELS), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Just use the difference between the true and the counterfactual prediction without anchor
  PREDRELFINAL$newfinal <- CFPREDRELS[[i]]$fit
  
  # 2) Rename that column
  colnames(PREDRELFINAL)[colnames(PREDRELFINAL) == 'newfinal'] <- paste(cfvars[i], "_predfinal", sep="")
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 5) Calculate whether the candidate would have been elected with the final predicted (counterfactual) number of votes
# a) Store the information in an overview
toselect <- c(c("pers_id","list_id", "total_votes_zlist", "elected_status"), cfvars)
PREDRELELEC <- subset(LUCANDS, select = toselect)
rm(toselect)

for (i in 1:ncol(PREDRELFINAL))
{
  pb <- txtProgressBar(min = 1, max = ncol(PREDRELFINAL), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Create an overview of how many candidates per list were elected.
  # a) An overview:
  LISTELEC <- sqldf("SELECT DISTINCT LUCANDS.list_id, LUCANDS.list_seats_won
                        FROM LUCANDS
                  ")
  
  # b) Remove lists that did not win any seat
  LISTELEC <- LISTELEC[which(LISTELEC$list_seats_won != 0),]
  
  # c) Get all number in a certain range
  LISTELEC$winner_pos <- lapply(LISTELEC$list_seats_won, function(x) paste(seq.int(1,x, by = 1), collapse=";"))
  LISTELEC$winner_pos <- as.character(LISTELEC$winner_pos)
  
  # d) Split winner_pos into multiple columns
  LISTELEC <- cSplit(LISTELEC, "winner_pos", ";")
  
  # e) Move to long format
  LISTELEC <- gather(LISTELEC, variable, winner_pos, winner_pos_1:winner_pos_7, factor_key=TRUE)
  
  # f) Keep only relevant rows (those that are not NA on winner_pos)
  LISTELEC <- LISTELEC[which(!is.na(LISTELEC$winner_pos)),]
  LISTELEC$elected <- 1
  
  # 2) Create another helper df where we generate the rank order on the list based on the values in PREDRELFINAL
  RANKS <- data.frame(LUCANDS$pers_id, LUCANDS$list_id, PREDRELFINAL[,i])
  colnames(RANKS) <- c("pers_id", "list_id", "predfinal_votes")
  
  # 3) Create a rank order per list
  # a) Order by list_id and then the number of votes
  RANKS <- RANKS[order(RANKS$list_id,RANKS$predfinal_votes),]
  
  # b) Invert the row order, so we have a descending number of votes
  RANKS<- RANKS[seq(dim(RANKS)[1],1),]
  
  # c) Add a rank order
  RANKS$rank_order <- ave(RANKS$predfinal_votes, RANKS$list_id, FUN = seq_along)
  
  
  # 4) Add information on whether someone would have one a seat to RANKS
  RANKS <- sqldf("SELECT RANKS.*, LISTELEC.elected
                 FROM RANKS LEFT JOIN LISTELEC
                 ON
                 (RANKS.list_id = LISTELEC.list_id)
                 AND
                 (RANKS.rank_order = LISTELEC.winner_pos)
                 ")
  
  # Set NAs to 0
  RANKS[which(is.na(RANKS$elected)),]$elected <- 0
  
  # 5) Add information from PREDRELFINAL and RANKS to PREDRELELC
  # a) The predicted number of votes (z-standardised) from PREDRELFINAL
  PREDRELELEC$predfinal <- PREDRELFINAL[,i]
  colnames(PREDRELELEC)[ncol(PREDRELELEC)] <- colnames(PREDRELFINAL)[i]
  
  # b) The elected status from RANK
  PREDRELELEC <- sqldf("SELECT PREDRELELEC.*, RANKS.elected
                 FROM PREDRELELEC LEFT JOIN RANKS
                 ON
                 (PREDRELELEC.list_id = RANKS.list_id)
                 AND
                 (PREDRELELEC.pers_id = RANKS.pers_id)
                 ")
  
  colnames(PREDRELELEC)[ncol(PREDRELELEC)] <- paste(cfvars[i], "_predelected", sep="")
  
  
  # 6) Remove dfs
  rm(RANKS, LISTELEC)
  
  
  setTxtProgressBar(pb, i)
}
close(pb)



# 6) Count how often different candidates would (not) have been elected.
PREDRELOVERVIEW <-  data.frame(matrix(nrow = length(cfvars), ncol = 7))
colnames(PREDRELOVERVIEW) <- c("variable", "real_cond", "cf_cond",
                               "elect_1_predelect_1", "elect_1_predelect_0", "elect_0_predelect_1", "elect_0_predelect_0")

for (i in 1:length(cfvars))
{
  pb <- txtProgressBar(min = 1, max = length(cfvars), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Define the relevant predelected column in PREDRELELEC
  varname <- paste(cfvars[i], "_predelected", sep="") # varname to search
  varpos <- grep(varname, colnames(PREDRELELEC))
  
  # 2) Count the number of cases for elect_1_predelect_1 (elected in actuality, predicted to be elected by the counterfactual model)
  PREDRELOVERVIEW$elect_1_predelect_1[i] <- nrow(PREDRELELEC[which(PREDRELELEC$elected_status == 1 & PREDRELELEC[,varpos] == 1),])
  
  # 3) Count the number of cases for elect_1_predelect_0 (elected in actuality, but not elected in the counterfactual model)
  PREDRELOVERVIEW$elect_1_predelect_0[i] <- nrow(PREDRELELEC[which(PREDRELELEC$elected_status == 1 & PREDRELELEC[,varpos] == 0),])
  
  # 4) Count the number of cases for elect_0_predelect_1 (not actually elected, predicted to be elected by the counterfactual model)
  PREDRELOVERVIEW$elect_0_predelect_1[i] <- nrow(PREDRELELEC[which(PREDRELELEC$elected_status == 0 & PREDRELELEC[,varpos] == 1),])
  
  # 5) Count the number of cases for elect_0_predelect_0 (not actually elected, and not elected in the counterfactual model)
  PREDRELOVERVIEW$elect_0_predelect_0[i] <- nrow(PREDRELELEC[which(PREDRELELEC$elected_status == 0 & PREDRELELEC[,varpos] == 0),])
  
  # 6) The variable in question  
  PREDRELOVERVIEW$variable[i] <- cfvars[i]
  
  
  setTxtProgressBar(pb, i)
}
close(pb)



# 7) Add information on conditions (real and counterfactual)

for (i in 1:nrow(PREDRELOVERVIEW))
{
  pb <- txtProgressBar(min = 1, max = nrow(PREDRELOVERVIEW), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Fill in the counterfactual condition
  PREDRELOVERVIEW$cf_cond[i] <- paste(CFCONDS$cf_cond[i], ": ", nrow(LUCANDS), sep="")
  
  # 2) For (regular) categorical variables, use tabled information
  if(grepl("^(gender2|non_swiss|prof_org_act2|locpuboff_highest_now2|regparl_incumb|locptyoff_any|regptyoff_any|precumul2)$", PREDRELOVERVIEW$variable[i])){
    # a) Get variable name
    varname <- PREDRELOVERVIEW$variable[i]
    
    # b) Find the variable position in LUCANDS
    varpos <- grep(varname, colnames(LUCANDS))
    
    # c) Table the variable
    TABLE <- data.frame(table(LUCANDS[,varpos]))
    
    # d) Obtain a string
    PREDRELOVERVIEW$real_cond[i] <- paste(paste(TABLE$Var1, ": ", TABLE$Freq, sep=""), collapse="\n")
  }
  
  # 3) For continuous measures on IG ties, calculate the share of zeroes and above-zeroes
  if(grepl("^(ig_formalties_eco_locreg|ig_formalties_fir_locreg|ig_formalties_hob_locreg|ig_formalties_ide_locreg|ig_formalties_pig_locreg)$", PREDRELOVERVIEW$variable[i])){
    # a) Get variable name
    varname <- PREDRELOVERVIEW$variable[i]
    
    # b) Find the variable position in LUCANDS
    varpos <- grep(varname, colnames(LUCANDS)) 
    
    # c) Extract how many IG ties are 0 or above
    PREDRELOVERVIEW$real_cond[i] <- 
      paste("ties = 0: ",
            length(LUCANDS[which(LUCANDS[,varpos] == 0),][,varpos]),
            "\n",
            "ties > 0: ",
            length(LUCANDS[which(LUCANDS[,varpos] > 0),][,varpos]),
            sep="")
    
    # d) Make the text in cf_cond more descriptive
    PREDRELOVERVIEW$cf_cond[i] <- sub("^0:", "0 ties:", PREDRELOVERVIEW$cf_cond[i])
  }
  
  # 4) For ballot_pos_rel1, we use a special approach
  if(grepl("^(ballot_pos_rel1)$", PREDRELOVERVIEW$variable[i])){
    # a) Find out how many candidates have their first list position above or below 0.5
    PREDRELOVERVIEW$real_cond[i] <- 
      paste("position < 0.6: ",
            length(LUCANDS[which(LUCANDS$ballot_pos_rel1 < 0.6),]$ballot_pos_rel1),
            "\n",
            "position >= 0.6: ",
            length(LUCANDS[which(LUCANDS$ballot_pos_rel1 >= 0.6),]$ballot_pos_rel1),
            sep="")
    # b) Make the text in cf_cond more descriptive
    PREDRELOVERVIEW$cf_cond[i] <- sub("^1:", "position = 1:", PREDRELOVERVIEW$cf_cond[i])
  }
  
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 8) Plot PREDRELOVERVIEW
# a) Move PREDRELOVERVIEW to long format
PREDRELOVERVIEWLONG<- gather(PREDRELOVERVIEW, elected_status, elected_num, elect_1_predelect_1:elect_0_predelect_0, factor_key=TRUE)

# b) Sort by PREDRELOVERVIEW$variable
PREDRELOVERVIEWLONG <- PREDRELOVERVIEWLONG[order(match(PREDRELOVERVIEWLONG$variable, PREDRELOVERVIEW$variable)),]
PREDRELOVERVIEWLONG$variable <- factor(PREDRELOVERVIEWLONG$variable, levels = unique(PREDRELOVERVIEWLONG$variable))


# c) Plot
counterfact_elecsucc_rel_noanchor <- 
  ggplot(PREDRELOVERVIEWLONG[which(grepl("elect_1_predelect_1|elect_0_predelect_1",PREDRELOVERVIEWLONG$elected_status) & PREDRELOVERVIEWLONG$elected_num > 0),],
         aes(fill=elected_status, y=elected_num, x=reorder(variable, desc(variable)))) +
  geom_bar(position="stack", stat="identity", colour="black") +
  geom_text(size=3, aes(color = elected_status, label = elected_num),
            position = position_stack(vjust = 0.5),show.legend = FALSE) +
  scale_fill_manual(name = "elected_status",
                    values =  c("#525252", "#cccccc"),
                    breaks = c("elect_0_predelect_1","elect_1_predelect_1"), # Legend order
                    labels=c('Different electee', 'Same electee')) +
  scale_color_manual(values = c("black", "white"),
                     breaks = c("elect_1_predelect_1","elect_0_predelect_1")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80"),
        axis.title.y = element_text(size = 13),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.25,0.0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous("Number of seats in parliament", position = "left",
                     limits = c(0, 120), breaks=seq(0,120,by = 20),
                     expand = c(0, 0)) + # removes lines between labels, e.g. gender2 and the bars
  scale_x_discrete("Electoral success of candidates if everyone...", position = "bottom",
                   labels=c('gender2'  = 'were non-female', 'non_swiss'  = 'had a Swiss surname', 
                            'prof_org_act2'  = 'worked a regular job', 'locpuboff_highest_now2'  = 'had no local public office', 
                            'regparl_incumb'  = 'were non-incumbent', 'locptyoff_any'  = 'had no local party role', 
                            'regptyoff_any'  = 'had no regional party role', 'ig_formalties_eco_locreg'  = 'had no formal ties to\neconomic interest groups', 
                            'ig_formalties_fir_locreg'  = 'had no formal ties to\nfirms', 'ig_formalties_hob_locreg'  = 'had no formal ties to\nhobby and leisure groups', 
                            'ig_formalties_ide_locreg'  = 'had no formal ties to\nidentity groups', 'ig_formalties_pig_locreg'  = 'had no formal ties to\npublic interest groups', 
                            'precumul2'  = 'were not pre-cumulated\non their list', 'ballot_pos_rel1'  = 'had the first place\non their list')) +
  coord_flip()




counterfact_elecsucc_rel_noanchor




# 9) Obtain information on the alternative ranking based on the true predicted share of preference votes
# Create an overview of how many candidates per list were elected.
# a) An overview:
LISTELEC <- sqldf("SELECT DISTINCT LUCANDS.list_id, LUCANDS.list_seats_won
                      FROM LUCANDS
                ")

# b) Remove lists that did not win any seat
LISTELEC <- LISTELEC[which(LISTELEC$list_seats_won != 0),]

# c) Get all number in a certain range
LISTELEC$winner_pos <- lapply(LISTELEC$list_seats_won, function(x) paste(seq.int(1,x, by = 1), collapse=";"))
LISTELEC$winner_pos <- as.character(LISTELEC$winner_pos)

# d) Split winner_pos into multiple columns
LISTELEC <- cSplit(LISTELEC, "winner_pos", ";")

# e) Move to long format
LISTELEC <- gather(LISTELEC, variable, winner_pos, winner_pos_1:winner_pos_7, factor_key=TRUE)

# f) Keep only relevant rows (those that are not NA on winner_pos)
LISTELEC <- LISTELEC[which(!is.na(LISTELEC$winner_pos)),]
LISTELEC$elected <- 1

# 2) Create another helper df where we generate the rank order on the list based on the values in PREDFINAL
RANKS <- data.frame(LUCANDS$pers_id, LUCANDS$list_id, PREDTRUE$fit)
colnames(RANKS) <- c("pers_id", "list_id", "PREDFINAL_votes")

# 7) Create a rank order per list
# a) Order by list_id and then the number of votes
RANKS <- RANKS[order(RANKS$list_id,RANKS$PREDFINAL_votes),]

# b) Invert the row order, so we have a descending number of votes
RANKS<- RANKS[seq(dim(RANKS)[1],1),]

# c) Add a rank order
RANKS$rank_order <- ave(RANKS$PREDFINAL_votes, RANKS$list_id, FUN = seq_along)


# 8) Add information on whether someone would have one a seat to RANKS
RANKS <- sqldf("SELECT RANKS.*, LISTELEC.elected
               FROM RANKS LEFT JOIN LISTELEC
               ON
               (RANKS.list_id = LISTELEC.list_id)
               AND
               (RANKS.rank_order = LISTELEC.winner_pos)
               ")

# Set NAs to 0
RANKS[which(is.na(RANKS$elected)),]$elected <- 0

# 10) Add information from PREDFINAL and RANKS to PREDELC
# a) The predicted number of votes (z-standardised) from PREDFINAL
PREDRELELEC$true_rel_pred <- PREDRELTRUE$fit

# b) The elected status from RANK
PREDRELELEC <- sqldf("SELECT PREDRELELEC.*, RANKS.elected
               FROM PREDRELELEC LEFT JOIN RANKS
               ON
               (PREDRELELEC.list_id = RANKS.list_id)
               AND
               (PREDRELELEC.pers_id = RANKS.pers_id)
               ")

colnames(PREDRELELEC)[ncol(PREDRELELEC)] <- "true_rel_pred_noanch"


# 10) Remove dfs
rm(RANKS, LISTELEC)




# 11) Count how often different candidates would (not) have been elected.
PREDRELTRUEOVERVIEW <-  data.frame(matrix(nrow = 1, ncol = 7))
colnames(PREDRELTRUEOVERVIEW) <- c("variable", "real_cond", "cf_cond",
                                   "elect_1_predelect_1", "elect_1_predelect_0", "elect_0_predelect_1", "elect_0_predelect_0")


# a) Count the number of cases for elect_1_predelect_1 (elected in actuality, predicted to be elected by the counterfactual model)
PREDRELTRUEOVERVIEW$elect_1_predelect_1 <- nrow(PREDRELELEC[which(PREDRELELEC$elected_status == 1 & PREDRELELEC[length(PREDRELELEC)] == 1),])

# b) Count the number of cases for elect_1_predelect_0 (elected in actuality, but not elected in the counterfactual model)
PREDRELTRUEOVERVIEW$elect_1_predelect_0 <- nrow(PREDRELELEC[which(PREDRELELEC$elected_status == 1 & PREDRELELEC[length(PREDRELELEC)] == 0),])

# c) Count the number of cases for elect_0_predelect_1 (not actually elected, predicted to be elected by the counterfactual model)
PREDRELTRUEOVERVIEW$elect_0_predelect_1 <- nrow(PREDRELELEC[which(PREDRELELEC$elected_status == 0 & PREDRELELEC[length(PREDRELELEC)] == 1),])

# d) Count the number of cases for elect_0_predelect_0 (not actually elected, and not elected in the counterfactual model)
PREDRELTRUEOVERVIEW$elect_0_predelect_0 <- nrow(PREDRELELEC[which(PREDRELELEC$elected_status == 0 & PREDRELELEC[length(PREDRELELEC)] == 0),])

# e) The variable in question  
PREDRELTRUEOVERVIEW$variable <- "true_rel_pred_noanch"



#################################
### Logit-Based Non-Anchoring ###
#################################

# 1) Define the variables for which we want to obtain counterfactuals
cfvars <- c("gender2", "non_swiss", "prof_org_act2", "locpuboff_highest_now2", "regparl_incumb",
            "locptyoff_any", "regptyoff_any",
            "ig_formalties_eco_locreg", "ig_formalties_fir_locreg", "ig_formalties_hob_locreg",
            "ig_formalties_ide_locreg", "ig_formalties_pig_locreg", 
            "precumul2", "ballot_pos_rel1"
)


# 2) Generate the "true" predicted probabilities, i.e. based on the original input data
# a) Predicted probabilities for the existing data
PREDPROBTRUE <- predict(m1elec$glm_res, newdata = LUCANDS, type = "link", se.fit = TRUE)

# b) Generate the data
PREDPROBTRUE <- data.frame(
  pred_prob = plogis(PREDPROBTRUE$fit),
  lwr = plogis(PREDPROBTRUE$fit - 1.959963984540054 * PREDPROBTRUE$se.fit),
  upr = plogis(PREDPROBTRUE$fit + 1.959963984540054 * PREDPROBTRUE$se.fit)
)



# 3) Calculate counterfactual predictions and store in a df what the counterfactual condition was
PREDPROBCF <-list()

CFCONDS <- data.frame(matrix(nrow = length(cfvars), ncol = 2))
colnames(CFCONDS) <- c("variable", "cf_cond")

for (i in 1:length(cfvars))
{
  pb <- txtProgressBar(min = 1, max = length(cfvars), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Find the variable position of the variable to manipulate in LUCANDS
  varpos <- grep(cfvars[i], colnames(LUCANDS))
  
  # 2) Get LUCANDS and store a copy
  LUCANDSMOD <- get("LUCANDS")
  
  # 3) Alter the respective variable
  if(cfvars[i] == "gender2"){
    LUCANDSMOD[,varpos] <- "not female"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "not female"
  }
  
  if(cfvars[i] == "non_swiss"){
    LUCANDSMOD[,varpos] <- "0"
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "prof_org_act2"){
    LUCANDSMOD[,varpos] <- "NOT WOR"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "NOT WOR"
  }
  
  if(cfvars[i] == "locpuboff_highest_now2"){
    LUCANDSMOD[,varpos] <- "0 - no local public office"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0 - no local public office"
  }
  
  if(cfvars[i] == "regparl_incumb"){
    LUCANDSMOD[,varpos] <- "challenger"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "challenger"
  }
  
  if(cfvars[i] == "locptyoff_any"){
    LUCANDSMOD[,varpos] <- "0 - no role in party"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0 - no role in party"
  }
  
  if(cfvars[i] == "regptyoff_any"){
    LUCANDSMOD[,varpos] <- "0 - no role in party"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0 - no role in party"
  }
  
  if(cfvars[i] == "ig_formalties_eco_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_fir_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_hob_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_ide_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "ig_formalties_pig_locreg"){
    LUCANDSMOD[,varpos] <- 0
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "0"
  }
  
  if(cfvars[i] == "precumul2"){
    LUCANDSMOD[,varpos] <- "not pre-cumulated"
    LUCANDSMOD[,varpos] <- as.factor(LUCANDSMOD[,varpos])
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "not pre-cumulated"
  }
  
  if(cfvars[i] == "ballot_pos_rel1"){
    LUCANDSMOD[,varpos] <- 1
    # Add info to CFCONDS
    CFCONDS$variable[i] <- cfvars[i]
    CFCONDS$cf_cond[i] <- "1"
  }
  
  # 4) Calculate the counterfactual prediction
  # a) Predicted probabilities for the existing data
  TEMPPRED <- predict(m1elec$glm_res, newdata = LUCANDSMOD, type = "link", se.fit = TRUE)
  
  # b) Generate the data
  TEMPPRED <- data.frame(
    pred_prob = plogis(TEMPPRED$fit),
    lwr = plogis(TEMPPRED$fit - 1.959963984540054 * TEMPPRED$se.fit),
    upr = plogis(TEMPPRED$fit + 1.959963984540054 * TEMPPRED$se.fit)
  )
  
  
  # 5) Add the predictions from TEMPPRED to the list PREDPROBCF
  PREDPROBCF[[toupper(cfvars[i])]] <- TEMPPRED
  rm(TEMPPRED,LUCANDSMOD)
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 4) Extract the counterfactual score from PREDPROBCF
PREDPROBFINAL <-  data.frame(matrix(nrow = nrow(LUCANDS), ncol = 0)) 

for (i in 1:length(PREDPROBCF))
{
  pb <- txtProgressBar(min = 1, max = length(PREDPROBCF), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Extract
  PREDPROBFINAL$newcol <- PREDPROBCF[[i]]$pred_prob
  
  # 2) Rename that column
  colnames(PREDPROBFINAL)[colnames(PREDPROBFINAL) == 'newcol'] <- paste(cfvars[i], "_cf_pred_prob", sep="")
  
  setTxtProgressBar(pb, i)
}
close(pb)




# 5) Calculate whether the candidate would have been elected with the final predicted (counterfactual) number of votes
# a) Store the information in an overview
toselect <- c(c("pers_id","list_id", "elected_status"), cfvars)
PREDPROBELEC <- subset(LUCANDS, select = toselect)
rm(toselect)

for (i in 1:ncol(PREDPROBFINAL))
{
  pb <- txtProgressBar(min = 1, max = ncol(PREDPROBFINAL), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Create an overview of how many candidates per list were elected.
  # a) An overview:
  LISTELEC <- sqldf("SELECT DISTINCT LUCANDS.list_id, LUCANDS.list_seats_won
                        FROM LUCANDS
                  ")
  
  # b) Remove lists that did not win any seat
  LISTELEC <- LISTELEC[which(LISTELEC$list_seats_won != 0),]
  
  # c) Get all number in a certain range
  LISTELEC$winner_pos <- lapply(LISTELEC$list_seats_won, function(x) paste(seq.int(1,x, by = 1), collapse=";"))
  LISTELEC$winner_pos <- as.character(LISTELEC$winner_pos)
  
  # d) Split winner_pos into multiple columns
  LISTELEC <- cSplit(LISTELEC, "winner_pos", ";")
  
  # e) Move to long format
  LISTELEC <- gather(LISTELEC, variable, winner_pos, winner_pos_1:winner_pos_7, factor_key=TRUE)
  
  # f) Keep only relevant rows (those that are not NA on winner_pos)
  LISTELEC <- LISTELEC[which(!is.na(LISTELEC$winner_pos)),]
  LISTELEC$elected <- 1
  
  # 2) Create another helper df where we generate the rank order on the list based on the values in PREDPROBFINAL
  RANKS <- data.frame(LUCANDS$pers_id, LUCANDS$list_id, PREDPROBFINAL[,i])
  colnames(RANKS) <- c("pers_id", "list_id", "PREDPROBFINAL_votes")
  
  # 3) Create a rank order per list
  # a) Order by list_id and then the number of votes
  RANKS <- RANKS[order(RANKS$list_id,RANKS$PREDPROBFINAL_votes),]
  
  # b) Invert the row order, so we have a descending number of votes
  RANKS<- RANKS[seq(dim(RANKS)[1],1),]
  
  # c) Add a rank order
  RANKS$rank_order <- ave(RANKS$PREDPROBFINAL_votes, RANKS$list_id, FUN = seq_along)
  
  
  # 4) Add information on whether someone would have one a seat to RANKS
  RANKS <- sqldf("SELECT RANKS.*, LISTELEC.elected
                 FROM RANKS LEFT JOIN LISTELEC
                 ON
                 (RANKS.list_id = LISTELEC.list_id)
                 AND
                 (RANKS.rank_order = LISTELEC.winner_pos)
                 ")
  
  # Set NAs to 0
  RANKS[which(is.na(RANKS$elected)),]$elected <- 0
  
  # 5) Add information from PREDPROBFINAL and RANKS to PREDELC
  # a) The predicted number of votes (z-standardised) from PREDPROBFINAL
  PREDPROBELEC$PREDPROBFINAL <- PREDPROBFINAL[,i]
  colnames(PREDPROBELEC)[ncol(PREDPROBELEC)] <- colnames(PREDPROBFINAL)[i]
  
  # b) The elected status from RANK
  PREDPROBELEC <- sqldf("SELECT PREDPROBELEC.*, RANKS.elected
                 FROM PREDPROBELEC LEFT JOIN RANKS
                 ON
                 (PREDPROBELEC.list_id = RANKS.list_id)
                 AND
                 (PREDPROBELEC.pers_id = RANKS.pers_id)
                 ")
  
  colnames(PREDPROBELEC)[ncol(PREDPROBELEC)] <- paste(cfvars[i], "_PREDPROBELECted", sep="")
  
  
  # 6) Remove dfs
  rm(RANKS, LISTELEC)
  
  
  setTxtProgressBar(pb, i)
}
close(pb)



# 7) Count how often different candidates would (not) have been elected.
PREDPROBOVERVIEW <-  data.frame(matrix(nrow = length(cfvars), ncol = 7))
colnames(PREDPROBOVERVIEW) <- c("variable", "real_cond", "cf_cond",
                                "elect_1_predelect_1", "elect_1_predelect_0", "elect_0_predelect_1", "elect_0_predelect_0")

for (i in 1:length(cfvars))
{
  pb <- txtProgressBar(min = 1, max = length(cfvars), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Define the relevant PREDPROBELECted column in PREDPROBELEC
  varname <- paste(cfvars[i], "_PREDPROBELECted", sep="") # varname to search
  varpos <- grep(varname, colnames(PREDPROBELEC))
  
  # 2) Count the number of cases for elect_1_predelect_1 (elected in actuality, predicted to be elected by the counterfactual model)
  PREDPROBOVERVIEW$elect_1_predelect_1[i] <- nrow(PREDPROBELEC[which(PREDPROBELEC$elected_status == 1 & PREDPROBELEC[,varpos] == 1),])
  
  # 3) Count the number of cases for elect_1_predelect_0 (elected in actuality, but not elected in the counterfactual model)
  PREDPROBOVERVIEW$elect_1_predelect_0[i] <- nrow(PREDPROBELEC[which(PREDPROBELEC$elected_status == 1 & PREDPROBELEC[,varpos] == 0),])
  
  # 4) Count the number of cases for elect_0_predelect_1 (not actually elected, predicted to be elected by the counterfactual model)
  PREDPROBOVERVIEW$elect_0_predelect_1[i] <- nrow(PREDPROBELEC[which(PREDPROBELEC$elected_status == 0 & PREDPROBELEC[,varpos] == 1),])
  
  # 5) Count the number of cases for elect_0_predelect_0 (not actually elected, and not elected in the counterfactual model)
  PREDPROBOVERVIEW$elect_0_predelect_0[i] <- nrow(PREDPROBELEC[which(PREDPROBELEC$elected_status == 0 & PREDPROBELEC[,varpos] == 0),])
  
  # 6) The variable in question  
  PREDPROBOVERVIEW$variable[i] <- cfvars[i]
  
  
  setTxtProgressBar(pb, i)
}
close(pb)




# 8) Add information on conditions (real and counterfactual)

for (i in 1:nrow(PREDPROBOVERVIEW))
{
  pb <- txtProgressBar(min = 1, max = nrow(PREDPROBOVERVIEW), style = 3) # takes a while to run, so lets add a progress bar
  
  # 1) Fill in the counterfactual condition
  PREDPROBOVERVIEW$cf_cond[i] <- paste(CFCONDS$cf_cond[i], ": ", nrow(LUCANDS), sep="")
  
  # 2) For (regular) categorical variables, use tabled information
  if(grepl("^(gender2|non_swiss|prof_org_act2|locpuboff_highest_now2|regparl_incumb|locptyoff_any|regptyoff_any|precumul2)$", PREDPROBOVERVIEW$variable[i])){
    # a) Get variable name
    varname <- PREDPROBOVERVIEW$variable[i]
    
    # b) Find the variable position in LUCANDS
    varpos <- grep(varname, colnames(LUCANDS))
    
    # c) Table the variable
    TABLE <- data.frame(table(LUCANDS[,varpos]))
    
    # d) Obtain a string
    PREDPROBOVERVIEW$real_cond[i] <- paste(paste(TABLE$Var1, ": ", TABLE$Freq, sep=""), collapse="\n")
  }
  
  # 3) For continuous measures on IG ties, calculate the share of zeroes and above-zeroes
  if(grepl("^(ig_formalties_eco_locreg|ig_formalties_fir_locreg|ig_formalties_hob_locreg|ig_formalties_ide_locreg|ig_formalties_pig_locreg)$", PREDPROBOVERVIEW$variable[i])){
    # a) Get variable name
    varname <- PREDPROBOVERVIEW$variable[i]
    
    # b) Find the variable position in LUCANDS
    varpos <- grep(varname, colnames(LUCANDS)) 
    
    # c) Extract how many IG ties are 0 or above
    PREDPROBOVERVIEW$real_cond[i] <- 
      paste("ties = 0: ",
            length(LUCANDS[which(LUCANDS[,varpos] == 0),][,varpos]),
            "\n",
            "ties > 0: ",
            length(LUCANDS[which(LUCANDS[,varpos] > 0),][,varpos]),
            sep="")
    
    # d) Make the text in cf_cond more descriptive
    PREDPROBOVERVIEW$cf_cond[i] <- sub("^0:", "0 ties:", PREDPROBOVERVIEW$cf_cond[i])
  }
  
  # 4) For ballot_pos_rel1, we use a special approach
  if(grepl("^(ballot_pos_rel1)$", PREDPROBOVERVIEW$variable[i])){
    # a) Find out how many candidates have their first list position above or below 0.5
    PREDPROBOVERVIEW$real_cond[i] <- 
      paste("position < 0.6: ",
            length(LUCANDS[which(LUCANDS$ballot_pos_rel1 < 0.6),]$ballot_pos_rel1),
            "\n",
            "position >= 0.6: ",
            length(LUCANDS[which(LUCANDS$ballot_pos_rel1 >= 0.6),]$ballot_pos_rel1),
            sep="")
    # b) Make the text in cf_cond more descriptive
    PREDPROBOVERVIEW$cf_cond[i] <- sub("^1:", "position = 1:", PREDPROBOVERVIEW$cf_cond[i])
  }
  
  
  setTxtProgressBar(pb, i)
}
close(pb)


# 9) Plot PREDPROBOVERVIEW
# a) Move PREDPROBOVERVIEW to long format
PREDPROBOVERVIEWLONG<- gather(PREDPROBOVERVIEW, elected_status, elected_num, elect_1_predelect_1:elect_0_predelect_0, factor_key=TRUE)

# b) Sort by PREDPROBOVERVIEW$variable
PREDPROBOVERVIEWLONG <- PREDPROBOVERVIEWLONG[order(match(PREDPROBOVERVIEWLONG$variable, PREDPROBOVERVIEW$variable)),]
PREDPROBOVERVIEWLONG$variable <- factor(PREDPROBOVERVIEWLONG$variable, levels = unique(PREDPROBOVERVIEWLONG$variable))


# c) Plot
counterfact_elecsucc_predprob <- 
  ggplot(PREDPROBOVERVIEWLONG[which(grepl("elect_1_predelect_1|elect_0_predelect_1",PREDPROBOVERVIEWLONG$elected_status) & PREDPROBOVERVIEWLONG$elected_num > 0),],
         aes(fill=elected_status, y=elected_num, x=reorder(variable, desc(variable)))) +
  geom_bar(position="stack", stat="identity", colour="black") +
  geom_text(size=3, aes(color = elected_status, label = elected_num),
            position = position_stack(vjust = 0.5),show.legend = FALSE) +
  scale_fill_manual(name = "elected_status",
                    values =  c("#525252", "#cccccc"),
                    breaks = c("elect_0_predelect_1","elect_1_predelect_1"), # Legend order
                    labels=c('Different electee', 'Same electee')) +
  scale_color_manual(values = c("black", "white"),
                     breaks = c("elect_1_predelect_1","elect_0_predelect_1")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80"),
        axis.title.y = element_text(size = 13),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0.25,0.0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous("Number of seats in parliament", position = "left",
                     limits = c(0, 120), breaks=seq(0,120,by = 20),
                     expand = c(0, 0)) + # removes lines between labels, e.g. gender2 and the bars
  scale_x_discrete("Electoral success of candidates if everyone...", position = "bottom",
                   labels=c('gender2'  = 'were non-female', 'non_swiss'  = 'had a Swiss surname', 
                            'prof_org_act2'  = 'worked a regular job', 'locpuboff_highest_now2'  = 'had no local public office', 
                            'regparl_incumb'  = 'were non-incumbent', 'locptyoff_any'  = 'had no local party role', 
                            'regptyoff_any'  = 'had no regional party role', 'ig_formalties_eco_locreg'  = 'had no formal ties to\neconomic interest groups', 
                            'ig_formalties_fir_locreg'  = 'had no formal ties to\nfirms', 'ig_formalties_hob_locreg'  = 'had no formal ties to\nhobby and leisure groups', 
                            'ig_formalties_ide_locreg'  = 'had no formal ties to\nidentity groups', 'ig_formalties_pig_locreg'  = 'had no formal ties to\npublic interest groups', 
                            'precumul2'  = 'were not pre-cumulated\non their list', 'ballot_pos_rel1'  = 'had the first place\non their list')) +
  coord_flip()




counterfact_elecsucc_predprob




# 10) Obtain information on the alternative ranking based on the true predicted probabilities
# Create an overview of how many candidates per list were elected.
# a) An overview:
LISTELEC <- sqldf("SELECT DISTINCT LUCANDS.list_id, LUCANDS.list_seats_won
                      FROM LUCANDS
                ")

# b) Remove lists that did not win any seat
LISTELEC <- LISTELEC[which(LISTELEC$list_seats_won != 0),]

# c) Get all number in a certain range
LISTELEC$winner_pos <- lapply(LISTELEC$list_seats_won, function(x) paste(seq.int(1,x, by = 1), collapse=";"))
LISTELEC$winner_pos <- as.character(LISTELEC$winner_pos)

# d) Split winner_pos into multiple columns
LISTELEC <- cSplit(LISTELEC, "winner_pos", ";")

# e) Move to long format
LISTELEC <- gather(LISTELEC, variable, winner_pos, winner_pos_1:winner_pos_7, factor_key=TRUE)

# f) Keep only relevant rows (those that are not NA on winner_pos)
LISTELEC <- LISTELEC[which(!is.na(LISTELEC$winner_pos)),]
LISTELEC$elected <- 1

# 2) Create another helper df where we generate the rank order on the list based on the values in PREDPROBFINAL
RANKS <- data.frame(LUCANDS$pers_id, LUCANDS$list_id, PREDPROBTRUE$pred_prob)
colnames(RANKS) <- c("pers_id", "list_id", "PREDPROBFINAL_votes")

# 11) Create a rank order per list
# a) Order by list_id and then the number of votes
RANKS <- RANKS[order(RANKS$list_id,RANKS$PREDPROBFINAL_votes),]

# b) Invert the row order, so we have a descending number of votes
RANKS<- RANKS[seq(dim(RANKS)[1],1),]

# c) Add a rank order
RANKS$rank_order <- ave(RANKS$PREDPROBFINAL_votes, RANKS$list_id, FUN = seq_along)


# 12) Add information on whether someone would have one a seat to RANKS
RANKS <- sqldf("SELECT RANKS.*, LISTELEC.elected
               FROM RANKS LEFT JOIN LISTELEC
               ON
               (RANKS.list_id = LISTELEC.list_id)
               AND
               (RANKS.rank_order = LISTELEC.winner_pos)
               ")

# Set NAs to 0
RANKS[which(is.na(RANKS$elected)),]$elected <- 0

# 13) Add information from PREDPROBFINAL and RANKS to PREDELC
# a) The predicted number of votes (z-standardised) from PREDPROBFINAL
PREDPROBELEC$true_pred_prob <- PREDPROBTRUE$pred_prob

# b) The elected status from RANK
PREDPROBELEC <- sqldf("SELECT PREDPROBELEC.*, RANKS.elected
               FROM PREDPROBELEC LEFT JOIN RANKS
               ON
               (PREDPROBELEC.list_id = RANKS.list_id)
               AND
               (PREDPROBELEC.pers_id = RANKS.pers_id)
               ")

colnames(PREDPROBELEC)[ncol(PREDPROBELEC)] <- "true_pred_prob_PREDPROBELECted"


# 14) Remove dfs
rm(RANKS, LISTELEC)




# 15) Count how often different candidates would (not) have been elected.
PREDPROBTRUEOVERVIEW <-  data.frame(matrix(nrow = 1, ncol = 7))
colnames(PREDPROBTRUEOVERVIEW) <- c("variable", "real_cond", "cf_cond",
                                    "elect_1_predelect_1", "elect_1_predelect_0", "elect_0_predelect_1", "elect_0_predelect_0")


# a) Count the number of cases for elect_1_predelect_1 (elected in actuality, predicted to be elected by the counterfactual model)
PREDPROBTRUEOVERVIEW$elect_1_predelect_1 <- nrow(PREDPROBELEC[which(PREDPROBELEC$elected_status == 1 & PREDPROBELEC[length(PREDPROBELEC)] == 1),])

# b) Count the number of cases for elect_1_predelect_0 (elected in actuality, but not elected in the counterfactual model)
PREDPROBTRUEOVERVIEW$elect_1_predelect_0 <- nrow(PREDPROBELEC[which(PREDPROBELEC$elected_status == 1 & PREDPROBELEC[length(PREDPROBELEC)] == 0),])

# c) Count the number of cases for elect_0_predelect_1 (not actually elected, predicted to be elected by the counterfactual model)
PREDPROBTRUEOVERVIEW$elect_0_predelect_1 <- nrow(PREDPROBELEC[which(PREDPROBELEC$elected_status == 0 & PREDPROBELEC[length(PREDPROBELEC)] == 1),])

# d) Count the number of cases for elect_0_predelect_0 (not actually elected, and not elected in the counterfactual model)
PREDPROBTRUEOVERVIEW$elect_0_predelect_0 <- nrow(PREDPROBELEC[which(PREDPROBELEC$elected_status == 0 & PREDPROBELEC[length(PREDPROBELEC)] == 0),])

# e) The variable in question  
PREDPROBTRUEOVERVIEW$variable <- "true_pred_prob"



# 15) Create a combined figure for all three benchmark approaches
PREDNOANCHOVERVIEWLONG$group <- "counterfactual_votes"
PREDPROBOVERVIEWLONG$group <- "counterfactual_win_seat"
PREDRELOVERVIEWLONG$group <- "counterfactual_vote_share"

PREDCOMBINED <- smartbind(PREDNOANCHOVERVIEWLONG,PREDRELOVERVIEWLONG, PREDPROBOVERVIEWLONG)
PREDCOMBINED$group <- factor(PREDCOMBINED$group, levels = unique(PREDCOMBINED$group))


# 16) Plot the figure
# Define the expressions as labels for facets

facet_labels <- as_labeller(c(
  counterfactual_votes = 'hat("counterfactual votes")',
  counterfactual_vote_share = 'hat("counterfactual vote share")',
  counterfactual_win_seat = 'hat(Pr) * "(counterfactual win seat)"'), label_parsed
)



# Modify the plot with increased space between facets
counterfact_elecsucc_combined_noanchor <- 
  ggplot(PREDCOMBINED[which(grepl("elect_1_predelect_1|elect_0_predelect_1",PREDCOMBINED$elected_status) & PREDCOMBINED$elected_num > 0),],
         aes(fill=elected_status, y=elected_num, x=reorder(variable, desc(variable)))) +
  geom_bar(position="stack", stat="identity", colour="black") +
  geom_text(size=3, aes(color = elected_status, label = elected_num),
            position = position_stack(vjust = 0.5), show.legend = FALSE) +
  scale_fill_manual(name = "elected_status",
                    values =  c("#525252", "#cccccc"),
                    breaks = c("elect_0_predelect_1","elect_1_predelect_1"),
                    labels=c('Different electee', 'Same electee')) +
  scale_color_manual(values = c("black", "white"),
                     breaks = c("elect_1_predelect_1","elect_0_predelect_1")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey80"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey80"),
        axis.title.y = element_text(size = 13),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        strip.text = element_text(size = 10), # facet wrap font size
        strip.background = element_rect(fill = "white", colour = "black", size = 0.5),
        plot.margin = unit(c(0,0.25,0.0,0.0), "cm"),
        panel.spacing = unit(0.4, "cm")) +  # adjust space between facet panels here
  scale_y_continuous("Number of seats in parliament", position = "left",
                     limits = c(0, 120), breaks=seq(0,120,by = 20),
                     expand = c(0, 0)) +
  scale_x_discrete("Electoral success of candidates if everyone...", position = "bottom",
                   labels=c('gender2'  = 'were non-female', 'non_swiss'  = 'had a Swiss surname', 
                            'prof_org_act2'  = 'worked a regular job', 'locpuboff_highest_now2'  = 'had no local public office', 
                            'regparl_incumb'  = 'were non-incumbent', 'locptyoff_any'  = 'had no local party role', 
                            'regptyoff_any'  = 'had no regional party role', 'ig_formalties_eco_locreg'  = 'had no formal ties to\neconomic interest groups', 
                            'ig_formalties_fir_locreg'  = 'had no formal ties to\nfirms', 'ig_formalties_hob_locreg'  = 'had no formal ties to\nhobby and leisure groups', 
                            'ig_formalties_ide_locreg'  = 'had no formal ties to\nidentity groups', 'ig_formalties_pig_locreg'  = 'had no formal ties to\npublic interest groups', 
                            'precumul2'  = 'were not pre-cumulated\non their list', 'ballot_pos_rel1'  = 'had the first place\non their list')) +
  coord_flip() +
  facet_wrap(~ group, labeller = facet_labels)

counterfact_elecsucc_combined_noanchor


# 17) Export figure
completefilename <- paste(getwd(),"./Figure3_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".tif", sep = "")
tiff(completefilename,height = 17, width = 19, units = "cm", compression = "lzw", res = 1200)
counterfact_elecsucc_combined_noanchor
dev.off()




#############
#############
# Figure A3 #
#############
#############


# 1) Determine what group each of the 870 candidates belongs to
# a) Generate a df
GROUPDIFF <- subset(PREDELEC, select = c("pers_id", "list_id", "total_votes_zlist", "elected_status"))

# b) Merge information on predicted-to-be-elected
PREDELEC[,which(grepl("predelected", colnames(PREDELEC)))]

GROUPDIFF$predicted_elected <- rowSums(PREDELEC[,which(grepl("predelected", colnames(PREDELEC)))])

# c) Assign the group
GROUPDIFF$cand_group <- ""
GROUPDIFF[which(GROUPDIFF$elected_status == 1),]$cand_group <- "true electee"
GROUPDIFF[which(GROUPDIFF$elected_status == 1 & GROUPDIFF$predicted_elected < 14),]$cand_group <- "counterfactual non-electee"
GROUPDIFF[which(GROUPDIFF$elected_status == 0 & GROUPDIFF$predicted_elected > 0),]$cand_group <- "counterfactual electee"
GROUPDIFF[which(GROUPDIFF$elected_status == 0 & GROUPDIFF$predicted_elected == 0),]$cand_group <- "hopeless candidate"




# 2) Compare groups
# Plot the means for total_votes_zlist for true electee, counterfactual electee, and hopeless candidate

# 1) true electee vs counterfactual electee
a <- GROUPDIFF[which(grepl("true electee", GROUPDIFF$cand_group)),]$total_votes_zlist
b <- GROUPDIFF[which(grepl("counterfactual non-electee", GROUPDIFF$cand_group)),]$total_votes_zlist

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "True electees vs counterfactual non-electees"
DICHOTEST$dv <- "total votes"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST


# Create a copy of DICHOTEST
TESTVOTE <- DICHOTEST

rm(a,b,result, DICHOTEST)



# 2) Counterfactual electee vs hopeless candidate
a <- GROUPDIFF[which(grepl("counterfactual non-electee", GROUPDIFF$cand_group)),]$total_votes_zlist
b <- GROUPDIFF[which(grepl("counterfactual electee", GROUPDIFF$cand_group)),]$total_votes_zlist

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "Counterfactual non-electees vs counterfactual electees"
DICHOTEST$dv <- "total votes"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST

TESTVOTE <- smartbind(TESTVOTE,DICHOTEST)
rm(a,b,result, DICHOTEST)



# 2) Counterfactual electee vs hopeless candidate
a <- GROUPDIFF[which(grepl("counterfactual electee", GROUPDIFF$cand_group)),]$total_votes_zlist
b <- GROUPDIFF[which(grepl("hopeless candidate", GROUPDIFF$cand_group)),]$total_votes_zlist

result <- t.test(a, b)

DICHOTEST <- data.frame()
DICHOTEST <- data.frame(cbind(result[['data.name']],result[['estimate']][1], result[['estimate']][2],
                              result[['statistic']], result[['p.value']],
                              result[['alternative']], result[['method']]), stringsAsFactors=FALSE)
colnames(DICHOTEST) <- c("comparison","mean1", "mean2","t", "pvalue", "alternative","method")
DICHOTEST$comparison <- "Counterfactual electees vs hopeless candidates"
DICHOTEST$dv <- "total votes"
DICHOTEST %>% relocate(dv, .after=comparison) -> DICHOTEST

TESTVOTE <- smartbind(TESTVOTE,DICHOTEST)
rm(a,b,result, DICHOTEST)




# 3) Store means in 1 instead of 2 columns
# a) Empty df
MEANSVOTES <- setNames(data.frame(matrix(ncol = 2, nrow = 4)), c("category", "mean"))

# b) Store values
MEANSVOTES[1,1] <- "Always-elected candidates"
MEANSVOTES[1,2] <- mean(GROUPDIFF[which(grepl("true electee", GROUPDIFF$cand_group)),]$total_votes_zlist, na.rm = TRUE)

MEANSVOTES[2,1] <- "Counterfactual non-electees"
MEANSVOTES[2,2] <- mean(GROUPDIFF[which(grepl("counterfactual non-electee", GROUPDIFF$cand_group)),]$total_votes_zlist, na.rm = TRUE)

MEANSVOTES[3,1] <- "Counterfactual electees"
MEANSVOTES[3,2] <- mean(GROUPDIFF[which(grepl("counterfactual electee", GROUPDIFF$cand_group)),]$total_votes_zlist, na.rm = TRUE)

MEANSVOTES[4,1] <- "Never-elected candidates"
MEANSVOTES[4,2] <- mean(GROUPDIFF[which(grepl("hopeless candidate", GROUPDIFF$cand_group)),]$total_votes_zlist, na.rm = TRUE)




# 5) Generate significance starts in a separate table
# a) Generate a separate data frame
STARS <- subset(TESTVOTE, select = c("comparison", "pvalue"))
STARS <- sqldf("SELECT DISTINCT STARS.*
                       FROM STARS
                    ")
STARS$pvalue <- as.numeric(STARS$pvalue)

# b) Calculate the stars variable
STARS$stars <- ifelse(STARS$pvalue<0.001, STARS$stars <- "***", ifelse(STARS$pvalue<0.01, STARS$stars <- "**", ifelse(STARS$pvalue<0.05, STARS$stars <- "*", ifelse(STARS$pvalue>=0.05, STARS$stars <- "n.s.", ""))))


# 6) Order the variables' factor levels according to row order
MEANSVOTES <- mutate(MEANSVOTES, category = factor(MEANSVOTES$category, rev(MEANSVOTES$category[1:nrow(MEANSVOTES)])))


# 7) Plotting
centeralignlabel <- function(x) stringr::str_wrap(x, width = 16) # Source: https://stackoverflow.com/questions/63403524/multi-line-and-center-align-x-axis-labels-using-ggplot



figurecandgroups <- ggplot() +
  geom_bar(data=MEANSVOTES, aes(x=category, y=mean),stat="identity", colour="black", position="dodge") +
  # geom_text(size=2, aes(x = Characteristic, y = PercentCharPres, label = Characteristic, group = Data), position=position_dodge(0.9),vjust=0.5, hjust = -0.2) +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major.x = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey80"),
        #panel.grid.minor.x = element_line(size = 0.25, linetype = 'solid',
        #                                  colour = "grey"),
        legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(colour = "black", size = 11),
        axis.text.x = element_text(colour = "black"),
        plot.margin = unit(c(0,0,0.0,0.0), "cm")) + # defining margins of figure top, right, bottom, left
  scale_y_continuous(name = "Average number of total preference votes (z-score)",position = "left", 
                     limits = c(-0.5, 2.7), breaks=seq(-1,2,by = 0.5),
                     expand = c(0, 0)) + # removes lines between labels, e.g. gender2 and the bars,
  scale_x_discrete(labels = centeralignlabel(rev(c("Always-elected candidates", "Counterfactual non-electees", "Counterfactual electees","Never-elected candidates")))) +
  geom_hline(yintercept = 0, size = 0.8) + # Black line at the y = 0 line
  coord_flip() +
  ## For the significance tests (True electees vs Counterfactual non-electees)
  annotate("segment", x=3.05, xend=3.95,y=MEANSVOTES$mean[1]+0.2, yend=MEANSVOTES$mean[1]+0.2, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=3.05, xend=3.05,y=MEANSVOTES$mean[2]+0.1, yend=MEANSVOTES$mean[1]+0.2, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=3.95, xend=3.95,y=MEANSVOTES$mean[1]+0.1, yend=MEANSVOTES$mean[1]+0.2, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 3.5, y = MEANSVOTES$mean[1]+0.3, hjust = 0, label = paste0(STARS$stars[1]),
           colour = "black", parse = FALSE) + # hjust = 0 for left allignment of text
  ## For the significance tests (True electees vs Counterfactual electees)
  annotate("segment", x=2.05, xend=2.95,y=MEANSVOTES$mean[2]+0.2, yend=MEANSVOTES$mean[2]+0.2, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=2.05, xend=2.05,y=MEANSVOTES$mean[3]+0.1, yend=MEANSVOTES$mean[2]+0.2, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=2.95, xend=2.95,y=MEANSVOTES$mean[2]+0.1, yend=MEANSVOTES$mean[2]+0.2, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 2.5, y = MEANSVOTES$mean[2]+0.3, hjust = 0, label = paste0(STARS$stars[2]),
           colour = "black", parse = FALSE) + # hjust = 0 for left allignment of text
  ## For the significance tests (Counterfactual electees vs Hopeless candidates)
  annotate("segment", x=1.05, xend=1.95,y=MEANSVOTES$mean[3]+0.2, yend=MEANSVOTES$mean[3]+0.2, color = "black", size = 0.75, lineend = "round") + # vertical black line
  annotate("segment", x=1.05, xend=1.05,y=0 +0.1, yend=MEANSVOTES$mean[3]+0.2, color = "black", size = 0.75) + # lower black line
  annotate("segment", x=1.95, xend=1.95,y=MEANSVOTES$mean[3]+0.1, yend=MEANSVOTES$mean[3]+0.2, color = "black", size = 0.75) + # upper black line
  annotate(geom = "text", x = 1.5, y = MEANSVOTES$mean[3]+0.3, hjust = 0, label = paste0(STARS$stars[3]),
           colour = "black", parse = FALSE) + # hjust = 0 for left allignment of text
  annotate(geom = "text", x = 4, y = 2.3, hjust = 0, label = paste("N = ",nrow(GROUPDIFF[which(GROUPDIFF$cand_group == "true electee"),]), sep=""),
           colour = "black", parse = FALSE) +
  annotate(geom = "text", x = 3, y = 2.3, hjust = 0, label = paste("N = ",nrow(GROUPDIFF[which(GROUPDIFF$cand_group == "counterfactual non-electee"),]), sep=""),
           colour = "black", parse = FALSE) +
  annotate(geom = "text", x = 2, y = 2.3, hjust = 0, label = paste("N = ",nrow(GROUPDIFF[which(GROUPDIFF$cand_group == "counterfactual electee"),]), sep=""),
           colour = "black", parse = FALSE) +
  annotate(geom = "text", x = 1, y = 2.3, hjust = 0, label = paste("N = ",nrow(GROUPDIFF[which(GROUPDIFF$cand_group == "hopeless candidate"),]), sep=""),
           colour = "black", parse = FALSE)

figurecandgroups


# 10) Export figure
completefilename <- paste(getwd(),"/FigureA3_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".tif", sep = "")
tiff(completefilename,height = 6.25, width = 19, units = "cm", compression = "lzw", res = 1200)
figurecandgroups
dev.off() 


############
############
# Table A2 #
############
############


# 1) Export the test statistics underlying Figure A3
TESTVOTE$mean1 <- as.numeric(TESTVOTE$mean1)
TESTVOTE$mean2 <- as.numeric(TESTVOTE$mean2)
TESTVOTE$t <- as.numeric(TESTVOTE$t)
TESTVOTE$pvalue <- as.numeric(TESTVOTE$pvalue)

TESTVOTE[] <- lapply(TESTVOTE,formatC,digits=3,format="f")

htmlTable(TESTVOTE,
          file = paste(getwd(),"./TableA2_", as.character(format(Sys.time(), "%Y-%m-%d_%H%M")), ".html", sep = ""))










