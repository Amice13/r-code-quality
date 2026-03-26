### This file calculates the tactical voting incentives for all individuals
# It calculates the main tactical incentives, as well as those relying on 
# ideological closeness and subjective winning probabilities. 
## The code predominantly comes from Eggers and Vyvian, APSR 2020, with some minor modifications. 

###### WARNING:
# The code used to calculate tactical incentives using subjective probabilities of winning 
# requires a very significant amount of computing time (over 12 hours in a standard computing 
# running with parallel computing). For this reason, the Intermediate Data folder includes
# the two saved working environments from R with all those calculations: InferredVoteShares.RData 
# and TacticalIncentivesSubjective.RData. 
# The computationally intensively consuming code beings in line: 426. 

### Data Dependencies: 
#   Recoded_data_for_tau.dta
#   ElectionResultsAllYearsReshaped.dta
### Code Dependencies: 
#   utils_pivotality_functions.R   (From Eggers and Vyvian, APSR 2020)
### Outputs: 
#   TacticalIncentivesMain.dta  
#   TacticalIncentivesClose.dta  
#   TacticalIncentivesSubjective.dta  
#   InferredVoteShares.RData
#   TacticalIncentivesSubjective.RData
#   InferredVoteShares.csv

rm(list = ls())

# Required Packages: 
library(readstata13)
library(foreign)
library(dplyr)
library(gtools)
library(parallel)
require(logr)


# Set main Working Directory 
setwd("C:/Users/lnunez6/Dropbox/Documents/Loyalty and Defection/AJPS Replication Materials")

log03 <- file.path(getwd(),"Logs","03_TauCalculation_log.log")
log <- log_open(log03, logdir = FALSE)

# Import functions to calculate pivotality from source file: 
source("./Code/utils_pivotality_functions.R")

# Import data with BES recodes for tau calculations
D = read.dta13("./Intermediate Data/Recoded_data_for_tau.dta",convert.factors= F)
# Reorder dataset by year, constituency ID, and individual voter ID
D = D[order(D$year, D$refno, D$id), ]

# Create vector with party short names (note for 2019 UKIP is actually Brexit Party)
parties = c("Lab", "Con", "LD", "UKIP", "Grn", "SNP", "PC")

# Create utility matrix with the modified feeling thermometers. 
U = D[,paste0("likeM",parties)]
# Set to NA those feeling thermometers for SNP in Wales and PC in Scotland. 
U[D$country != 2, "likeMSNP"] <- NA
U[D$country != 3, "likeMPC"] <- NA
# Store modified utility matrix in new object. 
U_party_feelpre = U


# Create function to use partyidentification as a further tie-breaker for utilities
plurality_sincere_vote_matrix <- function(U, tie.breaker = NULL){
  # deliver a matrix of 1's and 0's
  if(!is.null(tie.breaker)){
    stopifnot(nrow(tie.breaker) == nrow(U) & ncol(tie.breaker) == ncol(U))
    U = U + tie.breaker
  }
  psvm = matrix(NA, nrow = nrow(U), ncol = ncol(U))
  max.U = apply(U, 1, max, na.rm = T)
  for(j in 1:ncol(psvm)){
    psvm[,j] = as.integer(U[,j] == max.U) # if more than one is the max, there this is a tie that cannot be resolved. 
  }
  psvm
}

# Import Actual Election Results for 2015, 2017, and 2019. 
all_results = read.dta13("./Intermediate Data/ElectionResultsAllYearsReshaped.dta",convert.factors= F)
all_results$type = "actual"


# Cycle through types of preference information, election information types, and precision levels. 
pref_types = c("party_feelpre") # Only one level: modified party thermometers
info_types = c("actual") # Only one level: actual election results. 
ss = c(85) # Only one level: the precision level preferred in Eggers and Vyvian, APSR 2020. 

# Vector with years
years = c(2015,2017,2019)

# Function to count the number of missing utilities
how.many.not.missing = function(vec){sum(!is.na(vec))}

# Empty list to store results. 
P_mat_list = list()

# Matrix with Party Identification dummies. 
party_id_mat = as.matrix(D[,paste0("pid",parties)])

cat("Filling in tau and best insincere vote by year and constituency for:")
for(pref_type in pref_types){
  cat("\n -- ", pref_type, sep = "")
  U = get(paste0("U_", pref_type))
  S = suppressWarnings(plurality_sincere_vote_matrix(U, party_id_mat*.01))
  colnames(S) = parties
  S[is.na(S)] = 0
  
  # rules for inclusion: 
  hmnm = apply(U, 1, how.many.not.missing) # at least 3 non-missing prefs
  hmfaves = apply(S, 1, sum, na.rm = T) # 1 favorite party
  
  # some indicators so we can say why tau is missing 
  D[[paste0("drop_bc_too_few_prefs_", pref_type)]] = hmnm < 3
  D[[paste0("drop_bc_pref_tie_that_cant_be_resolved_by_partyID_", pref_type)]] = hmfaves > 1
  D[[paste0("drop_bc_no_favorite_", pref_type)]] = hmfaves == 0
  D[[paste0("how_many_not_missing_", pref_type)]] = hmnm
  
  use = (hmfaves == 1 & hmnm >= 3) # below we set tau to NA for Rs who don't satisfy these criteria: their tau would not be a reliable indicator of tactical vote incentive
  prop.table(table(use)) # Proportion for whom incentives not calculated due to missing preferences 
  # define preferred party (called party1 for legacy reasons, then mpp )
  D[[paste0("party1_", pref_type)]] = D[[paste0("left_", pref_type)]] = NA
  for(j in 1:length(parties)){
    D[[paste0("mpp_", pref_type)]][S[,j] == 1] = parties[j]
  }
  
  
  # set missing utilities to 0 -- this is like setting na.rm to T in the matrix multiplication.
  U[is.na(U)] = 0
  
  for(info_type in info_types){
    cat("\n    -- ", info_type, sep = "")
    for(s in ss){
      cat("\n      -- s=", s, sep = "")
      suffix = paste0(pref_type, "_", info_type, "_", s)
      tau_name = paste0("tau_", suffix)
      boo_name = paste0("mpv_", suffix) # storing Best Alternative Party (mpv for legacy reasons)
      boo2_name = paste0("lpv_", suffix) # storing Worst Alternative Party (lpv for legacy reasons)
      
      vote_code_name = paste0("vote_code_", suffix)
      D[[tau_name]] = D[[boo_name]] = D[[vote_code_name]] = NA
      for(this_year in years){
        cat("\n        -- ", this_year, ": ", sep = "")
        all_refnos = sort(unique(D$refno[D$year == this_year]))
        counter = 0
        for(this_refno in all_refnos){
          if(this_year %in% c(2010, 2015) & this_refno == 108){next} # Buckingham -- Bercow seat. results data is nonsense. 
          
          counter = counter + 1
          if(counter%%100 == 0){cat(counter)}else if(counter%%10 == 0){cat(".")}
          these = !is.na(D$year) & !is.na(D$refno) & D$year == this_year & D$refno == this_refno
          this_U = as.matrix(U[these, ])
          this_S = S[these, ]
          # get the P matrix -- either from storage or by computing fresh.
          # across preference types it is the same P matrix, so this speeds up the computations across preference types by a lot.
          P_mat_key = paste0(this_year, "_", this_refno, "_", info_type, "_", s)
          
          this_v_vec = as.numeric(all_results[all_results$type == info_type & all_results$year == this_year & all_results$refno == this_refno, parties])
          # normalize the v_vec to sum to 1
          this_v_vec = this_v_vec/sum(this_v_vec, na.rm = T)
          this_alpha_vec = this_v_vec*s
          # as in original code, set missing alpha components to 0, effectively assuming party with no result did not run and is therefore hopeless.
          this_alpha_vec[is.na(this_alpha_vec)] = 0
          names(this_alpha_vec) = parties 
          # compute P matrix
          this_P = plurality.P.matrix.analytical(this_alpha_vec)
          P_mat_list[[P_mat_key]] = this_P
          
          UP = this_U%*%this_P ## UP: the n x K matrix ofU expected utilities
          other_UP = (1 - this_S)*UP
          # compute tau 
          this_tau = apply(other_UP, 1, max, na.rm = T) - apply(this_S*UP, 1, max, na.rm = T)
          D[[tau_name]][these] = this_tau
          ## identify boo party: best non-sincere vote. depends on tau.  
          max_other_UP = apply(other_UP, 1, max)
          
          this_boo = rep(NA, length(this_tau))
          for(j in 1:length(parties)){
            this_boo[other_UP[,j] == max_other_UP] = parties[j]
          }
          D[[boo_name]][these] = this_boo
          ## identify the worst non-sincere vote (the vote that would give less utility)
          UP2 <- UP * matrix(rep(ifelse(is.na(this_v_vec), NA,1),nrow(UP)),nrow = nrow(UP),byrow = TRUE)
          min_UP2 <- apply(UP2,1,min,na.rm = TRUE)
          this_boo2 = rep(NA, length(this_tau))
          for(j in 1:length(parties)){
            this_boo2[UP2[,j] == min_UP2] = parties[j]
          }
          D[[boo2_name]][these] = this_boo2
        }
        cat("done.")
      }
      # some computations we can do when we have filled out everyone 
      # set tau and boo to missing if we determined we should not be using it. 
      D[[tau_name]][!use] = NA
      D[[boo_name]][!use] = NA
      # make tau cats here too
      decile.cuts = quantile(D[[tau_name]], probs = seq(0, 1,.1), na.rm = T)
      decile.cuts[abs(decile.cuts) == min(abs(decile.cuts))] = 0
      D[[paste0(tau_name, "_cat")]] = NA
      decile.midpoints = c()
      for(i in 1:(length(decile.cuts) - 1)){
        D[[paste0(tau_name, "_cat")]][!is.na(D[[tau_name]]) & D[[tau_name]] >= decile.cuts[i] & D[[tau_name]] < decile.cuts[i+1]] = i
      }
      # indicate vote code, which depends on tau.
      # 1: voted for favorite
      # 2: vote for best of others (tactical vote) 
      # 3: vote for other
      #D[[vote_code_name]] = NA
      #valid.vote = !is.na(D$vote.post.r) & D$vote.post.r != "don't know"
      #pref.data.present = !is.na(D[[paste0("party1_", pref_type)]]) & !is.na(D[[boo_name]])
      #D[[vote_code_name]][valid.vote & pref.data.present] = 3
      #D[[vote_code_name]][valid.vote & pref.data.present & D$vote.post.r == D[[paste0("party1_", pref_type)]]] = 1
      #D[[vote_code_name]][valid.vote & pref.data.present & D$vote.post.r == D[[boo_name]]] = 2
    }
  }
}


write.dta(D,"./Intermediate Data/TacticalIncentivesMain.dta")

###########################################################################################
###########################################################################################

######### USING IDEOLOGICAL CLOSENESS ########

# Import data with BES recodes for tau calculations
D = read.dta13("./Intermediate Data/Recoded_data_for_tau.dta",convert.factors= F)
# Reorder dataset by year, constituency ID, and individual voter ID
D = D[order(D$year, D$refno, D$id), ]

# Create vector with party short names (note for 2019 UKIP is actually Brexit Party)
parties = c("Lab", "Con", "LD", "UKIP", "Grn", "SNP", "PC")

# Create utility matrix with the modified feeling thermometers. 
U = D[,paste0("close",parties)]
# Set to NA those feeling thermometers for SNP in Wales and PC in Scotland. 
U[D$country != 2, "closeSNP"] <- NA
U[D$country != 3, "closePC"] <- NA
# Store modified utility matrix in new object. 
U_party_feelpre = U


# Create function to use partyidentification as a further tie-breaker for utilities
plurality_sincere_vote_matrix <- function(U, tie.breaker = NULL){
  # deliver a matrix of 1's and 0's
  if(!is.null(tie.breaker)){
    stopifnot(nrow(tie.breaker) == nrow(U) & ncol(tie.breaker) == ncol(U))
    U = U + tie.breaker
  }
  psvm = matrix(NA, nrow = nrow(U), ncol = ncol(U))
  max.U = apply(U, 1, max, na.rm = T)
  for(j in 1:ncol(psvm)){
    psvm[,j] = as.integer(U[,j] == max.U) # if more than one is the max, there this is a tie that cannot be resolved. 
  }
  psvm
}

# Import Actual Election Results for 2015, 2017, and 2019. 
all_results = read.dta13("./Intermediate Data/ElectionResultsAllYearsReshaped.dta",convert.factors= F)
all_results$type = "actual"


# Cycle through types of preference information, election information types, and precision levels. 
pref_types = c("party_feelpre") # Only one level: modified party thermometers
info_types = c("actual") # Only one level: actual election results. 
ss = c(85) # Only one level: the precision level preferred in Eggers and Vyvian, APSR 2020. 

# Vector with years
years = c(2015,2017,2019)

# Function to count the number of missing utilities
how.many.not.missing = function(vec){sum(!is.na(vec))}

# Empty list to store results. 
P_mat_list = list()

# Matrix with Party Identification dummies. 
party_id_mat = as.matrix(D[,paste0("pid",parties)])


cat("Filling in tau and best insincere vote by year and constituency for:")
for(pref_type in pref_types){
  cat("\n -- ", pref_type, sep = "")
  U = get(paste0("U_", pref_type))
  S = suppressWarnings(plurality_sincere_vote_matrix(U, party_id_mat*.01))
  colnames(S) = parties
  S[is.na(S)] = 0
  
  # rules for inclusion: 
  hmnm = apply(U, 1, how.many.not.missing) # at least 3 non-missing prefs
  hmfaves = apply(S, 1, sum, na.rm = T) # 1 favorite party
  
  # some indicators so we can say why tau is missing 
  D[[paste0("drop_bc_too_few_prefs_", pref_type)]] = hmnm < 3
  D[[paste0("drop_bc_pref_tie_that_cant_be_resolved_by_partyID_", pref_type)]] = hmfaves > 1
  D[[paste0("drop_bc_no_favorite_", pref_type)]] = hmfaves == 0
  D[[paste0("how_many_not_missing_", pref_type)]] = hmnm
  
  use = (hmfaves == 1 & hmnm >= 3) # below we set tau to NA for Rs who don't satisfy these criteria: their tau would not be a reliable indicator of tactical vote incentive
  
  # define preferred party (called party1 for legacy reasons, then mpp )
  D[[paste0("party1_", pref_type)]] = D[[paste0("left_", pref_type)]] = NA
  for(j in 1:length(parties)){
    D[[paste0("mpp_", pref_type)]][S[,j] == 1] = parties[j]
  }
  
  
  # set missing utilities to 0 -- this is like setting na.rm to T in the matrix multiplication.
  U[is.na(U)] = 0
  
  for(info_type in info_types){
    cat("\n    -- ", info_type, sep = "")
    for(s in ss){
      cat("\n      -- s=", s, sep = "")
      suffix = paste0(pref_type, "_", info_type, "_", s)
      tau_name = paste0("tau_", suffix)
      boo_name = paste0("mpv_", suffix) # storing Best Alternative Party (mpv for legacy reasons)
      boo2_name = paste0("lpv_", suffix) # storing Worst Alternative Party (lpv for legacy reasons)
      
      vote_code_name = paste0("vote_code_", suffix)
      D[[tau_name]] = D[[boo_name]] = D[[vote_code_name]] = NA
      for(this_year in years){
        cat("\n        -- ", this_year, ": ", sep = "")
        all_refnos = sort(unique(D$refno[D$year == this_year]))
        counter = 0
        for(this_refno in all_refnos){
          if(this_year %in% c(2010, 2015) & this_refno == 108){next} # Buckingham -- Bercow seat. results data is nonsense. 
          
          counter = counter + 1
          if(counter%%100 == 0){cat(counter)}else if(counter%%10 == 0){cat(".")}
          these = !is.na(D$year) & !is.na(D$refno) & D$year == this_year & D$refno == this_refno
          this_U = as.matrix(U[these, ])
          this_S = S[these, ]
          # get the P matrix -- either from storage or by computing fresh.
          # across preference types it is the same P matrix, so this speeds up the computations across preference types by a lot.
          P_mat_key = paste0(this_year, "_", this_refno, "_", info_type, "_", s)
          
          this_v_vec = as.numeric(all_results[all_results$type == info_type & all_results$year == this_year & all_results$refno == this_refno, parties])
          # normalize the v_vec to sum to 1
          this_v_vec = this_v_vec/sum(this_v_vec, na.rm = T)
          this_alpha_vec = this_v_vec*s
          # as in original code, set missing alpha components to 0, effectively assuming party with no result did not run and is therefore hopeless.
          this_alpha_vec[is.na(this_alpha_vec)] = 0
          names(this_alpha_vec) = parties 
          # compute P matrix
          this_P = plurality.P.matrix.analytical(this_alpha_vec)
          P_mat_list[[P_mat_key]] = this_P
          
          UP = this_U%*%this_P ## UP: the n x K matrix ofU expected utilities
          other_UP = (1 - this_S)*UP
          # compute tau 
          this_tau = apply(other_UP, 1, max, na.rm = T) - apply(this_S*UP, 1, max, na.rm = T)
          D[[tau_name]][these] = this_tau
          ## identify boo party: best non-sincere vote. depends on tau.  
          max_other_UP = apply(other_UP, 1, max)
          
          this_boo = rep(NA, length(this_tau))
          for(j in 1:length(parties)){
            this_boo[other_UP[,j] == max_other_UP] = parties[j]
          }
          D[[boo_name]][these] = this_boo
          ## identify the worst non-sincere vote (the vote that would give less utility)
          UP2 <- UP * matrix(rep(ifelse(is.na(this_v_vec), NA,1),nrow(UP)),nrow = nrow(UP),byrow = TRUE)
          min_UP2 <- apply(UP2,1,min,na.rm = TRUE)
          this_boo2 = rep(NA, length(this_tau))
          for(j in 1:length(parties)){
            this_boo2[UP2[,j] == min_UP2] = parties[j]
          }
          D[[boo2_name]][these] = this_boo2
        }
        cat("done.")
      }
      # some computations we can do when we have filled out everyone 
      # set tau and boo to missing if we determined we should not be using it. 
      D[[tau_name]][!use] = NA
      D[[boo_name]][!use] = NA
      # make tau cats here too
      decile.cuts = quantile(D[[tau_name]], probs = seq(0, 1,.1), na.rm = T)
      decile.cuts[abs(decile.cuts) == min(abs(decile.cuts))] = 0
      D[[paste0(tau_name, "_cat")]] = NA
      decile.midpoints = c()
      for(i in 1:(length(decile.cuts) - 1)){
        D[[paste0(tau_name, "_cat")]][!is.na(D[[tau_name]]) & D[[tau_name]] >= decile.cuts[i] & D[[tau_name]] < decile.cuts[i+1]] = i
      }
      # indicate vote code, which depends on tau.
      # 1: voted for favorite
      # 2: vote for best of others (tactical vote) 
      # 3: vote for other
      #D[[vote_code_name]] = NA
      #valid.vote = !is.na(D$vote.post.r) & D$vote.post.r != "don't know"
      #pref.data.present = !is.na(D[[paste0("party1_", pref_type)]]) & !is.na(D[[boo_name]])
      #D[[vote_code_name]][valid.vote & pref.data.present] = 3
      #D[[vote_code_name]][valid.vote & pref.data.present & D$vote.post.r == D[[paste0("party1_", pref_type)]]] = 1
      #D[[vote_code_name]][valid.vote & pref.data.present & D$vote.post.r == D[[boo_name]]] = 2
    }
  }
}
# Export the data for Tactical Incentives Using Ideological Closeness
write.dta(D,"./Intermediate Data/TacticalIncentivesClose.dta")

###########################################################################################
###########################################################################################

######### USING SUBJECTIVE WINNING PROBABILITIES ########

# The code to obtain the inferred vote shares from subjective winning probabilities
# is extremely computationally demanding and takes several hours to run (about  12 hours) 
# in a reasonable Windows desktop computer. 
# For this reason, the Intermediate Data folder contains the workspace with all content
# and results from this the estimation that follows below 
# included in lines 426 to 589 (InferredVoteShares.RData)

# The code to obtain the tactical incentives measure using the inferred vote shares
# calculated above is also computationally intensive, as it must calculate pivotality
# for every single respondent in each election year. It also takes a significant amount
# of time to run (about 3 hours). 
# For this reason, the Intermediate Data folder contains the workspace with all the 
# content and results from this estimation, which is saved in line 589 (TacticalIncentivesSubjective.RData) 
# 


#############################################################################
##### OBTAIN INFERRED VOTE SHARES FROM SUBJECTIVE WINNING PROBABILITIES #####
#############################################################################
commentout <- function(){ # Commented out until line 591
rm(list = ls())


# Import data with BES recodes for tau calculations
D = read.dta13("./Intermediate Data/Recoded_data_for_tau.dta",convert.factors= F)

# Reorder dataset by year, constituency ID, and individual voter ID
D = D[order(D$year, D$refno, D$id), ]

# Create vector with party short names (note for 2019 UKIP is actually Brexit Party)
parties = c("Lab", "Con", "LD", "UKIP", "Grn", "SNP", "PC")

# Create utility matrix with the modified feeling thermometers. 
U = D[,paste0("likeM",parties)]
# Set to NA those feeling thermometers for SNP in Wales and PC in Scotland. 
U[D$country != 2, "likeMSNP"] <- NA
U[D$country != 3, "likeMPC"] <- NA
# Store modified utility matrix in new object. 
U_party_feelpre = U


# winning probability variables from BES data (recoded for)
pvars <- c("winprCon","winprLab","winprLD","winprSNP","winprPC","winprUKIP","winprGrn")
pvars18 <- c("winprCon","winprLab","winprLD","winprSNP","winprPC","winprGrn","winprBxt")

# Keep relevant variables (wave 5, 2015, wave 12, 2017, wave 18, 2019)
d5 <- D[D$year == 2015, ]
pr5 <- d5[,c("id",pvars) ]
d12 <- D[D$year == 2017, ]
pr12 <- d12[,c("id",pvars) ]
d18 <- D[D$year == 2019, ]
pr18 <- d18[,c("id",pvars18)]

# Identify observations that have info on probabilities for at least 3 parties. 
keep <- which(rowSums(!is.na(pr5[,pvars]))>=3)
pr5 <- pr5[keep, ]
keep12 <- which(rowSums(!is.na(pr12[,pvars]))>=3)
pr12 <- pr12[keep12, ]
keep18 <- which(rowSums(!is.na(pr18[,pvars18]))>=3)
pr18 <- pr18[keep18, ]

# Replace all NAs with zero (zero probability of winning) and re-scale so they add up to 1
pr5[is.na(pr5)] <- 0
pr5[,pvars] <- pr5[,pvars]/rowSums(pr5[,pvars])
pr12[is.na(pr12)] <- 0
pr12[,pvars] <- pr12[,pvars]/rowSums(pr12[,pvars]) 
pr18[is.na(pr18)] <- 0
pr18[,pvars18] <- pr18[,pvars18]/rowSums(pr18[,pvars18])

# Function infers vote shares from winning probabilities. Uses simulation from betas (close to dirichlet)
# And then calculates probabilities of winning and minimizes sum of square root of distances between 
# inferred winning probabilities and reported probabilities. 
get.votes <- function(par,target){
  mult <- 10
  par <- par*mult
  n <- 1200
  set.seed(123); x1 <- rbeta(n,par[1],mult-par[1])
  set.seed(456); x2 <- rbeta(n,par[2],mult-par[2])
  set.seed(789); x3 <- rbeta(n,par[3],mult-par[3])
  set.seed(234); x4 <- rbeta(n,par[4],mult-par[4])
  set.seed(567); x5 <- rbeta(n,par[5],mult-par[5])
  set.seed(890); x6 <- rbeta(n,par[6],mult-par[6])
  set.seed(345); x7 <- rbeta(n,par[7],mult-par[7])
  
  x <- cbind(x1,x2,x3,x4,x5,x6,x7)
  
  w1 <- ifelse(x[,1] == apply(x,1,max),1,0)
  w2 <- ifelse(x[,2] == apply(x,1,max),1,0)
  w3 <- ifelse(x[,3] == apply(x,1,max),1,0)
  w4 <- ifelse(x[,4] == apply(x,1,max),1,0)
  w5 <- ifelse(x[,5] == apply(x,1,max),1,0)
  w6 <- ifelse(x[,6] == apply(x,1,max),1,0)
  w7 <- 1-w1-w2-w3-w4-w5-w6
  
  
  d1 <- 100*sqrt(abs(mean(w1)-target[1]))
  d2 <- 100*sqrt(abs(mean(w2)-target[2]))
  d3 <- 100*sqrt(abs(mean(w3)-target[3]))
  d4 <- 100*sqrt(abs(mean(w4)-target[4]))
  d5 <- 100*sqrt(abs(mean(w5)-target[5]))
  d6 <- 100*sqrt(abs(mean(w6)-target[6]))
  d7 <- 100*sqrt(abs(mean(w7)-target[7]))
  
  ds <- d1+d2+d3+d4+d5+d6+d7  
  return(ds)
}


# Mini functions to cycle through the respondents
cycleid <- function(id){ #remove together with line 591 to run this chunk of code (very time consuming)
  res <- optim(par = as.numeric(pr5[pr5$id == id,pvars]), fn = get.votes, target = pr5[pr5$id == id,pvars])
  return(c(res$par,id))
}
cycleid12 <- function(id){
  res <- optim(par = as.numeric(pr12[pr12$id == id,pvars]), fn = get.votes, target = pr12[pr12$id == id,pvars])
  return(c(res$par,id))
}
cycleid18 <- function(id){
  res <- optim(par = as.numeric(pr18[pr18$id == id,pvars18]), fn = get.votes, target = pr18[pr18$id == id,pvars18])
  return(c(res$par,id))
}


# List with IDs to cycle through
run5 <- as.list(pr5$id)
run12 <- as.list(pr12$id)
run18 <- as.list(pr18$id)

## Calculate vectors of inferred vote shares for each election, parallel computing
# 2015
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(gtools)))
clusterExport(cl,c("cycleid","get.votes","pr5","pvars"))
start_time <- Sys.time()
estimated.shares5 = parLapply(cl,run5,function(i) tryCatch(cycleid(i), error = function(e) NULL))
Sys.time() - start_time
stopCluster(cl)
# 2017
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(gtools)))
clusterExport(cl,c("cycleid12","get.votes","pr12","pvars"))
start_time <- Sys.time()
estimated.shares12 = parLapply(cl,run12,function(i) tryCatch(cycleid12(i), error = function(e) NULL))
Sys.time() - start_time
stopCluster(cl)
# 2019
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl,c(library(gtools)))
clusterExport(cl,c("cycleid18","get.votes","pr18","pvars18"))
start_time <- Sys.time()
estimated.shares18 = parLapply(cl,run18,function(i) tryCatch(cycleid18(i), error = function(e) NULL))
Sys.time() - start_time
stopCluster(cl)

# Unlist results and name columns
sshare5 <- data.frame(matrix(unlist(estimated.shares5), ncol = 8, byrow = TRUE))
colnames(sshare5) <- c("Con", "Lab", "LD", "SNP", "PC","Grn","UKIP","id")
sshare5$year <- 2015
sshare12 <- data.frame(matrix(unlist(estimated.shares12), ncol = 8, byrow = TRUE))
colnames(sshare12) <- c("Con", "Lab", "LD", "SNP", "PC","Grn","UKIP","id")
sshare12$year <- 2017
sshare18 <- data.frame(matrix(unlist(estimated.shares18), ncol = 8, byrow = TRUE))
colnames(sshare18) <- c("Con", "Lab", "LD", "SNP", "PC","Grn","UKIP","id")
sshare18$year <- 2019

# Put elections together, add id, and match with refno information from main data
sshares <- rbind(sshare5,sshare12,sshare18)
sshares$id <- as.integer(sshares$id)
# adding refno
dref <- D[, c("id","refno","year")]
sshares <- merge(sshares, dref, by = c("year","id"), sort = FALSE, all.x = TRUE, all.y = FALSE)
sshares <- unique(sshares)
# Reshape results 
x <-sshares%>%select(-c(refno))
dups <- x[! (x%>%duplicated()),]
dups <-  add_rownames(dups, var = "rownames")
sshares <- add_rownames(sshares, var = "rownames")
sshares <-  left_join(dups,sshares )
sshares <- sshares[, !(names(sshares) %in% c("rownames"))]
# Save Workspace
#save.image("./Intermediate Data/InferredVoteShares.RData")
# Save inferred voteshares to CSV File 
write.csv(sshares,"./Intermediate Data/InferredVoteShares.csv")

} # remove together with line 426 to run this chunk of code (very time consuming)

#########################################################################
##### OBTAIN TACTICAL INCENTIVES MEASURE USING INFERRED VOTE SHARES #####
#########################################################################
rm(list = ls())

source("./Code/utils_pivotality_functions.R")

D = read.dta13("./Intermediate Data/Recoded_data_for_tau.dta",convert.factors= F)
D = D[order(D$year, D$refno, D$id), ]

# Create vector with party short names (note for 2019 UKIP is actually Brexit Party)
parties = c("Lab", "Con", "LD", "UKIP", "Grn", "SNP", "PC")


# Create utility matrix with the modified feeling thermometers. 
U = D[,paste0("likeM",parties)]
# Set to NA those feeling thermometers for SNP in Wales and PC in Scotland. 
U[D$country != 2, "likeMSNP"] <- NA
U[D$country != 3, "likeMPC"] <- NA
U_party_feelpre = U

# Create function to use party identification as a further tie-breaker for utilities
plurality_sincere_vote_matrix <- function(U, tie.breaker = NULL){
  # deliver a matrix of 1's and 0's
  if(!is.null(tie.breaker)){
    stopifnot(nrow(tie.breaker) == nrow(U) & ncol(tie.breaker) == ncol(U))
    U = U + tie.breaker
  }
  psvm = matrix(NA, nrow = nrow(U), ncol = ncol(U))
  max.U = apply(U, 1, max, na.rm = T)
  for(j in 1:ncol(psvm)){
    psvm[,j] = as.integer(U[,j] == max.U) # if more than one is the max, there this is a tie that cannot be resolved. 
  }
  psvm
}


# Import Inferred Vote Shares (Calculated above)
all_results <- read.csv("./Intermediate Data/InferredVoteShares.csv")
all_results$type = "subjective"
# Import Actual Election Results
realres <- read.dta13("./Intermediate Data/ElectionResultsAllYearsReshaped.dta",convert.factors= F)
colnames(realres) <- c("pano","ONSConstID","ConstituencyName","Country","Region","year","rCon", "rLab", "rLD", "rSNP", "rPC","rGrn","rUKIP","refno")

# Merge inferred vote shares with actual vote shares
test <- merge(all_results, realres, by = c("year","refno"), all.x = TRUE, all.y = FALSE)
# Replace inferred vote shares with NA if party not running in particular constituency. 
test$Con <- ifelse(is.na(test$rCon) == TRUE, NA, test$Con)
test$Lab <- ifelse(is.na(test$rLab) == TRUE, NA, test$Lab)
test$LD <- ifelse(is.na(test$rLD) == TRUE, NA, test$LD)
test$SNP <- ifelse(is.na(test$rSNP) == TRUE, NA, test$SNP)
test$PC <- ifelse(is.na(test$rPC) == TRUE, NA, test$PC)
test$UKIP <- ifelse(is.na(test$rUKIP) == TRUE, NA, test$UKIP)
test$Grn <- ifelse(is.na(test$rGrn) == TRUE, NA, test$Grn)
# Keep relevant variables and observations
all_results <- test[, c("Con", "Lab", "LD", "SNP", "PC","Grn","UKIP","id","year","type","refno")]
all_results$refno <- all_results$id
all_results <- all_results[, c("Con", "Lab", "LD", "SNP", "PC","Grn","UKIP","year","type","refno")]
all_results <- unique(all_results)

# Cycle through types of preference information, election information types, and precision levels. 
pref_types = c("party_feelpre") # Only one level: modified party thermometers
info_types = c("subjective") # Only one level: inferred vote shares
ss = c(85) # Only one level: the precision level preferred in Eggers and Vyvian, APSR 2020. 

# Vector with years
years = c(2015,2017,2019)

# Function to count the number of missing utilities
how.many.not.missing = function(vec){sum(!is.na(vec))}

# Empty list to store results. 
P_mat_list = list()

# Matrix with Party Identification dummies. 
party_id_mat = as.matrix(D[,paste0("pid",parties)])

StarTime <- Sys.time()
cat("Filling in tau and best insincere vote by year and constituency for:")
for(pref_type in pref_types){
  cat("\n -- ", pref_type, sep = "")
  U = get(paste0("U_", pref_type))
  S = suppressWarnings(plurality_sincere_vote_matrix(U, party_id_mat*.01))
  colnames(S) = parties
  S[is.na(S)] = 0
  
  # rules for inclusion: 
  hmnm = apply(U, 1, how.many.not.missing) # at least 3 non-missing prefs
  hmfaves = apply(S, 1, sum, na.rm = T) # 1 favorite party
  
  # some indicators so we can say why tau is missing 
  D[[paste0("drop_bc_too_few_prefs_", pref_type)]] = hmnm < 3
  D[[paste0("drop_bc_pref_tie_that_cant_be_resolved_by_partyID_", pref_type)]] = hmfaves > 1
  D[[paste0("drop_bc_no_favorite_", pref_type)]] = hmfaves == 0
  D[[paste0("how_many_not_missing_", pref_type)]] = hmnm
  
  use = (hmfaves == 1 & hmnm >= 3) # below we set tau to NA for Rs who don't satisfy these criteria: their tau would not be a reliable indicator of tactical vote incentive
  
  # define preferred party (called party1 for legacy reasons)
  D[[paste0("party1_", pref_type)]] = D[[paste0("left_", pref_type)]] = NA
  for(j in 1:length(parties)){
    D[[paste0("mpp_", pref_type)]][S[,j] == 1] = parties[j]
  }
  
  # set missing utilities to 0 -- this is like setting na.rm to T in the matrix multiplication.
  U[is.na(U)] = 0
  
  for(info_type in info_types){
    cat("\n    -- ", info_type, sep = "")
    for(s in ss){
      cat("\n      -- s=", s, sep = "")
      suffix = paste0(pref_type, "_", info_type, "_", s)
      tau_name = paste0("tau_", suffix)
      boo_name = paste0("mpv_", suffix)
      boo2_name = paste0("lpv_", suffix)
      
      vote_code_name = paste0("vote_code_", suffix)
      D[[tau_name]] = D[[boo_name]] = D[[vote_code_name]] = NA
      for(this_year in years){
        cat("\n        -- ", this_year, ": ", sep = "")
        all_refnos = sort(unique(D$id[D$year == this_year])) 
        counter = 0
        for(this_refno in all_refnos){
          #if(this_year %in% c(2010, 2015) & this_refno == 108){next} # Buckingham -- Bercow seat. results data is nonsense. 
          cat(this_refno, ".")
          counter = counter + 1
          if(counter%%100 == 0){cat(counter)}else if(counter%%10 == 0){cat(".")}
          these = !is.na(D$year) & !is.na(D$id) & D$year == this_year & D$id == this_refno
          this_U = as.matrix(U[these, ])
          this_S = S[these, ]
          # get the P matrix -- either from storage or by computing fresh.
          # across preference types it is the same P matrix, so this speeds up the computations across preference types by a lot.
          P_mat_key = paste0(this_year, "_", this_refno, "_", info_type, "_", s)
          
          this_v_vec = as.numeric(all_results[all_results$type == info_type & all_results$year == this_year & all_results$refno == this_refno, parties])
          # normalize the v_vec to sum to 1
          this_v_vec = this_v_vec/sum(this_v_vec, na.rm = T)
          this_alpha_vec = this_v_vec*s
          # as in original code, set missing alpha components to 0, effectively assuming party with no result did not run and is therefore hopeless.
          this_alpha_vec[is.na(this_alpha_vec)] = 0
          names(this_alpha_vec) = parties 
          # compute P matrix
          this_P = plurality.P.matrix.analytical(this_alpha_vec)
          this_P[,is.na(this_v_vec)] <- 0
          P_mat_list[[P_mat_key]] = this_P
          
          UP = this_U%*%this_P ## UP: the n x K matrix ofU expected utilities
          other_UP = (1 - this_S)*UP
          # compute tau 
          this_tau = apply(other_UP, 1, max, na.rm = T) - apply(this_S*UP, 1, max, na.rm = T)
          D[[tau_name]][these] = this_tau
          ## identify boo party: best non-sincere vote. depends on tau.  
          max_other_UP = apply(other_UP, 1, max)
          
          this_boo = rep(NA, length(this_tau))
          for(j in 1:length(parties)){
            this_boo[other_UP[,j] == max_other_UP] = parties[j]
          }
          this_boo <- ifelse(sum(UP) == 0, NA,this_boo)
          D[[boo_name]][these] = this_boo
          ## identify the worst non-sincere vote (the vote that would give less utility)
          UP2 <- UP * matrix(rep(ifelse(is.na(this_v_vec), NA,1),nrow(UP)),nrow = nrow(UP),byrow = TRUE)
          min_UP2 <- suppressWarnings(apply(UP2,1,min,na.rm = TRUE))
          this_boo2 = rep(NA, length(this_tau))
          for(j in 1:length(parties)){
            this_boo2[UP2[,j] == min_UP2] = parties[j]
          }
          D[[boo2_name]][these] = this_boo2
        }
        cat("done.")
      }
      # some computations we can do when we have filled out everyone 
      # set tau and boo to missing if we determined we should not be using it. 
      D[[tau_name]][!use] = NA
      D[[boo_name]][!use] = NA
      # make tau cats here too
      decile.cuts = quantile(D[[tau_name]], probs = seq(0, 1,.1), na.rm = T)
      decile.cuts[abs(decile.cuts) == min(abs(decile.cuts))] = 0
      D[[paste0(tau_name, "_cat")]] = NA
      decile.midpoints = c()
      for(i in 1:(length(decile.cuts) - 1)){
        D[[paste0(tau_name, "_cat")]][!is.na(D[[tau_name]]) & D[[tau_name]] >= decile.cuts[i] & D[[tau_name]] < decile.cuts[i+1]] = i
      }
      # indicate vote code, which depends on tau.
      # 1: voted for favorite
      # 2: vote for best of others (tactical vote) 
      # 3: vote for other
      #D[[vote_code_name]] = NA
      #valid.vote = !is.na(D$vote.post.r) & D$vote.post.r != "don't know"
      #pref.data.present = !is.na(D[[paste0("party1_", pref_type)]]) & !is.na(D[[boo_name]])
      #D[[vote_code_name]][valid.vote & pref.data.present] = 3
      #D[[vote_code_name]][valid.vote & pref.data.present & D$vote.post.r == D[[paste0("party1_", pref_type)]]] = 1
      #D[[vote_code_name]][valid.vote & pref.data.present & D$vote.post.r == D[[boo_name]]] = 2
    }
  }
}

Sys.time()-StarTime

# Keep necessary variables
keepvars <- c("year","id","pano","mpp_party_feelpre",
              "lpv_party_feelpre_subjective_85",
              "mpv_party_feelpre_subjective_85",
              "tau_party_feelpre_subjective_85")
write.dta(D[,keepvars],"./Intermediate Data/TacticalIncentivesSubjective.dta")

#save.image("./Intermediate Data/TacticalIncentivesSubjective.RData")
log_code()
log_close(footer = TRUE)