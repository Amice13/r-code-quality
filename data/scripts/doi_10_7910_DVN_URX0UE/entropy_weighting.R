# This code generates the weights to have balanced key baseline covariates between follow-up sample and the initial sample
# The important package here is ebal (https://cran.r-project.org/web/packages/ebal/ebal.pdf)
# (Entropy reweighting to create balanced samples)

#Selection of the follow-up respondents


raw$follow = !is.na(raw$timing_knowledge_page_submit_followup)

# Creation of useful variables:

raw$strat1=as.numeric(raw$stratum == "old_f_white_nongop")
raw$strat2=as.numeric(raw$stratum == "old_f_white_gop")
raw$strat3=as.numeric(raw$stratum == "old_f_nonwhite")
raw$strat4=as.numeric(raw$stratum == "young_f_white_nongop")
raw$strat5=as.numeric(raw$stratum == "young_f_white_gop")
raw$strat6=as.numeric(raw$stratum == "young_f_nonwhite")
raw$strat7=as.numeric(raw$stratum == "old_m_white_nongop")
raw$strat8=as.numeric(raw$stratum == "old_m_white_gop")
raw$strat9=as.numeric(raw$stratum == "old_m_nonwhite")
raw$strat10=as.numeric(raw$stratum == "young_m_white_nongop")
raw$strat11=as.numeric(raw$stratum == "young_m_white_gop")
raw$strat12=as.numeric(raw$stratum == "young_m_nonwhite")

# Key baseline variables:
baseline_variables =  c("age", "party_dem", "party_rep",
                                     "hs_graduate", "hhi_above_60k",
                                     "sex_female","strat1",
                                     "strat2","strat3","strat4","strat5","strat7","strat8"
                                     ,"strat9","strat10","strat11","safety_mask_in_often",
                        "safety_mask_out_often",
                        "safety_hands_often",   
                        "safety_distance_often","black_more_likely_to_die")

# Data set without any missing key baseline variable
# Reason: the input matrix for ebal can not contain any NA
raw2=raw
raw2$complete_key_baseline = as.numeric(complete.cases(raw2[,baseline_variables]))

raw_without_missing = raw2[raw2$complete_key_baseline==1,]

# ebal input must be a matrix
covariates = as.matrix(raw_without_missing[,baseline_variables])

covariates_follow = as.matrix(raw_without_missing[raw_without_missing$follow==1,baseline_variables])



# We balance follow-up and initial groups with the following procedure:

# We weight follow-up respondents so that their baseline 
# variables are balanced with the ones of the starting population.
# in the input vector: 
#- 1 if target population (all population)
#- 0 if population we want to weight (follow population)

# Balance follow-up respondents:
bal_treated = ebalance(c(1-raw_without_missing$follow,rep(1,sum(raw_without_missing$follow==1))),rbind(covariates,covariates_follow), base.weight = NULL, norm.constant = NULL, coefs = NULL, max.iterations = 200,
                       constraint.tolerance = 0.01, print.level = 0)

weights_follow = bal_treated$w

#We normalize the weights (to have mean(weights)=1)
weights_follow = weights_follow *sum(raw_without_missing$follow)/(nrow(raw_without_missing))

# We put all the weights in a column
raw_without_missing$weights_follow=NA


i=1

for (k in 1:nrow(raw_without_missing)){
  
  if (raw_without_missing[k,"follow"]==1){
    raw_without_missing$weights_follow[k]=weights_follow[i]
    i=i+1
    
  }

}


# We add these weights to the data frame

raw = merge(raw,raw_without_missing[,c("response_id","weights_follow")],by="response_id",all=TRUE)
