# This code generates the weights to have balanced key baseline covariates between treatment and control groups
# The important package here is ebal (https://cran.r-project.org/web/packages/ebal/ebal.pdf)
# (Entropy reweighting to create balanced samples)

#Selection of the respondents who finished the knowledge questions 

raw$finished_knowledge = as.numeric(!is.na((raw$Q47_Page.Submit)))
raw$finished_links = as.numeric(!is.na((raw$Q94_Page.Submit)))
raw$finished_videos = as.numeric(!is.na(raw$Q35_Page.Submit))

raw$treat_finished_knowledge = raw$finished_knowledge * raw$anytreat
raw$control_finished_knowledge = raw$finished_knowledge * (1-raw$anytreat)

raw$treat_finished_links = raw$finished_links * raw$anytreat
raw$control_finished_links = raw$finished_links * (1-raw$anytreat)


# Creation of useful variables:

raw$age_strat = as.numeric(raw$age_strat )

raw$strat_1 = as.numeric((raw$stratum=="1"))
raw$strat_2 = as.numeric((raw$stratum=="2"))
raw$strat_3 = as.numeric((raw$stratum=="3"))
raw$strat_4 = as.numeric((raw$stratum=="4"))
raw$strat_5 = as.numeric((raw$stratum=="5"))
raw$strat_6 = as.numeric((raw$stratum=="6"))
raw$strat_7 = as.numeric((raw$stratum=="7"))
raw$strat_8 = as.numeric((raw$stratum=="8"))

raw$essential_worker[raw$eligible_Q10 != 1] = 0

# Key baseline variables:

baseline_variables  = c("money_covid","education_numeric","hh_size","age","food","female","resp_latino","strat_1","strat_2","strat_3","strat_5","strat_6","strat_7")


# Data set without any missing key baseline variable
# Reason: the input matrix for ebal can not contain any NA
raw_without_missing = na.omit(raw[,c("ResponseId","rid","finished_videos","treat_finished_links","control_finished_links","finished_knowledge","finished_links","treat_finished_knowledge","control_finished_knowledge",baseline_variables)])


# ebal input must be a matrix
covariates = as.matrix(raw_without_missing[,baseline_variables])

covariates_treated_knowledge = as.matrix(raw_without_missing[raw_without_missing$treat_finished_knowledge==1,baseline_variables])
covariates_control_knowledge = as.matrix(raw_without_missing[raw_without_missing$control_finished_knowledge==1,baseline_variables])
covariates_treated_links = as.matrix(raw_without_missing[raw_without_missing$treat_finished_links==1,baseline_variables])
covariates_control_links = as.matrix(raw_without_missing[raw_without_missing$control_finished_links==1,baseline_variables])

covariates_videos = as.matrix(raw_without_missing[raw_without_missing$finished_videos==1,baseline_variables])


# We balance treatment and control groups with the following procedure:

# We weight treated respondents (respectively control respondents) so that their baseline 
# variables are balanced with the ones of the whole population.
# Therefore each respondent (either control or treated) receives a weight if no key baseline variable is missing.

# We follow this procedure to compute 3 different weights:
# - one for those who finished knowledge questions (weights_knowledge)
# - one for those who finished links questions (weights_links)
# - one for those who finished videos 1,2 and 3 (weights_videos)

# Balance treated respondents:
bal_treated = ebalance(c(1-raw_without_missing$treat_finished_knowledge,rep(1,sum(raw_without_missing$treat_finished_knowledge==1))),rbind(covariates,covariates_treated_knowledge), base.weight = NULL, norm.constant = NULL, coefs = NULL, max.iterations = 200,
         constraint.tolerance = 0.01, print.level = 0)

weights_treated_knowledge = bal_treated$w

#We normalize the weights (to have mean(weights)=1)
weights_treated_knowledge  = weights_treated_knowledge *sum(raw_without_missing$treat_finished_knowledge)/(nrow(raw_without_missing))

bal_treated = ebalance(c(1-raw_without_missing$treat_finished_links,rep(1,sum(raw_without_missing$treat_finished_links==1))),rbind(covariates,covariates_treated_links), base.weight = NULL, norm.constant = NULL, coefs = NULL, max.iterations = 200,
                       constraint.tolerance = 0.01, print.level = 0)

weights_treated_links = bal_treated$w
#We normalize the weights (to have mean(weights)=1)
weights_treated_links  = weights_treated_links *sum(raw_without_missing$treat_finished_links)/(nrow(raw_without_missing))


# Balance control respondents:
bal_control = ebalance(c(1-raw_without_missing$control_finished_knowledge,rep(1,sum(raw_without_missing$control_finished_knowledge==1))),rbind(covariates,covariates_control_knowledge), base.weight = NULL, norm.constant = NULL, coefs = NULL, max.iterations = 200,
               constraint.tolerance = 0.01, print.level = 0)

weights_control_knowledge = bal_control$w
#We normalize the weights (to have mean(weights)=1)
weights_control_knowledge = weights_control_knowledge*sum(raw_without_missing$control_finished_knowledge)/(nrow(raw_without_missing))


bal_control = ebalance(c(1-raw_without_missing$control_finished_links,rep(1,sum(raw_without_missing$control_finished_links==1))),rbind(covariates,covariates_control_links), base.weight = NULL, norm.constant = NULL, coefs = NULL, max.iterations = 200,
                       constraint.tolerance = 0.01, print.level = 0)

weights_control_links = bal_control$w
#We normalize the weights (to have mean(weights)=1)
weights_control_links = weights_control_links*sum(raw_without_missing$control_finished_links)/(nrow(raw_without_missing))


# for videos:

# We do not have any pure control for this questions. Therefore, we only weight the treatment group.

bal = ebalance(c(1-raw_without_missing$finished_videos,rep(1,sum(raw_without_missing$finished_videos==1))),rbind(covariates,covariates_videos), base.weight = NULL, norm.constant = NULL, coefs = NULL, max.iterations = 200,
                       constraint.tolerance = 0.01, print.level = 0)

weights_videos= bal$w
#We normalize the weights (to have mean(weights)=1)
weights_videos = weights_videos*sum(raw_without_missing$finished_videos)/(nrow(raw_without_missing))


# We put all the weights in a column
raw_without_missing$weights_knowledge=1
raw_without_missing$weights_links=1
raw_without_missing$weights_videos=1

i=1
j=1
for (k in 1:nrow(raw_without_missing)){
  
  if (raw_without_missing[k,"treat_finished_knowledge"]==1){
    raw_without_missing$weights_knowledge[k]=weights_treated_knowledge[i]
    i=i+1
    
  }
  if (raw_without_missing[k,"control_finished_knowledge"]==1){
    raw_without_missing$weights_knowledge[k]=weights_control_knowledge[j]
    j=j+1
    
  }
}

i=1
j=1
for (k in 1:nrow(raw_without_missing)){
  if (raw_without_missing[k,"treat_finished_links"]==1){
    raw_without_missing$weights_links[k]=weights_treated_links[i]
    i=i+1
    
  }
  if (raw_without_missing[k,"control_finished_links"]==1){
    raw_without_missing$weights_links[k]=weights_control_links[j]
    j=j+1
    
  }
}
i=1
j=1
for (k in 1:nrow(raw_without_missing)){
  
  if (raw_without_missing[k,"finished_videos"]==1){
    raw_without_missing$weights_videos[k]=weights_videos[i]
    i=i+1
    
  }

}

# We add these weights to the data frame
raw = merge(raw,raw_without_missing[,c("ResponseId","rid","weights_knowledge","weights_links","weights_videos")],by="ResponseId",all=TRUE)
  
#just counting:

raw_finished_knowledge = raw %>% filter(finished_knowledge == 1)
raw_finished_links = raw %>% filter(finished_links == 1)

knowledge_complete_baseline = complete.cases(raw_finished_knowledge[,baseline_variables])
links_complete_baseline = complete.cases(raw_finished_links[,baseline_variables])

nomit_knowledge = nrow(raw_finished_knowledge) - sum(complete.cases(raw_finished_knowledge[,baseline_variables]))
nomit_links = nrow(raw_finished_links) - sum(complete.cases(raw_finished_links[,baseline_variables]))


