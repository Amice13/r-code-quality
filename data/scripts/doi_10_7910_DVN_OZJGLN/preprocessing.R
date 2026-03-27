# the following file uses data.csv to produce the new variables based on previous ones
# that are used throughout the analyses in the main paper as well as in the SI
# note that the variables based on allocationwhy have been coded manually
# note further that the 11 respondents whose responses to the party identification question 
# (2-point and 6-point) did not aligned 
# have been dropped from the data file, and are not included here

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(dplyr) # data manipulation
library(survey) # creating survey weights
library(tableone) # producing stratified tables
library(plyr) # more data wrangling
library(haven) # for the generic factorizing function
library(questionr) # weighted calculations
library(anesrake) # calculating survey weights
library(ggplot2) # base plotting
library(reshape) # casting/melting tables
library(mltools) # for one-hot encoding

data = read.csv('data.csv', stringsAsFactors = F)

# number of words written in the open-ended question why they sent the amount
data$n_words = sapply(strsplit(data$allocationwhy, ' '), length)

# number of justifications per person
justifications = c('ideology', 'myideo', 'extreme', 'fair', 'selfish',
                   'reciprocity', 'merit', 'ethics', 'nomatter', 'dontknowb',
                   'dishonesty', 'honesty', 'hardship', 'selfpres')
data$n_justifications = rowSums(data[,justifications])

# attention_check: did they remember correctly the partner's affiliation
data[which(data$party2alter==data$memory1), 'attention_check'] = 1
data[which(data$party2alter!=data$memory1), 'attention_check'] = 0

# identifying people who didn't take up the treatment, by loose definition, 
# i.e. memory2 and party2B should be the same on 2-point 
memory2_binary = rep('Republican', dim(data)[1])
memory2_binary[which(grepl('Democrat', data$memory2))] = 'Democrat'
memory2_binary[which(grepl('ND_undisclosed', data$memory2))] = 'ND_undisclosed'

party2alter_binary = rep('Republican', dim(data)[1])
party2alter_binary[which(grepl('Democrat', data$party2alter))] = 'Democrat'
party2alter_binary[which(grepl('ND_undisclosed', data$party2alter))] = 'ND_undisclosed'

data['memory2_binary'] = memory2_binary
data['party2alter_binary'] = party2alter_binary

loose_treatment_takeup = rep(0, dim(data)[1])
loose_treatment_takeup[which(data$memory2_binary==data$party2alter_binary | data$party2alter=='ND_undisclosed')] = 1
data['loose_treatment_takeup'] = loose_treatment_takeup

# fair contribution is defined as sending at least $1
data$fairb = ifelse(data$sending>=1,1,0)

# signaled_partisanship
partisan = rep('non-copartisan', dim(data)[1])
partisan[which(data$party1 == data$party2alter_binary)] = 'copartisan'
partisan[which(data$party2alter=='ND_undisclosed')] = 'ND_undisclosed'
data['signaled_partisanship'] = partisan

# believed_partisanship
beliefs = rep('non-copartisan', dim(data)[1])
beliefs[which(data$memory2_binary==data$party1)] = 'copartisan'
data['believed_partisanship'] = beliefs

# belief type is a variable that contains information about the signal and belief
# it takes 6 values based on the signal (copartisan, non-copartisan, non-disclosure) 
# and belief (coartisan, non-coparisan)
belief_type_part = rep('', dim(data)[1])
belief_type_part[which(data$signaled_partisanship=='non-copartisan' & data$believed_partisanship=='non-copartisan')] = 'non_copart_sig_non_copart_bel'
belief_type_part[which(data$signaled_partisanship=='non-copartisan' & data$believed_partisanship=='copartisan')] = 'non_copart_sig_copart_bel'
belief_type_part[which(data$signaled_partisanship=='copartisan' & data$believed_partisanship=='copartisan')] = 'copart_sig_copart_bel'
belief_type_part[which(data$signaled_partisanship=='copartisan' & data$believed_partisanship=='non-copartisan')] = 'copart_sig_non_copart_bel'
belief_type_part[which(data$signaled_partisanship=='ND_undisclosed' &  data$believed_partisanship=='non-copartisan')] = 'ND_sig_non_copart_bel'
belief_type_part[which(data$signaled_partisanship=='ND_undisclosed' &  data$believed_partisanship=='copartisan')] = 'ND_sig_copart_bel'

data['belief_type_part'] = belief_type_part

# party2alter_num & party2_num
# note that the below code creates a variable for the strength of alter's party identification
# note also that it gives 0 to those undisclosed - in later analyses thar rely on this variable
# the non-disclosure condition is not analyzed
data$party2alter_num = 0
data$party2alter_num[which(data$party2alter=='Not very strong Republican')] = 0
data$party2alter_num[which(data$party2alter=='Republican')] = 1
data$party2alter_num[which(data$party2alter=='Strong Republican')] = 2
data$party2alter_num[which(data$party2alter=='Not very strong Democrat')] = 0
data$party2alter_num[which(data$party2alter=='Democrat')] = 1
data$party2alter_num[which(data$party2alter=='Strong Democrat')] = 2

data$party2_num = 0
data$party2_num[which(data$party2=='Not very strong Republican')] = 0
data$party2_num[which(data$party2=='Republican')] = 1
data$party2_num[which(data$party2=='Strong Republican')] = 2
data$party2_num[which(data$party2=='Not very strong Democrat')] = 0
data$party2_num[which(data$party2=='Democrat')] = 1
data$party2_num[which(data$party2=='Strong Democrat')] = 2


# strength, match and belief
data$strength = 0
data$strength[which(data$party2=='Not very strong Republican')] = 0
data$strength[which(data$party2=='Republican')] = 1
data$strength[which(data$party2=='Strong Republican')] = 2
data$strength[which(data$party2=='Not very strong Democrat')] = 0
data$strength[which(data$party2=='Democrat')] = 1
data$strength[which(data$party2=='Strong Democrat')] = 2

# if the party affiliation of the participant is exactly the same as that of partner's, then match is 1, otherwise 0
data$match = ifelse(data$party2==data$party2alter, 1, 0)

# distance is the social distance between the participant and the alter, where each party strength is coded from 0 to 2
data$distance = abs(data$party2_num - data$party2alter_num)

# if participant believed the alter was a non-copartisan, then the value should be 1; otherwise, it should be 0 
# note that in the Non disclosure condition, whatever is believed is coded to be 1 as well
data$belief = 1
data$belief[which(grepl('copart_sig_copart_bel', data$belief_type_part))] = 0
data$belief[which(grepl('non_copart_sig_copart_bel', data$belief_type_part))] = 0


# use sample contains everyone who was:
# whose ansswers to the two party-identification quesion part1 and party2 do not give conflicing responses
# who correctly remembered what they were shown in every condidion
# in baseline and ND AND they loosely took up the treatment

data$use_sample = 0
data$use_sample[which(data$attention_check==1 & ((data$arm_num==3) | (data$arm_num %in% c(1,2) & data$loose_treatment_takeup==1)))] = 1

# The Methods section specifies the following exlusion criteria

# baseline/ND not meeting the loose treatment takeup condition
length(which(data$arm_num %in% c(1,2) & data$loose_treatment_takeup==0))

# not meeting the attention check condition
length(which(data$attention_check==0))

# Weights

# The results reported in the main paper are produced using weighting scheme A
# This scheme gives equal weight to Republicans and Democrats so that they compose 50-50 of the sample
# It also gives 33% of each party strength within Democrats and Republicans, respectively
scale = 0.5/prop.table(table(data$party1[which(data$use_sample==1)]))

reps = subset(data, use_sample==1 & party1=='Republican')
weights_A_reps = 0.3333333333/prop.table(table(reps$party2))
weights_A_reps = weights_A_reps * scale['Republican'][[1]]
dems = subset(data, use_sample==1 & party1=='Democrat')
weights_A_dems = 0.3333333333/prop.table(table(dems$party2))
weights_A_dems = weights_A_dems  * scale['Democrat'][[1]] 


weight_col = rep(0, dim(data)[1])
weight_col[which(data$party2=='Not very strong Republican' & data$use_sample==1)] = weights_A_reps['Not very strong Republican'][[1]]
weight_col[which(data$party2=='Strong Republican' & data$use_sample==1)] = weights_A_reps['Strong Republican'][[1]]
weight_col[which(data$party2=='Republican' & data$use_sample==1)] = weights_A_reps['Republican'][[1]]
weight_col[which(data$party2=='Not very strong Democrat' & data$use_sample==1)] = weights_A_dems['Not very strong Democrat'][[1]]
weight_col[which(data$party2=='Strong Democrat' & data$use_sample==1)] = weights_A_dems['Strong Democrat'][[1]]
weight_col[which(data$party2=='Democrat' & data$use_sample==1)] = weights_A_dems['Democrat'][[1]]

# Note that this gives a weight to all respondents whose are not in the use sample
data$weightsA = weight_col

# weights B #######

# WEIGHTING SCHEME B

# First, we need to harmonize the variables
# the vars used are: sex, race, income, education, age
# for the education variable we use the 4-point scale from CCES guidebook (p. 18) 
# for income, we collapse $80,000-$89,000 and $90,000-$100,000 to match the CCES breakdown
# see here https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ZSBZ7K&version=6.0

ours = subset(data, use_sample==1)

# RECATEGORIZATION OF OUR DATA ########

# gender (called sex in our data)
ours$sex = mapvalues(ours$sex, 
                     c('Male', 'Female', 'Something else'),
                     c('Non_Female', 'Female', 'Non_Female'))

# education (educ)
ours$educ = mapvalues(ours$educ, 
                      c(" 2-year degree (Associate’s)", " 4-year degree (Bachelor’s)",
                        " Graduate or professional degree", " High school or equivalent (e.g., GED)",
                        " Less than high school", " Some college"),
                      c("Some_college", "Bachelors", "Graduate_professional",
                        'HS_less', 'HS_less', 'Some_college'))
# family income (income)
ours$income = mapvalues(ours$income, 
                        c("$30,000 - $39,999", "$60,000 - $69,999", "$20,000 - $29,999",
                          "$90,000 - $99,999", "$100,000 - $149,999", "$50,000 - $59,999",
                          "$40,000 - $49,999", "$70,000 - $79,999",
                          "$10,000 - $19,999", "I do not wish to report my income", "Less than $10,000",
                          "$150,000 or more", "$80,000 - $89,999"),
                        c("35000", '65000', "25000",
                          "90000", "125000", "55000",
                          "45000", "75000",
                          "15000", "Prefer_not_to_say", "5000",
                          "150000_more", "90000"))

# race

ours$race = ifelse(ours$race=='White', 'White', 'Non_White')

# RECATEGORIZATION OF CCES ########

cces = read_dta("CCES2018_OUTPUT.dta" )
table(cces$gender)

# gender 
cces$gender = mapvalues(as.character(cces$gender), 
                        c(1,2),
                        c('Non_Female', 'Female'))

table(cces$educ)

# educ
cces$educ = mapvalues(as.character(cces$educ), 
                      c(1:6),
                      c("HS_less", 'HS_less', 
                        'Some_college', "Some_college",
                        "Bachelors", "Graduate_professional"))

# family income
table(cces$faminc_new)
cces$faminc_new = mapvalues(as.numeric(cces$faminc_new), 
                            c(1:16, 97),
                            c("5000", "15000", "25000",
                              "35000", "45000", "55000",
                              "65000", "75000", "90000",
                              "125000", "125000", "150000_more",
                              "150000_more","150000_more","150000_more",
                              "150000_more", "Prefer_not_to_say"))

# race
table(cces$race)
cces$race = ifelse(cces$race==1, 'White', 'Non_White')

cces['age'] = 2018-cces$birthyr

cces$gender = as_factor(cces$gender)
cces$educ = as_factor(cces$educ)
cces$faminc_new = as_factor(cces$faminc_new)
cces$race = as_factor(cces$race)
cces$pid3 = as_factor(cces$pid3)

# Combining both datasets
ours = ours[,c('age', 'sex', "educ", "income", "race", "party1")]
cces = cces[,c('age', 'gender', 'educ', 'faminc_new', "race", "pid3", "commonweight")]
ours['commonweight'] = 1
cces['ResponseId'] = NA
ours['ResponseId'] = data$ResponseId[which(data$use_sample==1)]
cces['from_cces'] = 1
ours['from_cces'] = 0
names(ours) = names(cces) 

cces = subset(cces, pid3 %in% c('Republican', 'Democrat'))

all = rbind(cces, ours)
all = as.data.frame(all)
everyone = all


everyone$age_binned = rep('18_19', dim(everyone)[1])
everyone$age_binned[which((everyone$age>=20) & (everyone$age<=24))] = '20_24'
everyone$age_binned[which((everyone$age>=25) & (everyone$age<=29))] = '25_29'
everyone$age_binned[which((everyone$age>=30) & (everyone$age<=34))] = '30_34'
everyone$age_binned[which((everyone$age>=35) & (everyone$age<=39))] = '35_39'
everyone$age_binned[which((everyone$age>=40) & (everyone$age<=49))] = '40_49'
everyone$age_binned[which((everyone$age>=50) & (everyone$age<=59))] = '50_59'
everyone$age_binned[which(everyone$age>=60)] = 'above_60'

# setting up the parameters 

max_weight = 5
max_weight_dems = max_weight / scale['Democrat'][[1]]
max_weight_reps =  max_weight / scale['Republican'][[1]]

# DEMS 

# balance table before weighting, i.e. CCES people are weighted, ours = 1

dems = subset(everyone, pid3=='Democrat')
dim(dems)

dems = droplevels(dems)

dems$gender = as.factor(dems$gender)
dems$race = as.factor(dems$race)
dems$faminc_new = as.factor(dems$faminc_new)
dems$educ = as.factor(dems$educ)
dems$age_binned = as.factor(dems$age_binned)

# DEMS: Weighting prep

# reorder levels for each of the variables in the data
dems$gender = factor(dems$gender, levels=c('Female', 'Non_Female'), ordered = T)

dems$race = factor(dems$race, levels=c('White', 'Non_White'), ordered = T)
dems$educ = factor(dems$educ, levels=c('HS_less', 'Some_college',
                                       'Bachelors', 'Graduate_professional'), ordered = T)
dems$faminc_new = factor(dems$faminc_new, levels=c('5000', '15000', '25000', '35000', '45000',
                                                   '55000', '65000', '75000', '90000', '125000', 
                                                   '150000_more', 'Prefer_not_to_say'), ordered = T)
dems$age_binned = factor(dems$age_binned, levels=c('18_19', '20_24', '25_29', '30_34', '35_39',
                                                   '40_49', '50_59', 'above_60'), ordered = T)

# disaggregate ours and cces dems

our_dems = subset(dems, from_cces==0)
dems_cces = subset(dems, from_cces==1)

our_dems$caseid = 1:dim(our_dems)[1]

# we need a list of proportions for each variable
gender_target = prop.table(Hmisc::wtd.table(dems_cces$gender, weights = dems_cces$commonweight)$sum.of.weights)
names(gender_target) = levels(dems_cces$gender)

race_target = prop.table(Hmisc::wtd.table(dems_cces$race, weights = dems_cces$commonweight)$sum.of.weights)
names(race_target) = levels(dems_cces$race)

educ_target = prop.table(Hmisc::wtd.table(dems_cces$educ, weights = dems_cces$commonweight)$sum.of.weights)
names(educ_target) = levels(dems_cces$educ)

income_target = prop.table(Hmisc::wtd.table(dems_cces$faminc_new, weights = dems_cces$commonweight)$sum.of.weights)
names(income_target) = levels(dems_cces$faminc_new)

age_target = prop.table(Hmisc::wtd.table(dems_cces$age_binned, weights = dems_cces$commonweight)$sum.of.weights)
names(age_target) = levels(dems_cces$age_binned)

targets = list(gender_target, race_target, educ_target, income_target, age_target)
names(targets) = c('gender','race', 'educ', 'faminc_new', 'age_binned')

dems_raked = anesrake( targets, our_dems,cap=max_weight_dems, caseid = our_dems$caseid)
dems_raked_nc = anesrake( targets, our_dems, cap=max_weight, caseid = our_dems$caseid)

# REPS: Weighting prep
reps = subset(everyone, pid3=='Republican')

# reorder levels for each of the variables in the data
reps$gender = factor(reps$gender, levels=c('Female', 'Non_Female'), ordered = T)

reps$race = factor(reps$race, levels=c('White', 'Non_White'), ordered = T)
reps$educ = factor(reps$educ, levels=c('HS_less', 'Some_college',
                                       'Bachelors', 'Graduate_professional'), ordered = T)
reps$faminc_new = factor(reps$faminc_new, levels=c('5000', '15000', '25000', '35000', '45000',
                                                   '55000', '65000', '75000', '90000', '125000', 
                                                   '150000_more', 'Prefer_not_to_say'), ordered = T)
reps$age_binned = factor(reps$age_binned, levels=c('18_19', '20_24', '25_29', '30_34', '35_39',
                                                   '40_49', '50_59', 'above_60'), ordered = T)

# disaggregate ours and cces reps

our_reps = subset(reps, from_cces==0)
reps_cces = subset(reps, from_cces==1)

our_reps$caseid = 1:dim(our_reps)[1]

# we need a list of proportions for each variable
gender_target = prop.table(Hmisc::wtd.table(reps_cces$gender, weights = reps_cces$commonweight)$sum.of.weights)
names(gender_target) = levels(reps_cces$gender)

race_target = prop.table(Hmisc::wtd.table(reps_cces$race, weights = reps_cces$commonweight)$sum.of.weights)
names(race_target) = levels(reps_cces$race)

educ_target = prop.table(Hmisc::wtd.table(reps_cces$educ, weights = reps_cces$commonweight)$sum.of.weights)
names(educ_target) = levels(reps_cces$educ)

income_target = prop.table(Hmisc::wtd.table(reps_cces$faminc_new, weights = reps_cces$commonweight)$sum.of.weights)
names(income_target) = levels(reps_cces$faminc_new)

age_target = prop.table(Hmisc::wtd.table(reps_cces$age_binned, weights = reps_cces$commonweight)$sum.of.weights)
names(age_target) = levels(reps_cces$age_binned)

targets = list(gender_target, race_target, educ_target, income_target, age_target)
names(targets) = c('gender','race', 'educ', 'faminc_new', 'age_binned')

reps_raked = anesrake( targets, our_reps,cap=max_weight_reps, caseid = our_reps$caseid)
reps_raked_nc = anesrake( targets, our_reps,cap=max_weight, caseid = our_dems$caseid)

##### combining weights with the use_sample

# Note that weightsB = 0 for all respondents whose use_sample value is 0
data[which(data$use_sample==1 & data$party1=='Democrat'),'weightsB'] = dems_raked$weightvec * scale['Democrat'][[1]]
data[which(data$use_sample==1 & data$party1=='Republican'),'weightsB'] = reps_raked$weightvec * scale['Republican'][[1]]


# binning for distribution tables

weight_group = rep('', dim(data)[1])
weight_group[which(data$weightsB>=0 & data$weightsB<1)] = '0_1'
weight_group[which(data$weightsB>=1 & data$weightsB<2)] = '1_2'
weight_group[which(data$weightsB>=2 & data$weightsB<3)] = '2_3'
weight_group[which(data$weightsB>=3 & data$weightsB<4)] = '3_4'
weight_group[which(data$weightsB>=4 )] = '4_5'

data$weightsB_binned = weight_group 


# creating variables for regression
# note that the code below creates the variable "weights" which only exists
# for respondents in the falsification condition where we derive resgression results
subdata = subset(data, arm_num==3 & use_sample==1 & belief_type_part %in% c('copart_sig_copart_bel', 'copart_sig_non_copart_bel'))

reps = subset(subdata, party1=='Republican')
dems = subset(subdata, party1=='Democrat')

rep_counts = reps %>%
  dplyr::group_by(party2, party2alter) %>%
  dplyr::mutate(n = length(party1)) %>%
  dplyr::distinct(party2, party2alter, n)
rep_counts$share = rep_counts$n / sum(rep_counts$n)
rep_counts$weights = (1/dim(rep_counts)[1]) / rep_counts$share

dem_counts = dems %>%
  dplyr::group_by(party2, party2alter) %>%
  dplyr::mutate(n = length(party1)) %>%
  dplyr::distinct(party2, party2alter, n)
dem_counts$share = dem_counts$n / sum(dem_counts$n)
dem_counts$weights = (1/dim(dem_counts)[1]) / dem_counts$share

pooled_counts = subdata %>%
  dplyr::group_by(party2, party2alter) %>%
  dplyr::mutate(n = length(party1)) %>%
  dplyr::distinct(party2, party2alter, n)
pooled_counts$share = pooled_counts$n / sum(pooled_counts$n)

pooled_counts$weights = (1/dim(pooled_counts)[1]) / pooled_counts$share

keep_cols = c('ResponseId', 'party2', 'party2alter', 'memory1', 'memory2', 'belief_type_part', 'arm_num')
reps = reps[,keep_cols]
dems = dems[,keep_cols]
subdata = subdata[,keep_cols]

reps = merge(reps, rep_counts, by=c("party2","party2alter")) 
dems = merge(dems, dem_counts, by=c("party2","party2alter")) 
pooled = merge(subdata, pooled_counts, by=c("party2","party2alter")) 

rd = rbind(rep_counts, dem_counts)
names(pooled_counts)[3] = "np"
names(pooled_counts)[4] = "sharep"
names(pooled_counts)[5] = "weightsp"


rd = merge(rd, pooled_counts, by=c('party2', 'party2alter'))
fls = merge(subset(data, arm_num==3 & use_sample==1 & belief_type_part %in% c('copart_sig_copart_bel', 'copart_sig_non_copart_bel') ), rd, by=c("party2","party2alter"), all.x = T)
data = rbind.fill(subset(data, ! ResponseId %in% fls$ResponseId), fls)

# alter variables saving originals and modifying for analysis
data$genderalter_original = data$genderalter
data$educalter_original = data$educalter
data$racealter_original = data$racealter

data$genderalter = mapvalues(data$genderalter, 
                             c('Male', 'Female', 'Something else'),
                             c('Non_Female', 'Female', 'Non_Female'))

# education (educ)
data$educalter = mapvalues(data$educalter, 
                           c(" 2-year degree (Associate’s)", " 4-year degree (Bachelor’s)",
                             " Graduate or professional degree", " High school or equivalent (e.g., GED)",
                             " Less than high school", " Some college"),
                           c("Some_college", "Bachelors", "Graduate_professional",
                             'HS_less', 'HS_less', 'Some_college'))

data$racealter = ifelse(data$racealter=='White', 'White', 'Non_White')

# weightsA for all people who passed attention check; i.e., where those who did not
# pass the loose treatment takeup explusion criteria are not fildeted out

# weightsA_all
use_sample = subset(data, attention_check==1)

scale = 0.5/prop.table(table(use_sample$party1))
reps = subset(use_sample, party1=='Republican')
weights_A_reps = 0.3333333333/prop.table(table(reps$party2))
weights_A_reps = weights_A_reps * scale['Republican'][[1]] 
dems = subset(use_sample, party1=='Democrat')
weights_A_dems = 0.3333333333/prop.table(table(dems$party2))
weights_A_dems = weights_A_dems  * scale['Democrat'][[1]] 


weight_col = rep(0, dim(use_sample)[1])

weight_col[which(use_sample$party2=='Not very strong Republican')] = weights_A_reps['Not very strong Republican'][[1]]
weight_col[which(use_sample$party2=='Strong Republican')] = weights_A_reps['Strong Republican'][[1]]
weight_col[which(use_sample$party2=='Republican')] = weights_A_reps['Republican'][[1]]
weight_col[which(use_sample$party2=='Not very strong Democrat')] = weights_A_dems['Not very strong Democrat'][[1]]
weight_col[which(use_sample$party2=='Strong Democrat')] = weights_A_dems['Strong Democrat'][[1]]
weight_col[which(use_sample$party2=='Democrat')] = weights_A_dems['Democrat'][[1]]

use_sample$weightsA_all = weight_col

data = merge(data, use_sample[,c('ResponseId', 'weightsA_all')], all.x = T)

# saving the original demographics and modifying for analysis

data$sex_original = data$sex
data$educ_original = data$educ
data$income_original = data$income
data$race_original = data$race

# sex
data$sex = mapvalues(data$sex, 
                     c('Male', 'Female', 'Something else'),
                     c('Non_Female', 'Female', 'Non_Female'))

# education (educ)
data$educ = mapvalues(data$educ, 
                      c(" 2-year degree (Associate’s)", " 4-year degree (Bachelor’s)",
                        " Graduate or professional degree", " High school or equivalent (e.g., GED)",
                        " Less than high school", " Some college"),
                      c("Some_college", "Bachelors", "Graduate_professional",
                        'HS_less', 'HS_less', 'Some_college'))
# family income (income)
data$income = mapvalues(data$income, 
                        c("$30,000 - $39,999", "$60,000 - $69,999", "$20,000 - $29,999",
                          "$90,000 - $99,999", "$100,000 - $149,999", "$50,000 - $59,999",
                          "$40,000 - $49,999", "$70,000 - $79,999",
                          "$10,000 - $19,999", "I do not wish to report my income", "Less than $10,000",
                          "$150,000 or more", "$80,000 - $89,999"),
                        c("35000", '65000', "25000",
                          "95000", "125000", "55000",
                          "45000", "75000",
                          "15000", "Prefer_not_to_say", "5000",
                          "150000_more", "85000"))

# race

data$race = ifelse(data$race=='White', 'White', 'Non_White')

data$fail_attention = abs(data$attention_check - 1 )
data$fail_treatment = abs(data$loose_treatment_takeup - 1 )

# some other vars for regression

data$age_binned = rep('18_19', dim(data)[1])
data$age_binned[which((data$age>=20) & (data$age<=24))] = '20_24'
data$age_binned[which((data$age>=25) & (data$age<=29))] = '25_29'
data$age_binned[which((data$age>=30) & (data$age<=34))] = '30_34'
data$age_binned[which((data$age>=35) & (data$age<=39))] = '35_39'
data$age_binned[which((data$age>=40) & (data$age<=49))] = '40_49'
data$age_binned[which((data$age>=50) & (data$age<=59))] = '50_59'
data$age_binned[which(data$age>=60)] = 'above_60'


data$non_copartisan = ifelse(data$signaled_partisanship=='non-copartisan', 1, 0) 
data$myideology = ifelse(data$party1=='Democrat', 0, 1)

# data with cces: including weights A and B
everyone$commonweight_A = everyone$commonweight
everyone$commonweight_B = everyone$commonweight

everyone$commonweight_B[which(everyone$from_cces==0 & everyone$pid3=='Democrat')] = data$weightsB[which(data$party1=='Democrat' & data$use_sample==1)]
everyone$commonweight_B[which(everyone$from_cces==0 & everyone$pid3=='Republican')] = data$weightsB[which(data$party1=='Republican' & data$use_sample==1)]

everyone$commonweight_A[which(everyone$from_cces==0 & everyone$pid3=='Democrat')] = data$weightsA[which(data$party1=='Democrat' & data$use_sample==1)]
everyone$commonweight_A[which(everyone$from_cces==0 & everyone$pid3=='Republican')] = data$weightsA[which(data$party1=='Republican' & data$use_sample==1)]

write.csv(everyone, 'data_w_cces.csv', row.names = F)
write.csv(data, 'cleaned_data.csv', row.names = F)
