# The code in file produces results reported in the main text of Abascal, Makovi and Sargsyan (2021)
# in order of appearance          

#############################################################
# Read me on variables used:
# arm_num - experimental condition that the participant participated in:
#           1 = baseline 
#           2 = nondisclosure
#           3 = falsification
#
# attention_check - binary variable, if the participant correctly recalled the partner's signaled partisanship, 
#           1 = if this condition is met
#           0 = otherwise
#
# signaled_partisanship - signaled partisanship of partner
#           copartisan = when the participant's self-reported partisanship and their partner's align
#           non-copartisan = when the participant's self-reported partisanship and their partner's do not align
#           ND_undisclosed = when the participant was paired with a partner in the non-disclosure condition whose partisanship was not revealed to them
#
# believed_partisanship - the partisanship of partner that the participant believed to be true
#           copartisan = when the participant believed to have the same partisanship as their partner
#           non-copartisan = otherwise
#
# dishonesty - binary variable if the participant expressed thoughts about their partner being dishonest
#           1 = if this condition is met
#           0 = otherwise
#
# use_sample - binary variable, and corresponds to selecting cases as described in the Methods
#           1 = if this condition is met
#           0 = otherwise
#############################################################
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Loading all the required packages ############
library(dplyr)
library(plyr)
library(haven)
library(questionr)
library(reshape)
library(knitr)
library(psych)
library(BSDA)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(weights)
source('plotting.R')

### Patterns of Beliefs ############

data = read.csv('cleaned_data.csv', stringsAsFactors = F)

# The following analyses are based on those 2,538 participants who met the screening criteria, 
# who correctly answered comprehension-check questions prior to playing the DG, 
# and who successfully took up treatment

table(data$use_sample)

# Table 1 #####
# Note that the observations in Table 1 contain 2618 respondents
# These respondents include additional participants whose data are not analyzed in the rest of the text
# For additional detail, see Methods

data = read.csv('cleaned_data.csv', stringsAsFactors = F)
data = subset(data, attention_check==1)

data$has_believed = ifelse(data$believed_partisanship==data$signaled_partisanship, 1, 0)

prop.table(table(data$signaled_partisanship[which(data$arm_num==1)],  
                 data$has_believed[which(data$arm_num==1)]), 1)

prop.table(table(data$signaled_partisanship[which(data$arm_num==2)],  
                 data$has_believed[which(data$arm_num==2)]), 1)

prop.table(table(data$signaled_partisanship[which(data$arm_num==3)],  
                 data$has_believed[which(data$arm_num==3)]), 1)

# t tests ######

# fls copartisan vs baseline copartisan

# This is significantly lower than the 93.61\% of participants in the 
# baseline condition who believed a signaled copartisan was a copartisan 
# ($P < 0.001$, one sided t-test,  H$_{1}: \mu_1 < \mu_2$).

x = data$has_believed[which(data$arm_num==1 & data$signaled_partisanship=='copartisan')]
y = data$has_believed[which(data$arm_num==3 & data$signaled_partisanship=='copartisan')]

t.test(x,y, alternative = 'greater')$p.value

# fls noncopartisan vs baseline noncopartisan

# By contrast, of participants whose partner was a signaled non-copartisan, 
# 99.02\% believed they were a non-copartisan. This is comparable to the 97.78\% 
# of participants in the baseline condition who believed a 
# signaled non-copartisan was a non-copartisan ($P = 0.159$, two sided t-test).

x = data$has_believed[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan')]
y = data$has_believed[which(data$arm_num==3 & data$signaled_partisanship=='non-copartisan')]

t.test(x,y, alternative = 'two.sided')$p.value

# nd 

# However, of participants whose partner withheld their party affiliation, 
# a majority (86.42\%) believed they were a non-copartisan 
# ($P < 0.001$, two-sided z-test, H$_{0} = 0.5$).

prop.table(table(data$believed_partisanship[which(data$arm_num==2 & data$signaled_partisanship=='ND_undisclosed')]))

x = data$believed_partisanship[which(data$arm_num==2 & data$signaled_partisanship=='ND_undisclosed')]
y = as.numeric(mapvalues(x, from=c('copartisan', 'non-copartisan'),
                         to=c(0,1)))

x <- length(y[y == 1])
n <- length(y)

prop.test(x, n, p = .5, alternative = "two.sided")

### Baseline and Falsification ############

# Note that the data are read in again, and need to be subsetted, so that
# the rest of the analyses is carried out on the use sample only.

# In the baseline and non-disclosure conditions, participants should have believed the identity 
# signaled by their partner. These participants ``took up treatment.'' The following analyses are 
# based on the vast majority of participants (90.03\%) in the baseline and non-disclosure conditions 
# who believed a signaled copartisan was, in fact, a copartisan or a signaled non-copartisan was, 
# in fact,a non-copartisan.

prop.table(table(data$loose_treatment_takeup[which(data$attention_check==1 & data$arm_num < 3)]))

# only 17 participants gave more than $1.00

data = subset(data, use_sample==1)
table(data$sending)

# Participants could contribute any amount between 
# \$0.00 and \$2.00, down to the \$0.01 increment; however, 
# the vast majority contributed \$0.00 (34.99\%) or \$1.00 (41.65\%). 

round(table(data$sending[which(data$use_sample == 1)])/sum(table(data$sending[which(data$use_sample == 1)]))*100,2)

# Table 2

tt = subset(data, attention_check==1) %>%
  dplyr::group_by(arm_num, signaled_partisanship)%>%
  dplyr::summarise(mean_w = wtd.mean(fairb, weightsA) * 100)
xx = cast(tt, arm_num ~ signaled_partisanship)
xx = xx[c(1,3,2),c(1,2,4,3)]
round(xx,2)

# t tests

# baseline copartisan vs non copartisan
# note that based on the above subset these data only contains participants who passed the attention check

# Indeed, 46.61\% of baseline participants with a copartisan partner behaved equitably; 
# by comparison, just 37.34\% of baseline participants with a non-copartisan partner 
# behaved equitably (Cohen's h $=$ 0.19, $P < 0.01$, two-sided t-test). 

vectorx = data$fairb[which(data$arm_num==1 & data$signaled_partisanship=='copartisan' & data$attention_check==1)]
weightx = data$weightsA[which(data$arm_num==1 & data$signaled_partisanship=='copartisan' & data$attention_check==1)]
vectory = data$fairb[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan' & data$attention_check==1)]
weighty = data$weightsA[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan' & data$attention_check==1)]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'two.sided')$coefficients[['p.value']]

# cohen's H
meanx <- wtd.mean(vectorx, weight = weightx)
meany <- wtd.mean(vectory, weight = weighty)
2 * asin(sqrt(meanx)) - 2 * asin(sqrt(meany)) 

# fls copartisan vs non copartisan 

# Specifically, 41.78\% of participants behaved equitably toward partners who signaled a 
# copartisan affiliation, compared to 40.81\% of participants who behaved equitably toward partners 
# who signaled a non-copartisan affiliation. This small difference, however, 
# is not statistically significant (Cohen's h $=$ 0.02, $P = 0.773$, two sided t-test). 

vectorx = data$fairb[which(data$arm_num==3 & data$signaled_partisanship=='copartisan')]
weightx = data$weightsA[which(data$arm_num==3 & data$signaled_partisanship=='copartisan')]
vectory = data$fairb[which(data$arm_num==3 & data$signaled_partisanship=='non-copartisan')]
weighty = data$weightsA[which(data$arm_num==3 & data$signaled_partisanship=='non-copartisan')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'two.sided')$coefficients[['p.value']]

## cohen's H
meanx <- wtd.mean(vectorx, weight = weightx)
meany <- wtd.mean(vectory, weight = weighty)
2 * asin(sqrt(meanx)) - 2 * asin(sqrt(meany)) 

# fls noncopartisan vs baseline non copartisan 

# Specifically, 40.71\% of participants behaved equitably toward a non-copartisan whom they 
# believed to be a non-copartisan in the falsification condition. By comparison, 37.34\% of 
# participants behaved equitably toward a non-copartisan in the baseline condition. 
# The difference, though, is not statistically significant ($P = 0.325$, two sided t-test).

vectorx = data$fairb[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan')]
weightx = data$weightsA[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan')]
vectory = data$fairb[which(data$arm_num==3 & data$signaled_partisanship=='non-copartisan' & data$believed_partisanship=='non-copartisan')]
weighty = data$weightsA[which(data$arm_num==3 & data$signaled_partisanship=='non-copartisan' & data$believed_partisanship=='non-copartisan')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'two.sided')$coefficients[['p.value']]

# fls singaled copartisan vs baseline signaled copartisan

# Second, participants give signaled copartisans slightly less in the falsification condition 
# than in the baseline condition. 41.78\% of participants behaved equitably toward a 
# signaled copartisan in the falsification condition, compared to 46.61\% of participants 
# who behaved equitably toward a signaled copartisan 
# in the baseline condition ($P = 0.083$, one sided t-test,  H$_{1}: \mu_1 < \mu_2$).

vectorx = data$fairb[which(data$arm_num==1 & data$signaled_partisanship=='copartisan')]
weightx = data$weightsA[which(data$arm_num==1 & data$signaled_partisanship=='copartisan')]
vectory = data$fairb[which(data$arm_num==3 & data$signaled_partisanship=='copartisan')]
weighty = data$weightsA[which(data$arm_num==3 & data$signaled_partisanship=='copartisan')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# baseline fairness vs fls fairness 

# Aggregating these trends, the shares of participants who behaved equitably toward 
# all partners---copartisan and non-copartisan---were comparable in the falsification 
# condition (41.30\%)  and the baseline condition 41.77\%, ($P = 0.848$, two sided t-test).

vectorx = data$fairb[which(data$arm_num==1)]
weightx = data$weightsA[which(data$arm_num==1)]
vectory = data$fairb[which(data$arm_num==3)]
weighty = data$weightsA[which(data$arm_num==3)]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'two.sided')$coefficients[['p.value']]

# Recall that among participants whose partner was a signaled copartisan, just 70.35\% 
# believed they were a copartisan. 49.45\% of these participants behaved equitably toward 
# their partners, compared to just 46.61\% who behaved equitably toward a 
# signaled copartisan in the baseline condition (though $P = 0.461$, two sided t-test)

prop.table(table(data$believed_partisanship[which(data$arm_num==3 & data$signaled_partisanship=='copartisan')]))
vectorx = data$fairb[which(data$arm_num==3 & data$signaled_partisanship=='copartisan' & data$believed_partisanship=='copartisan')]
weightx = data$weightsA[which(data$arm_num==3 & data$signaled_partisanship=='copartisan' & data$believed_partisanship=='copartisan')]
vectory = data$fairb[which(data$arm_num==1 & data$signaled_partisanship=='copartisan')]
weighty = data$weightsA[which(data$arm_num==1 & data$signaled_partisanship=='copartisan')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'two.sided')$coefficients[['p.value']]

# fls signaled copartisan non copartisan believed_partisanship

# Just 23.92\% of these participants behaved equitably toward their partner, significantly 
# less than the 40.72\% who behaved equitably toward a signaled non-copartisan in the 
# falsification condition ($P < 0.001$,  one sided t-test,  H$_{1}: \mu_1 < \mu_2$) 
# or the 37.34\% who behaved equitably toward a non-copartisan in the 
# baseline condition ($P < 0.01$, one sided t-test,  H$_{1}: \mu_1 < \mu_2$).

vectorx = data$fairb[which(data$arm_num==3 & data$signaled_partisanship=='copartisan' & data$believed_partisanship=='non-copartisan')]
weightx = data$weightsA[which(data$arm_num==3 & data$signaled_partisanship=='copartisan' & data$believed_partisanship=='non-copartisan')]
vectory = data$fairb[which(data$arm_num==3 & data$signaled_partisanship=='non-copartisan' & data$believed_partisanship=='non-copartisan')]
weighty = data$weightsA[which(data$arm_num==3 & data$signaled_partisanship=='non-copartisan' & data$believed_partisanship=='non-copartisan')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'less')$coefficients[['p.value']]

vectory = data$fairb[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan' & data$believed_partisanship=='non-copartisan')]
weighty = data$weightsA[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan' & data$believed_partisanship=='non-copartisan')]
wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'less')$coefficients[['p.value']]

# dishonesty references in fls

# Among participants in the falsification condition whose partner was a signaled copartisan, 
# 13.33\% referenced dishonesty in their open responses. This is greater than the 
# share (0.00\%) who used similar language when paired with a signaled non-copartisan 
# in the baseline condition ($P < 0.001$, one sided t-test,  H$_{1}: \mu_1 > \mu_2$) 
# or the share (0.83\%) who used similar language when paired with a signaled 
# non-copartisan in the non-disclosure 
# condition ($P < 0.001$, one sided t-test,  H$_{1}: \mu_1 > \mu_2$).

vectorx = data$dishonesty[which(data$arm_num==3 & data$signaled_partisanship=='copartisan')]
weightx = data$weightsA[which(data$arm_num==3 & data$signaled_partisanship=='copartisan')]
vectory = data$dishonesty[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan')]
weighty = data$weightsA[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

vectory = data$dishonesty[which(data$arm_num==2 & data$signaled_partisanship=='non-copartisan')]
weighty = data$weightsA[which(data$arm_num==2 & data$signaled_partisanship=='non-copartisan')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]


# Results indicate that a one-point increase in a participant's partisanship strength 
# corresponds to a 7.71\% increase in the predicted probability 
# of believing a signaled copartisan is a non-copartisan ($P < 0.01$)
subdata = subset(data, arm_num==3 & belief_type_part %in% c('copart_sig_copart_bel', 'copart_sig_non_copart_bel') & use_sample==1)

dem_strength = lm(belief~strength, weights = weights, data = subset(subdata, party1=='Democrat'))
rep_strength = lm(belief~strength, weights = weights, data = subset(subdata, party1=='Republican'))
pooled_strength = lm(belief~strength, weights = weightsp, data = subdata)

stargazer(dem_strength, rep_strength, pooled_strength, title="Results", align=TRUE,
          column.labels = c("Democrat", "Republican", 'Pooled'), object.names =FALSE, type = 'text', 
          star.cutoffs = c(.05, .01, .001))

### Non-Disclosure ############

# nd withheld vs baseline non copartisan

# This is slightly less than the 37.34\% who 
# behaved equitably toward a signaled non-copartisan in the baseline condition. 
# However, the difference is not statistically significant 
# ($P = 0.312$, one sided t-test,  H$_{1}: \mu_1 < \mu_2$). 

vectorx = data$fairb[which(data$arm_num==2 & data$signaled_partisanship=='ND_undisclosed')]
weightx = data$weightsA[which(data$arm_num==2 & data$signaled_partisanship=='ND_undisclosed')]
vectory = data$fairb[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan')]
weighty = data$weightsA[which(data$arm_num==1 & data$signaled_partisanship=='non-copartisan')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'less')$coefficients[['p.value']]

# fls copartisan signaled non copartisan believed vs nd withheld

# This is less than the 34.94\% of participants who behaved equitably toward a 
# partner who withheld their affiliation in the non-disclosure 
# condition ($P < 0.05$, one sided t-test,  H$_{1}: \mu_1 < \mu_2$).

vectorx = data$fairb[which(data$arm_num==3 & data$signaled_partisanship=='copartisan' & data$believed_partisanship=='non-copartisan')]
weightx = data$weightsA[which(data$arm_num==3 & data$signaled_partisanship=='copartisan' & data$believed_partisanship=='non-copartisan')]
vectory = data$fairb[which(data$arm_num==2 & data$signaled_partisanship=='ND_undisclosed')]
weighty = data$weightsA[which(data$arm_num==2 & data$signaled_partisanship=='ND_undisclosed')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative='less')$coefficients[['p.value']]

# Figure 1 raw

plot_helper(data, var='fairb', weights='weightsA')

# Comparisons reported on the figure by experimental condition

# Baseline: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed
vectorx = data$fairb[which(data$arm_num==1 & data$belief_type_part=='copart_sig_copart_bel' & data$use_sample == 1)]
weightx = data$weightsA[which(data$arm_num==1 & data$belief_type_part=='copart_sig_copart_bel' & data$use_sample == 1)]
vectory = data$fairb[which(data$arm_num==1 & data$belief_type_part=='non_copart_sig_non_copart_bel' & data$use_sample == 1)]
weighty = data$weightsA[which(data$arm_num==1 & data$belief_type_part=='non_copart_sig_non_copart_bel' & data$use_sample == 1)]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = data$fairb[which(data$arm_num==3 & data$belief_type_part=='copart_sig_copart_bel' & data$use_sample == 1)]
weightx = data$weightsA[which(data$arm_num==3 & data$belief_type_part=='copart_sig_copart_bel' & data$use_sample == 1)]
vectory = data$fairb[which(data$arm_num==3 & data$belief_type_part=='non_copart_sig_non_copart_bel' & data$use_sample == 1)]
weighty = data$weightsA[which(data$arm_num==3 & data$belief_type_part=='non_copart_sig_non_copart_bel' & data$use_sample == 1)]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]


# Falsification: copartisan signaled copartisan believed vs copartisan signaled non-copartisan believed

vectorx = data$fairb[which(data$arm_num==3 & data$belief_type_part=='copart_sig_copart_bel' & data$use_sample == 1)]
weightx = data$weightsA[which(data$arm_num==3 & data$belief_type_part=='copart_sig_copart_bel' & data$use_sample == 1)]
vectory = data$fairb[which(data$arm_num==3 & data$belief_type_part=='copart_sig_non_copart_bel' & data$use_sample == 1)]
weighty = data$weightsA[which(data$arm_num==3 & data$belief_type_part=='copart_sig_non_copart_bel' & data$use_sample == 1)]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: non-copartisan signaled non-copartisan believed vs copartisan signaled non-copartisan believed

vectorx = data$fairb[which(data$arm_num==3 & data$belief_type_part=='non_copart_sig_non_copart_bel' & data$use_sample == 1)]
weightx = data$weightsA[which(data$arm_num==3 & data$belief_type_part=='non_copart_sig_non_copart_bel' & data$use_sample == 1)]
vectory = data$fairb[which(data$arm_num==3 & data$belief_type_part=='copart_sig_non_copart_bel' & data$use_sample == 1)]
weighty = data$weightsA[which(data$arm_num==3 & data$belief_type_part=='copart_sig_non_copart_bel' & data$use_sample == 1)]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = data$fairb[which(data$arm_num==2 & data$belief_type_part=='copart_sig_copart_bel' & data$use_sample == 1)]
weightx = data$weightsA[which(data$arm_num==2 & data$belief_type_part=='copart_sig_copart_bel' & data$use_sample == 1)]
vectory = data$fairb[which(data$arm_num==2 & data$belief_type_part=='non_copart_sig_non_copart_bel' & data$use_sample == 1)]
weighty = data$weightsA[which(data$arm_num==2 & data$belief_type_part=='non_copart_sig_non_copart_bel' & data$use_sample == 1)]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: non-copartisan signaled non-copartisan believed vs not disclosed, non-copartisan believed
vectorx = data$fairb[which(data$arm_num==2 & data$belief_type_part=='non_copart_sig_non_copart_bel' & data$use_sample == 1)]
weightx = data$weightsA[which(data$arm_num==2 & data$belief_type_part=='non_copart_sig_non_copart_bel' & data$use_sample == 1)]
vectory = data$fairb[which(data$arm_num==2 & data$belief_type_part=='ND_sig_non_copart_bel' & data$use_sample == 1)]
weighty = data$weightsA[which(data$arm_num==2 & data$belief_type_part=='ND_sig_non_copart_bel' & data$use_sample == 1)]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]
