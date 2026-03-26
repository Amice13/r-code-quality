# The code in this file produces results reported in the SM of Abascal, Makovi and Sargsyan (2021)
# in order of appearance  

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr) # data manipulation
library(survey) # creating survey weights
library(tableone) # producing stratified tables
library(mltools) # for one-hot encoding
library(reshape) # casting and melting
library(questionr) # weighted calculations
library(plyr) # data manipulation
library(stargazer) # table creation
library(xtable) # latex table creation
library(weights) # weighted t test
source('plotting.R')

data = read.csv('cleaned_data.csv', stringsAsFactors = F)

# the following variables are used to reorder some of the tables
party_strength_order = c('Strong Democrat', 'Democrat', 'Not very strong Democrat',
                         'Strong Republican', 'Republican', 'Not very strong Republican')

party_strength_order_w_nd = c('Strong Democrat', 'Democrat', 'Not very strong Democrat',
                              'Strong Republican', 'Republican', 'Not very strong Republican',
                              'ND_undisclosed')

justifications = c('ideology', 'myideo', 'extreme', 'fair', 'selfish',
                   'reciprocity', 'merit', 'ethics', 'nomatter', 'dontknowb',
                   'dishonesty', 'honesty', 'hardship', 'selfpres')

# The average response contained 25.78 words (SD = 16.73)

round(mean(data$n_words, na.rm=T),2)
round(sd(data$n_words, na.rm=T),2)

# The average participant gave 1.55 reasons (SD = 0.83) for their allocation decision.

round(mean(data$n_justifications, na.rm=T),2)
round(sd(data$n_justifications, na.rm=T),2)

# FIGURE S2
tmp_df = subset(data, party1=='Democrat' & use_sample==1)
plot_helper(tmp_df, var='fairb', weights='weightsA')

# Baseline: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed
vectorx = tmp_df$fairb[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]


# Falsification: copartisan signaled copartisan believed vs copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel' )]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: non-copartisan signaled non-copartisan believed vs copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: non-copartisan signaled non-copartisan believed vs not disclosed, non-copartisan believed
vectorx = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='ND_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='ND_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# FIGURE S3

tmp_df = subset(data, party1=='Republican' & use_sample==1)

plot_helper(tmp_df, var='fairb', weights='weightsA')

# Baseline: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed
vectorx = tmp_df$fairb[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: copartisan signaled copartisan believed vs copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel' )]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: non-copartisan signaled non-copartisan believed vs copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: non-copartisan signaled non-copartisan believed vs not disclosed, non-copartisan believed
vectorx = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='ND_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='ND_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]



# FIGURE S4
tmp_df = subset(data, use_sample==1)

plot_helper(tmp_df, var='fairb', weights='weightsB')

# Baseline: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed
vectorx = tmp_df$fairb[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
weightx = tmp_df$weightsB[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsB[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
weightx = tmp_df$weightsB[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsB[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]


# Falsification: copartisan signaled copartisan believed vs copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
weightx = tmp_df$weightsB[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel' )]
weighty = tmp_df$weightsB[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: non-copartisan signaled non-copartisan believed vs copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel' )]
weightx = tmp_df$weightsB[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]
weighty = tmp_df$weightsB[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
weightx = tmp_df$weightsB[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsB[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: non-copartisan signaled non-copartisan believed vs not disclosed, non-copartisan believed
vectorx = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weightx = tmp_df$weightsB[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='ND_sig_non_copart_bel')]
weighty = tmp_df$weightsB[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='ND_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]



# Figure S5

tmp_df = subset(data, use_sample==1)

plot_helper(tmp_df, var='sending', weights='weightsA')

# Baseline: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed
vectorx = tmp_df$sending[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
vectory = tmp_df$sending[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = tmp_df$sending[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
vectory = tmp_df$sending[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]


# Falsification: copartisan signaled copartisan believed vs copartisan signaled non-copartisan believed

vectorx = tmp_df$sending[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
vectory = tmp_df$sending[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel' )]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: non-copartisan signaled non-copartisan believed vs copartisan signaled non-copartisan believed

vectorx = tmp_df$sending[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
vectory = tmp_df$sending[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = tmp_df$sending[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
vectory = tmp_df$sending[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: non-copartisan signaled non-copartisan believed vs not disclosed, non-copartisan believed
vectorx = tmp_df$sending[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
vectory = tmp_df$sending[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='ND_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='ND_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]


# Figure S6 
tmp_df = subset(data, use_sample==1 & flag==0 & doubt==0)

plot_helper(tmp_df, var='fairb', weights='weightsA')

# Baseline: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed
vectorx = tmp_df$fairb[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==1 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]


# Falsification: copartisan signaled copartisan believed vs copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_copart_bel' )]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel' )]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Falsification: non-copartisan signaled non-copartisan believed vs copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel' )]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==3 & tmp_df$belief_type_part=='copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: copartisan signaled copartisan believed vs non-copartisan signaled non-copartisan believed

vectorx = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='copart_sig_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]

# Non-Disclosure: non-copartisan signaled non-copartisan believed vs not disclosed, non-copartisan believed
vectorx = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
weightx = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='non_copart_sig_non_copart_bel')]
vectory = tmp_df$fairb[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='ND_sig_non_copart_bel')]
weighty = tmp_df$weightsA[which(tmp_df$arm_num==2 & tmp_df$belief_type_part=='ND_sig_non_copart_bel')]

wtd.t.test(x = vectorx, y=vectory, weight = weightx, weighty=weighty, samedata = FALSE, alternative = 'greater')$coefficients[['p.value']]



# TABLE S1

table(data$party2[which(data$use_sample==1)], 
      data$arm_num[which(data$use_sample==1)])[party_strength_order, ]

# TABLE S2

table(data$party2[which(data$use_sample==1)], 
      data$party2alter[which(data$use_sample==1)])[party_strength_order, party_strength_order_w_nd]


# TABLE S3

tbl = NULL

for (i in justifications){
  tbl = rbind(tbl, round(prop.table(table(data[which(data$use_sample==1),i], data$arm_num[which(data$use_sample==1)]),2)[2,] * 100, 2))  
}


rownames(tbl) = justifications

print(tbl)

# TABLE S4
data$sex = as.factor(data$sex)
data$race = as.factor(data$race)
data$income = as.factor(data$income)
data$educ = as.factor(data$educ)

all = one_hot(data.table::data.table(data), cols=c('sex', 'race', 
                                                   'income' ,'educ'))

all = as.data.frame(all)

our_vars = c('age', 'sex_Female', 'sex_Non_Female', 'race_White' ,'race_Non_White',
             'educ_HS_less', 'educ_Some_college', 'educ_Bachelors', 'educ_Graduate_professional',
             'income_5000', 'income_15000', 'income_25000', 'income_35000', 'income_45000',
             'income_55000', 'income_65000', 'income_75000', 'income_85000', 'income_95000',
             'income_125000', 'income_150000_more', 'income_Prefer_not_to_say', 'fail_attention',
             'fail_treatment')
  
ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$arm_num, weights = all$commonweight)
tmp = svyCreateTableOne(vars = our_vars,strata = 'arm_num', data = ttt,test=F,smd = F)
tmp = print(tmp, explain = T,test = F, smd = F, noSpaces = T,quote = F, padColnames=T)


# For the tables including CCES data, we will use data_cces

everyone = read.csv('data_w_cces.csv', stringsAsFactors = F)
everyone$gender = as.factor(everyone$gender)
everyone$race = as.factor(everyone$race)
everyone$faminc_new = as.factor(everyone$faminc_new)
everyone$educ = as.factor(everyone$educ)
everyone$age_binned = as.factor(everyone$age_binned)

our_vars = c('age', 'gender_Female', 'gender_Non_Female', 'race_White' ,'race_Non_White',
             'educ_HS_less', 'educ_Some_college', 'educ_Bachelors', 'educ_Graduate_professional',
             'faminc_new_5000', 'faminc_new_15000', 'faminc_new_25000', 'faminc_new_35000', 'faminc_new_45000',
             'faminc_new_55000', 'faminc_new_65000', 'faminc_new_75000', 'faminc_new_90000',
             'faminc_new_125000', 'faminc_new_150000_more', 'faminc_new_Prefer_not_to_say')


# TABLE S5 
all = one_hot(data.table::data.table(subset(everyone, pid3=='Democrat')), cols=c('gender', 'race', 
                                                                                 'faminc_new' ,'educ'))

all = as.data.frame(all)

ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$from_cces, weights = all$commonweight)
tmp = svyCreateTableOne(vars = our_vars,strata = 'from_cces', data = ttt,test=F,smd = T)
tmp = print(tmp, explain = T,test = F, smd = T, noSpaces = T,quote = F, padColnames=T)

# TABLE S6

all = one_hot(data.table::data.table(subset(everyone, pid3=='Republican')), cols=c('gender', 'race', 
                                                                                   'faminc_new' ,'educ'))

all = as.data.frame(all)

ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$from_cces, weights = all$commonweight)
tmp = svyCreateTableOne(vars = our_vars,strata = 'from_cces', data = ttt,test=F,smd = T)
tmp = print(tmp, explain = T,test = F, smd = T, noSpaces = T,quote = F, padColnames=T)



# TABLE S7

tt = subset(data, party1=='Democrat') %>%
  dplyr::group_by(arm_num, signaled_partisanship)%>%
  dplyr::summarise(mean_w = wtd.mean(fairb,weightsA) * 100)

tt
xx = cast(tt, arm_num ~ signaled_partisanship)
xx = xx[c(1,3,2),c(1,2,4,3)]
round(xx,2)


# TABLE S8

tt = subset(data, party1=='Republican') %>%
  dplyr::group_by(arm_num, signaled_partisanship)%>%
  dplyr::summarise(mean_w = wtd.mean(fairb,weightsA) * 100)

tt
xx = cast(tt, arm_num ~ signaled_partisanship)
xx = xx[c(1,3,2),c(1,2,4,3)]
round(xx,2)


# TABLE S9

tt = subset(data, use_sample==1) %>%
  dplyr::group_by(arm_num, signaled_partisanship)%>%
  dplyr::summarise(mean_w = wtd.mean(fairb,weightsB) * 100)

tt
xx = cast(tt, arm_num ~ signaled_partisanship)
xx = xx[c(1,3,2),c(1,2,4,3)]
round(xx,2)



# TABLE S10

our_vars = c('age_binned_18_19', 'age_binned_20_24', 'age_binned_25_29', 'age_binned_30_34',
             'age_binned_35_39', 'age_binned_40_49', 'age_binned_50_59', 'age_binned_above_60',
             'gender_Female', 'gender_Non_Female', 'race_White' ,'race_Non_White',
             'educ_HS_less', 'educ_Some_college', 'educ_Bachelors', 'educ_Graduate_professional',
             'faminc_new_5000', 'faminc_new_15000', 'faminc_new_25000', 'faminc_new_35000', 'faminc_new_45000',
             'faminc_new_55000', 'faminc_new_65000', 'faminc_new_75000', 'faminc_new_90000',
             'faminc_new_125000', 'faminc_new_150000_more', 'faminc_new_Prefer_not_to_say')



all = mltools::one_hot(data.table::data.table(subset(everyone,  pid3=='Democrat')), cols=c('age_binned','gender', 'race', 
                                                                                           'faminc_new' ,'educ'))


all = as.data.frame(all)
ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$from_cces, weights = all$commonweight_B)
tmp = svyCreateTableOne(vars = our_vars,strata = 'from_cces', data = ttt,test=F,smd = TRUE)
print(tmp, explain = T,test = F, smd = T, noSpaces = T,quote = F, padColnames=T)


# TABLE S11


res = subset(data, use_sample==1 & party1=='Democrat') %>%
  dplyr::group_by(weightsB_binned) %>%
  dplyr::summarize(count = length(weightsB_binned),
                   perc_total = round(length(weightsB_binned)/dim(subset(data, use_sample==1 & party1=='Democrat'))[1] * 100,2),
                   perc_sample = round(sum(weightsB)/(dim(subset(data, use_sample==1))[1]/2) * 100, 2))

res = as.data.frame(res)
res


# TABLE S12

all = mltools::one_hot(data.table::data.table(subset(everyone,  pid3=='Republican')), cols=c('age_binned','gender', 'race', 
                                                                                             'faminc_new' ,'educ'))


all = as.data.frame(all)
ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$from_cces, weights = all$commonweight_B)
tmp = svyCreateTableOne(vars = our_vars,strata = 'from_cces', data = ttt,test=F,smd = TRUE)
print(tmp, explain = T,test = F, smd = T, noSpaces = T,quote = F, padColnames=T)


# TABLE S13
res = subset(data, use_sample==1 & party1=='Republican') %>%
  dplyr::group_by(weightsB_binned) %>%
  dplyr::summarize(count = length(weightsB_binned),
                   perc_total = round(length(weightsB_binned)/dim(subset(data, use_sample==1 & party1=='Republican'))[1] * 100,2),
                   perc_sample = round(sum(weightsB)/(dim(subset(data, use_sample==1))[1]/2) * 100, 2))

res = as.data.frame(res)
res


# TABLE S14

tt = subset(data, use_sample==1) %>%
  dplyr::group_by(arm_num, signaled_partisanship)%>%
  dplyr::summarise(mean_w = wtd.mean(sending,weightsA))

tt
xx = cast(tt, arm_num ~ signaled_partisanship)
xx = xx[c(1,3,2),c(1,2,4,3)]
round(xx,2)


# TABLE S15

tt = subset(data, flag==0 & doubt==0) %>%
  dplyr::group_by(arm_num, signaled_partisanship)%>%
  dplyr::summarise(mean_w = wtd.mean(fairb,weightsA)*100)

tt
xx = cast(tt, arm_num ~ signaled_partisanship)
xx = xx[c(1,3,2),c(1,2,4,3)]
round(xx,2)



# TABLE S16
data$genderalter = as.factor(data$genderalter)
data$racealter = as.factor(data$racealter)
data$educalter = as.factor(data$educalter)

our_vars = c('genderalter_Female', 'genderalter_Non_Female',
             'racealter_White', 'racealter_Non_White',
             'educalter_HS_less', 'educalter_Some_college', 'educalter_Bachelors','educalter_Graduate_professional')

all = one_hot(data.table::data.table(subset(data, use_sample==1 & memory2_binary=='Democrat' & arm_num < 3), cols=c('genderalter', 'racealter', 'educalter')))

all = as.data.frame(all)

ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$arm_num, weights = rep(1, dim(all)[1]))
tmp = svyCreateTableOne(vars = our_vars,strata = 'arm_num', data = ttt,test=F,smd = T)
tmp = print(tmp, explain = T,test = F, smd = T, noSpaces = T,quote = F, padColnames=T)


# TABLE S17
all = one_hot(data.table::data.table(subset(data, use_sample==1 & memory2_binary=='Democrat' & arm_num %in% c(1, 3)), cols=c('genderalter', 'racealter', 'educalter')))

all = as.data.frame(all)

ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$arm_num, weights = rep(1, dim(all)[1]))
tmp = svyCreateTableOne(vars = our_vars,strata = 'arm_num', data = ttt,test=F,smd = T)
tmp = print(tmp, explain = T,test = F, smd = T, noSpaces = T,quote = F, padColnames=T)


# TABLE S18


all = one_hot(data.table::data.table(subset(data, use_sample==1 & memory2_binary=='Democrat' & arm_num %in% c(2, 3)), cols=c('genderalter', 'racealter', 'educalter')))

all = as.data.frame(all)

ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$arm_num, weights = rep(1, dim(all)[1]))
tmp = svyCreateTableOne(vars = our_vars,strata = 'arm_num', data = ttt,test=F,smd = T)
tmp = print(tmp, explain = T,test = F, smd = T, noSpaces = T,quote = F, padColnames=T)



# TABLE S19

all = one_hot(data.table::data.table(subset(data, use_sample==1 & memory2_binary=='Republican' & arm_num < 3), cols=c('genderalter', 'racealter', 'educalter')))

all = as.data.frame(all)

ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$arm_num, weights = rep(1, dim(all)[1]))
tmp = svyCreateTableOne(vars = our_vars,strata = 'arm_num', data = ttt,test=F,smd = T)
tmp = print(tmp, explain = T,test = F, smd = T, noSpaces = T,quote = F, padColnames=T)


# TABLE S20

all = one_hot(data.table::data.table(subset(data, use_sample==1 & memory2_binary=='Republican' & arm_num %in% c(1, 3)), cols=c('genderalter', 'racealter', 'educalter')))

all = as.data.frame(all)

ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$arm_num, weights = rep(1, dim(all)[1]))
tmp = svyCreateTableOne(vars = our_vars,strata = 'arm_num', data = ttt,test=F,smd = T)
tmp = print(tmp, explain = T,test = F, smd = T, noSpaces = T,quote = F, padColnames=T)

# TABLE S21

all = one_hot(data.table::data.table(subset(data, use_sample==1 & memory2_binary=='Republican' & arm_num %in% c(2, 3)), cols=c('genderalter', 'racealter', 'educalter')))

all = as.data.frame(all)

ttt = svydesign(data= all, id = seq(1:dim(all)[1]),strata = all$arm_num, weights = rep(1, dim(all)[1]))
tmp = svyCreateTableOne(vars = our_vars,strata = 'arm_num', data = ttt,test=F,smd = T)
tmp = print(tmp, explain = T,test = F, smd = T, noSpaces = T,quote = F, padColnames=T)


# TABLE S22
tt = subset(data, use_sample==1) %>%
  dplyr::group_by(arm_num, signaled_partisanship)%>%
  dplyr::summarise(mean_w = wtd.mean(fairb, weightsA) * 100)
xx = cast(tt, arm_num ~ signaled_partisanship)
xx = xx[c(1,3,2),c(1,2,4,3)]
round(xx,2)


# TABLE S23
subdata = subset(data, arm_num==3 & belief_type_part %in% c('copart_sig_copart_bel', 'copart_sig_non_copart_bel') & use_sample==1)

dem_strength = lm(belief~strength, weights = weights, data = subset(subdata, party1=='Democrat'))
rep_strength = lm(belief~strength, weights = weights, data = subset(subdata, party1=='Republican'))
pooled_strength = lm(belief~strength, weights = weightsp, data = subdata)

stargazer(dem_strength, rep_strength, pooled_strength, title="Results", align=TRUE,
          column.labels = c("Democrat", "Republican", 'Pooled'), object.names =FALSE, type = 'text', 
          star.cutoffs = c(.05, .01, .001))

# TABLE S24

dem_match = lm(belief~match, weights = weights, data = subset(subdata, party1=='Democrat'))
rep_match = lm(belief~match, weights = weights, data = subset(subdata, party1=='Republican'))
pooled_match = lm(belief~match, weights = weightsp, data = subdata)

stargazer(dem_match, rep_match, pooled_match, title="Results", align=TRUE,
          column.labels = c("Democrat", "Republican", 'Pooled'), object.names =FALSE, type = 'text', 
          star.cutoffs = c(.05, .01, .001))

# TABLE S25

dem_distance = lm(belief~distance, weights = weights, data = subset(subdata, party1=='Democrat'))
rep_distance = lm(belief~distance, weights = weights, data = subset(subdata, party1=='Republican'))
pooled_distance = lm(belief~distance, weights = weightsp, data = subdata)

stargazer(dem_distance, rep_distance, pooled_distance, title="Results", align=TRUE,
          column.labels = c("Democrat", "Republican", 'Pooled'), object.names =FALSE, type = 'text', 
          star.cutoffs = c(.05, .01, .001))

# TABLE S26

tt = subset(data, use_sample==1) %>%
  dplyr::group_by(arm_num, signaled_partisanship)%>%
  dplyr::summarise(mean_w = mean(fairb) * 100)
xx = cast(tt, arm_num ~ signaled_partisanship)
xx = xx[c(1,3,2),c(1,2,4,3)]
round(xx,2)


# TABLE S27
# This analysis is a robustness check on all respondents,
# including those who have “not taken up treatment” as defined 
# in the Methods section of the paper -> therefore weightsA_all are used


tt = subset(data, attention_check==1) %>%
  dplyr::group_by(arm_num, signaled_partisanship)%>%
  dplyr::summarise(mean_w = wtd.mean(fairb, weightsA_all) * 100)
xx = cast(tt, arm_num ~ signaled_partisanship)
xx = xx[c(1,3,2),c(1,2,4,3)]
round(xx,2)


# TABLE S28

m <- lm(fairb ~ non_copartisan + myideology + relevel(as.factor(age_binned), ref = "18_19") + sex + 
          relevel(as.factor(race), ref = "White") + relevel(as.factor(educ), ref = "HS_less") + 
          relevel(as.factor(income), ref = "5000"), weights = weightsA, data = subset(data, arm_num==1 & use_sample == 1))
m2 <- lm(fairb ~ non_copartisan + myideology + relevel(as.factor(age_binned), ref = "18_19") + sex + 
           relevel(as.factor(race), ref = "White") + relevel(as.factor(educ), ref = "HS_less") + 
          relevel(as.factor(income), ref = "5000"), weights = weightsA, data = subset(data, arm_num==3 & use_sample == 1))
stargazer(m, m2, title = "Results", align = TRUE, type = 'text', 
          star.cutoffs = c(.05, .01, .001))

