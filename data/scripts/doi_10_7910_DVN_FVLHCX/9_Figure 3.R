#############################
# Load packages
#############################
library(tidyverse)
library(lme4)
library(arm)

#############################
# Load and clean data
#############################
rm(list=setdiff(ls(), c('script', 'scripts', 'log_file')))

# Load candidate data
election <- readRDS('model_df.rds')

# Keep only elections 2010 and beyond 
election <- subset(election, year >= 2010)

# Remove top-two and louisiana-style primaires 
election <- subset(election, election$primary_type != "Top-Two" & 
                     election$primary_type != "Louisiana-style Primary")

#############################
# Estimate models, prepare for stacking
#############################

# Re-scale fundraising amounts 
election$first_90_model <- rescale(election$fundraising_amount_first)
election$filing_90_model <- rescale(election$fundraising_amount_registration_90) 
election$FEC_90_model <- rescale(election$fundraising_amount_quarter_90)
election$election_9_model <- rescale(election$fundraising_amount_nine_months_preelection)
election$q4_model <- rescale(election$fundraising_amount_twelve_months_prior)

# Run first itemized model
mr1 <- glmer(winwin ~ first_90_model + cand_party + seat_safety + 
               quality_total + primary_type + as.factor(female) + quality_cand + 
               as.factor(year) + (1|raceid), family = 'binomial',  
             data = election) 

# Run filing model
mr2 <- glmer(winwin ~ filing_90_model + cand_party + seat_safety + 
               quality_total + primary_type + as.factor(female) + quality_cand + 
               as.factor(year) + (1|raceid), family = 'binomial',  
             data = election) 

# Run FEC quarterly model
mr3 <- glmer(winwin ~ FEC_90_model + cand_party + seat_safety + quality_total + 
               primary_type + as.factor(female) + quality_cand + 
               as.factor(year) + (1|raceid), family = 'binomial',  
             data = election) 

# Run nine month model
mr4 <- glmer(winwin ~ election_9_model + cand_party + seat_safety + 
               quality_total + primary_type + as.factor(female) + quality_cand + 
               as.factor(year) + (1|raceid), family = 'binomial',  
             data = election) 

# Run twelve month model
mr5 <- glmer(winwin ~ q4_model + cand_party + seat_safety + quality_total + 
               primary_type + as.factor(female) + quality_cand + 
               as.factor(year) + (1|raceid), family = 'binomial', 
             data = election)

# Create model matrix with outcome variable
mat1 <- data.frame(cbind(mr1@frame[['winwin']], model.matrix(mr1)[,-1], 
                         mr1@frame[['raceid']]))
mat2 <- data.frame(cbind(mr2@frame[['winwin']], model.matrix(mr2)[,-1], 
                         mr2@frame[['raceid']]))
mat3 <- data.frame(cbind(mr3@frame[['winwin']], model.matrix(mr3)[,-1], 
                         mr3@frame[['raceid']]))
mat4 <- data.frame(cbind(mr4@frame[['winwin']], model.matrix(mr4)[,-1], 
                         mr4@frame[['raceid']]))
mat5 <- data.frame(cbind(mr5@frame[['winwin']], model.matrix(mr5)[,-1], 
                         mr5@frame[['raceid']]))

# Relabel column as "raceid" variable
colnames(mat1)[16] <- 'raceid'
colnames(mat2)[16] <- 'raceid'
colnames(mat3)[16] <- 'raceid'
colnames(mat4)[16] <- 'raceid'
colnames(mat5)[16] <- 'raceid'

# Relabel column additional columns
colnames(mat1) <- c('winwin', paste(colnames(mat1)[-1], 'm1', sep = '_'))
colnames(mat2) <- c('winwin', paste(colnames(mat2)[-1], 'm2', sep = '_'))
colnames(mat3) <- c('winwin', paste(colnames(mat3)[-1], 'm3', sep = '_'))
colnames(mat4) <- c('winwin', paste(colnames(mat4)[-1], 'm4', sep = '_'))
colnames(mat5) <- c('winwin', paste(colnames(mat5)[-1], 'm5', sep = '_'))

# Create empty dataframe for each matrix 
empty1 <- setNames(data.frame(matrix(ncol = ncol(mat1), nrow = nrow(mat1))), 
                   colnames(mat1)[-1])
empty1[is.na(empty1)] <- 0

empty2 <- setNames(data.frame(matrix(ncol = ncol(mat1), nrow = nrow(mat1))), 
                   colnames(mat2)[-1])
empty2[is.na(empty2)] <- 0

empty3 <- setNames(data.frame(matrix(ncol = ncol(mat1), nrow = nrow(mat1))), 
                   colnames(mat3)[-1])
empty3[is.na(empty3)] <- 0

empty4 <- setNames(data.frame(matrix(ncol = ncol(mat1), nrow = nrow(mat1))), 
                   colnames(mat4)[-1])
empty4[is.na(empty4)] <- 0

empty5 <- setNames(data.frame(matrix(ncol = ncol(mat1), nrow = nrow(mat1))), 
                   colnames(mat5)[-1])
empty5[is.na(empty5)] <- 0

# Stack mat1 and mat2
stack1 <- cbind(mat1, empty2)
stack2 <- cbind(empty1, mat2)
stack1$model <- 1
stack2$model <- 0

# Bind stacked models 
stacked1_2 <- rbind(stack1, stack2)

# Stack mat1 and mat3
stack1 <- cbind(mat1, empty3)
stack3 <- cbind(empty1, mat3)
stack1$model <- 1
stack3$model <- 0

# Bind stacked models 
stacked1_3 <- rbind(stack1, stack3)

# Stack mat1 and mat4 
stack1 <- cbind(mat1, empty4)
stack4 <- cbind(empty1, mat4)
stack1$model <- 1
stack4$model <- 0

# Bind stacked models 
stacked1_4 <- rbind(stack1, stack4)

# Stack mat1 and mat5 
stack1 <- cbind(mat1, empty5)
stack5 <- cbind(empty1, mat5)
stack1$model <- 1
stack5$model <- 0

# Bind stacked models
stacked1_5 <- rbind(stack1, stack5)

# Stack mat2 and mat3
stack2 <- cbind(mat2, empty3)
stack3 <- cbind(empty2, mat3)
stack2$model <- 1
stack3$model <- 0

# Bind stacked models 
stacked2_3 <- rbind(stack2, stack3)

# Stack mat2 and mat4
stack2 <- cbind(mat2, empty4)
stack4 <- cbind(empty2, mat4)
stack2$model <- 1
stack4$model <- 0

# Bind stacked models 
stacked2_4 <- rbind(stack2, stack4)

# Stack mat2 and mat5
stack2 <- cbind(mat2, empty5)
stack5 <- cbind(empty2, mat5)
stack2$model <- 1
stack5$model <- 0

# Bind stacked models 
stacked2_5 <- rbind(stack2, stack5)

# Stack mat3 and mat4
stack3 <- cbind(mat3, empty4)
stack4 <- cbind(empty3, mat4)
stack3$model <- 1
stack4$model <- 0

# Bind stacked models
stacked3_4 <- rbind(stack3, stack4)

# Stack mat3 and mat5
stack3 <- cbind(mat3, empty5)
stack5 <- cbind(empty3, mat5)
stack3$model <- 1
stack5$model <- 0

# Bind stacked models
stacked3_5 <- rbind(stack3, stack5)

# Stack mat4 and mat5
stack4 <- cbind(mat4, empty5)
stack5 <- cbind(empty4, mat5)
stack4$model <- 1
stack5$model <- 0

# Bind stacked models
stacked4_5 <- rbind(stack4, stack5)

#############################
# Estimate stacked models 
#############################

# Estimate m1 v m2
m.stacked.1.2 <- glmer(winwin ~ first_90_model_m1 + 
                         filing_90_model_m2 + 
                         cand_partyRepublican_m1 + 
                         seat_safetySame.Party_m1 + 
                         seat_safetyCompetitive_m1 + 
                         quality_total_m1 + 
                         primary_typeOpen.Primary_m1 + 
                         primary_typePartially.Closed_m1 + 
                         as.factor.female.1_m1 + 
                         quality_cand_m1 +  
                         as.factor.year.2012_m1 + as.factor.year.2014_m1 + 
                         as.factor.year.2016_m1 + as.factor.year.2018_m1 + 
                         as.factor.year.2020_m1 + (1|raceid_m1) +
                         cand_partyRepublican_m2 + 
                         seat_safetySame.Party_m2 + 
                         seat_safetyCompetitive_m2 + 
                         quality_total_m2 + 
                         primary_typeOpen.Primary_m2 + 
                         primary_typePartially.Closed_m2 + 
                         as.factor.female.1_m2 + 
                         quality_cand_m2 +  
                         as.factor.year.2012_m2 + as.factor.year.2014_m2 + 
                         as.factor.year.2016_m2 + as.factor.year.2018_m2 + 
                         as.factor.year.2020_m2 + (1|raceid_m2) + model, 
                       family = 'binomial',  data = stacked1_2)

# Print stacked model 
summary(m.stacked.1.2)

# Estimate m1 v m3
m.stacked.1.3 <- glmer(winwin ~ first_90_model_m1 + 
                         FEC_90_model_m3 + 
                         cand_partyRepublican_m1 + 
                         seat_safetySame.Party_m1 + 
                         seat_safetyCompetitive_m1 + 
                         quality_total_m1 + 
                         primary_typeOpen.Primary_m1 + 
                         primary_typePartially.Closed_m1 + 
                         as.factor.female.1_m1 + 
                         quality_cand_m1 +  
                         as.factor.year.2012_m1 + as.factor.year.2014_m1 + 
                         as.factor.year.2016_m1 + as.factor.year.2018_m1 + 
                         as.factor.year.2020_m1 + (1|raceid_m1) +
                         cand_partyRepublican_m3 + 
                         seat_safetySame.Party_m3 + 
                         seat_safetyCompetitive_m3 + 
                         quality_total_m3 + 
                         primary_typeOpen.Primary_m3 + 
                         primary_typePartially.Closed_m3 + 
                         as.factor.female.1_m3 + 
                         quality_cand_m3 +  
                         as.factor.year.2012_m3 + as.factor.year.2014_m3 + 
                         as.factor.year.2016_m3 + as.factor.year.2018_m3 + 
                         as.factor.year.2020_m3 + (1|raceid_m3) + model, 
                       family = 'binomial',  data = stacked1_3)

# Print stacked model
summary(m.stacked.1.3)

# Estimate m1 v m4
m.stacked.1.4 <- glmer(winwin ~ first_90_model_m1 + 
                         election_9_model_m4 + 
                         cand_partyRepublican_m1 + 
                         seat_safetySame.Party_m1 + 
                         seat_safetyCompetitive_m1 + 
                         quality_total_m1 + 
                         primary_typeOpen.Primary_m1 + 
                         primary_typePartially.Closed_m1 + 
                         as.factor.female.1_m1 + 
                         quality_cand_m1 +  
                         as.factor.year.2012_m1 + as.factor.year.2014_m1 + 
                         as.factor.year.2016_m1 + as.factor.year.2018_m1 + 
                         as.factor.year.2020_m1 + (1|raceid_m1) +
                         cand_partyRepublican_m4 + 
                         seat_safetySame.Party_m4 + 
                         seat_safetyCompetitive_m4 + 
                         quality_total_m4 + 
                         primary_typeOpen.Primary_m4 + 
                         primary_typePartially.Closed_m4 + 
                         as.factor.female.1_m4 + 
                         quality_cand_m4 +  
                         as.factor.year.2012_m4 + as.factor.year.2014_m4 + 
                         as.factor.year.2016_m4 + as.factor.year.2018_m4 + 
                         as.factor.year.2020_m4 + (1|raceid_m4) + model, 
                       family = 'binomial',  data = stacked1_4)

# Print stacked model
summary(m.stacked.1.4)

# Estimate m1 v m5
m.stacked.1.5 <- glmer(winwin ~ first_90_model_m1 + 
                         q4_model_m5 + 
                         cand_partyRepublican_m1 + 
                         seat_safetySame.Party_m1 + 
                         seat_safetyCompetitive_m1 + 
                         quality_total_m1 + 
                         primary_typeOpen.Primary_m1 + 
                         primary_typePartially.Closed_m1 + 
                         as.factor.female.1_m1 + 
                         quality_cand_m1 +  
                         as.factor.year.2012_m1 + as.factor.year.2014_m1 + 
                         as.factor.year.2016_m1 + as.factor.year.2018_m1 + 
                         as.factor.year.2020_m1 + (1|raceid_m1) +
                         cand_partyRepublican_m5 + 
                         seat_safetySame.Party_m5 + 
                         seat_safetyCompetitive_m5 + 
                         quality_total_m5 + 
                         primary_typeOpen.Primary_m5 + 
                         primary_typePartially.Closed_m5 + 
                         as.factor.female.1_m5 + 
                         quality_cand_m5 +  
                         as.factor.year.2012_m5 + as.factor.year.2014_m5 + 
                         as.factor.year.2016_m5 +  as.factor.year.2018_m5 + 
                         as.factor.year.2020_m5 + (1|raceid_m5) + model, 
                       family = 'binomial',  data = stacked1_5)

# Print stacked model
summary(m.stacked.1.5)

# Estimate m2 v m3
m.stacked.2.3 <- glmer(winwin ~ filing_90_model_m2 + 
                         FEC_90_model_m3 + 
                         cand_partyRepublican_m2 + 
                         seat_safetySame.Party_m2 + 
                         seat_safetyCompetitive_m2 + 
                         quality_total_m2 + 
                         primary_typeOpen.Primary_m2 + 
                         primary_typePartially.Closed_m2 + 
                         as.factor.female.1_m2 + 
                         quality_cand_m2 +  
                         as.factor.year.2012_m2 + as.factor.year.2014_m2 + 
                         as.factor.year.2016_m2 + as.factor.year.2018_m2 + 
                         as.factor.year.2020_m2 + (1|raceid_m2) +
                         cand_partyRepublican_m3 + 
                         seat_safetySame.Party_m3 + 
                         seat_safetyCompetitive_m3 + 
                         quality_total_m3 + 
                         primary_typeOpen.Primary_m3 + 
                         primary_typePartially.Closed_m3 + 
                         as.factor.female.1_m3 + 
                         quality_cand_m3 +  
                         as.factor.year.2012_m3 + as.factor.year.2014_m3 + 
                         as.factor.year.2016_m3 + as.factor.year.2018_m3 + 
                         as.factor.year.2020_m3 + (1|raceid_m3) + model, 
                       family = 'binomial',  data = stacked2_3)

# Print stacked model
summary(m.stacked.2.3)

# Estimate m2 v m4
m.stacked.2.4 <- glmer(winwin ~ filing_90_model_m2 + 
                         election_9_model_m4 + 
                         cand_partyRepublican_m2 + 
                         seat_safetySame.Party_m2 + 
                         seat_safetyCompetitive_m2 + 
                         quality_total_m2 + 
                         primary_typeOpen.Primary_m2 + 
                         primary_typePartially.Closed_m2 + 
                         as.factor.female.1_m2 + 
                         quality_cand_m2 +  
                         as.factor.year.2012_m2 + as.factor.year.2014_m2 + 
                         as.factor.year.2016_m2 + as.factor.year.2018_m2 + 
                         as.factor.year.2020_m2 + (1|raceid_m2) +
                         cand_partyRepublican_m4 + 
                         seat_safetySame.Party_m4 + 
                         seat_safetyCompetitive_m4 + 
                         quality_total_m4 + 
                         primary_typeOpen.Primary_m4 + 
                         primary_typePartially.Closed_m4 + 
                         as.factor.female.1_m4 + 
                         quality_cand_m4 +  
                         as.factor.year.2012_m4 + as.factor.year.2014_m4 + 
                         as.factor.year.2016_m4 + as.factor.year.2018_m4 + 
                         as.factor.year.2020_m4 + (1|raceid_m4) + model, 
                       family = 'binomial',  data = stacked2_4)

# Print stacked models
summary(m.stacked.2.4)

# Estimate m2 v m5
m.stacked.2.5 <- glmer(winwin ~ filing_90_model_m2 + 
                         q4_model_m5 + 
                         cand_partyRepublican_m2 + 
                         seat_safetySame.Party_m2 + 
                         seat_safetyCompetitive_m2 + 
                         quality_total_m2 + 
                         primary_typeOpen.Primary_m2 + 
                         primary_typePartially.Closed_m2 + 
                         as.factor.female.1_m2 + 
                         quality_cand_m2 +  
                         as.factor.year.2012_m2 + as.factor.year.2014_m2 + 
                         as.factor.year.2016_m2 + as.factor.year.2018_m2 + 
                         as.factor.year.2020_m2 + (1|raceid_m2) +
                         cand_partyRepublican_m5 + 
                         seat_safetySame.Party_m5 + 
                         seat_safetyCompetitive_m5 + 
                         quality_total_m5 + 
                         primary_typeOpen.Primary_m5 + 
                         primary_typePartially.Closed_m5 + 
                         as.factor.female.1_m5 + 
                         quality_cand_m5 +  
                         as.factor.year.2012_m5 + as.factor.year.2014_m5 + 
                         as.factor.year.2016_m5 + as.factor.year.2018_m5 + 
                         as.factor.year.2020_m5 + (1|raceid_m5) + model, 
                       family = 'binomial',  data = stacked2_5)

# Print stacked models
summary(m.stacked.2.5)

# Estimate m3 v m4
m.stacked.3.4 <- glmer(winwin ~ FEC_90_model_m3 + 
                         election_9_model_m4 + 
                         cand_partyRepublican_m3 + 
                         seat_safetySame.Party_m3 + 
                         seat_safetyCompetitive_m3 + 
                         quality_total_m3 + 
                         primary_typeOpen.Primary_m3 + 
                         primary_typePartially.Closed_m3 + 
                         as.factor.female.1_m3 + 
                         quality_cand_m3 +  
                         as.factor.year.2012_m3 + as.factor.year.2014_m3 + 
                         as.factor.year.2016_m3 + as.factor.year.2018_m3 + 
                         as.factor.year.2020_m3 + (1|raceid_m3) +
                         cand_partyRepublican_m4 + 
                         seat_safetySame.Party_m4 + 
                         seat_safetyCompetitive_m4 + 
                         quality_total_m4 + 
                         primary_typeOpen.Primary_m4 + 
                         primary_typePartially.Closed_m4 + 
                         as.factor.female.1_m4 + 
                         quality_cand_m4 +  
                         as.factor.year.2012_m4 + as.factor.year.2014_m4 + 
                         as.factor.year.2016_m4 + as.factor.year.2018_m4 + 
                         as.factor.year.2020_m4 + (1|raceid_m4) + model, 
                       family = 'binomial',  data = stacked3_4)

# Print stacked model
summary(m.stacked.3.4)

# Estimate m3 v m5
m.stacked.3.5 <- glmer(winwin ~ FEC_90_model_m3 + 
                         q4_model_m5 + 
                         cand_partyRepublican_m3 + 
                         seat_safetySame.Party_m3 + 
                         seat_safetyCompetitive_m3 + 
                         quality_total_m3 + 
                         primary_typeOpen.Primary_m3 + 
                         primary_typePartially.Closed_m3 + 
                         as.factor.female.1_m3 + 
                         quality_cand_m3 +  
                         as.factor.year.2012_m3 + as.factor.year.2014_m3 + 
                         as.factor.year.2016_m3 + as.factor.year.2018_m3 + 
                         as.factor.year.2020_m3 + (1|raceid_m3) +
                         cand_partyRepublican_m5 + 
                         seat_safetySame.Party_m5 + 
                         seat_safetyCompetitive_m5 + 
                         quality_total_m5 + 
                         primary_typeOpen.Primary_m5 + 
                         primary_typePartially.Closed_m5 + 
                         as.factor.female.1_m5 + 
                         quality_cand_m5 +  
                         as.factor.year.2012_m5 + as.factor.year.2014_m5 + 
                         as.factor.year.2016_m5 + as.factor.year.2018_m5 + 
                         as.factor.year.2020_m5 + (1|raceid_m5) + model, 
                       family = 'binomial',  data = stacked3_5)

# Print stacked model
summary(m.stacked.3.5)

# Estimate m4 v m5
m.stacked.4.5 <- glmer(winwin ~ election_9_model_m4 + 
                         q4_model_m5 + 
                         cand_partyRepublican_m4 + 
                         seat_safetySame.Party_m4 + 
                         seat_safetyCompetitive_m4 + 
                         quality_total_m4 + 
                         primary_typeOpen.Primary_m4 + 
                         primary_typePartially.Closed_m4 + 
                         as.factor.female.1_m4 + 
                         quality_cand_m4 +  
                         as.factor.year.2012_m4 + as.factor.year.2014_m4 + 
                         as.factor.year.2016_m4 + as.factor.year.2018_m4 + 
                         as.factor.year.2020_m4 + (1|raceid_m4) +
                         cand_partyRepublican_m5 + 
                         seat_safetySame.Party_m5 + 
                         seat_safetyCompetitive_m5 + 
                         quality_total_m5 + 
                         primary_typeOpen.Primary_m5 + 
                         primary_typePartially.Closed_m5 + 
                         as.factor.female.1_m5 + 
                         quality_cand_m5 +  
                         as.factor.year.2012_m5 + as.factor.year.2014_m5 + 
                         as.factor.year.2016_m5 + as.factor.year.2018_m5 + 
                         as.factor.year.2020_m5 + (1|raceid_m5) + model, 
                       family = 'binomial',  data = stacked4_5)

# Print stacked model
summary(m.stacked.4.5)

#############################
# Hypothesis testing between coefficients for the effect of early money
#############################

# Writing function to compare models 
difftest_lm <- function(x1, x2, model){
  diffest <- fixef(model)[x1]- fixef(model)[x2]
  vardiff <- (vcov(model)[x1, x1] + vcov(model)[x2, x2] - 2*(vcov(model)[x1, x2]))
  diffse <- sqrt(vardiff)
  tdiff <- (diffest)/(diffse)
  ptdiff <- 2*(1-pt(abs(tdiff), 5220, lower.tail=T))
  upr <- diffest + 1.96*diffse # will usually be very close to 1.96
  lwr <- diffest - 1.96*diffse
  return(data.frame(est=round(diffest, digits =2), 
              t=round(tdiff, digits = 2), 
              p=round(ptdiff, digits = 4), 
              lwr=round(lwr, digits = 2), 
              upr=round(upr, digits = 2)))
}

# Defining initial models for loop
vector <- c('first_90_model_m1', 'filing_90_model_m2', 'FEC_90_model_m3',
            'election_9_model_m4', 'q4_model_m5')

# Defining stacked models for loop
models <- c(NA, 'm.stacked.1.2', 'm.stacked.1.3', 'm.stacked.1.4', 'm.stacked.1.5',
            'm.stacked.1.2', NA, 'm.stacked.2.3', 'm.stacked.2.4', 'm.stacked.2.5',
            'm.stacked.1.3', 'm.stacked.2.3', NA, 'm.stacked.3.4', 'm.stacked.3.5',
            'm.stacked.1.4', 'm.stacked.2.4', 'm.stacked.3.4', NA, 'm.stacked.4.5',
            'm.stacked.1.5', 'm.stacked.2.5', 'm.stacked.3.5', 'm.stacked.4.5', NA)

# Creating empty matrix for loop
models.mat <- matrix(models, nrow = 5, ncol = 5, byrow = TRUE)

# Create empty dataframe for loop output 
plot.data <- data.frame(matrix(ncol = 8, nrow = 20))

# Rename columns 
colnames(plot.data) <- c('difference', 'tstat', 'pvalue', 'lb', 'ub',
                         'variable1', 'variable2', 'model')

# Set k
k <- 1

# Loop for testing difference between coefficients 
for (i in 1:5){
  for(j in 1:5){
    if(vector[i] == vector[j]) next
    temp <- difftest_lm(vector[i], vector[j], get(models.mat[i,j]))
    names <- cbind(vector[i], vector[j], models.mat[i,j])
    joint <- cbind(temp, names)
    plot.data[k,] <- joint
    k <- k+1
  }
}

# Remove nine month models from plotting data 
plot.data <- subset(plot.data, variable1 != 'election_9_model_m4' & 
                      variable2 != 'election_9_model_m4')


# Re-order x-axis
plot.data$variable1 <- factor(plot.data$variable1, 
                              levels = c('q4_model_m5', 'filing_90_model_m2',
                                         'FEC_90_model_m3', 'first_90_model_m1'))

plot.data$variable2 <- factor(plot.data$variable2, 
                              levels = c('q4_model_m5', 'filing_90_model_m2',
                                         'FEC_90_model_m3', 'first_90_model_m1'))

# Produce plot for hypothesis testing differences 
coefficients.openrace <- ggplot(plot.data, aes(x = variable1, 
                                               y=difference, 
                                               shape = variable2)) + 
  geom_pointrange(aes(ymin=lb, ymax=ub), 
                  size = 2, 
                  position = position_dodge(width = .4)) +
  theme_classic() +
  #facet_wrap(variable2 ~ ., ncol = 1) +
  geom_hline(yintercept = 0,  
             color = 'black', 
             linetype = 'solid', 
             size = .1) +
  labs(y = 'Difference in Effect Size\n', x = '\nEarly Money Measure', 
       shape = 'Early Money Measure') +
  scale_x_discrete(labels = c('Election-Centered\n12 Months', 
                              'Candidate-Centered\nCampaign Registration',
                              'Candidate-Centered\nFEC First Report', 
                              'Candidate-Centered\nFirst Itemized')) +
  theme(axis.text.x =element_text(size  = 18),
        axis.text.y = element_text(size = 18), 
        axis.title = element_text(size=25),
        legend.text=element_text(size=15),
        legend.title = element_text(size = 19),
        legend.direction = 'horizontal', 
        legend.position = 'bottom',
        legend.key.size = unit(1.5, 'lines')) +
  scale_shape_manual(labels = c('Election-Centered\n12 Months', 
                                'Candidate-Centered\nFirst Itemized',
                                'Candidate-Centered\nFEC First Report', 
                                'Candidate-Centered\nCampaign Registration'),
                     values = c(0, 16, 15, 17)) 

# Print hypothesis testing plot
coefficients.openrace

ggsave('Figure3.png', coefficients.openrace, width = 14, height = 10)

