## Author: Kabir Khanna and Jacob Brown
rm(list=ls())

# 2020 county-level data
df <- read.csv("data/counties_analysis.csv"); head(df); dim(df)

# demographic data
acs10 = read.csv('data/ACS_county_2010.csv')
acs14 = read.csv('data/ACS_county_2014.csv')
acs18 = read.csv('data/ACS_county_2018.csv')

names(acs10) = c('id', paste0(names(acs10)[2:length(names(acs10))],'_2010'))
names(acs14) = c('id', paste0(names(acs14)[2:length(names(acs14))],'_2014'))
names(acs18) = c('id', paste0(names(acs18)[2:length(names(acs18))],'_2018'))

df$id = as.numeric(df$fips)

df = merge(df,acs10,by='id',all.x=T)
df = merge(df,acs14,by='id',all.x=T)
df = merge(df,acs18,by='id',all.x=T)

head(df); dim(df)




# Compute ratios and regression by state
states <- unique(df$state)
states <- states
states <- states[order(states)]; print(states); length(states)
reg20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 3)); rownames(reg20.fit) <- states; names(reg20.fit) <- c("intercept", "slope", "rmse")
reg12.fit <- reg16.fit <- reg20.fit

reg.state.pred16 <- reg.out.rmse16 <- reg.out.bias16 <- rep(NA, length(states))

reg.state.pred20 <- reg.out.rmse20 <- reg.out.bias20 <- rep(NA, length(states))

 reg.pred16 <- rep(NA, nrow(df))
 reg.pred20 <- rep(NA, nrow(df))
 
 n.units.state.train.12 <- rep(NA, length(states))
 n.units.state.test.16 <- rep(NA, length(states))
 n.units.state.train.16 <- rep(NA, length(states))
 n.units.state.test.20 <- rep(NA, length(states))

 for (state in states) {
if(state!='AK'){
         if (sum(df$state == state & !is.na(df$reg12) & !is.na(df$total12)) > 1) {
             #l = ( !is.na(df$total12) & !is.na(df$pct_married_2010)& !is.na( df$pct_college_or_higher_2010)& !is.na( df$median_hh_income_2010)& !is.na( df$pct_white_2010)& !is.na( df$pct_black_2010)& !is.na( df$pct_hispanic_2010)& !is.na( df$pct_age_15_24_2010)& !is.na( df$pct_age_25_34_2010)& !is.na( df$pct_age_35_64_2010)& !is.na( df$pct_age_65_up_2010))

             fit12.temp <- lm(total12 ~ pct_married_2010 + pct_college_or_higher_2010 + median_hh_income_2010 + pct_white_2010 + pct_black_2010 + pct_hispanic_2010 + pct_age_15_24_2010 + pct_age_25_34_2010 + pct_age_35_64_2010 + pct_age_65_up_2010 + tot_pop_2010 + reg12, df[df$state == state , ], weights = reg12)
             n.units.state.train.12[states == state] <- nrow(fit12.temp$model)
             
              reg12.fit$intercept[states == state] <- coef(fit12.temp)[1]
             reg12.fit$intercept.se[states == state] <- summary(fit12.temp)$coefficients['(Intercept)','Std. Error']
             
             reg12.fit$pct_married[states == state] <- coef(fit12.temp)[2]
             if(!is.na(coef(fit12.temp)[2])){
             reg12.fit$pct_married.se[states == state] <- summary(fit12.temp)$coefficients['pct_married_2010','Std. Error']
             }
             
             reg12.fit$pct_college[states == state] <- coef(fit12.temp)[3]
             if(!is.na(coef(fit12.temp)[3])){
             reg12.fit$pct_college.se[states == state] <- summary(fit12.temp)$coefficients['pct_college_or_higher_2010','Std. Error']
             }
             
             reg12.fit$median_hh_income[states == state] <- coef(fit12.temp)[4]
             if(!is.na(coef(fit12.temp)[4])){
             reg12.fit$median_hh_income.se[states == state] <- summary(fit12.temp)$coefficients['median_hh_income_2010','Std. Error']
             }
             
             reg12.fit$pct_white[states == state] <- coef(fit12.temp)[5]
             if(!is.na(coef(fit12.temp)[5])){
             reg12.fit$pct_white.se[states == state] <- summary(fit12.temp)$coefficients['pct_white_2010','Std. Error']
             }
             
             reg12.fit$pct_black[states == state] <- coef(fit12.temp)[6]
             if(!is.na(coef(fit12.temp)[6])){
             reg12.fit$pct_black.se[states == state] <- summary(fit12.temp)$coefficients['pct_black_2010','Std. Error']
             }
             
             reg12.fit$pct_hispanic[states == state] <- coef(fit12.temp)[7]
             if(!is.na(coef(fit12.temp)[7])){
             reg12.fit$pct_hispanic.se[states == state] <- summary(fit12.temp)$coefficients['pct_hispanic_2010','Std. Error']
             }
             
             reg12.fit$pct_age_15_24[states == state] <- coef(fit12.temp)[8]
             if(!is.na(coef(fit12.temp)[8])){
             reg12.fit$pct_age_15_24.se[states == state] <- summary(fit12.temp)$coefficients['pct_age_15_24_2010','Std. Error']
             }
             
             reg12.fit$pct_age_25_34[states == state] <- coef(fit12.temp)[9]
             if(!is.na(coef(fit12.temp)[9])){
             reg12.fit$pct_age_25_34.se[states == state] <- summary(fit12.temp)$coefficients['pct_age_25_34_2010','Std. Error']
             }
             
             reg12.fit$pct_age_35_64[states == state] <- coef(fit12.temp)[10]
             
             if(!is.na(coef(fit12.temp)[10])){
             reg12.fit$pct_age_35_64.se[states == state] <- summary(fit12.temp)$coefficients['pct_age_35_64_2010','Std. Error']
             }
             
             reg12.fit$pct_age_65_up[states == state] <- coef(fit12.temp)[11]
             if(!is.na(coef(fit12.temp)[11])){
             reg12.fit$pct_age_65_up.se[states == state] <- summary(fit12.temp)$coefficients['pct_age_65_up_2010','Std. Error']
             }
             
             reg12.fit$tot_pop[states == state] <- coef(fit12.temp)[12]
             if(!is.na(coef(fit12.temp)[12])){
               reg12.fit$tot_pop.se[states == state] <- summary(fit12.temp)$coefficients['tot_pop_2010','Std. Error']
             }
             
             reg12.fit$rmse[states == state] <- sqrt(mean((predict(fit12.temp) - fit12.temp$model$total12) ^ 2,na.rm=T))

             total.hat16 <-  ifelse(!is.na(coef(fit12.temp)[1]),coef(fit12.temp)[1],0) +
               ifelse(!is.na(coef(fit12.temp)[2] * df$pct_married_2014[df$state == state ]),coef(fit12.temp)[2] * df$pct_married_2014[df$state == state ],0) +
               ifelse(!is.na(coef(fit12.temp)[3] * df$pct_college_or_higher_2014[df$state == state ]),coef(fit12.temp)[3] * df$pct_college_or_higher_2014[df$state == state ],0) +
               ifelse(!is.na(coef(fit12.temp)[4] * df$median_hh_income_2014[df$state == state ]),coef(fit12.temp)[4] * df$median_hh_income_2014[df$state == state ],0) +
               ifelse(!is.na(coef(fit12.temp)[5] * df$pct_white_2014[df$state == state ]),coef(fit12.temp)[5] * df$pct_white_2014[df$state == state ],0) +
               ifelse(!is.na(coef(fit12.temp)[6] * df$pct_black_2014[df$state == state ]),coef(fit12.temp)[6] * df$pct_black_2014[df$state == state ],0) +
               ifelse(!is.na(coef(fit12.temp)[7] * df$pct_hispanic_2014[df$state == state ]),coef(fit12.temp)[7] * df$pct_hispanic_2014[df$state == state ],0) +
               ifelse(!is.na(coef(fit12.temp)[8] * df$pct_age_15_24_2014[df$state == state ]),coef(fit12.temp)[8] * df$pct_age_15_24_2014[df$state == state ],0) +
               ifelse(!is.na(coef(fit12.temp)[9] * df$pct_age_25_34_2014[df$state == state ]),coef(fit12.temp)[9] * df$pct_age_25_34_2014[df$state == state ],0) +
               ifelse(!is.na(coef(fit12.temp)[10] * df$pct_age_35_64_2014[df$state == state ]),coef(fit12.temp)[10] * df$pct_age_35_64_2014[df$state == state ],0) +
               ifelse(!is.na(coef(fit12.temp)[11] * df$pct_age_65_up_2014[df$state == state ]),coef(fit12.temp)[11] * df$pct_age_65_up_2014[df$state == state ],0)+
               ifelse(!is.na(coef(fit12.temp)[12] * df$tot_pop_2014[df$state == state ]),coef(fit12.temp)[12] * df$tot_pop_2014[df$state == state ],0)+
               ifelse(!is.na(coef(fit12.temp)[13] * df$reg16[df$state == state ]),coef(fit12.temp)[13] * df$reg16[df$state == state ],0)
             
             n.units.state.test.16[states == state] <- sum(!is.na(total.hat16))
             

             reg.pred16[df$state == state ] <- total.hat16
             reg.out.bias16[states == state] <- mean(total.hat16 - df$total16[df$state == state ], na.rm = T)
             reg.out.rmse16[states == state] <- sqrt(mean((total.hat16 - df$total16[df$state == state]) ^ 2, na.rm = T))
             reg.state.pred16[states == state] <- sum(total.hat16, na.rm = T)



         }




         if (sum(df$state == state & !is.na(df$reg16) & !is.na(df$total16)) > 1) {
          #   l = ( !is.na(df$total16) & !is.na(df$pct_married_2014)& !is.na( df$pct_college_or_higher_2014)& !is.na( df$median_hh_income_2014)& !is.na( df$pct_white_2014)& !is.na( df$pct_black_2014)& !is.na( df$pct_hispanic_2014)& !is.na( df$pct_age_15_24_2014)& !is.na( df$pct_age_25_34_2014)& !is.na( df$pct_age_35_64_2014)& !is.na( df$pct_age_65_up_2014))

             fit16.temp <- lm(total16 ~ pct_married_2014 + pct_college_or_higher_2014 + median_hh_income_2014 + pct_white_2014 + pct_black_2014 + pct_hispanic_2014 + pct_age_15_24_2014 + pct_age_25_34_2014 + pct_age_35_64_2014 + pct_age_65_up_2014 + tot_pop_2014 + reg16, df[df$state == state, ], weights = reg16)
             n.units.state.train.16[states == state] <- nrow(fit16.temp$model)
             
              reg16.fit$intercept[states == state] <- coef(fit16.temp)[1]
             reg16.fit$intercept.se[states == state] <- summary(fit16.temp)$coefficients['(Intercept)','Std. Error']
             
             reg16.fit$pct_married[states == state] <- coef(fit16.temp)[2]
             if(!is.na(coef(fit16.temp)[2])){
             reg16.fit$pct_married.se[states == state] <- summary(fit16.temp)$coefficients['pct_married_2014','Std. Error']
             }
             
             reg16.fit$pct_college[states == state] <- coef(fit16.temp)[3]
             if(!is.na(coef(fit16.temp)[3])){
             reg16.fit$pct_college.se[states == state] <- summary(fit16.temp)$coefficients['pct_college_or_higher_2014','Std. Error']
             }
             
             reg16.fit$median_hh_income[states == state] <- coef(fit16.temp)[4]
             if(!is.na(coef(fit16.temp)[4])){
             reg16.fit$median_hh_income.se[states == state] <- summary(fit16.temp)$coefficients['median_hh_income_2014','Std. Error']
             }
             
             reg16.fit$pct_white[states == state] <- coef(fit16.temp)[5]
             if(!is.na(coef(fit16.temp)[5])){
             reg16.fit$pct_white.se[states == state] <- summary(fit16.temp)$coefficients['pct_white_2014','Std. Error']
             }
             
             reg16.fit$pct_black[states == state] <- coef(fit16.temp)[6]
             if(!is.na(coef(fit16.temp)[6])){
             reg16.fit$pct_black.se[states == state] <- summary(fit16.temp)$coefficients['pct_black_2014','Std. Error']
             }
             
             reg16.fit$pct_hispanic[states == state] <- coef(fit16.temp)[7]
             if(!is.na(coef(fit16.temp)[7])){
             reg16.fit$pct_hispanic.se[states == state] <- summary(fit16.temp)$coefficients['pct_hispanic_2014','Std. Error']
             }
             
             reg16.fit$pct_age_15_24[states == state] <- coef(fit16.temp)[8]
             if(!is.na(coef(fit16.temp)[8])){
             reg16.fit$pct_age_15_24.se[states == state] <- summary(fit16.temp)$coefficients['pct_age_15_24_2014','Std. Error']
             }
             
             reg16.fit$pct_age_25_34[states == state] <- coef(fit16.temp)[9]
             if(!is.na(coef(fit16.temp)[9])){
             reg16.fit$pct_age_25_34.se[states == state] <- summary(fit16.temp)$coefficients['pct_age_25_34_2014','Std. Error']
             }
             
             reg16.fit$pct_age_35_64[states == state] <- coef(fit16.temp)[10]
             if(!is.na(coef(fit16.temp)[10])){
             reg16.fit$pct_age_35_64.se[states == state] <- summary(fit16.temp)$coefficients['pct_age_35_64_2014','Std. Error']
             }
             
             reg16.fit$pct_age_65_up[states == state] <- coef(fit16.temp)[11]
             if(!is.na(coef(fit16.temp)[11])){
             reg16.fit$pct_age_65_up.se[states == state] <- summary(fit16.temp)$coefficients['pct_age_65_up_2014','Std. Error']
             }
             
             reg16.fit$tot_pop[states == state] <- coef(fit16.temp)[12]
             if(!is.na(coef(fit16.temp)[12])){
               reg16.fit$tot_pop.se[states == state] <- summary(fit16.temp)$coefficients['tot_pop_2014','Std. Error']
             }
             reg16.fit$rmse[states == state] <- sqrt(mean((predict(fit16.temp) - fit16.temp$model$total16) ^ 2,na.rm=T))

             total.hat20 <- ifelse(!is.na(coef(fit16.temp)[1]),coef(fit16.temp)[1],0) +
               ifelse(!is.na(coef(fit16.temp)[2] * df$pct_married_2018[df$state == state ]),coef(fit16.temp)[2] * df$pct_married_2018[df$state == state ],0) +
               ifelse(!is.na(coef(fit16.temp)[3] * df$pct_college_or_higher_2018[df$state == state ]),coef(fit16.temp)[3] * df$pct_college_or_higher_2018[df$state == state ],0) +
               ifelse(!is.na(coef(fit16.temp)[4] * df$median_hh_income_2018[df$state == state ]),coef(fit16.temp)[4] * df$median_hh_income_2018[df$state == state ],0) +
               ifelse(!is.na(coef(fit16.temp)[5] * df$pct_white_2018[df$state == state ]),coef(fit16.temp)[5] * df$pct_white_2018[df$state == state ],0) +
               ifelse(!is.na(coef(fit16.temp)[6] * df$pct_black_2018[df$state == state ]),coef(fit16.temp)[6] * df$pct_black_2018[df$state == state ],0) +
               ifelse(!is.na(coef(fit16.temp)[7] * df$pct_hispanic_2018[df$state == state ]),coef(fit16.temp)[7] * df$pct_hispanic_2018[df$state == state ],0) +
               ifelse(!is.na(coef(fit16.temp)[8] * df$pct_age_15_24_2018[df$state == state ]),coef(fit16.temp)[8] * df$pct_age_15_24_2018[df$state == state ],0) +
               ifelse(!is.na(coef(fit16.temp)[9] * df$pct_age_25_34_2018[df$state == state ]),coef(fit16.temp)[9] * df$pct_age_25_34_2018[df$state == state ],0) +
               ifelse(!is.na(coef(fit16.temp)[10] * df$pct_age_35_64_2018[df$state == state ]),coef(fit16.temp)[10] * df$pct_age_35_64_2018[df$state == state ],0) +
               ifelse(!is.na(coef(fit16.temp)[11] * df$pct_age_65_up_2018[df$state == state ]),coef(fit16.temp)[11] * df$pct_age_65_up_2018[df$state == state ],0)+
               ifelse(!is.na(coef(fit16.temp)[12] * df$tot_pop_2018[df$state == state ]),coef(fit16.temp)[12] * df$tot_pop_2018[df$state == state ],0)+
               ifelse(!is.na(coef(fit16.temp)[13] * df$reg20[df$state == state ]),coef(fit16.temp)[13] * df$reg20[df$state == state ],0)
             
             n.units.state.test.20[states == state] <- sum(!is.na(total.hat20))
             

             reg.pred20[df$state == state] <- total.hat20
             reg.out.bias20[states == state] <- mean(total.hat20 - df$total20[df$state == state], na.rm = T)
             reg.out.rmse20[states == state] <- sqrt(mean((total.hat20 - df$total20[df$state == state]) ^ 2, na.rm = T))
             reg.state.pred20[states == state] <- sum(total.hat20, na.rm = T)



         }
     }

 }
 
 
 
 
 
 
 
 
 
 holder = data.frame(bias = rep(NA,2),
                     bias.pct = rep(NA,2),
                     rmse = rep(NA,2),
                     rmse.pct = rep(NA,2),
                     Model = rep(NA,2),
                     Training = rep(NA,2),
                     Test = rep(NA,2),
                     N.Training = rep(NA,2),
                     N.Test = rep(NA,2),
                     Unit = 'County')








# Create holder for bias and mean squared
holder = data.frame(bias = rep(NA,2), bias.pct = rep(NA,2), rmse = rep(NA,2), rmse.pct = rep(NA,2), Model = rep(NA,2), Training = rep(NA,2), Test = rep(NA,2), Unit = 'County')


# Plot county predictions and residuals vs. observed data in 2016
bias.reg.all = mean(reg.pred16 - df$total16, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total16, na.rm = T) * 100
rmse.reg.all = sqrt(mean((reg.pred16 - df$total16) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total16, na.rm = T) * 100

holder$bias[1] = bias.reg.all
holder$bias.pct[1] = bias.reg.all.pct
holder$rmse[1] = rmse.reg.all
holder$rmse.pct[1] = rmse.reg.all.pct
holder$Model[1] = 'Registration + Demographics'
holder$Type[1] = 'WLS'
holder$Training[1] = '2012'
holder$Test[1] = '2016'

bias.reg.all = mean(reg.pred20 - df$total20, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total20, na.rm = T) * 100
rmse.reg.all = sqrt(mean((reg.pred20 - df$total20) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total20, na.rm = T) * 100

holder$bias[2] = bias.reg.all
holder$bias.pct[2] = bias.reg.all.pct
holder$rmse[2] = rmse.reg.all
holder$rmse.pct[2] = rmse.reg.all.pct
holder$Model[2] = 'Registration + Demographics'
holder$Type[2] = 'WLS'
holder$Training[2] = '2016'
holder$Test[2] = '2020'

holder = holder %>%
  mutate(n_units_training = c(sum(n.units.state.train.12, na.rm=T), sum(n.units.state.train.16, na.rm=T)),
         n_state_training = c( sum(!is.na(n.units.state.train.12)), sum(!is.na(n.units.state.train.16))),
         
         n_units_test = c( sum(n.units.state.test.16,na.rm=T), sum(n.units.state.test.20, na.rm=T)),
         n_state_test = c( sum(!is.na(n.units.state.test.16)),sum(!is.na(n.units.state.test.20))))





write.csv(holder, 'results/registration-demographics-model-county-stats-weighted.csv')


