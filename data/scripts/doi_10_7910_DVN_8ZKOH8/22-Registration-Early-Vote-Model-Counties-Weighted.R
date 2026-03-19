## Author: Kabir Khanna and Jacob Brown
rm(list=ls())
library(tidyverse)

# 2020 county-level data
df <- read.csv("data/counties_analysis.csv"); head(df); dim(df)

## Bring in 2020 eavs data
eavs = read_csv('data/2020_EAVS_for_Public_Release_nolabel[1].csv')
# make eavs variables for merge
eavs = eavs %>%
  mutate(adv20.eavs = F1f + F1d,
         fips = as.numeric(substr(FIPSCode,1,5)))%>%
  select(fips,adv20.eavs)%>%
  group_by(fips)%>%
  dplyr::summarize(adv20.eavs=sum(adv20.eavs,na.rm=T)
            )%>%
  filter(!duplicated(fips))

df = left_join(df, eavs, by = 'fips')%>%
  as.data.frame

# Model 1: regress total votes on contemporaneous Early Vote
fit_12 <- lm(total12 ~ adv12.eavs + reg12, df, weights = reg12); summary(fit_12)
fit_16 <- lm(total16 ~ adv16.eavs + reg12, df, weights = reg16); summary(fit_16)
fit_20 <- lm(total20 ~ adv20.eavs + reg12, df, weights = reg20); summary(fit_20)

# Total votes over Early Vote
sum(df$total12,na.rm=T) / sum(df$adv12.eavs,na.rm=T) #.75
sum(df$total16,na.rm=T) / sum(df$adv16.eavs,na.rm=T) #.79
sum(df$total20,na.rm=T) / sum(df$adv20.eavs,na.rm=T) #.79

# Compute ratios and regression by state
states <- unique(df$state)
states <- states[order(states)]; print(states); length(states)

reg20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 3)); rownames(reg20.fit) <- states; names(reg20.fit) <- c("intercept", "slope", "rmse")
reg12.fit <- reg16.fit <- reg20.fit

ratio20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 2)); rownames(ratio20.fit) <- states; names(ratio20.fit) <- c("ratio", "rmse")
ratio12.fit <- ratio16.fit <- ratio20.fit

reg.state.pred16 <- reg.out.rmse16 <- reg.out.bias16 <- rep(NA, length(states))
ratio.state.pred16 <- ratio.out.rmse16 <- ratio.out.bias16 <- rep(NA, length(states))
ratios.state.pred16 <- ratios.out.rmse16 <- ratios.out.bias16 <- rep(NA, length(states))

reg.state.pred20 <- reg.out.rmse20 <- reg.out.bias20 <- rep(NA, length(states))
ratio.state.pred20 <- ratio.out.rmse20 <- ratio.out.bias20 <- rep(NA, length(states))
ratios.state.pred20 <- ratios.out.rmse20 <- ratios.out.bias20 <- rep(NA, length(states))

ratios.pred16 <- ratio.pred16 <- reg.pred16 <- rep(NA, nrow(df))
ratios.pred20 <- ratio.pred20 <- reg.pred20 <- rep(NA, nrow(df))
n.units.state.train.12 <- rep(NA, length(states))
n.units.state.test.16 <- rep(NA, length(states))
n.units.state.train.16 <- rep(NA, length(states))
n.units.state.test.20 <- rep(NA, length(states))

for (state in states) {
 
    if (sum(df$state == state & !is.na(df$adv12.eavs) & !is.na(df$total12)) > 1) {
      fit12.temp <- lm(total12 ~ adv12.eavs + reg12, df[df$state == state, ], weights = reg12)
      n.units.state.train.12[states == state] <- nrow(fit12.temp$model)
      reg12.fit$intercept[states == state] <- coef(fit12.temp)[1]
      reg12.fit$intercept.se[states == state] <- summary(fit12.temp)$coefficients['(Intercept)','Std. Error']
      
      reg12.fit$slope[states == state] <- coef(fit12.temp)[2]
      reg12.fit$slope.se[states == state] <- summary(fit12.temp)$coefficients['adv12.eavs','Std. Error']
      reg12.fit$rmse[states == state] <- sqrt(mean((predict(fit12.temp) - df$total12[df$state == state & !is.na(df$adv12.eavs) & !is.na(df$total12)]) ^ 2))

      total.hat16 <- coef(fit12.temp)[1] + coef(fit12.temp)[2] * df$adv16.eavs[df$state == state]+ coef(fit12.temp)[3] * df$reg16[df$state == state]
      n.units.state.test.16[states == state] <- sum(!is.na(total.hat16))
      reg.pred16[df$state == state] <- total.hat16
      reg.out.bias16[states == state] <- mean(total.hat16 - df$total16[df$state == state], na.rm = T)
      reg.out.rmse16[states == state] <- sqrt(mean((total.hat16 - df$total16[df$state == state]) ^ 2, na.rm = T))
      reg.state.pred16[states == state] <- sum(total.hat16, na.rm = T)
    }
  

  
    if (sum(df$state == state & !is.na(df$adv16.eavs) & !is.na(df$total16)) > 1) {
      fit16.temp <- lm(total16 ~ adv16.eavs + reg16, df[df$state == state, ], weights = reg16)
      n.units.state.train.16[states == state] <- nrow(fit16.temp$model)
      reg16.fit$intercept[states == state] <- coef(fit16.temp)[1]
      reg16.fit$intercept.se[states == state] <- summary(fit16.temp)$coefficients['(Intercept)','Std. Error']
      
      reg16.fit$slope[states == state] <- coef(fit16.temp)[2]
      reg16.fit$slope.se[states == state] <- summary(fit16.temp)$coefficients['adv16.eavs','Std. Error']
      reg16.fit$rmse[states == state] <- sqrt(mean((predict(fit16.temp) - df$total16[df$state == state & !is.na(df$adv16.eavs) & !is.na(df$total16)]) ^ 2))

      total.hat20 <- coef(fit16.temp)[1] + coef(fit16.temp)[2] * df$adv20.eavs[df$state == state] + coef(fit16.temp)[3] * df$reg20[df$state == state]
      n.units.state.test.20[states == state] <- sum(!is.na(total.hat20))
      reg.pred20[df$state == state] <- total.hat20
      reg.out.bias20[states == state] <- mean(total.hat20 - df$total20[df$state == state], na.rm = T)
      reg.out.rmse20[states == state] <- sqrt(mean((total.hat20 - df$total20[df$state == state]) ^ 2, na.rm = T))
      reg.state.pred20[states == state] <- sum(total.hat20, na.rm = T)
    }
  }





# Create holder for bias and mean squared

holder = data.frame(bias = rep(NA,2), bias.pct = rep(NA,2), rmse = rep(NA,2), rmse.pct = rep(NA,2), Model = rep(NA,2),  Training = rep(NA,2), Test = rep(NA,2), Unit = 'County')


bias.reg.all = mean(reg.pred16 - df$total16, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total16, na.rm = T) * 100
rmse.reg.all = sqrt(mean((reg.pred16 - df$total16) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total16, na.rm = T) * 100

holder$bias[1] = bias.reg.all
holder$bias.pct[1] = bias.reg.all.pct
holder$rmse[1] = rmse.reg.all
holder$rmse.pct[1] = rmse.reg.all.pct
holder$Model[1] = 'Registration + Early Vote'
holder$Type[1] = 'WLS'
holder$Training[1] = '2012'
holder$Test[1] = '2016'



bias.reg.all = mean(reg.pred20 - df$total20, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total20, na.rm = T) * 100
rmse.reg.all = sqrt(mean((reg.pred20 - df$total20) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total20, na.rm = T) * 100

holder$bias[2] = bias.reg.all
holder$bias.pct[2] = bias.reg.all.pct
holder$rmse[2] = rmse.reg.all
holder$rmse.pct[2] = rmse.reg.all.pct
holder$Model[2] = 'Registration + Early Vote'
holder$Type[2] = 'WLS'
holder$Training[2] = '2016'
holder$Test[2] = '2020'








##
holder = holder %>%
  mutate(n_units_training = c(sum(n.units.state.train.12, na.rm=T), sum(n.units.state.train.16, na.rm=T)),
         n_state_training = c( sum(!is.na(n.units.state.train.12)), sum(!is.na(n.units.state.train.16))),
         
         n_units_test = c( sum(n.units.state.test.16,na.rm=T), sum(n.units.state.test.20, na.rm=T)),
         n_state_test = c( sum(!is.na(n.units.state.test.16)),sum(!is.na(n.units.state.test.20))))






write.csv(holder, 'results/registration-early-vote-model-county-stats-weighted.csv')

