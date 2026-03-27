## Author: Kabir Khanna and Jacob Brown
rm(list=ls())

library(tidyverse)
# 2020 county-level data
df <- read.csv("data/counties_analysis.csv"); head(df); dim(df)

# Model 1: regress total votes on contemporaneous registration


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
n.units.state.train.12 <- rep(NA, length(states))
n.units.state.test.16 <- rep(NA, length(states))
n.units.state.train.16 <- rep(NA, length(states))
n.units.state.test.20 <- rep(NA, length(states))

reg.state.pred20 <- reg.out.rmse20 <- reg.out.bias20 <- rep(NA, length(states))
ratio.state.pred20 <- ratio.out.rmse20 <- ratio.out.bias20 <- rep(NA, length(states))
ratios.state.pred20 <- ratios.out.rmse20 <- ratios.out.bias20 <- rep(NA, length(states))

ratios.pred16 <- ratio.pred16 <- reg.pred16 <- rep(NA, nrow(df))
ratios.pred20 <- ratio.pred20 <- reg.pred20 <- rep(NA, nrow(df))

for (state in states) {
  if (sum(df$state == state & !is.na(df$reg12) & !is.na(df$total12)) > 0) {
    ratio.temp = sum(df$total12[df$state == state & !is.na(df$reg12) & !is.na(df$total12)]) / sum(df$reg12[df$state == state & !is.na(df$reg12) & !is.na(df$total12)])
    ratios.temp = (df$total12 / df$reg12)[df$state == state]

    total12.hat.temp <- ratio.temp * df$reg12[df$state == state]
    ratio12.fit$ratio[states == state] <- ratio.temp
    ratio12.fit$rmse[states == state] <- sqrt(mean((total12.hat.temp - df$total12[df$state == state]) ^ 2, na.rm = T))

    ratio.pred16.temp <- ratio.temp * df$reg16[df$state == state]
    ratio.pred16[df$state == state] <- ratio.pred16.temp
    ratio.out.bias16[states == state] <- mean(ratio.pred16.temp - df$total16[df$state == state], na.rm = T)
    ratio.out.rmse16[states == state] <- sqrt(mean((ratio.pred16.temp - df$total16[df$state == state]) ^ 2, na.rm = T))
    ratio.state.pred16[states == state] <- sum(ratio.pred16.temp, na.rm = T)

    ratios.pred16.temp <- ratios.temp * df$reg16[df$state == state]
    ratios.pred16[df$state == state] <- ratios.pred16.temp
    ratios.out.bias16[states == state] <- mean(ratios.pred16.temp - df$total16[df$state == state], na.rm = T)
    ratios.out.rmse16[states == state] <- sqrt(mean((ratios.pred16.temp - df$total16[df$state == state]) ^ 2, na.rm = T))
    ratios.state.pred16[states == state] <- sum(ratios.pred16.temp, na.rm = T)
 
    if (sum(df$state == state & !is.na(df$reg12) & !is.na(df$total12)) > 1) {
      fit12.temp <- lm(total12 ~ reg12, df[df$state == state, ])
      n.units.state.train.12[states == state] <- nrow(fit12.temp$model)
      reg12.fit$intercept[states == state] <- coef(fit12.temp)[1]
      reg12.fit$intercept.se[states == state] <- summary(fit12.temp)$coefficients['(Intercept)','Std. Error']
      
      reg12.fit$slope[states == state] <- coef(fit12.temp)[2]
      reg12.fit$slope.se[states == state] <- summary(fit12.temp)$coefficients['reg12','Std. Error']
      
      reg12.fit$rmse[states == state] <- sqrt(mean((predict(fit12.temp) - df$total12[df$state == state & !is.na(df$reg12) & !is.na(df$total12)]) ^ 2))

      total.hat16 <- coef(fit12.temp)[1] + coef(fit12.temp)[2] * df$reg16[df$state == state]
      n.units.state.test.16[states == state] <- sum(!is.na(total.hat16))
      reg.pred16[df$state == state] <- total.hat16
      reg.out.bias16[states == state] <- mean(total.hat16 - df$total16[df$state == state], na.rm = T)
      reg.out.rmse16[states == state] <- sqrt(mean((total.hat16 - df$total16[df$state == state]) ^ 2, na.rm = T))
      reg.state.pred16[states == state] <- sum(total.hat16, na.rm = T)
    }
  }

  if (sum(df$state == state & !is.na(df$reg16) & !is.na(df$total16)) > 0) {
    ratio.temp = sum(df$total16[df$state == state & !is.na(df$reg16) & !is.na(df$total16)]) / sum(df$reg16[df$state == state & !is.na(df$reg16) & !is.na(df$total16)])
    ratios.temp = (df$total16 / df$reg16)[df$state == state]

    total16.hat.temp <- ratio.temp * df$reg16[df$state == state]
    ratio16.fit$ratio[states == state] <- ratio.temp
    ratio16.fit$rmse[states == state] <- sqrt(mean((total16.hat.temp - df$total16[df$state == state & !is.na(df$reg16) & !is.na(df$total16)]) ^ 2))

    ratio.pred20.temp <- ratio.temp * df$reg20[df$state == state]
    ratio.pred20[df$state == state] <- ratio.pred20.temp
    ratio.out.bias20[states == state] <- mean(ratio.pred20.temp - df$total20[df$state == state], na.rm = T)
    ratio.out.rmse20[states == state] <- sqrt(mean((ratio.pred20.temp - df$total20[df$state == state]) ^ 2, na.rm = T))
    ratio.state.pred20[states == state] <- sum(ratio.pred20.temp, na.rm = T)

    ratios.pred20.temp <- ratios.temp * df$reg20[df$state == state]
    ratios.pred20[df$state == state] <- ratios.pred20.temp
    ratios.out.bias20[states == state] <- mean(ratios.pred20.temp - df$total20[df$state == state], na.rm = T)
    ratios.out.rmse20[states == state] <- sqrt(mean((ratios.pred20.temp - df$total20[df$state == state]) ^ 2, na.rm = T))
    ratios.state.pred20[states == state] <- sum(ratios.pred20.temp, na.rm = T)

    if (sum(df$state == state & !is.na(df$reg16) & !is.na(df$total16)) > 1) {
      fit16.temp <- lm(total16 ~ reg16, df[df$state == state, ])
      n.units.state.train.16[states==state] <- nrow(fit12.temp$model)
      
      reg16.fit$intercept[states == state] <- coef(fit16.temp)[1]
      reg16.fit$intercept.se[states == state] <- summary(fit16.temp)$coefficients['(Intercept)','Std. Error']
      
      reg16.fit$slope[states == state] <- coef(fit16.temp)[2]
      reg16.fit$slope.se[states == state] <- summary(fit16.temp)$coefficients['reg16','Std. Error']
      reg16.fit$rmse[states == state] <- sqrt(mean((predict(fit16.temp) - df$total16[df$state == state & !is.na(df$reg16) & !is.na(df$total16)]) ^ 2))

      total.hat20 <- coef(fit16.temp)[1] + coef(fit16.temp)[2] * df$reg20[df$state == state]
      n.units.state.test.20[states == state] <- sum(!is.na(total.hat20))
      
      reg.pred20[df$state == state] <- total.hat20
      reg.out.bias20[states == state] <- mean(total.hat20 - df$total20[df$state == state], na.rm = T)
      reg.out.rmse20[states == state] <- sqrt(mean((total.hat20 - df$total20[df$state == state]) ^ 2, na.rm = T))
      reg.state.pred20[states == state] <- sum(total.hat20, na.rm = T)
    }
  }
}

out.m = data.frame(coef = c(reg12.fit$slope,reg16.fit$slope),
                   se = c(reg12.fit$slope.se,reg16.fit$slope.se),
                   variable = 'Registration',
                   model = 'Registration',
                   training = c(rep('2012', length(reg12.fit$slope)),rep('2016', length(reg16.fit$slope))),
                   test = c(rep('2016', length(reg12.fit$slope)),rep('2020', length(reg16.fit$slope))),
                   state = c(states,states)
)

write.csv(out.m, 'output/registration-model-county-coefficients.csv')






# Create holder for bias and mean squared

holder = data.frame(bias = rep(NA,6), bias.pct = rep(NA,6), rmse = rep(NA,6), rmse.pct = rep(NA,6), Model = rep(NA,6),  Training = rep(NA,6), Test = rep(NA,6), Unit = 'County')

bias.ratio.all = mean(ratio.pred16 - df$total16, na.rm = T); bias.ratio.all.pct = bias.ratio.all / mean(df$total16, na.rm = T) * 100
rmse.ratio.all = sqrt(mean((ratio.pred16 - df$total16) ^ 2, na.rm = T)); rmse.ratio.all.pct = rmse.ratio.all / mean(df$total16, na.rm = T) * 100

holder$bias[1] = bias.ratio.all
holder$bias.pct[1] = bias.ratio.all.pct
holder$rmse[1] = rmse.ratio.all
holder$rmse.pct[1] = rmse.ratio.all.pct
holder$Model[1] = 'Registration'
holder$Type[1] = 'Ratio'
holder$Training[1] = '2012'
holder$Test[1] = '2016'

bias.ratios.all = mean(ratios.pred16 - df$total16, na.rm = T); bias.ratios.all.pct = bias.ratios.all / mean(df$total16, na.rm = T) * 100
rmse.ratios.all = sqrt(mean((ratios.pred16 - df$total16) ^ 2, na.rm = T)); rmse.ratios.all.pct = rmse.ratios.all / mean(df$total16, na.rm = T) * 100

holder$bias[2] = bias.ratios.all
holder$bias.pct[2] = bias.ratios.all.pct
holder$rmse[2] = rmse.ratios.all
holder$rmse.pct[2] = rmse.ratios.all.pct
holder$Model[2] = 'Registration'
holder$Type[2] = 'Multiple Ratios'
holder$Training[2] = '2012'
holder$Test[2] = '2016'

bias.reg.all = mean(reg.pred16 - df$total16, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total16, na.rm = T) * 100
rmse.reg.all = sqrt(mean((reg.pred16 - df$total16) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total16, na.rm = T) * 100

holder$bias[3] = bias.reg.all
holder$bias.pct[3] = bias.reg.all.pct
holder$rmse[3] = rmse.reg.all
holder$rmse.pct[3] = rmse.reg.all.pct
holder$Model[3] = 'Registration'
holder$Type[3] = 'OLS'
holder$Training[3] = '2012'
holder$Test[3] = '2016'



# Store state vote totals
y = tapply(df$total16[!is.na(reg.pred16)], df$state[!is.na(reg.pred16)], sum, na.rm = T)
scatter_out = tibble(state =names(y),votes = as.vector(y), model = 'Registration', pred = reg.state.pred16, training = 2012, test = 2016)



# Plot county predictions and residuals vs. observed data in 2020
bias.ratio.all = mean(ratio.pred20 - df$total20, na.rm = T); bias.ratio.all.pct = bias.ratio.all / mean(df$total20, na.rm = T) * 100
rmse.ratio.all = sqrt(mean((ratio.pred20 - df$total20) ^ 2, na.rm = T)); rmse.ratio.all.pct = rmse.ratio.all / mean(df$total20, na.rm = T) * 100

holder$bias[4] = bias.ratio.all
holder$bias.pct[4] = bias.ratio.all.pct
holder$rmse[4] = rmse.ratio.all
holder$rmse.pct[4] = rmse.ratio.all.pct
holder$Model[4] = 'Registration'
holder$Type[4] = 'Ratio'
holder$Training[4] = '2016'
holder$Test[4] = '2020'

bias.ratios.all = mean(ratios.pred20 - df$total20, na.rm = T); bias.ratios.all.pct = bias.ratios.all / mean(df$total20, na.rm = T) * 100
rmse.ratios.all = sqrt(mean((ratios.pred20 - df$total20) ^ 2, na.rm = T)); rmse.ratios.all.pct = rmse.ratios.all / mean(df$total20, na.rm = T) * 100

holder$bias[5] = bias.ratios.all
holder$bias.pct[5] = bias.ratios.all.pct
holder$rmse[5] = rmse.ratios.all
holder$rmse.pct[5] = rmse.ratios.all.pct
holder$Model[5] = 'Registration'
holder$Type[5] = 'Multiple Ratios'
holder$Training[5] = '2016'
holder$Test[5] = '2020'


bias.reg.all = mean(reg.pred20 - df$total20, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total20, na.rm = T) * 100
rmse.reg.all = sqrt(mean((reg.pred20 - df$total20) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total20, na.rm = T) * 100

holder$bias[6] = bias.reg.all
holder$bias.pct[6] = bias.reg.all.pct
holder$rmse[6] = rmse.reg.all
holder$rmse.pct[6] = rmse.reg.all.pct
holder$Model[6] = 'Registration'
holder$Type[6] = 'OLS'
holder$Training[6] = '2016'
holder$Test[6] = '2020'



y = tapply(df$total20[!is.na(ratio.pred20)], df$state[!is.na(ratio.pred20)], sum, na.rm = T)


scatter_out = scatter_out%>%
  bind_rows(tibble(state =names(y),votes = as.vector(y), model = 'Registration', pred = reg.state.pred20, training = 2016, test = 2020))
write_csv(scatter_out,'output/registration-county-unweighted-state-scatter.csv')




######
holder = holder %>%
  mutate(n_units_training = c(NA,NA, sum(n.units.state.train.12, na.rm=T), NA, NA,sum(n.units.state.train.16, na.rm=T)),
         n_state_training = c(NA,NA, sum(!is.na(n.units.state.train.12)), NA, NA,sum(!is.na(n.units.state.train.16))),
         
         n_units_test = c(NA,NA, sum(n.units.state.test.16), NA, NA,sum(n.units.state.test.20)),
         n_state_test = c(NA,NA, sum(!is.na(n.units.state.test.16)), NA, NA,sum(!is.na(n.units.state.test.20))))

write.csv(holder, file = 'results/registration-model-county-stats-unweighted.csv')

