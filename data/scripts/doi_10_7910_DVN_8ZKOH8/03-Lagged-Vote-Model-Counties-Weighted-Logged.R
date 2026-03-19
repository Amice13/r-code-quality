## Author: Kabir Khanna and Jacob Brown
rm(list=ls())

# 2020 county-level data
df <- read.csv("data/counties_analysis.csv"); head(df); dim(df)
df$log_total12 = log(df$total12)
df$log_total16 = log(df$total16)
df$log_total20 = log(df$total20)
df$log_reg12 = log(df$reg12)
df$log_reg16 = log(df$reg16)
df$log_reg20 = log(df$reg20)

# Model 2: regress total vote on lagged total vote

states <- unique(as.character(df$state))
states <- states[order(states)]; print(states); length(states)

reg20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 3)); rownames(reg20.fit) <- states; names(reg20.fit) <- c("intercept", "slope", "rmse")
reg12.fit <- reg16.fit <- reg20.fit

ratio20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 2)); rownames(ratio20.fit) <- states; names(ratio20.fit) <- c("ratio", "rmse")
ratio12.fit <- ratio16.fit <- ratio20.fit

n.units.state.train.1216 <- rep(NA, length(states))
n.units.state.test.1620 <- rep(NA, length(states))

reg.state.pred20 <- reg.state.pred20.train <- reg.out.rmse20 <- reg.out.bias20 <- rep(NA, length(states))
ratio.state.pred20 <- ratio.out.rmse20 <- ratio.out.bias20 <- rep(NA, length(states))
ratios.state.pred20 <- ratios.out.rmse20 <- ratios.out.bias20 <- rep(NA, length(states))

ratios.pred20 <- ratio.pred20 <- reg.pred20 <-reg.pred20.train <-rep(NA, nrow(df))

for (state in states) {

    if (sum(df$state == state & !is.na(df$log_total12) & !is.na(df$log_total16)) > 1) {
      fit16.temp <- lm(log_total16 ~ log_total12, weights = log_reg12, df[df$state == state, ])
      n.units.state.train.1216[states == state] <- nrow(fit16.temp$model)

      reg16.fit$intercept[states == state] <- coef(fit16.temp)[1]
      reg16.fit$intercept.se[states == state] <- summary(fit16.temp)$coefficients['(Intercept)','Std. Error']

      reg16.fit$slope[states == state] <- coef(fit16.temp)[2]
      reg16.fit$slope.se[states == state] <- summary(fit16.temp)$coefficients['log_total12','Std. Error']
      reg16.fit$rmse[states == state] <- sqrt(mean((predict(fit16.temp) - df$log_total16[df$state == state & !is.na(df$log_total12) & !is.na(df$log_total16)]) ^ 2))

      total.hat20.train <- coef(fit16.temp)[1] + coef(fit16.temp)[2] * df$log_total12[df$state == state]
      total.hat20 <- coef(fit16.temp)[1] + coef(fit16.temp)[2] * df$log_total16[df$state == state]
      n.units.state.test.1620[states == state] <- sum(!is.na(total.hat20))


      reg.pred20[df$state == state] <- total.hat20
      reg.pred20.train[df$state == state] <- total.hat20.train

      reg.out.bias20[states == state] <- mean(total.hat20 - df$log_total20[df$state == state & !is.na(df$log_total16) & !is.na(df$log_total20)])
      reg.out.rmse20[states == state] <- sqrt(mean((total.hat20 - df$log_total20[df$state == state & !is.na(df$log_total16) & !is.na(df$log_total20)]) ^ 2))
      reg.state.pred20[states == state] <- sum(total.hat20, na.rm = T)
      reg.state.pred20.train[states == state] <- sum(total.hat20.train, na.rm = T)

    }

}






holder = data.frame(bias = rep(NA,1),
                    bias.pct = rep(NA,1),
                    rmse = rep(NA,1),
                    rmse.pct = rep(NA,1),
                    Model = rep(NA,1),
                    Training = rep(NA,1),
                    Test = rep(NA,1),
                    Unit = 'County',
                    bias.train = rep(NA,1),
                    bias.pct.train = rep(NA,1),
                    rmse.train = rep(NA,1),
                    rmse.pct.train = rep(NA,1))

bias.reg.all = mean(exp(reg.pred20) - df$total20, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total20, na.rm = T) * 100
rmse.reg.all = sqrt(mean((exp(reg.pred20) - df$total20) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total20, na.rm = T) * 100



holder$bias[1] = bias.reg.all
holder$bias.pct[1] = bias.reg.all.pct
holder$rmse[1] = rmse.reg.all
holder$rmse.pct[1] = rmse.reg.all.pct
holder$Model[1] = 'Lagged Vote'
holder$Type[1] = 'WLS'
holder$Training[1] = '2012-2016'
holder$Test[1] = '2016-2020'






######
holder = holder %>%
  mutate(n_units_training = c(sum(n.units.state.train.1216, na.rm=T)),
         n_state_training = c(sum(!is.na(n.units.state.train.1216))),

         n_units_test = c(sum(n.units.state.test.1620,na.rm=T)),
         n_state_test = c(sum(!is.na(n.units.state.test.1620))))


write.csv(holder, 'log/lagged-vote-model-county-stats-weighted-logged.csv')

