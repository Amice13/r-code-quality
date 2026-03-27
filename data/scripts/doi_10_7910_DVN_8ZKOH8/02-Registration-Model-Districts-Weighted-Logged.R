## Author: Kabir Khanna and Jacob Brown
rm(list=ls())

df <- read.csv("data/districts_analysis.csv")
df$log_total12 = log(df$total12)
df$log_total16 = log(df$total16)
df$log_total20 = log(df$total20)
df$log_reg12 = log(df$reg12)
df$log_reg16 = log(df$reg16)
df$log_reg20 = log(df$reg20)

states <- unique(df$state)
states <- states[order(states)]; print(states); length(states)

reg20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 3)); rownames(reg20.fit) <- states; names(reg20.fit) <- c("intercept", "slope", "rmse")
reg12.fit <- reg16.fit <- reg20.fit

ratio20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 2)); rownames(ratio20.fit) <- states; names(ratio20.fit) <- c("ratio", "rmse")
ratio12.fit <- ratio16.fit <- ratio20.fit

reg.state.pred16 <- reg.state.pred16.train <- reg.out.rmse16 <- reg.out.bias16 <- rep(NA, length(states))
ratio.state.pred16 <- ratio.out.rmse16 <- ratio.out.bias16 <- rep(NA, length(states))
ratios.state.pred16 <- ratios.out.rmse16 <- ratios.out.bias16 <- rep(NA, length(states))

n.units.state.train.12 <- rep(NA, length(states))
n.units.state.test.16 <- rep(NA, length(states))
n.units.state.train.16 <- rep(NA, length(states))
n.units.state.test.20 <- rep(NA, length(states))


reg.state.pred20 <- reg.out.rmse20 <- reg.out.bias20 <- rep(NA, length(states))
ratio.state.pred20 <- ratio.out.rmse20 <- ratio.out.bias20 <- rep(NA, length(states))
ratios.state.pred20 <- ratios.out.rmse20 <- ratios.out.bias20 <- rep(NA, length(states))
reg.state.pred20.train <- reg.out.rmse20.train <- reg.out.bias20.train <- rep(NA, length(states))

ratios.pred16 <- ratio.pred16 <- reg.pred16 <- reg.pred16.train <- rep(NA, nrow(df))
ratios.pred20 <- ratio.pred20 <- reg.pred20 <- reg.pred20.train <- rep(NA, nrow(df))

for (state in states) {

    if (sum(df$state == state & !is.na(df$log_reg12) & !is.na(df$log_total12)) > 1) {
      fit12.temp <- lm(log(total12) ~ log(reg12), weights = log(reg12), df[df$state == state, ])
      n.units.state.train.12[states == state] <- nrow(fit12.temp$model)

      reg12.fit$intercept[states == state] <- coef(fit12.temp)[1]
      reg12.fit$intercept.se[states == state] <- summary(fit12.temp)$coefficients['(Intercept)','Std. Error']

      reg12.fit$slope[states == state] <- coef(fit12.temp)[2]
      reg12.fit$slope.se[states == state] <- summary(fit12.temp)$coefficients['log(reg12)','Std. Error']

      reg12.fit$rmse[states == state] <- sqrt(mean((predict(fit12.temp) - df$log_total12[df$state == state & !is.na(df$log_reg12) & !is.na(df$log_total12)]) ^ 2))

      total.hat16.train <- coef(fit12.temp)[1] + coef(fit12.temp)[2] * df$log_reg12[df$state == state]
      total.hat16 <- coef(fit12.temp)[1] + coef(fit12.temp)[2] * df$log_reg16[df$state == state]
      n.units.state.test.16[states == state] <- sum(!is.na(total.hat16))

      reg.pred16[df$state == state] <- total.hat16
      reg.pred16.train[df$state == state] <- total.hat16.train

      reg.out.bias16[states == state] <- mean(total.hat16 - df$log_total16[df$state == state], na.rm = T)
      reg.out.rmse16[states == state] <- sqrt(mean((total.hat16 - df$log_total16[df$state == state]) ^ 2, na.rm = T))
      reg.state.pred16[states == state] <- sum(total.hat16, na.rm = T)
      reg.state.pred16.train[states == state] <- sum(total.hat16.train, na.rm = T)

    }



    if (sum(df$state == state & !is.na(df$log_reg16) & !is.na(df$log_total16)) > 1) {
      fit16.temp <- lm(log(total16) ~ log(reg16), weights = log(reg16), df[df$state == state, ])
      n.units.state.train.16[states==state] <- nrow(fit16.temp$model)

      reg16.fit$intercept[states == state] <- coef(fit16.temp)[1]
      reg16.fit$intercept.se[states == state] <- summary(fit16.temp)$coefficients['(Intercept)','Std. Error']

      reg16.fit$slope[states == state] <- coef(fit16.temp)[2]
      reg16.fit$slope.se[states == state] <- summary(fit16.temp)$coefficients['log(reg16)','Std. Error']

      reg16.fit$rmse[states == state] <- sqrt(mean((predict(fit16.temp) - df$log_total16[df$state == state & !is.na(df$log_reg16) & !is.na(df$log_total16)]) ^ 2))

      total.hat20 <- coef(fit16.temp)[1] + coef(fit16.temp)[2] * df$log_reg20[df$state == state]
      total.hat20.train <- coef(fit16.temp)[1] + coef(fit16.temp)[2] * df$log_reg16[df$state == state]

      n.units.state.test.20[states == state] <- sum(!is.na(total.hat20))

      reg.pred20[df$state == state] <- total.hat20
      reg.pred20.train[df$state == state] <- total.hat20.train

      reg.out.bias20[states == state] <- mean(total.hat20 - df$log_total20[df$state == state], na.rm = T)
      reg.out.rmse20[states == state] <- sqrt(mean((total.hat20 - df$log_total20[df$state == state]) ^ 2, na.rm = T))
      reg.state.pred20[states == state] <- sum(total.hat20, na.rm = T)
      reg.state.pred20.train[states == state] <- sum(total.hat20.train, na.rm = T)


  }
}






holder = data.frame(bias = rep(NA,2),
                    bias.pct = rep(NA,2),
                    rmse = rep(NA,2),
                    rmse.pct = rep(NA,2),
                    Model = rep(NA,2),
                    Training = rep(NA,2),
                    Test = rep(NA,2),
                    Unit = 'District',
                    bias.train = rep(NA,2),
                    bias.pct.train = rep(NA,2),
                    rmse.train = rep(NA,2),
                    rmse.pct.train = rep(NA,2)
)


bias.reg.all = mean(exp(reg.pred16) - df$total16, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total16, na.rm = T) * 100
rmse.reg.all = sqrt(mean((exp(reg.pred16) - df$total16) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total16, na.rm = T) * 100



holder$bias[1] = bias.reg.all
holder$bias.pct[1] = bias.reg.all.pct
holder$rmse[1] = rmse.reg.all
holder$rmse.pct[1] = rmse.reg.all.pct
holder$Model[1] = 'Registration'
holder$Type[1] = 'WLS'
holder$Training[1] = '2012'
holder$Test[1] = '2016'



y = tapply(df$total16, df$state, sum, na.rm = T)

scatter_out = tibble(state =names(y),votes = as.vector(y), model = 'Registration', pred = reg.state.pred16, training = 2012, test = 2016)

bias.reg.all = mean(exp(reg.pred20) - df$total20, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total20, na.rm = T) * 100
rmse.reg.all = sqrt(mean((exp(reg.pred20) - df$total20) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total20, na.rm = T) * 100


holder$bias[2] = bias.reg.all
holder$bias.pct[2] = bias.reg.all.pct
holder$rmse[2] = rmse.reg.all
holder$rmse.pct[2] = rmse.reg.all.pct
holder$Model[2] = 'Registration'
holder$Type[2] = 'WLS'
holder$Training[2] = '2012'
holder$Test[2] = '2020'






#######
holder = holder %>%
  mutate(n_units_training = c( sum(n.units.state.train.12, na.rm=T), sum(n.units.state.train.16, na.rm=T)),
         n_state_training = c( sum(!is.na(n.units.state.train.12)), sum(!is.na(n.units.state.train.16))),

         n_units_test = c( sum(n.units.state.test.16,na.rm=T), sum(n.units.state.test.20, na.rm=T)),
         n_state_test = c( sum(!is.na(n.units.state.test.16)), sum(!is.na(n.units.state.test.20))))



write.csv(holder, 'log/registration-model-district-stats-weighted-logged.csv')

