## Author: Kabir Khanna and Jacob Brown
rm(list=ls())

df <- read.csv("data/districts_analysis.csv")

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
  if (sum(df$state == state & !is.na(df$total12) & !is.na(df$total16)) > 0) {
    ratio.temp = sum(df$total16[df$state == state & !is.na(df$total12) & !is.na(df$total16)]) /
      sum(df$total12[df$state == state & !is.na(df$total12) & !is.na(df$total16)])
    ratios.temp = (df$total16 / df$total12)[df$state == state]
    
    total16.hat.temp <- ratio.temp * df$total12[df$state == state]
    ratio16.fit$ratio[states == state] <- ratio.temp
    ratio16.fit$rmse[states == state] <- sqrt(mean((total16.hat.temp - df$total16[df$state == state & !is.na(df$total12) & !is.na(df$total16)]) ^ 2))
    
    ratio.pred20.temp <- ratio.temp * df$total16[df$state == state]
    ratio.pred20[df$state == state] <- ratio.pred20.temp
    ratio.out.bias20[states == state] <- mean(ratio.pred20.temp - df$total20[df$state == state & !is.na(df$total16) & !is.na(df$total20)])
    ratio.out.rmse20[states == state] <- sqrt(mean((ratio.pred20.temp - df$total20[df$state == state & !is.na(df$total16) & !is.na(df$total20)]) ^ 2))
    ratio.state.pred20[states == state] <- sum(ratio.pred20.temp, na.rm = T)
    
    ratios.pred20.temp <- ratios.temp * df$total16[df$state == state]
    ratios.pred20[df$state == state] <- ratios.pred20.temp
    ratios.out.bias20[states == state] <- mean(ratios.pred20.temp - df$total20[df$state == state & !is.na(df$total16) & !is.na(df$total20)])
    ratios.out.rmse20[states == state] <- sqrt(mean((ratios.pred20.temp - df$total20[df$state == state & !is.na(df$total16) & !is.na(df$total20)]) ^ 2))
    ratios.state.pred20[states == state] <- sum(ratios.pred20.temp, na.rm = T)
    
    if (sum(df$state == state & !is.na(df$total12) & !is.na(df$total16)) > 1) {
      fit16.temp <- lm(total16 ~ total12, weights = reg12, df[df$state == state, ])
      n.units.state.train.1216[states == state] <- nrow(fit16.temp$model)
      
      reg16.fit$intercept[states == state] <- coef(fit16.temp)[1]
      reg16.fit$intercept.se[states == state] <- summary(fit16.temp)$coefficients['(Intercept)','Std. Error']
      
      reg16.fit$slope[states == state] <- coef(fit16.temp)[2]
      reg16.fit$slope.se[states == state] <- summary(fit16.temp)$coefficients['total12','Std. Error']
      reg16.fit$rmse[states == state] <- sqrt(mean((predict(fit16.temp) - df$total16[df$state == state & !is.na(df$total12) & !is.na(df$total16)]) ^ 2))
      
      total.hat20.train <- coef(fit16.temp)[1] + coef(fit16.temp)[2] * df$total12[df$state == state]
      total.hat20 <- coef(fit16.temp)[1] + coef(fit16.temp)[2] * df$total16[df$state == state]
      n.units.state.test.1620[states == state] <- sum(!is.na(total.hat20))
      
      
      reg.pred20[df$state == state] <- total.hat20
      reg.pred20.train[df$state == state] <- total.hat20.train
      
      reg.out.bias20[states == state] <- mean(total.hat20 - df$total20[df$state == state & !is.na(df$total16) & !is.na(df$total20)])
      reg.out.rmse20[states == state] <- sqrt(mean((total.hat20 - df$total20[df$state == state & !is.na(df$total16) & !is.na(df$total20)]) ^ 2))
      reg.state.pred20[states == state] <- sum(total.hat20, na.rm = T)
      reg.state.pred20.train[states == state] <- sum(total.hat20.train, na.rm = T)
      
    }
  }
}

out.m = data.frame(coef = c(reg16.fit$slope),
                   se = c(reg16.fit$slope.se),
                   variable = 'Lagged Vote',
                   model = 'Lagged Vote',
                   training = c(rep('2012-2016', length(reg12.fit$slope))),
                   test = c(rep('2016-2020', length(reg12.fit$slope))),
                   state = c(states)
)

write.csv(out.m, 'output/lagged-vote-model-district-coefficients-weighted.csv')




holder = data.frame(bias = rep(NA,3), 
                    bias.pct = rep(NA,3), 
                    rmse = rep(NA,3), 
                    rmse.pct = rep(NA,3), 
                    Model = rep(NA,3),  
                    Training = rep(NA,3), 
                    Test = rep(NA,3), 
                    Unit = 'District',
                    bias.train = rep(NA,3), 
                    bias.pct.train = rep(NA,3), 
                    rmse.train = rep(NA,3), 
                    rmse.pct.train = rep(NA,3))
bias.ratio.all = mean(ratio.pred20 - df$total20, na.rm = T); bias.ratio.all.pct = bias.ratio.all / mean(df$total20, na.rm = T) * 100
rmse.ratio.all = sqrt(mean((ratio.pred20 - df$total20) ^ 2, na.rm = T)); rmse.ratio.all.pct = rmse.ratio.all / mean(df$total20, na.rm = T) * 100

holder$bias[1] = bias.ratio.all
holder$bias.pct[1] = bias.ratio.all.pct
holder$rmse[1] = rmse.ratio.all
holder$rmse.pct[1] = rmse.ratio.all.pct
holder$Model[1] = 'Lagged Vote'
holder$Type[1] = 'Ratio'
holder$Training[1] = '2012-2016'
holder$Test[1] = '2016-2020'

bias.ratios.all = mean(ratios.pred20 - df$total20, na.rm = T); bias.ratios.all.pct = bias.ratios.all / mean(df$total20, na.rm = T) * 100
rmse.ratios.all = sqrt(mean((ratios.pred20 - df$total20) ^ 2, na.rm = T)); rmse.ratios.all.pct = rmse.ratios.all / mean(df$total20, na.rm = T) * 100

holder$bias[2] = bias.ratios.all
holder$bias.pct[2] = bias.ratios.all.pct
holder$rmse[2] = rmse.ratios.all
holder$rmse.pct[2] = rmse.ratios.all.pct
holder$Model[2] = 'Lagged Vote'
holder$Type[2] = 'Multiple Ratios'
holder$Training[2] = '2012-2016'
holder$Test[2] = '2016-2020'


bias.reg.all = mean(reg.pred20 - df$total20, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total20, na.rm = T) * 100
rmse.reg.all = sqrt(mean((reg.pred20 - df$total20) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total20, na.rm = T) * 100

bias.reg.all.train = mean(reg.pred20 - df$total16, na.rm = T); bias.reg.all.pct.train = bias.reg.all.train / mean(df$total16, na.rm = T) * 100
rmse.reg.all.train = sqrt(mean((reg.pred20 - df$total16) ^ 2, na.rm = T)); rmse.reg.all.pct.train = rmse.reg.all.train / mean(df$total16, na.rm = T) * 100


holder$bias[3] = bias.reg.all
holder$bias.pct[3] = bias.reg.all.pct
holder$rmse[3] = rmse.reg.all
holder$rmse.pct[3] = rmse.reg.all.pct
holder$Model[3] = 'Lagged Vote'
holder$Type[3] = 'WLS'
holder$Training[3] = '2012-2016'
holder$Test[3] = '2016-2020'
holder$bias.train[3] = bias.reg.all.train
holder$bias.pct.train[3] = bias.reg.all.pct.train
holder$rmse.train[3] = rmse.reg.all.train
holder$rmse.pct.train[3] = rmse.reg.all.pct.train



y = tapply(df$total20, as.character(df$state), sum, na.rm = T)
scatter_out = tibble(state =names(y),votes = as.vector(y), model = 'Lagged Vote', pred = reg.state.pred20, training = '2012-2016', test = '2016-2020')
write_csv(scatter_out,'output/lagged-vote-district-weighted-state-scatter.csv')




####
holder = holder %>%
  mutate(n_units_training = c(NA,NA, sum(n.units.state.train.1216, na.rm=T)),
         n_state_training = c(NA,NA, sum(!is.na(n.units.state.train.1216))),
         
         n_units_test = c(NA,NA, sum(n.units.state.test.1620,na.rm=T)),
         n_state_test = c(NA,NA, sum(!is.na(n.units.state.test.1620))))



write.csv(holder, 'results/lagged-vote-model-district-stats-weighted.csv')

