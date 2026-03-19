rm(list=ls())

library(tidyverse)
df <- read.csv("data/districts_analysis.csv")

d <- read_csv('data/1976-2020-house.csv')%>%
  filter(year%in%c(2012,2016))%>%
  group_by(year,state_po,district, party)%>%
  dplyr::summarise(share=sum(candidatevotes,na.rm=T)/totalvotes[1])%>%
  group_by(year,state_po,district)%>%
  dplyr::summarize(rep_share = ifelse(any(party=='REPUBLICAN'), share[party=='REPUBLICAN'], NA),
                   dem_share = ifelse(any(party=='DEMOCRAT'), share[party=='DEMOCRAT'], NA))%>%
  mutate(rep_share = ifelse(is.na(rep_share), 0, rep_share),
         dem_share = ifelse(is.na(dem_share), 0, dem_share),
         margin = abs(rep_share-dem_share))%>%
  pivot_wider(id_cols = c('state_po','district'), names_from = 'year', values_from=rep_share:margin)%>%
  ungroup%>%
  mutate(state=state_po,
         CD=as.numeric(district))%>%
  select(-state_po,-district)%>%
  as.data.frame


df = merge(df,d,by=c('state','CD'),all.x=T)
head(df); dim(df)


states <- unique(as.character(df$state))
states <- states[order(states)]; print(states); length(states)

reg20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 3)); rownames(reg20.fit) <- states; names(reg20.fit) <- c("intercept", "slope", "rmse")
reg12.fit <- reg16.fit <- reg20.fit

ratio20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 2)); rownames(ratio20.fit) <- states; names(ratio20.fit) <- c("ratio", "rmse")
ratio12.fit <- ratio16.fit <- ratio20.fit

reg.state.pred20 <- reg.out.rmse20 <- reg.out.bias20 <- rep(NA, length(states))
ratio.state.pred20 <- ratio.out.rmse20 <- ratio.out.bias20 <- rep(NA, length(states))
ratios.state.pred20 <- ratios.out.rmse20 <- ratios.out.bias20 <- rep(NA, length(states))

ratios.pred20 <- ratio.pred20 <- reg.pred20 <-rep(NA, nrow(df))

n.units.state.train.1216 <- rep(NA, length(states))
n.units.state.test.1620 <- rep(NA, length(states))


for (state in states) {
  
  if (sum(df$state == state & !is.na(df$margin_2012) & !is.na(df$total16)) > 1) {
    fit16.temp <- lm(total16 ~ margin_2012+reg16, weights=reg16, df[df$state == state, ])
    n.units.state.train.1216[states == state] <- nrow(fit16.temp$model)
     reg16.fit$intercept[states == state] <- coef(fit16.temp)[1]
    reg16.fit$slope[states == state] <- coef(fit16.temp)[2]
    reg16.fit$rmse[states == state] <- sqrt(mean((predict(fit16.temp) - df$total16[df$state == state & !is.na(df$margin_2012) & !is.na(df$total16)]) ^ 2))

    total.hat20 <- coef(fit16.temp)[1] + coef(fit16.temp)[2] * df$margin_2016[df$state == state] + coef(fit16.temp)[3] * df$reg20[df$state == state]
    n.units.state.test.1620[states == state] <- sum(!is.na(total.hat20))
    
     reg.pred20[df$state == state] <- total.hat20
    reg.out.bias20[states == state] <- mean(total.hat20 - df$total20[df$state == state & !is.na(df$margin_2016) & !is.na(df$total20)])
    reg.out.rmse20[states == state] <- sqrt(mean((total.hat20 - df$total20[df$state == state & !is.na(df$margin_2016) & !is.na(df$total20)]) ^ 2))
    reg.state.pred20[states == state] <- sum(total.hat20)
    }
  }


# Difference in county-level ratios between years

bias.reg.all = mean(reg.pred20 - df$total20, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total20, na.rm = T) * 100
rmse.reg.all = sqrt(mean((reg.pred20 - df$total20) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total20, na.rm = T) * 100

holder = data.frame(bias = rep(NA,1), bias.pct = rep(NA,1), rmse = rep(NA,1), rmse.pct = rep(NA,1), Model = rep(NA,1), Training = rep(NA,1), Test = rep(NA,1), Unit = 'District')


holder$bias[1] = bias.reg.all
holder$bias.pct[1] = bias.reg.all.pct
holder$rmse[1] = rmse.reg.all
holder$rmse.pct[1] = rmse.reg.all.pct
holder$Model[1] = 'Lagged Margin + Registration'
holder$Type[1] = 'WLS'
holder$Training[1] = '2012-2016'
holder$Test[1] = '2016-2020'









holder = holder %>%
  mutate(n_units_training = c( sum(n.units.state.train.1216, na.rm=T)),
         n_state_training = c( sum(!is.na(n.units.state.train.1216))),
         
         n_units_test = c( sum(n.units.state.test.1620,na.rm=T)),
         n_state_test = c( sum(!is.na(n.units.state.test.1620))))




write.csv(holder, 'results/competition-registration-district-stats-weighted.csv')

