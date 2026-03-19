rm(list=ls())

library(tidyverse)

df <- read.csv("data/districts_analysis.csv"); head(df); dim(df)

# demographic data
acs12 = read.csv('data/ACS_District_2012.csv')
acs16 = read.csv('data/ACS_District_2016.csv')

names(acs12) = c('id', paste0(names(acs12)[2:length(names(acs12))],'_2012'))
names(acs16) = c('id', paste0(names(acs16)[2:length(names(acs16))],'_2016'))

st = read.csv('https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv')


df$cd.id = ifelse(nchar(df$CD)==1, paste0('0',df$CD),df$CD)
st$state=str_replace_all(st$stusps,' ','')
df = merge(df,st,by='state',all.x=T)
df$state.id = ifelse(nchar(df$st)==1,paste0('0',df$st),df$st)
df$id=paste0(df$state.id,df$cd.id)


df = merge(df,acs12,by='id',all.x=T)
df = merge(df,acs16,by='id',all.x=T)

head(df); dim(df)

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
  if (sum(df$state == state & !is.na(df$margin_2012) & !is.na(df$total16)) > 0) {
  ratio.temp = sum(df$total16[df$state == state & !is.na(df$total12) & !is.na(df$total16)]) /
    sum(df$margin_2012[df$state == state & !is.na(df$margin_2012) & !is.na(df$total16)])
  ratios.temp = (df$total16 / df$margin_2012)[df$state == state]

  total16.hat.temp <- ratio.temp * df$margin_2012[df$state == state]
  ratio16.fit$ratio[states == state] <- ratio.temp
  ratio16.fit$rmse[states == state] <- sqrt(mean((total16.hat.temp - df$total16[df$state == state & !is.na(df$margin_2012) & !is.na(df$total16)]) ^ 2))

  ratio.pred20.temp <- ratio.temp * df$margin_2016[df$state == state]
  ratio.pred20[df$state == state] <- ratio.pred20.temp
  ratio.out.bias20[states == state] <- mean(ratio.pred20.temp - df$total20[df$state == state & !is.na(df$margin_2016) & !is.na(df$total20)])
  ratio.out.rmse20[states == state] <- sqrt(mean((ratio.pred20.temp - df$total20[df$state == state & !is.na(df$margin_2016) & !is.na(df$total20)]) ^ 2))
  ratio.state.pred20[states == state] <- sum(ratio.pred20.temp)

  ratios.pred20.temp <- ratios.temp * df$margin_2016[df$state == state]
  ratios.pred20[df$state == state] <- ratios.pred20.temp
  ratios.out.bias20[states == state] <- mean(ratios.pred20.temp - df$total20[df$state == state & !is.na(df$margin_2016) & !is.na(df$total20)])
  ratios.out.rmse20[states == state] <- sqrt(mean((ratios.pred20.temp - df$total20[df$state == state & !is.na(df$margin_2016) & !is.na(df$total20)]) ^ 2))
  ratios.state.pred20[states == state] <- sum(ratios.pred20.temp)

  if (sum(df$state == state & !is.na(df$margin_2012) & !is.na(df$total16)) > 1) {
    fit16.temp <- lm(total16 ~ margin_2012 + tot_pop_2012, df[df$state == state, ])
    n.units.state.train.1216[states == state] <- nrow(fit16.temp$model)
    
    reg16.fit$intercept[states == state] <- coef(fit16.temp)[1]
    reg16.fit$slope[states == state] <- coef(fit16.temp)[2]
    reg16.fit$slope.se[states == state] <- summary(fit16.temp)$coefficients['margin_2012','Std. Error']
    
    reg16.fit$rmse[states == state] <- sqrt(mean((predict(fit16.temp) - df$total16[df$state == state & !is.na(df$margin_2012) & !is.na(df$total16)& !is.na(df$tot_pop_2012)]) ^ 2))
    reg16.fit$tot_pop[states == state] <- coef(fit16.temp)[3]
    if(!is.na(coef(fit16.temp)[3])){
      reg16.fit$tot_pop.se[states == state] <- summary(fit16.temp)$coefficients['tot_pop_2012','Std. Error']
    }
    total.hat20 <- coef(fit16.temp)[1] + coef(fit16.temp)[2] * df$margin_2016[df$state == state]  + ifelse(!is.na(coef(fit16.temp)[3] * df$tot_pop_2016[df$state == state]),coef(fit16.temp)[3] * df$tot_pop_2016[df$state == state],0)
    n.units.state.test.1620[states == state] <- sum(!is.na(total.hat20))
    
    reg.pred20[df$state == state] <- total.hat20
    reg.out.bias20[states == state] <- mean(total.hat20 - df$total20[df$state == state & !is.na(df$margin_2016) & !is.na(df$total20)])
    reg.out.rmse20[states == state] <- sqrt(mean((total.hat20 - df$total20[df$state == state & !is.na(df$margin_2016) & !is.na(df$total20)]) ^ 2))
    reg.state.pred20[states == state] <- sum(total.hat20)
    }
  }
}

out.m = data.frame(coef = c(reg16.fit$slope, reg16.fit$tot_pop),
                   se = c(reg16.fit$slope.se, reg16.fit$tot_pop.se),
                   variable = c(rep('Lagged Margin',length(reg16.fit$slope)), rep('Population',length(reg16.fit$tot_pop))),
                   model = 'Lagged Margin',
                   training = '2012-2016',
                   test = '2016-2020',
                   state = c(states,states)
)

write.csv(out.m, 'output/competition-model-district-coefficients.csv')


y = tapply(df$total20, df$state, sum, na.rm = T)
scatter_out = tibble(state =names(y),votes = as.vector(y), model = 'Competition', pred = reg.state.pred20, training = '2012-2016', test = '2016-2020')


write_csv(scatter_out,'output/competition-district-unweighted-state-scatter.csv')








holder = data.frame(bias = rep(NA,3), bias.pct = rep(NA,3), rmse = rep(NA,3), rmse.pct = rep(NA,3), Model = rep(NA,3), Weights = rep(F,3), Training = rep(NA,3), Test = rep(NA,3), Unit = 'District')

bias.ratio.all = mean(ratio.pred20 - df$total20, na.rm = T); bias.ratio.all.pct = bias.ratio.all / mean(df$total20, na.rm = T) * 100
rmse.ratio.all = sqrt(mean((ratio.pred20 - df$total20) ^ 2, na.rm = T)); rmse.ratio.all.pct = rmse.ratio.all / mean(df$total20, na.rm = T) * 100


holder$bias[1] = bias.ratio.all
holder$bias.pct[1] = bias.ratio.all.pct
holder$rmse[1] = rmse.ratio.all
holder$rmse.pct[1] = rmse.ratio.all.pct
holder$Model[1] = 'Lagged Margin'
holder$Type[1] = 'Ratio'
holder$Training[1] = '2012-2016'
holder$Test[1] = '2016-2020'

bias.ratios.all = mean(ratios.pred20 - df$total20, na.rm = T); bias.ratios.all.pct = bias.ratios.all / mean(df$total20, na.rm = T) * 100
rmse.ratios.all = sqrt(mean((ratios.pred20 - df$total20) ^ 2, na.rm = T)); rmse.ratios.all.pct = rmse.ratios.all / mean(df$total20, na.rm = T) * 100

holder$bias[2] = bias.ratios.all
holder$bias.pct[2] = bias.ratios.all.pct
holder$rmse[2] = rmse.ratios.all
holder$rmse.pct[2] = rmse.ratios.all.pct
holder$Model[2] = 'Lagged Margin'
holder$Type[2] = 'Multiple Ratios'
holder$Training[2] = '2012-2016'
holder$Test[2] = '2016-2020'

bias.reg.all = mean(reg.pred20 - df$total20, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total20, na.rm = T) * 100
rmse.reg.all = sqrt(mean((reg.pred20 - df$total20) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total20, na.rm = T) * 100

holder$bias[3] = bias.reg.all
holder$bias.pct[3] = bias.reg.all.pct
holder$rmse[3] = rmse.reg.all
holder$rmse.pct[3] = rmse.reg.all.pct
holder$Model[3] = 'Lagged Margin'
holder$Type[3] = 'OLS'
holder$Training[3] = '2012-2016'
holder$Test[3] = '2016-2020'



holder = holder %>%
  mutate(n_units_training = c(NA,NA, sum(n.units.state.train.1216, na.rm=T)),
         n_state_training = c(NA,NA, sum(!is.na(n.units.state.train.1216))),
         
         n_units_test = c(NA,NA, sum(n.units.state.test.1620,na.rm=T)),
         n_state_test = c(NA,NA, sum(!is.na(n.units.state.test.1620))))



write.csv(holder, 'results/competition-model-district-stats-unweighted.csv')

