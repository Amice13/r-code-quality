## Author: Kabir Khanna and Jacob Brown
rm(list=ls())

# 2020 district-level data
df <- read.csv("data/districts_analysis.csv"); head(df); dim(df)


# demographic data
acs10 = read.csv('data/ACS_District_2010.csv')
acs14 = read.csv('data/ACS_District_2014.csv')
acs18 = read.csv('data/ACS_District_2018.csv')

names(acs10) = c('id', paste0(names(acs10)[2:length(names(acs10))],'_2010'))
names(acs14) = c('id', paste0(names(acs14)[2:length(names(acs14))],'_2014'))
names(acs18) = c('id', paste0(names(acs18)[2:length(names(acs18))],'_2018'))

st = read.csv('https://gist.githubusercontent.com/dantonnoriega/bf1acd2290e15b91e6710b6fd3be0a53/raw/11d15233327c8080c9646c7e1f23052659db251d/us-state-ansi-fips.csv')


df$cd.id = ifelse(nchar(df$CD)==1, paste0('0',df$CD),df$CD)
st$state=str_replace_all(st$stusps,' ','')
df = merge(df,st,by='state',all.x=T)
df$state.id = ifelse(nchar(df$st)==1,paste0('0',df$st),df$st)
df$id=paste0(df$state.id,df$cd.id)


df = merge(df,acs10,by='id',all.x=T)
df = merge(df,acs14,by='id',all.x=T)
df = merge(df,acs18,by='id',all.x=T)

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
# Model 2: regress total vote on lagged total vote

states <- unique(as.character(df$state))
states <- states[order(states)]; print(states); length(states)

reg20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 3)); rownames(reg20.fit) <- states; names(reg20.fit) <- c("intercept", "slope", "rmse")
reg12.fit <- reg16.fit <- reg20.fit

ratio20.fit <- as.data.frame(matrix(NA, nrow = length(states), ncol = 2)); rownames(ratio20.fit) <- states; names(ratio20.fit) <- c("ratio", "rmse")
ratio12.fit <- ratio16.fit <- ratio20.fit

reg.state.pred20 <- reg.state.pred20.train <-reg.out.rmse20 <- reg.out.bias20 <- rep(NA, length(states))
ratio.state.pred20 <- ratio.out.rmse20 <- ratio.out.bias20 <- rep(NA, length(states))
ratios.state.pred20 <- ratios.out.rmse20 <- ratios.out.bias20 <- rep(NA, length(states))

ratios.pred20 <- ratio.pred20 <- reg.pred20 <- reg.pred20.train <-rep(NA, nrow(df))

n.units.state.train.1216 <- rep(NA, length(states))
n.units.state.test.1620 <- rep(NA, length(states))


for (state in states) {


    if (sum(df$state == state & !is.na(df$total12) & 
            !is.na(df$total16) &
            !is.na(df$margin_2012) & 
             !is.na(df$reg16) & 
            !is.na(df$pct_married_2014) & 
            !is.na(df$pct_college_or_higher_2014) &
            !is.na(df$median_hh_income_2014) &
                   !is.na(df$pct_white_2014) &
                   !is.na(df$pct_black_2014) &
                   !is.na(df$pct_hispanic_2014) &
                   !is.na(df$pct_age_15_24_2014) &
                   !is.na(df$pct_age_25_34_2014) &
                   !is.na(df$pct_age_35_64_2014) &
                   !is.na(df$pct_age_65_up_2014) &
                   !is.na(df$tot_pop_2014)  ) > 1) {
      fit16.temp <- lm(total16 ~ total12 + reg16 + margin_2012 + pct_married_2014 + pct_college_or_higher_2014 + median_hh_income_2014 + pct_white_2014 + pct_black_2014 + pct_hispanic_2014 + pct_age_15_24_2014 + pct_age_25_34_2014 + pct_age_35_64_2014 + pct_age_65_up_2014 + tot_pop_2014, df[df$state == state, ], weights = reg16)
      n.units.state.train.1216[states == state] <- nrow(fit16.temp$model)
      
      reg16.fit$intercept[states == state] <- coef(fit16.temp)[1]
      reg16.fit$intercept.se[states == state] <- summary(fit16.temp)$coefficients['(Intercept)','Std. Error']
      
      reg16.fit$slope[states == state] <- coef(fit16.temp)[2]
      reg16.fit$slope.se[states == state] <- summary(fit16.temp)$coefficients['total12','Std. Error']
      reg16.fit$rmse[states == state] <- sqrt(mean((predict(fit16.temp) - df$total16[df$state == state & !is.na(df$total12) & 
                                                                                       !is.na(df$total16) &
                                                                                       !is.na(df$margin_2012) & 
                                                                                       !is.na(df$reg16) & 
                                                                                       !is.na(df$pct_married_2014) & 
                                                                                       !is.na(df$pct_college_or_higher_2014) &
                                                                                       !is.na(df$median_hh_income_2014) &
                                                                                       !is.na(df$pct_white_2014) &
                                                                                       !is.na(df$pct_black_2014) &
                                                                                       !is.na(df$pct_hispanic_2014) &
                                                                                       !is.na(df$pct_age_15_24_2014) &
                                                                                       !is.na(df$pct_age_25_34_2014) &
                                                                                       !is.na(df$pct_age_35_64_2014) &
                                                                                       !is.na(df$pct_age_65_up_2014) &
                                                                                       !is.na(df$tot_pop_2014)]) ^ 2))

      total.hat20 <- ifelse(!is.na(coef(fit16.temp)[1]),coef(fit16.temp)[1],0) + 
        ifelse(!is.na(coef(fit16.temp)[2] * df$total16[df$state == state]),coef(fit16.temp)[2] * df$total16[df$state == state],0) + 
        ifelse(!is.na(coef(fit16.temp)[3] * df$reg20[df$state == state]),coef(fit16.temp)[3] * df$reg20[df$state == state],0) +
        ifelse(!is.na(coef(fit16.temp)[4] * df$margin_2016[df$state == state]),coef(fit16.temp)[4] * df$margin_2016[df$state == state],0) +
        ifelse(!is.na(coef(fit16.temp)[5] * df$pct_married_2018[df$state == state ]),coef(fit16.temp)[5] * df$pct_married_2018[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[6] * df$pct_college_or_higher_2018[df$state == state ]),coef(fit16.temp)[6] * df$pct_college_or_higher_2018[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[7] * df$median_hh_income_2018[df$state == state ]),coef(fit16.temp)[7] * df$median_hh_income_2018[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[8] * df$pct_white_2018[df$state == state ]),coef(fit16.temp)[8] * df$pct_white_2018[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[9] * df$pct_black_2018[df$state == state ]),coef(fit16.temp)[9] * df$pct_black_2018[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[10] * df$pct_hispanic_2018[df$state == state ]),coef(fit16.temp)[10] * df$pct_hispanic_2018[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[11] * df$pct_age_15_24_2018[df$state == state ]),coef(fit16.temp)[11] * df$pct_age_15_24_2018[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[12] * df$pct_age_25_34_2018[df$state == state ]),coef(fit16.temp)[12] * df$pct_age_25_34_2018[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[13] * df$pct_age_35_64_2018[df$state == state ]),coef(fit16.temp)[13] * df$pct_age_35_64_2018[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[14] * df$pct_age_65_up_2018[df$state == state ]),coef(fit16.temp)[14] * df$pct_age_65_up_2018[df$state == state ],0)+
        ifelse(!is.na(coef(fit16.temp)[15] * df$tot_pop_2018[df$state == state ]),coef(fit16.temp)[15] * df$tot_pop_2018[df$state == state ],0)
      
      total.hat20.train <- ifelse(!is.na(coef(fit16.temp)[1]),coef(fit16.temp)[1],0) + 
        ifelse(!is.na(coef(fit16.temp)[2] * df$total12[df$state == state]),coef(fit16.temp)[2] * df$total12[df$state == state],0) + 
        ifelse(!is.na(coef(fit16.temp)[3] * df$reg16[df$state == state]),coef(fit16.temp)[3] * df$reg16[df$state == state],0) +
        ifelse(!is.na(coef(fit16.temp)[4] * df$margin_2012[df$state == state]),coef(fit16.temp)[4] * df$margin_2012[df$state == state],0) +
        ifelse(!is.na(coef(fit16.temp)[5] * df$pct_married_2014[df$state == state ]),coef(fit16.temp)[5] * df$pct_married_2014[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[6] * df$pct_college_or_higher_2014[df$state == state ]),coef(fit16.temp)[6] * df$pct_college_or_higher_2014[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[7] * df$median_hh_income_2014[df$state == state ]),coef(fit16.temp)[7] * df$median_hh_income_2014[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[8] * df$pct_white_2014[df$state == state ]),coef(fit16.temp)[8] * df$pct_white_2014[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[9] * df$pct_black_2014[df$state == state ]),coef(fit16.temp)[9] * df$pct_black_2014[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[10] * df$pct_hispanic_2014[df$state == state ]),coef(fit16.temp)[10] * df$pct_hispanic_2014[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[11] * df$pct_age_15_24_2014[df$state == state ]),coef(fit16.temp)[11] * df$pct_age_15_24_2014[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[12] * df$pct_age_25_34_2014[df$state == state ]),coef(fit16.temp)[12] * df$pct_age_25_34_2014[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[13] * df$pct_age_35_64_2014[df$state == state ]),coef(fit16.temp)[13] * df$pct_age_35_64_2014[df$state == state ],0) +
        ifelse(!is.na(coef(fit16.temp)[14] * df$pct_age_65_up_2014[df$state == state ]),coef(fit16.temp)[14] * df$pct_age_65_up_2014[df$state == state ],0)+
        ifelse(!is.na(coef(fit16.temp)[15] * df$tot_pop_2014[df$state == state ]),coef(fit16.temp)[15] * df$tot_pop_2014[df$state == state ],0)
      
        
      n.units.state.test.1620[states == state] <- sum(!is.na(total.hat20))
      
       reg.pred20[df$state == state] <- total.hat20
       reg.pred20.train[df$state == state] <- total.hat20.train
       
      reg.out.bias20[states == state] <- mean(total.hat20 - df$total20[df$state == state & !is.na(df$total16) & !is.na(df$total20)])
      reg.out.rmse20[states == state] <- sqrt(mean((total.hat20 - df$total20[df$state == state & !is.na(df$total16) & !is.na(df$total20)]) ^ 2))
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
                    Unit = 'District',
                    bias.train = rep(NA,1), 
                    bias.pct.train = rep(NA,1), 
                    rmse.train = rep(NA,1), 
                    rmse.pct.train = rep(NA,1))

# Plot district predictions and residuals vs. observed data


bias.reg.all = mean(reg.pred20 - df$total20, na.rm = T); bias.reg.all.pct = bias.reg.all / mean(df$total20, na.rm = T) * 100
rmse.reg.all = sqrt(mean((reg.pred20 - df$total20) ^ 2, na.rm = T)); rmse.reg.all.pct = rmse.reg.all / mean(df$total20, na.rm = T) * 100

bias.reg.all.train = mean(reg.pred20.train - df$total16, na.rm = T); bias.reg.all.pct.train = bias.reg.all.train / mean(df$total16, na.rm = T) * 100
rmse.reg.all.train = sqrt(mean((reg.pred20.train - df$total16) ^ 2, na.rm = T)); rmse.reg.all.pct.train = rmse.reg.all.train / mean(df$total16, na.rm = T) * 100


holder$bias[1] = bias.reg.all
holder$bias.pct[1] = bias.reg.all.pct
holder$rmse[1] = rmse.reg.all
holder$rmse.pct[1] = rmse.reg.all.pct
holder$Model[1] = 'Registration + Lagged Vote + Demographics + Lagged Margin'
holder$Type[1] = 'WLS'
holder$Training[1] = '2012-2016'
holder$Test[1] = '2016-2020'
holder$bias.train[1] = bias.reg.all.train
holder$bias.pct.train[1] = bias.reg.all.pct.train
holder$rmse.train[1] = rmse.reg.all.train
holder$rmse.pct.train[1] = rmse.reg.all.pct.train



##
holder = holder %>%
  mutate(n_units_training = c(sum(n.units.state.train.1216, na.rm=T)),
         n_state_training = c(sum(!is.na(n.units.state.train.1216))),
         
         n_units_test = c( sum(n.units.state.test.1620,na.rm=T)),
         n_state_test = c( sum(!is.na(n.units.state.test.1620))))




write.csv(holder, 'results/full-registration-lagged-vote-demographics-competition-model-district-stats-weighted.csv')
