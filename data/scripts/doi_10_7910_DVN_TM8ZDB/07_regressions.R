setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lfe)

dat = read.csv("../output/allstates_unusualness.csv")

dat = dat %>%
  mutate(unusual_precinct= (prop.d < 0.3 & party == "D") | (prop.d > 0.7 & party == "R"),
         unusual_precinct95 =  (prop.d < 0.05 & party == "D") | (prop.d > 0.95 & party == "R"),
         unusual_precinct90 =  (prop.d < 0.10 & party == "D") | (prop.d > 0.90 & party == "R"),
         unusual_precinct85 =  (prop.d < 0.15 & party == "D") | (prop.d > 0.85 & party == "R"),
         unusual_precinct80 =  (prop.d < 0.20 & party == "D") | (prop.d > 0.80 & party == "R"),
         unusual_precinct75 =  (prop.d < 0.25 & party == "D") | (prop.d > 0.75 & party == "R"),
         unusual_precinct65 =  (prop.d < 0.35 & party == "D") | (prop.d > 0.65 & party == "R"),
         outparty_donors = ifelse(party=="D", donorsright, donorsleft),
         inparty_donors = ifelse(party=="D", donorsleft, donorsright),
         inparty_votepct = ifelse(party=="D", DVOTE/(DVOTE + RVOTE), RVOTE/(DVOTE + RVOTE)),
         whitepct = white/pop,
         blackpct = black/pop,
         hispanicpct = hispanic/pop)

## Summary stats
mean(dat$unusual_precinct) # ~4%
table(dat$firms > 0, dat$unusual_precinct)
table(dat$donors200k > 0, dat$unusual_precinct)



m0 = felm(unusual_precinct ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop | state,
           data = dat)
summary(m0)


## Separate firms from rich voters, inparty voters, etc
m0a = felm(unusual_precinct ~ firms + topincome + whitepct + blackpct +
            hispanicpct + urban + pop | state,
          data = dat %>% filter(state %in% c("TX", "GA", "WI", "OH")))
summary(m0a)

m0b = felm(unusual_precinct ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop | state,
           data = dat %>% filter(state %in% c("OR", "MD", "MA", "NM")))
summary(m0b)

m0c = felm(unusual_precinct ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop | state,
           data = dat %>% filter(state %in% c("NC", "PA", "VA", "MN")))
summary(m0c)

m0d = felm(unusual_precinct ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop | state,
           data = dat %>% filter(state %in% c("AZ", "IA")))
summary(m0d)

stargazer::stargazer(m0a,m0b,m0c,m0d)


m0e = felm(unusual_precinct ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop | state,
           data = dat %>% filter(state %in% c( "GA", "WI", "OH")))
summary(m0e)
m0f = felm(unusual_precinct ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop | state,
           data = dat %>% filter(state %in% c("TX")))
summary(m0f)





## Separate rich voters from donors
m1 = felm(unusual_precinct ~ donors200k + topincome + whitepct + blackpct +
                            hispanicpct + urban + pop + inparty_votepct | state,
          data = dat)
summary(m1)


## Do parties prefer their own donors
m2 = felm(unusual_precinct ~  inparty_donors + outparty_donors + donorsswing +
                              topincome + whitepct + blackpct + hispanicpct + 
                              urban + pop + inparty_votepct |  state,
          data = dat)
summary(m2)


## Separate rich voters from donors
m3 = felm(unusual_precinct ~ donors200k + firms + topincome + whitepct +
                            blackpct + hispanicpct + urban + pop +
                            inparty_votepct | state,
          data = dat)
summary(m3)


## Do parties prefer their own donors
m4 = felm(unusual_precinct ~  inparty_donors + outparty_donors + donorsswing +
                              firms + topincome + whitepct + blackpct +
                              hispanicpct + urban + pop + inparty_votepct |  state,
          data = dat)
summary(m4)


## Is there variation by party
m5 = felm(unusual_precinct ~  inparty_donors + outparty_donors + donorsswing +
            firms*party + topincome + whitepct + blackpct +
            hispanicpct + urban + pop + inparty_votepct + party |  state,
          data = dat)
summary(m5)
stargazer::stargazer(m5)

### How do these p-values vary by state? We really only have N in a few states
stateabb = unique(dat$state)
out = list()
for(i in 1:length(stateabb)){
  tmp = felm(unusual_precinct ~  inparty_donors + outparty_donors + donorsswing +
               firms*party + topincome + whitepct + blackpct +
               hispanicpct + urban + pop + inparty_votepct + party |  state,
            data = dat %>% filter(state == stateabb[i]))
  tmp2 = broom::tidy(tmp)
  out[[i]] = tmp2 %>% select(term, estimate, std.error, p.value) %>%
    filter(term %in% c("firms", "donorsswing", "inparty_donors", "outparty_donors", "firms:partyR")) %>%
    mutate(state = stateabb[i])
}

out2 = do.call(rbind, out)
out2 = out2 %>% mutate(ci.lower = estimate - 1.97*std.error,
                       ci.upper = estimate + 1.97*std.error)



### Data visualization:
### The preference for inparty vs swing donors changes as unusualness changes
### cf more unusualness -> more swing donors

unusualness_thresholds = function(x){
  ## Define unusual precincts
  dat = dat %>%
    mutate(unusual_precinct= (prop.d < x & party == "D") | (prop.d > (1-x) & party == "R"))
  ## Run a regression
  tmp = felm(unusual_precinct ~  inparty_donors + outparty_donors + donorsswing +
              firms + topincome + whitepct + blackpct +
              hispanicpct + urban + pop + inparty_votepct |  state,
            data = dat)
  tmp2 = broom::tidy(tmp)
  ## Store the results
  out = tmp2 %>% select(term, estimate, std.error, p.value) %>%
    filter(term %in% c("donorsswing", "inparty_donors", "outparty_donors", "firms:partyR")) %>%
    mutate(thresh = x)
  return(out)
}

dat2 = lapply(X = seq(0.05,0.35, by=0.05), FUN = unusualness_thresholds)
dat2 = do.call(rbind, dat2)
dat2 = dat2 %>% mutate(confint.upper = estimate + 1.97*std.error,
                       confint.lower = estimate - 1.97*std.error)


ggplot(dat2, aes(x = thresh, y = estimate, color = term)) +
  geom_line(lwd=2) +
  geom_errorbar(aes(ymin = confint.lower, ymax = confint.upper)) + 
  mediocrethemes::theme_mediocre()



## Robustness to threshold

m95 = felm(unusual_precinct95 ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop,
           data = dat %>% filter(state %in% c("TX", "GA", "WI", "OH"))) %>%
  broom::tidy()
m90 = felm(unusual_precinct90 ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop,
           data = dat %>% filter(state %in% c("TX", "GA", "WI", "OH"))) %>%
  broom::tidy()
m85 = felm(unusual_precinct85 ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop,
           data = dat %>% filter(state %in% c("TX", "GA", "WI", "OH"))) %>%
  broom::tidy()
m80 = felm(unusual_precinct80 ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop,
           data = dat %>% filter(state %in% c("TX", "GA", "WI", "OH"))) %>%
  broom::tidy()
m75 = felm(unusual_precinct75 ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop,
           data = dat %>% filter(state %in% c("TX", "GA", "WI", "OH"))) %>%
  broom::tidy()
m70 = felm(unusual_precinct ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop,
           data = dat %>% filter(state %in% c("TX", "GA", "WI", "OH"))) %>%
  broom::tidy()
m65 = felm(unusual_precinct65 ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop,
           data = dat %>% filter(state %in% c("TX", "GA", "WI", "OH"))) %>%
  broom::tidy()

thresh_plotdf = rbind(m95[2,],m90[2,],m85[2,],m80[2,],
                      m75[2,],m70[2,],m65[2,])

thresh_plotdf = thresh_plotdf %>%
  mutate(thresh = seq(95,65,by=-5),
         upper = estimate + 1.97*std.error,
         lower = estimate - 1.97*std.error)

thresh_p1 = ggplot(thresh_plotdf, aes(x = thresh, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) + theme_bw() + 
  xlab("Unusualness Cutoff") + ylab("Point Estimate") + 
  geom_hline(yintercept=0, linetype="dashed")
thresh_p1



### Experimenting with other regressions
library(mlogit)
library(nnet)

## Table 1: Dichotomize firms
dat2 = dat %>% mutate(firms = as.numeric(firms>0),
                     topincome = log(topincome+0.001))
dat2$unusual_precinct = NA
dat2$unusual_precinct[dat2$prop.d > 0.7 & dat2$party == "R"] = 0
dat2$unusual_precinct[dat2$prop.d < 0.3 & dat2$party == "D"] = 2
dat2$unusual_precinct[is.na(dat2$unusual_precinct)] = 1
dat2$unusual_precinct = factor(dat2$unusual_precinct)
dat2$unusual_precinct = relevel(dat2$unusual_precinct, ref = 2)
table(dat2$unusual_precinct)


m0a = multinom(unusual_precinct ~ firms + topincome + whitepct + blackpct +
             hispanicpct + urban + pop + state,
           data = dat2 %>% filter(state %in% c("TX", "GA", "WI", "OH")))
broom::tidy(m0a)[c(2,13),]


m0a2 = multinom(unusual_precinct ~ firms + topincome + whitepct + blackpct +
                 hispanicpct + urban + pop ,
               data = dat2 %>% filter(state %in% c("TX")))
broom::tidy(m0a2)[c(2,10),]


m0a3 = multinom(unusual_precinct ~ firms + topincome + whitepct + blackpct +
                 hispanicpct + urban + pop + state,
               data = dat2 %>% filter(state %in% c( "GA", "WI", "OH")))
broom::tidy(m0a3)[c(2,12),]




m0b = multinom(unusual_precinct ~ firms + topincome + whitepct + blackpct +
               hispanicpct + urban + pop + state,
             data = dat2 %>% filter(state %in% c("OR", "MD", "MA", "NM")))
broom::tidy(m0b)

m0c = multinom(unusual_precinct ~ firms + topincome + whitepct + blackpct +
               hispanicpct + urban + pop + state,
             data = dat2 %>% filter(state %in% c("NC", "PA", "VA", "MN")))
broom::tidy(m0c)

m0d = multinom(unusual_precinct ~ firms + topincome + whitepct + blackpct +
               hispanicpct + urban + pop + state,
             data = dat2 %>% filter(state %in% c("AZ", "IA")))
broom::tidy(m0d)

stargazer::stargazer(m0a,m0b,m0c,m0d)




## 