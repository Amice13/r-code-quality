# R CODE FOR SUPPLEMENTARY APPENDICES "WAR POWER THROUGH RESTRAINT"

# leader risk score - drawn from Horowitz, Ellis, & Stam monadic LEAD data; 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SYZZEY

# presidential support summary - VoteView;
# https://voteview.com/articles/presidential_support_scores

## SET WORKING DIRECTORY
# setwd()

## PACKAGES
library(dplyr)
library(ggplot2)
library(haven)
library(sandwich)
library(lmtest)
library(foreign)

## DISPOSITIONAL RISK SCORES
dat <- read_dta("WhyLeadersFightMonadicReplication_updated.dta")

# fit logit model
mod <- glm(cwinit ~ milnoncombat + combat + rebel + warwin + warloss +
             rebelwin + rebelloss + leveledu + age + teacher +
             journalism + law + medicine + religion + activist +
             careerpolitician + creative + business + aristocratlandowner +
             police + militarycareer + scienceeng + bluecollar + gender +
             totalspouses + married + marriedinpower + divorced + childtotal +
             parstability + illegit + royalty + orphanbinary + officetenure1000 +
             yearssincemidinit + y2 + y3, dat, family = binomial(link = "logit"))

# remove NA cases from dat, to merge in risk predictions
dat2 <- dat[complete.cases(dat$milnoncombat, dat$combat, dat$rebel, dat$warwin, dat$warloss,
                           dat$rebelwin, dat$rebelloss, dat$leveledu, dat$age, dat$teacher,
                           dat$journalism, dat$law, dat$medicine, dat$religion, dat$activist,
                           dat$careerpolitician, dat$creative, dat$business, dat$aristocratlandowner, 
                           dat$police, dat$militarycareer, dat$scienceeng, dat$bluecollar, dat$gender,
                           dat$totalspouses, dat$married, dat$marriedinpower, dat$divorced, dat$childtotal,
                           dat$parstability, dat$illegit, dat$royalty, dat$orphanbinary, dat$officetenure1000,
                           dat$yearssincemidinit, dat$y2, dat$y3), ]

# generate risk prediction
dat2$leaderrisk <- predict(mod, type = "response")

# calculate average leader risk score by US president
tab2 <- dat2 %>% filter(year > 1944, ccode == 2) %>% dplyr::select(year, leadername, leaderrisk)

# calculate percentile upon taking office; use score from first year of presidency
percentile <- ecdf(dat2$leaderrisk)

# Truman
100*percentile(dat2[dat2$leadername == "Truman" & dat2$year == 1945, c("leaderrisk")])

# Eisenhower
100*percentile(dat2[dat2$leadername == "Dwight Eisenhower" & dat2$year == 1953, c("leaderrisk")]) 

# JFK
100*percentile(dat2[dat2$leadername == "John F. Kennedy" & dat2$year == 1961, c("leaderrisk")]) 

# LBJ (There are two entries for LBJ in 1964; I use the average of the risk socres)
100*percentile(mean(dat2[dat2$leadername == "Lyndon Johnson" & dat2$year == 1964, c("leaderrisk")]$leaderrisk))

# Nixon
100*percentile(dat2[dat2$leadername == "Nixon" & dat2$year == 1969, c("leaderrisk")])

# Ford
100*percentile(dat2[dat2$leadername == "Gerald Ford" & dat2$year == 1974, c("leaderrisk")]) 

# Carter
100*percentile(dat2[dat2$leadername == "Carter" & dat2$year == 1977, c("leaderrisk")]) 

# Reagan (There are two entries for Reagan in 1981; I use the average of the risk socres)
100*percentile(mean(dat2[dat2$leadername == "Ronald Reagan" & dat2$year == 1981, c("leaderrisk")]$leaderrisk))

# George HW Bush
100*percentile(dat2[dat2$leadername == "Bush" & dat2$year == 1989, c("leaderrisk")]) 

# Clinton (There are two entries for Clinton in 1993; I use the average of the risk socres)
100*percentile(mean(dat2[dat2$leadername == "William Clinton" & dat2$year == 1993, c("leaderrisk")]$leaderrisk)) 


## SITUATIONAL VOTEVIEW SCORES
sdat <- read.csv("presidential_support_summary.csv")

ggplot(sdat %>% filter(year > 1944, year < 2001), 
       aes(x = year, y = score, color = chamber)) +
  geom_line() +
  geom_hline(yintercept=50, linetype="dashed", 
             color = "black") +
  ggtitle("Presidential Support Scores (Voteview)") +
  theme_bw()


