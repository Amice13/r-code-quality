## Analysis Code to replicate appendix for:
## "Happiness and Surprise are associated with worse truth discernment of COVID-19 headlines 
##   among social media users in Nigeria"

### load packages and functions

library(estimatr, quietly = TRUE)
library(foreign, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(reshape2, quietly = TRUE)
library(gtools, quietly = TRUE)
library(lme4)
library(stringr, quietly = TRUE)
library(patchwork, quietly = TRUE)
#devtools::install_github("haozhu233/kableExtra")
library(kableExtra, quietly = TRUE)
library(stargazer, quietly = TRUE)
library(CBPS, quietly = TRUE)
library(sp, quietly = TRUE) 
library(spdep, quietly = TRUE)
library(rgdal, quietly = TRUE)
library(maps, quietly = TRUE)
library(mapdata, quietly = TRUE)
library(maptools, quietly = TRUE)
library(gpclib, quietly=TRUE)
library(ggmap, quietly = TRUE)
library(naijR)
library(doMC, quietly = TRUE)
library(texreg, quietly = TRUE)
library(haven, quietly = TRUE)


### set working directory
#setwd()

rm(list=ls())

### load data
load("Nigeria_COVIDmisinfo.RData")

# merge long and unique datasets
ng.all <- merge(ng.long, ng.uni, by = "respID")


###############
### TABLE 1 ###
###############
descriptlabels <-  c("Female", 
                     "Age",
                     "Some university",
                     "Urban", 
                     "Employed",
                     "Christian", 
                     "Religiosity",
                     "Voted",
                     "APC supporter",
                     "Daily FB user")

ngdescript <- subset(ng.uni, select = c("female", 
                                        "age",
                                        "someuni",
                                        "urban", 
                                        "job_income", 
                                        "christ", 
                                        "religiosity",
                                        "voted",
                                        "APC_party",
                                        "usefbdaily"))

stargazer(as.data.frame(ngdescript),
          title = "Descriptive statistics of study respondents",
          median = TRUE,
          digits=2, 
          digits.extra=2,
          covariate.labels = descriptlabels,
          font.size = "small",
          label = "tab:sumstats")


################
### FIGURE 1 ###
################

ng.uni$state2 <- ifelse(ng.uni$state!=1, ng.uni$state-2,1)

tomap <- as.data.frame(table(ng.uni$state2))
tomap$pct <- tomap$Freq/485*100

states.df <- as.data.frame(states())

tomap2 <- cbind(tomap, states.df[,"states()"][match(tomap$Var1, rownames(states.df))])
names(tomap2)[4] <- "state_name"

noppl <- states()[!states() %in% tomap2$state_name]

noppl.dat <- as.data.frame(matrix(0,6,2))
names(noppl.dat) <- names(tomap2)[2:3]
noppl.dat$state_name <- noppl

tomap3 <- rbind(tomap2[,c(2:4)], noppl.dat)
tomap3 <- tomap3[order(tomap3$state_name),]

bb <- c(0,0.1,1,5,20,30)

map_ng(region = states(), x = tomap3$pct, breaks = bb, 
       show.text =  FALSE,
       #title = "Percent of Respondents from each State in Nigeria", 
       leg.title = "Percent of respondents",
       col=4)

################
### FIGURE 2 ###
################

ggplot(ng.uni, aes(x=thinkFtrue))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  xlab("Total number of false headlines believed to be true") +
  ylab("Percent of respondents") + 
  scale_x_continuous(breaks = c(0:5)) + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), stat= "count", vjust = -.5, size = 5) +
  theme(text = element_text(size=14),
        axis.text = element_text(size=14))


################
### TABLE 2 ###
################

neutral.b <- lmer(belief ~ 1 + neutralC*trueC + (1 |respID) + (1 + neutralC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

neutral.c <- lmer(click ~ 1 + neutralC*trueC + (1 |respID) + (1 + neutralC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

neutral.s <- lmer(share ~ 1 + neutralC*trueC + (1 |respID) + (1 + neutralC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

stargazer(neutral.b, neutral.c, neutral.s,
          title = "Association between neutral (no emotion) and belief, click, and share",
          #column.labels = c("belief","click","share"),
          covariate.labels = c("neutral","true","neutral:true"),
          digits=2, 
          model.numbers = FALSE,
          single.row = FALSE,
          font.size = "small",
          label = "tab:main_belief",
          no.space = TRUE,
          omit.stat=c("LL","ser","f","aic","bic")
)

################
### TABLE 3  ###
################

happy.b  <- lmer(belief ~ 1 + happyC*trueC + (1 |respID) + (1 + happyC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

happy.c  <- lmer(click ~ 1 + happyC*trueC + (1 |respID) + (1 + happyC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

happy.s  <- lmer(share ~ 1 + happyC*trueC + (1 |respID) + (1 + happyC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 


stargazer(happy.b, happy.c, happy.s,
          #style = "apsr",
          title = "Association between happy and belief, click, and share",
          #column.labels = c("belief","click","share"),
          covariate.labels = c("happy","true","happy:true"),
          digits=2, 
          model.numbers = FALSE,
          single.row = FALSE,
          font.size = "small",
          label = "tab:main_belief_happy",
          no.space = TRUE,
          omit.stat=c("LL","ser","f","aic","bic")
)


################
### TABLE 4  ###
################

surprise.b  <- lmer(belief ~ 1 + surpriseC*trueC + (1 |respID) + (1 |headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

surprise.c  <- lmer(click ~ 1 + surpriseC*trueC + (1 |respID) + (1 |headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

surprise.s  <- lmer(share ~ 1 + surpriseC*trueC + (1 |respID) + (1 |headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 


stargazer(surprise.b, surprise.c, surprise.s,
          title = "Association between surprise and belief, click, and share",
          covariate.labels = c("surprise","true","surprise:true"),
          digits=2, 
          model.numbers = FALSE,
          single.row = FALSE,
          font.size = "small",
          label = "tab:main_belief_surprise",
          no.space = TRUE,
          omit.stat=c("LL","ser","f","aic","bic")
)



################
### TABLE 5  ###
################

anger.b  <- lmer(belief ~ 1 + angerC*trueC + (1 |respID) + (1 + angerC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

anger.c  <- lmer(click ~ 1 + angerC*trueC + (1 |respID) + (1 + angerC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

anger.s  <- lmer(share ~ 1 + angerC*trueC + (1 |respID) + (1 + angerC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 


stargazer(anger.b, anger.c, anger.s,
          #style = "apsr",
          title = "Association between anger and belief, click, and share",
          #column.labels = c("belief","click","share"),
          covariate.labels = c("anger","true","anger:true"),
          digits=2, 
          model.numbers = FALSE,
          single.row = FALSE,
          font.size = "small",
          label = "tab:main_belief_anger",
          no.space = TRUE,
          omit.stat=c("LL","ser","f","aic","bic")
)



################
### TABLE 6  ###
################

fear.b  <- lmer(belief ~ 1 + fearC*trueC + (1 |respID) + (1 + fearC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

fear.c  <- lmer(click ~ 1 + fearC*trueC + (1 |respID) + (1 + fearC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

fear.s  <- lmer(share ~ 1 + fearC*trueC + (1 |respID) + (1 + fearC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 


stargazer(fear.b, fear.c, fear.s,
          #style = "apsr",
          title = "Association between fear and belief, click, and share",
          #column.labels = c("belief","click","share"),
          covariate.labels = c("fear","true","fear:true"),
          digits=2, 
          model.numbers = FALSE,
          single.row = FALSE,
          font.size = "small",
          label = "tab:main_belief_fear",
          no.space = TRUE,
          omit.stat=c("LL","ser","f","aic","bic")
)


################
### TABLE 7  ###
################

sad.b  <- lmer(belief ~ 1 + sadC*trueC + (1 |respID) + (1 + sadC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

sad.c  <- lmer(click ~ 1 + sadC*trueC + (1 |respID) + (1 + sadC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

sad.s  <- lmer(share ~ 1 + sadC*trueC + (1 |respID) + (1 + sadC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 


stargazer(sad.b, sad.c, sad.s,
          #style = "apsr",
          title = "Association between sad and belief, click, and share",
          #column.labels = c("belief","click","share"),
          covariate.labels = c("sad","true","sad:true"),
          digits=2, 
          model.numbers = FALSE,
          single.row = FALSE,
          font.size = "small",
          label = "tab:main_belief_sad",
          no.space = TRUE,
          omit.stat=c("LL","ser","f","aic","bic")
)

################
### TABLE 7  ###
################

disgust.b  <- lmer(belief ~ 1 + disgustC*trueC + (1 |respID) + (1 + disgustC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

disgust.c  <- lmer(click ~ 1 + disgustC*trueC + (1 |respID) + (1 + disgustC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

disgust.s  <- lmer(share ~ 1 + disgustC*trueC + (1 |respID) + (1 + disgustC|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 


stargazer(disgust.b, disgust.c, disgust.s,
          #style = "apsr",
          title = "Association between disgust and belief, click, and share",
          #column.labels = c("belief","click","share"),
          covariate.labels = c("disgust","true","disgust:true"),
          digits=2, 
          model.numbers = FALSE,
          single.row = FALSE,
          font.size = "small",
          label = "tab:main_belief_disgust",
          no.space = TRUE,
          omit.stat=c("LL","ser","f","aic","bic")
)

####################
### TABLE 9 + 10 ###
####################

# replace NAs for emotional strength --> 0 (emotion not felt)
ng.long <- ng.long %>% mutate(., across(starts_with("emostrength"), ~ifelse(is.na(.x),0,.x)))

# belief
happy  <- lmer(belief ~ 1 + emostrengthhappy*trueC + (1 |respID) + (1 + emostrengthhappy|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

surprise  <- lmer(belief ~ 1 + emostrengthsurprise*trueC + (1 |respID) + (1 |headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

# click
happy.c  <- lmer(click ~ 1 + emostrengthhappy*trueC + (1 |respID) + (1 + emostrengthhappy|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

surprise.c  <- lmer(click ~ 1 + emostrengthsurprise*trueC + (1 |respID) + (1 |headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

# share
happy.s  <- lmer(share ~ 1 + emostrengthhappy*trueC + (1 |respID) + (1 + emostrengthhappy|headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

surprise.s  <- lmer(share ~ 1 + emostrengthsurprise*trueC + (1 |respID) + (1 |headline), data=ng.long, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit) 

# TAB 9 
stargazer(happy, happy.c, happy.s,
          title = "Strength of emotion and outcomes: Happiness",
          digits=2, 
          covariate.labels = c("happy strength","true","happy strength:true"),
          font.size = "small",
          label = "tab:emostrength_happy",
          no.space = TRUE)

# TAB 10
stargazer(surprise, surprise.c, surprise.s,
          title = "Strength of emotion and outcomes: Surprise",
          digits=2, 
          covariate.labels = c("surprise strength","true","surprise strength:true"),
          font.size = "small",
          label = "tab:emostrength_surprise",
          no.space = TRUE)


#####################
### TABLE 11 - 13 ###
#####################

# belief w/controls
neutral.control <- lmer(belief ~ 1 + neutralC*trueC + (1 |respID) + (1 + neutralC|headline) + age + female + edu + someuse  + APC_party, data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

happy.control <- lmer(belief ~ 1 + happyC*trueC + (1 |respID) + (1 + happyC|headline) + age + female + edu + someuse  + APC_party, data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

surprise.control <- lmer(belief ~ 1 + surpriseC*trueC + (1 |respID) + (1 |headline) + age + female + edu + someuse  + APC_party, data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

# click w/controls
neutral.control.c <- lmer(click ~ 1 + neutralC*trueC + (1 |respID) + (1 + neutralC|headline) + age + female + edu + someuse  + APC_party, data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

happy.control.c <- lmer(click ~ 1 + happyC*trueC + (1 |respID) + (1 + happyC|headline) + age + female + edu + someuse  + APC_party, data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

surprise.control.c <- lmer(click ~ 1 + surpriseC*trueC + (1 |respID) + (1 |headline) + age + female + edu + someuse  + APC_party, data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

# share w/controls
neutral.control.s <- lmer(share ~ 1 + neutralC*trueC + (1 |respID) + (1 + neutralC|headline) + age + female + edu + someuse + APC_party, data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

happy.control.s <- lmer(share ~ 1 + happyC*trueC + (1 |respID) + (1 + happyC|headline) + age + female + edu + someuse + APC_party, data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

surprise.control.s <- lmer(share ~ 1 + surpriseC*trueC + (1 |respID) + (1 |headline) + age + female + edu + someuse + APC_party, data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

# TAB 11
stargazer(neutral.control, neutral.control.c, neutral.control.s,
          title = "Predicting outcomes with controls: Neutral",
          digits=2, 
          covariate.labels = c("neutral","true","age","female","education","social media use","APC","neutral:true"),
          font.size = "small",
          label = "tab:controls_neutral",
          no.space = TRUE)

# TAB 12
stargazer(happy.control, happy.control.c, happy.control.s,
          title = "Predicting outcomes with controls: Happiness",
          digits=2,
          covariate.labels = c("happy","true","age","female","education","social media use","APC","happy:true"),
          font.size = "small",
          label = "tab:controls_happy",
          no.space = TRUE)

# TAB 13
stargazer(surprise.control, surprise.control.c, surprise.control.s,
          title = "Predicting outcomes with controls: Surprise",
          digits=2, 
          covariate.labels = c("surprise","true","age","female","education","social media use","APC","surprise:true"),
          font.size = "small",
          label = "tab:controls_surprise",
          no.space = TRUE)


################
### TABLE 14 ###
################


neutral.b.apc <- lmer(belief ~ 1 + neutralC*trueC*APC_party + (1 |respID) + (1 + neutralC|headline), data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

neutral.c.apc <- lmer(click ~ 1 + neutralC*trueC*APC_party + (1 |respID) + (1 + neutralC|headline), data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

neutral.s.apc <- lmer(share ~ 1 + neutralC*trueC*APC_party + (1 |respID) + (1 + neutralC|headline), data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

stargazer(neutral.b.apc, neutral.c.apc, neutral.s.apc,
          title = "Interacting support for governing party with emotional reaction and headline veracity",
          covariate.labels = c("neutral","true","APC","neutral:true","neutral:APC","true:APC","neutral:true:APC"),
          digits=2, 
          model.numbers = FALSE,
          single.row = FALSE,
          font.size = "small",
          label = "tab:APC_int",
          no.space = TRUE,
          omit.stat=c("LL","ser","f","aic","bic")
)

################
### TABLE 15 ###
################

happy.b.apc <- lmer(belief ~ 1 + happyC*trueC*APC_party + (1 |respID) + (1 + happyC|headline), data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

happy.c.apc <- lmer(click ~ 1 + happyC*trueC*APC_party + (1 |respID) + (1 + happyC|headline), data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

happy.s.apc <- lmer(share ~ 1 + happyC*trueC*APC_party + (1 |respID) + (1 + happyC|headline), data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

stargazer(happy.b.apc, happy.c.apc, happy.s.apc,
            title = "Interacting support for governing party with happy and headline veracity",
            covariate.labels = c("happy","true","APC","happy:true","happy:APC","true:APC","happy:true:APC"),
            digits=2, 
            model.numbers = FALSE,
            single.row = FALSE,
            font.size = "small",
            label = "tab:APC_int_happy",
            no.space = TRUE,
            omit.stat=c("LL","ser","f","aic","bic")
  )


################
### TABLE 16 ###
################

surprise.b.apc <- lmer(belief ~ 1 + surpriseC*trueC*APC_party + (1 |respID) + (1 |headline), data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

surprise.c.apc <- lmer(click ~ 1 + surpriseC*trueC*APC_party + (1 |respID) + (1 |headline), data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

surprise.s.apc <- lmer(share ~ 1 + surpriseC*trueC*APC_party + (1 |respID) + (1 |headline), data=ng.all, control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

stargazer(surprise.b.apc, surprise.c.apc, surprise.s.apc,
          title = "Interacting support for governing party with surprise and headline veracity",
          covariate.labels = c("surprise","true","APC","surprise:true","surprise:APC","true:APC","surprise:true:APC"),
          digits=2, 
          model.numbers = FALSE,
          single.row = FALSE,
          font.size = "small",
          label = "tab:APC_int_surprise",
          no.space = TRUE,
          omit.stat=c("LL","ser","f","aic","bic")
)

################
### TABLE 17 ###
################

ng.uni <- ng.uni %>% 
  mutate(trust_radio = na_if(trust_radio, -999)) %>% 
  mutate(trust_tv = na_if(trust_tv, -999)) %>% 
  mutate(trust_some = na_if(trust_some, -999)) %>% 
  mutate(media_trust = mean(c(trust_radio,trust_tv,trust_some,na.rm=T)))

# trust models
tv.trust <- lm(trust_tv ~ APC_party, data = ng.uni)
rad.trust <- lm(trust_radio ~ APC_party, data = ng.uni)
sm.trust <- lm(trust_some ~ APC_party, data = ng.uni)

stargazer(tv.trust, rad.trust, sm.trust,
          dep.var.labels=c("T.V.","Radio","Social Media"),
          title = "Correlation between support for governing party and trust in different media",
          covariate.labels = c("APC"),
          digits=2, 
          model.numbers = FALSE,
          single.row = FALSE,
          font.size = "small",
          label = "tab:APC_trust",
          no.space = TRUE,
          omit.stat=c("LL","ser","f","aic","bic")
)


###################
### FIGURES 3-5 ###
###################

headlines <- as.factor(sort(unique(ng.long$headline)))

neutral.est <- c()
neutral.se <- c()

happy.est <- c()
happy.se <- c()

surprise.est <- c()
surprise.se <- c()

for(i in 1:length(headlines)){
  neutral1 <- lmer(belief ~ neutralC*trueC + (1 |respID) + (1 + neutralC|headline),
                   data=ng.long[ng.long$headline!=as.character(headlines[i]),], #family="binomial",
                   control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)
  
  happy1 <- lmer(belief ~ happyC*trueC + (1 |respID) + (1 + happyC|headline),
                 data=ng.long[ng.long$headline!=unique(ng.long$headline)[i],],
                 control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)
  
  
  surprise1 <- lmer(belief ~ surpriseC*trueC + (1 |respID) + (1 |headline),
                    data=ng.long[ng.long$headline!=unique(ng.long$headline)[i],],
                    control=lmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit )
  
  
  neutral.est[i] <- summary(neutral1)$coefficients[4,1]
  neutral.se[i] <- summary(neutral1)$coefficients[4,2]
  
  happy.est[i] <- summary(happy1)$coefficients[4,1]
  happy.se[i] <- summary(happy1)$coefficients[4,2]
  
  surprise.est[i] <- summary(surprise1)$coefficients[4,1]
  surprise.se[i] <- summary(surprise1)$coefficients[4,2]
}


neutraltab <- as.data.frame(cbind(seq(1:10),neutral.est,neutral.se))
happytab <- as.data.frame(cbind(seq(1:10),happy.est,happy.se))
surprisetab <- as.data.frame(cbind(seq(1:10),surprise.est,surprise.se))

names(neutraltab) = names(happytab) = names(surprisetab) = c ("headline_out","est","se")

headlines <- gsub("_","\n",headlines)

neutral.out <- ggplot(neutraltab,aes(x=headline_out,y=est))+
  geom_point(size=2) +
  geom_errorbar(aes(ymin= est - 1.96*se, ymax= est + 1.96*se),width = .2) +
  #ggtitle("Neutral*true estimates - leave one headline out") +
  scale_x_continuous(breaks=c(1:10),labels=headlines) +
  xlab("Left out headline")+
  ylab("Coefficient (neutral*true)") + 
  geom_hline(yintercept=0,lty=2) +
  theme(axis.text=element_text(size=10))

happy.out <- ggplot(happytab,aes(x=headline_out,y=est))+
  geom_point(size=2) +
  geom_errorbar(aes(ymin= est - 1.96*se, ymax= est + 1.96*se),width = .2) +
  #ggtitle("happy*true estimates - leave one headline out") +
  scale_x_continuous(breaks=c(1:10),labels=headlines) +
  xlab("Left out headline")+
  ylab("Coefficient (happy*true)") + 
  geom_hline(yintercept=0,lty=2) +
  theme(axis.text=element_text(size=10))

surprise.out <- ggplot(surprisetab,aes(x=headline_out,y=est))+
  geom_point(size=2) +
  geom_errorbar(aes(ymin= est - 1.96*se, ymax= est + 1.96*se),width = .2) +
  #ggtitle("surprise*veracity estimates - leave one headline out") +
  scale_x_continuous(breaks=c(1:10),labels=headlines) +
  xlab("Left out headline")+
  ylab("Coefficient (surprise*true)") + 
  geom_hline(yintercept=0,lty=2) +
  theme(axis.text=element_text(size=10))


###################
###   TABLE 18  ###
###################

ab1=glmer(belief ~ veracity*A_or_B + (1|respID) + (1|headline), data=ng.all, family="binomial",
          control=glmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

ab2=glmer(click ~ veracity*A_or_B + (1|respID) + (1|headline), data=ng.all, family="binomial",
          control=glmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

ab3=glmer(share ~ veracity*A_or_B + (1|respID) + (1|headline), data=ng.all, family="binomial",
          control=glmerControl(optimizer = 'bobyqa',optCtrl=list(maxfun=2e4)), na.action=na.omit)

# relabel: A: beleif --> emotions, B: emotion --> belief

stargazer(ab1, ab2, ab3,
          title = "Order of questions for each outcome",
          median = TRUE,
          covariate.labels = c("True","Emotion first","True:Emotion first"),
          digits=2,
          digits.extra=2,
          font.size = "small",
          label = "tab:ordercheck")


###################
###  FIGURE 6   ###
###################


head.tab <- ng.long %>% 
  group_by(veracity) %>%
  summarise(
    fear.mean = mean(fear,na.rm=T),
    fear.sd = sd(fear, na.rm=T)/sqrt(length(na.omit(fear))),
    
    anger.mean = mean(anger,na.rm=T),
    anger.sd = sd(anger, na.rm=T)/sqrt(length(na.omit(anger))),
    
    sad.mean = mean(sad,na.rm=T),
    sad.sd = sd(sad, na.rm=T)/sqrt(length(na.omit(sad))),
    
    happy.mean = mean(happy,na.rm=T),
    happy.sd = sd(happy, na.rm=T)/sqrt(length(na.omit(happy))),
    
    surprise.mean = mean(surprise,na.rm=T),
    surprise.sd = sd(surprise, na.rm=T)/sqrt(length(na.omit(surprise))),
    
    disgust.mean = mean(disgust,na.rm=T),
    disgust.sd = sd(disgust, na.rm=T)/sqrt(length(na.omit(disgust))),
    
    neutral.mean = mean(neutral,na.rm=T),
    neutral.sd = sd(neutral, na.rm=T)/sqrt(length(na.omit(neutral))),
    
    belief.mean = mean(belief,na.rm=T),
    belief.sd = sd(belief, na.rm=T)/sqrt(length(na.omit(belief))),
    
    click.mean = mean(click,na.rm=T),
    click.sd = sd(click, na.rm=T)/sqrt(length(na.omit(click))),
    
    share.mean = mean(share,na.rm=T),
    share.sd = sd(share, na.rm=T)/sqrt(length(na.omit(share)))
  )

# transpose table #
head.tabt <- head.tab %>%
  gather(var, value,-veracity) %>% 
  separate(var, into = c("emotion", "var")) %>%
  spread(key = var, value = value)

head.tabt.emo <- head.tabt %>% filter(!emotion %in% c("belief","share","click"))

head.tabt.emo$emotion <- relevel(factor(head.tabt.emo$emotion), ref = "neutral")

ggplot(head.tabt.emo, aes(x=veracity, y = mean, fill = emotion)) + 
  geom_bar(stat="identity", position = position_dodge(.9)) + 
  geom_errorbar(aes(ymin = mean-1.96*sd, ymax = mean + 1.96*sd), width=.2, position = position_dodge(.9)) +
  #scale_y_continuous(labels=scales::percent) +
  #ggtitle("Prevalence of emotions by stimuli veracity") +
  xlab("Headline Veracity") + 
  ylab("Prevalence of emotion") +
  scale_fill_brewer(palette="Dark2")


###################
###  TABLE 19   ###
###################


head.tab <- ng.long %>% 
  group_by(headline) %>%
  summarise(
    fear.mean = mean(fear,na.rm=T),
    fear.sd = sd(fear, na.rm=T),
    
    anger.mean = mean(anger,na.rm=T),
    anger.sd = sd(anger, na.rm=T),
    
    sad.mean = mean(sad,na.rm=T),
    sad.sd = sd(sad, na.rm=T),
    
    happy.mean = mean(happy,na.rm=T),
    happy.sd = sd(happy, na.rm=T),
    
    surprise.mean = mean(surprise,na.rm=T),
    surprise.sd = sd(surprise, na.rm=T),
    
    disgust.mean = mean(disgust,na.rm=T),
    disgust.sd = sd(disgust, na.rm=T),
    
    neutral.mean = mean(neutral,na.rm=T),
    neutral.sd = sd(neutral, na.rm=T),
    
    belief.mean = mean(belief,na.rm=T),
    belief.sd = sd(belief, na.rm=T),
    
    click.mean = mean(click,na.rm=T),
    click.sd = sd(click, na.rm=T),
    
    share.mean = mean(share,na.rm=T),
    share.sd = sd(share, na.rm=T)
  )

# transpose table #
head.tabt <- head.tab %>%
  rownames_to_column %>% 
  gather(var, value,-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value)

names(head.tabt)  <- head.tabt[1,]
head.tabt2 <- head.tabt[-1,]

head.tabt3 <- head.tabt2 %>%
  mutate_at(vars(-headline),as.numeric) 

names(head.tabt3) <- c("Headline","False 1","False 2","False 3","False 4","False 5",
                       "True 1","True 2","True 3","True 4","True 5")

head.tabt3$Headline <- gsub("."," ",head.tabt3$Headline, fixed=TRUE)

kbl(head.tabt3, digits = 2, caption = "Distribution of emotions for each stimuli", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(0, bold=TRUE) 


###################
###  TABLE 20   ###
###################


head.tab <- ng.long %>% 
  group_by(headline) %>%
  summarise(
    
    belief.mean = mean(belief,na.rm=T),
    belief.sd = sd(belief, na.rm=T),
    
    click.mean = mean(click,na.rm=T),
    click.sd = sd(click, na.rm=T),
    
    share.mean = mean(share,na.rm=T),
    share.sd = sd(share, na.rm=T)
  )

# transpose table #
head.tabt <- head.tab %>%
  rownames_to_column %>% 
  gather(var, value,-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value)

names(head.tabt)  <- head.tabt[1,]
head.tabt2 <- head.tabt[-1,]

head.tabt3 <- head.tabt2 %>%
  mutate_at(vars(-headline),as.numeric) 

names(head.tabt3) <- c("Headline","False 1","False 2","False 3","False 4","False 5",
                       "True 1","True 2","True 3","True 4","True 5")

head.tabt3$Headline <- gsub("."," ",head.tabt3$Headline, fixed=TRUE)

kbl(head.tabt3, digits = 2, caption = "Distribution of outcomes for each stimuli", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(0, bold=TRUE) 