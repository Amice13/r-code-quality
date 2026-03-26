# !diagnostics off
## All results calculated using R version 4.3.0 (2023-04-21 ucrt) -- "Already Tomorrow"

## load libraries
library(readr)
library(magrittr)
library(sandwich)
library(xtable)
library(ggplot2)
library(lmtest)
library(haven)
library(Hmisc)
library(stargazer)
library(plm)
library(dplyr)
library(rdrobust)
library(descr)
library(ggthemes)
library(interplot)
library(mediation)
library(readxl)
library(broom)
library(tidyverse)
library(logistf) 
library(gridExtra)
library(dotwhisker) 
library(broom.mixed)
library(ggpubr)
library(jtools)
library(interactions)
library(vtable)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(lavaan)
library(survey)
library(srvyr)

 

#################################################
################### Survey 1 #####################
#################################################


##### read Data #######
survone <- read_csv("surveyone.csv")
names(survone)
survone= survone[-c(1,2),]
dim(survone) #1196

#check average response time:
survone$`Duration (in seconds)` %>% as.numeric() %>% mean /60
#check duplicates (none)
survone$ResponseId %>% unique %>% length #1196

#### comprehension qs #####

survone$Attention %>% table
survone= survone %>% mutate(
  Attention_dum= recode(Attention, 'Seal Island'= '1', 'Snail Island'='0',
                        'Sun Island'= '0', 'Smile Island'= '0', .defalt= NA_character_  ) %>% as.numeric()
)
survone$Attention_dum  %>% mean #98% pass


survone$comp= ifelse(
  survone$comp1== "The U.S. and Russia"&
    survone$comp2== "A small uninhabitable island off the coast of Alaska"&
    survone$comp3 == "Yes" &
    survone$comp4 =="Lead to ${e://Field/cost_short_dc}"&
    survone$comp5== "${e://Field/beneficiary_cap}" &
    survone$comp6 != "Receive a free vacation to Seal Island" &
    survone$comp6 != "Receive a chance to swim with seals",1,0
)
survone$comp %>% mean #78 pass
 

######## DVs  ########

## create 7 point scale for military support
survone$milop %>% table
survone$milopneut %>% table

survone= survone %>% mutate(
  milop_text= milop,
  milop_text= ifelse(milop_text== "Neither approve nor disapprove", milopneut, milop_text)
)

## change to numbers
survone= survone %>% mutate(
  milop= recode(milop_text, 'Approve strongly'= '7', 'Approve'= '6',
                "lean toward approving"= "5", "do not lean either way"= "4",
                "lean toward disapproving"="3",
                'Disapprove'='2', 'Disapprove strongly'= "1", 
                .default= NA_character_) %>% as.numeric,
  milop_five= recode(milop_text, 'Approve strongly'= '5', 'Approve'= '4',
                     "lean toward approving"= "3", "do not lean either way"= "3",
                     "lean toward disapproving"="3",
                     'Disapprove'='2', 'Disapprove strongly'= "1", 
                     .default= NA_character_) %>% as.numeric
)
#check DVs 
survone$milop %>% table
survone$milop %>% summary

 

## change to numeric 
survone$Benefit_Am %>% table
survone= survone %>% mutate (
  Benefit_Am= recode(Benefit_Am, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                     'Neither agree nor disagree'='3', 
                     'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                     .default= NA_character_) %>% as.numeric,
  Benefit_US= recode(Benefit_US, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                     'Neither agree nor disagree'='3', 
                     'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                     .default= NA_character_) %>% as.numeric,
  Benefit_me= recode(Benefit_me, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                     'Neither agree nor disagree'='3', 
                     'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                     .default= NA_character_) %>% as.numeric,
)

survone= survone %>% mutate (
  pride= recode(Ownership_5, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                'Neither agree nor disagree'='3', 
                'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                .default= NA_character_) %>% as.numeric,
  natint= recode(Ownership_10, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                 'Neither agree nor disagree'='3', 
                 'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                 .default= NA_character_) %>% as.numeric,
  percare= recode(Ownership_13, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric
)

 
survone$Tax %>% table
survone= survone %>% mutate(
  tax_num = recode(Tax, 'Very willing'='4', "Somewhat willing"= "3",
                   'Somewhat unwilling'='2', 'Very unwilling'= "1", 
                   .default= NA_character_) %>% as.numeric,
  enlist_num = recode(EnlistSelf, 'YES, strongly consider enlisting'= '3', 
                      'MAYBE, possibly consider enlisting'= '2',
                      'NO, would not consider enlisting'='1',.default= NA_character_) %>% as.numeric,
  enlist_num_close = recode(EnlistClose, 'NO, strongly discourage'= '1', 
                            'NO, would discourage somewhat'= '2', 'YES, would encourage somewhat' = '3',
                            'YES, strongly encourage'='4',.default= NA_character_) %>% as.numeric 
)
 


########## Explanatory Vars ############

### which condition
survone$benefit_short %>% table
survone= survone %>% mutate(
  cond= recode(benefit_short, 
               "Economic benefit for Alaskan residents ($150 billion annually)"= "al_ec",
               "Economic benefit for all Americans ($150 billion annually)"= "am_ec",
               "Economic benefit for employees of Occidental Oil ($150 billion annually)"= "oil_ec",
               "Security benefit for Alaskan residents (Increased protection)"= "al_sec",
               "Security benefit for all Americans (Increased protection)"= "am_sec",
               "Security benefit for employees of Occidental Oil (Increased protection)"= "oil_sec")  %>% factor 
  (levels=   c("oil_sec", 'oil_ec', 'al_sec', 'al_ec', 'am_sec',
                             'am_ec'))
) 
survone$cond %>% table (useNA = 'ifany') #matches

survone$beneficiary
survone= survone %>% mutate(
  who= recode(beneficiary,
              "employees of Occidental Oil"= "OilComp",
              "all Americans"= "Americans",
              "Alaskan residents"= "Alaskans"),
  who= who %>% factor(levels= c( "Alaskans", "Americans", "OilComp"))
)
survone$who %>% table (useNA = 'ifany') #matches

#### Code concentrated and diffuse dummies 

survone$diff= ifelse(survone$cond == "am_ec" | survone$cond == "am_sec",1,0)
survone$diff %>% table 
survone$concen= ifelse(survone$diff==1,0,1)
survone$concen %>% table

### Code economic and security benefit dummies 
survone$bentype= ifelse(survone$cond == "al_sec" | survone$cond == "am_sec"| survone$cond == "oil_sec", "sec", "ec")
survone$bentype %>% table(useNA = 'ifany')
survone$bentype = as.factor(survone$bentype)
survone$bentype = relevel(survone$bentype, ref= "sec") 




#########################
####### Controls #######
### making composite russia score
survone= survone %>% mutate(
  russia1= recode(Russia1, 'Not an adversary'= '1', 
                  'A serious problem but not an adversary'='0.5',
                  'An adversary'= '0', 
                  .default= NA_character_  ) %>% as.numeric(),
  russia2= recode(Russia2, 'The U.S. should pursue friendly cooperation with Russia'= '1', 
                  "The U.S. should actively work to limit Russia's power"='0',.default= NA_character_  ) %>% as.numeric(),
  russia3= recode(Russia3, 'Russia is trying to undermine U.S. power and influence'= '0', 
                  'Russia is trying to pursue friendly cooperation with the United States'='1',
                  .default= NA_character_  ) %>% as.numeric()
)
# higher score indicates more friendliness
# check mutation
survone$russia1 %>% table
survone$Russia1 %>% table
survone$russia2 %>% table
survone$Russia2 %>% table
survone$russia3 %>% table
survone$Russia3 %>% table 

survone= survone %>% group_by(ResponseId) %>% mutate(
  russia_comp= (russia1+ russia2+ russia3)/3
)
survone$russia_comp %>% table (useNA = 'ifany')


### Age
survone$age= survone$age %>% as.numeric
# survone$age %>% hist

### Militant
# higher scores: more militant 
survone= survone %>% mutate(
  # Mil 1: The use of military force only makes problems worse
  milnum1= recode(Mil1, 'Strongly agree'= '1', 'Somewhat agree'= '2',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'="4", 'Strongly disagree'= "5", 
                  .default= NA_character_) %>% as.numeric,
  #Mil 2: The best way to ensure peace is through military strength
  milnum2= recode(Mil2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  #Mil3: The U.S. should prioritize improving its military over other issues
  milnum3= recode(Mil3, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric
  
)

survone$Mil1 %>% table
survone$milnum1 %>% table
survone$Mil2 %>% table
survone$milnum2 %>% table
survone$Mil3 %>% table
survone$milnum3 %>% table
survone= survone %>% mutate(
  mil_comp =  (milnum1+ milnum2+ milnum3) /3
)
survone$mil_comp %>% table(useNA = 'ifany')



### Nationalistic

survone= survone %>% mutate(
  #proud to be American
  natnum1= recode(attnat_1, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  #other countries should be like US
  natnum2= recode(attnat_2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  #want to be an American citizen than other country
  natnum3= recode( Nat3, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                   'Neither agree nor disagree'='3', 
                   'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                   .default= NA_character_) %>% as.numeric
  
  
)

survone$natnum1 %>% table
survone$natnum2 %>% table
survone$natnum3 %>% table 

survone= survone %>% mutate(
  nat_comp =  (natnum1+ natnum2+ natnum3) /3
)

survone$nat_comp %>% table(useNA =  'ifany')


## mobility 
survone= survone %>% mutate(
  #The United States provides everyone with more or less equal opportunities for success
  mobnum4= recode(attnat_4, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  # In general, do you trust the U.S. government to appropriately redistribute wealth and income in the American society?
  mobnum1= recode(mob1, "Strongly trust"  = '5', "Somewhat trust"  = '4',
                  "Neither trust nor distrust"='3', 
                  "Somewhat distrust"  ='2', "Strongly distrust"  = "1", 
                  .default= NA_character_) %>% as.numeric,
  # In the U.S., it is relatively easy to succeed and live a comfortable life if one works hard enough
  mobnum2= recode(mob2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  # In the U.S., it is possible to start out poor, but become well-off by working hard
  mobnum3= recode(mob3, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  # Do you believe you have a good chance of moving up to a higher social class in the future?
  mobnum5= recode(permob, 'Strongly believe'= '5', 'Somewhat believe'= '4',
                  'Neither believe nor doubt'='3', 
                  'Somewhat doubt'='2', 'Strongly doubt'= "1", 
                  .default= NA_character_) %>% as.numeric
)

survone= survone %>% mutate(
  mob_comp =  (mobnum1+ mobnum2+ mobnum3+ mobnum4+mobnum5) /5
)



### Authoritarian
survone= survone %>% mutate(
  # Government authorities generally know what is best for the country
  authnum1= recode(Auth2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                   'Neither agree nor disagree'='3', 
                   'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                   .default= NA_character_) %>% as.numeric)
survone$Auth2 %>% table
survone$authnum1 %>% table


### Education ###
survone$educ %>% table (useNA = 'ifany' )
survone= survone %>% mutate(
  college= recode(educ,
                  "Currently pursuing or received a graduate degree (ex. MA, PhD, JD, MD)" ="1",
                  "Received a Bachelor's degree in college (4-year)" = "1",
                  "Currently pursuing a Bachelor's degree in college (4-year)" ="1",
                  .default= "0") %>% as.numeric)
survone$college %>% table


### Social class ###

survone= survone %>% mutate(
  # How would you describe your social class?
  sclass= recode(sclass, 'Upper class'= '5', 'Upper-middle class'= '4',
                 'Middle class'='3', 
                 'Working class'='2', 'Lower class'= "1", 
                 .default= NA_character_) %>% as.numeric)
survone$sclass %>% table

### Income ### 

survone$hincome%>% table (useNA = 'ifany')
survone= survone %>% mutate(
  income= recode(hincome, "$150,000 or more" = "6", "$100,000 to $149,999"= "5",
                 "$75,000 to $99,999"= "4", 
                 "$50,000 to $74,999"= "3", "$35,000 to $49,999" = "2", 
                 "$25,000 to $34,999" = '1',   "Less than $25,000" = "1",
                 .default= NA_character_) %>% as.numeric()
) 
survone$income %>% table(useNA = 'ifany')
survone$hincome <- as.factor(survone$hincome)


## Gender ##
survone$gender_char%>% table(useNA = 'ifany')
survone= survone %>% mutate(
  gender= recode(gender_char, "Female" ='1' , 'Male'= '0', .default = NA_character_) %>% as.numeric
)
survone$gender %>% table(useNA = 'ifany')

##Enviro ##
survone= survone %>% mutate(
  # The U.S. should invest more into renewable energy sources such as solar and wind power, over traditional energy sources such as coal and oil
  envnum= recode(enviro, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                 'Neither agree nor disagree'='3', 
                 'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                 .default= NA_character_) %>% as.numeric
)
survone$envnum %>% table(useNA = 'ifany')

##trickle 
survone= survone %>% mutate(
  # The government should provide tax cuts to corporations and the wealthy so that they can create more jobs in the economy
  trickle= recode(trickle, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric
)
survone$trickle %>% table

survone= survone %>% mutate(
  ideology= polview,
  polview= recode(polview, "Conservative"= "5", "Slightly conservative"= "4",
                  "Neither liberal nor conservative"= '3', "Slightly liberal"= "2",
                  "Liberal"= "1",
                  .default= NA_character_) %>% as.numeric
)
survone$polview %>% table
survone$ideology %>% table

### partyid 
survone= survone %>% mutate(
  party= recode(partyid, 
                'Republican'= 'rep', "Democrat"= "dem", 
                "Independent"= "indep", .default= NA_character_),
  partytwo= recode(party, 
                   'rep'= 'rep', "dem"= "dem", .default= NA_character_)
)
survone$partytwo %>% table
survone$party%>% table (useNA = 'ifany' )
survone$partyid %>% table (useNA = 'ifany' )


##race
survone$race %>% table
survone$white <- ifelse(grepl('White', survone$race)==T, 1, 0)
survone$white
survone$black <- ifelse(grepl('Black', survone$race)==T, 1, 0)
survone$black
survone$asian <- ifelse(grepl('Asian', survone$race)==T, 1, 0)
survone$asian
survone$hispanic <- ifelse(grepl('Hispanic', survone$race)==T, 1, 0)
survone$hispanic

###### survone_pass ###### 
## passed attention check and comprehension
survone_pass= survone %>% filter(Attention_dum==1 & comp==1)
dim(survone_pass) 
survone %>% filter(Attention_dum==1 & comp==1) %>% nrow()/ survone %>% nrow() #77.6



#######################################
###### Data Analysis (Main Text) ######
#######################################
 
#-------------------------------

######## Figure 2 ###########

#--------------------------------

## make matrix with average values by condition
mat=survone %>% group_by(cond) %>% summarise_at(c( 'Benefit_US', 'natint', 'milop_five',
                                                'pride', 'percare', 'tax_num'), mean)
mat$cond
colnames(mat)= c('cond', 'avBenefit_US', 'avnatint', 'avmilop',
                 'avpride', 'avpercare', 'avtax_num')
mat
dall2=merge(survone, mat, by='cond')
dall2$cond <- ordered (dall2$cond, 
                       levels=c('am_sec', 'am_ec', 'al_sec', 'al_ec',
                                'oil_sec', 'oil_ec'))
dall2$who <- ordered(dall2$who, levels= c('Americans', 'Alaskans', 'OilComp'))
 
## create ggplot
## in black and white   
p1= dall2  %>% filter(bentype== 'sec')  %>%  ggplot()+
  geom_point(aes(x=who, y=avBenefit_US,shape='Benefit US') )+
  geom_line(aes(x=who, y=avBenefit_US,group=1 ), 
            linetype=1  )+
  geom_point(aes(x=who, y=avnatint, shape= 'National Interest') )+
  geom_line(aes(x=who, y=avnatint,group=1 ), 
            linetype=1   )+
  geom_point(aes(x=who, y=avmilop,shape= 'SupportMilOp')  )+
  geom_line(aes(x=who, y=avmilop,group=1 ), 
            linetype=1   )+
  geom_point(aes(x=who, y=avpride,shape= 'Proud to Have') )+
  geom_line(aes(x=who, y=avpride,group=1 ), linetype=1
  )+
  geom_point(aes(x=who, y=avpercare,shape= 'Unhappy to Lose'  ))+
  geom_line(aes(x=who, y=avpercare,group=1 ), linetype=1
  )+
  geom_point(aes(x=who, y=avtax_num,shape= 'Contribute Money' ) )+
  geom_line(aes(x=who, y=avtax_num,group=1 ), 
            linetype=1   )+
  scale_shape_manual(values = c(16,17,0,4, 3,5))  + ylim(2,4.5)+
  theme_clean()+ ylab('')+xlab('Beneficiary ')+
  scale_color_discrete(guide= 'none')+
  ggtitle('(A) Benefit Type: \n Increased security protection')+
  theme(axis.text=element_text(size=14),  
        axis.title=element_text(size=12,face="bold"))
p2= dall2%>% filter(bentype== 'ec')  %>%  ggplot()+
  geom_point(aes(x=who, y=avBenefit_US,shape='Benefit US') )+
  geom_line(aes(x=who, y=avBenefit_US,group=1), linetype=1  )+
  geom_point(aes(x=who, y=avnatint, shape= 'National Interest') )+
  geom_line(aes(x=who, y=avnatint,group=1), linetype=1  )+
  geom_point(aes(x=who, y=avmilop,shape= 'SupportMilOp')  )+
  geom_line(aes(x=who, y=avmilop,group=1), linetype=1  )+
  geom_point(aes(x=who, y=avpride,shape= 'Proud to Have') )+
  geom_line(aes(x=who, y=avpride,group=1), linetype=1  )+
  geom_point(aes(x=who, y=avpercare,shape= 'Unhappy to Lose'  ))+
  geom_line(aes(x=who, y=avpercare,group=1), linetype=1  )+
  geom_point(aes(x=who, y=avtax_num,shape= 'Contribute Money' ) )+
  geom_line(aes(x=who, y=avtax_num,group=1), linetype=1  )+
  scale_shape_manual(values = c(16,17,0,4, 3,5))  + ylim(2,4.5)+
  theme_clean()+ ylab('')+xlab('Beneficiary ')+
  ggtitle('(B) Benefit Type: \n $150 billion annual benefit')+
  theme(axis.text=element_text(size=14),  
        axis.title=element_text(size=12,face="bold"))

f<-ggarrange(p1, p2,   ncol=2, common.legend = TRUE, legend="bottom")
f
 ggsave(f, file='fig2.pdf', height = 4.8, width = 9.3)



#--------------------------------

## Table 1 (Variances) ######

#--------------------------------

survone$who <- relevel(survone$who, ref = "Americans")
 
## numbers
tapply(survone$Benefit_US, survone$who, var) %>% round(2)
tapply(survone$natint, survone$who, var) %>% round(2)
tapply(survone$milop_five, survone$who, var) %>% round(2)
tapply(survone$pride, survone$who, var) %>% round(2)
tapply(survone$percare, survone$who, var) %>% round(2)
tapply(survone$tax_num, survone$who, var) %>% round(2)

## p-values
car::leveneTest(Benefit_US~ as.factor(who), data= survone)
car::leveneTest(natint~ as.factor(who), data= survone)
car::leveneTest(milop_five~ as.factor(who), data= survone)
car::leveneTest(pride~ as.factor(who), data= survone)
car::leveneTest(percare~ as.factor(who), data= survone)
car::leveneTest(tax_num~ as.factor(who), data= survone)



#######################################
###### Data Analysis (Appendix) ######
#######################################

#-----------------------------------

##  Appendix A1: main regression results #######

#-----------------------------------

##### prepare variables  
survone$who <- relevel(survone$who, ref = "Americans")
survone$bentype <- relevel(survone$bentype, ref = "sec")


#### Table A1 (Appendix A1.1) ###### 

m1=  lm(Benefit_US ~ who + bentype, data = survone)
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2=  lm(natint ~ who + bentype, data = survone)
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m3=  lm(milop_five ~ who + bentype, data = survone)
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))

m4=  lm(pride ~ who + bentype, data = survone)
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))

m5=  lm(percare ~ who + bentype, data = survone)
m5r= coeftest(m5, vcov = vcovHC(m5, type="HC1"))

m6=  lm(tax_num ~ who + bentype, data = survone)
m6r= coeftest(m6, vcov = vcovHC(m6, type="HC1"))

## table with robust SEs
stargazer(m1r, m2r, m3r, m4r, m5r, m6r,
          covariate.labels = 
            c(whoAlaskans = "Alaskan Residents",
              whoOilComp= "Occidental Oil",
              bentypeec = "Economic Benefit" ))
## get Observation and r2
stargazer(m1,m2,m3,m4, m5, m6)



####### Table A2  (Appendix A1.2)  ####
survone$who <- relevel(survone$who, ref = "Americans")
survone$bentype <- relevel(survone$bentype, ref = "sec")

m1=  lm(Benefit_US ~ who + bentype+ party+ income+college+gender+
          russia_comp+ mil_comp+ nat_comp, data = survone)
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2=  lm(natint ~ who + bentype+ party+ income+college+gender+
          russia_comp+ mil_comp+ nat_comp, data = survone)
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m3=  lm(milop_five ~ who + bentype+ party+ income+college+gender+
          russia_comp+ mil_comp+ nat_comp, data = survone)
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))

m4=  lm(pride ~ who + bentype+ party+ income+college+gender+
          russia_comp+ mil_comp+ nat_comp, data = survone)
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))

m5=  lm(percare ~ who + bentype+ party+ income+college+gender+
          russia_comp+ mil_comp+ nat_comp, data = survone)
m5r= coeftest(m5, vcov = vcovHC(m5, type="HC1"))

m6=  lm(tax_num ~ who + bentype+ party+ income+college+gender+
          russia_comp+ mil_comp+ nat_comp, data = survone)
m6r= coeftest(m6, vcov = vcovHC(m6, type="HC1"))


## table with robust ses
stargazer(m1r, m2r, m3r, m4r, m5r, m6r,
          covariate.labels = 
            c(whoAlaskans = "Alaskan Residents",
              whoOilComp= "Occidental Oil",
              bentypeec = "Economic Benefit",
              partyindep= "pid: Indep",
              partyrep= "pid: Rep",
              income= "Income",
              college= "College",
              gender= 'Female',
              russia_comp= 'Russia Friendly',
              mil_comp= "Militant",
              nat_comp= "Nationalist"), no.space = T)

## get obs and r2
stargazer(m1,m2,m3,m4, m5, m6)



#### Table A3  (Appendix A1.3)###### 

survone_pass$who <- relevel(survone_pass$who, ref = "Americans")
survone_pass$bentype <- relevel(survone_pass$bentype, ref = "sec")


m1=  lm(Benefit_US ~ who + bentype, data = survone_pass)
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2=  lm(natint ~ who + bentype, data = survone_pass)
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m3=  lm(milop_five ~ who + bentype, data = survone_pass)
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))

m4=  lm(pride ~ who + bentype, data = survone_pass)
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))

m5=  lm(percare ~ who + bentype, data = survone_pass)
m5r= coeftest(m5, vcov = vcovHC(m5, type="HC1")) 

m6=  lm(tax_num ~ who + bentype, data = survone_pass)
m6r= coeftest(m6, vcov = vcovHC(m6, type="HC1"))

## table with robust SEs
stargazer(m1r, m2r, m3r, m4r, m5r, m6r,
          covariate.labels = 
            c(whoAlaskans = "Alaskan Residents",
              whoOilComp= "Occidental Oil",
              bentypeec = "Economic Benefit" ))
## get Observation and r2
stargazer(m1,m2,m3,m4, m5, m6)




####### Table A4  (Appendix A1.4)################
 ## change baseline to Alaskans
survone$who <- relevel(survone$who, ref = "Alaskans")

m1=  lm(Benefit_US ~ who + bentype, data = survone)
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2=  lm(natint ~ who + bentype, data = survone)
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m3=  lm(milop_five ~ who + bentype, data = survone)
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))

m4=  lm(pride ~ who + bentype, data = survone)
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))

m5=  lm(percare ~ who + bentype, data = survone)
m5r= coeftest(m5, vcov = vcovHC(m5, type="HC1"))

m6=  lm(tax_num ~ who + bentype, data = survone)
m6r= coeftest(m6, vcov = vcovHC(m6, type="HC1"))

## table used
stargazer(m1r, m2r, m3r, m4r, m5r, m6r,
          covariate.labels = 
            c(whoAmericans = "All Americans",
              whoOilComp= "Occidental Oil",
              bentypeec = "Economic Benefit" ))
#get r2 and observations
stargazer(m1,m2,m3,m4, m5, m6)

## change baseline back to Americans
survone$who <- relevel(survone$who, ref = "Americans")


#---------------------------------------

### Appendix A2: Mediation Analysis  ####

#---------------------------------------

## natint (Table A5)
multipleMediation <- 'natint ~ b1 * Benefit_Am + b2 *Benefit_me + c *concen + gender+ party+ college+ income
Benefit_Am~ a1 * concen
Benefit_me ~ a2 *concen
indirect1 := a1 * b1
indirect2 := a2 * b2
contrast := indirect1 - indirect2
total := c + (a1 * b1) + (a2 * b2)
direct:= total- indirect1- indirect2
'
fit2 <- sem(model = multipleMediation, data = survone, se= 'bootstrap',
            iseed=123)
summary(fit2)


## milop  (Table A6)
multipleMediation <- 'milop ~ b1 * Benefit_Am + b2 *Benefit_me + c *concen + gender+ party+ college+ income
Benefit_Am~ a1 * concen
Benefit_me ~ a2 *concen
indirect1 := a1 * b1
indirect2 := a2 * b2
contrast := indirect1 - indirect2
total := c + (a1 * b1) + (a2 * b2)
direct:= total- indirect1- indirect2
'
fit2 <- sem(model = multipleMediation, data = survone, se= 'bootstrap',
            iseed=123)
summary(fit2)

 


#---------------------------------------

### Appendix A3: Heterogeneous Treatment Effects   ####

#---------------------------------------


##### A.3.1 Party  #####

m1= lm(Benefit_US ~ diff*partytwo+ bentype , data = survone)
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2= lm(natint ~ diff*partytwo+ bentype , data = survone)
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m3= lm(milop_five ~  diff*partytwo+ bentype , data = survone)
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))

m4= lm(pride ~ diff*partytwo + bentype, data = survone)
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))

m6= lm(percare ~ diff*partytwo+ bentype , data = survone)
m6r= coeftest(m6, vcov = vcovHC(m6, type="HC1"))

m7= lm(tax_num ~ diff*partytwo + bentype, data = survone)
m7r= coeftest(m7, vcov = vcovHC(m7, type="HC1"))

stargazer(m1r,  m2r, m3r, m4r,  m6r, m7r, 
          covariate.labels = c("diff"= 'Diffused Benefits',
                               'partytwo'= 'Party ID',
                               'bentypeec'= 'Economic Benefits' ))
stargazer(m1, m2, m3, m4,  m6, m7,
          covariate.labels = c("diff"= 'Diffused Benefits',
                               'partytwo'= 'Party ID',
                               'bentypeec'= 'Economic Benefits' ))

###### A,3,2 Political View  ####
m1= lm(Benefit_US ~ diff*polview+ bentype , data = survone)
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))
m2= lm(natint ~ diff*polview+ bentype , data = survone)
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))
m3= lm(milop_five ~  diff*polview+ bentype , data = survone)
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))
m4= lm(pride ~ diff*polview + bentype, data = survone)
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))
m6= lm(percare ~ diff*polview+ bentype , data = survone)
m6r= coeftest(m6, vcov = vcovHC(m6, type="HC1"))
m7= lm(tax_num ~ diff*polview + bentype, data = survone)
m7r= coeftest(m7, vcov = vcovHC(m7, type="HC1"))

stargazer(m1r,  m2r, m3r, m4r,  m6r, m7r, 
          covariate.labels = c("diff"= 'Diffuse Benefits',
                               'polview'= 'Ideology',
                               'bentypeec'= 'Economic Benefits' ))
stargazer(m1, m2, m3, m4,  m6, m7,
          covariate.labels = c("diff"= 'Diffuse Benefits',
                               'polview'= 'Ideology',
                               'bentypeec'= 'Economic Benefits' ))


#---------------------------------------

### Appendix A4: Balance Test   ####

#---------------------------------------

sumtable(survone, 
         vars =c('gender', 'age',
                 'partyid', 'ideology', 'college', 
                 'white', 'black', 'asian', 'hispanic',
                 'russia_comp',  'mil_comp', 'nat_comp'),
         group = 'cond',
         out = 'latex', 
         digits= 2,
         group.test = T)


#---------------------------------------

### Appendix A5: Interaction Effect: BenType X Diffuse  #####

#---------------------------------------

# bentype
m1= lm(Benefit_US ~ bentype*concen  , data = survone)
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))
m2= lm(natint ~ bentype*concen  , data = survone)
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))
m3= lm(milop_five ~  bentype*concen  , data = survone)
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))
m4= lm(pride ~ bentype*concen  , data = survone)
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))
m6= lm(percare ~ bentype*concen  , data = survone)
m6r= coeftest(m6, vcov = vcovHC(m6, type="HC1"))
m7= lm(tax_num ~ bentype*concen  , data = survone)
m7r= coeftest(m7, vcov = vcovHC(m7, type="HC1"))

stargazer(m1r,  m2r, m3r, m4r,  m6r, m7r)
stargazer(m1, m2, m3, m4,  m6, m7)

#----------------------------------------

## Appendix A6: Moderating Effects by Pre-existing Perceptions   ####

#-----------------------------------------

##### redistribution (Table A12) ######

## In general, do you trust the U.S. government to appropriately redistribute
## wealth and income in the American society? 

m1= lm(Benefit_US ~ diff*mobnum1+ bentype , data = survone)
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))
m2= lm(natint ~ diff*mobnum1+ bentype , data = survone)
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))
m3= lm(milop_five ~  diff*mobnum1+ bentype , data = survone)
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))
m4= lm(pride ~ diff*mobnum1 + bentype, data = survone)
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))
m6= lm(percare ~ diff*mobnum1+ bentype , data = survone)
m6r= coeftest(m6, vcov = vcovHC(m6, type="HC1"))
m7= lm(tax_num ~ diff*mobnum1 + bentype, data = survone)
m7r= coeftest(m7, vcov = vcovHC(m7, type="HC1"))

# with robust SEs
stargazer(m1r,  m2r, m3r, m4r,  m6r, m7r, 
          covariate.labels = c("diff"= 'Diffuse Benefits',
                               'mob1'= 'Party ID',
                               'bentypeec'= 'Economic Benefits' ))
#for obs and r2
stargazer(m1, m2, m3, m4,  m6, m7,
          covariate.labels = c("diff"= 'Diffuse Benefits',
                               'mob1'= 'Party ID',
                               'bentypeec'= 'Economic Benefits' ))



####### Support for Oil Energy (Table A13) #####

survone$oilsupp <- 6-survone$envnum
survone$oilsupp %>% table
survone$envnum %>% table

m1= lm(Benefit_US ~ diff*oilsupp+ bentype , data = survone)
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))
m2= lm(natint ~ diff*oilsupp+ bentype , data = survone)
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))
m3= lm(milop_five ~  diff*oilsupp+ bentype , data = survone)
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))
m4= lm(pride ~ diff*oilsupp + bentype, data = survone)
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))
m6= lm(percare ~ diff*oilsupp+ bentype , data = survone)
m6r= coeftest(m6, vcov = vcovHC(m6, type="HC1"))
m7= lm(tax_num ~ diff*oilsupp + bentype, data = survone)
m7r= coeftest(m7, vcov = vcovHC(m7, type="HC1"))

stargazer(m1r,  m2r, m3r, m4r,  m6r, m7r, 
          covariate.labels = c("diff"= 'Diffuse Benefits',
                               'oilsupp'= 'Oil support',
                               'bentypeec'= 'Economic Benefits' ))
stargazer(m1, m2, m3, m4,  m6, m7,
          covariate.labels = c("diff"= 'Diffuse Benefits',
                               'mob1'= 'Party ID',
                               'bentypeec'= 'Economic Benefits' ))
 

 

 
#################################################
################### Survey 2 #####################
#################################################


#-----------------------------------------
## Data Prep ####
#-----------------------------------------


## read data 
survtwo <- read_csv("surveytwo.csv")
names(survtwo)
dim(survtwo)
survtwo= survtwo[-c(1,2),] 
survtwo$`Duration (in seconds)` %>% as.numeric() %>% mean /60 
#check duplicates (none)
survtwo$ResponseId %>% unique %>% length #825 


##attention checks #####

survtwo$Attention %>% table
survtwo= survtwo %>% mutate(
  Attention_dum= recode(Attention, 'Seal Island'= '1', 'Snail Island'='0',
                        'Sun Island'= '0', 'Smile Island'= '0', .defalt= NA_character_  ) %>% as.numeric()
)
survtwo$Attention_dum  %>% mean  
 
survtwo$comp= ifelse(
  survtwo$comp2== "A small uninhabitable island off the coast of Alaska"&
    survtwo$comp3 == "Yes" &
    survtwo$comp4 =="Lead to ${e://Field/cost_short_dc}"&
    survtwo$comp5== "All Americans" &
    survtwo$comp6 != "Receive a free vacation to Seal Island" &
    survtwo$comp6 != "Receive a chance to swim with seals" &
    (survtwo$comp7 =="Only a small number of interest groups" | is.na(survtwo$comp7)==T)
  ,1,0
)
survtwo$comp %>% table (useNA = 'ifany')
survtwo$comp %>% mean #82 pass



######## DVs  ########
survtwo$milop %>% table
survtwo$milopneut %>% table

survtwo= survtwo %>% mutate(
  milop_text= milop,
  milop_text= ifelse(milop_text== "Neither approve nor disapprove", milopneut, milop_text)
)


## change to text
survtwo= survtwo %>% mutate(
  milop= recode(milop_text, 'Approve strongly'= '7', 'Approve'= '6',
                "lean toward approving"= "5", "do not lean either way"= "4",
                "lean toward disapproving"="3",
                'Disapprove'='2', 'Disapprove strongly'= "1", 
                .default= NA_character_) %>% as.numeric,
  milop_five= recode(milop_text, 'Approve strongly'= '5', 'Approve'= '4',
                     "lean toward approving"= "3", "do not lean either way"= "3",
                     "lean toward disapproving"="3",
                     'Disapprove'='2', 'Disapprove strongly'= "1", 
                     .default= NA_character_) %>% as.numeric
)
survtwo$milop %>% table
survtwo$milop %>% summary 

## change to numeric  
survtwo= survtwo %>% mutate (
  Benefit_Am= recode(Benefit_Am, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                     'Neither agree nor disagree'='3', 
                     'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                     .default= NA_character_) %>% as.numeric,
  Benefit_US= recode(Benefit_US, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                     'Neither agree nor disagree'='3', 
                     'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                     .default= NA_character_) %>% as.numeric,
  Benefit_me= recode(Benefit_me, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                     'Neither agree nor disagree'='3', 
                     'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                     .default= NA_character_) %>% as.numeric,
  pride= recode(Ownership_5, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                'Neither agree nor disagree'='3', 
                'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                .default= NA_character_) %>% as.numeric,
  natint= recode(Ownership_10, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                 'Neither agree nor disagree'='3', 
                 'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                 .default= NA_character_) %>% as.numeric,
  percare= recode(Ownership_13, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric
)

# survtwo$natint %>% table
 
survtwo= survtwo %>% mutate(
  tax_num = recode(Tax, 'Very willing'='4', "Somewhat willing"= "3",
                   'Somewhat unwilling'='2', 'Very unwilling'= "1", 
                   .default= NA_character_) %>% as.numeric)
# survtwo$tax_num %>% hist

survtwo= survtwo %>% mutate(
  enlist_num = recode(EnlistSelf, 'YES, strongly consider enlisting'= '3', 
                      'MAYBE, possibly consider enlisting'= '2',
                      'NO, would not consider enlisting'='1',.default= NA_character_) %>% as.numeric,
  enlist_num_close = recode(EnlistClose, 'NO, strongly discourage'= '1', 
                            'NO, would discourage somewhat'= '2', 'YES, would encourage somewhat' = '3',
                            'YES, strongly encourage'='4',.default= NA_character_) %>% as.numeric
)
# survtwo$enlist_num




######## Explanatory Vars ############

### which condition
survtwo$opposition %>% table(useNA = 'ifany')
survtwo$bentype %>% table(useNA = 'ifany')

survtwo= survtwo %>% mutate(
  cond= case_when(bentype == "economic" & opposition == "1" ~ "ec_opp",
                  bentype == "economic" & is.na(opposition)== T ~ "ec_con",
                  bentype == "security" & opposition == "1" ~ "sec_opp",
                  bentype == "security" &  is.na(opposition)== T ~ "sec_con") %>% 
         factor (levels=  c('ec_con', 'ec_opp',  'sec_con', 'sec_opp')), 
  bentype= bentype %>% factor(levels = c( 'security','economic')), 
  treat= ifelse(cond== "ec_opp"| cond == "sec_opp", 1, 0)
)
survtwo$treat%>% table(useNA = 'ifany')
survtwo$cond %>% table(useNA = 'ifany')



#########################
####### Controls #######
### making composite russia score
survtwo= survtwo %>% mutate(
  russia1= recode(Russia1, 'Russia is not an adversary'= '1', 
                  'Russia is a serious problem but not an adversary'='0.5',
                  'Russia is an adversary'= '0', 
                  .default= NA_character_  ) %>% as.numeric(),
  russia2= recode(Russia2, 'The U.S. should pursue friendly cooperation with Russia'= '1', 
                  "The U.S. should actively work to limit Russia's power"='0',.default= NA_character_  ) %>% as.numeric(),
  russia3= recode(Russia3, 'Russia is trying to undermine U.S. power and influence'= '0', 
                  'Russia is trying to pursue friendly cooperation with the United States'='1',
                  .default= NA_character_  ) %>% as.numeric()
)
# higher score indicates more friendliness

survtwo$russia1 %>% table
survtwo$Russia1 %>% table
survtwo$russia2 %>% table
survtwo$Russia2 %>% table
survtwo$russia3 %>% table
survtwo$Russia3 %>% table

survtwo= survtwo %>% mutate(
  russia_comp= (russia1+ russia2+ russia3)/3
)

### Age
survtwo$age= survtwo$age %>% as.numeric
# survtwo$age %>% hist

### Militant
survtwo= survtwo %>% mutate(
  milnum1= recode(Mil1, 'Strongly agree'= '1', 'Somewhat agree'= '2',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'="4", 'Strongly disagree'= "5", 
                  .default= NA_character_) %>% as.numeric,
  milnum2= recode(Mil2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  milnum3= recode(Mil3, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric
  
)
# higher scores: more militant 

survtwo$Mil1 %>% table
survtwo$milnum1 %>% table
survtwo$Mil2 %>% table
survtwo$milnum2 %>% table
survtwo$Mil3 %>% table
survtwo$milnum3 %>% table
survtwo= survtwo %>% mutate(
  mil_comp =  (milnum1+ milnum2+ milnum3) /3
)
survtwo$mil_comp %>% table(useNA = 'ifany')



### Nationalistic

survtwo= survtwo %>% mutate(
  natnum1= recode(attnat_1, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  natnum2= recode(attnat_2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  natnum3= recode( Nat3, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                   'Neither agree nor disagree'='3', 
                   'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                   .default= NA_character_) %>% as.numeric,
  
  
)
#proud to be American
survtwo$natnum1 %>% table
#other countries should be like US
survtwo$natnum2 %>% table
#want American citizen than other country
survtwo$natnum3 %>% table

survtwo= survtwo %>% mutate(
  nat_comp =  (natnum1+ natnum2+ natnum3) /3)
# survtwo$nat_comp %>% hist


## mobility 
survtwo= survtwo %>% mutate(
  mobnum4= recode(attnat_4, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  mobnum1= recode(econsystem, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1",
                  .default= NA_character_) %>% as.numeric,
  mobnum2= recode(mob2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  mobnum3= recode(mob3, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  mobnum5= recode(permob, 'Much better off'= '5', 'Somewhat better off'= '4',
                  'Neither better off nor worse off'='3', 
                  'Somewhat worse off'='2', 'Much worse off'= "1", 
                  .default= NA_character_) %>% as.numeric
)

survtwo= survtwo %>% mutate(
  mob_comp =  (mobnum1+ mobnum2+ mobnum3+ mobnum4+mobnum5) /5,
  mob_per=  mobnum5
)

# turst gov
survtwo$mobnum1 %>% table 
survtwo$econsystem %>% table
#succeed work 
survtwo$mobnum2 %>% table
survtwo$mobnum3 %>% table

# equal opportunity
survtwo$mobnum4 %>% table  
#personal
survtwo$permob %>% table
survtwo$mobnum5 %>% table


##ideology
survtwo$ideology = survtwo$polview
survtwo= survtwo %>% mutate(
  polview= recode(ideology, "Conservative"= "5", "Slightly conservative"= "4",
                  "Neither liberal nor conservative"= '3', "Slightly liberal"= "2",
                  "Liberal"= "1",
                  .default= NA_character_) %>% as.numeric
)
survtwo$polview %>% table
survtwo$ideology 
 

### Authoritarian
survtwo= survtwo %>% mutate(
  authnum1= recode(Auth2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                   'Neither agree nor disagree'='3', 
                   'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                   .default= NA_character_) %>% as.numeric)
survtwo$Auth2 %>% table
survtwo$authnum1 %>% table


### Education ###
survtwo$educ %>% table (useNA = 'ifany' )
survtwo= survtwo %>% mutate(
  college= recode(educ,
                  "Currently pursuing or received a graduate degree (ex. MA, PhD, JD, MD)" ="1",
                  "Received a Bachelor's degree in college (4-year)" = "1",
                  "Currently pursuing a Bachelor's degree in college (4-year)" ="1",
                  .default= "0") %>% as.numeric)
survtwo$college %>% table
 
### Income ### 

survtwo$hincome%>% table (useNA = 'ifany')
survtwo= survtwo %>% mutate(
  income= recode(hincome, "$150,000 or more" = "6", "$100,000 to $149,999"= "5",
                 "$75,000 to $99,999"= "4", 
                 "$50,000 to $74,999"= "3", "$35,000 to $49,999" = "2", 
                 "$25,000 to $34,999" = '1',   "Less than $25,000" = "1",
                 .default= NA_character_) %>% as.numeric()
)
survtwo$income %>% table
survtwo$hincome <- as.factor(survtwo$hincome)
 

## Gender ##
 
survtwo$gender_char %>% table(useNA = 'ifany')
survtwo= survtwo %>% mutate(
  gender= recode(gender_char, "Female" ='1' , 'Male'= '0', .default = NA_character_) %>% as.numeric
)
survtwo$gender %>% table(useNA = 'ifany')
 

##Enviro ##
survtwo= survtwo %>% mutate(
  envnum= recode(enviro, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                 'Neither agree nor disagree'='3', 
                 'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                 .default= NA_character_) %>% as.numeric
)
survtwo$envnum %>% table(useNA = 'ifany')




### partyid 
survtwo= survtwo %>% mutate(
  party= recode(partyid, 
                'Republican'= 'rep', "Democrat"= "dem", 
                "Independent"= "indep", .default= NA_character_),
  partytwo= recode(party, 
                   'rep'= 'rep', "dem"= "dem", .default= NA_character_)
)
survtwo$partytwo %>% table
survtwo$party%>% table (useNA = 'ifany' )
survtwo$partyid %>% table (useNA = 'ifany' )


##  race 
survtwo$race %>% table
survtwo$white <- ifelse(grepl('White', survtwo$race)==T, 1, 0)
survtwo$white
survtwo$black <- ifelse(grepl('Black', survtwo$race)==T, 1, 0)
survtwo$black
survtwo$asian <- ifelse(grepl('Asian', survtwo$race)==T, 1, 0)
survtwo$asian
survtwo$hispanic <- ifelse(grepl('Hispanic', survtwo$race)==T, 1, 0)
survtwo$hispanic


###### survtwo_pass ######
## passed attention check 
survtwo_pass= survtwo %>% filter(Attention_dum==1 & comp==1)
dim(survtwo)  
dim(survtwo_pass) #673   

 


###################################
##### Data Analysis for Survey 2 #####
##################################
 
#---------------------------------

######  Table 2 (main text) #######

#---------------------------------

##  subset data by treatment condition
survtwo$cond %>% table
con_sec= survtwo %>% filter(cond== 'sec_con')
opp_sec= survtwo %>% filter(cond == 'sec_opp')
con_ec= survtwo %>% filter(cond== 'ec_con')
opp_ec= survtwo %>% filter(cond == 'ec_opp')


## get number of observations for each condition
nrow(con_sec)
nrow(con_ec)
nrow(opp_sec)
nrow(opp_ec)

#### Put average values of support into table for the following DVs:

##Benefit_Am 
t.test(con_ec$Benefit_Am, opp_ec$Benefit_Am) #A,C
t.test(con_sec$Benefit_Am, opp_sec$Benefit_Am) #B,D

##Benefit_US 
t.test(con_ec$Benefit_US, opp_ec$Benefit_US) #A,C
t.test(con_sec$Benefit_US, opp_sec$Benefit_US) #B,D

##national interest 
t.test(con_ec$natint, opp_ec$natint) #A,C
t.test(con_sec$natint, opp_sec$natint) #B,D


#---------------------------------

####  Figure 3 (main text) #########

#---------------------------------

survtwo = survtwo %>% mutate(
  treat2 = ifelse(treat==1, 'Yes', 'No')
)
survtwo$treat2 %>% table
survtwo$treat %>% table
survtwo$bentype2 <- as.factor(survtwo$bentype)

## Interaction for benefit americans 
m3= lm(Benefit_Am ~  treat2 * bentype2  , data = survtwo)
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))
f3= cat_plot(m3, pred = 'treat2', modx = 'bentype2',
             interval = T,robust = 'HC1', geom = 'line', point.shape = T, 
             x.label = "Concentration Suggested",
             y.label = 'Benefit Americans',
             legend.main = 'Benefit Type', colors = c('grey60', 'black'),
             int.width = .95, errorbar.width = .05)+  
  scale_y_continuous(breaks = seq(3, 4.25, .5), limits = c(3, 4.3))+
  theme(legend.position = 'none', 
        axis.title.y  =element_text(size=18,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x=element_text(size=14,face="bold"),
        legend.text = element_text(size=15),
        legend.title = element_text(size = 15 ))

## Interaction for benefit US 
m4= lm(Benefit_US ~ treat2 * bentype2,survtwo )
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))
m4r
f4= cat_plot(m4, pred = 'treat2', modx = 'bentype2',
             interval = T,robust = 'HC1', geom = 'line', point.shape = T, 
             x.label = "Concentration Suggested",
             y.label = 'Benefit U.S.',
             legend.main = 'Benefit Type', colors = c('grey60', 'black'),
             int.width = .95, errorbar.width = .05)+  
  scale_y_continuous(breaks = seq(3, 4.25, .5), limits = c(3, 4.3))+
  theme(legend.position = 'none', 
        axis.title.y  =element_text(size=18,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x=element_text(size=14,face="bold"),
        legend.text = element_text(size=15),
        legend.title = element_text(size = 15 ))

## Interaction for national interest
m5= lm(natint ~ treat2 * bentype2 , data = survtwo)
m5r= coeftest(m5, vcov = vcovHC(m5, type="HC1"))
m5r
f5= cat_plot(m5, pred = 'treat2', modx = 'bentype2',
             interval = T,robust = 'HC1', geom = 'line', point.shape = T, 
             x.label = "Concentration Suggested",
             y.label = 'National Interest',
             legend.main = 'Benefit Type', colors = c('grey60', 'black'),
             int.width = .95, errorbar.width = .05)+  
  scale_y_continuous(breaks = seq(3, 4.25, .5), limits = c(3, 4.3))+
  theme(legend.position = 'none', 
        axis.title.y  =element_text(size=18,face="bold"),
        axis.text.x = element_text(size=11,face="bold"),
        axis.title.x=element_text(size=14,face="bold"),
        legend.text = element_text(size=15),
        legend.title = element_text(size = 15 ))



f<-  ggpubr::ggarrange(f3, f4,f5, ncol=3, common.legend = TRUE, legend="bottom")
f
 ggsave(f,width = 12, height = 5.5 ,filename = "fig3.pdf")


#---------------------------------

#### Figure 4 Density Graph (main text) ####

#---------------------------------

a= survtwo %>% filter(bentype=='economic') %>%
  ggplot (aes(Benefit_Am, fill= as.factor(treat))) + 
  geom_density(adjust= 1.7, alpha=0.7 ) +
  labs(fill="Condition")+theme_clean() + 
  xlab('Benefit Americans')+
  scale_fill_manual(values = c('grey75', 'grey0'),
                    name= 'Concentration Suggested',
                    labels= c('No','Yes'))+
  ylim(0, 0.38)+
  ggtitle('(B) Benefit: Oil Fields')
b= survtwo %>% filter(bentype=='security') %>% 
  ggplot (aes(Benefit_Am, fill= as.factor(treat))) + 
  geom_density(adjust= 2.5, alpha=0.7 ) +
  labs(fill="Condition")+theme_clean()+ 
  xlab('Benefit Americans')+
  scale_fill_manual(values = c('grey75', 'grey0'),
                    name= 'Concentration Suggested',
                    labels= c('No','Yes'))+ ylim(0, 0.38)+
  ggtitle('(A) Benefit: Strategic Locations')
f= ggpubr::ggarrange(b, a, ncol=2, common.legend = TRUE, legend="bottom")
f 
ggsave(f,width = 10.5, height = 5.2 ,filename= "fig4.pdf")
 

##################################
########## Appendix ##############
##################################
 
    
#-------------------------------------------

## Appendix B1: Changes in Variances #### 

#-------------------------------------------
 
#Check observations
survtwo$cond %>% table(useNA = 'ifany')

## variance for Benefit Americans 
tapply(survtwo$Benefit_Am, survtwo$cond, var)  
car::leveneTest(Benefit_Am~ as.factor(treat), data= survtwo %>% filter(bentype=='economic'))
car::leveneTest(Benefit_Am~ as.factor(treat), data= survtwo %>% filter(bentype=='security'))

## variance for Benefit US 
tapply(survtwo$Benefit_US, survtwo$cond, var)  
car::leveneTest(Benefit_US~ as.factor(cond), data= survtwo %>% filter(bentype=='economic'))
car::leveneTest(Benefit_US~ as.factor(cond), data= survtwo %>% filter(bentype=='security'))

## variance for natint  
tapply(survtwo$natint, survtwo$cond, var)  
car::leveneTest(natint~ as.factor(cond), data= survtwo %>% filter(bentype=='economic'))
car::leveneTest(natint~ as.factor(cond), data= survtwo %>% filter(bentype=='security'))

#--------------------------------------

## Appendix B2: interaction regression tables for Figure 3 ####

#--------------------------------------

###############  All sample (Table B2) ###############
m1= lm(Benefit_Am~ treat*bentype, survtwo) 
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2= lm(Benefit_US~ treat*bentype, survtwo) 
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m3= lm(natint~ treat*bentype, survtwo) 
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))

stargazer(m1r, m2r, m3r)
stargazer(m1, m2, m3) #to get r2 and obs


########### Passed attention checks (Table B3) ############
m1= lm(Benefit_Am~ treat*bentype, survtwo_pass) 
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2= lm(Benefit_US~ treat*bentype, survtwo_pass) 
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m3= lm(natint~ treat*bentype, survtwo_pass) 
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))

stargazer(m1r, m2r, m3r)
stargazer(m1, m2, m3) #to get r2 and obs



#--------------------------------------

## Appendix B3: interaction regression tables (other DVs) #########

#--------------------------------------


############### All sample (Table B4) ####################
m1= lm(milop_five~ treat*bentype, survtwo) 
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2= lm(Benefit_me~ treat*bentype, survtwo) 
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m3= lm(pride~ treat*bentype, survtwo) 
m3r= coeftest(m3, vcov = vcovHC(m4, type="HC1"))

m4= lm(tax_num~ treat*bentype, survtwo) 
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))

m5= lm(percare~ treat*bentype, survtwo) 
m5r= coeftest(m5, vcov = vcovHC(m5, type="HC1"))

stargazer(m1r, m2r, m3r, m4r, m5r) #table
stargazer(m1, m2, m3, m4, m5) #get obs and r2

############# Passed attention check (Table B5) ###############
m1= lm(milop_five~ treat*bentype, survtwo_pass) 
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2= lm(Benefit_me~ treat*bentype, survtwo_pass) 
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m3= lm(pride~ treat*bentype, survtwo_pass) 
m3r= coeftest(m3, vcov = vcovHC(m4, type="HC1"))

m4= lm(tax_num~ treat*bentype, survtwo_pass) 
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))

m5= lm(percare~ treat*bentype, survtwo_pass) 
m5r= coeftest(m5, vcov = vcovHC(m5, type="HC1"))

stargazer(m1r, m2r, m3r, m4r, m5r) #table
stargazer(m1, m2, m3, m4, m5) #get obs and r2


#-------------------------------------------

## Appendix B4: Balance Table #### 

#-------------------------------------------

## generate balance table 
sumtable(survtwo, 
         vars =c('gender', 'age',
                 'partyid', 'ideology', 'college',  
                 'white', 'black', 'asian', 'hispanic',
                 'russia_comp',  'mil_comp', 'nat_comp'),
         group = 'cond',
         out = 'latex', 
         digits= 2,
         group.test = T)  

#### weight results by party ######

## subset data
survtwo$partyid %>% table %>% prop.table()
survtwo_filter = survtwo %>% dplyr::select(milop, milop_five, natint, Benefit_US, 
                                   Benefit_Am, Benefit_me, pride, percare, partyid, 
                                   cond, treat, bentype)

## get proportions for parties and prep vars 
survtwo_filter$partyid %>% table %>% prop.table()
survtwo_filter$partyidnum= survtwo_filter$partyid %>% factor %>% as.numeric
survtwo_filter$partyidnum %>% table %>% prop.table()


## prepare data 
data.svy.unweighted <- svydesign(ids=~1, data=survtwo_filter)
partyid_dist = data.frame(partyidnum = c("1", "2", "3", "4"),
                          Freq = nrow(survtwo_filter) * c(0.39, 0.26, 0.06, 0.29))
partyidnum <- survtwo_filter$partyidnum

## make new data with adjusted proportions
survtwo_rake <- rake(design = data.svy.unweighted,
                     sample.margins = list(~partyidnum),
                     population.margins = list(partyid_dist))
 
## original results 
c= lm(natint~ treat*bentype, survtwo)  
b= lm(Benefit_US~ treat*bentype, survtwo)  
a= lm(Benefit_Am~ treat*bentype, survtwo)  

## weighted results 
b1= svyglm(Benefit_US ~ treat*bentype,
           family = gaussian(),
           data   = survtwo_filter,
           design = survtwo_rake)  

a1= svyglm(Benefit_Am~ treat*bentype,
           family = gaussian(),
           data   = survtwo_filter,
           design = survtwo_rake)  

c1= svyglm(natint ~ treat*bentype,
           family = gaussian(),
           data   = survtwo_filter,
           design = survtwo_rake) 

# print results 
stargazer(a,b,c, a1,b1,c1)

 
#################################################################################
######################## Survey 3 ########################
#################################################################################



#-----------------------------------------
## Data Prep ####
#-----------------------------------------

survthree <- read_csv("surveythree.csv")  
names(survthree)
dim(survthree)
survthree= survthree[-c(1,2),] 
#filter dataset to those who passed initial screening
survthree = survthree %>% filter(is.na(comp1)==F)
dim(survthree) #677 
#time spent:
survthree$`Duration (in seconds)` %>% as.numeric() %>% mean /60 
#check duplicates (none)
survthree$ResponseId %>% unique %>% length #677 

 
#### comprehension qs #####
survthree$oil <- as.numeric(survthree$oil)

survthree$Attention %>% table(useNA = 'ifany')
survthree= survthree %>% mutate(
  Attention_dum= recode(Attention, 'Seal Island'= '1', 'Snail Island'='0',
                        'Sun Island'= '0', 'Smile Island'= '0', .defalt= NA_character_  ) %>% as.numeric()
)
survthree$Attention_dum  %>% mean #97% pass
 
## get comprehension questions 
survthree$compcontrol= ifelse(
  survthree$oil ==0 &
    survthree$comp1==  "The U.S. and Russia"&
    survthree$comp22 == 'No' &
    survthree$comp3 == "Yes" &
    survthree$comp10== 'No' &
    survthree$comp4 =="Lead to ${e://Field/cost_short_dc}"&
    survthree$comp5== "All Americans" &
    survthree$comp6 =="Better protection from foreign security threats" &
    survthree$comp8 == "Not as large as what some policymakers argue"
  ,1,0
)
survthree$compopp= ifelse(
  survthree$oil ==1 &
    survthree$comp1== "The U.S. and Russia"&
    survthree$comp21 == "Yes" &
    survthree$comp3 == "Yes" &
    survthree$comp10== 'No' &
    survthree$comp4 =="Lead to ${e://Field/cost_short_dc}"&
    survthree$comp5== "All Americans" &
    survthree$comp6  =="Better protection from foreign security threats" &
    survthree$comp8 == "Not as large as what some policymakers argue"  
  ,1,0
)
survthree= survthree %>% mutate(
  comp=ifelse(compcontrol==1 | compopp==1 ,1 ,0)
)
survthree$comp %>% table(useNA = 'ifany') %>% prop.table() #69
 
######## DVs  ########
survthree$milop %>% table
survthree$milopneut %>% table

survthree= survthree %>% mutate(
  milop_text= milop,
  milop_text= ifelse(milop_text== "Neither approve nor disapprove", milopneut, milop_text)
)
 
## change to numbers
survthree= survthree %>% mutate(
  milop= recode(milop_text, 'Strongly approve'= '7', 'Approve'= '6',
                "lean toward approving"= "5", "do not lean either way"= "4",
                "lean toward disapproving"="3",
                'Disapprove'='2', 'Strongly disapprove'= "1", 
                .default= NA_character_) %>% as.numeric,
  milop_five= recode(milop_text, 'Strongly approve'= '5', 'Approve'= '4',
                     "lean toward approving"= "3", "do not lean either way"= "3",
                     "lean toward disapproving"="3",
                     'Disapprove'='2', 'Strongly disapprove'= "1", 
                     .default= NA_character_) %>% as.numeric 
)
 
## change to numeric  
survthree= survthree %>% mutate (
  Benefit_Am= recode(Ben_Am, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                     'Neither agree nor disagree'='3', 
                     'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                     .default= NA_character_) %>% as.numeric,
  Benefit_US= recode(Ben_US, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                     'Neither agree nor disagree'='3', 
                     'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                     .default= NA_character_) %>% as.numeric,
  Benefit_me= recode(Ben_me, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                     'Neither agree nor disagree'='3', 
                     'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                     .default= NA_character_) %>% as.numeric,
  natint= recode(USnat, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                 'Neither agree nor disagree'='3', 
                 'Somewhat disagree'='2', 'Strongly disagree'= "1",
                 .default= NA_character_) %>% as.numeric,
  tax_num = recode(Tax, 'Very willing'='5', "Somewhat willing"= "4",
                   "Neither willing nor unwilling"= '3',
                   'Somewhat unwilling'='2', 'Very unwilling'= "1", 
                   .default= NA_character_) %>% as.numeric)
 


#### Mediators ####

survthree$motive %>% table
survthree$Q79 %>% table

survthree= survthree %>% mutate (
  diffmotive= recode(motive, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                     'Neither agree nor disagree'='3', 
                     'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                     .default= NA_character_) %>% as.numeric,
  secmotive= recode(Q79, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                    'Neither agree nor disagree'='3', 
                    'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                    .default= NA_character_) %>% as.numeric)
survthree$diffmotive %>% table
survthree$secmotive %>% table

 
 
####### Controls #######
 
### making composite russia score
survthree= survthree %>% mutate(
  russia1= recode(Russia1, 'Russia is not an adversary'= '1', 
                  'Russia is a serious problem but not an adversary'='0.5',
                  'Russia is an adversary'= '0', 
                  .default= NA_character_  ) %>% as.numeric(),
  russia2= recode(Russia2, 'The U.S. should pursue friendly cooperation with Russia'= '1', 
                  "The U.S. should actively work to limit Russia's power"='0',.default= NA_character_  ) %>% as.numeric(),
  russia3= recode(Russia3, 'Russia is trying to undermine U.S. power and influence'= '0', 
                  'Russia is trying to pursue friendly cooperation with the United States'='1',
                  .default= NA_character_  ) %>% as.numeric()
)
# higher score indicates more friendliness

survthree$russia1 %>% table
survthree$Russia1 %>% table
survthree$russia2 %>% table
survthree$Russia2 %>% table
survthree$russia3 %>% table
survthree$Russia3 %>% table

survthree= survthree %>% mutate(
  russia_comp= (russia1+ russia2+ russia3)/3
)

### Age
survthree$age= survthree$age %>% as.numeric

### misc 
survthree= survthree %>% mutate(
  proind= recode(taxcut,  'Definitely yes'= '5', 'Probably yes'= '4',
                 'Might or might not'='3', 
                 'Probably not'='2', 'Definitely not'= "1", 
                 .default= NA_character_) %>% as.numeric,
  # Do you think U.S. politicians are, in general, trying to act in the best interest of the country?
  poltrust= recode(Q157, 'Definitely yes'= '5', 'Probably yes'= '4',
                   'Might or might not'='3', 
                   'Probably not'='2', 'Definitely not'= "1", 
                   .default= NA_character_) %>% as.numeric,
  #more support to fossil fuel 
  profuel1= recode(Q55_4, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                   'Neither agree nor disagree'='3', 
                   'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                   .default= NA_character_) %>% as.numeric,
  # should drill for more oil 
  profuel2= recode(Q99_2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                   'Neither agree nor disagree'='3', 
                   'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                   .default= NA_character_) %>% as.numeric,
  profuel =  (profuel1+ profuel2) /2
  
)
 

### Militant
survthree= survthree %>% mutate(
  #military force makes worse
  milnum1= recode(Q55_1, 'Strongly agree'= '1', 'Somewhat agree'= '2',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'="4", 'Strongly disagree'= "5", 
                  .default= NA_character_) %>% as.numeric,
  #peace through strength 
  milnum2= recode(Q99_1, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  # improve military over other
  milnum3= recode(Q99_4, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  mil_comp =  (milnum1+ milnum2+ milnum3) /3
)
survthree$mil_comp %>% table(useNA = 'ifany')



### Nationalistic

survthree= survthree %>% mutate(
  natnum1= recode(attnat_1, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  natnum2= recode(attnat_2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  nat_comp =  (natnum1+ natnum2) /2
  
)
#proud to be American
survthree$natnum1 %>% table
#other countries should be like US
survthree$natnum2 %>% table
survthree$nat_comp %>% table


## mobility 
survthree= survthree %>% mutate(
  # start out poor become rich
  mobnum3= recode(Q55_3, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric,
  # the current U.S. economic or political system can be trusted to appropriately redistribute wealth and income in the American society
  mobnum1= recode(Q99_3, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1",
                  .default= NA_character_) %>% as.numeric,
  #everyone equal opportunity 
  mobnum2= recode(attnat_4, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                  'Neither agree nor disagree'='3', 
                  'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                  .default= NA_character_) %>% as.numeric , 
  mob_comp =  (mobnum1+ mobnum2+ mobnum3) /3)


### Authoritarian
survthree= survthree %>% mutate(
  authoritarian= recode(Q55_2, 'Strongly agree'= '5', 'Somewhat agree'= '4',
                        'Neither agree nor disagree'='3', 
                        'Somewhat disagree'='2', 'Strongly disagree'= "1", 
                        .default= NA_character_) %>% as.numeric)

survthree$authoritarian %>% table


### Education ###
survthree$educ %>% table (useNA = 'ifany' )
survthree= survthree %>% mutate(
  college= recode(educ,
                  "Currently pursuing or received a graduate degree (ex. MA, PhD, JD, MD)" ="1",
                  "Received a Bachelor's degree in college (4-year)" = "1",
                  "Currently pursuing a Bachelor's degree in college (4-year)" ="1",
                  .default= "0") %>% as.numeric)
survthree$college %>% table
 

### Income ### 


survthree$hincome%>% table (useNA = 'ifany')
survthree= survthree %>% mutate(
  income= recode(hincome, "$150,000 or more" = "6", "$100,000 to $149,999"= "5",
                 "$75,000 to $99,999"= "4", 
                 "$50,000 to $74,999"= "3", "$35,000 to $49,999" = "2", 
                 "$25,000 to $34,999" = '1',   "Less than $25,000" = "1",
                 .default= NA_character_) %>% as.numeric()
) 
survthree$income %>% table(useNA = 'ifany')
survthree$hincome <- as.factor(survthree$hincome)


## Gender ##
survthree$gender_char%>% table(useNA = 'ifany')
survthree= survthree %>% mutate(
  gender= recode(gender_char, "Female" ='1' , 'Male'= '0', .default = NA_character_) %>% as.numeric
)
survthree$gender %>% table(useNA = 'ifany')
  

### partyid 
survthree$partylean %>% table
survthree= survthree %>% mutate(
  party= recode(partyid, 
                'Republican'= 'rep', "Democrat"= "dem", 
                "Independent"= "indep", .default= NA_character_),
  partytwo= case_when(party == 'rep' | partylean =='Closer to the Republican Party' ~'rep',
                      party == 'dem' | partylean =='Closer to the Democratic Party' ~'dem',
                      T ~ 'indep'),
  #combine 23 'other' respondents with independents
  partythree= case_when(partyid == 'Republican' ~ 'Republican',
                      partyid == 'Democrat' ~ 'Democrat',
                      partyid == 'Independent' ~ 'Independent',
                      partyid == 'Other' ~ 'Independent',
                      T ~ NA_character_)
  
)
survthree$partytwo %>% table(useNA = 'ifany' )
survthree$party%>% table (useNA = 'ifany' )
survthree$partythree %>% table(useNA = 'ifany' )
survthree$partyid %>% table (useNA = 'ifany' )

## ideology

survthree= survthree%>% mutate(
  ideology= polview,
  polview= recode(ideology, "Conservative"= "5", "Slightly conservative"= "4",
                  "Neither liberal nor conservative"= '3', "Slightly liberal"= "2",
                  "Liberal"= "1",
                  .default= NA_character_) %>% as.numeric
)
survthree$polview %>% table
survthree$ideology %>% table

#### race

survthree$race %>% table
survthree$white <- ifelse(grepl('White', survthree$race)==T, 1, 0)
survthree$black <- ifelse(grepl('Black', survthree$race)==T, 1, 0)
survthree$asian <- ifelse(grepl('Asian', survthree$race)==T, 1, 0)
survthree$hispanic <- ifelse(grepl('Hispanic', survthree$race)==T, 1, 0)


###### survthree_pass ####
## passed attention checks 

survthree_pass= survthree %>% filter(Attention_dum==1 & comp==1)
dim(survthree_pass) #466

 

###########################
###### Data Analysis ######
###########################
 
#-------------------------------------------

### Table 3: main regression #######

#-------------------------------------------

m1= lm(milop_five~ oil, survthree) 
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m4= lm(Benefit_US~ oil, survthree) 
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))

m5= lm(Benefit_Am~ oil, survthree) 
m5r= coeftest(m5, vcov = vcovHC(m5, type="HC1"))

m2= lm(natint~ oil, survthree) 
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1")) 

m3= lm(tax_num~ oil, survthree) 
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))

stargazer(m1r, m5r, m4r, m2r, m3r) ## main table 
stargazer(m1,  m5,  m4, m2, m3) ## get obs 

#-------------------------------------------

### Figure 5: Testing H3 mechanisms  #######

#-------------------------------------------
 
################  Figure 5(a) ################

violin_sec = survthree %>% ggplot(aes(x=as.factor(oil), y=secmotive)) +
  geom_violin(trim=FALSE, adjust=1.7,  fill='#A4A4A4')+
  geom_boxplot(width=0.1) + theme_classic()  + 
  ylim(1,5.5) +labs(  y= "Level of Agreement", 
                      x= "\nEconomic Value")+
  scale_x_discrete(labels = c('No', "Yes (Concentration Suggested)"))+ 
  theme(text = element_text(size = 15, face='bold'))  
violin_sec
  ggsave(violin_sec, filename= "fig5a.pdf", height = 6, width=5)


################  Figure 5(b) ################

## run regression M -> Y 
m1= lm(milop_five~ secmotive, survthree)
m2= lm(Benefit_Am~ secmotive, survthree)
m3= lm(Benefit_US~ secmotive, survthree)
m4= lm(natint~ secmotive, survthree)
m5= lm(tax_num~ secmotive, survthree)

## extract coefficients and standard errors

milop_table <-  cbind(
  m1$coefficients[2],
  sqrt(diag(vcov(m1)))[2]
)  %>% as.data.frame()
colnames(milop_table) <- (c('est', 'se'))
milop_table <- milop_table %>% mutate(
  ci_upper = est+se*1.96,
  ci_lower= est -se* 1.96 ,
  DVs = 'Support Mil'
)
milop_table

Benefit_Am_table <-  cbind(
  m2$coefficients[2],
  sqrt(diag(vcov(m2)))[2]
)  %>% as.data.frame()

colnames(Benefit_Am_table) <- (c('est', 'se'))
Benefit_Am_table <- Benefit_Am_table %>% mutate(
  ci_upper = est+se*1.96,
  ci_lower= est -se* 1.96 ,
  DVs = 'Benefit Americans'
)
Benefit_Am_table


Benefit_US_table <-  cbind(
  m3$coefficients[2],
  sqrt(diag(vcov(m3)))[2]
)  %>% as.data.frame()

colnames(Benefit_US_table) <- (c('est', 'se'))
Benefit_US_table <- Benefit_US_table %>% mutate(
  ci_upper = est+se*1.96,
  ci_lower= est -se* 1.96 ,
  DVs = 'Benefit US'
)
Benefit_US_table

natint_table <-  cbind(
  m4$coefficients[2],
  sqrt(diag(vcov(m4)))[2]
)  %>% as.data.frame()

colnames(natint_table) <- (c('est', 'se'))
natint_table <- natint_table %>% mutate(
  ci_upper = est+se*1.96,
  ci_lower= est -se* 1.96 ,
  DVs = 'NatInt'
)
natint_table

tax_num_table <-  cbind(
  m5$coefficients[2],
  sqrt(diag(vcov(m5)))[2]
)  %>% as.data.frame()

colnames(tax_num_table) <- (c('est', 'se'))
tax_num_table <- tax_num_table %>% mutate(
  ci_upper = est+se*1.96,
  ci_lower= est -se* 1.96 ,
  DVs = 'Contribute Money'
)
tax_num_table

## bind the extracted coefficients together
final_table= rbind(milop_table,
                   Benefit_US_table, Benefit_Am_table,  natint_table, tax_num_table)  %>% as.data.frame()

## plot 
fig <- ggplot( final_table)+
  geom_pointrange(aes( x= factor(DVs, levels = c( 'Contribute Money' ,  'NatInt' ,
                                                  'Benefit US' , 'Benefit Americans',
                                                  'Support Mil'  )),
                       y=  est, ymin= ci_lower, ymax= ci_upper,shape= DVs  ),
                  
                  lwd = 1/2 )  +
  geom_hline(yintercept = 0, colour = "black", lty = 2) + ylim(-0.2, 0.7)+
  coord_flip() + 
  ylab("Coefficient of ``Matter of National Security`` on DVs") +
  scale_x_discrete(labels=c('Support Mil'= 'Support \nMilitary Use',
                            'NatInt'=  "National\nInterest",
                            'Benefit US' =   'Benefit U.S.',
                            'Benefit Americans'=  'Benefit\nAmericans', 'Contribute Money'=  'Contribute\nMoney'))+
  xlab("") + theme_classic()+   
  theme( axis.title= element_text(size=15,face="bold"),
         axis.text=element_text(size=13,face="bold"),
         legend.position = 'none')
fig
 ggsave(fig, filename = 'fig5b.pdf', height=4.5, width= 7)

 

#--------------------------------------------

## Appendix C1: Full Causal Mediation   #######

#--------------------------------------------

## using lavaan 

## military operation 
model <- 'milop ~ b*secmotive + c*oil + partyid+ college+ gender+ income
secmotive ~ a*oil
indirect := a*b
total := c + (a*b)
direct := total -indirect'
milopfit <- sem(model, data=survthree,
                se="bootstrap", iseed= 123)
summary(milopfit)

## benefit americans 
model <- 'Benefit_Am ~ b*secmotive + c*oil + partyid+ college+ gender+ income
secmotive ~a*oil
indirect := a*b
total := c + (a*b)
direct := total -indirect'
Benefit_Amfit <- sem(model, data=survthree,
                     se="bootstrap", iseed= 123)
summary(Benefit_Amfit)

## benefit US 
model <- 'Benefit_US ~ b*secmotive + c*oil + partyid+ college+ gender+ income
secmotive ~ a*oil
indirect := a*b
total := c + (a*b)
direct := total -indirect'
Benefit_USfit <- sem(model, data=survthree,
                     se="bootstrap", iseed= 123)
summary(Benefit_USfit)

## national interest 
model <- 'natint ~ b*secmotive + c*oil +    partyid+ college+ gender+ income
secmotive ~ a*oil
indirect := a*b
total := c + (a*b)
direct := total -indirect'
natintfit <- sem(model, data=survthree,
                 se="bootstrap", iseed= 123)
summary(natintfit)

## pay taxes 
model <- 'tax_num ~b*secmotive + c*oil +    partyid+ college+ gender+ income
secmotive ~ a*oil
indirect := a*b
total := c + (a*b)
direct := total -indirect'
tax_numfit <- sem(model, data=survthree,
                  se="bootstrap", iseed= 123)
summary(tax_numfit)


####### Mediation using Causal Inference Framework ########
## these results have been taken out to meet the page limits in the appendix
# 
# total= lm (milop~ secmotive+ oil, survthree)
# medfit= lm(secmotive ~oil, survthree)
# set.seed(123)
# milop_mod= mediate(model.m = medfit, model.y = total, 
#                    treat = 'oil', mediator = 'secmotive', sims=500)  
# set.seed(123)
# milop_mod= mediate(model.m = medfit, model.y = total, 
#                    treat = 'oil', mediator = 'secmotive', sims=1000)  
# summary(milop_mod)
# plot(milop_mod)
# str(milop_mod)
# stargazer(milop_mod$model)
# 
# total= lm (natint~ secmotive+ oil, survthree)
# medfit= lm(secmotive ~oil, survthree)
# set.seed(123)
# natint_mod= mediate(model.m = medfit, model.y = total, 
#                     treat = 'oil', mediator = 'secmotive', sims=1000)  
# summary(natint_mod)
# plot(natint_mod)
# 
# total= lm (Benefit_US~ secmotive+ oil, survthree)
# medfit= lm(secmotive ~oil, survthree)
# set.seed(123)
# Benefit_US_mod= mediate(model.m = medfit, model.y = total, 
#                         treat = 'oil', mediator = 'secmotive', sims=1000)  
# summary(Benefit_US_mod)
# plot(Benefit_US_mod)
# 
# total= lm (tax_num~ secmotive+ oil, survthree)
# medfit= lm(secmotive ~oil, survthree)
# set.seed(123)
# tax_num_mod= mediate(model.m = medfit, model.y = total, 
#                      treat = 'oil', mediator = 'secmotive', sims=1000)  
# summary(tax_num_mod)
# plot(tax_num_mod)
# 
# total= lm (Benefit_Am~ secmotive+ oil, survthree)
# medfit= lm(secmotive ~oil, survthree)
# set.seed(123)
# Benefit_Am_mod= mediate(model.m = medfit, model.y = total, 
#                         treat = 'oil', mediator = 'secmotive', sims=1000)  
# summary(Benefit_Am_mod)
# plot(Benefit_Am_mod)

#--------------------------------------------

## Appendix C2:  Table 3 robustness checks ####

#--------------------------------------------
 

###### Passed attention checks (Appendix C2.1) #######

m1= lm(milop_five~ oil, survthree_pass) 
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2= lm(natint~ oil, survthree_pass) 
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m4= lm(Benefit_US~ oil, survthree_pass) 
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))

m5= lm(Benefit_Am~ oil, survthree_pass) 
m5r= coeftest(m5, vcov = vcovHC(m5, type="HC1"))

m6= lm(Benefit_me~ oil, survthree_pass) 
m6r= coeftest(m6, vcov = vcovHC(m6, type="HC1"))

m3= lm(tax_num~ oil, survthree_pass) 
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))

stargazer(  m1,  m5,  m4, m2, m3)
stargazer(m1r, m5r, m4r,   m2r, m3r    )

###### Table 3 with covariates (Appendix C2.2) #######

m1= lm(milop~ oil + partythree+ income+college+ gender+  college+ gender+ russia_comp+
         mil_comp+ nat_comp, survthree) 
m1r= coeftest(m1, vcov = vcovHC(m1, type="HC1"))

m2= lm(natint~ oil + partythree+ income+college+ gender+  college+ gender+ russia_comp+
         mil_comp+ nat_comp, survthree) 
m2r= coeftest(m2, vcov = vcovHC(m2, type="HC1"))

m4= lm(Benefit_US~ oil + partythree+ income+college+ gender+  college+ gender+ russia_comp+
         mil_comp+ nat_comp, survthree) 
m4r= coeftest(m4, vcov = vcovHC(m4, type="HC1"))

m5= lm(Benefit_Am~ oil + partythree+ income+college+ gender+  college+ gender+ russia_comp+
         mil_comp+ nat_comp, survthree) 
m5r= coeftest(m5, vcov = vcovHC(m5, type="HC1"))

m3= lm(tax_num~ oil + partythree+ income+college+ gender+  college+ gender+ russia_comp+
         mil_comp+ nat_comp, survthree) 
m3r= coeftest(m3, vcov = vcovHC(m3, type="HC1"))

stargazer(m1r, m5r, m4r, m2r, m3r) ## main table 
stargazer(m1,  m5,  m4, m2, m3) ## get obs 


#--------------------------------------------

## Appendix C3:  balance test ####

#--------------------------------------------
 
sumtable(survthree, 
         vars =c('gender', 'age',
                 'partyid', 
                 'ideology', 
                 'college',   
                 'white', 'black', 'asian', 'hispanic',
                 'russia_comp',  'mil_comp', 'nat_comp'),
         group = 'oil',
         out = 'latex', 
         digits= 2,
         group.test = T)


 