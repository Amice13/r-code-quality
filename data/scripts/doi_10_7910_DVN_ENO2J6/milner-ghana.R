rm(list=ls())

library(stargazer)
library(foreign)
library(sandwich)
library(lmtest)
library(xtable)
library(ggplot2)
library(gtsummary)
library(tidyverse)
library(haven)

setwd("")

load("ghana_raw.RData")
load("abr6_clean.RData")
load("abr8_clean.RData")

##### DEPENDENT VARIABLE #####

# should be easier for other countries to buy and sell their goods and services in ghana
ghana$out_trade_easier_dum <- as.numeric(ghana$out_trade_easier%in%c("Somewhat agree","Strongly agree"))
ghana$out_trade_easier_dum[is.na(ghana$out_trade_easier)] <- NA

mean(ghana$out_trade_easier_dum,na.rm=TRUE)

###### EXPLANATORY AND CONTROL VARIABLES #####

ghana$int_edu_cont <- as.numeric(ordered(ghana$int_edu,levels=c("No schooling","Some primary","Completed Primary", "Some secondary school",
                                                           "Completed Secondary","Some university or polytechnic","Completed University or polytechnic",
                                                           "Completed post-graduate training")))

ggplot(ghana,aes(x=int_edu_cont))+geom_histogram(bins=8,binwidth=.5)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),
        ,axis.ticks.y = element_blank(),axis.text.y=element_blank())+xlab("Education (1-8)")+ylab("Count")
ggsave(file="milner-ghana-output/hist-edu.pdf")

ghana$int_edu_primary <- as.numeric(ghana$int_edu_cont>=3)
ghana$int_edu_secondary <- as.numeric(ghana$int_edu_cont>=5)
ghana$int_edu_college <- as.numeric(ghana$int_edu_cont>=6)

ghana$int_religion <- as.factor(ghana$int_religion)
ghana$int_eth_large <- as.factor(ghana$int_eth_large)
ghana$int_eth_pres <- as.numeric(ghana$int_ethnicity=="GONJA")
ghana$int_pres_ghana_correct <- as.numeric(ghana$int_pres_ghana=="Correct")
ghana$int_court_ghana_correct <- as.numeric(ghana$int_court_ghana=="Correct")
ghana$int_mp_ghana_correct <- as.numeric(ghana$int_local_mp=="Yes"&ghana$int_mp_answer=="Correct")
ghana$int_pol_knowledge <- ghana$int_pres_ghana_correct+ghana$int_court_ghana_correct+ghana$int_mp_ghana_correct

ghana$int_female <- as.numeric(ghana$int_female=="Female")

table(ghana$int_tv_use)
table(ghana$int_phone)
table(ghana$int_internet_use) # this seems like the most normal distribution, best proxy for assets
ghana$int_internet_use_ordinal <- NA
ghana$int_internet_use_ordinal[ghana$int_internet_use=="Never"] <- 1
ghana$int_internet_use_ordinal[ghana$int_internet_use%in%c("Less than once a month","Once a month","2-3 times a month","Once a week","2-3 times a week")] <- 2
ghana$int_internet_use_ordinal[ghana$int_internet_use=="Daily"] <- 3

ghana$int_natlid <- NA
ghana$int_natlid[ghana$int_ethnic_attach%in%c("I feel only Ghanian","I feel more Ghanaian than (ethnicity)")] <- 2
ghana$int_natlid[ghana$int_ethnic_attach%in%c("I feel equally Ghanaian and (ethnicity)")] <- 1
ghana$int_natlid[ghana$int_ethnic_attach%in%c("I feel only (ethnicity)","I feel more (ethnicity than Ghananaian")] <- 0

ghana$int_skill <- NA
ghana$int_skill[ghana$int_duties_none==1] <- 0
ghana$int_skill[ghana$int_duties_business==1] <- 1
ghana$int_skill[ghana$int_duties_labor==1] <- 1
ghana$int_skill[ghana$int_duties_clerical==1] <- 2
ghana$int_skill[ghana$int_duties_computer==1] <- 2
ghana$int_skill[ghana$int_duties_managing==1] <- 3
ghana$int_highskill <- as.numeric(ghana$int_skill>=2)

ggplot(ghana,aes(x=int_skill))+geom_histogram(bins=5,binwidth=.5)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),
        ,axis.ticks.y = element_blank(),axis.text.y=element_blank())+xlab("Skill (0-3)")+ylab("Count")
ggsave(file="milner-ghana-output/hist-skill.pdf")

ghana$int_income_combined <- rowSums(ghana[,c("int_income","int_household_income")],na.rm=TRUE)
ghana$int_income_combined_log <- log(ghana$int_income_combined+1)

ghana$publicsector <- as.numeric(ghana$int_income_source=="In a government job or a political position")
table(ghana$publicsector[ghana$int_occupation_status=="Employed"],ghana$int_edu_cont[ghana$int_occupation_status=="Employed"])
mean(ghana$int_edu_cont[ghana$publicsector==1&ghana$int_occupation_status=="Employed"],na.rm=TRUE)
mean(ghana$int_edu_cont[ghana$publicsector==0&ghana$int_occupation_status=="Employed"],na.rm=TRUE)

##### DESCRIPTIVE STATISTICS #####

abr6_gha <- abr6 %>%
  select(ccode,age,edu,female,pov_cash,pov_food,natlid,Q96A) %>%
  filter(ccode=="GHA") %>%
  mutate(ag = as.numeric(Q96A==3),
         sample = "Afrobarometer round 6") %>%
  select(age,edu,female,pov_cash,pov_food,natlid,ag,sample)

abr8_gha <- abr8 %>%
  select(ccode,age,edu,female,pov_cash,pov_food,natlid,Q95C) %>%
  filter(ccode=="GHA") %>%
  mutate(ag = as.numeric(Q95C==3),
    sample = "Afrobarometer round 8") %>%
  select(age,edu,female,pov_cash,pov_food,natlid,ag,sample)

ghana <- ghana %>%
  mutate(pov_cash = recode(as_factor(int_pov_cash),
                           "Never" = 1,
                           "Just once or twice" = 2,
                           "Several times" = 3,
                           "Many times" = 4,
                           "Always" = 5),
         pov_food = recode(as_factor(int_pov_food),
                           "Never" = 1,
                           "Just once or twice" = 2,
                           "Several times" = 3,
                           "Many times" = 4,
                           "Always" = 5),
         natlid = recode(as_factor(int_ethnic_attach),
                         "I feel only (ethnicity)" = 0,
                         "I feel more (ethnicity) than (Ghanaian)" = 0,
                         "I feel equally Ghanaian and (ethnicity)" = 1,
                         "I feel more Ghanaian than (ethnicity)" = 2,
                         "I feel only Ghanian" = 2))

ghana_desc <- ghana %>%
  select(int_age,int_edu_cont,int_female,pov_cash,pov_food,natlid,int_industry) %>%
  mutate(age = int_age,
         edu = int_edu_cont,
         female = int_female,
         ag = as.numeric(int_industry == "Agriculture, Forestry, Fishing, and Hunting"),
         sample = "Ghana (2016)") %>%
  select(age,edu,female,pov_cash,pov_food,natlid,ag,sample)

tibble <- bind_rows(abr6_gha,abr8_gha,ghana_desc) %>%
  mutate(age = as.numeric(age))

theme_gtsummary_compact()
tbl <-  tibble %>%
  tbl_summary(
    by = "sample",
    type = list(age ~ "continuous2", edu ~ "continuous2",pov_cash ~ "continuous2",pov_food ~ "continuous2",natlid ~ "continuous2"),
    statistic = list(all_continuous() ~ c("{mean} ({sd})", "[{min},{max}]"),
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    label = list(age ~ "Age",
                 edu ~ "Education",
                 female ~ "Female",
                 pov_cash ~ "Lacked Cash Income",
                 pov_food ~ "Lacked Food",
                 natlid ~ "National ID",
                 ag ~ "Works in Agriculture"),
    missing_text = "(NA)"
  )  %>%
  as_gt() %>%
  gt::as_latex()
cat(tbl[1],file="milner-ghana-output/descriptives.tex")

##### REPLICATE AFROBAROMETER ANALYSIS ######

reg7 <- glm(out_trade_easier_dum~int_edu_cont+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana)
 se7 <- coeftest(reg7, vcov = vcovCL(reg7, cluster = ghana$int_constituency))

reg8 <- glm(out_trade_easier_dum~int_edu_primary+int_edu_secondary+int_edu_college+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana)
 se8 <- coeftest(reg8, vcov = vcovCL(reg8, cluster = ghana$int_constituency))

reg9 <- glm(out_trade_easier_dum~int_edu_cont+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset=int_occupation_status=="Employed")
 se9 <- coeftest(reg9, vcov = vcovCL(reg9, cluster = ghana$int_constituency[ghana$int_occupation_status=="Employed"]))

reg10 <- glm(out_trade_easier_dum~int_edu_primary+int_edu_secondary+int_edu_college+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset=int_occupation_status=="Employed")
 se10 <- coeftest(reg10, vcov = vcovCL(reg10, cluster = ghana$int_constituency[ghana$int_occupation_status=="Employed"]))

reg11 <- glm(out_trade_easier_dum~int_edu_cont+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset=!int_occupation_status=="Employed")
 se11 <- coeftest(reg11, vcov = vcovCL(reg11, cluster = ghana$int_constituency[!ghana$int_occupation_status=="Employed"]))

reg12 <- glm(out_trade_easier_dum~int_edu_primary+int_edu_secondary+int_edu_college+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset=!int_occupation_status=="Employed")
 se12 <- coeftest(reg12, vcov = vcovCL(reg12, cluster = ghana$int_constituency[!ghana$int_occupation_status=="Employed"]))

reg13 <- glm(out_trade_easier_dum~int_edu_cont*int_occupation_status+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana)
 se13 <- coeftest(reg13, vcov = vcovCL(reg13, cluster = ghana$int_constituency))

cat(stargazer(reg7,reg8,reg9,reg10,reg11,reg12,reg13,
              se = list(se7[,2], se8[,2],se9[,2],se10[,2],se11[,2],se12[,2],se13[,2]),
              p = list(se7[,4], se8[,4],se9[,4],se10[,4],se11[,4],se12[,4],se13[,4]),
              column.sep.width = "-10pt",font.size="small",no.space = TRUE,
              title = "Relationship between education and support for free trade (Ghana 2016)",
              dep.var.labels = "Support for free trade (0-1)", label = "tab:ghana-edu",
              covariate.labels = c("Edu","Primary","Secondary","College","Occ:Student","Occ:Retired","Occ:Unemployed","Occ:Other","Age","Female","Pol Knowledge","Edu*Occ:Student","Edu*Occ:Retired","Edu*Occ:Unemployed","Edu*Occ:Other"),
              omit=c("int_religion","int_eth_large","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth"),
                               c("Sample","Full","Full","Employed","Employed","Not Employed","Not Employed","Full"))),
              file="milner-ghana-output/edu.tex", sep="\n")


# alternative measure of skill (only available for subset of employed)

se9

reg13 <- glm(out_trade_easier_dum~int_edu_cont+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset=is.na(int_skill)==FALSE)
se13 <- coeftest(reg13, vcov = vcovCL(reg13, cluster = ghana$int_constituency[is.na(ghana$int_skill)==FALSE]))

reg14 <- glm(out_trade_easier_dum~int_skill+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset=is.na(int_skill)==FALSE)
se14 <- coeftest(reg14, vcov = vcovCL(reg14, cluster = ghana$int_constituency[is.na(ghana$int_skill)==FALSE]))

reg15 <- glm(out_trade_easier_dum~int_highskill+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset=is.na(int_skill)==FALSE)
se15 <- coeftest(reg15, vcov = vcovCL(reg15, cluster = ghana$int_constituency[is.na(ghana$int_skill)==FALSE]))

# FIGURE 2
cat(stargazer(reg9,reg13,reg14,reg15,
              se = list(se9[,2], se13[,2],se14[,2],se15[,2]),
              p = list(se9[,4], se13[,4],se14[,4],se15[,4]),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              title = "Alternative measure of skill (Ghana 2016)",
              dep.var.labels = "Support for free trade (0-1)", label = "tab:ghana-alt-skill",
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              covariate.labels = c("Edu","Skill (0-3)","High Skill (0-1)","Age","Female","Pol Knowledge"),
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth"),
                               c("Sample","Employed","Employed (subset)","Employed (subset)","Employed (subset)")),
              omit = c("int_religion","int_eth_large","Constant")),
               file="milner-ghana-output/alt-skill.tex", sep="\n")

save(reg7,reg8,reg9,reg10,reg11,reg12,reg13,reg14,reg15,
     se7,se8,se9,se10,se11,se12,se13,se14,se15,
     file="milner-ghana-output/regs.RData")

# income

reg18 <- glm(out_trade_easier_dum~int_internet_use_ordinal+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana)
se18 <- coeftest(reg18, vcov = vcovCL(reg18, cluster = ghana$int_constituency))

reg19 <- glm(out_trade_easier_dum~int_income_combined_log+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana)
se19 <- coeftest(reg19, vcov = vcovCL(reg19, cluster = ghana$int_constituency))

reg20 <- glm(out_trade_easier_dum~pov_cash+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana)
se20 <- coeftest(reg20, vcov = vcovCL(reg20, cluster = ghana$int_constituency))

reg21 <- glm(out_trade_easier_dum~pov_food+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana)
se21 <- coeftest(reg21, vcov = vcovCL(reg21, cluster = ghana$int_constituency))

reg22 <- glm(out_trade_easier_dum~int_internet_use_ordinal+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset = int_occupation_status=="Employed")
se22 <- coeftest(reg22, vcov = vcovCL(reg22, cluster = ghana$int_constituency[ghana$int_occupation_status=="Employed"]))

reg23 <- glm(out_trade_easier_dum~int_income_combined_log+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset = int_occupation_status=="Employed")
se23 <- coeftest(reg23, vcov = vcovCL(reg23, cluster = ghana$int_constituency[ghana$int_occupation_status=="Employed"]))

reg24 <- glm(out_trade_easier_dum~pov_cash+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset = int_occupation_status=="Employed")
se24 <- coeftest(reg24, vcov = vcovCL(reg24, cluster = ghana$int_constituency[ghana$int_occupation_status=="Employed"]))

reg25 <- glm(out_trade_easier_dum~pov_food+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset = int_occupation_status=="Employed")
se25 <- coeftest(reg25, vcov = vcovCL(reg25, cluster = ghana$int_constituency[ghana$int_occupation_status=="Employed"]))

cat(stargazer(reg18,reg19,reg20,reg21,reg22,reg23,reg24,reg25,
              se = list(se18[,2],se19[,2],se20[,2],se21[,2],se22[,2],se23[,2],se24[,2],se25[,2]),
              p = list(se18[,4],se19[,4],se20[,4],se21[,4],se22[,4],se23[,4],se24[,4],se25[,4]),
              title = "Using income to proxy for skill (Ghana 2016)",
              dep.var.labels = "Support for free trade (0-1)", label = "tab:ghana-income",
              covariate.labels = c("Internet Use","HH Income (log)","Lacked Cash","Lacked Food","Age","Female","Pol Knowledge"),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit = c("int_religion","int_eth_large","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth"),
                               c("Sample","Full","Full","Full","Full","Employed","Employed","Employed","Employed"))),
    file="milner-ghana-output/income.tex", sep="\n")

# public sector

se9

reg24 <- glm(out_trade_easier_dum~int_edu_cont+publicsector+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset = int_occupation_status=="Employed")
se24 <- coeftest(reg24, vcov = vcovCL(reg24, cluster = ghana$int_constituency[ghana$int_occupation_status=="Employed"]))

cat(stargazer(reg9,reg24,
              se = list(se9[,2],se24[,2]),
              title = "Controlling for the public sector (Ghana 2016)",
              dep.var.labels = "Support for free trade (0-1)",label = "tab:ghana-publicsector",
              order = c("edu_cont"),
              covariate.labels = c("Edu","Public Sector","Age","Female","Pol Knowledge"),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit = c("int_religion","int_eth_large","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth"),
                               c("Sample","Employed","Employed"))),
    file="milner-ghana-output/publicsector.tex", sep="\n")

# cultural (includes pride and identification but no measure of xenophobia)

se7

reg20 <- glm(out_trade_easier_dum~int_edu_cont+int_natlid+int_identity+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana)
se20 <- coeftest(reg20, vcov = vcovCL(reg20, cluster = ghana$int_constituency))

se9

reg21 <- glm(out_trade_easier_dum~int_edu_cont+int_natlid+int_identity+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana,subset=int_occupation_status=="Employed")
se21 <- coeftest(reg21, vcov = vcovCL(reg21, cluster = ghana$int_constituency[ghana$int_occupation_status=="Employed"]))

se14

reg22 <- glm(out_trade_easier_dum~int_skill+int_natlid+int_identity+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana)
se22 <- coeftest(reg22, vcov = vcovCL(reg22, cluster = ghana$int_constituency))


cat(stargazer(reg7,reg20,reg9,reg21,reg14,reg22,
              se = list(se7[,2], se20[,2],se9[,2], se21[,2],se14[,2], se22[,2]),
              title = "Testing non-economic models (Ghana 2016)",
              dep.var.labels="Support for free trade (0-1)",label="tab:ghana-cultural",
              order=c("int_edu_cont","int_skill"),
              covariate.labels = c("Edu","Skill","Natl ID","Pride","Age","Female","Pol Knowledge"),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit = c("int_religion","int_eth_large","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth"),
                               c("Sample","Full","Full","Employed","Employed","Employed (subset)","Employed (subset)"))),
    file="milner-ghana-output/culturalmodel.tex", sep="\n")

# political connections

se9

reg24 <- glm(out_trade_easier_dum~int_edu_cont+int_membership_community+int_pol_position+int_family_position+int_age+int_female+int_religion+int_eth_large+int_pol_knowledge, family = binomial(link="probit"),data=ghana, subset = int_occupation_status=="Employed")
se24 <- coeftest(reg24, vcov = vcovCL(reg24, cluster = ghana$int_constituency[ghana$int_occupation_status=="Employed"]))

cat(stargazer(reg9,reg24,
              se = list(se9[,2],se24[,2]),
              title = "Controlling for political connections (Ghana 2016)",
              dep.var.labels = "Support for free trade (0-1)",label = "tab:ghana-polconnections",
              order = c("edu_cont"),
              covariate.labels = c("Edu","Community Member:Inactive","Community Member:Active","Community Member:Leader","Holds Political Position","Family Holds Political Position","Age","Female","Pol Knowledge"),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit = c("int_religion","int_eth_large","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth"),
                               c("Sample","Employed","Employed"))),
    file="milner-ghana-output/polconnections.tex", sep="\n")

