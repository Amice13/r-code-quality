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

load("uganda_raw.RData")
load("abr6_clean.RData")
load("abr8_clean.RData")


##### DEPENDENT VARIABLES #####

# should be easier for other countries to buy and sell their goods and services in uganda
uganda$trade_easier_dum <- as.numeric(uganda$trade_easier%in%c("Somewhat agree","Strongly agree"))
uganda$trade_easier_dum[is.na(uganda$trade_easier)] <- NA
mean(uganda$trade_easier_dum,na.rm=TRUE)

###### EXPLANATORY AND CONTROL VARIABLES #####

uganda$constituency[is.na(uganda$constituency)] <- 999 # treat NAs as a constituency to avoid losing data
uganda$constituency <- as.factor(uganda$constituency)

uganda$edu_cont <- as.numeric(ordered(uganda$edu,levels=c("No schooling","Some primary","Completed primary", "Some secondary school",
                                                           "Completed secondary school","Some university or polytechnic","Completed university or polytechnic",
                                                           "Completed post-graduate training")))
ggplot(uganda,aes(x=edu_cont))+geom_histogram(bins=8,binwidth=.5)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),
        ,axis.ticks.y = element_blank(),axis.text.y=element_blank())+xlab("Education (1-8)")+ylab("Count")
ggsave(file="milner-uganda-output/hist-edu.pdf")

uganda$edu_primary <- as.numeric(uganda$edu_cont%in%c(3,4))
uganda$edu_secondary <- as.numeric(uganda$edu_cont%in%c(5,6))
uganda$edu_college <- as.numeric(uganda$edu_cont%in%c(7))
uganda$edu_postgrad <- as.numeric(uganda$edu_cont%in%c(8))

uganda$religion <- as.factor(uganda$religion)
uganda$ethnicity <- as.factor(uganda$ethnicity)
uganda$ethnicity_pres <- as.numeric(uganda$ethnicity=="Munyankole")
uganda$know_kcca_correct <- as.numeric(uganda$know_kcca=="Correct")
uganda$know_kcca_correct[is.na(uganda$know_kcca)] <- NA
uganda$know_mayor_correct <- as.numeric(uganda$know_mayor=="Correct")
uganda$know_mayor_correct[is.na(uganda$know_mayor)] <- NA
uganda$know_speaker_correct <- as.numeric(uganda$know_speaker=="Correct")
uganda$know_speaker_correct[is.na(uganda$know_speaker)] <- NA
uganda$pol_knowledge <- uganda$know_kcca_correct+uganda$know_mayor_correct+uganda$know_speaker_correct

uganda$female <- as.numeric(uganda$female=="Female")

table(uganda$tv_use) # this seems like a normal distribution
table(uganda$phone)
table(uganda$internet_use) # this seems like a normal distribution
uganda$tv_use_ordinal <- NA
uganda$tv_use_ordinal[uganda$tv_use=="Never"] <- 1
uganda$tv_use_ordinal[uganda$tv_use%in%c("Less than once a month","Once a month","2-3 times a month","Once a week","2-3 times a week")] <- 2
uganda$tv_use_ordinal[uganda$tv_use=="Daily"] <- 3
uganda$internet_use_ordinal <- NA
uganda$internet_use_ordinal[uganda$internet_use=="Never"] <- 1
uganda$internet_use_ordinal[uganda$internet_use%in%c("Less than once a month","Once a month","2-3 times a month","Once a week","2-3 times a week")] <- 2
uganda$internet_use_ordinal[uganda$internet_use=="Daily"] <- 3
uganda$asset_index <- uganda$internet_use_ordinal + uganda$tv_use_ordinal

uganda$natlid <- NA
uganda$natlid[uganda$ethnic_attach%in%c("I feel only Ugandan.","I feel more Ugandan than from my tribe.")] <- 2
uganda$natlid[uganda$ethnic_attach%in%c("I feel equally Ugandan and from my tribe")] <- 1
uganda$natlid[uganda$ethnic_attach%in%c("I feel only only from my tribe.","I feel more from my tribe than Ugandan.")] <- 0

uganda$duties_labor <- as.numeric(grepl("1",uganda$duties))
uganda$duties_managing <- as.numeric(grepl("2",uganda$duties))
uganda$duties_clerical <- as.numeric(grepl("3",uganda$duties))
uganda$duties_computer <- as.numeric(grepl("4",uganda$duties))
uganda$duties_business <- as.numeric(grepl("5",uganda$duties))
uganda$duties_none <- as.numeric(grepl("6",uganda$duties))

uganda$skill <- NA
uganda$skill[uganda$duties_none==1] <- 0
uganda$skill[uganda$duties_labor==1] <- 1
uganda$skill[uganda$duties_business==1] <- 1
uganda$skill[uganda$duties_clerical==1] <- 2
uganda$skill[uganda$duties_computer==1] <- 2
uganda$skill[uganda$duties_managing==1] <- 3
uganda$highskill <- as.numeric(uganda$skill>=2)

ggplot(uganda,aes(x=skill))+geom_histogram(bins=5,binwidth=.5)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),
        ,axis.ticks.y = element_blank(),axis.text.y=element_blank())+xlab("Skill (0-3)")+ylab("Count")
ggsave(file="milner-uganda-output/hist-skill.pdf")

uganda$income_combined <- rowSums(uganda[,c("income","household_income")],na.rm=TRUE)
uganda$income_combined_log <- log(uganda$income_combined+1)

uganda$publicsector <- as.numeric(uganda$income_source=="In a government job or a political position")
table(uganda$publicsector[uganda$occupation_status=="Employed"],uganda$edu_cont[uganda$occupation_status=="Employed"])
mean(uganda$edu_cont[uganda$publicsector==1&uganda$occupation_status=="Employed"],na.rm=TRUE)
mean(uganda$edu_cont[uganda$publicsector==0&uganda$occupation_status=="Employed"],na.rm=TRUE)

##### DESCRIPTIVE STATISTICS #####

abr6_uga <- abr6 %>%
  select(ccode,age,edu,female,pov_cash,pov_food,natlid,Q96A) %>%
  filter(ccode=="UGA") %>%
  mutate(ag = as.numeric(Q96A==3),
         sample = "Afrobarometer round 6") %>%
  select(age,edu,female,pov_cash,pov_food,natlid,ag,sample)

abr8_uga <- abr8 %>%
  select(ccode,age,edu,female,pov_cash,pov_food,natlid,Q95C) %>%
  filter(ccode=="UGA") %>%
  mutate(ag = as.numeric(Q95C==3),
         sample = "Afrobarometer round 8") %>%
  select(age,edu,female,pov_cash,pov_food,natlid,ag,sample)

uganda <- uganda %>%
  mutate(pov_cash = recode(as_factor(pov_cash),
                           "Never" = 1,
                           "Just once or twice" = 2,
                           "Several times" = 3,
                           "Many times" = 4,
                           "Always" = 5),
         natlid = recode(as_factor(ethnic_attach),
                         "I feel only from my tribe." = 0,
                         "I feel more from my tribe than Ugandan." = 0,
                         "I feel equally Ugandan and from my tribe" = 1,
                         "I feel more Ugandan than from my tribe." = 2,
                         "I feel only Ugandan." = 2))

uganda_desc <- uganda %>%
  select(age,edu_cont,female,pov_cash,pov_food,natlid,industry) %>%
  mutate(age = age,
         edu = edu_cont,
         female = female,
         ag = as.numeric(industry == "Agriculture, Forestry, Fishing and Hunting"),
         sample = "Uganda (2017)") %>%
  select(age,edu,female,pov_cash,pov_food,natlid,ag,sample)

tibble <- bind_rows(abr6_uga,abr8_uga,uganda_desc) %>%
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
cat(tbl[1],file="milner-uganda-output/descriptives.tex")


##### REPLICATE AFROBAROMETER ANALYSIS ######

reg7 <- glm(trade_easier_dum~edu_cont+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda)
se7 <- coeftest(reg7, vcov = vcovCL(reg7, cluster = uganda$constituency))

reg8 <- glm(trade_easier_dum~edu_primary+edu_secondary+edu_college+edu_postgrad+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda)
se8 <- coeftest(reg8, vcov = vcovCL(reg8, cluster = uganda$constituency))

reg9 <- glm(trade_easier_dum~edu_cont+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset=occupation_status=="Employed")
se9 <- coeftest(reg9, vcov = vcovCL(reg9, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

reg10 <- glm(trade_easier_dum~edu_primary+edu_secondary+edu_college+edu_postgrad+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset=occupation_status=="Employed")
se10 <- coeftest(reg10, vcov = vcovCL(reg10, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

reg11 <- glm(trade_easier_dum~edu_cont+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset=!occupation_status=="Employed")
se11 <- coeftest(reg11, vcov = vcovCL(reg11, cluster = uganda$constituency[!uganda$occupation_status=="Employed"]))

reg12 <- glm(trade_easier_dum~edu_primary+edu_secondary+edu_college+edu_postgrad+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset=!occupation_status=="Employed")
se12 <- coeftest(reg12, vcov = vcovCL(reg12, cluster = uganda$constituency[!uganda$occupation_status=="Employed"]))

reg13 <- glm(trade_easier_dum~edu_cont*occupation_status+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda)
se13 <- coeftest(reg13, vcov = vcovCL(reg13, cluster = uganda$constituency))

cat(stargazer(reg7,reg8,reg9,reg10,reg11,reg12,reg13,
              se = list(se7[,2], se8[,2],se9[,2],se10[,2],se11[,2],se12[,2],se13[,2]),
              p = list(se7[,4], se8[,4],se9[,4],se10[,4],se11[,4],se12[,4],se13[,4]),
              column.sep.width = "-10pt",font.size="small",no.space = TRUE,
              title = "Relationship between education and support for free trade (Uganda 2017)",
              dep.var.labels = "Support for free trade (0-1)", label = "tab:uganda-edu",
              covariate.labels = c("Edu","Primary","Secondary","College","Postgrad","Occ:Student","Occ:Homemaker","Occ:Retired","Occ:Unemployed","Occ:Other","Age","Female","Pol Knowledge","Edu*Occ:Student","Edu*Occ:Homemaker","Edu*Occ:Retired","Edu*Occ:Unemployed","Edu*Occ:Other"),
              omit=c("religion","ethnicity","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              #notes = "Standard errors clustered by constituency.",
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth"),
                               c("Sample","Full","Full","Employed","Employed","Not Employed","Not Employed","Full"))),
              file="milner-uganda-output/edu.tex", sep="\n")


# alternative measure of skill (only available for subset of employed)

se9

reg13 <- glm(trade_easier_dum~edu_cont+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset=occupation_status=="Employed")
se13 <- coeftest(reg13, vcov = vcovCL(reg13, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

reg14 <- glm(trade_easier_dum~skill+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset=occupation_status=="Employed")
se14 <- coeftest(reg14, vcov = vcovCL(reg14, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

reg15 <- glm(trade_easier_dum~highskill+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset=occupation_status=="Employed")
se15 <- coeftest(reg15, vcov = vcovCL(reg15, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

cat(stargazer(reg9,reg13,reg14,reg15,
              se = list(se9[,2], se13[,2],se14[,2],se15[,2]),
              p = list(se9[,4], se13[,4],se14[,4],se15[,4]),
              title = "Alternative measure of skill (Uganda 2017)",
              dep.var.labels = "Support for free trade (0-1)", label = "tab:uganda-alt-skill",
              #notes = "Standard errors clustered by constituency.",
              covariate.labels = c("Edu","Skill (0-3)","High Skill (0-1)","Age","Female","Pol Knowledge"),
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth"),
                               c("Sample","Employed","Employed","Employed","Employed")),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              omit = c("religion","ethnicity","Constant")),
               file="milner-uganda-output/alt-skill.tex", sep="\n")

save(reg7,reg8,reg9,reg10,reg11,reg12,reg13,reg14,reg15,
     se7,se8,se9,se10,se11,se12,se13,se14,se15,
     file="milner-uganda-output/regs.RData")

# income


reg18 <- glm(trade_easier_dum~internet_use_ordinal+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda)
se18 <- coeftest(reg18, vcov = vcovCL(reg18, cluster = uganda$constituency))

reg19 <- glm(trade_easier_dum~income_combined_log+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda)
se19 <- coeftest(reg19, vcov = vcovCL(reg19, cluster = uganda$constituency))

reg20 <- glm(trade_easier_dum~pov_cash+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda)
se20 <- coeftest(reg20, vcov = vcovCL(reg20, cluster = uganda$constituency))

reg20b <- glm(trade_easier_dum~pov_food+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda)
se20b <- coeftest(reg20b, vcov = vcovCL(reg20b, cluster = uganda$constituency))

reg21 <- glm(trade_easier_dum~internet_use_ordinal+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset = occupation_status=="Employed")
se21 <- coeftest(reg21, vcov = vcovCL(reg21, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

reg22 <- glm(trade_easier_dum~income_combined_log+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset = occupation_status=="Employed")
se22 <- coeftest(reg22, vcov = vcovCL(reg22, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

reg23 <- glm(trade_easier_dum~pov_cash+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset = occupation_status=="Employed")
se23 <- coeftest(reg23, vcov = vcovCL(reg23, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

reg23b <- glm(trade_easier_dum~pov_food+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset = occupation_status=="Employed")
se23b <- coeftest(reg23b, vcov = vcovCL(reg23b, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

cat(stargazer(reg18,reg19,reg20,reg20b,reg21,reg22,reg23,reg23b,
              se = list(se18[,2],se19[,2],se20[,2],se20b[,2],se21[,2],se22[,2],se23[,2],se23b[,2]),
              p = list(se18[,4],se19[,4],se20[,4],se20b[,4],se21[,4],se22[,4],se23[,4],se23b[,4]),
              title = "Using income to proxy for skill (Uganda 2017)",
              dep.var.labels = "Support for free trade (0-1)",label = "tab:uganda-2017-income",
              covariate.labels = c("Internet Use","HH Income (log)","Lacked Cash","Lacked Food","Age","Female","Pol Knowledge"),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit = c("religion","ethnicity","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth"),
                               c("Sample","Full","Full","Full","Full","Employed","Employed","Employed","Employed"))),
    file="milner-uganda-output/income.tex", sep="\n")

# public sector

se9

reg24 <- glm(trade_easier_dum~edu_cont+publicsector+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset = occupation_status=="Employed")
se24 <- coeftest(reg24, vcov = vcovCL(reg24, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

cat(stargazer(reg9,reg24,
              se = list(se9[,2],se24[,2]),
              p = list(se9[,4],se24[,4]),
              title = "Controlling for the public sector (Uganda 2017)",
              dep.var.labels = "Support for free trade (0-1)",label = "tab:uganda-2017-publicsector",
              order = c("edu_cont","skill"),
              covariate.labels = c("Edu","Public Sector","Age","Female","Pol Knowledge"),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit = c("religion","ethnicity","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth"),
                               c("Sample","Employed","Employed"))),
    file="milner-uganda-output/publicsector.tex", sep="\n")


# cultural (includes pride and identification but no measure of xenophobia)

se7

reg24 <- glm(trade_easier_dum~edu_cont+natlid+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda)
se24 <- coeftest(reg24, vcov = vcovCL(reg24, cluster = uganda$constituency))

se9

reg25 <- glm(trade_easier_dum~edu_cont+natlid+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset= occupation_status == "Employed")
se25 <- coeftest(reg25, vcov = vcovCL(reg25, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

se14

reg26 <- glm(trade_easier_dum~skill+natlid+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset= occupation_status == "Employed")
se26 <- coeftest(reg26, vcov = vcovCL(reg26, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

cat(stargazer(reg7,reg24,reg9,reg25,reg14,reg26,
              se = list(se7[,2],se24[,2],se9[,2],se25[,2],se14[,2],se26[,2]),
              p = list(se7[,4],se24[,4],se9[,4],se25[,4],se14[,4],se26[,4]),
              title = "Testing non-economic models (Uganda 2017)",
              dep.var.labels = "Support for free trade (0-1)", label = "tab:uganda-2017-cultural",
              order = c("edu_cont","skill"),
              covariate.labels = c("Edu","Skill","Natl ID","Age","Female","Pol Knowledge"),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit = c("religion","ethnicity","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth","Rel, Eth"),
                               c("Sample","Full","Full","Employed","Employed","Employed","Employed"))),
    file="milner-uganda-output/culturalmodel.tex", sep="\n")

# political connections

se9

reg24 <- glm(trade_easier_dum~edu_cont+membership_community+age+female+religion+ethnicity+pol_knowledge, family = binomial(link="probit"),data=uganda, subset = occupation_status=="Employed")
se24 <- coeftest(reg24, vcov = vcovCL(reg24, cluster = uganda$constituency[uganda$occupation_status=="Employed"]))

cat(stargazer(reg9,reg24,
              se = list(se9[,2],se24[,2]),
              p = list(se9[,4],se24[,4]),
              title = "Controlling for political connections (Uganda 2017)",
              dep.var.labels = "Support for free trade (0-1)",label = "tab:uganda-2017-polconnections",
              order = c("edu_cont","skill"),
              covariate.labels = c("Edu","Community Member:Inactive","Community Member:Active","Community Member:Leader","Age","Female","Pol Knowledge"),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              omit = c("religion","ethnicity","Constant"),
              add.lines = list(c("Addtl Controls","Rel, Eth","Rel, Eth"),
                               c("Sample","Employed","Employed"))),
    file="milner-uganda-output/polconnections.tex", sep="\n")


