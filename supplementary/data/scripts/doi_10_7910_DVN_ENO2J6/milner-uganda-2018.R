rm(list=ls())

library(stargazer)
library(readstata13)
library(foreign)
library(sandwich)
library(lmtest)
library(xtable)
library(ggplot2)
library(gtsummary)
library(tidyverse)
library(reshape2)
library(haven)

setwd("")

load("uganda18_raw.RData")
load("abr6_clean.RData")
load("abr8_clean.RData")

##### DEPENDENT VARIABLES #####

# should be easier for other countries to buy and sell their goods and services in Uganda
uganda18$trade_easier_dum <- as.numeric(uganda18$trade_easier%in%c(1,2))
uganda18$trade_easier_dum[uganda18$trade_easier%in%c(-1,-99)] <- NA
mean(uganda18$trade_easier_dum,na.rm=TRUE)

# favor placing new limits on imports (1) or oppose placing new limits on imports (0) ## NEW DV MEASURE!
uganda18$trade_imports_limit <- uganda18$trade_imports
uganda18$trade_imports_limit[uganda18$trade_imports%in%c(-1,-8,-99)] <- NA

# think trade has positive effect on family
uganda18$trade_effect_family_dum <- as.numeric(uganda18$trade_effect_family%in%c(1,2))
uganda18$trade_effect_family_dum[uganda18$trade_effect_family%in%c(-1,-8,-99)] <- NA

# think trade has positive effect on ugandan economy
uganda18$trade_effect_economy_dum <- as.numeric(uganda18$trade_effect_economy%in%c(1,2))
uganda18$trade_effect_economy_dum[uganda18$trade_effect_economy%in%c(-1,-8,-99)] <- NA

# think trade has positive effect on business
uganda18$trade_effect_business_dum <- as.numeric(uganda18$trade_effect_business%in%c(1,2))
uganda18$trade_effect_business_dum[uganda18$trade_effect_business%in%c(-1,-8,-10,-99)] <- NA

# think trade is good for the country
uganda18$trade_good_dum <- as.numeric(uganda18$trade_good%in%c(1,2))
uganda18$trade_good_dum[uganda18$trade_good%in%c(-1,-99)] <- NA

# free trade results in better products and lower prices
uganda18$trade_free_product_dum <- as.numeric(uganda18$trade_free_product%in%c(5,4))
uganda18$trade_free_product_dum[uganda18$trade_free_product%in%c(-1,-8)] <- NA

# free trade causes layoffs
uganda18$trade_free_laidoff_dum <- as.numeric(uganda18$trade_free_laidoff%in%c(5,4))
uganda18$trade_free_laidoff_dum[uganda18$trade_free_laidoff%in%c(-1,-8)] <- NA

# free trade widens gap between rich and poor
uganda18$trade_free_gap_dum <- as.numeric(uganda18$trade_free_gap%in%c(5,4))
uganda18$trade_free_gap_dum[uganda18$trade_free_gap%in%c(-1,-8)] <- NA

# free trade creates jobs
uganda18$trade_free_job_dum <- as.numeric(uganda18$trade_free_job%in%c(5,4))
uganda18$trade_free_job_dum[uganda18$trade_free_job%in%c(-1,-8)] <- NA

# free trade helps the poor
uganda18$trade_free_poor_dum <- as.numeric(uganda18$trade_free_poor%in%c(5,4))
uganda18$trade_free_poor_dum[uganda18$trade_free_poor%in%c(-1,-8)] <- NA

###### EXPLANATORY AND CONTROL VARIABLES #####

uganda18$age <- uganda18$ptc_age

uganda18$edu <- NA
uganda18$edu[uganda18$ptc_edu%in%c(0)] <- "No schooling"
uganda18$edu[uganda18$ptc_edu%in%c(1,2,3,4,5,6)] <- "Some primary"
uganda18$edu[uganda18$ptc_edu%in%c(7)] <- "Completed primary"
uganda18$edu[uganda18$ptc_edu%in%c(8,9,10,11,12)] <- "Some secondary school"
uganda18$edu[uganda18$ptc_edu%in%c(13)] <- "Completed secondary school"
uganda18$edu[uganda18$ptc_edu%in%c(15,17)] <- "Some university or polytechnic"
uganda18$edu[uganda18$ptc_edu%in%c(14,16)] <- "Completed university or polytechnic"

uganda18$edu_cont <- as.numeric(ordered(uganda18$edu,levels=c("No schooling","Some primary","Completed primary", "Some secondary school",
                                                           "Completed secondary school","Some university or polytechnic","Completed university or polytechnic")))

uganda18$edu_primary <- as.numeric(uganda18$edu_cont%in%c(3,4))
uganda18$edu_secondary <- as.numeric(uganda18$edu_cont%in%c(5,6))
uganda18$edu_college <- as.numeric(uganda18$edu_cont%in%c(7))

# no data on religion, only religiosity
uganda18$ethnicity <- as.factor(uganda18$ptc_ethnicity)
# no data on political knowledge

uganda18$female <- uganda18$ptc_female

table(uganda18$ptc_cellphone) # most have a cell phone <- best asset index because of no missingness
table(uganda18$ptc_smartphone) # fewer have a smartphone but lots of missing data
table(uganda18$ptc_internet) # mostly missing data
table(uganda18$ptc_pov_food) # mostly missing data

# no data on ethnic vs. national attachment

# no data on occupation status, so have to invent it

uganda18$primary_earner <- as.numeric(uganda18$ptc_primary_contribute==1)
table(uganda18$ptc_income_source,uganda18$primary_earner,exclude=NULL) # no missing data on either

uganda18$income_source <- factor(uganda18$ptc_income_source,
                                  levels=c(1,2,3,4,5,6,7,8,9,10,29,11,12,13,14,15,16,17,18,19,20,21,30,22,23,24,31,25,26),
                                  labels=c("Brewing alcohol/beer","Brickmaking","Broker","Casual labor","Carpentry and joinery","Construction",
                                  "As an employee in a company","Farmer","In a government job or a political position","As a health worker",
                                  "Market vendor","As a mechanic","Metal fabrication","Moneylending","As an NGO worker","Purchasing items for resale",
                                  "Restaurant employee/food vending","Saloon","Shoe repair","Tailoring","Taking care of animals","As a taxi or other driver",
                                  "Other small shop owner","Other self-employed","Homemaker","I'm a student","Retired","Unemployed","Other"))

uganda18$occupation_status <- "Employed"
uganda18$occupation_status[uganda18$ptc_income_source==23] <- "Homemaker"
uganda18$occupation_status[uganda18$ptc_income_source==24] <- "Student"
uganda18$occupation_status[uganda18$ptc_income_source==25] <- "Unemployed"
uganda18$occupation_status[uganda18$ptc_income_source==26] <- "Other"
uganda18$occupation_status[uganda18$ptc_income_source==31] <- "Retired"
uganda18$occupation_status <- as.factor(uganda18$occupation_status)

uganda18$ptc_income[which(uganda18$ptc_income%in%c(-1,-9))] <- NA
uganda18$ptc_income_int[which(uganda18$ptc_income_int%in%c(-1))] <- NA
uganda18$ptc_income_int[which(uganda18$ptc_income_int%in%c(999))] <- 90
uganda18$income <- (uganda18$ptc_income/uganda18$ptc_income_int)*365

uganda18$publicsector <- as.numeric(uganda18$income_source=="In a government job or a political position")
table(uganda18$publicsector[uganda18$occupation_status=="Employed"],uganda18$edu_cont[uganda18$occupation_status=="Employed"])
mean(uganda18$edu_cont[uganda18$publicsector==1&uganda18$occupation_status=="Employed"],na.rm=TRUE)
mean(uganda18$edu_cont[uganda18$publicsector==0&uganda18$occupation_status=="Employed"],na.rm=TRUE)

##### DESCRIPTIVE STATISTICS #####

abr6_uga <- abr6 %>%
  select(ccode,age,edu,female,phone,Q96A) %>%
  filter(ccode=="UGA") %>%
  mutate(ag = as.numeric(Q96A==3),
         sample = "Afrobarometer round 6") %>%
  select(age,edu,female,phone,ag,sample)

abr8_uga <- abr8 %>%
  select(ccode,age,edu,female,phone,Q95C) %>%
  filter(ccode=="UGA") %>%
  mutate(ag = as.numeric(Q95C==3),
         phone = as.numeric(phone==2),
         sample = "Afrobarometer round 8") %>%
  select(age,edu,female,phone,ag,sample)

uganda18_desc <- uganda18 %>%
  select(age,edu_cont,female,ptc_cellphone,income_source) %>%
  mutate(age = age,
         edu = edu_cont,
         female = as.numeric(female>0),
         phone = ptc_cellphone,
         ag = as.numeric(income_source == "Farmer"),
         sample = "Uganda (2018)") %>%
  select(age,edu,female,phone,ag,sample)

tibble <- bind_rows(abr6_uga,abr8_uga,uganda18_desc) %>%
  mutate(age = as.numeric(age))

theme_gtsummary_compact()
tbl <-  tibble %>%
  tbl_summary(
    by = "sample",
    type = list(age ~ "continuous2", edu ~ "continuous2"),
    statistic = list(all_continuous() ~ c("{mean} ({sd})", "[{min},{max}]"),
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    label = list(age ~ "Age",
                 edu ~ "Education",
                 female ~ "Female",
                 phone ~ "Owns Phone",
                 ag ~ "Works in Agriculture"),
    missing_text = "(NA)"
  )  %>%
  as_gt() %>%
  gt::as_latex()
cat(tbl[1],file="milner-uganda-2018-output/descriptives.tex")

##### REPLICATE AFROBAROMETER ANALYSIS ######


reg7 <- glm(trade_easier_dum~edu_cont+age+female+ethnicity, family = binomial(link="probit"),data=uganda18)

reg8 <- glm(trade_easier_dum~edu_primary+edu_secondary+edu_college+age+female+ethnicity, family = binomial(link="probit"),data=uganda18)

reg9 <- glm(trade_easier_dum~edu_cont+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset=occupation_status=="Employed")

reg10 <- glm(trade_easier_dum~edu_primary+edu_secondary+edu_college+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset=occupation_status=="Employed")

reg11 <- glm(trade_easier_dum~edu_cont+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset=!occupation_status=="Employed")

reg12 <- glm(trade_easier_dum~edu_primary+edu_secondary+edu_college+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset=!occupation_status=="Employed")

reg13 <- glm(trade_easier_dum~edu_cont*occupation_status+age+female+ethnicity, family = binomial(link="probit"),data=uganda18)

cat(stargazer(reg7,reg8,reg9,reg10,reg11,reg12,reg13,
              column.sep.width = "-10pt",font.size="small",no.space = TRUE,
              title = "Relationship between education and support for free trade (Uganda 2018)",
              dep.var.labels = "Support for free trade (0-1)", label = "tab:uganda-2018-edu",
              covariate.labels = c("Edu","Primary","Secondary","College","Occ:Homemaker","Occ:Other","Occ:Retired","Occ:Student","Occ:Unemployed","Age","Female","Edu*Occ:Homemaker","Edu*Occ:Other","Edu*Occ:Retired","Edu*Occ:Student","Edu*Occ:Unemployed"),
              omit=c("ethnicity","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Eth","Eth","Eth","Eth","Eth","Eth","Eth"),
                               c("Sample","Full","Full","Employed","Employed","Not Employed","Not Employed","Full"))),
              file="milner-uganda-2018-output/edu.tex", sep="\n")

# alternative measure of DV

reg14 <- glm(trade_imports_limit~edu_cont+age+female+ethnicity, family = binomial(link="probit"),data=uganda18)

reg15 <- glm(trade_imports_limit~edu_primary+edu_secondary+edu_college+age+female+ethnicity, family = binomial(link="probit"),data=uganda18)

reg16 <- glm(trade_imports_limit~edu_cont+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset=occupation_status=="Employed")

reg17 <- glm(trade_imports_limit~edu_primary+edu_secondary+edu_college+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset=occupation_status=="Employed")

reg18 <- glm(trade_imports_limit~edu_cont+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset=!occupation_status=="Employed")

reg19 <- glm(trade_imports_limit~edu_primary+edu_secondary+edu_college+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset=!occupation_status=="Employed")

reg20 <- glm(trade_imports_limit~edu_cont*occupation_status+age+female+ethnicity, family = binomial(link="probit"),data=uganda18)

cat(stargazer(reg14,reg15,reg16,reg17,reg18,reg19,reg20,
              column.sep.width = "-10pt",font.size="small",no.space = TRUE,
              title = "Relationship between education and opposition to free trade (Uganda 2018)",
              dep.var.labels = "Support for free trade (0-1)",label="tab:uganda-2018-alt-dv",
              covariate.labels = c("Edu","Primary","Secondary","College","Occ:Homemaker","Occ:Other","Occ:Retired","Occ:Student","Occ:Unemployed","Age","Female","Edu*Occ:Homemaker","Edu*Occ:Other","Edu*Occ:Retired","Edu*Occ:Student","Edu*Occ:Unemployed"),
              omit=c("ethnicity","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Eth","Eth","Eth","Eth","Eth","Eth","Eth"),
                               c("Sample","Full","Full","Employed","Employed","Not Employed","Not Employed","Full"))),
    file="milner-uganda-2018-output/alt-dv.tex", sep="\n")

cat(stargazer(reg7,reg14,
              column.sep.width = "-10pt",font.size="small",no.space = TRUE,
              dep.var.labels = c("Support for free trade (0-1)","Support for limiting imports (0-1)"),
              covariate.labels = c("Edu","Age","Female"),
              omit=c("ethnicity","Constant"),
              omit.stat=c("aic","LL"),
              label = "tab:compare-alt-dv",
              add.lines = list(c("Controls","Ethnicity","Ethnicity")),
              notes.label = "", float = FALSE),
    file="milner-uganda-2018-output/compare-alt-dv.tex", sep="\n")

# no alternative measure of skill

# income

reg21 <- glm(trade_easier_dum~ptc_cellphone+age+female+ethnicity, family = binomial(link="probit"),data=uganda18)

reg22 <- glm(trade_easier_dum~log(income+1)+age+female+ethnicity, family = binomial(link="probit"),data=uganda18)

reg23 <- glm(trade_easier_dum~ptc_cellphone+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset = occupation_status=="Employed")

reg24 <- glm(trade_easier_dum~log(income+1)+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset = occupation_status=="Employed")

cat(stargazer(reg21,reg22,reg23,reg24,
              title = "Using income to proxy for skill (Uganda 2018)",
              dep.var.labels = "Support for free trade (0-1)",label="tab:uganda-2018-income",
              covariate.labels = c("Phone Access","Income (log)","Age","Female"),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit = c("ethnicity","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Eth","Eth","Eth","Eth"),
                               c("Sample","Full","Full","Employed","Employed"))),
    file="milner-uganda-2018-output/income.tex", sep="\n")

# public sector

reg9

reg26 <- glm(trade_easier_dum~edu_cont+publicsector+age+female+ethnicity, family = binomial(link="probit"),data=uganda18, subset = occupation_status=="Employed")

cat(stargazer(reg9,reg26,
              title = "Controlling for public sector employment (Uganda 2018)",
              dep.var.labels = "Support for free trade (0-1)",label="tab:uganda-2018-publicsector",
              covariate.labels = c("Edu","Public Sector","Age","Female"),
              column.sep.width = "-10pt",font.size="small",no.space=TRUE,
              omit = c("ethnicity","Constant"),
              omit.stat=c("aic","LL"), notes.label = "", float = FALSE,
              add.lines = list(c("Addtl Controls","Eth","Eth"),
                               c("Sample","Employed","Employed"))),
    file="milner-uganda-2018-output/publicsector.tex", sep="\n")


# no cultural variables

##### MECHANISMS #####

mechs <- uganda18 %>%
  mutate(edu = as_factor(if_else(edu_cont>4,"High Education","Low Education"))) %>%
  group_by(edu) %>%
  select(edu, trade_easier_dum, trade_imports_limit,trade_effect_family_dum,trade_effect_business_dum,trade_effect_economy_dum,
         trade_free_product_dum,trade_free_laidoff_dum,trade_free_gap_dum,trade_free_job_dum,trade_free_poor_dum) %>%
  summarise_all("mean",na.rm=TRUE) %>%
  filter(!is.na(edu)) %>%
  pivot_longer(cols = 2:11,
               names_to = "var")

labels <- c("Support Free Trade","Limit Imports","Free Trade Helps Family","Free Trade Helps Business","Free Trade Helps Economy","Free Trade Lowers Prices","Free Trade Causes Layoffs","Free Trade Widens Gap","Free Trade Creates Jobs","Free Trade Helps Poor")

ggplot(data=mechs, aes(x=var))+
  geom_bar(stat="identity",aes(y=value,fill=edu,group=edu), position="dodge")+
  scale_fill_manual(values=c("High Education"="gray40","Low Education"="gray80"))+
  scale_y_continuous(limits=c(0,1))+geom_vline(xintercept=2.5,size=.5,linetype="dashed")+
  scale_x_discrete(labels= labels)+
  annotate("text",label="Policy Preferences",x=(1.5),y=.95,size=4)+annotate("text",label="Beliefs about Free Trade",x=(6.5),y=.95,size=4)+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.title.x=element_blank(),axis.text.x=element_text(angle=-90),axis.ticks.x=element_blank(),axis.title.y=element_blank(),legend.position="bottom",legend.title = element_blank())

# FIGURE 3
ggsave(file="milner-uganda-2018-output/mechanisms.pdf",height=6,width=11)

