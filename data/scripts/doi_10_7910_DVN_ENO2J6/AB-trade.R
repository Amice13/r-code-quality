rm(list=ls())
setwd("")
load("abr6_clean.RData")
load("abr8_clean.RData")

library(tidyverse)
library(sandwich)
library(lmtest)
library(stargazer)
library(labelled)
library(dplyr)
library(broom)
library(gtsummary)
library(rworldmap)
library(nnet)

##### SET VARIABLE LABELS #####

abr6 <- abr6 %>%
  set_variable_labels(
    openborders_dum = "Support for globalization (0-1)",
    edu = "Edu",
    primary = "Primary",
    secondary = "Secondary",
    highered = "Any Higher Ed",
    college = "College",
    age = "Age",
    female = "Female",
    rural = "Rural",
    assetindex = "Asset Index",
    natlid = "National ID",
    ethno = "Ethnocentrism",
    xeno = "Xenophobia",
    dem = "Supports Democracy"
  ) %>%
  mutate (wt = Combinwt)

abr8 <- abr8 %>%
  set_variable_labels(
    openborders_dum = "Support for globalization (0-1)",
    trade_dum = "Support for free trade (0-1)",
    edu = "Edu",
    primary = "Primary",
    secondary = "Secondary",
    highered = "Any Higher Ed",
    college = "College",
    age = "Age",
    female = "Female",
    rural = "Rural",
    assetindex = "Asset Index",
    natlid = "National ID",
    ethno = "Ethnocentrism",
    xeno = "Xenophobia",
    dem = "Supports Democracy"
  ) %>%
  mutate(wt = Combinwt)

##### DESCRIPTIVE STATISTICS #####

# Country Selection Into Afrobarometer

wep <- read_csv("10_31_19_0306pm_wep.csv")
sum(wep$gdp_WDI[wep$AB6==1])/sum(wep$gdp_WDI,na.rm=TRUE) # 87% of Africa's GDP (round 6)
sum(wep$gdp_WDI[wep$AB8==1])/sum(wep$gdp_WDI,na.rm=TRUE) # 84% of Africa's GDP (round 8)
sum(wep$pop_WDI[wep$AB6==1])/sum(wep$pop_WDI,na.rm=TRUE) # 77% of Africa's population (round 6)
sum(wep$pop_WDI[wep$AB8==1])/sum(wep$pop_WDI,na.rm=TRUE) # 73% of Africa's population (round 6)
wep[order(wep$gdppc_WDI),c("country","AB6","AB8","gdppc_WDI")]

tbl <- wep %>%
  mutate(AB6 = recode_factor(as_factor(AB6),
                                 "1" = "Included",
                                 "0" = "Excluded")) %>%
  select(AB6,polity2_P4,conflictincidence_UCDP,v2x_freexp_VDEM,trade_WDI) %>%
  tbl_summary(
    by = AB6,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    label = list(polity2_P4 ~ "Democracy (Polity)",
                 conflictincidence_UCDP ~ "Conflict Incidence (UCDP)",
                 v2x_freexp_VDEM ~ "Freedom of Expression (vDem)",
                 trade_WDI ~ "Trade as % of GDP (WDI)"),
    missing_text = "(NA)"
  )  %>%
  as_gt() %>%
  gt::as_latex()
cat(tbl[1],file="AB-output/selection-into-AB-r6.tex")

tbl <- wep %>%
  mutate(AB8 = recode_factor(as_factor(AB8),
                             "1" = "Included",
                             "0" = "Excluded")) %>%
  select(AB8,polity2_P4,conflictincidence_UCDP,v2x_freexp_VDEM,trade_WDI) %>%
  tbl_summary(
    by = AB8,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    label = list(polity2_P4 ~ "Democracy (Polity)",
                 conflictincidence_UCDP ~ "Conflict Incidence (UCDP)",
                 v2x_freexp_VDEM ~ "Freedom of Expression (vDem)",
                 trade_WDI ~ "Trade as % of GDP (WDI)"),
    missing_text = "(NA)"
  )  %>%
  as_gt() %>%
  gt::as_latex()
cat(tbl[1],file="AB-output/selection-into-AB-r8.tex")

summary(lm(data=wep,AB6~polity2_P4))
summary(lm(data=wep,AB6~conflictincidence_UCDP))
summary(lm(data=wep,AB6~v2x_freexp_VDEM))
summary(lm(data=wep,AB6~trade_WDI))
summary(lm(data=wep,AB8~polity2_P4))
summary(lm(data=wep,AB8~conflictincidence_UCDP))
summary(lm(data=wep,AB8~v2x_freexp_VDEM))
summary(lm(data=wep,AB8~trade_WDI))


# Education

ggplot(abr6,aes(x=edu))+geom_histogram(bins=9,binwidth=.5)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),
        ,axis.ticks.y = element_blank(),axis.text.y=element_blank())+xlab("Education (1-10)")+ylab("Count")
ggsave(file="ab-output/hist-edu-r6.pdf")

ggplot(abr8,aes(x=edu))+geom_histogram(bins=9,binwidth=.5)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),
        ,axis.ticks.y = element_blank(),axis.text.y=element_blank())+xlab("Education (1-10)")+ylab("Count")
ggsave(file="ab-output/hist-edu-r8.pdf")

# Skill

ggplot(abr6,aes(x=skill))+geom_histogram(bins=9,binwidth=.5)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),
        ,axis.ticks.y = element_blank(),axis.text.y=element_blank())+xlab("Skill (0-2)")+ylab("Count")
ggsave(file="ab-output/hist-skill-r6.pdf")

ggplot(abr8,aes(x=skill))+geom_histogram(bins=9,binwidth=.5)+theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.ticks.x = element_blank(),axis.text.x=element_blank(),
        ,axis.ticks.y = element_blank(),axis.text.y=element_blank())+xlab("Skill (0-2)")+ylab("Count")
ggsave(file="ab-output/hist-skill-r8.pdf")

# Edu in Public Sector vs. Other Sectors

table(abr6$publicsector[abr6$employment=="Employed"],abr6$edu[abr6$employment=="Employed"])
mean(abr6$edu[abr6$publicsector==1 & abr6$employment=="Employed"],na.rm=TRUE)
mean(abr6$edu[abr6$publicsector==0 & abr6$employment=="Employed"],na.rm=TRUE)

table(abr8$publicsector[abr8$employment=="Employed"],abr8$edu[abr8$employment=="Employed"])
mean(abr8$edu[abr8$publicsector==1 & abr8$employment=="Employed"],na.rm=TRUE)
mean(abr8$edu[abr8$publicsector==0 & abr8$employment=="Employed"],na.rm=TRUE)

# Regions per country

abr8 %>%
  group_by(COUNTRY) %>%
  summarise(count = n_distinct(REGION)) %>%
  print(n=34)
# >30 clusters in 4/34 countries, so omit clustered SEs

##### POOLED RESULTS #####

pooled_results <- function(ab,round,rnd,dv,dv_name) {
  
  reg1 <- glm(paste(dv,"~edu+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = ab$REGION))
  
  reg2 <- glm(paste(dv,"~edu+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Employed",weights=wt)
  se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = ab$REGION[ab$employment=="Employed"]))
  
  reg3 <- glm(paste(dv,"~edu+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (looking)",weights=wt)
  se3 <- coeftest(reg3, vcov = vcovCL(reg3, cluster = ab$REGION[ab$employment=="Unemployed (looking)"]))
  
  reg4 <- glm(paste(dv,"~edu+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (not looking)",weights=wt)
  se4 <- coeftest(reg4, vcov = vcovCL(reg4, cluster = ab$REGION[ab$employment=="Unemployed (not looking)"]))
  
  reg5 <- glm(paste(dv,"~primary+secondary+highered+college+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se5 <- coeftest(reg5, vcov = vcovCL(reg5, cluster = ab$REGION))
  
  reg6 <- glm(paste(dv,"~primary+secondary+highered+college+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Employed",weights=wt)
  se6 <- coeftest(reg6, vcov = vcovCL(reg6, cluster = ab$REGION[ab$employment=="Employed"]))
  
  reg7 <- glm(paste(dv,"~primary+secondary+highered+college+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (looking)",weights=wt)
  se7 <- coeftest(reg7, vcov = vcovCL(reg7, cluster = ab$REGION[ab$employment=="Unemployed (looking)"]))
  
  reg8 <- glm(paste(dv,"~primary+secondary+highered+college+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (not looking)",weights=wt)
  se8 <- coeftest(reg8, vcov = vcovCL(reg8, cluster = ab$REGION[ab$employment=="Unemployed (not looking)"]))

  cat(stargazer(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,
                title = paste0("Relationship between education and ",dv_name," (",round,")"), label = paste0("tab:ab-hh-ss-",rnd,"-",dv_name),
                dep.var.labels = var_label(ab[,dv])[[1]],
                covariate.labels = c(var_label(ab$edu),var_label(ab$primary),var_label(ab$secondary),var_label(ab$highered),var_label(ab$college),var_label(ab$female)),
                column.sep.width = "-10pt",font.size="small",
                add.lines = list(c("Sample",rep(c("Full","Employed","Looking","Not Looking"),2))),
                se = list(se1[,2], se2[,2],se3[,2],se4[,2],se5[,2],se6[,2],se7[,2],se8[,2]),
                p = list(se1[,4], se2[,4],se3[,4],se4[,4],se5[,4],se6[,4],se7[,4],se8[,4]),
                omit = c("COUNTRY","age","rural","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file=paste0("ab-output/ab-hh-ss-",rnd,"-",dv_name,".tex"), sep="\n")
    
}

pooled_results(abr6,"round 6","r6","openborders_dum","globalization") # missing data due to DV # TABLE 1
pooled_results(abr8,"round 8","r8","openborders_dum","globalization") # missing data due to DV
pooled_results(abr8,"round 8","r8","trade_dum","trade") # missing data due to DV # TABLE 2

##### VALIDATION #####

reg_validate <- glm(trade_dum~openborders_dum, family = binomial(link="probit"),data = abr8)
cat(stargazer(reg_validate,
    title = "Correlation between outcome variables",
    label = "tab:ab-validation-r8",
    dep.var.labels = var_label(abr8$trade_dum),
    covariate.labels = var_label(abr8$openborders_dum),
    column.sep.width = "-10pt", font.size="small",
    omit.stat = c("aic","LL"), omit = c("Constant"), notes.label = "", float = FALSE),
    file = "ab-output/ab-validation-r8.tex", sep="\n")

##### MAIN MODEL BY COUNTRY ####

wdi <- read_csv("WDI download June 8 2022.csv")

model_by_country <- function(ab,date,rnd,dv,dv_name) {
  
  wdi <- wdi %>%
    slice(-c(79:83)) %>%
    rename(ccode = `Country Code`,
           year = Time,
           gdppc = `GDP per capita (current US$) [NY.GDP.PCAP.CD]`) %>%
    filter(year==date) %>%
    select(ccode,gdppc)
  
  bycountry <- ab %>%
    nest(results = -ccode) %>%
    left_join(wdi) %>%
    arrange(gdppc) %>%
    mutate(ccode=factor(ccode, levels=ccode)) %>%
    mutate(model = map(results, ~glm(as.formula(paste(dv,"~edu+age+female+rural")),
                                family = binomial(link="probit"),
                                weights=wt,
                                data = .)),
           tidied = map(model, tidy)) %>%
           #new_se = map(model, ~coeftest(., vcov = vcovCL, cluster = ~REGION)),
           #tidied = map(new_se, tidy)) %>%
    unnest(tidied)
  
   ggplot(subset(bycountry, term=="edu")) +
     geom_point(aes(log(gdppc),estimate)) +
     geom_segment(aes(x=log(gdppc),y=(estimate-1.96*std.error),xend=log(gdppc),yend=(estimate+1.96*std.error)))+
     theme_bw()+
     theme(legend.position="none",axis.text.x=element_text(hjust=1,vjust=0))+xlab("GDP per capita (log)")+ylab(paste0("Education predicts support for ",dv_name,", by country"))
   ggsave(filename=paste0("ab-output/ab-mfx-",rnd,".pdf"))
   
}

# FIGURE 1
model_by_country(abr6,2014,"r6","openborders_dum","globalization")
model_by_country(abr8,2019,"r8","trade_dum","trade")

##### FACTOR ENDOWMENT MODEL #####

reg1 <- glm(openborders_dum~edu*ln_gdppc+COUNTRY+age+female+rural,family = binomial(link="probit"),data=abr6,weights=wt)
se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = abr6$COUNTRY))

reg2 <- glm(trade_dum~edu*ln_gdppc+COUNTRY+age+female+rural,family = binomial(link="probit"),data=abr8,weights=wt)
se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = abr8$COUNTRY))

# TABLE 3
cat(stargazer(reg1,reg2,
              title = "Cross-national test of factor endowment model", label = "tab:ab-hh-mr", order = c("edu"),
              dep.var.labels = c("Support for globalization (0-1)","Support for free trade (0-1)"),
              covariate.labels = c("Edu","Edu*GDPpc (log)","GDPpc (log)"),
              column.sep.width = "-10pt",font.size="small",
              add.lines = list(c("Sample",rep(c("round 6","round 8"),1))),
              se = list(se1[,2], se2[,2]),
              p = list(se1[,4], se2[,4]),
              omit = c("COUNTRY","age","rural","female","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file="ab-output/ab-hh-mr.tex", sep="\n")


# below is the full model separated by employment status

cross_national_full <- function(ab,round,rnd,dv,dv_name) {
  
  reg1 <- glm(paste(dv,"~edu*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = ab$COUNTRY))
  
  reg2 <- glm(paste(dv,"~edu*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = employment=="Employed",weights=wt)
  se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = ab$COUNTRY[ab$employment=="Employed"]))
  
  reg3 <- glm(paste(dv,"~edu*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = employment=="Unemployed (looking)",weights=wt)
  se3 <- coeftest(reg3, vcov = vcovCL(reg3, cluster = ab$COUNTRY[ab$employment=="Unemployed (looking)"]))
  
  reg4 <- glm(paste(dv,"~edu*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = employment=="Unemployed (not looking)",weights=wt)
  se4 <- coeftest(reg4, vcov = vcovCL(reg4, cluster = ab$COUNTRY[ab$employment=="Unemployed (not looking)"]))

  cat(stargazer(reg1,reg2,reg3,reg4,
                title = paste0("Cross-national test of factor endowment model (",round,")"), label = paste0("tab:ab-hh-mr-",rnd,"-",dv_name), order = c("edu"),
                dep.var.labels = var_label(ab[,dv])[[1]],
                covariate.labels = c("Edu","Edu*GDPpc (log)","GDPpc (log)"),
                column.sep.width = "-10pt",font.size="small",
                add.lines = list(c("Sample",rep(c("Full","Employed","Looking","Not Looking"),1))),
                se = list(se1[,2], se2[,2],se3[,2],se4[,2]),
                p = list(se1[,4], se2[,4],se3[,4],se4[,4]),
                omit = c("COUNTRY","age","rural","female","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file=paste0("ab-output/ab-hh-mr-",rnd,"-",dv_name,".tex"), sep="\n")
  
}

cross_national_full(abr6,"round 6","r6","openborders_dum","globalization")
cross_national_full(abr8,"round 8","r8","openborders_dum","globalization")
cross_national_full(abr8,"round 8","r8","trade_dum","trade")


# below are the main results with non-linearities

reg1 <- glm(openborders_dum~primary*ln_gdppc+secondary*ln_gdppc+highered*ln_gdppc+college*ln_gdppc+COUNTRY+age+female+rural,family = binomial(link="probit"),data=abr6,weights=wt)
se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = abr6$COUNTRY))

reg2 <- glm(trade_dum~primary*ln_gdppc+secondary*ln_gdppc+highered*ln_gdppc+college*ln_gdppc+COUNTRY+age+female+rural,family = binomial(link="probit"),data=abr8,weights=wt)
se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = abr8$COUNTRY))

cat(stargazer(reg1,reg2,
              title = "Cross-national test of factor endowment model (non-linearities)", label = "tab:ab-hh-mr-nonlinear",
              dep.var.labels = c("Support for globalization (0-1)","Support for free trade (0-1)"),
              covariate.labels = c("Primary","GDPpc (log)","Secondary","Higher Ed","College","Primary*GDPpc (log)","Secondary*GDPpc (log)","Higher Ed*GDPpc (log)","College*GDPpc (log)"),
              column.sep.width = "-10pt",font.size="small",
              add.lines = list(c("Sample",rep(c("round 6","round 8"),1))),
              se = list(se1[,2], se2[,2]),
              p = list(se1[,4], se2[,4]),
              omit = c("COUNTRY","age","rural","female","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file="ab-output/ab-hh-mr-nonlinear.tex", sep="\n")

##### FACTOR ENDOWMENT MODEL: SKILL RATIO #####

cross_national_skillratio <- function(ab,round,rnd,dv,dv_name) {
  
  reg1 <- glm(paste(dv,"~edu*skillratio+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = ab$COUNTRY))
  
  reg2 <- glm(paste(dv,"~edu*skillratio+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = employment=="Employed",weights=wt)
  se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = ab$COUNTRY[ab$employment=="Employed"]))
  
  reg3 <- glm(paste(dv,"~edu*skillratio+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = employment=="Unemployed (looking)",weights=wt)
  se3 <- coeftest(reg3, vcov = vcovCL(reg3, cluster = ab$COUNTRY[ab$employment=="Unemployed (looking)"]))
  
  reg4 <- glm(paste(dv,"~edu*skillratio+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = employment=="Unemployed (not looking)",weights=wt)
  se4 <- coeftest(reg4, vcov = vcovCL(reg4, cluster = ab$COUNTRY[ab$employment=="Unemployed (not looking)"]))
  
  cat(stargazer(reg1,reg2,reg3,reg4,
                title = paste0("Cross-national test of factor endowment model (skilled labor ratio) (",round,")"), label = paste0("tab:ab-skillratio-",rnd,"-",dv_name), order = c("edu"),
                dep.var.labels = var_label(ab[,dv])[[1]],
                covariate.labels = c("Edu","Edu*Skill Ratio","Skill Ratio"),
                column.sep.width = "-10pt",font.size="small",
                add.lines = list(c("Sample",rep(c("Full","Employed","Looking","Not Looking"),1))),
                se = list(se1[,2], se2[,2],se3[,2],se4[,2]),
                p = list(se1[,4], se2[,4],se3[,4],se4[,4]),
                omit = c("COUNTRY","age","rural","female","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file=paste0("ab-output/ab-skillratio-",rnd,"-",dv_name,".tex"), sep="\n")
  
}

cross_national_skillratio(abr6,"round 6","r6","openborders_dum","globalization")
cross_national_skillratio(abr8,"round 8","r8","trade_dum","trade")

##### FACTOR ENDOWMENT MODEL: REVEALED HUMAN CAPITAL INTENSITY #####

cross_national_rhci <- function(ab,round,rnd,dv,dv_name) {
  
  reg1 <- glm(paste(dv,"~edu*RHCI_ordinal+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = ab$COUNTRY))
  
  reg2 <- glm(paste(dv,"~edu*RHCI_ordinal+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = employment=="Employed",weights=wt)
  se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = ab$COUNTRY[ab$employment=="Employed"]))
  
  reg3 <- glm(paste(dv,"~edu*RHCI_ordinal+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = employment=="Unemployed (looking)",weights=wt)
  se3 <- coeftest(reg3, vcov = vcovCL(reg3, cluster = ab$COUNTRY[ab$employment=="Unemployed (looking)"]))
  
  reg4 <- glm(paste(dv,"~edu*RHCI_ordinal+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = employment=="Unemployed (not looking)",weights=wt)
  se4 <- coeftest(reg4, vcov = vcovCL(reg4, cluster = ab$COUNTRY[ab$employment=="Unemployed (not looking)"]))
  
  cat(stargazer(reg1,reg2,reg3,reg4,
                title = paste0("Cross-national test of factor endowment model (revealed human capital intensity) (",round,")"), label = paste0("tab:ab-rhci-",rnd,"-",dv_name), order = c("edu"),
                dep.var.labels = var_label(ab[,dv])[[1]],
                covariate.labels = c("Edu","Edu*RHCI","RHCI"),
                column.sep.width = "-10pt",font.size="small",
                add.lines = list(c("Sample",rep(c("Full","Employed","Looking","Not Looking"),1))),
                se = list(se1[,2], se2[,2],se3[,2],se4[,2]),
                p = list(se1[,4], se2[,4],se3[,4],se4[,4]),
                omit = c("COUNTRY","age","rural","female","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file=paste0("ab-output/ab-rhci-",rnd,"-",dv_name,".tex"), sep="\n")
  
}

cross_national_rhci(abr6,"round 6","r6","openborders_dum","globalization")
cross_national_rhci(abr8,"round 8","r8","trade_dum","trade")

##### LANDOWNER #####

land <- function(ab,round,rnd,dv,dv_name) {
  
  reg1 <- glm(paste(dv,"~landowner*ln_landabundance+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = ab$COUNTRY))
  
  reg2 <- glm(paste(dv,"~landowner+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = landabundant==1,weights=wt)
  se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = ab$COUNTRY[ab$landabundant==1]))
  
  reg3 <- glm(paste(dv,"~landowner+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab, subset = landabundant==0,weights=wt)
  se3 <- coeftest(reg3, vcov = vcovCL(reg3, cluster = ab$COUNTRY[ab$landabundant==0]))
  
  cat(stargazer(reg1,reg2,reg3,
                title = paste0("Cross-national test of factor endowment model (land abundance) (",round,")"), label = paste0("tab:ab-land-",rnd,"-",dv_name), order = c("landowner"),
                dep.var.labels = var_label(ab[,dv])[[1]],
                covariate.labels = c("Landowner","Landowner*Land Abundance (log)","Land Abundance (log)"),
                column.sep.width = "-10pt",font.size="small",
                add.lines = list(c("Sample",rep(c("Full","Land Abundant","Land Scarce"),1))),
                se = list(se1[,2], se2[,2],se3[,2]),
                p = list(se1[,4], se2[,4],se3[,4]),
                omit = c("COUNTRY","age","rural","female","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file=paste0("ab-output/ab-land-",rnd,"-",dv_name,".tex"), sep="\n")
  
}

land(abr6,"round 6","r6","openborders_dum","globalization")
land(abr8,"round 8","r8","trade_dum","trade")

##### ALTERNATIVE MEASURE OF SKILL #####

alt_skill <- function(ab,round,rnd,dv,dv_name) {

  
  reg1 <- glm(paste(dv,"~skill+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = ab$REGION))
  
  reg2 <- glm(paste(dv,"~skill+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Employed",weights=wt)
  se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = ab$REGION[ab$employment=="Employed"]))
  
  reg3 <- glm(paste(dv,"~skill+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (looking)",weights=wt)
  se3 <- coeftest(reg3, vcov = vcovCL(reg3, cluster = ab$REGION[ab$employment=="Unemployed (looking)"]))
  
  reg4 <- glm(paste(dv,"~skill+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (not looking)",weights=wt)
  se4 <- coeftest(reg4, vcov = vcovCL(reg4, cluster = ab$REGION[ab$employment=="Unemployed (not looking)"]))
  
  reg5 <- glm(paste(dv,"~skill*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se5 <- coeftest(reg5, vcov = vcovCL(reg5, cluster = ab$COUNTRY))
  
  reg6 <- glm(paste(dv,"~skill*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Employed",weights=wt)
  se6 <- coeftest(reg6, vcov = vcovCL(reg6, cluster = ab$COUNTRY[ab$employment=="Employed"]))
  
  reg7 <- glm(paste(dv,"~skill*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (looking)",weights=wt)
  se7 <- coeftest(reg7, vcov = vcovCL(reg7, cluster = ab$COUNTRY[ab$employment=="Unemployed (looking)"]))
  
  reg8 <- glm(paste(dv,"~skill*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (not looking)",weights=wt)
  se8 <- coeftest(reg8, vcov = vcovCL(reg8, cluster = ab$COUNTRY[ab$employment=="Unemployed (not looking)"]))
  
  cat(stargazer(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,
                title = paste0("Substituting an alternative measure of skill (",round,")"), label = paste0("tab:ab-altskill-",rnd,"-",dv_name),
                dep.var.labels = var_label(ab[,dv])[[1]],
                covariate.labels = c("Skill","GDPpc (log)","Skill*GDPpc (log)"),
                column.sep.width = "-10pt",font.size="small",
                add.lines = list(c("Sample",rep(c("Full","Employed","Looking","Not Looking"),2))),
                se = list(se1[,2],se2[,2],se3[,2],se4[,2],se5[,2],se6[,2],se7[,2],se8[,2]),
                p = list(se1[,4],se2[,4],se3[,4],se4[,4],se5[,4],se6[,4],se7[,4],se8[,4]),
                omit = c("COUNTRY","age","female","rural","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file=paste0("ab-output/ab-altskill-",rnd,"-",dv_name,".tex"), sep="\n")
  
}

alt_skill(abr6,"round 6","r6","openborders_dum","globalization")
alt_skill(abr8,"round 8","r8","trade_dum","trade")

##### INCOME AS PROXY FOR SKILL #####

income <- function(ab,round,rnd,dv,dv_name) {
  
  reg1 <- glm(paste(dv,"~assetindex+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = ab$REGION))
  
  reg2 <- glm(paste(dv,"~assetindex+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Employed",weights=wt)
  se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = ab$REGION[ab$employment=="Employed"]))
  
  reg3 <- glm(paste(dv,"~assetindex+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (looking)",weights=wt)
  se3 <- coeftest(reg3, vcov = vcovCL(reg3, cluster = ab$REGION[ab$employment=="Unemployed (looking)"]))
  
  reg4 <- glm(paste(dv,"~assetindex+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (not looking)",weights=wt)
  se4 <- coeftest(reg4, vcov = vcovCL(reg4, cluster = ab$REGION[ab$employment=="Unemployed (not looking)"]))
  
  reg5 <- glm(paste(dv,"~assetindex*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se5 <- coeftest(reg5, vcov = vcovCL(reg5, cluster = ab$COUNTRY))
  
  reg6 <- glm(paste(dv,"~assetindex*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Employed",weights=wt)
  se6 <- coeftest(reg6, vcov = vcovCL(reg6, cluster = ab$COUNTRY[ab$employment=="Employed"]))
  
  reg7 <- glm(paste(dv,"~assetindex*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (looking)",weights=wt)
  se7 <- coeftest(reg7, vcov = vcovCL(reg7, cluster = ab$COUNTRY[ab$employment=="Unemployed (looking)"]))
  
  reg8 <- glm(paste(dv,"~assetindex*ln_gdppc+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (not looking)",weights=wt)
  se8 <- coeftest(reg8, vcov = vcovCL(reg8, cluster = ab$COUNTRY[ab$employment=="Unemployed (not looking)"]))
  
  cat(stargazer(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,
                title = paste0("Using income to proxy for skill (",round,")"), label = paste0("tab:ab-income-",rnd,"-",dv_name),
                dep.var.labels = var_label(ab[,dv])[[1]],
                covariate.labels = c("Asset Index","GDPpc (log)","Asset Index*GDPpc (log)"),
                column.sep.width = "-10pt",font.size="small",
                add.lines = list(c("Sample",rep(c("Full","Employed","Looking","Not Looking"),2))),
                se = list(se1[,2],se2[,2],se3[,2],se4[,2],se5[,2],se6[,2],se7[,2],se8[,2]),
                p = list(se1[,4],se2[,4],se3[,4],se4[,4],se5[,4],se6[,4],se7[,4],se8[,4]),
                omit = c("COUNTRY","age","female","rural","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file=paste0("ab-output/ab-income-",rnd,"-",dv_name,".tex"), sep="\n")
  
}

income(abr6,"round 6","r6","openborders_dum","globalization")
income(abr8,"round 8","r8","trade_dum","trade")

##### CONSUMPTION #####

consumption <- function(ab,round,rnd,dv,dv_name) {
  
  reg1 <- glm(paste(dv,"~edu+highprices+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = ab$REGION))
  
  reg2 <- glm(paste(dv,"~edu+highprices+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Employed",weights=wt)
  se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = ab$REGION[ab$employment=="Employed"]))
  
  reg3 <- glm(paste(dv,"~edu+highprices+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (looking)",weights=wt)
  se3 <- coeftest(reg3, vcov = vcovCL(reg3, cluster = ab$REGION[ab$employment=="Unemployed (looking)"]))
  
  reg4 <- glm(paste(dv,"~edu+highprices+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (not looking)",weights=wt)
  se4 <- coeftest(reg4, vcov = vcovCL(reg4, cluster = ab$REGION[ab$employment=="Unemployed (not looking)"]))
  
  reg5 <- glm(paste(dv,"~edu*ln_gdppc+highprices+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se5 <- coeftest(reg5, vcov = vcovCL(reg5, cluster = ab$COUNTRY))
  
  reg6 <- glm(paste(dv,"~edu*ln_gdppc+highprices+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Employed",weights=wt)
  se6 <- coeftest(reg6, vcov = vcovCL(reg6, cluster = ab$COUNTRY[ab$employment=="Employed"]))
  
  reg7 <- glm(paste(dv,"~edu*ln_gdppc+highprices+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (looking)",weights=wt)
  se7 <- coeftest(reg7, vcov = vcovCL(reg7, cluster = ab$COUNTRY[ab$employment=="Unemployed (looking)"]))
  
  reg8 <- glm(paste(dv,"~edu*ln_gdppc+highprices+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (not looking)",weights=wt)
  se8 <- coeftest(reg8, vcov = vcovCL(reg8, cluster = ab$COUNTRY[ab$employment=="Unemployed (not looking)"]))
  
  cat(stargazer(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,
                title = paste0("Testing the consumption model (",round,")"), label = paste0("tab:ab-consumption-",rnd,"-",dv_name),
                dep.var.labels = var_label(ab[,dv])[[1]],
                covariate.labels = c("Edu","GDPpc (log)","High Prices","Edu*GDPpc (log)"),
                column.sep.width = "-10pt",font.size="small",
                add.lines = list(c("Sample",rep(c("Full","Employed","Looking","Not Looking"),2))),
                se = list(se1[,2], se2[,2],se3[,2],se4[,2],se5[,2],se6[,2],se7[,2],se8[,2]),
                p = list(se1[,4], se2[,4],se3[,4],se4[,4],se5[,4],se6[,4],se7[,4],se8[,4]),
                omit = c("COUNTRY","age","female","rural","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file=paste0("ab-output/ab-consumption-",rnd,"-",dv_name,".tex"), sep="\n")
  
}

consumption(abr6,"round 6","r6","openborders_dum","globalization")
consumption(abr8,"round 8","r8","trade_dum","trade")

##### PUBLIC SECTOR #####

reg2 <- glm(openborders_dum~edu+publicsector+COUNTRY+age+female+rural,family = binomial(link="probit"),data=abr6,subset = employment=="Employed",weights=wt)
se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = abr6$REGION[abr6$employment=="Employed"]))

reg6 <- glm(openborders_dum~edu*ln_gdppc+publicsector+COUNTRY+age+female+rural,family = binomial(link="probit"),data=abr6,subset = employment=="Employed",weights=wt)
se6 <- coeftest(reg6, vcov = vcovCL(reg6, cluster = abr6$COUNTRY[abr6$employment=="Employed"]))

reg2b <- glm(trade_dum~edu+publicsector+COUNTRY+age+female+rural,family = binomial(link="probit"),data=abr8,subset = employment=="Employed",weights=wt)
se2b <- coeftest(reg2b, vcov = vcovCL(reg2b, cluster = abr8$REGION[abr8$employment=="Employed"]))

reg6b <- glm(trade_dum~edu*ln_gdppc+publicsector+COUNTRY+age+female+rural,family = binomial(link="probit"),data=abr8,subset = employment=="Employed",weights=wt)
se6b <- coeftest(reg6b, vcov = vcovCL(reg6b, cluster = abr8$COUNTRY[abr8$employment=="Employed"]))

cat(stargazer(reg2,reg6,reg2b,reg6b,
              title = paste0("Controlling for the public sector"), label = paste0("tab:ab-publicsector"),
              dep.var.labels = c("Support for globalization (0-1)", "Support for free trade (0-1)"),
              covariate.labels = c("Edu","GDPpc (log)","Public Sector","Edu*GDPpc (log)"),
              column.sep.width = "-10pt",font.size="small",
              add.lines = list(c("Round",c("Round 6","Round 6","Round 8","Round 8")),
                               c("Sample",rep(c("Employed","Employed"),2))),
              se = list(se2[,2], se6[,2],se2b[,2],se6b[,2]),
              p = list(se2[,4], se6[,4],se2b[,4],se6b[,4]),
              omit = c("COUNTRY","age","female","rural","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file=paste0("ab-output/ab-publicsector.tex"), sep="\n")

##### CULTURAL EXPLANATIONS #####

cultural <- function(ab,round,rnd,dv,dv_name) {
  
  reg1 <- glm(paste(dv,"~edu+natlid+ethno+xeno+dem+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = ab$REGION))
  
  reg2 <- glm(paste(dv,"~edu+natlid+ethno+xeno+dem+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Employed",weights=wt)
  se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = ab$REGION[ab$employment=="Employed"]))
  
  reg3 <- glm(paste(dv,"~edu+natlid+ethno+xeno+dem+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (looking)",weights=wt)
  se3 <- coeftest(reg3, vcov = vcovCL(reg3, cluster = ab$REGION[ab$employment=="Unemployed (looking)"]))
  
  reg4 <- glm(paste(dv,"~edu+natlid+ethno+xeno+dem+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (not looking)",weights=wt)
  se4 <- coeftest(reg4, vcov = vcovCL(reg4, cluster = ab$REGION[ab$employment=="Unemployed (not looking)"]))
  
  reg5 <- glm(paste(dv,"~edu*ln_gdppc+natlid+ethno+xeno+dem+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,weights=wt)
  se5 <- coeftest(reg5, vcov = vcovCL(reg5, cluster = ab$COUNTRY))
  
  reg6 <- glm(paste(dv,"~edu*ln_gdppc+natlid+ethno+xeno+dem+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Employed",weights=wt)
  se6 <- coeftest(reg6, vcov = vcovCL(reg6, cluster = ab$COUNTRY[ab$employment=="Employed"]))
  
  reg7 <- glm(paste(dv,"~edu*ln_gdppc+natlid+ethno+xeno+dem+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (looking)",weights=wt)
  se7 <- coeftest(reg7, vcov = vcovCL(reg7, cluster = ab$COUNTRY[ab$employment=="Unemployed (looking)"]))
  
  reg8 <- glm(paste(dv,"~edu*ln_gdppc+natlid+ethno+xeno+dem+COUNTRY+age+female+rural"),family = binomial(link="probit"),data=ab,subset = employment=="Unemployed (not looking)",weights=wt)
  se8 <- coeftest(reg8, vcov = vcovCL(reg8, cluster = ab$COUNTRY[ab$employment=="Unemployed (not looking)"]))
  
  cat(stargazer(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,
                title = paste0("Testing non-economic models (",round,")"), label = paste0("tab:ab-cultural-",rnd,"-",dv_name),
                dep.var.labels = var_label(ab[,dv])[[1]],
                covariate.labels = c("Edu","GDPpc (log)","National ID","Ethnocentrism","Xenophobia","Supports Democracy","Edu*GDPpc (log)"),
                column.sep.width = "-10pt",font.size="small",
                add.lines = list(c("Sample",rep(c("Full","Employed","Looking","Not Looking"),2))),
                se = list(se1[,2], se2[,2],se3[,2],se4[,2],se5[,2],se6[,2],se7[,2],se8[,2]),
                p = list(se1[,4], se2[,4],se3[,4],se4[,4],se5[,4],se6[,4],se7[,4],se8[,4]),
                omit = c("COUNTRY","age","female","rural","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file=paste0("ab-output/ab-cultural-",rnd,"-",dv_name,".tex"), sep="\n")
  
}

cultural(abr6,"round 6","r6","openborders_dum","globalization")
cultural(abr8,"round 8","r8","trade_dum","trade")

##### DONT KNOW #####

abr6$openborders_dk <- relevel(abr6$openborders_dk, ref = "Oppose")
abr8$trade_dk <- relevel(abr8$trade_dk, ref = "Oppose")

reg1 <- multinom(openborders_dum~edu+COUNTRY+age+female+rural, data=abr6, weights=wt)

reg2 <- multinom(openborders_dk~edu+COUNTRY+age+female+rural, data=abr6, weights=wt, maxit=200)

cat(stargazer(reg1, reg2,
          title = "Modeling don't knows (round 6)", label = "tab:ab-dkhhss-r6-openborders",
          dep.var.labels = c("Support","Support","Neither","DK"),
          covariate.labels = c("Edu","Age","Female","Rural"),
          column.sep.width = "-10pt",font.size="small",
          add.lines = list(c("Model","Without DKs","With DKs","With DKs","With DKs"),
                           c("N",nrow(residuals(reg1)),rep(nrow(residuals(reg2)),3))),
          omit = c("COUNTRY","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file="ab-output/ab-dkhhss-r6-openborders.tex", sep="\n")

reg3 <- multinom(openborders_dum~edu*ln_gdppc+COUNTRY+age+female+rural, data=abr6, weights=wt)

reg4 <- multinom(openborders_dk~edu*ln_gdppc+COUNTRY+age+female+rural, data=abr6, weights=wt,maxit=200)

cat(stargazer(reg3, reg4,
              title = "Modeling don't knows (round 6)", label = "tab:ab-dkhhmr-r6-openborders",
              dep.var.labels = c("Support","Support","Neither","DK"),
              order = c("edu","ln_gdppc"),
              covariate.labels = c("Edu","Edu*GDPpc (log)","GDPpc (log)","Age","Female","Rural"),
              column.sep.width = "-10pt",font.size="small",
              add.lines = list(c("Model","Without DKs","With DKs","With DKs","With DKs"),
                               c("N",nrow(residuals(reg1)),rep(nrow(residuals(reg2)),3))),
              omit = c("COUNTRY","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file="ab-output/ab-dkhhmr-r6-openborders.tex", sep="\n")


reg5 <- multinom(trade_dum~edu+COUNTRY+age+female+rural, data=abr8, weights=wt)

reg6 <- multinom(trade_dk~edu+COUNTRY+age+female+rural, data=abr8, weights=wt,maxit=200)

cat(stargazer(reg5, reg6,
              title = "Modeling don't knows (round 8)", label = "tab:ab-dkhhss-r8-trade",
              dep.var.labels = c("Support","Support","Neither","DK"),
              covariate.labels = c("Edu","Age","Female","Rural"),
              column.sep.width = "-10pt",font.size="small",
              add.lines = list(c("Model","Without DKs","With DKs","With DKs","With DKs"),
                               c("N",nrow(residuals(reg1)),rep(nrow(residuals(reg2)),3))),
              omit = c("COUNTRY","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file="ab-output/ab-dkhhss-r8-trade.tex", sep="\n")

reg7 <- multinom(trade_dum~edu*ln_gdppc+COUNTRY+age+female+rural, data=abr8, weights=wt)

reg8 <- multinom(trade_dk~edu*ln_gdppc+COUNTRY+age+female+rural, data=abr8, weights=wt,maxit=200)

cat(stargazer(reg7, reg8,
              title = "Modeling don't knows (round 8)", label = "tab:ab-dkhhmr-r8-openborders",
              dep.var.labels = c("Support","Support","Neither","DK"),
              order = c("edu","ln_gdppc"),
              covariate.labels = c("Edu","Edu*GDPpc (log)","GDPpc (log)","Age","Female","Rural"),
              column.sep.width = "-10pt",font.size="small",
              add.lines = list(c("Model","Without DKs","With DKs","With DKs","With DKs"),
                               c("N",nrow(residuals(reg1)),rep(nrow(residuals(reg2)),3))),
              omit = c("COUNTRY","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file="ab-output/ab-dkhhmr-r8-trade.tex", sep="\n")

##### COVID #####

abr8_covid <- abr8 %>%
  select(COUNTRY,postcovid,gdppc,ln_gdppc) %>%
  distinct()

abr8_covid %>%
  group_by(postcovid) %>%
  summarise(mean = mean(gdppc))

reg1 <- glm(trade_dum ~ edu + COUNTRY + age + female + rural, family = binomial(link= "probit"), data = abr8, weights = wt, subset = postcovid == 0)
se1 <- coeftest(reg1, vcov = vcovCL(reg1, cluster = abr8$REGION[abr8$postcovid==0]))

reg2 <- glm(trade_dum ~ edu*ln_gdppc + COUNTRY + age + female + rural, family = binomial(link= "probit"), data = abr8, weights = wt, subset = postcovid == 0)
se2 <- coeftest(reg2, vcov = vcovCL(reg2, cluster = abr8$COUNTRY[abr8$postcovid==0]))

reg3 <- glm(trade_dum ~ edu + COUNTRY + age + female + rural, family = binomial(link= "probit"), data = abr8, weights = wt, subset = postcovid == 1)
se3 <- coeftest(reg3, vcov = vcovCL(reg3, cluster = abr8$REGION[abr8$postcovid==1]))

reg4 <- glm(trade_dum ~ edu*ln_gdppc + COUNTRY + age + female + rural, family = binomial(link= "probit"), data = abr8, weights = wt, subset = postcovid == 1)
se4 <- coeftest(reg4, vcov = vcovCL(reg4, cluster = abr8$COUNTRY[abr8$postcovid==1]))


cat(stargazer(reg1,reg2,reg3,reg4,
              label = "tab:ab-covid",
          dep.var.labels = c("Support for free trade (0-1)"),
          order = c("edu","ln_gdppc"),
          covariate.labels = c("Edu","Edu*GDPpc (log)","GDPpc (log)","Age","Female","Rural"),
          column.sep.width = "-10pt",font.size="small",
          add.lines = list(c("Sample","Pre-COVID","Pre-COVID","Post-COVID","Post-COVID")),
          omit = c("age","female","rural","COUNTRY","Constant"), omit.stat=c("aic","LL"), notes.label = "", float = FALSE), file="ab-output/ab-covid.tex", sep="\n")


##### GHANA AND UGANDA #####

abr6_gha <- abr6 %>%
  filter(ccode=="GHA")

reg1_abr6_gha <- glm(openborders_dum~edu+age+female+rural,family = binomial(link="probit"),data=abr6_gha,weights=wt)
se1_abr6_gha <- coeftest(reg1_abr6_gha, vcov = vcovCL(reg1_abr6_gha, cluster = abr6_gha$REGION))

reg2_abr6_gha <- glm(openborders_dum~edu+age+female+rural,family = binomial(link="probit"),data=abr6_gha,subset = employment=="Employed",weights=wt)
se2_abr6_gha <- coeftest(reg2_abr6_gha, vcov = vcovCL(reg2_abr6_gha, cluster = abr6_gha$REGION[abr6_gha$employment=="Employed"]))

reg3_abr6_gha <- glm(openborders_dum~edu+age+female+rural,family = binomial(link="probit"),data=abr6_gha,subset = employment=="Unemployed (looking)",weights=wt)
se3_abr6_gha <- coeftest(reg3_abr6_gha, vcov = vcovCL(reg3_abr6_gha, cluster = abr6_gha$REGION[abr6_gha$employment=="Unemployed (looking)"]))

abr8_gha <- abr8 %>%
  filter(ccode=="GHA")

reg1_abr8_gha <- glm(trade_dum~edu+age+female+rural,family = binomial(link="probit"),data=abr8_gha,weights=wt)
se1_abr8_gha <- coeftest(reg1_abr8_gha, vcov = vcovCL(reg1_abr8_gha, cluster = abr8_gha$REGION))

reg2_abr8_gha <- glm(trade_dum~edu+age+female+rural,family = binomial(link="probit"),data=abr8_gha,subset = employment=="Employed",weights=wt)
se2_abr8_gha <- coeftest(reg2_abr8_gha, vcov = vcovCL(reg2_abr8_gha, cluster = abr8_gha$REGION[abr8_gha$employment=="Employed"]))

abr6_uga <- abr6 %>%
  filter(ccode=="UGA")

reg1_abr6_uga <- glm(openborders_dum~edu+age+female+rural,family = binomial(link="probit"),data=abr6_uga,weights=wt)
se1_abr6_uga <- coeftest(reg1_abr6_uga, vcov = vcovCL(reg1_abr6_uga, cluster = abr6_uga$REGION))

reg2_abr6_uga <- glm(openborders_dum~edu+age+female+rural,family = binomial(link="probit"),data=abr6_uga,subset = employment=="Employed",weights=wt)
se2_abr6_uga <- coeftest(reg2_abr6_uga, vcov = vcovCL(reg2_abr6_uga, cluster = abr6_uga$REGION[abr6_uga$employment=="Employed"]))

abr8_uga <- abr8 %>%
  filter(ccode=="UGA")

reg1_abr8_uga <- glm(trade_dum~edu+age+female+rural,family = binomial(link="probit"),data=abr8_uga,weights=wt)
se1_abr8_uga <- coeftest(reg1_abr8_uga, vcov = vcovCL(reg1_abr8_uga, cluster = abr8_uga$REGION))

reg2_abr8_uga <- glm(trade_dum~edu+age+female+rural,family = binomial(link="probit"),data=abr8_uga,subset = employment=="Employed",weights=wt)
se2_abr8_uga <- coeftest(reg2_abr8_uga, vcov = vcovCL(reg2_abr8_uga, cluster = abr8_uga$REGION[abr8_uga$employment=="Employed"]))


save(reg1_abr6_gha,reg2_abr6_gha,reg1_abr8_gha,reg2_abr8_gha,
     se1_abr6_gha,se2_abr6_gha,se1_abr8_gha,se2_abr8_gha,
     reg1_abr6_uga,reg2_abr6_uga,reg1_abr8_uga,reg2_abr8_uga,
     se1_abr6_uga,se2_abr6_uga,se1_abr8_uga,se2_abr8_uga,
     file="ab-output/regs.RData")

# MAPS

map <- as.data.frame(aggregate(abr6[,c("openborders_dum")],by=list(abr6$ccode),FUN=mean,na.rm=TRUE))
colnames(map) <- c("country","openborders_dum")
map$inclusion <- 1

maps <- joinCountryData2Map(map, joinCode = "ISO3", nameJoinColumn = "country", nameCountryColumn = "ctry")

pdf("ab-output/map-r6-selection.pdf")
mapss <- mapCountryData(mapToPlot = maps, nameColumnToPlot = "inclusion", mapRegion = "africa",
                        catMethod = "categorical",colourPalette = "black2White",
                        addLegend = FALSE, mapTitle = "")
dev.off()

pdf("ab-output/map-r6-globalization.pdf")
mapss2 <- mapCountryData(mapToPlot = maps, nameColumnToPlot = "openborders_dum", mapRegion = "africa",
                         catMethod = "categorical",colourPalette = "white2Black",
                         addLegend = FALSE, mapTitle = "")
do.call( addMapLegend, c(mapss2, legendWidth=0.5, legendMar = 3))
dev.off()

map <- as.data.frame(aggregate(abr8[,c("ln_gdppc","openborders_dum","trade_dum")],by=list(abr8$ccode),FUN=mean,na.rm=TRUE))
colnames(map) <- c("country","ln_gdppc","openborders_dum","trade_dum")
map$inclusion <- 1

maps <- joinCountryData2Map(map, joinCode = "ISO3", nameJoinColumn = "country", nameCountryColumn = "ctry")

pdf("ab-output/map-r8-selection.pdf")
mapss <- mapCountryData(mapToPlot = maps, nameColumnToPlot = "inclusion", mapRegion = "africa",
                        catMethod = "categorical",colourPalette = "black2White",
                        addLegend = FALSE, mapTitle = "")
dev.off()


pdf("ab-output/map-r8-globalization.pdf")
mapss2 <- mapCountryData(mapToPlot = maps, nameColumnToPlot = "openborders_dum", mapRegion = "africa",
                         catMethod = "categorical",colourPalette = "white2Black",
                         addLegend = FALSE, mapTitle = "")
do.call( addMapLegend, c(mapss2, legendWidth=0.5, legendMar = 3))
dev.off()

pdf("ab-output/map-r8-trade.pdf")
mapss2 <- mapCountryData(mapToPlot = maps, nameColumnToPlot = "trade_dum", mapRegion = "africa",
                         catMethod = "categorical",colourPalette = "white2Black",
                         addLegend = FALSE, mapTitle = "")
do.call( addMapLegend, c(mapss2, legendWidth=0.5, legendMar = 3))
dev.off()

ggplot(map, aes(x=ln_gdppc,y=trade_dum,label=country)) + geom_text()


##### BENCHMARK AGAINST LATIN AMERICA #####

abr6_gdppc <- abr6 %>%
  select(cname, gdppc) %>%
  distinct() %>%
  mutate(sample = "Afrobarometer (round 6)")

cname <- c("Paraguay","Honduras","Mexico","Venezuela","Guatemala","Brazil","Bolivia","Peru","Uruguay","Argentina","Ecuador",
        "Costa Rica","El Salvador","Chile","Nicaragua","Colombia","Panama")
gdppc <- c(5260,2800,7100,5670,4000,6580,2150,4260,8090,11010,3140,7880,4670,7090,2520,7730,5040) # from Beaulieu et al. (2005)
sample <- rep("Latinobarometro (1996)",length(cname))
latam_gdppc <- as.data.frame(cbind(cname,gdppc,sample))

benchmark <- rbind(abr6_gdppc,latam_gdppc)
benchmark <- benchmark %>% mutate(gdppc = as.numeric(gdppc),
                                  ln_gdppc = log(gdppc))

# FIGURE 4
ggplot(data=benchmark,aes(x=ln_gdppc,fill=sample)) + geom_density(alpha=.5) + xlab("GDP per capita (logged)") + ylab("Density")+theme_bw() +
  scale_fill_viridis_d(option="D") + theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.title = element_blank(),legend.position = "bottom")
ggsave(file="AB-output/sample-distribution.pdf",height=4,width=4)
