##############################################################################
#### This file provides code for replicating all the results
#### found in the manuscript for "Corporate Influence in World Bank Lending" 
#### as well as the associated online appendix.
#### (Malik and Stone, 2017)
##############################################################################

rm(list=ls())
library(foreign)
library(MASS)
library(stargazer)
library(frm)
library(ggplot2)
library(corrplot)

### Load all the datasets needed for replication:
### Note: The reader will have to put in the appropriate paths to the datasets depending
### on where they have been saved.

wbfull <- read.csv("...insert file path here/Final_wbfull.csv")
wb_proc <- read.csv("...insert file path here/Final_wbproc.csv")
wb <- read.csv("...insert file path here/Final_wb.csv")
kilby <- read.csv("...insert file path here/Final_kilbycompare.csv")
ieg<-read.csv("...insert file path here/Final_IEG_data.csv")

### The following code replicates Figure 1 from the main text (as well
# as Figure A1 in Online Appendix)
wbfull2 <- subset(wbfull, !is.na(Outcome_Num2) & !is.na(avgPDO))
# calculate densities
denDat <- list(length=6)
for (i in 1:6) {
  temp <- subset(wbfull2, Outcome_Num2==i)
  denDat[[i]] <- with(density(temp$avgPDO), cbind(x, y))
}
for(i in 1:length(denDat)){
  denDat[[i]] <- data.frame(value=sort(unique(wbfull2$Outcome_Num2))[i], x=denDat[[i]][,1], y=denDat[[i]][,2])
}
denDat <- do.call(rbind, denDat)
denDat$value = as.factor(denDat$value)
# plot figure
ggplot(denDat)+
  geom_line(aes(x=x, y=y, linetype=value)) +
  xlab('Performance') +
  ylab('Density') +
  ggtitle('') +
  guides(fill = guide_legend(title = "Evaluation"),
         linetype = guide_legend(title = "Evaluation")) +
  theme_bw(16) +
  xlim(c(0,4)) +
  ylim(c(0,1))

### The following code replicates Table 2 from the main text
fordesc1 <- data.frame(cbind(wbfull$disb_perc3, 
                             wbfull$Outcome_Num2,
                             wbfull$avgPDO,
                             wbfull$MNC_any,
                             wbfull$MNC_Inv,
                             wbfull$polity_end_lag, 
                             log(wbfull$pop_end_lag),
                             log(wbfull$rgdp_end_lag), 
                             wbfull$CorruptScore_end,                             
                             wbfull$PDO_Table, 
                             wbfull$Key_Matrix,
                             wbfull$Comp_.Analysis, 
                             wbfull$Ratings_Obj_Ach, 
                             wbfull$IBRD, 
                             wbfull$IDA, 
                             wbfull$approvalyr,
                             wbfull$wbclosingyr,
                             wbfull$projsize_pop_st2))
stargazer(fordesc1, type= "latex",
          summary.stat=c("mean", "median", "sd", "min", "max"),
          covariate.labels=c("Disbursement Proportion",
                             "Evaluation",
                             "Performance",
                             "MNC Contractor",
                             "US MNC",
                             "Polity$_{t-1}$", 
                             "log (Population$_{t-1})$",
                             "log (GDP per capita$_{t-1}$)",
                             "Control of Corruption",
                             "Report Type 4",
                             "Report Type 3", 
                             "Report Type 2",
                             "Report Type 1", 
                             "IBRD", 
                             "IDA",
                             "Approval Year", 
                             "Report Year",
                             "Project Size per capita"))
# Note that US Fortune 500 descriptives come from the following because
# regressions for that main variable are run using a slightly different
# dataset, given data availability etc, as discussed earlier.
summary(wb$p_obs_usa_end_5yr)
sd(wb$p_obs_usa_end_5yr, na.rm=T)

# Corresponding Table A2 on Descriptive Stats in Online Appendix (more variables than
# Table 2 in paper). Note that because we have more than one main dataset being used in the paper,
# some of the descriptives are generated outside of the stargazer table; all 
# information in the paper is generated using the following code, however.
fordesc1_app <- data.frame(cbind(wbfull$disb_perc3, 
                             wbfull$Outcome_Num2,
                             wbfull$avgPDO,
                             wbfull$MNC_any,
                             wbfull$MNC_Inv,
                             wbfull$Fra_MNC,
                             wbfull$Ger_MNC,
                             wbfull$Japan_MNC,
                             wbfull$UK_MNC,
                             wbfull$polity_end_lag, 
                             log(wbfull$pop_end_lag),
                             log(wbfull$rgdp_end_lag), 
                             wbfull$CorruptScore_end,                             
                             wbfull$PDO_Table, 
                             wbfull$Key_Matrix,
                             wbfull$Comp_.Analysis, 
                             wbfull$Ratings_Obj_Ach, 
                             wbfull$IBRD, 
                             wbfull$IDA, 
                             wbfull$approvalyr,
                             wbfull$wbclosingyr,
                             wbfull$projsize_pop_st2))
stargazer(fordesc1_app, type= "latex",
          summary.stat=c("n", "mean", "median", "sd", "min", "max"),
          covariate.labels=c("Disbursement Proportion",
                             "Evaluation",
                             "Performance",
                             "MNC Contractor",
                             "US MNC",
                             "France MNC",
                             "Germany MNC",
                             "Japan MNC",
                             "UK MNC",
                             "Polity$_{t-1}$", 
                             "log (Population$_{t-1})$",
                             "log (GDP per capita$_{t-1}$)",
                             "Control of Corruption",
                             "Report Type 4",
                             "Report Type 3", 
                             "Report Type 2",
                             "Report Type 1", 
                             "IBRD", 
                             "IDA",
                             "Approval Year", 
                             "Report Year",
                             "Project Size per capita"))

# Table A2 information continued; CHANGE THIS IN PAPER!!!
summary(wb$p_obs_usa_end_5yr)
sd(wb$p_obs_usa_end_5yr, na.rm=T)

summary(wb$p_obs_fra_end_5yr)
sd(wb$p_obs_fra_end_5yr, na.rm=T)

summary(wb$p_obs_ger_end_5yr)
sd(wb$p_obs_ger_end_5yr, na.rm=T)

summary(wb$p_obs_jap_end_5yr)
sd(wb$p_obs_jap_end_5yr, na.rm=T)

summary(wb$p_obs_uk_end_5yr)
sd(wb$p_obs_uk_end_5yr, na.rm=T)

summary(wb_proc$Any_Management)
sd(wb_proc$Any_Management, na.rm=T)

summary(wb_proc$US_PT_Management)
sd(wb_proc$US_PT_Management, na.rm=T)

summary(wb_proc$France_PT_Management)
sd(wb_proc$France_PT_Management, na.rm=T)

summary(wb_proc$Germany_PT_Management)
sd(wb_proc$Germany_PT_Management, na.rm=T)

summary(wb_proc$Japan_PT_Management)
sd(wb_proc$Japan_PT_Management, na.rm=T)

summary(wb_proc$UK_PT_Management)
sd(wb_proc$UK_PT_Management, na.rm=T)

### The following code replicates Table 3 from the main text (and Table A3
# in the online appendix, which presents an extension of Table 3)

## Splitting in to good (vs not) performing projects, and Any MNC involvement (vs not)
goodMNC <- subset(wbfull, avgPDO>3 & MNC_any==1)
goodNoMNC <- subset(wbfull, avgPDO>3 & MNC_any==0)
badMNC <- subset(wbfull, avgPDO<=3 & MNC_any==1)
badNoMNC <- subset(wbfull, avgPDO<=3 & MNC_any==0)
# for means and p-value of difference in means
t.test(goodMNC$disb_perc3, goodNoMNC$disb_perc3)
t.test(badMNC$disb_perc3, badNoMNC$disb_perc3)

# US MNC involvement
USgoodMNC <- subset(wbfull, avgPDO>3 & MNC_Inv==1)
USgoodNoMNC <- subset(wbfull, avgPDO>3 & MNC_Inv==0)
USbadMNC <- subset(wbfull, avgPDO<=3 & MNC_Inv==1)
USbadNoMNC <- subset(wbfull, avgPDO<=3 & MNC_Inv==0)
t.test(USgoodMNC$disb_perc3, USgoodNoMNC$disb_perc3)
t.test(USbadMNC$disb_perc3, USbadNoMNC$disb_perc3)

# Any Management MNC
pgoodMNC <- subset(wb_proc, avgPDO>3 & Any_Management==1)
pgoodNoMNC <- subset(wb_proc, avgPDO>3 & Any_Management==0)
pbadMNC <- subset(wb_proc, avgPDO<=3 & Any_Management==1)
pbadNoMNC <- subset(wb_proc, avgPDO<=3 & Any_Management==0)
t.test(pgoodMNC$disb_perc3, pgoodNoMNC$disb_perc3)
t.test(pbadMNC$disb_perc3, pbadNoMNC$disb_perc3)

# US Management MNC
USpgoodMNC <- subset(wb_proc, avgPDO>3 & US_PT_Management==1)
USpgoodNoMNC <- subset(wb_proc, avgPDO>3 & US_PT_Management==0)
USpbadMNC <- subset(wb_proc, avgPDO<=3 & US_PT_Management==1)
USpbadNoMNC <- subset(wb_proc, avgPDO<=3 & US_PT_Management==0)
t.test(USpgoodMNC$disb_perc3, USpgoodNoMNC$disb_perc3)
t.test(USpbadMNC$disb_perc3, USpbadNoMNC$disb_perc3)

### The following code replicates Table 4 from the main text (and Table A4
# in the online appendix)
wbfull2 <- subset(wbfull, !is.na(disb_perc3) & !is.na(Outcome_Num2 >-1) & 
                    !is.na(avgPDO) & !is.na(MNC_any) & !is.na(polity_end_lag) &
                    !is.na(pop_end_lag) & !is.na(rgdp_end_lag) & !is.na(CorruptScore_end) &
                    !is.na(PDO_Table) & !is.na(Key_Matrix) & !is.na(Comp_.Analysis) &
                    !is.na(Ratings_Obj_Ach) & !is.na(IBRD) & !is.na(IDA) &
                    !is.na(wbclosingyr) & !is.na(projsize_pop_st2)) 

disb1_any<- (lm(disb_perc3 ~ avgPDO +
                  Outcome_Num2 +
                  MNC_any +
                  projsize_pop_st2 +      
                  polity_end_lag +
                  CorruptScore_end +
                  log(rgdp_end_lag) +
                  log(pop_end_lag) +
                  IBRD +
                  wbclosingyr +
                  PDO_Table +
                  Key_Matrix +
                  Comp_.Analysis + 
                  as.factor(Country) +
                  as.factor(wbclosingyr)-1,                 
                data=wbfull2))

disb1_US <- (lm(disb_perc3 ~ avgPDO +
                  Outcome_Num2 +
                  MNC_Inv +
                  projsize_pop_st2 +      
                  polity_end_lag +
                  CorruptScore_end +
                  log(rgdp_end_lag) +
                  log(pop_end_lag) +
                  IBRD +
                  wbclosingyr +
                  PDO_Table +
                  Key_Matrix +
                  Comp_.Analysis +
                  as.factor(Country) +
                  as.factor(wbclosingyr)-1,                 
                data=wbfull2))

disb1_Fra <- (lm(disb_perc3 ~ avgPDO +
                   Outcome_Num2 +
                   Fra_MNC +
                   projsize_pop_st2 +      
                   polity_end_lag +
                   CorruptScore_end +
                   log(rgdp_end_lag) +
                   log(pop_end_lag) +
                   IBRD +
                   wbclosingyr +
                   PDO_Table +
                   Key_Matrix +
                   Comp_.Analysis +
                   as.factor(Country) +
                   as.factor(wbclosingyr)-1,                 
                 data=wbfull2))

disb1_Ger <- (lm(disb_perc3 ~ avgPDO +
                   Outcome_Num2 +
                   Ger_MNC +
                   projsize_pop_st2 +      
                   polity_end_lag +
                   CorruptScore_end +
                   log(rgdp_end_lag) +
                   log(pop_end_lag) +
                   IBRD +
                   wbclosingyr +
                   PDO_Table +
                   Key_Matrix +
                   Comp_.Analysis +
                   as.factor(Country) +
                   as.factor(wbclosingyr)-1,                 
                 data=wbfull2))

disb1_Japan <- (lm(disb_perc3 ~ avgPDO +
                     Outcome_Num2 +
                     Japan_MNC +
                     projsize_pop_st2 +      
                     polity_end_lag +
                     CorruptScore_end +
                     log(rgdp_end_lag) +
                     log(pop_end_lag) +
                     IBRD +
                     wbclosingyr +
                     PDO_Table +
                     Key_Matrix +
                     Comp_.Analysis +
                     as.factor(Country) +
                     as.factor(wbclosingyr)-1,                 
                   data=wbfull2))

disb1_UK <- (lm(disb_perc3 ~ avgPDO +
                  Outcome_Num2 +
                  UK_MNC +
                  projsize_pop_st2 +      
                  polity_end_lag +
                  CorruptScore_end +
                  log(rgdp_end_lag) +
                  log(pop_end_lag) +
                  IBRD +
                  wbclosingyr +
                  PDO_Table +
                  Key_Matrix +
                  Comp_.Analysis +
                  as.factor(Country) +
                  as.factor(wbclosingyr)-1,                 
                data=wbfull2))

# generating results table
stargazer(disb1_any, disb1_US, disb1_Fra, disb1_Ger, disb1_Japan, disb1_UK,
          type="latex",
          title="Project Disbursement and MNC Contractors",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement proportion"),
          column.separate=c(6),
          covariate.labels=c("Performance", "Evaluation",                            
                             "Any MNC Contractor", "US MNC",
                             "France MNC", "Germany MNC",
                             "Japan MNC", "UK MNC",
                             "Project Size per capita",
                             "Polity$_{t-1}$", "Control of Corruption",
                             "Log(GDP per capita)$_{t-1}$",
                             "Log(Population)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(15:115),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table 5 from the main text (and Table A5 in
# the online appendix, which has the complete results for the same specifications)
any.proctype_disb <- (lm(disb_perc3 ~ avgPDO +
                           Outcome_Num2 +
                           Any_Management +
                           polity_end_lag +
                           projsize_pop_st2 +      
                           CorruptScore_end +
                           log(rgdp_end_lag) +
                           log(pop_end_lag) +
                           IBRD +
                           wbclosingyr +
                           PDO_Table +
                           Key_Matrix +
                           Comp_.Analysis +
                           as.factor(wbclosingyr) +
                           as.factor(Country)-1,   
                         data=wb_proc))

any.proctype_disbUS <- (lm(disb_perc3 ~ avgPDO +
                             Outcome_Num2 +
                             US_PT_Management +
                             polity_end_lag +
                             projsize_pop_st2 +      
                             CorruptScore_end +
                             log(rgdp_end_lag) +
                             log(pop_end_lag) +
                             IBRD +
                             wbclosingyr +
                             PDO_Table +
                             Key_Matrix +
                             Comp_.Analysis +
                             as.factor(wbclosingyr) +
                             as.factor(Country)-1,   
                           data=wb_proc))

any.proctype_disbJap <- (lm(disb_perc3 ~ avgPDO +
                              Outcome_Num2 +
                              Japan_PT_Management +
                              polity_end_lag +
                              projsize_pop_st2 +      
                              CorruptScore_end +
                              log(rgdp_end_lag) +
                              log(pop_end_lag) +
                              IBRD +
                              wbclosingyr +
                              PDO_Table +
                              Key_Matrix +
                              Comp_.Analysis +
                              as.factor(wbclosingyr) +
                              as.factor(Country)-1,   
                            data=wb_proc))

any.proctype_disbUK <- (lm(disb_perc3 ~ avgPDO +
                             Outcome_Num2 +
                             UK_PT_Management +
                             polity_end_lag +
                             projsize_pop_st2 +      
                             CorruptScore_end +
                             log(rgdp_end_lag) +
                             log(pop_end_lag) +
                             IBRD +
                             wbclosingyr +
                             PDO_Table +
                             Key_Matrix +
                             Comp_.Analysis +
                             as.factor(wbclosingyr) +
                             as.factor(Country)-1,   
                           data=wb_proc))

any.proctype_disbGer <- (lm(disb_perc3 ~ avgPDO +
                              Outcome_Num2 +
                              Germany_PT_Management +
                              polity_end_lag +
                              projsize_pop_st2 +      
                              CorruptScore_end +
                              log(rgdp_end_lag) +
                              log(pop_end_lag) +
                              IBRD +
                              wbclosingyr +
                              PDO_Table +
                              Key_Matrix +
                              Comp_.Analysis +
                              as.factor(wbclosingyr) +
                              as.factor(Country)-1,   
                            data=wb_proc))

any.proctype_disbFra <- (lm(disb_perc3 ~ avgPDO +
                              Outcome_Num2 +
                              France_PT_Management +
                              polity_end_lag +
                              projsize_pop_st2 +      
                              CorruptScore_end +
                              log(rgdp_end_lag) +
                              log(pop_end_lag) +
                              IBRD +
                              wbclosingyr +
                              PDO_Table +
                              Key_Matrix +
                              Comp_.Analysis +
                              as.factor(wbclosingyr) +
                              as.factor(Country)-1,   
                            data=wb_proc))

stargazer(any.proctype_disb, any.proctype_disbUS, any.proctype_disbFra,
          any.proctype_disbGer, any.proctype_disbJap, any.proctype_disbUK,
          type="latex",
          title="Project Disbursement and MNC Management Contractors",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement proportion"),
          column.separate=c(6),
          covariate.labels=c("Performance", "Evaluation",                            
                             "Any Management", "US MNC",
                             "France MNC", "Germany MNC",
                             "Japan MNC", "UK MNC",
                             "Proj. Size pc",
                             "Polity$_{t-1}$", "Corruption Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Population)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(16:114),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table 6 from the main text (and Table A6 in
# the online appendix, which has the complete results for the same specifications)
mod.o1_any <- (lm(Outcome_Num2 ~ avgPDO +
                    Any_Management +
                    polity_end_lag +
                    CorruptScore_end +
                    log(rgdp_end_lag) +
                    log(pop_end_lag) +
                    projsize_pop_st2 +
                    IBRD +
                    wbclosingyr +
                    PDO_Table +
                    Key_Matrix +
                    Comp_.Analysis +
                    as.factor(Country) +
                    as.factor(wbclosingyr) -1,
                  data=wb_proc))

mod.o1_anyUS <- (lm(Outcome_Num2 ~ avgPDO +
                      US_PT_Management +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      projsize_pop_st2 +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(Country) +
                      as.factor(wbclosingyr)-1,
                    data=wb_proc))

mod.o1_anyJap <- (lm(Outcome_Num2 ~ avgPDO +
                       Japan_PT_Management +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country) +
                       as.factor(wbclosingyr)-1,
                     data=wb_proc))

mod.o1_anyUK <- (lm(Outcome_Num2 ~ avgPDO +
                      UK_PT_Management +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      projsize_pop_st2 +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(Country) +
                      as.factor(wbclosingyr)-1,
                    data=wb_proc))

mod.o1_anyGer <- (lm(Outcome_Num2 ~ avgPDO +
                       Germany_PT_Management +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country) +
                       as.factor(wbclosingyr)-1,
                     data=wb_proc))

mod.o1_anyFra <- (lm(Outcome_Num2 ~ avgPDO +
                       France_PT_Management +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country) +
                       as.factor(wbclosingyr)-1,
                     data=wb_proc))

stargazer(mod.o1_any, mod.o1_anyUS, mod.o1_anyFra, 
          mod.o1_anyGer, mod.o1_anyJap, mod.o1_anyUK,
          type="latex",
          title="Project Evaluation and Management MNC Contractors",
          style="ajps",
          summary=TRUE,
          column.labels=c("Evaluation"),
          column.separate=c(6),
          covariate.labels=c("Performance", 
                             "Any MNC", "US MNC",
                             "France MNC", "Germany MNC",
                             "Japan MNC", "UK MNC",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corr.Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(15:113),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table 7 from the main text (and Table A7 in
# the online appendix, which has the complete results for the same specifications)
mod.pdo1_any <- (lm(avgPDO ~ Any_Management +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      projsize_pop_st2 +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(Country) +
                      as.factor(wbclosingyr) -1,
                    data=wb_proc))

mod.pdo1_anyUS <- (lm(avgPDO ~ US_PT_Management +
                        polity_end_lag +
                        CorruptScore_end +
                        log(rgdp_end_lag) +
                        log(pop_end_lag) +
                        projsize_pop_st2 +
                        IBRD +
                        wbclosingyr +
                        PDO_Table +
                        Key_Matrix +
                        Comp_.Analysis +
                        as.factor(Country) +
                        as.factor(wbclosingyr)-1,
                      data=wb_proc))

mod.pdo1_anyJap <- (lm(avgPDO ~ Japan_PT_Management +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag) +
                         log(pop_end_lag) +
                         projsize_pop_st2 +
                         IBRD +
                         wbclosingyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country) +
                         as.factor(wbclosingyr)-1,
                       data=wb_proc))

mod.pdo1_anyUK <- (lm(avgPDO ~ UK_PT_Management +
                        polity_end_lag +
                        CorruptScore_end +
                        log(rgdp_end_lag) +
                        log(pop_end_lag) +
                        projsize_pop_st2 +
                        IBRD +
                        wbclosingyr +
                        PDO_Table +
                        Key_Matrix +
                        Comp_.Analysis +
                        as.factor(Country) +
                        as.factor(wbclosingyr)-1,
                      data=wb_proc))

mod.pdo1_anyGer <- (lm(avgPDO ~ Germany_PT_Management +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag) +
                         log(pop_end_lag) +
                         projsize_pop_st2 +
                         IBRD +
                         wbclosingyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country) +
                         as.factor(wbclosingyr)-1,
                       data=wb_proc))

mod.pdo1_anyFra <- (lm(avgPDO ~ France_PT_Management +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag) +
                         log(pop_end_lag) +
                         projsize_pop_st2 +
                         IBRD +
                         wbclosingyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country) +
                         as.factor(wbclosingyr)-1,
                       data=wb_proc))

stargazer(mod.pdo1_any, mod.pdo1_anyUS, mod.pdo1_anyFra, 
          mod.pdo1_anyGer, mod.pdo1_anyJap, mod.pdo1_anyUK,
          type="latex",
          title="Project Performance and Management MNC Contractors",
          style="ajps",
          summary=TRUE,
          column.labels=c("Performance"),
          column.separate=c(6),
          covariate.labels=c("Any MNC", "US MNC",
                             "France MNC", "Germany MNC",
                             "Japan MNC", "UK MNC",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corr. Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(14:112),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table 8 from the main text (and Table A8 in
# the online appendix, which has the complete results for the same specifications)
disb2_US<- (lm(disb_perc2 ~ avgPDO +
                 Outcome_Num +
                 p_obs_usa_end_5yr +
                 projsize_pop_st2 +      
                 projsize_pop_st2*p_obs_usa_end_5yr +
                 polity_end_lag +
                 CorruptScore_end +
                 log(rgdp_end_lag) +
                 log(pop_end_lag) +
                 total_active_proj_end +
                 IBRD +
                 reportyr +
                 PDO_Table +
                 Key_Matrix +
                 Comp_.Analysis +
                 as.factor(Country)+
                 as.factor(reportyr)-1,                 
               data=wb))

summary(lm(disb_perc2 ~ avgPDO +
                 Outcome_Num +
                 p_obs_usa_end_5yr +
                 projsize_pop_st2 +      
                 projsize_pop_st2*p_obs_usa_end_5yr +
                 polity_end_lag +
                 CorruptScore_end +
                 log(rgdp_end_lag) +
                 log(pop_end_lag) +
                 total_active_proj_end +
                 IBRD +
                 reportyr +
                 PDO_Table +
                 Key_Matrix +
                 Comp_.Analysis,
               data=wb))

disb2_Fra<- (lm(disb_perc2 ~ avgPDO +
                  Outcome_Num +
                  p_obs_fra_end_5yr +
                  projsize_pop_st2 +      
                  projsize_pop_st2*p_obs_fra_end_5yr +
                  polity_end_lag +
                  CorruptScore_end +
                  log(rgdp_end_lag) +
                  log(pop_end_lag) +
                  total_active_proj_end +
                  IBRD +
                  reportyr +
                  PDO_Table +
                  Key_Matrix +
                  Comp_.Analysis +
                  as.factor(Country)+
                  as.factor(reportyr)-1,                 
                data=wb))

disb2_Ger<- (lm(disb_perc2 ~ avgPDO +
                  Outcome_Num +
                  p_obs_ger_end_5yr +
                  projsize_pop_st2 +      
                  projsize_pop_st2*p_obs_ger_end_5yr +
                  polity_end_lag +
                  CorruptScore_end +
                  log(rgdp_end_lag) +
                  log(pop_end_lag) +
                  total_active_proj_end +
                  IBRD +
                  reportyr +
                  PDO_Table +
                  Key_Matrix +
                  Comp_.Analysis +
                  as.factor(Country)+
                  as.factor(reportyr)-1,                 
                data=wb))

disb2_Jap<- (lm(disb_perc2 ~ avgPDO +
                  Outcome_Num +
                  p_obs_jap_end_5yr +
                  projsize_pop_st2 +      
                  projsize_pop_st2*p_obs_jap_end_5yr +
                  polity_end_lag +
                  CorruptScore_end +
                  log(rgdp_end_lag) +
                  log(pop_end_lag) +
                  total_active_proj_end +
                  IBRD +
                  reportyr +
                  PDO_Table +
                  Key_Matrix +
                  Comp_.Analysis +
                  as.factor(Country)+
                  as.factor(reportyr)-1,                 
                data=wb))

disb2_UK<- (lm(disb_perc2 ~ avgPDO +
                 Outcome_Num +
                 p_obs_uk_end_5yr +
                 projsize_pop_st2 +      
                 projsize_pop_st2*p_obs_uk_end_5yr +
                 polity_end_lag +
                 CorruptScore_end +
                 log(rgdp_end_lag) +
                 log(pop_end_lag) +
                 total_active_proj_end +
                 IBRD +
                 reportyr +
                 PDO_Table +
                 Key_Matrix +
                 Comp_.Analysis +
                 as.factor(Country)+
                 as.factor(reportyr)-1,                 
               data=wb))


########################################################
##### NOTE: Given the structure of the regressions and
# table generation, some manual re-labelling needs to be done
# to ensure the variables are labeled correctly. They are all
# generated correctly below but the interaction terms are not labeled 
# correctly automatically; this was fixed in the .tex file
# manually. 
########################################################
stargazer(disb2_US, disb2_Fra, disb2_Ger, disb2_Jap, disb2_UK,
          type="latex",
          title="Disbursement and Fortune 500 Investment",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement Proportion"),
          column.separate=c(5),
          covariate.labels=c("Performance", "Evaluation",                            
                             "US F500", "France F500",
                             "Germany F500", "Japan F500",
                             "UK F500",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corr. Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop.)$_{t-1}$",
                             "Num. active projects", 
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2", 
                             "US F500*Projsize",
                             "France F500*Projsize",
                             "Germany F500*Projsize",
                             "Japan F500*Projsize",
                             "UK F500*Projsize"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          #omit = c("Constant", "as.factor(Country)-1"),
          omit=c(20:110),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Figure 2 from the main text (same
# as Figure A2 in the online appendix)
projsize.seq1 <- seq(from=0, to=50, length=500)
int_invprojsize1 <- disb2_US$coefficients[3] + disb2_US$coefficients[111]*projsize.seq1
se_1 <- sqrt(vcov(disb2_US)[3,3] + projsize.seq1^2*vcov(disb2_US)["p_obs_usa_end_5yr:projsize_pop_st2","p_obs_usa_end_5yr:projsize_pop_st2"] + 2*projsize.seq1*vcov(disb2_US)["p_obs_usa_end_5yr","p_obs_usa_end_5yr:projsize_pop_st2"])
CI_int1_upper <- int_invprojsize1 + 1.96*(se_1)
CI_int1_lower <- int_invprojsize1 - 1.96*(se_1)
for_disbplot1 <- as.data.frame(cbind(projsize.seq1, int_invprojsize1, CI_int1_upper, CI_int1_lower))
plot1_gg <- ggplot(for_disbplot1) +
  geom_line(aes(x = projsize.seq1, y = int_invprojsize1), col = "black", lwd = 1) + 
  geom_ribbon(aes(x = projsize.seq1, ymin = CI_int1_lower, ymax = CI_int1_upper), fill = "dark gray", alpha = 0.3) + 
  geom_hline(yintercept = 0, lty = 2, lwd = 0.6, col = "black") + 
  scale_x_continuous("\n Projsize Per Capita", breaks=c(0, 10, 20, 30, 40, 50)) +
  scale_y_continuous("Effect of US Fortune 500 \n", breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold")) 
plot1_gg

### Note on Table 9 (and A9, which is the same): The information used to construct Table 9 
# was extracted at a much earlier stage of the dataset, before other variables were 
# merged in, and information on specific Contractor names and contracts 
# were not kept in the final version for a variety of reasons. Please contact 
# the authors for further information, if interested.

### The following code replicates Table 10 from the main text (and Tables
# A10 and A11 from the online appendix)

## Table 10/Table A10
disb.usaid2<- (lm(disb_perc2 ~ avgPDO +
                    Outcome_Num +
                    usdisb_bill_end_lag +
                    projsize_pop_st2 + 
                    polity_end_lag +
                    CorruptScore_end +
                    log(rgdp_end_lag) +
                    log(pop_end_lag) +
                    total_active_proj_end +
                    IBRD +
                    reportyr +
                    PDO_Table +
                    Key_Matrix +
                    Comp_.Analysis,
                  data=wb))

disb.sscore2 <-(lm(disb_perc2 ~ avgPDO +
                     Outcome_Num +
                     s3unus_end_lag + 
                     projsize_pop_st2 + 
                     polity_end_lag +
                     CorruptScore_end +
                     log(rgdp_end_lag) +
                     log(pop_end_lag) +
                     total_active_proj_end +
                     IBRD +
                     reportyr +
                     PDO_Table +
                     Key_Matrix +
                     Comp_.Analysis,
                   data=wb))

disb.sscore.imp2 <- (lm(disb_perc2 ~ avgPDO +
                          Outcome_Num +
                          s_imp_us_end_lag + 
                          projsize_pop_st2 + 
                          polity_end_lag +
                          CorruptScore_end +
                          log(rgdp_end_lag) +
                          log(pop_end_lag) +
                          total_active_proj_end +
                          IBRD +
                          reportyr +
                          PDO_Table +
                          Key_Matrix +
                          Comp_.Analysis,
                        data=wb))

disb.UNSCmem2 <-(lm(disb_perc2 ~ avgPDO +
                      Outcome_Num +
                      UNSCmem_end +
                      projsize_pop_st2 +      
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      total_active_proj_end +
                      IBRD+
                      reportyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis,                 
                    data=wb))

disb.edmem2 <-(lm(disb_perc2 ~ avgPDO +
                    Outcome_Num +
                    ed_ibrd_end +
                    projsize_pop_st2 +      
                    polity_end_lag +
                    CorruptScore_end +
                    log(rgdp_end_lag) +
                    log(pop_end_lag) +
                    total_active_proj_end +
                    IBRD+
                    reportyr +
                    PDO_Table +
                    Key_Matrix +
                    Comp_.Analysis,                 
                  data=wb))

stargazer(disb.usaid2, disb.sscore2, disb.sscore.imp2, disb.UNSCmem2, disb.edmem2,
          type="latex",
          title="Disbursement and Geopolitical Interests (No Fixed Effects)",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement Proportion"),
          column.separate=c(5),
          covariate.labels=c("Performance", "Evaluation",                            
                             "US Aid$_{t-1}$ (in billion USD)", "SScore$_{t-1}$", 
                             "SScore Imp.$_{t-1}$", "UNSC Membership",
                             "Executive Director",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corruption Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop.)$_{t-1}$",
                             "Num. active projects", 
                             "IBRD", "Report Year"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(17:20),
          keep.stat=c("n", "adj.rsq")
)

## Table A11
disb.usaid<- (lm(disb_perc2 ~ avgPDO +
                   Outcome_Num +
                   usdisb_bill_end_lag +
                   projsize_pop_st2 + 
                   polity_end_lag +
                   CorruptScore_end +
                   log(rgdp_end_lag) +
                   log(pop_end_lag) +
                   total_active_proj_end +
                   IBRD +
                   reportyr +
                   PDO_Table +
                   Key_Matrix +
                   Comp_.Analysis +
                   as.factor(Country)+
                   as.factor(reportyr)-1,
                 data=wb))

disb.sscore <-(lm(disb_perc2 ~ avgPDO +
                    Outcome_Num +
                    s3unus_end_lag + 
                    projsize_pop_st2 + 
                    polity_end_lag +
                    CorruptScore_end +
                    log(rgdp_end_lag) +
                    log(pop_end_lag) +
                    total_active_proj_end +
                    IBRD +
                    reportyr +
                    PDO_Table +
                    Key_Matrix +
                    Comp_.Analysis +
                    as.factor(Country)+
                    as.factor(reportyr)-1,                          
                  data=wb))

disb.sscore.imp<- (lm(disb_perc2 ~ avgPDO +
                        Outcome_Num +
                        s_imp_us_end_lag + 
                        projsize_pop_st2 + 
                        polity_end_lag +
                        CorruptScore_end +
                        log(rgdp_end_lag) +
                        log(pop_end_lag) +
                        total_active_proj_end +
                        IBRD +
                        reportyr +
                        PDO_Table +
                        Key_Matrix +
                        Comp_.Analysis +
                        as.factor(Country)+
                        as.factor(reportyr)-1,
                      data=wb))

disb.UNSCmem <-(lm(disb_perc2 ~ avgPDO +
                     Outcome_Num +
                     UNSCmem_end +
                     projsize_pop_st2 +      
                     polity_end_lag +
                     CorruptScore_end +
                     log(rgdp_end_lag) +
                     log(pop_end_lag) +
                     total_active_proj_end +
                     IBRD+
                     reportyr +
                     PDO_Table +
                     Key_Matrix +
                     Comp_.Analysis  +
                     as.factor(Country)+
                     as.factor(reportyr)-1,                 
                   data=wb))

disb.edmem <-(lm(disb_perc2 ~ avgPDO +
                   Outcome_Num +
                   ed_ibrd_end +
                   projsize_pop_st2 +      
                   polity_end_lag +
                   CorruptScore_end +
                   log(rgdp_end_lag) +
                   log(pop_end_lag) +
                   total_active_proj_end +
                   IBRD+
                   reportyr +
                   PDO_Table +
                   Key_Matrix +
                   Comp_.Analysis +
                   as.factor(Country)+
                   as.factor(reportyr)-1,                 
                 data=wb))

stargazer(disb.usaid, disb.sscore, disb.sscore.imp, disb.UNSCmem, disb.edmem,
          type="latex",
          title="Disbursement and Geopolitical Interests",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement Proportion"),
          column.separate=c(5),
          covariate.labels=c("Performance", "Evaluation",                            
                             "US Aid$_{t-1}$ (in billion USD)", "SScore$_{t-1}$", 
                             "SScore Imp.$_{t-1}$", "UNSC Membership",
                             "Executive Director",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corruption Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop.)$_{t-1}$",
                             "Num. active projects", 
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          #omit = c("Constant", "as.factor(Country)-1"),
          omit=c(16:125),
          keep.stat=c("n", "adj.rsq")
)

####################################################
### Additional Results from Online Appendix
####################################################

### The following code replicates Figure A3 from the online appendix
plot_disb <- ggplot(data=wbfull, aes(wbfull$disb_perc3)) + 
  geom_histogram(binwidth=0.05, col="black", fill="gray") +
  scale_x_continuous("Disbursement Proportion", breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_y_continuous("Count") +
  theme_bw() +  
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold")) 

plot_disb

### The following code replicates the results presented in Tables A12 and A13 
# of the online appendix.

OLS_calc <- function(x, y) {  
  return((solve(t(x)%*%x))%*%(t(x)%*%y))
}

wb_proc2 <- subset(wb_proc, !is.na(polity_end_lag) & !is.na(pop_end_lag) &
                     !is.na(rgdp_end_lag) & !is.na(CorruptScore_end) & 
                     !is.na(Any_Management) & !is.na(PDO_Table) & 
                     !is.na(Key_Matrix) & !is.na(Comp_.Analysis) & 
                     !is.na(Ratings_Obj_Ach) & !is.na(IBRD) &
                     !is.na(wbclosingyr) & !is.na(projsize_pop_st2) & 
                     !is.na(disb_perc3) & !is.na(Outcome_Num2) & !is.na(avgPDO))

y <- matrix(wb_proc2$disb_perc3, ncol=1)
x <- matrix(cbind(1,wb_proc2$avgPDO, wb_proc2$Outcome_Num2, wb_proc2$Any_Management,
                  wb_proc2$projsize_pop_st2, wb_proc2$polity_end_lag,
                  wb_proc2$CorruptScore_end, log(wb_proc2$rgdp_end_lag),
                  log(wb_proc2$pop_end_lag), wb_proc2$IBRD,
                  wb_proc2$wbclosingyr, wb_proc2$PDO_Table,
                  wb_proc2$Key_Matrix, wb_proc2$Comp_.Analysis),ncol=14)

# without country fixed effects (first column of Table A12)
Mboot=1000
bboot=matrix(0,Mboot,ncol(x))
for (i in 1:Mboot){
  d=cbind(y,x)
  dnew=d[sample(nrow(d),replace=T),]
  Ynew=dnew[,1]
  Xnew=dnew[,-1]
  ols=OLS_calc(Xnew,Ynew)
  bboot[i,] = matrix(ols,1,length(ols))
  print(i)
}
std_dev_bboot <- apply (bboot, 2, sd)
std_dev_bboot # gives new SEs (reported in Table A12; may differ marginally
# from the SEs in the paper due to the bootstrap method)

# original coeffs for comparison
compare_MNC1 <- lm(disb_perc3 ~ avgPDO +
                     Outcome_Num2 +
                     Any_Management +
                     projsize_pop_st2 +      
                     polity_end_lag +
                     CorruptScore_end +
                     log(rgdp_end_lag) +
                     log(pop_end_lag) +
                     IBRD +
                     wbclosingyr +
                     PDO_Table +
                     Key_Matrix +
                     Comp_.Analysis, data=wb_proc2)

# note that the following calculations were used to manually construct Table A12
MNC_noFE_coef <- compare_MNC1$coef
cbind(MNC_noFE_coef - 1.99*std_dev_bboot,
      MNC_noFE_coef + 1.99*std_dev_bboot)
cbind(MNC_noFE_coef - 1.96*std_dev_bboot,
      MNC_noFE_coef + 1.96*std_dev_bboot)
cbind(MNC_noFE_coef - 1.65*std_dev_bboot,
      MNC_noFE_coef + 1.65*std_dev_bboot)

# second column of Table A12 (with country-clustered SEs)
library(lmtest) ; library(sandwich)

# cluster bootstrap function
clusbootreg <- function(formula, data, cluster, reps=1000){
  reg1 <- lm(formula, data)
  clusters <- names(table(cluster))
  sterrs <- matrix(NA, nrow=reps, ncol=length(coef(reg1)))
  for(i in 1:reps){
    index <- sample(1:length(clusters), length(clusters), replace=TRUE)
    aa <- clusters[index]
    bb <- table(aa)
    bootdat <- NULL
    for(j in 1:max(bb)){
      cc <- data[cluster %in% names(bb[bb %in% j]),]
      for(k in 1:j){
        bootdat <- rbind(bootdat, cc)
      }
    }
    sterrs[i,] <- coef(lm(formula, bootdat))
    print(i)
  }
  val <- cbind(coef(reg1),apply(sterrs,2,sd)) 
  colnames(val) <- c("Estimate","Std. Error")
  return(val)
}

MNC.clusterdisb1 <- clusbootreg(formula=disb_perc3 ~ avgPDO +
                                  Outcome_Num2 +
                                  Any_Management +
                                  projsize_pop_st2 +      
                                  polity_end_lag +
                                  CorruptScore_end +
                                  log(rgdp_end_lag) +
                                  log(pop_end_lag) +
                                  IBRD +
                                  wbclosingyr +
                                  PDO_Table +
                                  Key_Matrix +
                                  Comp_.Analysis,
                                data=wb_proc2, cluster=wb_proc2$Country, 
                                reps=1000) 
# note that the following calculations were used to manually construct Table A12
coefs.MNCcluster <- MNC.clusterdisb1[,1]
std_err.MNCcluster <- MNC.clusterdisb1[,2]
std_err.MNCcluster
CI_L_MNCcluster1 <- coefs.MNCcluster - 1.99*std_err.MNCcluster
CI_U_MNCcluster1 <- coefs.MNCcluster + 1.99*std_err.MNCcluster
cbind(CI_L_MNCcluster1,CI_U_MNCcluster1)
CI_L_MNCcluster2 <- coefs.MNCcluster - 1.96*std_err.MNCcluster
CI_U_MNCcluster2 <- coefs.MNCcluster + 1.96*std_err.MNCcluster
cbind(CI_L_MNCcluster2,CI_U_MNCcluster2)
CI_L_MNCcluster3 <- coefs.MNCcluster - 1.65*std_err.MNCcluster
CI_U_MNCcluster3 <- coefs.MNCcluster + 1.65*std_err.MNCcluster
cbind(CI_L_MNCcluster3,CI_U_MNCcluster3)

### Table A13 (make sure the OLS_calc and clustbootreg functions above
# have been run before running the code below)
y <- matrix(wb_proc2$disb_perc3, ncol=1)
x <- matrix(cbind(1,wb_proc2$avgPDO, wb_proc2$Outcome_Num2, wb_proc2$US_PT_Management,
                  wb_proc2$projsize_pop_st2, wb_proc2$polity_end_lag,
                  wb_proc2$CorruptScore_end, log(wb_proc2$rgdp_end_lag),
                  log(wb_proc2$pop_end_lag), wb_proc2$IBRD,
                  wb_proc2$wbclosingyr, wb_proc2$PDO_Table,
                  wb_proc2$Key_Matrix, wb_proc2$Comp_.Analysis),ncol=14)

# Table A13, first column (without Country FE)
Mboot=1000
bboot=matrix(0,Mboot,ncol(x))
for (i in 1:Mboot){
  d=cbind(y,x)
  dnew=d[sample(nrow(d),replace=T),]
  Ynew=dnew[,1]
  Xnew=dnew[,-1]
  ols=OLS_calc(Xnew,Ynew)
  bboot[i,] = matrix(ols,1,length(ols))
  print(i)
}

#### take std deviation of each row
std_dev_bbootUS <- apply (bboot, 2, sd)
std_dev_bbootUS # gives new SEs

# original coeffs for using with new SEs
compare_MNC_US <- lm(disb_perc3 ~ avgPDO +
                       Outcome_Num2 +
                       US_PT_Management +
                       projsize_pop_st2 +      
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis, data=wb_proc2)

# as above, the following code is used to calculate new CIs and then these
# were put in to Table A13 manually
MNC_noFE_coefUS <- compare_MNC_US$coef
cbind(MNC_noFE_coefUS - 1.99*std_dev_bbootUS,
      MNC_noFE_coefUS + 1.99*std_dev_bbootUS)
cbind(MNC_noFE_coefUS - 1.96*std_dev_bbootUS,
      MNC_noFE_coefUS + 1.96*std_dev_bbootUS)
cbind(MNC_noFE_coefUS - 1.65*std_dev_bbootUS,
      MNC_noFE_coefUS + 1.65*std_dev_bbootUS)

# Table A13, second column (with country-clustered SEs)
MNC.clusterdisbUS <- clusbootreg(formula=disb_perc3 ~ avgPDO +
                                   Outcome_Num2 +
                                   US_PT_Management +
                                   projsize_pop_st2 +      
                                   polity_end_lag +
                                   CorruptScore_end +
                                   log(rgdp_end_lag) +
                                   log(pop_end_lag) +
                                   IBRD +
                                   wbclosingyr +
                                   PDO_Table +
                                   Key_Matrix +
                                   Comp_.Analysis,
                                 data=wb_proc2, cluster=wb_proc2$Country, 
                                 reps=1000) 

# as above, the following code is used to calculate new CIs and then these
# were put in to Table A13 manually
coefs.MNCclusterUS <- MNC.clusterdisbUS[,1] 
std_err.MNCclusterUS <- MNC.clusterdisbUS[,2]
std_err.MNCclusterUS # new SEs
CI_L_MNCclusterUS <- coefs.MNCclusterUS - 1.99*std_err.MNCclusterUS
CI_U_MNCclusterUS <- coefs.MNCclusterUS + 1.99*std_err.MNCclusterUS
cbind(CI_L_MNCclusterUS,CI_U_MNCclusterUS)
CI_L_MNCcluster2US <- coefs.MNCclusterUS - 1.96*std_err.MNCclusterUS
CI_U_MNCcluster2US <- coefs.MNCclusterUS + 1.96*std_err.MNCclusterUS
cbind(CI_L_MNCcluster2US,CI_U_MNCcluster2US)
CI_L_MNCcluster3US <- coefs.MNCclusterUS - 1.65*std_err.MNCclusterUS
CI_U_MNCcluster3US <- coefs.MNCclusterUS + 1.65*std_err.MNCclusterUS
cbind(CI_L_MNCcluster3US,CI_U_MNCcluster3US)

### The following code replicates the results shown in Table A14 
# of the online appendix; note that the table itself was constructed
# manually, using output produced in the following four regressions (one
# for each column of Table A14)

flogMNC <- subset(wbfull, !is.na(disb_perc3) & !is.na(avgPDO) &
                    !is.na(MNC_any) & !is.na(projsize_pop_st2) &
                    !is.na(polity_end_lag) & !is.na(CorruptScore_end) &
                    !is.na(rgdp_end_lag) & !is.na(pop_end_lag) & 
                    !is.na(IBRD) & !is.na(wbclosingyr))
MNC_reg1 <- cbind(flogMNC$avgPDO, flogMNC$MNC_any, flogMNC$projsize_pop_st2, flogMNC$polity_end_lag,
                  flogMNC$CorruptScore_end, log(flogMNC$rgdp_end_lag), log(flogMNC$pop_end_lag),
                  flogMNC$IBRD, flogMNC$wbclosingyr, flogMNC$PDO_Table, flogMNC$Key_Matrix, 
                  flogMNC$Comp_.Analysis)
colnames(MNC_reg1) <- c("Performance", "Any MNC", "Project Size pc", "Polity", 
                        "Corruption Control", "log GDP pc", "log Pop", "IBRD",
                        "Report Yr", "Report Type 1", "Report Type 2", "Report Type 3")

disb1_frac <- frm(flogMNC$disb_perc3, 
                  MNC_reg1,
                  linkfrac="logit")

MNC_reg2 <- cbind(flogMNC$avgPDO, flogMNC$MNC_Inv, flogMNC$projsize_pop_st2, flogMNC$polity_end_lag,
                  flogMNC$CorruptScore_end, log(flogMNC$rgdp_end_lag), log(flogMNC$pop_end_lag),
                  flogMNC$IBRD, flogMNC$wbclosingyr, flogMNC$PDO_Table, flogMNC$Key_Matrix, 
                  flogMNC$Comp_.Analysis)
colnames(MNC_reg2) <- c("Performance", "US MNC", "Project Size pc", "Polity", 
                        "Corruption Control", "log GDP pc", "log Pop", "IBRD",
                        "Report Yr", "Report Type 1", "Report Type 2", "Report Type 3")
disb2_frac <- frm(flogMNC$disb_perc3, 
                  MNC_reg2,
                  linkfrac="logit")

flogMNC_proc <- subset(wb_proc, !is.na(disb_perc3) & !is.na(avgPDO) &
                         !is.na(US_PT_Management) & !is.na(projsize_pop_st2) &
                         !is.na(polity_end_lag) & !is.na(CorruptScore_end) &
                         !is.na(rgdp_end_lag) & !is.na(pop_end_lag) & 
                         !is.na(IBRD) & !is.na(wbclosingyr))
MNC_reg_proc <- cbind(flogMNC_proc$avgPDO, flogMNC_proc$Any_Management, 
                       flogMNC_proc$projsize_pop_st2, flogMNC_proc$polity_end_lag,
                       flogMNC_proc$CorruptScore_end, log(flogMNC_proc$rgdp_end_lag), 
                       log(flogMNC_proc$pop_end_lag),
                       flogMNC_proc$IBRD, flogMNC_proc$wbclosingyr, flogMNC_proc$PDO_Table, 
                       flogMNC_proc$Key_Matrix, 
                       flogMNC_proc$Comp_.Analysis)
colnames(MNC_reg_proc) <- c("Performance", "Any Management", "Project Size pc", "Polity", 
                             "Corruption Control", "log GDP pc", "log Pop", "IBRD",
                             "Report Yr", "Report Type 1", "Report Type 2", "Report Type 3")
disb3_frac <- frm(flogMNC_proc$disb_perc3, 
                  MNC_reg_proc,
                  linkfrac="logit")

MNC_reg_proc2 <- cbind(flogMNC_proc$avgPDO, flogMNC_proc$US_PT_Management, 
                      flogMNC_proc$projsize_pop_st2, flogMNC_proc$polity_end_lag,
                      flogMNC_proc$CorruptScore_end, log(flogMNC_proc$rgdp_end_lag), log(flogMNC_proc$pop_end_lag),
                      flogMNC_proc$IBRD, flogMNC_proc$wbclosingyr, flogMNC_proc$PDO_Table, flogMNC_proc$Key_Matrix, 
                      flogMNC_proc$Comp_.Analysis)
colnames(MNC_reg_proc2) <- c("Performance", "US Management", "Project Size pc", "Polity", 
                            "Corruption Control", "log GDP pc", "log Pop", "IBRD",
                            "Report Yr", "Report Type 1", "Report Type 2", "Report Type 3")
disb4_frac <- frm(flogMNC_proc$disb_perc3, 
                  MNC_reg_proc2,
                  linkfrac="logit")


### The following code replicates Table A15 from the online appendix
for_comm <- subset(wbfull, select=c(4,22,30))
wb_MNC <- merge(wb, for_comm, by="projID")

disb_CommMNC <- (lm(disb_perc2 ~ avgPDO +
                      Outcome_Num +
                      USD_Comm_begin +  
                      MNC_any +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      total_active_proj_end +
                      IBRD +
                      reportyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(Country)+
                      as.factor(reportyr)-1,                 
                    data=wb_MNC))

disb_CommMNC_US <- (lm(disb_perc2 ~ avgPDO +
                         Outcome_Num +
                         USD_Comm_begin +  
                         MNC_Inv +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag) +
                         log(pop_end_lag) +
                         total_active_proj_end +
                         IBRD +
                         reportyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country)+
                         as.factor(reportyr)-1,                 
                       data=wb_MNC))

stargazer(disb_CommMNC, disb_CommMNC_US,
          type="latex",
          title="Disbursement, MNC Contractors, and Project Size",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement proportion"),
          column.separate=c(2),
          covariate.labels=c("Performance", "Evaluation", 
                             "Project Commitment (USD mill.)",
                             "Any MNC", "US MNC",
                             "Polity$_{t-1}$", "Control of Corruption",
                             "Log(GDP per capita)$_{t-1}$",
                             "Log(Population)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(12:110),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Figure A4 from the online appendix
projectsize <- subset(wb, projsize_pop_st2<=50) # dropping outliers, since results are plotted for
                                         # this range in all regressions as well

plot_psize <- ggplot(data=projectsize, aes(projectsize$projsize_pop_st2)) + 
  geom_histogram(binwidth=5, col="black", fill="gray") +
  scale_x_continuous("Project Size Per Capita (USD)", 
                     breaks=c(0,10,20,30,40,50)) +
  scale_y_continuous("Count") +
  theme_bw() +  
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold")) 
plot_psize

### The following code replicates the results presented in Table A16
# in the online appendix
wb2 <- subset(wb, !is.na(polity_end_lag) & !is.na(pop_end_lag) &
                     !is.na(rgdp_end_lag) & !is.na(CorruptScore_end) & 
                     !is.na(p_obs_usa_end_5yr) & !is.na(PDO_Table) & 
                     !is.na(Key_Matrix) & !is.na(Comp_.Analysis) & 
                     !is.na(Ratings_Obj_Ach) & !is.na(IBRD) &
                     !is.na(reportyr) & !is.na(projsize_pop_st2) & 
                     !is.na(total_active_proj_end) & !is.na(disb_perc2) & 
                     !is.na(Outcome_Num) & !is.na(avgPDO))
# standardizing measure for project size
wb2$projsize_st <- (wb2$projsize_pop_st2 - 
                      mean(wb2$projsize_pop_st2))/sd(wb2$projsize_pop_st2)
disb_standard <- (lm(disb_perc2 ~ avgPDO +
                       Outcome_Num +
                       p_obs_usa_end_5yr +
                       projsize_st +      
                       projsize_st*p_obs_usa_end_5yr +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       total_active_proj_end +
                       IBRD +
                       reportyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country) +
                       as.factor(reportyr)-1,                 
                     data=wb2))
# note that, similar to the models above with interaction terms, given the placement
# of the interaction coefficient in stargazer's output, some of the "extra" rows
# that stargazer outputs were manually deleted when inserting the table in the paper.
stargazer(disb_standard,
          type="latex",
          title="Disbursement, US Fortune 500, and Standardized Project Size",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement proportion"),
          column.separate=c(1),
          covariate.labels=c("Performance", "Evaluation",
                             "US F500",
                             "Standard. projsize",
                             "Polity$_{t-1}$",                              
                             "Control of Corruption",
                             "Log(GDP per capita)$_{t-1}$",
                             "Log(Population)$_{t-1}$",
                             "Num. Active Proj.",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(12:105),
          keep.stat=c("n", "adj.rsq")
)

# figure associated with above model, to show that the standardized project size per
# capita variable is significant for the relevant range. Note that this figure is 
# NOT included in the online appendix.
projsize.seq2 <- seq(from=-1, to=15, length=200)
int_invprojsize2 <- disb_standard$coefficients[3] + disb_standard$coefficients[111]*projsize.seq2
se_2 <- sqrt(vcov(disb_standard)[3,3] + 
               projsize.seq2^2*vcov(disb_standard)["p_obs_usa_end_5yr:projsize_st","p_obs_usa_end_5yr:projsize_st"] + 
               2*projsize.seq2*vcov(disb_standard)["p_obs_usa_end_5yr","p_obs_usa_end_5yr:projsize_st"])
CI_int_2_upper <- int_invprojsize2 + 1.96*(se_2)
CI_int_2_lower <- int_invprojsize2 - 1.96*(se_2)
for_disbplot_2 <- as.data.frame(cbind(projsize.seq2, 
                                      int_invprojsize2, 
                                      CI_int_2_upper, CI_int_2_lower))
plot1_gg_2 <- ggplot(for_disbplot_2) +
  geom_line(aes(x = projsize.seq2, y = int_invprojsize2), col = "black", lwd = 1) + 
  geom_ribbon(aes(x =projsize.seq2, ymin = CI_int_2_lower, ymax = CI_int_2_upper), 
              fill = "dark gray", alpha = 0.3) + 
  geom_hline(yintercept = 0, lty = 2, lwd = 0.6, col = "black") +
  scale_x_continuous("\n Projsize Per Capita", breaks=c(0,5,10,15,20)) +
  scale_y_continuous("Effect of US Fortune 500 \n", 
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold"))
plot1_gg_2

### The following code replicates Figure A5 in the online appendix
for_corr <- data.frame(cbind(wbfull$disb_perc3,
                             wbfull$Outcome_Num2,
                             wbfull$avgPDO,
                             wbfull$MNC_any,
                             wbfull$MNC_Inv,
                             wbfull$Fra_MNC,
                             wbfull$Ger_MNC,
                             wbfull$Japan_MNC,
                             wbfull$UK_MNC,
                             wbfull$p_obs_usa_end_5yr,
                             wbfull$projsize_pop_st2))
colnames(for_corr) <- c("Disb.", "Eval.", "Perf.",
                        "Any MNC", "US MNC", "France MNC", "Germany MNC", "Japan MNC", 
                        "UK MNC", "US F500", "ProjSize pc")
correlations <- cor(for_corr, use="pairwise.complete.obs")
cor_color <- c("black")
corrplot(correlations, method="number",
         type="lower", col=cor_color,
         tl.col="black", tl.cex=0.4,
         number.cex=0.45,
         tl.pos="ld",
         number.digits=3,
         cl.pos="n",
         number.font=1)

### The following code replicates Table A17 from the online appendix
mod.o2_any <- (lm(Outcome_Num2 ~ avgPDO +
                    MNC_any +
                    polity_end_lag +
                    CorruptScore_end +
                    log(rgdp_end_lag) +
                    log(pop_end_lag) +
                    projsize_pop_st2 +
                    IBRD +
                    wbclosingyr +
                    PDO_Table +
                    Key_Matrix +
                    Comp_.Analysis +
                    as.factor(Country) +
                    as.factor(wbclosingyr) -1,
                  data=wbfull))

mod.o2_anyUS <- (lm(Outcome_Num2 ~ avgPDO +
                      MNC_Inv +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      projsize_pop_st2 +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(Country) +
                      as.factor(wbclosingyr)-1,
                    data=wbfull))

mod.o2_anyJap <- (lm(Outcome_Num2 ~ avgPDO +
                       Japan_MNC +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country) +
                       as.factor(wbclosingyr)-1,
                     data=wbfull))

mod.o2_anyUK <- (lm(Outcome_Num2 ~ avgPDO +
                      UK_MNC +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      projsize_pop_st2 +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(Country) +
                      as.factor(wbclosingyr)-1,
                    data=wbfull))

mod.o2_anyGer <- (lm(Outcome_Num2 ~ avgPDO +
                       Ger_MNC +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country) +
                       as.factor(wbclosingyr)-1,
                     data=wbfull))

mod.o2_anyFra <- (lm(Outcome_Num2 ~ avgPDO +
                       Fra_MNC +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country) +
                       as.factor(wbclosingyr)-1,
                     data=wbfull))

stargazer(mod.o2_any, mod.o2_anyUS, mod.o2_anyFra, 
          mod.o2_anyGer, mod.o2_anyJap, mod.o2_anyUK,
          type="latex",
          title="Project Evaluation and MNC Contractors",
          style="ajps",
          summary=TRUE,
          column.labels=c("Evaluation"),
          column.separate=c(6),
          covariate.labels=c("Performance", 
                             "Any MNC", "US MNC",
                             "France MNC", "Germany MNC",
                             "Japan MNC", "UK MNC",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corr.Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(15:113),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table A18 from the online appendix
mod.o3_any <- (lm(Outcome_Num2 ~ avgPDO +
                    MNC_any +
                    polity_end_lag +
                    CorruptScore_end +
                    log(rgdp_end_lag) +
                    log(pop_end_lag) +
                    projsize_pop_st2 +
                    IBRD +
                    wbclosingyr +
                    PDO_Table +
                    Key_Matrix +
                    Comp_.Analysis +
                    as.factor(Country)-1,
                  data=wbfull))

mod.o3_anyUS <- (lm(Outcome_Num2 ~ avgPDO +
                      MNC_Inv +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      projsize_pop_st2 +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(Country)-1,
                    data=wbfull))

mod.o3_anyFra <- (lm(Outcome_Num2 ~ avgPDO +
                       Fra_MNC +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country)-1,
                     data=wbfull))

mod.o3_anyGer <- (lm(Outcome_Num2 ~ avgPDO +
                       Ger_MNC +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country)-1,
                     data=wbfull))

mod.o3_anyJap <- (lm(Outcome_Num2 ~ avgPDO +
                       Japan_MNC +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country)-1,
                     data=wbfull))

mod.o3_anyUK <- (lm(Outcome_Num2 ~ avgPDO +
                      UK_MNC +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      projsize_pop_st2 +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(Country)-1,
                    data=wbfull))

stargazer(mod.o3_any, mod.o3_anyUS, mod.o3_anyFra, 
          mod.o3_anyGer, mod.o3_anyJap, mod.o3_anyUK,
          type="latex",
          title="Project Evaluation and MNC Contractors (Country FE only)",
          style="ajps",
          summary=TRUE,
          column.labels=c("Evaluation"),
          column.separate=c(6),
          covariate.labels=c("Performance", 
                             "Any MNC", "US MNC",
                             "France MNC", "Germany MNC",
                             "Japan MNC", "UK MNC",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corr.Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(15:109),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table A19 in the online appendix
mod.o4_any <- (lm(Outcome_Num2 ~ avgPDO +
                    MNC_any +
                    polity_end_lag +
                    CorruptScore_end +
                    log(rgdp_end_lag) +
                    log(pop_end_lag) +
                    projsize_pop_st2 +
                    IBRD +
                    wbclosingyr +
                    PDO_Table +
                    Key_Matrix +
                    Comp_.Analysis +
                    as.factor(wbclosingyr)-1,
                  data=wbfull))

mod.o4_anyUS <- (lm(Outcome_Num2 ~ avgPDO +
                      MNC_Inv +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      projsize_pop_st2 +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(wbclosingyr)-1,
                    data=wbfull))

mod.o4_anyFra <- (lm(Outcome_Num2 ~ avgPDO +
                       Fra_MNC +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(wbclosingyr)-1,
                     data=wbfull))

mod.o4_anyGer <- (lm(Outcome_Num2 ~ avgPDO +
                       Ger_MNC +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(wbclosingyr)-1,
                     data=wbfull))

mod.o4_anyJap <- (lm(Outcome_Num2 ~ avgPDO +
                       Japan_MNC +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(wbclosingyr)-1,
                     data=wbfull))

mod.o4_anyUK <- (lm(Outcome_Num2 ~ avgPDO +
                      UK_MNC +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      projsize_pop_st2 +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(wbclosingyr)-1,
                    data=wbfull))

stargazer(mod.o4_any, mod.o4_anyUS, mod.o4_anyFra, 
          mod.o4_anyGer, mod.o4_anyJap, mod.o4_anyUK,
          type="latex",
          title="Project Evaluation and MNC Contractors (Year FE only)",
          style="ajps",
          summary=TRUE,
          column.labels=c("Evaluation"),
          column.separate=c(6),
          covariate.labels=c("Performance", 
                             "Any MNC", "US MNC",
                             "France MNC", "Germany MNC",
                             "Japan MNC", "UK MNC",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corr.Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(15:34),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table A20 from the online appendix
eb.MNC.op1 <- polr(as.factor(Outcome_Num2) ~ avgPDO + MNC_any + polity_end_lag +
                     CorruptScore_end + log(rgdp_end_lag) + log(pop_end_lag) +
                     projsize_pop_st2 + IBRD + wbclosingyr + PDO_Table +
                     Key_Matrix + Comp_.Analysis,
                   data=wbfull, method=c("probit"),
                   Hess=TRUE)

# Note that the following specification does not converge (as mentioned in Table A20):
eb.MNC.op2 <- polr(as.factor(Outcome_Num2) ~ avgPDO + MNC_any + polity_end_lag +
                     CorruptScore_end + log(rgdp_end_lag) + log(pop_end_lag) +
                     projsize_pop_st2 + IBRD + wbclosingyr + PDO_Table +
                     Key_Matrix + Comp_.Analysis + as.factor(wbclosingyr),
                   data=wbfull, method=c("probit"),
                   Hess=TRUE)

eb.MNC.op3 <- polr(as.factor(Outcome_Num2) ~ avgPDO + MNC_Inv + polity_end_lag +
                     CorruptScore_end + log(rgdp_end_lag) + log(pop_end_lag) +
                     projsize_pop_st2 + IBRD + wbclosingyr + PDO_Table +
                     Key_Matrix + Comp_.Analysis,
                   data=wbfull, method=c("probit"),
                   Hess=TRUE)

eb.MNC.op4 <- polr(as.factor(Outcome_Num2) ~ avgPDO + MNC_Inv + polity_end_lag +
                     CorruptScore_end + log(rgdp_end_lag) + log(pop_end_lag) +
                     projsize_pop_st2 + IBRD + wbclosingyr + PDO_Table +
                     Key_Matrix + Comp_.Analysis + as.factor(wbclosingyr),
                   data=wbfull, method=c("probit"),
                   Hess=TRUE)

stargazer(eb.MNC.op1, eb.MNC.op3, 
          type="latex",
          title="Project Outcomes and Ordered Probit - MNC Contractor",
          style="ajps",
          summary=TRUE,
          column.labels=c("World Bank Outcome"),
          column.separate=c(2),
          covariate.labels=c("Performance", 
                             "Any MNC", 
                             "US MNC",
                             "Polity$_{t-1}$", 
                             "Control of Corruption",
                             "Log(GDP per capita)$_{t-1}$",
                             "Log(Population)$_{t-1}$",
                             "Project Size per cap.",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(10:12),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table A21 from the online appendix
eb.MNC.op5 <- polr(as.factor(Outcome_Num2) ~ avgPDO + Any_Management + polity_end_lag +
                     CorruptScore_end + log(rgdp_end_lag) + log(pop_end_lag) +
                     projsize_pop_st2 + IBRD + wbclosingyr + PDO_Table +
                     Key_Matrix + Comp_.Analysis,
                   data=wb_proc, method=c("probit"),
                   Hess=TRUE)

# Note that the following specification does not converge (as mentioned in Table A21 also):
eb.MNC.op6 <- polr(as.factor(Outcome_Num2) ~ avgPDO + Any_Management + polity_end_lag +
                     CorruptScore_end + log(rgdp_end_lag) + log(pop_end_lag) +
                     projsize_pop_st2 + IBRD + wbclosingyr + PDO_Table +
                     Key_Matrix + Comp_.Analysis + as.factor(Country),
                   data=wb_proc, method=c("probit"),
                   Hess=TRUE)

eb.MNC.op7 <- polr(as.factor(Outcome_Num2) ~ avgPDO + US_PT_Management + polity_end_lag +
                     CorruptScore_end + log(rgdp_end_lag) + log(pop_end_lag) +
                     projsize_pop_st2 + IBRD + wbclosingyr + PDO_Table +
                     Key_Matrix + Comp_.Analysis,
                   data=wb_proc, method=c("probit"),
                   Hess=TRUE)

# Note that the following specification does not converge (as mentioned in Table A21 also)
eb.MNC.op8 <- polr(as.factor(Outcome_Num2) ~ avgPDO + US_PT_Management + polity_end_lag +
                     CorruptScore_end + log(rgdp_end_lag) + log(pop_end_lag) +
                     projsize_pop_st2 + IBRD + wbclosingyr + PDO_Table +
                     Key_Matrix + Comp_.Analysis + as.factor(Country),
                   data=wb_proc, method=c("probit"),
                   Hess=TRUE)

stargazer(eb.MNC.op5, eb.MNC.op7, type="latex",
          title="Project Outcomes and Ordered Probit - MNC Management Contractors",
          style="ajps",
          summary=TRUE,
          column.labels=c("World Bank Outcome"),
          column.separate=c(3),
          covariate.labels=c("Performance", 
                             "Any Management", 
                             "US Management",
                             "Polity$_{t-1}$", 
                             "Control of Corruption",
                             "Log(GDP per capita)$_{t-1}$",
                             "Log(Population)$_{t-1}$",
                             "Project Size per cap.",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(11:16),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table A22 from the online appendix
mod.pdo2_any <- (lm(avgPDO ~ MNC_any +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      projsize_pop_st2 +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      as.factor(Country) +
                      as.factor(wbclosingyr) -1,
                    data=wbfull))

mod.pdo2_anyUS <- (lm(avgPDO ~ MNC_Inv +
                        polity_end_lag +
                        CorruptScore_end +
                        log(rgdp_end_lag) +
                        log(pop_end_lag) +
                        projsize_pop_st2 +
                        IBRD +
                        wbclosingyr +
                        PDO_Table +
                        Key_Matrix +
                        Comp_.Analysis +
                        as.factor(Country) +
                        as.factor(wbclosingyr)-1,
                      data=wbfull))

mod.pdo2_anyJap <- (lm(avgPDO ~ Japan_MNC +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag) +
                         log(pop_end_lag) +
                         projsize_pop_st2 +
                         IBRD +
                         wbclosingyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country) +
                         as.factor(wbclosingyr)-1,
                       data=wbfull))

mod.pdo2_anyUK <- (lm(avgPDO ~ UK_MNC +
                        polity_end_lag +
                        CorruptScore_end +
                        log(rgdp_end_lag) +
                        log(pop_end_lag) +
                        projsize_pop_st2 +
                        IBRD +
                        wbclosingyr +
                        PDO_Table +
                        Key_Matrix +
                        Comp_.Analysis +
                        as.factor(Country) +
                        as.factor(wbclosingyr)-1,
                      data=wbfull))

mod.pdo2_anyGer <- (lm(avgPDO ~ Ger_MNC +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag) +
                         log(pop_end_lag) +
                         projsize_pop_st2 +
                         IBRD +
                         wbclosingyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country) +
                         as.factor(wbclosingyr)-1,
                       data=wbfull))

mod.pdo2_anyFra <- (lm(avgPDO ~ Fra_MNC +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag) +
                         log(pop_end_lag) +
                         projsize_pop_st2 +
                         IBRD +
                         wbclosingyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country) +
                         as.factor(wbclosingyr)-1,
                       data=wbfull))

stargazer(mod.pdo2_any, mod.pdo2_anyUS, mod.pdo2_anyFra, 
          mod.pdo2_anyGer, mod.pdo2_anyJap, mod.pdo2_anyUK,
          type="latex",
          title="Project Performance and MNC Contractors",
          style="ajps",
          summary=TRUE,
          column.labels=c("Performance"),
          column.separate=c(6),
          covariate.labels=c("Any MNC", "US MNC",
                             "France MNC", "Germany MNC",
                             "Japan MNC", "UK MNC",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corr. Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          #omit = c("Constant", "as.factor(Country)-1"),
          omit=c(14:112),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table A23 from the online appendix
disb.sectors <- (lm(disb_perc2 ~ avgPDO +
                      Outcome_Num +
                      p_obs_usa_end_5yr +
                      projsize_pop_st2 +      
                      projsize_pop_st2*p_obs_usa_end_5yr +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      total_active_proj_end +
                      IBRD +
                      reportyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis +
                      Agriculture +
                      P_Admin +
                      Info +
                      Education +
                      Finance +
                      Health +
                      Power +
                      Transport +
                      Water_San +
                      Industry +
                      Envir +
                      Labor +
                      as.factor(Country)+
                      as.factor(reportyr),                 
                    data=wb))
summary(disb.sectors)

## Note: similar to before, because of the interaction term, some of the 
# stargazer output labels are not correct, and were fixed manually
# before putting the table in the appendix; see Table A23 for details
stargazer(disb.sectors, type="latex",
          title="Project Disbursement, US Fortune 500 and Sectors",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement proportion"),
          column.separate=c(1),
          covariate.labels=c("Performance", "Evaluation",                            
                             "US Fortune 500", "Project Size per capita",
                             "Polity$_{t-1}$", "Control of Corruption",
                             "Log(GDP per capita)$_{t-1}$",
                             "Log(Population)$_{t-1}$",
                             "Num. active projects", 
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2", "Agriculture", "Public Admin.",
                             "Information", "Education", "Finance", "Health",
                             "Power", "Transport", "Water and San.", "Industry",
                             "Environment", "Labor",
                             "Fortune 500*Projsize"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(25:105),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Figure A6 from the online appendix. 
# running same regression as above to get coefficients for the plot
disb2_Fra<- (lm(disb_perc2 ~ avgPDO +
                  Outcome_Num +
                  p_obs_fra_end_5yr +
                  projsize_pop_st2 +      
                  projsize_pop_st2*p_obs_fra_end_5yr +
                  polity_end_lag +
                  CorruptScore_end +
                  log(rgdp_end_lag) +
                  log(pop_end_lag) +
                  total_active_proj_end +
                  IBRD +
                  reportyr +
                  PDO_Table +
                  Key_Matrix +
                  Comp_.Analysis +
                  as.factor(Country)+
                  as.factor(reportyr)-1,                 
                data=wb))

projsize.seq2 <- seq(from=0, to=50, length=500)
int_invprojsize2 <- disb2_Fra$coefficients[3] + disb2_Fra$coefficients[111]*projsize.seq2
se_2 <- sqrt(vcov(disb2_Fra)[3,3] + 
               projsize.seq2^2*vcov(disb2_Fra)[110,110] + 
               2*projsize.seq2*vcov(disb2_Fra)[3,110])
CI_int2_upper <- int_invprojsize2 + 1.96*(se_2)
CI_int2_lower <- int_invprojsize2 - 1.96*(se_2)
for_disbplot2 <- as.data.frame(cbind(projsize.seq2, int_invprojsize2, 
                                     CI_int2_upper, CI_int2_lower))
plot2_gg <- ggplot(for_disbplot2) +
  geom_line(aes(x = projsize.seq2, y = int_invprojsize2), col = "black", lwd = 1) + 
  geom_ribbon(aes(x = projsize.seq2, ymin = CI_int2_lower, ymax = CI_int2_upper), 
              fill = "dark gray", alpha = 0.3) + 
  geom_hline(yintercept = 0, lty = 2, lwd = 0.6, col = "black") + 
  scale_x_continuous("\n Projsize Per Capita", breaks=c(0, 10, 20, 30, 40, 50)) +
  scale_y_continuous("Effect of France Fortune 500 \n", 
                     breaks = seq(0,0.5,0.1)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold")) 
plot2_gg

### The following code replicates Figure A7 from the online appendix
disb2_Jap<- (lm(disb_perc2 ~ avgPDO +
                  Outcome_Num +
                  p_obs_jap_end_5yr +
                  projsize_pop_st2 +      
                  projsize_pop_st2*p_obs_jap_end_5yr +
                  polity_end_lag +
                  CorruptScore_end +
                  log(rgdp_end_lag) +
                  log(pop_end_lag) +
                  total_active_proj_end +
                  IBRD +
                  reportyr +
                  PDO_Table +
                  Key_Matrix +
                  Comp_.Analysis +
                  as.factor(Country)+
                  as.factor(reportyr)-1,                 
                data=wb))
projsize.seq3 <- seq(from=0, to=50, length=500)
int_invprojsize3 <- disb2_Jap$coefficients[3] + 
  disb2_Jap$coefficients[111]*projsize.seq3
se_3 <- sqrt(vcov(disb2_Jap)[3,3] + 
               projsize.seq3^2*vcov(disb2_Jap)[110,110] + 
               2*projsize.seq3*vcov(disb2_Jap)[3,110])
CI_int3_upper <- int_invprojsize3 + 1.96*(se_3)
CI_int3_lower <- int_invprojsize3 - 1.96*(se_3)
for_disbplot3 <- as.data.frame(cbind(projsize.seq3, int_invprojsize3, 
                                     CI_int3_upper, CI_int3_lower))
plot3_gg <- ggplot(for_disbplot3) +
  geom_line(aes(x = projsize.seq3, y = int_invprojsize3), col = "black", lwd = 1) + 
  geom_ribbon(aes(x = projsize.seq3, ymin = CI_int3_lower, ymax = CI_int3_upper), 
              fill = "dark gray", alpha = 0.3) + 
  geom_hline(yintercept = 0, lty = 2, lwd = 0.6, col = "black") + 
  scale_x_continuous("\n Projsize Per Capita", breaks=c(0, 10, 20, 30, 40, 50)) +
  scale_y_continuous("Effect of Japan Fortune 500 \n", 
                     breaks = seq(0,1,0.2)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold")) 
plot3_gg

### The following code replicates Table A24 from the online appendix
disb.fdi.stock <- (lm(disb_perc3 ~ avgPDO +
                        Outcome_Num2 +
                        Total_FDI_USDbill +
                        projsize_pop_st2 +      
                        polity_end_lag +                        
                        CorruptScore_end +
                        log(rgdp_end_lag)+
                        log(pop_end_lag) +
                        IBRDnew2 +
                        wbclosingyr +
                        PDO_Table +
                        Key_Matrix +
                        Comp_.Analysis +
                        as.factor(Country) +
                        as.factor(wbclosingyr),                 
                      data=wbfull))

disb.fdi.stock2 <- (lm(disb_perc3 ~ avgPDO +
                         Outcome_Num2 +
                         Total_FDI_USDbill +
                         projsize_pop_st2 +      
                         Any_Management +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag)+
                         log(pop_end_lag) +
                         IBRDnew2 +
                         wbclosingyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country) +
                         as.factor(wbclosingyr),                 
                       data=wb_proc))

disb.fdi.stock3 <- (lm(disb_perc3 ~ avgPDO +
                         Outcome_Num2 +
                         Total_FDI_USDbill +
                         projsize_pop_st2 +      
                         US_PT_Management +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag)+
                         log(pop_end_lag) +
                         IBRDnew2 +
                         wbclosingyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country) +
                         as.factor(wbclosingyr),                 
                       data=wb_proc))

disb.fdi.stock4 <- (lm(disb_perc3 ~ avgPDO +
                         Outcome_Num2 +
                         Total_FDI_USDbill +
                         projsize_pop_st2 +      
                         projsize_pop_st2*p_obs_usa_end_5yr +
                         p_obs_usa_end_5yr +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag)+
                         log(pop_end_lag) +
                         IBRDnew2 +
                         wbclosingyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country) +
                         as.factor(wbclosingyr),                 
                       data=wbfull))

disb.fdi.flow <- (lm(disb_perc3 ~ avgPDO +
                       Outcome_Num2 +
                       FDI_netinflows_USDbill +
                       projsize_pop_st2 +      
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag)+
                       log(pop_end_lag) +
                       IBRDnew2 +
                       wbclosingyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country) +
                       as.factor(wbclosingyr),                 
                     data=wbfull))

disb.fdi.flow2 <- (lm(disb_perc3 ~ avgPDO +
                        Outcome_Num2 +
                        FDI_netinflows_USDbill +
                        projsize_pop_st2 +      
                        Any_Management +
                        polity_end_lag +
                        CorruptScore_end +
                        log(rgdp_end_lag)+
                        log(pop_end_lag) +
                        IBRDnew2 +
                        wbclosingyr +
                        PDO_Table +
                        Key_Matrix +
                        Comp_.Analysis +
                        as.factor(Country) +
                        as.factor(wbclosingyr),                 
                      data=wb_proc))

disb.fdi.flow3 <- (lm(disb_perc3 ~ avgPDO +
                        Outcome_Num2 +
                        FDI_netinflows_USDbill +
                        projsize_pop_st2 +      
                        US_PT_Management +
                        polity_end_lag +
                        CorruptScore_end +
                        log(rgdp_end_lag)+
                        log(pop_end_lag) +
                        IBRDnew2 +
                        wbclosingyr +
                        PDO_Table +
                        Key_Matrix +
                        Comp_.Analysis +
                        as.factor(Country) +
                        as.factor(wbclosingyr),                 
                      data=wb_proc))

disb.fdi.flow4 <- (lm(disb_perc3 ~ avgPDO +
                        Outcome_Num2 +
                        FDI_netinflows_USDbill +
                        projsize_pop_st2 +      
                        p_obs_usa_end_5yr +
                        p_obs_usa_end_5yr*projsize_pop_st2 +
                        polity_end_lag +
                        CorruptScore_end +
                        log(rgdp_end_lag)+
                        log(pop_end_lag) +
                        IBRDnew2 +
                        wbclosingyr +
                        PDO_Table +
                        Key_Matrix +
                        Comp_.Analysis +
                        as.factor(Country) +
                        as.factor(wbclosingyr),                 
                      data=wbfull))
## Note: as earlier table outputs using stargazer where there is an interaction
# term, the labelling on the following output needs to be adjusted manually
# slightly to match the correct variable names with the correct coefficient 
# (for the interaction term only). See Table A24.
stargazer(disb.fdi.stock, disb.fdi.stock2, disb.fdi.stock3, disb.fdi.stock4,
          disb.fdi.flow, disb.fdi.flow2, disb.fdi.flow3, disb.fdi.flow4,
          type="latex",
          title="Total FDI and Disbursement",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement proportion"),
          column.separate=c(8),
          covariate.labels=c("Performance", "Evaluation",
                             "FDI stock (USD bill.)",                            
                             "FDI flow (USD bill.)", 
                             "Project Size per capita",
                             "MNC Management Contractor",
                             "US Management Contractor", 
                             "US F500",
                             "Polity$_{t-1}$",                              
                             "Control of Corruption",
                             "Log(GDP per capita)$_{t-1}$",
                             "Log(Population)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          #omit = c("Constant", "as.factor(Country)-1"),
          omit=c(20:120),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table A25 from the online appendix
disb.usaid2<- (lm(disb_perc2 ~ avgPDO +
                    Outcome_Num +
                    usdisb_bill_end_lag +
                    projsize_pop_st2 + 
                    projsize_pop_st2*usdisb_bill_end_lag+
                    polity_end_lag +
                    CorruptScore_end +
                    log(rgdp_end_lag) +
                    log(pop_end_lag) +
                    total_active_proj_end +
                    IBRD +
                    reportyr +
                    PDO_Table +
                    Key_Matrix +
                    Comp_.Analysis +
                    as.factor(Country)+
                    as.factor(reportyr)-1,
                  data=wb))

disb.sscore2 <-(lm(disb_perc2 ~ avgPDO +
                     Outcome_Num +
                     s3unus_end_lag + 
                     projsize_pop_st2 + 
                     projsize_pop_st2*s3unus_end_lag +
                     polity_end_lag +
                     CorruptScore_end +
                     log(rgdp_end_lag) +
                     log(pop_end_lag) +
                     total_active_proj_end +
                     IBRD +
                     reportyr +
                     PDO_Table +
                     Key_Matrix +
                     Comp_.Analysis +
                     as.factor(Country)+
                     as.factor(reportyr)-1,                          
                   data=wb))

disb.sscore.imp2<- (lm(disb_perc2 ~ avgPDO +
                         Outcome_Num +
                         s_imp_us_end_lag + 
                         projsize_pop_st2 + 
                         projsize_pop_st2*s_imp_us_end_lag + 
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag) +
                         log(pop_end_lag) +
                         total_active_proj_end +
                         IBRD +
                         reportyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country)+
                         as.factor(reportyr)-1,
                       data=wb))

disb.UNSCmem2 <-(lm(disb_perc2 ~ avgPDO +
                      Outcome_Num +
                      UNSCmem_end +
                      projsize_pop_st2 +      
                      projsize_pop_st2*UNSCmem_end +
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      total_active_proj_end +
                      IBRD+
                      reportyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis  +
                      as.factor(Country)+
                      as.factor(reportyr)-1,                 
                    data=wb))

disb.edmem2 <-(lm(disb_perc2 ~ avgPDO +
                    Outcome_Num +
                    ed_ibrd_end +
                    projsize_pop_st2 +      
                    projsize_pop_st2*ed_ibrd_end +
                    polity_end_lag +
                    CorruptScore_end +
                    log(rgdp_end_lag) +
                    log(pop_end_lag) +
                    total_active_proj_end +
                    IBRD+
                    reportyr +
                    PDO_Table +
                    Key_Matrix +
                    Comp_.Analysis +
                    as.factor(Country)+
                    as.factor(reportyr)-1,                 
                  data=wb))

## Note that the stargazer output does not match the interaction term variables
# and coefficients correctly (and outputs extra rows), so that labeling was 
# corrected manually when putting the table in to the appendix. See Table A25.
stargazer(disb.usaid2, disb.sscore2, disb.sscore.imp2, disb.UNSCmem2, disb.edmem2,
          type="latex",
          title="Disbursement and Geopolitical Interests with Interactions",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement Proportion"),
          column.separate=c(5),
          covariate.labels=c("Performance", "Evaluation",                            
                             "US Aid$_{t-1}$ (in billion USD)", "All UN Votes$_{t-1}$", 
                             "Imp. UN Votes$_{t-1}$", "UNSC Membership",
                             "Executive Director",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corruption Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop.)$_{t-1}$",
                             "Num. active projects", 
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2", "US Aid*Projsize",
                             "All UN*Projsize", "Imp. UN*Projsize",
                             "UNSC*Projsize", "ED*Projsize"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(20:113),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Figure A8 from the online appendix;
# make sure the disb.sscore2 regression from the table above (Table A25)
# is run before running the following code for the plot
projsize.seq.un <- seq(from=0, to=50, length=500)
int_invprojsize.un <- disb.sscore2$coefficients[3] + 
  disb.sscore2$coefficients[121]*projsize.seq.un
se_un <- sqrt(vcov(disb.sscore2)[3,3] + 
                projsize.seq.un^2*vcov(disb.sscore2)["s3unus_end_lag:projsize_pop_st2","s3unus_end_lag:projsize_pop_st2"] + 
                2*projsize.seq.un*vcov(disb.sscore2)["s3unus_end_lag","s3unus_end_lag:projsize_pop_st2"])
CI_int.un_upper <- int_invprojsize.un + 1.96*(se_un)
CI_int.un_lower <- int_invprojsize.un - 1.96*(se_un)
for_disbplot.un <- as.data.frame(cbind(projsize.seq.un, 
                                       int_invprojsize.un,
                                       CI_int.un_upper, 
                                       CI_int.un_lower))
plot.un_gg <- ggplot(for_disbplot.un) +
  geom_line(aes(x = projsize.seq.un, y = int_invprojsize.un), col = "black", lwd = 1) + 
  geom_ribbon(aes(x = projsize.seq.un, ymin = CI_int.un_lower, 
                  ymax = CI_int.un_upper), fill = "dark gray", alpha = 0.3) + 
  geom_hline(yintercept = 0, lty = 2, lwd = 0.6, col = "black") + 
  scale_x_continuous("\n Projsize Per Capita", 
                     breaks=seq(0,50,10)) +
  scale_y_continuous("Effect of UN Voting \n", 
                     breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3)) +
  theme_bw() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        plot.title = element_text(lineheight= 0.5, size = 14, face="bold")) 
plot.un_gg

### The following code replicates results presented in Table A26
geopol_F500 <- (lm(disb_perc2 ~ avgPDO +
                    Outcome_Num +
                    p_obs_usa_end_5yr +
                    p_obs_usa_end_5yr*projsize_pop_st2 +
                    usdisb_bill_end_lag +
                    s3unus_end_lag +
                    s_imp_us_end_lag +
                    UNSCmem_end +
                    ed_ibrd_end +
                    projsize_pop_st2 + 
                    polity_end_lag +
                    CorruptScore_end +
                    log(rgdp_end_lag) +
                    log(pop_end_lag) +
                    total_active_proj_end +
                    IBRD +
                    reportyr +
                    PDO_Table +
                    Key_Matrix +
                    Comp_.Analysis +
                    as.factor(Country) +
                    as.factor(reportyr)-1,
                  data=wb))

stargazer(geopol_F500,
          type="latex",
          title="Disbursement, Fortune 500 and Geopolitical Interests",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement Proportion"),
          column.separate=c(1),
          covariate.labels=c("Performance", "Evaluation", 
                             "US Fortune 500",
                             "US Aid$_{t-1}$ (in billion USD)", "SScore$_{t-1}$", 
                             "SScore Imp.$_{t-1}$", "UNSC Membership",
                             "Executive Director",
                             "Proj Size pc",
                             "Polity$_{t-1}$", "Corruption Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop.)$_{t-1}$",
                             "Num. active projects", 
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2", "US F500*ProjSize"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          #omit = c("Constant", "as.factor(Country)-1"),
          omit=c(20:105),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates Table A27 from the online appendix
mod.eval_USaid <- (lm(Outcome_Num ~ avgPDO +
                        usdisb_bill_end_lag +
                        polity_end_lag +
                        CorruptScore_end +
                        log(rgdp_end_lag) +
                        log(pop_end_lag) +
                        projsize_pop_st2 +
                        IBRD +
                        reportyr +
                        PDO_Table +
                        Key_Matrix +
                        Comp_.Analysis +
                        as.factor(Country) +
                        as.factor(reportyr)-1,
                      data=wb))

mod.eval_sscore <- (lm(Outcome_Num ~ avgPDO +
                         s3unus_end_lag +
                         polity_end_lag +
                         CorruptScore_end +
                         log(rgdp_end_lag) +
                         log(pop_end_lag) +
                         projsize_pop_st2 +
                         IBRD +
                         reportyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis +
                         as.factor(Country) +
                         as.factor(reportyr)-1,
                       data=wb))

mod.eval_sscore_imp <- (lm(Outcome_Num ~ avgPDO +
                             s_imp_us_end_lag +
                             polity_end_lag +
                             CorruptScore_end +
                             log(rgdp_end_lag) +
                             log(pop_end_lag) +
                             projsize_pop_st2 +
                             IBRD +
                             reportyr +
                             PDO_Table +
                             Key_Matrix +
                             Comp_.Analysis +
                             as.factor(Country) +
                             as.factor(reportyr)-1,
                           data=wb))

mod.eval_UNSC <- (lm(Outcome_Num ~ avgPDO +
                       UNSCmem_end +
                       polity_end_lag +
                       CorruptScore_end +
                       log(rgdp_end_lag) +
                       log(pop_end_lag) +
                       projsize_pop_st2 +
                       IBRD +
                       reportyr +
                       PDO_Table +
                       Key_Matrix +
                       Comp_.Analysis +
                       as.factor(Country) +
                       as.factor(reportyr)-1,
                     data=wb))

mod.eval_ED <- (lm(Outcome_Num ~ avgPDO +
                     ed_ibrd_end +
                     polity_end_lag +
                     CorruptScore_end +
                     log(rgdp_end_lag) +
                     log(pop_end_lag) +
                     projsize_pop_st2 +
                     IBRD +
                     reportyr +
                     PDO_Table +
                     Key_Matrix +
                     Comp_.Analysis +
                     as.factor(Country) +
                     as.factor(reportyr)-1,
                   data=wb))

stargazer(mod.eval_USaid, mod.eval_sscore, mod.eval_sscore_imp,
          mod.eval_UNSC, mod.eval_ED,
          type="latex",
          title="Project Evaluation and Gepolitics",
          style="ajps",
          summary=TRUE,
          column.labels=c("Evaluation"),
          column.separate=c(5),
          covariate.labels=c("Performance",
                             "US Aid$_{t-1}$ (in billion USD)",
                             "SScore$_{t-1}$", "SScore Imp.$_{t-1}$",
                             "UNSC Membership", "Executive Director",
                             "Polity$_{t-1}$", "Corr. Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Pop)$_{t-1}$",
                             "Proj. size pc",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          #omit = c("Constant", "as.factor(Country)-1"),
          omit=c(14:121),
          keep.stat=c("n", "adj.rsq")
)

### The following code replicates results presented in Table A28 in the online appendix
disb1_kilbyproc <- (lm(disb_perc3 ~ avgPDO +
                     Outcome_Num2 +
                     US_friend_lag +
                     projsize_pop_st2 +      
                     polity_end_lag.x +
                     CorruptScore_end +
                     log(rgdp_end_lag.x) +
                     log(pop_end_lag.x) +
                     IBRD +
                     wbclosingyr +
                     PDO_Table +
                     Key_Matrix +
                     Comp_.Analysis + 
                     as.factor(Country) +
                     as.factor(wbclosingyr)-1,                 
                   data=kilby))

disb2_kilbyproc <- (lm(disb_perc3 ~ avgPDO +
                     Outcome_Num2 +
                     Any_Management +
                     US_friend_lag +
                     projsize_pop_st2 +      
                     polity_end_lag.x +
                     CorruptScore_end +
                     log(rgdp_end_lag.x) +
                     log(pop_end_lag.x) +
                     IBRD +
                     wbclosingyr +
                     PDO_Table +
                     Key_Matrix +
                     Comp_.Analysis + 
                     as.factor(Country) +
                     as.factor(wbclosingyr)-1,                 
                   data=kilby))

disb3_kilbyproc <- (lm(disb_perc3 ~ avgPDO +
                         Outcome_Num2 +
                         US_PT_Management +
                         US_friend_lag +
                         projsize_pop_st2 +      
                         polity_end_lag.x +
                         CorruptScore_end +
                         log(rgdp_end_lag.x) +
                         log(pop_end_lag.x) +
                         IBRD +
                         wbclosingyr +
                         PDO_Table +
                         Key_Matrix +
                         Comp_.Analysis + 
                         as.factor(Country) +
                         as.factor(wbclosingyr)-1,                 
                       data=kilby))

stargazer(disb1_kilbyproc, disb2_kilbyproc, disb3_kilbyproc,
          type="latex",
          title="Project Disbursement and US Friend (Kilby comparison)",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement proportion"),
          column.separate=c(3),
          covariate.labels=c("Performance", "Evaluation",                            
                             "Any Management", "US Management",
                             "US Friend",
                             "Proj. Size pc",
                             "Polity$_{t-1}$", "Corruption Control",
                             "Log(GDP pc)$_{t-1}$",
                             "Log(Population)$_{t-1}$",
                             "IBRD", "Report Year"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(14:119),
          keep.stat=c("n", "adj.rsq")
)

### The following code repilcates Table A29 from the online appendix
disb_IEG_MNC <- (lm(disb_perc3 ~ avgPDO +
                      IEG_Outcome_ICRNum2 +
                      MNC_any +
                      projsize_pop_st2 +      
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis+
                      as.factor(Country) +
                      as.factor(wbclosingyr)-1,                 
                    data=ieg))

disb_IEG_MNCUS <- (lm(disb_perc3 ~ avgPDO +
                      IEG_Outcome_ICRNum2 +
                      MNC_Inv +
                      projsize_pop_st2 +      
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis+
                      as.factor(Country) +
                      as.factor(wbclosingyr)-1,                 
                    data=ieg))

disb_IEG_MNC2 <-(lm(disb_perc3 ~ IEG_Outcome_avgPDONum2 +
              Outcome_Num2 +
             MNC_any+
             projsize_pop_st2 +      
             polity_end_lag +
             CorruptScore_end +
             log(rgdp_end_lag) +
             log(pop_end_lag) +
             IBRD +
             wbclosingyr +
             PDO_Table +
             Key_Matrix +
             Comp_.Analysis+
             as.factor(Country) +
            as.factor(wbclosingyr)-1,                 
           data=ieg))

disb_IEG_MNCUS2 <-(lm(disb_perc3 ~ IEG_Outcome_avgPDONum2 +
                      Outcome_Num2 +
                      MNC_Inv +
                      projsize_pop_st2 +      
                      polity_end_lag +
                      CorruptScore_end +
                      log(rgdp_end_lag) +
                      log(pop_end_lag) +
                      IBRD +
                      wbclosingyr +
                      PDO_Table +
                      Key_Matrix +
                      Comp_.Analysis+
                      as.factor(Country) +
                      as.factor(wbclosingyr)-1,                 
                    data=ieg))

stargazer(disb_IEG_MNC, disb_IEG_MNCUS, disb_IEG_MNC2, disb_IEG_MNCUS2,
          type="latex",
          title="IEG Outcomes and Disbursement - MNC Contractors",
          style="ajps",
          summary=TRUE,
          column.labels=c("Disbursement proportion"),
          column.separate=c(4),
          covariate.labels=c("Performance", "IEG for Evaluation",
                             "IEG for Performance", "Evaluation",                            
                             "Any MNC", "US MNC",
                             "Project Size per capita",
                             "Polity$_{t-1}$", "Control of Corruption",
                             "Log(GDP per capita)$_{t-1}$",
                             "Log(Population)$_{t-1}$",
                             "IBRD", "Report Year", 
                             "Report Type 4", "Report Type 3",
                             "Report Type 2"),
          dep.var.labels.include=FALSE,
          align=TRUE,
          omit=c(14:111),
          keep.stat=c("n", "adj.rsq")
)

#################################################################################