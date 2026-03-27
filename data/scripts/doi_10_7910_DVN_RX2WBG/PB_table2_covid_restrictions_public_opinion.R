# The Following R Code replicates the following items in the manuscript titled: "Nail in the Coffin or Lifeline? Evaluating the Electoral Impact of COVID-19 on President Trump in the 2020 Election"". The items are the following:

# 1) Table 2: Percent of Partisans Who Believe COVID-19 Restrictions Are Being Lifted Too Slowly

#Set Local Working Directory
setwd("/PB Data Replication Files")

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 99)
set.seed(1993)


# Voter Study Group

library(readstata13)
library(margins)
library(ggplot2)
library(multiwayvcov)
library(descr)
library(lmtest)
library(stargazer)

'%!in%' <- function(x,y)!('%in%'(x,y))

#### Data Wrangling ####
options(scipen=999)

vs <- subset(read.dta13("voter_panel.dta"),select=c("weight_genpop_2020Sep","weight_genpop_2020Nov","inputstate_2020Sep","cdid_2020Nov","track_2020Sep","track_2020Nov","ideo5_2020Sep","ideo5_2020Nov","faminc_2020Sep","race_2020Sep","trumpapp_covid_2020Sep","trumpapp_covid_2020Nov","trumpapp_2020Nov","trumpapp_2020Sep","educ_2020Sep","pid3_2020Sep","pid3_2020Nov","covid_sickyou_2020Sep", "covid_sickfamily_2020Sep", "covid_sickwork_2020Sep", "covid_sickfriend_2020Sep", "covid_sickyou_2020Nov","covid_sickfamily_2020Nov","covid_sickwork_2020Nov","covid_sickfriend_2020Nov","endtime_2020Sep","endtime_2020Nov","covid_concern_2020Nov","covid_endrestrictions_2020Nov","covid_concern_2020Sep","covid_endrestrictions_2020Sep","issue_covid_2020Nov","covid_restrict_school_2020Sep","covid_restrict_travel_2020Sep","finance_event_income_2020Sep","presvote_2020Sep","presvote_2020Nov","pid3_2016","presvote_2016","econtrend_2020Nov","econtrend_2020Sep","housevote_2020Sep","housevote_2020Nov","housevote_2018"))
vs <- subset(vs,!is.na(vs$endtime_2020Sep)) # N = 5900 for 2020 Sep/Nov Panel

vs$lift_restrictions_too_slow_nov <- ifelse(vs$covid_endrestrictions_2020Nov == "Not lift the restrictions quickly enough",1,ifelse(vs$covid_endrestrictions_2020Nov == "Lift the restrictions too quickly",0,NA))

vs$lift_restrictions_too_slow_sep <- ifelse(vs$covid_endrestrictions_2020Sep == "Not lift the restrictions quickly enough",1,ifelse(vs$covid_endrestrictions_2020Sep == "Lift the restrictions too quickly",0,NA))

#Code Party Identification
vs$pid3_coded <- ifelse(vs$pid3_2020Sep == "Democrat","Democrat",ifelse(vs$pid3_2020Sep == "Republican","Republican",ifelse(vs$pid3_2020Sep == "Independent","Independent","Independent")))
vs$pid3_coded <- factor(vs$pid3_coded,levels=c("Democrat","Independent","Republican"))

################## Cross Table #######################
library(plyr)
library(diagis)

#******************** September ***********************
All <- data.frame(fit = weighted.mean(vs$lift_restrictions_too_slow_sep, w = vs$weight_genpop_2020Sep, na.rm = T),
                  se.fit = weighted_se(vs$lift_restrictions_too_slow_sep, w = vs$weight_genpop_2020Sep, na.rm = T))

All$Var1 <- 1
All$pid3_coded <- "General Public"

All.1 <- subset(All, Var1 == 1)
All.1$Var1 <- NULL
All.1$Freq <- NULL

#Partisans
Results <- lm(lift_restrictions_too_slow_sep ~ pid3_coded, data = vs, weights=weight_genpop_2020Sep)
newdat <- data.frame(pid3_coded = na.omit(levels(vs$pid3_coded)))
PID_Table <- cbind(newdat, predict(Results,newdat, se.fit = T))

Table_sep <- plyr::rbind.fill(All.1, PID_Table)

Table_sep$Wave <- "September"

Table_sep$id  <- 1:nrow(Table_sep)


#N
N_DF <- rbind(data.frame(pid3_coded = "General Public", N = sum(!is.na(vs$lift_restrictions_too_slow_sep))),
              setNames(data.frame(table(Results$model$pid3_coded)), c("pid3_coded", "N")))

Table_sep <- merge(Table_sep, N_DF, by = c("pid3_coded"))
Table_sep <- Table_sep[order(Table_sep$id), ]

#******************** November ***********************

x <- na.omit(subset(vs,select=c(lift_restrictions_too_slow_nov,weight_genpop_2020Nov)))
All <- data.frame(fit = weighted.mean(x$lift_restrictions_too_slow_nov, w = x$weight_genpop_2020Nov, na.rm = T),se.fit = weighted_se(x$lift_restrictions_too_slow_nov, w = x$weight_genpop_2020Nov, na.rm = T))


All$Var1 <- 1

All$pid3_coded <- "General Public"

All.1 <- subset(All, Var1 == 1)
All.1$Var1 <- NULL
All.1$Freq <- NULL

#Partisans
Results <- lm(lift_restrictions_too_slow_nov ~ pid3_coded, data = vs, weights=weight_genpop_2020Nov)
newdat <- data.frame(pid3_coded = na.omit(levels(vs$pid3_coded)))
PID_Table <- cbind(newdat, predict(Results,newdat, se.fit = T))

Table_nov <- plyr::rbind.fill(All.1, PID_Table)

Table_nov$Wave <- "November"
Table_nov$id  <- 1:nrow(Table_nov)

#N
N_DF <- rbind(data.frame(pid3_coded = "General Public", N = sum(!is.na(vs$lift_restrictions_too_slow_nov))),
              setNames(data.frame(table(Results$model$pid3_coded)), c("pid3_coded", "N")))

Table_nov <- merge(Table_nov, N_DF, by = c("pid3_coded"))
Table_nov <- Table_nov[order(Table_nov$id), ]


#******************** Merge Table ***********************
Final_Table.1 <- rbind(Table_sep, Table_nov)
Final_Table.2 <-  with(Final_Table.1, data.frame(Wave, pid3_coded,N, fit = round(fit*100,2), se.fit = round(se.fit*100, 2)))

#******************** Subset Table ***********************
Final_Table.S <- subset(Final_Table.2, Wave == "September")
Final_Table.N <- subset(Final_Table.2, Wave == "November")

#******************** Make Table ***********************
#This is only a placeholder
Results1 <- lm(lift_restrictions_too_slow_sep ~ pid3_2016, data = vs)
Results2 <- lm(lift_restrictions_too_slow_sep ~ pid3_2016, data = vs)

September_N_Note <- paste("September Wave: General Public N = ", subset(Final_Table.S, pid3_coded == "General Public")$N, 
               ", Democratic Partisans N = ", subset(Final_Table.S, pid3_coded == "Democrat")$N,
               ", Independent Partisans N = ", subset(Final_Table.S, pid3_coded == "Independent")$N, 
               ", Republican Partisans N = ", subset(Final_Table.S, pid3_coded == "Republican")$N, ".", sep = "")

November_N_Note <- paste("November Wave: General Public N = ", subset(Final_Table.N, pid3_coded == "General Public")$N, 
                         ", Democratic Partisans N = ", subset(Final_Table.N, pid3_coded == "Democrat")$N,
                         ", Independent Partisans N = ", subset(Final_Table.N, pid3_coded == "Independent")$N, 
                         ", Republican Partisans N = ", subset(Final_Table.N, pid3_coded == "Republican")$N, ".", sep = "")


stargazer(list(Results1, Results2), omit = c("Constant"),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
          type = "text",          
          single.row=T,
          model.numbers = FALSE,
          dep.var.labels.include =F,
          star.cutoffs =  NA,
          column.labels = c("September", "November"),
          coef= list(c(NA, Final_Table.S$fit), c(NA, Final_Table.N$fit)),
          se= list(c(NA, Final_Table.S$se.fit), c(NA, Final_Table.N$se.fit)),
          omit.stat = "all",
          notes.append = FALSE,
          notes.align = "l",
          digits = 2,
          notes=  c("Estimates are weighted mean percentage with standard errors in parenthesis.", September_N_Note, November_N_Note),
          #notes.label = paste("Estimates are weighted mean percentage with standard errors in parenthesis.\n", September_N_Note,"\n",November_N_Note,sep =""),
          title="Voter Survey: Percent of Partisans Who Believe COVID-19 Restrictions Are Being Lifted Too Slowly",
          dep.var.caption = "Waves",
          covariate.labels = Final_Table.N$pid3_coded,
          out = "table2_restrictions_cross_tabulation.tex")
