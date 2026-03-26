# The Following R Code replicates the following items in the manuscript titled: "Nail in the Coffin or Lifeline? Evaluating the Electoral Impact of COVID-19 on President Trump in the 2020 Election"". The items are the following:

# 1) Full model tables of the September & November waves of the Voter Study data for Figure 4: Relationship Between Public Health Concern & Political Support for President Trump articulated in Appendix Tables: 13A (DV: Trump COVID-19 Approval), 14A (DV: Trump Vote Intention), 15A (DV: 2016 HRC-2020 DJT Electoral Preference Change), 16A (2016 DJT-2020 JRB Electoral Preference  Change)

# 2) Full model tables of the September & November waves of the Voter Study data for Figure 5: Relationship Between Local Severity Context & COVID-19 Attitudes articulated in Appendix Tables: 23A (DV: COVID-19 Public Health Concern), 24A (DV: COVID-19 Restrictions Concern)

# 3) Full model tables of the September & November waves of the Voter Study data for Figure 6: Relationship Between Public Health Concern/Restrictions Concern & Political Support for President Trump articulated in Appendix Tables: 18A (DV: Trump COVID-19 Approval), 19A (DV: Trump Vote Intention), 20A (DV: 2016 HRC-2020 DJT Electoral Preference Change), 21A (2016 DJT-2020 JRB Electoral Preference  Change)

library(readstata13)
library(margins)
library(ggplot2)
library(multiwayvcov)
library(descr)
library(forcats)
library(stargazer)
library(broom)
library(dplyr)

#### Data Wrangling ####

#### Data Wrangling ####

#Set Local Working Directory
setwd("/PB Data Replication Files")

options(scipen=999)

vs <- subset(read.dta13("voter_panel.dta"),select=c("weight_genpop_2020Sep","weight_genpop_2020Nov","inputstate_2020Sep","cdid_2020Nov","track_2020Sep","track_2020Nov","ideo5_2020Sep","ideo5_2020Nov","faminc_2020Sep","race_2020Sep","trumpapp_covid_2020Sep","trumpapp_covid_2020Nov","trumpapp_2020Nov","trumpapp_2020Sep","educ_2020Sep","pid3_2020Sep","pid3_2020Nov","covid_sickyou_2020Sep", "covid_sickfamily_2020Sep", "covid_sickwork_2020Sep", "covid_sickfriend_2020Sep", "covid_sickyou_2020Nov","covid_sickfamily_2020Nov","covid_sickwork_2020Nov","covid_sickfriend_2020Nov","endtime_2020Sep","endtime_2020Nov","covid_concern_2020Nov","covid_endrestrictions_2020Nov","covid_concern_2020Sep","covid_endrestrictions_2020Sep","issue_covid_2020Nov","covid_restrict_school_2020Sep","covid_restrict_travel_2020Sep","finance_event_income_2020Sep","presvote_2020Sep","presvote_2020Nov","pid3_2016","presvote_2016","econtrend_2020Nov","econtrend_2020Sep","housevote_2020Sep","housevote_2020Nov","housevote_2018","gender_2020Sep","birthyr_2020Sep"))
vs <- subset(vs,!is.na(vs$endtime_2020Sep)) # N = 5900 for 2020 Sep/Nov Panel

vs$date_sep <- as.Date(vs$endtime_2020Sep)
vs$date_nov <- as.Date(vs$endtime_2020Nov)

vs$state <- as.character(vs$inputstate_2020Sep)
vs$state[vs$state=="wyoming"] <- "WY"
vs$state[vs$state=="Wyoming"] <- "WY"
vs$state[vs$state=="wisconsin"] <- "WI"
vs$state[vs$state=="Wisconsin"] <- "WI"
vs$state[vs$state=="west virginia"] <- "WV"
vs$state[vs$state=="West Virginia"] <- "WV"
vs$state[vs$state=="washington"] <- "WA"
vs$state[vs$state=="Washington"] <- "WA"
vs$state[vs$state=="virginia"] <- "VA"
vs$state[vs$state=="Virginia"] <- "VA"
vs$state[vs$state=="vermont"] <- "VT"
vs$state[vs$state=="Vermont"] <- "VT"
vs$state[vs$state=="utah"] <- "UT"
vs$state[vs$state=="Utah"] <- "UT"
vs$state[vs$state=="texas"] <- "TX"
vs$state[vs$state=="Texas"] <- "TX"
vs$state[vs$state=="tennessee"] <- "TN"
vs$state[vs$state=="Tennessee"] <- "TN"
vs$state[vs$state=="south dakota"] <- "SD"
vs$state[vs$state=="South Dakota"] <- "SD"
vs$state[vs$state=="south carolina"] <- "SC"
vs$state[vs$state=="South Carolina"] <- "SC"
vs$state[vs$state=="rhode island"] <- "RI"
vs$state[vs$state=="Rhode Island"] <- "RI"
vs$state[vs$state=="pennsylvania"] <- "PA"
vs$state[vs$state=="Pennsylvania"] <- "PA"
vs$state[vs$state=="oregon"] <- "OR"
vs$state[vs$state=="Oregon"] <- "OR"
vs$state[vs$state=="oklahoma"] <- "OK"
vs$state[vs$state=="Oklahoma"] <- "OK"
vs$state[vs$state=="ohio"] <- "OH"
vs$state[vs$state=="Ohio"] <- "OH"
vs$state[vs$state=="north dakota"] <- "ND"
vs$state[vs$state=="North Dakota"] <- "ND"
vs$state[vs$state=="north carolina"] <- "NC"
vs$state[vs$state=="North Carolina"] <- "NC"
vs$state[vs$state=="new york"] <- "NY"
vs$state[vs$state=="New York"] <- "NY"
vs$state[vs$state=="new mexico"] <- "NM"
vs$state[vs$state=="New Mexico"] <- "NM"
vs$state[vs$state=="new jersey"] <- "NJ"
vs$state[vs$state=="New Jersey"] <- "NJ"
vs$state[vs$state=="new hampshire"] <- "NH"
vs$state[vs$state=="New Hampshire"] <- "NH"
vs$state[vs$state=="nevada"] <- "NV"
vs$state[vs$state=="Nevada"] <- "NV"
vs$state[vs$state=="nebraska"] <- "NE"
vs$state[vs$state=="Nebraska"] <- "NE"
vs$state[vs$state=="montana"] <- "MT"
vs$state[vs$state=="Montana"] <- "MT"
vs$state[vs$state=="missouri"] <- "MO"
vs$state[vs$state=="Missouri"] <- "MO"
vs$state[vs$state=="mississippi"] <- "MS"
vs$state[vs$state=="Mississippi"] <- "MS"
vs$state[vs$state=="minnesota"] <- "MN"
vs$state[vs$state=="Minnesota"] <- "MN"
vs$state[vs$state=="michigan"] <- "MI"
vs$state[vs$state=="Michigan"] <- "MI"
vs$state[vs$state=="massachusetts"] <- "MA"
vs$state[vs$state=="Massachusetts"] <- "MA"
vs$state[vs$state=="maryland"] <- "MD"
vs$state[vs$state=="Maryland"] <- "MD"
vs$state[vs$state=="maine"] <- "ME"
vs$state[vs$state=="Maine"] <- "ME"
vs$state[vs$state=="louisiana"] <- "LA"
vs$state[vs$state=="Louisiana"] <- "LA"
vs$state[vs$state=="kentucky"] <- "KY"
vs$state[vs$state=="Kentucky"] <- "KY"
vs$state[vs$state=="kansas"] <- "KS"
vs$state[vs$state=="Kansas"] <- "KS"
vs$state[vs$state=="iowa"] <- "IA"
vs$state[vs$state=="Iowa"] <- "IA"
vs$state[vs$state=="indiana"] <- "IN"
vs$state[vs$state=="Indiana"] <- "IN"
vs$state[vs$state=="illinois"] <- "IL"
vs$state[vs$state=="Illinois"] <- "IL"
vs$state[vs$state=="idaho"] <- "ID"
vs$state[vs$state=="Idaho"] <- "ID"
vs$state[vs$state=="hawaii"] <- "HI"
vs$state[vs$state=="Hawaii"] <- "HI"
vs$state[vs$state=="georgia"] <- "GA"
vs$state[vs$state=="Georgia"] <- "GA"
vs$state[vs$state=="florida"] <- "FL"
vs$state[vs$state=="Florida"] <- "FL"
vs$state[vs$state=="delaware"] <- "DE"
vs$state[vs$state=="Delaware"] <- "DE"
vs$state[vs$state=="District of Columbia"] <- "DE"
vs$state[vs$state=="connecticut"] <- "CT"
vs$state[vs$state=="Connecticut"] <- "CT"
vs$state[vs$state=="colorado"] <- "CO"
vs$state[vs$state=="Colorado"] <- "CO"
vs$state[vs$state=="california"] <- "CA"
vs$state[vs$state=="California"] <- "CA"
vs$state[vs$state=="arkansas"] <- "AR"
vs$state[vs$state=="Arkansas"] <- "AR"
vs$state[vs$state=="arizona"] <- "AZ"
vs$state[vs$state=="Arizona"] <- "AZ"
vs$state[vs$state=="alaska"] <- "AK"
vs$state[vs$state=="Alaska"] <- "AK"
vs$state[vs$state=="alabama"] <- "AL"
vs$state[vs$state=="Alabama"] <- "AL"

vs$district <- vs$cdid_2020Nov
vs$district[vs$district == " "] <- NA
vs$congressional_district <- ifelse(!is.na(vs$district),paste(vs$state,vs$district,sep=""),NA)
vs$congressional_district <- ifelse(!is.na(vs$district),paste(vs$state,vs$district,sep=""),NA)

vs$pid3_coded <- ifelse(vs$pid3_2020Sep == "Democrat","Democrat",ifelse(vs$pid3_2020Sep == "Republican","Republican",ifelse(vs$pid3_2020Sep == "Independent","Independent","Independent")))
vs$pid3_coded <- factor(vs$pid3_coded,levels=c("Democrat","Independent","Republican"))

vs$lift_restrictions_too_slow_nov <- ifelse(vs$covid_endrestrictions_2020Nov == "Not lift the restrictions quickly enough",1,ifelse(vs$covid_endrestrictions_2020Nov == "Lift the restrictions too quickly",0,NA))
freq(vs$lift_restrictions_too_slow_nov[vs$pid3_coded == "Democrat"])
freq(vs$lift_restrictions_too_slow_nov[vs$pid3_coded == "Republican"])

vs$lift_restrictions_too_slow_sep <- ifelse(vs$covid_endrestrictions_2020Sep == "Not lift the restrictions quickly enough",1,ifelse(vs$covid_endrestrictions_2020Sep == "Lift the restrictions too quickly",0,NA))

vs$pandemic_income_drop <- ifelse(vs$finance_event_income_2020Sep == "Yes",1,ifelse(vs$finance_event_income_2020Sep == "No",0,NA))

vs$trump_vote_sep <- ifelse(vs$presvote_2020Sep == "Donald Trump",1,ifelse(vs$presvote_2020Sep == "Joe Biden",0,NA))

vs$trump_vote_nov <- ifelse(vs$presvote_2020Nov == "Donald Trump",1,ifelse(vs$presvote_2020Nov == "Joe Biden",0,NA))

vs$row_id <- as.numeric(rownames(vs))

# COVID Contextual Data Merging. Note COVID-19 congressional district data taken from the Harvard Center for Population and Development Studies’ COVID-19 Metrics for United States Congressional Districts Project

load("covid_cd_data.Rdata")

for(i in 3:ncol(district_covid)){
  colnames(district_covid)[i] <- paste("district_",colnames(district_covid)[i],sep="")
}
x <- subset(district_covid,select=c("date","congress_district","district_covid_deaths_per100k"))
colnames(x) <- c("date_nov","congressional_district","nov_district_covid_deaths_per100k")

vs <- merge(vs,x,by=c("date_nov","congressional_district"),all=T)

vs <- subset(vs,!is.na(vs$row_id))
nrow(vs)==max(vs$row_id)

# Sep
x <- subset(district_covid,select=c("date","congress_district","district_covid_deaths_per100k"))
colnames(x) <- c("date_sep","congressional_district","sep_district_covid_deaths_per100k")

vs <- merge(vs,x,by=c("date_sep","congressional_district"),all=T)

vs <- subset(vs,!is.na(vs$row_id))
nrow(vs)==max(vs$row_id)

vs$nov_lack_covid_concern <- ifelse(vs$covid_concern_2020Nov %in% c("Very Concerned","Somewhat concerned"),1,0)
vs$sep_lack_covid_concern <- ifelse(vs$covid_concern_2020Sep %in% c("Very Concerned","Somewhat concerned"),1,0)

vs$trump_approval_nov <- ifelse(vs$trumpapp_covid_2020Nov %in% c("Strongly approve","Somewhat approve"),1,ifelse(vs$trumpapp_covid_2020Nov %in% c("Strongly disapprove","Somewhat disapprove"),0,NA))
vs$trump_approval_sep <- ifelse(vs$trumpapp_covid_2020Sep %in% c("Strongly Approve","Somewhat Approve"),1,ifelse(vs$trumpapp_covid_2020Sep %in% c("Strongly Disapprove","Somewhat Disapprove"),0,NA))

vs$conservative <- as.numeric(factor(vs$ideo5_2020Sep,levels=c("Very Liberal","Liberal","Moderate","Conservative","Very Conservative")))

vs$race <- ifelse(vs$race_2020Sep == "White","white",ifelse(vs$race_2020Sep == "Black","black",ifelse(vs$race_2020Sep == "Hispanic","hispanic","other")))

vs$education_coded <- ifelse(vs$educ_2020Sep == "No HS","no hs",ifelse(vs$educ_2020Sep == "High school graduate","hs",ifelse(vs$educ_2020Sep == "Some college","some college",ifelse(vs$educ_2020Sep == "2-year","some college",ifelse(vs$educ_2020Sep == "4-year","ba",ifelse(vs$educ_2020Sep == "Post-grad","post-ba",NA))))))

vs$education_coded <- factor(vs$education_coded,levels=c("no hs","hs","some college","ba","post-ba"))

x <- subset(vs,select=c(covid_sickfamily_2020Sep,covid_sickyou_2020Sep,covid_sickwork_2020Sep,covid_sickfriend_2020Sep))

x$covid_sickfamily_2020Sep <- as.character(x$covid_sickfamily_2020Sep)
x$covid_sickyou_2020Sep <- as.character(x$covid_sickyou_2020Sep)
x$covid_sickwork_2020Sep <- as.character(x$covid_sickwork_2020Sep)
x$covid_sickfriend_2020Sep <- as.character(x$covid_sickfriend_2020Sep)

x[x == "Yes"] <- 1
x[x == "No"] <- 0
x[x == "Maybe"] <- NA

x$covid_sickfamily_2020Sep <- as.numeric(x$covid_sickfamily_2020Sep)
x$covid_sickyou_2020Sep <- as.numeric(x$covid_sickyou_2020Sep)
x$covid_sickwork_2020Sep <- as.numeric(x$covid_sickwork_2020Sep)
x$covid_sickfriend_2020Sep <- as.numeric(x$covid_sickfriend_2020Sep)

x$proximity_covid_sep <- rowSums(x,na.rm = T)
x <- subset(x,select=c(proximity_covid_sep))
vs <- cbind(vs,x)

x <- subset(vs,select=c(covid_sickfamily_2020Nov,covid_sickyou_2020Nov,covid_sickwork_2020Nov,covid_sickfriend_2020Nov))

x$covid_sickfamily_2020Nov <- as.character(x$covid_sickfamily_2020Nov)
x$covid_sickyou_2020Nov <- as.character(x$covid_sickyou_2020Nov)
x$covid_sickwork_2020Nov <- as.character(x$covid_sickwork_2020Nov)
x$covid_sickfriend_2020Nov <- as.character(x$covid_sickfriend_2020Nov)

x[x == "Yes"] <- 1
x[x == "No"] <- 0
x[x == "Maybe"] <- NA

x$covid_sickfamily_2020Nov <- as.numeric(x$covid_sickfamily_2020Nov)
x$covid_sickyou_2020Nov <- as.numeric(x$covid_sickyou_2020Nov)
x$covid_sickwork_2020Nov <- as.numeric(x$covid_sickwork_2020Nov)
x$covid_sickfriend_2020Nov <- as.numeric(x$covid_sickfriend_2020Nov)

x$proximity_covid_nov <- rowSums(x,na.rm = T)
x <- subset(x,select=c(proximity_covid_nov))
vs <- cbind(vs,x)

vs$household_income <- as.numeric(vs$faminc_2020Sep)
vs$household_income <- ifelse(vs$household_income == 17,NA,vs$household_income)
vs$household_income_bin <- factor(cut_number(as.numeric(vs$household_income),3),labels=c("L Income","M Income","H Income"))

vs$positive_retro_econ_eval_sep <- as.numeric(factor(vs$econtrend_2020Sep,levels=c("Getting worse","About the same","Getting better")))

vs$positive_retro_econ_eval_nov <- as.numeric(factor(vs$econtrend_2020Nov,levels=c("Getting worse","About the same","Getting better")))

vs$pid3_coded_2016 <- ifelse(vs$pid3_2016 == "Democrat","Democrat",ifelse(vs$pid3_2016 == "Republican","Republican",ifelse(vs$pid3_2016 == "Independent","Independent",NA)))
vs$pid3_coded_2016 <- factor(vs$pid3_coded_2016,levels=c("Democrat","Independent","Republican"))

vs$pid_change_2016_2020 <- as.numeric(vs$pid3_coded)-as.numeric(vs$pid3_coded_2016)

vs$trump_vote_2016 <- ifelse(vs$presvote_2016 == "Donald Trump",1,ifelse(vs$presvote_2016 %in% c("Hillary Clinton"),0,NA))

table(vs$trump_vote_2016,vs$trump_vote_sep)

vs$trump_vote_sep_2016_2020 <- ifelse(vs$trump_vote_2016==0 & vs$trump_vote_sep == 1,1,ifelse(vs$trump_vote_2016==1 & vs$trump_vote_sep == 0,0,ifelse(vs$trump_vote_2016==1 & vs$trump_vote_sep == 1,0,ifelse(vs$trump_vote_2016== 0 & vs$trump_vote_sep == 0,0,NA))))

vs$biden_vote_sep_2016_2020 <- ifelse(vs$trump_vote_2016==0 & vs$trump_vote_sep == 1,0,ifelse(vs$trump_vote_2016==1 & vs$trump_vote_sep == 0,1,ifelse(vs$trump_vote_2016==1 & vs$trump_vote_sep == 1,0,ifelse(vs$trump_vote_2016== 0 & vs$trump_vote_sep == 0,0,NA))))

vs$trump_vote_nov_2016_2020 <- ifelse(vs$trump_vote_2016==0 & vs$trump_vote_nov == 1,1,ifelse(vs$trump_vote_2016==1 & vs$trump_vote_nov == 0,0,ifelse(vs$trump_vote_2016==1 & vs$trump_vote_nov == 1,0,ifelse(vs$trump_vote_2016== 0 & vs$trump_vote_nov == 0,0,NA))))

vs$biden_vote_nov_2016_2020 <- ifelse(vs$trump_vote_2016==0 & vs$trump_vote_nov == 1,0,ifelse(vs$trump_vote_2016==1 & vs$trump_vote_nov == 0,1,ifelse(vs$trump_vote_2016==1 & vs$trump_vote_nov == 1,0,ifelse(vs$trump_vote_2016== 0 & vs$trump_vote_nov == 0,0,NA))))

vs$gop_vote_sep <- ifelse(vs$housevote_2020Sep == "The Republican Party candidate",1,ifelse(vs$housevote_2020Sep == "The Democratic Party candidate",0,NA))

vs$gop_vote_nov <- ifelse(vs$housevote_2020Nov == "$HouseCand2Name ($HouseCand2Party)",1,ifelse(vs$housevote_2020Nov == "$HouseCand1Name ($HouseCand1Party)",0,NA))

vs$gop_vote_2018 <- ifelse(vs$housevote_2018 == "The Republican Party candidate",1,ifelse(vs$housevote_2018 == "The Democratic Party candidate",0,NA))

vs$gop_vote_sep_2018_2020 <- ifelse(vs$gop_vote_2018==0 & vs$gop_vote_sep == 1,1,0)
vs$gop_vote_nov_2018_2020 <- ifelse(vs$gop_vote_2018==0 & vs$gop_vote_nov == 1,1,0)

vs$dem_vote_sep_2018_2020 <- ifelse(vs$gop_vote_2018==0 & vs$gop_vote_nov == 1,1,0)
vs$dem_vote_nov_2018_2020 <- ifelse(vs$gop_vote_2018==1 & vs$gop_vote_nov == 0,1,0)

vs$female <- factor(vs$gender_2020Sep,levels=c("Male","Female"))

vs$age <- 2020-vs$birthyr_2020Sep
#### Concern Only Additive Approval Model ####

# Lack Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model_sep <- glm(trump_approval_sep ~ sep_lack_covid_concern + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_se <- cluster.vcov(model_sep,vs$congressional_district)
model_sep_se <- sqrt(diag(model_sep_se))

# Lack Concern X Party -> Approval
summary(model_sep_int <- glm(trump_approval_sep ~ sep_lack_covid_concern*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_int_se <- cluster.vcov(model_sep_int,vs$congressional_district)
model_sep_int_se <- sqrt(diag(model_sep_int_se))

# Lack Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model_nov <- glm(trump_approval_nov ~ nov_lack_covid_concern + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_se <- cluster.vcov(model_nov,vs$congressional_district)
model_nov_se <- sqrt(diag(model_nov_se))

# Lack Concern X Party -> Approval
summary(model_nov_int <- glm(trump_approval_nov ~ nov_lack_covid_concern*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_int_se <- cluster.vcov(model_nov_int,vs$congressional_district)
model_nov_int_se <- sqrt(diag(model_nov_int_se))


#************************* Table ***********************
model_sep_se <- data.frame(model_sep_se)$model_sep_se
model_sep_int_se <- data.frame(model_sep_int_se)$model_sep_int_se
model_nov_se <- data.frame(model_nov_se)$model_nov_se
model_nov_int_se <- data.frame(model_nov_int_se)$model_nov_int_se

model_sep_est <- tidy(model_sep)$estimate
model_sep_int_est  <- tidy(model_sep_int)$estimate
model_nov_est <- tidy(model_nov)$estimate
model_nov_int_est <- tidy(model_nov_int)$estimate

#Key Stats
model_sep_N <- formatC(nrow(model_sep$model), format="d", big.mark=",")
model_sep_int_N <- formatC(nrow(model_sep_int$model), format="d", big.mark=",")
model_nov_N <- formatC(nrow(model_nov$model), format="d", big.mark=",")
model_nov_int_N <- formatC(nrow(model_nov_int$model), format="d", big.mark=",")

model_sep_LL <- formatC(round(logLik(model_sep)[1],3), digits = 3, format="f", big.mark=",")
model_sep_int_LL <- formatC(round(logLik(model_sep_int)[1],3), digits = 3, format="f", big.mark=",")
model_nov_LL <- formatC(round(logLik(model_nov)[1],3), digits = 3, format="f", big.mark=",")
model_nov_int_LL <- formatC(round(logLik(model_nov_int)[1],3), digits = 3, format="f", big.mark=",")

model_sep_AIC <- formatC(round(AIC(model_sep),3),digits = 3, format="f", big.mark=",")
model_sep_int_AIC <- formatC(round(AIC(model_sep_int),3), digits = 3, format="f", big.mark=",")
model_nov_AIC <- formatC(round(AIC(model_nov),3),digits = 3,  format="f", big.mark=",")
model_nov_int_AIC <- formatC(round(AIC(model_nov_int),3), digits = 3, format="f", big.mark=",")

#Final
M1 <- stargazer(list(model_sep,model_sep_int,
                     model_sep,model_sep_int),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
                type = "text",
                coef= list(model_sep_est,model_sep_int_est,model_nov_est,model_nov_int_est),
                se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),
                title="Voter Survey Logistic Regression Models Assessing President Trump COVID-19 Job Approval",
                dep.var.labels.include =FALSE,
                dep.var.caption="Dependent Variable: Trump COVID-19 Approval",
                single.row=T,
                notes =c("Robust district-clustered standard errors reported."),
                notes.align = "r",
                column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov.Interactive Model"),
                column.separate = c(1, 1),
                out = "Voter Survey Table M1.tex",
                
                omit.stat = c("all"),
                add.lines = list(c("Observations", model_sep_N, model_sep_int_N, model_nov_N, model_nov_int_N),
                                 c("Log Likelihood", model_sep_LL, model_sep_int_LL, model_nov_LL, model_nov_int_LL),
                                 c("Akaike Inf. Crit.", model_sep_AIC, model_sep_int_AIC, model_nov_AIC, model_nov_int_AIC)),
                
                covariate.labels=c("Lack of Covid Concern",
                                   "COVID-19 Deaths (100K People)", 
                                   "Independent Partisan",
                                   "Republican Partisan",
                                   "Conservative",
                                   "Latino Respondent",
                                   "Other Race Respondent",
                                   "White Respondent",
                                   "HS Education",
                                   "Some College Education",
                                   "BA Education",
                                   "Post-BA Education",
                                   "Household Income",
                                   "COVID-19 Infection",
                                   "Economic Evaluations",
                                   "Gender: Female",
                                   "Age",
                                   "Lack of Covid Concern x PID: Independent",
                                   "Lack of Covid Concern x PID: Republican", 
                                   "Constant"))




#### Concern Only Additive Vote Model ####

# Lack Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model_sep <- glm(trump_vote_sep ~ sep_lack_covid_concern + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_se <- cluster.vcov(model_sep,vs$congressional_district)
model_sep_se <- sqrt(diag(model_sep_se))

# Lack Concern X Party -> vote
summary(model_sep_int <- glm(trump_vote_sep ~ sep_lack_covid_concern*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_int_se <- cluster.vcov(model_sep_int,vs$congressional_district)
model_sep_int_se <- sqrt(diag(model_sep_int_se))

# Lack Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model_nov <- glm(trump_vote_nov ~ nov_lack_covid_concern + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_se <- cluster.vcov(model_nov,vs$congressional_district)
model_nov_se <- sqrt(diag(model_nov_se))

# Lack Concern X Party -> vote
summary(model_nov_int <- glm(trump_vote_nov ~ nov_lack_covid_concern*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_int_se <- cluster.vcov(model_nov_int,vs$congressional_district)
model_nov_int_se <- sqrt(diag(model_nov_int_se))

#************************* Table ***********************
model_sep_se <- data.frame(model_sep_se)$model_sep_se
model_sep_int_se <- data.frame(model_sep_int_se)$model_sep_int_se
model_nov_se <- data.frame(model_nov_se)$model_nov_se
model_nov_int_se <- data.frame(model_nov_int_se)$model_nov_int_se

model_sep_est <- tidy(model_sep)$estimate
model_sep_int_est  <- tidy(model_sep_int)$estimate
model_nov_est <- tidy(model_nov)$estimate
model_nov_int_est <- tidy(model_nov_int)$estimate

#Key Stats
model_sep_N <- formatC(nrow(model_sep$model), format="d", big.mark=",")
model_sep_int_N <- formatC(nrow(model_sep_int$model), format="d", big.mark=",")
model_nov_N <- formatC(nrow(model_nov$model), format="d", big.mark=",")
model_nov_int_N <- formatC(nrow(model_nov_int$model), format="d", big.mark=",")

model_sep_LL <- formatC(round(logLik(model_sep)[1],3), digits = 3, format="f", big.mark=",")
model_sep_int_LL <- formatC(round(logLik(model_sep_int)[1],3), digits = 3, format="f", big.mark=",")
model_nov_LL <- formatC(round(logLik(model_nov)[1],3), digits = 3, format="f", big.mark=",")
model_nov_int_LL <- formatC(round(logLik(model_nov_int)[1],3), digits = 3, format="f", big.mark=",")

model_sep_AIC <- formatC(round(AIC(model_sep),3),digits = 3, format="f", big.mark=",")
model_sep_int_AIC <- formatC(round(AIC(model_sep_int),3), digits = 3, format="f", big.mark=",")
model_nov_AIC <- formatC(round(AIC(model_nov),3),digits = 3,  format="f", big.mark=",")
model_nov_int_AIC <- formatC(round(AIC(model_nov_int),3), digits = 3, format="f", big.mark=",")

M2 <- stargazer(list(model_sep,model_sep_int,
                     model_sep,model_sep_int),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
                type = "text",
                coef= list(model_sep_est,model_sep_int_est,model_nov_est,model_nov_int_est),
                se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),
                title="Voter Survey Logistic Regression Models Assessing President Trump Electoral Preference",
                dep.var.labels.include =FALSE,
                dep.var.caption="Dependent Variable: Trump Vote Intention",
                notes =c("Robust district-clustered standard errors reported."),
                notes.align = "r",
                single.row=T,
                column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov. Interactive Model"),
                out = "Voter Survey Table M2.tex",
                
                omit.stat = c("all"),
                add.lines = list(c("Observations", model_sep_N, model_sep_int_N, model_nov_N, model_nov_int_N),
                                 c("Log Likelihood", model_sep_LL, model_sep_int_LL, model_nov_LL, model_nov_int_LL),
                                 c("Akaike Inf. Crit.", model_sep_AIC, model_sep_int_AIC, model_nov_AIC, model_nov_int_AIC)),
                
                
                covariate.labels=c("Lack of Covid Concern", 
                                   "COVID-19 Deaths (100K People)", 
                                   "Independent Partisan",
                                   "Republican Partisan",
                                   "Conservative",
                                   "Latino Respondent",
                                   "Other Race Respondent",
                                   "White Respondent",
                                   "HS Education",
                                   "Some College Education",
                                   "BA Education",
                                   "Post-BA Education",
                                   "Household Income",
                                   "COVID-19 Infection",
                                   "Economic Evaluations",
                                   "Gender: Female",
                                   "Age",
                                   "Lack of Covid Concern x PID: Independent",
                                   "Lack of Covid Concern x PID: Republican", 
                                   "Constant"))

#### Concern Only Additive Trump-Gain Vote-Change Model ####

# Lack Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model_sep <- glm(trump_vote_sep_2016_2020 ~  sep_lack_covid_concern + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_se <- cluster.vcov(model_sep,vs$congressional_district)
model_sep_se <- sqrt(diag(model_sep_se))

# Lack Concern X Party -> vote
summary(model_sep_int <- glm(trump_vote_sep_2016_2020 ~  sep_lack_covid_concern*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_int_se <- cluster.vcov(model_sep_int,vs$congressional_district)
model_sep_int_se <- sqrt(diag(model_sep_int_se))

# Lack Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model_nov <- glm(trump_vote_nov_2016_2020 ~ nov_lack_covid_concern + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_se <- cluster.vcov(model_nov,vs$congressional_district)
model_nov_se <- sqrt(diag(model_nov_se))

# Lack Concern X Party -> vote
summary(model_nov_int <- glm(trump_vote_nov_2016_2020 ~ nov_lack_covid_concern*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_int_se <- cluster.vcov(model_nov_int,vs$congressional_district)
model_nov_int_se <- sqrt(diag(model_nov_int_se))


#************************* Table ***********************
model_sep_se <- data.frame(model_sep_se)$model_sep_se
model_sep_int_se <- data.frame(model_sep_int_se)$model_sep_int_se
model_nov_se <- data.frame(model_nov_se)$model_nov_se
model_nov_int_se <- data.frame(model_nov_int_se)$model_nov_int_se

model_sep_est <- tidy(model_sep)$estimate
model_sep_int_est  <- tidy(model_sep_int)$estimate
model_nov_est <- tidy(model_nov)$estimate
model_nov_int_est <- tidy(model_nov_int)$estimate


#Key Stats
model_sep_N <- formatC(nrow(model_sep$model), format="d", big.mark=",")
model_sep_int_N <- formatC(nrow(model_sep_int$model), format="d", big.mark=",")
model_nov_N <- formatC(nrow(model_nov$model), format="d", big.mark=",")
model_nov_int_N <- formatC(nrow(model_nov_int$model), format="d", big.mark=",")

model_sep_LL <- formatC(round(logLik(model_sep)[1],3), digits = 3, format="f", big.mark=",")
model_sep_int_LL <- formatC(round(logLik(model_sep_int)[1],3), digits = 3, format="f", big.mark=",")
model_nov_LL <- formatC(round(logLik(model_nov)[1],3), digits = 3, format="f", big.mark=",")
model_nov_int_LL <- formatC(round(logLik(model_nov_int)[1],3), digits = 3, format="f", big.mark=",")

model_sep_AIC <- formatC(round(AIC(model_sep),3),digits = 3, format="f", big.mark=",")
model_sep_int_AIC <- formatC(round(AIC(model_sep_int),3), digits = 3, format="f", big.mark=",")
model_nov_AIC <- formatC(round(AIC(model_nov),3),digits = 3,  format="f", big.mark=",")
model_nov_int_AIC <- formatC(round(AIC(model_nov_int),3), digits = 3, format="f", big.mark=",")


M3 <- stargazer(list(model_sep,model_sep_int,
                     model_sep,model_sep_int),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
                type = "text",
                coef= list(model_sep_est,model_sep_int_est,model_nov_est,model_nov_int_est),
                se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),
                title="Voter Survey Logistic Regression Models Assessing 2016-2020 Electoral Preference Change",
                dep.var.labels.include =FALSE,
                dep.var.caption="Dependent Variable: 2016 HRC-2020 DJT Vote Switch",
                notes =c("Robust district-clustered standard errors reported."),
                notes.align = "r",
                single.row=T,
                column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov. Interactive Model"),
                out = "Voter Survey Table M3.tex",
                
                omit.stat = c("all"),
                add.lines = list(c("Observations", model_sep_N, model_sep_int_N, model_nov_N, model_nov_int_N),
                                 c("Log Likelihood", model_sep_LL, model_sep_int_LL, model_nov_LL, model_nov_int_LL),
                                 c("Akaike Inf. Crit.", model_sep_AIC, model_sep_int_AIC, model_nov_AIC, model_nov_int_AIC)),
                
                
                covariate.labels=c("Lack of Covid Concern", 
                                   "COVID-19 Deaths (100K People)", 
                                   "Independent Partisan",
                                   "Republican Partisan",
                                   "Conservative",
                                   "Latino Respondent",
                                   "Other Race Respondent",
                                   "White Respondent",
                                   "HS Education",
                                   "Some College Education",
                                   "BA Education",
                                   "Post-BA Education",
                                   "Household Income",
                                   "COVID-19 Infection",
                                   "Economic Evaluations",
                                   "Trump Vote 2016",
                                   "PID Change",
                                   "Gender: Female",
                                   "Age",
                                   "Lack of Covid Concern x PID: Independent",
                                   "Lack of Covid Concern x PID: Republican", 
                                   "Constant"))




#### Concern Only Additive Biden-Gain Vote-Change Model ####

# Lack Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model_sep <- glm(biden_vote_sep_2016_2020 ~  sep_lack_covid_concern + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_se <- cluster.vcov(model_sep,vs$congressional_district)
model_sep_se <- sqrt(diag(model_sep_se))

# Lack Concern X Party -> vote
summary(model_sep_int <- glm(biden_vote_sep_2016_2020 ~  sep_lack_covid_concern*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_int_se <- cluster.vcov(model_sep_int,vs$congressional_district)
model_sep_int_se <- sqrt(diag(model_sep_int_se))

# Lack Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model_nov <- glm(biden_vote_nov_2016_2020 ~ nov_lack_covid_concern + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_se <- cluster.vcov(model_nov,vs$congressional_district)
model_nov_se <- sqrt(diag(model_nov_se))

# Lack Concern X Party -> vote
summary(model_nov_int <- glm(biden_vote_nov_2016_2020 ~ nov_lack_covid_concern*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_int_se <- cluster.vcov(model_nov_int,vs$congressional_district)
model_nov_int_se <- sqrt(diag(model_nov_int_se))


#************************* Table ***********************
model_sep_se <- data.frame(model_sep_se)$model_sep_se
model_sep_int_se <- data.frame(model_sep_int_se)$model_sep_int_se
model_nov_se <- data.frame(model_nov_se)$model_nov_se
model_nov_int_se <- data.frame(model_nov_int_se)$model_nov_int_se

model_sep_est <- tidy(model_sep)$estimate
model_sep_int_est  <- tidy(model_sep_int)$estimate
model_nov_est <- tidy(model_nov)$estimate
model_nov_int_est <- tidy(model_nov_int)$estimate


#Key Stats
model_sep_N <- formatC(nrow(model_sep$model), format="d", big.mark=",")
model_sep_int_N <- formatC(nrow(model_sep_int$model), format="d", big.mark=",")
model_nov_N <- formatC(nrow(model_nov$model), format="d", big.mark=",")
model_nov_int_N <- formatC(nrow(model_nov_int$model), format="d", big.mark=",")

model_sep_LL <- formatC(round(logLik(model_sep)[1],3), digits = 3, format="f", big.mark=",")
model_sep_int_LL <- formatC(round(logLik(model_sep_int)[1],3), digits = 3, format="f", big.mark=",")
model_nov_LL <- formatC(round(logLik(model_nov)[1],3), digits = 3, format="f", big.mark=",")
model_nov_int_LL <- formatC(round(logLik(model_nov_int)[1],3), digits = 3, format="f", big.mark=",")

model_sep_AIC <- formatC(round(AIC(model_sep),3),digits = 3, format="f", big.mark=",")
model_sep_int_AIC <- formatC(round(AIC(model_sep_int),3), digits = 3, format="f", big.mark=",")
model_nov_AIC <- formatC(round(AIC(model_nov),3),digits = 3,  format="f", big.mark=",")
model_nov_int_AIC <- formatC(round(AIC(model_nov_int),3), digits = 3, format="f", big.mark=",")



M4 <- stargazer(list(model_sep,model_sep_int,
                     model_sep,model_sep_int),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
                type = "text",
                coef= list(model_sep_est,model_sep_int_est,model_nov_est,model_nov_int_est),
                se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),
                title="Voter Survey Logistic Regression Models Assessing 2016-2020 Electoral Preference Change",
                dep.var.labels.include = FALSE,
                dep.var.caption="Dependent Variable: 2016 DJT-2020 JRB Vote Switch",
                notes =c("Robust district-clustered standard errors reported.", "Missing standard errors are too high to report."),
                notes.align = "r",
                single.row=T,
                column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov. Interactive Model"),
                out = "Voter Survey Table M4.tex",
                
                omit.stat = c("all"),
                add.lines = list(c("Observations", model_sep_N, model_sep_int_N, model_nov_N, model_nov_int_N),
                                 c("Log Likelihood", model_sep_LL, model_sep_int_LL, model_nov_LL, model_nov_int_LL),
                                 c("Akaike Inf. Crit.", model_sep_AIC, model_sep_int_AIC, model_nov_AIC, model_nov_int_AIC)),
                
                
                covariate.labels=c("Lack of Covid Concern", 
                                   "COVID-19 Deaths (100K People)", 
                                   "Independent Partisan",
                                   "Republican Partisan",
                                   "Conservative",
                                   "Latino Respondent",
                                   "Other Race Respondent",
                                   "White Respondent",
                                   "HS Education",
                                   "Some College Education",
                                   "BA Education",
                                   "Post-BA Education",
                                   "Household Income",
                                   "COVID-19 Infection",
                                   "Economic Evaluations",
                                   "Trump Vote 2016",
                                   "PID Change",
                                   "Gender: Female",
                                   "Age",
                                   "Lack of Covid Concern x PID: Independent",
                                   "Lack of Covid Concern x PID: Republican", 
                                   "Constant"))

#### Concern & Lockdown Additive Approval Model ####

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model_sep <- glm(trump_approval_sep ~ sep_lack_covid_concern + lift_restrictions_too_slow_sep + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_se <- cluster.vcov(model_sep,vs$congressional_district)
model_sep_se <- sqrt(diag(model_sep_se))

# Heath Concern X Party -> Approval
summary(model_sep_int <- glm(trump_approval_sep ~ sep_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_sep*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_int_se <- cluster.vcov(model_sep_int,vs$congressional_district)
model_sep_int_se <- sqrt(diag(model_sep_int_se))

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model_nov <- glm(trump_approval_nov ~ nov_lack_covid_concern + lift_restrictions_too_slow_nov + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_se <- cluster.vcov(model_nov,vs$congressional_district)
model_nov_se <- sqrt(diag(model_nov_se))

# Heath Concern X Party -> Approval
summary(model_nov_int <- glm(trump_approval_nov ~ nov_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_nov*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_int_se <- cluster.vcov(model_nov_int,vs$congressional_district)
model_nov_int_se <- sqrt(diag(model_nov_int_se))


#************************* Table ***********************

model_sep_se <- data.frame(model_sep_se)$model_sep_se
model_sep_int_se <- data.frame(model_sep_int_se)$model_sep_int_se
model_nov_se <- data.frame(model_nov_se)$model_nov_se
model_nov_int_se <- data.frame(model_nov_int_se)$model_nov_int_se

model_sep_est <- tidy(model_sep)$estimate
model_sep_int_est  <- tidy(model_sep_int)$estimate
model_nov_est <- tidy(model_nov)$estimate
model_nov_int_est <- tidy(model_nov_int)$estimate

#Key Stats
model_sep_N <- formatC(nrow(model_sep$model), format="d", big.mark=",")
model_sep_int_N <- formatC(nrow(model_sep_int$model), format="d", big.mark=",")
model_nov_N <- formatC(nrow(model_nov$model), format="d", big.mark=",")
model_nov_int_N <- formatC(nrow(model_nov_int$model), format="d", big.mark=",")

model_sep_LL <- formatC(round(logLik(model_sep)[1],3), digits = 3, format="f", big.mark=",")
model_sep_int_LL <- formatC(round(logLik(model_sep_int)[1],3), digits = 3, format="f", big.mark=",")
model_nov_LL <- formatC(round(logLik(model_nov)[1],3), digits = 3, format="f", big.mark=",")
model_nov_int_LL <- formatC(round(logLik(model_nov_int)[1],3), digits = 3, format="f", big.mark=",")

model_sep_AIC <- formatC(round(AIC(model_sep),3),digits = 3, format="f", big.mark=",")
model_sep_int_AIC <- formatC(round(AIC(model_sep_int),3), digits = 3, format="f", big.mark=",")
model_nov_AIC <- formatC(round(AIC(model_nov),3),digits = 3,  format="f", big.mark=",")
model_nov_int_AIC <- formatC(round(AIC(model_nov_int),3), digits = 3, format="f", big.mark=",")


M6 <- stargazer(list(model_sep,model_sep_int,
                     model_sep,model_sep_int),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
                type = "text",
                coef= list(model_sep_est,model_sep_int_est,model_nov_est,model_nov_int_est),
                se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),
                title="Voter Survey Full Logistic Regression Models Assessing President Trump COVID-19 Job Approval",
                dep.var.labels.include =FALSE,
                dep.var.caption="Dependent Variable: Trump COVID-19 Approval",
                notes =c("Robust district-clustered standard errors reported."),
                notes.align = "r",
                single.row=T,
                out = "Voter Survey Table M6.tex",
                column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov. Interactive Model"),
                
                omit.stat = c("all"),
                add.lines = list(c("Observations", model_sep_N, model_sep_int_N, model_nov_N, model_nov_int_N),
                                 c("Log Likelihood", model_sep_LL, model_sep_int_LL, model_nov_LL, model_nov_int_LL),
                                 c("Akaike Inf. Crit.", model_sep_AIC, model_sep_int_AIC, model_nov_AIC, model_nov_int_AIC)),
                
                
                covariate.labels=c("Lack of Covid Concern", 
                                   "Lift Restrictions Too Slow",
                                   "COVID-19 Deaths (100K People)", 
                                   "Independent Partisan",
                                   "Republican Partisan",
                                   "Conservative",
                                   "Latino Respondent",
                                   "Other Race Respondent",
                                   "White Respondent",
                                   "HS Education",
                                   "Some College Education",
                                   "BA Education",
                                   "Post-BA Education",
                                   "Household Income",
                                   "COVID-19 Infection",
                                   "Economic Evaluations",
                                   "Gender: Female",
                                   "Age",
                                   "Lack of Covid Concern x PID: Independent",
                                   "Lack of Covid Concern x PID: Republican", 
                                   "Lift Restrictions Too Slow x PID: Independent",
                                   "Lift Restrictions Too Slow x PID: Republican", 
                                   "Constant"))

#### Concern & Lockdown Additive Vote Model ####

# Heath Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model_sep <- glm(trump_vote_sep ~ sep_lack_covid_concern + lift_restrictions_too_slow_sep + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_se <- cluster.vcov(model_sep,vs$congressional_district)
model_sep_se <- sqrt(diag(model_sep_se))

# Heath Concern X Party -> vote
summary(model_sep_int <- glm(trump_vote_sep ~ sep_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_sep*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_int_se <- cluster.vcov(model_sep_int,vs$congressional_district)
model_sep_int_se <- sqrt(diag(model_sep_int_se))

# Heath Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model_nov <- glm(trump_vote_nov ~ nov_lack_covid_concern + lift_restrictions_too_slow_nov + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_se <- cluster.vcov(model_nov,vs$congressional_district)
model_nov_se <- sqrt(diag(model_nov_se))

# Heath Concern X Party -> vote
summary(model_nov_int <- glm(trump_vote_nov ~ nov_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_nov*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_int_se <- cluster.vcov(model_nov_int,vs$congressional_district)
model_nov_int_se <- sqrt(diag(model_nov_int_se))

#************************* Table ***********************

model_sep_se <- data.frame(model_sep_se)$model_sep_se
model_sep_int_se <- data.frame(model_sep_int_se)$model_sep_int_se
model_nov_se <- data.frame(model_nov_se)$model_nov_se
model_nov_int_se <- data.frame(model_nov_int_se)$model_nov_int_se

model_sep_est <- tidy(model_sep)$estimate
model_sep_int_est  <- tidy(model_sep_int)$estimate
model_nov_est <- tidy(model_nov)$estimate
model_nov_int_est <- tidy(model_nov_int)$estimate

#Key Stats
model_sep_N <- formatC(nrow(model_sep$model), format="d", big.mark=",")
model_sep_int_N <- formatC(nrow(model_sep_int$model), format="d", big.mark=",")
model_nov_N <- formatC(nrow(model_nov$model), format="d", big.mark=",")
model_nov_int_N <- formatC(nrow(model_nov_int$model), format="d", big.mark=",")

model_sep_LL <- formatC(round(logLik(model_sep)[1],3), digits = 3, format="f", big.mark=",")
model_sep_int_LL <- formatC(round(logLik(model_sep_int)[1],3), digits = 3, format="f", big.mark=",")
model_nov_LL <- formatC(round(logLik(model_nov)[1],3), digits = 3, format="f", big.mark=",")
model_nov_int_LL <- formatC(round(logLik(model_nov_int)[1],3), digits = 3, format="f", big.mark=",")

model_sep_AIC <- formatC(round(AIC(model_sep),3),digits = 3, format="f", big.mark=",")
model_sep_int_AIC <- formatC(round(AIC(model_sep_int),3), digits = 3, format="f", big.mark=",")
model_nov_AIC <- formatC(round(AIC(model_nov),3),digits = 3,  format="f", big.mark=",")
model_nov_int_AIC <- formatC(round(AIC(model_nov_int),3), digits = 3, format="f", big.mark=",")


M7 <- stargazer(list(model_sep,model_sep_int,
                     model_sep,model_sep_int),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
                type = "text",
                coef= list(model_sep_est,model_sep_int_est,model_nov_est,model_nov_int_est),
                se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),
                title="Voter Survey Full Logistic Regression Models Assessing President Trump Electoral Preference",
                dep.var.labels.include =FALSE,
                dep.var.caption="Dependent Variable: Trump Vote Intention",
                notes =c("Robust district-clustered standard errors reported."),
                notes.align = "r",
                single.row=T,
                out = "Voter Survey Table M7.tex",
                column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov. Interactive Model"),
                
                omit.stat = c("all"),
                add.lines = list(c("Observations", model_sep_N, model_sep_int_N, model_nov_N, model_nov_int_N),
                                 c("Log Likelihood", model_sep_LL, model_sep_int_LL, model_nov_LL, model_nov_int_LL),
                                 c("Akaike Inf. Crit.", model_sep_AIC, model_sep_int_AIC, model_nov_AIC, model_nov_int_AIC)),
                
                
                covariate.labels=c("Lack of Covid Concern", 
                                   "Lift Restrictions Too Slow",
                                   "COVID-19 Deaths (100K People)", 
                                   "Independent Partisan",
                                   "Republican Partisan",
                                   "Conservative",
                                   "Latino Respondent",
                                   "Other Race Respondent",
                                   "White Respondent",
                                   "HS Education",
                                   "Some College Education",
                                   "BA Education",
                                   "Post-BA Education",
                                   "Household Income",
                                   "COVID-19 Infection",
                                   "Economic Evaluations",
                                   "Gender: Female",
                                   "Age",
                                   "Lack of Covid Concern x PID: Independent",
                                   "Lack of Covid Concern x PID: Republican", 
                                   "Lift Restrictions Too Slow x PID: Independent",
                                   "Lift Restrictions Too Slow x PID: Republican", 
                                   "Constant"))



#### Concern & Lockdown Additive Trump-Gain Vote-Change Model ####

# Heath Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model_sep <- glm(trump_vote_sep_2016_2020 ~  sep_lack_covid_concern + lift_restrictions_too_slow_sep + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_se <- cluster.vcov(model_sep,vs$congressional_district)
model_sep_se <- sqrt(diag(model_sep_se))

# Heath Concern X Party -> vote
summary(model_sep_int <- glm(trump_vote_sep_2016_2020 ~  sep_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_sep*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_int_se <- cluster.vcov(model_sep_int,vs$congressional_district)
model_sep_int_se <- sqrt(diag(model_sep_int_se))

# Heath Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model_nov <- glm(trump_vote_nov_2016_2020 ~ nov_lack_covid_concern + lift_restrictions_too_slow_nov + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_se <- cluster.vcov(model_nov,vs$congressional_district)
model_nov_se <- sqrt(diag(model_nov_se))

# Heath Concern X Party -> vote
summary(model_nov_int <- glm(trump_vote_nov_2016_2020 ~ nov_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_nov*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_int_se <- cluster.vcov(model_nov_int,vs$congressional_district)
model_nov_int_se <- sqrt(diag(model_nov_int_se))

#************************* Table ***********************

model_sep_se <- data.frame(model_sep_se)$model_sep_se
model_sep_int_se <- data.frame(model_sep_int_se)$model_sep_int_se
model_nov_se <- data.frame(model_nov_se)$model_nov_se
model_nov_int_se <- data.frame(model_nov_int_se)$model_nov_int_se

model_sep_est <- tidy(model_sep)$estimate
model_sep_int_est  <- tidy(model_sep_int)$estimate
model_nov_est <- tidy(model_nov)$estimate
model_nov_int_est <- tidy(model_nov_int)$estimate


#Key Stats
model_sep_N <- formatC(nrow(model_sep$model), format="d", big.mark=",")
model_sep_int_N <- formatC(nrow(model_sep_int$model), format="d", big.mark=",")
model_nov_N <- formatC(nrow(model_nov$model), format="d", big.mark=",")
model_nov_int_N <- formatC(nrow(model_nov_int$model), format="d", big.mark=",")

model_sep_LL <- formatC(round(logLik(model_sep)[1],3), digits = 3, format="f", big.mark=",")
model_sep_int_LL <- formatC(round(logLik(model_sep_int)[1],3), digits = 3, format="f", big.mark=",")
model_nov_LL <- formatC(round(logLik(model_nov)[1],3), digits = 3, format="f", big.mark=",")
model_nov_int_LL <- formatC(round(logLik(model_nov_int)[1],3), digits = 3, format="f", big.mark=",")

model_sep_AIC <- formatC(round(AIC(model_sep),3),digits = 3, format="f", big.mark=",")
model_sep_int_AIC <- formatC(round(AIC(model_sep_int),3), digits = 3, format="f", big.mark=",")
model_nov_AIC <- formatC(round(AIC(model_nov),3),digits = 3,  format="f", big.mark=",")
model_nov_int_AIC <- formatC(round(AIC(model_nov_int),3), digits = 3, format="f", big.mark=",")

M8 <- stargazer(list(model_sep,model_sep_int,
                     model_sep,model_sep_int),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
                type = "text",
                coef= list(model_sep_est,model_sep_int_est,model_nov_est,model_nov_int_est),
                se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),
                title="Voter Survey Full Logistic Regression Models Assessing 2016-2020 Electoral Preference Change",
                dep.var.labels.include =FALSE,
                dep.var.caption="Dependent Variable: 2016 HRC-2020 DJT Vote Switch",
                notes =c("Robust district-clustered standard errors reported.", "Missing standard errors are too high to report."),
                notes.align = "r",
                single.row=T,
                out = "Voter Survey Table M8.tex",
                column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov. Interactive Model"),
                
                omit.stat = c("all"),
                add.lines = list(c("Observations", model_sep_N, model_sep_int_N, model_nov_N, model_nov_int_N),
                                 c("Log Likelihood", model_sep_LL, model_sep_int_LL, model_nov_LL, model_nov_int_LL),
                                 c("Akaike Inf. Crit.", model_sep_AIC, model_sep_int_AIC, model_nov_AIC, model_nov_int_AIC)),
                
                
                covariate.labels=c("Lack of Covid Concern", 
                                   "Lift Restrictions Too Slow",
                                   "COVID-19 Deaths (100K People)", 
                                   "Independent Partisan",
                                   "Republican Partisan",
                                   "Conservative",
                                   "Latino Respondent",
                                   "Other Race Respondent",
                                   "White Respondent",
                                   "HS Education",
                                   "Some College Education",
                                   "BA Education",
                                   "Post-BA Education",
                                   "Household Income",
                                   "COVID-19 Infection",
                                   "Economic Evaluations",
                                   "Trump Vote 2016",
                                   "PID Change",
                                   "Gender: Female",
                                   "Age",
                                   "Lack of Covid Concern x PID: Independent",
                                   "Lack of Covid Concern x PID: Republican", 
                                   "Lift Restrictions Too Slow x PID: Independent",
                                   "Lift Restrictions Too Slow x PID: Republican", 
                                   "Constant"))



#### Concern & Lockdown Additive Biden-Gain Vote-Change Model ####

# Heath Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model_sep <- glm(biden_vote_sep_2016_2020 ~  sep_lack_covid_concern + lift_restrictions_too_slow_sep + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_se <- cluster.vcov(model_sep,vs$congressional_district)
model_sep_se <- sqrt(diag(model_sep_se))

# Heath Concern X Party -> vote
summary(model_sep_int <- glm(biden_vote_sep_2016_2020 ~  sep_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_sep*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_int_se <- cluster.vcov(model_sep_int,vs$congressional_district)
model_sep_int_se <- sqrt(diag(model_sep_int_se))

# Heath Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model_nov <- glm(biden_vote_nov_2016_2020 ~ nov_lack_covid_concern + lift_restrictions_too_slow_nov + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_se <- cluster.vcov(model_nov,vs$congressional_district)
model_nov_se <- sqrt(diag(model_nov_se))

# Heath Concern X Party -> vote
summary(model_nov_int <- glm(biden_vote_nov_2016_2020 ~ nov_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_nov*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + trump_vote_2016 + pid_change_2016_2020+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_int_se <- cluster.vcov(model_nov_int,vs$congressional_district)
model_nov_int_se <- sqrt(diag(model_nov_int_se))

stargazer(model_sep,model_sep_int,model_nov,model_nov_int,se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),title="Voter Survey Full Logistic Regression Models Assessing 2016-2020 Electoral Preference Change",dep.var.labels.include =FALSE,dep.var.caption="Dependent Variable: 2016 DJT-2020 JRB Vote Switch",notes.label="Robust district-clustered standard errors reported.",single.row=T,column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov. Interactive Model"))

#************************* Table ***********************

model_sep_se <- data.frame(model_sep_se)$model_sep_se
model_sep_int_se <- data.frame(model_sep_int_se)$model_sep_int_se
model_nov_se <- data.frame(model_nov_se)$model_nov_se
model_nov_int_se <- data.frame(model_nov_int_se)$model_nov_int_se

model_sep_est <- tidy(model_sep)$estimate
model_sep_int_est  <- tidy(model_sep_int)$estimate
model_nov_est <- tidy(model_nov)$estimate
model_nov_int_est <- tidy(model_nov_int)$estimate

#Key Stats
model_sep_N <- formatC(nrow(model_sep$model), format="d", big.mark=",")
model_sep_int_N <- formatC(nrow(model_sep_int$model), format="d", big.mark=",")
model_nov_N <- formatC(nrow(model_nov$model), format="d", big.mark=",")
model_nov_int_N <- formatC(nrow(model_nov_int$model), format="d", big.mark=",")

model_sep_LL <- formatC(round(logLik(model_sep)[1],3), digits = 3, format="f", big.mark=",")
model_sep_int_LL <- formatC(round(logLik(model_sep_int)[1],3), digits = 3, format="f", big.mark=",")
model_nov_LL <- formatC(round(logLik(model_nov)[1],3), digits = 3, format="f", big.mark=",")
model_nov_int_LL <- formatC(round(logLik(model_nov_int)[1],3), digits = 3, format="f", big.mark=",")

model_sep_AIC <- formatC(round(AIC(model_sep),3),digits = 3, format="f", big.mark=",")
model_sep_int_AIC <- formatC(round(AIC(model_sep_int),3), digits = 3, format="f", big.mark=",")
model_nov_AIC <- formatC(round(AIC(model_nov),3),digits = 3,  format="f", big.mark=",")
model_nov_int_AIC <- formatC(round(AIC(model_nov_int),3), digits = 3, format="f", big.mark=",")

M9 <- stargazer(list(model_sep,model_sep_int,
                     model_sep,model_sep_int),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
                type = "text",
                coef= list(model_sep_est,model_sep_int_est,model_nov_est,model_nov_int_est),
                se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),
                title="Voter Survey Full Logistic Regression Models Assessing 2016-2020 Electoral Preference Change",
                dep.var.labels.include =FALSE,
                dep.var.caption="Dependent Variable: 2016 DJT-2020 JRB Vote Switch",
                notes =c("Robust district-clustered standard errors reported."),
                notes.align = "r",
                single.row=T,
                out = "Voter Survey Table M9.tex",
                column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov. Interactive Model"),
                
                omit.stat = c("all"),
                add.lines = list(c("Observations", model_sep_N, model_sep_int_N, model_nov_N, model_nov_int_N),
                                 c("Log Likelihood", model_sep_LL, model_sep_int_LL, model_nov_LL, model_nov_int_LL),
                                 c("Akaike Inf. Crit.", model_sep_AIC, model_sep_int_AIC, model_nov_AIC, model_nov_int_AIC)),
                
                
                covariate.labels=c("Lack of Covid Concern", 
                                   "Lift Restrictions Too Slow",
                                   "COVID-19 Deaths (100K People)", 
                                   "Independent Partisan",
                                   "Republican Partisan",
                                   "Conservative",
                                   "Latino Respondent",
                                   "Other Race Respondent",
                                   "White Respondent",
                                   "HS Education",
                                   "Some College Education",
                                   "BA Education",
                                   "Post-BA Education",
                                   "Household Income",
                                   "COVID-19 Infection",
                                   "Economic Evaluations",
                                   "Trump Vote 2016",
                                   "PID Change",
                                   "Gender: Female",
                                   "Age",
                                   "Lack of Covid Concern x PID: Independent",
                                   "Lack of Covid Concern x PID: Republican", 
                                   "Lift Restrictions Too Slow x PID: Independent",
                                   "Lift Restrictions Too Slow x PID: Republican", 
                                   "Constant"))

#### Modeling Heath Concern ####

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model_sep <- glm(sep_lack_covid_concern ~ sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_se <- cluster.vcov(model_sep,vs$congressional_district)
model_sep_se <- sqrt(diag(model_sep_se))

# Heath Concern X Party -> Approval
summary(model_sep_int <- glm(sep_lack_covid_concern ~ sep_district_covid_deaths_per100k*pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_int_se <- cluster.vcov(model_sep_int,vs$congressional_district)
model_sep_int_se <- sqrt(diag(model_sep_int_se))

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model_nov <- glm(nov_lack_covid_concern ~  nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_se <- cluster.vcov(model_nov,vs$congressional_district)
model_nov_se <- sqrt(diag(model_nov_se))

# Heath Concern X Party -> Approval
summary(model_nov_int <- glm(nov_lack_covid_concern ~ nov_district_covid_deaths_per100k*pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_int_se <- cluster.vcov(model_nov_int,vs$congressional_district)
model_nov_int_se <- sqrt(diag(model_nov_int_se))

#************************* Table ***********************

model_sep_se <- data.frame(model_sep_se)$model_sep_se
model_sep_int_se <- data.frame(model_sep_int_se)$model_sep_int_se
model_nov_se <- data.frame(model_nov_se)$model_nov_se
model_nov_int_se <- data.frame(model_nov_int_se)$model_nov_int_se

model_sep_est <- tidy(model_sep)$estimate
model_sep_int_est  <- tidy(model_sep_int)$estimate
model_nov_est <- tidy(model_nov)$estimate
model_nov_int_est <- tidy(model_nov_int)$estimate


#Key Stats
model_sep_N <- formatC(nrow(model_sep$model), format="d", big.mark=",")
model_sep_int_N <- formatC(nrow(model_sep_int$model), format="d", big.mark=",")
model_nov_N <- formatC(nrow(model_nov$model), format="d", big.mark=",")
model_nov_int_N <- formatC(nrow(model_nov_int$model), format="d", big.mark=",")

model_sep_LL <- formatC(round(logLik(model_sep)[1],3), digits = 3, format="f", big.mark=",")
model_sep_int_LL <- formatC(round(logLik(model_sep_int)[1],3), digits = 3, format="f", big.mark=",")
model_nov_LL <- formatC(round(logLik(model_nov)[1],3), digits = 3, format="f", big.mark=",")
model_nov_int_LL <- formatC(round(logLik(model_nov_int)[1],3), digits = 3, format="f", big.mark=",")

model_sep_AIC <- formatC(round(AIC(model_sep),3),digits = 3, format="f", big.mark=",")
model_sep_int_AIC <- formatC(round(AIC(model_sep_int),3), digits = 3, format="f", big.mark=",")
model_nov_AIC <- formatC(round(AIC(model_nov),3),digits = 3,  format="f", big.mark=",")
model_nov_int_AIC <- formatC(round(AIC(model_nov_int),3), digits = 3, format="f", big.mark=",")


M11 <- stargazer(list(model_sep,model_sep_int,
                      model_sep,model_sep_int),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
                 type = "text",
                 coef= list(model_sep_est,model_sep_int_est,model_nov_est,model_nov_int_est),
                 se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),
                 title="Voter Survey Full Logistic Regression Models Assessing COVID Public Health Concern",
                 dep.var.labels.include =FALSE,
                 dep.var.caption="Dependent Variable: COVID Health Concern",
                 notes =c("Robust district-clustered standard errors reported."),
                 notes.align = "r",
                 single.row=T,
                 out = "Voter Survey Table M11.tex",
                 column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov. Interactive Model"),
                 
                 omit.stat = c("all"),
                 add.lines = list(c("Observations", model_sep_N, model_sep_int_N, model_nov_N, model_nov_int_N),
                                  c("Log Likelihood", model_sep_LL, model_sep_int_LL, model_nov_LL, model_nov_int_LL),
                                  c("Akaike Inf. Crit.", model_sep_AIC, model_sep_int_AIC, model_nov_AIC, model_nov_int_AIC)),
                 
                 
                 covariate.labels=c("COVID-19 Deaths (100K People)", 
                                    "Independent Partisan",
                                    "Republican Partisan",
                                    "Conservative",
                                    "Latino Respondent",
                                    "Other Race Respondent",
                                    "White Respondent",
                                    "HS Education",
                                    "Some College Education",
                                    "BA Education",
                                    "Post-BA Education",
                                    "Household Income",
                                    "COVID-19 Infection",
                                    "Gender: Female",
                                    "Age",
                                    "Economic Evaluations",
                                    "COVID-19 Deaths (100K People) x PID: Independent",
                                    "COVID-19 Deaths (100K People) x PID: Republican", 
                                    "Constant"))


#### Modeling Lockdown Worry ####

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model_sep <- glm(lift_restrictions_too_slow_sep ~ sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_se <- cluster.vcov(model_sep,vs$congressional_district)
model_sep_se <- sqrt(diag(model_sep_se))

# Heath Concern X Party -> Approval
summary(model_sep_int <- glm(lift_restrictions_too_slow_sep ~ sep_district_covid_deaths_per100k*pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep+ female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))
model_sep_int_se <- cluster.vcov(model_sep_int,vs$congressional_district)
model_sep_int_se <- sqrt(diag(model_sep_int_se))

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model_nov <- glm(lift_restrictions_too_slow_nov ~  nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_se <- cluster.vcov(model_nov,vs$congressional_district)
model_nov_se <- sqrt(diag(model_nov_se))

# Heath Concern X Party -> Approval
summary(model_nov_int <- glm(lift_restrictions_too_slow_nov ~ nov_district_covid_deaths_per100k*pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov+ female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))
model_nov_int_se <- cluster.vcov(model_nov_int,vs$congressional_district)
model_nov_int_se <- sqrt(diag(model_nov_int_se))

#************************* Table ***********************

model_sep_se <- data.frame(model_sep_se)$model_sep_se
model_sep_int_se <- data.frame(model_sep_int_se)$model_sep_int_se
model_nov_se <- data.frame(model_nov_se)$model_nov_se
model_nov_int_se <- data.frame(model_nov_int_se)$model_nov_int_se

model_sep_est <- tidy(model_sep)$estimate
model_sep_int_est  <- tidy(model_sep_int)$estimate
model_nov_est <- tidy(model_nov)$estimate
model_nov_int_est <- tidy(model_nov_int)$estimate


#Key Stats
model_sep_N <- formatC(nrow(model_sep$model), format="d", big.mark=",")
model_sep_int_N <- formatC(nrow(model_sep_int$model), format="d", big.mark=",")
model_nov_N <- formatC(nrow(model_nov$model), format="d", big.mark=",")
model_nov_int_N <- formatC(nrow(model_nov_int$model), format="d", big.mark=",")

model_sep_LL <- formatC(round(logLik(model_sep)[1],3), digits = 3, format="f", big.mark=",")
model_sep_int_LL <- formatC(round(logLik(model_sep_int)[1],3), digits = 3, format="f", big.mark=",")
model_nov_LL <- formatC(round(logLik(model_nov)[1],3), digits = 3, format="f", big.mark=",")
model_nov_int_LL <- formatC(round(logLik(model_nov_int)[1],3), digits = 3, format="f", big.mark=",")

model_sep_AIC <- formatC(round(AIC(model_sep),3),digits = 3, format="f", big.mark=",")
model_sep_int_AIC <- formatC(round(AIC(model_sep_int),3), digits = 3, format="f", big.mark=",")
model_nov_AIC <- formatC(round(AIC(model_nov),3),digits = 3,  format="f", big.mark=",")
model_nov_int_AIC <- formatC(round(AIC(model_nov_int),3), digits = 3, format="f", big.mark=",")

M12 <- stargazer(list(model_sep,model_sep_int,
                      model_sep,model_sep_int),  #model_nov_est, model_nov_int_est -- placeholders to get on one line.
                 type = "text",
                 coef= list(model_sep_est,model_sep_int_est,model_nov_est,model_nov_int_est),
                 se= list(model_sep_se,model_sep_int_se,model_nov_se,model_nov_int_se),
                 title="Voter Survey Full Logistic Regression Models Assessing COVID Restrictions Concern",
                 dep.var.labels.include =FALSE,
                 dep.var.caption="Dependent Variable: COVID Restrictions Too Slowly Concern",
                 notes =c("Robust district-clustered standard errors reported."),
                 notes.align = "r",
                 single.row=T,
                 out = "Voter Survey Table M12.tex",
                 column.labels = c("Sept. Additive Model","Sept. Interactive Model","Nov. Additive Model","Nov. Interactive Model"),
                 
                 omit.stat = c("all"),
                 add.lines = list(c("Observations", model_sep_N, model_sep_int_N, model_nov_N, model_nov_int_N),
                                  c("Log Likelihood", model_sep_LL, model_sep_int_LL, model_nov_LL, model_nov_int_LL),
                                  c("Akaike Inf. Crit.", model_sep_AIC, model_sep_int_AIC, model_nov_AIC, model_nov_int_AIC)),
                 
                 
                 covariate.labels=c("COVID-19 Deaths (100K People)", 
                                    "Independent Partisan",
                                    "Republican Partisan",
                                    "Conservative",
                                    "Latino Respondent",
                                    "Other Race Respondent",
                                    "White Respondent",
                                    "HS Education",
                                    "Some College Education",
                                    "BA Education",
                                    "Post-BA Education",
                                    "Household Income",
                                    "COVID-19 Infection",
                                    "Economic Evaluations",
                                    "Gender: Female",
                                    "Age",
                                    "COVID-19 Deaths (100K People) x PID: Independent",
                                    "COVID-19 Deaths (100K People) x PID: Republican", 
                                    "Constant"))


########################## Final Tables ########################
cat("\f")
M1 %>% paste(., collapse = "\n") %>% cat("\n")
M2 %>% paste(., collapse = "\n") %>% cat("\n")
M3 %>% paste(., collapse = "\n") %>% cat("\n")
M4 %>% paste(., collapse = "\n") %>% cat("\n")
M6 %>% paste(., collapse = "\n") %>% cat("\n")
M7 %>% paste(., collapse = "\n") %>% cat("\n")
M8 %>% paste(., collapse = "\n") %>% cat("\n")
M9 %>% paste(., collapse = "\n") %>% cat("\n")
M11 %>% paste(., collapse = "\n") %>% cat("\n")
M12 %>% paste(., collapse = "\n") %>% cat("\n")

#Then add #[-1.8ex] to the model names section

