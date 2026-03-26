# The Following R Code replicates the following items in the manuscript titled: "Nail in the Coffin or Lifeline? Evaluating the Electoral Impact of COVID-19 on President Trump in the 2020 Election"". The items are the following:

# 1) Analysis of the September & November waves of the Voter Study data for Figure 4: Relationship Between Public Health Concern & Political Support for President Trump

# 2) Analysis of the September & November waves of the Voter Study data for Figure 5: Relationship Between Local Severity Context & COVID-19 Attitudes

# 3) Analysis of the September & November waves of the Voter Study data for Figure 6: Relationship Between Public Health Concern/Restrictions Concern & Political Support for President Trump

library(readstata13)
library(margins)
library(ggplot2)
library(multiwayvcov)
library(descr)
library(forcats)

#### Data Wrangling ####

#Set Local Working Directory
setwd("/Users/caalgara/Dropbox/2020 Covid Election Project/PB_Revise_Resubmit/Final PB Data Files")

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
summary(model <- glm(trump_approval_sep ~ sep_lack_covid_concern + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_additive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_sep$lower_90 <- margins_model1_additive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 
margins_model1_additive_sep$upper_90 <- margins_model1_additive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 

# Lack Concern X Party -> Approval
summary(model <- glm(trump_approval_sep ~ sep_lack_covid_concern*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_interactive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_sep$lower_90 <- margins_model1_interactive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 
margins_model1_interactive_sep$upper_90 <- margins_model1_interactive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 

# Lack Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_approval_nov ~ nov_lack_covid_concern + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_additive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_nov$lower_90 <- margins_model1_additive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 
margins_model1_additive_nov$upper_90 <- margins_model1_additive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 

# Lack Concern X Party -> Approval
summary(model <- glm(trump_approval_nov ~ nov_lack_covid_concern*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_interactive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_nov$lower_90 <- margins_model1_interactive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 
margins_model1_interactive_nov$upper_90 <- margins_model1_interactive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 

margins_model1_additive_sep$wave <- "September"
margins_model1_additive_nov$wave <- "November"

margins_additive_concern_only_approval <- rbind(margins_model1_additive_sep,margins_model1_additive_nov)

margins_model1_interactive_sep$wave <- "September"
margins_model1_interactive_nov$wave <- "November"

margins_interactive_concern_only_approval <- rbind(margins_model1_interactive_sep,margins_model1_interactive_nov)

rm(margins_model1_additive_sep,margins_model1_additive_nov,margins_model1_interactive_sep,margins_model1_interactive_nov,x,model,i)

#### Concern Only Additive Vote Model ####

# Lack Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_vote_sep ~ sep_lack_covid_concern + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_additive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_sep$lower_90 <- margins_model1_additive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 
margins_model1_additive_sep$upper_90 <- margins_model1_additive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 

# Lack Concern X Party -> vote
summary(model <- glm(trump_vote_sep ~ sep_lack_covid_concern*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_interactive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_sep$lower_90 <- margins_model1_interactive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 
margins_model1_interactive_sep$upper_90 <- margins_model1_interactive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 

# Lack Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_vote_nov ~ nov_lack_covid_concern + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_additive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_nov$lower_90 <- margins_model1_additive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 
margins_model1_additive_nov$upper_90 <- margins_model1_additive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 

# Lack Concern X Party -> vote
summary(model <- glm(trump_vote_nov ~ nov_lack_covid_concern*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_interactive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_nov$lower_90 <- margins_model1_interactive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 
margins_model1_interactive_nov$upper_90 <- margins_model1_interactive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 

margins_model1_additive_sep$wave <- "September"
margins_model1_additive_nov$wave <- "November"

margins_additive_concern_only_vote <- rbind(margins_model1_additive_sep,margins_model1_additive_nov)

margins_model1_interactive_sep$wave <- "September"
margins_model1_interactive_nov$wave <- "November"

margins_interactive_concern_only_vote <- rbind(margins_model1_interactive_sep,margins_model1_interactive_nov)

rm(margins_model1_additive_sep,margins_model1_additive_nov,margins_model1_interactive_sep,margins_model1_interactive_nov,x,model,i)

#### Concern Only Additive Trump-Gain Vote-Change Model ####

# Lack Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_vote_sep_2016_2020 ~  sep_lack_covid_concern + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_additive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_sep$lower_90 <- margins_model1_additive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 
margins_model1_additive_sep$upper_90 <- margins_model1_additive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 

# Lack Concern X Party -> vote
summary(model <- glm(trump_vote_sep_2016_2020 ~  sep_lack_covid_concern*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_interactive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_sep$lower_90 <- margins_model1_interactive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 
margins_model1_interactive_sep$upper_90 <- margins_model1_interactive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 

# Lack Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_vote_nov_2016_2020 ~ nov_lack_covid_concern + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_additive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_nov$lower_90 <- margins_model1_additive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 
margins_model1_additive_nov$upper_90 <- margins_model1_additive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 

# Lack Concern X Party -> vote
summary(model <- glm(trump_vote_nov_2016_2020 ~ nov_lack_covid_concern*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_interactive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_nov$lower_90 <- margins_model1_interactive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 
margins_model1_interactive_nov$upper_90 <- margins_model1_interactive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 

margins_model1_additive_sep$wave <- "September"
margins_model1_additive_nov$wave <- "November"

margins_additive_concern_only_vote_change_trump <- rbind(margins_model1_additive_sep,margins_model1_additive_nov)

margins_model1_interactive_sep$wave <- "September"
margins_model1_interactive_nov$wave <- "November"

margins_interactive_concern_only_vote_change_trump <- rbind(margins_model1_interactive_sep,margins_model1_interactive_nov)

rm(margins_model1_additive_sep,margins_model1_additive_nov,margins_model1_interactive_sep,margins_model1_interactive_nov,x,model,i)

#### Concern Only Additive Biden-Gain Vote-Change Model ####

# Lack Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(biden_vote_sep_2016_2020 ~  sep_lack_covid_concern + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_additive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_sep$lower_90 <- margins_model1_additive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 
margins_model1_additive_sep$upper_90 <- margins_model1_additive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 

# Lack Concern X Party -> vote
summary(model <- glm(biden_vote_sep_2016_2020 ~  sep_lack_covid_concern*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_interactive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_sep$lower_90 <- margins_model1_interactive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 
margins_model1_interactive_sep$upper_90 <- margins_model1_interactive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 

# Lack Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(biden_vote_nov_2016_2020 ~ nov_lack_covid_concern + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_additive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_nov$lower_90 <- margins_model1_additive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 
margins_model1_additive_nov$upper_90 <- margins_model1_additive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 

# Lack Concern X Party -> vote
summary(model <- glm(biden_vote_nov_2016_2020 ~ nov_lack_covid_concern*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_interactive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_nov$lower_90 <- margins_model1_interactive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 
margins_model1_interactive_nov$upper_90 <- margins_model1_interactive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 

margins_model1_additive_sep$wave <- "September"
margins_model1_additive_nov$wave <- "November"

margins_additive_concern_only_vote_change_biden <- rbind(margins_model1_additive_sep,margins_model1_additive_nov)

margins_model1_interactive_sep$wave <- "September"
margins_model1_interactive_nov$wave <- "November"

margins_interactive_concern_only_vote_change_biden <- rbind(margins_model1_interactive_sep,margins_model1_interactive_nov)

rm(margins_model1_additive_sep,margins_model1_additive_nov,margins_model1_interactive_sep,margins_model1_interactive_nov,x,model,i)

#### Concern Only Figure: Presidential ####

margins_additive_concern_only_approval$dv <- "DV: Trump COVID-19 Approval"
margins_additive_concern_only_approval$pid3_coded <- "Baseline" 
margins_interactive_concern_only_approval$dv <- "DV: Trump COVID-19 Approval"

margins_additive_concern_only_vote$dv <- "DV: Trump Vote"
margins_additive_concern_only_vote$pid3_coded <- "Baseline" 
margins_interactive_concern_only_vote$dv <- "DV: Trump Vote"

margins_additive_concern_only_vote_change_biden$dv <- "DV: Trump 2016-Biden 2020 Switch"
margins_additive_concern_only_vote_change_biden$pid3_coded <- "Baseline"
margins_interactive_concern_only_vote_change_biden$dv <- "DV: Trump 2016-Biden 2020 Switch"

margins_additive_concern_only_vote_change_trump$dv <- "DV: Clinton 2016-Trump 2020 Switch"
margins_additive_concern_only_vote_change_trump$pid3_coded <- "Baseline"
margins_interactive_concern_only_vote_change_trump$dv <- "DV: Clinton 2016-Trump 2020 Switch"

x <- rbind(margins_additive_concern_only_approval,margins_additive_concern_only_vote,margins_additive_concern_only_vote_change_biden,margins_additive_concern_only_vote_change_trump,margins_interactive_concern_only_approval,margins_interactive_concern_only_vote,margins_interactive_concern_only_vote_change_biden,margins_interactive_concern_only_vote_change_trump)

x$facet_label <- paste(x$wave," Panel Wave",paste(" \n",x$dv,sep=""),sep="")
x <- subset(x,x$factor %in% c("nov_lack_covid_concern","sep_lack_covid_concern"))
x$variable <- ifelse(x$factor %in% c("lift_restrictions_too_slow_nov","lift_restrictions_too_slow_sep"),"Restrictions Concern",ifelse(x$factor %in% c("nov_lack_covid_concern","sep_lack_covid_concern"),"Health Concern",NA))

x$shape <- ifelse(x$variable == "Health Concern",22,21)
x$color_sig <- ifelse(x$p < 0.10,"sig","no sig")
x$facet_label <- factor(x$facet_label,levels=c("September Panel Wave \nDV: Trump COVID-19 Approval","November Panel Wave \nDV: Trump COVID-19 Approval","September Panel Wave \nDV: Trump Vote","November Panel Wave \nDV: Trump Vote","September Panel Wave \nDV: Clinton 2016-Trump 2020 Switch","November Panel Wave \nDV: Clinton 2016-Trump 2020 Switch","September Panel Wave \nDV: Trump 2016-Biden 2020 Switch","November Panel Wave \nDV: Trump 2016-Biden 2020 Switch"))

plot <- ggplot(x,aes(x=pid3_coded,y=AME,group=variable,alpha=color_sig)) + theme_bw() + facet_wrap(~facet_label,ncol = 2) + geom_linerange(aes(x= pid3_coded, ymin = lower_90, ymax = upper_90), lwd  = 1,position = position_dodge(width=0.5)) + geom_pointrange(aes(x= pid3_coded, ymin = lower, ymax = upper,shape=variable,fill=variable), lwd = 1/2,position = position_dodge(width=0.5)) + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip() + theme(legend.position = "bottom") + scale_y_continuous("Estimated Marginal Effect of COVID-19 Health Concern on Trump Support",breaks=seq(-1,1,0.10)) + scale_shape_manual("Covariates of Interest",values=x$shape) + scale_fill_manual("Covariates of Interest",values=rep("white",32)) + scale_alpha_manual("Covariates of Interest",values=c(0.2,1),guide="none") + scale_x_discrete("") + theme(legend.position = "none") #+ labs(caption="\n90% & 95% Model CIs estimated from district-clustered robust standard errors. Darker sharded point estimates significant at p < 0.10. \nHeterogenous effects estimated from interactive model. Models control for 2016-2020 change in partisan preferences, \neducation, race, gender, ideology, 2016 presidential vote, economic evaluations, & COVID-19 infection proximity.\n N = 4 unified baseline additive models (two per panel wave & two outcome variables of interest) & 4 unifed \ninteractive models (two per panel wave & two outcome variables of interest) for a Total N = 8 models.")
ggsave(file="fig4_vs_presidential_models_health_concerns.png", plot, width = 8, height = 6, units = "in",bg="white")
print(plot)

#### Concern & Lockdown Additive Approval Model ####

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_approval_sep ~ sep_lack_covid_concern + lift_restrictions_too_slow_sep + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_additive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","lift_restrictions_too_slow_sep"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_sep$lower_90 <- margins_model1_additive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 
margins_model1_additive_sep$upper_90 <- margins_model1_additive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 

# Heath Concern X Party -> Approval
summary(model <- glm(trump_approval_sep ~ sep_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_sep*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_interactive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","lift_restrictions_too_slow_sep"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_sep$lower_90 <- margins_model1_interactive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 
margins_model1_interactive_sep$upper_90 <- margins_model1_interactive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_approval_nov ~ nov_lack_covid_concern + lift_restrictions_too_slow_nov + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_additive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","lift_restrictions_too_slow_nov"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_nov$lower_90 <- margins_model1_additive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 
margins_model1_additive_nov$upper_90 <- margins_model1_additive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 

# Heath Concern X Party -> Approval
summary(model <- glm(trump_approval_nov ~ nov_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_nov*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_interactive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","lift_restrictions_too_slow_nov"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_nov$lower_90 <- margins_model1_interactive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 
margins_model1_interactive_nov$upper_90 <- margins_model1_interactive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 

margins_model1_additive_sep$wave <- "September"
margins_model1_additive_nov$wave <- "November"

margins_additive_concern_lockdown_approval <- rbind(margins_model1_additive_sep,margins_model1_additive_nov)

margins_model1_interactive_sep$wave <- "September"
margins_model1_interactive_nov$wave <- "November"

margins_interactive_concern_lockdown_approval <- rbind(margins_model1_interactive_sep,margins_model1_interactive_nov)

rm(margins_model1_additive_sep,margins_model1_additive_nov,margins_model1_interactive_sep,margins_model1_interactive_nov,x,model,i)

#### Concern & Lockdown Additive Vote Model ####

# Heath Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_vote_sep ~ sep_lack_covid_concern + lift_restrictions_too_slow_sep + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_additive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","lift_restrictions_too_slow_sep"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_sep$lower_90 <- margins_model1_additive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 
margins_model1_additive_sep$upper_90 <- margins_model1_additive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 

# Heath Concern X Party -> vote
summary(model <- glm(trump_vote_sep ~ sep_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_sep*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_interactive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","lift_restrictions_too_slow_sep"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_sep$lower_90 <- margins_model1_interactive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 
margins_model1_interactive_sep$upper_90 <- margins_model1_interactive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 

# Heath Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_vote_nov ~ nov_lack_covid_concern + lift_restrictions_too_slow_nov + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_additive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","lift_restrictions_too_slow_nov"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_nov$lower_90 <- margins_model1_additive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 
margins_model1_additive_nov$upper_90 <- margins_model1_additive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 

# Heath Concern X Party -> vote
summary(model <- glm(trump_vote_nov ~ nov_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_nov*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_interactive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","lift_restrictions_too_slow_nov"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_nov$lower_90 <- margins_model1_interactive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 
margins_model1_interactive_nov$upper_90 <- margins_model1_interactive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 

margins_model1_additive_sep$wave <- "September"
margins_model1_additive_nov$wave <- "November"

margins_additive_concern_lockdown_vote <- rbind(margins_model1_additive_sep,margins_model1_additive_nov)

margins_model1_interactive_sep$wave <- "September"
margins_model1_interactive_nov$wave <- "November"

margins_interactive_concern_lockdown_vote <- rbind(margins_model1_interactive_sep,margins_model1_interactive_nov)

rm(margins_model1_additive_sep,margins_model1_additive_nov,margins_model1_interactive_sep,margins_model1_interactive_nov,x,model,i)

#### Concern & Lockdown Additive Trump-Gain Vote-Change Model ####

# Heath Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_vote_sep_2016_2020 ~  sep_lack_covid_concern + lift_restrictions_too_slow_sep + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_additive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","lift_restrictions_too_slow_sep"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_sep$lower_90 <- margins_model1_additive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 
margins_model1_additive_sep$upper_90 <- margins_model1_additive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 

# Heath Concern X Party -> vote
summary(model <- glm(trump_vote_sep_2016_2020 ~  sep_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_sep*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_interactive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","lift_restrictions_too_slow_sep"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_sep$lower_90 <- margins_model1_interactive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 
margins_model1_interactive_sep$upper_90 <- margins_model1_interactive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 

# Heath Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(trump_vote_nov_2016_2020 ~ nov_lack_covid_concern + lift_restrictions_too_slow_nov + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_additive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","lift_restrictions_too_slow_nov"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_nov$lower_90 <- margins_model1_additive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 
margins_model1_additive_nov$upper_90 <- margins_model1_additive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 

# Heath Concern X Party -> vote
summary(model <- glm(trump_vote_nov_2016_2020 ~ nov_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_nov*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_interactive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","lift_restrictions_too_slow_nov"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_nov$lower_90 <- margins_model1_interactive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 
margins_model1_interactive_nov$upper_90 <- margins_model1_interactive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 

margins_model1_additive_sep$wave <- "September"
margins_model1_additive_nov$wave <- "November"

margins_additive_concern_lockdown_vote_change_trump <- rbind(margins_model1_additive_sep,margins_model1_additive_nov)

margins_model1_interactive_sep$wave <- "September"
margins_model1_interactive_nov$wave <- "November"

margins_interactive_concern_lockdown_vote_change_trump <- rbind(margins_model1_interactive_sep,margins_model1_interactive_nov)

rm(margins_model1_additive_sep,margins_model1_additive_nov,margins_model1_interactive_sep,margins_model1_interactive_nov,x,model,i)

#### Concern & Lockdown Additive Biden-Gain Vote-Change Model ####

# Heath Concern -> Vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(biden_vote_sep_2016_2020 ~  sep_lack_covid_concern + lift_restrictions_too_slow_sep + sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_additive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","lift_restrictions_too_slow_sep"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_sep$lower_90 <- margins_model1_additive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 
margins_model1_additive_sep$upper_90 <- margins_model1_additive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 

# Heath Concern X Party -> vote
summary(model <- glm(biden_vote_sep_2016_2020 ~  sep_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_sep*pid3_coded + sep_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_interactive_sep <- summary(margins(model,variables=c("sep_lack_covid_concern","lift_restrictions_too_slow_sep"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_sep$lower_90 <- margins_model1_interactive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 
margins_model1_interactive_sep$upper_90 <- margins_model1_interactive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 

# Heath Concern -> vote, Note: District Deaths -> Increase Support as well
summary(model <- glm(biden_vote_nov_2016_2020 ~ nov_lack_covid_concern + lift_restrictions_too_slow_nov + nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_additive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","lift_restrictions_too_slow_nov"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_nov$lower_90 <- margins_model1_additive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 
margins_model1_additive_nov$upper_90 <- margins_model1_additive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 

# Heath Concern X Party -> vote
summary(model <- glm(biden_vote_nov_2016_2020 ~ nov_lack_covid_concern*pid3_coded + lift_restrictions_too_slow_nov*pid3_coded + nov_district_covid_deaths_per100k + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age + trump_vote_2016 + pid_change_2016_2020, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_interactive_nov <- summary(margins(model,variables=c("nov_lack_covid_concern","lift_restrictions_too_slow_nov"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_nov$lower_90 <- margins_model1_interactive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 
margins_model1_interactive_nov$upper_90 <- margins_model1_interactive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 

margins_model1_additive_sep$wave <- "September"
margins_model1_additive_nov$wave <- "November"

margins_additive_concern_lockdown_vote_change_biden <- rbind(margins_model1_additive_sep,margins_model1_additive_nov)

margins_model1_interactive_sep$wave <- "September"
margins_model1_interactive_nov$wave <- "November"

margins_interactive_concern_lockdown_vote_change_biden <- rbind(margins_model1_interactive_sep,margins_model1_interactive_nov)

rm(margins_model1_additive_sep,margins_model1_additive_nov,margins_model1_interactive_sep,margins_model1_interactive_nov,x,model,i)

#### Concern & Lockdown Figure: Presidential ####


margins_additive_concern_lockdown_approval$dv <- "DV: Trump COVID-19 Approval"
margins_additive_concern_lockdown_approval$pid3_coded <- "Baseline" 
margins_interactive_concern_lockdown_approval$dv <- "DV: Trump COVID-19 Approval"

margins_additive_concern_lockdown_vote$dv <- "DV: Trump Vote"
margins_additive_concern_lockdown_vote$pid3_coded <- "Baseline" 
margins_interactive_concern_lockdown_vote$dv <- "DV: Trump Vote"

margins_additive_concern_lockdown_vote_change_biden$dv <- "DV: Trump 2016-Biden 2020 Switch"
margins_additive_concern_lockdown_vote_change_biden$pid3_coded <- "Baseline"
margins_interactive_concern_lockdown_vote_change_biden$dv <- "DV: Trump 2016-Biden 2020 Switch"

margins_additive_concern_lockdown_vote_change_trump$dv <- "DV: Clinton 2016-Trump 2020 Switch"
margins_additive_concern_lockdown_vote_change_trump$pid3_coded <- "Baseline"
margins_interactive_concern_lockdown_vote_change_trump$dv <- "DV: Clinton 2016-Trump 2020 Switch"

x <- rbind(margins_additive_concern_lockdown_approval,margins_additive_concern_lockdown_vote,margins_additive_concern_lockdown_vote_change_biden,margins_additive_concern_lockdown_vote_change_trump,margins_interactive_concern_lockdown_approval,margins_interactive_concern_lockdown_vote,margins_interactive_concern_lockdown_vote_change_biden,margins_interactive_concern_lockdown_vote_change_trump)

x$facet_label <- paste(x$wave," Panel Wave",paste(" \n",x$dv,sep=""),sep="")
x <- subset(x,x$factor %in% c("nov_lack_covid_concern","sep_lack_covid_concern","lift_restrictions_too_slow_nov","lift_restrictions_too_slow_sep"))
x$variable <- ifelse(x$factor %in% c("lift_restrictions_too_slow_nov","lift_restrictions_too_slow_sep"),"Restrictions Concern",ifelse(x$factor %in% c("nov_lack_covid_concern","sep_lack_covid_concern"),"Health Concern",NA))

x$shape <- ifelse(x$variable == "Health Concern",21,22)
x$color_sig <- ifelse(x$p < 0.10,"sig","no sig")
x$facet_label <- factor(x$facet_label,levels=c("September Panel Wave \nDV: Trump COVID-19 Approval","November Panel Wave \nDV: Trump COVID-19 Approval","September Panel Wave \nDV: Trump Vote","November Panel Wave \nDV: Trump Vote","September Panel Wave \nDV: Clinton 2016-Trump 2020 Switch","November Panel Wave \nDV: Clinton 2016-Trump 2020 Switch","September Panel Wave \nDV: Trump 2016-Biden 2020 Switch","November Panel Wave \nDV: Trump 2016-Biden 2020 Switch"))

plot <- ggplot(x,aes(x=pid3_coded,y=AME,group=variable,alpha=color_sig)) + theme_bw() + facet_wrap(~facet_label,ncol = 2) + geom_linerange(aes(x= pid3_coded, ymin = lower_90, ymax = upper_90), lwd  = 1,position = position_dodge(width=0.5)) + geom_pointrange(aes(x= pid3_coded, ymin = lower, ymax = upper,shape=variable,fill=variable), lwd = 1/2,position = position_dodge(width=0.5)) + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip() + theme(legend.position = "bottom") + scale_y_continuous("Estimated Marginal Effect of COVID-19 Attitudes on Trump Support",breaks=seq(-1,1,0.10)) + scale_shape_manual("Covariates of Interest",values=x$shape) + scale_fill_manual("Covariates of Interest",values=rep("white",32)) + scale_alpha_manual("Covariates of Interest",values=c(0.2,1),guide="none") + scale_x_discrete("") #+ labs(caption="\n90% & 95% Model CIs estimated from district-clustered robust standard errors. Darker sharded point estimates significant at p < 0.10. \nHeterogenous effects estimated from interactive model. Models control for 2016-2020 change in partisan preferences, \neducation, race, gender, ideology, 2016 presidential vote, economic evaluations, & COVID-19 infection proximity.\n N = 4 unified baseline additive models (two per panel wave & two outcome variables of interest) & 4 unifed \ninteractive models (two per panel wave & two outcome variables of interest) for a Total N = 8 models.")
ggsave(file="fig6_vs_presidential_models_health_restriction_concerns.png", plot, width = 8, height = 6, units = "in",bg="white")
print(plot)

#### Modeling Heath Concern ####

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model <- glm(sep_lack_covid_concern ~ sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_additive_sep <- summary(margins(model,variables=c("sep_district_covid_deaths_per100k","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_sep$lower_90 <- margins_model1_additive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 
margins_model1_additive_sep$upper_90 <- margins_model1_additive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 

# Heath Concern X Party -> Approval
summary(model <- glm(sep_lack_covid_concern ~ sep_district_covid_deaths_per100k*pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_interactive_sep <- summary(margins(model,variables=c("sep_district_covid_deaths_per100k"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_sep$lower_90 <- margins_model1_interactive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 
margins_model1_interactive_sep$upper_90 <- margins_model1_interactive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model <- glm(nov_lack_covid_concern ~  nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_additive_nov <- summary(margins(model,variables=c("nov_district_covid_deaths_per100k","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_nov$lower_90 <- margins_model1_additive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 
margins_model1_additive_nov$upper_90 <- margins_model1_additive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 

# Heath Concern X Party -> Approval
summary(model <- glm(nov_lack_covid_concern ~ nov_district_covid_deaths_per100k*pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_interactive_nov <- summary(margins(model,variables=c("nov_district_covid_deaths_per100k"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_nov$lower_90 <- margins_model1_interactive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 
margins_model1_interactive_nov$upper_90 <- margins_model1_interactive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 

margins_model1_additive_sep$wave <- "September"
margins_model1_additive_nov$wave <- "November"

margins_additive_modelling_concern_approval <- rbind(margins_model1_additive_sep,margins_model1_additive_nov)

margins_model1_interactive_sep$wave <- "September"
margins_model1_interactive_nov$wave <- "November"

margins_interactive_modelling_concern_approval <- rbind(margins_model1_interactive_sep,margins_model1_interactive_nov)

rm(margins_model1_additive_sep,margins_model1_additive_nov,margins_model1_interactive_sep,margins_model1_interactive_nov,x,model,i)

#### Modeling Lockdown Worry ####

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model <- glm(lift_restrictions_too_slow_sep ~ sep_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_additive_sep <- summary(margins(model,variables=c("sep_district_covid_deaths_per100k","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_sep$lower_90 <- margins_model1_additive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 
margins_model1_additive_sep$upper_90 <- margins_model1_additive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_additive_sep$SE 

# Heath Concern X Party -> Approval
summary(model <- glm(lift_restrictions_too_slow_sep ~ sep_district_covid_deaths_per100k*pid3_coded + conservative + race + education_coded + household_income + proximity_covid_sep + positive_retro_econ_eval_sep + female + age, data=vs, weights=weight_genpop_2020Sep, family = binomial(link = "logit")))

margins_model1_interactive_sep <- summary(margins(model,variables=c("sep_district_covid_deaths_per100k"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_sep$lower_90 <- margins_model1_interactive_sep$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 
margins_model1_interactive_sep$upper_90 <- margins_model1_interactive_sep$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_sep$SE 

# Heath Concern -> Approval, Note: District Deaths -> Increase Support as well
summary(model <- glm(lift_restrictions_too_slow_nov ~  nov_district_covid_deaths_per100k + pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_additive_nov <- summary(margins(model,variables=c("nov_district_covid_deaths_per100k","pid3_coded"),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))
margins_model1_additive_nov$lower_90 <- margins_model1_additive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 
margins_model1_additive_nov$upper_90 <- margins_model1_additive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_additive_nov$SE 

# Heath Concern X Party -> Approval
summary(model <- glm(lift_restrictions_too_slow_nov ~ nov_district_covid_deaths_per100k*pid3_coded + conservative + race + education_coded + household_income + proximity_covid_nov + positive_retro_econ_eval_nov + female + age, data=vs, weights=weight_genpop_2020Nov, family = binomial(link = "logit")))

margins_model1_interactive_nov <- summary(margins(model,variables=c("nov_district_covid_deaths_per100k"),at=list(pid3_coded=c("Democrat","Republican","Independent")),change="minmax",vcov=cluster.vcov(model,vs$congressional_district),level=0.95,type="response"))

margins_model1_interactive_nov$lower_90 <- margins_model1_interactive_nov$AME - qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 
margins_model1_interactive_nov$upper_90 <- margins_model1_interactive_nov$AME + qt(0.95,df.residual(model)) * margins_model1_interactive_nov$SE 

margins_model1_additive_sep$wave <- "September"
margins_model1_additive_nov$wave <- "November"

margins_additive_modelling_lockdown_approval <- rbind(margins_model1_additive_sep,margins_model1_additive_nov)

margins_model1_interactive_sep$wave <- "September"
margins_model1_interactive_nov$wave <- "November"

margins_interactive_modelling_lockdown_approval <- rbind(margins_model1_interactive_sep,margins_model1_interactive_nov)

rm(margins_model1_additive_sep,margins_model1_additive_nov,margins_model1_interactive_sep,margins_model1_interactive_nov,x,model,i)

margins_additive_modelling_concern_approval$pid3_coded <- "Baseline"

margins_additive_modelling_concern_approval$dv <- "DV: COVID-19 Health Concern"
margins_interactive_modelling_concern_approval$dv <- "DV: COVID-19 Health Concern"

margins_additive_modelling_lockdown_approval$pid3_coded <- "Baseline"

margins_additive_modelling_lockdown_approval$dv <- "DV: COVID-19 Restrictions Concern"
margins_interactive_modelling_lockdown_approval$dv <- "DV: COVID-19 Restrictions Concern"

x <- rbind(margins_additive_modelling_concern_approval,margins_interactive_modelling_concern_approval,margins_additive_modelling_lockdown_approval,margins_interactive_modelling_lockdown_approval)

x$facet_label <- paste(x$wave," Panel Wave",paste(" \n",x$dv,sep=""),sep="")
x$variable <- ifelse(x$factor %in% c("nov_district_covid_deaths_per100k","sep_district_covid_deaths_per100k"),"District COVID-19 Death Severity",ifelse(x$factor %in% c("pid3_codedIndependent"),"Independent",ifelse(x$factor %in% c("pid3_codedRepublican"),"Republican",NA)))

x$variable2 <- paste(x$variable,"X",x$pid3_coded)  
x$variable2 <- gsub("X Baseline","Baseline Effect",x$variable2)
x$variable2 <- factor(x$variable2,levels=c("Independent Baseline Effect","Republican Baseline Effect","District COVID-19 Death Severity Baseline Effect","District COVID-19 Death Severity X Democrat","District COVID-19 Death Severity X Independent","District COVID-19 Death Severity X Republican"))

x$color_sig <- ifelse(x$p < 0.05,"sig","no sig")
x$facet_label <- factor(x$facet_label,levels=c("September Panel Wave \nDV: COVID-19 Health Concern","November Panel Wave \nDV: COVID-19 Health Concern","September Panel Wave \nDV: COVID-19 Restrictions Concern","November Panel Wave \nDV: COVID-19 Restrictions Concern"))

x$variable2 <- fct_rev(x$variable2)

plot <- ggplot(x,aes(x=variable2,y=AME,group=variable2,alpha=color_sig)) + theme_bw() + facet_wrap(~facet_label) + geom_linerange(aes(x= variable2, ymin = lower_90, ymax = upper_90), lwd  = 1) + geom_pointrange(aes(x= variable2, ymin = lower, ymax = upper), lwd = 1/2,shape=21,fill="white") + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip() + theme(legend.position = "none") + scale_y_continuous("Estimated Marginal Effect on COVID-19 Sentiments",breaks=seq(-1,1,0.20)) + scale_alpha_manual("",values=c(0.2,1),guide="none") + scale_x_discrete("") #+ labs(caption="\n90% & 95% Model CIs estimated from district-clustered robust standard errors. Darker sharded point estimates significant at p < 0.05. \nModels control for education, race, gender, ideology, economic evaluations, & COVID-19 infection proximity.\n N = 2 unified baseline additive models (two per panel wave x two outcome variables of interest) & 2 unifed \ninteractive models (two per panel wave x two outcome variables of interest) for a Total N = 4 models. \nHeterogenous effects estimated from interactive model.")
print(plot)
ggsave(file="fig5_vs_health_restriction_attitudes_models.png", plot, width = 8, height = 4.5, units = "in",bg="white")