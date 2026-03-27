# Libraries
library(plyr)
library(readstata13)
library(survey)
library(stringr)
library(csodata)

##################
##### Organize panel data
#####################

# Read in the pre and post data from Qualtrics
load("Data_prepost.RData")

# Create outcomes with mean values:

pre$status_quo_mean <- apply(pre[,c("status_quo_1","status_quo_2")],1,mean)
post$status_quo_mean <- apply(post[,c("status_quo_1","status_quo_2")],1,mean)

pre$directional_mean <- apply(pre[,c("directional_1","directional_2")],1,mean)
post$directional_mean <- apply(post[,c("directional_1","directional_2")],1,mean)

pre$self_mean <- apply(pre[,c("self_1","self_2")],1,mean)
post$self_mean <- apply(post[,c("self_1","self_2")],1,mean)

# Organize other variables:

pre$age <- as.numeric(mapvalues(pre$validation_age, c("25-34","45-54","35-44","18-24","55-64","65-74","75-84"), 
                                c(2,4,3,1,5,6,7)))

pre$male <- as.numeric(mapvalues(pre$gender, c("Male","Female"), c(1,0)))

pre$education <- as.numeric(mapvalues(pre$education, c("Certificate or diploma" ,"University primary degree or equivalent", 
                                                       "Leaving Cert or equivalent","University higher degree or equivalent" , 
                                                       "Junior/Intermediate/ Group Cert or equivalent", "Complete Primary","None"),
                                      c(4,5,3,6,2,1,0)))

pre$married <- as.numeric(as.character(mapvalues(pre$marital_status, 
                                                 c("Living with a partner", "Never Married" , "Married", "Separated/Divorced","Widowed"),
                                                 c(1,0,1,0,0))))

pre$hh_income <- as.numeric(mapvalues(pre$hh_income, c( "€451-€700 per week" ,   "€241-€450 per week", 
                                                        "€701 or more per week", "Below €240 per week"  ), c(3,2,4,1)))

pre$religiosity <- as.numeric(mapvalues(pre$religiosity, c("Several times a week","Once a week","2 or 3 times a month",
                                                           "Once a month","Several times a year","Once a year","Less frequently"), 7:1))

pre$ban <- as.numeric(mapvalues(pre$vote_choice, c("Vote Yes (Repeal the abortion ban).",
                                                   "Vote No (Not repeal the abortion ban).",""), c(-1,1,0)))

# Create a variable in pre survey indicating whether the respondent answered or not in the post survey
pre$recontact <- NA
pre$recontact[which(pre$PID %in% post$PID)] <- 1
pre$recontact[!(pre$PID %in% post$PID)] <- 0

# Create variable for number of days
pre$date <- str_sub(pre$RecordedDate,1,10)
post$date <- str_sub(post$RecordedDate,1,10)
pre$date <- as.numeric(mapvalues(pre$date,c("2018-05-22", "2018-05-17" ,"2018-05-21", 
                                            "2018-05-24" ,"2018-05-18", "2018-05-19", "2018-05-16" ,
                                            "2018-05-23", "2018-05-20" ,"2018-05-15"),
                                 c(-3,-8,-4,-1,-7,-6,-9,-2,-5,-10)))
post$date <- as.numeric(mapvalues(post$date,c("2018-05-29", "2018-05-30", "2018-06-03",
                                              "2018-06-01" ,"2018-05-31","2018-06-04", "2018-06-02", 
                                              "2018-06-07" ,"2018-06-05" ,"2018-06-06", "2018-06-08"),
                                  c(4,5,9,7,6,10,8,13,11,12,14)))

# Calculate inverse probability weights 
mod <- glm(recontact ~ age + male + education + hh_income + religiosity + married + status_quo_mean + directional_mean + self_mean + thermo, data=pre,family="binomial") 
preds <- predict(mod, type="response")
pre$weights <- 1/preds

# Assign post survey values to pre survey and combine
combined <- merge(pre, post[,c("PID","date","status_quo_1","status_quo_2","directional_1","directional_2","attention_check","self_1","self_2","thermo","status_quo_mean","directional_mean","self_mean")], by="PID",all.x=T)

############################
####### Prep post-stratification weights based on 2020 INES
################################

# Read in the INES 2020 data.
load("2020 UCD Online Election Poll.Rdata")
ines <- x

# Year of birth
# We weren't able to recruit anyone 85 or above. So in our data, we combine 75-84 and 85+ categories.
# Also, collapse 65-74 and 75-84 in our data because INES data ends at 65+.
age_ines_vec <- unname(prop.table(table(ines$age)))

combined$age_six_category <- as.numeric(mapvalues(combined$validation_age, 
                                                  c("18-24","25-34","35-44","45-54","55-64","65-74","75-84"),
                                       c(1,2,3,4,5,6,6)))

# Sex
ines$male <-  as.numeric(mapvalues(as.character(ines$gender), c("Female","Male","Other"), c(0,1,0)))

gender_ines_vec <- rep(NA,2)
names(gender_ines_vec) <- c("female","male")
gender_ines_vec[1] <- length(which(ines$male==0))
gender_ines_vec[2] <- length(which(ines$male==1))
gender_ines_vec <- prop.table(gender_ines_vec)
gender_ines_vec <- unname(gender_ines_vec)

# highest level of education completed
# We have a category called "University higher degree or equivalent" that is not in the INES. So collapse "university primary degree or equivalent" and "university higher degree or equivalent".
# In the INES, combine less than junior/inter cert and secondary school junior cert/inter cert.
ines$educ <- ines$education
ines$educ <- as.numeric(mapvalues(as.character(ines$educ), c("1 - None"       ,                            
                                  "2 - Primary School"                       ,  
                                  "3 - Less than Junior/Inter Cert"         ,   
                                  "4 - Secondary School Junior Cert/Inter Cert",
                                  "5 - Secondary School Leaving Cert"          ,
                                  "6 - Post Leaving Cert Qualifications"      , 
                                  "7 - Third Level Degree"), c(1,2,3,3,4,5,6)))

combined$educ_six_category <- combined$education+1
combined$educ_six_category[combined$educ_six_category==7] <- 6

educ_ines_vec <- prop.table(table(ines$educ))
educ_ines_vec <- unname(educ_ines_vec)
educ_ines_vec <- as.vector(educ_ines_vec)

##################
##### Calculate INES-based post-stratification weights for dataset with respondents who answered both waves
#####################

# Reduce the data to fully filled rows
combined <- subset(combined,!is.na(status_quo_1.y))

# Calculate psw for combined

combined_unweighted <- svydesign(ids=~1, data=combined)

gender.dist <- data.frame(male=c(0,1), Freq = nrow(combined)*gender_ines_vec)

educ.dist <- data.frame(educ_six_category=1:6, Freq=nrow(combined)*educ_ines_vec)

age.dist <- data.frame(age_six_category=1:6, Freq=nrow(combined)*age_ines_vec)
age.dist <- age.dist[,-2]
colnames(age.dist)[2] <- "Freq"

combined_rake <- rake(design=combined_unweighted, sample.margins=list(~male,~educ_six_category,~age_six_category), 
                      population.margins=list(gender.dist,educ.dist,age.dist))

combined_rake_trim <- trimWeights(combined_rake, lower=, upper=3,strict=T)
combined$weights_pst_ines <- weights(combined_rake_trim)

############################
####### Prep post-stratification weights based on 2016 census
################################

## Read in data on gender and age
sexagedata <- read.csv("EY007.20201202T101230.csv")

# Delete All ages observations
sexagedata <- sexagedata[-c(1:3),]

# Keep those 18 or above
sexagedata <- subset(sexagedata, Age.Last.Birthday!="Under 1 year")
sexagedata <- subset(sexagedata, Age.Last.Birthday!="All ages")
sexagedata$age <- as.numeric(gsub("\\D", "", sexagedata$Age.Last.Birthday))
sexagedata <- subset(sexagedata, age >= 18)

# Delete Population at or under/over this age
sexagedata <- subset(sexagedata, At.Each.Year.of.Age=="Population")

# Delete Both sexes
sexagedata <- subset(sexagedata, Sex!="Both sexes")

# Calculate proportion of gender groups
sexagedata <- subset(sexagedata, CensusYear==2016)
men <- sum(subset(sexagedata,Sex=="Male")[,c("VALUE")])
women <- sum(subset(sexagedata,Sex=="Female")[,c("VALUE")])
total <- men + women
gender_census_vec <- c(women/total,men/total)

# Calculate proportion of age groups
# Our dataset has seven age groups. 
age_census_vec <- rep(NA,7)
names(age_census_vec) <- c("18-24","25-34","35-44","45-54","55-64","65-74","75+")
age_census_vec[1] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=18 & sexagedata$age<=24)])
age_census_vec[2] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=25 & sexagedata$age<=34)])
age_census_vec[3] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=35 & sexagedata$age<=44)])
age_census_vec[4] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=45 & sexagedata$age<=54)])
age_census_vec[5] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=55 & sexagedata$age<=64)])
age_census_vec[6] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=65 & sexagedata$age<=74)])
age_census_vec[7] <- sum(sexagedata$"VALUE"[which(sexagedata$age>=75)])
age_census_vec <- prop.table(age_census_vec)

## Read in data on education
educdata <- read.csv("EZ055.20201202T101232.csv")

# Subset to 18 or above in age
educdata$age <- as.numeric(gsub("\\D", "", educdata$Single.Year.of.Age))
educdata <- subset(educdata, age>=18)

# Delete Both sexes
educdata <- subset(educdata, Sex!="Both sexes")

# Delete non-relevant educations
educdata <- subset(educdata, CensusYear==2016)
educdata <- subset(educdata, Highest.Level.of.Education.Completed!="Total education ceased and not ceased")
educdata <- subset(educdata, Highest.Level.of.Education.Completed!="Not stated")

# Calculate proportion of education groups
educdata$educ_six_category <- as.numeric(as.character(mapvalues(educdata$Highest.Level.of.Education.Completed,
                                                   c("No formal education"    ,                                    
                                                     "Primary"                 ,                                   
                                                     "Lower secondary"          ,                                  
                                                     "Upper secondary"           ,                                 
                                                     "Technical/vocational"           ,                            
                                                     "Advanced certificate/completed apprenticeship"      ,        
                                                     "Higher certificate"                             ,            
                                                     "Ordinary bachelor degree/professional qualification or both",
                                                     "Honours bachelor degree/professional qualification or both" ,
                                                     "Postgraduate diploma or degree"                        ,     
                                                     "Doctorate (Ph.D.)"                   ,                       
                                                     "Economic status - total at school, university, etc."   ,     
                                                     "Economic status - other" ),
                                                   c(1,2,3,4,5,5,5,6,6,6,6,NA,NA))))
educdata <- subset(educdata,!is.na(educ_six_category))
educ_census_vec <- prop.table(c(by(educdata$"VALUE",educdata$"educ",sum)))

##################
##### Calculate census-based post-stratification weights for dataset with respondents who answered both waves
#####################

# Calculate psw for combined

combined_unweighted <- svydesign(ids=~1, data=combined)

gender.dist <- data.frame(male=c(0,1), Freq = nrow(combined)*gender_census_vec)

educ.dist <- data.frame(educ_six_category=1:6, Freq=nrow(combined)*educ_census_vec)

age.dist <- data.frame(age=1:7, Freq=nrow(combined)*age_census_vec)

combined_rake <- rake(design=combined_unweighted, sample.margins=list(~male,~educ_six_category,~age), 
                      population.margins=list(gender.dist,educ.dist,age.dist))

combined_rake_trim <- trimWeights(combined_rake, lower=, upper=3,strict=T)
combined$weights_pst_census <- weights(combined_rake_trim)

##################
##### Keep only variable used for analysis
#####################

pre <- pre[,c("status_quo_1","status_quo_2","directional_1","directional_2","self_1","self_2",
              "male","validation_age","education")]

post <- post[,c("status_quo_1","status_quo_2","directional_1","directional_2","self_1","self_2")]

combined <- combined[,-which(colnames(combined) %in% c("StartDate","EndDate","RecordedDate","gender",
                          "marital_status","turnout","vote_choice","recontact","age_six_category"))]

##################
##### Save the data
#####################

save(pre, post, combined, file="Data_prepostcombined.RData")










