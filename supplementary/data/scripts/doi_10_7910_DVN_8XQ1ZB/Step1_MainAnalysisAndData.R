#######
#######
####### Replication files for Do Women Officers Police Differently? Evidence from Traffic Stops
####### This file cleans the raw data and runs the analysis for the body of the paper. 
####### Last Updated: Jan. 2021
#######
#######


###
### 1. Setting up the space. 
###

# Setting the working directory:
setwd("D:/OneDrive - London School of Economics/Desktop/lse assignments/GV330/summative replication")

# Installing the needed libraries:
#install.packages("pscl",dependencies = T)
#install.packages("ggplot2",dependencies = T)
#install.packages("texreg",dependencies = T)
#install.packages("readr",dependencies = T)
#install.packages("arm",dependencies = T)
#install.packages("dplyr",dependencies = T)

# Opening up those libraries:
need <- c("ggplot2", "dplyr", "texreg","readr","pscl","arm","stats","fixest","arsenal") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T))

######################Step1_Main_Analysis_And_Data###############################################
# Loading the raw data:
rm(list = ls()) # clear workspace

nc_new = read_csv("Data/Officer_Traffic_Stops_Update.csv")
nc_old = read_csv("Data/Officer_Traffic_Stops_Original.csv")
nc = bind_rows(nc_new,nc_old)
fl = read_csv("Data/fl_statewide_2019_08_13.csv")


###
### 2. Producing the data sets for each table. 
###

# Cleaning the NC Data
nc$driver_re = as.numeric(ifelse(nc$Driver_Race=="White"&
                                   nc$Driver_Ethnicity=="Non-Hispanic","0",
                                 ifelse(nc$Driver_Race=="Black"&
                                          nc$Driver_Ethnicity=="Non-Hispanic","1",
                                        ifelse(nc$Driver_Ethnicity=="Hispanic","2",NA)))) #code driver race/ethnicity:white=0, black=1, hispanic=2
nc$of_rg = ifelse(nc$Officer_Race=="White",
                  ifelse(nc$Officer_Gender=="Male","0","1"),
                  ifelse(nc$Officer_Race=="Black/African American",
                         ifelse(nc$Officer_Gender=="Male","2","3"),NA)) #code officer race and gender: 0 for white male, 1 for white female, 2 for black male, 3 for black female, NA for others. 
nc$of_race = ifelse(nc$Officer_Race=="White",0,
                    ifelse(nc$Officer_Race=="Black/African American",1,NA)) #code officer race: 0 for white, 1 for black, NA for others.
nc$of_gender = ifelse(nc$Officer_Gender=="Male","0","1")  #code officer gender, 0 for male, 1 for female
nc$investigatory = ifelse(grepl("Impaired|Speeding|Light|Movement",
                                as.character(nc$Reason_for_Stop)),0,1) #coded as investigatory if the reason for stop is not among "impaired" "speeding" "light" or "movement". But what about "SeatBelt", "Vehicle Equipment"?
nc$investigatory = ifelse(grepl("Check",as.character(nc$Reason_for_Stop)),
                          NA,nc$investigatory) #if the reason for stop is "check", code it as NA
nc$race_gender = ifelse(nc$driver_re=="0",
                        ifelse(nc$Driver_Gender=="Male","0","1"),
                        ifelse(nc$driver_re=="1",
                               ifelse(nc$Driver_Gender=="Male","2","3"),NA)) #code driver's race and gender, 0 for white male, 1 for white female, 2 for black male, 3 for black female, NA for others.
nc$search = ifelse(nc$Was_a_Search_Conducted=="Yes",1,0) #if searched, code as 1, else code as 0

nc$subject_sex = tolower(nc$Driver_Gender) #"Male" to "male, "Female" to "female" for subject sex
nc$subject_age = nc$Driver_Age #duplicate Driver age variable as subject_age
nc$officer_sex = tolower(nc$Officer_Gender) #"Male" to "male, "Female" to "female" for officer sex
nc$month = apply(as.matrix(as.character(nc$Month_of_Stop)),1,
                 function(x){strsplit(x,"/",fixed=T)[[1]][2]}) #extract month information: "2019/01" as "01".
nc$year = apply(as.matrix(as.character(nc$Month_of_Stop)),1,
                function(x){strsplit(x,"/",fixed=T)[[1]][1]}) #extract year information: "2019/01" as "2019".

nc$arrest = ifelse(nc$Result_of_Stop=="Arrest",1,0) #code 1 if subject was arrested, 0 otherwise
save(nc,file="Data/NorthCarolina.RData") #save data

#compare nc data with those provided by author
nc1 <- nc
load("Data/NorthCarolina.RData")
comparedf(nc,nc1)

# Cleaning the FL data.
violations_list = strsplit(paste(fl$reason_for_stop,collapse = "|"),"|",fixed = T) #sometimes there are multiple reasons for stop, e.g. "NO REGISTRATION|IMPROPER TURN". This function makes a list of the reasons
violations_list_small = unique(violations_list[[1]])[2:71] #tab all types of violations. [2:71] to avoid NA
violations_indicator = violations_list_small[c(1,2,5,6,7,9,10,14,19,
                                               20,23,40,45)]  #list of types of violations deemed as non-investigatory
fl$investigatory = ifelse(is.na(fl$violation),NA,
                          ifelse(fl$violation %in% violations_indicator, 0, 1)) #code 0 if the violation is included in the non-investigatory list above, 1 otherwise
fl$contraband_found = ifelse(grepl("contraband",
                                   tolower(fl$violation)),1,0) #code 1 if contraband is found, 0 otherwise
fl$race_gender = ifelse(fl$subject_race=="white",
                        ifelse(fl$subject_sex=="male",0,1),
                        ifelse(fl$subject_race=="black",
                               ifelse(fl$subject_sex=="male",2,3),
                               ifelse(fl$subject_race=="hispanic",
                                      ifelse(fl$subject_sex=="male",4,5),NA))) #code driver race and gender, 0 for white male, 1 for white female, 2 for black male, 3 for black female, 4 for hispanic male, 5 for hispanic female, NA otherwise.
fl$of_rg = ifelse(fl$officer_race=="white",
                  ifelse(fl$officer_sex=="male",0,1),
                  ifelse(fl$officer_race=="black",
                         ifelse(fl$officer_sex=="male",2,3),
                         ifelse(fl$officer_race=="hispanic",
                                ifelse(fl$officer_sex=="male",4,5),NA))) #code officer race and gender: 0 for white male, 1 for white female, 2 for black male, 3 for black female, 4 for hispanic male, 5 for hispanic female, NA otherwise.
fl$of_race = ifelse(fl$officer_race=="white",0,
                    ifelse(fl$officer_race=="black",1,
                           ifelse(fl$officer_race=="hispanic",2,
                                  ifelse(fl$officer_race=="asian/pacific islander",3,
                                         ifelse(fl$officer_race=="other",4,NA))))) #code officer race: 0 for white, 1 for black, 2 for hispanic, 3 for asian.pacific islander, NA otherise.
fl$of_gender = ifelse(fl$officer_sex=="male",0,1) #code office gender, 0 for male, 1 for female.
fl$out_of_state = ifelse(fl$vehicle_registration_state=="FL",0,1) #code 0 for driving a florida car, 1 for driving a vehicle out of this state.
fl$hour_of_day = apply(as.matrix(as.character(fl$time)),1,
                       function(x)(strsplit(x,":",fixed = T)[[1]][1])) #extract hour of day information: "12:40:41" as "12".
fl$month = apply(as.matrix(as.character(fl$date)),1,
                 function(x)(paste(strsplit(x,"-",fixed = T)[[1]][2], 
                                   collapse = "_"))) #extract month information: "2010-10-28" as "10".
fl$year = apply(as.matrix(as.character(fl$date)),1,
                function(x)(paste(strsplit(x,"-",fixed = T)[[1]][1],
                                  collapse = "_")))   #extract year information: "2010-10-28" as "2010".
fl = subset(fl,fl$year!="2016"&fl$year!="2017"&fl$year!="2018") #Narrows down to complete years that don't report extreme missingness on key outcome. 
fl.officers = names(table(fl$officer_id_hash))[table(fl$officer_id_hash)>1000] #make a list of officers that appear in more than 1000 obs.
fl$officers_include = ifelse(fl$officer_id_hash%in%fl.officers,1,0) #code in the original dataset, 1 if the officer is included in the list above, 0 if not.
fl.counties = names(table(fl$county_name))[table(fl$county_name)>1000] #make a list of counties that appear in more than 1000 obs.
fl$county_include = ifelse(fl$county_name%in%fl.counties,1,0) #code in the original dataset, 1 if the county is included in the list above, 0 if not.
fl.ag.id = aggregate(fl$of_gender,
                     list(fl$officer_id_hash,fl$year,fl$county_name),
                     mean)  #create a new data frame that summarises officer gender ('fl$of_gender') for each unique combination of officer ID, year, and county name. 
fl.ag.id$officer = ifelse(!is.na(fl.ag.id$x),1,0) #code Officer variable in the new dataframe as 1 if the x variable is not NA, 0 for NA. Presumedly for finding out whether the officer was present in that year.
fl.ag.gender = aggregate(fl.ag.id[,c("x","officer")],
                         list(fl.ag.id$Group.2,fl.ag.id$Group.3),
                         sum,na.rm=T) #calculate in a new dataframe, number of female and total officers for each county and each year.
fl.ag.gender$prop.female = fl.ag.gender$x/fl.ag.gender$officer #calculate the corresponding female-to-total ratio.
colnames(fl.ag.gender) = c("year","county_name","count.female","tot.officer","prop.female") #give the dataset column names
fl = merge(fl,fl.ag.gender,by=c("year","county_name"),all.x=T) #pass the above information back to the main dataset
fl$officer_exclude = ifelse(fl$officer_years_of_service<0|fl$officer_years_of_service>40,1,0) #exlude officers that have abonormal values in years of service
fl.ag.id2 = aggregate(fl$of_gender,
                      list(fl$officer_id_hash),
                      mean) #create a dataframe of indidual officers by their sex
fl$search_occur = ifelse(fl$search_conducted == 0, 0, 
                         ifelse(fl$search_basis != "other",1,NA)) #
fl$contra = ifelse(is.na(fl$search_occur),0,
                   ifelse(fl$search_occur==1,fl$contraband_found,0)) #only code 1 in "contra" if contraband was found upon a search, 0 even if contraband is found without a search/not clear wheter a search was conducted.

complete = complete.cases(fl[,c("search_occur","race_gender","subject_age",
                                "out_of_state","investigatory","of_gender",
                                "of_race","officer_years_of_service","officer_age",
                                "hour_of_day","month","year","county_name")]) #find the cases that have no missing variables in these columns.
fl.sm = fl[complete,] #create a new dataframe for the complete cases
complete2 = complete.cases(fl[,c("search_occur","of_gender")]) #find the cases that have no missing variables in these columns, tolerate missing values in control variables.
table(complete) #table number of cases complete/incomplete in all variables
table(complete2) #table number of cases complete/incomplete in mainindependent/dependent variables

fl.missingness = apply(fl[,c("search_occur","race_gender","subject_age",
                             "out_of_state","investigatory","of_gender",
                             "of_race","officer_years_of_service","officer_age",
                             "county_name")],
                       2,
                       FUN = function(x){table(is.na(x))}) #record missingness in data for these variables.
#save(fl,file="Data/FloridaLarge.RData") #don't over-write the original data
#save(fl.sm,file="Data/FloridaSmall.RData") #don't over-write the original data

fl$stops = ifelse(!is.na(fl$search_occur),1,0) #record stop occurence if search data is not missing
fl$contra.ttest = ifelse(fl$search_occur==1,fl$contra,NA) #code 1 if contraband was found upon a search, 0 if not found but a search occured, NA otherwise.
prop.test(table(fl$of_gender,fl$contra.ttest)) #do a difference-of-proportions test
fl$of_exper = ifelse(fl$officer_years_of_service>=
                       mean(fl$officer_years_of_service,na.rm=T),1,0) #code 1 for officers more experienced than average, 0 otherwise
fl$of_age = ifelse(fl$officer_age<30,1,
                   ifelse(fl$officer_age>64,3,2)) #code 1 for officer younger than 30, 2 for between 30 and 64, 3 for older than 64
fl$driver_age = ifelse(fl$subject_age<30,1,
                       ifelse(fl$subject_age>64,3,2)) #code 1 for driver younger than 30, 2 for between 30 and 64, 3 for older than 64
fl$hour_of_day2 = as.numeric(fl$hour_of_day) #make the hour_of_day numeric rather than a string
fl$tod = ifelse(fl$hour_of_day2<3,1,
                ifelse(fl$hour_of_day2<6,2,
                       ifelse(fl$hour_of_day2<9,3,
                              ifelse(fl$hour_of_day2<12,4,
                                     ifelse(fl$hour_of_day2<15,5,
                                            ifelse(fl$hour_of_day2<18,6,
                                                   ifelse(fl$hour_of_day2<21,7,8))))))) #categorise time of day: 1 for before 3am, 2 for 3am-6am, 3 for 6am-9am,..., 8 for 9pm-12am.

fl.ag.officers = aggregate(fl[,c("stops","search_occur","contra")],
                           by=list(fl$officer_id_hash,
                                   fl$of_race,fl$of_gender,
                                   fl$of_exper,fl$of_age,
                                   fl$race_gender,fl$driver_age,
                                   fl$out_of_state,fl$investigatory,
                                   fl$year,fl$tod),
                           sum,na.rm=T) #aggregates by officer ID, officer race, officer gender, officer experience, officer age, driver race and gender, driver age, whether the driver is out-of-state or not, whether the stop was investigatory or not, year, and time of day.
colnames(fl.ag.officers) = c("officer_id","of_race","of_gender","of_exper",
                             "of_age","race_gender","driver_age",
                             "out_of_state","investigatory","year",
                             "tod","stops","search_occur","contra") #assign column names
fl.ag.officers$contra.search.rate = (fl.ag.officers$contra/fl.ag.officers$search_occur)*10 #calculate hit rate per 10 searches
fl.ag.officers$contra.stop.rate = (fl.ag.officers$contra/fl.ag.officers$stops)*100 #calculate hit rate per 100 stops
#save(fl.ag.officers,file="Data/FL_Aggregated.RData") #do not over-write the original dataset

#compare fl.ag.officers data with those provided by author
fl.ag.officers1 <- fl.ag.officers
load("Data/FL_Aggregated.RData")
comparedf(fl.ag.officers,fl.ag.officers1)

# Data for Figure 1
search.df = data.frame("Department" = c("CPD","CPD","FHP","FHP"),
                       "Gender" = c("Male","Female","Male","Female"),
                       "Rate" = c(prop.table(table(nc$of_gender,nc$search),1)[,2],
                                  prop.table(table(fl$of_gender[fl.sm$county_include==1&
                                                                  fl.sm$officer_exclude==0],
                                                   fl$search_occur[fl.sm$county_include==1&
                                                                     fl.sm$officer_exclude==0]),1)[,2])) #produce data for figure 1 (Search Rates by Agency and Sex of Officer)
#save(search.df,file="Data/Fig1_Data.RData") #don't overwrite

###
### 3. Regressions
###

#
# For the Main Text:
#

# Search Regressions
fl.search.sm = lm(search_occur~factor(of_gender),data=fl) #simple regression of search rate on gender
#save(fl.search.sm, file="Data/FLSearch_Sm_OLS.RData") #don't overwrite
fl.search = lm(search_occur~factor(race_gender)+
                 subject_age+out_of_state+
                 investigatory+
                 factor(of_gender)+factor(of_race)+
                 officer_years_of_service+officer_age+
                 factor(hour_of_day)+factor(month)+factor(year)+
                 factor(county_name),
               data=fl.sm,  
               subset=fl.sm$county_include==1&fl.sm$officer_exclude==0)
save(fl.search,file="Data/FLSearch_OLS.RData")
nc.search.sm = lm(search~factor(of_gender),data = nc)
#nc.search.sm1 = lm_robust(search~factor(of_gender),data = nc) #Dianyi OLS regression with robust se
#nc.search.sm2 = feglm(search~factor(of_gender),data = nc, family=binomial(link='logit'), vcov="HC1") #Dianyi logit regression with robust se
#nc.search.sm3 = glm(search~factor(of_gender),data = nc, family=binomial(link='logit')) 
save(nc.search.sm,file="Data/NCSearch_Sm_OLS.RData")
nc.search = lm(search~factor(race_gender)+subject_age+
                 investigatory+
                 factor(of_race)+
                 factor(of_gender)+Officer_Years_of_Service+
                 factor(month)+factor(year)+
                 factor(CMPD_Division),
               data=nc)
save(nc.search,file="Data/NCSearch_OLS.RData")

# Contraband Regressions
fl.contra = lm(contra~factor(race_gender)+
                 subject_age+out_of_state+
                 investigatory+
                 factor(of_gender)+factor(of_race)+
                 officer_years_of_service+officer_age+
                 factor(hour_of_day)+factor(month)+factor(year)+
                 factor(county_name),
               data=fl.sm,  
               subset=fl.sm$county_include==1&
                 fl.sm$search_occur==1&
                 fl.sm$officer_exclude==0)
save(fl.contra,file="Data/FlContra_OLS.RData")
contra.search.rate.reg = lm(contra.search.rate ~ factor(of_gender) + factor(of_exper) + 
                              factor(of_age) +factor(of_race) +
                              factor(race_gender) + factor(driver_age)+ 
                              investigatory + out_of_state +
                              factor(year)+factor(tod),
                            data=fl.ag.officers,
                            subset=fl.ag.officers$search_occur>0)
save(contra.search.rate.reg,file="Data/FlSearchRate_OLS.RData")
contra.stop.rate.reg = lm(contra.stop.rate ~ factor(of_gender) + factor(of_exper) + 
                            factor(of_age) + factor(of_race) +
                            factor(race_gender) + factor(driver_age)+ 
                            investigatory + out_of_state +
                            factor(year)+factor(tod),
                          data=fl.ag.officers)
save(contra.stop.rate.reg,file="Data/FlStopRate_OLS.RData")