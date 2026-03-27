#Title: Why Do Voters Prefer Local Candidates? Evidence from Danish Conjoint Survey Experiment
#Author: Niels Nyholt 

#File purpose: Transform and recode data, so it is ready for analysis. 
#NB: Set working directory to the folder where you have saved "survey_data.dta"
rm(list=ls())

setwd()

#The following analyses were carried out using R version 4.3.1
require(tidyverse) #version 2.0.0
require(haven) #version 2.5.3


raw_data <- read_dta("survey_data.dta") %>% 
  zap_labels()

#Transformations####
#I transform the data from broad to narrow format, and calculate several additional variables based on corespondence between caracteristics of the respondent and character profiles. Furthermore, I also simplify a couple of variables to ease interpretations.

transformed_data <- raw_data %>% 
    mutate(
      local1 = (ifelse(local1==977, NA, local1)-1)/(5-1),
      local2 = (ifelse(local2==977, NA, local2)-1)/(5-1),
      local3 = (ifelse(local3==977, NA, local3)-1)/(5-1),
    
    
    conj1_fc_a=ifelse(conj1_fc==3, NA, ifelse(conj1_fc==1, 1, 0)),
    conj1_fc_b=ifelse(conj1_fc==3, NA, ifelse(conj1_fc==2, 1, 0)),
    
    conj2_fc_a=ifelse(conj2_fc==3, NA, ifelse(conj2_fc==1, 1, 0)),
    conj2_fc_b=ifelse(conj2_fc==3, NA, ifelse(conj2_fc==2, 1, 0)),
    
    conj3_fc_a=ifelse(conj3_fc==3, NA, ifelse(conj3_fc==1, 1, 0)),
    conj3_fc_b=ifelse(conj3_fc==3, NA, ifelse(conj3_fc==2, 1, 0)),
    
    conj4_fc_a=ifelse(conj4_fc==3, NA, ifelse(conj4_fc==1, 1, 0)),
    conj4_fc_b=ifelse(conj4_fc==3, NA, ifelse(conj4_fc==2, 1, 0)),
    
    conj5_fc_a=ifelse(conj5_fc==3, NA, ifelse(conj5_fc==1, 1, 0)),
    conj5_fc_b=ifelse(conj5_fc==3, NA, ifelse(conj5_fc==2, 1, 0)),
    
    conj1_timing_a=page_p_conj1_timing,
    conj1_timing_b=page_p_conj1_timing,
    conj2_timing_a=page_p_conj2_timing,
    conj2_timing_b=page_p_conj2_timing,
    conj3_timing_a=page_p_conj3_timing,
    conj3_timing_b=page_p_conj3_timing,
    conj4_timing_a=page_p_conj4_timing,
    conj4_timing_b=page_p_conj4_timing,
    conj5_timing_a=page_p_conj5_timing,
    conj5_timing_b=page_p_conj5_timing,
    
    
    ) %>% 
  rename(
    conj1_sup_a=conj1_sup_1,
    conj1_sup_b=conj1_sup_2,
    conj2_sup_a=conj2_sup_1,
    conj2_sup_b=conj2_sup_2,
    conj3_sup_a=conj3_sup_1,
    conj3_sup_b=conj3_sup_2,
    conj4_sup_a=conj4_sup_1,
    conj4_sup_b=conj4_sup_2,
    conj5_sup_a=conj5_sup_1,
    conj5_sup_b=conj5_sup_2,
    
  ) %>% 
  pivot_longer(c(9:103, 140:159)) %>% 
  mutate(name = ifelse(str_count(name, "_")>=3,
                  str_replace(name, "([^_]+)_([^_]+)_([^_]+)", "\\1_\\2\\3"),
                  name))  %>% 
  separate(name, c("contestnr", "var", "profile")) %>% 
  subset(F== is.na(profile)) %>% 
  pivot_wider(names_from = var, names_prefix = "Feat", values_from = value) %>% 
  mutate(
    FeatGender_Female=ifelse(Featgender==2, 1,0),
    FeatGender=factor(Featgender, level = c(1,2), labels = c("Man", "Woman")),
    FeatAge_27_33=ifelse(Featageint <=33 , 1,0),
    FeatAge_34_49=ifelse(Featageint > 33 & Featageint <= 49 , 1,0),
    FeatAge_50_63=ifelse(Featageint > 49 & Featageint <= 63 , 1,0),
    FeatAge_64_74=ifelse(Featageint > 63 & Featageint <= 74 , 1,0),
    FeatAge=factor(Featage, level = c(1,2,3,4), labels = c("27-33", "34-49", "50-63", "64-74")),
    FeatOccupation_selfemployed = ifelse(Featoccupation == 1, 1, 0),
    FeatOccupation_farmer = ifelse(Featoccupation == 2, 1, 0),
    FeatOccupation_highschoolteacher = ifelse(Featoccupation == 3, 1, 0),
    FeatOccupation_doctor = ifelse(Featoccupation == 4, 1, 0),
    FeatOccupation_lawyer = ifelse(Featoccupation == 5, 1, 0),
    FeatOccupation=factor(Featoccupation, levels = c(1,2,3,4,5), labels = c("Self-employed", "Farmer", "High school teacher", "Doctor", "Lawyer")),
    FeatPartisanship_A = ifelse(Featpartisanship == 1, 1, 0),
    FeatPartisanship_B = ifelse(Featpartisanship == 2, 1, 0),
    FeatPartisanship_C = ifelse(Featpartisanship == 3, 1, 0),
    FeatPartisanship_D = ifelse(Featpartisanship == 4, 1, 0),
    FeatPartisanship_F = ifelse(Featpartisanship == 5, 1, 0),
    FeatPartisanship_O = ifelse(Featpartisanship == 6, 1, 0),
    FeatPartisanship_V = ifelse(Featpartisanship == 7, 1, 0),
    FeatPartisanship_OE = ifelse(Featpartisanship == 8, 1, 0),
    FeatPartisanship=factor(Featpartisanship, levels = c(5, 1,2,3,4,6,7,8), labels = c( "Socialist People's Party", "Social Democrats", "Radical Liberals", "Conservative People's Party", "The New Right", "Danish People's Party", "Liberals", "Unity List")),
    FeatDlocalism_ll = ifelse(Featdescriptivelocalism == 1, 1, 0),
    FeatDlocalism_al = ifelse(Featdescriptivelocalism == 2, 1, 0),
    FeatDlocalism_la = ifelse(Featdescriptivelocalism == 3, 1, 0),
    FeatDlocalism_aa = ifelse(Featdescriptivelocalism == 4, 1, 0),
    FeatDlocalism = factor(Featdescriptivelocalism, levels = c(4, 3, 2, 1), labels = c("Raised and lives elsewhere","Raised locally, lives elsewhere", "Raised elsewhere, lives locally", "Raised and lives locally")),
    FeatDlocalism_grownlocal = factor(ifelse(FeatDlocalism_la == 1 | FeatDlocalism_ll == 1, 1,0), levels = c(0, 1), labels = c("Raised elsewhere", "Raised locally")),
    FeatDlocalism_livelocal = factor(ifelse(FeatDlocalism_al == 1 | FeatDlocalism_ll == 1, 1,0), levels = c(0, 1), labels = c("Lives elsewhere", "Lives locally")),
    FeatBlocalism_null = ifelse(Featbehaviorallocalism == 1 , 1, 0),
    FeatBlocalism_plus = ifelse(Featbehaviorallocalism %in% c(2, 3, 4) , 1, 0),
    FeatBlocalism_low = ifelse(Featbehaviorallocalism == 4, 1, 0),
    FeatBlocalism_medium = ifelse(Featbehaviorallocalism == 3, 1, 0),
    FeatBlocalism_high = ifelse(Featbehaviorallocalism == 2, 1, 0),
    FeatBlocalism_positive = ifelse(FeatBlocalism_medium == 1 | FeatBlocalism_high== 1 , 1, 0),
    FeatBlocalism_negative = ifelse(FeatBlocalism_low == 1 , 1, 0),
    FeatBlocalism = factor(Featbehaviorallocalism, levels = c(1, 4, 3, 2), labels = c("No behavioral information", "Only works on national issues", "Splits time between national and local issues","Primarily works on local issues" )),
    FeatSlocalism_null = ifelse(Featsymboliclocalism == 1, 1, 0),
    FeatSlocalism_plus = ifelse(Featsymboliclocalism %in% c(2, 3, 4, 5), 1, 0),
    FeatSlocalism_low = ifelse(Featsymboliclocalism == 5, 1, 0),
    FeatSlocalism_lowmedium = ifelse(Featsymboliclocalism == 4, 1, 0),
    FeatSlocalism_mediumhigh = ifelse(Featsymboliclocalism == 3, 1, 0),
    FeatSlocalism_high = ifelse(Featsymboliclocalism == 2, 1, 0),
    FeatSlocalism_positive = ifelse(FeatSlocalism_mediumhigh==1 | FeatSlocalism_high==1, 1 , 0),
    FeatSlocalism_negative = ifelse(FeatSlocalism_low==1 | FeatSlocalism_lowmedium==1, 1 , 0),
    FeatSlocalism = factor(Featsymboliclocalism, levels = c(1, 5, 4, 3, 2), labels = c("No symbolic information ", "Not seen regularly", "Seen during campaign", "Knows some names, has been active locally", "Knows names, active locally")),
    
    Featlocalism=(ifelse(FeatDlocalism_ll==1, 1,
                         ifelse(FeatDlocalism_al==1, 1,
                                ifelse(FeatDlocalism_la==1, 0,
                                       ifelse(FeatDlocalism_aa==1, 0,NA))))+
                    ifelse(FeatBlocalism_high==1, 1,
                           ifelse(FeatBlocalism_medium==1, 2/3,
                                  ifelse(FeatBlocalism_low==1, 1/3,
                                         ifelse(FeatBlocalism_null==1, 0, NA))))+
                    ifelse(FeatSlocalism_high==1, 1,
                           ifelse(FeatSlocalism_mediumhigh==1, .75,
                                  ifelse(FeatSlocalism_lowmedium==1, .50,
                                         ifelse(FeatSlocalism_low==1, 0,
                                                ifelse(FeatSlocalism_null==1, 0.25, NA)))))
    )/3,
    
    
    FeatSup = ifelse(Featsup == 6, NA, 
               ifelse(Featsup == 1, 1,
               ifelse(Featsup == 2, 0.75,
               ifelse(Featsup == 3, 0.5 ,
               ifelse(Featsup == 4, 0.25,
               ifelse(Featsup == 5, 0, NA)))))),
    
    #Control variables at the respondent level.

    SameAge = factor(ifelse(age_recoded>74 & Featageint>70, 1,  #
                     ifelse(age_recoded<27 & Featageint<31, 1,
                     ifelse(Featageint > age_recoded-5 & Featageint < age_recoded+5, 1, 0))), levels = c(1, 0), labels = c("Similiar age", "Not similiar age")), #Same age defined as a candidate which is within a the same age as the respondent +- 5 years. For younger and older respondents, the interval is expanded to include the oldest and youngest candidates.
    SameGender = factor(ifelse(gender == 1 & FeatGender == "Woman"|
                        gender == 2 & FeatGender == "Man", 1,0), levels = c(1, 0), labels = c("Same gender", "Not same gender")),
    
    partisanship_party = ifelse(partisanship1 == 1 & partisanship2 < 16, partisanship2,
                            ifelse(partisanship1 %in% c(2,3) & partisanship4 < 16, partisanship4,
                            ifelse(partisanship1 == 1 & partisanship2 == 17 |
                            partisanship1 %in% c(2,3) & partisanship4 == 17, NA,
                            ifelse(partisanship1 %in% c(2,3) & partisanship4 == 18, "Independent",NA)))),
    partisanship_strength = ifelse(is.na(partisanship4)==F & partisanship4 == 18, 0, 
                               ifelse(is.na(partisanship4)==F & partisanship4 %in% c(1:16), 1/3,
                               ifelse(partisanship3 == 2, 2/3,
                               ifelse(partisanship3 == 1, 1, NA)))),
    
    SamePartisanship = factor(ifelse(partisanship_party == 1 & FeatPartisanship == "Social Democrats", 1, 
                   ifelse(partisanship_party == 2 & FeatPartisanship == "Radical Liberals", 1,
                   ifelse(partisanship_party == 3 & FeatPartisanship == "Conservative People's Party", 1,
                   ifelse(partisanship_party == 4 & FeatPartisanship == "The New Right", 1,
                   ifelse(partisanship_party == 6 & FeatPartisanship == "Socialist People's Party", 1,
                   ifelse(partisanship_party == 10 & FeatPartisanship == "Danish People's Party", 1,
                   ifelse(partisanship_party == 12 & FeatPartisanship == "Liberals", 1,
                   ifelse(partisanship_party == 13 & FeatPartisanship == "Unity List", 1, 0)))))))),levels = c(1, 0), labels = c("Same party", "Not same party")),
    
    SameBlock = factor(ifelse(partisanship_party %in% c(1,2,6,7,13,14,15) & FeatPartisanship %in% c("Social Democrats", 
                                                                                                "Radical Liberals",
                                                                                                "Socialist People's Party",
                                                                                                "Unity List") |
                         partisanship_party %in% c(3,4,5,8,9,10,11,12) &  FeatPartisanship %in% c("Conservative People's Party",
                                                                                                     "The New Right",
                                                                                                     "Danish People's Party",
                                                                                                     "Liberals"), 1,0), levels = c(1, 0), labels = c("Same block", "Not same block")),
    SameOccupation = factor(ifelse(FeatOccupation=="Lawyer" & employee_jobtitle==32, 1 ,
                     ifelse(FeatOccupation=="High school teacher" & employee_jobtitle==38, 1,
                     ifelse(FeatOccupation=="Farmer" & occupation2==5 , 1,
                     ifelse(FeatOccupation=="Doctor" & employee_jobtitle==3, 1,
                     ifelse(FeatOccupation=="Self-employed" & occupation2 %in% c(6,7,8), 1,0))))), levels = c(1, 0), labels = c("Same occupation", "Not same occutpation")),           
    gender = factor(gender, levels = c(2, 1), labels = c("Men", "Women")),
    house_type = factor(ifelse(house_type %in% c(1, 2, 3), "House", ifelse(house_type %in% c(4, 5), "Apartment", "Other")), levels = c("House", "Apartment", "Other")),
    age = factor(ifelse(age_recoded<=33, "-33]",
                 ifelse(age_recoded>33 & age_recoded<=49, "(33-49]",
                 ifelse(age_recoded>49 & age_recoded<=63, "(50-63]",
                 ifelse(age_recoded>63, "(63-", NA))))),
    education = factor(ifelse(profile_education==1, "Primary education",
                              ifelse(profile_education %in% c(2, 3), "Upper secondary education",
                                     ifelse(profile_education == 4, "Vocational education and training",
                                            ifelse(profile_education %in% c(5, 6), "Short cycle and bachelors",
                                                   ifelse(profile_education %in% c(7, 8), "Masters and PhDs",NA))))), levels = c("Primary education", "Upper secondary education", "Vocational education and training", "Short cycle and bachelors", "Masters and PhDs")),
    income = factor(personal_income_recoded, levels = c(1, 2, 3, 4), labels = c("Less than 200k", "200k-400k", "400k and more", "NA")),
    urban = factor(ifelse(urban %in% c(1, 2), "Large city (more than 100,000)",
                          ifelse(urban %in% c(3, 4), "Medium city (10,000 - 100,000)",
                                 ifelse(urban %in% c(5, 6), "Small city (less than 10,000) or rural", NA)))),
    
   #Construct incicies: 
   local_raw=(local1+local2+local3)/3,
   local=ifelse(is.na(local1) & is.na(local2) & is.na(local3), NA,
         ifelse(is.na(local1) & is.na(local2) , local3,
         ifelse(is.na(local2) & is.na(local3) , local1,
         ifelse(is.na(local1) & is.na(local3) , local2,
         ifelse(is.na(local1), (local2+local3)/2,
         ifelse(is.na(local2), (local1+local3)/2,
         ifelse(is.na(local3), (local1+local2)/2, local_raw))))))),
   
   Prime=ifelse(Prime==1,1,0)
      ) %>% 
  group_by(id) %>% 
    mutate(Supmean=mean(FeatSup,na.rm = T)) %>% 
    ungroup()

transformed_data <- transformed_data %>% 
    mutate(FeatSup2=FeatSup-Supmean,
           FeatAge2=Featageint^2,
           flocal=factor(ifelse(local<mean(transformed_data$local, na.rm=T), 0, ifelse(local>mean(transformed_data$local, na.rm=T) , 1, NA)), levels = c(0,1), labels = c("low", "high"))
           ) 


write_dta(transformed_data, "transformed_data.dta")

