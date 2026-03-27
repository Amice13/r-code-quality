# Libraries
install.packages(c("plyr"))
library(plyr)

##########################
#### Organize data on Conservatives
##########################

# Read in the Conservative data
cons <- read.csv("Exp_conservative.csv",stringsAsFactors = F)

# Create vote_loyal variable
cons$vote_loyal <- as.numeric(mapvalues(cons$vote_choice, c("Labour","Conservative","Liberal Democrat","United Kingdom Independence Party (UKIP)","Other","Scottish National Party (SNP)","Green Party"), c(0,1,rep(0,5))))

# Create age variable
cons$age <- 2017 - as.numeric(cons$year_born)
cons$age_sq <- (cons$age)^2

# Create male variable
cons$male <- ifelse(cons$gender=="Male", 1,0)

# Recode educ_level to numeric that ranges from 1 to 3
cons$educ_level_numeric <- cons$educ_level
cons$educ_level_numeric <- as.numeric(as.character(mapvalues(cons$educ_level_numeric,c(  "City and Guild certificated - advanced" ,               
                                                                                         "University or CNAA first degree (eg BA, B.Sc, B.Ed)"  , 
                                                                                         "GCE A level or Higher Certificate"                    , 
                                                                                         "Other technical, professional or higher qualification)",
                                                                                         "City and Guild certificate"                            ,
                                                                                         "Teaching qualification (not degree)"                   ,
                                                                                         "CSE grade 1, GCE O level, GCSE, School Certificate"   , 
                                                                                         "University diploma"                                   , 
                                                                                         "University or CNAA higher degree (eg M.Sc, Ph.D)"      ,
                                                                                         "Recognised trade apprenticeship completed"            , 
                                                                                         "No formal qualifications"                              ,
                                                                                         "Scottish Higher Certificate"                           ,
                                                                                         "CSE grades 2-5"                                        ,
                                                                                         "Youth training certificate/skillseekers"               ,
                                                                                         "Scottish Ordinary/Lower Certificate"                   ,
                                                                                         "Clerical and commercial"                               ,
                                                                                         "Nursing qualification (eg SEN, SRN, SCM, RGN)", "ONC"
                              ),c(1,3,2,3,1,2,1,2,3,1,1,2,1,1,1,1,2,1))))

# Recode educ_level to numeric that ranges from 2 to 8
cons$educ_level_numeric2 <- cons$educ_level
cons$educ_level_numeric2 <- as.numeric(as.character(mapvalues(cons$educ_level_numeric2,c( "City and Guild certificated - advanced" ,               
                                                                                          "University or CNAA first degree (eg BA, B.Sc, B.Ed)"  , 
                                                                                          "GCE A level or Higher Certificate"                    , 
                                                                                          "Other technical, professional or higher qualification)",
                                                                                          "City and Guild certificate"                            ,
                                                                                          "Teaching qualification (not degree)"                   ,
                                                                                          "CSE grade 1, GCE O level, GCSE, School Certificate"   , 
                                                                                          "University diploma"                                   , 
                                                                                          "University or CNAA higher degree (eg M.Sc, Ph.D)"      ,
                                                                                          "Recognised trade apprenticeship completed"            , 
                                                                                          "No formal qualifications"                              ,
                                                                                          "Scottish Higher Certificate"                           ,
                                                                                          "CSE grades 2-5"                                        ,
                                                                                          "Youth training certificate/skillseekers"               ,
                                                                                          "Scottish Ordinary/Lower Certificate"                   ,
                                                                                          "Clerical and commercial"                               ,
                                                                                          "Nursing qualification (eg SEN, SRN, SCM, RGN)","ONC"  ),
                                                              c(4,7,5,8,3,6,4,6,8,3,2,5,3,3,4,3,6,4))))

# Create educ_age2
unique(cons$educ_age) # Assign 0 to "I received no formal education." Assign value of educ_age_2_TEXT to those answered "I completed education at the age of:". Assign current age to those who answered "I am still in school."
cons$educ_age2 <- cons$educ_age
cons$educ_age2[cons$educ_age=="I received no formal education."] <- 0
cons$educ_age_2_TEXT <- as.numeric(cons$educ_age_2_TEXT)
cons$educ_age2[cons$educ_age=="I completed education at the age of:"] <- cons$educ_age_2_TEXT[cons$educ_age=="I completed education at the age of:"]
cons$educ_age2[cons$educ_age=="I am still in school."] <- cons$age[cons$educ_age=="I am still in school."]
cons$educ_age2 <- as.numeric(cons$educ_age2)

# Manipulation check
cons$manipulation_neg <- as.numeric(mapvalues(cons$manipulation, c("Very positive","Somewhat positive","Neither negative nor positive","Somewhat negative","Very negative"), 1:5))

##########################
#### Organize data on Labour
##########################

# Read in the Labour data
lab <- read.csv("Exp_labour.csv",stringsAsFactors = F)

# Create vote_loyal variable
lab$vote_loyal <- as.numeric(mapvalues(lab$vote_choice, c("Labour","Conservative","Liberal Democrat","United Kingdom Independence Party (UKIP)","Other","Scottish National Party (SNP)","Green Party"), c(1,0,rep(0,5))))

# Create age variable
lab$age <- 2017 - as.numeric(as.character(lab$year_born))
lab$age_sq <- (lab$age)^2

# Create male variable
lab$male <- ifelse(lab$gender=="Male", 1,0)

# Recode educ_level to numeric that ranges from 1 to 3
lab$educ_level_numeric <- lab$educ_level
lab$educ_level_numeric <- as.numeric(as.character(mapvalues(lab$educ_level_numeric,c(  "City and Guild certificated - advanced" ,               
                                                                                       "University or CNAA first degree (eg BA, B.Sc, B.Ed)"  , 
                                                                                       "GCE A level or Higher Certificate"                    , 
                                                                                       "Other technical, professional or higher qualification)",
                                                                                       "City and Guild certificate"                            ,
                                                                                       "Teaching qualification (not degree)"                   ,
                                                                                       "CSE grade 1, GCE O level, GCSE, School Certificate"   , 
                                                                                       "University diploma"                                   , 
                                                                                       "University or CNAA higher degree (eg M.Sc, Ph.D)"      ,
                                                                                       "Recognised trade apprenticeship completed"            , 
                                                                                       "No formal qualifications"                              ,
                                                                                       "Scottish Higher Certificate"                           ,
                                                                                       "CSE grades 2-5"                                        ,
                                                                                       "Youth training certificate/skillseekers"               ,
                                                                                       "Scottish Ordinary/Lower Certificate"                   ,
                                                                                       "Clerical and commercial"                               ,
                                                                                       "Nursing qualification (eg SEN, SRN, SCM, RGN)"  ),c(1,3,2,3,1,2,1,2,3,1,1,2,1,1,1,1,2))))

# Recode educ_level to numeric that ranges from 2 to 8
lab$educ_level_numeric2 <- lab$educ_level
lab$educ_level_numeric2 <- as.numeric(as.character(mapvalues(lab$educ_level_numeric2,c( "City and Guild certificated - advanced" ,               
                                                                                        "University or CNAA first degree (eg BA, B.Sc, B.Ed)"  , 
                                                                                        "GCE A level or Higher Certificate"                    , 
                                                                                        "Other technical, professional or higher qualification)",
                                                                                        "City and Guild certificate"                            ,
                                                                                        "Teaching qualification (not degree)"                   ,
                                                                                        "CSE grade 1, GCE O level, GCSE, School Certificate"   , 
                                                                                        "University diploma"                                   , 
                                                                                        "University or CNAA higher degree (eg M.Sc, Ph.D)"      ,
                                                                                        "Recognised trade apprenticeship completed"            , 
                                                                                        "No formal qualifications"                              ,
                                                                                        "Scottish Higher Certificate"                           ,
                                                                                        "CSE grades 2-5"                                        ,
                                                                                        "Youth training certificate/skillseekers"               ,
                                                                                        "Scottish Ordinary/Lower Certificate"                   ,
                                                                                        "Clerical and commercial"                               ,
                                                                                        "Nursing qualification (eg SEN, SRN, SCM, RGN)"  ),
                                                             c(4,7,5,8,3,6,4,6,8,3,2,5,3,3,4,3,6))))

# Create educ_age2
lab$educ_age2 <- lab$educ_age
lab$educ_age2[lab$educ_age=="I received no formal education."] <- 0
lab$educ_age_2_TEXT <- as.numeric(lab$educ_age_2_TEXT)
lab$educ_age2[lab$educ_age=="I completed education at the age of:"] <- lab$educ_age_2_TEXT[lab$educ_age=="I completed education at the age of:"]
lab$educ_age2[lab$educ_age=="I am still in school."] <- lab$age[lab$educ_age=="I am still in school."]
lab$educ_age2 <- as.numeric(lab$educ_age2)

# Manipulation check
lab$manipulation_neg <- as.numeric(mapvalues(lab$manipulation, c("Very positive","Somewhat positive","Neither negative nor positive","Somewhat negative","Very negative"), 1:5))

##########################
#### Create combined data
##########################

# Create indicator variable for labour
lab$lab <- 1
cons$lab <- 0

# Combine the two
combined <- merge(lab, cons, all=T)

##########################
#### Save the three data objects
##########################

lab <- lab[,c("manipulation_neg","vote_loyal","treatment","age","age_sq","male","educ_age2","educ_level_numeric","educ_level_numeric2","ethnic","lab")]
cons <- cons[,c("manipulation_neg","vote_loyal","treatment","age","age_sq","male","educ_age2","educ_level_numeric","educ_level_numeric2","ethnic","lab")]
combined <- combined[,c("manipulation_neg","vote_loyal","treatment","age","age_sq","male","educ_age2","educ_level_numeric","educ_level_numeric2","ethnic","lab")]
save(lab,cons,combined,file="Study2.RData")
