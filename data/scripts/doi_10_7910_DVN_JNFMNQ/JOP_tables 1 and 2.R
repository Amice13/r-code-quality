
#Genetic Attributions

data <- read.csv(file.choose())
attach(data)



attach(data)


##Table 1: Attribution of 18 Traits to Genetics, the Environment, and Personal Choice

Gene_Attr_Means <-data.frame (Handedness_genetic , Height_genetic , Obese_genetic , Eating_Disorder_genetic , Anxious_genetic , Addict_Drugs_genetic , Mental_Disability_genetic ,
                              Intelligence_genetic , Athletic_Ability_genetic , Aptitude_Language_genetic , Extroverted_genetic , Conscientiousness_genetic , Openness_Experiences_genetic , 
                              Being_Homosexual_genetic , Criminal_Record_genetic , Tolerant_Differences_genetic , Political_Ideology_genetic , Deeply_Religious_genetic)
summary(Gene_Attr_Means)

Environment_Attr_Means <- data.frame( Handedness_environment , Height_environment , Obese_environment , Eating_Disorder_environment , Anxious_environment , Addict_Drugs_environment , Mental_Disability_environment ,
                                      Intelligence_environment , Athletic_Ability_environment , Aptitude_Language_environment , Extroverted_environment , Conscientiousness_environment , Openness_Experiences_environment , 
                                      Being_Homosexual_environment , Criminal_Record_environment , Tolerant_Differences_environment , Political_Ideology_environment , Deeply_Religious_environment)
summary(Environment_Attr_Means)

Choice_Attr_Means <- data.frame(Handedness_choice , Height_choice , Obese_choice , Eating_Disorder_choice , Anxious_choice , Addict_Drugs_choice , Mental_Disability_choice ,
                                Intelligence_choice , Athletic_Ability_choice , Aptitude_Language_choice , Extroverted_choice , Conscientiousness_choice , Openness_Experiences_choice , 
                                Being_Homosexual_choice , Criminal_Record_choice , Tolerant_Differences_choice , Political_Ideology_choice , Deeply_Religious_choice)
summary(Choice_Attr_Means)

##Table 2: Partial Correlations for Ideology, Tolerance, and Genetic Attributions 
install.packages("ppcor")
library(ppcor)

IdeoCorr.data <- data.frame( Conservative_Ideology , Mean_Genetic_Attr , Female , Age , White , Religious_Attendance , Education , Income)

IdeoCorr.data <- na.omit(IdeoCorr.data)

pcor(IdeoCorr.data)


Toler_Homosexuals.data <- data.frame(Tolerance_Homosexuals, Being_Homosexual_genetic, Female , Age , White , Religious_Attendance , 
                                     Education , Income ,Conservative_Ideology, Republican_Partisanship, DescribeAs_Homosexual) 


Toler_Homosexuals.data<- na.omit(Toler_Homosexuals.data)


pcor(Toler_Homosexuals.data)

Toler_DrugAddict.data <- data.frame(Tolerance_DrugAddicts, Addict_Drugs_genetic, Female , Age , White , Religious_Attendance , 
                                    Education , Income ,Conservative_Ideology, Republican_Partisanship, DescribeAs_DrugAddict) 

Toler_DrugAddict.data<- na.omit(Toler_DrugAddict.data)

pcor(Toler_DrugAddict.data)



Toler_Obese.data <- data.frame(Tolerance_Obese, Obese_genetic, Female , Age , White , Religious_Attendance , 
                               Education , Income ,Conservative_Ideology, Republican_Partisanship, DescribeAs_Obese) 

Toler_Obese.data<- na.omit(Toler_Obese.data)

pcor(Toler_Obese.data)


Toler_MentalDisabled.data <- data.frame(Tolerance_MentalDisabled, Mental_Disability_genetic, Female , Age , White , Religious_Attendance , 
                                        Education , Income ,Conservative_Ideology, Republican_Partisanship, DescribeAs_MentalDisabled) 


Toler_MentalDisabled.data<- na.omit(Toler_MentalDisabled.data)

pcor(Toler_MentalDisabled.data)



Toler_OpposeIdeology.data <- data.frame(Tolerance_OpposeIdeology, Political_Ideology_genetic, Female , Age , White , Religious_Attendance , 
                                        Education , Income ,Conservative_Ideology, Republican_Partisanship, DescribeAs_Political) 

Toler_OpposeIdeology.data<- na.omit(Toler_OpposeIdeology.data)

pcor(Toler_OpposeIdeology.data)




