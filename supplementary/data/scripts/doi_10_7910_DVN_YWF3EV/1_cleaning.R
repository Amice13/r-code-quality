#########################################################
#                                                       #
#Date created:  November 28, 2021                       #
#Created by: Chinemelu Okafor and Shivram Viswanathan   #
#Date updated:                                          #
#Updated by:                                            #
#Notes:                                                 #
#This code cleans Round 5 of the raw Afrobarometer data #
# "merged_r5_data.dta" and returns the cleaned data     #
# "ReplicationPaper_Afrobarometer_Cleaned.rds" for      #
# creating our measures.                                #
#                                                       #
#########################################################

####File Inputs####
input_raw_afrobarometer_dta <- 'data/raw/merged_r5_data.dta' #Raw afrobarometer data provided by authors

####File Outputs####
output_clean_afrobarometer_rds <- 'data/clean/ReplicationPaper_Afrobarometer_Cleaned.rds'  

####Reading in data####
afrobar_data <- read_dta(input_raw_afrobarometer_dta) #51587 observations

####Cleaning dataset and Recoding variables####
#1. Recoding Rural = 2(1) and Urban = 1(0)
afrobar_data$URBRUR[afrobar_data$URBRUR == 3] <- 1

afrobar_dataf <- afrobar_data %>% mutate(URBRUR = URBRUR-1)
afrobar_dataf <- afrobar_dataf %>% mutate(URBRUR = as.factor(URBRUR))

#2. Recoding Female = 2(1) and Male = 1(0)
afrobar_dataf <- afrobar_data %>% mutate(Q101 = Q101-1)
afrobar_dataf <- afrobar_dataf %>% mutate(Q101 = as.factor(Q101))

#3. Age: dropping refuse to answer, dont know, missing
afrobar_dataf <- filter(afrobar_dataf, !Q1 %in% c(-1, 998, 999))

#4. Education: dropping refuse to answer, dont know, missing
afrobar_dataf <- filter(afrobar_dataf, !Q97 %in% c(-1, 99))

#5. Education: dropping refuse to answer, dont know, missing
afrobar_dataf <- filter(afrobar_dataf, !Q97 %in% c(-1, 99))

#6. Present Living conditions: dropping refuse to answer, dont know, missing
afrobar_dataf <- filter(afrobar_dataf, !Q3B %in% c(-1, 9))

#7. Ethnicity: dropping refuse to answer, dont know, missing
afrobar_dataf <- filter(afrobar_dataf, !Q84 %in% c(9997, 9998, 9999, -1, 9990, 9995, 9900)) 
afrobar_dataf <- filter(afrobar_dataf, !is.na(Q84)) #28 countries; 41699 observations

####Defining initial list of questions and country sample####
listofqs <- c("Q11 Q12 Q13A Q13B Q13C Q13D Q14 Q15 Q16 Q17A Q17B Q17C Q18 Q19 Q20 Q21 Q22 Q23 Q24 Q25A Q25B Q26A Q26B Q26C Q26D Q26E Q27 Q28 Q29A Q29B Q29C Q30A Q30B Q30C Q30D Q31A Q31B Q31C Q32 Q33 Q34 Q35 Q36 Q37 Q38 Q39 Q40 Q41 Q42 Q43 Q44 Q45 Q46A Q46B Q46C Q46D Q47A Q47B Q47C Q47D Q47E Q48A Q48B Q48C Q49 Q50 Q51 Q52A Q52B Q52C Q52D Q52E Q53 Q54 Q55 Q56A Q56B Q56C Q56D Q56E Q56F Q56G Q56H Q56I Q57 Q58 Q59A Q59B Q59C Q59D Q59E Q59F Q59G Q59H Q59I Q59J Q60A Q60B Q60C Q60D Q60E Q60F Q60G Q61A Q61B Q61C Q61D Q61E Q61F Q62A Q62B Q63PT1 Q63PT2 Q63PT3 Q64 Q65A Q65B Q65C Q65D Q65E Q65F Q65G Q65H Q65I Q65J Q65K Q65L Q65M Q65N Q65O Q65P Q66A Q66B Q66C Q66D Q66E Q67A Q67B Q67C Q67D Q67E Q68A Q68B Q68C Q68D Q68E Q68F Q69A Q69B Q69C Q69D Q69E Q69F Q70A Q70B Q70C Q71A Q71B Q71C Q72A Q72B Q72C Q73A Q73B Q73C Q73D Q73E Q74 Q75A Q75B Q75C Q76A Q76B Q77 Q78 Q78A1 Q85A Q85B Q85C Q86A Q86B Q86C Q86D Q86E Q86F Q87 Q88A Q88B Q88C Q89A Q98A Q98B")
listofqs <- strsplit(listofqs, " ")
listofqs <- unlist(listofqs)

listofqs2 <- c("Q13A Q13B Q13C Q13D Q14 Q15 Q16 Q17A Q17B Q17C Q18 Q19 Q20 Q21 Q22 Q23 Q24 Q25A Q25B Q26A Q26B Q26C Q26D Q26E Q27 Q28 Q29A Q29B Q29C Q30A Q30B Q30C Q30D Q31A Q31B Q31C Q32 Q33 Q34 Q35 Q36 Q37 Q38 Q39 Q40 Q41 Q42 Q43 Q44 Q45 Q48A Q48B Q48C Q49 Q50 Q51 Q52A Q52B Q52C Q52D Q52E Q53 Q54 Q55 Q56A Q56B Q56C Q56D Q56E Q56F Q56G Q56H Q56I Q57 Q58 Q59A Q59B Q59C Q59D Q59E Q59F Q59G Q59H Q59I Q59J Q60A Q60B Q60C Q60D Q60E Q60F Q60G Q61A Q61B Q61C Q61D Q61E Q61F Q62A Q62B Q64 Q65A Q65B Q65C Q65D Q65E Q65F Q65G Q65H Q65I Q65J Q65K Q65L Q65M Q65N Q65O Q65P Q66A Q66B Q66C Q66D Q66E Q67A Q67B Q67C Q67D Q67E Q68A Q68B Q68C Q68D Q68E Q68F Q69A Q69B Q69C Q69D Q69E Q69F Q70A Q70B Q70C Q71A Q71B Q71C Q72A Q72B Q72C Q73A Q73B Q73C Q73D Q73E Q74 Q75A Q75B Q75C Q76A Q76B Q78 Q78A1 Q85A Q85B Q85C Q86A Q86B Q86C Q86D Q86E Q86F Q87 Q88A Q88B Q88C Q89A Q98B")
listofqs2 <- strsplit(listofqs2, " ")
listofqs2 <- unlist(listofqs2)

#All countries
countno <- c("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28")
countno <- strsplit(countno, " ")
countno <- unlist(countno)

#Benin and Senegal
countno_ben_sen <- c("1 15")
countno_ben_sen <- strsplit(countno_ben_sen, " ")
countno_ben_sen <- unlist(countno_ben_sen)

#Benin Ethnicities: 103 34 107 102v27 108 105 110 109 10 1
#Senegal Ethnicities: 660 661 21 664 662 663 665 666 668 669

#Replacing missing, nonanswered, skipped values in 1st list of questions with NA
indexlistofqs <- grepl(paste(listofqs,collapse="|"), names(afrobar_dataf)) 
afrobar_dataf[indexlistofqs] <- lapply(afrobar_dataf[indexlistofqs], function(i) replace(i, i %in% c(-1, 9995, 9998, 9999, 998, 99, 98), NA))

#Replacing missing, nonanswered, skipped values in 2nd list of questions with NA
indexlistofqs2 <- grepl(paste(listofqs2,collapse="|"), names(afrobar_dataf)) 
afrobar_dataf[indexlistofqs2] <- lapply(afrobar_dataf[indexlistofqs2], function(i) replace(i, i %in% c(9), NA))

#More variable adjustments
listofqs5 <- c("Q18 Q19 Q20 Q21 Q22 Q23 Q24 Q33 Q34 Q35 Q36 Q37 Q38 Q39 Q40 Q41 Q49 Q50 Q51 Q78")
listofqs5 <- strsplit(listofqs5, " ")
listofqs5 <- unlist(listofqs5)
indexlistofqs5 <- grepl(paste(listofqs5,collapse="|"), names(afrobar_dataf)) 
afrobar_dataf[indexlistofqs5] <- lapply(afrobar_dataf[indexlistofqs5], function(i) replace(i, i %in% c(5), NA))

listofqs8 <- c("Q28 Q42 Q73A Q73C Q89A Q98B")
listofqs8 <- strsplit(listofqs8, " ")
listofqs8 <- unlist(listofqs8)
indexlistofqs8 <- grepl(paste(listofqs8,collapse="|"), names(afrobar_dataf)) 
afrobar_dataf[indexlistofqs8] <- lapply(afrobar_dataf[indexlistofqs8], function(i) replace(i, i %in% c(8), NA))

listofqs7 <- c("Q61A Q61B Q61C Q61D Q61E Q64 Q68A Q68B Q68C Q68D Q68E Q68F Q69A Q69B Q69C Q69D Q69E Q69F Q73C Q73D Q73E Q75C")
listofqs7 <- strsplit(listofqs7, " ")
listofqs7 <- unlist(listofqs7)
indexlistofqs7 <- grepl(paste(listofqs7,collapse="|"), names(afrobar_dataf)) 
afrobar_dataf[indexlistofqs7] <- lapply(afrobar_dataf[indexlistofqs7], function(i) replace(i, i %in% c(7), NA))

listofqs9996 <- c("Q63PT2 Q63PT3")
listofqs9996 <- strsplit(listofqs9996, " ")
listofqs9996 <- unlist(listofqs9996)
indexlistofqs9996 <- grepl(paste(listofqs9996,collapse="|"), names(afrobar_dataf)) 
afrobar_dataf[indexlistofqs9996] <- lapply(afrobar_dataf[indexlistofqs9996], function(i) replace(i, i %in% c(9996), NA))

#Our final list of "cultural" questions
finalqlist <- c("Q73A Q73B Q73C Q73D Q73E Q29A Q29B Q29C Q70A Q70B Q70C Q86A Q86B Q86C Q86D Q86E Q86F Q87 Q18 Q19 Q20 Q21 Q22 Q23 Q24 Q28 Q33 Q34 Q35 Q36 Q37 Q38 Q39 Q40 Q41 Q42 Q49 Q50 Q51 Q61A Q61B Q61C Q61D Q61E Q64 Q68A Q68B Q68C Q68D Q68E Q68F Q69A Q69B Q69C Q69D Q69E Q69F Q75C Q78 Q85A Q85B Q89A Q98B Q13A Q13B Q13C Q13D Q14 Q15 Q16 Q17A Q17B Q17C Q25A Q25B Q26A Q26B Q26C Q26D Q26E Q30A Q30B Q30C Q30D Q31A Q31B Q31C Q32 Q43 Q46A Q46B Q46C Q46D Q47A Q47B Q47C Q47D Q47E Q48A Q48B Q48C Q52A Q52B Q52C Q52D Q52E Q53 Q54 Q55 Q56A Q56B Q56C Q56D Q56E Q56F Q56G Q56H Q56I Q59A Q59B Q59C Q59D Q59E Q59F Q59G Q59H Q59I Q59J Q60A Q60B Q60C Q60D Q60E Q60F Q60G Q61F Q62A Q62B Q65A Q65B Q65C Q65D Q65E Q65F Q65G Q65H Q65I Q65J Q65K Q65L Q65M Q65N Q65O Q65P Q66A Q66B Q66C Q66D Q66E Q67A Q67B Q67C Q67D Q67E Q71A Q71B Q71C Q74 Q75A Q75B Q76A Q76B Q78A1 Q85C Q88A Q88B Q88C Q11 Q12 Q27 Q44 Q45 Q57 Q58 Q63PT1 Q63PT2 Q63PT3 Q72A Q72B Q72C Q77 Q98A")

finalqlist <- strsplit(finalqlist, " ")
finalqlist <- unlist(finalqlist)

####Subsetting data and saving cleaned dataset####
afrobar_dataf <- afrobar_dataf %>% select(c(COUNTRY, Q84, finalqlist))

saveRDS(afrobar_dataf, file = output_clean_afrobarometer_rds) #41699 observations, 194 Columns




