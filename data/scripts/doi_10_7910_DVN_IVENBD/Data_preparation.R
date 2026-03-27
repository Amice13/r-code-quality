# DATA-PREPARATION FOR 
##"The Personalization of Electoral Participation? 
###The Relationship Between Trait Evaluations of Presidential Candidates and Turnout Decisions in American Presidential Elections 1980-2020"
###By Segerberg, Tim (2024)

# The datafile used for the analysis is ANES's Time Series Cumulative Data File 1948-2020 that is found on the following URL:
## https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/

#Clear everything
rm(list = ls())
#ANES DATA
library(haven)
library(tidyverse)
anes_timeseries_cdf_stata_20220916 <- read_dta("anes_timeseries_cdf_stata_20220916.dta")
ANES<-anes_timeseries_cdf_stata_20220916
ANES<-ANES[ANES$VCF0004 > 1978,]
ANES<-ANES[ANES$VCF0004 != 1982,]
ANES<-ANES[ANES$VCF0004 != 1986,]
ANES<-ANES[ANES$VCF0004 != 1990,]
ANES<-ANES[ANES$VCF0004 != 1994,]
ANES<-ANES[ANES$VCF0004 != 1998,]
ANES<-ANES[ANES$VCF0004 != 2002,]
ANES$VCF0004<-na.omit(ANES$VCF0004)
ANES$election_year <- na.omit(ANES$VCF0004)

#Recoding the variables
  #COMPETENCE-variables
    #Knowledgable 1980-2008
      #Original values: 1: Extremely well, 2: Quite well, 3: Not too well, 4: Not well at all, 8: DK, 9: NA
        #New values: 0: Not well at all, 1: Not too well, 2: Quite well, 3: Extremely well
#Democratic candidate
ANES$dem_knowledge<-ANES$VCF0354
ANES$dem_knowledge[ANES$VCF0354 == 1]<-3 #Extremely knowledgable 
ANES$dem_knowledge[ANES$VCF0354 == 2]<-2
ANES$dem_knowledge[ANES$VCF0354 == 3]<-1
ANES$dem_knowledge[ANES$VCF0354 == 4]<-0 #Not knowledgable at all
ANES$dem_knowledge[ANES$VCF0354 == 8]<-NA
ANES$dem_knowledge[ANES$VCF0354 == 9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$dem_knowledge<-(ANES$dem_knowledge-0)/(3-0)
#Republican candidate
ANES$rep_knowledge<-ANES$VCF0366
ANES$rep_knowledge[ANES$VCF0366 == 1]<-3 #Extremely knowledgable 
ANES$rep_knowledge[ANES$VCF0366 == 2]<-2
ANES$rep_knowledge[ANES$VCF0366 == 3]<-1
ANES$rep_knowledge[ANES$VCF0366 == 4]<-0 #Not knowledgable at all
ANES$rep_knowledge[ANES$VCF0366 == 8]<-NA
ANES$rep_knowledge[ANES$VCF0366 == 9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$rep_knowledge<-(ANES$rep_knowledge-0)/(3-0)

    #Intelligence (1984-2008 NOT 1996)
      #Original values: 1: Extremely well, 2: Quite well, 3: Not too well, 4: Not well at all, 8: DK, 9: NA
        #New values: 0: Not well at all, 1: Not too well, 2: Quite well, 3: Extremely 
#Demoratic Candidate
ANES$dem_intelligence<-ANES$VCF0350
ANES$dem_intelligence[ANES$VCF0350 == 1]<-3 #Extremely intelligent 
ANES$dem_intelligence[ANES$VCF0350 == 2]<-2
ANES$dem_intelligence[ANES$VCF0350 == 3]<-1
ANES$dem_intelligence[ANES$VCF0350 == 4]<-0 #Not intelligient at all
ANES$dem_intelligence[ANES$VCF0350 == 8]<-NA
ANES$dem_intelligence[ANES$VCF0350 == 9]<-NA
### Remove 1996, since data is missing for Republican candidates this year.
ANES$dem_intelligence<-ifelse(ANES$VCF0004 != 1996, ANES$dem_intelligence, NA)
#Normating the variable for the index, ranging between 0 and 1 
#Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$dem_intelligence<-(ANES$dem_intelligence-0)/(3-0)

#Republican candidate
ANES$rep_intelligence<-ANES$VCF0362
ANES$rep_intelligence[ANES$VCF0362 == 1]<-3 #Extremely intelligent 
ANES$rep_intelligence[ANES$VCF0362 == 2]<-2
ANES$rep_intelligence[ANES$VCF0362 == 3]<-1
ANES$rep_intelligence[ANES$VCF0362 == 4]<-0 #Not intelligient at all
ANES$rep_intelligence[ANES$VCF0362 == 8]<-NA
ANES$rep_intelligence[ANES$VCF0362 == 9]<-NA
ANES$rep_intelligence<-(ANES$rep_intelligence-0)/(3-0)

    #Strong leadership 1980-2008
      #Original values: 1: Extremely well, 2: Quite well, 3: Not too well, 4: Not well at all, 8: DK, 9: NA
        #New values: 0: Not well at all, 1: Not too well, 2: Quite well, 3: Extremely well
#Democratic candidate
ANES$dem_leadership<-ANES$VCF0356
ANES$dem_leadership[ANES$VCF0356 == 1]<-3 #Extremely strong leadership 
ANES$dem_leadership[ANES$VCF0356 == 2]<-2
ANES$dem_leadership[ANES$VCF0356 == 3]<-1
ANES$dem_leadership[ANES$VCF0356 == 4]<-0 #Not strong leadership at all
ANES$dem_leadership[ANES$VCF0356 == 8]<-NA
ANES$dem_leadership[ANES$VCF0356 == 9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$dem_leadership<-(ANES$dem_leadership-0)/(3-0)
#Republican candidate
ANES$rep_leadership<-ANES$VCF0368
ANES$rep_leadership[ANES$VCF0368 == 1]<-3 #Extremely strong leadership 
ANES$rep_leadership[ANES$VCF0368 == 2]<-2
ANES$rep_leadership[ANES$VCF0368 == 3]<-1
ANES$rep_leadership[ANES$VCF0368 == 4]<-0 #Not strong leadership at all
ANES$rep_leadership[ANES$VCF0368 == 8]<-NA
ANES$rep_leadership[ANES$VCF0368 == 9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$rep_leadership<-(ANES$rep_leadership-0)/(3-0)
   #CHARACTER-variables
    #Moral 1980-2008
      #Original values: 1: Extremely well, 2: Quite well, 3: Not too well, 4: Not well at all, 8: DK, 9: NA
        #New values: 0: Not well at all, 1: Not too well, 2: Quite well, 3: Extremely well
#Democratic candidate
ANES$dem_moral<-ANES$VCF0355
ANES$dem_moral[ANES$VCF0355 == 1]<-3 #Extremely moral  
ANES$dem_moral[ANES$VCF0355 == 2]<-2
ANES$dem_moral[ANES$VCF0355 == 3]<-1
ANES$dem_moral[ANES$VCF0355 == 4]<-0 #Not moral at all
ANES$dem_moral[ANES$VCF0355 == 8]<-NA
ANES$dem_moral[ANES$VCF0355 == 9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$dem_moral<-(ANES$dem_moral-0)/(3-0)
#Republican candidate
ANES$rep_moral<-ANES$VCF0367
ANES$rep_moral[ANES$VCF0367 == 1]<-3 #Extremely moral  
ANES$rep_moral[ANES$VCF0367 == 2]<-2
ANES$rep_moral[ANES$VCF0367 == 3]<-1
ANES$rep_moral[ANES$VCF0367 == 4]<-0 #Not moral at all
ANES$rep_moral[ANES$VCF0367 == 8]<-NA
ANES$rep_moral[ANES$VCF0367 == 9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$rep_moral<-(ANES$rep_moral-0)/(3-0)
    #Inspriring 1980-1996
      #Original values: 1: Extremely well, 2: Quite well, 3: Not too well, 4: Not well at all, 8: DK, 9: NA
        #New values: 0: Not well at all, 1: Not too well, 2: Quite well, 3: Extremely well
#Democratic candidate
ANES$dem_inspiring<-ANES$VCF0353
ANES$dem_inspiring[ANES$VCF0353 == 1]<-3 #Extremely inspiring  
ANES$dem_inspiring[ANES$VCF0353 == 2]<-2
ANES$dem_inspiring[ANES$VCF0353 == 3]<-1
ANES$dem_inspiring[ANES$VCF0353 == 4]<-0 #Not inspiring at all
ANES$dem_inspiring[ANES$VCF0353 == 8]<-NA
ANES$dem_inspiring[ANES$VCF0353 == 9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$dem_inspiring<-(ANES$dem_inspiring-0)/(3-0)
#Republican candidate
ANES$rep_inspiring<-ANES$VCF0365
ANES$rep_inspiring[ANES$VCF0365 == 1]<-3 #Extremely inspiring  
ANES$rep_inspiring[ANES$VCF0365 == 2]<-2
ANES$rep_inspiring[ANES$VCF0365 == 3]<-1
ANES$rep_inspiring[ANES$VCF0365 == 4]<-0 #Not inspiring at all
ANES$rep_inspiring[ANES$VCF0365 == 8]<-NA
ANES$rep_inspiring[ANES$VCF0365 == 9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$rep_inspiring<-(ANES$rep_inspiring-0)/(3-0)
    #Really cares about people like you 1984-2008
      #Original values: 1: Extremely well, 2: Quite well, 3: Not too well, 4: Not well at all, 8: DK, 9: NA
        #New values: 0: Not well at all, 1: Not too well, 2: Quite well, 3: Extremely well
#Democratic candidate
ANES$dem_caring<-ANES$VCF0357
ANES$dem_caring[ANES$VCF0357 == 1]<-3 #Extremely caring  
ANES$dem_caring[ANES$VCF0357 == 2]<-2
ANES$dem_caring[ANES$VCF0357 == 3]<-1
ANES$dem_caring[ANES$VCF0357 == 4]<-0 #Not caring at all
ANES$dem_caring[ANES$VCF0357 == 8]<-NA
ANES$dem_caring[ANES$VCF0357 == 9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$dem_caring<-(ANES$dem_caring-0)/(3-0)
#Republican candidate
ANES$rep_caring<-ANES$VCF0369
ANES$rep_caring[ANES$VCF0369 == 1]<-3 #Extremely caring  
ANES$rep_caring[ANES$VCF0369 == 2]<-2
ANES$rep_caring[ANES$VCF0369 == 3]<-1
ANES$rep_caring[ANES$VCF0369 == 4]<-0 #Not caring at all
ANES$rep_caring[ANES$VCF0369 == 8]<-NA
ANES$rep_caring[ANES$VCF0369 == 9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$rep_caring<-(ANES$rep_caring-0)/(3-0)

  #COMPETENCE-variables
    #Knowledgable 2012-2020
      #Original values: 1: Extremely well, 2: Very well, 3: Moderately well, 4: Slightly well, 5: Not well at all, -8: DK, -9: NA
        #New values: 0: Not well at all, 1: Slightly well, 2: Moderately well, 3: Very well, 4: Extremely well
#Democratic candidate
ANES$dem_knowledge_2012_2020<-ANES$VCF9211
ANES$dem_knowledge_2012_2020[ANES$VCF9211 == 1]<-4 #Extremely knowledgable 
ANES$dem_knowledge_2012_2020[ANES$VCF9211 == 2]<-3
ANES$dem_knowledge_2012_2020[ANES$VCF9211 == 3]<-2
ANES$dem_knowledge_2012_2020[ANES$VCF9211 == 4]<-1
ANES$dem_knowledge_2012_2020[ANES$VCF9211 == 5]<-0 #Not knowledgable at all
ANES$dem_knowledge_2012_2020[ANES$VCF9211 == -8]<-NA
ANES$dem_knowledge_2012_2020[ANES$VCF9211 == -9]<-NA
ANES$dem_knowledge_2012_2020[ANES$VCF0004 == 2008]<-NA #Remove 2008, since it is measured in VCF0354
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$dem_knowledge_2012_2020<-(ANES$dem_knowledge_2012_2020-0)/(4-0)
#Republican candidate
ANES$rep_knowledge_2012_2020<-ANES$VCF9215
ANES$rep_knowledge_2012_2020[ANES$VCF9215 == 1]<-4 #Extremely knowledgable 
ANES$rep_knowledge_2012_2020[ANES$VCF9215 == 2]<-3
ANES$rep_knowledge_2012_2020[ANES$VCF9215 == 3]<-2
ANES$rep_knowledge_2012_2020[ANES$VCF9215 == 4]<-1
ANES$rep_knowledge_2012_2020[ANES$VCF9215 == 5]<-0 #Not knowledgable at all
ANES$rep_knowledge_2012_2020[ANES$VCF9215 == -8]<-NA
ANES$rep_knowledge_2012_2020[ANES$VCF9215 == -9]<-NA
ANES$rep_knowledge_2012_2020[ANES$VCF0004 == 2008]<-NA #Remove 2008, since it is measured in VCF0366
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$rep_knowledge_2012_2020<-(ANES$rep_knowledge_2012_2020-0)/(4-0)
    #Strong leadership 2012-2020
      #Original values: 1: Extremely well, 2: Very well, 3: Moderately well, 4: Slightly well, 5: Not well at all, -8: DK, -9: NA
        #New values: 0: Not well at all, 1: Slightly well, 2: Moderately well, 3: Very well, 4: Extremely well
#Democratic candidate
ANES$dem_leadership_2012_2020<-ANES$VCF9209
ANES$dem_leadership_2012_2020[ANES$VCF9209 == 1]<-4 #Extremely strong leadership 
ANES$dem_leadership_2012_2020[ANES$VCF9209 == 2]<-3
ANES$dem_leadership_2012_2020[ANES$VCF9209 == 3]<-2
ANES$dem_leadership_2012_2020[ANES$VCF9209 == 4]<-1
ANES$dem_leadership_2012_2020[ANES$VCF9209 == 5]<-0 #Not strong leadership at all
ANES$dem_leadership_2012_2020[ANES$VCF9209 == -8]<-NA
ANES$dem_leadership_2012_2020[ANES$VCF9209 == -9]<-NA
ANES$dem_leadership_2012_2020[ANES$VCF0004 == 2008]<-NA #Remove 2008, since it is measured in VCF0356
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$dem_leadership_2012_2020<-(ANES$dem_leadership_2012_2020-0)/(4-0)
#Republican candidate
ANES$rep_leadership_2012_2020<-ANES$VCF9213
ANES$rep_leadership_2012_2020[ANES$VCF9213 == 1]<-4  #Extremely strong leadership 
ANES$rep_leadership_2012_2020[ANES$VCF9213 == 2]<-3
ANES$rep_leadership_2012_2020[ANES$VCF9213 == 3]<-2
ANES$rep_leadership_2012_2020[ANES$VCF9213 == 4]<-1
ANES$rep_leadership_2012_2020[ANES$VCF9213 == 5]<-0 #Not strong leadership at all
ANES$rep_leadership_2012_2020[ANES$VCF9213 == -8]<-NA
ANES$rep_leadership_2012_2020[ANES$VCF9213 == -9]<-NA
ANES$rep_leadership_2012_2020[ANES$VCF0004 == 2008]<-NA #Remove 2008, since it is measured in VCF0368
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$rep_leadership_2012_2020<-(ANES$rep_leadership_2012_2020-0)/(4-0)
  #Character-variables
    #Honest 2008-2020
      #Original values: 1: Extremely well, 2: Very well, 3: Moderately well, 4: Slightly well, 5: Not well at all, -8: DK, -9: NA
        #New values: 0: Not well at all, 1: Slightly well, 2: Moderately well, 3: Very well, 4: Extremely well
#Democratic candidate
ANES$dem_honest<-ANES$VCF9212
ANES$dem_honest[ANES$VCF9212 == 1]<-4 #Extremely honest 
ANES$dem_honest[ANES$VCF9212 == 2]<-3
ANES$dem_honest[ANES$VCF9212 == 3]<-2
ANES$dem_honest[ANES$VCF9212 == 4]<-1
ANES$dem_honest[ANES$VCF9212 == 5]<-0 #Not honest at all
ANES$dem_honest[ANES$VCF9212 == -8]<-NA
ANES$dem_honest[ANES$VCF9212 == -9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$dem_honest<-(ANES$dem_honest-0)/(4-0)
#Republican candidate
ANES$rep_honest<-ANES$VCF9216
ANES$rep_honest[ANES$VCF9216 == 1]<-4 #Extremely honest 
ANES$rep_honest[ANES$VCF9216 == 2]<-3
ANES$rep_honest[ANES$VCF9216 == 3]<-2
ANES$rep_honest[ANES$VCF9216 == 4]<-1
ANES$rep_honest[ANES$VCF9216 == 5]<-0 #Not honest at all
ANES$rep_honest[ANES$VCF9216 == -8]<-NA
ANES$rep_honest[ANES$VCF9216 == -9]<-NA
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$rep_honest<-(ANES$rep_honest-0)/(4-0)
    #Really cares about people like you 2012-2020
      #Original values: 1: Extremely well, 2: Very well, 3: Moderately well, 4: Slightly well, 5: Not well at all, -8: DK, -9: NA
        #New values: 0: Not well at all, 1: Slightly well, 2: Moderately well, 3: Very well, 4: Extremely well
#Democratic candidate
ANES$dem_caring_2012_2020<-ANES$VCF9210
ANES$dem_caring_2012_2020[ANES$VCF9210 == 1]<-4 #Extremely caring
ANES$dem_caring_2012_2020[ANES$VCF9210 == 2]<-3
ANES$dem_caring_2012_2020[ANES$VCF9210 == 3]<-2
ANES$dem_caring_2012_2020[ANES$VCF9210 == 4]<-1
ANES$dem_caring_2012_2020[ANES$VCF9210 == 5]<-0 #Not caring at all
ANES$dem_caring_2012_2020[ANES$VCF9210 == -8]<-NA
ANES$dem_caring_2012_2020[ANES$VCF9210 == -9]<-NA
ANES$dem_caring_2012_2020[ANES$VCF0004 == 2008]<-NA #Remove 2008, since it is measured in VCF0357
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$dem_caring_2012_2020<-(ANES$dem_caring_2012_2020-0)/(4-0)
#Republican candidate
ANES$rep_caring_2012_2020<-ANES$VCF9214
ANES$rep_caring_2012_2020[ANES$VCF9214 == 1]<-4 #Extremely caring
ANES$rep_caring_2012_2020[ANES$VCF9214 == 2]<-3
ANES$rep_caring_2012_2020[ANES$VCF9214 == 3]<-2
ANES$rep_caring_2012_2020[ANES$VCF9214 == 4]<-1
ANES$rep_caring_2012_2020[ANES$VCF9214 == 5]<-0 #Not caring at all
ANES$rep_caring_2012_2020[ANES$VCF9214 == -8]<-NA
ANES$rep_caring_2012_2020[ANES$VCF9214 == -9]<-NA
ANES$rep_caring_2012_2020[ANES$VCF0004 == 2008]<-NA #Remove 2008, since it is measured in VCF0369
#Normating the variable for the index, ranging between 0 and 1 
  #Using the formula: (variabel-min. value)/(max. value-min. value)
ANES$rep_caring_2012_2020<-(ANES$rep_caring_2012_2020-0)/(4-0)

#Constructing the indexes
  #Competence-index for BOTH candidates (2008 and before)
ANES$competence_all<-(ANES$dem_knowledge + ANES$rep_knowledge + ANES$dem_leadership + ANES$rep_leadership)
  #Normating the variable for the index, ranging between 0 and 1 
    #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$competence_all)
ANES$competence_all<-(ANES$competence_all-0)/(4-0)
summary(ANES$competence_all)
  
  #Competence-index for BOTH candidates (after 2008)
ANES$competence_all_2<-(ANES$dem_leadership_2012_2020 + ANES$rep_leadership_2012_2020 + ANES$rep_knowledge_2012_2020 + ANES$dem_knowledge_2012_2020)
summary(ANES$competence_all_2)
ANES$competence_all_2<-(ANES$competence_all_2-0)/(4-0)
summary(ANES$competence_all_2)

  #Combining the indexes, measuring BOTH democratic and republican candidates' COMPETENCE
    # between 1980 and 2020, ranging between 0 and 1
ANES$competence_full<-ifelse(ANES$VCF0004 < 2009, ANES$competence_all, ANES$competence_all_2)

  #Character-index for BOTH candidates (1980-2008)
    #Starting with moral (1980-2008)
ANES$character_moral_all<-(ANES$dem_moral + ANES$rep_moral)
    #Normating the variable for the index, ranging between 0 and 1 
      #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_moral_all)
ANES$character_moral_all<-(ANES$character_moral_all-0)/(2-0)
summary(ANES$character_moral_all)
    #Then using inspiring (1980-1996)
ANES$character_inspiring_all<-(ANES$dem_inspiring + ANES$rep_inspiring)
      #Normating the variable for the index, ranging between 0 and 1 
        #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_inspiring_all)
ANES$character_inspiring_all<-(ANES$character_inspiring_all-0)/(2-0)
summary(ANES$character_inspiring_all)
    #Then using caring (1984-2008)
ANES$character_caring_all<-(ANES$dem_caring + ANES$rep_caring)
      #Normating the variable for the index, ranging between 0 and 1 
        #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_caring_all)
ANES$character_caring_all<-(ANES$character_caring_all-0)/(2-0)
summary(ANES$character_caring_all)
    #Then using honest (2008-2020)
ANES$character_honest_all<-(ANES$dem_honest + ANES$rep_honest)
      #Normating the variable for the index, ranging between 0 and 1 
        #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_honest_all)
ANES$character_honest_all<-(ANES$character_honest_all-0)/(2-0)
summary(ANES$character_honest_all)
    #Then using "caring" (2012-2020)
ANES$character_caring_all_2012_2020<-(ANES$dem_caring_2012_2020 + ANES$rep_caring_2012_2020)
      #Normating the variable for the index, ranging between 0 and 1 
        #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_caring_all_2012_2020)
ANES$character_caring_all_2012_2020<-(ANES$character_caring_all_2012_2020-0)/(2-0)
summary(ANES$character_caring_all_2012_2020)

#Combining the indexes, measuring BOTH democratic and republican candidates' CHARACTER
# between 1980 and 2020, ranging between 0 and 1
ANES$character_full<-ifelse(ANES$VCF0004 < 2009, ANES$character_moral_all,
                            ifelse(ANES$VCF0004 < 1997, ANES$character_inspiring_all,
                                   ifelse(ANES$VCF0004 < 2009 & ANES$VCF0004 > 1983, ANES$character_caring_all,
                                          ifelse(ANES$VCF0004 <= 2020 & ANES$VCF0004 >= 2008, ANES$character_honest_all,
                                                 ifelse(ANES$VCF0004 <= 2020 & ANES$VCF0004 >= 2012, ANES$character_caring_all_2012_2020, NA)))))
                            
  #DEMOCRATIC CANDIDATE's TRAITS
    #COMPETENCE
        #Constructing the indexes
            #Competence-index for DEMOCRATIC candidates (2008 and before)
ANES$competence_dem<-(ANES$dem_knowledge + ANES$dem_leadership)
                #Normating the variable for the index, ranging between 0 and 1 
                    #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$competence_dem)
ANES$competence_dem<-(ANES$competence_dem-0)/(2-0)
summary(ANES$competence_all)
            #Competence-index for DEMOCRATIC candidates INTELLIGENCE (1984-1992)
ANES$competence_intel <- ifelse(ANES$VCF0004 >= 1984 & ANES$VCF0004 <= 1992, ANES$dem_intelligence, NA)
summary(ANES$competence_intel)
            #Competence-index for DEMOCRATIC candidates INTELLIGENCE (2000-2008)
ANES$competence_intel2 <- ifelse(ANES$VCF0004 >= 2000 & ANES$VCF0004 <= 2008, ANES$dem_intelligence, NA)
summary(ANES$competence_intel2)
            #Competence-index for DEMOCRATIC candidates (after 2008)
ANES$competence_dem_2<-(ANES$dem_leadership_2012_2020 + ANES$dem_knowledge_2012_2020)
summary(ANES$competence_dem_2)
ANES$competence_dem_2<-(ANES$competence_dem_2-0)/(2-0)
summary(ANES$competence_dem_2)
                      #Combining the indexes, measuring DEMOCRATIC candidates' COMPETENCE
                        #between 1980 and 2020, ranging between 0 and 1
ANES$competence_dem <- ifelse(ANES$VCF0004 < 2009, ANES$competence_dem,
                              ifelse(ANES$VCF0004 >= 1984 & ANES$VCF0004 <= 1992, ANES$competence_intel,
                                     ifelse(ANES$VCF0004 >= 2000 & ANES$VCF0004 <= 2008, ANES$competence_intel2,
                                            ANES$competence_dem_2)))
summary(ANES$competence_dem)

    #CHARACTER
      #Constructing the indexes
          #CHARACTER-index for DEMOCRATIC candidates
                #Starting with moral (1980-2008)
ANES$character_moral_dem<-(ANES$dem_moral)
                  #Normating the variable for the index, ranging between 0 and 1 
                      #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_moral_dem)
ANES$character_moral_dem<-(ANES$character_moral_dem-0)/(1-0)
summary(ANES$character_moral_dem)
                #Then using inspiring (1980-1996)
ANES$character_inspiring_dem<-(ANES$dem_inspiring)
                  #Normating the variable for the index, ranging between 0 and 1 
                      #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_inspiring_dem)
ANES$character_inspiring_dem<-(ANES$character_inspiring_dem-0)/(1-0)
summary(ANES$character_inspiring_dem)
                #Then using caring (1984-2008)
ANES$character_caring_dem<-(ANES$dem_caring)
                  #Normating the variable for the index, ranging between 0 and 1 
                    #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_caring_dem)
ANES$character_caring_dem<-(ANES$character_caring_dem-0)/(1-0)
summary(ANES$character_caring_dem)
              #Then using honest (2008-2020)
ANES$character_honest_dem<-(ANES$dem_honest)
                  #Normating the variable for the index, ranging between 0 and 1 
                    #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_honest_dem)
ANES$character_honest_dem<-(ANES$character_honest_dem-0)/(1-0)
summary(ANES$character_honest_dem)
              #Then using "caring" (2012-2020)
ANES$character_caring_dem_2012_2020<-(ANES$dem_caring_2012_2020)
                #Normating the variable for the index, ranging between 0 and 1 
                  #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_caring_dem_2012_2020)
ANES$character_caring_dem_2012_2020<-(ANES$character_caring_dem_2012_2020-0)/(1-0)
summary(ANES$character_caring_dem_2012_2020)

#Combining the indexes, measuring DEMOCRATIC candidates' CHARACTER
# between 1980 and 2020, ranging between 0 and 1
ANES$character_dem<-ifelse(ANES$VCF0004 < 2009, ANES$character_moral_dem,
                            ifelse(ANES$VCF0004 < 1997, ANES$character_inspiring_dem,
                                   ifelse(ANES$VCF0004 < 2009 & ANES$VCF0004 > 1983, ANES$character_caring_dem,
                                          ifelse(ANES$VCF0004 <= 2020 & ANES$VCF0004 >= 2008, ANES$character_honest_dem,
                                                 ifelse(ANES$VCF0004 <= 2020 & ANES$VCF0004 >= 2012, ANES$character_caring_dem_2012_2020, NA)))))

#REVERSED VARIABLE
ANES$competence_dem_reversed<-(1-ANES$competence_dem)

#REPUBLICAN CANDIDATE's TRAITS
  #COMPETENCE
    #Constructing the indexes
      #Competence-index for REPUBLICAN candidates (2008 and before)
ANES$competence_rep<-(ANES$rep_knowledge + ANES$rep_leadership)
      #Normating the variable for the index, ranging between 0 and 1 
      #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$competence_rep)
ANES$competence_rep<-(ANES$competence_rep-0)/(2-0)
summary(ANES$competence_all)
      #Competence-index for REPUBLICAN candidates INTELLIGENCE (1984-1992)
ANES$competence_intel <- ifelse(ANES$VCF0004 >= 1984 & ANES$VCF0004 <= 1992, ANES$rep_intelligence, NA)
summary(ANES$competence_intel)
      #Competence-index for REPUBLICAN candidates INTELLIGENCE (2000-2008)
ANES$competence_intel2 <- ifelse(ANES$VCF0004 >= 2000 & ANES$VCF0004 <= 2008, ANES$rep_intelligence, NA)
summary(ANES$competence_intel2)
      #Competence-index for REPUBLICAN candidates (after 2008)
ANES$competence_rep_2<-(ANES$rep_leadership_2012_2020 + ANES$rep_knowledge_2012_2020)
summary(ANES$competence_rep_2)
ANES$competence_rep_2<-(ANES$competence_rep_2-0)/(2-0)
summary(ANES$competence_rep_2)
      #Combining the indexes, measuring REPUBLICAN candidates' COMPETENCE
          #between 1980 and 2020, ranging between 0 and 1
ANES$competence_rep <- ifelse(ANES$VCF0004 < 2009, ANES$competence_rep,
                              ifelse(ANES$VCF0004 >= 1984 & ANES$VCF0004 <= 1992, ANES$competence_intel,
                                     ifelse(ANES$VCF0004 >= 2000 & ANES$VCF0004 <= 2008, ANES$competence_intel2,
                                            ANES$competence_rep_2)))

summary(ANES$competence_rep)
  #CHARACTER
    #Constructing the indexes
      #CHARACTER-index for REPUBLICAN candidates
        #Starting with moral (1980-2008)
ANES$character_moral_rep<-(ANES$rep_moral)
          #Normating the variable for the index, ranging between 0 and 1 
            #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_moral_rep)
ANES$character_moral_rep<-(ANES$character_moral_rep-0)/(1-0)
summary(ANES$character_moral_rep)
        #Then using inspiring (1980-1996)
ANES$character_inspiring_rep<-(ANES$rep_inspiring)
          #Normating the variable for the index, ranging between 0 and 1 
            #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_inspiring_rep)
ANES$character_inspiring_rep<-(ANES$character_inspiring_rep-0)/(1-0)
summary(ANES$character_inspiring_rep)
      #Then using caring (1984-2008)
ANES$character_caring_rep<-(ANES$rep_caring)
        #Normating the variable for the index, ranging between 0 and 1 
        #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_caring_rep)
ANES$character_caring_rep<-(ANES$character_caring_rep-0)/(1-0)
summary(ANES$character_caring_rep)
    #Then using honest (2008-2020)
ANES$character_honest_rep<-(ANES$rep_honest)
      #Normating the variable for the index, ranging between 0 and 1 
        #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_honest_rep)
ANES$character_honest_rep<-(ANES$character_honest_rep-0)/(1-0)
summary(ANES$character_honest_rep)
    #Then using "caring" (2012-2020)
ANES$character_caring_rep_2012_2020<-(ANES$rep_caring_2012_2020)
      #Normating the variable for the index, ranging between 0 and 1 
        #Using the formula: (variabel-min. value)/(max. value-min. value)
summary(ANES$character_caring_rep_2012_2020)
ANES$character_caring_rep_2012_2020<-(ANES$character_caring_rep_2012_2020-0)/(1-0)
summary(ANES$character_caring_rep_2012_2020)

  #Combining the indexes, measuring REPUBLICAN candidates' CHARACTER
  # between 1980 and 2020, ranging between 0 and 1
ANES$character_rep<-ifelse(ANES$VCF0004 < 2009, ANES$character_moral_rep,
                           ifelse(ANES$VCF0004 < 1997, ANES$character_inspiring_rep,
                                  ifelse(ANES$VCF0004 < 2009 & ANES$VCF0004 > 1983, ANES$character_caring_rep,
                                         ifelse(ANES$VCF0004 <= 2020 & ANES$VCF0004 >= 2008, ANES$character_honest_rep,
                                                ifelse(ANES$VCF0004 <= 2020 & ANES$VCF0004 >= 2012, ANES$character_caring_rep_2012_2020, NA)))))


#REVERSED VARIABLE
ANES$competence_rep_reversed<-(1-ANES$competence_rep)
ANES$competence_dem_reversed<-(1-ANES$competence_dem)
ANES$character_rep_reversed<-(1-ANES$character_rep)
ANES$character_dem_reversed<-(1-ANES$character_dem)

#POLARIZING VARIABLES
  #COMPETENCE
    #HIGH DEMOCRAT — LOW REPUBLICAN
ANES$competence_high_dem_low_rep<-(ANES$competence_dem-ANES$competence_rep)
  #Factor variable
competence_high_dem_low_rep_f<-factor(ANES$competence_high_dem_low_rep, levels=c(0, 1, 2), labels=c("more negative", "indifferent", "more positive"))
ANES$competence_high_dem_low_rep2<-((ANES$competence_high_dem_low_rep + 1)/2)

    #HIGH REPUBLICAN — LOW DEMOCRAT
ANES$competence_high_rep_low_dem<-(ANES$competence_rep-ANES$competence_dem)
  #CHARACTER
    #HIGH DEMOCRAT — LOW REPUBLICAN
ANES$character_high_dem_low_rep<-(ANES$character_dem-ANES$character_rep)
    #HIGH REPUBLICAN — LOW DEMOCRAT
ANES$character_high_rep_low_dem<-(ANES$character_rep-ANES$character_dem)
summary(ANES$competence_high_rep_low_dem)

#ELECTION YEAR : FACTOR VARIABLE
ANES$election_1980_2020<-ANES$VCF0004
ANES$election_1980_2020[ANES$VCF0004 < 1979]<-NA
election_factor <- factor(ANES$election_1980_2020, levels=c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020), labels=c("1980", "1984", "1988", "1992", "1996", "2000", "2004", "2008", "2012", "2016", "2020"))
election_factor <- na.omit(election_factor)
summary(election_factor)

#VOTING TURNOUT DECISION
    #Original values: 0: DK/NA 1: Not registered, and did not vote, 2: Registered, but did not vote, 3: Voted (registered)
      #New values: 0: Did not vote, 1: Did vote
ANES$turnout<-ANES$VCF0703
ANES$turnout[ANES$VCF0703 == 1]<-0 
ANES$turnout[ANES$VCF0703 == 2]<-0
ANES$turnout[ANES$VCF0703 == 3]<-1
ANES$turnout[ANES$VCF0703 == 0]<-NA
ANES$turnout[ANES$VCF0004 < 1980]<-NA

#VALIDATED VOTE
ANES$turnout_validated<-ANES$VCF9155  
ANES$turnout_validated[ANES$VCF9155 == 0]<-0
ANES$turnout_validated[ANES$VCF9155 == 1]<-1
ANES$turnout_validated[ANES$VCF9155 == 3]<-0
ANES$turnout_validated[ANES$VCF9155 == 5]<-0

#VOTING TURNOUT LAST ELECTION
  #Original values: 0: Did not vote in previous election, 1: Voted Democratic, 2: Voted Republican, 3: Voted but did not tell who, 5: Voted, other candidate, 9: DK/NA/Refused to say
ANES$turnout_prev<-ANES$VCF9027
ANES$turnout_prev[ANES$VCF9027 == 0]<-1 
ANES$turnout_prev[ANES$VCF9027 == 1]<-0
ANES$turnout_prev[ANES$VCF9027 == 2]<-0
ANES$turnout_prev[ANES$VCF9027 == 3]<-0
ANES$turnout_prev[ANES$VCF9027 == 5]<-0
ANES$turnout_prev[ANES$VCF9027 == 9]<-NA

#AGE GROUPS
  #Grouping: 0: 18-29, 1: 30-49, 2: 50-64, 3: 65+
ANES$age<-ANES$VCF0101
ANES$age_cat<-cut(ANES$age, breaks = c(18, 29, 49, 64, Inf), labels = c("18-29", "30-49", "50-64", "65+"))
ANES$age_cat[ANES$age == 0]<-NA
summary(ANES$age_cat)

#AGE CONTINUOUS
ANES$age_cont<-ANES$VCF0101
ANES$age_cont[ANES$VCF0101 == 00]<-NA
ANES$age_cont[ANES$VCF0101 == 17]<-NA

#Age Groups, one variable for each value (used for centering the variable to the cluster mean)
ANES$age_18_29<-ANES$age_cont
ANES$age_18_29[ANES$age_cont < 30]<-1
ANES$age_18_29[ANES$age_cont > 29]<-0
ANES$age_30_49[ANES$age_cont > 29 & ANES$age_cont < 50]<-1
ANES$age_30_49[ANES$age_cont < 30 & ANES$age_cont > 49]<-0
ANES$age_50_64[ANES$age_cont > 49 & ANES$age_cont < 65]<-1
ANES$age_50_64[ANES$age_cont < 50 & ANES$age_cont > 64]<-0
ANES$age_65plus[ANES$age_cont > 64]<-1
ANES$age_50_64[ANES$age_cont < 65]<-0
ANES$age_30_49[ANES$age_cont > 30]<-0
ANES$age_30_49[ANES$age_cont > 30]<-0



#EDUCATION
  #Grouping: 
    # 0: Grade school or less (0-8grades); 2020: less than highschool credential, 
    # 1: High school (12 grades orfewer, incl. non-college)
    # 2: Some college (13 grades or more but no degree
    # 3: College or advanced degree
ANES$edu_cat<-ANES$VCF0110
ANES$edu_cat[ANES$VCF0110 == 0]<-NA
ANES$edu_cat[ANES$VCF0110 == 1]<-0
ANES$edu_cat[ANES$VCF0110 == 2]<-1
ANES$edu_cat[ANES$VCF0110 == 3]<-2
ANES$edu_cat[ANES$VCF0110 == 4]<-3
ANES$edu_cat<-factor(ANES$edu_cat)
summary(ANES$edu_cat)

#PARTY IDENTIFICATION
  # 0: Democrat(including leaners), 1: Independent, 2: Republican(including leaners)
ANES$PID<-ANES$VCF0303
ANES$PID[ANES$VCF0303 == 0]<-NA
ANES$PID[ANES$VCF0303 == 1]<-0
ANES$PID[ANES$VCF0303 == 2]<-1
ANES$PID[ANES$VCF0303 == 3]<-2
ANES$turnout<-ANES$VCF0703
ANES$turnout[ANES$VCF0703 == 1]<-0 
ANES$turnout[ANES$VCF0703 == 2]<-0
ANES$turnout[ANES$VCF0703 == 3]<-1
ANES$turnout[ANES$VCF0703 == 0]<-NA
ANES$turnout[ANES$VCF0004 < 1980]<-NA

#AGE GROUPS
  #Grouping: 0: 18-30, 1: 31-50, 2: 51+
ANES$age<-ANES$VCF0101
ANES$age_cat<-cut(ANES$age, breaks = c(18, 29, 50, Inf), labels = c("18-30", "31-50", "51+"))
ANES$age_cat[ANES$age == 0]<-NA
summary(ANES$age_cat)


#GENDER
  #0: Male, 1: Female
ANES$gender<-ANES$VCF0104
ANES$gender[ANES$VCF0104 == 1]<-0
ANES$gender[ANES$VCF0104 == 2]<-1
ANES$gender[ANES$VCF0104 == 0]<-NA
ANES$gender[ANES$VCF0104 == 3]<-NA

#GENDER_FACTOR
ANES$gender_f<-factor(ANES$gender, levels=c(0, 1), labels=c("male", "female"))

#EDUCATION
  #Grouping: 
    # 0: Grade school or less (0-8grades); 2020: less than highschool credential, 
    # 0: No degree, 1: College or advanced degree
ANES$edu<-ANES$VCF0110
ANES$edu[ANES$VCF0110 == 0]<-NA
ANES$edu[ANES$VCF0110 == 1]<-0
ANES$edu[ANES$VCF0110 == 2]<-0
ANES$edu[ANES$VCF0110 == 3]<-0
ANES$edu[ANES$VCF0110 == 4]<-1
summary(ANES$edu)

#EDUCATION FACTOR
ANES$edu_f<-factor(ANES$edu, levels=c(0, 1), labels=c("No college degree", "College degree"))

#INCOME
ANES$income<-ANES$VCF0114
ANES$income[ANES$VCF0114 == 0]<-NA
ANES$income[ANES$VCF0114 == 1]<-0
ANES$income[ANES$VCF0114 == 2]<-0
ANES$income[ANES$VCF0114 == 3]<-1
ANES$income[ANES$VCF0114 == 4]<-1
ANES$income[ANES$VCF0114 == 5]<-1

#INCOME factor
ANES$income_f<-factor(ANES$income, levels=c(0, 1), labels=c("Low income", "Medium & high income"))

#PARTY IDENTIFICATION
  # 0: Independent, 1: Democrat(including leaners), 2: Republican(including leaners)
ANES$PID<-ANES$VCF0303
ANES$PID[ANES$VCF0303 == 0]<-NA
ANES$PID[ANES$VCF0303 == 1]<-1
ANES$PID[ANES$VCF0303 == 2]<-0
ANES$PID[ANES$VCF0303 == 3]<-2

ANES$PID_ind<-ANES$PID
ANES$PID_ind[ANES$PID == 0]<-1
ANES$PID_ind[ANES$PID == 1]<-0
ANES$PID_ind[ANES$PID == 2]<-0

ANES$PID_dem<-ANES$PID
ANES$PID_dem[ANES$PID == 0]<-0
ANES$PID_dem[ANES$PID == 1]<-1
ANES$PID_dem[ANES$PID == 2]<-0

ANES$PID_rep<-ANES$PID
ANES$PID_rep[ANES$PID == 0]<-0
ANES$PID_rep[ANES$PID == 1]<-0
ANES$PID_rep[ANES$PID == 2]<-1

  #Factor variable
ANES$PID_fac<-factor(ANES$PID, levels=c(0,1,2), labels=c("Independent","Democrat", "Republican"))

#STRENGTH OF PARTISANSHIP
    # 0: Independent or apolitical, 1: Leaning independent, 2: Weak partisan, 3: Strong partisans
ANES$PID_strength<-ANES$VCF0305
ANES$PID_strength[ANES$VCF0305 == 0]<-NA
ANES$PID_strength[ANES$VCF0305 == 1]<-0
ANES$PID_strength[ANES$VCF0305 == 2]<-1
ANES$PID_strength[ANES$VCF0305 == 3]<-2
ANES$PID_strength[ANES$VCF0305 == 4]<-3

#STRENGTH OF PARTISANSHIP, FACTOR
ANES$PID_strength_f<-factor(ANES$PID_strength, levels=c(0,1,2,3), labels=c("Indendent or apolitical","Leaning independent", "Weak partisan", "Strong partisan"))

#WEAK PARTISANSHP
ANES$PID_weak<-ANES$PID_strength
ANES$PID_weak[ANES$PID_strength == 0]<-1
ANES$PID_weak[ANES$PID_strength == 1]<-0
ANES$PID_weak[ANES$PID_strength == 2]<-0
ANES$PID_weak[ANES$PID_strength == 3]<-0
ANES$PID_weak_f<-factor(ANES$PID_weak, levels=c(0,1), labels=c("Weak or strong partisan", "Independent or apolitical"))
table(ANES$PID_weak_f)
#STRONG PARTISANSHIP
ANES$PID_strong<-ANES$PID_strength
ANES$PID_strong[ANES$PID_strength == 0]<-0
ANES$PID_strong[ANES$PID_strength == 1]<-1
ANES$PID_strong[ANES$PID_strength == 2]<-1
ANES$PID_strong[ANES$PID_strength == 3]<-1
ANES$PID_strong_f<-factor(ANES$PID_strong, levels=c(0,1), labels=c("Independent or apolitical", "Weak or strong partisan"))
table(ANES$PID_strong_f)
#Intermediate PARTISANSHIP
ANES$PID_int<-ANES$PID_strength
ANES$PID_int[ANES$PID_strength == 0]<-0
ANES$PID_int[ANES$PID_strength == 1]<-0
ANES$PID_int[ANES$PID_strength == 2]<-1
ANES$PID_inte[ANES$PID_strength == 3]<-1
ANES$PID_int_f<-factor(ANES$PID_int, levels=c(0,1), labels=c("Independent or apolitical or leaning", "Weak or strong partisan"))
table(ANES$PID_strong_f)


#SUBSETTING THE DATA
  #Only DEMOCRATIC IDENTITY
ANES_DEM<-ANES
ANES_DEM<-ANES_DEM[ANES_DEM$PID == 1, ]

      #Create a new factor = election_year_dem
ANES_DEM$VCF0004<-ANES_DEM$VCF0004
ANES_DEM$election_1980_2020<-ANES_DEM$VCF0004
election_factor_dem<-factor(ANES_DEM$election_1980_2020, levels=c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020), labels=c("1980", "1984", "1988", "1992", "1996", "2000", "2004", "2008", "2012", "2016", "2020"))
summary(election_factor_dem)

    #Create a new factor = age_cat_dem
ANES_DEM$age<-ANES_DEM$VCF0101
ANES_DEM$age_cat_dem<-cut(ANES_DEM$age, breaks = c(18, 29, 49, 64, Inf), labels = c("18-29", "30-49", "50-64", "65+"))
ANES_DEM$age_cat_dem[ANES_DEM$age == 0]<-NA

summary(ANES_DEM$age_cat_dem)


    #Create a new factor = gender_f_dem
ANES_DEM$gender_dem<-ANES_DEM$VCF0104
ANES_DEM$gender_dem[ANES_DEM$VCF0104 == 1]<-0
ANES_DEM$gender_dem[ANES_DEM$VCF0104 == 2]<-1
ANES_DEM$gender_dem[ANES_DEM$VCF0104 == 0]<-NA
ANES_DEM$gender_dem[ANES_DEM$VCF0104 == 3]<-NA

#GENDER_FACTOR_DEM
ANES_DEM$gender_f_dem<-factor(ANES_DEM$gender_dem, levels=c(0, 1), labels=c("male", "female"))

    #Create a new factor = edu_f_dem
ANES_DEM$edu_dem<-ANES_DEM$VCF0110
ANES_DEM$edu_dem[ANES_DEM$VCF0110 == 0]<-NA
ANES_DEM$edu_dem[ANES_DEM$VCF0110 == 1]<-0
ANES_DEM$edu_dem[ANES_DEM$VCF0110 == 2]<-0
ANES_DEM$edu_dem[ANES_DEM$VCF0110 == 3]<-0
ANES_DEM$edu_dem[ANES_DEM$VCF0110 == 4]<-1
summary(ANES_DEM$edu_dem)

#EDUCATION FACTOR
ANES_DEM$edu_f_dem<-factor(ANES_DEM$edu_dem, levels=c(0, 1), labels=c("No college degree", "College degree"))

#POLARIZING VARIABLE FACTOR
competence_high_dem_low_rep_f_dem<-factor(ANES_DEM$competence_high_dem_low_rep2, levels=c(0, 1), labels=c("indifferent", "more positive"))

    ### Create a new factor = PID_strength_dem
ANES_DEM$PID_strength_f_dem<-factor(ANES_DEM$PID_strength, levels=c(0,1,2,3), labels=c("Indendent or apolitical","Leaning independent", "Weak partisan", "Strong partisan"))

  #Only REPUBLICAN IDENTITY
ANES_REP<-ANES
ANES_REP<-ANES_REP[ANES_REP$PID == 2, ]

      #Create a new factor = election_year_rep
ANES_REP$VCF0004<-ANES_REP$VCF0004
ANES_REP$election_1980_2020<-ANES_REP$VCF0004
election_factor_rep<-factor(ANES_REP$election_1980_2020, levels=c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020), labels=c("1980", "1984", "1988", "1992", "1996", "2000", "2004", "2008", "2012", "2016", "2020"))
summary(election_factor_rep)

    #Create a new factor = age_cat_rep
ANES_REP$age<-ANES_REP$VCF0101
ANES_REP$age_cat_rep<-cut(ANES_REP$age, breaks = c(18, 29, 49, 64, Inf), labels = c("18-29", "30-49", "50-64", "65+"))
ANES_REP$age_cat_rep[ANES_REP$age == 0]<-NA
summary(ANES_REP$age_cat_rep)

#Create a new factor = gender_f_rep
ANES_REP$gender_rep<-ANES_REP$VCF0104
ANES_REP$gender_rep[ANES_REP$VCF0104 == 1]<-0
ANES_REP$gender_rep[ANES_REP$VCF0104 == 2]<-1
ANES_REP$gender_rep[ANES_REP$VCF0104 == 0]<-NA
ANES_REP$gender_rep[ANES_REP$VCF0104 == 3]<-NA

#GENDER_FACTOR_REP
ANES_REP$gender_f_rep<-factor(ANES_REP$gender_rep, levels=c(0, 1), labels=c("male", "female"))

#Create a new factor = edu_f_rep
ANES_REP$edu_rep<-ANES_REP$VCF0110
ANES_REP$edu_rep[ANES_REP$VCF0110 == 0]<-NA
ANES_REP$edu_rep[ANES_REP$VCF0110 == 1]<-0
ANES_REP$edu_rep[ANES_REP$VCF0110 == 2]<-0
ANES_REP$edu_rep[ANES_REP$VCF0110 == 3]<-0
ANES_REP$edu_rep[ANES_REP$VCF0110 == 4]<-1
summary(ANES_REP$edu_rep)

#EDUCATION FACTOR
ANES_REP$edu_f_rep<-factor(ANES_REP$edu_rep, levels=c(0, 1), labels=c("No college degree", "College degree"))

### Create a new factor = PID_strength_rep
ANES_REP$PID_strength_f_rep<-factor(ANES_REP$PID_strength, levels=c(0,1,2,3), labels=c("Indendent or apolitical","Leaning independent", "Weak partisan", "Strong partisan"))

  #Only INDEPENDENT IDENTITY
ANES_IND<-ANES
ANES_IND<-ANES_IND[ANES_IND$PID == 1, ]

    #Create a new factor = election_year_ind
ANES_IND$VCF0004<-ANES_IND$VCF0004
ANES_IND$election_1980_2020<-ANES_IND$VCF0004
election_factor_ind<-factor(ANES_IND$election_1980_2020, levels=c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020), labels=c("1980", "1984", "1988", "1992", "1996", "2000", "2004", "2008", "2012", "2016", "2020"))
summary(election_factor_ind)

    # Only DID NOT vote previous election
ANES_ABS<-ANES
ANES_ABS<-ANES_ABS[ANES_ABS$turnout_prev == 1, ]

  #Create a new factor = election_year_abs
ANES_ABS$VCF0004<-ANES_ABS$VCF0004
ANES_ABS$election_1980_2020<-ANES_ABS$VCF0004
election_factor_abs<-factor(ANES_ABS$election_1980_2020, levels=c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020), labels=c("1980", "1984", "1988", "1992", "1996", "2000", "2004", "2008", "2012", "2016", "2020"))
summary(election_factor_abs)


#Thermometer scale democratic candidate
ANES$term_dem_cand<-ANES$VCF0424
ANES$term_dem_cand[ANES$VCF0424 == 98]<-NA
ANES$term_dem_cand[ANES$VCF0424 == 99]<-NA


#SUBSETTING DATA BASED ON PARTISAN STRENGTH
ANES_PARTISANS<-ANES[ANES$PID_strength == 3, ]
ANES_NON_PARTISANS<-ANES[ANES$PID_strength == 0, ]


#VARIABLES SO FAR CODED:
#[...]

xtabs(~ ANES$competence_high_dem_low_rep + ANES$competence_high_rep_low_dem)
summary(ANES$competence_high_dem_low_rep)
summary(ANES$character_high_dem_low_rep)

### Prepare data, centering predictors around its grand mean
##### PERSONALITY TRAITS
#Competence Democrats
gm_dem <- mean(ANES$competence_dem, na.rm=T)
ANES$competence_dem_gm <- ANES$competence_dem - gm_dem
#Competence Republicans
gm_rep <- mean(ANES$competence_rep, na.rm=T)
ANES$competence_rep_gm <- ANES$competence_rep - gm_rep
#Character Democrats
gm_dem_c <- mean(ANES$character_dem, na.rm=T)
ANES$character_dem_gm <- ANES$character_dem - gm_dem_c
#Character Republican
gm_rep_c <- mean(ANES$character_rep, na.rm=T)
ANES$character_rep_gm <- ANES$character_rep - gm_rep_c
# PID Strength
gm_PID <- mean(ANES$PID_strength, na.rm=T)
ANES$PID_strength_gm <- ANES$PID_strength - gm_PID
# PID Strength (as factor)
gm_PID_f <- mean(ANES$PID_strength_f, na.rm=T)
ANES$PID_strength_gm_f <- ANES$PID_strength_f - gm_PID_f

## Polarization variable
ANES$comp_diff<-abs(ANES$competence_dem-ANES$competence_rep)
ANES$char_diff<-abs(ANES$character_dem-ANES$character_rep)
ANES$full_diff<-abs((ANES$character_dem+ANES$competence_dem)-(ANES$competence_rep+ANES$character_rep))


###Polarization variable, centered around grand mean
ANES$comp_diff_gm<-abs(ANES$competence_dem_gm - ANES$competence_rep_gm)
ANES$char_diff_gm<-abs(ANES$character_dem_gm - ANES$character_rep_gm)
ANES$char_diff_abs_gm<-abs(ANES$character_dem_gm - ANES$character_rep_gm)
ANES$comp_diff_abs_gm<-abs(ANES$competence_dem_gm - ANES$competence_rep_gm)

#Character Difference
char_diff_gm <- mean(ANES$char_diff, na.rm=T)
ANES$char_diff_gm <- ANES$char_diff - char_diff_gm
#competence Difference
comp_diff_gm <- mean(ANES$comp_diff, na.rm=T)
ANES$comp_diff_gm <- ANES$comp_diff - comp_diff_gm

####CONTROLS, center around the grand-mean
#income: high and medium (=1), low (=0)
gm_income <- mean(ANES$income, na.rm=T)
ANES$income_gm <- ANES$income - gm_income
#gender (0=man; 1=woman)
gm_gender <- mean(ANES$gender, na.rm=T)
ANES$gender_gm <- ANES$gender - gm_gender
#Education (0= no college degree; 1= college degree)
gm_edu <- mean(ANES$edu, na.rm=T)
ANES$edu_gm <- ANES$edu - gm_edu


