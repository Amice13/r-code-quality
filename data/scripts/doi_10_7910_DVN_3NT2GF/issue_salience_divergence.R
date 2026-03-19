#####Create Data Set with Position and Salience by Dimension#####

#set key to access data#

mp_setapikey(key = "e0651f3ae260c42f92028272c768c705")

sample <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark",
            "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
            "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Montenegro",
            "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovakia",
            "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom")

#Make Data Set - simple sum of components#

party_level <- mp_maindataset(version = "current") %>% #download the data
  filter(countryname %in% sample) %>%
  dplyr::select(countryname, edate, party, partyname, parfam, pervote, per401, 
         per402, per403, per404, per412, per413, per505, per504, per702, 
         per701, per414, per409, per410, per416, per608, per607, per705,
         per411, per501, per104, per105, per106, per603, per604, per605, per202,
         per110, per108, per406, per407, per109, per107, per601, per602, per302,
         per301) %>% #keep only relevant variables from Volkens and Merz (2013)
  filter(!is.na(per401) & !is.na(pervote)) %>% #Drop parties with missing data (per401 is only NA when all others are also NA)
  group_by(party, edate) %>% #Group by party-election
  dplyr::mutate(role_of_the_state_salience = per401 + per402 + per403 + per404 + per412 + per413 ,
         welfare_state_salience = per505 + per504  ,
         union_salience = per702 + per701 ,
         finance_salience = per414 + per409,
         growth_salience = per410 + per416 ,
         multiculturalism_salience = per608 + per607 + per705 ,
         environment_salience = per411 + per501  ,
         peace_salience = per104 + per105 + per106  ,
         moral_salience = per603 + per604  ,
         civil_rights_salience = per605 + per202 ,
         eu_salience = per110 + per108,
         protectionism_salience = per407 + per406 ,
         internationalism_salience = per109 + per107  ,
         nationalism_salience = per601 + per602  ,
         centralization_salience = per302 + per301)   %>% #Create a variable for each salience
  arrange(countryname, party, edate) %>% #arrange the data set by country-pary-election
  separate(edate, into = c('year', 'month','day'), convert = TRUE, remove = F) %>% #adjust date variable
  mutate(family = case_when(
    parfam == 10 ~ 'Green',
    parfam == 20 ~ 'Left',
    parfam == 30 ~ 'Social Dem.',
    parfam == 40 ~ 'Liberal',
    parfam == 50 ~ 'Christian Dem.',
    parfam == 60 ~ 'Conservative',
    parfam == 70 ~ 'Nationalist',
    parfam == 80 ~ 'Agrarian',
    parfam == 90 ~ 'Ethnic',
    parfam == 95 ~ 'Special Issue'
  )) #Add party family labels

#Remove unnecessary variables#

salience_dif <- party_level %>%
  dplyr::select(countryname, edate, pervote, party, role_of_the_state_salience, welfare_state_salience,
         union_salience, finance_salience, growth_salience, multiculturalism_salience,
         environment_salience, peace_salience, moral_salience, civil_rights_salience,
         eu_salience, protectionism_salience, internationalism_salience, nationalism_salience, centralization_salience) %>% # drop unneccessary variables
  unite(country_election,countryname, edate, remove = F) %>% #make variable for each country-election
  group_by(country_election) %>% #group by country-election variables
  dplyr::mutate(election = cur_group_id()) %>% #create numeric group id
  arrange(election)%>% #arrange by this numeric country-election id
  na.omit #drop any missing rows

#Calculate differentiation#

sal_dif<- list()

for(i in 1:max(salience_dif$election)){
  
  #Isolate parties of interest#
  
  sal <- as.matrix(salience_dif[salience_dif$election == i, 6:20])
  
  #transpose the matrix so that each column is a party#
  
  sal <- t(sal)
  
  #Create a vector of weights (vote shares)
  
  v <- salience_dif$pervote[salience_dif$election == i]
  
  #Create a matrix to hold the correlations#
  
  cors <- matrix(nrow = length(v), ncol = length(v))
  
  #Create a matrix to hold the weights#
  
  w <- matrix(nrow = length(v), ncol = length(v))
  
  #Create vector to hold the weighted average of the correlations#
  cor_w_a <- vector()
  
  #Start double loop to fill the matrices
  
  for(j in 1: length(v)){
    for(k in 1:length(v)){
      
      #Create pairwise correlations#
      
      cors[j,k] <- cor(sal[,j], sal[,k])
      
      
      
      #Fill matrix of weights#
      
      w[j,k] <- v[k]
      
      
    }
    
    
  }
  

  
  #create matrix to hold correlations times weights#
  
  cor_w <- matrix(nrow = length(v), ncol = length(v))
  
  #Start another double loop to fill cor_w#
  
  for(j in 1:length(v)){
    for(k in 1:length(v)){
      
      #multiply the correlations by the weights#
      
      cor_w[j,k] <- cors[j,k] * w[j,k]
    }
    
    #Fill the vector#
    
    cor_w_a[j] <- sum(cor_w[j,-j ]) / sum(w[j,-j])
  }
 
  
  sal_dif[i] <- as.data.frame(cor_w_a)
}

#Convert results into a data frame#

sal_dif <- data.frame(sal_dif = unlist(sal_dif))

#Add sal_dif to salience_dif, select relevant variables, and invert sal_dif.

salience_dif <- cbind(salience_dif, sal_dif) %>%
  dplyr::select(countryname, edate, party, sal_dif) %>%
  dplyr::mutate(sal_dif = -sal_dif) #Higher = more divergence

#Add sal_dif to party_level full dataset#

party_level <- left_join(party_level, salience_dif, by = c('party', 'edate', 'countryname')) %>%
  select(countryname, edate, year, month, day, party, partyname, family, pervote, sal_dif)

#Create country-level aggregates#

country_main <- party_level %>%
  group_by(countryname, edate) %>%
  dplyr::summarize(diff = mean(sal_dif),
            diff_w = weighted.mean(sal_dif, pervote)) %>%
  separate(edate, into = c('year', 'month', 'day'), remove = F)%>%
  unite('country_year', countryname, year, sep = '-', remove = F) %>%
  ungroup() 

country_main$year <- as.numeric(country_main$year)
country_main$month <- as.numeric(country_main$month)
country_main$day <- as.numeric(country_main$day)

#Create family-level dataset#

family_level <- party_level %>%
  group_by(family, year) %>%
  dplyr::summarize(diff = mean(sal_dif),
            diff_w = weighted.mean(sal_dif, pervote))

#Breaking Up by Mainstream and challenger party#
party_level$Mainstream <- ifelse(party_level$family %in% c('Christian Dem.', 'Conservative', 'Liberal', 'Social Dem.'), 
                                "Mainstream",
                                "Challenger")

mainstream_level <- party_level %>%
  dplyr::group_by(Mainstream, year) %>%
  dplyr::summarize(diff = mean(sal_dif, na.rm = T))

