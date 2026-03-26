#####Create Dataset with Position by Issue#####

#set key to access data#

mp_setapikey(key = "e0651f3ae260c42f92028272c768c705")

#Make Data Set - simple sum of components#

party_pol <- mp_maindataset(version = "current") %>% #download the data
  dplyr::select(countryname, edate, party, partyname, parfam, pervote, per401, 
                per402, per403, per404, per412, per413, per505, per504, per702, 
                per701, per414, per409, per410, per416, per608, per607, per705,
                per411, per501, per104, per105, per106, per603, per604, per605, per202,
                per110, per108, per406, per407, per109, per107, per601, per602, per302,
                per301) %>% #keep only relevant variables from Volkens and Merz (2013)
  filter(!is.na(per401) & !is.na(pervote)) %>% #Drop parties with missing data (per401 is only NA when all others are also NA)
  group_by(party, edate) %>%
  dplyr::mutate(role_of_the_state = log(per401 + per402 +.5) -  log(per403 + per404 + per412 + per413 + .5) ,
         welfare_state = log(per505 + .5) - log(per504 + .5),
         union = log(per702 +.5) - log(per701 + 05) ,
         finance = log(per414 +.5) - log(per409 +.5),
         growth = log(per410 +.5) - log(per416 +.5) ,
         multiculturalism = log(per608 + .5) - log(per607 + per705 +.5) ,
         environment = log(per411 +.5) - log(per501 + .5),
         peace= log(per104 +.5) - log(per105 + per106 +.5)  ,
         moral = log(per603 +.5) - log(per604 + .5),
         civil_rights = log(per605 + .5) - log(per202 +.5) ,
         eu = log(per110 +.5) - log(per108+.5),
         protectionism = log(per407 + .5) - log(per406 +.5) ,
         internationalism = log(per109 +.5) - log(per107 + .5)  ,
         nationalism = log(per601 + .5) - log(per602 +.5)  ,
         centralization = log(per302 +.5) - log(per301 +.5))   %>%
  arrange(countryname, party, edate) %>%
  separate(edate, into = c('year', 'month','day'), convert = TRUE, remove = F) %>%
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
  ))

#Create Average Economic and average noneconomic party positions#

party_pol <- party_pol %>%
  dplyr::mutate(econ = (role_of_the_state + welfare_state + union + finance + growth + protectionism)/6,
         nonecon = (multiculturalism + environment + peace + moral + civil_rights + eu + internationalism + nationalism + centralization)/9)

#Calculate Polarization for each election#

country_pol <- party_pol %>%
  dplyr::group_by(countryname, edate) %>%
  dplyr::summarize(econ_pol = sqrt(wtd.var(econ, pervote)),
            nonecon_pol = sqrt(wtd.var(nonecon, pervote)))
