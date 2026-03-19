#########################################################
#                                                       #
#Date created:  November 28, 2021                       #
#Created by: Chinemelu Okafor and Shivram Viswanathan   #
#Date updated:                                          #
#Updated by:                                            #
#Notes:                                                 #
#This code uses the cleaned Afrobarometer data          #
# "ReplicationPaper_Afrobarometer_Cleaned.rds" to create#
# ELF, CF and FST measures. It returns both a dataset of# 
# measures for all countries and of Benin and Senegal   #
# separately.                                           #
#                                                       #
#########################################################

####File Inputs####
input_cleaned_afrobarometer_rds <- 'data/clean/ReplicationPaper_Afrobarometer_Cleaned.rds' #Cleaned afrobarometer data provided by authors

####File Outputs####
output_Benin_Senegal_Measures_rds <- 'data/clean/Benin_Senegal_Measures.rds'  
output_All_Country_Measures_rds <- 'data/clean/All_Country_Measures.rds' 

####Defining our lists####
#All Countries
countno <- c("1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28")
countno <- strsplit(countno, " ")
countno <- unlist(countno)

#All Country Names
countname <- c("Benin", "Botswana", "Burkina Faso", "Cape Verde", "Ghana", "Kenya",
               "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique",
               "Namibia", "Nigeria", "Senegal", "South Africa", "Tanzania", "Uganda",
               "Zambia", "Zimbabwe", "Mauritius", "Sierra Leona", "Niger", "Togo",
               "Burundi", "Cameroon", "Cote d'Ivoire", "Guinea", "Ethiopia",
               "Swaziland", "Algeria", "Egypt", "Morocco", "Sudan", "Tunisia")
countname_3 <- c("BEN", "BWA", "BFA", "CPV", "GHA", "KEN",
               "LSO", "LBR", "MDG", "MWI", "MLI", "MOZ",
               "NAM", "NGA", "SEN", "ZAF", "TZA", "UGA",
               "ZMB", "ZWE", "MUS", "SLE", "NER", "TGO",
               "BDI", "CMR", "CIV", "GIN", "ETH",
               "SWZ", "DZA", "EGY", "MAR", "SDN", "TUN")

#Benin and Senegal
countno_ben_sen <- c("1 15")
countno_ben_sen <- strsplit(countno_ben_sen, " ")
countno_ben_sen <- unlist(countno_ben_sen)

#Final list of "cultural" questions
finalqlist <- c("Q73A Q73B Q73C Q73D Q73E Q29A Q29B Q29C Q70A Q70B Q70C Q86A Q86B Q86C Q86D Q86E Q86F Q87 Q18 Q19 Q20 Q21 Q22 Q23 Q24 Q28 Q33 Q34 Q35 Q36 Q37 Q38 Q39 Q40 Q41 Q42 Q49 Q50 Q51 Q61A Q61B Q61C Q61D Q61E Q64 Q68A Q68B Q68C Q68D Q68E Q68F Q69A Q69B Q69C Q69D Q69E Q69F Q75C Q78 Q85A Q85B Q89A Q98B Q13A Q13B Q13C Q13D Q14 Q15 Q16 Q17A Q17B Q17C Q25A Q25B Q26A Q26B Q26C Q26D Q26E Q30A Q30B Q30C Q30D Q31A Q31B Q31C Q32 Q43 Q46A Q46B Q46C Q46D Q47A Q47B Q47C Q47D Q47E Q48A Q48B Q48C Q52A Q52B Q52C Q52D Q52E Q53 Q54 Q55 Q56A Q56B Q56C Q56D Q56E Q56F Q56G Q56H Q56I Q59A Q59B Q59C Q59D Q59E Q59F Q59G Q59H Q59I Q59J Q60A Q60B Q60C Q60D Q60E Q60F Q60G Q61F Q62A Q62B Q65A Q65B Q65C Q65D Q65E Q65F Q65G Q65H Q65I Q65J Q65K Q65L Q65M Q65N Q65O Q65P Q66A Q66B Q66C Q66D Q66E Q67A Q67B Q67C Q67D Q67E Q71A Q71B Q71C Q74 Q75A Q75B Q76A Q76B Q78A1 Q85C Q88A Q88B Q88C Q11 Q12 Q27 Q44 Q45 Q57 Q58 Q63PT1 Q63PT2 Q63PT3 Q72A Q72B Q72C Q77 Q98A")

finalqlist <- strsplit(finalqlist, " ")
finalqlist <- unlist(finalqlist)

####Reading in data####
afrobar_dataf <- readRDS(input_cleaned_afrobarometer_rds)

####Creating ELF, CF, and FST measures for All countries####
countrylist <- countno

#creating measures tibble
All_Country_measures <- tibble(
  country = character(),
  question = character(),
  ethnic_elf = numeric(),
  ethnic_pol = numeric(),
  chisq = numeric(),
  fst = numeric(),
  cultelf = numeric(),
  cultpol = numeric()
)

#loop over each country :
for ( y in countrylist ) {
  y <- as.numeric(y)
  tempdata <- afrobar_dataf %>% 
    filter(COUNTRY == y) #changed s003 to COUNTRY
  
  #number of total respondents in country
  totpop <- nrow(tempdata)
  nethn <- length(unique(tempdata$Q84))
  
  #calculating ethnic shares (ethnic respondents / all respondents)
  ethshares <- tempdata %>% 
    group_by(Q84) %>% #Ethnic variable changed from x051 to Q84
    summarize(n = n()) %>% 
    mutate(ethshare = n / totpop) %>% 
    mutate(ethsharesq = ethshare^2) %>% 
    mutate(p = ethsharesq * (1-ethshare))
  
  elfeth <- 1 - as.numeric(summarize(ethshares, ethsharesq = sum(ethsharesq)))
  poleth <- as.numeric(summarize(ethshares, p = sum(p)))
  
  #for each question ('culture' questions)
  for (q in finalqlist) {
    #enquote item
    enquo(q)
    #subset data
    tempdata_oneq <- tempdata %>% 
      select(COUNTRY, Q84, !!q)
    
    #number of total respondents for a question (numbtot)
    numbtot <- tempdata_oneq %>%   
      drop_na(!!q) %>% 
      summarize(n = n()) %>% as.numeric()
    
    #skip if no respondents
    if (numbtot == 0) {
      All_Country_measures <- All_Country_measures %>% 
        add_row(
          country = as.character(y), 
          question = q,
        )
      next
    }
    else {
      #all possible answers to the question
      maxnum <- tempdata_oneq %>% select(!!q) %>% drop_na() %>% unique() %>% pull()
      
      #get total number of ethnicities that answered question (numethn3)
      numethn3 <- tempdata_oneq %>% 
        drop_na(!!q) %>% select(Q84) %>% unique() %>% pull()
      
      attach(tempdata_oneq)
      #cross tab counts of responses and ethnicity (in stata: tab `q' x051) (varble)
      resp_count <- table(Q84, get(q))
      
      #number of ppl that responded to question (numbans2)
      numbans2 <- rowSums(resp_count)
      # share of responses to question in ethnicity (sum_eth/share) = numbans2/numbtot
      share <- numbans2/numbtot
      
      # ***compute share responding specific answer of total responses to question 
      #(freqans = varble[eth, ] / numbans2) / freqans_total = varble_all[resp] / numbtot
      freqans <- resp_count / numbans2
      freqans_total <- colSums(resp_count) / numbtot
      detach(tempdata_oneq)
      
      # ***cultural elf = 1 - sum over all responses [ freqans^2 ] (cultelf/cultelf_all)
      cultelf <- 1- rowSums(freqans^2)
      cultelf_all <- 1 -sum(freqans_total^2)
      
      # ***cultural pol = sum over all responses[ (freqans^2) * (1-freqans)] (cultelf/cultpol_all)
      cultpol <- rowSums(  freqans^2 * (1-freqans) )
      cultpol_all <- sum( freqans_total^2 *(1-freqans_total))
      
      #chisq = sum over all responses [ ((freqans_total - freqans)*share)^2 / (freqans_total * share) ]
      #chisq[q, country] = sum over all ethnicities [chisq]
      chi_sq_temp <- sum(
        rowSums( 
          (t(freqans_total - t(freqans)) * share)^2 / (share %*% t(freqans_total)) 
        )
      )
      #fst[q, country] = ( 1 - (sum over all ethnicities [cultelf * share]  /  (cultelf_all)
      fst_temp <- 1 - sum(cultelf * share)/cultelf_all
      
      #TODO: distance scaled CF
      
      #TODO : fstdist[q, country] =  same form but use cultelf_dist instead
      
    } #end else block
    
    #add measures by question to dataset
    All_Country_measures <- All_Country_measures %>% 
      add_row(
        country = as.character(y), 
        question = q,
        chisq = chi_sq_temp,
        fst = fst_temp,
        cultelf = cultelf_all,
        cultpol = cultpol_all
      )
    
  } #end question loop
  
  #add elf to all rows with this country
  
  All_Country_measures <- All_Country_measures %>% #5376 observations
    mutate(ethnic_elf = ifelse(country == as.character(y), elfeth, ethnic_elf),
           ethnic_pol = ifelse(country == as.character(y), poleth, ethnic_pol)
    ) 
  
  
} #end country loop

####Creating ELF, CF, and FST measures for Benin and Senegal####
countrylist <- countno_ben_sen

#creating measures tibble
Ben_Sen_measures <- tibble(
  country = character(),
  question = character(),
  ethnic_elf = numeric(),
  ethnic_pol = numeric(),
  chisq = numeric(),
  fst = numeric(),
  cultelf = numeric(),
  cultpol = numeric()
)

#loop over each country :
for ( y in countrylist ) {
  y <- as.numeric(y)
  tempdata <- afrobar_dataf %>% 
    filter(COUNTRY == y) #changed s003 to COUNTRY
  
  #number of total respondents in country
  totpop <- nrow(tempdata)
  nethn <- length(unique(tempdata$Q84))
  
  #calculating ethnic shares (ethnic respondents / all respondents)
  ethshares <- tempdata %>% 
    group_by(Q84) %>% #Ethnic variable changed from x051 to Q84
    summarize(n = n()) %>% 
    mutate(ethshare = n / totpop) %>% 
    mutate(ethsharesq = ethshare^2) %>% 
    mutate(p = ethsharesq * (1-ethshare))
  
  elfeth <- 1 - as.numeric(summarize(ethshares, ethsharesq = sum(ethsharesq)))
  poleth <- as.numeric(summarize(ethshares, p = sum(p)))
  
  #for each question ('culture' questions)
  for (q in finalqlist) {
    #enquote item
    enquo(q)
    #subset data
    tempdata_oneq <- tempdata %>% 
      select(COUNTRY, Q84, !!q)
    
    #number of total respondents for a question (numbtot)
    numbtot <- tempdata_oneq %>%   
      drop_na(!!q) %>% 
      summarize(n = n()) %>% as.numeric()
    
    #skip if no respondents
    if (numbtot == 0) {
      Ben_Sen_measures <- Ben_Sen_measures %>% 
        add_row(
          country = as.character(y), 
          question = q,
        )
      next
    }
    else {
      #all possible answers to the question
      maxnum <- tempdata_oneq %>% select(!!q) %>% drop_na() %>% unique() %>% pull()
      
      #get total number of ethnicities that answered question (numethn3)
      numethn3 <- tempdata_oneq %>% 
        drop_na(!!q) %>% select(Q84) %>% unique() %>% pull()
      
      attach(tempdata_oneq)
      #cross tab counts of responses and ethnicity (in stata: tab `q' x051) (varble)
      resp_count <- table(Q84, get(q))
      
      #number of ppl that responded to question (numbans2)
      numbans2 <- rowSums(resp_count)
      # share of responses to question in ethnicity (sum_eth/share) = numbans2/numbtot
      share <- numbans2/numbtot
      
      # ***compute share responding specific answer of total responses to question 
      #(freqans = varble[eth, ] / numbans2) / freqans_total = varble_all[resp] / numbtot
      freqans <- resp_count / numbans2
      freqans_total <- colSums(resp_count) / numbtot
      detach(tempdata_oneq)
      
      # ***cultural elf = 1 - sum over all responses [ freqans^2 ] (cultelf/cultelf_all)
      cultelf <- 1- rowSums(freqans^2)
      cultelf_all <- 1 -sum(freqans_total^2)
      
      # ***cultural pol = sum over all responses[ (freqans^2) * (1-freqans)] (cultelf/cultpol_all)
      cultpol <- rowSums(  freqans^2 * (1-freqans) )
      cultpol_all <- sum( freqans_total^2 *(1-freqans_total))
      
      #chisq = sum over all responses [ ((freqans_total - freqans)*share)^2 / (freqans_total * share) ]
      #chisq[q, country] = sum over all ethnicities [chisq]
      chi_sq_temp <- sum(
        rowSums( 
          (t(freqans_total - t(freqans)) * share)^2 / (share %*% t(freqans_total)) 
        )
      )
      #fst[q, country] = ( 1 - (sum over all ethnicities [cultelf * share]  /  (cultelf_all)
      fst_temp <- 1 - sum(cultelf * share)/cultelf_all
      
      #TODO: distance scaled CF
      
      #TODO : fstdist[q, country] =  same form but use cultelf_dist instead
      
    } #end else block
    
    #add measures by question to dataset
    Ben_Sen_measures <- Ben_Sen_measures %>% 
      add_row(
        country = as.character(y), 
        question = q,
        chisq = chi_sq_temp,
        fst = fst_temp,
        cultelf = cultelf_all,
        cultpol = cultpol_all
      )
    
  } #end question loop
  
  #add elf to all rows with this country
  
  Ben_Sen_measures <- Ben_Sen_measures %>% #384 observations
    mutate(ethnic_elf = ifelse(country == as.character(y), elfeth, ethnic_elf),
           ethnic_pol = ifelse(country == as.character(y), poleth, ethnic_pol)
    ) 
  
  
} #end country loop


####Creating Country Averages of relevant measures####
All_Country_measures <- All_Country_measures %>% 
  group_by(country) %>%
  mutate(ELF = mean(ethnic_elf, na.rm = TRUE)) %>%
  mutate(CF = mean(cultelf, na.rm = TRUE)) %>%
  mutate(FST = mean(fst, na.rm = TRUE))
  
Ben_Sen_measures <- Ben_Sen_measures %>% 
  group_by(country) %>%
  mutate(ELF = mean(ethnic_elf, na.rm = TRUE)) %>%
  mutate(CF = mean(cultelf, na.rm = TRUE)) %>%
  mutate(FST = mean(fst, na.rm = TRUE))

####Adding Country Name and code columns to datasets####
#Adding countryname and countrycode column
All_Country_measures <- All_Country_measures %>% 
  mutate(countryname = country) 

All_Country_measures <- All_Country_measures %>% 
  mutate(countrycode = country) 

for (i in seq_along(countno)){
  All_Country_measures$countryname[All_Country_measures$countryname == i] <- countname[i]}

for (i in seq_along(countno)){
  All_Country_measures$countrycode[All_Country_measures$countrycode == i] <- countname_3[i]}

#Adding countryname and countrycode column
Ben_Sen_measures <- Ben_Sen_measures %>% 
  mutate(countryname = country) 

Ben_Sen_measures <- Ben_Sen_measures %>% 
  mutate(countrycode = country) 

for (i in seq_along(countno)){
  Ben_Sen_measures$countryname[Ben_Sen_measures$countryname == i] <- countname[i]}

for (i in seq_along(countno)){
  Ben_Sen_measures$countrycode[Ben_Sen_measures$countrycode == i] <- countname_3[i]}

####Saving datasets####
saveRDS(All_Country_measures, output_All_Country_Measures_rds)
saveRDS(Ben_Sen_measures, output_Benin_Senegal_Measures_rds)

