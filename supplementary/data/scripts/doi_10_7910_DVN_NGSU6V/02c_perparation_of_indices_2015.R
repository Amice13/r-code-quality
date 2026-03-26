# load packages and data --------------------------------------------------
library(haven)
library(tidyverse)
library(dplyr)
library(stringr)
library(SnowballC)

root <- "C:/Users/Wieczorek_W_Station/Dropbox/Projekte/Effektive_Schulsteuerung_PISA/Daten/PISA/"
path <- paste0(root,"Aufbereitete_Daten/")
setwd(path)

path_year <- paste0(path,"2015","/")
setwd(path_year)
list.files()

# Create Variables for year 2000 ------------------------------------------
#countries <- c("DEU","GBR")
countries <- c()

for(f in list.files()){
  countries <- append(countries, str_sub(f, 1,3))
}


countries <- as.list(unique(countries))


for(co in countries){
  print(paste("currently at:",co))
  
  df <- read.csv(paste0(co,"_2015.csv")) 
  df <- as_tibble(df)
  
  df <- df %>%
    select(-starts_with("X")) # remove unneccessary weights and variables
  
  # Immigration Status ------------------------------------------------------
  df <- df %>%  mutate(mig_2nd = case_when((immig == "NATIVE" | immig == "FIRST-GENERATION") ~ 0,
                                           immig == "SECOND-GENERATION" ~ 1,
                                           immig == "NO RESPONSE" ~  NA_real_)) %>%
    relocate(mig_2nd, .after = immig)
  
  # Language of Test spoken at home (old) -----------------------------------------
  
  #df <- df %>% mutate(langn = case_when(
  #  str_detect(tolower(langn), paste("german", collapse = "|")) ~ 1,
  #  str_detect(tolower(langn),"na") ~ NA_real_,
  #  str_detect(tolower(langn), paste("german", collapse = "|"), negate = T) ~ 0)
  #)
  # Language of Test spoken at home (new) -----------------------------------------
  langhome <- NA
  if(co == "DEU"){
    langhome = "german"
  } else if(co =="GBR" | co == "USA" | co == "AUT"){
    langhome = "english"
  } else if(co == "FIN"){
    langhome = "finnish"
  } else if(co =="SWE"){
    langhome = "swedish"
  } else if(co == "SGP"){
    langhome1 = "malay"
    langhome2 = "english"
  } else if(co == "JPN"){
    langhome = "japanese"
  } else if(co == "KOR"){
    langhome = "korean"
  } else if(co == "CAN"){
    langhome1 = "english"
    langhome2 = "french"
  }
  
  if(co == "CAN"| co == "SGP"){
    df <- df %>% mutate(langn = case_when(
      str_detect(tolower(langn), paste(langhome1, collapse = "|")) | 
        str_detect(tolower(langn), paste(langhome2, collapse = "|"))  ~ 1,
      str_detect(tolower(langn),"n/a") ~ NA_real_,
      str_detect(tolower(langn), paste(langhome1, collapse = "|"), negate = T)|
        str_detect(tolower(langn), paste(langhome1, collapse = "|"), negate = T)~ 0)
    )
  } else{
    df <- df %>% mutate(langn = case_when(
      str_detect(tolower(langn), paste(langhome, collapse = "|")) ~ 1,
      str_detect(tolower(langn),"n/a") ~ NA_real_,
      str_detect(tolower(langn), paste(langhome, collapse = "|"), negate = T) ~ 0)
    )
    
  }
  
  # School Level: ESCS ------------------------------------------------------
  escs_grouped <- df %>% group_by(cntschid) %>% 
    select(escs,cntschid) %>%
    na.omit() %>% summarise_at(vars(escs),list(mean_escs = mean))
  
  df <- left_join(df,escs_grouped)
  df <- df %>% relocate(mean_escs, .after = escs)
  
  # School Level: Migration Background, 2nd generation ----------------------
  mig_grouped <- df %>% group_by(cntschid) %>% 
    select(mig_2nd,cntschid) %>%
    na.omit() %>%
    summarise_at(vars(mig_2nd),list(mean_mig = mean))
  df <- left_join(df,mig_grouped)
  df <- df %>% relocate(mean_mig, .after = mig_2nd)
  
  # School Level: Mean Disciplinary Climate ---------------------------------
  discli_grouped <- df %>% 
    group_by(cntschid) %>% 
    select(cntschid,disclisci) %>%
    na.omit() %>%
    summarise_at(vars(disclisci),list(mean_disclima = mean))
  
  df <- left_join(df,discli_grouped)
  df <- df %>% relocate(mean_disclima, .after = disclisci) %>% rename(disclima = disclisci)
  remove(escs_grouped,mig_grouped, discli_grouped)
  
  
  # student skipping classes = SC061Q02TA
  df <- df %>% 
    mutate(student_absenteeism = case_when(sc061q02ta == "NOT AT ALL" ~ 1,
                                           (sc061q02ta != "NOT AT ALL" & sc061q02ta !=  "NO RESPONSE") ~ 0,
                                           sc061q02ta == "NO RESPONSE" ~ NA_real_)) %>%
    relocate(student_absenteeism, .after = disclima)  
  # teacher absenteeism = SC061Q07TA
  df <- df %>% 
    mutate(teacher_absenteeism = case_when(sc061q07ta == "NOT AT ALL" ~ 1,
                                           (sc061q07ta != "NOT AT ALL" & sc061q07ta !=  "NO RESPONSE") ~ 0,
                                           sc061q07ta == "NO RESPONSE" ~ NA_real_)) %>%
    relocate(teacher_absenteeism, .after = student_absenteeism)    
  
  # Accountability ----------------------------------------------------------
  varnames <- df %>% select(starts_with("sc035")) %>% colnames()
  
  for(v in varnames){
    df[v] <- case_when(df[[v]] == "YES" ~ 1,
                      df[[v]] == "NO" ~ 0,
                      df[[v]] == T ~ NA_real_)
  }
  
  df["accountability"] <- df %>% select(varnames) %>% rowSums()
  # Rename indices, clean and save data -------------------------------------
  remove(varnames,v)
  df <- df %>% rename(autonomy = schaut) %>%
    rename(leadership = lead) %>%
    select(-starts_with("sc035")) %>%
    select(-starts_with("st034")| -starts_with("sc061")) %>%
    select(-starts_with("st038"))
  
  write.csv(df, paste0(co,"with_indices_2015.csv"))
}
