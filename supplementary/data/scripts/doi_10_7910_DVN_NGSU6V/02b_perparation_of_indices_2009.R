# load packages and data --------------------------------------------------
library(haven)
library(tidyverse)
library(dplyr)
library(stringr)
library(SnowballC)

root <- "C:/Users/Wieczorek_W_Station/Dropbox/Projekte/Effektive_Schulsteuerung_PISA/Daten/PISA/"
path <- paste0(root,"Aufbereitete_Daten/")
setwd(path)

path_year <- paste0(path,"2009","/")
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
  
  df <- read.csv(paste0(co,"_2009.csv")) 
  df <- as_tibble(df)
  
  df <- df %>% #select(-starts_with("w_fstr")) %>%
    select(-starts_with("X")) # remove unneccessary weights and variables
  
  # Immigration Status ------------------------------------------------------
  df <- df %>%  mutate(mig_2nd = case_when((immig == "Native" | immig == "First-Generation") ~ 0,
                                     immig == "Second-Generation" ~ 1,
                                     immig == "N/A" ~  NA_real_)) %>%
                relocate(mig_2nd, .after = immig)
  
  # Language of Test spoken at home -----------------------------------------
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
  escs_grouped <- df %>% group_by(schoolid) %>% 
    select(escs,schoolid) %>%
    na.omit() %>% summarise_at(vars(escs),list(mean_escs = mean))
  
  df <- left_join(df,escs_grouped)
  df <- df %>% relocate(mean_escs, .after = escs)
  
  # School Level: Migration Background, 2nd generation ----------------------
  mig_grouped <- df %>% group_by(schoolid) %>% 
    select(mig_2nd,schoolid) %>%
    na.omit() %>%
    summarise_at(vars(mig_2nd),list(mean_mig = mean))
  df <- left_join(df,mig_grouped)
  
  # School Level: Mean Disciplinary Climate ---------------------------------
  discli_grouped <- df %>% 
    group_by(schoolid) %>% 
    select(schoolid,disclima) %>%
    na.omit() %>%
    summarise_at(vars(disclima),list(mean_disclima = mean))
  
  df <- left_join(df,discli_grouped)
  df <- df %>% relocate(mean_disclima, .after = disclima)
  remove(escs_grouped,mig_grouped, discli_grouped)
  
  
  # Student absenteeism = SC17Q02
  df <- df %>% 
    mutate(student_absenteeism = case_when(sc17q02 == "Not at all" ~ 1,
                                           (sc17q02 != "Not at all" & sc17q02 !=  "N/A" & sc17q02 != "Miss") ~ 0,
                                           (sc17q02 == "N/A" | sc17q02 == "Miss") ~ NA_real_)) %>%
    relocate(student_absenteeism, .after = disclima)  
  
  # Teacher absenteeism = SC17Q06 
  df <- df %>% 
    mutate(teacher_absenteeism = case_when(sc17q06 == "Not at all" ~ 1,
                                           (sc17q06 != "Not at all" & sc17q06 !=  "N/A" & sc17q06 != "Miss") ~ 0,
                                           (sc17q06 == "N/A" | sc17q06 == "Miss") ~ NA_real_)) %>%
    relocate(teacher_absenteeism, .after = student_absenteeism)  
  
  # School Autonomy ---------------------------------------------------------
  varnames <- df %>% select(starts_with("sc24")) %>%
     colnames()
  for(v in varnames){
    df[v] <- case_when(df[[v]] == "Tick" ~ 1,
                       df[[v]] == "No Tick" ~ 0,
                       df[[v]] == "N/A" ~ NA_real_)
  }
  
  ## get variable names of principals and teachers
  princ_teachers <- varnames[str_detect(varnames,regex("[12]$"))]
  ## get variable names of school authorities (local) and national authorities
  nat_authorities <- varnames[str_detect(varnames,regex("[45]$"))]
  
  if(co != "SGP"){
  df["autonomy"] <- df %>% select(princ_teachers) %>% 
    rowSums() - df %>% select(nat_authorities) %>% rowSums()
  }else{
    rowums_princ_teachers <- df %>% 
      select(princ_teachers) %>% 
      rowSums()
    rowsums_national_authorities <- df %>% 
      select(nat_authorities) 
    empty_columns <- colSums(is.na(rowsums_national_authorities) | rowsums_national_authorities == "") == nrow(rowsums_national_authorities)
    rowsums_national_authorities <- rowsums_national_authorities[, !empty_columns]
    rowsums_national_authorities <- rowsums_national_authorities %>%
      rowSums()
    
    df["autonomy"] <- rowums_princ_teachers - rowsums_national_authorities
  }
  
  # School Authority ------------------------------------------------------
  ## possible answers with format (1 yes; 0 no)
  ## Educational Leadership --> When principal answered "yes" to a question
  ## of sc24 item-battery
  
  varnames <- df %>% select(starts_with("sc24")) %>%
    select(ends_with("1")) %>% colnames()
  
  
  df["leadership"] <- df %>% select(varnames) %>%
    mutate(leadership  = rowMeans(.)) %>% select(leadership)
  
  # Accountability-Variable -------------------------------------------------
  
  varnames <- df %>% select(starts_with("sc16")) %>%
    colnames()
  
  for(v in varnames){
    df[v] <- case_when(df[[v]] == "Yes" ~ 1,
                       df[[v]] == "No" ~ 0,
                       df[[v]] == "N/A" ~ NA_real_)
  }
  
  df["accountability"] <- df %>% select(starts_with("sc16")) %>% rowSums()
  
  remove(nat_authorities,princ_teachers,v,varnames)
  
  # Clean and save dataframe ------------------------------------------------
  df <- df %>% select(-starts_with("sc16")) %>%
    select(-starts_with("sc24")| -starts_with("sc17")) %>%
    select(-starts_with("sc26"))
  
  write.csv(df, paste0(co,"with_indices_2009.csv"))
  
}
