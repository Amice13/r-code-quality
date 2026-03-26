# load packages and data --------------------------------------------------
library(haven)
library(tidyverse)
library(dplyr)
library(stringr)

## show package vesrion number
packageVersion('haven')
packageVersion('tidyverse')
packageVersion('dplyr')
packageVersion('stringr')


root <- "C:/Users/Wieczorek_W_Station/Dropbox/Projekte/Effektive_Schulsteuerung_PISA/Daten/PISA/"
path <- paste0(root,"Aufbereitete_Daten/")
setwd(path)

path_year <- paste0(path,"2000","/")
setwd(path_year)
list.files()


# Create Variables for year 2000 ------------------------------------------
#countries <- c("DEU","GBR")
countries <- c()

for(f in list.files()){
  countries <- append(countries, str_sub(f, 1,3))
}
countries <- unique(countries)
countries <- countries[str_detect(countries,"200", negate=TRUE)]

for(co in countries){
  print(paste("currently at:",co))
  
  df <- read.csv(paste0(co,"_2000.csv")) 
  df <- as_tibble(df)
  
 # df <- df %>% select(-starts_with("w_fstr")) # remove unneccessary weights

  # Immigration Status ------------------------------------------------------
  ## st16q01 = country of birth, self
  ## st16q02 = country of birth, mother
  ## st16q03 = country of birth, father
  df %>% select(starts_with("st16")) %>% unique()
  
  df <- df %>%  mutate(mig = case_when(st16q01 != "<Country of Test>" ~ 1,
                                      (st16q01 == "<Country of Test>" & 
                                        (st16q02 != "<Country of Test>" | 
                                         st16q03 != "<Country of Test>")) ~ 2,
                                       (st16q01 == "<Country of Test>" &
                                        st16q02 == "<Country of Test>" &
                                        st16q03 == "<Country of Test>") ~ 0 )
                        )
  
  df <- df %>%  mutate(mig_2nd = ifelse(mig == 2,1,0)) %>%
     as_factor() %>%
     select(-starts_with("X"))
  
  
  str_detect(df$st17q01,"<Other Languages>")
  
  # Language of the test spoken at home -------------------------------------
  df <- df %>% mutate(langn = case_when(str_detect(df$st17q01,"<Test language>")  ~ 1,
                                        str_detect(df$st17q01,"<Other Languages>") ~ 0),
                                      st17q01 == T ,NA_real_) %>%
  relocate(langn, .after = st17q01)  
  
  
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
  
  
  # Student absenteeism = SC19Q02
  df <- df %>% 
    mutate(student_absenteeism = case_when(sc19q02 == "Not at all" ~ 1,
                                           (sc19q02 == "Mis" | sc19q02== "M/R") ~ NA_real_,
                                            (sc19q02 != "Not at all" & sc19q02 != "Mis" &  sc19q02 != "M/R")~ 0)) %>%
    relocate(student_absenteeism, .after = disclima)  
  
  # Teacher absenteeism = SC19Q08
  df <- df %>% 
    mutate(teacher_absenteeism =   case_when(sc19q08 == "Not at all" ~ 1,
            (sc19q08 == "Mis" | sc19q08== "M/R") ~ NA_real_,
            (sc19q08 != "Not at all" & sc19q08 != "Mis" &  sc19q08 != "M/R")~ 0)) %>%
    relocate(teacher_absenteeism, .after = student_absenteeism)   
  
  # School Authority 2000 ---------------------------------------------------
  ## possible answers with format (1 yes; 0 no)
  # first value = not a school responsibility 
  # second value = appointed or elected board 
  # third value = principal
  # fourth value = department head
  # fifth value = teachers
  
  ## relevant are third values
  ## recode sc22-items, as they were originally strings with a5-spss-Format
  # e.g. 00001 maps to 1, that is the fifth answer is affirmed, whereas
  # others were declined
  
  varnames <- df %>% select(starts_with("sc22")) %>% colnames()
  ## aim 1) fill values
  
  for (v in varnames) {
   # print(v)
    
    # change numeric variable into string
    df[v] <- as.character(df[[v]])
    
    # perform first test: check character lenght and assign lenght to test-variable
    df <- df %>%
      mutate(test = case_when(nchar(df[[v]]) == 5 ~ 5, 
                              nchar(df[[v]]) == 4 ~ 4,
                              nchar(df[[v]]) == 3 ~ 3,
                              nchar(df[[v]]) == 2 ~ 2,
                              nchar(df[[v]]) == 1 ~ 1)) 
    
    # perform transformation and fill empty spaces
    df <- df %>% mutate(testx = case_when(test == 5 ~ get(v),
                                               test == 4 ~ paste0("0",get(v)),
                                               test == 3 ~ paste0("00",get(v)),
                                               test == 2 ~ paste0("000", get(v)),
                                               test == 1 ~ paste0("0000",get(v)))) %>%
      mutate(newvar = testx) %>% 
      select(-starts_with("test"))
    
    # map onto old variable
    df[v] <- df["newvar"]  
    
    #delete old variable
    df <- df %>% select(-starts_with("newvar"))
  }
  
  # aim 2) count every string with a 1 on third position
  
  for(v in varnames){
    df <- df %>% mutate(string_start = str_locate(df[[v]], "(1)*")[,1],
                        string_end = str_locate(df[[v]], "(1)*")[,2])
    
    
    varname <- paste0(v,"_principal")
    
    df[varname] <- if_else((df$string_start <=3  & df$string_end >=3),1,0)
    df <- df %>% select(-starts_with("string"))
    
  }
  # aim 3) calculate educational leadership-score from variables in 2)
  
  #df["leadership"] <- df %>% select(ends_with("_principal")) %>% rowwise() %>%  count()
  
  
  df["leadership"] <- 
    df %>%select(ends_with("_principal")) %>% 
    mutate(leadership  = rowMeans(.)) %>% select(leadership)
    
  
  # Accountability-Variable -------------------------------------------------
  ## relable variables
  varnames <- df %>% select(starts_with("sc18")) %>% colnames
  
  for(v in varnames){
    df[v] <- df %>% 
      mutate(test = case_when(
                    df[[v]] == "N/A" ~ NA_real_,
                    df[[v]] == "Yes" ~ 1,
                    df[[v]] == "No" ~ 0
                              )
                            ) %>% select(test)
  }
  
  df["accountability"] <- df %>% select(varnames) %>% rowSums()
  
  ## clean dataframe
  df <- df %>% select(-ends_with("principal")) %>% 
    select(-starts_with("sc22")| -starts_with("sc19")) %>%
    select(-matches("st1[678]")) %>% 
    select(-matches("sc18"))
  
  write.csv(df, paste0(co,"with_indices_2000.csv"))
}
