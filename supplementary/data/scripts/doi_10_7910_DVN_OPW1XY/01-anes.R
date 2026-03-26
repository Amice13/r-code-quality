# ========================================================================= #
# ASKING THE RIGHT QUESTIONS
# - Replicate Figure 1 in main text (based on CES data)
# - Author: Patrick W. Kraft
# - Date: 03/19/2022
# ========================================================================= #
# NOTE: DOWNLOAD THE FOLLOWING DATA SETS FROM https://electionstudies.org/
# - 2020 Time Series Study (Stata file + redacted open-ends)
# - 2016 Time Series Study (Stata file + redacted open-ends)
# - 2012 Time Series Study (Stata file + redacted open-ends)

## Required packages
library(tidyverse)
library(labelled)
library(preText)
library(haven)
library(stm)



# Prepare closed-ended survey responses -----------------------------------

anes2012 <- read_dta("data/anes_timeseries_2012_Stata12.dta") %>% 
  remove_val_labels() %>% 
  transmute(
    caseid = caseid,
    online = mode - 1,
    age = na_if(dem_age_r_x, -2),
    gender = recode_factor(gender_respondent_x, 
                           `1` = "Male", 
                           `2` = "Female"),
    female = as.numeric(gender == "Female"),
    educ_cont = recode(dem_edugroup_x,
                       `-9` = NA_real_,
                       `-2` = NA_real_),
    pid_cont = na_if(pid_x, -2),
    educ_pid = educ_cont * pid_cont
  )

anes2016 <- read_dta("data/anes_timeseries_2016_Stata13.dta") %>% 
  remove_val_labels() %>% 
  transmute(
    caseid = V160001_orig,
    online = V160501 - 1,
    age = recode(V161267,
                 `-9` = NA_real_,
                 `-8` = NA_real_),
    gender = recode_factor(V161342, 
                           `1` = "Male", 
                           `2` = "Female",
                           .default = NA_character_),
    female = as.numeric(gender == "Female"),
    educ_cont = recode(V161270,
                       `1` = 1, # less than HS
                       `2` = 1, # less than HS
                       `3` = 1, # less than HS
                       `4` = 1, # less than HS
                       `5` = 1, # less than HS
                       `6` = 1, # less than HS
                       `7` = 1, # less than HS
                       `8` = 1, # less than HS
                       `9` = 2, # HS
                       `10` = 3, # Some post HS no BA
                       `11` = 3, # Some post HS no BA
                       `12` = 3, # Some post HS no BA
                       `13` = 4, # BA
                       `14` = 5, # Graduate degree
                       `15` = 5, # Graduate degree
                       `16` = 5, # Graduate degree
                       .default = NA_real_),
    pid_cont = recode(V161158x,
                      `-9` = NA_real_,
                      `-8` = NA_real_),
    educ_pid = educ_cont * pid_cont
  )

anes2020 <- read_dta("data/anes_timeseries_2020_stata_20210324.dta") %>% 
  remove_val_labels() %>% 
  transmute(
    caseid = V200001,
    online = recode(V200002,
                    `1` = 0,
                    `2` = 0,
                    `3` = 1),
    age = na_if(V201507x, -9),
    gender = recode_factor(V201600, 
                           `1` = "Male", 
                           `2` = "Female",
                           .default = NA_character_),
    female = as.numeric(gender == "Female"),
    educ_cont = recode(V201511x,
                       `-9` = NA_real_,
                       `-8` = NA_real_,
                       `-2` = NA_real_),
    pid_cont = recode(V201231x,
                      `-9` = NA_real_,
                      `-8` = NA_real_),
    educ_pid = educ_cont * pid_cont
  )



# Prepare open-ended survey responses -------------------------------------

## Function for pre-processing
oePrep <- function(x){
  out <- x %>% 
    str_to_lower() %>% 
    str_replace(fixed("//"), " ") %>% 
    str_replace(fixed("\\"), " ") %>% 
    str_replace(fixed("..."), " ") %>% 
    str_replace(fixed("/"), " ") %>% 
    str_replace("\\s+", " ") %>% 
    str_trim()
  out[out %in% readLines("data/nonresponse.txt")] <- ""
  out[is.na(out)] <- ""
  return(out)
}

## Function for spell-checking
oeSpell <- function(x){
  tibble(x) %>% 
    write_csv("tmp3141593.csv", na = "", col_names = FALSE)
  spell <- aspell("tmp3141593.csv") %>%
    filter(Suggestions!="NULL")
  for(i in 1:nrow(spell)){
    x[spell$Line[i]] <- gsub(spell$Original[i], 
                             unlist(spell$Suggestions[i])[1], 
                             x[spell$Line[i]])
  }
  rm(spell)
  file.remove("tmp3141593.csv")
  return(x)
}

## Load open-ended responses and run pre-processing & spell-check
oe2012 <- read_csv("data/anes_timeseries_2012_openends.csv") %>% 
  mutate(across(where(is.character), oePrep)) %>% 
  mutate(across(where(is.character), oeSpell))
oe2016 <- read_csv("data/anes_timeseries_2016_redacted_openends.csv") %>% 
  mutate(across(where(is.character), oePrep)) %>% 
  mutate(across(where(is.character), oeSpell))
oe2020 <- read_csv("data/anes_timeseries_2020_redacted_openends.csv") %>% 
  mutate(across(where(is.character), oePrep)) %>% 
  mutate(across(where(is.character), oeSpell))
  


# Fit Structural Topic Model ----------------------------------------------

## ANES 2012 ----

df2012 <- anes2012 %>% 
  inner_join(tibble(caseid = oe2012$caseid,
                    resp = apply(oe2012[,-1], 1, paste, collapse = " "))) %>% 
  mutate(resp = str_trim(resp)) %>% 
  filter(resp != "") %>% 
  na.omit()

tmp2012 <- textProcessor(
  documents = df2012$resp,
  metadata = dplyr::select(df2012, age, female, educ_cont, pid_cont, educ_pid),
  customstopwords = readLines("data/stopwords.txt")
)

out2012 <- prepDocuments(
  tmp2012$documents, 
  tmp2012$vocab, 
  tmp2012$meta, 
  lower.thresh = 10
)

fit2012 <- stm(
  out2012$documents, 
  out2012$vocab, 
  prevalence = as.matrix(out2012$meta), 
  K = 50, 
  seed = 12345
)

## ANES 2016 ----

df2016 <- anes2016 %>% 
  inner_join(tibble(caseid = oe2016$caseid,
                    resp = apply(oe2016[,-1], 1, paste, collapse = " "))) %>% 
  mutate(resp = str_trim(resp)) %>% 
  filter(resp != "") %>% 
  na.omit()

tmp2016 <- textProcessor(
  documents = df2016$resp,
  metadata = dplyr::select(df2016, age, female, educ_cont, pid_cont, educ_pid),
  customstopwords = readLines("data/stopwords.txt")
)

out2016 <- prepDocuments(
  tmp2016$documents, 
  tmp2016$vocab, 
  tmp2016$meta, 
  lower.thresh = 10
)

fit2016 <- stm(
  out2016$documents, 
  out2016$vocab, 
  prevalence = as.matrix(out2016$meta), 
  K = 50, 
  seed = 12345
)


## ANES 2020 ----

df2020 <- anes2020 %>% 
  inner_join(tibble(caseid = oe2020$caseid,
                    resp = apply(oe2020[,-1], 1, paste, collapse = " "))) %>% 
  mutate(resp = str_trim(resp)) %>% 
  filter(resp != "") %>% 
  na.omit()

tmp2020 <- textProcessor(
  documents = df2020$resp,
  metadata = dplyr::select(df2020, age, female, educ_cont, pid_cont, educ_pid),
  customstopwords = readLines("data/stopwords.txt")
)

out2020 <- prepDocuments(
  tmp2020$documents, 
  tmp2020$vocab, 
  tmp2020$meta, 
  lower.thresh = 10
)

fit2020 <- stm(
  out2020$documents, 
  out2020$vocab, 
  prevalence = as.matrix(out2020$meta), 
  K = 50, 
  seed = 12345
)



# Topic differences b/w men and women -------------------------------------

## estimate topic prevalence effects
prep2012 <- estimateEffect(~ age + female + educ_cont + pid_cont + educ_pid, 
                           fit2012, meta = out2012$meta, uncertainty = "Global")
prep2016 <- estimateEffect(~ age + female + educ_cont + pid_cont + educ_pid, 
                           fit2016, meta = out2016$meta, uncertainty = "Global")
prep2020 <- estimateEffect(~ age + female + educ_cont + pid_cont + educ_pid,
                           fit2020, meta = out2020$meta, uncertainty = "Global")

## select topics with largest gender effects
tmp2012 <- tibble(estimate = sapply(summary(prep2012)$tables, 
                                    function(x) x["female","Estimate"]), 
                  topics = prep2012$topics) %>% arrange(estimate)
topics2012 <- c(head(tmp2012$topics, 5), tail(tmp2012$topics, 5))
tmp2016 <- tibble(estimate = sapply(summary(prep2016)$tables, 
                                    function(x) x["female","Estimate"]), 
                  topics = prep2016$topics) %>% arrange(estimate)
topics2016 <- c(head(tmp2016$topics, 5), tail(tmp2016$topics, 5))
tmp2020 <- tibble(estimate = sapply(summary(prep2020)$tables, 
                                    function(x) x["female","Estimate"]), 
                  topics = prep2020$topics) %>% arrange(estimate)
topics2020 <- c(head(tmp2020$topics, 5), tail(tmp2020$topics, 5))

## Export Figure 1: gender differences in topic proportions
png("output/fig01-stm_gender.png", height=5.5, width=4.5, units = "in", res = 400)
par(mfrow=c(3,1), mar=c(2.2,0.5,2.2,0.5))
plot.estimateEffect(prep2012, covariate = "female", topics = topics2012, model = fit2012
                    , xlim = c(-.05,.015), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "prob", n=5, verbose.labels = F, width=50
                    , main = "2012 ANES")
plot.estimateEffect(prep2016, covariate = "female", topics = topics2016, model = fit2016
                    , xlim = c(-.05,.015), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "prob", n=5, verbose.labels = F, width=50
                    , main = "2016 ANES")
plot.estimateEffect(prep2020, covariate = "female", topics = topics2020, model = fit2020
                    , xlim = c(-.05,.015), method = "difference", cov.value1 = 1, cov.value2 = 0
                    , labeltype = "prob", n=5, verbose.labels = F, width=50
                    , main = "2020 ANES")
dev.off()



# Export Session Info -----------------------------------------------------

sessionInfo() %>% 
  capture.output() %>% 
  writeLines("output/01-anes_sessionInfo.txt")
