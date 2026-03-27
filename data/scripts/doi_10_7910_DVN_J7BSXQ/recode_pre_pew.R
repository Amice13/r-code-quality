#### Pew Pre-Wave Recoding (Cleaned March 2021)
rm(list = ls())

library(MASS)
library(tidyverse)
library(foreign)

### region definitions
northeast_states = c("Massachusetts", "Pennsylvania", "Connecticut",
                     "New Hampshire", "New York", "New Jersey", "Maine",
                     "Rhode Island", "Vermont")
midwest_states = c("Minnesota", "Iowa", "Nebraska", "Ohio", "Indiana",
                   "Wisconsin", "Kansas", "Missouri", "Illinois", "Michigan",
                   "South Dakota", "North Dakota") 
south_states = c("Delaware", "Texas", "North Carolina", "Georgia", "Alabama",
                 "West Virginia", "Maryland", "District of Columbia",
                 "Kentucky", "South Carolina", "Tennessee", "Louisiana",
                 "Oklahoma", "Florida", "Arkansas", "Virginia", "Mississippi") 
west_states = c("California", "Oregon", "Washington", "Hawaii", "New Mexico",
                "Alaska", "Colorado", "Utah", "Wyoming", "Arizona", "Nevada",
                "Montana", "Idaho")

################################################################################
#### Pew #######################################################################
################################################################################

### load original Pew .sav file
#late october pew
## Originally downloaded from: 
## https://www.pewresearch.org/politics/2016/10/27/as-election-nears-voters-divided-over-democracy-and-respect/
## Downloaded 2018/10/11

pew <- read.spss("./uploaded/Oct16 public.sav", to.data.frame=TRUE)

# Q11HORSE2 = q11 + q10 + q10a where:
# q10: if the election were today what candidate would you choose among: trump, clinton, stein, johnson, other, don't know, refused
# q10a: Asked ONLY IF responded other or don't know on q10 - if the election were held today which candidate do you lean toward among the same options as above
# q11: Asked ONLY IF respondents answered stein, johnson, other, don't know in q10/q10a: suppose there were only two major candidates for president trump and hilary, who would you choose? however still allows responses: clinton, trump, other, don't know
# (There's also a q11a which follows up to those who answer other or don't know in q11 to lean, but this is not included in Q11HORSE2)

#note that after we filter for those that don't plan to vote in plan1 (at bottom), we loose the NAs and end up grouping DNK-refused to lean +other in other
pew <- pew %>%
    mutate(recode_vote_2016 =
               case_when(str_detect(Q11HORSE2, "Trump") ~ "Republican",
                         str_detect(Q11HORSE2, "Clinton") ~ "Democrat",
                         is.na(Q11HORSE2) ~ NA_character_,
                         TRUE ~ "Other"
               ))

nrow(pew)

## Start by looking at missingness (fraction):
lapply(pew[, c("age", "sex", "racethn", "state", "party", "educ2")], 
       function(x) (sum(is.na(x)) + sum(grepl("Don't know/Refused", x))) / length(x))

#age and party and educ2 have no NAs, only don't know/refused
#racethn does have NAs and no don't know/refused

## First recodes
pew <- pew %>% mutate(
  # age
  recode_age = ifelse(age == "Don't know/Refused (VOL.)", 
                      NA, 
                      as.numeric(as.character(age))),
  
  # gender
  recode_female = case_when(sex == "Female" ~ "Female",
                            TRUE ~ "Male"),
  # race/ethnicity
  # Note: combines missing with Other
  recode_race = case_when(racethn == "White, non-Hisp" ~ "White",
                          racethn == "Black, non-Hisp" ~ "Black",
                          racethn == "Hispanic" ~ "Hispanic",
                          TRUE ~ "Other"),
  # region (note there are no cases that do not fall into these groups such that we fill TRUE ~ South)
  recode_region = case_when(state %in% northeast_states ~ "Northeast",
                            state %in% west_states ~ "West",
                            state %in% midwest_states ~ "Midwest",
                            state %in% south_states ~ "South",
                            TRUE ~ "South"),
  
  # party -- combines refused + no preference with Independents
  recode_pid_3way = case_when( party == "Democrat" ~ "Dem",
                               party == "Republican" ~ "Rep",
                               TRUE ~ "Ind"),
  
  # state
  recode_inputstate = state,
  
  # education -- leaving missing for now (14 Don't know)
  recode_educ = factor(case_when( 
    educ2 == "Less than high school (Grades 1-8 or no formal schooling) " ~ "No HS",
    educ2 == "High school incomplete (Grades 9-11 or Grade 12 with NO diploma)" ~ "No HS",
    educ2 == "High school graduate (Grade 12 with diploma or GED certificate)" ~ "High school graduate",
    educ2 == "Some college, no degree (includes some community college)" ~ "Some college",
    educ2 == "Two year associate degree from a college or university" ~ "2-year",
    educ2 == "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)" ~ "4-year",
    educ2 == "Some postgraduate or professional schooling, no postgraduate degree" ~ "Post-grad",
    educ2 == "Postgraduate or professional degree, including master's, doctorate, medical or law degree" ~ "Post-grad",
    TRUE ~ NA_character_), 
    levels = c("No HS", "High school graduate", "Some college", "2-year", "4-year", "Post-grad"))
)


## modeling age
## listwise delete missing on age/education
#45 observations dropped= 41 missingon age + 4 missing on educ
age_model <- glm(recode_age ~ recode_female + recode_race + recode_region + 
                   recode_pid_3way + recode_educ + child,
                 family = Gamma(),
                 data = pew)


#reports 41 observations dropped = those missing on age
age_model_no_educ <- glm(recode_age ~ recode_female + recode_race + 
                           recode_region + recode_pid_3way + child,
                         family = Gamma(),
                         data = pew)

## recode age and age buckets
pew <- pew %>%
  mutate(recode_age = case_when(
    ## if age is not missing, keep current age
    !is.na(recode_age) ~ recode_age,
    ## if age is missing but not education, use age_model
    is.na(recode_age) & !is.na(recode_educ) ~ round(predict(age_model, ., type = "response"), 0),
    ## if age and education are missing, use age_model_no_educ
    is.na(recode_educ) ~ round(predict(age_model_no_educ, ., type = "response"), 0)
  ),
  
  ## four way age bucket; should be no na's at this point
  recode_age_bucket = factor(case_when( recode_age <= 35 ~ "18 to 35",
                                        recode_age <= 50 ~ "36 to 50",
                                        recode_age < 65 ~ "51 to 64",
                                        !is.na(recode_age) ~ "65+"),
                             levels = c("18 to 35", "36 to 50", "51 to 64", "65+")),
  
  ## three way age bucket; should be no NAs at this point
  recode_age_3way = case_when( recode_age <= 50 ~ "a_18to50",
                               recode_age < 65 ~ "b_51to64",
                               !is.na(recode_age) ~ "c_65")
  )

## education model (ordered logit)
#drops 14 missing on educ
educ_model <- polr(recode_educ ~ recode_female + recode_race + recode_region + 
                     recode_pid_3way + recode_age + child,
                   data = pew)

pew <- pew %>%
  ## impute missing educ bucket
  mutate(recode_educ = case_when(
    ## if not missing, use current education
    !is.na(recode_educ) ~ recode_educ,
    ## if missing, use educ_model prediction
    is.na(recode_educ) ~ predict(educ_model, newdata =., type = "class")
  ),
  
  recode_educ_3way = factor(case_when(
    recode_educ %in% c("No HS", "High school graduate", "Some college") ~ "No College",
    recode_educ %in% c("2-year", "4-year") ~ "College",
    TRUE ~ "Post-grad"), 
    levels = c("No College", "College", "Post-grad"))
  )  

####### New Variables: income, bornagain, church attendance,
### Pew
pew <- pew %>% mutate(
  recode_relig = factor(case_when(
    #say unitarians are protestant (only 4 of these), Christians are protestant (269)
    relig == "Protestant (Baptist, Methodist, Non-denominational, Lutheran...)" ~ "Protestant",
    relig == "Christian (VOL.)" ~ "Protestant",
    relig == "Unitarian (Universalist) (VOL.)" ~ "Protestant",
    relig == "Jewish (Judaism)" ~ "Jewish",
    relig == "Roman Catholic (Catholic)" ~ "Catholic",
    relig == "Nothing in particular" ~ "Nothing in particular",
    relig == "Agnostic (not sure if there is a God)" ~ "Agnostic",
    relig == "Atheist (do not believe in God)" ~ "Atheist",
    relig == "Buddhist" ~ "Buddhist",
    relig == "Muslim (Islam)" ~ "Muslim",
    relig == "Mormon (Church of Jesus Christ of Latter-day Saints/LDS)" ~ "Mormon",
    relig == "Hindu" ~ "Hindu",
    relig == "Orthodox (Greek, Russian, or some other orthodox church)" ~ "Orthodox",
    #these people were asked a follow up if considered christain in $chr
    #out of 41 (31) of these: 11 (8) said yes christ, 29 (22) no, 1 refused
    relig == "Something else (SPECIFY:______)" ~ "Something else",
    #those who respond don't know/refuse =NA (36) (NB: also asked if consider christian in chr)
    #out of these 50 (36): 30 (24) said yes christian, 11 (5) said no, 9 (7) refused
    relig == "Don't Know/Refused (VOL.)" & chr == "Yes" ~ "Protestant",
    #leaves only 20 (12) missing
    TRUE ~ NA_character_),
    levels = c("Protestant", "Jewish", "Catholic", "Nothing in particular", 
               "Agnostic", "Atheist", "Buddhist", "Muslim", "Mormon", "Hindu",
               "Orthodox", "Something else")),
  
  #supposedly only asked:if chr = yes or if relig = 1-4,13
  #(Protestant, Catholic, Mormon, Orthodox, Christian)
  #though looking at the NAs that's not totally true some in these groups still have NAs
  recode_born = case_when(born == "Yes, would" ~ "Yes",
                          born == "No, would not" ~ "No",
                          #46 refused but we think it's safe to say these people are not
                          born == "Don't know/Refused (VOL.)" ~ "No",
                          #NA's (589) these are mainly non-christian denominations
                          #so I'd say safe to say they are not born again
                          TRUE ~ "No"),
  
  recode_income = factor(case_when(
    income == "Less than $10,000" ~ "<10k",
    income == "10 to under $20,000" ~ "10-20k",
    income == "20 to under $30,000" ~ "20-30k",
    income == "30 to under $40,000" ~ "30-40k",
    income == "40 to under $50,000" ~ "40-50k",
    income == "50 to under $75,000" ~ "50-100k",
    income == "75 to under $100,000" ~ "50-100k",
    income == "100 to under $150,000 [OR]" ~ "100-150k",
    income == "$150,000 or more" ~ ">150k",
    #222 refused
    income == "[VOL. DO NOT READ] Don't know/Refused" ~ "Prefer not to say",
    #there should be no NAs, but as precaution lump them with prefer not to say
    TRUE ~ NA_character_),
    levels = c("<10k", "10-20k", "20-30k", "30-40k", "40-50k", "50-100k",
               "100-150k",">150k", "Prefer not to say")),
  recode_income_5way = factor(case_when(
    income == "Less than $10,000" ~ "<20k",
    income == "10 to under $20,000" ~ "<20k",
    income == "20 to under $30,000" ~ "20-50k",
    income == "30 to under $40,000" ~ "20-50k",
    income == "40 to under $50,000" ~ "20-50k",
    income == "50 to under $75,000" ~ "50-100k",
    income == "75 to under $100,000" ~ "50-100k",
    income == "100 to under $150,000 [OR]" ~ "100-150k",
    income == "$150,000 or more" ~ ">150k",
    #222 refused
    income == "[VOL. DO NOT READ] Don't know/Refused" ~ "Prefer not to say",
    #there should be no NAs
    TRUE ~ NA_character_),
    levels = c("<20k", "20-50k", "50-100k", "100-150k",">150k", "Prefer not to say")),
  
  recode_income_3way = factor(case_when(
    income == "Less than $10,000" ~ "<50k",
    income == "10 to under $20,000" ~ "<50k",
    income == "20 to under $30,000" ~ "<50k",
    income == "30 to under $40,000" ~ "<50k",
    income == "40 to under $50,000" ~ "<50k",
    income == "50 to under $75,000" ~ "50-150k",
    income == "75 to under $100,000" ~ "50-150k",
    income == "100 to under $150,000 [OR]" ~ "50-150k",
    income == "$150,000 or more" ~ ">150k",
    #222 refused
    income == "[VOL. DO NOT READ] Don't know/Refused" ~ "Prefer not to say",
    #there should be no NAs
    TRUE ~ NA_character_),
    levels = c("<50k", "50-150k",">150k","Prefer not to say"))
)


## Remove those who say they definitely will not vote 
#463 NAs (all also have NA vote choice) and 46 don't plan to vote (some have vote pref but if they refuse then fine to drop)
#(5 of those that are NA here are missing also on age/educ so we get from 45 missing to 40 missing total)

pew <- pew%>%
  filter(plan1 %in% c("Plan to vote", "Already voted", "Don't know/Refused (VOL.)"))

pew <- select(pew,
                    "recode_age_bucket", "recode_female", "recode_race", 
                    "recode_region", "recode_pid_3way", "recode_educ", "recode_inputstate",
                    "recode_relig", "recode_born", "recode_income", "recode_income_5way", "recode_educ_3way",
                    "recode_income_3way", "recode_age_3way",
                    "recode_vote_2016")

saveRDS(pew, "./generated/pew.rds")