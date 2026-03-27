# THIS FILE IDENTIFIES THE NAMES OF ITEMS AND SCALES FOR ANALYSIS AND GROUPING
# VARIABLES FOR DIF ANALYSIS


# 1. IDENTIFY SCALES TO USE =============================================


#' 1A. MOBILIZATION ----------------
mobil_vars <- c(
  "mobilpo_rmob", # TALK WITH ANYONE? (MOBILPO_RMOB)
  "mobilpo_rally", # political meetings? (MOBILPO_RRALLY)
  "mobilpo_sign", # WEAR A BUTTON OR PUT UP SIGN (MOBILPO_RBUTTN)
  "mobilpo_otherwork", # WORK FOR PARTY (MOBILPO_RCAMPWK)
  "mobilpo_ctbcand", # GIVE $ TO PARTY INDIVIDUAL CANDIDATE (MOBILPO_CTBCAND)
  "mobilpo_ctbpty", # GIVE $ TO PARTY (MOBILPO_CTBPTY)
  "mobilpo_ctboth" # GIVE $ TO OTHER GROUP (MOBILPO_CTBOTH)
)


##  _1B. LIMITED GOVERNMENT ----------------
# ANSOLABEHERE, RODDEN, & SNYDER (APSR 2008) USE THREE BINARY ITEMS
# SO IGNORING FIVE-POINT 'govrole_regbus'
ltd_vars <- c(
  "govrole_big",
  "govrole_market",
  "govrole_lessmore"
  )



##  _1C. MORAL TRADITIONALISM ----------------
trad_vars <- c(
  "trad_adjust",
  "trad_lifestyle",
  "trad_tolerant",
  "trad_famval"
  )



##  _1D. RACIAL RESENTMENT  ----------------

race_vars <- c(
  "resent_workway",
  "resent_slavery",
  "resent_deserve",
  "resent_try"
  )



##  _1E. AUTHORITARIANISM  ----------------

auth_vars <- c(
  "auth_ind",
  "auth_cur",
  "auth_obed",
  "auth_consid"
  )



##  _1F. TIPI  ----------------
agree_vars <- c(
  "agree_critical",
  "agree_sympathetic"
  )

emot_vars <- c(
  "emot_anxious",
  "emot_calm"
  )

open_vars <- c(
  "open_open",
  "open_conventional"
  )

extra_vars <- c(
  "extra_extraverted",
  "extra_reserved"
  )

conscien_vars <- c(
  "conscien_dependable",
  "conscien_disorganized"
  )


tipi_vars <- c(
  agree_vars,
  emot_vars,
  open_vars,
  extra_vars,
  conscien_vars
)



##  _1G. WORDSUM  ----------------
wordsum_vars <-  c(
  "wordsum_setb_crt",
  "wordsum_setd_crt",
  "wordsum_sete_crt",
  "wordsum_setf_crt",
  "wordsum_setg_crt",
  "wordsum_seth_crt",
  "wordsum_setj_crt",
  "wordsum_setk_crt",
  "wordsum_setl_crt",
  "wordsum_seto_crt"
)


##  _1H. EQUALITARIANSIM/ EGALITARIANISM  ----------------

# ONLY USING FOUR FROM 2016, NOT EXTRA TWO FROM 2012
egal_vars <- c(
  "egal_equal", # EGAL_DONECESS
  "egal_worryless", #EGAL_WORRYLESS
  "egal_notbigprob", #EGAL_NOTBIGPROB
  "egal_fewerprobs" # EGAL_FEWERPROBS
)



##  _1I. NEGATIVE BLACK STEREOTYPES ----------------

# ON 2016, USES HARDWORKING AND VIOLENT,
# BUT IN 2012, USES HARDWORKING AND INTELLIGENT

stereo_vars <- c(
  "stype_hwkblack",
  "stype_intblack"
)

# 2. NAMES OF ALL SCALES TO TEST  =============================================

scale_names <- c(
  "mobil_vars",
  "ltd_vars",
  "auth_vars",
  "wordsum_vars",
  "trad_vars",
  "race_vars",
  "agree_vars",
  "emot_vars",
  "open_vars",
  "extra_vars",
  "conscien_vars",
  "egal_vars",
  "stereo_vars"
)




#  MAP SCALE NAMES TO LABELS
scale_labs <- c(
  "Non-Voting Participation",
  "Limited Government",
  "Authoritarianism",
  "Wordsum",
  "Moral Traditionalism",
  "Racial Resentment",
  "TIPI: Agreeableness",
  "TIPI: Emotional Stability",
  "TIPI: Openness To Experiences",
  "TIPI: Extraversion",
  "TIPI: Conscientiousness",
  "Egalitarianism",
  "Negative Black Stereotypes"
)

labdata_scale <- data_frame(
  scale_abrv = scale_names,
  scale_lab  = scale_labs
  )

item_ids <- data_frame(scale_abrv = scale_names)  %>%
  mutate(item = map(scale_abrv, get))  %>%
  unnest()  %>%
  group_by(scale_abrv)  %>%
  mutate(item_id = row_number())


# 3.  GROUPING VARIABLES TO TEST   =============================================


group_vars <- c(
    "female",
    "pid3",
    "libcon3",
    "turnout",
    "race_nominal",
    "educ_ordinal" ,
    "age",
    "income",
    "votechoice",
    "internet"
  )

#  MAP GROUP VAR NAMES TO LABELS
group_labs <- c(
    "Gender",
    "Party ID",
    "Ideology",
    "Electoral turnout",
    "Race/ethnicity",
    "Education" ,
    "Age",
    "Income",
    "Pres. vote choice",
    "Survey mode"
)

labdata_group <- data_frame(
  group_abrv = group_vars,
  group_lab  = group_labs
  )


#  4.  ALL SCALE ITEMS   =============================================

all_scale_items <- map(scale_names, get)  %>% unlist()




#  5. VALUES OF GROUPING VARIABLES   ==========================================

group_labs_list <- list(
  female = c("Male", "Female"),
  pid3 = c("Democratic", "Independent", "Republican"),
  libcon3 = c("Liberal", "Moderate", "Conservative"),
  turnout = c("Did not vote", "Voted"),
  race_nominal = c("White",
                 "Black",
                 "Hispanic",
                 "Other"),
  educ_ordinal = c("< High school",
                 "High school",
                 "Some college",
                 "Bachelor's",
                 "Grad. deg."),
  age = c("< 40 years",
                 "40 - 59",
                 ">= 60"),
  income = c("< $25k",
                 "$25k - $64k",
                 ">= $65k"),
  votechoice = c("Dem. cand.", "Rep. cand."),
  internet = c("Face-to-face", "Internet")
)


group_labs_df <- labdata_group  %>%
  group_by(group_abrv, group_lab)  %>%
  nest(.key = value_lab)  %>%
  mutate(value_lab = map(group_abrv, ~group_labs_list[[.x]]))  %>%
  unnest()  %>%
  group_by(group_abrv)  %>%
  mutate(anova_id =  row_number() - 1)  %>%
  mutate(term = paste0("group_var", anova_id) )  %>%
  mutate(term = ifelse(term == "group_var0", "(Intercept)", term))  %>%
  mutate(term = ifelse(term == "group_var1" & group_abrv %in% c("pid3", "libcon3"),
                       "group_var0",
                       term))  %>%
  mutate(term = ifelse(term == "group_var2" & group_abrv %in% c("pid3", "libcon3"),
                       "group_var1",
                       term)) %>%
  mutate(value_lab = factor(value_lab, .$value_lab[1:nrow(.)]))


facet_labs <- group_labs_df  %>%
  filter(term == "(Intercept)")  %>%
  mutate(facet_lab = paste0(group_lab, "\nBaseline = ", value_lab) )  %>%
  select(group_abrv, facet_lab, baseline = value_lab)
