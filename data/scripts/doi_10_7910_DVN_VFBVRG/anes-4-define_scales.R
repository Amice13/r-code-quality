################################################################################
# Created By: Pietryka
# Creation Date:  2017-02-13
# Purpose: CODE SCALE ITEMS FROM ANES
# R Version: R version 3.3.3 (2017-03-06)
# Data Input: 'Data/Derived/2012/anes-2-define_groupvars.RData' &
#             'Data/Derived/2016/anes-3-rename_scalevars.RData'
# Data Output: Data/Derived/2012/groups_and_scales.csv &
#             'Data/Derived/2016/groups_and_scales.csv'
#
# Questions: mpietryka@fsu.edu
################################################################################

#=============================================
#'#  1. PREAMBLE
#=============================================


# LOAD PACKAGES ----------------#
library(tidyverse) # MANY CONVENIENCE FUNCTIONS
library(sjmisc) # RECODE AND VARIABLE LABEL FUNCTIONS


# LOAD FUNCTIONS TO RESCALE ITEMS---------------- #
source("Functions/FUN-rescale_items.R")

# LOAD FUNCTION TO CONFIRM THAT SCALE ITEMS HAVE POSITIVE CORRELATIONS #
source("Functions/FUN-check_scale_items.R")

# LOAD DATA  ----------------#

# LOAD DATA CREATED IN 'DataClean/anes-2-define_groupvars.R'
# CONTAINS OBJECT: 'a12_r'
load("Data/Derived/2012/anes-2-define_groupvars.RData")
# CONTAINS OBJECT: 'a16_rename'
load("Data/Derived/2016/anes-3-rename_scalevars.RData")

# LOAD OBJECTS THAT IDENTIFY SCALE ITEMS
source("DataClean/scale_items.R")

#=============================================
#'#  2. SCALES THAT APPEAR IDENTICAL IN 2012 & 2016
#=============================================

# NEW OBJECTS FOR CLEANED SCALES
a12_scales <- a12_r
a16_scales <- a16_rename



#' MOBILIZATION ----------------####
# ON 2016 POST:  MOBILPO_PTYMOB- MOBILPO_CTBOTH
# ON 2012 POST:  mobilpo_party - mobilpo_ctboth


a12_scales <- rescale_items(a12_scales,
                      all_vars =  mobil_vars,
                      reverse_vars = mobil_vars
                      )

a16_scales <- rescale_items(a16_scales,
                            all_vars =  mobil_vars,
                            reverse_vars = mobil_vars
)

check_scale_items(a12_scales, a16_scales, mobil_vars)



#'##  LIMITED GOVERNMENT ----------------####
# LAST ITEM is five-point scale, others are binary
# ON 2016 POST: LIMTGOV_GOVBIG, LIMTGOV_FREEMKT, LIMTGOV_LESSGOVT, LIMTGOV_REGBUS
# ON 2012 POST: govrole_big, govrole_market, govrole_lessmore, govrole_regbus
# ANSOLABEHERE, RODDEN, & SNYDER (APSR 2008) USE THREE BINARY ITEMS

a12_scales <- rescale_items(
  a12_scales,
  all_vars =  ltd_vars,
  reverse_vars = c("govrole_big",
                   "govrole_lessmore")
)

a16_scales <- rescale_items(
  a16_scales,
  all_vars =  ltd_vars,
  reverse_vars = c("govrole_big",
                   "govrole_lessmore")
)

check_scale_items(a12_scales, a16_scales, ltd_vars)




#'##  MORAL TRADITIONALISM ----------------####
# IN 2016 POST: TRAD_ADJMORAL, TRAD_LIFESTYL, TRAD_TOLERANT, TRAD_MORETRAD



a12_scales <- rescale_items(
  a12_scales,
  all_vars =  trad_vars,
  reverse_vars = c("trad_lifestyle",
                   "trad_famval")
)


a16_scales <- rescale_items(
  a16_scales,
  all_vars =  trad_vars,
  reverse_vars = c("trad_lifestyle",
                   "trad_famval")
)

check_scale_items(a12_scales, a16_scales, trad_vars)





#'##  RACIAL RESENTMENT  ----------------####
# IN 2016 POST: RESENT_WORKWAY, RESENT_GENRTNS, RESENT_DESERVE, RESENT_TRYHARD


a12_scales <- rescale_items(
  a12_scales,
  all_vars =  race_vars,
  reverse_vars = c("resent_workway", "resent_try")
)


a16_scales <- rescale_items(
  a16_scales,
  all_vars =  race_vars,
  reverse_vars = c("resent_workway", "resent_try")
  )

check_scale_items(a12_scales, a16_scales, race_vars)




#'##  AUTHORITARIANISM  ----------------####
# IN 2016 POST: AUTH_CHILDIND, AUTH_CHILDCUR, AUTH_CHILDOBED, AUTH_CHILDCONSID


# ASSIGN MISSING VALUES
a12_scales <- a12_scales  %>% mutate_each(
        funs(ifelse(. %in% c(seq(-9, -1, 1), 3, 4), NA, .)),
        one_of(auth_vars))
a16_scales <- a16_scales  %>% mutate_each(
  funs(ifelse(. %in% c(seq(-9, -1, 1), 3, 4), NA, .)),
  one_of(auth_vars))

# TREATS PEOPLE WHO SAY "BOTH" AS NA

# BINARIZE
a12_scales  <- a12_scales   %>%
  mutate(auth_ind = if_else(auth_ind == 2, 1, 0), missing = NA)  %>%
  mutate(auth_cur = if_else(auth_cur == 2, 1, 0), missing = NA)  %>%
  mutate(auth_obed = if_else(auth_obed == 1, 1, 0), missing = NA)  %>%
  mutate(auth_consid = if_else(auth_consid == 2, 1, 0), missing = NA)

a16_scales  <- a16_scales   %>%
  mutate(auth_ind = if_else(auth_ind == 2, 1, 0), missing = NA)  %>%
  mutate(auth_cur = if_else(auth_cur == 2, 1, 0), missing = NA)  %>%
  mutate(auth_obed = if_else(auth_obed == 1, 1, 0), missing = NA)  %>%
  mutate(auth_consid = if_else(auth_consid == 2, 1, 0), missing = NA)


check_scale_items(a12_scales, a16_scales, auth_vars)




#'##  TIPI  ----------------####
# IN 2016 POST: TIPI_TIPIEXTRA - TIPI_TIPICONV



# IDENTIFY VARIABLES
a12_scales  <- a12_scales  %>%
  mutate(extra_extraverted = tipi_extra) %>%
  mutate(agree_critical = tipi_crit) %>%
  mutate(conscien_dependable = tipi_dep) %>%
  mutate(emot_anxious = tipi_anx) %>%
  mutate(open_open = tipi_open) %>%
  mutate(extra_reserved = tipi_resv) %>%
  mutate(agree_sympathetic = tipi_warm) %>%
  mutate(conscien_disorganized = tipi_disorg) %>%
  mutate(emot_calm = tipi_calm) %>%
  mutate(open_conventional = tipi_conv)




a12_scales <- rescale_items(
  a12_scales,
  all_vars =  tipi_vars,
  reverse_vars = c(
    "agree_critical",
    "emot_anxious",
    "extra_reserved",
    "conscien_disorganized",
    "open_conventional")
)

a16_scales <- rescale_items(
  a16_scales,
  all_vars =  tipi_vars,
  reverse_vars = c(
    "agree_critical",
    "emot_anxious",
    "extra_reserved",
    "conscien_disorganized",
    "open_conventional"
  )
)




check_scale_items(a12_scales, a16_scales, tipi_vars)

# negative correlations found, but only between items from different traits



#'##  WORDSUM  ----------------####
# IN 2016 PRE: WORDSUM_SETB, WORDSUM_SETD, WORDSUM_SETE, WORDSUM_SETF,
#   WORDSUM_SETG, WORDSUM_SETH, WORDSUM_SETJ, WORDSUM_SETK, WORDSUM_SETL,
#   WORDSUM_SETO
# IN 2012 PRE: wordsum_setb - wordsum_seto


# ITEM NAME/ VALUE FOR CORRECT ANSWER
wordsum_items <- frame_data(
  ~item,          ~correct_val,
  "wordsum_setb", 5,
  "wordsum_setd", 3,
  "wordsum_sete", 1,
  "wordsum_setf", 3,
  "wordsum_setg", 5,
  "wordsum_seth", 4,
  "wordsum_setj", 1,
  "wordsum_setk", 1,
  "wordsum_setl", 4,
  "wordsum_seto", 2)





# BINARIZE (1 = CORRECT; 0 = INCORRECT)
a12_scales <- wordsum_items  %>%
  apply(1, function(x){
    if_else(a12_scales[, x["item"]] == x["correct_val"], 1, 0)
  })  %>%
  tbl_df()  %>%
  set_names(nm = paste0(wordsum_items$item, "_crt")) %>%
  bind_cols(a12_scales, .)  %>%
  # ASSIGN MISSING VALUES TO ANYONE WHO DID NOT RESPOND TO
  # FIVE OR MORE ITEMS (Following Maholtra et al)
  mutate(wordsum_misscount =
           rowSums(select(., one_of(wordsum_items$item)) < 0))  %>%
  mutate_each(funs(ifelse(wordsum_misscount >= 5, NA, .)),
        one_of(wordsum_vars))

a16_scales <- wordsum_items$item  %>%
  sapply(function(x){
    if_else(a16_scales[, x] == 1, 1, 0)
  })  %>%
  tbl_df()  %>%
  set_names(nm = wordsum_vars) %>%
  bind_cols(a16_scales, .)  %>%
  # ASSIGN MISSING VALUES TO ANYONE WHO DID NOT RESPOND TO
  # FIVE OR MORE ITEMS (Following Maholtra et al)
  mutate(wordsum_misscount =
           rowSums(select(., one_of(wordsum_items$item)) < 0))  %>%
  mutate_each(funs(ifelse(wordsum_misscount >= 5, NA, .)),
              one_of(wordsum_vars))


check_scale_items(a12_scales, a16_scales, wordsum_vars)


#=============================================
#'#  3. SCALES THAT APPEAR TO DIFFER IN 2012 & 2016
#=============================================

#'##  EGALITARIANISM  ----------------####
# IN 2016 POST: EGAL_DONECESS, EGAL_WORRYLESS, EGAL_NOTBIGPROB, EGAL_FEWERPROBS
# DIFFERENT NUMBER OF ITEMS in 2016 (N = 4) THAN IN 2012  (N = 6)
# IN 2012 POST: egal_equal, egal_toofar, egal_bigprob, egal_worryless,
#               egal_notbigprob, egal_fewerprobs
# ONLY USING FOUR FROM 2016, NOT EXTRA TWO FROM 2012


a12_scales <- rescale_items(
  a12_scales,
  all_vars = egal_vars,
  reverse_vars = c("egal_equal", "egal_fewerprobs")
)




a16_scales <- rescale_items(
  a16_scales,
  all_vars = egal_vars,
  reverse_vars = c("egal_equal", "egal_fewerprobs")
)

check_scale_items(a12_scales, a16_scales, egal_vars)




#'##  NEGATIVE BLACK STEREOTYPES  ----------------####

# ON 2016 POST: stype_hwkwhite - STYPEPO_VIOLASIAN
# ON 2016, USES HARDWORKING AND VIOLENT,
# BUT IN 2012, USES HARDWORKING AND INTELLIGENT
# ON 2012 POST: stype_hwkwhite - stype_intasian

a12_scales <- rescale_items(
  a12_scales,
  all_vars = stereo_vars,
  reverse_vars = NULL
  )

a16_scales <- rescale_items(
  a16_scales,
  all_vars = stereo_vars,
  reverse_vars = NULL
)


check_scale_items(a12_scales, a16_scales, stereo_vars)



#=============================================
#'#   4. SUBSET DATA
#=============================================

a12_scales <- a12_scales   %>%
  select(caseid,
         weight_full,
         strata_full,
         psu_full,
         one_of(group_vars),
         one_of(all_scale_items))

a16_scales <- a16_scales   %>%
  select(caseid = V160001,
         weight_full = V160102,
         strata_full = V160201,
         psu_full = V160202,
         one_of(group_vars),
         one_of(all_scale_items))  %>%
  # DROP CASES WITH ZERO WEIGHT
  filter(weight_full > 0)

# NOTE: the 2012 ANES public release provides all PSUs, but the 2016 ANES public
# release of the psu_full variable (V160202) does not provide the full PSU. As
# of February 2017, one must obtain the restricted data to access the PSUs.


#=============================================
#'#  5. SAVE
#=============================================

save(a12_scales, file = "Data/Derived/2012/groups_and_scales.RData")
save(a16_scales, file = "Data/Derived/2016/groups_and_scales.RData")



#=============================================
#'#  6. Display Session Info
#=============================================
sessionInfo()
