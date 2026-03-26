
# FUNCTION TO GENERATE GROUPING VARIABLES


gen_groupvars <- function(data,
          gender_var, # NAME OF GENDER VAR
            gender_codes, # RECODE SCHEME FOR GENDER FROM sjmisc::rec
          pid_var,
            pid_codes,
          libcon_var,
            libcon_codes,
          turnout_var,
             turnout_codes,
          race_var,
            race_codes,
          educ_var,
            educ_codes,
          age_var,
            age_codes,
          income_var,
            income_codes,
          votechoice_var,
            votechoice_codes,
          mode_var,
            mode_codes
          ){

data_recode <-  data  %>%

# ------- GENDER  ---------------- #
# USE SUMMARY MEASURE, 1 = FEMALE; 0 = MALE;
# no missing
rename_("gender_var" = gender_var)  %>%
mutate(female = rec(gender_var,
        rec = gender_codes,
        var.label = "gender (derived from 'gender_respondent_x')",
        val.labels = c("male", "female")
        ))  %>%


# ------- PARTY ID  ---------------- #
# USE SUMMARY MEASURE, -1 = Democrat; 0 = Independent; 1 = Republican
# ALL ELSE MISSING
rename_("pid_var" = pid_var)  %>%
mutate(pid3 = rec(pid_var,
      rec = pid_codes,
      var.label = "Party ID, 3pt (derived from 'pid_x')",
      val.labels = c("Dems/leaners", "Ind", "Reps/leaners")
      ))  %>%

# ------- IDEOLOGY  ---------------- #
# USE PRE-ELECTION MEASURE, -1 = Democrat; 0 = Independent; 1 = Republican
# ALL ELSE MISSING
rename_("libcon_var" = libcon_var)  %>%
mutate(libcon3 = rec(libcon_var,
      rec = libcon_codes,
      var.label = "Liberal/conservative, 3pt (derived from 'libcpre_self')",
      val.labels = c("liberal", "moderate", "conservative")
      ))  %>%

# ------- TURNOUT  ---------------- #
# POST-ELECTION MEASURE, 1 = VOTED; 0 = DID NOT
# ALL ELSE MISSING
rename_("turnout_var" = turnout_var)  %>%
mutate(turnout = rec( turnout_var,
      rec = turnout_codes,
      var.label = "Did R vote in Nov GE? (derived from 'rvote2012_x')",
      val.labels = c("did not vote", "voted")
      ))  %>%

# ------- RACE  ---------------- #
# PRE-ELECTION MEASURE, CODEBOOK DOES NOT MATCH VALUE LABELS IN RAW DATA
# 0 = "White non-Hispanic",
# 1 = "Black non-Hispanic",
# 2 = "Hispanic",
# 3 = "Other non-Hispanic"
# ALL ELSE MISSING

rename_("race_var" = race_var)  %>%
mutate(race_nominal = rec(race_var,
      rec = race_codes,
      var.label = "R race/ethnicity  (derived from 'dem_raceeth_x')",
      val.labels = c("White non-Hispanic",
                     "Black non-Hispanic",
                     "Hispanic",
                     "Other non-Hispanic")
      ))  %>%


# ------- EDUCATION  ---------------- #
# PRE-ELECTION MEASURES
# ONE ORDINAL, ONE BINARY

# ORDINAL
rename_("educ_var" = educ_var)  %>%
mutate(educ_ordinal = rec(educ_var,
      rec = educ_codes,
      var.label = "R race/ethnicity  (derived from 'dem_edugroup_x')",
      val.labels = c("< high school",
                     "High school",
                     "Some post-high-school",
                     "Bachelor's degree",
                     "Graduate degree")
      ))  %>%


# ------- AGE  ---------------- #
# USE PRE-ELECTION MEASURE,
# CONVERT TO THREE GROUPS BASED ROUGHLY ON 33 and 66th percentiles
# 0 = (< 40); 1 = (>= 40 & < 60); 2 = (>= 60)
# ALL ELSE MISSING

rename_("age_var" = age_var)  %>%
mutate(age = rec(age_var,
      rec = age_codes,
      var.label = "Age on pre-election interview (derived from 'dem_age_r_x')",
      val.labels = c("R < 40 years",
                     "R >= 40 & < 60",
                     "R >= 60")
      ))  %>%

# ------- INCOME  ---------------- #
# USE PRE-ELECTION MEASURE,
# CONVERT TO THREE GROUPS BASED ROUGHLY ON 33 and 66th percentiles
# WOULD USE 65k INSTEAD OF 60k, but THAT CUTPOINT NOT AVAILABLE in 2008
# 0 = (< 25k); 1 = (>= 25k & < 60k); 2 = (>= 60k)
# ALL ELSE MISSING

rename_("income_var" = income_var)  %>%
mutate(income = rec(income_var,
      rec = income_codes,
      var.label = "Income on pre-election (derived from 'inc_incgroup_pre')",
      val.labels = c("R < $25k",
                     "R >= $25k & < $65k",
                     "R >= $65k")
      ))  %>%


# ------- CANDIDATE CHOICE  ---------------- #
#  POST-ELECTION SUMMARY MEASURE
# 0 = DEM CAND; 1 = REP CAND;
# ALL ELSE MISSING

rename_("votechoice_var" = votechoice_var)  %>%
mutate(votechoice = rec(votechoice_var,
      rec = votechoice_codes,
      var.label = "For whom did R vote for President in 2012 (derived from 'presvote2012_x')",
      val.labels = c("Dem. Cand.", "Rep. Cand.")
      ))


# EVALUATE IF 'mode_var' NOT NULL
if(is.null(mode_var) == FALSE){
data_recode <-  data_recode  %>%
  # ------- SURVEY MODE  ---------------- #
  # USE SUMMARY MEASURE, 1 = internet; 0 = face to face;
  # no missing
  # NOT AVAILABLE IN 2008 DATA
  rename_("mode_var" = mode_var)  %>%
  mutate(internet = rec(mode_var,
        rec = mode_codes,
        var.label = "mode of interview (derived from 'mode')",
        val.labels = c("face-to-face", "Internet")
        ))
}


return(data_recode)
}

