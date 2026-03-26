################################################################################
# ---
# title: "Replication script for 'From Respondents to Networks'"
# author: "Pietryka (mpietryka@fsu.edu)"
# date: "2017-06-24"
# ---

# PURPOSE
# This code produces the models presented in the article "From Respondents to
# Networks: Bridging between Individuals, Discussants, and the Network in the
# Study of Political Discussion," by Pietryka, Reilly, Maliniak, Miller,
# Huckfeldt,  and Rapoport, published in Political Behavior.
################################################################################


# =============================================
#  PREAMBLE ===================================
# =============================================

# ----------------- LOAD PACKAGES ----------------

# DATA CLEANING FUNCTIONS (FUNCTIONS USED: 'select' '%>%')
library(dplyr)

# READ IN CSVs (FUNCTION USED:'read_csv')
library(readr)

# FOR ORDERED LOGITS (FUNCTIONS USED: 'lrm', rms::robcov')
library(rms)

# DISPLAY FORMATTED RESULTS (FUNCTION USED: 'screenreg')
library(texreg)

# DISPLAY VERSION NUMBERS FOR R & PACKAGES IN USE
sessionInfo()

# ----------------- FILE PATHS ----------------

# INSERT THE PATH TO THE DATA FOLDER ON YOUR COMPUTER
data_path <- ""

# ----------------- LOAD DATA ----------------

# LOAD DATA USED FOR TABLES 1, 3, AND 4
indivlevel_df <- readr::read_csv(paste0(data_path, "individual-level_data.csv"))

# LOAD DATA USED FOR TABLE 2
dyads_df      <- readr::read_csv(paste0(data_path, "dyadic_data.csv"))


# =============================================
#  Table 1  ===================================
# =============================================



# Subset data to include non-missing for Model 2 --------------------
table1_df <- indivlevel_df  %>%
  dplyr::select(noise_awareness,
         attended_party,
         academic_year,
         nodes,
         edges,
         indegree,
         local_centrality)  %>%
  na.omit()


# TABLE 1, MODEL 1 --------------------
table1_mod1 <- rms::lrm(
  noise_awareness ~
    attended_party +
    academic_year,
  data = table1_df,
  x = TRUE,
  y = TRUE)


# TABLE 1, MODEL 2 --------------------
table1_mod2 <- rms::lrm(
  noise_awareness ~
    attended_party +
    academic_year +
    nodes +
    edges +
    indegree +
    local_centrality,
  data = table1_df,
  x = TRUE,
  y = TRUE
)

# DISPLAY TABLE
texreg::screenreg(list(table1_mod1, table1_mod2), stars = .05)


# =============================================
#  Table 2  ===================================
# =============================================

# TABLE 2, MODEL 1 --------------------
table2_mod1 <-  rms::lrm(
  noise_awareness ~
    ego_attended_party +
    ego_academic_year +
    alter_attended_party +
    nodes +
    edges +
    indegree  +
    local_centrality,
  data = dyads_df,
  x = TRUE,
  y = TRUE
)
# Huber-White cluster robust errors
table2_mod1_rms <- rms::robcov(table2_mod1, dyads_df$ego_id)


# TABLE 2, MODEL 2 --------------------
table2_mod2 <-  rms::lrm(
  noise_awareness ~
    ego_attended_party +
    ego_academic_year +
    alter_attended_party  +
    nodes +
    edges  +
    indegree  +
    local_centrality +
    alter_attended_party * nodes +
    alter_attended_party * edges,
  data = dyads_df,
  x = TRUE,
  y = TRUE
)
# Huber-White cluster robust errors
table2_mod2_rms <- rms::robcov(table2_mod2, dyads_df$ego_id)


# DISPLAY TABLE
texreg::screenreg(list(table2_mod1_rms, table2_mod2_rms), stars = .05)




# =============================================
#  Table 3  ===================================
# =============================================

# Subset data to include non-missing for Model 3 --------------------
table3_df <- indivlevel_df  %>%
  dplyr::select(noise_awareness,
         attended_party,
         academic_year,
         nodes,
         edges,
         indegree,
         local_centrality,
         zone1_attended_party,
         zone2_attended_party)  %>%
  na.omit()

# TABLE 3, MODEL 1 --------------------
table3_mod1 <- rms::lrm(
  noise_awareness ~
    attended_party +
    academic_year +
    zone1_attended_party,
  data = table3_df,
  x = TRUE,
  y = TRUE
)

# TABLE 3, MODEL 2 --------------------
table3_mod2 <-  rms::lrm(
  noise_awareness ~
    attended_party +
    academic_year +
    zone1_attended_party +
    zone2_attended_party,
  data = table3_df,
  x = TRUE,
  y = TRUE
)

# TABLE 3, MODEL 3 --------------------
table3_mod3 <-  rms::lrm(
  noise_awareness ~
    attended_party +
    academic_year +
    nodes +
    edges  +
    indegree +
    local_centrality +
    zone1_attended_party +
    zone2_attended_party,
  data = table3_df,
  x = TRUE,
  y = TRUE
)


# DISPLAY TABLE
texreg::screenreg(list(table3_mod1, table3_mod2, table3_mod3), stars = .05)



# =============================================
#  Table 4  ===================================
# =============================================

# Subset data to include non-missing for Model 3 --------------------
table4_df <- indivlevel_df  %>%
  dplyr::select(
    friend1,
    validated_turnout_2010,
    noise_awareness,
    attended_party,
    academic_year,
    national_interest,
    validated_turnout_2009,
    family_economic_status,
    white,
    black,
    female,
    zone1_validated_turnout_2010,
    zone2_validated_turnout_2010,
    nodes,
    edges,
    indegree,
    local_centrality
  )  %>%
  na.omit()


# TABLE 4, MODEL 1 --------------------

table4_mod1 <-  glm(
  validated_turnout_2010 ~
    noise_awareness +
    attended_party +
    academic_year +
    national_interest +
    validated_turnout_2009 +
    family_economic_status +
    white +
    black +
    female,
  family = "binomial",
  data = table4_df
)

# TABLE 4, MODEL 2 --------------------

table4_mod2 <-  glm(
  validated_turnout_2010 ~
    noise_awareness +
    attended_party +
    academic_year +
    national_interest +
    validated_turnout_2009 +
    family_economic_status +
    white +
    black +
    female +
    zone1_validated_turnout_2010,
  family = "binomial",
  data = table4_df
)


# TABLE 4, MODEL 3 --------------------

table4_mod3 <-  glm(
  validated_turnout_2010 ~
    noise_awareness +
    attended_party +
    academic_year +
    national_interest +
    validated_turnout_2009 +
    family_economic_status +
    white +
    black +
    female +
    zone1_validated_turnout_2010 +
    zone2_validated_turnout_2010 +
    nodes +
    edges +
    indegree +
    local_centrality,
  family = "binomial",
  data = table4_df
)

# DISPLAY TABLE
texreg::screenreg(list(table4_mod1, table4_mod2, table4_mod3), stars = .05)


# TABLE 4, ANALYSIS OF DEVIANCE  --------------------

# COMPARE MODELS 1 & 2
m1m2_test <- anova(table4_mod1, table4_mod2, test = "Chisq")
m1m2_test

# COMPARE MODELS 2 & 3
m2m3_test <- anova(table4_mod2, table4_mod3, test = "Chisq")
m2m3_test



