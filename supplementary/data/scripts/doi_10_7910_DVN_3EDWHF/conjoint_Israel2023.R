#### conjoint analysis for Israel 2023: replication code 20260302 ###

#### Loading packages ####
library(cjoint)
library(tidyverse)

#### Loading data ####
conjoint_data <- read.qualtrics("IsraelSurvey2023Populismdata_new.csv",responses= NULL, covariates= c("religiosity", "Likud","Bibi","ideology","pop_z"),
                                respondentID="userID",
                                ranks=c("Q21_1", "Q22_1", "Q23_1", "Q24_1", "Q25_1" , "Q26_1", "Q27_1", "Q21_2", "Q22_2", "Q23_2", "Q24_2", "Q25_2" , "Q26_2", "Q27_2"))

####  Translation  ####
Israelpolicy <- conjoint_data %>% 
  select(!c(contains("rowpos")))

Israelpolicy <- Israelpolicy %>%
  mutate(
    Antijew = case_when(
      antijew == "תומכת בהענשתם"~"punish",
      antijew == "תמשיך במדיניות הקיימת"~"leave"
    )
  )

Israelpolicy <- Israelpolicy %>%
  mutate(
    Diplomacy = case_when(
      arabs == "תרחיב"~"expand",
      arabs == "לא תרחיב"~"not expand"
    )
  )

Israelpolicy <- Israelpolicy %>%
  mutate(
    Harediwork = case_when(
      haredi == "תומכת"~"support",
      haredi == "לא תומכת"~"not support"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Trade = case_when(
      imex == "תומכת"~"support",
      imex == "מתנגדת"~"oppose"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Inequality = case_when(
      inequality == "תשלם"~"subsiding",
      inequality == "לא תשלם"~"nonsubsiding"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Judicialreform = case_when(
      judicialreform == "תומכת"~"support",
      judicialreform == "מתנגדת"~"oppose"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Jewishmajority = case_when(
      jewmajority == "תחקוק"~"agree",
      jewmajority == "לא תחקוק"~"disagree"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    LGBT = case_when(
      lgbt == "תגביר"~"increase",
      lgbt == "תצמצם"~"decrease",
      lgbt == "תשאיר כמו שהוא"~"leave"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    Settlement = case_when(
      settlement == "תגביר"~"increase",
      settlement == "תצמצם"~"decrease",
      settlement == "תשאיר כמו שהיא"~"leave"
    )
  )
Israelpolicy <- Israelpolicy %>%
  mutate(
    PA = case_when(
      pa == "תפתח מחדש"~"restart",
      pa == "לא תפתח"~"not restart"
    )
  )

#### Convert the variables to factors ####
Israelpolicy$Antijew <- as.factor(Israelpolicy$Antijew)
Israelpolicy$Diplomacy <- as.factor(Israelpolicy$Diplomacy)
Israelpolicy$Harediwork <- as.factor(Israelpolicy$Harediwork)
Israelpolicy$Trade <- as.factor(Israelpolicy$Trade)
Israelpolicy$Inequality <- as.factor(Israelpolicy$Inequality)
Israelpolicy$Jewishmajority <- as.factor(Israelpolicy$Jewishmajority)
Israelpolicy$Judicialreform <- as.factor(Israelpolicy$Judicialreform)
Israelpolicy$LGBT <- as.factor(Israelpolicy$LGBT)
Israelpolicy$PA <- as.factor(Israelpolicy$PA)
Israelpolicy$Settlement <- as.factor(Israelpolicy$Settlement)

#### Putting labels ####
attr_list <- list()             
attr_list[["Antijew"]] <- c("punish","leave")

attr_list[["Diplomacy"]] <- c("expand", "not expand")

attr_list[["Harediwork"]] <- c("support","not support")

attr_list[["Trade"]] <- c("support","oppose") 

attr_list[["Inequality"]] <- c("subsiding", "nonsubsiding")

attr_list[["Judicialreform"]] <- c("support","oppose")

attr_list[["Jewishmajority"]] <- c("agree","disagree")

attr_list[["LGBT"]] <- c("increase","leave","decrease")

attr_list[["PA"]] <- c("restart", "not restart") 

attr_list[["Settlement"]] <- c("increase", "leave", "decrease")


#### Conjoint analysis ####
cjoint_design <- makeDesign(type = "constraints",
                            attribute.levels = attr_list)


cjoint_pool <- cjoint::amce(selected ~ Antijew + Diplomacy + Harediwork + Trade + Inequality + Judicialreform + Jewishmajority + LGBT + PA + Settlement, 
                    data=Israelpolicy,
                    respondent.id= "respondentIndex",
                    cluster = TRUE,
                    na.ignore=TRUE)

#### Plot of conjoint analysis ####
library(extrafont)

plot(cjoint_pool, point.size = 1.2, dodge.size = 1.2, text.size = 18
)

summary(cjoint_pool)

####################### Pro Bibi vs Anti Bibi #############################

antibibi <- filter(Israelpolicy, Bibi <= 49)

cjoint_antibibi <- cjoint::amce(selected ~ Antijew + Diplomacy + Harediwork + Trade + Inequality + Judicialreform + Jewishmajority + LGBT + PA + Settlement, 
                        data=antibibi,
                        respondent.id= "respondentIndex",
                        cluster = TRUE,
                        na.ignore=TRUE)

plot(cjoint_antibibi, main="Anti-Netanyahu", point.size = 1.2, dodge.size = 1.2, text.size = 18)

summary(cjoint_antibibi)


probibi <- filter(Israelpolicy, Bibi >= 51)

cjoint_probibi <- cjoint::amce(selected ~ Antijew + Diplomacy + Harediwork + Trade + Inequality + Judicialreform + Jewishmajority + LGBT + PA + Settlement,
                       data=probibi,
                       respondent.id= "respondentIndex",
                       cluster = TRUE,
                       na.ignore=TRUE)

plot(cjoint_probibi, main="Pro-Netayahu", point.size = 1.2, dodge.size = 1.2, text.size = 18)

summary(cjoint_probibi)

write.csv(Israelpolicy, "Israelpolicy.csv", row.names=FALSE)


### difference in AMCE test
# =========================
# 0. Packages
# =========================
library(dplyr)
library(sandwich)
library(lmtest)
library(broom)
library(stringr)
library(tibble)

# =========================
# 1. Data preparation
# =========================
# df: conjoint long data
# each row = one profile evaluated by one respondent
#
# REQUIRED VARIABLES:
# respondent_id       : respondent ID
# choice              : outcome variable (e.g., vote likelihood)
# netanyahu_thermo    : 0-100 feeling thermometer
#
# conjoint attributes:
# Antijew
# Diplomacy
# Harediwork
# Inequality
# Jewishmajority
# Judicialreform
# LGBT
# PA
# Settlement
# Trade

# ---- Pro-Netanyahu dummy ----
df <- df %>%
  mutate(
    pro_bibi = ifelse(netanyahu_thermo >= 51, 1, 0)
  )

# ---- Relevel factors so that baselines match your tables ----
df <- Israelpolicy %>%
  mutate(
    Antijew        = factor(Antijew),
    Diplomacy      = factor(Diplomacy),
    Harediwork     = factor(Harediwork),
    Inequality     = factor(Inequality),
    Jewishmajority = factor(Jewishmajority),
    Judicialreform = factor(Judicialreform),
    LGBT           = factor(LGBT),
    PA             = factor(PA),
    Settlement     = factor(Settlement),
    Trade          = factor(Trade)
  )

# Baselines
# Change these labels only if your actual factor labels differ
df$Antijew        <- relevel(df$Antijew, ref = "leave")
df$Diplomacy      <- relevel(df$Diplomacy, ref = "expand")
df$Harediwork     <- relevel(df$Harediwork, ref = "not support")
df$Inequality     <- relevel(df$Inequality, ref = "nonsubsiding")
df$Jewishmajority <- relevel(df$Jewishmajority, ref = "agree")
df$Judicialreform <- relevel(df$Judicialreform, ref = "oppose")
df$LGBT           <- relevel(df$LGBT, ref = "decrease")
df$PA             <- relevel(df$PA, ref = "not restart")
df$Settlement     <- relevel(df$Settlement, ref = "decrease")
df$Trade          <- relevel(df$Trade, ref = "oppose")

# pro_bibi as factor
df$pro_bibi <- factor(
  ifelse(df$Bibi >= 51, 1, 0),
  levels = c(0, 1),
  labels = c("Anti", "Pro")
)

# =========================
# 2. Interaction model
# =========================
# Main effects = Anti-Bibi AMCEs
# Interaction terms = difference between Pro-Bibi and Anti-Bibi
# Pro-Bibi AMCE = main effect + interaction effect

fit <- lm(
  selected ~
    Antijew * pro_bibi +
    Diplomacy * pro_bibi +
    Harediwork * pro_bibi +
    Inequality * pro_bibi +
    Jewishmajority * pro_bibi +
    Judicialreform * pro_bibi +
    LGBT * pro_bibi +
    PA * pro_bibi +
    Settlement * pro_bibi +
    Trade * pro_bibi,
  data = df
)

# Cluster-robust SE by respondent
VcovCL <- vcovCL(fit, cluster = ~ respondentIndex, type = "HC1")

# coefficient table
coeftab <- coeftest(fit, vcov. = VcovCL)
coef_df <- tidy(coeftab) %>%
  rename(
    Estimate = estimate,
    SE = std.error,
    z = statistic,
    p = p.value
  )

print(coef_df)

# =========================
# 3. Create AMCE comparison table
# =========================
# For each non-baseline level:
# Anti-Bibi AMCE = main effect
# Difference      = interaction term
# Pro-Bibi AMCE   = main effect + interaction
#
# IMPORTANT:
# The regex below assumes standard coefficient names from lm()
# If your factor labels contain spaces/punctuation, it still usually works,
# but if needed, inspect rownames(coeftab).

# Mapping of attributes and non-baseline levels
attr_levels <- tribble(
  ~Attribute,        ~Level,                    ~main_term,
  "Antijew",         "punish",                  "Antijewpunish",
  "Diplomacy",       "not expand",              "Diplomacynot expand",
  "Harediwork",      "support",                 "Harediworksupport",
  "Inequality",      "subsiding",               "Inequalitysubsiding",
  "Jewishmajority",  "disagree",                "Jewishmajoritydisagree",
  "Judicialreform",  "support",                 "Judicialreformsupport",
  "LGBT",            "increase",                "LGBTincrease",
  "LGBT",            "leave",                   "LGBTleave",
  "PA",              "restart",                 "PArestart",
  "Settlement",      "increase",                "Settlementincrease",
  "Settlement",      "leave",                   "Settlementleave",
  "Trade",           "support",                 "Tradesupport"
)

# Function to extract linear combination with robust SE
lincom <- function(beta_names, weights, coef_vec, vcov_mat) {
  b <- coef_vec[beta_names]
  V <- vcov_mat[beta_names, beta_names, drop = FALSE]
  est <- sum(weights * b)
  se  <- sqrt(as.numeric(t(weights) %*% V %*% weights))
  z   <- est / se
  p   <- 2 * pnorm(-abs(z))
  c(Estimate = est, SE = se, z = z, p = p)
}

coef_vec <- coef(fit)

results_list <- vector("list", nrow(attr_levels))

for (i in seq_len(nrow(attr_levels))) {
  main_term <- attr_levels$main_term[i]
  int_term  <- paste0(main_term, ":pro_bibiPro")
  
  # If R orders the interaction term the other way, use this fallback
  if (!(int_term %in% names(coef_vec))) {
    int_term <- paste0("pro_bibiPro:", main_term)
  }
  
  # Anti-Bibi = main effect
  anti_res <- lincom(
    beta_names = c(main_term),
    weights    = c(1),
    coef_vec   = coef_vec,
    vcov_mat   = VcovCL
  )
  
  # Difference = interaction effect
  diff_res <- lincom(
    beta_names = c(int_term),
    weights    = c(1),
    coef_vec   = coef_vec,
    vcov_mat   = VcovCL
  )
  
  # Pro-Bibi = main + interaction
  pro_res <- lincom(
    beta_names = c(main_term, int_term),
    weights    = c(1, 1),
    coef_vec   = coef_vec,
    vcov_mat   = VcovCL
  )
  
  results_list[[i]] <- tibble(
    Attribute   = attr_levels$Attribute[i],
    Level       = attr_levels$Level[i],
    
    Anti_Est    = anti_res["Estimate"],
    Anti_SE     = anti_res["SE"],
    Anti_p      = anti_res["p"],
    
    Pro_Est     = pro_res["Estimate"],
    Pro_SE      = pro_res["SE"],
    Pro_p       = pro_res["p"],
    
    Diff_Est    = diff_res["Estimate"],
    Diff_SE     = diff_res["SE"],
    Diff_p      = diff_res["p"]
  )
}

amce_diff_table <- bind_rows(results_list)

print(amce_diff_table)

# =========================
# 4. Add significance stars
# =========================
stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
}

amce_diff_table <- amce_diff_table %>%
  mutate(
    Anti_sig = stars(Anti_p),
    Pro_sig  = stars(Pro_p),
    Diff_sig = stars(Diff_p)
  )

print(amce_diff_table)

# =========================
# 5. Judicial reform only
# =========================
judicial_test <- amce_diff_table %>%
  filter(Attribute == "Judicialreform", Level == "support")

print(judicial_test)

# =========================
# 6. Nice formatted output
# =========================
amce_diff_table_fmt <- amce_diff_table %>%
  mutate(
    Anti_AMCE = sprintf("%.6f%s (%.6f)", Anti_Est, Anti_sig, Anti_SE),
    Pro_AMCE  = sprintf("%.6f%s (%.6f)", Pro_Est, Pro_sig, Pro_SE),
    Diff_AMCE = sprintf("%.6f%s (%.6f)", Diff_Est, Diff_sig, Diff_SE)
  ) %>%
  select(Attribute, Level, Anti_AMCE, Pro_AMCE, Diff_AMCE, Diff_p)

print(amce_diff_table_fmt)

# write.csv(amce_diff_table_fmt, "amce_difference_test.csv", row.names = FALSE)

