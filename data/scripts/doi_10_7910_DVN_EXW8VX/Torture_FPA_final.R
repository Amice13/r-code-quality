# =============================================================================
# Humanitarian Concerns and Acceptance of Syrian Refugees in Turkey - 
# REPLICATION ANALYSIS
# =============================================================================

# -----------------------------------------------------------------------------
# PACKAGE INSTALLATION (run once, then comment out)
# -----------------------------------------------------------------------------
# install.packages(c(sandwich", "ggplot2", "cjoint", "dplyr", 
#                    "gridExtra", "ggplotify", "psych", "readr", 
#                    "conflicted", "ggtext", "ggh4x"))

# -----------------------------------------------------------------------------
# LOAD PACKAGES
# -----------------------------------------------------------------------------
library(sandwich)
library(ggplot2)
library(cjoint)
library(dplyr)
library(gridExtra)
library(grid)
library(ggplotify)
library(psych)
library(readr)
library(conflicted)
library(ggtext)
library(ggh4x)

# Clear workspace
rm(list = ls())

# -----------------------------------------------------------------------------
# LOAD DATA -- set working directory and if necessary the path to the data file
# -----------------------------------------------------------------------------
setwd(" ")
data <- read_csv("ProfileDataset_replication.csv")

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# LOG FILE
# -----------------------------------------------------------------------------
#sink("Torture_FPA_Replication.txt", split = TRUE)  # split=TRUE shows output in console AND saves to file


# RECODE RESPONDENT VARIABLES
# -----------------------------------------------------------------------------

# Recode age into categories
data$respondent_age2 <- ifelse(
  data$respondent_age >= 18 & data$respondent_age <= 30, 1,
  ifelse(data$respondent_age > 30 & data$respondent_age <= 50, 2, 3)
)

# Recode education into categories
data$respondent_education2 <- ifelse(
  data$respondent_education == 9, 1,
  ifelse(data$respondent_education == 8, 2,
         ifelse(data$respondent_education == 7, 3,
                ifelse(data$respondent_education %in% c(5, 6), 4, 5)))
)

# Create contact indices
data$contact_pos <- (data$conversations + data$shopped + 
                       data$friends + data$dated) / 4
data$contact_neg <- (data$argument + data$walkingroups + 
                       data$cheated) / 3

summary(data$contact_pos)
summary(data$contact_neg)

# -----------------------------------------------------------------------------
# CONVERT VARIABLES TO FACTORS
# -----------------------------------------------------------------------------

# Profile characteristics
profile_vars <- c("female", "age", "education", "ethnicity", "religion", 
                  "fighter", "turkishfriends", "knowsturkish", "tortured")
data[profile_vars] <- lapply(data[profile_vars], as.factor)

# Respondent characteristics
respondent_vars <- c("respondent_female", "respondent_party", "respondent_age2", 
                     "respondent_education2", "respondent_kurd_lang", 
                     "respondent_arab_lang", "coverhair", "pray", "world_citizen",
                     "proud_turkish", "mem_local_community", "mem_muslim_community",
                     "mem_turkish_nation", "auto_individual")
data[respondent_vars] <- lapply(data[respondent_vars], as.factor)

# Contact variables
contact_vars <- c("conversations", "shopped", "friends", "dated", 
                  "argument", "walkingroups", "cheated", 
                  "contact_pos", "contact_neg")
data[contact_vars] <- lapply(data[contact_vars], as.factor)

# -----------------------------------------------------------------------------
# RENAME VARIABLES
# -----------------------------------------------------------------------------

# Profile variable names
profile_names <- c(
  "female" = "Gender",
  "age" = "Age",
  "education" = "Education",
  "ethnicity" = "Ethnicity",
  "religion" = "Religion",
  "fighter" = "Fighter",
  "turkishfriends" = "Local friends",
  "knowsturkish" = "Knows Turkish",
  "tortured" = "Tortured"
)

# Respondent variable names
respondent_names <- c(
  "respondent_female" = "Respondent gender",
  "respondent_age" = "Respondent age cont.",
  "respondent_age2" = "Respondent age",
  "respondent_education2" = "Respondent education",
  "respondent_party" = "Respondent party",
  "coverhair" = "Hair cover",
  "pray" = "Pray",
  "respondent_arab_lang" = "Arab respondent",
  "respondent_kurd_lang" = "Kurdish respondent",
  "respondent_religion" = "Respondent religion"
)

# Contact variable names
contact_names <- c(
  "conversations" = "Conversations",
  "shopped" = "Shopped",
  "friends" = "Friends",
  "dated" = "Dated",
  "argument" = "Argument",
  "walkingroups" = "Walkingroups",
  "cheated" = "Cheated",
  "contact_pos" = "Positive contact",
  "contact_neg" = "Negative contact"
)

# Apply all name changes
names(data)[match(names(profile_names), names(data))] <- profile_names
names(data)[match(names(respondent_names), names(data))] <- respondent_names
names(data)[match(names(contact_names), names(data))] <- contact_names

# -----------------------------------------------------------------------------
# DEFINE FACTOR LEVELS
# -----------------------------------------------------------------------------

# Profile characteristics
data$Gender <- factor(data$Gender, 
                      levels = c(0, 1), 
                      labels = c("Male", "Female"))

data$Age <- factor(data$Age, 
                   levels = c(1, 2, 3), 
                   labels = c("18-30", "31-50", ">51"))

data$Ethnicity <- factor(data$Ethnicity, 
                         levels = c(1, 2, 3), 
                         labels = c("Turkoman", "Kurd", "Arab"))

data$Religion <- factor(data$Religion, 
                        levels = c(1, 2, 3), 
                        labels = c("Sunni", "Alawite", "Christian"))

data$Education <- factor(data$Education, 
                         levels = c(1, 2, 3, 4, 5), 
                         labels = c("None", "Primary", "Middle", "High", "University"))

data$`Local friends` <- factor(data$`Local friends`, 
                               levels = c(0, 1), 
                               labels = c("No", "Yes"))

data$`Knows Turkish` <- factor(data$`Knows Turkish`, 
                               levels = c(0, 1), 
                               labels = c("No", "Yes"))

data$Fighter <- factor(data$Fighter, 
                       levels = c(1, 2, 3), 
                       labels = c("No", "FSA", "Asad"))

data$Tortured <- factor(data$Tortured, 
                        levels = c(0, 1), 
                        labels = c("No", "Yes"))

# Respondent characteristics
data$`Respondent gender` <- factor(data$`Respondent gender`, 
                                   levels = c(0, 1), 
                                   labels = c("Male", "Female"))

data$`Respondent age` <- factor(data$`Respondent age`, 
                                levels = c(1, 2, 3), 
                                labels = c("18-30", "31-50", ">50"))

data$`Respondent education` <- factor(data$`Respondent education`, 
                                      levels = c(1, 2, 3, 4, 5), 
                                      labels = c("None", "Primary", "Middle", "High", "University"))

data$`Respondent party` <- factor(data$`Respondent party`, 
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8), 
                                  labels = c("AKP", "CHP", "MHP", "HDP", 
                                             "Iyi Parti", "Other", "Didn't vote", "No answer"))

data$`Respondent religion` <- factor(data$`Respondent religion`, 
                                     levels = c(1, 2, 3, 4, 5), 
                                     labels = c("None", "Islam", "Christianity", "Judaism", "Other"))

data$`Arab respondent` <- factor(data$`Arab respondent`, 
                                 levels = c(0, 1), 
                                 labels = c("Non-Arab", "Arab"))

data$`Kurdish respondent` <- factor(data$`Kurdish respondent`, 
                                    levels = c(0, 1), 
                                    labels = c("Non-Kurdish", "Kurdish"))

# Contact variables
yes_no_vars <- c("Conversations", "Shopped", "Friends", "Dated", 
                 "Argument", "Walkingroups", "Cheated")

for (var in yes_no_vars) {
  data[[var]] <- factor(data[[var]], 
                        levels = c(0, 1), 
                        labels = c("No", "Yes"))
}

data$`Positive contact` <- factor(data$`Positive contact`, 
                                  levels = c(0, 0.25, 0.5, 0.75, 1), 
                                  labels = c("None", "One", "Two", "Three", "Four"))

data$`Negative contact` <- factor(data$`Negative contact`, 
                                  levels = c(0, 0.333333333333333, 0.666666666666667, 1), 
                                  labels = c("None", "One", "Two", "Three"))

# -----------------------------------------------------------------------------
# DEFINE CONJOINT DESIGN
# -----------------------------------------------------------------------------

# Define attribute levels
levels.test <- list(
  "Gender" = c("Male", "Female"),
  "Age" = c("18-30", "31-50", ">51"),
  "Ethnicity" = c("Turkoman", "Kurd", "Arab"),
  "Religion" = c("Sunni", "Alawite", "Christian"),
  "Education" = c("None", "Primary", "Middle", "High", "University"),
  "Local friends" = c("No", "Yes"),
  "Knows Turkish" = c("No", "Yes"),
  "Fighter" = c("No", "FSA", "Asad"),
  "Tortured" = c("No", "Yes"),
  "Respondent gender" = c("Male", "Female"),
  "Respondent age" = c("18-30", "31-50", ">50"),
  "Respondent education" = c("None", "Primary", "Middle", "High", "University"),
  "Respondent party" = c("AKP", "CHP", "MHP", "HDP", "Iyi Parti", 
                         "Other", "Didn't vote", "No answer"),
  "Positive contact" = c("None", "One", "Two", "Three", "Four"),
  "Negative contact" = c("None", "One", "Two", "Three")
)

# Define constraints (empty list - no constraints)
constraint_list <- list()

# Define marginal weights
marginal_weights <- list()

# Uniform weights for most attributes
uniform_attrs <- c("Gender", "Age", "Ethnicity", "Religion", "Education", 
                   "Local friends", "Knows Turkish", "Fighter", "Tortured",
                   "Respondent party", "Positive contact", "Negative contact")

for (attr in uniform_attrs) {
  n_levels <- length(levels.test[[attr]])
  marginal_weights[[attr]] <- rep(1/n_levels, n_levels)
}

# Custom weights for specific respondent characteristics
marginal_weights[["Respondent gender"]] <- c(0.5, 0.5)
marginal_weights[["Respondent age"]] <- c(0.28, 0.40, 0.32)
marginal_weights[["Respondent education"]] <- c(0.11, 0.26, 0.26, 0.21, 0.16)

# Create designs
torturedesign_reweight <- makeDesign(
  type = "constraints", 
  attribute.levels = levels.test, 
  constraints = constraint_list, 
  level.probs = marginal_weights
)

torturedesign_noweight <- makeDesign(
  type = "constraints", 
  attribute.levels = levels.test,
  constraints = constraint_list
)

# -----------------------------------------------------------------------------
# CREATE DEPENDENT VARIABLE: ATTITUDE INDEX
# -----------------------------------------------------------------------------

# Principal Component Analysis
pca <- principal(
  data[c("neighborbinary", "workpermitbinary", "citizenshipbinary")], 
  nfactors = 3, 
  rotate = "varimax"
)

summary(pca)
print(pca$values)  # Using first factor with eigenvalue > 2
print(pca$loadings)

# Create attitude index (rescaled 0-1)
data$attitude <- (data$neighborbinary * 0.261 + 
                    data$workpermitbinary * 0.926 + 
                    data$citizenshipbinary * 0.282) / 1.4690

summary(data$attitude)

# -----------------------------------------------------------------------------
# FIGURE 2: MAIN ANALYSIS - UNCONDITIONAL AMCEs
# -----------------------------------------------------------------------------

# Define formula for all models
conjoint_formula <- as.formula(
  ~ Gender + Age + Ethnicity + Religion + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured
)

# Run models for each dependent variable
results1 <- amce(
  update(conjoint_formula, neighborbinary ~ .),
  data = data,
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results2 <- amce(
  update(conjoint_formula, workpermitbinary ~ .),
  data = data,
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results3 <- amce(
  update(conjoint_formula, citizenshipbinary ~ .),
  data = data,
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results4 <- amce(
  update(conjoint_formula, attitude ~ .),
  data = data,
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

# Print summaries
summary(results1)
summary(results2)
summary(results3)
summary(results4)

# Extract AMCE coefficients from all models
coef1 <- summary(results1)$amce
coef2 <- summary(results2)$amce
coef3 <- summary(results3)$amce
coef4 <- summary(results4)$amce

# Add grouping variable
coef1$group <- "Neighbor"
coef2$group <- "Work permit"
coef3$group <- "Citizenship"
coef4$group <- "Index"

# Combine data
combined_coefs <- rbind(coef1, coef2, coef3, coef4)

# Get baseline information
baselines <- summary(results1)$baselines_amce
baseline_map <- setNames(baselines$Level, baselines$Attribute)
baseline_map["Religion"] <- "Sunni"  # Override religion baseline

print("Baseline categories:")
print(baseline_map)

# Add baseline info to labels
combined_coefs$Level_with_baseline <- paste0(
  combined_coefs$Level, 
  " (vs. ", 
  baseline_map[combined_coefs$Attribute], 
  ")"
)

# Create attribute-level identifier with bold formatting
combined_coefs$attr_level <- paste0(
  "**", combined_coefs$Attribute, "**: ", 
  combined_coefs$Level_with_baseline
)

# Define level ordering (reversed for plotting)
level_order <- c(
  "**Tortured**: Yes (vs. No)",
  "**Ethnicity**: Arab (vs. Turkoman)", 
  "**Ethnicity**: Kurd (vs. Turkoman)",
  "**Religion**: Christian (vs. Sunni)", 
  "**Religion**: Alawite (vs. Sunni)",
  "**Gender**: Female (vs. Male)",
  "**Age**: 31-50 (vs. 18-30)", 
  "**Age**: >51 (vs. 18-30)",
  "**Education**: Primary (vs. None)", 
  "**Education**: Middle (vs. None)",
  "**Education**: High (vs. None)", 
  "**Education**: University (vs. None)",
  "**Knows Turkish**: Yes (vs. No)",
  "**Local friends**: Yes (vs. No)",
  "**Fighter**: FSA (vs. No)", 
  "**Fighter**: Asad (vs. No)"
)

# Check for missing levels
existing_levels <- intersect(level_order, combined_coefs$attr_level)
missing_levels <- setdiff(combined_coefs$attr_level, level_order)

if (length(missing_levels) > 0) {
  print("Levels in data but not in manual order:")
  print(missing_levels)
  level_order <- c(level_order, missing_levels)
}

# Set factor levels (reversed for plotting)
final_order <- intersect(level_order, combined_coefs$attr_level)
combined_coefs$attr_level <- factor(combined_coefs$attr_level, levels = rev(final_order))

# Set group order
combined_coefs$group <- factor(
  combined_coefs$group, 
  levels = c("Index", "Citizenship", "Work permit", "Neighbor")
)

# Create plot
p_combined_four <- ggplot(
  combined_coefs, 
  aes(x = Estimate, y = attr_level, color = group, shape = group)
) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(
    aes(xmin = Estimate - 1.96 * `Std. Err`, 
        xmax = Estimate + 1.96 * `Std. Err`), 
    position = position_dodge(width = 0.7), 
    height = 0.3
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(
    limits = c(-0.175, 0.175),
    breaks = seq(-0.175, 0.175, 0.025),
    labels = sprintf("%.3f", seq(-0.175, 0.175, 0.025))
  ) +
  scale_color_manual(
    values = c(
      "Neighbor" = "#2E86AB",
      "Work permit" = "#A23B72",
      "Citizenship" = "#F39237",
      "Index" = "#26A96C"
    ),
    breaks = c("Neighbor", "Work permit", "Citizenship", "Index")
  ) +
  scale_shape_manual(
    values = c(
      "Neighbor" = 16,
      "Work permit" = 17,
      "Citizenship" = 15,
      "Index" = 18
    ),
    breaks = c("Neighbor", "Work permit", "Citizenship", "Index")
  ) +
  labs(
    x = "Change in the prob. of choosing a profile",
    y = "",
    color = "Dependent variable",
    shape = "Dependent variable"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 11),
    axis.text.y = element_markdown(size = 10),
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

print(p_combined_four)

# -----------------------------------------------------------------------------
# FIGURE 3: TORTURE EFFECTS BY PROFILE CHARACTERISTICS
# -----------------------------------------------------------------------------

# Helper function to extract torture coefficients
extract_torture_coef <- function(results, group_name, facet_name) {
  coef_data <- summary(results)$amce
  torture_coef <- coef_data[coef_data$Attribute == "Tortured", ]
  torture_coef$group <- group_name
  torture_coef$facet <- facet_name
  return(torture_coef)
}

# ------ By Profile Ethnicity (results5-7) ------
results5 <- amce(
  attitude ~ Gender + Age + Religion + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Turkoman"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results6 <- amce(
  attitude ~ Gender + Age + Religion + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Kurd"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results7 <- amce(
  attitude ~ Gender + Age + Religion + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Arab"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

ethnicity_data <- rbind(
  extract_torture_coef(results5, "Turkoman", "Ethnicity"),
  extract_torture_coef(results6, "Kurd", "Ethnicity"),
  extract_torture_coef(results7, "Arab", "Ethnicity")
)

# ------ By Profile Religion (results8-10) ------
results8 <- amce(
  attitude ~ Gender + Age + Ethnicity + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Sunni"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results9 <- amce(
  attitude ~ Gender + Age + Ethnicity + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Alawite"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results10 <- amce(
  attitude ~ Gender + Age + Ethnicity + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Christian"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

religion_data <- rbind(
  extract_torture_coef(results8, "Sunni", "Religion"),
  extract_torture_coef(results9, "Alawite", "Religion"),
  extract_torture_coef(results10, "Christian", "Religion")
)

# ------ By Profile Education (results11-12) ------
results11 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Education == "Primary"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results12 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Education == "University"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

education_data <- rbind(
  extract_torture_coef(results11, "Primary", "Education"),
  extract_torture_coef(results12, "University", "Education")
)

# ------ By Profile Fighter Status (results13-15) ------
results13 <- amce(
  attitude ~ Gender + Age + Ethnicity + Religion + Education + 
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "No"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results14 <- amce(
  attitude ~ Gender + Age + Ethnicity + Religion + Education + 
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "FSA"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results15 <- amce(
  attitude ~ Gender + Age + Ethnicity + Religion + Education + 
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "Asad"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

fighting_data <- rbind(
  extract_torture_coef(results13, "Non-fighter", "Fighting"),
  extract_torture_coef(results14, "FSA fighter", "Fighting"),
  extract_torture_coef(results15, "Assad fighter", "Fighting")
)

# ------ Combine and Plot ------
all_torture_profile_data <- rbind(ethnicity_data, religion_data, education_data, fighting_data)

# Set factor levels
all_torture_profile_data$facet <- factor(
  all_torture_profile_data$facet, 
  levels = c("Ethnicity", "Religion", "Education", "Fighting")
)

all_torture_profile_data$group <- factor(
  all_torture_profile_data$group, 
  levels = c("Turkoman", "Arab", "Kurd",
             "Sunni", "Alawite", "Christian",
             "Primary", "University",
             "Non-fighter", "FSA fighter", "Assad fighter")
)

# Create plot
p_torture_profile <- ggplot(
  all_torture_profile_data, 
  aes(x = Estimate, y = group, color = group)
) +
  geom_point(size = 4) +
  geom_errorbarh(
    aes(xmin = Estimate - 1.96 * `Std. Err`, 
        xmax = Estimate + 1.96 * `Std. Err`), 
    height = 0.2, 
    linewidth = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  facet_wrap(~facet, scales = "free_y", ncol = 2) +
  scale_x_continuous(
    limits = c(-0.15, 0.15),
    breaks = seq(-0.15, 0.15, 0.05),
    labels = sprintf("%.2f", seq(-0.15, 0.15, 0.05))
  ) +
  scale_color_manual(
    values = c(
      "Turkoman" = "#2E86AB", "Arab" = "#A23B72", "Kurd" = "#F39237",
      "Sunni" = "#26A96C", "Alawite" = "#C73E1D", "Christian" = "#8E44AD",
      "Primary" = "#E67E22", "University" = "#3498DB",
      "Non-fighter" = "#95A5A6", "FSA fighter" = "#E74C3C", "Assad fighter" = "#2C3E50"
    )
  ) +
  labs(
    x = "Effect of Torture on Attitude (AMCE)",
    y = "",
    title = "**Torture** Effects by Profile Characteristics",
    subtitle = "Average Marginal Component Effects with 95% Confidence Intervals"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_markdown(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing = unit(1, "lines")
  )

print(p_torture_profile)

# -----------------------------------------------------------------------------
# FIGURE 4: TORTURE EFFECTS BY RESPONDENT CHARACTERISTICS
# -----------------------------------------------------------------------------

# ------ By Respondent Religiosity - Hair Cover (results16-17) ------
results16 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Hair cover` == 5 | data$`Hair cover` == 6 | data$`Hair cover` == 7), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results17 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Hair cover` == 1 | data$`Hair cover` == 2 | data$`Hair cover` == 3), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

# ------ By Respondent Religiosity - Prayer Frequency (results18-21) ------
results18 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Pray == 1), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results19 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Pray == 2), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results20 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Pray == 3), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results21 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Pray == 4), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

religiosity_data <- rbind(
  extract_torture_coef(results17, "Women in family cover hair (1-3)", "Respondent Religiosity"),
  extract_torture_coef(results16, "Women in family cover hair (5-7)", "Respondent Religiosity"),
  extract_torture_coef(results18, "Pray (never)", "Respondent Religiosity"),
  extract_torture_coef(results19, "Pray (Ramadan)", "Respondent Religiosity"),
  extract_torture_coef(results20, "Pray (Fridays)", "Respondent Religiosity"),
  extract_torture_coef(results21, "Pray (5 times a day)", "Respondent Religiosity")
)

# ------ By World Citizen Values (results22-26) ------
results22 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "1"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results23 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "2"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results24 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "3"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results25 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "4"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results26 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "5"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

worldcitizen_data <- rbind(
  extract_torture_coef(results22, "World citizen 1", "World Citizen"),
  extract_torture_coef(results23, "World citizen 2", "World Citizen"),
  extract_torture_coef(results24, "World citizen 3", "World Citizen"),
  extract_torture_coef(results25, "World citizen 4", "World Citizen"),
  extract_torture_coef(results26, "World citizen 5", "World Citizen")
)

# ------ By Respondent Gender (results27-28) ------
results27 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Respondent gender` == "Male"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results28 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Respondent gender` == "Female"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

respondent_gender_data <- rbind(
  extract_torture_coef(results27, "Male respondents", "Respondent Gender"),
  extract_torture_coef(results28, "Female respondents", "Respondent Gender")
)

# Test statistical significance between male and female respondents
z_score <- (0.027532 - 0.0701672) / sqrt(0.0101556^2 + 0.010395^2)
print(paste("Male-Female Z-score:", z_score))

# ------ By OHAL (State of Emergency) Provinces (results29-30) ------
results29 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$province == "Van" | data$province == "Siirt" | 
                      data$province == "Mardin" | data$province == "Diyarbakir" | 
                      data$province == "Bingol" | data$province == "Batman"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results30 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$province != "Van" & data$province != "Siirt" & 
                      data$province != "Mardin" & data$province != "Diyarbakir" & 
                      data$province != "Bingol" & data$province != "Batman"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

ohal_data <- rbind(
  extract_torture_coef(results29, "OHAL provinces", "Province Type"),
  extract_torture_coef(results30, "Non-OHAL provinces", "Province Type")
)

# ------ By Respondent Partisanship (results31-32) ------
results31 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Respondent party` == "AKP"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results32 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Respondent party` == "CHP"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

partisanship_data <- rbind(
  extract_torture_coef(results31, "AKP supporters", "Respondent Partisanship"),
  extract_torture_coef(results32, "CHP supporters", "Respondent Partisanship")
)

# ------ By Respondent Contact with Refugees (results33-41) ------
# Positive contact
results33 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Positive contact` == "None"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results34 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Positive contact` == "One"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results35 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Positive contact` == "Two"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results36 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Positive contact` == "Three"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results37 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Positive contact` == "Four"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

# Negative contact
results38 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Negative contact` == "None"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results39 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Negative contact` == "One"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results40 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Negative contact` == "Two"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

results41 <- amce(
  attitude ~ Religion + Age + Ethnicity + Gender + Education + 
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Negative contact` == "Three"), ],
  cluster = TRUE, 
  respondent.id = "idnumbernew", 
  design = torturedesign_reweight
)

contact_data <- rbind(
  extract_torture_coef(results33, "Positive contact: None", "Contact with Refugees"),
  extract_torture_coef(results34, "Positive contact: One", "Contact with Refugees"),
  extract_torture_coef(results35, "Positive contact: Two", "Contact with Refugees"),
  extract_torture_coef(results36, "Positive contact: Three", "Contact with Refugees"),
  extract_torture_coef(results37, "Positive contact: Four", "Contact with Refugees"),
  extract_torture_coef(results38, "Negative contact: None", "Contact with Refugees"),
  extract_torture_coef(results39, "Negative contact: One", "Contact with Refugees"),
  extract_torture_coef(results40, "Negative contact: Two", "Contact with Refugees"),
  extract_torture_coef(results41, "Negative contact: Three", "Contact with Refugees")
)

# ------ Combine all respondent characteristic data ------
all_torture_respondent_data <- rbind(
  religiosity_data, 
  worldcitizen_data, 
  respondent_gender_data, 
  ohal_data, 
  partisanship_data, 
  contact_data
)

# Set factor levels for proper ordering
all_torture_respondent_data$facet <- factor(
  all_torture_respondent_data$facet, 
  levels = c("Respondent Religiosity", "World Citizen", 
             "Respondent Gender", "Province Type", 
             "Respondent Partisanship", "Contact with Refugees")
)

# Set group ordering within each facet
all_torture_respondent_data$group <- factor(
  all_torture_respondent_data$group, 
  levels = c(
    "Women in family cover hair (1-3)", "Women in family cover hair (5-7)",           
    "Pray (never)", "Pray (Ramadan)", "Pray (Fridays)", "Pray (5 times a day)",
    "World citizen 1", "World citizen 2", "World citizen 3", "World citizen 4", "World citizen 5",
    "Male respondents", "Female respondents",
    "OHAL provinces", "Non-OHAL provinces",
    "AKP supporters", "CHP supporters",
    "Positive contact: Four", "Positive contact: Three", "Positive contact: Two", 
    "Positive contact: One", "Positive contact: None",
    "Negative contact: Three", "Negative contact: Two", 
    "Negative contact: One", "Negative contact: None"
  )
)

# Create custom scales for each facet
scales_x <- list(
  `Respondent Religiosity` = scale_x_continuous(
    limits = c(-0.15, 0.15),
    breaks = seq(-0.15, 0.15, 0.05),
    labels = sprintf("%.2f", seq(-0.15, 0.15, 0.05))
  ),
  `World Citizen` = scale_x_continuous(
    limits = c(-0.15, 0.15),
    breaks = seq(-0.15, 0.15, 0.05),
    labels = sprintf("%.2f", seq(-0.15, 0.15, 0.05))
  ),
  `Respondent Gender` = scale_x_continuous(
    limits = c(-0.15, 0.15),
    breaks = seq(-0.15, 0.15, 0.05),
    labels = sprintf("%.2f", seq(-0.15, 0.15, 0.05))
  ),
  `Province Type` = scale_x_continuous(
    limits = c(-0.15, 0.15),
    breaks = seq(-0.15, 0.15, 0.05),
    labels = sprintf("%.2f", seq(-0.15, 0.15, 0.05))
  ),
  `Respondent Partisanship` = scale_x_continuous(
    limits = c(-0.15, 0.15),
    breaks = seq(-0.15, 0.15, 0.05),
    labels = sprintf("%.2f", seq(-0.15, 0.15, 0.05))
  ),
  `Contact with Refugees` = scale_x_continuous(
    limits = c(-0.3, 0.3),
    breaks = seq(-0.3, 0.3, 0.1),
    labels = sprintf("%.1f", seq(-0.3, 0.3, 0.1))
  )
)

# Create plot
p_torture_respondent <- suppressWarnings(
  ggplot(all_torture_respondent_data, aes(x = Estimate, y = group, color = group)) +
    geom_point(size = 4) +
    geom_errorbarh(
      aes(xmin = Estimate - 1.96 * `Std. Err`, 
          xmax = Estimate + 1.96 * `Std. Err`), 
      height = 0, 
      linewidth = 1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
    facet_wrap(~facet, scales = "free_y", ncol = 2) +
    facetted_pos_scales(x = scales_x) +
    scale_color_manual(
      values = c(
        "Women in family cover hair (1-3)" = "#3498DB", 
        "Women in family cover hair (5-7)" = "#8E44AD", 
        "Pray (never)" = "#27AE60", "Pray (Ramadan)" = "#E67E22", 
        "Pray (Fridays)" = "#F1C40F", "Pray (5 times a day)" = "#2E86AB",
        "World citizen 1" = "#E74C3C", "World citizen 2" = "#E67E22", 
        "World citizen 3" = "#F1C40F", "World citizen 4" = "#2ECC71", 
        "World citizen 5" = "#27AE60",
        "Male respondents" = "#34495E", "Female respondents" = "#E91E63",
        "OHAL provinces" = "#C0392B", "Non-OHAL provinces" = "#2E86AB",
        "AKP supporters" = "#FFA500", "CHP supporters" = "#FF1744",
        "Positive contact: None" = "#BDC3C7", "Positive contact: One" = "#A8E6CF", 
        "Positive contact: Two" = "#7FCD91", "Positive contact: Three" = "#56B870", 
        "Positive contact: Four" = "#27AE60",
        "Negative contact: None" = "#BDC3C7", "Negative contact: One" = "#FFAB91", 
        "Negative contact: Two" = "#FF7043", "Negative contact: Three" = "#E74C3C"
      )
    ) +
    labs(
      x = "Effect of Torture on Attitude (AMCE)",
      y = "",
      title = "**Torture** Effects by Respondent Characteristics",
      subtitle = "Average Marginal Component Effects with 95% Confidence Intervals"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      plot.title = element_markdown(size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 10),
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.spacing = unit(1, "lines")
    )
)

print(p_torture_respondent)

# =============================================================================
# END OF MAIN ANALYSIS
# =============================================================================


# =============================================================================
# APPENDIX: DESCRIPTIVE STATISTICS AND ADDITIONAL ANALYSES
# =============================================================================

# -----------------------------------------------------------------------------
# APPENDIX SECTION B: DESCRIPTIVE STATISTICS
# -----------------------------------------------------------------------------

# FIGURE B1 -- Respondent age
ggplot(data, aes(x = `Respondent age cont.`, fill = `Respondent age cont.`)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "grey50") + 
  labs(y = "Percent", x = "Respondent age") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks = seq(18, 66, by = 1)) +
  scale_fill_discrete(guide = "none")

# FIGURE B2 -- Respondent gender
ggplot(data, aes(x = `Respondent gender`, fill = `Respondent gender`)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "grey50") + 
  labs(y = "Percent", x = "Respondent gender") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_fill_discrete(guide = "none")

# FIGURE B3 -- Respondent education
ggplot(data, aes(x = `Respondent education`, fill = `Respondent education`)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "grey50") + 
  labs(y = "Percent", x = "Respondent education") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_fill_discrete(guide = "none")

# FIGURE B4 -- Respondent partisanship
ggplot(data, aes(x = `Respondent party`, fill = `Respondent party`)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "grey50") + 
  labs(y = "Percent", x = "Respondent partisanship") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_fill_discrete(guide = "none")

# FIGURE B5 -- Respondent province
ggplot(data, aes(x = reorder(province, province, function(x) -length(x)), fill = province)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "grey50") + 
  labs(y = "Percent", x = "Respondent province") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_fill_discrete(guide = "none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# ------ DV Descriptive Stats: Level of Acceptance of Refugees ------

# FIGURE B6 -- Neighbor acceptance
ggplot(data, aes(x = neighborscale2, fill = neighborscale2)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "grey50") + 
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            stat = "count", vjust = -0.5, size = 3) +
  labs(y = "Percent", x = "Attitude") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(
    breaks = c(0, 0.16666, 0.3333333, 0.5, 0.6666667, 0.8333333, 1),
    labels = c("Definitely don't want", "Don't want", "Slightly don't want", 
               "Neutral", "Slightly want", "Want", "Definitely want")
  ) +
  scale_fill_discrete(guide = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# FIGURE B7 -- Work permit acceptance
ggplot(data, aes(x = workpermitscale2, fill = workpermitscale2)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "grey50") + 
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            stat = "count", vjust = -0.5, size = 3) +
  labs(y = "Percent", x = "Attitude") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(
    breaks = c(0, 0.16666, 0.3333333, 0.5, 0.6666667, 0.8333333, 1),
    labels = c("Definitely don't want", "Don't want", "Slightly don't want", 
               "Neutral", "Slightly want", "Want", "Definitely want")
  ) +
  scale_fill_discrete(guide = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# FIGURE B8 -- Citizenship acceptance
ggplot(data, aes(x = citizenshipscale2, fill = citizenshipscale2)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "grey50") + 
  geom_text(aes(y = (..count..)/sum(..count..), 
                label = scales::percent((..count..)/sum(..count..), accuracy = 0.1)),
            stat = "count", vjust = -0.5, size = 3) +
  labs(y = "Percent", x = "Attitude") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(
    breaks = c(0, 0.16666, 0.3333333, 0.5, 0.6666667, 0.8333333, 1),
    labels = c("Definitely don't want", "Don't want", "Slightly don't want", 
               "Neutral", "Slightly want", "Want", "Definitely want")
  ) +
  scale_fill_discrete(guide = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------ FIGURES B9-B15 -- Contact with Refugees ------

contact_vars <- c("Conversations", "Shopped", "Friends", "Dated", "Argument", "Walkingroups", "Cheated")

for (var in contact_vars) {
  p <- ggplot(data, aes(x = .data[[var]], fill = .data[[var]])) +  
    geom_bar(aes(y = (..count..)/sum(..count..)), fill = "grey50") + 
    labs(y = "Percent", x = var) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    scale_fill_discrete(guide = "none")
  print(p)
}


# -----------------------------------------------------------------------------
# FIGURES C16-C18: SEPARATE DVs BY PROFILE ETHNICITY
# -----------------------------------------------------------------------------

# ------ Neighbor as DV (results42-44) ------
results42 <- amce(
  neighborbinary ~ Gender + Age + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Turkoman"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results43 <- amce(
  neighborbinary ~ Gender + Age + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Kurd"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results44 <- amce(
  neighborbinary ~ Gender + Age + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Arab"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# ------ Work Permit as DV (results45-47) ------
results45 <- amce(
  workpermitbinary ~ Gender + Age + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Turkoman"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results46 <- amce(
  workpermitbinary ~ Gender + Age + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Kurd"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results47 <- amce(
  workpermitbinary ~ Gender + Age + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Arab"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# ------ Citizenship as DV (results48-50) ------
results48 <- amce(
  citizenshipbinary ~ Gender + Age + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Turkoman"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results49 <- amce(
  citizenshipbinary ~ Gender + Age + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Kurd"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results50 <- amce(
  citizenshipbinary ~ Gender + Age + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Ethnicity == "Arab"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# Plotting
order2 <- c("Tortured", "Religion", "Fighter", "Gender", "Age", "Education", 
            "Knows Turkish", "Local friends")

p42 <- as.grob(expression(plot(
  results42, main = "Neighbor-Turkoman", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order2
)))

p43 <- as.grob(expression(plot(
  results43, main = "Neighbor-Kurd", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order2
)))

p44 <- as.grob(expression(plot(
  results44, main = "Neighbor-Arab", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order2
)))

p45 <- as.grob(expression(plot(
  results45, main = "Work permit-Turkoman", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order2
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p46 <- as.grob(expression(plot(
  results46, main = "Work permit-Kurd",  
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"),
  text.size = 11, label.baseline = FALSE, group.order = order2
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p47 <- as.grob(expression(plot(
  results47, main = "Work permit-Arab", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"),
  text.size = 11, label.baseline = FALSE, group.order = order2
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p48 <- as.grob(expression(plot(
  results48, main = "Citizenship-Turkoman",  
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"),
  text.size = 11, label.baseline = FALSE, group.order = order2
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p49 <- as.grob(expression(plot(
  results49, main = "Citizenship-Kurd", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"),
  text.size = 11, label.baseline = FALSE, group.order = order2
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p50 <- as.grob(expression(plot(
  results50, main = "Citizenship-Arab", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"),
  text.size = 11, label.baseline = FALSE, group.order = order2
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

# Arrange plots
p_ethnicity_neighbor <- grid.arrange(p42, p43, p44, ncol = 3, nrow = 1)
p_ethnicity_workpermit <- grid.arrange(p45, p46, p47, ncol = 3, nrow = 1)
p_ethnicity_citizenship <- grid.arrange(p48, p49, p50, ncol = 3, nrow = 1)

# -----------------------------------------------------------------------------
# FIGURES C19-C21: SEPARATE DVs BY PROFILE RELIGION
# -----------------------------------------------------------------------------

# ------ Attitude Index by Religion (results51-53) ------
results51 <- amce(
  attitude ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Sunni"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results52 <- amce(
  attitude ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Alawite"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results53 <- amce(
  attitude ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Christian"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# Statistical tests
z_sunni_alawite <- (0.039885 - 0.0424327) / sqrt(0.012658^2 + 0.012413^2)
z_sunni_christian <- (0.039885 - 0.0662748) / sqrt(0.012658^2 + 0.012707^2)
z_alawite_christian <- (0.0424327 - 0.0662748) / sqrt(0.012413^2 + 0.012707^2)

# ------ Neighbor as DV (results54-56) ------
results54 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Sunni"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results55 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Alawite"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results56 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Christian"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# ------ Work Permit as DV (results57-59) ------
results57 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Sunni"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results58 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Alawite"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results59 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Christian"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# ------ Citizenship as DV (results60-62) ------
results60 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Sunni"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results61 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Alawite"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results62 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Religion == "Christian"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# Plotting
order3 <- c("Tortured", "Ethnicity", "Fighter", "Gender", "Age", "Education", 
            "Knows Turkish", "Local friends")

p51 <- as.grob(expression(plot(
  results51, main = "Sunni", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.175, 0.19), 
  breaks = c(-0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order3
)))

p52 <- as.grob(expression(plot(
  results52, main = "Alawite", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.175, 0.19), 
  breaks = c(-0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order3
)))

p53 <- as.grob(expression(plot(
  results53, main = "Christian", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.175, 0.19), 
  breaks = c(-0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order3
)))

p_religion_attitude <- grid.arrange(p51, p52, p53, ncol = 3, nrow = 1)

# Plotting for neighbor, work permit, citizenship
p54 <- as.grob(expression(plot(
  results54, main = "Neighbor-Sunni", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order3
)))

p55 <- as.grob(expression(plot(
  results55, main = "Neighbor-Alawite", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order3
)))

p56 <- as.grob(expression(plot(
  results56, main = "Neighbor-Christian", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order3
)))

p57 <- as.grob(expression(plot(
  results57, main = "Work permit-Sunni", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order3
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p58 <- as.grob(expression(plot(
  results58, main = "Work permit-Alawite",  
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"),
  text.size = 11, label.baseline = FALSE, group.order = order3
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p59 <- as.grob(expression(plot(
  results59, main = "Work permit-Christian", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"),
  text.size = 11, label.baseline = FALSE, group.order = order3
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p60 <- as.grob(expression(plot(
  results60, main = "Citizenship-Sunni",  
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"),
  text.size = 11, label.baseline = FALSE, group.order = order3
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p61 <- as.grob(expression(plot(
  results61, main = "Citizenship-Alawite", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"),
  text.size = 11, label.baseline = FALSE, group.order = order3
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p62 <- as.grob(expression(plot(
  results62, main = "Citizenship-Christian", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"),
  text.size = 11, label.baseline = FALSE, group.order = order3
) + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))))

p_religion_neighbor <- grid.arrange(p54, p55, p56, ncol = 3, nrow = 1)
p_religion_workpermit <- grid.arrange(p57, p58, p59, ncol = 3, nrow = 1)
p_religion_citizenship <- grid.arrange(p60, p61, p62, ncol = 3, nrow = 1)

# -----------------------------------------------------------------------------
# FIGURE C22: SEPARATE DVs BY PROFILE EDUCATION
# -----------------------------------------------------------------------------

# ------ Neighbor as DV (results63-64) ------
results63 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Education == "Primary"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results64 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Education == "University"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# ------ Work Permit as DV (results65-66) ------
results65 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Education == "Primary"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results66 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Education == "University"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# ------ Citizenship as DV (results67-68) ------
results67 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Education == "Primary"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results68 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$Education == "University"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# Plotting
order6 <- c("Tortured", "Ethnicity", "Religion", "Fighter", "Gender", "Age", 
            "Knows Turkish", "Local friends")

p63 <- as.grob(expression(plot(
  results63, main = "Neighbor-Primary education", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p64 <- as.grob(expression(plot(
  results64, main = "Neighbor-University education", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p65 <- as.grob(expression(plot(
  results65, main = "Work permit-Primary education", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p66 <- as.grob(expression(plot(
  results66, main = "Work permit-University education", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p67 <- as.grob(expression(plot(
  results67, main = "Citizenship-Primary education", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p68 <- as.grob(expression(plot(
  results68, main = "Citizenship-University education", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p_education_all <- grid.arrange(p63, p64, p65, p66, p67, p68, ncol = 3, nrow = 2)

# -----------------------------------------------------------------------------
# FIGURES C23-C25: SEPARATE DVs BY PROFILE FIGHTER STATUS
# -----------------------------------------------------------------------------

# ------ Neighbor as DV (results69-71) ------
results69 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "No"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results70 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "FSA"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results71 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "Asad"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# Statistical tests for neighbor torture effects
z_asad_fsa_neighbor <- (-0.0018115 - 0.0071643) / sqrt(0.0090158^2 + 0.0098473^2)
z_asad_nonfighter_neighbor <- (-0.0018115 - 0.0287300) / sqrt(0.0090158^2 + 0.0097899^2)
z_fsa_nonfighter_neighbor <- (0.0071643 - 0.0287300) / sqrt(0.0098473^2 + 0.0097899^2)

# ------ Work Permit as DV (results72-74) ------
results72 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "No"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results73 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "FSA"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results74 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "Asad"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# Statistical tests for work permit torture effects
z_asad_fsa_wp <- (0.02476 - 0.09067) / sqrt(2.971893e-03 + 3.518860e-03)
z_asad_nonfighter_wp <- (0.02476 - 0.22845) / sqrt(2.971893e-03 + 3.453214e-03)
z_fsa_nonfighter_wp <- (0.09067 - 0.22845) / sqrt(3.518860e-03 + 3.453214e-03)

# ------ Citizenship as DV (results75-77) ------
results75 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "No"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results76 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "FSA"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

results77 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Tortured,
  data = data[which(data$Fighter == "Asad"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)

# Statistical tests for citizenship torture effects
z_asad_fsa_cit <- (-0.021042 - 0.01555) / sqrt(2.769014e-03 + 3.340767e-03)
z_asad_nonfighter_cit <- (-0.021042 - 0.15485) / sqrt(2.769014e-03 + 3.281601e-03)
z_fsa_nonfighter_cit <- (0.01555 - 0.15485) / sqrt(3.340767e-03 + 3.281601e-03)

# Plotting
order5 <- c("Tortured", "Ethnicity", "Religion", "Gender", "Age", "Education", 
            "Knows Turkish", "Local friends")

p69 <- as.grob(expression(plot(
  results69, main = "Neighbor-Non-fighter", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order5
)))

p70 <- as.grob(expression(plot(
  results70, main = "Neighbor-FSA fighter", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order5
)))

p71 <- as.grob(expression(plot(
  results71, main = "Neighbor-Asad fighter", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order5
)))

p72 <- as.grob(expression(plot(
  results72, main = "Work permit-Non-fighter", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order5
)))

p73 <- as.grob(expression(plot(
  results73, main = "Work permit-FSA fighter", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order5
)))

p74 <- as.grob(expression(plot(
  results74, main = "Work permit-Asad fighter", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order5
)))

p75 <- as.grob(expression(plot(
  results75, main = "Citizenship-Non-fighter", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.125), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order5
)))

p76 <- as.grob(expression(plot(
  results76, main = "Citizenship-FSA fighter", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order5
)))

p77 <- as.grob(expression(plot(
  results77, main = "Citizenship-Asad fighter", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.1, 0.1), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order5
)))

p_fighter_neighbor <- grid.arrange(p69, p70, p71, ncol = 3, nrow = 1)
p_fighter_workpermit <- grid.arrange(p72, p73, p74, ncol = 3, nrow = 1)
p_fighter_citizenship <- grid.arrange(p75, p76, p77, ncol = 3, nrow = 1)

# -----------------------------------------------------------------------------
# FIGURE C26: SEPARATE DVs BY RESPONDENT PARTISANSHIP
# -----------------------------------------------------------------------------

# ------ Neighbor as DV (results78-81) ------

##FIGURE C26 -- results by three DVs and by respondent partisanship

results78 <- amce(neighborbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="AKP"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results78)

results79 <- amce(neighborbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="CHP"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results79)

##akp-chp--neighbor
(0.0391409 - 0.0362875)/ sqrt(0.016843^2 + 0.019314^2)



results80 <- amce(neighborbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="MHP" | data$"Respondent party"=="Iyi Parti"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results80)


results81 <- amce(neighborbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="HDP"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results81)




results82 <- amce(workpermitbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="AKP"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results82)

results83 <- amce(workpermitbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="CHP"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results83)

##akp-chp--work permit
(0.0420176 - 0.028612)/ sqrt(0.016381^2 + 0.019575^2)



results84 <- amce(workpermitbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="MHP" | data$"Respondent party"=="Iyi Parti"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results84)


results85 <- amce(workpermitbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="HDP"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results85)


results86 <- amce(citizenshipbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="AKP"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results86)

results87 <- amce(citizenshipbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="CHP"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results87)


##akp-chp--citizenship
(0.0466150 - 0.06375181)/ sqrt(0.016597^2 + 0.019852^2)



results88 <- amce(citizenshipbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="MHP" | data$"Respondent party"=="Iyi Parti"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results88)


results89 <- amce(citizenshipbinary ~ Gender +
                    Age +
                    Ethnicity + Religion+ Education+
                    `Local friends` + `Knows Turkish`
                  + Fighter + Tortured, data = data[ which(data$"Respondent party"=="HDP"), ],
                  cluster=TRUE, respondent.id="idnumbernew", design=torturedesign_reweight)
summary(results89)


##Plotting only the interaction

order6<-c("Tortured", "Ethnicity", "Religion", "Fighter", "Gender", "Age", "Education", "Knows Turkish", "Local friends")

p78<- as.grob(expression(plot(results78, main="Neighbor-AKP", xlab="Change in the prob. of choosing a profile",  
                              plot.display="all",
                              xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"), 
                              text.size=11,label.baseline=FALSE, group.order = order6)))

p79<- as.grob(expression(plot(results79, main="Neighbor-CHP", xlab="Change in the prob. of choosing a profile",  
                              plot.display="all",
                              xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                              text.size=11,label.baseline=FALSE, group.order = order6)))

p80<- as.grob(expression(plot(results80, main="Neighbor-MHP+Iyi Parti", xlab="Change in the prob. of choosing a profile",  
                              plot.display="all",
                              xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                              text.size=11,label.baseline=FALSE, group.order = order6)))

p81<- as.grob(expression(plot(results81, main="Neighbor-HDP", xlab="Change in the prob. of choosing a profile",  
                              plot.display="all",
                              xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                              text.size=11,label.baseline=FALSE, group.order = order6)))


p82<- as.grob(expression(plot(results82, main="Work permit-AKP", xlab="Change in the prob. of choosing a profile",  
                              plot.display="all",
                              xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                              text.size=11,label.baseline=FALSE, group.order = order6)))

p83<- as.grob(expression(plot(results83, main="Work permit-CHP", xlab="Change in the prob. of choosing a profile",  
                              plot.display="all",
                              xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                              text.size=11,label.baseline=FALSE, group.order = order6)))#, facet.names = "Tortured")))

p84<- as.grob(expression(plot(results84, main="Work permit-MHP+Iyi Parti", xlab="Change in the prob. of choosing a profile",  
                               plot.display="all",
                               xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                               text.size=11,label.baseline=FALSE, group.order = order6)))#, facet.names = "Tortured")))

p85<- as.grob(expression(plot(results85, main="Work permit-HDP", xlab="Change in the prob. of choosing a profile",  
                               plot.display="all",
                               xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                               text.size=11,label.baseline=FALSE, group.order = order6)))#, facet.names = "Tortured")))


p86<- as.grob(expression(plot(results86, main="Citizenship-AKP", xlab="Change in the prob. of choosing a profile",  
                               plot.display="all",
                               xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                               text.size=11,label.baseline=FALSE, group.order = order6)))#, facet.names = "Tortured")))

p87<- as.grob(expression(plot(results87, main="Citizenship-CHP", xlab="Change in the prob. of choosing a profile",  
                               plot.display="all",
                               xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                               text.size=11,label.baseline=FALSE, group.order = order6)))#, facet.names = "Tortured")))

p88<- as.grob(expression(plot(results88, main="Citizenship-MHP+Iyi Parti", xlab="Change in the prob. of choosing a profile",  
                               plot.display="all",
                               xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                               text.size=11,label.baseline=FALSE, group.order = order6)))#, facet.names = "Tortured")))

p89<- as.grob(expression(plot(results89, main="Citizenship-HDP", xlab="Change in the prob. of choosing a profile",  
                               plot.display="all",
                               xlim=c(-.21,.21), breaks=c(-.2, -.15, -.1, -.075, -.05, -.025, 0, .025, .05, .075, .1, .15, .2), labels=c("-.2", "-.15", "-.1", "-.075", "-.05","-.025", "0" ,".025",".05", ".075", ".1",".15", ".2"),
                               text.size=11,label.baseline=FALSE, group.order = order6)))#, facet.names = "Tortured")))


#Plotting only AKP and CHP
p_partisanship<- grid.arrange(p78,p79,p82,p83,p86,p87,ncol=3,nrow=2)

# -----------------------------------------------------------------------------
# FIGURE C27: SEPARATE DVs BY RESPONDENT GENDER
# -----------------------------------------------------------------------------

# ------ Neighbor as DV (results90-91) ------
results90 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Respondent gender` == "Male"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results90)

results91 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Respondent gender` == "Female"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results91)

# Statistical test for neighbor torture effects
(0.0624550 - 0.03245494) / sqrt(0.011756^2 + 0.011903^2)  # female-male

# ------ Work Permit as DV (results92-93) ------
results92 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Respondent gender` == "Male"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results92)

results93 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Respondent gender` == "Female"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results93)

# Statistical test for work permit torture effects
(0.0695590 - 0.023776) / sqrt(0.011976^2 + 0.011427^2)  # female-male

# ------ Citizenship as DV (results94-95) ------
results94 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Respondent gender` == "Male"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results94)

results95 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Respondent gender` == "Female"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results95)

# Statistical test for citizenship torture effects
(0.0793023 - 0.0353091) / sqrt(0.012140^2 + 0.011902^2)  # female-male

# Plotting
p90 <- as.grob(expression(plot(
  results90, main = "Neighbor-Male respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.15, 0.15), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p91 <- as.grob(expression(plot(
  results91, main = "Neighbor-Female respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.15, 0.15), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p92 <- as.grob(expression(plot(
  results92, main = "Work permit-Male respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.15, 0.15), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p93 <- as.grob(expression(plot(
  results93, main = "Work permit-Female respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.15, 0.15), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p94 <- as.grob(expression(plot(
  results94, main = "Citizenship-Male respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.15, 0.15), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p95 <- as.grob(expression(plot(
  results95, main = "Citizenship-Female respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.15, 0.15), 
  breaks = c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1), 
  labels = c("-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p_gender <- grid.arrange(p90, p91, p92, p93, p94, p95, ncol = 3, nrow = 2)

# -----------------------------------------------------------------------------
# FIGURE C28: SEPARATE DVs BY RESPONDENT PROVINCE (OHAL)
# -----------------------------------------------------------------------------

# Create OHAL subset
ohal <- subset(data,
               province == "Van" | province == "Siirt" |
                 province == "Mardin" | province == "Diyarbakir" | 
                 province == "Bingol" | province == "Batman")

# ------ Neighbor as DV (results96-97) ------
results96 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$province == "Van" | data$province == "Siirt" |
                      data$province == "Mardin" | data$province == "Diyarbakir" |
                      data$province == "Bingol" | data$province == "Batman"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results96)

results97 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$province != "Van" & data$province != "Siirt" &
                      data$province != "Mardin" & data$province != "Diyarbakir" &
                      data$province != "Bingol" & data$province != "Batman"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results97)

# ------ Work Permit as DV (results98-99) ------
results98 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$province == "Van" | data$province == "Siirt" |
                      data$province == "Mardin" | data$province == "Diyarbakir" |
                      data$province == "Bingol" | data$province == "Batman"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results98)

results99 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$province != "Van" & data$province != "Siirt" &
                      data$province != "Mardin" & data$province != "Diyarbakir" &
                      data$province != "Bingol" & data$province != "Batman"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results99)

# Statistical test for work permit torture effects
(0.0254165 - 0.0127800) / sqrt(0.0085337^2 + 0.0090304^2)  # religious-nonreligious

# ------ Citizenship as DV (results100-101) ------
results100 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$province == "Van" | data$province == "Siirt" |
                      data$province == "Mardin" | data$province == "Diyarbakir" |
                      data$province == "Bingol" | data$province == "Batman"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results100)

results101 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$province != "Van" & data$province != "Siirt" &
                      data$province != "Mardin" & data$province != "Diyarbakir" &
                      data$province != "Bingol" & data$province != "Batman"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results101)

# Statistical test for citizenship torture effects
(0.0189484 - 0.0018904) / sqrt(0.0086529^2 + 0.0082595^2)  # religious-nonreligious

# Plotting
p96 <- as.grob(expression(plot(
  results96, main = "Neighbor-OHAL", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.185, 0.295), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175", "0.2", "0.225", "0.25", "0.275"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p97 <- as.grob(expression(plot(
  results97, main = "Neighbor-Not OHAL", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.185, 0.15), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p98 <- as.grob(expression(plot(
  results98, main = "Work permit-OHAL", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.185, 0.295), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175", "0.2", "0.225", "0.25", "0.275"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p99 <- as.grob(expression(plot(
  results99, main = "Work permit-Not OHAL", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.185, 0.15), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p100 <- as.grob(expression(plot(
  results100, main = "Citizenship-OHAL", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.185, 0.295), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175", "0.2", "0.225", "0.25", "0.275"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p101 <- as.grob(expression(plot(
  results101, main = "Citizenship-Not OHAL", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.185, 0.15), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p_ohal <- grid.arrange(p96, p97, p98, p99, p100, p101, ncol = 3, nrow = 2)

# -----------------------------------------------------------------------------
# FIGURE C29: SEPARATE DVs BY RESPONDENT RELIGIOSITY
# -----------------------------------------------------------------------------

# ------ Neighbor as DV (results102-103) ------
results102 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Hair cover` == 5 | data$`Hair cover` == 6 | data$`Hair cover` == 7), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results102)

results103 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Hair cover` == 1 | data$`Hair cover` == 2 | data$`Hair cover` == 3), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results103)

# ------ Work Permit as DV (results104-105) ------
results104 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Hair cover` == 5 | data$`Hair cover` == 6 | data$`Hair cover` == 7), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results104)

results105 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Hair cover` == 1 | data$`Hair cover` == 2 | data$`Hair cover` == 3), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results105)

# Statistical test for work permit torture effects
(0.0254165 - 0.0127800) / sqrt(0.0085337^2 + 0.0090304^2)  # religious-nonreligious

# ------ Citizenship as DV (results106-107) ------
results106 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Hair cover` == 5 | data$`Hair cover` == 6 | data$`Hair cover` == 7), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results106)

results107 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$`Hair cover` == 1 | data$`Hair cover` == 2 | data$`Hair cover` == 3), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results107)

# Statistical test for citizenship torture effects
(0.0189484 - 0.0018904) / sqrt(0.0086529^2 + 0.0082595^2)  # religious-nonreligious

# Plotting
p102 <- as.grob(expression(plot(
  results102, main = "Neighbor-Religious respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p103 <- as.grob(expression(plot(
  results103, main = "Neighbor-Non-religious respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p104 <- as.grob(expression(plot(
  results104, main = "Work permit-Religious respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p105 <- as.grob(expression(plot(
  results105, main = "Work permit-Non-religious respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.175, 0.185), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p106 <- as.grob(expression(plot(
  results106, main = "Citizenship-Religious respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p107 <- as.grob(expression(plot(
  results107, main = "Citizenship-Non-religious respondent", 
  xlab = "Change in the prob. of choosing a profile",  
  plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p_religiosity <- grid.arrange(p102, p103, p104, p105, p106, p107, ncol = 3, nrow = 2)

# -----------------------------------------------------------------------------
# FIGURE C30: SEPARATE DVs BY RESPONDENT VALUES (WORLD CITIZEN)
# -----------------------------------------------------------------------------

# ------ Neighbor as DV (results108-109) ------
results108 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "3" | data$world_citizen == "4" | data$world_citizen == "5"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results108)

results109 <- amce(
  neighborbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "1" | data$world_citizen == "2"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results109)

# ------ Work Permit as DV (results110-111) ------
results110 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "3" | data$world_citizen == "4" | data$world_citizen == "5"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results110)

results111 <- amce(
  workpermitbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "1" | data$world_citizen == "2"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results111)

# Statistical test for work permit torture effects
(0.0206481 - 0.0074247) / sqrt((0.0061023^2) + (0.017060^2))  # world citizen-not world citizen

# ------ Citizenship as DV (results112-113) ------
results112 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "3" | data$world_citizen == "4" | data$world_citizen == "5"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results112)

results113 <- amce(
  citizenshipbinary ~ Gender + Age + Ethnicity + Religion + Education +
    `Local friends` + `Knows Turkish` + Fighter + Tortured,
  data = data[which(data$world_citizen == "1" | data$world_citizen == "2"), ],
  cluster = TRUE,
  respondent.id = "idnumbernew",
  design = torturedesign_reweight
)
summary(results113)

# Plotting
p108 <- as.grob(expression(plot(
  results108, main = "Neighbor-World cit (high)", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p109 <- as.grob(expression(plot(
  results109, main = "Neighbor-World cit (low)", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p110 <- as.grob(expression(plot(
  results110, main = "Work permit-World cit (high)", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p111 <- as.grob(expression(plot(
  results111, main = "Work permit-World cit (low)", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p112 <- as.grob(expression(plot(
  results112, main = "Citizenship-World cit (high)", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p113 <- as.grob(expression(plot(
  results113, main = "Citizenship-World cit (low)", 
  xlab = "Change in the prob. of choosing a profile",  
   plot.display = "all",
  xlim = c(-0.175, 0.175), 
  breaks = c(-0.175, -0.15, -0.125, -0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175), 
  labels = c("-0.175", "-0.15", "-0.125", "-0.1", "-0.075", "-0.05", "-0.025", "0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15", "0.175"), 
  text.size = 11, label.baseline = FALSE, group.order = order6
)))

p42 <- grid.arrange(p108, p109, p110, p111, p112, p113, ncol = 3, nrow = 2)

# =============================================================================
# END OF APPENDIX ANALYSIS
# =============================================================================

#sink()