# Medicare Eligibility and Reported Support for Proposals to Expand Medicare
# N.M. Kavanagh, A.L. Campbell, A. McIntyre
# February 28, 2024

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Load packages
library(here)         # Working directory
library(readstata13)  # Dataset tools
library(dplyr)        # Analysis tools
library(lfe)          # Modeling tools
library(rdrobust)     # Modeling tools
library(ggplot2)      # Graphing tools
library(cowplot)      # Graphing tools
library(scales)       # Graphing tools
library(statar)       # Graphing tools
library(arsenal)      # Table tools

##############################################################################
# Dataset preparation
##############################################################################

# Set RD threshold
THRESHOLD <- 65

# Read datasets into R
cces_2018 <- read.dta13("CES 2018/cces18_common_vv.dta")
cces_2019 <- read.dta13("CES 2019/CCES19_Common_OUTPUT.dta")
cces_2020 <- read.dta13("CES 2020/CES20_Common_OUTPUT_vv.dta")
cces_2021 <- read.dta13("CES 2021/CCES21_Common_OUTPUT.dta")
cces_2022 <- read.dta13("CES 2022/CES22_Common.dta")

# Year
cces_2018$year <- 2018
cces_2019$year <- 2019
cces_2020$year <- 2020
cces_2021$year <- 2021
cces_2022$year <- 2022

# Age
cces_2018$age <- 2018 - cces_2018$birthyr
cces_2019$age <- 2019 - cces_2019$birthyr
cces_2020$age <- 2020 - cces_2020$birthyr
cces_2021$age <- 2021 - cces_2021$birthyr
cces_2022$age <- 2022 - cces_2022$birthyr

# Gender
cces_2018 <- cces_2018 %>% mutate(
  gender_cl = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female"))

cces_2019 <- cces_2019 %>% mutate(
  gender_cl = case_when(
    gender == "Male"   ~ "Male",
    gender == "Female" ~ "Female"))

cces_2020 <- cces_2020 %>% mutate(
  gender_cl = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female"))

cces_2021 <- cces_2021 %>% mutate(
  gender_cl = case_when(
    gender4 == 1 ~ "Male",
    gender4 == 2 ~ "Female",
    gender4 == 3 ~ "Non-binary",
    gender4 == 4 ~ "Other"))

cces_2022 <- cces_2022 %>% mutate(
  gender_cl = case_when(
    gender4 == "Man"        ~ "Male",
    gender4 == "Woman"      ~ "Female",
    gender4 == "Non-binary" ~ "Non-binary",
    gender4 == "Other"      ~ "Other"))

# Race/ethnicity
cces_2018 <- cces_2018 %>% mutate(
  race_cl = case_when(
    race %in% c(1)   ~ "White",
    race %in% c(2)   ~ "Black",
    race %in% c(3)   ~ "Hispanic/Latino",
    race %in% c(4)   ~ "Asian",
    race %in% c(5)   ~ "Native American",
    race %in% c(6:8) ~ "Other"))

cces_2019 <- cces_2019 %>% mutate(
  race_cl = case_when(
    race %in% c("White")                            ~ "White",
    race %in% c("Black")                            ~ "Black",
    race %in% c("Hispanic")                         ~ "Hispanic/Latino",
    race %in% c("Asian")                            ~ "Asian",
    race %in% c("Native American")                  ~ "Native American",
    race %in% c("Mixed", "Other", "Middle Eastern") ~ "Other"))

cces_2020 <- cces_2020 %>% mutate(
  race_cl = case_when(
    race %in% c(1)   ~ "White",
    race %in% c(2)   ~ "Black",
    race %in% c(3)   ~ "Hispanic/Latino",
    race %in% c(4)   ~ "Asian",
    race %in% c(5)   ~ "Native American",
    race %in% c(6:8) ~ "Other"))

cces_2021 <- cces_2021 %>% mutate(
  race_cl = case_when(
    race %in% c(1)   ~ "White",
    race %in% c(2)   ~ "Black",
    race %in% c(3)   ~ "Hispanic/Latino",
    race %in% c(4)   ~ "Asian",
    race %in% c(5)   ~ "Native American",
    race %in% c(6:8) ~ "Other"))

cces_2022 <- cces_2022 %>% mutate(
  race_cl = case_when(
    race %in% c("White")           ~ "White",
    race %in% c("Black")           ~ "Black",
    race %in% c("Hispanic")        ~ "Hispanic/Latino",
    race %in% c("Asian")           ~ "Asian",
    race %in% c("Native American") ~ "Native American",
    race %in% c("Two or more races", "Other",
                "Middle Eastern") ~ "Other"))

# Educational attainment
cces_2018 <- cces_2018 %>% mutate(
  edu_cl = case_when(
    educ %in% c(1)   ~ "Less than high school",
    educ %in% c(2)   ~ "High school graduate",
    educ %in% c(3:4) ~ "Some college or 2-year degree",
    educ %in% c(5:6) ~ "College graduate or higher"))

cces_2019 <- cces_2019 %>% mutate(
  edu_cl = case_when(
    educ %in% c("No HS")                  ~ "Less than high school",
    educ %in% c("High school graduate")   ~ "High school graduate",
    educ %in% c("Some college", "2-year") ~ "Some college or 2-year degree",
    educ %in% c("4-year", "Post-grad")    ~ "College graduate or higher"))

cces_2020 <- cces_2020 %>% mutate(
  edu_cl = case_when(
    educ %in% c(1)   ~ "Less than high school",
    educ %in% c(2)   ~ "High school graduate",
    educ %in% c(3:4) ~ "Some college or 2-year degree",
    educ %in% c(5:6) ~ "College graduate or higher"))

cces_2021 <- cces_2021 %>% mutate(
  edu_cl = case_when(
    educ %in% c(1)   ~ "Less than high school",
    educ %in% c(2)   ~ "High school graduate",
    educ %in% c(3:4) ~ "Some college or 2-year degree",
    educ %in% c(5:6) ~ "College graduate or higher"))

cces_2022 <- cces_2022 %>% mutate(
  edu_cl = case_when(
    educ %in% c("No HS")                  ~ "Less than high school",
    educ %in% c("High school graduate")   ~ "High school graduate",
    educ %in% c("Some college", "2-year") ~ "Some college or 2-year degree",
    educ %in% c("4-year", "Post-grad")    ~ "College graduate or higher"))

# Marital status
# Note: Includes civil partnership with never married
# Consistent with coding of American Community Survey
cces_2018 <- cces_2018 %>% mutate(
  marital_cl = case_when(
    marstat %in% c(1)   ~ "Married",
    marstat %in% c(2)   ~ "Separated",
    marstat %in% c(3)   ~ "Divorced",
    marstat %in% c(4)   ~ "Widowed",
    marstat %in% c(5:6) ~ "Never married"))

cces_2019 <- cces_2019 %>% mutate(
  marital_cl = case_when(
    marstat %in% c("Married")   ~ "Married",
    marstat %in% c("Separated") ~ "Separated",
    marstat %in% c("Divorced")  ~ "Divorced",
    marstat %in% c("Widowed")   ~ "Widowed",
    marstat %in% c("Never married", "Domestic / civil partnership") ~ "Never married"))

cces_2020 <- cces_2020 %>% mutate(
  marital_cl = case_when(
    marstat %in% c(1)   ~ "Married",
    marstat %in% c(2)   ~ "Separated",
    marstat %in% c(3)   ~ "Divorced",
    marstat %in% c(4)   ~ "Widowed",
    marstat %in% c(5:6) ~ "Never married"))

cces_2021 <- cces_2021 %>% mutate(
  marital_cl = case_when(
    marstat %in% c(1)   ~ "Married",
    marstat %in% c(2)   ~ "Separated",
    marstat %in% c(3)   ~ "Divorced",
    marstat %in% c(4)   ~ "Widowed",
    marstat %in% c(5:6) ~ "Never married"))

cces_2022 <- cces_2022 %>% mutate(
  marital_cl = case_when(
    marstat %in% c("Married")   ~ "Married",
    marstat %in% c("Separated") ~ "Separated",
    marstat %in% c("Divorced")  ~ "Divorced",
    marstat %in% c("Widowed")   ~ "Widowed",
    marstat %in% c("Never married", "Domestic / civil partnership") ~ "Never married"))

# Party identification
cces_2018 <- cces_2018 %>% mutate(
  pid_cl = case_when(
    pid3 %in% c(1)   ~ "Democrat",
    pid3 %in% c(2)   ~ "Republican",
    pid3 %in% c(3:5) ~ "Independent, other, or not sure"))

cces_2019 <- cces_2019 %>% mutate(
  pid_cl = case_when(
    pid3 %in% c("Democrat")   ~ "Democrat",
    pid3 %in% c("Republican") ~ "Republican",
    pid3 %in% c("Independent", "Other", "Not sure") ~
      "Independent, other, or not sure"))

cces_2020 <- cces_2020 %>% mutate(
  pid_cl = case_when(
    pid3 %in% c(1)   ~ "Democrat",
    pid3 %in% c(2)   ~ "Republican",
    pid3 %in% c(3:5) ~ "Independent, other, or not sure"))

cces_2021 <- cces_2021 %>% mutate(
  pid_cl = case_when(
    pid3 %in% c(1)   ~ "Democrat",
    pid3 %in% c(2)   ~ "Republican",
    pid3 %in% c(3:5) ~ "Independent, other, or not sure"))

cces_2022 <- cces_2022 %>% mutate(
  pid_cl = case_when(
    pid3 %in% c("Democrat")   ~ "Democrat",
    pid3 %in% c("Republican") ~ "Republican",
    pid3 %in% c("Independent", "Other", "Not sure") ~
      "Independent, other, or not sure"))

# Public health insurance
# In 2018, Medicare and Medicaid separately asked
# Combining since not asked separately in other years
cces_2018 <- cces_2018 %>% mutate(
  public_ins = case_when(
    healthins_2 == 1 | healthins_3 == 1 ~ 1, TRUE ~ 0))

cces_2019 <- cces_2019 %>% mutate(
  public_ins = case_when(
    healthins_2 == "selected" ~ 1, TRUE ~ 0))

cces_2020 <- cces_2020 %>% mutate(
  public_ins = case_when(
    healthins_2 == 1 ~ 1, TRUE ~ 0))

cces_2021 <- cces_2021 %>% mutate(
  public_ins = case_when(
    healthins_2 == 1 ~ 1, TRUE ~ 0))

cces_2022 <- cces_2022 %>% mutate(
  public_ins = case_when(
    healthins_2 == "selected" ~ 1, TRUE ~ 0))

# Support for Medicare for All
cces_2018 <- cces_2018 %>% mutate(
  support_m4a = case_when(
    CC18_327a == 1 ~ 1, CC18_327a == 2 ~ 0))

cces_2019 <- cces_2019 %>% mutate(
  support_m4a = case_when(
    CC19_327a == "Support" ~ 1, CC19_327a == "Oppose" ~ 0))

cces_2020 <- cces_2020 %>% mutate(
  support_m4a = case_when(
    CC20_327a == 1 ~ 1, CC20_327a == 2 ~ 0))

cces_2021 <- cces_2021 %>% mutate(
  support_m4a = case_when(
    CC21_320a == 1 ~ 1, CC21_320a == 2 ~ 0))

cces_2022 <- cces_2022 %>% mutate(
  support_m4a = case_when(
    CC22_327a == "Support" ~ 1, CC22_327a == "Oppose" ~ 0))

# Support for public option
cces_2019 <- cces_2019 %>% mutate(
  public_option = case_when(
    CC19_327b == "Support" ~ 1, CC19_327b == "Oppose" ~ 0))

# Support for lowering Medicare age
cces_2019 <- cces_2019 %>% mutate(
  lower_age = case_when(
    CC19_327c == "Support" ~ 1, CC19_327c == "Oppose" ~ 0))

cces_2020 <- cces_2020 %>% mutate(
  lower_age = case_when(
    CC20_327c == 1 ~ 1, CC20_327c == 2 ~ 0))

# Support for drug negotiation
cces_2020 <- cces_2020 %>% mutate(
  drug_neg = case_when(
    CC20_327b == 1 ~ 1, CC20_327b == 2 ~ 0))

cces_2022 <- cces_2022 %>% mutate(
  drug_neg = case_when(
    CC22_327b == "Support" ~ 1, CC22_327b == "Oppose" ~ 0))

# Select variables of interest
cces_2018_select <- cces_2018 %>%
  select(caseid, commonweight, year, age, gender_cl, race_cl, edu_cl, marital_cl,
         pid_cl, public_ins, support_m4a)
cces_2019_select <- cces_2019 %>%
  select(caseid, commonweight, year, age, gender_cl, race_cl, edu_cl, marital_cl,
         pid_cl, public_ins, support_m4a, public_option, lower_age)
cces_2020_select <- cces_2020 %>%
  select(caseid, commonweight, year, age, gender_cl, race_cl, edu_cl, marital_cl,
         pid_cl, public_ins, support_m4a, lower_age, drug_neg)
cces_2021_select <- cces_2021 %>%
  select(caseid, commonweight, year, age, gender_cl, race_cl, edu_cl, marital_cl,
         pid_cl, public_ins, support_m4a)
cces_2022_select <- cces_2022 %>%
  select(caseid, commonweight, year, age, gender_cl, race_cl, edu_cl, marital_cl,
         pid_cl, public_ins, support_m4a, drug_neg)

# Merge years together
cces_all <- bind_rows(cces_2018_select, cces_2019_select)
cces_all <- bind_rows(cces_all,         cces_2020_select)
cces_all <- bind_rows(cces_all,         cces_2021_select)
cces_all <- bind_rows(cces_all,         cces_2022_select)

# Re-level race/ethnicity
cces_all$race_cl <- factor(
  cces_all$race_cl, levels = c("White", "Black", "Hispanic/Latino",
                               "Asian", "Native American", "Other"))

# Re-level education
cces_all$edu_cl <- factor(
  cces_all$edu_cl, levels = c("Less than high school", "High school graduate",
                              "Some college or 2-year degree", "College graduate or higher"))

# Re-level marital status
cces_all$marital_cl <- factor(
  cces_all$marital_cl, levels = c("Married", "Separated", "Divorced",
                                  "Widowed", "Never married"))

# Dichotomize age
cces_all <- cces_all %>% mutate(
  age_two = case_when(
    age > 65 ~ 1,
    age < 65 ~ 0))

# Dichotomize gender
cces_all <- cces_all %>% mutate(
  gender_two = case_when(
    gender_cl %in% c("Male") ~ 1,
    !is.na(gender_cl)        ~ 0))

# Dichotomize race/ethnicity
cces_all <- cces_all %>% mutate(
  race_two = case_when(
    race_cl %in% c("White") ~ 1,
    !is.na(race_cl)         ~ 0))

# Dichotomize marital status
cces_all <- cces_all %>% mutate(
  marital_two = case_when(
    marital_cl %in% c("Married") ~ 1,
    !is.na(marital_cl)           ~ 0))

# Dichotomize education
cces_all <- cces_all %>% mutate(
  edu_two = case_when(
    edu_cl %in% c("College graduate or higher") ~ 1,
    !is.na(edu_cl)                              ~ 0))

##############################################################################
# Descriptives: Demographic characteristics
##############################################################################

# Drop age 65 since exact birth date unknown
# That is, unclear if 64 or 65 at time of survey
cces_all <- cces_all %>% mutate(
  age = case_when(
    age == 65 ~ NA_real_, TRUE ~ age))

# Require at least 1 outcome
cces_all <- cces_all %>% mutate(
  has_outcome = case_when(
    !is.na(public_ins) | !is.na(support_m4a) | !is.na(public_option) |
      !is.na(lower_age) | !is.na(drug_neg) ~ 1
  ))

# Complete cases for models
cces_model <- cces_all %>%
  filter_at(vars(has_outcome, age), all_vars(!is.na(.)))

# Characteristics: All respondents unweighted
summary(tableby(~ age + gender_cl + race_cl + edu_cl + marital_cl + pid_cl,
                cces_model, digits.pct=1), text=T)

# Characteristics: All respondents weighted
summary(tableby(~ age + gender_cl + race_cl + edu_cl + marital_cl + pid_cl,
                cces_model, digits.pct=1, weights=commonweight), text=T)

# Number of respondents by outcome
table(is.na(cces_model$public_ins))
table(is.na(cces_model$support_m4a))
table(is.na(cces_model$public_option))
table(is.na(cces_model$lower_age))
table(is.na(cces_model$drug_neg))

##############################################################################
# Descriptives: Rates by age group
##############################################################################

# Unweighted outcomes by age
summary(tableby(age_two ~ public_ins + support_m4a + public_option + lower_age + drug_neg,
                cces_model, digits.pct=0), text=T)

# Unadjusted rates: Public insurance
addmargins(table(cces_model$public_ins))
prop.table(table(cces_model$public_ins))
addmargins(table(cces_model$age_two, cces_model$public_ins))
addmargins(prop.table(table(cces_model$age_two, cces_model$public_ins), 1))

# Unadjusted rates: Medicare for All
addmargins(table(cces_model$support_m4a))
prop.table(table(cces_model$support_m4a))
addmargins(table(cces_model$age_two, cces_model$support_m4a))
addmargins(prop.table(table(cces_model$age_two, cces_model$support_m4a), 1))

# Unadjusted rates: Public option
addmargins(table(cces_model$public_option))
prop.table(table(cces_model$public_option))
addmargins(table(cces_model$age_two, cces_model$public_option))
addmargins(prop.table(table(cces_model$age_two, cces_model$public_option), 1))

# Unadjusted rates: Lower Medicare age
addmargins(table(cces_model$lower_age))
prop.table(table(cces_model$lower_age))
addmargins(table(cces_model$age_two, cces_model$lower_age))
addmargins(prop.table(table(cces_model$age_two, cces_model$lower_age), 1))

# Unadjusted rates: Drug negotiation
addmargins(table(cces_model$drug_neg))
prop.table(table(cces_model$drug_neg))
addmargins(table(cces_model$age_two, cces_model$drug_neg))
addmargins(prop.table(table(cces_model$age_two, cces_model$drug_neg), 1))

##############################################################################
# Main models: Simple, unadjusted differences by age
##############################################################################

# Unadjusted differences by age: Public insurance
diff_1 <- felm(public_ins ~ age_two | 0 | 0 | 0, data=cces_model)
round(diff_1$coef, 3)[2]
round(diff_1$coef - 1.96*diff_1$rse, 3)[2]
round(diff_1$coef + 1.96*diff_1$rse, 3)[2]

# Unadjusted differences by age: Medicare for All
diff_2 <- felm(support_m4a ~ age_two | 0 | 0 | 0, data=cces_model)
round(diff_2$coef, 3)[2]
round(diff_2$coef - 1.96*diff_2$rse, 3)[2]
round(diff_2$coef + 1.96*diff_2$rse, 3)[2]

# Unadjusted differences by age: Public option
diff_3 <- felm(public_option ~ age_two | 0 | 0 | 0, data=cces_model)
round(diff_3$coef, 3)[2]
round(diff_3$coef - 1.96*diff_3$rse, 3)[2]
round(diff_3$coef + 1.96*diff_3$rse, 3)[2]

# Unadjusted differences by age: Lower Medicare age
diff_4 <- felm(lower_age ~ age_two | 0 | 0 | 0, data=cces_model)
round(diff_4$coef, 3)[2]
round(diff_4$coef - 1.96*diff_4$rse, 3)[2]
round(diff_4$coef + 1.96*diff_4$rse, 3)[2]

# Unadjusted differences by age: Drug negotiation
diff_5 <- felm(drug_neg ~ age_two | 0 | 0 | 0, data=cces_model)
round(diff_5$coef, 3)[2]
round(diff_5$coef - 1.96*diff_5$rse, 3)[2]
round(diff_5$coef + 1.96*diff_5$rse, 3)[2]

##############################################################################
# Graphs: Insurance and policy discontinuities
##############################################################################

# Function to generate RDD plots
# Requires: Df, outcome variable, caption, plot title, y-axis title
# Returns: Formatted coefficient plot
print_rdd_plot <- function(df, OUTCOME, CAPTION, TITLE, Y_TITLE) {
  
  # Parse outcome string
  outcome <- sym(OUTCOME)
  
  # Generate RDD plot
  plot <- ggplot() +
    
    # Threshold line
    geom_vline(xintercept=THRESHOLD, linetype="dashed") +
    
    # Binned scatterplots for each side of 65
    stat_binmean(n=1000, data=subset(df, age < 65),
                 aes(x=age, y=!!outcome), size=1) +
    stat_binmean(n=1000, data=subset(df, age > 65 & age <= 90),
                 aes(x=age, y=!!outcome), size=1) +
    
    # Global polynomial fit lines
    # Note: Limit to ≤90 years given sparse data over 90
    geom_smooth(data=subset(df, age < 65),
                aes(x=age, y=!!outcome), formula=y ~ poly(x, 4, raw=TRUE),
                method = "lm", se = F, color="red") +
    geom_smooth(data=subset(df, age > 65 & age <= 90),
                aes(x=age, y=!!outcome), formula=y ~ poly(x, 4, raw=TRUE),
                method = "lm", se = F, color="red") +
    
    # Cosmetic modifications
    xlab("Age") + ylab(Y_TITLE) +
    coord_cartesian(xlim=c(18,90), ylim=c(0,1)) +
    labs(caption=TITLE) +
    theme_test() +
    theme(legend.position = "bottom",
          text = element_text(size = 10, face = "bold"),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_line(color="light gray", linewidth=0.5),
          panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
          panel.grid.minor.y = element_line(color="light gray", linewidth=0.25),
          plot.caption = element_text(face = "italic", hjust=0.5)) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2),
                       labels = function(x) paste0(x*100,"%"))
  
  # Return plot
  return(plot)
}

# Generate main figures
plot_all <- plot_grid(
  
  # Public insurance
  print_rdd_plot(cces_model, "public_ins",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Public insurance coverage\n(e.g. Medicare or Medicaid)"),
  
  # Medicare for All
  print_rdd_plot(cces_model, "support_m4a",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Support for expanding\nMedicare to all Americans"),
  
  # Public option
  print_rdd_plot(cces_model, "public_option",
                 TITLE   = "Only polled in 2019",
                 Y_TITLE = "Support for a public\noption in Medicare"),
  
  # Lower Medicare age
  print_rdd_plot(cces_model, "lower_age",
                 TITLE   = "Pooled polls from 2019-2020",
                 Y_TITLE = "Support for lowering\nMedicare age to 50"),
  
  # Drug negotiation
  print_rdd_plot(cces_model, "drug_neg",
                 TITLE   = "Pooled polls from 2020 & 2022",
                 Y_TITLE = "Support for government\nnegotiation of drug prices"),
  
  # Cosmetic modifications
  nrow=2, scale=0.975, labels=c("A.", "B.", "C.", "D.", "E.")
)

# Export figure
ggsave(plot=plot_all, file="Exhibits/RDD figures, main.pdf",
       width=9.5, height=5.75, units='in', dpi=600)

##############################################################################
# Main models: Local linear RD regressions
##############################################################################

# Treat year as factor
cces_model$year <- as.factor(cces_model$year)

# Main RD model: Public insurance
model_ins_uw <- rdrobust(cces_model$public_ins, cces_model$age, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year))
summary(model_ins_uw)
model_ins_wt <- rdrobust(cces_model$public_ins, cces_model$age, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year),
                         weights=cces_model$commonweight)
summary(model_ins_wt)

# Main RD model: Medicare for All
model_m4a_uw <- rdrobust(cces_model$support_m4a, cces_model$age, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year))
summary(model_m4a_uw)
model_m4a_wt <- rdrobust(cces_model$support_m4a, cces_model$age, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year),
                         weights=cces_model$commonweight)
summary(model_m4a_wt)

# Main RD model: Public option
model_opt_uw <- rdrobust(cces_model$public_option, cces_model$age, c=THRESHOLD,
                         p=1, kernel="triangular")
summary(model_opt_uw)
model_opt_wt <- rdrobust(cces_model$public_option, cces_model$age, c=THRESHOLD,
                         p=1, kernel="triangular",
                         weights=cces_model$commonweight)
summary(model_opt_wt)

# Main RD model: Lower Medicare age
model_low_uw <- rdrobust(cces_model$lower_age, cces_model$age, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year))
summary(model_low_uw)
model_low_wt <- rdrobust(cces_model$lower_age, cces_model$age, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year),
                         weights=cces_model$commonweight)
summary(model_low_wt)

# Main RD model: Drug negotiation
model_neg_uw <- rdrobust(cces_model$drug_neg, cces_model$age, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year))
summary(model_neg_uw)
model_neg_wt <- rdrobust(cces_model$drug_neg, cces_model$age, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year),
                         weights=cces_model$commonweight)
summary(model_neg_wt)

##############################################################################
# Robustness models: Instrumental variables
##############################################################################

# Not included in main paper or additional analyses given concerns re:
# exclusion restriction and imperfect measure of Medicare uptake

# Robustness IV model: Medicare for All
model_m4a_iv <- rdrobust(y=cces_model$support_m4a, x=cces_model$age,
                         fuzzy=cces_model$public_ins, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year))
summary(model_m4a_iv)

# Robustness IV model: Public option
model_opt_iv <- rdrobust(y=cces_model$public_option, x=cces_model$age,
                         fuzzy=cces_model$public_ins, c=THRESHOLD,
                         p=1, kernel="triangular")
summary(model_opt_iv)

# Robustness IV model: Lower Medicare age
model_low_iv <- rdrobust(y=cces_model$lower_age, x=cces_model$age,
                         fuzzy=cces_model$public_ins, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year))
summary(model_low_iv)

# Robustness IV model: Drug negotiation
model_neg_iv <- rdrobust(y=cces_model$drug_neg, x=cces_model$age,
                         fuzzy=cces_model$public_ins, c=THRESHOLD,
                         p=1, kernel="triangular", covs=cbind(cces_model$year))
summary(model_neg_iv)

##############################################################################
# Robustness graphs: Demographic discontinuities
##############################################################################

# Graph: Discontinuity in survey weights at cutoff
plot_weight <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
  # Binned scatterplots for each side of 65
  stat_binmean(n=1000, data=subset(cces_model, age < 65), aes(x=age, y=commonweight)) +
  stat_binmean(n=1000, data=subset(cces_model, age > 65), aes(x=age, y=commonweight)) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Mean CES survey weight") +
  coord_cartesian(xlim=c(45,85), ylim=c(0.6,1.4)) +
  labs(caption="Pooled surveys from 2018-2022") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color="light gray", linewidth=0.5),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25),
        plot.caption = element_text(face = "italic", hjust=0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 2, 0.2))

# Export figure
ggsave(plot=plot_weight, file="Exhibits/RDD figures, weighting.pdf",
       width=3.5, height=3, units='in', dpi=600)

# Generate weighted demographics by age
library(survey)
design <- svydesign(data=cces_model, weights=~commonweight, id=~1)
table <- svyby(~gender_two + race_two + marital_two + edu_two,
               ~age, design, svymean, na.rm=TRUE, vartype="ci")

# Graph: Unweighted gender discontinuity
plot_gnd_uw <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
  # Binned scatterplots for each side of 65
  stat_binmean(n=1000, data=subset(cces_model, age < 65), aes(x=age, y=gender_two)) +
  stat_binmean(n=1000, data=subset(cces_model, age > 65), aes(x=age, y=gender_two)) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Unweighted proportion\nof men respondents") +
  coord_cartesian(xlim=c(45,85), ylim=c(0,1)) +
  labs(caption="Pooled surveys from 2018-2022") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color="light gray", linewidth=0.5),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25),
        plot.caption = element_text(face = "italic", hjust=0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Graph: Weighted gender discontinuity
plot_gnd_wt <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
  # Weighted proportions at each age
  geom_point(data=table, aes(x=age, y=gender_two)) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Weighted proportion\nof men respondents") +
  coord_cartesian(xlim=c(45,85), ylim=c(0,1)) +
  labs(caption="Pooled surveys from 2018-2022") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color="light gray", linewidth=0.5),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25),
        plot.caption = element_text(face = "italic", hjust=0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Graph: Unweighted race discontinuity
plot_rac_uw <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
  # Binned scatterplots for each side of 65
  stat_binmean(n=1000, data=subset(cces_model, age < 65), aes(x=age, y=race_two)) +
  stat_binmean(n=1000, data=subset(cces_model, age > 65), aes(x=age, y=race_two)) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Unweighted proportion\nof white respondents") +
  coord_cartesian(xlim=c(45,85), ylim=c(0,1)) +
  labs(caption="Pooled surveys from 2018-2022") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color="light gray", linewidth=0.5),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25),
        plot.caption = element_text(face = "italic", hjust=0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Graph: Weighted race discontinuity
plot_rac_wt <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
  # Weighted proportions at each age
  geom_point(data=table, aes(x=age, y=race_two)) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Weighted proportion\nof white respondents") +
  coord_cartesian(xlim=c(45,85), ylim=c(0,1)) +
  labs(caption="Pooled surveys from 2018-2022") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color="light gray", linewidth=0.5),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25),
        plot.caption = element_text(face = "italic", hjust=0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Graph: Unweighted marital status discontinuity
plot_mar_uw <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
  # Binned scatterplots for each side of 65
  stat_binmean(n=1000, data=subset(cces_model, age < 65), aes(x=age, y=marital_two)) +
  stat_binmean(n=1000, data=subset(cces_model, age > 65), aes(x=age, y=marital_two)) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Unweighted proportion\nof married respondents") +
  coord_cartesian(xlim=c(45,85), ylim=c(0,1)) +
  labs(caption="Pooled surveys from 2018-2022") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color="light gray", linewidth=0.5),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25),
        plot.caption = element_text(face = "italic", hjust=0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Graph: Weighted marital status discontinuity
plot_mar_wt <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
  # Weighted proportions at each age
  geom_point(data=table, aes(x=age, y=marital_two)) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Weighted proportion\nof married respondents") +
  coord_cartesian(xlim=c(45,85), ylim=c(0,1)) +
  labs(caption="Pooled surveys from 2018-2022") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color="light gray", linewidth=0.5),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25),
        plot.caption = element_text(face = "italic", hjust=0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Graph: Unweighted education discontinuity
plot_edu_uw <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
  # Binned scatterplots for each side of 65
  stat_binmean(n=1000, data=subset(cces_model, age < 65), aes(x=age, y=edu_two)) +
  stat_binmean(n=1000, data=subset(cces_model, age > 65), aes(x=age, y=edu_two)) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Unweighted proportion\nwith college degree") +
  coord_cartesian(xlim=c(45,85), ylim=c(0,1)) +
  labs(caption="Pooled surveys from 2018-2022") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color="light gray", linewidth=0.5),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25),
        plot.caption = element_text(face = "italic", hjust=0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Graph: Weighted education discontinuity
plot_edu_wt <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
  # Weighted proportions at each age
  geom_point(data=table, aes(x=age, y=edu_two)) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Weighted proportion\nwith college degree") +
  coord_cartesian(xlim=c(45,85), ylim=c(0,1)) +
  labs(caption="Pooled surveys from 2018-2022") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_line(color="light gray", linewidth=0.5),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25),
        plot.caption = element_text(face = "italic", hjust=0.5)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Combine demographic figures
plot_demo <- plot_grid(plot_gnd_uw, plot_gnd_wt,
                       plot_rac_uw, plot_rac_wt,
                       plot_mar_uw, plot_mar_wt,
                       plot_edu_uw, plot_edu_wt, nrow=4)

# Export figure
ggsave(plot=plot_demo, file="Exhibits/RDD figures, demographics.pdf",
       width=6, height=10, units='in', dpi=600)

##############################################################################
# Robustness models: Gender subgroups
##############################################################################

# Subset by men and women
cces_men <- subset(cces_model, gender_cl == "Male")
cces_wmn <- subset(cces_model, gender_cl == "Female")

# Generate RDD figures: Men
plot_men <- plot_grid(
  
  # Public insurance
  print_rdd_plot(cces_men, "public_ins",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Public insurance coverage\n(e.g. Medicare or Medicaid)"),
  
  # Medicare for All
  print_rdd_plot(cces_men, "support_m4a",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Support for expanding\nMedicare to all Americans"),
  
  # Public option
  print_rdd_plot(cces_men, "public_option",
                 TITLE   = "Only polled in 2019",
                 Y_TITLE = "Support for a public\noption in Medicare"),
  
  # Lower Medicare age
  print_rdd_plot(cces_men, "lower_age",
                 TITLE   = "Pooled polls from 2019-2020",
                 Y_TITLE = "Support for lowering\nMedicare age to 50"),
  
  # Drug negotiation
  print_rdd_plot(cces_men, "drug_neg",
                 TITLE   = "Pooled polls from 2020 & 2022",
                 Y_TITLE = "Support for government\nnegotiation of drug prices"),
  
  # Cosmetic modifications
  nrow=2, scale=0.975
)

# Generate RDD figures: Women
plot_wmn <- plot_grid(
  
  # Public insurance
  print_rdd_plot(cces_wmn, "public_ins",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Public insurance coverage\n(e.g. Medicare or Medicaid)"),
  
  # Medicare for All
  print_rdd_plot(cces_wmn, "support_m4a",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Support for expanding\nMedicare to all Americans"),
  
  # Public option
  print_rdd_plot(cces_wmn, "public_option",
                 TITLE   = "Only polled in 2019",
                 Y_TITLE = "Support for a public\noption in Medicare"),
  
  # Lower Medicare age
  print_rdd_plot(cces_wmn, "lower_age",
                 TITLE   = "Pooled polls from 2019-2020",
                 Y_TITLE = "Support for lowering\nMedicare age to 50"),
  
  # Drug negotiation
  print_rdd_plot(cces_wmn, "drug_neg",
                 TITLE   = "Pooled polls from 2020 & 2022",
                 Y_TITLE = "Support for government\nnegotiation of drug prices"),
  
  # Cosmetic modifications
  nrow=2, scale=0.975
)

# Combine men and women figures
plot_gender <- plot_grid(plot_men, plot_wmn, labels=c("Men", "Women"),
                         scale=0.9, label_x=0.5, hjust=0.5, nrow=1)

# Export figure
ggsave(plot=plot_gender, file="Exhibits/RDD figures, gender.pdf",
       width=18, height=5.35, units='in', dpi=600)

# Robustness RD models for men
# Robustness RD model: Public insurance for men
model_ins_men_uw <- rdrobust(cces_men$public_ins, cces_men$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_men$year))
summary(model_ins_men_uw)
model_ins_men_wt <- rdrobust(cces_men$public_ins, cces_men$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_men$year),
                             weights=cces_men$commonweight)
summary(model_ins_men_wt)

# Robustness RD model: Medicare for All for men
model_m4a_men_uw <- rdrobust(cces_men$support_m4a, cces_men$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_men$year))
summary(model_m4a_men_uw)
model_m4a_men_wt <- rdrobust(cces_men$support_m4a, cces_men$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_men$year),
                             weights=cces_men$commonweight)
summary(model_m4a_men_wt)

# Robustness RD model: Public option for men
model_opt_men_uw <- rdrobust(cces_men$public_option, cces_men$age, c=THRESHOLD,
                             p=1, kernel="triangular")
summary(model_opt_men_uw)
model_opt_men_wt <- rdrobust(cces_men$public_option, cces_men$age, c=THRESHOLD,
                             p=1, kernel="triangular",
                             weights=cces_men$commonweight)
summary(model_opt_men_wt)

# Robustness RD model: Lower Medicare age for men
model_low_men_uw <- rdrobust(cces_men$lower_age, cces_men$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_men$year))
summary(model_low_men_uw)
model_low_men_wt <- rdrobust(cces_men$lower_age, cces_men$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_men$year),
                             weights=cces_men$commonweight)
summary(model_low_men_wt)

# Robustness RD model: Drug negotiation for men
model_neg_men_uw <- rdrobust(cces_men$drug_neg, cces_men$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_men$year))
summary(model_neg_men_uw)
model_neg_men_wt <- rdrobust(cces_men$drug_neg, cces_men$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_men$year),
                             weights=cces_men$commonweight)
summary(model_neg_men_wt)

# Robustness RD models for women
# Robustness RD model: Public insurance for women
model_ins_wmn_uw <- rdrobust(cces_wmn$public_ins, cces_wmn$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wmn$year))
summary(model_ins_wmn_uw)
model_ins_wmn_wt <- rdrobust(cces_wmn$public_ins, cces_wmn$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wmn$year),
                             weights=cces_wmn$commonweight)
summary(model_ins_wmn_wt)

# Robustness RD model: Medicare for All for women
model_m4a_wmn_uw <- rdrobust(cces_wmn$support_m4a, cces_wmn$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wmn$year))
summary(model_m4a_wmn_uw)
model_m4a_wmn_wt <- rdrobust(cces_wmn$support_m4a, cces_wmn$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wmn$year),
                             weights=cces_wmn$commonweight)
summary(model_m4a_wmn_wt)

# Robustness RD model: Public option for women
model_opt_wmn_uw <- rdrobust(cces_wmn$public_option, cces_wmn$age, c=THRESHOLD,
                             p=1, kernel="triangular")
summary(model_opt_wmn_uw)
model_opt_wmn_wt <- rdrobust(cces_wmn$public_option, cces_wmn$age, c=THRESHOLD,
                             p=1, kernel="triangular",
                             weights=cces_wmn$commonweight)
summary(model_opt_wmn_wt)

# Robustness RD model: Lower Medicare age for women
model_low_wmn_uw <- rdrobust(cces_wmn$lower_age, cces_wmn$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wmn$year))
summary(model_low_wmn_uw)
model_low_wmn_wt <- rdrobust(cces_wmn$lower_age, cces_wmn$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wmn$year),
                             weights=cces_wmn$commonweight)
summary(model_low_wmn_wt)

# Robustness RD model: Drug negotiation for women
model_neg_wmn_uw <- rdrobust(cces_wmn$drug_neg, cces_wmn$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wmn$year))
summary(model_neg_wmn_uw)
model_neg_wmn_wt <- rdrobust(cces_wmn$drug_neg, cces_wmn$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wmn$year),
                             weights=cces_wmn$commonweight)
summary(model_neg_wmn_wt)

##############################################################################
# Robustness models: Race/ethnicity subgroups
##############################################################################

# Subset by race/ethnicity
cces_wht <- subset(cces_model, race_cl == "White")
cces_nwt <- subset(cces_model, race_cl != "White")

# Generate RDD figures: White
plot_wht <- plot_grid(
  
  # Public insurance
  print_rdd_plot(cces_wht, "public_ins",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Public insurance coverage\n(e.g. Medicare or Medicaid)"),
  
  # Medicare for All
  print_rdd_plot(cces_wht, "support_m4a",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Support for expanding\nMedicare to all Americans"),
  
  # Public option
  print_rdd_plot(cces_wht, "public_option",
                 TITLE   = "Only polled in 2019",
                 Y_TITLE = "Support for a public\noption in Medicare"),
  
  # Lower Medicare age
  print_rdd_plot(cces_wht, "lower_age",
                 TITLE   = "Pooled polls from 2019-2020",
                 Y_TITLE = "Support for lowering\nMedicare age to 50"),
  
  # Drug negotiation
  print_rdd_plot(cces_wht, "drug_neg",
                 TITLE   = "Pooled polls from 2020 & 2022",
                 Y_TITLE = "Support for government\nnegotiation of drug prices"),
  
  # Cosmetic modifications
  nrow=2, scale=0.975
)

# Generate RDD figures: Non-white
plot_nwt <- plot_grid(
  
  # Public insurance
  print_rdd_plot(cces_nwt, "public_ins",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Public insurance coverage\n(e.g. Medicare or Medicaid)"),
  
  # Medicare for All
  print_rdd_plot(cces_nwt, "support_m4a",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Support for expanding\nMedicare to all Americans"),
  
  # Public option
  print_rdd_plot(cces_nwt, "public_option",
                 TITLE   = "Only polled in 2019",
                 Y_TITLE = "Support for a public\noption in Medicare"),
  
  # Lower Medicare age
  print_rdd_plot(cces_nwt, "lower_age",
                 TITLE   = "Pooled polls from 2019-2020",
                 Y_TITLE = "Support for lowering\nMedicare age to 50"),
  
  # Drug negotiation
  print_rdd_plot(cces_nwt, "drug_neg",
                 TITLE   = "Pooled polls from 2020 & 2022",
                 Y_TITLE = "Support for government\nnegotiation of drug prices"),
  
  # Cosmetic modifications
  nrow=2, scale=0.975
)

# Combine white and non-white figures
plot_race <- plot_grid(plot_wht, plot_nwt, labels=c("White", "Non-white"),
                       scale=0.9, label_x=0.5, hjust=0.5, nrow=1)

# Export figure
ggsave(plot=plot_race, file="Exhibits/RDD figures, race.pdf",
       width=18, height=5.35, units='in', dpi=600)

# Robustness RD models for white respondents
# Robustness RD model: Public insurance for white
model_ins_wht_uw <- rdrobust(cces_wht$public_ins, cces_wht$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wht$year))
summary(model_ins_wht_uw)
model_ins_wht_wt <- rdrobust(cces_wht$public_ins, cces_wht$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wht$year),
                             weights=cces_wht$commonweight)
summary(model_ins_wht_wt)

# Robustness RD model: Medicare for All for white
model_m4a_wht_uw <- rdrobust(cces_wht$support_m4a, cces_wht$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wht$year))
summary(model_m4a_wht_uw)
model_m4a_wht_wt <- rdrobust(cces_wht$support_m4a, cces_wht$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wht$year),
                             weights=cces_wht$commonweight)
summary(model_m4a_wht_wt)

# Robustness RD model: Public option for white
model_opt_wht_uw <- rdrobust(cces_wht$public_option, cces_wht$age, c=THRESHOLD,
                             p=1, kernel="triangular")
summary(model_opt_wht_uw)
model_opt_wht_wt <- rdrobust(cces_wht$public_option, cces_wht$age, c=THRESHOLD,
                             p=1, kernel="triangular",
                             weights=cces_wht$commonweight)
summary(model_opt_wht_wt)

# Robustness RD model: Lower Medicare age for white
model_low_wht_uw <- rdrobust(cces_wht$lower_age, cces_wht$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wht$year))
summary(model_low_wht_uw)
model_low_wht_wt <- rdrobust(cces_wht$lower_age, cces_wht$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wht$year),
                             weights=cces_wht$commonweight)
summary(model_low_wht_wt)

# Robustness RD model: Drug negotiation for white
model_neg_wht_uw <- rdrobust(cces_wht$drug_neg, cces_wht$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wht$year))
summary(model_neg_wht_uw)
model_neg_wht_wt <- rdrobust(cces_wht$drug_neg, cces_wht$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_wht$year),
                             weights=cces_wht$commonweight)
summary(model_neg_wht_wt)

# Robustness RD models for non-white respondents
# Robustness RD model: Public insurance for non-white
model_ins_nwt_uw <- rdrobust(cces_nwt$public_ins, cces_nwt$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_nwt$year))
summary(model_ins_nwt_uw)
model_ins_nwt_wt <- rdrobust(cces_nwt$public_ins, cces_nwt$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_nwt$year),
                             weights=cces_nwt$commonweight)
summary(model_ins_nwt_wt)

# Robustness RD model: Medicare for All for non-white
model_m4a_nwt_uw <- rdrobust(cces_nwt$support_m4a, cces_nwt$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_nwt$year))
summary(model_m4a_nwt_uw)
model_m4a_nwt_wt <- rdrobust(cces_nwt$support_m4a, cces_nwt$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_nwt$year),
                             weights=cces_nwt$commonweight)
summary(model_m4a_nwt_wt)

# Robustness RD model: Public option for non-white
model_opt_nwt_uw <- rdrobust(cces_nwt$public_option, cces_nwt$age, c=THRESHOLD,
                             p=1, kernel="triangular")
summary(model_opt_nwt_uw)
model_opt_nwt_wt <- rdrobust(cces_nwt$public_option, cces_nwt$age, c=THRESHOLD,
                             p=1, kernel="triangular",
                             weights=cces_nwt$commonweight)
summary(model_opt_nwt_wt)

# Robustness RD model: Lower Medicare age for non-white
model_low_nwt_uw <- rdrobust(cces_nwt$lower_age, cces_nwt$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_nwt$year))
summary(model_low_nwt_uw)
model_low_nwt_wt <- rdrobust(cces_nwt$lower_age, cces_nwt$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_nwt$year),
                             weights=cces_nwt$commonweight)
summary(model_low_nwt_wt)

# Robustness RD model: Drug negotiation for non-white
model_neg_nwt_uw <- rdrobust(cces_nwt$drug_neg, cces_nwt$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_nwt$year))
summary(model_neg_nwt_uw)
model_neg_nwt_wt <- rdrobust(cces_nwt$drug_neg, cces_nwt$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_nwt$year),
                             weights=cces_nwt$commonweight)
summary(model_neg_nwt_wt)

##############################################################################
# Robustness models: Party identification
##############################################################################

# Subset by Democrats, Republicans, independents/others
cces_dem <- subset(cces_model, pid_cl == "Democrat")
cces_rep <- subset(cces_model, pid_cl == "Republican")
cces_ind <- subset(cces_model, pid_cl == "Independent, other, or not sure")

# Generate RDD figures: Democrats
plot_dem <- plot_grid(
  
  # Public insurance
  print_rdd_plot(cces_dem, "public_ins",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Public insurance coverage\n(e.g. Medicare or Medicaid)"),
  
  # Medicare for All
  print_rdd_plot(cces_dem, "support_m4a",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Support for expanding\nMedicare to all Americans"),
  
  # Public option
  print_rdd_plot(cces_dem, "public_option",
                 TITLE   = "Only polled in 2019",
                 Y_TITLE = "Support for a public\noption in Medicare"),
  
  # Lower Medicare age
  print_rdd_plot(cces_dem, "lower_age",
                 TITLE   = "Pooled polls from 2019-2020",
                 Y_TITLE = "Support for lowering\nMedicare age to 50"),
  
  # Drug negotiation
  print_rdd_plot(cces_dem, "drug_neg",
                 TITLE   = "Pooled polls from 2020 & 2022",
                 Y_TITLE = "Support for government\nnegotiation of drug prices"),
  
  # Cosmetic modifications
  nrow=2, scale=0.975
)

# Generate RDD figures: Republicans
plot_rep <- plot_grid(
  
  # Public insurance
  print_rdd_plot(cces_rep, "public_ins",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Public insurance coverage\n(e.g. Medicare or Medicaid)"),
  
  # Medicare for All
  print_rdd_plot(cces_rep, "support_m4a",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Support for expanding\nMedicare to all Americans"),
  
  # Public option
  print_rdd_plot(cces_rep, "public_option",
                 TITLE   = "Only polled in 2019",
                 Y_TITLE = "Support for a public\noption in Medicare"),
  
  # Lower Medicare age
  print_rdd_plot(cces_rep, "lower_age",
                 TITLE   = "Pooled polls from 2019-2020",
                 Y_TITLE = "Support for lowering\nMedicare age to 50"),
  
  # Drug negotiation
  print_rdd_plot(cces_rep, "drug_neg",
                 TITLE   = "Pooled polls from 2020 & 2022",
                 Y_TITLE = "Support for government\nnegotiation of drug prices"),
  
  # Cosmetic modifications
  nrow=2, scale=0.975
)

# Generate RDD figures: Independents/others
plot_ind <- plot_grid(
  
  # Public insurance
  print_rdd_plot(cces_ind, "public_ins",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Public insurance coverage\n(e.g. Medicare or Medicaid)"),
  
  # Medicare for All
  print_rdd_plot(cces_ind, "support_m4a",
                 TITLE   = "Pooled polls from 2018-2022",
                 Y_TITLE = "Support for expanding\nMedicare to all Americans"),
  
  # Public option
  print_rdd_plot(cces_ind, "public_option",
                 TITLE   = "Only polled in 2019",
                 Y_TITLE = "Support for a public\noption in Medicare"),
  
  # Lower Medicare age
  print_rdd_plot(cces_ind, "lower_age",
                 TITLE   = "Pooled polls from 2019-2020",
                 Y_TITLE = "Support for lowering\nMedicare age to 50"),
  
  # Drug negotiation
  print_rdd_plot(cces_ind, "drug_neg",
                 TITLE   = "Pooled polls from 2020 & 2022",
                 Y_TITLE = "Support for government\nnegotiation of drug prices"),
  
  # Cosmetic modifications
  nrow=2, scale=0.975
)

# Combine party identification figures
plot_pid <- plot_grid(plot_dem, plot_rep, plot_ind,
                      labels=c("Democrats", "Republicans", "Independents, other, or not sure"),
                      scale=0.9, label_x=0.5, hjust=0.5, nrow=2)

# Export figure
ggsave(plot=plot_pid, file="Exhibits/RDD figures, party identification.pdf",
       width=18, height=10.7, units='in', dpi=600)

# Robustness RD models for Democrats
# Robustness RD model: Public insurance for Democrats
model_ins_dem_uw <- rdrobust(cces_dem$public_ins, cces_dem$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_dem$year))
summary(model_ins_dem_uw)
model_ins_dem_wt <- rdrobust(cces_dem$public_ins, cces_dem$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_dem$year),
                             weights=cces_dem$commonweight)
summary(model_ins_dem_wt)

# Robustness RD model: Medicare for All for Democrats
model_m4a_dem_uw <- rdrobust(cces_dem$support_m4a, cces_dem$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_dem$year))
summary(model_m4a_dem_uw)
model_m4a_dem_wt <- rdrobust(cces_dem$support_m4a, cces_dem$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_dem$year),
                             weights=cces_dem$commonweight)
summary(model_m4a_dem_wt)

# Robustness RD model: Public option for Democrats
model_opt_dem_uw <- rdrobust(cces_dem$public_option, cces_dem$age, c=THRESHOLD,
                             p=1, kernel="triangular")
summary(model_opt_dem_uw)
model_opt_dem_wt <- rdrobust(cces_dem$public_option, cces_dem$age, c=THRESHOLD,
                             p=1, kernel="triangular",
                             weights=cces_dem$commonweight)
summary(model_opt_dem_wt)

# Robustness RD model: Lower Medicare age for Democrats
model_low_dem_uw <- rdrobust(cces_dem$lower_age, cces_dem$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_dem$year))
summary(model_low_dem_uw)
model_low_dem_wt <- rdrobust(cces_dem$lower_age, cces_dem$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_dem$year),
                             weights=cces_dem$commonweight)
summary(model_low_dem_wt)

# Robustness RD model: Drug negotiation for Democrats
model_neg_dem_uw <- rdrobust(cces_dem$drug_neg, cces_dem$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_dem$year))
summary(model_neg_dem_uw)
model_neg_dem_wt <- rdrobust(cces_dem$drug_neg, cces_dem$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_dem$year),
                             weights=cces_dem$commonweight)
summary(model_neg_dem_wt)

# Robustness RD models for Republicans
# Robustness RD model: Public insurance for Republicans
model_ins_rep_uw <- rdrobust(cces_rep$public_ins, cces_rep$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_rep$year))
summary(model_ins_rep_uw)
model_ins_rep_wt <- rdrobust(cces_rep$public_ins, cces_rep$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_rep$year),
                             weights=cces_rep$commonweight)
summary(model_ins_rep_wt)

# Robustness RD model: Medicare for All for Republicans
model_m4a_rep_uw <- rdrobust(cces_rep$support_m4a, cces_rep$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_rep$year))
summary(model_m4a_rep_uw)
model_m4a_rep_wt <- rdrobust(cces_rep$support_m4a, cces_rep$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_rep$year),
                             weights=cces_rep$commonweight)
summary(model_m4a_rep_wt)

# Robustness RD model: Public option for Republicans
model_opt_rep_uw <- rdrobust(cces_rep$public_option, cces_rep$age, c=THRESHOLD,
                             p=1, kernel="triangular")
summary(model_opt_rep_uw)
model_opt_rep_wt <- rdrobust(cces_rep$public_option, cces_rep$age, c=THRESHOLD,
                             p=1, kernel="triangular",
                             weights=cces_rep$commonweight)
summary(model_opt_rep_wt)

# Robustness RD model: Lower Medicare age for Republicans
model_low_rep_uw <- rdrobust(cces_rep$lower_age, cces_rep$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_rep$year))
summary(model_low_rep_uw)
model_low_rep_wt <- rdrobust(cces_rep$lower_age, cces_rep$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_rep$year),
                             weights=cces_rep$commonweight)
summary(model_low_rep_wt)

# Robustness RD model: Drug negotiation for Republicans
model_neg_rep_uw <- rdrobust(cces_rep$drug_neg, cces_rep$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_rep$year))
summary(model_neg_rep_uw)
model_neg_rep_wt <- rdrobust(cces_rep$drug_neg, cces_rep$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_rep$year),
                             weights=cces_rep$commonweight)
summary(model_neg_rep_wt)

# Robustness RD models for independents/others
# Robustness RD model: Public insurance for independents
model_ins_ind_uw <- rdrobust(cces_ind$public_ins, cces_ind$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_ind$year))
summary(model_ins_ind_uw)
model_ins_ind_wt <- rdrobust(cces_ind$public_ins, cces_ind$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_ind$year),
                             weights=cces_ind$commonweight)
summary(model_ins_ind_wt)

# Robustness RD model: Medicare for All for independents
model_m4a_ind_uw <- rdrobust(cces_ind$support_m4a, cces_ind$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_ind$year))
summary(model_m4a_ind_uw)
model_m4a_ind_wt <- rdrobust(cces_ind$support_m4a, cces_ind$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_ind$year),
                             weights=cces_ind$commonweight)
summary(model_m4a_ind_wt)

# Robustness RD model: Public option for independents
model_opt_ind_uw <- rdrobust(cces_ind$public_option, cces_ind$age, c=THRESHOLD,
                             p=1, kernel="triangular")
summary(model_opt_ind_uw)
model_opt_ind_wt <- rdrobust(cces_ind$public_option, cces_ind$age, c=THRESHOLD,
                             p=1, kernel="triangular",
                             weights=cces_ind$commonweight)
summary(model_opt_ind_wt)

# Robustness RD model: Lower Medicare age for independents
model_low_ind_uw <- rdrobust(cces_ind$lower_age, cces_ind$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_ind$year))
summary(model_low_ind_uw)
model_low_ind_wt <- rdrobust(cces_ind$lower_age, cces_ind$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_ind$year),
                             weights=cces_ind$commonweight)
summary(model_low_ind_wt)

# Robustness RD model: Drug negotiation for independents
model_neg_ind_uw <- rdrobust(cces_ind$drug_neg, cces_ind$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_ind$year))
summary(model_neg_ind_uw)
model_neg_ind_wt <- rdrobust(cces_ind$drug_neg, cces_ind$age, c=THRESHOLD,
                             p=1, kernel="triangular", covs=cbind(cces_ind$year),
                             weights=cces_ind$commonweight)
summary(model_neg_ind_wt)
