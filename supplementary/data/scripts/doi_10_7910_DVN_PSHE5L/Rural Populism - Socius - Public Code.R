library(lavaan)
library(tidyverse)
library(semTools)
library(semPlot)
library(xtable)
library(gt)

# Load data
rural <- read_csv(file = "Downloads/320-942_Rural_West_COVID-19_Survey_4.5.21.csv")
# Change all the "." to NAs
rural[] <- lapply(rural, function(x) if(is.character(x)) ifelse(x == ".", NA, x) else x)
# See how many classes of columns there are
table(sapply(rural, class)) # character (32), numeric (85)
# Change "RAW_" columns from character to numeric

# Rescale 100 scale (RAW) column to out of 7 to align with other data scales.
cols_to_convert <- paste0("RAW_Q24", LETTERS[1:9])
# Apply the transformation
rural <- rural %>%
  mutate(across(all_of(cols_to_convert), ~ as.numeric(.)))
# Check Class
class(rural$RAW_Q24C)
# Never Heard Of ("NHO") values in RAW columns are NAs

# # Standardize the scores
# rural <- rural %>%
#   mutate(
#     RAW_Q24A = scale(RAW_Q24A, center = TRUE, scale = TRUE),
#     RAW_Q24B = scale(RAW_Q24B, center = TRUE, scale = TRUE),
#     RAW_Q24C = scale(RAW_Q24C, center = TRUE, scale = TRUE),
#     RAW_Q24D = scale(RAW_Q24D, center = TRUE, scale = TRUE),
#     RAW_Q24E = scale(RAW_Q24E, center = TRUE, scale = TRUE),
#     RAW_Q24F = scale(RAW_Q24F, center = TRUE, scale = TRUE),
#     RAW_Q24G = scale(RAW_Q24G, center = TRUE, scale = TRUE),
#     RAW_Q24H = scale(RAW_Q24H, center = TRUE, scale = TRUE),
#     RAW_Q24I = scale(RAW_Q24I, center = TRUE, scale = TRUE)
#   ) # This might not be used, since its for the radical groups support.

# ------------------------------------------------------------------------------
# Generating a Socioeconomic Status (SES) Measurement

# Clean EDUCATION variable. It is already ordered low 1 - 5 high. The only thing
# to do is turn values of 6 (No answer) to NAs.
# rural$QF[rural$QF == 6] <- NA

# Make has college degree variable
rural <- rural %>%
  mutate(is_college = if_else(QF %in% c(4, 5), 1, 0))
table(rural$is_college)

# # Clean INCOME variable. 
# rural$Q20[rural$Q20 == 7] <- NA
# # Recode income from 1-6 to actual estimates.
# rural <- rural %>%
#   mutate(Q20 = case_when(
#     Q20 == 1 ~ 15000,
#     Q20 == 2 ~ 45000,
#     Q20 == 3 ~ 75000,
#     Q20 == 4 ~ 105000,
#     Q20 == 5 ~ 135000,
#     Q20 == 6 ~ 175000
#   ))

# # Clean EMPLOYMENT STATUS.
# rural$Q7C[rural$Q7C == 9] <- NA
# rural <- rural %>%
#   mutate(Q7C = case_when(
#     Q7C == 1 ~ 3, # Full time
#     Q7C == 2 ~ 2, # Part time
#     Q7C %in% c(3, 4, 5, 6, 7, 8) ~ 1 # Not Employed or Other
#   ))

# # Clean HOUSING STABILITY.
# rural$Q10[rural$Q10 == 3] <- NA
# rural <- rural %>%
#   mutate(Q10 = case_when(
#     Q10 == 1 ~ 1, # Stable
#     Q10 == 2 ~ 0  # Unstable
#   ))


# # Standardize the scores
# rural <- rural %>%
#   mutate(
#     QF = scale(QF, center = TRUE, scale = TRUE),
#     Q20 = scale(Q20, center = TRUE, scale = TRUE),
#     Q7C = scale(Q7C, center = TRUE, scale = TRUE),
#     Q10 = scale(Q10, center = TRUE, scale = TRUE)
#   )

# # Calculate composite SES score
# rural <- rural %>%
#   mutate(SES_score = rowMeans(select(., QF, Q20, Q7C, Q10), na.rm = TRUE))
# 
# summary(rural$SES_score)

# ------------------------------------------------------------------------------
# Here we generate a lot of dummy variables, transforming categorical questions
# into binary variables. An excluded REFERENCE CATEGORY is needed so the model
# uses it as the baseline against which other categories are compared.
# Including all categories can lead to perfect multicollinearity, where the
# sum of all dummy variables equals one, causing issues in regression models.

# # Create dummy variables for each race category except the reference category (e.g., "White")
# rural$QE[rural$QE == 8] <- NA
# rural <- rural %>%
#   mutate(
#     #White = ifelse(QE == "1", 1, 0)), # Reference Group
#     Black = ifelse(QE == "2", 1, 0),
#     Asian = ifelse(QE == "3", 1, 0),
#     Native = ifelse(QE == "4", 1, 0),
#     Pacific = ifelse(QE == "5", 1, 0),
#     Mixed = ifelse(QE == "6", 1, 0),
#     Other_Race = ifelse(QE == "7", 1, 0)
#   )


# Assuming "DK/NA" means "Don't know / No answer" and is treated as missing data,
# it will be 0 in all these new columns unless you decide to handle it differently.

# GENDER
rural$is_male = ifelse(rural$QC == "1", 1, 0) # Reference Group
table(rural$is_male)

# rural$QC[rural$QC == 4] <- NA
# rural$gender_male = ifelse(rural$QC == "1", 1, 0) # Reference Group
# #rural$Female = ifelse(rural$QC == "2", 1, 0)
# rural$Neither_Gender = ifelse(rural$QC == "3" | rural$QC == "4", 1, 0)

# Hispanic
# rural$Hispanic = ifelse(rural$QD == "1", 1, 0)

# Party Politics
rural$XQ14[rural$XQ14 == 4] <- NA
rural$XQ14[rural$XQ14 == 5] <- NA
rural$is_republican = ifelse(rural$XQ14 == "1", 1, 0)
table(rural$is_republican)

# rural$XQ14[rural$XQ14 == 4] <- NA
# rural$XQ14[rural$XQ14 == 5] <- NA
# rural$Republican = ifelse(rural$XQ14 == "1", 1, 0)
# rural$Democrat = ifelse(rural$XQ14 == "2", 1, 0)
# #rural$Independent = ifelse(rural$XQ14 == "3", 1, 0) # Reference Group

# # Translate URBAN_CODE (1-3 is Metro, but this study is only 4-9 Nonmetro)
# rural$URBAN_CODE <- rural$URBAN_CODE + 3
# table(rural$URBAN_CODE)
# # Make codes for adjacency to Metro areas
# rural$metro_adjacent <- ifelse(rural$URBAN_CODE == "4" | rural$URBAN_CODE == "6" | rural$URBAN_CODE == "8", 1, 0)
# rural$metro_nonadjacent <- ifelse(rural$URBAN_CODE == "5" | rural$URBAN_CODE == "7" | rural$URBAN_CODE == "9", 1, 0)
# table(rural$metro_adjacent)
# table(rural$metro_nonadjacent)

# ------------------------------------------------------------------------------
# Deal with NAs and standardization of other variables.

# Clean AGE varaible.
# # Recode income from 1-6 to actual estimates.
# rural$QB[rural$QB == 12] <- NA
# rural <- rural %>%
#   mutate(QB = case_when(
#     QB == 1 ~ 21,
#     QB == 2 ~ 27,
#     QB == 3 ~ 32,
#     QB == 4 ~ 37,
#     QB == 5 ~ 42,
#     QB == 6 ~ 47,
#     QB == 7 ~ 52,
#     QB == 8 ~ 57,
#     QB == 9 ~ 62,
#     QB == 10 ~ 70,
#     QB == 11 ~ 80
#   ))

# AGE (is old)
# Recode QB values and create the is_over65 variable
table(rural$QB)
rural <- rural %>% 
  mutate(is_over65 = if_else(QB %in% c(10, 11), 1, 0))
table(rural$is_over65)
# # Standardize the scores
# rural <- rural %>%
#   mutate(
#     QB = scale(QB, center = TRUE, scale = TRUE)
#   )

# # Clean CONSERVATIVE score.
# rural$Q15[rural$Q15 == 8] <- NA # Liberal 1 - 7 Conservative
# 
# # Make Democract, Republican and Independent Columns
# rural$is_republican <- ifelse(rural$XQ14 == 1, 1, 0)
# rural$is_democrat <- ifelse(rural$XQ14 == 2, 1, 0)
# rural$is_independent <- ifelse(rural$XQ14 == 3, 1, 0)


# ------------------------------------------------------------------------------
# ---------------------------SPECIFYING MODELS----------------------------------
# -------------------------------------------------SCRATCH WAY BELOW------------

# MEASUREMENT MODEL
mediation_model_violence_ctrls1 <- '
  # Measurement part
  polgriev =~ Q23A + Q23B + Q23C + Q23D + Q23E + Q23F
  econgriev =~ Q23I + Q23J + Q23K + Q23H + Q23L
  cultgriev =~ Q23O + Q23G + Q23N + Q23M
  violence =~ Q25B + Q25C + Q25D
  
  # MI Changes
  Q23H ~~ Q23I
  Q23A ~~ Q23B

'

# getting rid of Q23L to test if the validity of economic model is better (didn't change a LOT, but some)
# getting rid of Q23O from cult (env) to see how it affects the model
# STRUCTURAL MODEL
mediation_model_violence_ctrls2 <- '
  # Measurement part
  polgriev =~ Q23A + Q23B + Q23C + Q23D + Q23E + Q23F
  econgriev =~ Q23I + Q23J + Q23K + Q23H + Q23L
  cultgriev =~ Q23O + Q23G + Q23N + Q23M
  violence =~ Q25B + Q25C + Q25D
  
  # Correlated Errors (from MI in Measurement Model)
  Q23H ~~ Q23I
  Q23A ~~ Q23B
  
  # Correlated Errors (from MI in Structural Model)
  Q15 ~~ cultgriev
  Q15 ~~ econgriev
  cultgriev ~~ econgriev

  # Structural part
  violence ~ c*econgriev + d*cultgriev + e*Q15 + b1*polgriev

  polgriev ~ a1*econgriev + a2*cultgriev + a3*Q15 

  # Indirect effects for econgriev
  indirect1_econgriev := a1*b1
  total_indirect_econgriev := indirect1_econgriev
  total_effect_econgriev := c + total_indirect_econgriev

  # Indirect effects for cultgriev
  indirect1_cultgriev := a2*b1
  total_indirect_cultgriev := indirect1_cultgriev
  total_effect_cultgriev := d + total_indirect_cultgriev

  # Indirect effects for Q15
  indirect1_Q15 := a3*b1
  total_indirect_Q15 := indirect1_Q15
  total_effect_Q15 := e + total_indirect_Q15

  # Overall total indirect effect
  overall_total_indirect := total_indirect_econgriev + total_indirect_cultgriev + total_indirect_Q15
  
  # Overall total effect
  overall_total_effect := total_effect_econgriev + total_effect_cultgriev + total_effect_Q15
'


# Alternative Reliability Model with Controls
mediation_model_violence_ctrls4 <- '
  # Measurement part
  polgriev =~ Q23A + Q23B + Q23C + Q23D + Q23E + Q23F
  econgriev =~ Q23I + Q23J + Q23K + Q23H + Q23L
  cultgriev =~ Q23O + Q23G + Q23N + Q23M
  violence =~ Q25B + Q25C + Q25D
  
  # Correlated Errors (from MI in Measurement Model)
  Q23H ~~ Q23I
  Q23A ~~ Q23B
  
  # Correlated Errors (from MI in Structural Model)
  Q15 ~~ cultgriev
  Q15 ~~ econgriev
  cultgriev ~~ econgriev

  # Structural part
  violence ~ c*econgriev + d*cultgriev + e*Q15 + b1*polgriev + is_college + is_over65 + is_male + is_republican

  polgriev ~ a1*econgriev + a2*cultgriev + a3*Q15 + is_college + is_over65 + is_male + is_republican
  
  econgriev ~ is_college + is_over65 + is_male + is_republican
  
  cultgriev ~ is_college + is_over65 + is_male + is_republican

  # Indirect effects for econgriev
  indirect1_econgriev := a1*b1
  total_indirect_econgriev := indirect1_econgriev
  total_effect_econgriev := c + total_indirect_econgriev

  # Indirect effects for cultgriev
  indirect1_cultgriev := a2*b1
  total_indirect_cultgriev := indirect1_cultgriev
  total_effect_cultgriev := d + total_indirect_cultgriev

  # Indirect effects for Q15
  indirect1_Q15 := a3*b1
  total_indirect_Q15 := indirect1_Q15
  total_effect_Q15 := e + total_indirect_Q15

  # Overall total indirect effect
  overall_total_indirect := total_indirect_econgriev + total_indirect_cultgriev + total_indirect_Q15
  
  # Overall total effect
  overall_total_effect := total_effect_econgriev + total_effect_cultgriev + total_effect_Q15
'



# Fit the MEASUREMENT MODEL to your data
measurement <- cfa(mediation_model_violence_ctrls1, data = rural, sampling.weights = "WTS", missing = "ML", estimator = "MLR")
# Summarize the results
summary(measurement, fit.measures = TRUE, standardized = TRUE)
modindices(measurement, sort = TRUE, minimum.value = 10) # Model Indices
residuals(measurement)$cov
reliability(measurement)


# Fit the STRUCTURAL MODEL to your data
structural <- cfa(mediation_model_violence_ctrls2, data = rural, sampling.weights = "WTS", missing = "ML", estimator = "MLR")
# Summarize the results
summary(structural, fit.measures = TRUE, standardized = TRUE)
standardizedsolution(structural)
modindices(structural, sort = TRUE, minimum.value = 10) # Model Indices
residuals(structural)$cov 

# Some estimates
param_estimates <- parameterEstimates(structural)
head(param_estimates)
standard_errors <- param_estimates$se
print(standard_errors)

# Fit the STRUCTURAL MODEL WITH CONTROLS to your data
structural_ctrl <- cfa(mediation_model_violence_ctrls4, data = rural, sampling.weights = "WTS", missing = "ML", estimator = "MLR")
# Summarize the results
summary(structural_ctrl, fit.measures = TRUE, standardized = TRUE)
modindices(structural_ctrl, sort = TRUE, minimum.value = 10) # Model Indices
residuals(structural_ctrl)$cov 


# ------------------------------------------------------------------------------
# Discriminant and Convergent Validity

library(psych)

# Creates the "usual" table
rel <- function(object, CR = TRUE, AVE = TRUE, MSV = TRUE, HTMT = TRUE, digits) {
  cor.lv <- data.frame(lavaan::lavInspect(object, "cor.lv"))
  pt <- lavaan::parTable(object)[lavaan::parTable(object)$op == "=~",]
  mtrx <- round(cor.lv, digits)
  diag(mtrx) <- ""
  if(HTMT) {
    htmt  <- round(semTools::htmt(paste(pt[,"lhs"], pt[,"op"], pt[,"rhs"], sep = ""), data.frame(lavaan::lavInspect(object, "data"))), digits)
    upper <- htmt[upper.tri(htmt)]
  }
  else upper <- ""
  mtrx[upper.tri(mtrx)] <- upper
  if(MSV) mtrx <- cbind(MSV = round(sapply(abs(cor.lv), function(x) max(x[x != max(x)])^2), digits), mtrx)
  if(AVE) mtrx <- cbind(AVE = round(semTools::AVE(object), digits), mtrx)
  if(CR)  mtrx <- cbind(CR = round(semTools::compRelSEM(object), digits), mtrx)
  return(mtrx)
}

rel(structural, CR = TRUE, AVE = TRUE, MSV = TRUE, HTMT = TRUE, digits = 3)

# ------------------------------------------------------------------------------

# Create the updated "rel" function with Cronbach's alpha calculation
rel <- function(object, CR = TRUE, AVE = TRUE, MSV = TRUE, HTMT = TRUE, alpha = TRUE, digits) {
  cor.lv <- data.frame(lavaan::lavInspect(object, "cor.lv"))
  pt <- lavaan::parTable(object)[lavaan::parTable(object)$op == "=~",]
  mtrx <- round(cor.lv, digits)
  diag(mtrx) <- ""
  
  # Calculate HTMT
  if(HTMT) {
    htmt  <- round(semTools::htmt(paste(pt[,"lhs"], pt[,"op"], pt[,"rhs"], sep = ""), 
                                  data.frame(lavaan::lavInspect(object, "data"))), digits)
    upper <- htmt[upper.tri(htmt)]
  }
  else upper <- ""
  mtrx[upper.tri(mtrx)] <- upper
  
  # Calculate MSV
  if(MSV) mtrx <- cbind(MSV = round(sapply(abs(cor.lv), function(x) max(x[x != max(x)])^2), digits), mtrx)
  
  # Calculate AVE
  if(AVE) mtrx <- cbind(AVE = round(semTools::AVE(object), digits), mtrx)
  
  # Calculate Composite Reliability
  if(CR)  mtrx <- cbind(CR = round(semTools::compRelSEM(object), digits), mtrx)
  
  # Calculate Cronbach's alpha
  if(alpha) {
    data <- data.frame(lavaan::lavInspect(object, "data"))
    alpha_values <- sapply(unique(pt$lhs), function(factor) {
      items <- pt$rhs[pt$lhs == factor]
      alpha_score <- psych::alpha(data[, items])$total[["raw_alpha"]]
      return(round(alpha_score, digits))
    })
    mtrx <- cbind(Alpha = alpha_values, mtrx)
  }
  
  return(mtrx)
}

# Apply the function to your structural model
rel(measurement, CR = TRUE, AVE = TRUE, MSV = TRUE, HTMT = TRUE, alpha = TRUE, digits = 3)