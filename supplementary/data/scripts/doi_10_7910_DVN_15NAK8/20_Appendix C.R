setwd("")

# Load Packages -----------------------------------------------------------
library(lavaan)

# Load Data ---------------------------------------------------------------
source("01_cleaning.R")

# Main Text ---------------------------------------------------------------
m_clpm <- '
# Regressions 
id_w2 ~ id_w1 + solid_w1 
solid_w2 ~ id_w1 + solid_w1

# Covariances
id_w1 ~~ solid_w1
id_w2 ~~ solid_w2

# Factors
id_w1 =~ v1*poc_id_imp_w1 + v2*poc_id_myself_w1 + v3*poc_id_think_w1
id_w2 =~ v1*poc_id_imp_w2 + v2*poc_id_myself_w2 + v3*poc_id_think_w2
solid_w1 =~ z1*poc_solid_solid_w1 + z2*poc_solid_probs_w1 + z3*poc_solid_linked_w1
solid_w2 =~ z1*poc_solid_solid_w2 + z2*poc_solid_probs_w2 + z3*poc_solid_linked_w2

# Method factor
method =~ 1*poc_id_imp_w1 + 1*poc_id_myself_w1 + 1*poc_id_think_w1 + 1*poc_id_imp_w2 + 1*poc_id_myself_w2 + 1*poc_id_think_w2 + 1*poc_solid_solid_w1 + 1*poc_solid_probs_w1 + 1*poc_solid_linked_w1 + 1*poc_solid_solid_w2 + 1*poc_solid_probs_w2 + 1*poc_solid_linked_w2 + 1*trust_most_w1 + 1*trust_ppltrusting_w1 + 1*trust_most_w2 + 1*trust_ppltrusting_w2
method ~~ 0*solid_w1 + 0*solid_w2 + 0*id_w1 + 0*id_w2

# fix thresholds (with null thresholds from freely estimated model = 0)
poc_id_imp_w1 | id1_t1*t1 + id1_t2*t2 + id1_t3*t3 + id1_t4*t4
poc_id_imp_w2 | id1_t1*t1 + id1_t2*t2 + id1_t3*t3 + id1_t4*t4

poc_id_myself_w1 | id2_t1*t1 + id2_t2*t2 + id2_t3*t3 + id2_t4*t4
poc_id_myself_w2 | id2_t1*t1 + id2_t2*t2 + id2_t3*t3 + id2_t4*t4

poc_id_think_w1 | id3_t1*t1 + id3_t2*t2 + id3_t3*t3 + id3_t4*t4
poc_id_think_w2 | id3_t1*t1 + id3_t2*t2 + id3_t3*t3 + id3_t4*t4

poc_solid_solid_w1 | solid1_t1*t1 + solid1_t2*t2 + solid1_t3*t3 + solid1_t4*t4
poc_solid_solid_w2 | solid1_t1*t1 + solid1_t2*t2 + solid1_t3*t3 + solid1_t4*t4

poc_solid_probs_w1 | solid2_t1*t1 + solid2_t2*t2 + 0*t3 + solid2_t4*t4
poc_solid_probs_w2 | solid2_t1*t1 + solid2_t2*t2 + 0*t3 + solid2_t4*t4

poc_solid_linked_w1 | solid3_t1*t1 + solid3_t2*t2 + 0*t3 + solid3_t4*t4
poc_solid_linked_w2 | solid3_t1*t1 + solid3_t2*t2 + 0*t3 + solid3_t4*t4

# item error covariances
# temporal
poc_id_imp_w1 ~~ poc_id_imp_w2
poc_id_myself_w1 ~~ poc_id_myself_w2
poc_id_think_w1 ~~ poc_id_think_w2

poc_solid_solid_w1 ~~ poc_solid_solid_w2
poc_solid_probs_w1 ~~ poc_solid_probs_w2
poc_solid_linked_w1 ~~ poc_solid_linked_w2

# within-wave (uncomment for for model in paper; retaining inflates negative variances)
# poc_id_imp_w1 ~~ poc_id_myself_w1
# poc_id_imp_w2 ~~ poc_id_myself_w2

trust_most_w1 ~~ trust_ppltrusting_w1 + trust_most_w2
trust_ppltrusting_w1 + trust_most_w2 ~~ trust_ppltrusting_w2

# Fixing residual variances to 0 for negatives 
poc_id_imp_w2 ~~ 0*poc_id_imp_w2
poc_id_myself_w2 ~~ 0*poc_id_myself_w2
'

clpm_weight <- sem(m_clpm,
                   subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)),
                   sampling.weights = "weight_recontacts_overall_W2_r")

# Controls -------------------------------------------------------------------
m_clpm_cont <- '
# Regressions 
id_w2 ~ id_w1 + solid_w1 + pid7r + ideo5r + ideo5r_dk + interest + fem + age + educ_r + gen2 + gen3
solid_w2 ~ id_w1 + solid_w1 + pid7r + ideo5r + ideo5r_dk + interest + fem + age + educ_r + gen2 + gen3

# Covariances
id_w1 ~~ solid_w1
id_w2 ~~ solid_w2

# Factors
id_w1 =~ v1*poc_id_imp_w1 + v2*poc_id_myself_w1 + v3*poc_id_think_w1
id_w2 =~ v1*poc_id_imp_w2 + v2*poc_id_myself_w2 + v3*poc_id_think_w2
solid_w1 =~ z1*poc_solid_solid_w1 + z2*poc_solid_probs_w1 + z3*poc_solid_linked_w1
solid_w2 =~ z1*poc_solid_solid_w2 + z2*poc_solid_probs_w2 + z3*poc_solid_linked_w2

# Method factor
method =~ 1*poc_id_imp_w1 + 1*poc_id_myself_w1 + 1*poc_id_think_w1 + 1*poc_id_imp_w2 + 1*poc_id_myself_w2 + 1*poc_id_think_w2 + 1*poc_solid_solid_w1 + 1*poc_solid_probs_w1 + 1*poc_solid_linked_w1 + 1*poc_solid_solid_w2 + 1*poc_solid_probs_w2 + 1*poc_solid_linked_w2 + 1*trust_most_w1 + 1*trust_ppltrusting_w1 + 1*trust_most_w2 + 1*trust_ppltrusting_w2
method ~~ 0*solid_w1 + 0*solid_w2 + 0*id_w1 + 0*id_w2

# fix thresholds (with null thresholds from freely estimated model = 0)
poc_id_imp_w1 | id1_t1*t1 + id1_t2*t2 + id1_t3*t3 + 0*t4
poc_id_imp_w2 | id1_t1*t1 + id1_t2*t2 + id1_t3*t3 + 0*t4

poc_id_myself_w1 | id2_t1*t1 + id2_t2*t2 + id2_t3*t3 + 0*t4
poc_id_myself_w2 | id2_t1*t1 + id2_t2*t2 + id2_t3*t3 + 0*t4

poc_id_think_w1 | id3_t1*t1 + id3_t2*t2 + id3_t3*t3 + 0*t4
poc_id_think_w2 | id3_t1*t1 + id3_t2*t2 + id3_t3*t3 + 0*t4

poc_solid_solid_w1 | solid1_t1*t1 + solid1_t2*t2 + solid1_t3*t3 + 0*t4
poc_solid_solid_w2 | solid1_t1*t1 + solid1_t2*t2 + solid1_t3*t3 + 0*t4

poc_solid_probs_w1 | solid2_t1*t1 + solid2_t2*t2 + solid2_t3*t3 + solid2_t4*t4
poc_solid_probs_w2 | solid2_t1*t1 + solid2_t2*t2 + solid2_t3*t3 + solid2_t4*t4

poc_solid_linked_w1 | solid3_t1*t1 + solid3_t2*t2 + solid3_t3*t3 + 0*t4
poc_solid_linked_w2 | solid3_t1*t1 + solid3_t2*t2 + solid3_t3*t3 + 0*t4

# item error covariances
# temporal
poc_id_imp_w1 ~~ poc_id_imp_w2
poc_id_myself_w1 ~~ poc_id_myself_w2
poc_id_think_w1 ~~ poc_id_think_w2

poc_solid_solid_w1 ~~ poc_solid_solid_w2
poc_solid_probs_w1 ~~ poc_solid_probs_w2
poc_solid_linked_w1 ~~ poc_solid_linked_w2

trust_most_w1 ~~ trust_ppltrusting_w1 + trust_most_w2
trust_ppltrusting_w1 + trust_most_w2 ~~ trust_ppltrusting_w2

# Fixing residual variances to 0 for negatives 
poc_id_imp_w2 ~~ 0*poc_id_imp_w2
poc_id_myself_w2 ~~ 0*poc_id_myself_w2
'
clpm_controls <- sem(m_clpm_cont,
                     subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)),
                     sampling.weights = "weight_recontacts_overall_W2_r")

# Racial ID -----------------------------------------------------
m_clpm_rac <- '
# Regressions 
id_w2 ~ id_w1 + solid_w1 + racid_w1 + pid7r + ideo5r + ideo5r_dk + interest + fem + age + educ_r + gen2 + gen3  
solid_w2 ~ id_w1 + solid_w1 + racid_w1 + pid7r + ideo5r + ideo5r_dk + interest + fem + age + educ_r + gen2 + gen3  

# Covariances
id_w1 ~~ solid_w1
racid_w1 ~~ id_w1 + solid_w1
id_w2 ~~ solid_w2

# Factors
id_w1 =~ v1*poc_id_imp_w1 + v2*poc_id_myself_w1 + v3*poc_id_think_w1
id_w2 =~ v1*poc_id_imp_w2 + v2*poc_id_myself_w2 + v3*poc_id_think_w2
solid_w1 =~ z1*poc_solid_solid_w1 + z2*poc_solid_probs_w1 + z3*poc_solid_linked_w1
solid_w2 =~ z1*poc_solid_solid_w2 + z2*poc_solid_probs_w2 + z3*poc_solid_linked_w2
racid_w1 =~ y1*com_rac_id_imp_w1 + y2*com_rac_id_myself_w1 + y3*com_rac_id_think_w1

# Method factor
method =~ 1*poc_id_imp_w1 + 1*poc_id_myself_w1 + 1*poc_id_think_w1 + 1*poc_id_imp_w2 + 1*poc_id_myself_w2 + 1*poc_id_think_w2 + 1*com_rac_id_imp_w1 + 1*com_rac_id_myself_w1 + 1*com_rac_id_think_w1 + 1*poc_solid_solid_w1 + 1*poc_solid_probs_w1 + 1*poc_solid_linked_w1 + 1*poc_solid_solid_w2 + 1*poc_solid_probs_w2 + 1*poc_solid_linked_w2 + 1*trust_most_w1 + 1*trust_ppltrusting_w1 + 1*trust_most_w2 + 1*trust_ppltrusting_w2
method ~~ 0*solid_w1 + 0*solid_w2 + 0*id_w1 + 0*id_w2  + 0*racid_w1 #+ 0*racid_w2


# fix thresholds (with null thresholds from freely estimated model = 0)
poc_id_imp_w1 | id1_t1*t1 + id1_t2*t2 + id1_t3*t3 + 0*t4
poc_id_imp_w2 | id1_t1*t1 + id1_t2*t2 + id1_t3*t3 + 0*t4

poc_id_myself_w1 | id2_t1*t1 + id2_t2*t2 + id2_t3*t3 + 0*t4
poc_id_myself_w2 | id2_t1*t1 + id2_t2*t2 + id2_t3*t3 + 0*t4

poc_id_think_w1 | id3_t1*t1 + id3_t2*t2 + id3_t3*t3 + 0*t4
poc_id_think_w2 | id3_t1*t1 + id3_t2*t2 + id3_t3*t3 + 0*t4

poc_solid_solid_w1 | solid1_t1*t1 + solid1_t2*t2 + solid1_t3*t3 + 0*t4
poc_solid_solid_w2 | solid1_t1*t1 + solid1_t2*t2 + solid1_t3*t3 + 0*t4

poc_solid_probs_w1 | solid2_t1*t1 + solid2_t2*t2 + solid2_t3*t3 + solid2_t4*t4
poc_solid_probs_w2 | solid2_t1*t1 + solid2_t2*t2 + solid2_t3*t3 + solid2_t4*t4

poc_solid_linked_w1 | solid3_t1*t1 + solid3_t2*t2 + solid3_t3*t3 + 0*t4
poc_solid_linked_w2 | solid3_t1*t1 + solid3_t2*t2 + solid3_t3*t3 + 0*t4

# item error covariances
# temporal
poc_id_imp_w1 ~~ poc_id_imp_w2
poc_id_myself_w1 ~~ poc_id_myself_w2
poc_id_think_w1 ~~ poc_id_think_w2

poc_solid_solid_w1 ~~ poc_solid_solid_w2
poc_solid_probs_w1 ~~ poc_solid_probs_w2
poc_solid_linked_w1 ~~ poc_solid_linked_w2

trust_most_w1 ~~ trust_ppltrusting_w1 + trust_most_w2
trust_ppltrusting_w1 + trust_most_w2 ~~ trust_ppltrusting_w2

# Fixing residual variances to 0 for negatives 
poc_id_imp_w2 ~~ 0*poc_id_imp_w2
poc_id_myself_w2 ~~ 0*poc_id_myself_w2
'

clpm_raceID <- sem(m_clpm_rac,
                   subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)),
                   sampling.weights = "weight_recontacts_overall_W2_r")

# For Table ---------------------------------------------------------------
# Main Text
standardizedSolution(clpm_weight)[which(standardizedSolution(clpm_weight)$op == "~"), ]
fitMeasures(clpm_weight)[c("chisq.scaled",  "df", "cfi.scaled", "srmr", "rmsea.scaled",
                           "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")]
# Controls
standardizedSolution(clpm_controls)[which(standardizedSolution(clpm_controls)$op == "~"), ]
fitMeasures(clpm_controls)[c("chisq.scaled",  "df", "cfi.scaled", "srmr", "rmsea.scaled",
                           "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")]
# Racial ID
standardizedSolution(clpm_raceID)[which(standardizedSolution(clpm_raceID)$op == "~"), ]
fitMeasures(clpm_raceID)[c("chisq.scaled",  "df", "cfi.scaled", "srmr", "rmsea.scaled",
                           "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")]
