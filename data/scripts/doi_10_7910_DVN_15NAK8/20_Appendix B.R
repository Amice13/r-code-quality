setwd("")

# Load Packages -----------------------------------------------------------
library(lavaan)

# Load Data ---------------------------------------------------------------
source("01_cleaning.R")


# Table B1: Top Panel ----------------------------------------------------------------
m_id_config <- '
id_w1 =~ poc_id_imp_w1 + poc_id_myself_w1 + poc_id_think_w1
id_w2 =~ poc_id_imp_w2 + poc_id_myself_w2 + poc_id_think_w2

# fix thresholds
poc_id_imp_w1 | t1 + t2 + t3 + t4
poc_id_imp_w2 | t1 + t2 + t3 + t4

poc_id_myself_w1 | t1 + t2 + t3 + t4
poc_id_myself_w2 | t1 + t2 + t3 + t4

poc_id_think_w1 | t1 + t2 + t3 + t4
poc_id_think_w2 | t1 + t2 + t3 + t4

# item error covariances
poc_id_imp_w1 ~~ poc_id_imp_w2
poc_id_myself_w1 ~~ poc_id_myself_w2
poc_id_think_w1 ~~ poc_id_think_w2
'
m_id_metric <- '
id_w1 =~ poc_id_imp_w1 + v2*poc_id_myself_w1 + v3*poc_id_think_w1
id_w2 =~ poc_id_imp_w2 + v2*poc_id_myself_w2 + v3*poc_id_think_w2

poc_id_imp_w1 | t1 + t2 + t3 + t4
poc_id_imp_w2 | t1 + t2 + t3 + t4

poc_id_myself_w1 | t1 + t2 + t3 + t4
poc_id_myself_w2 | t1 + t2 + t3 + t4

poc_id_think_w1 | t1 + t2 + t3 + t4
poc_id_think_w2 | t1 + t2 + t3 + t4

# item error covariances
poc_id_imp_w1 ~~ poc_id_imp_w2
poc_id_myself_w1 ~~ poc_id_myself_w2
poc_id_think_w1 ~~ poc_id_think_w2
'
m_id_scalar <- '
id_w1 =~ poc_id_imp_w1 + v2*poc_id_myself_w1 + v3*poc_id_think_w1
id_w2 =~ poc_id_imp_w2 + v2*poc_id_myself_w2 + v3*poc_id_think_w2

# fix thresholds
poc_id_imp_w1 | id1_t1*t1 + id1_t2*t2 + id1_t3*t3 + id1_t4*t4
poc_id_imp_w2 | id1_t1*t1 + id1_t2*t2 + id1_t3*t3 + id1_t4*t4

poc_id_myself_w1 | id2_t1*t1 + id2_t2*t2 + id2_t3*t3 + id2_t4*t4
poc_id_myself_w2 | id2_t1*t1 + id2_t2*t2 + id2_t3*t3 + id2_t4*t4

poc_id_think_w1 | id3_t1*t1 + id3_t2*t2 + id3_t3*t3 + id3_t4*t4
poc_id_think_w2 | id3_t1*t1 + id3_t2*t2 + id3_t3*t3 + id3_t4*t4

# item error covariances
poc_id_imp_w1 ~~ poc_id_imp_w2
poc_id_myself_w1 ~~ poc_id_myself_w2
poc_id_think_w1 ~~ poc_id_think_w2
'
stab_id_config <- cfa(m_id_config,
                      subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)))
stab_id_metric <- cfa(m_id_metric,
                      subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)))
stab_id_scalar <- cfa(m_id_scalar,
                      subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)))

# Info for Table B1
rbind(fitMeasures(stab_id_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled",
                                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")],
      fitMeasures(stab_id_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled",
                                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")],
      fitMeasures(stab_id_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled",
                                    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")])

lavTestLRT(stab_id_config, stab_id_metric, stab_id_scalar,
           model.names = c("Config", "Metric", "Scalar"),
           method = "satorra.bentler.2001")

# Table B1: Bottom Panel ----------------------------------------------------------------
m_solid_config <- '
solid_w1 =~ poc_solid_solid_w1 + poc_solid_probs_w1 + poc_solid_linked_w1
solid_w2 =~ poc_solid_solid_w2 + poc_solid_probs_w2 + poc_solid_linked_w2

# fix thresholds
poc_solid_solid_w1 | t1 + t2 + t3 + t4
poc_solid_solid_w2 | t1 + t2 + t3 + t4

poc_solid_probs_w1 | t1 + t2 + t3 + t4
poc_solid_probs_w2 | t1 + t2 + t3 + t4

poc_solid_linked_w1 | t1 + t2 + t3 + t4
poc_solid_linked_w2 | t1 + t2 + t3 + t4

# item error covariances
poc_solid_solid_w1 ~~ poc_solid_solid_w2
poc_solid_probs_w1 ~~ poc_solid_probs_w2
poc_solid_linked_w1 ~~ poc_solid_linked_w2
'
m_solid_metric <- '
solid_w1 =~ poc_solid_solid_w1 + z2*poc_solid_probs_w1 + z3*poc_solid_linked_w1
solid_w2 =~ poc_solid_solid_w2 + z2*poc_solid_probs_w2 + z3*poc_solid_linked_w2

poc_solid_solid_w1 | t1 + t2 + t3 + t4
poc_solid_solid_w2 | t1 + t2 + t3 + t4

poc_solid_probs_w1 | t1 + t2 + t3 + t4
poc_solid_probs_w2 | t1 + t2 + t3 + t4

poc_solid_linked_w1 | t1 + t2 + t3 + t4
poc_solid_linked_w2 | t1 + t2 + t3 + t4

# item error covariances
poc_solid_solid_w1 ~~ poc_solid_solid_w2
poc_solid_probs_w1 ~~ poc_solid_probs_w2
poc_solid_linked_w1 ~~ poc_solid_linked_w2
'
m_solid_scalar <- '
solid_w1 =~ poc_solid_solid_w1 + z2*poc_solid_probs_w1 + z3*poc_solid_linked_w1
solid_w2 =~ poc_solid_solid_w2 + z2*poc_solid_probs_w2 + z3*poc_solid_linked_w2

# fix thresholds
poc_solid_solid_w1 | solid1_t1*t1 + solid1_t2*t2 + solid1_t3*t3 + solid1_t4*t4
poc_solid_solid_w2 | solid1_t1*t1 + solid1_t2*t2 + solid1_t3*t3 + solid1_t4*t4

poc_solid_probs_w1 | solid2_t1*t1 + solid2_t2*t2 + solid2_t3*t3 + solid2_t4*t4
poc_solid_probs_w2 | solid2_t1*t1 + solid2_t2*t2 + solid2_t3*t3 + solid2_t4*t4

poc_solid_linked_w1 | solid3_t1*t1 + solid3_t2*t2 + solid3_t3*t3 + solid3_t4*t4
poc_solid_linked_w2 | solid3_t1*t1 + solid3_t2*t2 + solid3_t3*t3 + solid3_t4*t4

# item error covariances
poc_solid_solid_w1 ~~ poc_solid_solid_w2
poc_solid_probs_w1 ~~ poc_solid_probs_w2
poc_solid_linked_w1 ~~ poc_solid_linked_w2
'

stab_solid_config <- cfa(m_solid_config,
                         subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)))
stab_solid_metric <- cfa(m_solid_metric,
                         subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)))
stab_solid_scalar <- cfa(m_solid_scalar,
                         subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)))

# Info for Table B1
rbind(fitMeasures(stab_solid_config)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled",
                                       "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")],
      fitMeasures(stab_solid_metric)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled",
                                       "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")],
      fitMeasures(stab_solid_scalar)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled",
                                       "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")])

lavTestLRT(stab_solid_config, stab_solid_metric, stab_solid_scalar,
           model.names = c("Config", "Metric", "Scalar"),
           method = "satorra.bentler.2001")
