setwd("")

# Load Packages -----------------------------------------------------------
library(survey)
library(jtools)
library(lavaan)
library(tidySEM)
library(ggplot2)


# Load Data ---------------------------------------------------------------
source("01_cleaning.R")

# Sample Size --------------------------------------------------------
# count total Asian, Black, Latino, and Multiracial respondents completing Waves 1 and 2 (p 3)
sum(table(poc_dat$raceOS[which(poc_dat$w2_recontact == 1)])[-1])

# Print Wave 1 sample (p 13)
table(poc_dat$raceOS)
# Print Wave 2 sample (p13)
table(poc_dat$raceOS[which(poc_dat$w2_recontact == 1)])
# Percentage recontacted
prop.table(table(poc_dat$raceOS_fact, poc_dat$w2_recontact), 1)

# Stability (Table 3) ---------------------------------------------------------------
# * Scales ----------------------------------------------------------------
# create survey object to estimate weighted scale correlation across waves
d <- svydesign(~1, data = subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)), 
               weights = ~weight_recontacts_overall_W2_r)

# Identity
svycor(poc_id_sc_w2 ~ poc_id_sc_w1, 
       subset(d, raceOS_fact != "white"),
       na.rm = T)
svycor(poc_id_sc_w2 ~ poc_id_sc_w1, 
       subset(d, raceOS_fact == "asian"),
       na.rm = T)
svycor(poc_id_sc_w2 ~ poc_id_sc_w1, 
       subset(d, raceOS_fact == "black"),
       na.rm = T)
svycor(poc_id_sc_w2 ~ poc_id_sc_w1, 
       subset(d, raceOS_fact == "latino"),
       na.rm = T)
svycor(poc_id_sc_w2 ~ poc_id_sc_w1, 
       subset(d, raceOS_fact == "multiracial"),
       na.rm = T)

# Solidarity
svycor(poc_solid_sc_w2 ~ poc_solid_sc_w1, 
       subset(d, raceOS_fact != "white"),
       na.rm = T)
svycor(poc_solid_sc_w2 ~ poc_solid_sc_w1, 
       subset(d, raceOS_fact == "asian"),
       na.rm = T)
svycor(poc_solid_sc_w2 ~ poc_solid_sc_w1, 
       subset(d, raceOS_fact == "black"),
       na.rm = T)
svycor(poc_solid_sc_w2 ~ poc_solid_sc_w1, 
       subset(d, raceOS_fact == "latino"),
       na.rm = T)
svycor(poc_solid_sc_w2 ~ poc_solid_sc_w1, 
       subset(d, raceOS_fact == "multiracial"),
       na.rm = T)
# * SEM Error Correction -----------------------------------------------------
# ID
m_id <- '
id_w1 =~ NA*poc_id_imp_w1 + v1*poc_id_imp_w1 + v2*poc_id_myself_w1 + v3*poc_id_think_w1
id_w2 =~ NA*poc_id_imp_w2 + v1*poc_id_imp_w2 + v2*poc_id_myself_w2 + v3*poc_id_think_w2

method =~ 1*poc_id_imp_w1 + 1*poc_id_myself_w1 + 1*poc_id_think_w1 + 1*poc_id_imp_w2 + 1*poc_id_myself_w2 + 1*poc_id_think_w2 + 1*trust_most_w1 + 1*trust_ppltrusting_w1 + 1*trust_most_w2 + 1*trust_ppltrusting_w2
method ~~ 0*id_w1 + 0*id_w2

# variances
id_w1 ~~ 1*id_w1
id_w2 ~~ 1*id_w2

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
stab_id_out <- cfa(m_id,
                   subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)),
                   sampling.weights = "weight_recontacts_overall_W2_r")
summary(stab_id_out)

m_id_groupTest <- '
id_w1 =~ NA*poc_id_imp_w1 + v1*poc_id_imp_w1 + v2*poc_id_myself_w1 + v3*poc_id_think_w1
id_w2 =~ NA*poc_id_imp_w2 + v1*poc_id_imp_w2 + v2*poc_id_myself_w2 + v3*poc_id_think_w2

method =~ 1*poc_id_imp_w1 + 1*poc_id_myself_w1 + 1*poc_id_think_w1 + 1*poc_id_imp_w2 + 1*poc_id_myself_w2 + 1*poc_id_think_w2 + 1*trust_most_w1 + 1*trust_ppltrusting_w1 + 1*trust_most_w2 + 1*trust_ppltrusting_w2
method ~~ 0*id_w1 + 0*id_w2

# variances
id_w1 ~~ 1*id_w1
id_w2 ~~ 1*id_w2

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

# for hypothesis testing across groups
id_w2 ~~ c(s1, s2, s3, s4)*id_w1
sMA := s1-s2
sML := s1-s3
sMB := s1-s4

sAM := s2-s1
sAL := s2-s3
sAB := s2-s4

sLM := s3-s1
sLA := s3-s2
sLB := s3-s4

sBM := s4-s1
sBA := s4-s2
sBL := s4-s3
'

stab_id_out_race <- cfa(m_id_groupTest,
                        subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)),
                        sampling.weights = "weight_recontacts_overall_W2_r",
                        group = "raceOS_fact")
summary(stab_id_out_race)
# defined parameters at the bottom related to in-text info (p 17)

# Solidarity
m_solid <- '
solid_w1 =~ NA*poc_solid_solid_w1 + z1*poc_solid_solid_w1 + z2*poc_solid_probs_w1 + z3*poc_solid_linked_w1
solid_w2 =~ NA*poc_solid_solid_w2 + z1*poc_solid_solid_w2 + z2*poc_solid_probs_w2 + z3*poc_solid_linked_w2

method =~ 1*poc_solid_solid_w1 + 1*poc_solid_probs_w1 + 1*poc_solid_linked_w1 + 1*poc_solid_solid_w2 + 1*poc_solid_probs_w2 + 1*poc_solid_linked_w2 + 1*trust_most_w1 + 1*trust_ppltrusting_w1 + 1*trust_most_w2 + 1*trust_ppltrusting_w2
method ~~ 0*solid_w1 + 0*solid_w2

# variances
solid_w1 ~~ 1*solid_w1
solid_w2 ~~ 1*solid_w2

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

stab_solid_out <- cfa(m_solid,
                      subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)),
                      sampling.weights = "weight_recontacts_overall_W2_r")
summary(stab_solid_out)


m_solid_groupTest <- '
solid_w1 =~ NA*poc_solid_solid_w1 + z1*poc_solid_solid_w1 + z2*poc_solid_probs_w1 + z3*poc_solid_linked_w1
solid_w2 =~ NA*poc_solid_solid_w2 + z1*poc_solid_solid_w2 + z2*poc_solid_probs_w2 + z3*poc_solid_linked_w2

method =~ 1*poc_solid_solid_w1 + 1*poc_solid_probs_w1 + 1*poc_solid_linked_w1 + 1*poc_solid_solid_w2 + 1*poc_solid_probs_w2 + 1*poc_solid_linked_w2 + 1*trust_most_w1 + 1*trust_ppltrusting_w1 + 1*trust_most_w2 + 1*trust_ppltrusting_w2
method ~~ 0*solid_w1 + 0*solid_w2

# variances
solid_w1 ~~ 1*solid_w1
solid_w2 ~~ 1*solid_w2

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

# hypothesis testing across groups
solid_w2 ~~ c(s1, s2, s3, s4)*solid_w1
sMA := s1-s2
sML := s1-s3
sMB := s1-s4

sAM := s2-s1
sAL := s2-s3
sAB := s2-s4

sLM := s3-s1
sLA := s3-s2
sLB := s3-s4

sBM := s4-s1
sBA := s4-s2
sBL := s4-s3
'
stab_solid_out_race <- cfa(m_solid_groupTest,
                           subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)),
                           sampling.weights = "weight_recontacts_overall_W2_r",
                           group = "raceOS_fact")
summary(stab_solid_out_race)


# CLPM (Figure 1) ---------------------------------------------------------
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
summary(clpm_weight, fit.measures = T, standardized = T)

# ** Table A1 -------------------------------------------------------------
standardizedSolution(clpm_weight)[which(standardizedSolution(clpm_weight)$op == "~"), ]
fitMeasures(clpm_weight)[c("chisq.scaled",  "df", "cfi.scaled", "srmr", "rmsea.scaled",
                           "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")]

# ** Figure 1 ----------------------------------------------------------------
# prepare data for graph
p_data <- prepare_graph(clpm_weight, 
                        layout = get_layout("id_w1", "id_w2", 
                                            "solid_w1", "solid_w2", 
                                            rows = 2))
nodes_df <- p_data$nodes
nodes_df$label <- c("People~of~color~identity[T1]", "People~of~color~identity[T2]", # ~ functions as whitespace for parsing
                    "People~of~color~solidarity[T1]", "People~of~color~solidarity[T2]")
# Set original labels to blank
p_data$nodes$label <- "" 

# modify edges
edges(p_data)$est_sig_std <- gsub("[*]{2}", "*", edges(p_data)$est_sig_std)
edges(p_data)$label <- paste0(edges(p_data)$est_sig_std, "\n", 
                              "(", edges(p_data)$se_std, ")")
edges(p_data)$show[5:10] <- FALSE
edges(p_data)$connect_to[2] <- "left"
edges(p_data)$label_color <- "white"
edges(p_data)$label_fill <- "black"

# plot
plot(p_data) + geom_text(data = nodes_df, aes(x=x, y=y, label=label), parse = TRUE)

ggsave("./figures/figure1.pdf", width = 7, height = 4)

# Fit for figure caption
fitMeasures(clpm_weight)[c("chisq.scaled",  "df", "cfi.scaled", "srmr", "rmsea.scaled",
                           "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")]

# Descriptives  ------------------------------------------------------------
# * In-Text notes -------------------------------------------------------
# create survey object to estimate weighted means
d <- svydesign(~1, data = subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)), 
               weights = ~weight_recontacts_overall_W2_r)

# Black/non-Black ID and Solidarity means (in-text)
svyby(~poc_id_sc_w1 + poc_id_sc_w2 + poc_solid_sc_w1 + poc_solid_sc_w2, 
      ~ race_b,
      subset(d, raceOS_fact != "white"),
      svymean,
      na.rm = T)

# SDs for ID (in-text)
sqrt(svyby(~poc_id_sc_w1, 
           ~ race_b,
           subset(d, raceOS_fact != "white"),
           svyvar,
           na.rm = T)[, 2])
sqrt(svyby(~poc_id_sc_w2, 
           ~ race_b,
           subset(d, raceOS_fact != "white"),
           svyvar,
           na.rm = T)[, 2])
# SDs for solidarity (in-text)
sqrt(svyby(~poc_solid_sc_w1,
           ~ race_b,
           subset(d, raceOS_fact != "white"),
           svyvar,
           na.rm = T)[, 2])
sqrt(svyby(~poc_solid_sc_w2, 
           ~ race_b,
           subset(d, raceOS_fact != "white"),
           svyvar,
           na.rm = T)[, 2])



# * Footnote 3 ------------------------------------------------------------
# PoC ID: wave 1 and wave 2 means by group
svyby(~poc_id_sc_w1 + poc_id_sc_w2, 
      ~ raceOS_fact,
      subset(d, raceOS_fact != "white" & raceOS_fact != "black"),
      svymean,
      na.rm = T)

# t-tests of differences in averages
svyttest(poc_id_sc_w1 ~ raceOS_fact,
         subset(d, raceOS_fact == "multiracial" | raceOS_fact == "asian"))
svyttest(poc_id_sc_w2 ~ raceOS_fact,
         subset(d, raceOS_fact == "multiracial" | raceOS_fact == "asian"))
svyttest(poc_id_sc_w1 ~ raceOS_fact,
         subset(d, raceOS_fact == "multiracial" | raceOS_fact == "latino"))
svyttest(poc_id_sc_w2 ~ raceOS_fact,
         subset(d, raceOS_fact == "multiracial" | raceOS_fact == "latino"))

# PoC Solid: wave 1 and wave 2 means by group
svyby(~ poc_solid_sc_w1 + poc_solid_sc_w2, 
      ~ raceOS_fact,
      subset(d, raceOS_fact != "white" & raceOS_fact != "black"),
      svymean,
      na.rm = T)


# t-tests of differences in averages
svyttest(poc_solid_sc_w1 ~ raceOS_fact,
         subset(d, raceOS_fact == "multiracial" | raceOS_fact == "asian"))
svyttest(poc_solid_sc_w2 ~ raceOS_fact,
         subset(d, raceOS_fact == "multiracial" | raceOS_fact == "asian"))
svyttest(poc_solid_sc_w1 ~ raceOS_fact,
         subset(d, raceOS_fact == "multiracial" | raceOS_fact == "latino"))
svyttest(poc_solid_sc_w2 ~ raceOS_fact,
         subset(d, raceOS_fact == "multiracial" | raceOS_fact == "latino"))

# Black/Non-Black Equivalence (Table 4) ----------------
clpm_weight_grouped <- sem(m_clpm,
                           group = "race_b", 
                           subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)),
                           sampling.weights = "weight_recontacts_overall_W2_r")

clpm_weight_const <- sem(m_clpm,
                         group = "race_b",
                         subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)),
                         sampling.weights = "weight_recontacts_overall_W2_r",
                         group.equal = c("regressions",
                                         "lv.covariances"
                         ))

# Table 4 information
rbind(fitMeasures(clpm_weight_grouped)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled",
                                         "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")],
      fitMeasures(clpm_weight_const)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled",
                                       "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")])

# Reported p-value for likelihood ratio test (p21)
lavTestLRT(clpm_weight_grouped, clpm_weight_const,
           model.names = c("Free", "Constrained"),
           method = "satorra.bentler.2001")


# * Footnote 5 -----------------------------------------------------
clpm_weight_const <- sem(m_clpm,
                         group = "race_b",
                         subset(poc_dat, !is.na(weight_recontacts_overall_W2_r)),
                         sampling.weights = "weight_recontacts_overall_W2_r",
                         group.equal = c("regressions",
                                         "means",
                                         "intercepts",
                                         "lv.variances",
                                         "lv.covariances",
                                         "residuals",
                                         "residual.covariances"
                         ))

rbind(fitMeasures(clpm_weight)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled",
                                 "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")],
      fitMeasures(clpm_weight_const)[c("chisq.scaled", "df", "cfi.scaled", "srmr", "rmsea.scaled",
                                       "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled")])

