setwd("")

# Load Packages -----------------------------------------------------------
library(modelsummary)

# Load Data ---------------------------------------------------------------
source("01_cleaning.R")


# Policy ------------------------------------------------------------------
# * With ID and Solidarity ------------------------------------------------
m_daca_w1 <- lm(pp_daca_w1_sc ~ poc_id_sc_w1 + poc_solid_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                 data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_daca_w2 <- lm(pp_daca_w2_sc ~ poc_id_sc_w2 + poc_solid_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

m_visas_w1 <- lm(pp_visas_w1_sc ~ poc_id_sc_w1 + poc_solid_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
               data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_visas_w2 <- lm(pp_visas_w2_sc ~ poc_id_sc_w2 + poc_solid_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
               data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

m_reps_w1 <- lm(pp_reparations_w1_sc ~ poc_id_sc_w1 + poc_solid_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
               data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_reps_w2 <- lm(pp_reparations_w2_sc ~ poc_id_sc_w2 + poc_solid_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
               data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

m_afact_w1 <- lm(pp_afactColl_w1_sc ~ poc_id_sc_w1 + poc_solid_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
               data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_afact_w2 <- lm(pp_afactColl_w2_sc ~ poc_id_sc_w2 + poc_solid_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
               data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

# * Without Solidarity ------------------------------------------------
m_id_daca_w1 <- lm(pp_daca_w1_sc ~ poc_id_sc_w1  + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_id_daca_w2 <- lm(pp_daca_w2_sc ~ poc_id_sc_w2  + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

m_id_visas_w1 <- lm(pp_visas_w1_sc ~ poc_id_sc_w1  + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                 data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_id_visas_w2 <- lm(pp_visas_w2_sc ~ poc_id_sc_w2  + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                 data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

m_id_reps_w1 <- lm(pp_reparations_w1_sc ~ poc_id_sc_w1  + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_id_reps_w2 <- lm(pp_reparations_w2_sc ~ poc_id_sc_w2  + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

m_id_afact_w1 <- lm(pp_afactColl_w1_sc ~ poc_id_sc_w1  + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                 data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_id_afact_w2 <- lm(pp_afactColl_w2_sc ~ poc_id_sc_w2  + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                 data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)


# Vote Choice -------------------------------------------------------------
# * With ID and Solidarity ------------------------------------------------
m_jbdt_w1 <- lm(vc_jb_dt_w1 ~ poc_id_sc_w1 + poc_solid_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_jbdt_w2 <- lm(vc_jb_dt_w2 ~ poc_id_sc_w2 + poc_solid_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

m_jbrd_w1 <- lm(vc_jb_rd_w1 ~ poc_id_sc_w1 + poc_solid_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_jbrd_w2 <- lm(vc_jb_rd_w2 ~ poc_id_sc_w2 + poc_solid_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)


m_jbnh_w1 <- lm(vc_jb_nh_w1 ~ poc_id_sc_w1 + poc_solid_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_jbnh_w2 <- lm(vc_jb_nh_w2 ~ poc_id_sc_w2 + poc_solid_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)


m_jbts_w1 <- lm(vc_jb_ts_w1 ~ poc_id_sc_w1 + poc_solid_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_jbts_w2 <- lm(vc_jb_ts_w2 ~ poc_id_sc_w2 + poc_solid_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)


# * Without Solidarity ------------------------------------------------
m_id_jbdt_w1 <- lm(vc_jb_dt_w1 ~ poc_id_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_id_jbdt_w2 <- lm(vc_jb_dt_w2 ~ poc_id_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

m_id_jbrd_w1 <- lm(vc_jb_rd_w1 ~ poc_id_sc_w1  + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_id_jbrd_w2 <- lm(vc_jb_rd_w2 ~ poc_id_sc_w2  + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)


m_id_jbnh_w1 <- lm(vc_jb_nh_w1 ~ poc_id_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_id_jbnh_w2 <- lm(vc_jb_nh_w2 ~ poc_id_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)


m_id_jbts_w1 <- lm(vc_jb_ts_w1 ~ poc_id_sc_w1 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_id_jbts_w2 <- lm(vc_jb_ts_w2 ~ poc_id_sc_w2 + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

# * With PID -------------------------------------------------------------

m_pid_jbdt_w1 <- lm(vc_jb_dt_w1 ~ poc_id_sc_w1 + poc_solid_sc_w1 + as.factor(pid3_r) + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                    data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_pid_jbdt_w2 <- lm(vc_jb_dt_w2 ~ poc_id_sc_w2 + poc_solid_sc_w2 + as.factor(pid3_r) + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                    data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)

m_pid_jbrd_w1 <- lm(vc_jb_rd_w1 ~ poc_id_sc_w1 + poc_solid_sc_w1 + as.factor(pid3_r) + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                    data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_pid_jbrd_w2 <- lm(vc_jb_rd_w2 ~ poc_id_sc_w2 + poc_solid_sc_w2 + as.factor(pid3_r) + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                    data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)


m_pid_jbnh_w1 <- lm(vc_jb_nh_w1 ~ poc_id_sc_w1 + poc_solid_sc_w1 + as.factor(pid3_r) + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                    data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_pid_jbnh_w2 <- lm(vc_jb_nh_w2 ~ poc_id_sc_w2 + poc_solid_sc_w2 + as.factor(pid3_r) + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                    data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)


m_pid_jbts_w1 <- lm(vc_jb_ts_w1 ~ poc_id_sc_w1 + poc_solid_sc_w1 + as.factor(pid3_r) + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                    data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)
m_pid_jbts_w2 <- lm(vc_jb_ts_w2 ~ poc_id_sc_w2 + poc_solid_sc_w2 + as.factor(pid3_r) + raceOS_fact + interest + fem + age + educ_r + gen2 + gen3,
                    data = poc_dat, subset = raceOS_fact != "white", weights = weight_recontacts_overall_W2_r)


# Tables E1-E5------------------------------------------------------------------
coefs <- c("poc_id_sc_w1" = "PoC ID", 
           "poc_id_sc_w2" = "PoC ID", 
           "poc_solid_sc_w1" = "PoC Solidarity", 
           "poc_solid_sc_w2" = "PoC Solidarity",
           "as.factor(pid3_r)0" = "Independent",
           "as.factor(pid3_r)1" = "Republican",
           "raceOS_factlatino" = "Latino",
           "raceOS_factasian" = "Asian",
           "raceOS_factmultiracial" = "Multiracial",
           "interest" = "Political Interest",
           "fem" = "Female",
           "age" = "Age",
           "educ_r" = "Education",
           "gen2" = "Second Generation",
           "gen3" = "Third + Generation",
           "(Intercept)" = "Constant")


modelsummary(list("DACA - W1" = m_daca_w1, 
                  "DACA - W2" = m_daca_w2,
                  "Visas\nW1" = m_visas_w1, 
                  "Visas\nW2" = m_visas_w2,
                  "Reparations\nW1" = m_reps_w1, 
                  "Reparations\nW2" = m_reps_w2,
                  "Affirm. Action\nW1" = m_afact_w1,
                  "Affirm. Action\nW2" = m_afact_w2
                  ),
             title = "PoC ID and PoC Solidarity's Predictive Consistency on Policy Opinion",
             output = './figures/tableE1.docx',
             stars = c('*' = .05), 
             coef_map = coefs,
             notes = list('OLS coefficients with standard errors in parentheses. Variables scaled 0-1.'),
             gof_map = c("nobs", "r.squared"))


modelsummary(list("DACA - W1" = m_id_daca_w1, 
                  "DACA - W2" = m_id_daca_w2,
                  "Visas\nW1" = m_id_visas_w1, 
                  "Visas\nW2" = m_id_visas_w2,
                  "Reparations\nW1" = m_id_reps_w1, 
                  "Reparations\nW2" = m_id_reps_w2,
                  "Affirm. Action\nW1" = m_id_afact_w1,
                  "Affirm. Action\nW2" = m_id_afact_w2
                  ),
             title = "PoC ID's Predictive Consistency on Policy Opinion",
             output = './figures/tableE2.docx',
             stars = c('*' = .05), 
             coef_map = coefs,
             notes = list('OLS coefficients with standard errors in parentheses. Variables scaled 0-1.'),
             gof_map = c("nobs", "r.squared"))


modelsummary(list("Trump - W1" = m_jbdt_w1, 
                  "Trump - W2" = m_jbdt_w2,
                  "DeSantis\nW1" = m_jbrd_w1, 
                  "DeSantis\nW2" = m_jbrd_w2,
                  "Haley\nW1" = m_jbnh_w1, 
                  "Haley\nW2" = m_jbnh_w2,
                  "Scott\nW1" = m_jbts_w1, 
                  "Scott\nW2" = m_jbts_w2),
             title = "PoC ID and PoC Solidarity's Predictive Consistency on Horserace Vote Preference for Joe Biden over...",
             output = './figures/tableE3.docx',
             stars = c('*' = .05), 
             coef_map = coefs,
             notes = list('OLS coefficients with standard errors in parentheses. Variables scaled 0-1.'),
             gof_map = c("nobs", "r.squared"))

modelsummary(list("Trump - W1" = m_id_jbdt_w1, 
                  "Trump - W2" = m_id_jbdt_w2,
                  "DeSantis\nW1" = m_id_jbrd_w1, 
                  "DeSantis\nW2" = m_id_jbrd_w2,
                  "Haley\nW1" = m_id_jbnh_w1, 
                  "Haley\nW2" = m_id_jbnh_w2,
                  "Scott\nW1" = m_id_jbts_w1, 
                  "Scott\nW2" = m_id_jbts_w2),
             title = "PoC ID's Predictive Consistency on Horserace Vote Preference for Joe Biden over...",
             output = './figures/tableE4.docx',
             stars = c('*' = .05), 
             coef_map = coefs,
             notes = list('OLS coefficients with standard errors in parentheses. Variables scaled 0-1.'),
             gof_map = c("nobs", "r.squared")
             )

modelsummary(list("Trump - W1" = m_pid_jbdt_w1, 
                  "Trump - W2" = m_pid_jbdt_w2,
                  "DeSantis\nW1" = m_pid_jbrd_w1, 
                  "DeSantis\nW2" = m_pid_jbrd_w2,
                  "Haley\nW1" = m_pid_jbnh_w1, 
                  "Haley\nW2" = m_pid_jbnh_w2,
                  "Scott\nW1" = m_pid_jbts_w1, 
                  "Scott\nW2" = m_pid_jbts_w2),
             title = "PoC ID and PoC Solidarity's Predictive Consistency on Horserace Vote Preference for Joe Biden over..., controlling for partisanship",
             output = './figures/tableE5.docx',
             stars = c('*' = .05), 
             coef_map = coefs,
             notes = list('OLS coefficients with standard errors in parentheses. Variables scaled 0-1.'),
             gof_map = c("nobs", "r.squared")
)