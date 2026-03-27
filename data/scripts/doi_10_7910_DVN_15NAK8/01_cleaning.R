source("09_misc functions.R")

# Load Data ---------------------------------------------------------------
poc_dat <- haven::read_dta("./100_Data/ampsW1W2.dta")

# Meta --------------------------------------------------------------------
poc_dat$w2_recontact <- ifelse(poc_dat$UMAS0083_Recontact %in% c(1, 2, 3, 4, 5), 1, 0)

poc_dat$weight_recontacts_overall_W2_r <- NA
poc_dat$weight_recontacts_overall_W2_r[which(poc_dat$weight_recontacts_overall_W2 != "No Data")] <- as.numeric(poc_dat$weight_recontacts_overall_W2[which(poc_dat$weight_recontacts_overall_W2 != "No Data")]) 


# Demos -------------------------------------------------------------------
poc_dat$raceOS_fact <- factor(poc_dat$raceOS,
                              labels = c("white", "black", "latino", "asian", "multiracial"))
poc_dat$raceOS_fact[which(poc_dat$raceOS_fact == "wave2")] <- NA
levels(poc_dat$raceOS_fact)[1]<- NA

poc_dat$race_b <- ifelse(poc_dat$raceOS_fact == "black", 1, 0)
poc_dat$race_l <- ifelse(poc_dat$raceOS_fact == "latino", 1, 0)
poc_dat$race_a <- ifelse(poc_dat$raceOS_fact == "asian", 1, 0)
poc_dat$race_m <- ifelse(poc_dat$raceOS_fact == "multiracial", 1, 0)

poc_dat$age <- NA
poc_dat$age[which(poc_dat$birthyr != "No Data")] <- 2023-as.numeric(poc_dat$birthyr[which(poc_dat$birthyr != "No Data")])

poc_dat$educ_r <- ifelse(poc_dat$educ > 0, poc_dat$educ, NA)
poc_dat$fem <- NA
poc_dat$fem[which(poc_dat$gender == 2)] <- 1
poc_dat$fem[which(poc_dat$gender == 1)] <- 0


poc_dat$gen_stat <- ifelse(poc_dat$immigrant > 0, poc_dat$immigrant, NA) 
poc_dat$gen1 <- ifelse(poc_dat$gen_stat <= 3, 1, 0)
poc_dat$gen2 <- ifelse(poc_dat$gen_stat == 4, 1, 0)
poc_dat$gen3 <- ifelse(poc_dat$gen_stat == 5, 1, 0)

poc_dat$interest <- ifelse(poc_dat$newsint > 0 & poc_dat$newsint < 7, poc_dat$newsint*-1 + 5, NA)

poc_dat$interest_hilo <- ifelse(poc_dat$interest > median(poc_dat$interest, na.rm = T), 1, 0)
poc_dat$interest_himedlo <- NA
poc_dat$interest_himedlo[which(poc_dat$interest < median(poc_dat$interest, na.rm = T))] <- 0
poc_dat$interest_himedlo[which(poc_dat$interest == median(poc_dat$interest, na.rm = T))] <- 1
poc_dat$interest_himedlo[which(poc_dat$interest > median(poc_dat$interest, na.rm = T))] <- 2

# Trust -------------------------------------------------------------------
poc_dat$trust_most_w1 <- ifelse(poc_dat$Q2_1 < 8 & poc_dat$Q2_1 > -1, poc_dat$Q2_1*-1 + 6, NA)
poc_dat$trust_ppltrusting_w1 <- ifelse(poc_dat$Q2_2 < 8 & poc_dat$Q2_2 > -1, poc_dat$Q2_2*-1 + 6, NA)

poc_dat$trust_most_w2 <- ifelse(poc_dat$Q2_1_W2 < 8 & poc_dat$Q2_1_W2 > -1, poc_dat$Q2_1_W2*-1 + 6, NA)
poc_dat$trust_ppltrusting_w2 <- ifelse(poc_dat$Q2_2_W2 < 8 & poc_dat$Q2_2_W2 > -1, poc_dat$Q2_2_W2*-1 + 6, NA)

# Political ---------------------------------------------------------------
poc_dat$pid7r <- ifelse(poc_dat$pid7 > -1, poc_dat$pid7, NA)
poc_dat$pid7r[which(poc_dat$pid7 == 8)] <- 4 # Dk as independent

poc_dat$pid3_r <- NA
poc_dat$pid3_r[which(poc_dat$pid7r < 4)] <- -1
poc_dat$pid3_r[which(poc_dat$pid7r == 4)] <- 0
poc_dat$pid3_r[which(poc_dat$pid7r > 4)] <- 1

poc_dat$ideo5r <- ifelse(poc_dat$ideo5 > -1, poc_dat$ideo5, NA)
poc_dat$ideo5r[which(poc_dat$ideo5 == 6)] <- 3 # Dk as moderate
poc_dat$ideo5r_dk <- ifelse(poc_dat$ideo5 == 6, 1, 0)

# Racial ID ---------------------------------------------------------------
poc_dat$rac_id_imp_w1 <- ifelse(poc_dat$Q28_1 > 0 & poc_dat$Q28_1 <= 5, 
                                poc_dat$Q28_1*-1 + 6, 
                                NA)
poc_dat$rac_id_myself_w1 <- ifelse(poc_dat$Q28_2 > 0 & poc_dat$Q28_2 <= 5, 
                                   poc_dat$Q28_2*-1 + 6, 
                                   NA)
poc_dat$rac_id_think_w1 <- ifelse(poc_dat$Q28_3 > 0 & poc_dat$Q28_3 <= 5, 
                                  poc_dat$Q28_3*-1 + 6, 
                                  NA)

poc_dat$multirac_id_imp_w1 <- ifelse(poc_dat$Q29_1 > 0 & poc_dat$Q29_1 <= 5, 
                                poc_dat$Q29_1*-1 + 6, 
                                NA)
poc_dat$multirac_id_myself_w1 <- ifelse(poc_dat$Q29_2 > 0 & poc_dat$Q29_2 <= 5, 
                                   poc_dat$Q29_2*-1 + 6, 
                                   NA)
poc_dat$multirac_id_think_w1 <- ifelse(poc_dat$Q29_3 > 0 & poc_dat$Q29_3 <= 5, 
                                  poc_dat$Q29_3*-1 + 6, 
                                  NA)

poc_dat$com_rac_id_imp_w1 <- NA
poc_dat$com_rac_id_imp_w1[which(!is.na(poc_dat$rac_id_imp_w1))] <- poc_dat$rac_id_imp_w1[which(!is.na(poc_dat$rac_id_imp_w1))]
poc_dat$com_rac_id_imp_w1[which(!is.na(poc_dat$multirac_id_imp_w1))] <- poc_dat$multirac_id_imp_w1[which(!is.na(poc_dat$multirac_id_imp_w1))]

poc_dat$com_rac_id_myself_w1 <- NA
poc_dat$com_rac_id_myself_w1[which(!is.na(poc_dat$rac_id_myself_w1))] <- poc_dat$rac_id_myself_w1[which(!is.na(poc_dat$rac_id_myself_w1))]
poc_dat$com_rac_id_myself_w1[which(!is.na(poc_dat$multirac_id_myself_w1))] <- poc_dat$multirac_id_myself_w1[which(!is.na(poc_dat$multirac_id_myself_w1))]

poc_dat$com_rac_id_think_w1 <- NA
poc_dat$com_rac_id_think_w1[which(!is.na(poc_dat$rac_id_think_w1))] <- poc_dat$rac_id_think_w1[which(!is.na(poc_dat$rac_id_think_w1))]
poc_dat$com_rac_id_think_w1[which(!is.na(poc_dat$multirac_id_think_w1))] <- poc_dat$multirac_id_think_w1[which(!is.na(poc_dat$multirac_id_think_w1))]


# POC ID ------------------------------------------------------------------
poc_dat$poc_id_imp_w1 <- ifelse(poc_dat$Q30_1 > 0 & poc_dat$Q30_1 <= 5, 
                                poc_dat$Q30_1*-1 + 6, 
                                NA)
poc_dat$poc_id_myself_w1 <- ifelse(poc_dat$Q30_2 > 0 & poc_dat$Q30_2 <= 5, 
                                poc_dat$Q30_2*-1 + 6, 
                                NA)
poc_dat$poc_id_think_w1 <- ifelse(poc_dat$Q30_3 > 0 & poc_dat$Q30_3 <= 5, 
                                poc_dat$Q30_3*-1 + 6, 
                                NA)

poc_dat$poc_id_w1 <- (poc_dat$poc_id_imp_w1 + poc_dat$poc_id_myself_w1 + poc_dat$poc_id_think_w1)/3
poc_dat$poc_id_sc_w1 <- (poc_dat$poc_id_w1-1)/4

poc_dat$poc_id_imp_w2 <- ifelse(poc_dat$Q30_1_W2 > 0 & poc_dat$Q30_1_W2 <= 5, 
                                poc_dat$Q30_1_W2*-1 + 6, 
                                NA)
poc_dat$poc_id_myself_w2 <- ifelse(poc_dat$Q30_2_W2 > 0 & poc_dat$Q30_2_W2 <= 5, 
                                   poc_dat$Q30_2_W2*-1 + 6, 
                                   NA)
poc_dat$poc_id_think_w2 <- ifelse(poc_dat$Q30_3_W2 > 0 & poc_dat$Q30_3_W2 <= 5, 
                                  poc_dat$Q30_3_W2*-1 + 6, 
                                  NA)

poc_dat$poc_id_w2 <- (poc_dat$poc_id_imp_w2 + poc_dat$poc_id_myself_w2 + poc_dat$poc_id_think_w2)/3
poc_dat$poc_id_sc_w2 <- (poc_dat$poc_id_w2-1)/4

# POC Solidarity ----------------------------------------------------------
poc_dat$poc_solid_solid_w1 <- ifelse(poc_dat$Q31_1 > 0 & poc_dat$Q31_1 <= 5, 
                                     poc_dat$Q31_1*-1 + 6, 
                                     NA)
poc_dat$poc_solid_probs_w1 <- ifelse(poc_dat$Q31_2 > 0 & poc_dat$Q31_2 <= 5, 
                                     poc_dat$Q31_2*-1 + 6, 
                                     NA)
poc_dat$poc_solid_linked_w1 <- ifelse(poc_dat$Q31_3 > 0 & poc_dat$Q31_3 <= 5, 
                                     poc_dat$Q31_3*-1 + 6, 
                                     NA)

poc_dat$poc_solid_w1 <- (poc_dat$poc_solid_solid_w1 + poc_dat$poc_solid_probs_w1 + poc_dat$poc_solid_linked_w1)/3
poc_dat$poc_solid_sc_w1 <- (poc_dat$poc_solid_w1-1)/4

poc_dat$poc_solid_solid_w2 <- ifelse(poc_dat$Q31_1_W2 > 0 & poc_dat$Q31_1_W2 <= 5, 
                                     poc_dat$Q31_1_W2*-1 + 6, 
                                     NA)
poc_dat$poc_solid_probs_w2 <- ifelse(poc_dat$Q31_2_W2 > 0 & poc_dat$Q31_2_W2 <= 5, 
                                     poc_dat$Q31_2_W2*-1 + 6, 
                                     NA)
poc_dat$poc_solid_linked_w2 <- ifelse(poc_dat$Q31_3_W2 > 0 & poc_dat$Q31_3_W2 <= 5, 
                                      poc_dat$Q31_3_W2*-1 + 6, 
                                      NA)

poc_dat$poc_solid_w2 <- (poc_dat$poc_solid_solid_w2 + poc_dat$poc_solid_probs_w2 + poc_dat$poc_solid_linked_w2)/3
poc_dat$poc_solid_sc_w2 <- (poc_dat$poc_solid_w2-1)/4


# Policy Opinions ---------------------------------------------------------
poc_dat$pp_daca_w1 <- ifelse(poc_dat$Q14a_1 > 0, poc_dat$Q14a_1*-1 + 6, NA)
poc_dat$pp_daca_w2 <- ifelse(poc_dat$Q14a_1_W2 > 0, poc_dat$Q14a_1_W2*-1 + 6, NA)

poc_dat$pp_daca_w1_sc <- scale01(poc_dat$pp_daca_w1)
poc_dat$pp_daca_w2_sc <- scale01(poc_dat$pp_daca_w2)

poc_dat$pp_visas_w1 <- ifelse(poc_dat$Q14a_3 > 0, poc_dat$Q14a_3*-1 + 6, NA)
poc_dat$pp_visas_w2 <- ifelse(poc_dat$Q14a_3_W2 > 0, poc_dat$Q14a_3_W2*-1 + 6, NA)

poc_dat$pp_visas_w1_sc <- scale01(poc_dat$pp_visas_w1)
poc_dat$pp_visas_w2_sc <- scale01(poc_dat$pp_visas_w2)

poc_dat$pp_reparations_w1 <- ifelse(poc_dat$Q14a_4 > 0, poc_dat$Q14a_4*-1 + 6, NA)
poc_dat$pp_reparations_w2 <- ifelse(poc_dat$Q14a_4_W2 > 0, poc_dat$Q14a_4_W2*-1 + 6, NA)

poc_dat$pp_reparations_w1_sc <- scale01(poc_dat$pp_reparations_w1)
poc_dat$pp_reparations_w2_sc <- scale01(poc_dat$pp_reparations_w2)

poc_dat$pp_afactColl_w1 <- ifelse(poc_dat$Q15a_1 > 0, poc_dat$Q15a_1*-1 + 6, NA)
poc_dat$pp_afactColl_w2 <- ifelse(poc_dat$Q15a_1_W2 > 0, poc_dat$Q15a_1_W2*-1 + 6, NA)

poc_dat$pp_afactColl_w1_sc <- scale01(poc_dat$pp_afactColl_w1)
poc_dat$pp_afactColl_w2_sc <- scale01(poc_dat$pp_afactColl_w2)


# VC ----------------------------------------------------------------------
poc_dat$vc_jb_dt_w1 <- NA
poc_dat$vc_jb_dt_w1[which(poc_dat$Q13a == 2)] <- 0
poc_dat$vc_jb_dt_w1[which(poc_dat$Q13a == 3)] <- .5 # dk
poc_dat$vc_jb_dt_w1[which(poc_dat$Q13a == 1)] <- 1 # biden
poc_dat$vc_jb_dt_w2 <- NA
poc_dat$vc_jb_dt_w2[which(poc_dat$Q13a_W2 == 2)] <- 0
poc_dat$vc_jb_dt_w2[which(poc_dat$Q13a_W2 == 3)] <- .5 # dk
poc_dat$vc_jb_dt_w2[which(poc_dat$Q13a_W2 == 1)] <- 1 # biden


poc_dat$vc_jb_rd_w1 <- NA
poc_dat$vc_jb_rd_w1[which(poc_dat$Q13b == 2)] <- 0
poc_dat$vc_jb_rd_w1[which(poc_dat$Q13b == 3)] <- .5 # dk
poc_dat$vc_jb_rd_w1[which(poc_dat$Q13b == 1)] <- 1 # biden
poc_dat$vc_jb_rd_w2 <- NA
poc_dat$vc_jb_rd_w2[which(poc_dat$Q13b_W2 == 2)] <- 0
poc_dat$vc_jb_rd_w2[which(poc_dat$Q13b_W2 == 3)] <- .5 # dk
poc_dat$vc_jb_rd_w2[which(poc_dat$Q13b_W2 == 1)] <- 1 # biden


poc_dat$vc_jb_nh_w1 <- NA
poc_dat$vc_jb_nh_w1[which(poc_dat$Q13c == 2)] <- 0
poc_dat$vc_jb_nh_w1[which(poc_dat$Q13c == 3)] <- .5 # dk
poc_dat$vc_jb_nh_w1[which(poc_dat$Q13c == 1)] <- 1 # biden
poc_dat$vc_jb_nh_w2 <- NA
poc_dat$vc_jb_nh_w2[which(poc_dat$Q13c_W2 == 2)] <- 0
poc_dat$vc_jb_nh_w2[which(poc_dat$Q13c_W2 == 3)] <- .5 # dk
poc_dat$vc_jb_nh_w2[which(poc_dat$Q13c_W2 == 1)] <- 1 # biden


poc_dat$vc_jb_ts_w1 <- NA
poc_dat$vc_jb_ts_w1[which(poc_dat$Q13d == 2)] <- 0
poc_dat$vc_jb_ts_w1[which(poc_dat$Q13d == 3)] <- .5 # dk
poc_dat$vc_jb_ts_w1[which(poc_dat$Q13d == 1)] <- 1 # biden
poc_dat$vc_jb_ts_w2 <- NA
poc_dat$vc_jb_ts_w2[which(poc_dat$Q13d_W2 == 2)] <- 0
poc_dat$vc_jb_ts_w2[which(poc_dat$Q13d_W2 == 3)] <- .5 # dk
poc_dat$vc_jb_ts_w2[which(poc_dat$Q13d_W2 == 1)] <- 1 # biden

