

## ---------------------------
## 00_analysis_mainresults
## ---------------------------


## ---------------------------
## Import packages
## ---------------------------

rm(list = ls())
library(here)
library(dplyr)
library(stargazer)
library(ggplot2)
library(Hmisc)
library(scales)
library(xtable)
library(kableExtra)
library(DeclareDesign)
library(ivreg)

source(here("utils.R"))

## ---------------------------
## Define paths and other parameters
## ---------------------------

TABLE_OUTPUT_DIR = "tables/"
FIG_OUTPUT_DIR = "figures/"
color_map = c("Residential" = "darkgreen",
              "Online or none" = "wheat4") 


## ---------------------------
## Read in data and separate treatment 
## and control
## ---------------------------

## read in experimental data and create
## more descriptive treatment indicator
plot_labels = c("Residential", "Online or none")
exp_df_wprobs = read.csv(here("analytic_df.csv")) %>%
      mutate(invite_forplot_char = ifelse(randomized_invite == "invite", "Residential",
                                     "Online or none"),
             invite_forplot = factor(invite_forplot_char, 
                                     levels = plot_labels, 
                                     ordered = TRUE))

## separate into treatment and control
tx_group = exp_df_wprobs %>%
          filter(randomized_invite == "invite" &
                totalunits_t1t2_all > 0 & !is.na(totalunits_t1t2_all) &
                outcome_weighted_firstyear_gpa > 0) %>%
              mutate(treatment = ifelse(randomized_invite == 
                                          "invite",
                                        1, 0))
control_group = exp_df_wprobs %>%
          filter(randomized_invite == "no_invite" &
                totalunits_t1t2_all > 0 & !is.na(totalunits_t1t2_all) & 
                outcome_weighted_firstyear_gpa > 0) %>%
              mutate(treatment = ifelse(randomized_invite == 
                                          "invite",
                                        1, 0))
analytic_samp = rbind.data.frame(tx_group,
                                 control_group) 

## misc: ks-test of difference between gpa earned during summer bridge
## and gpa during school year
ks.test(analytic_samp$sb_gpa, analytic_samp$outcome_weighted_firstyear_gpa)
ks.test(tx_group$sb_gpa, tx_group$outcome_weighted_firstyear_gpa)

## field-specific samples due to course-specific enrollment patterns
tx_group_wri = tx_group %>% filter(outcome_fy_writing_gpa != 0 &
                                     !is.na(outcome_fy_writing_gpa)) 

control_group_wri = control_group %>% filter(outcome_fy_writing_gpa != 0 &
                                          !is.na(outcome_fy_writing_gpa)) 


## analytic sample for stem gpa - non-zero stem enrollment
tx_group_stemgpa = tx_group %>% filter(outcome_stem_category != "Never STEM GPA" &
                                         !illogical_stem &
                                         (t1_units_s_graded > 0 | 
                                            t2_units_s_graded > 0 | 
                                            sb_mol_units > 0 | 
                                            sb_egr_units > 0)) 
control_group_stemgpa = control_group %>% filter(outcome_stem_category != "Never STEM GPA" &
                                                   !illogical_stem &
                                                   (t1_units_s_graded > 0 | 
                                                      t2_units_s_graded > 0 | 
                                                      sb_mol_units > 0 | 
                                                      sb_egr_units > 0))

tx_group_stemgpa_noSB = tx_group %>% filter(outcome_stem_category != "Never STEM GPA" &
                                              (t1_units_s_graded  > 0 | 
                                                 t2_units_s_graded > 0)) 
control_group_stemgpa_noSB = control_group %>% filter(outcome_stem_category != "Never STEM GPA" &
                                                        (t1_units_s_graded > 0 | 
                                                           t2_units_s_graded > 0))


## ---------------------------
## Estimate models: general analytic sample
## ---------------------------

regs_weightGPA_noSB = run_regs("outcome_weighted_firstyear_gpa",
                         data = analytic_samp)
regs_weightGPA_SB = run_regs("outcome_weighted_firstyear_gpa_includeSB",
                         data = analytic_samp)

regs_perc100 = run_regs("outcome_perc_non100level_t1t2",
                         data = analytic_samp)


regs_withdrawal = run_regs("outcome_academic_leave",
                         data = analytic_samp)

regs_toofew = run_regs("outcome_toofew_credits",
                         data = analytic_samp)

regs_totalunits_noSB = run_regs("totalunits_t1t2_all",
                         data = analytic_samp)

regs_totalunits_SB = run_regs("outcome_totalunits_t1t2_includesb",
                         data = analytic_samp)

regs_perc_units_graded_noSB = run_regs("outcome_perc_units_graded_noSB",
                         data = analytic_samp)

regs_perc_units_graded_SB = run_regs("outcome_perc_units_graded_SB",
                         data = analytic_samp)

## ---------------------------
## Estimate models: reviewer-requested difficulty-weighted GPA
## ---------------------------
regs_diffadjusted_gpa  = run_regs("outcome_gpa_diffadjusted",
                               data = analytic_samp) # positive DV= outperforms

## ---------------------------
## Estimate models: field-specific samples
## ---------------------------


## outcomes with different samples
writing_samp <-  rbind.data.frame(tx_group_wri, control_group_wri)

regs_writing = run_regs("outcome_fy_writing_gpa",
                        data = writing_samp)

regs_percstem_SB = run_regs("outcome_perc_units_stem_withSB",
                         data = rbind.data.frame(tx_group, control_group))

regs_percstem_noSB = run_regs("outcome_perc_units_stem_noSB",
                         data = rbind.data.frame(tx_group, control_group))

regs_stemgpa_SB = run_regs("outcome_stem_gpa_withSB",
                         data = rbind.data.frame(tx_group_stemgpa, control_group_stemgpa))

regs_stemgpa_noSB = run_regs("outcome_stem_gpa_withSB",
                         data = rbind.data.frame(tx_group_stemgpa_noSB, control_group_stemgpa_noSB))


## ---------------------------
## Create tables with results
## ---------------------------

tables_omit <- c("strata_forRA", "block_id",
                 "ologit_linearpred") # non-treatment coefficients to omit

## ---------------------------
## Reviewer request difficulty-adjusted GPA
## ---------------------------

gen_reg_table(regs_diffadjusted_gpa,
              vec_col_labels = c("Block FE", "Block FE+linear predictor", "Tier FE", "Tier FE+linear predictor"),
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (all specifications): effect of GPA adjusted for measures of course difficulty. Omits block and tier fixed effects",
              label_str = "itt_regtable_gpadjusted_allspec",
              table_name_str = "itt_regtable_gpadjusted_allspec")


## ---------------------------
## Results 1: measures of academic difficulty
## ---------------------------

## assign relevant regs to shorter names due to stargazer setup
r1 <- regs_perc100
r2 <- regs_perc_units_graded_SB
r3 <- regs_totalunits_noSB
r4 <- regs_totalunits_SB


difficulty_labels <- c("Perc. courses > 100-level", "Perc. courses graded", "Total units (excludes SB)", "Total units (includes SB)")


gen_reg_table(list(r1$block, r2$block, r3$block, r4$block),
              vec_col_labels = difficulty_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (main specification: blocks): effect of SB program invitation on program difficulty. The table omits the block-specific fixed effects",
              label_str = "itt_reg_blocks_difficulty",
              table_name_str = "itt_regtable_difficulty_blocks")

gen_reg_table(list(r1$tier, r2$tier, r3$tier, r4$tier),
              vec_col_labels = difficulty_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (secondary specification: tiers): effect of SB program invitation on program difficulty. The table omits the tier-specific fixed effects",
              label_str = "itt_reg_tiers_difficulty",
              table_name_str = "itt_regtable_difficulty_tiers")

gen_reg_table(list(r1$tier_ologit, r2$tier_ologit, r3$tier_ologit, r4$tier_ologit),
              vec_col_labels = difficulty_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (secondary specification: tiers + linear predictor): effect of SB program invitation on program difficulty. The table omits the tier-specific fixed effects and linear predictor coefficient.",
              label_str = "itt_reg_tierslin_difficulty",
              table_name_str = "itt_regtable_difficulty_tier_ologit")

gen_reg_table(list(r1$block_ologit, r2$block_ologit, r3$block_ologit, r4$block_ologit),
              vec_col_labels = difficulty_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (secondary specification: blocks + linear predictor): effect of SB program invitation on program difficulty. The table omits the block-specific fixed effects and linear predictor coefficient.",
              label_str = "itt_reg_blockslin_difficulty",
              table_name_str = "itt_regtable_difficulty_block_ologit")


rm(r1, r2, r3, r4)


## ---------------------------
## Results 2: gpa, withdrawal, and too few credits 
## ---------------------------

r1 <- regs_weightGPA_SB
r2 <- regs_weightGPA_noSB
r3 <- regs_withdrawal
r4 <- regs_toofew

gpa_labels <- c("First-year GPA (includes SB)", 
  "First-year GPA (excludes SB)",
  "Withdrawal", "Too-few credits")

gen_reg_table(list(r1$block, r2$block, r3$block, r4$block),
              vec_col_labels = gpa_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (main specification: blocks): effect of SB program invitation on first-year GPA, rates of withdrawal, and rates of too-few crdits. The table omits the block-specific fixed effects",
              label_str = "itt_reg_blocks_gpa",
              table_name_str = "itt_regtable_gpa_blocks")

gen_reg_table(list(r1$tier, r2$tier, r3$tier, r4$tier),
              vec_col_labels = gpa_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (secondary specification: tiers): effect of SB program invitation on first-year GPA, rates of withdrawal, and rates of too-few credits. The table omits the tier-specific fixed effects",
              label_str = "itt_reg_tiers_gpa",
              table_name_str = "itt_regtable_gpa_tiers")

gen_reg_table(list(r1$tier_ologit, r2$tier_ologit, r3$tier_ologit, r4$block_ologit),
              vec_col_labels = gpa_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (secondary specification: tiers + linear predictor): effect of SB program invitation on first-year GPA, rates of withdrawal, and rates of too-few credits. The table omits the tier-specific fixed effects and linear predictor coefficient.",
              label_str = "itt_reg_tierslin_gpa",
              table_name_str = "itt_regtable_gpa_tier_ologit")

gen_reg_table(list(r1$block_ologit, r2$block_ologit, r3$block_ologit, r4$block_ologit),
              vec_col_labels = gpa_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (secondary specification: blocks + linear predictor): effect of SB program invitation on first-year GPA, rates of withdrawal, and rates of too-few credits. The table omits the block-specific fixed effects and linear predictor coefficient.",
              label_str = "itt_reg_blockslin_gpa",
              table_name_str = "itt_regtable_gpa_block_ologit")

rm(r1, r2, r3, r4, gpa_labels)


## ---------------------------
## Results 3: stem and writing gpas
## ---------------------------


r1 <- regs_stemgpa_SB
r2 <- regs_stemgpa_noSB
r3 <- regs_writing

gpa_labels <- c("STEM GPA (includes SB)", 
                "STEM GPA (excludes SB)",
                "Writing course GPA")

gen_reg_table(list(r1$block, r2$block, r3$block),
              vec_col_labels = gpa_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (main specification: blocks): effect of SB program invitation on STEM and writing GPAs. The table omits the block-specific fixed effects",
              label_str = "itt_reg_blocks_gpaspec",
              table_name_str = "itt_regtable_gpaspec_blocks")

gen_reg_table(list(r1$tier, r2$tier, r3$tier),
              vec_col_labels = gpa_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (secondary specification: tiers): effect of SB program invitation on STEM and writing GPAs. The table omits the tier-specific fixed effects",
              label_str = "itt_reg_tiers_gpaspec",
              table_name_str = "itt_regtable_gpaspec_tiers")

gen_reg_table(list(r1$tier_ologit, r2$tier_ologit, r3$tier_ologit),
              vec_col_labels = gpa_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (secondary specification: tiers + linear predictor): effect of SB program invitation on STEM and writing GPAs. The table omits the tier-specific fixed effects and linear predictor coefficient.",
              label_str = "itt_reg_tierslin_gpaspec",
              table_name_str = "itt_regtable_gpaspec_tier_ologit")

gen_reg_table(list(r1$block_ologit, r2$block_ologit, r3$block_ologit),
              vec_col_labels = gpa_labels,
              vec_cols_omit = tables_omit,
              caption_str = "ITT estimates (secondary specification: blocks + linear predictor): effect of SB program invitation on STEM and writing GPAs. The table omits the block-specific fixed effects and linear predictor coefficient.",
              label_str = "itt_reg_blockslin_gpaspec",
              table_name_str = "itt_regtable_gpaspec_block_ologit")

rm(r1, r2, r3)

## ---------------------------
## Complier analysis: check monotonicity assumption
## ---------------------------


### d = 2 (in person)
samp_comply = analytic_samp %>% filter(!is.na(is_any_SBinperson))
pr_d2_z1 = sum(samp_comply$is_any_SBinperson[samp_comply$treatment == 1])/nrow(samp_comply %>%
                                                                            filter(treatment == 1))
pr_d2_z0 = sum(samp_comply$is_any_SBinperson[samp_comply$treatment == 0])/nrow(samp_comply %>%
                                                                            filter(treatment == 0))
d2_diff = pr_d2_z1 - pr_d2_z0 
pr_d21_z1 = (sum(samp_comply$is_any_SBinperson[samp_comply$treatment == 1]) +
            sum(samp_comply$is_any_SBonline[samp_comply$treatment == 1]))/nrow(samp_comply %>%
                                                                            filter(treatment == 1))
pr_d21_z0 = (sum(samp_comply$is_any_SBinperson[samp_comply$treatment == 0]) +
            sum(samp_comply$is_any_SBonline[samp_comply$treatment == 0]))/nrow(samp_comply %>%
                                                                            filter(treatment == 0))
d21_diff = pr_d21_z1 - pr_d21_z0 
sprintf("In person probs D >= 2; Z = 1 is %s; Z = 0 is %s; diff is %s",
        round(pr_d2_z1, 3), 
        round(pr_d2_z0, 3), 
        round(d2_diff, 3))
sprintf("In person or online probs D >= 1; Z = 1 is %s; Z = 0 is %s; diff is %s",
        round(pr_d21_z1, 3), 
        round(pr_d21_z0, 3), 
        round(d21_diff, 3))



## ---------------------------
## Complier analysis: estimate models
## ---------------------------

## create a three-level ordinal variable
## keeping all 418 and assuming missing data = neither attend
analytic_samp <- analytic_samp %>%
        mutate(comply_3level = case_when(is_any_SBinperson ~ 2,
                                         is_any_SBonline ~ 1,
                                         TRUE ~ 0))

### academic difficulty outcomes
compliance_perc100_block = compliance_reg_auto(data = analytic_samp %>% mutate(ra_weight = 1), outcome_var = "outcome_perc_non100level_t1t2",
                      control_var = "block_id", comply_var = "comply_3level") 

compliance_totalunits_block = compliance_reg_auto(data = analytic_samp %>% mutate(ra_weight = 1), outcome_var = "outcome_totalunits_t1t2_includesb",
                      control_var = "block_id", comply_var = "comply_3level") 

compliance_totalunits_noSB_block = compliance_reg_auto(data = analytic_samp %>% mutate(ra_weight = 1), outcome_var = "totalunits_t1t2_all",
                      control_var = "block_id", comply_var = "comply_3level") 

compliance_percgraded_block = compliance_reg_auto(data = analytic_samp %>% mutate(ra_weight = 1), outcome_var = "outcome_perc_units_graded_SB",
                      control_var = "block_id", comply_var = "comply_3level") 


### gpa and withdrawal outcomes
compliance_gpa_nosb = compliance_reg_auto(data = analytic_samp %>% mutate(ra_weight = 1), outcome_var = "outcome_weighted_firstyear_gpa",
                      control_var = "block_id", comply_var = "comply_3level") 
compliance_gpa_withsb = compliance_reg_auto(data = analytic_samp %>% mutate(ra_weight = 1), outcome_var = "outcome_weighted_firstyear_gpa_includeSB",
                      control_var = "block_id", comply_var = "comply_3level") 
compliance_withdrawal = compliance_reg_auto(data = analytic_samp %>% mutate(ra_weight = 1), outcome_var = "outcome_academic_leave",
                      control_var = "block_id", comply_var = "comply_3level") 
compliance_toofew = compliance_reg_auto(data = analytic_samp %>% mutate(ra_weight = 1), outcome_var = "outcome_toofew_credits",
                                        control_var = "block_id", comply_var = "comply_3level") 


iv_forgraph = rbind.data.frame(get_iv_summary(compliance_perc100_block, ">100 level"),
                 get_iv_summary(compliance_totalunits_block, "Total units\n(includes SB)"),
                 get_iv_summary(compliance_totalunits_noSB_block, "Total units\n(excludes SB)"),
                 get_iv_summary(compliance_percgraded_block, "Prop. graded")) 

iv_forgraph_acadoutcomes = rbind.data.frame(get_iv_summary(compliance_gpa_nosb, "First-year GPA (excludes summer grades)"),
                 get_iv_summary(compliance_gpa_withsb, "First-year GPA (includes summer grades)"),
                 get_iv_summary(compliance_withdrawal, "Takes academic leave"),
                 get_iv_summary(compliance_toofew, "Too-few credits")) 

itt_forgraph = rbind.data.frame(get_itt_summary(regs_perc100$block, ">100 level"),
                 get_itt_summary(regs_totalunits_SB$block, "Total units\n(includes SB)"),
                 get_itt_summary(regs_totalunits_noSB$block, "Total units\n(excludes SB)"),
                 get_itt_summary(regs_perc_units_graded_SB$block, "Prop. graded")) 

itt_forgraph_acadoutcomes = rbind.data.frame(get_itt_summary(regs_weightGPA_noSB$block, "First-year GPA (excludes summer grades)"),
                 get_itt_summary(regs_weightGPA_SB$block, "First-year GPA (includes summer grades)"),
                 get_itt_summary(regs_weightGPA_SB$block, "Takes academic leave"),
                 get_itt_summary(regs_toofew$block, "Too-few credits")) 


## ---------------------------
## Formatted main-text table: program difficulty outcomes
## ---------------------------

## add CI to the outcomes
# read from 5a
d_table <- readr::read_rds(here("difficulty_outcomes_table_prep.rds"))
table_percover100 <- d_table$table_perc_over100
table_graded <- d_table$table_perc_units_graded_withSB
table_units <- d_table$table_totalunits_noSB
table_units_wsb <- d_table$table_totalunits_withSB

compare_iv_itt_difficultyoutcomes = rbind.data.frame(iv_forgraph %>% mutate(type = "CACE"),
                                               itt_forgraph %>% mutate(type = "ITT")) %>%
              mutate(outcome = case_when(outcome == ">100 level" ~ "Prop. non-introductory classes",
                                               outcome == "Prop. graded" ~ "Prop. units for grade",
                                               outcome == "Total units\n(excludes SB)" ~ "Total units (excludes SB)",
                                               outcome == "Total units\n(includes SB)" ~ "Total units (includes SB)")) %>%
          mutate(control_mean = 
           case_when(grepl("non-introductory", outcome) & type == "ITT" ~ 
                       as.numeric(table_percover100["control_value"][1]),
                     grepl("Total units.*includes", outcome) & type == "ITT" ~ 
                       as.numeric(table_units_wsb["control_value"][1]),
                     grepl("Total units.*excludes", outcome) & type == "ITT" ~
                       as.numeric(table_units["control_value"][1]),
                     grepl("for grade", outcome) & type == "ITT" ~
                       as.numeric(table_graded["control_value"][1])),
         control_lower = 
           case_when(grepl("non-introductory", outcome) & type == "ITT" ~ 
                       control_mean - 1.96*as.numeric(table_percover100["control_sem"][1]),
                     grepl("Total units.*includes", outcome) & type == "ITT" ~ 
                       control_mean - 1.96*as.numeric(table_units_wsb["control_sem"][1]),
                     grepl("Total units.*excludes", outcome) & type == "ITT" ~
                       control_mean - 1.96*as.numeric(table_units["control_sem"][1]),
                     grepl("for grade", outcome) & type == "ITT" ~ 
                       control_mean - 1.96*as.numeric(table_graded["control_sem"][1])),
         control_upper = 
           case_when(grepl("non-introductory", outcome) & type == "ITT" ~ 
                       control_mean + 1.96*as.numeric(table_percover100["control_sem"][1]),
                     grepl("Total units.*includes", outcome) & type == "ITT" ~ 
                       control_mean + 1.96*as.numeric(table_units_wsb["control_sem"][1]),
                     grepl("Total units.*excludes", outcome) & type == "ITT" ~
                       control_mean + 1.96*as.numeric(table_units["control_sem"][1]),
                     grepl("for grade", outcome) & type == "ITT" ~ 
                       control_mean + 1.96*as.numeric(table_graded["control_sem"][1]))) %>% 
  mutate(beta_combined = sprintf("%s\n[%s, %s]", round(beta, 2), round(lower, 2), round(upper, 2)),
         control_combined  = ifelse(type != "CACE", 
                                    sprintf("%s\n[%s, %s]", round(control_mean, 2), round(control_lower, 2), 
                                            round(control_upper, 2)),
                                    ""),
         total_n = n) %>%
  select(outcome, type, beta_combined, control_combined, total_n) %>%
  arrange(desc(outcome), desc(type))


## setup data
newt <- paste0("\\midrule\\multirow{", c(2,2,2), "}{*}{\\textbf{",
               "\\parbox{2cm}{",
               compare_iv_itt_difficultyoutcomes$outcome, "}}}")

newt[duplicated(newt)] <- ""

compare_iv_itt_difficultyoutcomes$outcome <- newt

colnames(compare_iv_itt_difficultyoutcomes) <- c("Outcome", "Estimand", 
                                           "Estimate", "Control mean", "N")

# reorder outcomes manually
compare_iv_itt_difficultyoutcomes_neworder <- compare_iv_itt_difficultyoutcomes[c(7:8, 5:6, 1:4),]

init_difficultyoutcomes_table <- xtable(compare_iv_itt_difficultyoutcomes_neworder, 
                                      sanitize.text.function = force,
                                      caption = "SB Program causes students to pursue a more challenging first-year academic program. The first set of columns show the regression estimates from two specifications, 
                                      examining outcomes that reflect a more challenging first-year academic program.  The smaller point estimates with tighter confidence intervals correspond to the ITT effects on 
                                      students invited regardless of attendance. The larger point estimates with  wider confidence intervals correspond to the effects on students who ``complied,'' or accepted the offer 
                                      to attend the residential program. The second set of columns show the raw mean for the control group group reweighted by the inverse probability weights discussed in Section \\ref{sec:iptw} 
                                      and the lower and upper bounds on those means (the standard error around the mean +- 1.96). 
                                      We see significant impacts on three of four outcomes, with the differences in total credits going away when we do not include credits earned over the summer.",
                                      label = "tab:difficulty_outcomes_summary") 


align_difficulty <- paste("|p{0.8in}|p{0.8in}|c|c|p{0.3in}|c|", collapse = "")
align(init_difficultyoutcomes_table) <- align_difficulty

print(init_difficultyoutcomes_table, 
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = force,
      size = "\\fontsize{6pt}{6pt}\\selectfont \\ \\def\\arraystretch{1.5}%",
      file = paste0(here(TABLE_OUTPUT_DIR), 
                    "difficulty_outcomes_maintext.tex"))




## ---------------------------
## Formatted main-text table: academic outcomes 
## ---------------------------


ac_table <- readr::read_rds(here("academic_outcomes_table_prep.rds"))
table_gpa_includeSB <- ac_table$table_gpa_includeSB
table_gpa_noSB <- ac_table$table_gpa_noSB
table_withdrawal <- ac_table$table_withdrawal
table_toofew <- ac_table$table_toofewcredits



compare_iv_itt_acadoutcomes = rbind.data.frame(iv_forgraph_acadoutcomes %>% mutate(type = "CACE"),
                                  itt_forgraph_acadoutcomes %>% mutate(type = "ITT")) %>%
                  mutate(control_mean = 
                      case_when(grepl("academic leave", outcome) & type == "ITT" ~ 
                                as.numeric(table_withdrawal["control_value"][1]),
                                grepl("First-year GPA.*includes.*", outcome) & type == "ITT" ~ 
                              as.numeric(table_gpa_includeSB["control_value"][1]),
                              grepl("First-year GPA.*excludes.*", outcome) & type == "ITT" ~
                              as.numeric(table_gpa_noSB["control_value"][1]),
                              grepl("Too-few credits", outcome) & type == "ITT" ~
                              as.numeric(table_toofew["control_value"][1])),
                      control_lower = 
                         case_when(grepl("academic leave", outcome) & type == "ITT" ~ 
                                control_mean - 1.96*as.numeric(table_withdrawal["control_sem"][1]),
                                grepl("First-year GPA.*includes.*", outcome) & type == "ITT" ~ 
                              control_mean- 1.96*as.numeric(table_gpa_includeSB["control_sem"][1]),
                              grepl("First-year GPA.*excludes.*", outcome) & type == "ITT" ~
                               control_mean- 1.96*as.numeric(table_gpa_noSB["control_sem"][1]),
                              grepl("Too-few credits", outcome) & type == "ITT" ~ 
                              control_mean - 1.96*as.numeric(table_toofew["control_sem"][1])),
                       control_upper = 
                         case_when(grepl("academic leave", outcome) & type == "ITT" ~ 
                                control_mean + 1.96*as.numeric(table_withdrawal["control_sem"][1]),
                                grepl("First-year GPA.*includes.*", outcome) & type == "ITT" ~ 
                              control_mean + 1.96*as.numeric(table_gpa_includeSB["control_sem"][1]),
                              grepl("First-year GPA.*excludes.*", outcome) & type == "ITT" ~
                              control_mean + 1.96*as.numeric(table_gpa_noSB["control_sem"][1]),
                              grepl("Too-few credits", outcome) & type == "ITT" ~ 
                                control_mean + 1.96*as.numeric(table_toofew["control_sem"][1]))) %>% 
                  mutate(beta_combined = sprintf("%s\n[%s, %s]", round(beta, 2), round(lower, 2), round(upper, 2)),
                         control_combined  = ifelse(type != "CACE", 
                                              sprintf("%s\n[%s, %s]", round(control_mean, 2), round(control_lower, 2), 
                                                     round(control_upper, 2)),
                                              ""),
                         total_n = n) %>%
                  select(outcome, type, beta_combined, control_combined, total_n) %>%
                  arrange(desc(outcome), desc(type))

# edits to get two outcomes on one line
# manually add multirow, inspired by:
# https://stackoverflow.com/questions/15036754/r-package-xtable-how-to-create-a-latextable-with-multiple-rows-and-columns-from
newt <- paste0("\\midrule\\multirow{", c(2,2,2), "}{*}{\\textbf{",
               "\\parbox{2cm}{",
       compare_iv_itt_acadoutcomes$outcome, "}}}")

newt[duplicated(newt)] <- ""

compare_iv_itt_acadoutcomes$outcome <- newt

colnames(compare_iv_itt_acadoutcomes) <- c("Outcome", "Estimand", 
                                           "Estimate", "Control mean", "N")

# reorder outcomes manually
compare_iv_itt_acadoutcomes_neworder <- compare_iv_itt_acadoutcomes[c(5:8, 1:4),]

init_academicoutcomes_table <- xtable(compare_iv_itt_acadoutcomes_neworder, 
                                      sanitize.text.function = force,
              caption = "SB Program has no adverse impact on GPA, rates of too-few credits, or rates of
              academic leave. Both the Intent-to-Treat (ITT) effect and complier
              average causal effect (CACE) estimates are presented, along with
              the 95\\% confidence intervals. The ITT effect represents the average
              impact of invitation to attend the SB program while the CACE is the
              effect of attending the program among those who would accept the
              invitation. We find little evidence for the impact of invitation to or
              participation in SB program on these outcomes.",
      label = "tab:academic_outcomes_summary") 


align_difficulty <- paste("|p{0.8in}|p{0.8in}|c|c|p{0.3in}|c|", collapse = "")
align(init_academicoutcomes_table) <- align_difficulty

print(init_academicoutcomes_table, 
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = force,
      size = "\\fontsize{6pt}{6pt}\\selectfont \\ \\def\\arraystretch{1.5}%",
      file = paste0(here(TABLE_OUTPUT_DIR), 
    "academic_outcomes_maintext.tex"))

