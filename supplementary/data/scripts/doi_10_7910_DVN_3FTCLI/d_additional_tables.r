#********************************************************************************
# R-script: Replication File for Tables
# "Bring a Friend: Leveraging Financial and Peer Support to Improve Women's Reproductive Agency in India"
# By: S Anukriti, Catalina Herrera-Almanza, Mahesh Karra
# Date: 17 December 2025
#********************************************************************************

###LOAD PACKAGES

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")
library(kableExtra)

if (!requireNamespace("haven", quietly = TRUE)) install.packages("haven")
library(haven)

options(scipen = 999)
options(knitr.kable.NA = '')

###SETTING WORKING DIRECTORY
###To use the correct file directory, replace line 25 with the appropriate file path where the replication data file and do-files are stored.
path = "INSERT PATH HERE"


# Table 7: MHT Index ----

table7 <- readLines(file.path(path, 'tables/table7_mht.tex'))
table7_sharpenedqvals_solo <- read_dta(file.path(path, "tables/table7_sharpenedqvals_solo.dta"))
table7_sharpenedqvals_baf <- read_dta(file.path(path, "tables/table7_sharpenedqvals_baf.dta"))
table7_sharpenedqvals_joint <- read_dta(file.path(path, "tables/table7_sharpenedqvals_joint.dta"))




table7_ed <- c(table7[1:6],
               
               paste0("\\emph{Anderson q-value} & ",
                      paste(
                        paste0("(",
                               round(table7_sharpenedqvals_solo$bky06_qval[1:4], 3),
                               ")"), 
                        collapse = " & " ),
                      " \\\\"
               ),
               table7[7:14],
               paste0("\\emph{Anderson q-value} & ",
                      paste(
                        paste0("(",
                               round(table7_sharpenedqvals_baf$bky06_qval[1:4], 3),
                               ")"), 
                        collapse = " & " ),
                      " \\\\"
               ),
               table7[15:20],
               paste0("\\emph{Anderson q-value (Own = BAF)} & ",
                      paste(
                        paste0("(",
                               round(table7_sharpenedqvals_joint$bky06_qval[1:4], 3),
                               ")"), 
                        collapse = " & " ),
                      " \\\\"
               ),
               table7[21]
)

table7_ed <- gsub("\\\\$", "\\\\\\", table7_ed)


filename <- 'tables/table7_ed.tex'
file.create(file.path(path, filename))
fileConn <- file(file.path(path, filename), open = "w")
writeLines(table7_ed, fileConn)
close(fileConn)


# Table A.2: Summary of Observations -----

vars <- c('ask_peer_adc', 'asked_hmil', 'asked_other', 
          'asked_sil', 'asked_nonfam', 
          'visit_adc_fp_corr', 'visit_any_fp_new_corr',
          'visit_adc_alone', 'visit_adc_w_hhmil_new',
          'visit_adc_w_nohmil_new', 'visit_adc_w_sil_new',
          'visit_adc_w_other_new', 
          'mod_method_el', 'mod_method_since_bl', 'heard_mod_method',  'preg_ever', 'wants_child',
          'outsider_new', 'atleast1_visithf_VoutHH2', 'peer_advise_FP_VoHH2',
          'afraid_being_seen')


data_endline_all <- read_dta(file.path(path, "data_endline_all.dta")) %>% 
  mutate(rn = row_number())

tab_obs <- data_endline_all %>% 
  select(rn, all_of(vars)) %>% 
  gather(var, val, -rn) %>% 
  
  reframe(N = n(),
          .by = var) %>% 
  
  left_join(
    data_endline_all %>% 
      select(rn, attrition_w2, all_of(vars)) %>% 
      gather(var, val, -c(rn, attrition_w2)) %>%
      filter(attrition_w2 == 1) %>% 
      
      reframe(attrited = n(),
              .by = var),
    by = join_by(var)
  ) %>% 
  
  left_join(
    data_endline_all %>% 
      filter(attrition_w2 == 0) %>% 
      select(rn, phone, all_of(vars)) %>% 
      gather(var, val, -c(rn, phone)) %>%
      mutate(phone = if_else(is.na(phone), 0, phone)) %>% 
      count(var, phone) %>% 
      spread(phone, n) %>% 
      rename(in_person = `0`,
             phone = `1`),
    by = join_by(var)
  ) %>% 
  select(var, N, attrited, in_person, phone) %>% 
  arrange(var) %>% 
  
  left_join(
    data_endline_all %>% 
      filter(attrition_w2 == 0) %>% 
      select(rn, phone, all_of(vars)) %>% 
      gather(var, val, -c(rn, phone), na.rm = T) %>% 
      mutate(phone = if_else(is.na(phone), 0, phone)) %>% 
      
      reframe(nmiss = n(),
              .by = c(var, phone)) %>% 
      spread(phone, nmiss) %>% 
      rename(nmiss_in_person = `0`,
             nmiss_phone = `1`) %>% 
      replace(is.na(.), 0),
    by = join_by(var)
  ) %>% 
  mutate(miss_in_person = in_person - nmiss_in_person,
         miss_phone = phone - nmiss_phone) %>% 
  
  left_join(
    data_endline_all %>% 
      filter(attrition_w2 == 0) %>% 
      mutate(miss_covariates = if_any(c(base_school_yrs, base_current_fp, 
                                        base_wants_child, base_num_friend, 
                                        base_mobi, base_work, base_ever_visit_fp, 
                                        base_mod_method, base_phone), 
                                      is.na) * 1) %>% 
      select(rn, miss_covariates, all_of(vars)) %>% 
      gather(var, val, -c(rn, miss_covariates), na.rm = T) %>% 
      # filter(miss_covariates == 0) %>% 
      count(var, miss_covariates) %>% 
      spread(miss_covariates, n) %>% 
      rename(estm_sample = `0`,
             miss_cov = `1`) %>% 
      select(var, miss_cov, estm_sample),
    by = join_by(var)
  ) %>% 
  
  mutate(var = recode_factor(var,
                             `ask_peer_adc` = 'Someone',
                             `asked_hmil` = 'Husband/MIL',
                             `asked_other` = 'Non-husband/non-MIL',
                             `asked_sil` = 'Sister-in-law',
                             `asked_nonfam` = 'Non-relative',
                             
                             `visit_adc_fp_corr` = 'Visited the ADC',
                             `visit_any_fp_new_corr` = 'Visited any clinic',
                             `visit_adc_alone` = 'Alone',
                             `visit_adc_w_hhmil_new` = 'with Husband/MIL',
                             `visit_adc_w_nohmil_new` = 'with non-husband/ non-MIL',
                             `visit_adc_w_sil_new` = 'with Sister-in-law',
                             `visit_adc_w_other_new` = 'with non-relative',
                             `mod_method_el` = 'Using a modern method at endline',
                             `mod_method_since_bl` = 'Has used a modern method since baseline',
                             `heard_mod_method` = 'Number of modern FP method heard',
                             `preg_ever` = 'Pregnancy since baseline',
                             `wants_child` = 'Wants another child',
                             `outsider_new` = 'Number of close peers outside HH in village',
                             `atleast1_visithf_VoutHH2` = 'Accompanied to health facility',
                             `peer_advise_FP_VoHH2` = 'Advised woman to use FP',
                             `afraid_being_seen` = 'Stigma: Afraid of being seen')) %>% 
  arrange(var) %>% 
  mutate(baseline = c(rep('No', 6), 'Yes', rep('No', 5), 
                      'Yes', 'No', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes', 'No'))



tab_obs %>% 
  select(var, nmiss_in_person, nmiss_phone, miss_in_person, 
         miss_phone, miss_cov, estm_sample, baseline) %>% 
  mutate_all(as.character) %>% 
  
  bind_rows(
    tibble(var = c(" ", " "),
           nmiss_in_person = c("Full sample", '(1)'),
           nmiss_phone = c('Attrited', '(2)'),
           miss_in_person = c("In-person", '(3)'),
           miss_phone = c("Phone", "(4)")) %>% 
      
      
      bind_rows(
        tab_obs %>% 
          select(N, attrited, in_person, phone) %>% 
          unique() %>% 
          mutate(var = 'Observations') %>% 
          select(var, everything()) %>% 
          rename(
            nmiss_in_person = N,
            nmiss_phone = attrited,
            miss_in_person = in_person,
            miss_phone = phone
          ) %>% 
          mutate_all(as.character)
      )
  ) %>%
  
  
  
  
  
  kbl(booktabs = T, linesep = "", format = 'latex', escape = F,
      align = c('l',rep('c', 7)),
      caption = 'Summary of the number of observations for each outcome',
      col.names = linebreak(c('Outcomes', 
                              'In-person\n(1)', 'Phone\n(2)', 'In-person\n(3)', 'Phone\n(4)',
                              'Missing in\ncovaraites\n(5)', 'Estimation\nsample\n(6)', 
                              'Collected \nat baseline\n(7)'), align = 'c')) %>% 
  kable_styling(latex_options = c('HOLD_position', 'scale_down')) %>% 
  add_header_above(c(" " = 1, 
                     'Non-missing sample' = 2, 'Missing sample' = 2, 
                     ' ' = 3), bold = T) %>% 
  pack_rows("Panel A:", 1, 19, italic = F , bold = T) %>% 
  pack_rows("Sought company to visit the ADC from:", 1, 5, italic = F, bold = F) %>% 
  pack_rows("Visited the ADC:", 8, 12, italic = F , bold = F) %>% 
  pack_rows("Peer engagement: Has close peers outside HH in village that:", 17, 18, italic = F , bold = F) %>% 
  pack_rows("Panel B:", 20, 22, italic = F , bold = T, hline_before = T) %>% 
  row_spec(21, hline_after = T) %>% 
  save_kable(file.path(path, "tables/appendix_table2.tex"))



# Table A.7: Observation Table Index ---- 


data_endline_index <- read_dta(file = file.path(path, "data_endline_final.dta")) %>% 
  rename(table2 = z_table2,
         table3 = z_table3,
         table4 = z_table4,
         table5 = z_table5)



outcomes2 <- c(
  "ask_peer_adc", "asked_hmil", "asked_other",
  "asked_sil", "asked_nonfam"
)

outcomes3 <- c(
  "visit_adc_fp_corr", "visit_any_fp_new_corr", "visit_adc_alone",
  "visit_adc_w_hhmil_new", "visit_adc_w_nohmil_new",
  "visit_adc_w_sil_new", "visit_adc_w_other_new"
)

outcomes4 <- c(
  "heard_mod_method", "afraid_being_seen",
  "mod_method_el", "mod_method_since_bl",
  "wants_child", "preg_ever"
)

outcomes5 <- c(
  "outsider_new", "atleast1_visithf_VoutHH2",
  "peer_advise_FP_VoHH2"
)




# Put all outcome vectors in a list
outcome_list <- list(
  outcomes2 = outcomes2,
  outcomes3 = outcomes3,
  outcomes4 = outcomes4,
  outcomes5 = outcomes5
)

# Corresponding z-table variables
z_tables <- paste0("table", 2:5)

# Loop over each outcome set and compute the index
index_list <- map2(
  outcome_list,
  z_tables,
  ~ data_endline_index %>%
    select(caseid, all_of(.x), all_of(.y)) %>%
    pivot_longer(!caseid, names_to = "var", values_to = "val") %>%
    summarise(!!paste0("index", substr(.y, 6, 6)) := sum(!is.na(val)), .by = var) %>% 
    mutate(var = if_else(var == .y, "index", var))
)

# Combine all index columns by variable
final_index_df <- reduce(index_list, full_join, by = "var") %>% 
  mutate(var = recode_factor(var,
                             `ask_peer_adc` = 'Someone',
                             `asked_hmil` = 'Husband/MIL',
                             `asked_other` = 'Non-husband/non-MIL',
                             `asked_sil` = 'Sister-in-law',
                             `asked_nonfam` = 'Non-relative',
                             
                             `visit_adc_fp_corr` = 'Visited the ADC',
                             `visit_any_fp_new_corr` = 'Visited any clinic',
                             `visit_adc_alone` = 'Alone',
                             `visit_adc_w_hhmil_new` = 'with Husband/MIL',
                             `visit_adc_w_nohmil_new` = 'with non-husband/ non-MIL',
                             `visit_adc_w_sil_new` = 'with Sister-in-law',
                             `visit_adc_w_other_new` = 'with non-relative',
                             
                             `heard_mod_method` = 'Number of modern FP method heard',
                             `afraid_being_seen` = 'Stigma: Afraid of being seen',
                             `mod_method_el` = 'Using a modern method at endline',
                             `mod_method_since_bl` = 'Has used a modern method since baseline',
                             `wants_child` = 'Wants another child',
                             `preg_ever` = 'Pregnancy since baseline',
                             
                             `outsider_new` = 'Number of close peers outside HH in village',
                             `atleast1_visithf_VoutHH2` = 'Accompanied to health facility',
                             `peer_advise_FP_VoHH2` = 'Advised woman to use FP',
                             
                             `index` = 'Observations in Index')) %>% 
  arrange(var)

final_index_df %>% 
  mutate_all(as.character) %>% 
  
  bind_rows(
    data_endline_index %>% 
      select(treatment, starts_with('table')) %>% 
      gather(var, val, -treatment) %>% 
      reframe(mean = mean(val, na.rm = T),
              .by = c(var, treatment)) %>% 
      mutate(var = gsub('table', 'index', var),
             mean = format(round(mean, 3), nsmall = 3),
             treatment = recode_factor(treatment,
                                       `0` = 'Control',
                                       `1` = 'OWN',
                                       `2` = 'BAF')) %>% 
      spread(var, mean) %>% 
      arrange(treatment) %>% 
      rename(var = treatment)
  ) %>% 
  
  kbl(booktabs = T, linesep = "", format = 'latex', escape = F,
      align = c('l',rep('c', 4)),
      caption = 'Summary of the number of observations for each outcome used in the Tables 2-5 indices',
      col.names = linebreak(c('Outcomes', 
                              'Table 2\nindex', 'Table 3\nindex', 
                              'Table 4\nindex', 'Table 5\nindex'), align = 'c')) %>% 
  kable_styling(latex_options = c('HOLD_position', 'scale_down')) %>% 
  
  pack_rows("Sought company to visit the ADC from:", 1, 5, italic = F, bold = F) %>% 
  pack_rows("Visited the ADC:", 8, 12, italic = F , bold = F) %>% 
  pack_rows("Peer engagement: Has close peers outside HH in village that:", 20, 21, italic = F , bold = F) %>% 
  pack_rows("Index mean:", 23, 25, italic = F , bold = F) %>% 
  row_spec(21, extra_latex_after = "\\midrule %") %>% 
  save_kable(file.path(path, "tables/appendix_table7.tex"))



# Table A.24: Balance at Baseline, by Endline Attrition Status ----

# Balance table version 1 (test + norm diff)
appendix_table24_ndiff <- read_tsv(file.path(path, "tables/appendix_table24_ndiff.csv")) %>% 
  mutate(Std_Diff = paste0(' & ', format(round(Std_Diff, 3), nsmall = 3), 
                           '  \\\\\\\\')) %>% 
  pull(Std_Diff)


appendix_table24 <- readLines(file.path(path, 'tables/appendix_table24.tex'))


for (n in 1:length(appendix_table24_ndiff)) {
  i = 2*n + 1
  j = 2*n + 2
  
  appendix_table24[i] <- gsub('\\\\\\\\',
                             appendix_table24_ndiff[n], 
                             appendix_table24[i])
  appendix_table24[j] <- gsub('\\\\\\\\',
                             " & \\\\\\\\", 
                             appendix_table24[j])
}


appendix_table24[length(appendix_table24)-2] <- gsub('\\\\\\\\',
                                                   "&  \\\\\\\\",
                                                   appendix_table24[length(appendix_table24)-2])

appendix_table24[length(appendix_table24)-1] <- gsub('\\\\\\\\',
                                                   "&  \\\\\\\\",
                                                   appendix_table24[length(appendix_table24)-1])

filename <- 'tables/appendix_table24_ed.tex'
file.create(file.path(path, filename))
fileConn <- file(file.path(path, filename), open = "w")
writeLines(appendix_table24, fileConn)
close(fileConn)



#  Table A.25: Balance at Baseline, by Endline Survey Mode ----

# Balance table version 1 (test + norm diff)
appendix_table25_ndiff <- read_tsv(file.path(path, "tables/appendix_table25_ndiff.csv")) %>% 
  mutate(Std_Diff = paste0(' & ', format(round(Std_Diff, 3), nsmall = 3), 
                           '  \\\\\\\\')) %>% 
  pull(Std_Diff)


appendix_table25 <- readLines(file.path(path, 'tables/appendix_table25.tex'))


for (n in 1:length(appendix_table25_ndiff)) {
  i = 2*n + 1
  j = 2*n + 2
  
  appendix_table25[i] <- gsub('\\\\\\\\',
                              appendix_table25_ndiff[n], 
                              appendix_table25[i])
  appendix_table25[j] <- gsub('\\\\\\\\',
                              " & \\\\\\\\", 
                              appendix_table25[j])
}


appendix_table25[length(appendix_table25)-2] <- gsub('\\\\\\\\',
                                                     "&  \\\\\\\\",
                                                     appendix_table25[length(appendix_table25)-2])

appendix_table25[length(appendix_table25)-1] <- gsub('\\\\\\\\',
                                                     "&  \\\\\\\\",
                                                     appendix_table25[length(appendix_table25)-1])

filename <- 'tables/appendix_table25_ed.tex'
file.create(file.path(path, filename))
fileConn <- file(file.path(path, filename), open = "w")
writeLines(appendix_table25, fileConn)
close(fileConn)


# Table A.26: Lee Bounds for Attrited Sample ----

appendix_table26_leebounds <- read_dta(file.path(path, "tables/appendix_table26.dta"))


appendix_table26_leebounds %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate_at(c('lb_b', 'lb_se',  'lb_p', 'diff_pval_lb', 
              'ub_b', 'ub_se',  'ub_p', 'diff_pval_ub'), 
            ~ format(., nsmall = 3)) %>% 
  mutate(y = recode_factor(y,
                           `ask_peer_adc` = 'Someone',
                           `asked_hmil` = 'Husband/MIL',
                           `asked_other` = 'Non-husband/non-MIL',
                           `asked_sil` = 'Sister-in-law',
                           `asked_nonfam` = 'Non-relative',
                           
                           `visit_adc_fp_corr` = 'Visited the ADC',
                           `visit_any_fp_new_corr` = 'Visited any clinic',
                           `visit_adc_alone` = 'Alone',
                           `visit_adc_w_hhmil_new` = 'with Husband/MIL',
                           `visit_adc_w_nohmil_new` = 'with non-husband/ non-MIL',
                           `visit_adc_w_sil_new` = 'with Sister-in-law',
                           `visit_adc_w_other_new` = 'with non-relative',
                           `mod_method_el` = 'Using a modern method at endline',
                           `mod_method_since_bl` = 'Has used a modern method since baseline',
                           `preg_ever` = 'Pregnancy since baseline',
                           `outsider_new` = 'Number of close peers outside HH in village',
                           `atleast1_visithf_VoutHH` = 'Accompanied to health facility',
                           `peer_advise_FP_VoHH` = 'Advised woman to use FP',
                           `afraid_being_seen` = 'Stigma: Afraid of being seen'),
         treat = recode_factor(treat,
                               `OWN` = 'Own',
                               `BAF` = 'BAF'),
         
         lb_sign = if_else(lb_p < .01, "***",
                           if_else(lb_p >=.01 & lb_p < .05, "**",
                                   if_else(lb_p >=.05 & lb_p < .1, "*", ""))),
         ub_sign = if_else(ub_p < .01, "***",
                           if_else(ub_p >=.01 & ub_p < .05, "**",
                                   if_else(ub_p >=.05 & ub_p < .1, "*", ""))),
         
         lb_b = paste0(lb_b, lb_sign),
         ub_b = paste0(ub_b, ub_sign)) %>%
  arrange(y, treat) %>% 
  
  select(y, treat, lb_b, lb_se, lb_p, diff_pval_lb,
         ub_b, ub_se, ub_p, diff_pval_ub, obs) %>% 
  mutate(across(c(diff_pval_lb, diff_pval_ub),
                ~ str_replace_all(., "^\\s*NA$", ""))) %>% 
  
  
  kbl(booktabs = T, linesep = "", format = 'latex', escape = F,
      align = c('l','c', 'l', 'c', 'c', 'c', 'l', 'c', 'c', 'c', 'c'),
      caption = 'Lee Bounds test, accounting for attrition sample only',
      col.names = c('Outcomes', 'Treatment', 'Coef.', 'SE', 'P-value', 'Diff (p-value)',
                    'Coef.', 'SE', 'P-value', 'Diff (p-value)', 'Obs.')) %>% 
  kable_styling(latex_options = c('HOLD_position', 'scale_down', 'threeparttable')) %>% 
  add_header_above(c(" " = 2, "Lower-bound" = 4, 'Upper-bound' = 4, ' ' = 1)) %>% 
  collapse_rows(c(1), latex_hline = 'none') %>% 
  pack_rows("Sought company to visit the ADC from:", 1, 10, italic = F, bold = F) %>% 
  pack_rows("Visited the ADC:", 15, 24, italic = F , bold = F) %>% 
  pack_rows("Peer engagement: Has close peers outside HH in village that:", 
            33, 36, italic = F , bold = F) %>% 
  
  save_kable(file = file.path(path, "tables/appendix_table26.tex"))



# Table A.27: Lee Bounds for Attrited Sample + Missing Outcomes ----

appendix_table27_leebounds_all <- read_dta(file.path(path, "tables/appendix_table27.dta"))


appendix_table27_leebounds_all %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate_at(c('lb_b', 'lb_se',  'lb_p', 'diff_pval_lb', 
              'ub_b', 'ub_se',  'ub_p', 'diff_pval_ub'), 
            ~ format(., nsmall = 3)) %>% 
  mutate(y = recode_factor(y,
                           `ask_peer_adc` = 'Someone',
                           `asked_hmil` = 'Husband/MIL',
                           `asked_other` = 'Non-husband/non-MIL',
                           `asked_sil` = 'Sister-in-law',
                           `asked_nonfam` = 'Non-relative',
                           
                           `visit_adc_fp_corr` = 'Visited the ADC',
                           `visit_any_fp_new_corr` = 'Visited any clinic',
                           `visit_adc_alone` = 'Alone',
                           `visit_adc_w_hhmil_new` = 'with Husband/MIL',
                           `visit_adc_w_nohmil_new` = 'with non-husband/ non-MIL',
                           `visit_adc_w_sil_new` = 'with Sister-in-law',
                           `visit_adc_w_other_new` = 'with non-relative',
                           `mod_method_el` = 'Using a modern method at endline',
                           `mod_method_since_bl` = 'Has used a modern method since baseline',
                           `preg_ever` = 'Pregnancy since baseline',
                           `outsider_new` = 'Number of close peers outside HH in village',
                           `atleast1_visithf_VoutHH` = 'Accompanied to health facility',
                           `peer_advise_FP_VoHH` = 'Advised woman to use FP',
                           `afraid_being_seen` = 'Stigma: Afraid of being seen'),
         treat = recode_factor(treat,
                               `OWN` = 'Own',
                               `BAF` = 'BAF'),
         
         lb_sign = if_else(lb_p < .01, "***",
                           if_else(lb_p >=.01 & lb_p < .05, "**",
                                   if_else(lb_p >=.05 & lb_p < .1, "*", ""))),
         ub_sign = if_else(ub_p < .01, "***",
                           if_else(ub_p >=.01 & ub_p < .05, "**",
                                   if_else(ub_p >=.05 & ub_p < .1, "*", ""))),
         
         lb_b = paste0(lb_b, lb_sign),
         ub_b = paste0(ub_b, ub_sign)) %>%
  arrange(y, treat) %>% 
  
  select(y, treat, lb_b, lb_se, lb_p, diff_pval_lb,
         ub_b, ub_se, ub_p, diff_pval_ub, obs) %>% 
  mutate(across(c(diff_pval_lb, diff_pval_ub),
                ~ str_replace_all(., "^\\s*NA$", ""))) %>% 
  # print(n = 40)
  
  kbl(booktabs = T, linesep = "", format = 'latex', escape = F,
      align = c('l','c', 'l', 'c', 'c', 'c', 'l', 'c', 'c', 'c', 'c'),
      caption = 'Lee Bounds test, accounting for attrition sample and phone survey sample with missing outcome values',
      col.names = c('Outcomes', 'Treatment', 'Coef.', 'SE', 'P-value', 'Diff (p-value)',
                    'Coef.', 'SE', 'P-value', 'Diff (p-value)', 'Obs.')) %>% 
  kable_styling(latex_options = c('HOLD_position', 'scale_down', 'threeparttable')) %>% 
  add_header_above(c(" " = 2, "Lower-bound" = 4, 'Upper-bound' = 4, ' ' = 1)) %>% 
  collapse_rows(c(1), latex_hline = 'none') %>% 
  pack_rows("Sought company to visit the ADC from:", 1, 10, italic = F, bold = F) %>% 
  pack_rows("Visited the ADC:", 15, 24, italic = F , bold = F) %>% 
  pack_rows("Peer engagement: Has close peers outside HH in village that:", 
            33, 36, italic = F , bold = F) %>% 
  
  save_kable(file.path(path, "tables/appendix_table27.tex"))



# Table A.28: Inverse Probability Weighting for Attrition Sample ----

appendix_table_28_ipw <- read_dta(file = file.path(path, "tables/appendix_table28.dta"))

appendix_table_28_ipw %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate_at(c('own_b', 'own_se',  'own_p', 
              'baf_b', 'baf_se',  'baf_p', 'joint_p'), 
            ~ format(., nsmall = 3)) %>% 
  mutate(y = recode_factor(y,
                           `ask_peer_adc` = 'Someone',
                           `asked_hmil` = 'Husband/MIL',
                           `asked_other` = 'Non-husband/non-MIL',
                           `asked_sil` = 'Sister-in-law',
                           `asked_nonfam` = 'Non-relative',
                           
                           `visit_adc_fp_corr` = 'Visited the ADC',
                           `visit_any_fp_new_corr` = 'Visited any clinic',
                           `visit_adc_alone` = 'Alone',
                           `visit_adc_w_hhmil_new` = 'with Husband/MIL',
                           `visit_adc_w_nohmil_new` = 'with non-husband/ non-MIL',
                           `visit_adc_w_sil_new` = 'with Sister-in-law',
                           `visit_adc_w_other_new` = 'with non-relative',
                           `mod_method_el` = 'Using a modern method at endline',
                           `mod_method_since_bl` = 'Has used a modern method since baseline',
                           `preg_ever` = 'Pregnancy since baseline',
                           `outsider_new` = 'Number of close peers outside HH in village',
                           `atleast1_visithf_VoutHH` = 'Accompanied to health facility',
                           `peer_advise_FP_VoHH` = 'Advised woman to use FP',
                           `afraid_being_seen` = 'Stigma: Afraid of being seen'),
         
         own_sign = if_else(own_p < .01, "***",
                            if_else(own_p >=.01 & own_p < .05, "**",
                                    if_else(own_p >=.05 & own_p < .1, "*", ""))),
         baf_sign = if_else(baf_p < .01, "***",
                            if_else(baf_p >=.01 & baf_p < .05, "**",
                                    if_else(baf_p >=.05 & baf_p < .1, "*", ""))),
         
         own_b = paste0(own_b, own_sign),
         baf_b = paste0(baf_b, baf_sign),
         
         rn = row_number()) %>%
  arrange(desc(rn)) %>% 
  
  select(-c(own_sign, baf_sign,  rn)) %>%
  # print(n = 40)
  
  kbl(booktabs = T, linesep = "", format = 'latex', escape = F,
      align = c('l', 'l', 'c', 'c', 'l', 'c', 'c', 'c' ,'c'),
      caption = 'Inverse Probability Weighted (IPW) estimation, accounting for attrition sample only',
      col.names = c('Outcomes', 'Coef.', 'SE', 'P-value',
                    'Coef.', 'SE', 'P-value', 'Obs.', 'P-value (Own = BAF)')) %>% 
  kable_styling(latex_options = c('HOLD_position', 'scale_down')) %>% 
  add_header_above(c(" " = 1, "Own Voucher" = 3, 
                     'BAF Voucher' = 3, ' ' = 2)) %>% 
  collapse_rows(1, latex_hline = 'none') %>% 
  pack_rows("Sought company to visit the ADC from:", 1, 5, italic = F, bold = F) %>% 
  pack_rows("Visited the ADC:", 8, 12, italic = F , bold = F) %>% 
  pack_rows("Peer engagement: Has close peers outside HH in village that:", 17, 18, italic = F , bold = F) %>% 
  save_kable(file.path(path, "tables/appendix_table28.tex"))

# Table A.29: Inverse Probability Weighting for Attrition Sample + Missing Outcomes ----

appendix_table_29_ipw_all <- read_dta(file.path(path, "tables/appendix_table29.dta"))

appendix_table_29_ipw_all %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate_at(c('own_b', 'own_se',  'own_p', 
              'baf_b', 'baf_se',  'baf_p', 'joint_p'), 
            ~ format(., nsmall = 3)) %>% 
  mutate(y = recode_factor(y,
                           `ask_peer_adc` = 'Someone',
                           `asked_hmil` = 'Husband/MIL',
                           `asked_other` = 'Non-husband/non-MIL',
                           `asked_sil` = 'Sister-in-law',
                           `asked_nonfam` = 'Non-relative',
                           
                           `visit_adc_fp_corr` = 'Visited the ADC',
                           `visit_any_fp_new_corr` = 'Visited any clinic',
                           `visit_adc_alone` = 'Alone',
                           `visit_adc_w_hhmil_new` = 'with Husband/MIL',
                           `visit_adc_w_nohmil_new` = 'with non-husband/ non-MIL',
                           `visit_adc_w_sil_new` = 'with Sister-in-law',
                           `visit_adc_w_other_new` = 'with non-relative',
                           `mod_method_el` = 'Using a modern method at endline',
                           `mod_method_since_bl` = 'Has used a modern method since baseline',
                           `preg_ever` = 'Pregnancy since baseline',
                           `outsider_new` = 'Number of close peers outside HH in village',
                           `atleast1_visithf_VoutHH` = 'Accompanied to health facility',
                           `peer_advise_FP_VoHH` = 'Advised woman to use FP',
                           `afraid_being_seen` = 'Stigma: Afraid of being seen'),
         
         own_sign = if_else(own_p < .01, "***",
                            if_else(own_p >=.01 & own_p < .05, "**",
                                    if_else(own_p >=.05 & own_p < .1, "*", ""))),
         baf_sign = if_else(baf_p < .01, "***",
                            if_else(baf_p >=.01 & baf_p < .05, "**",
                                    if_else(baf_p >=.05 & baf_p < .1, "*", ""))),
         
         own_b = paste0(own_b, own_sign),
         baf_b = paste0(baf_b, baf_sign),
         
         rn = row_number()) %>%
  arrange(desc(rn)) %>% 
  
  select(-c(own_sign, baf_sign,  rn)) %>%
  # print(n = 40)
  
  kbl(booktabs = T, linesep = "", format = 'latex', escape = F,
      align = c('l', 'l', 'c', 'c', 'l', 'c', 'c', 'c', 'c'),
      caption = 'Inverse Probability Weighted (IPW) estimation, accounting for attrition sample and phone survey sample with missing outcome values',
      col.names = c('Outcomes', 'Coef.', 'SE', 'P-value',
                    'Coef.', 'SE', 'P-value', 'Obs.', 'P-value (Own = BAF)')) %>% 
  kable_styling(latex_options = c('HOLD_position', 'scale_down')) %>% 
  add_header_above(c(" " = 1, "Own Voucher" = 3, 
                     'BAF Voucher' = 3, ' ' = 2)) %>% 
  collapse_rows(1, latex_hline = 'none') %>% 
  pack_rows("Sought company to visit the ADC from:", 1, 5, italic = F, bold = F) %>% 
  pack_rows("Visited the ADC:", 8, 12, italic = F , bold = F) %>% 
  pack_rows("Peer engagement: Has close peers outside HH in village that:", 17, 18, italic = F , bold = F) %>% 
  save_kable(file.path(path, "tables/appendix_table29.tex"))



