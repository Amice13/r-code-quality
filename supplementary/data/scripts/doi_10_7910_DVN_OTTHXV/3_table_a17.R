library(lfe)

wave2_survey <-
  estimation_data %>%
  mutate(party_num = case_when(D == "PKB" ~ 1,
                               D == "GERINDRA" ~ 2,
                               D == "PDIP" ~ 3,
                               D == "GOLKAR" ~ 4,
                               D == "NASDEM" ~ 5,
                               D == "PKS" ~ 8,
                               D == "PAN" ~ 12,
                               D == "DEMOKRAT" ~ 14,
                               D == "PERINDO" ~ 16,
                               D == "PPP" ~ 17,
                               TRUE ~ NA_real_)) %>%
  mutate(nomor_urut = str_remove(E, "Nomor Urut ")) %>%
  filter(A == "Wave 2") %>%
  select(F3, party_num, nomor_urut, A, GG, GG2, Y6, JML, H) %>%
  mutate(F3 = trimws(F3))

election_results <- read_csv("./_3_data/dprd2_vote_w_seats.csv")


election_results <- left_join(election_results %>% 
                                mutate(nomor_urut = as.character(nomor_urut)), 
                              wave2_survey, by = c("nama_dapil" = "F3",
                                                   "party" = "party_num",
                                                   "nomor_urut"))


election_results_sample <-
  election_results %>%
  mutate(cand_vote_share = as.numeric(jml_suara_indiv)/as.numeric(jml_suara_total)) %>%
  filter(nomor_urut %in% c(1,2,3),
         party %in% c(1,2,3,4,5,8,12,14,16, 17)) %>%
  filter(!is.infinite(cand_vote_share)) %>%
  mutate(treatment = case_when(A == "Wave 2" ~ 1,
                               TRUE ~ 0)) %>%
  mutate(nama_kab = str_remove_all(nama_dapil, "[:digit:]"),
         nama_kab = trimws(nama_kab)) %>%
  group_by(nama_kab) %>%
  mutate(val = sum(A == "Wave 2", na.rm = T)) %>%
  ungroup() %>%
  group_by(kode_dapil, party) %>%
  arrange(desc(cand_vote_share)) %>%
  mutate(rank = seq_along(cand_vote_share)) %>%
  ungroup() %>%
  arrange(kode_dapil, party) %>%
  mutate(understand = case_when(Y6 %in% c("Hampir semuanya dapat dipahami dengan baik", "Ya, hampir semua pertanyaan") ~ 1,
                                is.na(Y6) ~ 1,
                                TRUE ~ 0),
         treatment_observed = case_when(GG2 == "Ya, pernah lihat/baca hasilnya" ~ 1,
                                        is.na(GG2) ~ 0,
                                        TRUE ~ 0)) %>%
  mutate(treatment_restrict = case_when(understand == 1 & treatment_observed == 0 ~ 1,
                                        TRUE ~ 0))



#vote share model
vs_bivariate_mod <- lm(cand_vote_share ~ treatment + nomor_urut, data = election_results_sample %>% filter(val > 0))
vs_bivariate_mod_fe <- felm(cand_vote_share ~ treatment + nomor_urut | factor(nama_kab), data = election_results_sample %>% filter(val > 0))

#rank ordering model
rank_bivariate_mod <- lm(rank ~ treatment + nomor_urut, data = election_results_sample %>% filter(val > 0))
rank_bivariate_mod_fe <- felm(rank ~ treatment + nomor_urut | factor(nama_kab), data = election_results_sample %>% filter(val > 0))



#extracting the observations
observations <- c(nobs(vs_bivariate_mod),nobs(vs_bivariate_mod_fe),nobs(rank_bivariate_mod),nobs(rank_bivariate_mod_fe))

#adding robust standard errors

vs_bivariate_mod <- coeftest(vs_bivariate_mod, vcov=vcovHC(vs_bivariate_mod,type="HC0"))
vs_bivariate_mod_fe <- coeftest(vs_bivariate_mod_fe, vcov=vcovHC(vs_bivariate_mod_fe,type="HC0"))
rank_bivariate_mod <- coeftest(rank_bivariate_mod, vcov=vcovHC(rank_bivariate_mod,type="HC0"))
rank_bivariate_mod_fe <- coeftest(rank_bivariate_mod_fe, vcov=vcovHC(rank_bivariate_mod_fe,type="HC0"))


#putting models in a list
table <- list(vs_bivariate_mod, vs_bivariate_mod_fe, rank_bivariate_mod, rank_bivariate_mod_fe)

#writing the sub note
note_text <- paste("Standard errors were calculated using the Huber-White (HC0) correction. The first two columns
                   examine individual candidates' within-party vote shares. The second two columns look at candidates'
                   party list ranking as a function of their vote share.")

#creating the stargazer table
table = stargazer(table, type = 'latex', 
                  title = "The Effect of Intervention on Electoral Outcomes",
                  label = 'tab:table_vote',
                  model.names = F,
                  model.numbers = T,
                  digits = 3,
                  column.separate = c(2, 2),
                  column.labels = c("Vote Share (\\%):", "Ranking"),
                  
                  multicolumn = F,
                  dep.var.labels = NULL, 
                  add.lines = list(c("Observations", observations),
                                   c("Fixed Effects", "N", "Y", "N", "Y")),
                  covariate.labels = c("Treatment", "List Position: 2", "List Position: 3"),
                  #star.cutoffs = c(0.05, 0.01),
                  #float.env = 'sidewaystable',
                  keep.stat = c("n"),
                  notes = NULL,
                  notes.align = 'l')


#amending the stargazer output, appending note, printing, and rescaling (to 0.8 size)
#note that the amending stargazer output is removing certain lines from the standard output, such as
#the output has a line for "dependent variable", as well as lines breaks in between each coefficient, which we want to remove
#it also removes the **<0.05 line, etc, because this is contained in the new "note_text" object
table[-c(10, 11, 12, 18, 21, 24, 27, 33)] %>%
  write_latex(., note_text, './_4_outputs/tables/table_a17.tex', .8)

