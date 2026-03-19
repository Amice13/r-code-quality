
models_data <- 
  estimation_data %>%
  filter(!is.na(treatment)) %>%
  filter(treatment_observed == 0) %>%
  filter(Y6 == "Hampir semuanya dapat dipahami dengan baik") %>%
  mutate(gender = case_when(V_1 == "Laki-laki" ~ 1,
                            V_1 == "Perempuan" ~ 0,
                            TRUE ~ NA_real_)) %>%
  mutate(age = V_2)  %>%
  mutate(education = case_when(V_3 == "Sekolah Menengah Atas (SMA) atau sederajat" ~ 1,
                               V_3 == "Diploma (D1/D2/D3)" ~ 2,
                               V_3 == "S1" ~ 3,
                               V_3 == "S2" ~ 4, 
                               V_3 == "S3" ~ 5,
                               TRUE ~ NA_real_))  %>%
  mutate(college_diploma = case_when(V_3 == "Sekolah Menengah Atas (SMA) atau sederajat" ~ 0,
                                     V_3 == "Diploma (D1/D2/D3)" ~ 1,
                                     V_3 == "S1" ~ 1,
                                     V_3 == "S2" ~ 1, 
                                     V_3 == "S3" ~ 1,
                                     TRUE ~ NA_real_))  %>%
  mutate(political_experience = case_when(V_9 == "Tidak" ~ 0, 
                                          V_9 == "Ya" ~ 1,
                                          TRUE ~ NA_real_))

#generate weights
predict_mod <- lm(treatment ~ age + gender + education + political_experience, data = models_data)
p_treatment <- predict(predict_mod, newdata = models_data)
models_data$p_treatment <- p_treatment
models_data <-
  models_data %>%
  mutate(IPW = case_when(treatment == 1 ~ 1/p_treatment,
                         treatment == 0 ~ 1/(1-p_treatment)))

#here are the base models
test_mod1 <- lm(V_13_A==4 ~ treatment, data = models_data, weights = IPW)
test_mod2 <- lm(V_13_B==4 ~ treatment, data = models_data, weights = IPW)
test_mod3 <- lm(V_13_C==4 ~ treatment, data = models_data, weights = IPW)
test_mod4 <- lm(V_13_D==4 ~ treatment, data = models_data, weights = IPW)
test_mod5 <- lm(V_13_E==4 ~ treatment, data = models_data, weights = IPW)
test_mod6 <- lm(V_13_F==4 ~ treatment, data = models_data, weights = IPW)
test_mod7 <- lm(V_13_G==4 ~ treatment, data = models_data, weights = IPW)
test_mod8 <- lm(avg_index_v13 ~ treatment, data = models_data, weights = IPW)

#extracting the observations
observations <- c(nobs(test_mod1),nobs(test_mod2),nobs(test_mod3),nobs(test_mod4),nobs(test_mod5),nobs(test_mod6), nobs(test_mod7), nobs(test_mod8))

#adding robust standard errors

test_mod1 <- coeftest(test_mod1, vcov=vcovHC(test_mod1,type="HC0"))
test_mod2 <- coeftest(test_mod2, vcov=vcovHC(test_mod2,type="HC0"))
test_mod3 <- coeftest(test_mod3, vcov=vcovHC(test_mod3,type="HC0"))
test_mod4 <- coeftest(test_mod4, vcov=vcovHC(test_mod4,type="HC0"))
test_mod5 <- coeftest(test_mod5, vcov=vcovHC(test_mod5,type="HC0"))
test_mod6 <- coeftest(test_mod6, vcov=vcovHC(test_mod6,type="HC0"))
test_mod7 <- coeftest(test_mod7, vcov=vcovHC(test_mod7,type="HC0"))
test_mod8 <- coeftest(test_mod8, vcov=vcovHC(test_mod8,type="HC0"))


#putting models in a list -- note reordering
table <- list(test_mod1, test_mod2, test_mod3, test_mod7, test_mod5, test_mod6, test_mod4, test_mod8)

#writing the sub note
note_text <- paste(" Coefficients from OLS regression with inverse propensity weights. Robust and unclustered standard errors.
  Outcomes capturing whether respondents thought voters thought the following issues were very important: 
  (1) health, (2) education, (3) civil rights, (4) economy, (5) minority rights, (6) climate change, (7) pollution  (8) environment (index).")

#creating the stargazer table
table = stargazer(table, type = 'latex', 
                  title = "The Effect of Intervention on Politicians' Second-Order Views (Inverse Propensity Weighted)",
                  label = 'tab:table_2',
                  model.names = F,
                  model.numbers = T,
                  digits = 3,
                  column.separate = c(5, 3),
                  column.labels = c("Non-Environmental Issues:", "Environmental Issues:"),
                  multicolumn = F,
                  dep.var.labels = NULL, 
                  add.lines = list(c("Observations", observations)),
                  covariate.labels = c("Treatment"),
                  #star.cutoffs = c(0.05, 0.01),
                  #float.env = 'sidewaystable',
                  keep.stat = c("n"),
                  notes = NULL,
                  notes.align = 'l')


#amending the stargazer output, appending note, printing, and rescaling (to 0.8 size)
#note that the amending stargazer output is removing certain lines from the standard output, such as
#the output has a line for "dependent variable", as well as lines breaks in between each coefficient, which we want to remove
#it also removes the **<0.05 line, etc, because this is contained in the new "note_text" object
table[-c(10, 11, 12, 18, 21, 26)] %>%
  append(., c("\\cline{2-6} \\cline{7-9}",
              " &  &  &  &  &  &  & \\\\",
              " & Health & Education & Civ. Rights & Economy & Min. Rights & Clim. Change & Pollution & Env. Index \\\\"), after = 11) %>%
  write_latex(., note_text, './_4_outputs/tables/table_a39.tex', .8)


