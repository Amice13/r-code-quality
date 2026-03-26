
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
test_mod1 <- lm(V_61_A==4 ~ treatment, data = models_data, weights = IPW)
test_mod2 <- lm(V_61_B==4 ~ treatment, data = models_data, weights = IPW)
test_mod3 <- lm(V_61_C==4 ~ treatment, data = models_data, weights = IPW)
test_mod4 <- lm(V_61_D==4 ~ treatment, data = models_data, weights = IPW)
test_mod5 <- lm(V_61_E==4 ~ treatment, data = models_data, weights = IPW)
test_mod6 <- lm(V_62_A==4 ~ treatment, data = models_data, weights = IPW)
test_mod7 <- lm(V_62_B==4 ~ treatment, data = models_data, weights = IPW)

#extracting the observations
observations <- c(nobs(test_mod1),nobs(test_mod2),nobs(test_mod3),nobs(test_mod4),nobs(test_mod5),nobs(test_mod6), nobs(test_mod7))

#adding robust standard errors

test_mod1 <- coeftest(test_mod1, vcov=vcovHC(test_mod1,type="HC0"))
test_mod2 <- coeftest(test_mod2, vcov=vcovHC(test_mod2,type="HC0"))
test_mod3 <- coeftest(test_mod3, vcov=vcovHC(test_mod3,type="HC0"))
test_mod4 <- coeftest(test_mod4, vcov=vcovHC(test_mod4,type="HC0"))
test_mod5 <- coeftest(test_mod5, vcov=vcovHC(test_mod5,type="HC0"))
test_mod6 <- coeftest(test_mod6, vcov=vcovHC(test_mod6,type="HC0"))
test_mod7 <- coeftest(test_mod7, vcov=vcovHC(test_mod7,type="HC0"))

#extracting the observations
observations <- c(nobs(test_mod1),nobs(test_mod2),nobs(test_mod3),nobs(test_mod4),nobs(test_mod5),nobs(test_mod6), nobs(test_mod7))

#putting models in a list
table <- list(test_mod1, test_mod2, test_mod3, test_mod4, test_mod5, test_mod6, test_mod7)

#writing the sub note
note_text <- paste(" Coefficients from OLS regression with inverse propensity weights. Standard errors were calculated using the Huber-White (HC0) correction. 
                   The first five outcomes measure perceived urgency of action required on (1) extreme heat, (2) flooding, 
                   (3) rising sea levels, (4) deforestation, (5) air pollution. The final two columns measure support for (6) 
                   a carbon tax and (7) a deforestation ban.")

#creating the stargazer table
table = stargazer(table, type = 'latex', 
                  title = "The Effect of Intervention on Policy Outcomes (Inverse Propensity Weighted)",
                  label = 'tab:testing_effect_main',
                  model.names = F,
                  model.numbers = T,
                  digits = 3,
                  column.separate = c(5, 3),
                  column.labels = c("Does Issue Merit Policy Attention:", "Support For Policy:"),
                  
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
  append(., c("\\cline{2-6} \\cline{7-8}",
              " &  &  &  &  &  &  & \\\\",
              " & Extreme Heat & Flooding & Rising Sea Levels & Deforestation & Air Pollution & Carbon Tax & Deforestation Ban \\\\"), after = 11) %>%
  write_latex(., note_text, './_4_outputs/tables/table_a40.tex', .8)






